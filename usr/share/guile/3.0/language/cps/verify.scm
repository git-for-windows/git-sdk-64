;;; Diagnostic checker for CPS
;;; Copyright (C) 2014-2021,2023 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;; 
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; A routine to detect invalid CPS.
;;;
;;; Code:

(define-module (language cps verify)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (srfi srfi-11)
  #:export (verify))

(define (intset-pop set)
  (match (intset-next set)
    (#f (values set #f))
    (i (values (intset-remove set i) i))))

(define-syntax-rule (make-worklist-folder* seed ...)
  (lambda (f worklist seed ...)
    (let lp ((worklist worklist) (seed seed) ...)
      (call-with-values (lambda () (intset-pop worklist))
        (lambda (worklist i)
          (if i
              (call-with-values (lambda () (f i seed ...))
                (lambda (i* seed ...)
                  (let add ((i* i*) (worklist worklist))
                    (match i*
                      (() (lp worklist seed ...))
                      ((i . i*) (add i* (intset-add worklist i)))))))
              (values seed ...)))))))

(define worklist-fold*
  (case-lambda
    ((f worklist seed)
     ((make-worklist-folder* seed) f worklist seed))))

(define (check-distinct-vars conts)
  (define (adjoin-def var seen)
    (when (intset-ref seen var)
      (error "duplicate var name" seen var))
    (intset-add seen var))
  (intmap-fold
   (lambda (label cont seen)
     (match (intmap-ref conts label)
       (($ $kargs names vars term)
        (fold1 adjoin-def vars seen))
       (($ $kfun src meta (and self (not #f)) tail clause)
        (adjoin-def self seen))
       (_ seen))
     )
   conts
   empty-intset))

(define (compute-available-definitions conts kfun)
  "Compute and return a map of LABEL->VAR..., where VAR... are the
definitions that are available at LABEL."
  (define (adjoin-def var defs)
    (when (intset-ref defs var)
      (error "var already present in defs" defs var))
    (intset-add defs var))

  (define (propagate defs succ out)
    (let* ((in (intmap-ref defs succ (lambda (_) #f)))
           (in* (if in (intset-intersect in out) out)))
      (if (eq? in in*)
          (values '() defs)
          (values (list succ)
                  (intmap-add defs succ in* (lambda (old new) new))))))

  (define (visit-cont label defs)
    (let ((in (intmap-ref defs label)))
      (define (propagate0 out)
        (values '() defs))
      (define (propagate1 succ out)
        (propagate defs succ out))
      (define (propagate2 succ0 succ1 out)
        (let*-values (((changed0 defs) (propagate defs succ0 out))
                      ((changed1 defs) (propagate defs succ1 out)))
          (values (append changed0 changed1) defs)))
      (define (propagate* succs out)
        (let lp ((succs succs) (changed '()) (defs defs))
          (match succs
            (() (values changed defs))
            ((succ . succs)
             (let-values (((changed* defs) (propagate defs succ out)))
               (lp succs (append changed* changed) defs))))))

      (match (intmap-ref conts label)
        (($ $kargs names vars term)
         (let ((out (fold1 adjoin-def vars in)))
           (match term
             (($ $continue k)
              (propagate1 k out))
             (($ $branch kf kt)
              (propagate2 kf kt out))
             (($ $switch kf kt*)
              (propagate* (cons kf kt*) out))
             (($ $prompt k kh)
              (propagate2 k kh out))
             (($ $throw)
              (propagate0 out)))))
        (($ $kreceive arity k)
         (propagate1 k in))
        (($ $kfun src meta self tail clause)
         (let ((out (if self (adjoin-def self in) in)))
           (if clause
               (propagate1 clause out)
               (propagate0 out))))
        (($ $kclause arity kbody kalt)
         (if kalt
             (propagate2 kbody kalt in)
             (propagate1 kbody in)))
        (($ $ktail) (propagate0 in)))))

  (worklist-fold* visit-cont
                  (intset kfun)
                  (intmap-add empty-intmap kfun empty-intset)))

(define (intmap-for-each f map)
  (intmap-fold (lambda (k v seed) (f k v) seed) map *unspecified*))

(define (check-valid-var-uses conts kfun)
  (define (adjoin-def var defs) (intset-add defs var))
  (let visit-fun ((kfun kfun) (free empty-intset) (first-order empty-intset))
    (define (visit-exp exp bound first-order)
      (define (check-use var)
        (unless (intset-ref bound var)
          (error "unbound var" var)))
      (define (visit-first-order kfun)
        (if (intset-ref first-order kfun)
            first-order
            (visit-fun kfun empty-intset (intset-add first-order kfun))))
      (match exp
        ((or ($ $const) ($ $prim)) first-order)
        (($ $fun kfun)
         (visit-fun kfun bound first-order))
        (($ $const-fun kfun)
         (visit-first-order kfun))
        (($ $code kfun)
         (visit-first-order kfun))
        (($ $rec names vars (($ $fun kfuns) ...))
         (let ((bound (fold1 adjoin-def vars bound)))
           (fold1 (lambda (kfun first-order)
                   (visit-fun kfun bound first-order))
                  kfuns first-order)))
        (($ $values args)
         (for-each check-use args)
         first-order)
        (($ $call proc args)
         (check-use proc)
         (for-each check-use args)
         first-order)
        (($ $callk kfun proc args)
         (when proc (check-use proc))
         (for-each check-use args)
         (visit-first-order kfun))
        (($ $calli args callee)
         (for-each check-use args)
         (check-use callee)
         first-order)
        (($ $primcall name param args)
         (for-each check-use args)
         first-order)))
    (define (visit-term term bound first-order)
      (define (check-use var)
        (unless (intset-ref bound var)
          (error "unbound var" var)))
      (define (visit-first-order kfun)
        (if (intset-ref first-order kfun)
            first-order
            (visit-fun kfun empty-intset (intset-add first-order kfun))))
      (match term
        (($ $continue k src exp)
         (match exp
           ((or ($ $const) ($ $prim)) first-order)
           (($ $fun kfun)
            (visit-fun kfun bound first-order))
           (($ $const-fun kfun)
            (visit-first-order kfun))
           (($ $code kfun)
            (visit-first-order kfun))
           (($ $rec names vars (($ $fun kfuns) ...))
            (let ((bound (fold1 adjoin-def vars bound)))
              (fold1 (lambda (kfun first-order)
                       (visit-fun kfun bound first-order))
                     kfuns first-order)))
           (($ $values args)
            (for-each check-use args)
            first-order)
           (($ $call proc args)
            (check-use proc)
            (for-each check-use args)
            first-order)
           (($ $callk kfun proc args)
            (when proc (check-use proc))
            (for-each check-use args)
            (visit-first-order kfun))
           (($ $calli args callee)
            (for-each check-use args)
            (check-use callee)
            first-order)
           (($ $primcall name param args)
            (for-each check-use args)
            first-order)))
        (($ $branch kf kt src name param args)
         (for-each check-use args)
         first-order)
        (($ $switch kf kt* src arg)
         (check-use arg)
         first-order)
        (($ $prompt k kh src escape? tag)
         (check-use tag)
         first-order)
        (($ $throw src op param args)
         (for-each check-use args)
         first-order)))
    (intmap-fold
     (lambda (label bound first-order)
       (let ((bound (intset-union free bound)))
         (match (intmap-ref conts label)
           (($ $kargs names vars term)
            (visit-term term (fold1 adjoin-def vars bound) first-order))
           (_ first-order))))
     (compute-available-definitions conts kfun)
     first-order)))

(define (check-label-partition conts kfun)
  ;; A continuation can only belong to one function.
  (intmap-fold
   (lambda (kfun body seen)
     (intset-fold
      (lambda (label seen)
        (intmap-add seen label kfun
                    (lambda (old new)
                      (error "label used by two functions" label old new))))
      body
      seen))
   (compute-reachable-functions conts kfun)
   empty-intmap))

(define (compute-reachable-labels conts kfun)
  (intmap-fold (lambda (kfun body seen) (intset-union seen body))
               (compute-reachable-functions conts kfun)
               empty-intset))

(define (check-arities conts kfun)
  (define (check-arity exp cont)
    (define (assert-unary)
      (match cont
        (($ $kargs (_) (_)) #t)
        (_ (error "expected unary continuation" cont))))
    (define (assert-nullary)
      (match cont
        (($ $kargs () ()) #t)
        (_ (error "expected unary continuation" cont))))
    (define (assert-n-ary n)
      (match cont
        (($ $kargs names vars)
         (unless (= (length vars) n)
           (error "expected n-ary continuation" n cont)))
        (_ (error "expected $kargs continuation" cont))))
    (match exp
      ((or ($ $const) ($ $prim) ($ $const-fun) ($ $code) ($ $fun))
       (assert-unary))
      (($ $rec names vars funs)
       (unless (= (length names) (length vars) (length funs))
         (error "invalid $rec" exp))
       (assert-n-ary (length names))
       (match cont
         (($ $kargs names vars*)
          (unless (equal? vars* vars)
            (error "bound variable mismatch" vars vars*)))))
      (($ $values args)
       (match cont
         (($ $ktail) #t)
         (_ (assert-n-ary (length args)))))
      (($ $call proc args)
       (match cont
         ((or ($ $kreceive) ($ $ktail)) #t)
         (_ (error "expected $kreceive or $ktail continuation" cont))))
      ((or ($ $calli) ($ $callk))
       (match cont
         ((or ($ $kargs) ($ $kreceive) ($ $ktail)) #t)
         (_ (error "expected $kargs, $kreceive or $ktail continuation" cont))))
      (($ $primcall name param args)
       (match cont
         (($ $kargs) #t)
         (($ $kreceive)
          (match exp
            (($ $primcall 'call-thunk/no-inline #f (thunk)) #t)
            (_ (cont (error "bad continuation" exp cont)))))))))
  (define (check-term term)
    (define (assert-nullary k)
      (match (intmap-ref conts k)
        (($ $kargs () ()) #t)
        (cont (error "expected nullary cont" cont))))
    (match term
      (($ $continue k src exp)
       (check-arity exp (intmap-ref conts k)))
      (($ $branch kf kt src op param args)
       (assert-nullary kf)
       (assert-nullary kt))
      (($ $switch kf kt* src arg)
       (assert-nullary kf)
       (for-each assert-nullary kt*))
      (($ $prompt k kh src escape? tag)
       (assert-nullary k)
       (match (intmap-ref conts kh)
         (($ $kreceive) #t)
         (cont (error "bad prompt handler" cont))))
      (($ $throw)
       #t)))
  (let ((reachable (compute-reachable-labels conts kfun)))
    (intmap-for-each
     (lambda (label cont)
       (when (intset-ref reachable label)
         (match cont
           (($ $kargs names vars term)
            (unless (= (length names) (length vars))
              (error "broken $kargs" label names vars))
            (check-term term))
           (_ #t))))
     conts)))

(define (check-functions-bound-once conts kfun)
  (let ((reachable (compute-reachable-labels conts kfun)))
    (define (add-fun fun functions)
      (when (intset-ref functions fun)
        (error "function already bound" fun))
      (intset-add functions fun))
    (intmap-fold
     (lambda (label cont functions)
       (if (intset-ref reachable label)
           (match cont
             (($ $kargs _ _ ($ $continue _ _ ($ $fun kfun)))
              (add-fun kfun functions))
             (($ $kargs _ _ ($ $continue _ _ ($ $rec _ _ (($ $fun kfuns) ...))))
              (fold1 add-fun kfuns functions))
             (_ functions))
           functions))
     conts
     empty-intset)))

(define (verify conts)
  (check-distinct-vars conts)
  (check-label-partition conts 0)
  (check-valid-var-uses conts 0)
  (check-arities conts 0)
  (check-functions-bound-once conts 0)
  conts)
