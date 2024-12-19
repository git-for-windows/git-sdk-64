;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2015, 2017-2021 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; The fundamental lambda calculus reductions, like beta and eta
;;; reduction and so on.  Pretty lame currently.
;;;
;;; Code:

(define-module (language cps simplify)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:export (simplify))

(define (intset-maybe-add! set k add?)
  (if add? (intset-add! set k) set))

(define (intset-add*! set k*)
  (fold1 (lambda (k set) (intset-add! set k)) k* set))

(define (fold2* f l1 l2 seed)
  (let lp ((l1 l1) (l2 l2) (seed seed))
    (match (cons l1 l2)
      ((() . ()) seed)
      (((x1 . l1) . (x2 . l2)) (lp l1 l2 (f x1 x2 seed))))))

(define (transform-conts f conts)
  (persistent-intmap
   (intmap-fold (lambda (k v out)
                  (let ((v* (f k v)))
                    (cond
                     ((equal? v v*) out)
                     (v* (intmap-replace! out k v*))
                     (else (intmap-remove out k)))))
                conts
                conts)))

(define (compute-singly-referenced-vars conts)
  (define (visit label cont single multiple)
    (define (add-ref var single multiple)
      (if (intset-ref single var)
          (values single (intset-add! multiple var))
          (values (intset-add! single var) multiple)))
    (define (ref var) (add-ref var single multiple))
    (define (ref* vars) (fold2 add-ref vars single multiple))
    (match cont
      (($ $kargs _ _ ($ $continue _ _ exp))
       (match exp
         ((or ($ $const) ($ $prim) ($ $fun) ($ $rec) ($ $const-fun) ($ $code))
          (values single multiple))
         (($ $call proc args)
          (ref* (cons proc args)))
         (($ $callk k proc args)
          (ref* (if proc (cons proc args) args)))
         (($ $calli args callee)
          (ref* (cons callee args)))
         (($ $primcall name param args)
          (ref* args))
         (($ $values args)
          (ref* args))))
      (($ $kargs _ _ ($ $branch kf kt src op param args))
       (ref* args))
      (($ $kargs _ _ ($ $switch kf kt* src arg))
       (ref arg))
      (($ $kargs _ _ ($ $prompt k kh src escape? tag))
       (ref tag))
      (($ $kargs _ _ ($ $throw src op param args))
       (ref* args))
      (_
       (values single multiple))))
  (let*-values (((single multiple) (values empty-intset empty-intset))
                ((single multiple) (intmap-fold visit conts single multiple)))
    (intset-subtract (persistent-intset single)
                     (persistent-intset multiple))))

;;; Continuations whose values are simply forwarded to another and not
;;; used in any other way may be elided via eta reduction over labels.
;;;
;;; There is an exception however: we must exclude strongly-connected
;;; components (SCCs).  The only kind of SCC we can build out of $values
;;; expressions are infinite loops.
;;;
;;; Condition A below excludes single-node SCCs.  Single-node SCCs
;;; cannot be reduced.
;;;
;;; Condition B conservatively excludes edges to labels already marked
;;; as candidates.  This prevents back-edges and so breaks SCCs, and is
;;; optimal if labels are sorted.  If the labels aren't sorted it's
;;; suboptimal but cheap.
(define (compute-eta-reductions conts kfun singly-used)
  (define (singly-used? vars)
    (match vars
      (() #t)
      ((var . vars)
       (and (intset-ref singly-used var) (singly-used? vars)))))
  (define (visit-fun kfun body eta)
    (define (visit-cont label eta)
      (match (intmap-ref conts label)
        (($ $kargs names vars ($ $continue k src ($ $values vars)))
         (intset-maybe-add! eta label
                            (match (intmap-ref conts k)
                              (($ $kargs)
                               (and (not (eqv? label k)) ; A
                                    (not (intset-ref eta label)) ; B
                                    (singly-used? vars)))
                              (_ #f))))
        (_
         eta)))
    (intset-fold visit-cont body eta))
  (persistent-intset
   (intmap-fold visit-fun
                (compute-reachable-functions conts kfun)
                empty-intset)))

(define (eta-reduce conts kfun)
  (let* ((singly-used (compute-singly-referenced-vars conts))
         (label-set (compute-eta-reductions conts kfun singly-used)))
    ;; Replace any continuation to a label in LABEL-SET with the label's
    ;; continuation.  The label will denote a $kargs continuation, so
    ;; only terms that can continue to $kargs need be taken into
    ;; account.
    (define (subst label)
      (if (intset-ref label-set label)
          (match (intmap-ref conts label)
            (($ $kargs _ _ ($ $continue k)) (subst k)))
          label))
    (transform-conts
     (lambda (label cont)
       (and (not (intset-ref label-set label))
            (rewrite-cont cont
              (($ $kargs names syms ($ $branch kf kt src op param args))
               ($kargs names syms
                 ($branch (subst kf) (subst kt) src op param args)))
              (($ $kargs names syms ($ $switch kf kt* src arg))
               ($kargs names syms
                 ($switch (subst kf) (map subst kt*) src arg)))
              (($ $kargs names syms ($ $prompt k kh src escape? tag))
               ($kargs names syms
                 ($prompt (subst k) (subst kh) src escape? tag)))
              (($ $kargs names syms ($ $continue k src ($ $const val)))
               ,(match (intmap-ref conts k)
                  (($ $kargs (_)
                             ((? (lambda (var) (intset-ref singly-used var))
                                 var))
                             ($ $branch kf kt _ 'false? #f (var)))
                   (build-cont
                     ($kargs names syms
                       ($continue (subst (if val kf kt)) src ($values ())))))
                  (_
                   (build-cont
                     ($kargs names syms
                       ($continue (subst k) src ($const val)))))))
              (($ $kargs names syms ($ $continue k src exp))
               ($kargs names syms
                 ($continue (subst k) src ,exp)))
              (($ $kreceive ($ $arity req () rest () #f) k)
               ($kreceive req rest (subst k)))
              (($ $kclause arity body alt)
               ($kclause ,arity (subst body) alt))
              (($ $kfun src meta self tail entry)
               ($kfun src meta self tail (and entry (subst entry))))
              (_ ,cont))))
     conts)))

(define (compute-beta-reductions conts kfun)
  (define (visit-fun kfun body beta)
    (let* ((conts (intmap-select conts body))
           (single (compute-singly-referenced-labels conts)))
      (define (visit-cont label cont beta)
        (match cont
          ;; A continuation's body can be inlined in place of a $values
          ;; expression if the continuation is a $kargs.  It should only
          ;; be inlined if it is used only once, and not recursively.
          (($ $kargs _ _ ($ $continue k src ($ $values)))
           (intset-maybe-add! beta label
                              (and (intset-ref single k)
                                   (match (intmap-ref conts k)
                                     (($ $kargs) #t)
                                     (_ #f)))))
          (_
           beta)))
      (intmap-fold visit-cont conts beta)))
  (persistent-intset
   (intmap-fold visit-fun
                (compute-reachable-functions conts kfun)
                empty-intset)))

(define (compute-beta-var-substitutions conts label-set)
  (define (add-var-substs label var-map)
    (match (intmap-ref conts label)
      (($ $kargs _ _ ($ $continue k _ ($ $values vals)))
       (match (intmap-ref conts k)
         (($ $kargs names vars)
          (fold2* (lambda (var val var-map)
                    (intmap-add! var-map var val))
                  vars vals var-map))))))
  (intset-fold add-var-substs label-set empty-intmap))

(define (beta-reduce conts kfun)
  (let* ((label-set (compute-beta-reductions conts kfun))
         (var-map (compute-beta-var-substitutions conts label-set)))
    (define (subst var)
      (match (intmap-ref var-map var (lambda (_) #f))
        (#f var)
        (val (subst val))))
    (define (transform-term label term)
      (if (intset-ref label-set label)
          (match term
            (($ $continue k)
             (match (intmap-ref conts k)
               (($ $kargs _ _ term)
                (transform-term k term)))))
          (rewrite-term term
            (($ $continue k src exp)
             ($continue k src
               ,(rewrite-exp exp
                  ((or ($ $const) ($ $prim) ($ $fun) ($ $rec) ($ $const-fun)
                       ($ $code))
                   ,exp)
                  (($ $call proc args)
                   ($call (subst proc) ,(map subst args)))
                  (($ $callk k proc args)
                   ($callk k (and proc (subst proc)) ,(map subst args)))
                  (($ $calli args callee)
                   ($calli ,(map subst args) (subst callee)))
                  (($ $primcall name param args)
                   ($primcall name param ,(map subst args)))
                  (($ $values args)
                   ($values ,(map subst args))))))
            (($ $branch kf kt src op param args)
             ($branch kf kt src op param ,(map subst args)))
            (($ $switch kf kt* src arg)
             ($switch kf kt* src (subst arg)))
            (($ $prompt k kh src escape? tag)
             ($prompt k kh src escape? (subst tag)))
            (($ $throw src op param args)
             ($throw src op param ,(map subst args))))))
    (transform-conts
     (lambda (label cont)
       (rewrite-cont cont
         (($ $kargs names syms term)
          ($kargs names syms ,(transform-term label term)))
         (_ ,cont)))
     conts)))

(define (simplify conts)
  (eta-reduce (beta-reduce conts 0) 0))
