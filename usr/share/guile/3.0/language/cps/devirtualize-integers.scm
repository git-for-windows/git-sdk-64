;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

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
;;; Some parts of programs operate on exact integers.  An exact integer
;;; is either a fixnum or a bignum.  It's often the case that if we know
;;; that a number is a fixnum, all operations on it can be unboxed in
;;; terms of s64 operations.  But if there's a series of operations and
;;; each one works on either bignums or fixnums, then the mixing of
;;; fixnums and bignums through that one control and data flow path
;;; makes it impossible for the compiler to specialize operations to
;;; either type.
;;;
;;; This "integer devirtualization" pass tries to duplicate the control
;;; and data flow of exact integers into two flows: one for bignums and
;;; one for fixnums.  This causes code growth, so it's something we need
;;; to be careful about.
;;;
;;; Code:

(define-module (language cps devirtualize-integers)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (language cps)
  #:use-module (language cps effects-analysis)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:export (devirtualize-integers))

;; Compute a map from VAR -> COUNT, where COUNT indicates the number of
;; times in the source program that VAR is used.
(define (compute-use-counts cps)
  (define (add-use use-counts var)
    (let ((count (1+ (intmap-ref use-counts var (lambda (_) 0)))))
      (intmap-add! use-counts var count (lambda (old new) new))))
  (define (add-uses use-counts vars)
    (match vars
      (() use-counts)
      ((var . vars) (add-uses (add-use use-counts var) vars))))
  (persistent-intmap
   (intmap-fold
    (lambda (label cont use-counts)
      (match cont
        (($ $kargs names vars term)
         (match term
           (($ $continue k src exp)
            (match exp
              ((or ($ $const) ($ $prim) ($ $fun) ($ $const-fun) ($ $code) ($ $rec))
               use-counts)
              (($ $values args)
               (add-uses use-counts args))
              (($ $call proc args)
               (add-uses (add-use use-counts proc) args))
              (($ $callk kfun proc args)
               (add-uses (if proc (add-use use-counts proc) use-counts) args))
              (($ $primcall name param args)
               (add-uses use-counts args))))
           (($ $branch kf kt src op param args)
            (add-uses use-counts args))
           (($ $switch kf kt* src arg)
            (add-use use-counts arg))
           (($ $prompt k kh src escape? tag)
            (add-use use-counts tag))
           (($ $throw src op param args)
            (add-uses use-counts args))))
        (_ use-counts)))
    cps
    (transient-intmap))))

(define (bailout? cps label)
  (match (intmap-ref cps label)
    (($ $kargs _ _ ($ $throw)) #t)
    (_ #f)))

(define (peel-trace cps label fx kexit use-counts)
  "For the graph starting at LABEL, try to peel out a trace that uses
the variable FX.  A peelable trace consists of effect-free terms, or
terms that only have &type-check effect but which use FX or some
variable that was defined using FX as an input.  No variable defined in
the trace should be referenced outside of it."
  (let peel-cont ((cps cps) (label label)
                  (live-vars empty-intmap) ;; var -> pending refcount
                  (fresh-vars empty-intmap) ;; old-name -> new name
                  (vars-of-interest (intset-add empty-intset fx))
                  (defs-of-interest? #f))
    (define (fail) (with-cps cps #f))
    (define (add-live-vars live-vars vars)
      (match vars
        (() live-vars)
        ((var . vars)
         (add-live-vars
          (let ((count (intmap-ref use-counts var (lambda (_) 0))))
            (if (zero? count)
                live-vars
                (intmap-add live-vars var count)))
          vars))))
    (define (subtract-uses live-vars vars)
      (match vars
        (() live-vars)
        ((var . vars)
         (subtract-uses
          (let ((count (intmap-ref live-vars var (lambda (_) #f))))
            (cond
             ((not count) live-vars)
             ((= count 1) (intmap-remove live-vars var))
             (else (intmap-replace live-vars var (1- count)))))
          vars))))
    (match (intmap-ref cps label)
      ;; We know the initial label is a $kargs, and we won't follow the
      ;; graph to get to $kreceive etc, so we can stop with these two
      ;; continuation kinds.  (For our purposes, only $values can
      ;; continue to $ktail.)
      (($ $ktail) (fail))
      (($ $kargs names vars term)
       (let* ((vars-of-interest
               (if defs-of-interest?
                   (fold1 (lambda (var set) (intset-add set var))
                          vars vars-of-interest)
                   vars-of-interest))
              (live-vars (add-live-vars live-vars vars))
              (fresh-vars (fold (lambda (var fresh-vars)
                                  (intmap-add fresh-vars var (fresh-var)))
                                fresh-vars vars))
              (peeled-vars (map (lambda (var) (intmap-ref fresh-vars var))
                                vars)))
         (define (rename-uses args)
           (map (lambda (arg) (intmap-ref fresh-vars arg (lambda (arg) arg)))
                args))
         (define (any-use-of-interest? args)
           (or-map (lambda (arg) (intset-ref vars-of-interest arg))
                   args))
         (define (continue k live-vars defs-of-interest? can-terminate-trace?
                           make-term)
           (define (stitch cps k)
             (with-cps cps
               (letk label* ($kargs names peeled-vars ,(make-term k)))
               label*))
           (define (terminate)
             (stitch cps k))
           (with-cps cps
             (let$ k* (peel-cont k live-vars fresh-vars vars-of-interest
                                 defs-of-interest?))
             ($ ((lambda (cps)
                   (cond
                    (k* (stitch cps k*))
                    ((and can-terminate-trace? (eq? live-vars empty-intmap))
                     (terminate))
                    (else (fail))))))))
         (match term
           (($ $branch kf kt src op param args)
            ;; kt or k is kf; var of interest is in args
            (let* ((live-vars (subtract-uses live-vars args))
                   (uses-of-interest? (any-use-of-interest? args))
                   (defs-of-interest? #f) ;; Branches don't define values.
                   (can-terminate-trace? uses-of-interest?)
                   (peeled-args (rename-uses args)))
              (cond
               ((not uses-of-interest?)
                (fail))
               ((bailout? cps kt)
                (continue kf live-vars defs-of-interest? can-terminate-trace?
                          (lambda (kf)
                            (build-term
                              ($branch kf kt src op param peeled-args)))))
               ((bailout? cps kf)
                (continue kt live-vars defs-of-interest? can-terminate-trace?
                          (lambda (kt)
                            (build-term
                              ($branch kf kt src op param peeled-args)))))
               ((eq? live-vars empty-intmap)
                (with-cps cps
                  (letk label*
                        ($kargs names peeled-vars
                          ($branch kf kt src op param peeled-args)))
                  label*))
               (else
                (fail)))))
           (($ $switch)
            ;; Don't know how to peel past a switch.  The arg of a
            ;; switch is unboxed anyway.
            (fail))
           (($ $continue k src exp)
            (match exp
              (($ $const)
               ;; fine.
               (continue k live-vars #f #f
                         (lambda (k)
                           (build-term ($continue k src ,exp)))))
              (($ $values args)
               (let ((uses-of-interest? (any-use-of-interest? args))
                     (live-vars (subtract-uses live-vars args))
                     (peeled-args (rename-uses args)))
                 (continue k live-vars
                           uses-of-interest? #f
                           (lambda (k)
                             (build-term
                               ($continue k src ($values peeled-args)))))))
              (($ $primcall name param args)
               ;; exp is effect-free or var of interest in args
               (let* ((fx (expression-effects exp))
                      (uses-of-interest? (any-use-of-interest? args))
                      (live-vars (subtract-uses live-vars args))
                      (peeled-args (rename-uses args)))
                 ;; If the primcall uses a value of interest,
                 ;; consider it for peeling even if it would cause a
                 ;; type check; perhaps the peeling causes the type
                 ;; check to go away.
                 (if (or (eqv? fx &no-effects)
                         (and uses-of-interest? (eqv? fx &type-check)))
                     (continue k live-vars
                               ;; Primcalls that use values of interest
                               ;; define values of interest.
                               uses-of-interest? #t
                               (lambda (k)
                                 (build-term
                                   ($continue k src
                                     ($primcall name param ,peeled-args)))))
                     (fail))))
              (_ (fail))))))))))

(define (peel-traces-in-function cps body use-counts)
  (intset-fold
   (lambda (label cps)
     (match (intmap-ref cps label)
       ;; Traces start with a fixnum? predicate.  We could expand this
       ;; in the future if we wanted to.
       (($ $kargs names vars ($ $branch kf kt src 'fixnum? #f (x)))
        (if (and (bailout? cps kf) #f)
            ;; Don't peel traces whose alternate is just a bailout.
            cps
            (with-cps cps
              (let$ kt (peel-trace kt x kf use-counts))
              ($ ((lambda (cps)
                    (if kt
                        (with-cps cps
                          (setk label
                                ($kargs names vars
                                  ($branch kf kt src 'fixnum? #f (x)))))
                        cps)))))))
       (_ cps)))
   body
   cps))

(define (devirtualize-integers cps)
  (let ((use-counts (compute-use-counts cps)))
    (with-fresh-name-state cps
      (intmap-fold
       (lambda (kfun body cps)
         (peel-traces-in-function cps body use-counts))
       (compute-reachable-functions cps)
       cps))))
