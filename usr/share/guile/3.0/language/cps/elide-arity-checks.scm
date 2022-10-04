;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2021 Free Software Foundation, Inc.

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
;;; If we have a $callk to a $kfun that has a $kclause, in most cases we
;;; can skip arity checks because the caller knows what arity the callee
;;; is expecting.
;;;
;;; Code:

(define-module (language cps elide-arity-checks)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:export (elide-arity-checks))

(define (arity-matches? arity self proc args)
  (match arity
    (($ $arity req () #f () #f)
     (= (+ (length req) (if self 1 0))
        (+ (length args) (if proc 1 0))))
    (_ #f)))

(define (maybe-elide-arity-check cps kfun proc args)
  (match (intmap-ref cps kfun)
    (($ $kfun fsrc meta self ktail kentry)
     (match (and kentry (intmap-ref cps kentry))
       (($ $kclause (? (lambda (arity)
                         (arity-matches? arity self proc args))
                       arity)
           kbody #f)
        ;; This is a compatible $callk to a $kfun that checks its arity
        ;; and has no alternate; arrange to elide the check.
        (match (intmap-ref cps kbody)
          (($ $kargs fnames fvars term)
           (match term
             (($ $continue (? (lambda (k) (eq? k ktail))) _
                 ($ $callk kfun'
                    (? (lambda (proc') (eq? proc' self)))
                    (? (lambda (args) (equal? args fvars)))))
              ;; This function already trampolines out to another
              ;; function; forward this call there.  Could recurse but
              ;; we shouldn't need to, and we don't so as to avoid
              ;; divergence.
              (with-cps cps
                (build-exp
                  ($callk kfun' proc args))))
             (_
              ;; Define a new unchecked function containing the body of
              ;; this function.
              (let ((self' (and self (fresh-var)))
                    (fvars' (map (lambda (_) (fresh-var)) fvars)))
                (with-cps cps
                  ;; Entry of new kfun' is the $kargs kbody.
                  (letk kfun' ($kfun fsrc meta self ktail kbody))
                  (letk ktail' ($ktail))
                  (letk kbody' ($kargs fnames fvars'
                                 ($continue ktail' fsrc
                                   ($callk kfun' self' fvars'))))
                  (letk kentry' ($kclause ,arity kbody' #f))
                  (setk kfun ($kfun fsrc meta self' ktail' kentry'))
                  ;; Dispatch source $callk to new kfun'.
                  (build-exp
                    ($callk kfun' proc args)))))))))
       (_
        ;; Either this is already a $callk to a "raw" $kfun (one that
        ;; doesn't check its arity), in which case we're good; or a call
        ;; with possibly incompatible arity, or a call to a case-lambda,
        ;; in which case we punt for now.
        (with-cps cps
          (build-exp ($callk kfun proc args))))))))

;; This transformation removes references to arity-checking $kfun's, but
;; doesn't remove them, leaving that to renumbering or DCE to fix up.
(define (elide-arity-checks cps)
  (with-fresh-name-state cps
    (persistent-intmap
     (intmap-fold
      (lambda (label cont cps)
        (match cont
          (($ $kargs names vars
              ($ $continue k src ($ $callk kfun proc args)))
           (with-cps cps
             (let$ exp (maybe-elide-arity-check kfun proc args))
             (setk label ($kargs names vars
                           ($continue k src ,exp)))))
          (_ cps)))
      (persistent-intmap cps)
      (transient-intmap cps)))))
