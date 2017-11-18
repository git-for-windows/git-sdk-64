;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.

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
;;; A pass that replaces free references to recursive functions with
;;; bound references.
;;;
;;; Code:

(define-module (language cps self-references)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:export (resolve-self-references))

(define* (resolve-self-references cps #:optional (label 0) (env empty-intmap))
  (define (subst var)
    (intmap-ref env var (lambda (var) var)))

  (define (rename-exp label cps names vars k src exp)
    (let ((exp (rewrite-exp exp
                 ((or ($ $const) ($ $prim)) ,exp)
                 (($ $call proc args)
                  ($call (subst proc) ,(map subst args)))
                 (($ $callk k proc args)
                  ($callk k (subst proc) ,(map subst args)))
                 (($ $primcall name args)
                  ($primcall name ,(map subst args)))
                 (($ $branch k ($ $values (arg)))
                  ($branch k ($values ((subst arg)))))
                 (($ $branch k ($ $primcall name args))
                  ($branch k ($primcall name ,(map subst args))))
                 (($ $values args)
                  ($values ,(map subst args)))
                 (($ $prompt escape? tag handler)
                  ($prompt escape? (subst tag) handler)))))
      (intmap-replace! cps label
                       (build-cont
                         ($kargs names vars ($continue k src ,exp))))))

  (define (visit-exp cps label names vars k src exp)
    (match exp
      (($ $fun label)
       (resolve-self-references cps label env))
      (($ $rec names vars (($ $fun labels) ...))
       (fold (lambda (label var cps)
               (match (intmap-ref cps label)
                 (($ $kfun src meta self)
                  (resolve-self-references cps label
                                           (intmap-add env var self)))))
             cps labels vars))
      (_ (rename-exp label cps names vars k src exp))))
  
  (intset-fold (lambda (label cps)
                 (match (intmap-ref cps label)
                   (($ $kargs names vars ($ $continue k src exp))
                    (visit-exp cps label names vars k src exp))
                   (_ cps)))
               (compute-function-body cps label)
               cps))
