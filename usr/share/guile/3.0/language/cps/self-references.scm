;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2021 Free Software Foundation, Inc.

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

  (define (rename-exp exp)
    (rewrite-exp exp
      ((or ($ $const) ($ $prim)) ,exp)
      (($ $call proc args)
       ($call (subst proc) ,(map subst args)))
      (($ $callk k proc args)
       ($callk k (and proc (subst proc)) ,(map subst args)))
      (($ $calli args callee)
       ($calli ,(map subst args) (subst callee)))
      (($ $primcall name param args)
       ($primcall name param ,(map subst args)))
      (($ $values args)
       ($values ,(map subst args)))))

  (define (rename-term term)
    (rewrite-term term
      (($ $continue k src exp)
       ($continue k src ,(rename-exp exp)))
      (($ $branch kf kt src op param args)
       ($branch kf kt src op param ,(map subst args)))
      (($ $switch kf kt* src arg)
       ($switch kf kt* src (subst arg)))
      (($ $prompt k kh src escape? tag)
       ($prompt k kh src escape? (subst tag)))
      (($ $throw src op param args)
       ($throw src op param ,(map subst args)))))

  (define (visit-label label cps)
    (match (intmap-ref cps label)
      (($ $kargs _ _ ($ $continue k src ($ $fun label)))
       (resolve-self-references cps label env))
      (($ $kargs _ _ ($ $continue k src
                        ($ $rec names vars (($ $fun labels) ...))))
       (fold (lambda (label var cps)
               (match (intmap-ref cps label)
                 (($ $kfun src meta self)
                  (resolve-self-references cps label
                                           (intmap-add env var self)))))
             cps labels vars))
      (($ $kargs names vars term)
       (intmap-replace! cps label
                        (build-cont ($kargs names vars ,(rename-term term)))))
      (_ cps)))

  (intset-fold visit-label (compute-function-body cps label) cps))
