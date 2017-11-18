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
;;; Primcalls that don't correspond to VM instructions are treated as if
;;; they are calls, and indeed the later reify-primitives pass turns
;;; them into calls.  Because no return arity checking is done for these
;;; primitives, if a later optimization pass simplifies the primcall to
;;; a VM operation, the tail of the simplification has to be a
;;; primcall to 'values.  Most of these primcalls can be elided, and
;;; that is the job of this pass.
;;;
;;; Code:

(define-module (language cps elide-values)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps intmap)
  #:export (elide-values))

(define (inline-values cps k src args)
  (match (intmap-ref cps k)
    (($ $ktail)
     (with-cps cps
       (build-term
         ($continue k src ($values args)))))
    (($ $kreceive ($ $arity req () rest () #f) kargs)
     (cond
      ((and (not rest) (= (length args) (length req)))
       (with-cps cps
         (build-term
           ($continue kargs src ($values args)))))
      ((and rest (>= (length args) (length req)))
       (let ()
         (define (build-rest cps k tail)
           (match tail
             (()
              (with-cps cps
                (build-term ($continue k src ($const '())))))
             ((v . tail)
              (with-cps cps
                (letv rest)
                (letk krest ($kargs ('rest) (rest)
                              ($continue k src ($primcall 'cons (v rest)))))
                ($ (build-rest krest tail))))))
         (with-cps cps
           (letv rest)
           (letk krest ($kargs ('rest) (rest)
                         ($continue kargs src
                           ($values ,(append (list-head args (length req))
                                             (list rest))))))
           ($ (build-rest krest (list-tail args (length req)))))))
      (else (with-cps cps #f))))))

(define (elide-values conts)
  (with-fresh-name-state conts
    (persistent-intmap
     (intmap-fold
      (lambda (label cont out)
        (match cont
          (($ $kargs names vars ($ $continue k src ($ $primcall 'values args)))
           (call-with-values (lambda () (inline-values out k src args))
             (lambda (out term)
               (if term
                   (let ((cont (build-cont ($kargs names vars ,term))))
                     (intmap-replace! out label cont))
                   out))))
          (_ out)))
      conts
      conts))))
