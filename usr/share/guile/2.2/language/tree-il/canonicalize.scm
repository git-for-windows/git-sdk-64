;;; Tree-il canonicalizer

;; Copyright (C) 2011, 2012, 2013 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language tree-il canonicalize)
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (canonicalize))

(define (tree-il-any proc exp)
  (tree-il-fold (lambda (exp res)
                  (or res (proc exp)))
                (lambda (exp res) res)
                #f exp))

(define (canonicalize x)
  (post-order
   (lambda (x)
     (match x
       (($ <let> src () () () body)
        body)
       (($ <letrec> src _ () () () body)
        body)
       (($ <fix> src () () () body)
        body)
       (($ <lambda> src meta #f)
        ;; Give a body to case-lambda with no clauses.
        (make-lambda
         src meta
         (make-lambda-case
          #f '() #f #f #f '() '()
          (make-primcall
           #f
           'throw
           (list (make-const #f 'wrong-number-of-args)
                 (make-const #f #f)
                 (make-const #f "Wrong number of arguments")
                 (make-const #f '())
                 (make-const #f #f)))
          #f)))
       (($ <prompt> src escape-only? tag body handler)
        ;; The prompt handler should be a simple lambda, so that we
        ;; can inline it.
        (match handler
          (($ <lambda> _ _
              ($ <lambda-case> _ req #f rest #f () syms body #f))
           x)
          (else
           (let ((handler-sym (gensym))
                 (args-sym (gensym)))
             (make-let
              #f (list 'handler) (list handler-sym) (list handler)
              (make-prompt
               src escape-only? tag body
               (make-lambda
                #f '()
                (make-lambda-case
                 #f '() #f 'args #f '() (list args-sym)
                 (make-primcall
                  #f 'apply
                  (list (make-lexical-ref #f 'handler handler-sym)
                        (make-lexical-ref #f 'args args-sym)))
                 #f))))))))
       (_ x)))
   x))
