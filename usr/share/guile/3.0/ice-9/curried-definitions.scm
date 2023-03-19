;;; Copyright (C) 2010, 2013  Free Software Foundation, Inc.
;;;
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

(define-module (ice-9 curried-definitions)
  #:replace ((cdefine . define)
             (cdefine* . define*)
             define-public
             define*-public))

(define-syntax make-currying-define
  (syntax-rules ::: ()
    ((_ currying-name lambda-name)
     (define-syntax currying-name
       (lambda (St-Ax)
         (syntax-case St-Ax ()
           ((_ ((head2 . rest2) . rest) docstring body body* ...)
            (string? (syntax->datum #'docstring))
            ;; Keep moving docstring to outermost lambda.
            #'(currying-name (head2 . rest2)
                docstring
                (lambda-name rest body body* ...)))
           ((_ (head . rest) body body* ...)
            #'(currying-name head
                (lambda-name rest body body* ...)))
           ((_ name val)
            #'(define name val))))))))

(make-currying-define cdefine lambda)
(make-currying-define cdefine* lambda*)

(define-syntax make-currying-define-public
  (syntax-rules ::: ()
    ((_ public-name define-name)
     (define-syntax public-name
       (lambda (St-Ax)
         (syntax-case St-Ax ()
           ((_ binding body body* ...)
            #`(begin
                (define-name binding body body* ...)
                (export #,(let find-name ((form #'binding))
                            (syntax-case form ()
                              ((head . tail)
                               (find-name #'head))
                              (name
                               #'name))))))))))))

(make-currying-define-public define-public cdefine)
(make-currying-define-public define*-public cdefine*)
