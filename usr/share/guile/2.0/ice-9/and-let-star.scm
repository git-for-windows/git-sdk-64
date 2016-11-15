;;;; and-let-star.scm --- and-let* syntactic form (SRFI-2) for Guile
;;;;
;;;; Copyright (C) 1999, 2001, 2004, 2006, 2013 Free Software Foundation, Inc.
;;;; 
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

(define-module (ice-9 and-let-star)
  :export-syntax (and-let*))

(define-syntax %and-let*
  (lambda (form)
    (syntax-case form ()
      ((_ orig-form ())
       #'#t)
      ((_ orig-form () body bodies ...)
       #'(begin body bodies ...))
      ((_ orig-form ((var exp) c ...) body ...)
       (identifier? #'var)
       #'(let ((var exp))
           (and var (%and-let* orig-form (c ...) body ...))))
      ((_ orig-form ((exp) c ...) body ...)
       #'(and exp (%and-let* orig-form (c ...) body ...)))
      ((_ orig-form (var c ...) body ...)
       (identifier? #'var)
       #'(and var (%and-let* orig-form (c ...) body ...)))
      ((_ orig-form (bad-clause c ...) body ...)
       (syntax-violation 'and-let* "Bad clause" #'orig-form #'bad-clause)))))

(define-syntax and-let*
  (lambda (form)
    (syntax-case form ()
      ((_ (c ...) body ...)
       #`(%and-let* #,form (c ...) body ...)))))

(cond-expand-provide (current-module) '(srfi-2))
