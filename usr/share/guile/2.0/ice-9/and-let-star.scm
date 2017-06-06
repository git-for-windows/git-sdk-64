;;;; and-let-star.scm --- and-let* syntactic form (SRFI-2) for Guile
;;;;
;;;; Copyright (C) 1999, 2001, 2004, 2006, 2013,
;;;;   2015 Free Software Foundation, Inc.
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

      ;; Handle zero-clauses special-case.
      ((_ orig-form () . body)
       #'(begin #t . body))

      ;; Reduce clauses down to one regardless of body.
      ((_ orig-form ((var expr) rest . rest*) . body)
       (identifier? #'var)
       #'(let ((var expr))
           (and var (%and-let* orig-form (rest . rest*) . body))))
      ((_ orig-form ((expr) rest . rest*) . body)
       #'(and expr (%and-let* orig-form (rest . rest*) . body)))
      ((_ orig-form (var rest . rest*) . body)
       (identifier? #'var)
       #'(and var (%and-let* orig-form (rest . rest*) . body)))

      ;; Handle 1-clause cases without a body.
      ((_ orig-form ((var expr)))
       (identifier? #'var)
       #'expr)
      ((_ orig-form ((expr)))
       #'expr)
      ((_ orig-form (var))
       (identifier? #'var)
       #'var)

      ;; Handle 1-clause cases with a body.
      ((_ orig-form ((var expr)) . body)
       (identifier? #'var)
       #'(let ((var expr))
           (and var (begin . body))))
      ((_ orig-form ((expr)) . body)
       #'(and expr (begin . body)))
      ((_ orig-form (var) . body)
       (identifier? #'var)
       #'(and var (begin . body)))

      ;; Handle bad clauses.
      ((_ orig-form (bad-clause . rest) . body)
       (syntax-violation 'and-let* "Bad clause" #'orig-form #'bad-clause)))))

(define-syntax and-let*
  (lambda (form)
    (syntax-case form ()
      ((_ (c ...) body ...)
       #`(%and-let* #,form (c ...) body ...)))))

(cond-expand-provide (current-module) '(srfi-2))
