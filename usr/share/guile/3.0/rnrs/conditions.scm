;;; conditions.scm --- The R6RS conditions library

;;      Copyright (C) 2010, 2020 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(library (rnrs conditions (6))
  (export &condition
	  condition
	  simple-conditions
	  condition?
	  condition-predicate
	  condition-accessor
	  define-condition-type
	  
	  &message
	  make-message-condition
	  message-condition?
	  condition-message

	  &warning
	  make-warning
	  warning?

	  &serious
	  make-serious-condition
	  serious-condition?

	  &error
	  make-error
	  error?

	  &violation
	  make-violation
	  violation?

	  &assertion
	  make-assertion-violation
	  assertion-violation?

	  &irritants
	  make-irritants-condition
	  irritants-condition?
	  condition-irritants

	  &who
	  make-who-condition
	  who-condition?
	  condition-who

	  &non-continuable
	  make-non-continuable-violation
	  non-continuable-violation?

	  &implementation-restriction
	  make-implementation-restriction-violation
	  implementation-restriction-violation?

	  &lexical
	  make-lexical-violation
	  lexical-violation?

	  &syntax
	  make-syntax-violation
	  syntax-violation?
	  syntax-violation-form
	  syntax-violation-subform

	  &undefined
	  make-undefined-violation
	  undefined-violation?)
  (import (rename (ice-9 exceptions)
                  (&exception &condition)
                  (make-exception condition)
                  (simple-exceptions simple-conditions)
                  (exception? condition?)
                  (exception-predicate condition-predicate)
                  (exception-accessor condition-accessor)
                  (define-exception-type define-condition-type)

                  (make-exception-with-message make-message-condition)
                  (exception-with-message? message-condition?)
                  (exception-message condition-message)

                  (&error &serious)
                  (make-error make-serious-condition)
                  (error? serious-condition?)

                  (&external-error &error)
                  (make-external-error make-error)
                  (external-error? error?)

                  (&programming-error &violation)
                  (make-programming-error make-violation)
                  (programming-error? violation?)

                  (&assertion-failure &assertion)
                  (make-assertion-failure make-assertion-violation)
                  (assertion-failure? assertion-violation?)

                  (make-exception-with-irritants make-irritants-condition)
                  (exception-with-irritants? irritants-condition?)
                  (exception-irritants condition-irritants)

                  (make-exception-with-origin make-who-condition)
                  (exception-with-origin? who-condition?)
                  (exception-origin condition-who)

                  (make-non-continuable-error make-non-continuable-violation)
                  (non-continuable-error? non-continuable-violation?)

                  (make-implementation-restriction-error
                   make-implementation-restriction-violation)
                  (implementation-restriction-error?
                   implementation-restriction-violation?)

                  (make-lexical-error make-lexical-violation)
                  (lexical-error? lexical-violation?)

                  (make-syntax-error make-syntax-violation)
                  (syntax-error? syntax-violation?)
                  (syntax-error-form syntax-violation-form)
                  (syntax-error-subform syntax-violation-subform)

                  (&undefined-variable &undefined)
                  (make-undefined-variable-error make-undefined-violation)
                  (undefined-variable-error? undefined-violation?))))
