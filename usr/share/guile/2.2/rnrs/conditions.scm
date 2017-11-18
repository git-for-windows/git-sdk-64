;;; conditions.scm --- The R6RS conditions library

;;      Copyright (C) 2010 Free Software Foundation, Inc.
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
  (import (only (guile) and=> @@)
	  (rnrs base (6))
	  (rnrs lists (6))
	  (rnrs records procedural (6)))

  (define &compound-condition (make-record-type-descriptor 
			       '&compound-condition #f #f #f #f
			       '#((immutable components))))
  (define compound-condition? (record-predicate &compound-condition))
  
  (define make-compound-condition 
    (record-constructor (make-record-constructor-descriptor 
			 &compound-condition #f #f)))
  (define simple-conditions
    (let ((compound-ref (record-accessor &compound-condition 0)))
      (lambda (condition)
        (cond ((compound-condition? condition)
               (compound-ref condition))
              ((condition-internal? condition)
               (list condition))
              (else
               (assertion-violation 'simple-conditions
                                    "not a condition"
                                    condition))))))

  (define (condition? obj) 
    (or (compound-condition? obj) (condition-internal? obj)))

  (define condition
    (lambda conditions
      (define (flatten cond)
	(if (compound-condition? cond) (simple-conditions cond) (list cond)))
      (or (for-all condition? conditions)
	  (assertion-violation 'condition "non-condition argument" conditions))
      (if (or (null? conditions) (> (length conditions) 1))
	  (make-compound-condition (apply append (map flatten conditions)))
	  (car conditions))))
  
  (define-syntax define-condition-type
    (syntax-rules ()
      ((_ condition-type supertype constructor predicate
	  (field accessor) ...)
       (letrec-syntax
	   ((transform-fields
	     (syntax-rules ()
	       ((_ (f a) . rest)
		(cons '(immutable f a) (transform-fields . rest)))
	       ((_) '())))

	    (generate-accessors
	     (syntax-rules ()
	       ((_ counter (f a) . rest)
		(begin (define a 
                         (condition-accessor 
                          condition-type
                          (record-accessor condition-type counter)))
		       (generate-accessors (+ counter 1) . rest)))
	       ((_ counter) (begin)))))
	 (begin
	   (define condition-type 
	     (make-record-type-descriptor 
	      'condition-type supertype #f #f #f 
	      (list->vector (transform-fields (field accessor) ...))))
	   (define constructor
	     (record-constructor 
	      (make-record-constructor-descriptor condition-type #f #f)))
	   (define predicate (condition-predicate condition-type))
	   (generate-accessors 0 (field accessor) ...))))))

  (define &condition (@@ (rnrs records procedural) &condition))
  (define &condition-constructor-descriptor
    (make-record-constructor-descriptor &condition #f #f))
  (define condition-internal? (record-predicate &condition))

  (define (condition-predicate rtd)
    (let ((rtd-predicate (record-predicate rtd)))
      (lambda (obj)
	(cond ((compound-condition? obj) 
	       (exists rtd-predicate (simple-conditions obj)))
	      ((condition-internal? obj) (rtd-predicate obj))
	      (else #f)))))

  (define (condition-accessor rtd proc)
    (let ((rtd-predicate (record-predicate rtd)))
      (lambda (obj)
	(cond ((rtd-predicate obj) (proc obj))
	      ((compound-condition? obj) 
	       (and=> (find rtd-predicate (simple-conditions obj)) proc))
	      (else #f)))))

  (define-condition-type &message &condition 
    make-message-condition message-condition? 
    (message condition-message))

  (define-condition-type &warning &condition make-warning warning?)

  (define &serious (@@ (rnrs records procedural) &serious))
  (define make-serious-condition 
    (@@ (rnrs records procedural) make-serious-condition))
  (define serious-condition? (condition-predicate &serious))

  (define-condition-type &error &serious make-error error?)

  (define &violation (@@ (rnrs records procedural) &violation))
  (define make-violation (@@ (rnrs records procedural) make-violation))
  (define violation? (condition-predicate &violation))

  (define &assertion (@@ (rnrs records procedural) &assertion))
  (define make-assertion-violation 
    (@@ (rnrs records procedural) make-assertion-violation))
  (define assertion-violation? (condition-predicate &assertion))

  (define-condition-type &irritants &condition 
    make-irritants-condition irritants-condition?
    (irritants condition-irritants))

  (define-condition-type &who &condition
    make-who-condition who-condition?
    (who condition-who))

  (define-condition-type &non-continuable &violation
    make-non-continuable-violation
    non-continuable-violation?)

  (define-condition-type &implementation-restriction
    &violation
    make-implementation-restriction-violation
    implementation-restriction-violation?)

  (define-condition-type &lexical &violation
    make-lexical-violation lexical-violation?)

  (define-condition-type &syntax &violation
    make-syntax-violation syntax-violation?
    (form syntax-violation-form)
    (subform syntax-violation-subform))

  (define-condition-type &undefined &violation
    make-undefined-violation undefined-violation?)
  
)
