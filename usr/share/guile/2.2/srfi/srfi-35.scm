;;; srfi-35.scm --- Conditions                 -*- coding: utf-8 -*-

;; Copyright (C) 2007-2011, 2017 Free Software Foundation, Inc.
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

;;; Author: Ludovic Courtès <ludo@gnu.org>

;;; Commentary:

;; This is an implementation of SRFI-35, "Conditions".  Conditions are a
;; means to convey information about exceptional conditions between parts of
;; a program.

;;; Code:

(define-module (srfi srfi-35)
  #:use-module (srfi srfi-1)
  #:export (make-condition-type condition-type?
            make-condition condition? condition-has-type? condition-ref
            make-compound-condition extract-condition
            define-condition-type condition
            &condition
            &message message-condition? condition-message
            &serious serious-condition?
            &error error?))

(cond-expand-provide (current-module) '(srfi-35))


;;;
;;; Condition types.
;;;

(define %condition-type-vtable
  ;; The vtable of all condition types.
  ;;   vtable fields: vtable, self, printer
  ;;   user fields:   id, parent, all-field-names
  (let ((s (make-vtable (string-append standard-vtable-fields "prprpr")
                        (lambda (ct port)
                          (format port "#<condition-type ~a ~a>"
                                  (condition-type-id ct)
                                  (number->string (object-address ct)
                                                  16))))))
    (set-struct-vtable-name! s 'condition-type)
    s))

(define (%make-condition-type layout id parent all-fields)
  (let ((struct (make-struct/no-tail %condition-type-vtable
                                     (make-struct-layout layout) ;; layout
                                     print-condition             ;; printer
                                     id parent all-fields)))

    ;; Hack to associate STRUCT with a name, providing a better name for
    ;; GOOPS classes as returned by `class-of' et al.
    (set-struct-vtable-name! struct (cond ((symbol? id) id)
                                          ((string? id) (string->symbol id))
                                          (else         (string->symbol ""))))
    struct))

(define (condition-type? obj)
  "Return true if OBJ is a condition type."
  (and (struct? obj)
       (eq? (struct-vtable obj)
	    %condition-type-vtable)))

(define (condition-type-id ct)
  (and (condition-type? ct)
       (struct-ref ct (+ vtable-offset-user 0))))

(define (condition-type-parent ct)
  (and (condition-type? ct)
       (struct-ref ct (+ vtable-offset-user 1))))

(define (condition-type-all-fields ct)
  (and (condition-type? ct)
       (struct-ref ct (+ vtable-offset-user 2))))


(define (struct-layout-for-condition field-names)
  ;; Return a string denoting the layout required to hold the fields listed
  ;; in FIELD-NAMES.
  (let loop ((field-names field-names)
	     (layout      '("pr")))
    (if (null? field-names)
	(string-concatenate/shared layout)
	(loop (cdr field-names)
	      (cons "pr" layout)))))

(define (print-condition c port)
  ;; Print condition C to PORT in a way similar to how records print:
  ;; #<condition TYPE [FIELD: VALUE ...] ADDRESS>.
  (define (field-values)
    (let* ((type    (struct-vtable c))
           (strings (fold (lambda (field result)
                            (cons (format #f "~A: ~S" field
                                          (condition-ref c field))
                                  result))
                          '()
                          (condition-type-all-fields type))))
      (string-join (reverse strings) " ")))

  (format port "#<condition ~a [~a] ~a>"
          (condition-type-id (condition-type c))
          (field-values)
          (number->string (object-address c) 16)))

(define (make-condition-type id parent field-names)
  "Return a new condition type named ID, inheriting from PARENT, and with the
fields whose names are listed in FIELD-NAMES.  FIELD-NAMES must be a list of
symbols and must not contain names already used by PARENT or one of its
supertypes."
  (if (symbol? id)
      (if (condition-type? parent)
	  (let ((parent-fields (condition-type-all-fields parent)))
	    (if (and (every symbol? field-names)
		     (null? (lset-intersection eq?
					       field-names parent-fields)))
		(let* ((all-fields (append parent-fields field-names))
		       (layout     (struct-layout-for-condition all-fields)))
		  (%make-condition-type layout
                                        id parent all-fields))
		(error "invalid condition type field names"
		       field-names)))
	  (error "parent is not a condition type" parent))
      (error "condition type identifier is not a symbol" id)))

(define (make-compound-condition-type id parents)
  ;; Return a compound condition type made of the types listed in PARENTS.
  ;; All fields from PARENTS are kept, even same-named ones, since they are
  ;; needed by `extract-condition'.
  (cond ((null? parents)
         (error "`make-compound-condition-type' passed empty parent list"
                id))
        ((null? (cdr parents))
         (car parents))
        (else
         (let* ((all-fields (append-map condition-type-all-fields
                                        parents))
                (layout     (struct-layout-for-condition all-fields)))
           (%make-condition-type layout
                                 id
                                 parents         ;; list of parents!
                                 all-fields)))))


;;;
;;; Conditions.
;;;

(define (condition? c)
  "Return true if C is a condition."
  (and (struct? c)
       (condition-type? (struct-vtable c))))

(define (condition-type c)
  (and (struct? c)
       (let ((vtable (struct-vtable c)))
	 (if (condition-type? vtable)
	     vtable
	     #f))))

(define (condition-has-type? c type)
  "Return true if condition C has type TYPE."
  (if (and (condition? c) (condition-type? type))
      (let loop ((ct (condition-type c)))
        (or (eq? ct type)
            (and ct
                 (let ((parent (condition-type-parent ct)))
                   (if (list? parent)
                       (any loop parent) ;; compound condition
                       (loop (condition-type-parent ct)))))))
      (throw 'wrong-type-arg "condition-has-type?"
             "Wrong type argument")))

(define (condition-ref c field-name)
  "Return the value of the field named FIELD-NAME from condition C."
  (if (condition? c)
      (if (symbol? field-name)
	  (let* ((type   (condition-type c))
		 (fields (condition-type-all-fields type))
		 (index  (list-index (lambda (name)
				       (eq? name field-name))
				     fields)))
	    (if index
		(struct-ref c index)
		(error "invalid field name" field-name)))
	  (error "field name is not a symbol" field-name))
      (throw 'wrong-type-arg "condition-ref"
             "Wrong type argument: ~S" c)))

(define (make-condition-from-values type values)
  (apply make-struct/no-tail type values))

(define (make-condition type . field+value)
  "Return a new condition of type TYPE with fields initialized as specified
by FIELD+VALUE, a sequence of field names (symbols) and values."
  (if (condition-type? type)
      (let* ((all-fields (condition-type-all-fields type))
	     (inits      (fold-right (lambda (field inits)
				       (let ((v (memq field field+value)))
					 (if (pair? v)
					     (cons (cadr v) inits)
					     (error "field not specified"
						    field))))
				     '()
				     all-fields)))
	(make-condition-from-values type inits))
      (throw 'wrong-type-arg "make-condition"
             "Wrong type argument: ~S" type)))

(define (make-compound-condition . conditions)
  "Return a new compound condition composed of CONDITIONS."
  (let* ((types  (map condition-type conditions))
	 (ct     (make-compound-condition-type 'compound types))
	 (inits  (append-map (lambda (c)
			       (let ((ct (condition-type c)))
				 (map (lambda (f)
					(condition-ref c f))
				      (condition-type-all-fields ct))))
			     conditions)))
    (make-condition-from-values ct inits)))

(define (extract-condition c type)
  "Return a condition of condition type TYPE with the field values specified
by C."

  (define (first-field-index parents)
    ;; Return the index of the first field of TYPE within C.
    (let loop ((parents parents)
	       (index   0))
      (let ((parent (car parents)))
	(cond ((null? parents)
	       #f)
	      ((eq? parent type)
	       index)
	      ((pair? parent)
	       (or (loop parent index)
		   (loop (cdr parents)
			 (+ index
			    (apply + (map condition-type-all-fields
					  parent))))))
	      (else
	       (let ((shift (length (condition-type-all-fields parent))))
		 (loop (cdr parents)
		       (+ index shift))))))))

  (define (list-fields start-index field-names)
    ;; Return a list of the form `(FIELD-NAME VALUE...)'.
    (let loop ((index       start-index)
	       (field-names field-names)
	       (result      '()))
      (if (null? field-names)
	  (reverse! result)
	  (loop (+ 1 index)
		(cdr field-names)
		(cons* (struct-ref c index)
		       (car field-names)
		       result)))))

  (if (and (condition? c) (condition-type? type))
      (let* ((ct     (condition-type c))
             (parent (condition-type-parent ct)))
        (cond ((eq? type ct)
               c)
              ((pair? parent)
               ;; C is a compound condition.
               (let ((field-index (first-field-index parent)))
                 ;;(format #t "field-index: ~a ~a~%" field-index
                 ;;        (list-fields field-index
                 ;;                     (condition-type-all-fields type)))
                 (apply make-condition type
                        (list-fields field-index
                                     (condition-type-all-fields type)))))
              (else
               ;; C does not have type TYPE.
               #f)))
      (throw 'wrong-type-arg "extract-condition"
             "Wrong type argument")))


;;;
;;; Syntax.
;;;

(define-syntax-rule (define-condition-type name parent pred (field-name field-accessor) ...)
  (begin
    (define name
      (make-condition-type 'name parent '(field-name ...)))
    (define (pred c)
      (condition-has-type? c name))
    (define (field-accessor c)
      (condition-ref c 'field-name))
    ...))

(define-syntax-rule (compound-condition (type ...) (field ...))
  ;; Create a compound condition using `make-compound-condition-type'.
  (condition ((make-compound-condition-type '%compound `(,type ...))
              field ...)))

(define-syntax condition-instantiation
  ;; Build the `(make-condition type ...)' call.
  (syntax-rules ()
    ((_ type (out ...))
     (make-condition type out ...))
    ((_ type (out ...) (field-name field-value) rest ...)
     (condition-instantiation type (out ... 'field-name field-value) rest ...))))

(define-syntax condition
  (syntax-rules ()
    ((_ (type field ...))
     (condition-instantiation type () field ...))
    ((_ (type field ...) ...)
     (compound-condition (type ...) (field ... ...)))))


;;;
;;; Standard condition types.
;;;

(define &condition
  ;; The root condition type.
  (make-struct/no-tail %condition-type-vtable
                       (make-struct-layout "")
                       (lambda (c port)
                         (display "<&condition>"))
                       '&condition #f '() '()))

(define-condition-type &message &condition
  message-condition?
  (message condition-message))

(define-condition-type &serious &condition
  serious-condition?)

(define-condition-type &error &serious
  error?)

;;; srfi-35.scm ends here
