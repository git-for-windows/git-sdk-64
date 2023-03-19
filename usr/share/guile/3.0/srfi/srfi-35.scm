;;; srfi-35.scm --- Conditions                 -*- coding: utf-8 -*-

;; Copyright (C) 2007-2011, 2017, 2022 Free Software Foundation, Inc.
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
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:re-export ((make-exception-type . make-condition-type)
               (exception-type? . condition-type?)
               (exception? . condition?)
               (make-exception . make-compound-condition)
               (&exception . &condition)
               &message
               (exception-with-message? . message-condition?)
               (exception-message . condition-message)
               (&error . &serious)
               (error? . serious-condition?)
               (external-error? . error?))
  #:re-export-and-replace ((&external-error . &error))
  #:export (make-condition
            define-condition-type
            condition-has-type?
            condition-ref
            extract-condition
            condition))

(cond-expand-provide (current-module) '(srfi-35))

(define (make-condition type . field+value)
  "Return a new condition of type TYPE with fields initialized as specified
by FIELD+VALUE, a sequence of field names (symbols) and values."
  (unless (exception-type? type)
    (scm-error 'wrong-type-arg "make-condition" "Not a condition type: ~S"
               (list type) #f))
  (let* ((fields (record-type-fields type))
         (uninitialized (list 'uninitialized))
         (inits (make-vector (length fields) uninitialized)))
    (let lp ((args field+value))
      (match args
        (()
         (let lp ((i 0) (fields fields))
           (when (< i (vector-length inits))
             (when (eq? (vector-ref inits i) uninitialized)
               (error "field not specified" (car fields)))
             (lp (1+ i) (cdr fields))))
         (apply make-struct/simple type (vector->list inits)))
        (((and (? symbol?) field) value . args)
         (let lp ((i 0) (fields fields))
           (when (null? fields)
             (error "unknown field" field))
           (cond
            ((eq? field (car fields))
             (unless (eq? (vector-ref inits i) uninitialized)
               (error "duplicate initializer" field))
             (vector-set! inits i value))
            (else
             (lp (1+ i) (cdr fields)))))
         (lp args))
        (inits
         (scm-error 'wrong-type-arg "make-condition"
                    "Bad initializer list tail: ~S"
                    (list inits) #f))))))

(define (condition-has-type? c type)
  "Return true if condition C has type TYPE."
  (unless (exception-type? type)
    (scm-error 'wrong-type-arg "condition-has-type?" "Not a condition type: ~S"
               (list type) #f))
  (or-map (record-predicate type) (simple-exceptions c)))

;; Precondition: C is a simple condition.
(define (simple-condition-ref c field-name not-found)
  (match (list-index (record-type-fields (struct-vtable c)) field-name)
    (#f (not-found))
    (pos (struct-ref c pos))))

(define (condition-ref c field-name)
  "Return the value of the field named FIELD-NAME from condition C."
  (let lp ((conditions (simple-exceptions c)))
    (match conditions
      (() (error "invalid field name" field-name))
      ((c . conditions)
       (simple-condition-ref c field-name (lambda () (lp conditions)))))))

(define (make-condition-from-values type values)
  (apply make-struct/simple type values))

(define (extract-condition c type)
  "Return a condition of condition type TYPE with the field values specified
by C."
  (unless (exception-type? type)
    (scm-error 'wrong-type-arg "extract-condition" "Not a condition type: ~S"
               (list type) #f))
  (let ((pred (record-predicate type)))
    (or-map (lambda (x) (and (pred x) x)) (simple-exceptions c))))

(define-syntax define-condition-type
  (lambda (s)
    (syntax-case s ()
      ((_ type parent predicate (field accessor) ...)
       ;; The constructor is unused, but generate a new name for each
       ;; condition to avoid '-Wshadowed-toplevel' warnings when several
       ;; condition types are defined in the same compilation unit.
       (with-syntax ((unused-constructor
                      (datum->syntax
                       #'type
                       (symbol-append '#{ make-}# (syntax->datum #'type)))))
         #'(define-exception-type type parent
             unused-constructor predicate
             (field accessor) ...))))))

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
     (make-exception (condition-instantiation type () field ...)
                     ...))))
