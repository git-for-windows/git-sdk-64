;;; srfi-16.scm --- case-lambda

;; Copyright (C) 2001, 2002, 2006, 2009, 2014 Free Software Foundation, Inc.
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

;;; Author: Martin Grabmueller

;;; Commentary:

;; Implementation of SRFI-16.  `case-lambda' is a syntactic form
;; which permits writing functions acting different according to the
;; number of arguments passed.
;;
;; The syntax of the `case-lambda' form is defined in the following
;; EBNF grammar.
;;
;; <case-lambda>
;;    --> (case-lambda <case-lambda-clause>)
;; <case-lambda-clause>
;;    --> (<signature> <definition-or-command>*)
;; <signature>
;;    --> (<identifier>*)
;;      | (<identifier>* . <identifier>)
;;      | <identifier>
;;
;; The value returned by a `case-lambda' form is a procedure which
;; matches the number of actual arguments against the signatures in
;; the various clauses, in order.  The first matching clause is
;; selected, the corresponding values from the actual parameter list
;; are bound to the variable names in the clauses and the body of the
;; clause is evaluated.

;;; Code:

(define-module (srfi srfi-16)
  #:re-export (case-lambda))

;; Case-lambda is now provided by core psyntax.
