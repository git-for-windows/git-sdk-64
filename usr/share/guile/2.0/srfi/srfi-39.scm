;;; srfi-39.scm --- Parameter objects

;; 	Copyright (C) 2004, 2005, 2006, 2008, 2011 Free Software Foundation, Inc.
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

;;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;;; Date: 2004-05-05

;;; Commentary:

;; This is an implementation of SRFI-39 (Parameter objects).
;;
;; The implementation is based on Guile's fluid objects, and is, therefore,
;; thread-safe (parameters are thread-local).
;;
;; In addition to the forms defined in SRFI-39 (`make-parameter',
;; `parameterize'), a new procedure `with-parameters*' is provided.
;; This procedures is analogous to `with-fluids*' but taking as first
;; argument a list of parameter objects instead of a list of fluids.
;;

;;; Code:

(define-module (srfi srfi-39)
  ;; helper procedure not in srfi-39.
  #:export (with-parameters*)
  #:re-export (make-parameter
               parameterize
               current-input-port current-output-port current-error-port))

(cond-expand-provide (current-module) '(srfi-39))

(define (with-parameters* params values thunk)
  (let more ((params params)
	     (values values)
	     (fluids '())     ;; fluids from each of PARAMS
	     (convs  '()))    ;; VALUES with conversion proc applied
    (if (null? params)
	(with-fluids* fluids convs thunk)
        (more (cdr params) (cdr values)
              (cons (parameter-fluid (car params)) fluids)
              (cons ((parameter-converter (car params)) (car values)) convs)))))
