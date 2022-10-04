;;; srfi-31.scm --- special form for recursive evaluation

;; 	Copyright (C) 2004, 2006, 2012 Free Software Foundation, Inc.
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

;;; Original author: Rob Browning <rlb@defaultvalue.org>

(define-module (srfi srfi-31)
  #:export (rec))

(cond-expand-provide (current-module) '(srfi-31))

(define-syntax rec
  (syntax-rules ()
    "Return the given object, defined in a lexical environment where
NAME is bound to itself."
    ((_ (name . formals) body ...)                ; procedure
     (letrec ((name (lambda formals body ...)))
       name))
    ((_ name expr)                                ; arbitrary object
     (letrec ((name expr))
       name))))
