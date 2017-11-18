;;; Syntax utilities

;;; Copyright (C) 2017 Free Software Foundation, Inc.

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

;;; Code:

(define-module (system syntax)
  #:use-module (system syntax internal)
  #:re-export (syntax?
               syntax-local-binding
               (%syntax-module . syntax-module)
               syntax-locally-bound-identifiers
               syntax-session-id))

;; Used by syntax.c.
(define (print-syntax obj port)
  ;; FIXME: Use syntax->datum instad of syntax-expression, when
  ;; syntax->datum can operate on new syntax objects.
  (format port "#<syntax ~s>" (syntax-expression obj)))
