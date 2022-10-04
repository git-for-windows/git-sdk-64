;;;; Copyright (C) 2017, 2020 Free Software Foundation, Inc.
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
;;;;

(define-module (ice-9 deprecated)
  #:use-module (ice-9 copy-tree)
  #:export ((copy-tree* . copy-tree)))

(define-syntax-rule (define-deprecated name message exp)
  (begin
    (define-syntax rule
      (identifier-syntax
       (begin
         (issue-deprecation-warning message)
         exp)))
    (export rule)))

(define %allow-legacy-syntax-objects? (make-parameter #f))
(define-deprecated allow-legacy-syntax-objects?
  "allow-legacy-syntax-objects? is deprecated and has no effect.  Guile
3.0 has no legacy syntax objects."
  %allow-legacy-syntax-objects?)

(define (copy-tree* x)
  (issue-deprecation-warning
   "copy-tree in the default environment is deprecated.  Import it
from (ice-9 copy-tree) instead.")
  (copy-tree x))
