;;; Guile Lowlevel Intermediate Language

;; Copyright (C) 2001, 2009, 2010, 2013 Free Software Foundation, Inc.

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

(define-module (language bytecode spec)
  #:use-module (system base language)
  #:use-module (system vm objcode)
  #:export (bytecode))

(define (compile-objcode x e opts)
  (values (bytecode->objcode x) e e))

(define (decompile-objcode x e opts)
  (values (objcode->bytecode x) e))

(define-language bytecode
  #:title	"Guile Bytecode Vectors"
  #:reader	(lambda (port env) (read port))
  #:printer	write
  #:compilers   `((objcode . ,compile-objcode))
  #:decompilers `((objcode . ,decompile-objcode))
  #:for-humans? #f
  )
