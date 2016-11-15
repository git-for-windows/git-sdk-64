;;; Guile Virtual Machine Assembly

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

(define-module (language assembly spec)
  #:use-module (system base language)
  #:use-module (language assembly compile-bytecode)
  #:use-module (language assembly decompile-bytecode)
  #:export (assembly))

(define-language assembly
  #:title	"Guile Virtual Machine Assembly Language"
  #:reader	(lambda (port env) (read port))
  #:printer	write
  #:parser      read ;; fixme: make a verifier?
  #:compilers   `((bytecode . ,compile-bytecode))
  #:decompilers `((bytecode . ,decompile-bytecode))
  #:for-humans? #f
  )
