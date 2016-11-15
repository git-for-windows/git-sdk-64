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

(define-module (language glil spec)
  #:use-module (system base language)
  #:use-module (language glil)
  #:use-module (language glil compile-assembly)
  #:export (glil))

(define (write-glil exp . port)
  (apply write (unparse-glil exp) port))

(define (compile-asm x e opts)
  (values (compile-assembly x) e e))

(define-language glil
  #:title	"Guile Lowlevel Intermediate Language (GLIL)"
  #:reader	(lambda (port env) (read port))
  #:printer	write-glil
  #:parser      parse-glil
  #:compilers   `((assembly . ,compile-asm))
  #:for-humans? #f
  )
