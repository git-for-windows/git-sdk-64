;;; Tree Intermediate Language

;; Copyright (C) 2009, 2010, 2011, 2013, 2015 Free Software Foundation, Inc.

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

(define-module (language tree-il spec)
  #:use-module (system base language)
  #:use-module (system base pmatch)
  #:use-module (language tree-il)
  #:use-module (language tree-il compile-cps)
  #:export (tree-il))

(define (write-tree-il exp . port)
  (apply write (unparse-tree-il exp) port))

(define (join exps env)
  (pmatch exps
    (() (make-void #f))
    ((,x) x)
    ((,x . ,rest)
     (make-seq #f x (join rest env)))
    (else (error "what!" exps env))))

(define-language tree-il
  #:title	"Tree Intermediate Language"
  #:reader	(lambda (port env) (read port))
  #:printer	write-tree-il
  #:parser      parse-tree-il
  #:joiner      join
  #:compilers   `((cps . ,compile-cps))
  #:for-humans? #f)
