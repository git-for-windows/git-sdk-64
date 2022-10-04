;;; Tree Intermediate Language

;; Copyright (C) 2009-2011,2013,2015,2020 Free Software Foundation, Inc.

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
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:use-module ((language tree-il analyze) #:select (make-analyzer))
  #:use-module ((language tree-il optimize) #:select (make-lowerer))
  #:export (tree-il))

(define (write-tree-il exp . port)
  (apply write (unparse-tree-il exp) port))

(define (join exps env)
  (match exps
    (() (make-void #f))
    ((x) x)
    ((x . rest)
     (make-seq #f x (join rest env)))
    (_ (error "what!" exps env))))

(define (choose-compiler target optimization-level opts)
  (define (load-compiler compiler)
    (module-ref (resolve-interface `(language tree-il ,compiler)) compiler))
  (if (match (memq #:cps? opts)
        ((_ cps? . _) cps?)
        (#f (<= 2 optimization-level)))
      (cons 'cps (load-compiler 'compile-cps))
      (cons 'bytecode (load-compiler 'compile-bytecode))))

(define-language tree-il
  #:title	"Tree Intermediate Language"
  #:reader	(lambda (port env) (read port))
  #:printer	write-tree-il
  #:parser      parse-tree-il
  #:joiner      join
  #:compiler-chooser choose-compiler
  #:analyzer    make-analyzer
  #:lowerer     make-lowerer
  #:for-humans? #f)
