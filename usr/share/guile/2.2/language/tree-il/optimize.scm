;;; Tree-il optimizer

;; Copyright (C) 2009, 2011, 2012, 2013, 2014, 2015 Free Software Foundation, Inc.

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

(define-module (language tree-il optimize)
  #:use-module (language tree-il)
  #:use-module (language tree-il primitives)
  #:use-module (language tree-il peval)
  #:use-module (language tree-il fix-letrec)
  #:use-module (language tree-il debug)
  #:use-module (ice-9 match)
  #:export (optimize
            tree-il-default-optimization-options))

(define (optimize x env opts)
  (let ((peval (match (memq #:partial-eval? opts)
                 ((#:partial-eval? #f _ ...)
                  ;; Disable partial evaluation.
                  (lambda (x e) x))
                 (_ peval))))
    (fix-letrec
     (verify-tree-il
      (peval (expand-primitives (resolve-primitives x env))
             env)))))

(define (tree-il-default-optimization-options)
  '(#:partial-eval? #t))
