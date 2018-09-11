;;; Guile Emacs Lisp

;; Copyright (C) 2011, 2017 Free Software Foundation, Inc.

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

(define-module (language elisp falias)
  #:export (falias?
            make-falias
            falias-function
            falias-object))

(define <falias-vtable>
  (make-struct/no-tail
   <applicable-struct-vtable>
   (make-struct-layout "pwpw")
   (lambda (object port)
     (format port "#<falias ~S>" (falias-object object)))))

(set-struct-vtable-name! <falias-vtable> 'falias)

(define (falias? object)
  (and (struct? object)
       (eq? (struct-vtable object) <falias-vtable>)))

(define (make-falias f object)
  (make-struct/no-tail <falias-vtable> f object))

(define (falias-function object)
  (struct-ref object 0))

(define (falias-object object)
  (struct-ref object 1))
