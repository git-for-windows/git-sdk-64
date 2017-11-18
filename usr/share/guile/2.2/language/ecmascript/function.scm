;;; ECMAScript for Guile

;; Copyright (C) 2009 Free Software Foundation, Inc.

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

(define-module (language ecmascript function)
  #:use-module (oop goops)
  #:use-module (language ecmascript base)
  #:export (*function-prototype* *program-wrappers*))


(define-class <js-program-wrapper> (<js-object>))

(define *program-wrappers* (make-doubly-weak-hash-table 31))

(define *function-prototype* (make <js-object> #:class "Function"
                                   #:value (lambda args *undefined*)))

(define-js-method *function-prototype* (toString)
  (format #f "~A" (js-value this)))

(define-js-method *function-prototype* (apply this-arg array)
  (cond ((or (null? array) (eq? array *undefined*))
         (call/this this-arg (js-value this)))
        ((is-a? array <js-array-object>)
         (call/this this-arg
                    (lambda ()
                      (apply (js-value this)
                             (vector->list (js-array-vector array))))))
        (else
         (throw 'TypeError 'apply array))))

(define-js-method *function-prototype* (call this-arg . args)
  (call/this this-arg
             (lambda ()
               (apply (js-value this) args))))

(define-method (pget (o <applicable>) p)
  (let ((wrapper (hashq-ref *program-wrappers* o)))
    (if wrapper
        (pget wrapper p)
        (pget *function-prototype* p))))

(define-method (pput (o <applicable>) p v)
  (let ((wrapper (hashq-ref *program-wrappers* o)))
    (if wrapper
        (pput wrapper p v)
        (let ((wrapper (make <js-program-wrapper> #:value o #:class "Function"
                             #:prototype *function-prototype*)))
          (hashq-set! *program-wrappers* o wrapper)
          (pput wrapper p v)))))

(define-method (js-prototype (o <applicable>))
  (let ((wrapper (hashq-ref *program-wrappers* o)))
    (if wrapper
        (js-prototype wrapper)
        #f)))

(define-method (js-constructor (o <applicable>))
  (let ((wrapper (hashq-ref *program-wrappers* o)))
    (if wrapper
        (js-constructor wrapper)
        #f)))
