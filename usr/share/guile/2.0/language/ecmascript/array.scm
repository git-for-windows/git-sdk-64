;;; ECMAScript for Guile

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

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

(define-module (language ecmascript array)
  #:use-module (oop goops)
  #:use-module (language ecmascript base)
  #:use-module (language ecmascript function)
  #:export (*array-prototype* new-array))


(define-class <js-array-object> (<js-object>)
  (vector #:init-value #() #:accessor js-array-vector #:init-keyword #:vector))

(define (new-array . vals)
  (let ((o (make <js-array-object> #:class "Array"
                 #:prototype *array-prototype*)))
    (pput o 'length (length vals))
    (let ((vect (js-array-vector o)))
      (let lp ((i 0) (vals vals))
        (cond ((not (null? vals))
               (vector-set! vect i (car vals))
               (lp (1+ i) (cdr vals)))
              (else o))))))

(define *array-prototype* (make <js-object> #:class "Array"
                                #:value new-array
                                #:constructor new-array))

(hashq-set! *program-wrappers* new-array *array-prototype*)

(pput *array-prototype* 'prototype *array-prototype*)
(pput *array-prototype* 'constructor new-array)

(define-method (pget (o <js-array-object>) p)
  (cond ((and (integer? p) (exact? p) (>= p 0))
         (let ((v (js-array-vector o)))
           (if (< p (vector-length v))
               (vector-ref v p)
               (next-method))))
        ((or (and (symbol? p) (eq? p 'length))
             (and (string? p) (string=? p "length")))
         (vector-length (js-array-vector o)))
        (else (next-method))))

(define-method (pput (o <js-array-object>) p v)
  (cond ((and (integer? p) (exact? p) (>= 0 p))
         (let ((vect (js-array-vector o)))
           (if (< p (vector-length vect))
               (vector-set! vect p v)
               ;; Fixme: round up to powers of 2?
               (let ((new (make-vector (1+ p) 0)))
                 (vector-move-left! vect 0 (vector-length vect) new 0)
                 (set! (js-array-vector o) new)
                 (vector-set! new p v)))))
        ((or (and (symbol? p) (eq? p 'length))
             (and (string? p) (string=? p "length")))
         (let ((vect (js-array-vector o)))
           (let ((new (make-vector (->uint32 v) 0)))
             (vector-move-left! vect 0 (min (vector-length vect) (->uint32 v))
                                new 0)
             (set! (js-array-vector o) new))))
        (else (next-method))))

(define-js-method *array-prototype* (toString)
  (format #f "~A" (js-array-vector this)))

(define-js-method *array-prototype* (concat . rest)
  (let* ((len (apply + (->uint32 (pget this 'length))
                     (map (lambda (x) (->uint32 (pget x 'length)))
                          rest)))
         (rv (make-vector len 0)))
    (let lp ((objs (cons this rest)) (i 0))
      (cond ((null? objs) (make <js-array-object> #:class "Array"
                                #:prototype *array-prototype*
                                #:vector rv))
            ((is-a? (car objs) <js-array-object>)
             (let ((v (js-array-vector (car objs))))
               (vector-move-left! v 0 (vector-length v)
                                  rv i)
               (lp (cdr objs) (+ i (vector-length v)))))
            (else
             (error "generic array concats not yet implemented"))))))

(define-js-method *array-prototype* (join . separator)
  (let lp ((i (1- (->uint32 (pget this 'length)))) (l '()))
    (if (< i 0)
        (string-join l (if separator (->string (car separator)) ","))
        (lp (1+ i)
            (cons (->string (pget this i)) l)))))

(define-js-method *array-prototype* (pop)
  (let ((len (->uint32 (pget this 'length))))
    (if (zero? len)
        *undefined*
        (let ((ret (pget this (1- len))))
          (pput this 'length (1- len))
          ret))))

(define-js-method *array-prototype* (push . args)
  (let lp ((args args))
    (if (null? args)
        (->uint32 (pget this 'length))
        (begin (pput this (->uint32 (pget this 'length)) (car args))
               (lp (cdr args))))))
