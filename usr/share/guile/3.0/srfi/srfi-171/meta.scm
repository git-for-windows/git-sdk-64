;; 	Copyright (C) 2020 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(define-module (srfi srfi-171 meta)
  #:use-module (srfi srfi-9)
  #:use-module ((rnrs bytevectors) #:select (bytevector-length bytevector-u8-ref))
  #:export (reduced reduced?
            unreduce
            ensure-reduced
            preserving-reduced

            list-reduce
            vector-reduce
            string-reduce
            bytevector-u8-reduce
            port-reduce
            generator-reduce))


;; A reduced value is stops the transduction.
(define-record-type <reduced>
  (reduced val)
  reduced?
  (val unreduce))

(define (ensure-reduced x)
  "Ensure that @var{x} is reduced"
  (if (reduced? x)
      x
      (reduced x)))

;; helper function that wraps a reduced value twice since reducing functions (like list-reduce)
;; unwraps them. tconcatenate is a good example: it re-uses it's reducer on it's input using list-reduce.
;; If that reduction finishes early and returns a reduced value, list-reduce would "unreduce"
;; that value and try to continue the transducing process.
(define (preserving-reduced reducer)
  (lambda (a b)
    (let ((return (reducer a b)))
      (if (reduced? return)
          (reduced return)
          return))))

;; This is where the magic tofu is cooked
(define (list-reduce f identity lst)
  (if (null? lst)
      identity
      (let ((v (f identity (car lst))))
        (if (reduced? v)
            (unreduce v)
            (list-reduce f v (cdr lst))))))

(define (vector-reduce f identity vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0) (acc identity))
      (if (= i len)
          acc
          (let ((acc (f acc (vector-ref vec i))))
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))

(define (string-reduce f identity str)
  (let ((len (string-length str)))
    (let loop ((i 0) (acc identity))
      (if (= i len)
          acc
          (let ((acc (f acc (string-ref str i))))
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))

(define (bytevector-u8-reduce f identity vec)
  (let ((len (bytevector-length vec)))
    (let loop ((i 0) (acc identity))
      (if (= i len)
          acc
          (let ((acc (f acc (bytevector-u8-ref vec i))))
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))

(define (port-reduce f identity reader port)
  (let loop ((val (reader port)) (acc identity))
    (if (eof-object? val)
        acc
        (let ((acc (f acc val)))
          (if (reduced? acc)
              (unreduce acc)
              (loop (reader port) acc))))))

(define (generator-reduce f identity gen)
  (let loop ((val (gen)) (acc identity))
    (if (eof-object? val)
        acc
        (let ((acc (f acc val)))
          (if (reduced? acc)
              (unreduce acc)
              (loop (gen) acc))))))

