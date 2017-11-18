;;;; streams.scm --- general lazy streams
;;;; -*- Scheme -*-

;;;; Copyright (C) 1999, 2001, 2004, 2006, 2015 Free Software Foundation, Inc.
;;;; 
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

;; the basic stream operations are inspired by
;; (i.e. ripped off) Scheme48's `stream' package,
;; modulo stream-empty? -> stream-null? renaming.

(define-module (ice-9 streams)
  #:use-module ((srfi srfi-41) #:prefix srfi-41:)
  #:export (make-stream
            vector->stream port->stream
            stream->reversed-list
            stream->list&length stream->reversed-list&length
            stream->vector
            stream-fold)
  #:re-export ((srfi-41:stream-car . stream-car)
               (srfi-41:stream-cdr . stream-cdr)
               (srfi-41:stream-null? . stream-null?)
               (srfi-41:list->stream . list->stream)
               (srfi-41:stream->list . stream->list)
               (srfi-41:stream-for-each . stream-for-each)
               (srfi-41:stream-map . stream-map)))

;; Use:
;;
;; (make-stream producer initial-state)
;;  - PRODUCER is a function of one argument, the current state.
;;    it should return either a pair or an atom (i.e. anything that
;;    is not a pair).  if PRODUCER returns a pair, then the car of the pair
;;    is the stream's head value, and the cdr is the state to be fed
;;    to PRODUCER later.  if PRODUCER returns an atom, then the stream is
;;    considered depleted.
;;
;; (stream-car stream)
;; (stream-cdr stream)
;; (stream-null? stream)
;;  - yes.
;;
;; (list->stream list)
;; (vector->stream vector)
;;  - make a stream with the same contents as LIST/VECTOR.
;;
;; (port->stream port read)
;;  - makes a stream of values which are obtained by READing from PORT.
;;
;; (stream->list stream)
;;  - returns a list with the same contents as STREAM.
;;
;; (stream->reversed-list stream)
;;  - as above, except the contents are in reversed order.
;;
;; (stream->list&length stream)
;; (stream->reversed-list&length stream)
;;  - multiple-valued versions of the above two, the second value is the
;;    length of the resulting list (so you get it for free).
;;
;; (stream->vector stream)
;;  - yes.
;;
;; (stream-fold proc init stream0 ...)
;;  - PROC must take (+ 1 <number-of-stream-arguments>) arguments, like this:
;;    (PROC car0 ... init).  *NOTE*: the INIT argument is last, not first.
;;    I don't have any preference either way, but it's consistent with
;;    `fold[lr]' procedures from SRFI-1.  PROC is applied to successive
;;    elements of the given STREAM(s) and to the value of the previous
;;    invocation (INIT on the first invocation).  the last result from PROC
;;    is returned.
;;
;; (stream-for-each proc stream0 ...)
;;  - like `for-each' we all know and love.
;;
;; (stream-map proc stream0 ...)
;;  - like `map', except returns a stream of results, and not a list.

;; Code:

(define (make-stream m state)
  (srfi-41:stream-let recur ((state state))
    (let ((state (m state)))
      (if (pair? state)
          (srfi-41:stream-cons (car state) (recur (cdr state)))
          srfi-41:stream-null))))

(define (vector->stream v)
  (make-stream
   (let ((len (vector-length v)))
     (lambda (i)
       (or (= i len)
           (cons (vector-ref v i) (+ 1 i)))))
   0))

(define (stream->reversed-list&length stream)
  (let loop ((s stream) (acc '()) (len 0))
    (if (srfi-41:stream-null? s)
        (values acc len)
        (loop (srfi-41:stream-cdr s)
          (cons (srfi-41:stream-car s) acc) (+ 1 len)))))

(define (stream->reversed-list stream)
  (call-with-values
   (lambda () (stream->reversed-list&length stream))
   (lambda (l len) l)))

(define (stream->list&length stream)
  (call-with-values
   (lambda () (stream->reversed-list&length stream))
   (lambda (l len) (values (reverse! l) len))))

(define (stream->vector stream)
  (call-with-values
   (lambda () (stream->reversed-list&length stream))
   (lambda (l len)
     (let ((v (make-vector len)))
       (let loop ((i 0) (l l))
         (if (not (null? l))
             (begin
               (vector-set! v (- len i 1) (car l))
               (loop (+ 1 i) (cdr l)))))
       v))))

(define (stream-fold f init stream . rest)
  (if (null? rest) ;fast path
      (stream-fold-one f init stream)
      (stream-fold-many f init (cons stream rest))))

(define (stream-fold-one f r stream)
  (if (srfi-41:stream-null? stream)
      r
      (stream-fold-one f
                       (f (srfi-41:stream-car stream) r)
                       (srfi-41:stream-cdr stream))))

(define (stream-fold-many f r streams)
  (if (or-map srfi-41:stream-null? streams)
      r
      (stream-fold-many f
                        (apply f (let recur ((cars
                                              (map srfi-41:stream-car streams)))
                                   (if (null? cars)
                                       (list r)
                                       (cons (car cars)
                                             (recur (cdr cars))))))
                        (map srfi-41:stream-cdr streams))))

(define (port->stream port read)
  (make-stream (lambda (p)
                 (let ((o (read p)))
                   (or (eof-object? o)
                       (cons o p))))
               port))

;;; streams.scm ends here
