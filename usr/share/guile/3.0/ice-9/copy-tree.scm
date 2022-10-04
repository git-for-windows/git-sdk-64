;;; copy-tree
;;; Copyright (C) 1995-2010,2018,2020 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Copying pairs and vectors of data, while detecting cycles.
;;;
;;; Code:


(define-module (ice-9 copy-tree)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:replace (copy-tree))

;;; copy-tree creates deep copies of pairs and vectors, but not of any
;;; other data types.
;;; 
;;; To avoid infinite recursion due to cyclic structures, the
;;; hare-and-tortoise pattern is used to detect cycles.

(define (make-race obj)
  (define (make-race advance-tortoise? tortoise-path hare-tail)
    (define (advance! hare)
      (let ((tail (list hare)))
        (set-cdr! hare-tail tail)
        (set! hare-tail tail))
      (when (eq? hare (car tortoise-path))
        (scm-error 'wrong-type-arg "copy-tree"
                   "Expected non-circular data structure: ~S" (list hare) #f))
      (when advance-tortoise?
        (set! tortoise-path (cdr tortoise-path)))
      (set! advance-tortoise? (not advance-tortoise?)))
    (define (split!)
      (make-race advance-tortoise? tortoise-path hare-tail))
    (values advance! split!))
  (let ((path (cons obj '())))
    (make-race #f path path)))

(define (copy-tree obj)
  "Recursively copy the data tree that is bound to @var{obj}, and return a\n"
  "the new data structure.  @code{copy-tree} recurses down the\n"
  "contents of both pairs and vectors (since both cons cells and vector\n"
  "cells may point to arbitrary objects), and stops recursing when it hits\n"
  "any other object."
  (define (trace? x) (or (pair? x) (vector? x)))
  (define (visit obj advance! split!)
    (define (visit-head obj)
      (if (trace? obj)
          (let-values (((advance! split!) (split!)))
            (advance! obj)
            (visit obj advance! split!))
          obj))
    (define (visit-tail obj)
      (when (trace? obj) (advance! obj))
      (visit obj advance! split!))
    (cond
     ((pair? obj)
      (let* ((head (visit-head (car obj)))
             (tail (visit-tail (cdr obj))))
        (cons head tail)))
     ((vector? obj)
      (let* ((len (vector-length obj))
             (v (make-vector len)))
        (let lp ((i 0))
          (when (< i len)
            (vector-set! v i (visit-head (vector-ref obj i)))
            (lp (1+ i))))
        v))
     (else
      obj)))
  (let-values (((advance! split!) (make-race obj)))
    (visit obj advance! split!)))
