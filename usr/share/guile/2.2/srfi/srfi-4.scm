;;; srfi-4.scm --- Homogeneous Numeric Vector Datatypes

;; Copyright (C) 2001, 2002, 2004, 2006, 2009, 2010,
;;   2012, 2014 Free Software Foundation, Inc.
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

;;; Author: Martin Grabmueller <mgrabmue@cs.tu-berlin.de>

;;; Commentary:

;; This module exports the homogeneous numeric vector procedures as
;; defined in SRFI-4.  They are fully documented in the Guile
;; Reference Manual.

;;; Code:

(define-module (srfi srfi-4)
  #:use-module (rnrs bytevectors)
  #:export (;; Unsigned 8-bit vectors.
            u8vector? make-u8vector u8vector u8vector-length u8vector-ref
            u8vector-set! u8vector->list list->u8vector

            ;; Signed 8-bit vectors.
            s8vector? make-s8vector s8vector s8vector-length s8vector-ref
            s8vector-set! s8vector->list list->s8vector

            ;; Unsigned 16-bit vectors.
            u16vector? make-u16vector u16vector u16vector-length u16vector-ref
            u16vector-set! u16vector->list list->u16vector

            ;; Signed 16-bit vectors.
            s16vector? make-s16vector s16vector s16vector-length s16vector-ref
            s16vector-set! s16vector->list list->s16vector

            ;; Unsigned 32-bit vectors.
            u32vector? make-u32vector u32vector u32vector-length u32vector-ref
            u32vector-set! u32vector->list list->u32vector

            ;; Signed 32-bit vectors.
            s32vector? make-s32vector s32vector s32vector-length s32vector-ref
            s32vector-set! s32vector->list list->s32vector

            ;; Unsigned 64-bit vectors.
            u64vector? make-u64vector u64vector u64vector-length u64vector-ref
            u64vector-set! u64vector->list list->u64vector

            ;; Signed 64-bit vectors.
            s64vector? make-s64vector s64vector s64vector-length s64vector-ref
            s64vector-set! s64vector->list list->s64vector

            ;; 32-bit floating point vectors.
            f32vector? make-f32vector f32vector f32vector-length f32vector-ref
            f32vector-set! f32vector->list list->f32vector

            ;; 64-bit floating point vectors.
            f64vector? make-f64vector f64vector f64vector-length f64vector-ref
            f64vector-set! f64vector->list list->f64vector))

(cond-expand-provide (current-module) '(srfi-4))

;; Need quasisyntax to do this effectively using syntax-case
(define-macro (define-bytevector-type tag infix size)
  `(begin
     (define (,(symbol-append tag 'vector?) obj)
       (and (bytevector? obj) (eq? (array-type obj) ',tag)))
     (define (,(symbol-append 'make- tag 'vector) len . fill)
       (apply make-srfi-4-vector ',tag len fill))
     (define (,(symbol-append tag 'vector-length) v)
       (let ((len (/ (bytevector-length v) ,size)))
         (if (integer? len)
             len
             (error "fractional length" v ',tag ,size))))
     (define (,(symbol-append tag 'vector) . elts)
       (,(symbol-append 'list-> tag 'vector) elts))
     (define (,(symbol-append 'list-> tag 'vector) elts)
       (let* ((len (length elts))
              (v (,(symbol-append 'make- tag 'vector) len)))
         (let lp ((i 0) (elts elts))
           (if (and (< i len) (pair? elts))
               (begin
                 (,(symbol-append tag 'vector-set!) v i (car elts))
                 (lp (1+ i) (cdr elts)))
               v))))
     (define (,(symbol-append tag 'vector->list) v)
       (let lp ((i (1- (,(symbol-append tag 'vector-length) v))) (elts '()))
         (if (< i 0)
             elts
             (lp (1- i) (cons (,(symbol-append tag 'vector-ref) v i) elts)))))
     (define (,(symbol-append tag 'vector-ref) v i)
       (,(symbol-append 'bytevector- infix '-ref) v (* i ,size)))
     (define (,(symbol-append tag 'vector-set!) v i x)
       (,(symbol-append 'bytevector- infix '-set!) v (* i ,size) x))
     (define (,(symbol-append tag 'vector-set!) v i x)
       (,(symbol-append 'bytevector- infix '-set!) v (* i ,size) x))))

(define-bytevector-type u8 u8 1)
(define-bytevector-type s8 s8 1)
(define-bytevector-type u16 u16-native 2)
(define-bytevector-type s16 s16-native 2)
(define-bytevector-type u32 u32-native 4)
(define-bytevector-type s32 s32-native 4)
(define-bytevector-type u64 u64-native 8)
(define-bytevector-type s64 s64-native 8)
(define-bytevector-type f32 ieee-single-native 4)
(define-bytevector-type f64 ieee-double-native 8)
