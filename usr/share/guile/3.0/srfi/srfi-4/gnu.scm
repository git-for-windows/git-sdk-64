;;; Extensions to SRFI-4

;; Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.
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

;;; Commentary:

;; Extensions to SRFI-4. Fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-4 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:export (;; Complex numbers with 32- and 64-bit components.
            c32vector? make-c32vector c32vector c32vector-length c32vector-ref
            c32vector-set! c32vector->list list->c32vector

            c64vector? make-c64vector c64vector c64vector-length c64vector-ref
            c64vector-set! c64vector->list list->c64vector

            ;; Somewhat polymorphic conversions.
            any->u8vector any->s8vector any->u16vector any->s16vector
            any->u32vector any->s32vector any->u64vector any->s64vector
            any->f32vector any->f64vector any->c32vector any->c64vector

            ;; copy range
            u8vector-copy s8vector-copy u16vector-copy s16vector-copy
            u32vector-copy s32vector-copy u64vector-copy s64vector-copy
            f32vector-copy f64vector-copy c32vector-copy c64vector-copy

            ;; copy range with destination
            u8vector-copy! s8vector-copy! u16vector-copy! s16vector-copy!
            u32vector-copy! s32vector-copy! u64vector-copy! s64vector-copy!
            f32vector-copy! f64vector-copy! c32vector-copy! c64vector-copy!

            ;; from libguile
            srfi-4-vector-type-size make-srfi-4-vector))


(define make-srfi-4-vector (@@ (srfi srfi-4) make-srfi-4-vector))
(define srfi-4-vector-type-size (@@ (srfi srfi-4) srfi-4-vector-type-size))

(define (bytevector-c32-native-ref v i)
  (make-rectangular (bytevector-ieee-single-native-ref v i)
                    (bytevector-ieee-single-native-ref v (+ i 4))))
(define (bytevector-c32-native-set! v i x)
  (bytevector-ieee-single-native-set! v i (real-part x))
  (bytevector-ieee-single-native-set! v (+ i 4) (imag-part x)))
(define (bytevector-c64-native-ref v i)
  (make-rectangular (bytevector-ieee-double-native-ref v i)
                    (bytevector-ieee-double-native-ref v (+ i 8))))
(define (bytevector-c64-native-set! v i x)
  (bytevector-ieee-double-native-set! v i (real-part x))
  (bytevector-ieee-double-native-set! v (+ i 8) (imag-part x)))

((@@ (srfi srfi-4) define-bytevector-type) c32 c32-native 8)
((@@ (srfi srfi-4) define-bytevector-type) c64 c64-native 16)

(define sizeof-u8 1)
(define sizeof-u16 2)
(define sizeof-u32 4)
(define sizeof-u64 8)

(define sizeof-s8 1)
(define sizeof-s16 2)
(define sizeof-s32 4)
(define sizeof-s64 8)

(define sizeof-f32 4)
(define sizeof-f64 8)

(define sizeof-c32 8)
(define sizeof-c64 16)

(define-macro (type-check v tag)
  `(unless (,(symbol-append tag 'vector?) ,v)
     (scm-error 'wrong-type-arg #f
                ,(format #f "expecting ~a, got ~~A" (symbol-append tag 'vector))
                (list ,v) (list ,v))))

(define-macro (define-funs . tags)
  `(begin
     ,@(append-map
        (lambda (tag)
          (list
; any->xxvector
           `(define (,(symbol-append 'any-> tag 'vector) obj)
              (cond ((,(symbol-append tag 'vector?) obj) obj)
                    ((pair? obj) (,(symbol-append 'list-> tag 'vector) obj))
                    ((and (array? obj) (eqv? 1 (array-rank obj)))
                     (let* ((len (array-length obj))
                            (v (,(symbol-append 'make- tag 'vector) len)))
                       (let lp ((i 0))
                         (if (< i len)
                           (begin
                             (,(symbol-append tag 'vector-set!)
                              v i (array-ref obj i))
                             (lp (1+ i)))
                           v))))
                    (else (scm-error 'wrong-type-arg #f "" '() (list obj)))))
           
; xxvector-copy!
           `(define* (,(symbol-append tag 'vector '-copy!) dst at src #:optional (start 0) end)

              ,(format #f
                  "Copy a block of elements from @var{src} to @var{dst}, both of
                  which must be ~as, starting in @var{dst} at @var{at} and
                  starting in @var{src} at @var{start} and ending at @var{end}.  It
                  is an error for @var{dst} to have a length less than @var{at} +
                  (@var{end} - @var{start}). @var{at} and @var{start} default to 0
                  and @var{end} defaults to the length of @var{src}.

                  If source and destination overlap, copying takes place as if the
                  source is first copied into a temporary vector and then into the
                  destination."
                (symbol-append tag 'vector))

              (type-check src ,tag)
              (type-check dst ,tag)
              (let ((sof ,(symbol-append 'sizeof- tag))
                    (len (- (or end (,(symbol-append tag 'vector-length) src)) start)))
                (bytevector-copy! src (* start sof) dst (* at sof) (* len sof))))
           
; xxvector-copy
           `(define* (,(symbol-append tag 'vector '-copy) src #:optional (start 0) end)

              ,(format #f
                 "Returns a freshly allocated ~a containing the elements of ~a
                  @var{src} between @var{start} and @var{end}. @var{start} defaults
                  to 0 and @var{end} defaults to the length of @var{src}."
                 (symbol-append tag 'vector)
                 (symbol-append tag 'vector))

              (type-check src ,tag)
              (let* ((sof ,(symbol-append 'sizeof- tag))
                     (len (- (or end (,(symbol-append tag 'vector-length) src)) start))
                     (dst (,(symbol-append 'make- tag 'vector) len)))
                (bytevector-copy! src (* start sof) dst 0 (* len sof))
                dst))
           ))
        tags)))

(define-funs u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 c32 c64)
