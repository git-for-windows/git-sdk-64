;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 1999, 2001, 2004, 2006, 2017 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (ice-9 arrays)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:export (array-copy))

;; This is actually defined in boot-9.scm, apparently for backwards
;; compatibility.

;; (define (array-shape a)
;;   (map (lambda (ind) (if (number? ind) (list 0 (+ -1 ind)) ind))
;;        (array-dimensions a)))

; FIXME writes over the array twice if (array-type) is #t
(define (array-copy a)
  (let ((b (apply make-typed-array (array-type a) *unspecified* (array-shape a))))
    (array-copy! a b)
    b))


;; Printing arrays

;; The dimensions aren't printed out unless they cannot be deduced from
;; the content, which happens only when certain axes are empty. #:dims?
;; can be used to force this printing. An array with all the dimensions
;; printed out is still readable syntax, this can be useful for
;; truncated-print.

(define* (array-print-prefix a port #:key dims?)
  (put-char port #\#)
  (display (array-rank a) port)
  (let ((t (array-type a)))
    (unless (eq? #t t)
      (display t port)))
  (let ((ss (array-shape a)))
    (let loop ((s ss) (slos? #f) (szero? #f) (slens? dims?))
      (define lo caar)
      (define hi cadar)
      (if (null? s)
        (when (or slos? slens?)
          (pair-for-each (lambda (s)
                           (when slos?
                             (put-char port #\@)
                             (display (lo s) port))
                           (when slens?
                             (put-char port #\:)
                             (display (- (hi s) (lo s) -1) port)))
                         ss))
        (let ((zero-size? (zero? (- (hi s) (lo s) -1))))
          (loop (cdr s)
                (or slos? (not (zero? (lo s))))
                (or szero? zero-size?)
                (or slens? (and (not zero-size?) szero?))))))))
