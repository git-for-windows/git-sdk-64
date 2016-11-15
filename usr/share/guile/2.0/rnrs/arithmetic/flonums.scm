;;; flonums.scm --- The R6RS flonums arithmetic library

;;      Copyright (C) 2010, 2011, 2013 Free Software Foundation, Inc.
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


(library (rnrs arithmetic flonums (6))
  (export flonum?
	  real->flonum

	  fl=? fl<? fl<=? fl>? fl>=?

	  flinteger? flzero? flpositive? flnegative? flodd? fleven? flfinite?
	  flinfinite? flnan?

	  flmax flmin

	  fl+ fl* fl- fl/

	  flabs

	  fldiv-and-mod
	  fldiv
	  flmod
	  fldiv0-and-mod0
	  fldiv0
	  flmod0

	  flnumerator
	  fldenominator

	  flfloor flceiling fltruncate flround

	  flexp fllog flsin flcos fltan flacos flasin flatan

	  flsqrt flexpt

	  &no-infinities
	  make-no-infinities-violation
	  no-infinities-violation?
	  
	  &no-nans
	  make-no-nans-violation
	  no-nans-violation?

	  fixnum->flonum)
  (import (ice-9 optargs)
	  (only (guile) inf?)
	  (rnrs arithmetic fixnums (6))
	  (rnrs base (6))
	  (rnrs control (6))
	  (rnrs conditions (6))
	  (rnrs exceptions (6))
	  (rnrs lists (6))
	  (rnrs r5rs (6)))

  (define (flonum? obj) (and (real? obj) (inexact? obj)))
  (define (assert-flonum . args)
    (or (for-all flonum? args) (raise (make-assertion-violation))))
  (define (assert-iflonum . args)
    (or (for-all (lambda (i) (and (flonum? i) (integer? i))) args)
	(raise (make-assertion-violation))))

  (define (ensure-flonum z)
    (cond ((real? z) z)
          ((zero? (imag-part z)) (real-part z))
          (else +nan.0)))

  (define (real->flonum x) 
    (or (real? x) (raise (make-assertion-violation)))
    (exact->inexact x))

  (define (fl=? . args) (apply assert-flonum args) (apply = args))
  (define (fl<? . args) (apply assert-flonum args) (apply < args))
  (define (fl<=? . args) (apply assert-flonum args) (apply <= args))
  (define (fl>? . args) (apply assert-flonum args) (apply > args))
  (define (fl>=? . args) (apply assert-flonum args) (apply >= args))

  (define (flinteger? fl) (assert-flonum fl) (integer? fl))
  (define (flzero? fl) (assert-flonum fl) (zero? fl))
  (define (flpositive? fl) (assert-flonum fl) (positive? fl))
  (define (flnegative? fl) (assert-flonum fl) (negative? fl))
  (define (flodd? ifl) (assert-iflonum ifl) (odd? ifl))
  (define (fleven? ifl) (assert-iflonum ifl) (even? ifl))
  (define (flfinite? fl) (assert-flonum fl) (not (or (inf? fl) (nan? fl))))
  (define (flinfinite? fl) (assert-flonum fl) (inf? fl))
  (define (flnan? fl) (assert-flonum fl) (nan? fl))

  (define (flmax fl1 . args)
    (let ((flargs (cons fl1 args)))
      (apply assert-flonum flargs)
      (apply max flargs)))

  (define (flmin fl1 . args)
    (let ((flargs (cons fl1 args)))
      (apply assert-flonum flargs)
      (apply min flargs)))

  (define (fl+ . args)
    (apply assert-flonum args)
    (if (null? args) 0.0 (apply + args)))

  (define (fl* . args)
    (apply assert-flonum args)
    (if (null? args) 1.0 (apply * args)))

  (define (fl- fl1 . args)
    (let ((flargs (cons fl1 args)))
      (apply assert-flonum flargs)
      (apply - flargs)))

  (define (fl/ fl1 . args)
    (let ((flargs (cons fl1 args)))
      (apply assert-flonum flargs)
      (apply / flargs)))

  (define (flabs fl) (assert-flonum fl) (abs fl))

  (define (fldiv-and-mod fl1 fl2)
    (assert-iflonum fl1 fl2)
    (div-and-mod fl1 fl2))

  (define (fldiv fl1 fl2)
    (assert-iflonum fl1 fl2)
    (div fl1 fl2))

  (define (flmod fl1 fl2)
    (assert-iflonum fl1 fl2)
    (mod fl1 fl2))

  (define (fldiv0-and-mod0 fl1 fl2)
    (assert-iflonum fl1 fl2)
    (div0-and-mod0 fl1 fl2))

  (define (fldiv0 fl1 fl2)
    (assert-iflonum fl1 fl2)
    (div0 fl1 fl2))

  (define (flmod0 fl1 fl2)
    (assert-iflonum fl1 fl2)
    (mod0 fl1 fl2))

  (define (flnumerator fl) (assert-flonum fl) (numerator fl))
  (define (fldenominator fl) (assert-flonum fl) (denominator fl))
  
  (define (flfloor fl) (assert-flonum fl) (floor fl))
  (define (flceiling fl) (assert-flonum fl) (ceiling fl))
  (define (fltruncate fl) (assert-flonum fl) (truncate fl))
  (define (flround fl) (assert-flonum fl) (round fl))

  (define (flexp fl) (assert-flonum fl) (exp fl))
  (define fllog
    (case-lambda
      ((fl)
       (assert-flonum fl)
       ;; add 0.0 to fl, to change -0.0 to 0.0,
       ;; so that (fllog -0.0) will be -inf.0, not -inf.0+pi*i.
       (ensure-flonum (log (+ fl 0.0))))
      ((fl fl2)
       (assert-flonum fl fl2)
       (ensure-flonum (/ (log (+ fl 0.0))
                         (log (+ fl2 0.0)))))))

  (define (flsin fl) (assert-flonum fl) (sin fl))
  (define (flcos fl) (assert-flonum fl) (cos fl))
  (define (fltan fl) (assert-flonum fl) (tan fl))
  (define (flasin fl) (assert-flonum fl) (ensure-flonum (asin fl)))
  (define (flacos fl) (assert-flonum fl) (ensure-flonum (acos fl)))
  (define flatan
    (case-lambda
      ((fl) (assert-flonum fl) (atan fl))
      ((fl fl2) (assert-flonum fl fl2) (atan fl fl2))))

  (define (flsqrt fl) (assert-flonum fl) (ensure-flonum (sqrt fl)))
  (define (flexpt fl1 fl2) (assert-flonum fl1 fl2) (ensure-flonum (expt fl1 fl2)))

  (define-condition-type &no-infinities
    &implementation-restriction
    make-no-infinities-violation
    no-infinities-violation?)

  (define-condition-type &no-nans
    &implementation-restriction
    make-no-nans-violation
    no-nans-violation?)

  (define (fixnum->flonum fx)
    (or (fixnum? fx) (raise (make-assertion-violation))) 
    (exact->inexact fx))
)
