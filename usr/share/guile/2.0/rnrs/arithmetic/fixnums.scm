;;; fixnums.scm --- The R6RS fixnums arithmetic library

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


(library (rnrs arithmetic fixnums (6))
  (export fixnum?
	  
	  fixnum-width
	  least-fixnum
	  greatest-fixnum

	  fx=?
	  fx>?
	  fx<?
	  fx>=?
	  fx<=?

	  fxzero?
	  fxpositive?
	  fxnegative?
	  fxodd?
	  fxeven?

	  fxmax
	  fxmin
	  
	  fx+
	  fx*
	  fx-

	  fxdiv-and-mod
	  fxdiv
	  fxmod
	  fxdiv0-and-mod0
	  fxdiv0
	  fxmod0

	  fx+/carry
	  fx-/carry
	  fx*/carry

	  fxnot
	  fxand
	  fxior
	  fxxor
	  fxif

	  fxbit-count
	  fxlength
	  fxfirst-bit-set
	  fxbit-set?
	  fxcopy-bit
	  fxbit-field
	  fxcopy-bit-field

	  fxarithmetic-shift
	  fxarithmetic-shift-left
	  fxarithmetic-shift-right

	  fxrotate-bit-field
	  fxreverse-bit-field)
  (import (only (guile) ash
		        cons*
			define-inlinable
			inexact->exact
			logand
			logbit?
			logcount
			logior
			lognot
			logxor
			most-positive-fixnum 
			most-negative-fixnum
			object-address)
	  (ice-9 optargs)
	  (rnrs base (6))
	  (rnrs control (6))
	  (rnrs arithmetic bitwise (6))
	  (rnrs conditions (6))
	  (rnrs exceptions (6))
	  (rnrs lists (6)))

  (define fixnum-width
    (let ((w (do ((i 0 (+ 1 i))
                  (n 1 (* 2 n)))
                 ((> n most-positive-fixnum)
                  (+ 1 i)))))
      (lambda () w)))

  (define (greatest-fixnum) most-positive-fixnum)
  (define (least-fixnum) most-negative-fixnum)

  (define (fixnum? obj)
    (not (= 0 (logand 2 (object-address obj)))))

  (define-inlinable (inline-fixnum? obj)
    (not (= 0 (logand 2 (object-address obj)))))

  (define-syntax assert-fixnum
    (syntax-rules ()
      ((_ arg ...)
       (or (and (inline-fixnum? arg) ...)
	   (raise (make-assertion-violation))))))

  (define (assert-fixnums args)
    (or (for-all inline-fixnum? args) (raise (make-assertion-violation))))

  (define-syntax define-fxop*
    (syntax-rules ()
      ((_ name op)
       (define name
	(case-lambda
	  ((x y)
	   (assert-fixnum x y)
	   (op x y))
	  (args
	   (assert-fixnums args)
           (apply op args)))))))

  ;; All these predicates don't check their arguments for fixnum-ness,
  ;; as this doesn't seem to be strictly required by R6RS.

  (define fx=? =)
  (define fx>? >)
  (define fx<? <)
  (define fx>=? >=)
  (define fx<=? <=)

  (define fxzero? zero?)
  (define fxpositive? positive?)
  (define fxnegative? negative?)
  (define fxodd? odd?)
  (define fxeven? even?)

  (define-fxop* fxmax max)
  (define-fxop* fxmin min)

  (define (fx+ fx1 fx2)
    (assert-fixnum fx1 fx2) 
    (let ((r (+ fx1 fx2))) 
      (or (inline-fixnum? r)
          (raise (make-implementation-restriction-violation)))
      r))

  (define (fx* fx1 fx2)
    (assert-fixnum fx1 fx2) 
    (let ((r (* fx1 fx2))) 
      (or (inline-fixnum? r)
          (raise (make-implementation-restriction-violation)))
      r))

  (define* (fx- fx1 #:optional fx2)
    (assert-fixnum fx1)
    (if fx2 
	(begin 
	  (assert-fixnum fx2) 
	  (let ((r (- fx1 fx2))) 
	    (or (inline-fixnum? r) (raise (make-assertion-violation)))
	    r))
	(let ((r (- fx1))) 
	  (or (inline-fixnum? r) (raise (make-assertion-violation)))
	  r)))

  (define (fxdiv fx1 fx2)
    (assert-fixnum fx1 fx2)
    (div fx1 fx2))

  (define (fxmod fx1 fx2)
    (assert-fixnum fx1 fx2)
    (mod fx1 fx2))

  (define (fxdiv-and-mod fx1 fx2)
    (assert-fixnum fx1 fx2)
    (div-and-mod fx1 fx2))

  (define (fxdiv0 fx1 fx2)
    (assert-fixnum fx1 fx2)
    (div0 fx1 fx2))
  
  (define (fxmod0 fx1 fx2)
    (assert-fixnum fx1 fx2)
    (mod0 fx1 fx2))

  (define (fxdiv0-and-mod0 fx1 fx2)
    (assert-fixnum fx1 fx2)
    (div0-and-mod0 fx1 fx2))

  (define (fx+/carry fx1 fx2 fx3)
    (assert-fixnum fx1 fx2 fx3)
    (let* ((s (+ fx1 fx2 fx3))
	   (s0 (mod0 s (expt 2 (fixnum-width))))
	   (s1 (div0 s (expt 2 (fixnum-width)))))
      (values s0 s1)))

  (define (fx-/carry fx1 fx2 fx3)
    (assert-fixnum fx1 fx2 fx3)
    (let* ((d (- fx1 fx2 fx3))
	   (d0 (mod0 d (expt 2 (fixnum-width))))
	   (d1 (div0 d (expt 2 (fixnum-width)))))
      (values d0 d1)))

  (define (fx*/carry fx1 fx2 fx3)
    (assert-fixnum fx1 fx2 fx3)
    (let* ((s (+ (* fx1 fx2) fx3))
	   (s0 (mod0 s (expt 2 (fixnum-width))))
	   (s1 (div0 s (expt 2 (fixnum-width)))))
      (values s0 s1)))

  (define (fxnot fx) (assert-fixnum fx) (lognot fx))
  (define-fxop* fxand logand)
  (define-fxop* fxior logior)
  (define-fxop* fxxor logxor)

  (define (fxif fx1 fx2 fx3) 
    (assert-fixnum fx1 fx2 fx3) 
    (bitwise-if fx1 fx2 fx3))

  (define (fxbit-count fx)
    (assert-fixnum fx)
    (if (negative? fx)
        (bitwise-not (logcount fx))
        (logcount fx)))

  (define (fxlength fx) (assert-fixnum fx) (bitwise-length fx))
  (define (fxfirst-bit-set fx) (assert-fixnum fx) (bitwise-first-bit-set fx))
  (define (fxbit-set? fx1 fx2) (assert-fixnum fx1 fx2) (logbit? fx2 fx1))

  (define (fxcopy-bit fx1 fx2 fx3) 
    (assert-fixnum fx1 fx2 fx3) 
    (bitwise-copy-bit fx1 fx2 fx3))

  (define (fxbit-field fx1 fx2 fx3)
    (assert-fixnum fx1 fx2 fx3)
    (bitwise-bit-field fx1 fx2 fx3))

  (define (fxcopy-bit-field fx1 fx2 fx3 fx4)
    (assert-fixnum fx1 fx2 fx3 fx4)
    (bitwise-copy-bit-field fx1 fx2 fx3 fx4))

  (define (fxarithmetic-shift fx1 fx2) (assert-fixnum fx1 fx2) (ash fx1 fx2))
  (define fxarithmetic-shift-left fxarithmetic-shift)

  (define (fxarithmetic-shift-right fx1 fx2)
    (assert-fixnum fx1 fx2) (ash fx1 (- fx2)))

  (define (fxrotate-bit-field fx1 fx2 fx3 fx4)
    (assert-fixnum fx1 fx2 fx3 fx4)
    (bitwise-rotate-bit-field fx1 fx2 fx3 fx4))
  
  (define (fxreverse-bit-field fx1 fx2 fx3)
    (assert-fixnum fx1 fx2 fx3)
    (bitwise-reverse-bit-field fx1 fx2 fx3))

)
