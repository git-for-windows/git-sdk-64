;;; bitwise.scm --- The R6RS bitwise arithmetic operations library

;;      Copyright (C) 2010, 2013 Free Software Foundation, Inc.
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


(library (rnrs arithmetic bitwise (6))
  (export bitwise-not
	  
	  bitwise-and
	  bitwise-ior
	  bitwise-xor
	  
	  bitwise-if
	  bitwise-bit-count
	  bitwise-length

	  bitwise-first-bit-set
	  bitwise-bit-set?
	  bitwise-copy-bit
	  bitwise-bit-field
	  bitwise-copy-bit-field

	  bitwise-arithmetic-shift
	  bitwise-arithmetic-shift-left
	  bitwise-arithmetic-shift-right
	  bitwise-rotate-bit-field
	  bitwise-reverse-bit-field)
  (import (rnrs base (6))
	  (rnrs control (6))
          (rename (only (srfi srfi-60) bitwise-if
                                       integer-length
                                       first-set-bit
                                       copy-bit
                                       bit-field
                                       copy-bit-field
                                       rotate-bit-field
                                       reverse-bit-field)
                  (integer-length bitwise-length)
                  (first-set-bit bitwise-first-bit-set)
                  (bit-field bitwise-bit-field)
                  (reverse-bit-field bitwise-reverse-bit-field))
	  (rename (only (guile) lognot 
			        logand 
				logior
				logxor 
				logcount 
				logbit?
				modulo
				ash)
		  (lognot bitwise-not)
		  (logand bitwise-and) 
		  (logior bitwise-ior) 
		  (logxor bitwise-xor)
		  (ash bitwise-arithmetic-shift)))

  (define (bitwise-bit-count ei)
    (if (negative? ei)
        (bitwise-not (logcount ei))
        (logcount ei)))

  (define (bitwise-bit-set? ei1 ei2) (logbit? ei2 ei1))

  (define (bitwise-copy-bit ei1 ei2 ei3)
    ;; The specification states that ei3 should be either 0 or 1.
    ;; However, other values have been tolerated by both Guile 2.0.x and
    ;; the sample implementation given the R6RS library document, so for
    ;; backward compatibility we continue to permit it.
    (copy-bit ei2 ei1 (logbit? 0 ei3)))

  (define (bitwise-copy-bit-field ei1 ei2 ei3 ei4)
    (copy-bit-field ei1 ei4 ei2 ei3))

  (define (bitwise-rotate-bit-field ei1 ei2 ei3 ei4)
    (rotate-bit-field ei1 ei4 ei2 ei3))

  (define bitwise-arithmetic-shift-left bitwise-arithmetic-shift)
  (define (bitwise-arithmetic-shift-right ei1 ei2)
    (bitwise-arithmetic-shift ei1 (- ei2))))
