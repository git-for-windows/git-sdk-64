;;; srfi-27.scm --- Sources of Random Bits

;; Copyright (C) 2010 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module is fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-27)
  #:export (random-integer
            random-real
            default-random-source
            make-random-source
            random-source?
            random-source-state-ref
            random-source-state-set!
            random-source-randomize!
            random-source-pseudo-randomize!
            random-source-make-integers
            random-source-make-reals)
  #:use-module (srfi srfi-9))

(cond-expand-provide (current-module) '(srfi-27))

(define-record-type :random-source
  (%make-random-source state)
  random-source?
  (state random-source-state set-random-source-state!))

(define (make-random-source)
  (%make-random-source (seed->random-state 0)))

(define (random-source-state-ref s)
  (random-state->datum (random-source-state s)))

(define (random-source-state-set! s state)
  (set-random-source-state! s (datum->random-state state)))

(define (random-source-randomize! s)
  (let ((time (gettimeofday)))
    (set-random-source-state! s (seed->random-state
                                 (+ (* (car time) 1e6) (cdr time))))))

(define (random-source-pseudo-randomize! s i j)
  (set-random-source-state! s (seed->random-state (i+j->seed i j))))

(define (i+j->seed i j)
  (logior (ash (spread i 2) 1)
          (spread j 2)))

(define (spread n amount)
  (let loop ((result 0) (n n) (shift 0))
    (if (zero? n)
        result
        (loop (logior result
                      (ash (logand n 1) shift))
              (ash n -1)
              (+ shift amount)))))

(define (random-source-make-integers s)
  (lambda (n)
    (random n (random-source-state s))))

(define random-source-make-reals
  (case-lambda
    ((s)
     (lambda ()
       (let loop ()
         (let ((x (random:uniform (random-source-state s))))
           (if (zero? x)
               (loop)
               x)))))
    ((s unit)
     (or (and (real? unit) (< 0 unit 1))
         (error "unit must be real between 0 and 1" unit))
     (random-source-make-reals s))))

(define default-random-source (make-random-source))
(define random-integer (random-source-make-integers default-random-source))
(define random-real (random-source-make-reals default-random-source))
