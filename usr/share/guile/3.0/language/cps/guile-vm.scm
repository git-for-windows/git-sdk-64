;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2023 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Backend-specific lowering and optimization when targetting Guile's
;;; bytecode virtual machine.
;;;
;;; Code:

(define-module (language cps guile-vm)
  #:use-module (ice-9 match)
  #:use-module (language cps guile-vm loop-instrumentation)
  #:use-module (language cps guile-vm lower-primcalls)
  #:use-module (language cps guile-vm reify-primitives)
  #:use-module (system base target)
  #:export (make-lowerer
            available-optimizations
            target-symbol-hash
            target-symbol-hash-bits
            target-has-unbound-boxes?))

;; This hash function is originally from
;; http://burtleburtle.net/bob/c/lookup3.c by Bob Jenkins, May 2006,
;; Public Domain.  No warranty.
(define (jenkins-lookup3-hashword2 str)
  (define (u32 x) (logand x #xffffFFFF))
  (define (shl x n) (u32 (ash x n)))
  (define (shr x n) (ash x (- n)))
  (define (rot x n) (logior (shl x n) (shr x (- 32 n))))
  (define (add x y) (u32 (+ x y)))
  (define (sub x y) (u32 (- x y)))
  (define (xor x y) (logxor x y))

  (define (mix a b c)
    (let* ((a (sub a c)) (a (xor a (rot c 4)))  (c (add c b))
           (b (sub b a)) (b (xor b (rot a 6)))  (a (add a c))
           (c (sub c b)) (c (xor c (rot b 8)))  (b (add b a))
           (a (sub a c)) (a (xor a (rot c 16))) (c (add c b))
           (b (sub b a)) (b (xor b (rot a 19))) (a (add a c))
           (c (sub c b)) (c (xor c (rot b 4)))  (b (add b a)))
      (values a b c)))
  (define (final a b c)
    (let* ((c (xor c b)) (c (sub c (rot b 14)))
           (a (xor a c)) (a (sub a (rot c 11)))
           (b (xor b a)) (b (sub b (rot a 25)))
           (c (xor c b)) (c (sub c (rot b 16)))
           (a (xor a c)) (a (sub a (rot c 4)))
           (b (xor b a)) (b (sub b (rot a 14)))
           (c (xor c b)) (c (sub c (rot b 24))))
      (values a b c)))

  (define len (string-length str))
  (define (add-char x index)
    (add x (char->integer (string-ref str index))))

  (let ((init (add #xdeadbeef (add (shl len 2) 47))))
    (let lp ((i 0) (a init) (b init) (c init))
      (let ((remaining (- len i)))
        (cond
         ((< 3 remaining)
          (call-with-values (lambda ()
                              (mix (add-char a i)
                                   (add-char b (+ i 1))
                                   (add-char c (+ i 2))))
            (lambda (a b c)
              (lp (+ i 3) a b c))))
         (else
          (let* ((a (if (<= 1 remaining) (add-char a i) a))
                 (b (if (<= 2 remaining) (add-char b (+ i 1)) b))
                 (c (if (<= 3 remaining) (add-char c (+ i 2)) c)))
            (final a b c))))))))

(define (target-symbol-hash str)
  (call-with-values (lambda () (jenkins-lookup3-hashword2 str))
    (lambda (a b c)
      ;; The high 32 bits of the hash on a 64-bit platform are
      ;; equivalent to the hash on a 32-bit platform.  The top two bits
      ;; are zero to allow the hash to fit in a fixnum.
      (ash (case (target-word-size)
             ((4) c)
             ((8) (logior (ash c 32) b))
             (else (error "unexpected target word size" (target-word-size))))
           -2))))

(define target-symbol-hash-bits
  (- (* (target-word-size) 8) 2))

(define (make-lowerer optimization-level opts)
  (lambda (exp env)
    (add-loop-instrumentation
     (reify-primitives
      (lower-primcalls exp)))))

(define (available-optimizations)
  '())

(define target-has-unbound-boxes? #t)
