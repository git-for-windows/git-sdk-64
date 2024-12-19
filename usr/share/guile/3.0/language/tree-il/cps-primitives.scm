;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2015, 2017-2020, 2023 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; Information about named primitives, as they appear in $prim and
;;; $primcall.
;;;
;;; Code:

(define-module (language tree-il cps-primitives)
  #:use-module (ice-9 match)
  #:use-module (language bytecode)
  #:use-module (system base types internal)
  #:export (tree-il-primitive->cps-primitive+nargs+nvalues
            branching-primitive?
            heap-type-predicate?
            number-type-predicate?))

(define *primitives* (make-hash-table))

(define-syntax define-cps-primitive
  (syntax-rules ()
    ((_ (tree-il-primitive cps-primitive) nargs nvalues)
     (hashq-set! *primitives* 'tree-il-primitive
                 '#(cps-primitive nargs nvalues)))
    ((_ primitive nargs nvalues)
     (define-cps-primitive (primitive primitive) nargs nvalues))))

;; tree-il-prim -> #(cps-prim nargs nvalues) | #f
(define (tree-il-primitive->cps-primitive+nargs+nvalues name)
  (hashq-ref *primitives* name))

(define-cps-primitive (make-variable box) 1 1)
(define-cps-primitive (variable-ref box-ref) 1 1)
(define-cps-primitive (variable-set! box-set!) 2 0)
(define-cps-primitive (%variable-ref %box-ref) 1 1)
(define-cps-primitive (%variable-set! %box-set!) 2 0)

(define-cps-primitive current-module 0 1)
(define-cps-primitive (module-ensure-local-variable! define!) 2 1)

(define-cps-primitive wind 2 0)
(define-cps-primitive unwind 0 0)
(define-cps-primitive push-dynamic-state 1 0)
(define-cps-primitive pop-dynamic-state 0 0)

(define-cps-primitive push-fluid 2 0)
(define-cps-primitive pop-fluid 0 0)
(define-cps-primitive fluid-ref 1 1)
(define-cps-primitive fluid-set! 2 0)

(define-cps-primitive string-length 1 1)
(define-cps-primitive string-ref 2 1)
(define-cps-primitive string-set! 3 0)
(define-cps-primitive string->number 1 1)

(define-cps-primitive string->symbol 1 1)
(define-cps-primitive symbol->string 1 1)

(define-cps-primitive symbol->keyword 1 1)
(define-cps-primitive keyword->symbol 1 1)

(define-cps-primitive integer->char 1 1)
(define-cps-primitive char->integer 1 1)

(define-cps-primitive cons 2 1)
(define-cps-primitive car 1 1)
(define-cps-primitive cdr 1 1)
(define-cps-primitive set-car! 2 0)
(define-cps-primitive set-cdr! 2 0)

(define-cps-primitive (+ add) 2 1)
(define-cps-primitive (- sub) 2 1)
(define-cps-primitive (* mul) 2 1)
(define-cps-primitive (/ div) 2 1)
(define-cps-primitive (quotient quo) 2 1)
(define-cps-primitive (remainder rem) 2 1)
(define-cps-primitive (modulo mod) 2 1)
(define-cps-primitive (exact->inexact inexact) 1 1)
(define-cps-primitive sqrt 1 1)
(define-cps-primitive abs 1 1)
(define-cps-primitive floor 1 1)
(define-cps-primitive ceiling 1 1)
(define-cps-primitive sin 1 1)
(define-cps-primitive cos 1 1)
(define-cps-primitive tan 1 1)
(define-cps-primitive asin 1 1)
(define-cps-primitive acos 1 1)
(define-cps-primitive atan 1 1)
(define-cps-primitive atan2 2 1)

(define-cps-primitive lsh 2 1)
(define-cps-primitive rsh 2 1)
(define-cps-primitive logand 2 1)
(define-cps-primitive logior 2 1)
(define-cps-primitive logxor 2 1)
(define-cps-primitive logsub 2 1)
(define-cps-primitive logbit? 2 1)

(define-cps-primitive allocate-vector 1 1)
(define-cps-primitive make-vector 2 1)
(define-cps-primitive vector-length 1 1)
(define-cps-primitive vector-ref 2 1)
(define-cps-primitive vector-set! 3 0)
(define-cps-primitive vector-init! 3 0)

(define-cps-primitive struct-vtable 1 1)
(define-cps-primitive allocate-struct 2 1)
(define-cps-primitive struct-ref 2 1)
;; Unhappily, and undocumentedly, struct-set! returns the value that was
;; set.  There is code that relies on this.  The struct-set! lowering
;; routines ensure this return arity.
(define-cps-primitive struct-set! 3 1)
(define-cps-primitive struct-init! 3 0)

(define-cps-primitive class-of 1 1)

(define-cps-primitive string-utf8-length 1 1)
(define-cps-primitive utf8->string 1 1)
(define-cps-primitive string->utf8 1 1)
(define-cps-primitive (bytevector-length bv-length) 1 1)
(define-cps-primitive (bytevector-u8-ref bv-u8-ref) 2 1)
(define-cps-primitive (bytevector-u16-native-ref bv-u16-ref) 2 1)
(define-cps-primitive (bytevector-u32-native-ref bv-u32-ref) 2 1)
(define-cps-primitive (bytevector-u64-native-ref bv-u64-ref) 2 1)
(define-cps-primitive (bytevector-s8-ref bv-s8-ref) 2 1)
(define-cps-primitive (bytevector-s16-native-ref bv-s16-ref) 2 1)
(define-cps-primitive (bytevector-s32-native-ref bv-s32-ref) 2 1)
(define-cps-primitive (bytevector-s64-native-ref bv-s64-ref) 2 1)
(define-cps-primitive (bytevector-ieee-single-native-ref bv-f32-ref) 2 1)
(define-cps-primitive (bytevector-ieee-double-native-ref bv-f64-ref) 2 1)
(define-cps-primitive (bytevector-u8-set! bv-u8-set!) 3 0)
(define-cps-primitive (bytevector-u16-native-set! bv-u16-set!) 3 0)
(define-cps-primitive (bytevector-u32-native-set! bv-u32-set!) 3 0)
(define-cps-primitive (bytevector-u64-native-set! bv-u64-set!) 3 0)
(define-cps-primitive (bytevector-s8-set! bv-s8-set!) 3 0)
(define-cps-primitive (bytevector-s16-native-set! bv-s16-set!) 3 0)
(define-cps-primitive (bytevector-s32-native-set! bv-s32-set!) 3 0)
(define-cps-primitive (bytevector-s64-native-set! bv-s64-set!) 3 0)
(define-cps-primitive (bytevector-ieee-single-native-set! bv-f32-set!) 3 0)
(define-cps-primitive (bytevector-ieee-double-native-set! bv-f64-set!) 3 0)

(define-cps-primitive current-thread 0 1)

(define-cps-primitive make-atomic-box 1 1)
(define-cps-primitive atomic-box-ref 1 1)
(define-cps-primitive atomic-box-set! 2 0)
(define-cps-primitive atomic-box-swap! 2 1)
(define-cps-primitive atomic-box-compare-and-swap! 3 1)

(define *branching-primitive-arities* (make-hash-table))
(define-syntax-rule (define-branching-primitive name nargs)
  (hashq-set! *branching-primitive-arities* 'name '(0 . nargs)))

(define-syntax define-immediate-type-predicate
  (syntax-rules ()
    ((_ name #f mask tag) #f)
    ((_ name pred mask tag)
     (define-branching-primitive pred 1))))
(define *heap-type-predicates* (make-hash-table))
(define-syntax-rule (define-heap-type-predicate name pred mask tag)
  (begin
    (hashq-set! *heap-type-predicates* 'pred #t)
    (define-branching-primitive pred 1)))

(visit-immediate-tags define-immediate-type-predicate)
(visit-heap-tags define-heap-type-predicate)

;; We only need to define those branching primitives that are used as
;; Tree-IL primitives.  There are others like u64-= which are emitted by
;; CPS code.
(define-branching-primitive eq? 2)
(define-branching-primitive heap-numbers-equal? 2)
(define-branching-primitive < 2)
(define-branching-primitive <= 2)
(define-branching-primitive = 2)

(define-branching-primitive procedure? 1)

(define-branching-primitive number? 1)
(define-branching-primitive complex? 1)
(define-branching-primitive real? 1)
(define-branching-primitive rational? 1)
(define-branching-primitive integer? 1)
(define-branching-primitive exact-integer? 1)

(define *number-type-predicates* (make-hash-table))
(define-syntax-rule (define-number-type-predicate pred nargs)
  (begin
    (hashq-set! *number-type-predicates* 'pred #t)
    (define-branching-primitive pred nargs)))

(define-number-type-predicate exact? 1)
(define-number-type-predicate inexact? 1)

(define (branching-primitive? name)
  "Is @var{name} a primitive that can only appear in $branch CPS terms?"
  (hashq-ref *branching-primitive-arities* name))

(define (heap-type-predicate? name)
  "Is @var{name} a predicate that needs guarding by @code{heap-object?}
 before it is lowered to CPS?"
  (hashq-ref *heap-type-predicates* name))

(define (number-type-predicate? name)
  "Is @var{name} a predicate that needs guarding by @code{number?}
 before it is lowered to CPS?"
  (hashq-ref *number-type-predicates* name))

