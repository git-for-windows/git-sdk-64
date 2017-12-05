;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.

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
;;; Optimizations on CPS.
;;;
;;; Code:

(define-module (language cps optimize)
  #:use-module (ice-9 match)
  #:use-module (language cps constructors)
  #:use-module (language cps contification)
  #:use-module (language cps cse)
  #:use-module (language cps dce)
  #:use-module (language cps elide-values)
  #:use-module (language cps licm)
  #:use-module (language cps peel-loops)
  #:use-module (language cps prune-top-level-scopes)
  #:use-module (language cps prune-bailouts)
  #:use-module (language cps rotate-loops)
  #:use-module (language cps self-references)
  #:use-module (language cps simplify)
  #:use-module (language cps specialize-primcalls)
  #:use-module (language cps specialize-numbers)
  #:use-module (language cps type-fold)
  #:use-module (language cps verify)
  #:export (optimize-higher-order-cps
            optimize-first-order-cps
            cps-default-optimization-options))

(define (kw-arg-ref args kw default)
  (match (memq kw args)
    ((_ val . _) val)
    (_ default)))

(define *debug?* #f)

(define (maybe-verify program)
  (if *debug?*
      (verify program)
      program))

(define-syntax-rule (define-optimizer optimize (pass kw default) ...)
  (define* (optimize program #:optional (opts '()))
    ;; This series of assignments to `program' used to be a series of
    ;; let* bindings of `program', as you would imagine.  In compiled
    ;; code this is fine because the compiler is able to allocate all
    ;; let*-bound variable to the same slot, which also means that the
    ;; garbage collector doesn't have to retain so many copies of the
    ;; term being optimized.  However during bootstrap, the interpreter
    ;; doesn't do this optimization, leading to excessive data retention
    ;; as the terms are rewritten.  To marginally improve bootstrap
    ;; memory usage, here we use set! instead.  The compiler should
    ;; produce the same code in any case, though currently it does not
    ;; because it doesn't do escape analysis on the box created for the
    ;; set!.
    (maybe-verify program)
    (set! program
      (if (kw-arg-ref opts kw default)
          (maybe-verify (pass program))
          program))
    ...
    (maybe-verify program)))

;; Passes that are needed:
;;
;;  * Abort contification: turning abort primcalls into continuation
;;    calls, and eliding prompts if possible.
;;
(define-optimizer optimize-higher-order-cps
  ;; FIXME: split-rec call temporarily moved to compile-bytecode and run
  ;; unconditionally, because closure conversion requires it.  Move the
  ;; pass back here when that's fixed.
  ;;
  ;; (split-rec #:split-rec? #t)
  (eliminate-dead-code #:eliminate-dead-code? #t)
  (prune-top-level-scopes #:prune-top-level-scopes? #t)
  (simplify #:simplify? #t)
  (contify #:contify? #t)
  (inline-constructors #:inline-constructors? #t)
  (elide-values #:elide-values? #t)
  (prune-bailouts #:prune-bailouts? #t)
  (peel-loops #:peel-loops? #t)
  (eliminate-common-subexpressions #:cse? #t)
  (type-fold #:type-fold? #t)
  (resolve-self-references #:resolve-self-references? #t)
  (eliminate-dead-code #:eliminate-dead-code? #t)
  (simplify #:simplify? #t))

(define-optimizer optimize-first-order-cps
  (specialize-numbers #:specialize-numbers? #t)
  (hoist-loop-invariant-code #:licm? #t)
  (eliminate-common-subexpressions #:cse? #t)
  (eliminate-dead-code #:eliminate-dead-code? #t)
  ;; Running simplify here enables rotate-loops to do a better job.
  (simplify #:simplify? #t)
  (rotate-loops #:rotate-loops? #t)
  (simplify #:simplify? #t)
  (specialize-primcalls #:specialize-primcalls? #t))

(define (cps-default-optimization-options)
  (list ;; #:split-rec? #t
   #:simplify? #t
   #:eliminate-dead-code? #t
   #:prune-top-level-scopes? #t
   #:contify? #t
   #:inline-constructors? #t
   #:specialize-primcalls? #t
   #:elide-values? #t
   #:prune-bailouts? #t
   #:peel-loops? #t
   #:cse? #t
   #:type-fold? #t
   #:resolve-self-references? #t
   #:specialize-numbers? #t
   #:licm? #t
   #:rotate-loops? #t
   ;; This one is used by the slot allocator.
   #:precolor-calls? #t))
