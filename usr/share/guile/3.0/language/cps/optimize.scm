;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2018,2020,2021,2023 Free Software Foundation, Inc.

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
;;; Optimizations on CPS.
;;;
;;; Code:

(define-module (language cps optimize)
  #:use-module (ice-9 match)
  #:use-module (language cps closure-conversion)
  #:use-module (language cps contification)
  #:use-module (language cps cse)
  #:use-module (language cps dce)
  #:use-module (language cps devirtualize-integers)
  #:use-module (language cps elide-arity-checks)
  #:use-module (language cps licm)
  #:use-module (language cps peel-loops)
  #:use-module (language cps prune-top-level-scopes)
  #:use-module (language cps renumber)
  #:use-module (language cps rotate-loops)
  #:use-module (language cps return-types)
  #:use-module (language cps self-references)
  #:use-module (language cps simplify)
  #:use-module (language cps specialize-numbers)
  #:use-module (language cps specialize-primcalls)
  #:use-module (language cps split-rec)
  #:use-module (language cps switch)
  #:use-module (language cps type-fold)
  #:use-module (language cps verify)
  #:use-module (system base optimize)
  #:use-module (system base target)
  #:export (optimize-higher-order-cps
            optimize-first-order-cps
            cps-optimizations
            make-cps-lowerer))

(define *debug?* #f)

(define (maybe-verify program)
  (if *debug?*
      (verify program)
      program))

(define-syntax-rule (define-optimizer optimize (pass kw) ...)
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
      (if (assq-ref opts kw)
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
  ;; (split-rec #:split-rec?)
  (eliminate-dead-code #:eliminate-dead-code?)
  (prune-top-level-scopes #:prune-top-level-scopes?)
  (simplify #:simplify?)
  (contify #:contify?)
  (simplify #:simplify?)
  (devirtualize-integers #:devirtualize-integers?)
  (peel-loops #:peel-loops?)
  (eliminate-common-subexpressions #:cse?)
  (type-fold #:type-fold?)
  (resolve-self-references #:resolve-self-references?)
  (eliminate-dead-code #:eliminate-dead-code?)
  (simplify #:simplify?))

(define-optimizer optimize-first-order-cps
  (elide-arity-checks #:elide-arity-checks?)
  (specialize-numbers #:specialize-numbers?)
  (hoist-loop-invariant-code #:licm?)
  (specialize-primcalls #:specialize-primcalls?)
  (optimize-branch-chains #:optimize-branch-chains?)
  (eliminate-common-subexpressions #:cse?)
  (optimize-known-return-types #:optimize-known-return-types?)
  (eliminate-dead-code #:eliminate-dead-code?)
  ;; Running simplify here enables rotate-loops to do a better job.
  (simplify #:simplify?)
  (rotate-loops #:rotate-loops?)
  (simplify #:simplify?))

(define (cps-optimizations)
  (available-optimizations 'cps))

(define (make-backend-cps-lowerer optimization-level opts)
  (let* ((iface (resolve-interface `(language cps ,(target-runtime))))
         (make-lowerer (module-ref iface 'make-lowerer)))
    (make-lowerer optimization-level opts)))

(define (lower-cps/generic exp opts)
  ;; FIXME: For now the closure conversion pass relies on $rec instances
  ;; being separated into SCCs.  We should fix this to not be the case,
  ;; and instead move the split-rec pass back to
  ;; optimize-higher-order-cps.
  (set! exp (split-rec exp))
  (set! exp (optimize-higher-order-cps exp opts))
  (set! exp (convert-closures exp))
  (optimize-first-order-cps exp opts))

(define (select-optimizations optimization-level opts all-opts)
  (define (kw-arg-ref args kw default)
    (match (memq kw args)
      ((_ val . _) val)
      (_ default)))
  (define (enabled-for-level? level) (<= level optimization-level))
  (let lp ((all-opts all-opts))
    (match all-opts
      (() '())
      (((kw level) . all-opts)
       (acons kw (kw-arg-ref opts kw (enabled-for-level? level))
              (lp all-opts))))))

(define (make-cps-lowerer optimization-level opts)
  (define generic-opts
    (select-optimizations optimization-level opts (cps-optimizations)))
  (define lower-cps/backend
    (make-backend-cps-lowerer optimization-level opts))
  (lambda (exp env)
    (renumber
     (lower-cps/backend
      (lower-cps/generic exp generic-opts)
      env))))
