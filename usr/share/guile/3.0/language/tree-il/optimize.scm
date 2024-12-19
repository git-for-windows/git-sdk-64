;;; Tree-il optimizer

;; Copyright (C) 2009, 2010-2015, 2018-2021, 2024 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language tree-il optimize)
  #:use-module (ice-9 match)
  ;; FIXME: Perhaps allow bootstrap builds to skip fix-letrec, because
  ;; it imports intset, intmap, etc.
  #:use-module (language tree-il fix-letrec)
  #:use-module (system base optimize)
  #:export (optimize
            make-lowerer
            tree-il-optimizations))

(define-syntax-rule (lazy-ref mod proc)
  (module-ref (resolve-interface 'mod) 'proc))

(define (dump-optimized-tree-il exp env)
  ((lazy-ref (ice-9 pretty-print) pretty-print)
   ((lazy-ref (language scheme decompile-tree-il) decompile-tree-il)
    exp env '()))
  exp)

(define (make-optimizer opts)
  (define-syntax lookup
    (syntax-rules ()
      ((lookup kw id)
       (lookup kw id id))
      ((lookup kw submodule proc)
       (and (assq-ref opts kw)
            (lazy-ref (language tree-il submodule) proc)))))
  (let ((verify     (or (lookup #:verify-tree-il? debug verify-tree-il)
                        (lambda (exp) exp)))
        (modulify   (lookup #:resolve-free-vars? resolve-free-vars))
        (resolve    (lookup #:resolve-primitives? primitives resolve-primitives))
        (expand     (lookup #:expand-primitives? primitives expand-primitives))
        (letrectify (lookup #:letrectify? letrectify))
        (seal?      (assq-ref opts #:seal-private-bindings?))
        (xinline?   (assq-ref opts #:cross-module-inlining?))
        (demux      (lookup #:demux-lambda? demux-lambda))
        (peval      (lookup #:partial-eval? peval))
        (eta-expand (lookup #:eta-expand? eta-expand))
        (inlinables (lookup #:inlinable-exports? inlinable-exports)))
    (define-syntax-rule (run-pass! (proc exp arg ...))
      (when proc (set! exp (verify (proc exp arg ...)))))
    (lambda (exp env)
      (verify exp)
      (run-pass! (modulify exp))
      (run-pass! (resolve exp env))
      (run-pass! (expand exp))
      (run-pass! (letrectify exp #:seal-private-bindings? seal?))
      (run-pass! (demux exp))
      (run-pass! (fix-letrec exp))
      (run-pass! (peval exp env #:cross-module-inlining? xinline?))
      (run-pass! (eta-expand exp))
      (run-pass! (inlinables exp))
      (when (assq-ref opts #:dump-optimized-tree-il?)
        (dump-optimized-tree-il exp env))
      exp)))

(define (optimize x env opts)
  ((make-optimizer opts) x env))

(define (tree-il-optimizations)
  (available-optimizations 'tree-il))

(define (tree-il-options)
  (cons* '(#:dump-optimized-tree-il? #f)
         '(#:verify-tree-il? #f)
         (tree-il-optimizations)))

(define (make-lowerer optimization-level opts)
  (define (kw-arg-ref args kw default)
    (match (memq kw args)
      ((_ val . _) val)
      (_ default)))
  (define (enabled-for-level? level) (and level (<= level optimization-level)))
  (make-optimizer
   (let lp ((all-opts (tree-il-options)))
     (match all-opts
       (() '())
       (((kw level) . all-opts)
        (acons kw (kw-arg-ref opts kw (enabled-for-level? level))
               (lp all-opts)))))))
