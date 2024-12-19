;;; Optimization flags

;; Copyright (C) 2018,2020-2022,2024 Free Software Foundation, Inc.

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

(define-module (system base optimize)
  #:use-module (ice-9 match)
  #:export (available-optimizations
            pass-optimization-level
            optimizations-for-level))

(define* (available-optimizations #:optional lang-name)
  (match lang-name
    ('tree-il
     '((#:cps? 2)
       (#:resolve-free-vars? 2)
       (#:resolve-primitives? 1)
       (#:expand-primitives? 1)
       (#:letrectify? 2)
       (#:demux-lambda? 2)
       (#:seal-private-bindings? 3)
       (#:partial-eval? 1)
       (#:eta-expand? 2)
       (#:inlinable-exports? 2)
       (#:cross-module-inlining? 2)))
    ('cps
     '( ;; (#:split-rec? #t)
       (#:simplify? 2)
       (#:eliminate-dead-code? 2)
       (#:prune-top-level-scopes? 2)
       (#:contify? 2)
       (#:specialize-primcalls? 2)
       (#:peel-loops? 2)
       (#:cse? 2)
       (#:type-fold? 2)
       (#:elide-arity-checks? 2)
       (#:optimize-known-return-types? 2)
       (#:resolve-self-references? 2)
       (#:devirtualize-integers? 2)
       (#:specialize-numbers? 2)
       (#:optimize-branch-chains? 2)
       (#:licm? 2)
       (#:rotate-loops? 2)
       ;; This one is used by the slot allocator.
       (#:precolor-calls? 2)))
    (#f
     (append (available-optimizations 'tree-il)
             (available-optimizations 'cps)))))

(define (pass-optimization-level kw)
  (match (assq kw (available-optimizations))
    ((kw level) level)
    (_ (error "unknown optimization" kw))))

;; Turn on all optimizations unless -O0.
(define (optimizations-for-level level)
  (let lp ((options (available-optimizations)))
    (match options
      (() '())
      (((kw at-level) . options)
       (cons* kw (<= at-level level) (lp options))))))
