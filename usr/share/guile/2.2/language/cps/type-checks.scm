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
;;; This pass kills dead expressions: code that has no side effects, and
;;; whose value is unused.  It does so by marking all live values, and
;;; then discarding other values as dead.  This happens recursively
;;; through procedures, so it should be possible to elide dead
;;; procedures as well.
;;;
;;; Code:

(define-module (language cps type-checks)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps effects-analysis)
  #:use-module (language cps types)
  #:use-module (language cps intmap)
  #:export (elide-type-checks
            compute-effects/elide-type-checks))

(define (elide-type-checks conts kfun effects)
  "Elide &type-check effects from EFFECTS for the function starting at
KFUN where we can prove that no assertion will be raised at run-time."
  (let ((types (infer-types conts kfun)))
    (define (visit-primcall effects fx label name args)
      (if (primcall-types-check? types label name args)
          (intmap-replace! effects label (logand fx (lognot &type-check)))
          effects))
    (persistent-intmap
     (intmap-fold (lambda (label types effects)
                    (let ((fx (intmap-ref effects label)))
                      (cond
                       ((causes-all-effects? fx) effects)
                       ((causes-effect? fx &type-check)
                        (match (intmap-ref conts label)
                          (($ $kargs _ _ exp)
                           (match exp
                             (($ $continue k src ($ $primcall name args))
                              (visit-primcall effects fx label name args))
                             (($ $continue k src
                                 ($ $branch _ ($primcall name args)))
                              (visit-primcall effects fx label name args))
                             (_ effects)))
                          (_ effects)))
                       (else effects))))
                  types
                  effects))))

(define (compute-effects/elide-type-checks conts)
  (intmap-fold (lambda (label cont effects)
                 (match cont
                   (($ $kfun) (elide-type-checks conts label effects))
                   (_ effects)))
               conts
               (compute-effects conts)))
