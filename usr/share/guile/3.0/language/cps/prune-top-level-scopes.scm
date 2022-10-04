;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2014, 2015, 2017 Free Software Foundation, Inc.

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
;;; A simple pass to prune unneeded top-level scopes.
;;;
;;; Code:

(define-module (language cps prune-top-level-scopes)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:export (prune-top-level-scopes))

(define (compute-used-scopes conts)
  (persistent-intset
   (intmap-fold
    (lambda (label cont used-scopes)
      (match cont
        (($ $kargs _ _
            ($ $continue k src
               ($ $primcall 'cached-toplevel-box (scope name bound?))))
         (intset-add! used-scopes scope))
        (_
         used-scopes)))
    conts
    empty-intset)))

(define (prune-top-level-scopes conts)
  (let* ((used-scopes (compute-used-scopes conts)))
    (intmap-map
     (lambda (label cont)
       (match cont
         (($ $kargs names vars
             ($ $continue k src
                ($ $primcall 'cache-current-module! (scope-id) (module))))
          (if (intset-ref used-scopes scope-id)
              cont
              (build-cont ($kargs names vars
                                  ($continue k src ($values ()))))))
         (_
          cont)))
     conts)))
