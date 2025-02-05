;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2016, 2017, 2018, 2020, 2023 Free Software Foundation, Inc.

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
;;; A pass to add "instrument-loop" primcalls at loop headers.
;;;
;;; Code:

(define-module (language cps guile-vm loop-instrumentation)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps renumber)
  #:export (add-loop-instrumentation))

(define (compute-loop-headers cps)
  (define (maybe-add-header label k headers)
    "Add K to headers if it is a target of a backward branch."
    (if (<= k label)
        (intset-add! headers k)
        headers))
  (define (visit-cont label cont headers)
    (match cont
      (($ $kargs names vars ($ $continue k))
       (maybe-add-header label k headers))
      (($ $kargs names vars ($ $branch kf kt))
       (maybe-add-header label kf (maybe-add-header label kt headers)))
      (($ $kargs names vars ($ $switch kf kt*))
       (fold1 (lambda (k headers) (maybe-add-header label k headers))
              (cons kf kt*)
              headers))
      (_ headers)))
  (persistent-intset (intmap-fold visit-cont cps empty-intset)))

(define (add-loop-instrumentation cps)
  (define (add-instrumentation label cps)
    (match (intmap-ref cps label)
      (($ $kargs names vars term)
       (with-cps cps
         (letk k ($kargs () () ,term))
         (setk label
               ($kargs names vars
                 ($continue k #f
                   ($primcall 'instrument-loop #f ()))))))))
  (let* ((cps (renumber cps))
         (headers (compute-loop-headers cps)))
    (with-fresh-name-state cps
      (persistent-intmap (intset-fold add-instrumentation headers cps)))))
