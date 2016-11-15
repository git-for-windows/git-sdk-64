;;; srfi-42.scm --- Eager comprehensions

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

;; This module is not yet documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-42)
  #:export (:
            :-dispatch-ref
            :-dispatch-set!
            :char-range
            :dispatched
            :do
            :generator-proc
            :integers
            :let
            :list
            :parallel
            :port
            :range
            :real-range
            :string
            :until
            :vector
            :while
            any?-ec
            append-ec
            dispatch-union
            do-ec
            every?-ec
            first-ec
            fold-ec
            fold3-ec
            last-ec
            list-ec
            make-initial-:-dispatch
            max-ec
            min-ec
            product-ec
            string-append-ec
            string-ec
            sum-ec
            vector-ec
            vector-of-length-ec))

(cond-expand-provide (current-module) '(srfi-42))

(include-from-path "srfi/srfi-42/ec.scm")
