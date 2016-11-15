;;; srfi-67.scm --- Compare Procedures

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

(define-module (srfi srfi-67)
  #:export (</<=?
            </<?
            <=/<=?
            <=/<?
            <=?
            <?
            =?
            >/>=?
            >/>?
            >=/>=?
            >=/>?
            >=?
            >?
            boolean-compare
            chain<=?
            chain<?
            chain=?
            chain>=?
            chain>?
            char-compare
            char-compare-ci
            compare-by<
            compare-by<=
            compare-by=/<
            compare-by=/>
            compare-by>
            compare-by>=
            complex-compare
            cond-compare
            debug-compare
            default-compare
            if-not=?
            if3
            if<=?
            if<?
            if=?
            if>=?
            if>?
            integer-compare
            kth-largest
            list-compare
            list-compare-as-vector
            max-compare
            min-compare
            not=?
            number-compare
            pair-compare
            pair-compare-car
            pair-compare-cdr
            pairwise-not=?
            rational-compare
            real-compare
            refine-compare
            select-compare
            symbol-compare
            vector-compare
            vector-compare-as-list)
  #:replace (string-compare string-compare-ci)
  #:use-module (srfi srfi-27))

(cond-expand-provide (current-module) '(srfi-67))

(include-from-path "srfi/srfi-67/compare.scm")
