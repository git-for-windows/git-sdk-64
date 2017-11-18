;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2010 Free Software Foundation, Inc.
;;;
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

(define-module (system base lalr)

  ;; XXX: In theory this import is not needed but the evaluator (not the
  ;; compiler) complains about `lexical-token' being unbound when expanding
  ;; `(define-record-type lexical-token ...)' if we omit it.
  #:use-module (srfi srfi-9)

  #:export (lalr-parser print-states

            make-lexical-token lexical-token?
            lexical-token-category
            lexical-token-source
            lexical-token-value

            make-source-location source-location?
            source-location-input
            source-location-line
            source-location-column
            source-location-offset
            source-location-length
            source-location->source-properties

            ;; `lalr-parser' is a defmacro, which produces code that refers to
            ;; these drivers.
            lr-driver glr-driver))

;; The LALR parser generator was written by Dominique Boucher.  It's available
;; from http://code.google.com/p/lalr-scm/ and released under the LGPLv3+.
(include-from-path "system/base/lalr.upstream.scm")

(define (source-location->source-properties loc)
  `((filename . ,(source-location-input loc))
    (line . ,(source-location-line loc))
    (column . ,(source-location-column loc))))
