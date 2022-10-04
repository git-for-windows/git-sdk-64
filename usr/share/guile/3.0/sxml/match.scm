;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2010, 2011 Free Software Foundation, Inc.
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

(define-module (sxml match)
  #:export (sxml-match
            sxml-match-let
            sxml-match-let*)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 control))


;;; Commentary:
;;;
;;; This module provides an SXML pattern matcher, written by Jim Bender.  This
;;; allows application code to match on SXML nodes and attributes without having
;;; to deal with the details of s-expression matching, without worrying about
;;; the order of attributes, etc.
;;;
;;; It is fully documented in the Guile Reference Manual.
;;;
;;; Code:



;;;
;;; PLT compatibility layer.
;;;

(define-syntax-rule (syntax-object->datum stx)
  (syntax->datum stx))

(define-syntax-rule (void)
  *unspecified*)

(define (raise-syntax-error x msg obj sub)
  (throw 'sxml-match-error x msg obj sub))

(define-syntax module
  (syntax-rules (provide require)
    ((_ name lang (provide p_ ...) (require r_ ...)
        body ...)
     (begin body ...))))


;;;
;;; Include upstream source file.
;;;

;; This file was taken from
;; <http://planet.plt-scheme.org/package-source/jim/sxml-match.plt/1/1/> on
;; 2010-05-24.  It was written by Jim Bender <benderjg2@aol.com> and released
;; under the MIT/X11 license
;; <http://www.gnu.org/licenses/license-list.html#X11License>.
;;
;; Modified the `sxml-match1' macro to allow multiple-value returns (upstream
;; was notified.)

(include-from-path "sxml/sxml-match.ss")

;;; match.scm ends here
