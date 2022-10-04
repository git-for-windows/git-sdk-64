;;; ck, to facilitate applicative-order macro programming

;;; Copyright (C) 2012 Free Software Foundation, Inc
;;; Copyright (C) 2009, 2011 Oleg Kiselyov
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;
;;;
;;; Originally written by Oleg Kiselyov and later contributed to Guile.
;;;
;;; Based on the CK machine introduced in:
;;;
;;; Matthias Felleisen and Daniel P. Friedman: Control operators, the
;;; SECD machine, and the lambda-calculus.  In Martin Wirsing, editor,
;;; Formal Description of Programming Concepts III, pages
;;; 193-217. Elsevier, Amsterdam, 1986.
;;;
;;; See http://okmij.org/ftp/Scheme/macros.html#ck-macros for details.
;;;

(define-module (system base ck)
  #:export (ck))

(define-syntax ck
  (syntax-rules (quote)
    ((ck () 'v) v)                      ; yield the value on empty stack

    ((ck (((op ...) ea ...) . s) 'v)    ; re-focus on the other argument, ea
     (ck-arg s (op ... 'v) ea ...))

    ((ck s (op ea ...))                 ; Focus: handling an application;
     (ck-arg s (op) ea ...))))          ; check if args are values

(define-syntax ck-arg
  (syntax-rules (quote)
    ((ck-arg s (op va ...))             ; all arguments are evaluated,
     (op s va ...))                     ; do the redex

    ((ck-arg s (op ...) 'v ea1 ...)     ; optimization when the first ea
     (ck-arg s (op ... 'v) ea1 ...))    ; was already a value

    ((ck-arg s (op ...) ea ea1 ...)     ; focus on ea, to evaluate it
     (ck (((op ...) ea1 ...) . s) ea))))
