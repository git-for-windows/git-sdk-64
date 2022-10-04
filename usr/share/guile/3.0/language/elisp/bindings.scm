;;; Guile Emacs Lisp

;;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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

;;; Code:

(define-module (language elisp bindings)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (make-bindings
            with-lexical-bindings
            with-dynamic-bindings
            with-function-bindings
            get-lexical-binding
            get-function-binding))

;;; This module defines routines to handle analysis of symbol bindings
;;; used during elisp compilation.  This data allows to collect the
;;; symbols, for which globals need to be created, or mark certain
;;; symbols as lexically bound.
;;;
;;; The lexical bindings of symbols are stored in a hash-table that
;;; associates symbols to fluids; those fluids are used in the
;;; with-lexical-binding and with-dynamic-binding routines to associate
;;; symbols to different bindings over a dynamic extent.

;;; Record type used to hold the data necessary.

(define-record-type bindings
  (%make-bindings lexical-bindings function-bindings)
  bindings?
  (lexical-bindings lexical-bindings)
  (function-bindings function-bindings))

;;; Construct an 'empty' instance of the bindings data structure to be
;;; used at the start of a fresh compilation.

(define (make-bindings)
  (%make-bindings (make-hash-table) (make-hash-table)))

;;; Get the current lexical binding (gensym it should refer to in the
;;; current scope) for a symbol or #f if it is dynamically bound.

(define (get-lexical-binding bindings sym)
  (let* ((lex (lexical-bindings bindings))
         (slot (hash-ref lex sym #f)))
    (if slot
        (fluid-ref slot)
        #f)))

(define (get-function-binding bindings symbol)
  (and=> (hash-ref (function-bindings bindings) symbol)
         fluid-ref))

;;; Establish a binding or mark a symbol as dynamically bound for the
;;; extent of calling proc.

(define (with-symbol-bindings bindings syms targets proc)
  (if (or (not (list? syms))
          (not (and-map symbol? syms)))
      (error "can't bind non-symbols" syms))
  (let ((lex (lexical-bindings bindings)))
    (for-each (lambda (sym)
                (if (not (hash-ref lex sym))
                    (hash-set! lex sym (make-fluid))))
              syms)
    (with-fluids* (map (lambda (sym) (hash-ref lex sym)) syms)
                  targets
                  proc)))

(define (with-lexical-bindings bindings syms targets proc)
  (if (or (not (list? targets))
          (not (and-map symbol? targets)))
      (error "invalid targets for lexical binding" targets)
      (with-symbol-bindings bindings syms targets proc)))

(define (with-dynamic-bindings bindings syms proc)
  (with-symbol-bindings bindings
                        syms
                        (map (lambda (el) #f) syms)
                        proc))

(define (with-function-bindings bindings symbols gensyms thunk)
  (let ((fb (function-bindings bindings)))
    (for-each (lambda (symbol)
                (if (not (hash-ref fb symbol))
                    (hash-set! fb symbol (make-fluid))))
              symbols)
    (with-fluids* (map (cut hash-ref fb <>) symbols)
                  gensyms
                  thunk)))
