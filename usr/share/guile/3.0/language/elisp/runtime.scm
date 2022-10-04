;;; Guile Emacs Lisp

;;; Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
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

(define-module (language elisp runtime)
  #:export (nil-value
            t-value
            value-slot-module
            function-slot-module
            elisp-bool
            ensure-fluid!
            symbol-fluid
            set-symbol-fluid!
            symbol-value
            set-symbol-value!
            symbol-function
            set-symbol-function!
            symbol-bound?
            symbol-fbound?
            makunbound!
            fmakunbound!)
  #:export-syntax (defspecial prim))

;;; This module provides runtime support for the Elisp front-end.

;;; Values for t and nil. (FIXME remove this abstraction)

(define nil-value #nil)

(define t-value #t)

;;; Modules for the binding slots.
;;; Note: Naming those value-slot and/or function-slot clashes with the
;;; submodules of these names!

(define value-slot-module '(language elisp runtime value-slot))

(define function-slot-module '(language elisp runtime function-slot))

;;; Routines for access to elisp dynamically bound symbols.  This is
;;; used for runtime access using functions like symbol-value or set,
;;; where the symbol accessed might not be known at compile-time.  These
;;; always access the dynamic binding and can not be used for the
;;; lexical!

(define (ensure-fluid! module sym)
  (let ((intf (resolve-interface module))
        (resolved (resolve-module module)))
    (if (not (module-defined? intf sym))
        (let ((fluid (make-unbound-fluid)))
          (module-define! resolved sym fluid)
          (module-export! resolved `(,sym))))))

(define (symbol-fluid symbol)
  (let ((module (resolve-module value-slot-module)))
    (ensure-fluid! value-slot-module symbol) ;++ implicit special proclamation
    (module-ref module symbol)))

(define (set-symbol-fluid! symbol fluid)
  (let ((module (resolve-module value-slot-module)))
    (module-define! module symbol fluid)
    (module-export! module (list symbol)))
  fluid)

(define (symbol-value symbol)
  (fluid-ref (symbol-fluid symbol)))

(define (set-symbol-value! symbol value)
  (fluid-set! (symbol-fluid symbol) value)
  value)

(define (symbol-function symbol)
  (let ((module (resolve-module function-slot-module)))
    (module-ref module symbol)))

(define (set-symbol-function! symbol value)
  (let ((module (resolve-module function-slot-module)))
   (module-define! module symbol value)
   (module-export! module (list symbol)))
  value)

(define (symbol-bound? symbol)
  (and
   (module-bound? (resolve-interface value-slot-module) symbol)
   (let ((var (module-variable (resolve-module value-slot-module)
                               symbol)))
     (and (variable-bound? var)
          (if (fluid? (variable-ref var))
              (fluid-bound? (variable-ref var))
              #t)))))

(define (symbol-fbound? symbol)
  (and
   (module-bound? (resolve-interface function-slot-module) symbol)
   (variable-bound?
    (module-variable (resolve-module function-slot-module)
                     symbol))))

(define (makunbound! symbol)
  (if (module-bound? (resolve-interface value-slot-module) symbol)
      (let ((var (module-variable (resolve-module value-slot-module)
                                  symbol)))
        (if (and (variable-bound? var) (fluid? (variable-ref var)))
            (fluid-unset! (variable-ref var))
            (variable-unset! var))))
    symbol)

(define (fmakunbound! symbol)
  (if (module-bound? (resolve-interface function-slot-module) symbol)
      (variable-unset! (module-variable
                        (resolve-module function-slot-module)
                        symbol)))
  symbol)

;;; Define a predefined macro for use in the function-slot module.

(define (make-id template-id . data)
  (let ((append-symbols
         (lambda (symbols)
           (string->symbol
            (apply string-append (map symbol->string symbols))))))
    (datum->syntax template-id
                   (append-symbols
                    (map (lambda (datum)
                           ((if (identifier? datum)
                                syntax->datum
                                identity)
                            datum))
                         data)))))

(define-syntax defspecial
  (lambda (x)
    (syntax-case x ()
      ((_ name args body ...)
       (with-syntax ((scheme-name (make-id #'name 'compile- #'name)))
         #'(define scheme-name
             (cons 'special-operator (lambda args body ...))))))))
