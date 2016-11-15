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
            reference-variable
            set-variable!
            runtime-error
            macro-error)
  #:export-syntax (built-in-func built-in-macro defspecial prim))

;;; This module provides runtime support for the Elisp front-end.

;;; Values for t and nil. (FIXME remove this abstraction)

(define nil-value #nil)

(define t-value #t)

;;; Modules for the binding slots.
;;; Note: Naming those value-slot and/or function-slot clashes with the
;;; submodules of these names!

(define value-slot-module '(language elisp runtime value-slot))

(define function-slot-module '(language elisp runtime function-slot))

;;; Report an error during macro compilation, that means some special
;;; compilation (syntax) error; or report a simple runtime-error from a
;;; built-in function.

(define (macro-error msg . args)
  (apply error msg args))

(define runtime-error macro-error)

;;; Convert a scheme boolean to Elisp.

(define (elisp-bool b)
  (if b
      t-value
      nil-value))

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

(define (reference-variable module sym)
  (let ((resolved (resolve-module module)))
   (cond
    ((equal? module function-slot-module)
     (module-ref resolved sym))
    (else
     (ensure-fluid! module sym)
     (fluid-ref (module-ref resolved sym))))))

(define (set-variable! module sym value)
  (let ((intf (resolve-interface module))
        (resolved (resolve-module module)))
    (cond
     ((equal? module function-slot-module)
      (cond
       ((module-defined? intf sym)
        (module-set! resolved sym value))
      (else
       (module-define! resolved sym value)
       (module-export! resolved `(,sym)))))
    (else
     (ensure-fluid! module sym)
     (fluid-set! (module-ref resolved sym) value))))
  value)

;;; Define a predefined function or predefined macro for use in the
;;; function-slot and macro-slot modules, respectively.

(define-syntax built-in-func
  (syntax-rules ()
    ((_ name value)
     (begin
       (define-public name value)))))

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

(define-syntax built-in-macro
  (lambda (x)
    (syntax-case x ()
      ((_ name value)
       (with-syntax ((scheme-name (make-id #'name 'macro- #'name)))
        #'(begin
            (define-public scheme-name
              (make-fluid (cons 'macro value)))))))))

(define-syntax defspecial
  (lambda (x)
    (syntax-case x ()
      ((_ name args body ...)
       (with-syntax ((scheme-name (make-id #'name 'compile- #'name)))
         #'(begin
             (define scheme-name
               (make-fluid
                (cons 'special-operator
                      (lambda args body ...))))))))))

;;; Call a guile-primitive that may be rebound for elisp and thus needs
;;; absolute addressing.

(define-syntax prim
  (syntax-rules ()
    ((_ sym args ...)
     ((@ (guile) sym) args ...))))
