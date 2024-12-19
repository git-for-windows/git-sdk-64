;;; User interface messages

;; Copyright (C) 2009-2012,2016,2018,2020-2021,2023 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; This module provide a simple interface to send messages to the user.
;;; TODO: Internationalize messages.
;;;
;;; Code:

(define-module (system base message)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (*current-warning-port*
            *current-warning-prefix*
            warning

            warning-type? warning-type-name warning-type-description
            warning-type-printer lookup-warning-type

            %warning-types))


;;;
;;; Source location
;;;

(define (location-string loc)
  (define (format-loc file line column)
    (format #f "~a:~a:~a"
            (or file "<stdin>")
            (1+ line)
            column))
  (match loc
    (#(file line column)
     (format-loc file line column))
    ((? pair? loc)
     (format-loc (assoc-ref loc 'filename)
                 (assoc-ref loc 'line)
                 (assoc-ref loc 'column)))
    (_ "<unknown-location>")))


;;;
;;; Warnings
;;;

;; This name existed before %current-warning-port was introduced, but
;; otherwise it is a deprecated binding.
(define *current-warning-port*
  ;; Can't play the identifier-syntax deprecation game in Guile 2.0, as
  ;; other modules might depend on this being a normal binding and not a
  ;; syntax binding.
  (parameter-fluid current-warning-port))

(define *current-warning-prefix*
  ;; Prefix string when emitting a warning.
  (make-fluid ";;; "))


(define-record-type <warning-type>
  (make-warning-type name description printer)
  warning-type?
  (name         warning-type-name)
  (description  warning-type-description)
  (printer      warning-type-printer))

(define %warning-types
  ;; List of known warning types.
  (map (lambda (args)
         (apply make-warning-type args))

       (let-syntax ((emit
                     (lambda (s)
                       (syntax-case s ()
                         ((_ port fmt args ...)
                          (string? (syntax->datum #'fmt))
                          (with-syntax ((fmt
                                         (string-append "~a"
                                                        (syntax->datum
                                                         #'fmt))))
                            #'(format port fmt
                                      (fluid-ref *current-warning-prefix*)
                                      args ...)))))))
         `((unsupported-warning ;; a "meta warning"
            "warn about unknown warning types"
            ,(lambda (port unused name)
               (emit port "warning: unknown warning type `~A'~%"
                     name)))

           (unused-variable
            "report unused variables"
            ,(lambda (port loc name)
               (emit port "~A: warning: unused variable `~A'~%"
                     loc name)))

           (unused-toplevel
            "report unused local top-level variables"
            ,(lambda (port loc name)
               (emit port "~A: warning: possibly unused local top-level variable `~A'~%"
                     loc name)))

           (unused-module
            "report unused modules"
            ,(lambda (port loc name definitely-unused?)
               (if definitely-unused?
                   (emit port "~A: warning: unused module ~a~%"
                         loc name)
                   (emit port "~A: warning: possibly unused module ~a~%"
                         loc name))))

           (shadowed-toplevel
            "report shadowed top-level variables"
            ,(lambda (port loc name previous-loc)
               (emit port "~A: warning: shadows previous definition of `~A' at ~A~%"
                     loc name
                     (location-string previous-loc))))

           (unbound-variable
            "report possibly unbound variables"
            ,(lambda (port loc name)
               (emit port "~A: warning: possibly unbound variable `~A'~%"
                     loc name)))

           (macro-use-before-definition
            "report possibly mis-use of macros before they are defined"
            ,(lambda (port loc name)
               (emit port "~A: warning: macro `~A' used before definition~%"
                     loc name)))

           (use-before-definition
            "report uses of top-levels before they are defined"
            ,(lambda (port loc name)
               (emit port "~A: warning: `~A' used before definition~%"
                     loc name)))

           (non-idempotent-definition
            "report names that can refer to imports on first load, but module definitions on second load"
            ,(lambda (port loc name)
               (emit port "~A: warning: non-idempotent binding for `~A'.  When first loaded, value for `~A` comes from imported binding, but later module-local definition overrides it; any module reload would capture module-local binding rather than import.~%"
                     loc name name)))

           (arity-mismatch
            "report procedure arity mismatches (wrong number of arguments)"
            ,(lambda (port loc name certain?)
               (if certain?
                   (emit port
                         "~A: warning: wrong number of arguments to `~A'~%"
                         loc name)
                   (emit port
                         "~A: warning: possibly wrong number of arguments to `~A'~%"
                         loc name))))

           (duplicate-case-datum
            "report a duplicate datum in a case expression"
            ,(lambda (port loc datum clause case-expr)
               (emit port
                     "~A: warning: duplicate datum ~S in clause ~S of case expression ~S~%"
                     loc datum clause case-expr)))

           (bad-case-datum
            "report a case datum that cannot be meaningfully compared using `eqv?'"
            ,(lambda (port loc datum clause case-expr)
               (emit port
                     "~A: warning: datum ~S cannot be meaningfully compared using `eqv?' in clause ~S of case expression ~S~%"
                     loc datum clause case-expr)))

           (format
            "report wrong number of arguments to `format'"
            ,(lambda (port loc . rest)
               (define (escape-newlines str)
                 (list->string
                  (string-fold-right (lambda (c r)
                                       (if (eq? c #\newline)
                                           (append '(#\\ #\n) r)
                                           (cons c r)))
                                     '()
                                     str)))

               (define (range min max)
                 (cond ((eq? min 'any)
                        (if (eq? max 'any)
                            "any number" ;; can't happen
                            (emit #f "up to ~a" max)))
                       ((eq? max 'any)
                        (emit #f "at least ~a" min))
                       ((= min max) (number->string min))
                       (else
                        (emit #f "~a to ~a" min max))))

               (match rest
                 (('simple-format fmt opt)
                  (emit port
                        "~A: warning: ~S: unsupported format option ~~~A, use (ice-9 format) instead~%"
                        loc (escape-newlines fmt) opt))
                 (('wrong-format-arg-count fmt min max actual)
                  (emit port
                        "~A: warning: ~S: wrong number of `format' arguments: expected ~A, got ~A~%"
                        loc (escape-newlines fmt)
                        (range min max) actual))
                 (('syntax-error 'unterminated-iteration fmt)
                  (emit port "~A: warning: ~S: unterminated iteration~%"
                        loc (escape-newlines fmt)))
                 (('syntax-error 'unterminated-conditional fmt)
                  (emit port "~A: warning: ~S: unterminated conditional~%"
                        loc (escape-newlines fmt)))
                 (('syntax-error 'unexpected-semicolon fmt)
                  (emit port "~A: warning: ~S: unexpected `~~;'~%"
                        loc (escape-newlines fmt)))
                 (('syntax-error 'unexpected-conditional-termination fmt)
                  (emit port "~A: warning: ~S: unexpected `~~]'~%"
                        loc (escape-newlines fmt)))
                 (('wrong-port wrong-port)
                  (emit port
                        "~A: warning: ~S: wrong port argument~%"
                        loc wrong-port))
                 (('wrong-format-string fmt)
                  (emit port
                        "~A: warning: ~S: wrong format string~%"
                        loc fmt))
                 (('non-literal-format-string)
                  (emit port
                        "~A: warning: non-literal format string~%"
                        loc))
                 (('wrong-num-args count)
                  (emit port
                        "~A: warning: wrong number of arguments to `format'~%"
                        loc))
                 (else
                  (emit port "~A: `format' warning~%" loc)))))))))

(define (lookup-warning-type name)
  "Return the warning type NAME or `#f' if not found."
  (find (lambda (wt)
          (eq? name (warning-type-name wt)))
        %warning-types))

(define (warning type location . args)
  "Emit a warning of type TYPE for source location LOCATION (a source
property alist) using the data in ARGS."
  (let ((wt   (lookup-warning-type type))
        (port (current-warning-port)))
    (if (warning-type? wt)
        (apply (warning-type-printer wt)
               port (location-string location)
               args)
        (format port "~A: unknown warning type `~A': ~A~%"
                (location-string location) type args))))
