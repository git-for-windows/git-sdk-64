;;; Language interface for Wisp in Guile

;; Copyright (C) 2005--2014 by David A. Wheeler and Alan Manuel K. Gloria
;; Copyright (C) 2014--2023 Arne Babenhauserheide.
;; Copyright (C) 2023 Maxime Devos <maximedevos@telenet.be>

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

;; adapted from spec.scm: https://gitorious.org/nacre/guile-sweet/?p=nacre:guile-sweet.git;a=blob;f=sweet/spec.scm;hb=ae306867e371cb4b56e00bb60a50d9a0b8353109

(define-module (language wisp spec)
  #:use-module (language wisp)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (language scheme compile-tree-il)
  #:use-module (language scheme decompile-tree-il)
  #:export (wisp))

;;;
;;; Language definition
;;;


(define (read-one-wisp-sexp port env)
  ;; Allow using "# foo" as #(foo).
  ;; Don't use the globally-acting read-hash-extend, because this
  ;; doesn't make much sense in parenthese-y (non-Wisp) Scheme.
  ;; Instead, use fluids to temporarily add the extension.
  (with-fluids ((%read-hash-procedures (fluid-ref %read-hash-procedures)))
    (read-hash-extend #\# (lambda args #\# ))
    ;; Read Wisp files as UTF-8, to support non-ASCII characters.
    ;; TODO: would be nice to support ';; coding: whatever' lines
    ;; like in parenthese-y Scheme.
    (set-port-encoding! port "UTF-8")
    (if (eof-object? (peek-char port))
        (read-char port) ; return eof: we’re done
        (let ((chunk (wisp-scheme-read-chunk port)))
          (and (not (null? chunk)) ; <---- XXX: maybe (pair? chunk)
               (car chunk))))))

(define-language wisp
  #:title "Wisp Scheme Syntax. See SRFI-119 for details"
  ;; . #:reader read-one-wisp-sexp
  #:reader read-one-wisp-sexp ; : lambda (port env) : let ((x (read-one-wisp-sexp port env))) (display x)(newline) x ;
  #:compilers `((tree-il . ,compile-tree-il))
  #:decompilers `((tree-il . ,decompile-tree-il))
  #:evaluator (lambda (x module) (primitive-eval x))
  #:printer write ; TODO: backtransform to Wisp? Use source-properties?
  #:make-default-environment
  (lambda ()
    ;; Ideally we'd duplicate the whole module hierarchy so that `set!',
    ;; `fluid-set!', etc. don't have any effect in the current environment.
    (let ((m (make-fresh-user-module)))
      ;; Provide a separate `current-reader' fluid so that
      ;; compile-time changes to `current-reader' are
      ;; limited to the current compilation unit.
      (module-define! m 'current-reader (make-fluid))
      m)))
