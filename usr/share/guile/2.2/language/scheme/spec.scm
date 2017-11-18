;;; Guile Scheme specification

;; Copyright (C) 2001, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language scheme spec)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (language scheme compile-tree-il)
  #:use-module (language scheme decompile-tree-il)
  #:export (scheme))

;;;
;;; Language definition
;;;

(define-language scheme
  #:title	"Scheme"
  #:reader      (lambda (port env)
                  ;; Use the binding of current-reader from the environment.
                  ;; FIXME: Handle `read-options' as well?
                  ((or (and=> (and=> (module-variable env 'current-reader)
                                     variable-ref)
                              fluid-ref)
                       read)
                   port))

  #:compilers   `((tree-il . ,compile-tree-il))
  #:decompilers `((tree-il . ,decompile-tree-il))
  #:evaluator	(lambda (x module) (primitive-eval x))
  #:printer	write
  #:make-default-environment
                (lambda ()
                  ;; Ideally we'd duplicate the whole module hierarchy so that `set!',
                  ;; `fluid-set!', etc. don't have any effect in the current environment.
                  (let ((m (make-fresh-user-module)))
                    ;; Provide a separate `current-reader' fluid so that
                    ;; compile-time changes to `current-reader' are
                    ;; limited to the current compilation unit.
                    (module-define! m 'current-reader (make-fluid))

                    ;; Default to `simple-format', as is the case until
                    ;; (ice-9 format) is loaded.  This allows
                    ;; compile-time warnings to be emitted when using
                    ;; unsupported options.
                    (module-set! m 'format simple-format)

                    m)))
