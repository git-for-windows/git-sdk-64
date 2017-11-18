;;;; Copyright (C) 2003, 2005, 2006, 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
;;;;
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
;;;;

(define-module (ice-9 deprecated)
  #:use-module ((ice-9 threads) #:prefix threads:))

(define-syntax-rule (define-deprecated var msg exp)
  (begin
    (define-syntax var
      (lambda (x)
        (issue-deprecation-warning msg)
        (syntax-case x ()
          ((id arg (... ...)) #'(let ((x id)) (x arg (... ...))))
          (id (identifier? #'id) #'exp))))
    (export var)))

(define-deprecated _IONBF
  "`_IONBF' is deprecated.  Use the symbol 'none instead."
  'none)
(define-deprecated _IOLBF
  "`_IOLBF' is deprecated.  Use the symbol 'line instead."
  'line)
(define-deprecated _IOFBF
  "`_IOFBF' is deprecated.  Use the symbol 'block instead."
  'block)

(define-syntax define-deprecated/threads
  (lambda (stx)
    (define (threads-name id)
      (datum->syntax id (symbol-append 'threads: (syntax->datum id))))
    (syntax-case stx ()
      ((_ name)
       (with-syntax ((name* (threads-name #'name))
                     (warning (string-append
                               "Import (ice-9 threads) to have access to `"
                               (symbol->string (syntax->datum #'name)) "'.")))
         #'(define-deprecated name warning name*))))))

(define-syntax-rule (define-deprecated/threads* name ...)
  (begin (define-deprecated/threads name) ...))

(define-deprecated/threads*
  call-with-new-thread
  yield
  cancel-thread
  join-thread
  thread?
  make-mutex
  make-recursive-mutex
  lock-mutex
  try-mutex
  unlock-mutex
  mutex?
  mutex-owner
  mutex-level
  mutex-locked?
  make-condition-variable
  wait-condition-variable
  signal-condition-variable
  broadcast-condition-variable
  condition-variable?
  current-thread
  all-threads
  thread-exited?
  total-processor-count
  current-processor-count)

(define-public make-dynamic-state
  (case-lambda
    (()
     (issue-deprecation-warning
      "`(make-dynamic-state)' is deprecated; use `(current-dynamic-state)'
instead.")
     (current-dynamic-state))
    ((parent)
     (issue-deprecation-warning
      "`(make-dynamic-state PARENT)' is deprecated; now that reified
dynamic state objects are themselves copies, just use PARENT directly.")
     parent)))
