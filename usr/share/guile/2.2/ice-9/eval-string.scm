;;; Evaluating code from users

;;; Copyright (C) 2011, 2013 Free Software Foundation, Inc.

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

(define-module (ice-9 eval-string)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (system vm program)
  #:use-module (system vm loader)
  #:replace (eval-string))

(define (ensure-language x)
  (if (language? x)
      x
      (lookup-language x)))

(define* (read-and-eval port #:key (lang (current-language)))
  (parameterize ((current-language (ensure-language lang)))
    (define (read)
      ((language-reader (current-language)) port (current-module)))
    (define (eval exp)
      ((language-evaluator (current-language)) exp (current-module)))
            
    (let ((exp (read)))
      (if (eof-object? exp)
          ;; The behavior of read-and-compile and of the old
          ;; eval-string.
          *unspecified*
          (let lp ((exp exp))
            (call-with-values
                (lambda () (eval exp))
              (lambda vals
                (let ((next (read)))
                  (cond
                   ((eof-object? next)
                    (apply values vals))
                   (else
                    (lp next)))))))))))

(define* (eval-string str #:key
                      (module (current-module))
                      (file #f)
                      (line #f)
                      (column #f)
                      (lang (current-language))
                      (compile? #f))
  (define (maybe-with-module module thunk)
    (if module
        (save-module-excursion
         (lambda ()
           (set-current-module module)
           (thunk)))
        (thunk)))

  (let ((lang (ensure-language lang)))
    (call-with-input-string
     str
     (lambda (port)
       (maybe-with-module
        module
        (lambda ()
          (if module
              (set-current-module module))
          (if file
              (set-port-filename! port file))
          (if line
              (set-port-line! port line))
          (if column
              (set-port-column! port line))

          (if (or compile? (not (language-evaluator lang)))
              ((load-thunk-from-memory
                (read-and-compile port #:from lang #:to 'bytecode)))
              (read-and-eval port #:lang lang))))))))
