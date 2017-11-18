;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;;;   2005, 2006, 2007, 2008, 2009, 2010, 2011, 2013 Free Software Foundation, Inc.
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

(define-module (ice-9 top-repl)
  #:use-module (ice-9 top-repl)
  #:use-module ((system repl repl) #:select (start-repl))

  ;; #:replace, as with deprecated code enabled these will be in the root env
  #:replace (top-repl))

(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk) (thunk))
      (lambda (thunk)
        (let ((handler #f))
          (dynamic-wind
            (lambda ()
              (set! handler
                    (sigaction SIGINT
                      (lambda (sig)
                        (scm-error 'signal #f "User interrupt" '()
                                   (list sig))))))
            thunk
            (lambda ()
              (if handler
                  ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                  (sigaction SIGINT (car handler) (cdr handler))
                  ;; restore original C handler.
                  (sigaction SIGINT #f))))))))

(define (top-repl)
  (let ((guile-user-module (resolve-module '(guile-user))))

    ;; Use some convenient modules (in reverse order)

    (set-current-module guile-user-module)
    (process-use-modules 
     (append
      '(((ice-9 r5rs))
        ((ice-9 session)))
      (if (provided? 'regex)
          '(((ice-9 regex)))
          '())
      (if (provided? 'threads)
          '(((ice-9 threads)))
          '())))

    (call-with-sigint
     (lambda ()
       (and (defined? 'setlocale)
            (catch 'system-error
              (lambda ()
                (setlocale LC_ALL ""))
              (lambda (key subr fmt args errno)
                (format (current-error-port)
                        "warning: failed to install locale: ~a~%"
                        (strerror (car errno))))))

       (let ((status (start-repl (current-language))))
         (run-hook exit-hook)
         status)))))
