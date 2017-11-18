;;; simple.scm --- The R6RS simple I/O library

;;      Copyright (C) 2010, 2011, 2014 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(library (rnrs io simple (6))
  (export eof-object 
          eof-object?

	  call-with-input-file
	  call-with-output-file
	  
	  input-port?
	  output-port?

	  current-input-port
	  current-output-port
	  current-error-port

	  with-input-from-file
	  with-output-to-file

	  open-input-file
	  open-output-file

	  close-input-port
	  close-output-port

	  read-char
	  peek-char
	  read
	  write-char
	  newline
	  display
	  write

	  &i/o make-i/o-error i/o-error?
	  &i/o-read make-i/o-read-error i/o-read-error?
	  &i/o-write make-i/o-write-error i/o-write-error?

	  &i/o-invalid-position 
	  make-i/o-invalid-position-error 
	  i/o-invalid-position-error? 
	  i/o-error-position
	  
	  &i/o-filename
	  make-i/o-filename-error
	  i/o-filename-error?
	  i/o-error-filename
	  
	  &i/o-file-protection 
	  make-i/o-file-protection-error
	  i/o-file-protection-error?

	  &i/o-file-is-read-only
	  make-i/o-file-is-read-only-error
	  i/o-file-is-read-only-error?

	  &i/o-file-already-exists
	  make-i/o-file-already-exists-error
	  i/o-file-already-exists-error?

	  &i/o-file-does-not-exist
	  make-i/o-file-does-not-exist-error
	  i/o-file-does-not-exist-error?

	  &i/o-port
	  make-i/o-port-error
	  i/o-port-error?
	  i/o-error-port)	  

  (import (only (rnrs io ports)
                call-with-port
                close-port
                open-file-input-port
                open-file-output-port
                eof-object 
                eof-object?
                file-options
                buffer-mode
                native-transcoder
                get-char
                lookahead-char
                get-datum
                put-char
                put-datum

                input-port? 
                output-port?)
          (only (guile)
                @@
                current-input-port
                current-output-port
                current-error-port

                define*

                with-input-from-port
                with-output-to-port)
	  (rnrs base (6))
          (rnrs files (6)) ;for the condition types
          )

  (define display (@@ (rnrs io ports) display))

  (define (call-with-input-file filename proc)
    (call-with-port (open-file-input-port filename) proc))

  (define (call-with-output-file filename proc)
    (call-with-port (open-file-output-port filename) proc))

  (define (with-input-from-file filename thunk)
    (call-with-input-file filename
      (lambda (port) (with-input-from-port port thunk))))

  (define (with-output-to-file filename thunk)
    (call-with-output-file filename
      (lambda (port) (with-output-to-port port thunk))))

  (define (open-input-file filename)
    (open-file-input-port filename
                          (file-options)
                          (buffer-mode block)
                          (native-transcoder)))

  (define (open-output-file filename)
    (open-file-output-port filename
                           (file-options)
                           (buffer-mode block)
                           (native-transcoder)))

  (define close-input-port close-port)
  (define close-output-port close-port)

  (define* (read-char #:optional (port (current-input-port)))
    (get-char port))

  (define* (peek-char #:optional (port (current-input-port)))
    (lookahead-char port))

  (define* (read #:optional (port (current-input-port)))
    (get-datum port))

  (define* (write-char char #:optional (port (current-output-port)))
    (put-char port char))

  (define* (newline #:optional (port (current-output-port)))
    (put-char port #\newline))

  (define* (write object #:optional (port (current-output-port)))
    (put-datum port object))

  )
