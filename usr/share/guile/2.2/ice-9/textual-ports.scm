;;;; textual-ports.scm --- Textual I/O on ports

;;;;	Copyright (C) 2016 Free Software Foundation, Inc.
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

;;; Commentary:
;;;
;;; Code:

(define-module (ice-9 textual-ports)
  #:use-module (ice-9 ports internal)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 rdelim)
  #:re-export (get-string-n!
               put-char
               put-string)
  #:export (get-char
            unget-char
            unget-string
            lookahead-char
            get-string-n
            get-string-all
            get-line))

(define (get-char port)
  (read-char port))

(define (lookahead-char port)
  (peek-char port))

(define (unget-char port char)
  (unread-char char port))

(define* (unget-string port string #:optional (start 0)
                       (count (- (string-length string) start)))
  (unread-string (if (and (zero? start)
                          (= count (string-length string)))
                     string
                     (substring/shared string start (+ start count)))
                 port))

(define (get-line port)
  (read-line port 'trim))

(define (get-string-all port)
  (read-string port))

(define (get-string-n port count)
  "Read up to @var{count} characters from @var{port}.
If no characters could be read before encountering the end of file,
return the end-of-file object, otherwise return a string containing
the characters read."
  (let* ((s (make-string count))
         (rv (get-string-n! port s 0 count)))
    (cond ((eof-object? rv) rv)
          ((= rv count)     s)
          (else             (substring/shared s 0 rv)))))
