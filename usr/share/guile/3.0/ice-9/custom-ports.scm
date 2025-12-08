;;; custom-ports.scm --- Defining new ports in Scheme
;;; Copyright (C) 2023 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Code:

(define-module (ice-9 custom-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-9)
  #:declarative? #f ; Because of extension.
  #:export (make-custom-port))

;; Replaced by extension; here just to suppress warnings.
(define %make-custom-port error)
(define %custom-port-data error)

(define-record-type <custom-port-data>
  (make-custom-port-data print read write read-wait-fd write-wait-fd
                         seek close get-natural-buffer-sizes
                         random-access? input-waiting? truncate)
  custom-port-data?
  (print custom-port-data-print)
  (read custom-port-data-read)
  (write custom-port-data-write)
  (read-wait-fd custom-port-data-read-wait-fd)
  (write-wait-fd custom-port-data-write-wait-fd)
  (seek custom-port-data-seek)
  (close custom-port-data-close)
  (get-natural-buffer-sizes custom-port-data-get-natural-buffer-sizes)
  (random-access? custom-port-data-random-access?)
  (input-waiting? custom-port-data-input-waiting?)
  (truncate custom-port-data-truncate))

(define-syntax define-custom-port-dispatcher
  (lambda (stx)
    (define (prefixed-name prefix suffix)
      (datum->syntax suffix (symbol-append prefix (syntax->datum suffix))))
    (syntax-case stx ()
      ((_ stem arg ...)
       (with-syntax ((accessor (prefixed-name 'custom-port-data- #'stem))
                     (dispatcher (prefixed-name 'custom-port- #'stem)))
         #'(define (dispatcher port data arg ...)
             ((accessor data) port arg ...)))))))

;; These bindings are captured by the extension.
(define (custom-port-read port bv start count)
  ((custom-port-data-read (%custom-port-data port)) port bv start count))
(define (custom-port-write port bv start count)
  ((custom-port-data-write (%custom-port-data port)) port bv start count))
(define-custom-port-dispatcher print out-port)
(define-custom-port-dispatcher read-wait-fd)
(define-custom-port-dispatcher write-wait-fd)
(define-custom-port-dispatcher seek offset whence)
(define-custom-port-dispatcher close)
(define-custom-port-dispatcher get-natural-buffer-sizes read-size write-size)
(define-custom-port-dispatcher random-access?)
(define-custom-port-dispatcher input-waiting?)
(define-custom-port-dispatcher truncate length)


(eval-when (expand load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_custom_ports"))

(define* (make-default-print #:key (id "custom-port"))
  (lambda (port out-port)
    (define mode
      (cond
       ((port-closed? port) "closed:")
       ((input-port? port) (if (output-port? port) "input-output:" "input:"))
       ((output-port? port) "output:")
       (else "bogus:")))
    (put-string out-port "#<")
    (put-string out-port mode)
    (put-string out-port id)
    (put-string out-port " ")
    (put-string out-port (number->string (object-address port) 16))
    (put-string out-port ">")))

(define (default-read-wait-fd port) #f)
(define (default-write-wait-fd port) #f)

(define (default-seek port offset whence)
  (error "custom port did not define a seek method" port))

(define (default-close port) (values))

(define (default-get-natural-buffer-sizes port read-buf-size write-buf-size)
  (values read-buf-size write-buf-size))

(define (make-default-random-access? seek)
  (if seek
      (lambda (port) #t)
      (lambda (port) #f)))

(define (default-input-waiting? port) #t)
(define (default-truncate port length)
  (error "custom port did not define a truncate method" port))

(define* (make-custom-port
          #:key
          read
          write
          (read-wait-fd default-read-wait-fd)
          (input-waiting? (and read default-input-waiting?))
          (write-wait-fd default-write-wait-fd)
          (seek #f)
          (random-access? #f)
          (close #f)
          (get-natural-buffer-sizes default-get-natural-buffer-sizes)
          (id "custom-port")
          (print (make-default-print #:id id))
          (truncate default-truncate)
          (encoding (and=> (fluid-ref %default-port-encoding) string->symbol))
          (conversion-strategy (fluid-ref %default-port-conversion-strategy))
          (close-on-gc? #f))
  "Create a custom port whose behavior is determined by the methods passed
as keyword arguments.  Supplying a @code{#:read} method will make an input
port, passing @code{#:write} will make an output port, and passing them
both will make an input/output port.

See the manual for full documentation on the semantics of these
methods."
  (define (canonicalize-encoding encoding)
    (match encoding
      (#f 'ISO-8859-1)
      ((or 'ISO-8859-1 'UTF-8
           'UTF-16 'UTF-16LE 'UTF-16BE
           'UTF-32 'UTF-32LE 'UTF-32BE) encoding)
      ((? symbol?)
       (string->symbol (string-upcase (symbol->string encoding))))))
  (define (canonicalize-conversion-strategy conversion-strategy)
    (match conversion-strategy
      ('escape 'escape)
      ('substitute 'substitute)
      (_ 'error)))
  (let ((seek (or seek default-seek))
        (close (or close default-close))
        (random-access? (or random-access?
                            (if seek (lambda (_) #t) (lambda (_) #f))))
        (close-on-gc? (and close close-on-gc?)))
    (define data
      (make-custom-port-data print read write read-wait-fd write-wait-fd
                             seek close get-natural-buffer-sizes
                             random-access? input-waiting? truncate))
    (unless (or read write)
      (error "Must have at least one I/O method (#:read and #:write)"))
    (%make-custom-port (->bool read) (->bool write) data
                       (canonicalize-encoding encoding)
                       (canonicalize-conversion-strategy conversion-strategy)
                       close-on-gc?)))
