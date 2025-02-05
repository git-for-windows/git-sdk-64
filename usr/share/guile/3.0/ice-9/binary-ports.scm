;;; binary-ports.scm --- Binary IO on ports
;;; Copyright (C) 2009-2011,2013,2016,2019,2021,2023,2024 Free Software Foundation, Inc.
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

;;; Author: Ludovic Courtès <ludo@gnu.org>

;;; Commentary:
;;;
;;; The I/O port API of the R6RS is provided by this module.  In many areas
;;; it complements or refines Guile's own historical port API.  For instance,
;;; it allows for binary I/O with bytevectors.
;;;
;;; Code:

(define-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:autoload   (rnrs bytevectors gnu) (bytevector-slice)
  #:use-module (ice-9 match)
  #:use-module (ice-9 custom-ports)
  #:export (eof-object
            open-bytevector-input-port
            open-bytevector-output-port
            get-u8
            lookahead-u8
            get-bytevector-n
            get-bytevector-n!
            get-bytevector-some
            get-bytevector-some!  ; Guile extension, not in R6RS
            get-bytevector-all
            get-string-n!
            put-u8
            put-bytevector
            unget-bytevector
            make-custom-binary-input-port
            make-custom-binary-output-port
            make-custom-binary-input/output-port
            call-with-input-bytevector
            call-with-output-bytevector))

;; Note that this extension also defines %make-transcoded-port, which is
;; not exported but is used by (rnrs io ports).

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_r6rs_ports")

(define (call-with-input-bytevector bv proc)
  "Call the one-argument procedure @var{proc} with a newly created
binary input port from which the bytevector @var{bv}'s contents may be
read.  All values yielded by @var{proc} are returned."
  (proc (open-bytevector-input-port bv)))

(define (call-with-output-bytevector proc)
  "Call the one-argument procedure @var{proc} with a newly created
binary output port.  When the function returns, port is closed and the
bytevector composed of the bytes written into the port is returned."
  (call-with-values
      (lambda ()
        (open-bytevector-output-port))
    (lambda (port get-bytevector)
      (proc port)
      (let ((bv (get-bytevector)))
        (close-port port)
        bv))))

(define (type-error proc expecting val)
  (scm-error 'wrong-type-arg proc "Wrong type (expecting `~S'): ~S"
             (list expecting val) (list val)))

(define (custom-binary-port-read read)
  (unless (procedure? read)
    (type-error "custom-binary-port-read" "procedure" read))
  (lambda (port bv start count)
    (let ((ret (read bv start count)))
      (unless (and (exact-integer? ret) (<= 0 ret count))
        (scm-error 'out-of-range "custom-binary-port-read"
                   "Value out of range: ~S" (list ret) (list ret)))
      ret)))

(define (custom-binary-port-write write)
  (unless (procedure? write)
    (type-error "custom-binary-port-write" "procedure" write))
  (lambda (port bv start count)
    (let ((ret (write bv start count)))
      (unless (and (exact-integer? ret) (<= 0 ret count))
        (scm-error 'out-of-range "custom-binary-port-write"
                   "Value out of range: ~S" (list ret) (list ret)))
      ret)))

(define (custom-binary-port-seek get-position set-position!)
  (when get-position
    (unless (procedure? get-position)
      (type-error "custom-binary-port-seek" "procedure" get-position)))
  (when set-position!
    (unless (procedure? set-position!)
      (type-error "custom-binary-port-seek" "procedure" set-position!)))

  (define (seek port offset whence)
    (cond
     ((eqv? whence SEEK_CUR)
      (unless get-position
        (type-error "custom-binary-port-seek"
                    "R6RS custom binary port with `port-position` support"
                    port))
      (if (zero? offset)
          (get-position)
          (seek port (+ (get-position) offset) SEEK_SET)))
     ((eqv? whence SEEK_SET)
      (unless set-position!
        (type-error "custom-binary-port-seek"
                    "Seekable R6RS custom binary port"
                    port))
      (set-position! offset)
      ;; Assume setting the position succeeds.
      offset)
     ((eqv? whence SEEK_END)
      (error "R6RS custom binary ports do not support `SEEK_END'"))))
  seek)

(define (custom-binary-port-close close)
  (match close
    (#f (lambda (port) #t))
    ((? procedure?) (lambda (port) (close)))
    (_ (type-error "custom-binary-port-close" "procedure" close))))

(define (custom-binary-port-random-access? set-position!)
  (if set-position!
      (lambda (port) #t)
      (lambda (port) #f)))

(define (make-custom-binary-input-port id read get-position set-position! close)
  (unless (string? id)
    (type-error "make-custom-binary-input-port" "string" id))
  (make-custom-port #:id id
                    #:read (custom-binary-port-read read)
                    #:seek (custom-binary-port-seek get-position set-position!)
                    #:close (custom-binary-port-close close)
                    #:random-access?
                    (custom-binary-port-random-access? set-position!)
                    ;; FIXME: Instead default to current encoding, if
                    ;; someone reads text from this port.
                    #:encoding 'ISO-8859-1 #:conversion-strategy 'error))

(define (make-custom-binary-output-port id write get-position set-position!
                                        close)
  (unless (string? id)
    (type-error "make-custom-binary-output-port" "string" id))
  (make-custom-port #:id id
                    #:write (custom-binary-port-write write)
                    #:seek (custom-binary-port-seek get-position set-position!)
                    #:close (custom-binary-port-close close)
                    #:random-access?
                    (custom-binary-port-random-access? set-position!)
                    ;; FIXME: Instead default to current encoding, if
                    ;; someone reads text from this port.
                    #:encoding 'ISO-8859-1 #:conversion-strategy 'error))

(define (make-custom-binary-input/output-port id read write get-position
                                              set-position! close)
  (unless (string? id)
    (type-error "make-custom-binary-input/output-port" "string" id))
  (make-custom-port #:id id
                    #:read (custom-binary-port-read read)
                    #:write (custom-binary-port-write write)
                    #:seek (custom-binary-port-seek get-position set-position!)
                    #:close (custom-binary-port-close close)
                    #:random-access?
                    (custom-binary-port-random-access? set-position!)
                    ;; FIXME: Instead default to current encoding, if
                    ;; someone reads text from this port.
                    #:encoding 'ISO-8859-1 #:conversion-strategy 'error))


;;;
;;; Binary input.
;;;

(define (get-bytevector-all port)
  "Read from @var{port}, blocking as necessary, until
the end-of-file is reached.  Return either a new bytevector containing
the data read or the end-of-file object (if no data were available)."
  (define initial-capacity 4096)

  (let loop ((bv (make-bytevector initial-capacity))
             (capacity initial-capacity)
             (size 0))
    (match (get-bytevector-n! port bv size (- capacity size))
      ((? eof-object?)
       (bytevector-slice bv 0 size))
      (read
       (let ((size (+ read size)))
         (if (= capacity size)
             (let* ((capacity (* capacity 2))
                    (new (make-bytevector capacity)))
               (bytevector-copy! bv 0 new 0 size)
               (loop new capacity size))
             (loop bv capacity size)))))))
