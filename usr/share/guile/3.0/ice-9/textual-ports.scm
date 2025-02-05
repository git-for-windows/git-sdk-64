;;;; textual-ports.scm --- Textual I/O on ports

;;;;	Copyright (C) 2016, 2023 Free Software Foundation, Inc.
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
  #:use-module (ice-9 custom-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs bytevectors gnu)
  #:re-export (get-string-n!
               put-char
               put-string)
  #:export (get-char
            unget-char
            unget-string
            lookahead-char
            get-string-n
            get-string-all
            get-line
            make-custom-textual-input-port
            make-custom-textual-output-port
            make-custom-textual-input/output-port))

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

(define (type-error proc expecting val)
  (scm-error 'wrong-type-arg proc "Wrong type (expecting `~S'): ~S"
             (list expecting val) (list val)))

(define (custom-textual-port-read+flush-input read)
  (unless (procedure? read)
    (type-error "custom-textual-port-read" "procedure" read))
  (define-values (transcoder get-bytes) (open-bytevector-output-port))
  (define buffer #f)
  (define buffer-pos 0)
  (define (%read port bv start count)
    (unless (and buffer (< buffer-pos (bytevector-length buffer)))
      (let* ((str (make-string (max (port-read-buffering port) 1)))
             (chars (read str 0 (string-length str))))
        (unless (and (exact-integer? chars) (<= 0 chars (string-length str)))
          (scm-error 'out-of-range "custom-textual-port-read"
                     "Value out of range: ~S" (list chars) (list chars)))
        (unless (eq? (port-encoding port) (port-encoding transcoder))
          (set-port-encoding! transcoder (port-encoding port)))
        (unless (eq? (port-conversion-strategy port)
                     (port-conversion-strategy transcoder))
          (set-port-conversion-strategy! transcoder
                                         (port-conversion-strategy port)))
        (put-string transcoder str 0 chars)
        (set! buffer (get-bytes))
        (set! buffer-pos 0)))

    (let ((to-copy (min count (- (bytevector-length buffer) buffer-pos))))
      (bytevector-copy! buffer buffer-pos bv start to-copy)
      (if (= (bytevector-length buffer) (+ buffer-pos to-copy))
          (set! buffer #f)
          (set! buffer-pos (+ buffer-pos to-copy)))
      to-copy))
  (define (%flush-input)
    (get-bytes)
    (set! buffer #f))
  (values %read %flush-input))

(define (custom-textual-port-write write)
  (unless (procedure? write)
    (type-error "custom-textual-port-write" "procedure" write))
  (lambda (port bv start count)
    (let* ((bytes (bytevector-slice bv start count))
           (str (call-with-input-bytevector
                 bytes
                 (lambda (bport)
                   (set-port-encoding! bport (port-encoding port))
                   (set-port-conversion-strategy!
                    bport
                    (port-conversion-strategy port))
                   (get-string-all bport))))
           (len (string-length str)))
      (let lp ((written 0))
        (cond
         ((= written len) count)
         (else
          (let ((to-write (- len written)))
            (let ((res (write str written to-write)))
              (unless (and (exact-integer? res) (<= 0 res to-write))
                (scm-error 'out-of-range "custom-textual-port-write"
                           "Value out of range: ~S" (list res) (list res)))
              (lp (+ written res))))))))))

(define (custom-textual-port-seek get-position set-position! flush-input)
  (when get-position
    (unless (procedure? get-position)
      (type-error "custom-textual-port-seek" "procedure" get-position)))
  (when set-position!
    (unless (procedure? set-position!)
      (type-error "custom-textual-port-seek" "procedure" set-position!)))

  (define (seek port offset whence)
    (cond
     ((eqv? whence SEEK_CUR)
      (unless get-position
        (type-error "custom-textual-port-seek"
                    "R6RS custom textual port with `port-position` support"
                    port))
      (if (zero? offset)
          (get-position)
          (seek port (+ (get-position) offset) SEEK_SET)))
     ((eqv? whence SEEK_SET)
      (unless set-position!
        (type-error "custom-textual-port-seek"
                    "Seekable R6RS custom textual port"
                    port))
      (flush-input)
      (set-position! offset)
      ;; Assume setting the position succeeds.
      offset)
     ((eqv? whence SEEK_END)
      (error "R6RS custom textual ports do not support `SEEK_END'"))))
  seek)

(define (custom-textual-port-close close)
  (match close
    (#f (lambda (port) #t))
    ((? procedure?) (lambda (port) (close)))
    (_ (type-error "custom-textual-port-close" "procedure" close))))

(define (custom-textual-port-random-access? set-position!)
  (if set-position!
      (lambda (port) #t)
      (lambda (port) #f)))

(define (make-custom-textual-input-port id read get-position set-position!
                                        close)
  (unless (string? id)
    (type-error "make-custom-textual-input-port" "string" id))
  (define-values (%read %flush-input)
    (custom-textual-port-read+flush-input read))
  (make-custom-port #:id id
                    #:read %read
                    #:seek (custom-textual-port-seek get-position set-position!
                                                     %flush-input)
                    #:close (custom-textual-port-close close)
                    #:random-access?
                    (custom-textual-port-random-access? set-position!)))

(define (make-custom-textual-output-port id write get-position set-position!
                                         close)
  (unless (string? id)
    (type-error "make-custom-textual-output-port" "string" id))
  (define (flush-input) #t)
  (make-custom-port #:id id
                    #:write (custom-textual-port-write write)
                    #:seek (custom-textual-port-seek get-position set-position!
                                                     flush-input)
                    #:close (custom-textual-port-close close)
                    #:random-access?
                    (custom-textual-port-random-access? set-position!)))

(define (make-custom-textual-input/output-port id read write get-position
                                               set-position! close)
  (unless (string? id)
    (type-error "make-custom-textual-input/output-port" "string" id))
  (define-values (%read %flush-input)
    (custom-textual-port-read+flush-input read))
  (make-custom-port #:id id
                    #:read %read
                    #:write (custom-textual-port-write write)
                    #:seek (custom-textual-port-seek get-position set-position!
                                                     %flush-input)
                    #:close (custom-textual-port-close close)
                    #:random-access?
                    (custom-textual-port-random-access? set-position!)))
