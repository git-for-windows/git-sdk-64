;;; "Soft" ports
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
;;; Implementation of legacy soft-port interface.
;;;
;;; Code:


(define-module (ice-9 soft-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 custom-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs bytevectors gnu)
  #:export (deprecated-make-soft-port)
  #:replace (make-soft-port))

(define (type-error proc expecting val)
  (scm-error 'wrong-type-arg proc "Wrong type (expecting `~S'): ~S"
             (list expecting val) (list val)))

(define (deprecated-soft-port-read %get-char)
  (unless (procedure? %get-char)
    (type-error "deprecated-soft-port-read" "procedure" %get-char))
  (define encode-buf-size 10)
  (define buffer (make-bytevector encode-buf-size))
  (define buffer-pos 0)
  (define buffer-len 0)
  (define transcoder
    (make-custom-binary-output-port
     "transcoder"
     (lambda (bv start count)
       (let ((to-copy (min encode-buf-size count)))
         (bytevector-copy! bv start buffer 0 to-copy)
         (set! buffer-pos 0)
         (set! buffer-len to-copy)
         to-copy))
     #f #f #f))
  (lambda (port bv start count)
    (let lp ((start start) (count count) (ret 0))
      (unless (< buffer-pos buffer-len)
        (match (%get-char)
          ((or #f (? eof-object?)) ret)
          (ch
           (unless (eq? (port-encoding port) (port-encoding transcoder))
             (set-port-encoding! transcoder (port-encoding port)))
           (unless (eq? (port-conversion-strategy port)
                        (port-conversion-strategy transcoder))
             (set-port-conversion-strategy! transcoder
                                            (port-conversion-strategy port)))
           (put-char transcoder ch)
           (force-output transcoder))))
      (let ((to-copy (min count (- buffer-len buffer-pos))))
        (bytevector-copy! buffer buffer-pos bv start to-copy)
        (set! buffer-pos (+ buffer-pos to-copy))
        to-copy))))

(define (deprecated-soft-port-write %put-string %flush)
  (unless (procedure? %put-string)
    (type-error "deprecated-soft-port-write" "procedure" %put-string))
  (when %flush
    (unless (procedure? %flush)
      (type-error "deprecated-soft-port-write" "procedure" %flush)))
  (lambda (port bv start count)
    (let* ((bytes (bytevector-slice bv start count))
           (str (call-with-input-bytevector
                 bytes
                 (lambda (bport)
                   (set-port-encoding! bport (port-encoding port))
                   (set-port-conversion-strategy!
                    bport
                    (port-conversion-strategy port))
                   (get-string-all bport)))))
      (%put-string str)
      (if %flush (%flush))
      count)))

(define (deprecated-soft-port-close %close)
  (unless (procedure? %close)
    (type-error "soft-port-close" "procedure" %close))
  (lambda (port) (%close)))

(define (deprecated-soft-port-input-waiting? %input-ready)
  (unless (procedure? %input-ready)
    (type-error "deprecated-soft-port-close" "procedure" %input-ready))
  (lambda (port) (< 0 (%input-ready))))

(define (%deprecated-make-soft-port %put-char %put-string %flush %get-char
                                    %close %input-ready
                                    reading? writing? buffering)
  (cond
   ((not (or reading? writing?))
    (%make-void-port ""))
   (else
    (let ((port
           (make-custom-port
            #:id "soft-port"
            #:read (and reading? (deprecated-soft-port-read %get-char))
            #:write (and writing? (deprecated-soft-port-write %put-string %flush))
            #:seek (lambda (port offset whence)
                     (error "soft ports are not seekable"))
            #:close (and %close
                         (deprecated-soft-port-close %close))
            #:get-natural-buffer-sizes (lambda (port read-size write-size)
                                         ;; The in-practice expectation
                                         ;; is that soft ports have
                                         ;; unbuffered output.
                                         (values read-size 1))
            #:random-access? (lambda (port) #f)
            #:input-waiting? (if %input-ready
                                 (deprecated-soft-port-input-waiting? %input-ready)
                                 (lambda (port) #t))
            #:close-on-gc? #t)))
      (when buffering
        (setvbuf port buffering))
      port))))

(define (deprecated-make-soft-port vtable modes)
  "Return a port capable of receiving or delivering characters as
specified by the @var{modes} string (@pxref{File Ports, open-file}).
@var{pv} must be a vector of length 5 or 6.  Its components are as
follows:

@enumerate 0
@item
procedure accepting one character for output
@item
procedure accepting a string for output
@item
thunk for flushing output
@item
thunk for getting one character
@item
thunk for closing port (not by garbage collection)
@item
(if present and not @code{#f}) thunk for computing the number of
characters that can be read from the port without blocking.  @end
enumerate

For an output-only port only elements 0, 1, 2, and 4 need be procedures.
For an input-only port only elements 3 and 4 need be procedures.  Thunks
2 and 4 can instead be @code{#f} if there is no useful operation for
them to perform.

If thunk 3 returns @code{#f} or an @code{eof-object}
(@pxref{Input, eof-object?, ,r5rs, The Revised^5 Report on
Scheme}) it indicates that the port has reached end-of-file.
For example:

@lisp
(define stdout (current-output-port))
(define p (make-soft-port
           (vector
            (lambda (c) (write c stdout))
            (lambda (s) (display s stdout))
            (lambda () (display \".\" stdout))
            (lambda () (char-upcase (read-char)))
            (lambda () (display \"@@\" stdout)))
           \"rw\"))

(write p p) @result{} #<input-output: soft 8081e20>
@end lisp"
  (define reading?
    (or (string-index modes #\r)
        (string-index modes #\+)))
  (define writing?
    (or (string-index modes #\w)
        (string-index modes #\a)
        (string-index modes #\+)))
  (define buffering
    (and writing?
         (cond
          ((string-index modes #\0) 'none)
          ((string-index modes #\l) 'line)
          (else #f))))
  (match vtable
    (#(%put-char %put-string %flush %get-char %close)
     (%deprecated-make-soft-port %put-char %put-string %flush %get-char %close
                                 #f reading? writing? buffering))
    (#(%put-char %put-string %flush %get-char %close %chars-waiting)
     (%deprecated-make-soft-port %put-char %put-string %flush %get-char %close
                                 %chars-waiting reading? writing? buffering))))

(define (soft-port-read read-string)
  (unless (procedure? read-string)
    (type-error "soft-port-read" "procedure" read-string))
  (define-values (transcoder get-bytes) (open-bytevector-output-port))
  (define buffer #f)
  (define buffer-pos 0)
  (lambda (port bv start count)
    (unless (and buffer (< buffer-pos (bytevector-length buffer)))
      (let* ((str (read-string)))
        (unless (eq? (port-encoding port) (port-encoding transcoder))
          (set-port-encoding! transcoder (port-encoding port)))
        (unless (eq? (port-conversion-strategy port)
                     (port-conversion-strategy transcoder))
          (set-port-conversion-strategy! transcoder
                                         (port-conversion-strategy port)))
        (put-string transcoder str)
        (set! buffer (get-bytes))
        (set! buffer-pos 0)))

    (let ((to-copy (min count (- (bytevector-length buffer) buffer-pos))))
      (bytevector-copy! buffer buffer-pos bv start to-copy)
      (if (= (bytevector-length buffer) (+ buffer-pos to-copy))
          (set! buffer #f)
          (set! buffer-pos (+ buffer-pos to-copy)))
      to-copy)))

(define (soft-port-write write-string)
  (unless (procedure? write-string)
    (type-error "soft-port-write" "procedure" write-string))
  (lambda (port bv start count)
    (write-string
     (call-with-input-bytevector
      (bytevector-slice bv start count)
      (lambda (bport)
        (set-port-encoding! bport (port-encoding port))
        (set-port-conversion-strategy!
         bport
         (port-conversion-strategy port))
        (get-string-all bport))))
    count))

(define* (make-soft-port #:key
                         (id "soft-port")
                         (read-string #f)
                         (write-string #f)
                         (input-waiting? #f)
                         (close #f)
                         (close-on-gc? #f))
  "Return a new port.  If the @var{read-string} keyword argument is
present, the port will be an input port.  If @var{write-string} is
present, the port will be an output port.  If both are supplied, the
port will be open for input and output.

When the port's internal buffers are empty, @var{read-string} will be
called with no arguments, and should return a string.  Returning \"\"
indicates end-of-stream.  Similarly when a port flushes its write
buffer, the characters in that buffer will be passed to the
@var{write-string} procedure as its single argument.  @var{write-string}
returns unspecified values.

If supplied, @var{input-waiting?} should return @code{#t} if the soft
port has input which would be returned directly by @var{read-string}.

If supplied, @var{close} will be called when the port is closed, with no
arguments.  If @var{close-on-gc?} is @code{#t}, @var{close} will
additionally be called when the port becomes unreachable, after flushing
any pending write buffers."
  (unless (or read-string write-string)
    (error "Expected at least one of #:read-string, #:write-string"))
  (when (and input-waiting? (not read-string))
    (error "Supplying #:input-waiting? requires a #:read-string"))
  (make-custom-port
   #:id id
   #:read (and read-string (soft-port-read read-string))
   #:write (and write-string (soft-port-write write-string))
   #:close (and close (lambda (port) (close)))
   #:input-waiting? (and input-waiting?
                         (lambda (port) (input-waiting?)))
   #:close-on-gc? close-on-gc?
   #:encoding 'UTF-8))
