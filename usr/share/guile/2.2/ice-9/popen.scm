;; popen emulation, for non-stdio based ports.

;;;; Copyright (C) 1998-2001, 2003, 2006, 2010-2013, 2019
;;;;   Free Software Foundation, Inc.
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

(define-module (ice-9 popen)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9)
  #:export (port/pid-table open-pipe* open-pipe close-pipe open-input-pipe
            open-output-pipe open-input-output-pipe))

(eval-when (expand load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_popen"))

(define-record-type <pipe-info>
  (make-pipe-info pid)
  pipe-info?
  (pid pipe-info-pid set-pipe-info-pid!))

(define (make-rw-port read-port write-port)
  (define (read! bv start count)
    (let ((result (get-bytevector-some! read-port bv start count)))
      (if (eof-object? result)
          0
          result)))

  (define (write! bv start count)
    (put-bytevector write-port bv start count)
    count)

  (define (close)
    (close-port read-port)
    (close-port write-port))

  (define rw-port
    (make-custom-binary-input/output-port "ice-9-popen-rw-port"
                                          read!
                                          write!
                                          #f ;get-position
                                          #f ;set-position!
                                          close))
  ;; Enable buffering on 'read-port' so that 'get-bytevector-some' will
  ;; return non-trivial blocks.
  (setvbuf read-port 'block 16384)

  ;; Inherit the port-encoding from the read-port.
  (set-port-encoding! rw-port (port-encoding read-port))

  ;; Reset the port encoding on the underlying ports to inhibit BOM
  ;; handling there.  Instead, the BOM handling (if any) will be handled
  ;; in the rw-port.  In the current implementation of Guile ports,
  ;; using binary I/O primitives alone is not enough to reliably inhibit
  ;; BOM handling, if the port encoding is set to UTF-{8,16,32}.
  (set-port-encoding! read-port "ISO-8859-1")
  (set-port-encoding! write-port "ISO-8859-1")

  rw-port)

;; a guardian to ensure the cleanup is done correctly when
;; an open pipe is gc'd or a close-port is used.
(define pipe-guardian (make-guardian))

;; a weak hash-table to store the process ids.
;; XXX use of this table is deprecated.  It is no longer used here, and
;; is populated for backward compatibility only (since it is exported).
(define port/pid-table (make-weak-key-hash-table 31))
(define port/pid-table-mutex (make-mutex))

(define (open-pipe* mode command . args)
  "Executes the program @var{command} with optional arguments
@var{args} (all strings) in a subprocess.
A port to the process (based on pipes) is created and returned.
@var{mode} specifies whether an input, an output or an input-output
port to the process is created: it should be the value of
@code{OPEN_READ}, @code{OPEN_WRITE} or @code{OPEN_BOTH}."
  (call-with-values (lambda ()
                      (apply open-process mode command args))
    (lambda (read-port write-port pid)
      (let ((port (or (and read-port write-port
                           (make-rw-port read-port write-port))
                      read-port
                      write-port
                      (%make-void-port mode)))
            (pipe-info (make-pipe-info pid)))

        ;; Guard the pipe-info instead of the port, so that we can still
        ;; call 'waitpid' even if 'close-port' is called (which clears
        ;; the port entry).
        (pipe-guardian pipe-info)
        (%set-port-property! port 'popen-pipe-info pipe-info)

        ;; XXX populate port/pid-table for backward compatibility.
        (with-mutex port/pid-table-mutex
          (hashq-set! port/pid-table port pid))

        port))))

(define (open-pipe command mode)
  "Executes the shell command @var{command} (a string) in a subprocess.
A port to the process (based on pipes) is created and returned.
@var{mode} specifies whether an input, an output or an input-output
port to the process is created: it should be the value of
@code{OPEN_READ}, @code{OPEN_WRITE} or @code{OPEN_BOTH}."
  (open-pipe* mode "/bin/sh" "-c" command))

(define (fetch-pipe-info port)
  (%port-property port 'popen-pipe-info))

(define (close-process port pid)
  (close-port port)
  (cdr (waitpid pid)))

(define (close-pipe p)
  "Closes the pipe created by @code{open-pipe}, then waits for the process
to terminate and returns its status value, @xref{Processes, waitpid}, for
information on how to interpret this value."
  (let ((pipe-info (fetch-pipe-info p)))
    (unless pipe-info
      (error "close-pipe: port not created by (ice-9 popen)"))
    (let ((pid (pipe-info-pid pipe-info)))
      (unless pid
        (error "close-pipe: pid has already been cleared"))
      ;; clear the pid to avoid repeated calls to 'waitpid'.
      (set-pipe-info-pid! pipe-info #f)
      (close-process p pid))))

(define (reap-pipes)
  (let loop ()
    (let ((pipe-info (pipe-guardian)))
      (when pipe-info
        (let ((pid (pipe-info-pid pipe-info)))
          ;; maybe 'close-pipe' was already called.
          (when pid
            ;; clean up without reporting errors.  also avoids blocking
            ;; the process: if the child isn't ready to be collected,
            ;; puts it back into the guardian's live list so it can be
            ;; tried again the next time the cleanup runs.
            (catch 'system-error
              (lambda ()
                (let ((pid/status (waitpid pid WNOHANG)))
                  (if (zero? (car pid/status))
                      (pipe-guardian pipe-info) ; not ready for collection
                      (set-pipe-info-pid! pipe-info #f))))
              (lambda args #f))))
        (loop)))))

(add-hook! after-gc-hook reap-pipes)

(define (open-input-pipe command)
  "Equivalent to @code{open-pipe} with mode @code{OPEN_READ}"
  (open-pipe command OPEN_READ))

(define (open-output-pipe command)
  "Equivalent to @code{open-pipe} with mode @code{OPEN_WRITE}"
  (open-pipe command OPEN_WRITE))

(define (open-input-output-pipe command)
  "Equivalent to @code{open-pipe} with mode @code{OPEN_BOTH}"
  (open-pipe command OPEN_BOTH))

