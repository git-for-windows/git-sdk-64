;;; Repl server

;; Copyright (C)  2003, 2010, 2011, 2014, 2016 Free Software Foundation, Inc.

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

(define-module (system repl server)
  #:use-module (system repl repl)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)           ; cut
  #:export (make-tcp-server-socket
            make-unix-domain-server-socket
            run-server
            spawn-server
            stop-server-and-clients!))

;; List of pairs of the form (SOCKET . FORCE-CLOSE), where SOCKET is a
;; socket port, and FORCE-CLOSE is a thunk that forcefully shuts down
;; the socket.
(define *open-sockets* '())

(define sockets-lock (make-mutex))

;; WARNING: it is unsafe to call 'close-socket!' from another thread.
;; Note: although not exported, this is used by (system repl coop-server)
(define (close-socket! s)
  (with-mutex sockets-lock
    (set! *open-sockets* (assq-remove! *open-sockets* s)))
  ;; Close-port could block or raise an exception flushing buffered
  ;; output.  Hmm.
  (close-port s))

;; Note: although not exported, this is used by (system repl coop-server)
(define (add-open-socket! s force-close)
  (with-mutex sockets-lock
    (set! *open-sockets* (acons s force-close *open-sockets*))))

(define (stop-server-and-clients!)
  (cond
   ((with-mutex sockets-lock
      (match *open-sockets*
        (() #f)
        (((s . force-close) . rest)
         (set! *open-sockets* rest)
         force-close)))
    => (lambda (force-close)
         (force-close)
         (stop-server-and-clients!)))))

(define* (make-tcp-server-socket #:key
                          (host #f)
                          (addr (if host (inet-aton host) INADDR_LOOPBACK))
                          (port 37146))
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock AF_INET addr port)
    sock))

(define* (make-unix-domain-server-socket #:key (path "/tmp/guile-socket"))
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock AF_UNIX path)
    sock))

;; List of errno values from 'select' or 'accept' that should lead to a
;; retry in 'run-server'.
(define errs-to-retry
  (delete-duplicates
   (filter-map (lambda (name)
                 (and=> (module-variable the-root-module name)
                        variable-ref))
               '(EINTR EAGAIN EWOULDBLOCK))))

(define* (run-server #:optional (server-socket (make-tcp-server-socket)))
  (run-server* server-socket serve-client))

;; Note: although not exported, this is used by (system repl coop-server)
(define (run-server* server-socket serve-client)
  ;; We use a pipe to notify the server when it should shut down.
  (define shutdown-pipes      (pipe))
  (define shutdown-read-pipe  (car shutdown-pipes))
  (define shutdown-write-pipe (cdr shutdown-pipes))

  ;; 'shutdown-server' is called by 'stop-server-and-clients!'.
  (define (shutdown-server)
    (display #\!  shutdown-write-pipe)
    (force-output shutdown-write-pipe))

  (define monitored-ports
    (list server-socket
          shutdown-read-pipe))

  (define (accept-new-client)
    (catch #t
      (lambda ()
        (let ((ready-ports (car (select monitored-ports '() '()))))
          ;; If we've been asked to shut down, return #f.
          (and (not (memq shutdown-read-pipe ready-ports))
               (accept server-socket))))
      (lambda k-args
        (let ((err (system-error-errno k-args)))
          (cond
           ((memv err errs-to-retry)
            (accept-new-client))
           (else
            (warn "Error accepting client" k-args)
            ;; Retry after a timeout.
            (sleep 1)
            (accept-new-client)))))))

  ;; Put the socket into non-blocking mode.
  (fcntl server-socket F_SETFL
         (logior O_NONBLOCK
                 (fcntl server-socket F_GETFL)))

  (sigaction SIGPIPE SIG_IGN)
  (add-open-socket! server-socket shutdown-server)
  (listen server-socket 5)
  (let lp ((client (accept-new-client)))
    ;; If client is false, we are shutting down.
    (if client
        (let ((client-socket (car client))
              (client-addr (cdr client)))
          (make-thread serve-client client-socket client-addr)
          (lp (accept-new-client)))
        (begin (close shutdown-write-pipe)
               (close shutdown-read-pipe)
               (close server-socket)))))

(define* (spawn-server #:optional (server-socket (make-tcp-server-socket)))
  (make-thread run-server server-socket))

(define (serve-client client addr)

  (let ((thread (current-thread)))
    ;; Close the socket when this thread exits, even if canceled.
    (set-thread-cleanup! thread (lambda () (close-socket! client)))
    ;; Arrange to cancel this thread to forcefully shut down the socket.
    (add-open-socket! client (lambda () (cancel-thread thread))))

  (guard-against-http-request client)

  (with-continuation-barrier
   (lambda ()
     (parameterize ((current-input-port client)
                    (current-output-port client)
                    (current-error-port client)
                    (current-warning-port client))
       (with-fluids ((*repl-stack* '()))
         (start-repl))))))


;;;
;;; The following code adds protection to Guile's REPL servers against
;;; HTTP inter-protocol exploitation attacks, a scenario whereby an
;;; attacker can, via an HTML page, cause a web browser to send data to
;;; TCP servers listening on a loopback interface or private network.
;;; See <https://en.wikipedia.org/wiki/Inter-protocol_exploitation> and
;;; <https://www.jochentopf.com/hfpa/hfpa.pdf>, The HTML Form Protocol
;;; Attack (2001) by Tochen Topf <jochen@remote.org>.
;;;
;;; Here we add a procedure to 'before-read-hook' that looks for a possible
;;; HTTP request-line in the first line of input from the client socket.  If
;;; present, the socket is drained and closed, and a loud warning is written
;;; to stderr (POSIX file descriptor 2).
;;;

(define (with-temporary-port-encoding port encoding thunk)
  "Call THUNK in a dynamic environment in which the encoding of PORT is
temporarily set to ENCODING."
  (let ((saved-encoding #f))
    (dynamic-wind
      (lambda ()
        (unless (port-closed? port)
          (set! saved-encoding (port-encoding port))
          (set-port-encoding! port encoding)))
      thunk
      (lambda ()
        (unless (port-closed? port)
          (set! encoding (port-encoding port))
          (set-port-encoding! port saved-encoding))))))

(define (with-saved-port-line+column port thunk)
  "Save the line and column of PORT before entering THUNK, and restore
their previous values upon normal or non-local exit from THUNK."
  (let ((saved-line #f) (saved-column #f))
    (dynamic-wind
      (lambda ()
        (unless (port-closed? port)
          (set! saved-line   (port-line   port))
          (set! saved-column (port-column port))))
      thunk
      (lambda ()
        (unless (port-closed? port)
          (set-port-line!   port saved-line)
          (set-port-column! port saved-column))))))

(define (drain-input-and-close socket)
  "Drain input from SOCKET using ISO-8859-1 encoding until it would block,
and then close it.  Return the drained input as a string."
  (dynamic-wind
    (lambda ()
      ;; Enable full buffering mode on the socket to allow
      ;; 'get-bytevector-some' to return non-trivial chunks.
      (setvbuf socket _IOFBF))
    (lambda ()
      (let loop ((chunks '()))
        (let ((result (and (char-ready? socket)
                           (get-bytevector-some socket))))
          (if (bytevector? result)
              (loop (cons (bytevector->string result "ISO-8859-1")
                          chunks))
              (string-concatenate-reverse chunks)))))
    (lambda ()
      ;; Close the socket even in case of an exception.
      (close-port socket))))

(define permissive-http-request-line?
  ;; This predicate is deliberately permissive
  ;; when checking the Request-URI component.
  (let ((cs (ucs-range->char-set #x20 #x7E))
        (rx (make-regexp
             (string-append
              "^(OPTIONS|GET|HEAD|POST|PUT|DELETE|TRACE|CONNECT) "
              "[^ ]+ "
              "HTTP/[0-9]+.[0-9]+$"))))
    (lambda (line)
      "Return true if LINE might plausibly be an HTTP request-line,
otherwise return #f."
      ;; We cannot simplify this to a simple 'regexp-exec', because
      ;; 'regexp-exec' cannot cope with NUL bytes.
      (and (string-every cs line)
           (regexp-exec  rx line)))))

(define (check-for-http-request socket)
  "Check for a possible HTTP request in the initial input from SOCKET.
If one is found, close the socket and print a report to STDERR (fdes 2).
Otherwise, put back the bytes."
  ;; Temporarily set the port encoding to ISO-8859-1 to allow lossless
  ;; reading and unreading of the first line, regardless of what bytes
  ;; are present.  Note that a valid HTTP request-line contains only
  ;; ASCII characters.
  (with-temporary-port-encoding socket "ISO-8859-1"
    (lambda ()
      ;; Save the port 'line' and 'column' counters and later restore
      ;; them, since unreading what we read is not sufficient to do so.
      (with-saved-port-line+column socket
        (lambda ()
          ;; Read up to (but not including) the first CR or LF.
          ;; Although HTTP mandates CRLF line endings, we are permissive
          ;; here to guard against the possibility that in some
          ;; environments CRLF might be converted to LF before it
          ;; reaches us.
          (match (read-delimited "\r\n" socket 'peek)
            ((? eof-object?)
             ;; We found EOF before any input.  Nothing to do.
             'done)

            ((? permissive-http-request-line? request-line)
             ;; The input from the socket began with a plausible HTTP
             ;; request-line, which is unlikely to be legitimate and may
             ;; indicate an possible break-in attempt.

             ;; First, set the current port parameters to a void-port,
             ;; to avoid sending any more data over the socket, to cause
             ;; the REPL reader to see EOF, and to swallow any remaining
             ;; output gracefully.
             (let ((void-port (%make-void-port "rw")))
               (current-input-port   void-port)
               (current-output-port  void-port)
               (current-error-port   void-port)
               (current-warning-port void-port))

             ;; Read from the socket until we would block,
             ;; and then close it.
             (let ((drained-input (drain-input-and-close socket)))

               ;; Print a report to STDERR (POSIX file descriptor 2).
               ;; XXX Can we do better here?
               (call-with-port (dup->port 2 "w")
                 (cut format <> "
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ POSSIBLE BREAK-IN ATTEMPT ON THE REPL SERVER                @@
@@ BY AN HTTP INTER-PROTOCOL EXPLOITATION ATTACK.  See:        @@
@@ <https://en.wikipedia.org/wiki/Inter-protocol_exploitation> @@
@@ Possible HTTP request received: ~S
@@ The associated socket has been closed.                      @@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
                      (string-append request-line
                                     drained-input)))))

            (start-line
             ;; The HTTP request-line was not found, so
             ;; 'unread' the characters that we have read.
             (unread-string start-line socket))))))))

(define (guard-against-http-request socket)
  "Arrange for the Guile REPL to check for an HTTP request in the
initial input from SOCKET, in which case the socket will be closed.
This guards against HTTP inter-protocol exploitation attacks, a scenario
whereby an attacker can, via an HTML page, cause a web browser to send
data to TCP servers listening on a loopback interface or private
network."
  (%set-port-property! socket 'guard-against-http-request? #t))

(define* (maybe-check-for-http-request
          #:optional (socket (current-input-port)))
  "Apply check-for-http-request to SOCKET if previously requested by
guard-against-http-request.  This procedure is intended to be added to
before-read-hook."
  (when (%port-property socket 'guard-against-http-request?)
    (check-for-http-request socket)
    (unless (port-closed? socket)
      (%set-port-property! socket 'guard-against-http-request? #f))))

;; Install the hook.
(add-hook! before-read-hook
           maybe-check-for-http-request)

;;; Local Variables:
;;; eval: (put 'with-temporary-port-encoding 'scheme-indent-function 2)
;;; eval: (put 'with-saved-port-line+column  'scheme-indent-function 1)
;;; End:
