;;; Repl server

;; Copyright (C)  2003, 2010, 2011, 2014 Free Software Foundation, Inc.

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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
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

  (with-continuation-barrier
   (lambda ()
     (parameterize ((current-input-port client)
                    (current-output-port client)
                    (current-error-port client)
                    (current-warning-port client))
       (with-fluids ((*repl-stack* '()))
         (start-repl))))))
