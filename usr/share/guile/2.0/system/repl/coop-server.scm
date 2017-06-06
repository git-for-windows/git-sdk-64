;;; Cooperative REPL server

;; Copyright (C) 2014, 2016 Free Software Foundation, Inc.

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

(define-module (system repl coop-server)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-9)
  #:use-module ((system repl repl)
                #:select (start-repl* prompting-meta-read))
  #:use-module ((system repl server)
                #:select (run-server* make-tcp-server-socket
                                      add-open-socket! close-socket!
                                      guard-against-http-request))
  #:export (spawn-coop-repl-server
            poll-coop-repl-server))

(define-record-type <coop-repl-server>
  (%make-coop-repl-server mutex queue)
  coop-repl-server?
  (mutex coop-repl-server-mutex)
  (queue coop-repl-server-queue))

(define (make-coop-repl-server)
  (%make-coop-repl-server (make-mutex) (make-q)))

(define (coop-repl-server-eval coop-server opcode . args)
  "Queue a new instruction with the symbolic name OPCODE and an arbitrary
number of arguments, to be processed the next time COOP-SERVER is polled."
  (with-mutex (coop-repl-server-mutex coop-server)
    (enq! (coop-repl-server-queue coop-server)
          (cons opcode args))))

(define-record-type <coop-repl>
  (%make-coop-repl mutex condvar thunk cont)
  coop-repl?
  (mutex coop-repl-mutex)
  (condvar coop-repl-condvar)  ; signaled when thunk becomes non-#f
  (thunk coop-repl-read-thunk set-coop-repl-read-thunk!)
  (cont coop-repl-cont set-coop-repl-cont!))

(define (make-coop-repl)
  (%make-coop-repl (make-mutex) (make-condition-variable) #f #f))

(define (coop-repl-read coop-repl)
  "Read an expression via the thunk stored in COOP-REPL."
  (let ((thunk
         (with-mutex (coop-repl-mutex coop-repl)
           (unless (coop-repl-read-thunk coop-repl)
             (wait-condition-variable (coop-repl-condvar coop-repl)
                                      (coop-repl-mutex coop-repl)))
           (let ((thunk (coop-repl-read-thunk coop-repl)))
             (unless thunk
               (error "coop-repl-read: condvar signaled, but thunk is #f!"))
             (set-coop-repl-read-thunk! coop-repl #f)
             thunk))))
    (thunk)))

(define (store-repl-cont cont coop-repl)
  "Save the partial continuation CONT within COOP-REPL."
  (set-coop-repl-cont! coop-repl
                       (lambda (exp)
                         (coop-repl-prompt
                          (lambda () (cont exp))))))

(define (coop-repl-prompt thunk)
  "Apply THUNK within a prompt for cooperative REPLs."
  (call-with-prompt 'coop-repl-prompt thunk store-repl-cont))

(define (make-coop-reader coop-repl)
  "Return a new procedure for reading user input from COOP-REPL.  The
generated procedure passes the responsibility of reading input to
another thread and aborts the cooperative REPL prompt."
  (lambda (repl)
    (let ((read-thunk
           ;; Need to preserve the REPL stack and current module across
           ;; threads.
           (let ((stack (fluid-ref *repl-stack*))
                 (module (current-module)))
             (lambda ()
               (with-fluids ((*repl-stack* stack))
                 (set-current-module module)
                 (prompting-meta-read repl))))))
      (with-mutex (coop-repl-mutex coop-repl)
        (when (coop-repl-read-thunk coop-repl)
          (error "coop-reader: read-thunk is not #f!"))
        (set-coop-repl-read-thunk! coop-repl read-thunk)
        (signal-condition-variable (coop-repl-condvar coop-repl))))
    (abort-to-prompt 'coop-repl-prompt coop-repl)))

(define (reader-loop coop-server coop-repl)
  "Run an unbounded loop that reads an expression for COOP-REPL and
stores the expression within COOP-SERVER for later evaluation."
  (coop-repl-server-eval coop-server 'eval coop-repl
                         (coop-repl-read coop-repl))
  (reader-loop coop-server coop-repl))

(define (poll-coop-repl-server coop-server)
  "Poll the cooperative REPL server COOP-SERVER and apply a pending
operation if there is one, such as evaluating an expression typed at the
REPL prompt.  This procedure must be called from the same thread that
called spawn-coop-repl-server."
  (let ((op (with-mutex (coop-repl-server-mutex coop-server)
              (let ((queue (coop-repl-server-queue coop-server)))
                (and (not (q-empty? queue))
                     (deq! queue))))))
    (when op
      (match op
        (('new-repl client)
         (start-repl-client coop-server client))
        (('eval coop-repl exp)
         ((coop-repl-cont coop-repl) exp))))
    *unspecified*))

(define (start-coop-repl coop-server)
  "Start a new cooperative REPL process for COOP-SERVER."
  ;; Calling stop-server-and-clients! from a REPL will cause an
  ;; exception to be thrown when trying to read from the socket that has
  ;; been closed, so we catch that here.
  (false-if-exception
   (let ((coop-repl (make-coop-repl)))
     (make-thread reader-loop coop-server coop-repl)
     (start-repl* (current-language) #f (make-coop-reader coop-repl)))))

(define (run-coop-repl-server coop-server server-socket)
  "Start the cooperative REPL server for COOP-SERVER using the socket
SERVER-SOCKET."
  (run-server* server-socket (make-coop-client-proc coop-server)))

(define* (spawn-coop-repl-server
          #:optional (server-socket (make-tcp-server-socket)))
  "Create and return a new cooperative REPL server object, and spawn a
new thread to listen for connections on SERVER-SOCKET.  Proper
functioning of the REPL server requires that poll-coop-repl-server be
called periodically on the returned server object."
  (let ((coop-server (make-coop-repl-server)))
    (make-thread run-coop-repl-server
                 coop-server
                 server-socket)
    coop-server))

(define (make-coop-client-proc coop-server)
  "Return a new procedure that is used to schedule the creation of a new
cooperative REPL for COOP-SERVER."
  (lambda (client addr)
    (coop-repl-server-eval coop-server 'new-repl client)))

(define (start-repl-client coop-server client)
  "Run a cooperative REPL for COOP-SERVER within a prompt.  All input
and output is sent over the socket CLIENT."

  ;; Add the client to the list of open sockets, with a 'force-close'
  ;; procedure that closes the underlying file descriptor.  We do it
  ;; this way because we cannot close the port itself safely from
  ;; another thread.
  (add-open-socket! client (lambda () (close-fdes (fileno client))))

  (guard-against-http-request client)

  (with-continuation-barrier
   (lambda ()
     (coop-repl-prompt
      (lambda ()
        (parameterize ((current-input-port client)
                       (current-output-port client)
                       (current-error-port client)
                       (current-warning-port client))
          (with-fluids ((*repl-stack* '()))
            (save-module-excursion
             (lambda ()
               (start-coop-repl coop-server)))))

        ;; This may fail if 'stop-server-and-clients!' is called,
        ;; because the 'force-close' procedure above closes the
        ;; underlying file descriptor instead of the port itself.
        (false-if-exception
         (close-socket! client)))))))
