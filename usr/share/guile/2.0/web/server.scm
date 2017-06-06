;;; Web server

;; Copyright (C)  2010, 2011, 2012, 2013, 2015 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; (web server) is a generic web server interface, along with a main
;;; loop implementation for web servers controlled by Guile.
;;;
;;; The lowest layer is the <server-impl> object, which defines a set of
;;; hooks to open a server, read a request from a client, write a
;;; response to a client, and close a server.  These hooks -- open,
;;; read, write, and close, respectively -- are bound together in a
;;; <server-impl> object.  Procedures in this module take a
;;; <server-impl> object, if needed.
;;;
;;; A <server-impl> may also be looked up by name.  If you pass the
;;; `http' symbol to `run-server', Guile looks for a variable named
;;; `http' in the `(web server http)' module, which should be bound to a
;;; <server-impl> object.  Such a binding is made by instantiation of
;;; the `define-server-impl' syntax.  In this way the run-server loop can
;;; automatically load other backends if available.
;;;
;;; The life cycle of a server goes as follows:
;;;
;;;   * The `open' hook is called, to open the server. `open' takes 0 or
;;;     more arguments, depending on the backend, and returns an opaque
;;;     server socket object, or signals an error.
;;;
;;;   * The `read' hook is called, to read a request from a new client.
;;;     The `read' hook takes one arguments, the server socket.  It
;;;     should return three values: an opaque client socket, the
;;;     request, and the request body. The request should be a
;;;     `<request>' object, from `(web request)'.  The body should be a
;;;     string or a bytevector, or `#f' if there is no body.
;;;
;;;     If the read failed, the `read' hook may return #f for the client
;;;     socket, request, and body.
;;;
;;;   * A user-provided handler procedure is called, with the request
;;;     and body as its arguments.  The handler should return two
;;;     values: the response, as a `<response>' record from `(web
;;;     response)', and the response body as a string, bytevector, or
;;;     `#f' if not present.  We also allow the reponse to be simply an
;;;     alist of headers, in which case a default response object is
;;;     constructed with those headers.
;;;
;;;   * The `write' hook is called with three arguments: the client
;;;     socket, the response, and the body.  The `write' hook returns no
;;;     values.
;;;
;;;   * At this point the request handling is complete. For a loop, we
;;;     loop back and try to read a new request.
;;;
;;;   * If the user interrupts the loop, the `close' hook is called on
;;;     the server socket.
;;;
;;; Code:

(define-module (web server)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (system repl error-handling)
  #:use-module (ice-9 control)
  #:use-module (ice-9 iconv)
  #:export (define-server-impl
            lookup-server-impl

            make-server-impl
            server-impl?
            server-impl-name
            server-impl-open
            server-impl-read
            server-impl-write
            server-impl-close

            open-server
            read-client
            handle-request
            sanitize-response
            write-client
            close-server
            serve-one-client
            run-server))

(define *timer* (gettimeofday))
(define (print-elapsed who)
  (let ((t (gettimeofday)))
    (pk who (+ (* (- (car t) (car *timer*)) 1000000)
               (- (cdr t) (cdr *timer*))))
    (set! *timer* t)))

(eval-when (expand)
  (define *time-debug?* #f))

(define-syntax debug-elapsed
  (lambda (x)
    (syntax-case x ()
      ((_ who)
       (if *time-debug?*
           #'(print-elapsed who)
           #'*unspecified*)))))

(define-record-type server-impl
  (make-server-impl name open read write close)
  server-impl?
  (name server-impl-name)
  (open server-impl-open)
  (read server-impl-read)
  (write server-impl-write)
  (close server-impl-close))

(define-syntax-rule (define-server-impl name open read write close)
  (define name
    (make-server-impl 'name open read write close)))

(define (lookup-server-impl impl)
  "Look up a server implementation.  If IMPL is a server
implementation already, it is returned directly.  If it is a symbol, the
binding named IMPL in the ‘(web server IMPL)’ module is
looked up.  Otherwise an error is signaled.

Currently a server implementation is a somewhat opaque type, useful only
for passing to other procedures in this module, like
‘read-client’."
  (cond
   ((server-impl? impl) impl)
   ((symbol? impl)
    (let ((impl (module-ref (resolve-module `(web server ,impl)) impl)))
      (if (server-impl? impl)
          impl
          (error "expected a server impl in module" `(web server ,impl)))))
   (else
    (error "expected a server-impl or a symbol" impl))))

;; -> server
(define (open-server impl open-params)
  "Open a server for the given implementation.  Return one value, the
new server object.  The implementation's ‘open’ procedure is
applied to OPEN-PARAMS, which should be a list."
  (apply (server-impl-open impl) open-params))

;; -> (client request body | #f #f #f)
(define (read-client impl server)
  "Read a new client from SERVER, by applying the implementation's
‘read’ procedure to the server.  If successful, return three
values: an object corresponding to the client, a request object, and the
request body.  If any exception occurs, return ‘#f’ for all three
values."
  (call-with-error-handling
   (lambda ()
     ((server-impl-read impl) server))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'backtrace 'debug)
   #:post-error (lambda _ (values #f #f #f))))

(define (extend-response r k v . additional)
  (let ((r (set-field r (response-headers)
                      (assoc-set! (copy-tree (response-headers r))
                                  k v))))
    (if (null? additional)
        r
        (apply extend-response r additional))))

;; -> response body
(define (sanitize-response request response body)
  "\"Sanitize\" the given response and body, making them appropriate for
the given request.

As a convenience to web handler authors, RESPONSE may be given as
an alist of headers, in which case it is used to construct a default
response.  Ensures that the response version corresponds to the request
version.  If BODY is a string, encodes the string to a bytevector,
in an encoding appropriate for RESPONSE.  Adds a
‘content-length’ and ‘content-type’ header, as necessary.

If BODY is a procedure, it is called with a port as an argument,
and the output collected as a bytevector.  In the future we might try to
instead use a compressing, chunk-encoded port, and call this procedure
later, in the write-client procedure.  Authors are advised not to rely
on the procedure being called at any particular time."
  (cond
   ((list? response)
    (sanitize-response request
                       (build-response #:version (request-version request)
                                       #:headers response)
                       body))
   ((not (equal? (request-version request) (response-version response)))
    (sanitize-response request
                       (adapt-response-version response
                                               (request-version request))
                       body))
   ((not body)
    (values response #vu8()))
   ((string? body)
    (let* ((type (response-content-type response
                                        '(text/plain)))
           (declared-charset (assq-ref (cdr type) 'charset))
           (charset (or declared-charset "utf-8")))
      (sanitize-response
       request
       (if declared-charset
           response
           (extend-response response 'content-type
                            `(,@type (charset . ,charset))))
       (string->bytevector body charset))))
   ((procedure? body)
    (let* ((type (response-content-type response
                                        '(text/plain)))
           (declared-charset (assq-ref (cdr type) 'charset))
           (charset (or declared-charset "utf-8")))
      (sanitize-response
       request
       (if declared-charset
           response
           (extend-response response 'content-type
                            `(,@type (charset . ,charset))))
       (call-with-encoded-output-string charset body))))
   ((not (bytevector? body))
    (error "unexpected body type"))
   ((and (response-must-not-include-body? response)
         body
         ;; FIXME make this stricter: even an empty body should be prohibited.
         (not (zero? (bytevector-length body))))
    (error "response with this status code must not include body" response))
   (else
    ;; check length; assert type; add other required fields?
    (values (let ((rlen (response-content-length response))
                  (blen (bytevector-length body)))
              (cond
               (rlen (if (= rlen blen)
                         response
                         (error "bad content-length" rlen blen)))
               (else (extend-response response 'content-length blen))))
            (if (eq? (request-method request) 'HEAD)
                ;; Responses to HEAD requests must not include bodies.
                ;; We could raise an error here, but it seems more
                ;; appropriate to just do something sensible.
                #f
                body)))))

;; -> response body state
(define (handle-request handler request body state)
  "Handle a given request, returning the response and body.

The response and response body are produced by calling the given
HANDLER with REQUEST and BODY as arguments.

The elements of STATE are also passed to HANDLER as
arguments, and may be returned as additional values.  The new
STATE, collected from the HANDLER's return values, is then
returned as a list.  The idea is that a server loop receives a handler
from the user, along with whatever state values the user is interested
in, allowing the user's handler to explicitly manage its state."
  (call-with-error-handling
   (lambda ()
     (call-with-values (lambda ()
                         (with-stack-and-prompt
                          (lambda ()
                            (apply handler request body state))))
       (lambda (response body . state)
         (call-with-values (lambda ()
                             (debug-elapsed 'handler)
                             (sanitize-response request response body))
           (lambda (response body)
             (debug-elapsed 'sanitize)
             (values response body state))))))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'backtrace 'debug)
   #:post-error (lambda _
                  (values (build-response #:code 500) #f state))))

;; -> unspecified values
(define (write-client impl server client response body)
  "Write an HTTP response and body to CLIENT.  If the server and
client support persistent connections, it is the implementation's
responsibility to keep track of the client thereafter, presumably by
attaching it to the SERVER argument somehow."
  (call-with-error-handling
   (lambda ()
     ((server-impl-write impl) server client response body))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'backtrace 'debug)
   #:post-error (lambda _ (values))))

;; -> unspecified values
(define (close-server impl server)
  "Release resources allocated by a previous invocation of
‘open-server’."
  ((server-impl-close impl) server))

(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk handler-thunk) (thunk))
      (lambda (thunk handler-thunk)
        (let ((handler #f))
          (catch 'interrupt
            (lambda ()
              (dynamic-wind
                (lambda ()
                  (set! handler
                        (sigaction SIGINT (lambda (sig) (throw 'interrupt)))))
                thunk
                (lambda ()
                  (if handler
                      ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                      (sigaction SIGINT (car handler) (cdr handler))
                      ;; restore original C handler.
                      (sigaction SIGINT #f)))))
            (lambda (k . _) (handler-thunk)))))))

(define (with-stack-and-prompt thunk)
  (call-with-prompt (default-prompt-tag)
                    (lambda () (start-stack #t (thunk)))
                    (lambda (k proc)
                      (with-stack-and-prompt (lambda () (proc k))))))
  
;; -> new-state
(define (serve-one-client handler impl server state)
  "Read one request from SERVER, call HANDLER on the request
and body, and write the response to the client.  Return the new state
produced by the handler procedure."
  (debug-elapsed 'serve-again)
  (call-with-values
      (lambda ()
        (read-client impl server))
    (lambda (client request body)
      (debug-elapsed 'read-client)
      (if client
          (call-with-values
              (lambda ()
                (handle-request handler request body state))
            (lambda (response body state)
              (debug-elapsed 'handle-request)
              (write-client impl server client response body)
              (debug-elapsed 'write-client)
              state))
          state))))

(define* (run-server handler #:optional (impl 'http) (open-params '())
                     . state)
  "Run Guile's built-in web server.

HANDLER should be a procedure that takes two or more arguments,
the HTTP request and request body, and returns two or more values, the
response and response body.

For example, here is a simple \"Hello, World!\" server:

@example
 (define (handler request body)
   (values '((content-type . (text/plain)))
           \"Hello, World!\"))
 (run-server handler)
@end example

The response and body will be run through ‘sanitize-response’
before sending back to the client.

Additional arguments to HANDLER are taken from
STATE.  Additional return values are accumulated into a new
STATE, which will be used for subsequent requests.  In this way a
handler can explicitly manage its state.

The default server implementation is ‘http’, which accepts
OPEN-PARAMS like ‘(#:port 8081)’, among others.  See \"Web
Server\" in the manual, for more information."
  (let* ((impl (lookup-server-impl impl))
         (server (open-server impl open-params)))
    (call-with-sigint
     (lambda ()
       (let lp ((state state))
         (lp (serve-one-client handler impl server state))))
     (lambda ()
       (close-server impl server)
       (values)))))
