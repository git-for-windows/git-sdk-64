;;; Web I/O: HTTP

;; Copyright (C)  2010, 2011, 2012, 2015 Free Software Foundation, Inc.

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
;;; This is the HTTP implementation of the (web server) interface.
;;;
;;; `read-request' sets the character encoding on the new port to
;;; latin-1.  See the note in request.scm regarding character sets,
;;; strings, and bytevectors for more information.
;;;
;;; Code:

(define-module (web server http)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (ice-9 poll)
  #:export (http))


(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock family addr port)
    sock))

(define-record-type <http-server>
  (make-http-server socket poll-idx poll-set)
  http-server?
  (socket http-socket)
  (poll-idx http-poll-idx set-http-poll-idx!)
  (poll-set http-poll-set))

(define *error-events* (logior POLLHUP POLLERR))
(define *read-events* POLLIN)
(define *events* (logior *error-events* *read-events*))

;; -> server
(define* (http-open #:key
                    (host #f)
                    (family AF_INET)
                    (addr (if host
                              (inet-pton family host)
                              INADDR_LOOPBACK))
                    (port 8080)
                    (socket (make-default-socket family addr port)))
  (listen socket 128)
  (sigaction SIGPIPE SIG_IGN)
  (let ((poll-set (make-empty-poll-set)))
    (poll-set-add! poll-set socket *events*)
    (make-http-server socket 0 poll-set)))

(define (bad-request port)
  (write-response (build-response #:version '(1 . 0) #:code 400
                                  #:headers '((content-length . 0)))
                  port))

;; -> (client request body | #f #f #f)
(define (http-read server)
  (let* ((poll-set (http-poll-set server)))
    (let lp ((idx (http-poll-idx server)))
      (let ((revents (poll-set-revents poll-set idx)))
        (cond
         ((zero? idx)
          ;; The server socket, and the end of our downward loop.
          (cond
           ((zero? revents)
            ;; No client ready, and no error; poll and loop.
            (poll poll-set)
            (lp (1- (poll-set-nfds poll-set))))
           ((not (zero? (logand revents *error-events*)))
            ;; An error.
            (set-http-poll-idx! server idx)
            (throw 'interrupt))
           (else
            ;; A new client. Add to set, poll, and loop.
            ;;
            ;; FIXME: preserve meta-info.
            (let ((client (accept (poll-set-port poll-set idx))))
              ;; Buffer input and output on this port.
              (setvbuf (car client) 'block)
              ;; From "HOP, A Fast Server for the Diffuse Web", Serrano.
              (setsockopt (car client) SOL_SOCKET SO_SNDBUF (* 12 1024))
              (poll-set-add! poll-set (car client) *events*)
              (poll poll-set)
              (lp (1- (poll-set-nfds poll-set)))))))
         ((zero? revents)
          ;; Nothing on this port.
          (lp (1- idx)))
         ;; Otherwise, a client socket with some activity on
         ;; it. Remove it from the poll set.
         (else
          (let ((port (poll-set-remove! poll-set idx)))
            ;; Record the next index in all cases, in case the EOF check
            ;; throws an error.
            (set-http-poll-idx! server (1- idx))
            (cond
             ((eof-object? (peek-char port))
              ;; EOF.
              (close-port port)
              (lp (1- idx)))
             (else
              ;; Otherwise, try to read a request from this port.
              (with-throw-handler
               #t
               (lambda ()
                 (let ((req (read-request port)))
                   (values port
                           req
                           (read-request-body req))))
               (lambda (k . args)
                 (define-syntax-rule (cleanup-catch statement)
                   (catch #t
                     (lambda () statement)
                     (lambda (k . args)
                       (format (current-error-port) "In ~a:\n" 'statement)
                       (print-exception (current-error-port) #f k args))))
                 (cleanup-catch (bad-request port))
                 (cleanup-catch (close-port port)))))))))))))

(define (keep-alive? response)
  (let ((v (response-version response)))
    (and (or (< (response-code response) 400)
             (= (response-code response) 404))
         (case (car v)
           ((1)
            (case (cdr v)
              ((1) (not (memq 'close (response-connection response))))
              ((0) (memq 'keep-alive (response-connection response)))))
           (else #f)))))

;; -> 0 values
(define (http-write server client response body)
  (let* ((response (write-response response client))
         (port (response-port response)))
    (cond
     ((not body))                       ; pass
     ((bytevector? body)
      (write-response-body response body))
     (else
      (error "Expected a bytevector for body" body)))
    (cond
     ((keep-alive? response)
      (force-output port)
      (poll-set-add! (http-poll-set server) port *events*))
     (else
      (close-port port)))
    (values)))

;; -> unspecified values
(define (http-close server)
  (let ((poll-set (http-poll-set server)))
    (let lp ((n (poll-set-nfds poll-set)))
      (if (positive? n)
          (begin
            (close-port (poll-set-remove! poll-set (1- n)))
            (lp (1- n)))))))

(define-server-impl http
  http-open
  http-read
  http-write
  http-close)
