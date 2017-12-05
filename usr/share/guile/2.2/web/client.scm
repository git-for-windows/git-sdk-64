;;; Web client

;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016, 2017 Free Software Foundation, Inc.

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
;;; (web client) is a simple HTTP URL fetcher for Guile.
;;;
;;; In its current incarnation, (web client) is synchronous.  If you
;;; want to fetch a number of URLs at once, probably the best thing to
;;; do is to write an event-driven URL fetcher, similar in structure to
;;; the web server.
;;;
;;; Another option, good but not as performant, would be to use threads,
;;; possibly via a thread pool.
;;;
;;; Code:

(define-module (web client)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 rdelim)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (web http)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module ((rnrs io ports)
                #:prefix rnrs-ports:)
  #:export (current-http-proxy
            open-socket-for-uri
            http-get
            http-get*
            http-head
            http-post
            http-put
            http-delete
            http-trace
            http-options))

(define %http-receive-buffer-size
  ;; Size of the HTTP receive buffer.
  65536)

;; Autoload GnuTLS so that this module can be used even when GnuTLS is
;; not available.  At compile time, this yields "possibly unbound
;; variable" warnings, but these are OK: we know that the variables will
;; be bound if we need them, because (guix download) adds GnuTLS as an
;; input in that case.

;; XXX: Use this hack instead of #:autoload to avoid compilation errors.
;; See <http://bugs.gnu.org/12202>.
(module-autoload! (current-module)
                  '(gnutls) '(make-session connection-end/client))

(define gnutls-module
  (delay
    (catch 'misc-error
      (lambda ()
        (let ((module (resolve-interface '(gnutls))))
          ;; In some 2.1/2.2 installations installed alongside Guile 2.0, gnutls
          ;; can be imported but the bindings are broken as "unknown type".
          ;; Here we check that gnutls-version is the right type (a procedure)
          ;; to make sure the bindings are ok.
          (if (procedure? (module-ref module 'gnutls-version))
              module
              #f)))
      (const #f))))

(define (ensure-gnutls)
  (if (not (force gnutls-module))
      (throw 'gnutls-not-available "(gnutls) module not available")))

(define current-http-proxy
  (make-parameter (let ((proxy (getenv "http_proxy")))
                    (and (not (equal? proxy ""))
                         proxy))))

(define (tls-wrap port server)
  "Return PORT wrapped in a TLS connection to SERVER.  SERVER must be a DNS
host name without trailing dot."
  (define (log level str)
    (format (current-error-port)
            "gnutls: [~a|~a] ~a" (getpid) level str))

  (ensure-gnutls)

  (let ((session (make-session connection-end/client)))
    ;; Some servers such as 'cloud.github.com' require the client to support
    ;; the 'SERVER NAME' extension.  However, 'set-session-server-name!' is
    ;; not available in older GnuTLS releases.  See
    ;; <http://bugs.gnu.org/18526> for details.
    (if (module-defined? (force gnutls-module)
                         'set-session-server-name!)
        (set-session-server-name! session server-name-type/dns server)
        (format (current-error-port)
                "warning: TLS 'SERVER NAME' extension not supported~%"))

    (set-session-transport-fd! session (fileno port))
    (set-session-default-priority! session)

    ;; The "%COMPAT" bit allows us to work around firewall issues (info
    ;; "(gnutls) Priority Strings"); see <http://bugs.gnu.org/23311>.
    ;; Explicitly disable SSLv3, which is insecure:
    ;; <https://tools.ietf.org/html/rfc7568>.
    (set-session-priorities! session "NORMAL:%COMPAT:-VERS-SSL3.0")

    (set-session-credentials! session (make-certificate-credentials))

    ;; Uncomment the following lines in case of debugging emergency.
    ;;(set-log-level! 10)
    ;;(set-log-procedure! log)

    (handshake session)
    ;; FIXME: It appears that session-record-port is entirely
    ;; sufficient; it's already a port.  The only value of this code is
    ;; to keep a reference on "port", to keep it alive!  To fix this we
    ;; need to arrange to either hand GnuTLS its own fd to close, or to
    ;; arrange a reference from the session-record-port to the
    ;; underlying socket.
    (let ((record (session-record-port session)))
      (define (read! bv start count)
        (define read-bv (get-bytevector-some record))
        (if (eof-object? read-bv)
            0  ; read! returns 0 on eof-object
            (let ((read-bv-len (bytevector-length read-bv)))
              (bytevector-copy! read-bv 0 bv start (min read-bv-len count))
              (when (< count read-bv-len)
                (unget-bytevector record bv count (- read-bv-len count)))
              read-bv-len)))
      (define (write! bv start count)
        (put-bytevector record bv start count)
        (force-output record)
        count)
      (define (get-position)
        (rnrs-ports:port-position record))
      (define (set-position! new-position)
        (rnrs-ports:set-port-position! record new-position))
      (define (close)
        (unless (port-closed? port)
          (close-port port))
        (unless (port-closed? record)
          (close-port record)))
      (setvbuf record 'block)
      (make-custom-binary-input/output-port "gnutls wrapped port" read! write!
                                            get-position set-position!
                                            close))))

(define (ensure-uri-reference uri-or-string)
  (cond
   ((string? uri-or-string) (string->uri-reference uri-or-string))
   ((uri-reference? uri-or-string) uri-or-string)
   (else (error "Invalid URI-reference" uri-or-string))))

(define (open-socket-for-uri uri-or-string)
  "Return an open input/output port for a connection to URI."
  (define http-proxy (current-http-proxy))
  (define uri (ensure-uri-reference (or http-proxy uri-or-string)))
  (define addresses
    (let ((port (uri-port uri)))
      (delete-duplicates
       (getaddrinfo (uri-host uri)
                    (cond (port => number->string)
                          ((uri-scheme uri) => symbol->string)
                          (else (error "Not an absolute URI" uri)))
                    (if port
                        AI_NUMERICSERV
                        0))
       (lambda (ai1 ai2)
         (equal? (addrinfo:addr ai1) (addrinfo:addr ai2))))))
  (define https?
    (eq? 'https (uri-scheme uri)))
  (define (open-socket)
    (let loop ((addresses addresses))
      (let* ((ai (car addresses))
             (s  (with-fluids ((%default-port-encoding #f))
                   ;; Restrict ourselves to TCP.
                   (socket (addrinfo:fam ai) SOCK_STREAM IPPROTO_IP))))
        (catch 'system-error
          (lambda ()
            (connect s (addrinfo:addr ai))

            ;; Buffer input and output on this port.
            (setvbuf s 'block)
            ;; If we're using a proxy, make a note of that.
            (when http-proxy (set-http-proxy-port?! s #t))
            s)
          (lambda args
            ;; Connection failed, so try one of the other addresses.
            (close s)
            (if (null? (cdr addresses))
                (apply throw args)
                (loop (cdr addresses))))))))

  (let-syntax ((with-https-proxy
                (syntax-rules ()
                  ((_ exp)
                   ;; For HTTPS URIs, honor 'https_proxy', not 'http_proxy'.
                   ;; FIXME: Proxying is not supported for https.
                   (let ((thunk (lambda () exp)))
                     (if (and https?
                              current-http-proxy)
                         (parameterize ((current-http-proxy #f))
                           (when (and=> (getenv "https_proxy")
                                        (negate string-null?))
                             (format (current-error-port)
                                     "warning: 'https_proxy' is ignored~%"))
                           (thunk))
                         (thunk)))))))
    (with-https-proxy
     (let ((s (open-socket)))
       ;; Buffer input and output on this port.
       (setvbuf s 'block %http-receive-buffer-size)

       (if https?
           (tls-wrap s (uri-host uri))
           s)))))

(define (extend-request r k v . additional)
  (let ((r (set-field r (request-headers)
                      (assoc-set! (copy-tree (request-headers r))
                                  k v))))
    (if (null? additional)
        r
        (apply extend-request r additional))))

;; -> request body
(define (sanitize-request request body)
  "\"Sanitize\" the given request and body, ensuring that they are
complete and coherent.  This method is most useful for methods that send
data to the server, like POST, but can be used for any method.  Return
two values: a request and a bytevector, possibly the same ones that were
passed as arguments.

If BODY is a string, encodes the string to a bytevector, in an encoding
appropriate for REQUEST.  Adds a ‘content-length’ and ‘content-type’
header, as necessary.

If BODY is a procedure, it is called with a port as an argument, and the
output collected as a bytevector.  In the future we might try to instead
use a compressing, chunk-encoded port, and call this procedure later.
Authors are advised not to rely on the procedure being called at any
particular time.

Note that we rely on the request itself already having been validated,
as is the case by default with a request returned by `build-request'."
  (cond
   ((not body)
    (let ((length (request-content-length request)))
      (if length
          ;; FIXME make this stricter: content-length header should be
          ;; prohibited if there's no body, even if the content-length
          ;; is 0.
          (unless (zero? length)
            (error "content-length, but no body"))
          (when (assq 'transfer-encoding (request-headers request))
            (error "transfer-encoding not allowed with no body")))
      (values request #vu8())))
   ((string? body)
    (let* ((type (request-content-type request '(text/plain)))
           (declared-charset (assq-ref (cdr type) 'charset))
           (charset (or declared-charset "utf-8")))
      (sanitize-request
       (if declared-charset
           request
           (extend-request request 'content-type
                            `(,@type (charset . ,charset))))
       (string->bytevector body charset))))
   ((procedure? body)
    (let* ((type (request-content-type request
                                        '(text/plain)))
           (declared-charset (assq-ref (cdr type) 'charset))
           (charset (or declared-charset "utf-8")))
      (sanitize-request
       (if declared-charset
           request
           (extend-request request 'content-type
                            `(,@type (charset . ,charset))))
       (call-with-encoded-output-string charset body))))
   ((not (bytevector? body))
    (error "unexpected body type"))
   (else
    (values (let ((rlen (request-content-length request))
                  (blen (bytevector-length body)))
              (cond
               (rlen (if (= rlen blen)
                         request
                         (error "bad content-length" rlen blen)))
               (else (extend-request request 'content-length blen))))
            body))))

(define (decode-response-body response body)
  ;; `body' is either #f or a bytevector.
  (cond
   ((not body) body)
   ((bytevector? body)
    (let ((rlen (response-content-length response))
          (blen (bytevector-length body)))
      (cond
       ((and rlen (not (= rlen blen)))
        (error "bad content-length" rlen blen))
       ((response-content-type response)
        => (lambda (type)
             (cond
              ((text-content-type? (car type))
               ;; RFC 2616 3.7.1: "When no explicit charset parameter is
               ;; provided by the sender, media subtypes of the "text"
               ;; type are defined to have a default charset value of
               ;; "ISO-8859-1" when received via HTTP."
               (bytevector->string body (or (assq-ref (cdr type) 'charset)
                                            "iso-8859-1")))
              (else body))))
       (else body))))
   (else
    (error "unexpected body type" body))))

;; We could expose this to user code if there is demand.
(define* (request uri #:key
                  (body #f)
                  (port (open-socket-for-uri uri))
                  (method 'GET)
                  (version '(1 . 1))
                  (keep-alive? #f)
                  (headers '())
                  (decode-body? #t)
                  (streaming? #f)
                  (request
                   (build-request
                    (ensure-uri-reference uri)
                    #:method method
                    #:version version
                    #:headers (if keep-alive?
                                  headers
                                  (cons '(connection close) headers))
                    #:port port)))
  (call-with-values (lambda () (sanitize-request request body))
    (lambda (request body)
      (let ((request (write-request request port)))
        (when body
          (write-request-body request body))
        (force-output (request-port request))
        (let ((response (read-response port)))
          (cond
           ((eq? (request-method request) 'HEAD)
            (unless keep-alive?
              (close-port port))
            (values response #f))
           (streaming?
            (values response
                    (response-body-port response
                                        #:keep-alive? keep-alive?
                                        #:decode? decode-body?)))
           (else
            (let ((body (read-response-body response)))
              (unless keep-alive?
                (close-port port))
              (values response
                      (if decode-body?
                          (decode-response-body response body)
                          body))))))))))

(define* (http-get uri #:key
                   (body #f)
                   (port (open-socket-for-uri uri))
                   (version '(1 . 1)) (keep-alive? #f)
                   ;; #:headers is the new name of #:extra-headers.
                   (extra-headers #f) (headers (or extra-headers '()))
                   (decode-body? #t) (streaming? #f))
  "Connect to the server corresponding to URI and ask for the
resource, using the ‘GET’ method.  If you already have a port open,
pass it as PORT.  The port will be closed at the end of the
request unless KEEP-ALIVE? is true.  Any extra headers in the
alist HEADERS will be added to the request.

If BODY is not ‘#f’, a message body will also be sent with the HTTP
request.  If BODY is a string, it is encoded according to the
content-type in HEADERS, defaulting to UTF-8.  Otherwise BODY should be
a bytevector, or ‘#f’ for no body.  Although it's allowed to send a
message body along with any request, usually only POST and PUT requests
have bodies.  See ‘http-put’ and ‘http-post’ documentation, for more.

If DECODE-BODY? is true, as is the default, the body of the
response will be decoded to string, if it is a textual content-type.
Otherwise it will be returned as a bytevector.

However, if STREAMING? is true, instead of eagerly reading the response
body from the server, this function only reads off the headers.  The
response body will be returned as a port on which the data may be read.
Unless KEEP-ALIVE? is true, the port will be closed after the full
response body has been read.

Returns two values: the response read from the server, and the response
body as a string, bytevector, #f value, or as a port (if STREAMING? is
true)."
  (when extra-headers
    (issue-deprecation-warning
     "The #:extra-headers argument to http-get has been renamed to #:headers. "
     "Please update your code."))
  (request uri #:method 'GET #:body body
           #:port port #:version version #:keep-alive? keep-alive?
           #:headers headers #:decode-body? decode-body?
           #:streaming? streaming?))

(define* (http-get* uri #:key
                    (body #f)
                    (port (open-socket-for-uri uri))
                    (version '(1 . 1)) (keep-alive? #f)
                    ;; #:headers is the new name of #:extra-headers.
                    (extra-headers #f) (headers (or extra-headers '()))
                    (decode-body? #t))
  "Deprecated in favor of (http-get #:streaming? #t)."
  (issue-deprecation-warning
   "`http-get*' has been deprecated.  "
   "Instead, use `http-get' with the #:streaming? #t keyword argument.")
  (http-get uri #:body body
            #:port port #:version version #:keep-alive? keep-alive?
            #:headers headers #:decode-body? #t #:streaming? #t))

(define-syntax-rule (define-http-verb http-verb method doc)
  (define* (http-verb uri #:key
                      (body #f)
                      (port (open-socket-for-uri uri))
                      (version '(1 . 1))
                      (keep-alive? #f)
                      (headers '())
                      (decode-body? #t)
                      (streaming? #f))
    doc
    (request uri
             #:body body #:method method
             #:port port #:version version #:keep-alive? keep-alive?
             #:headers headers #:decode-body? decode-body?
             #:streaming? streaming?)))

(define-http-verb http-head
  'HEAD
  "Fetch message headers for the given URI using the HTTP \"HEAD\"
method.

This function is similar to ‘http-get’, except it uses the \"HEAD\"
method.  See ‘http-get’ for full documentation on the various keyword
arguments that are accepted by this function.

Returns two values: the resulting response, and ‘#f’.  Responses to HEAD
requests do not have a body.  The second value is only returned so that
other procedures can treat all of the http-foo verbs identically.")

(define-http-verb http-post
  'POST
  "Post data to the given URI using the HTTP \"POST\" method.

This function is similar to ‘http-get’, except it uses the \"POST\"
method.  See ‘http-get’ for full documentation on the various keyword
arguments that are accepted by this function.

Returns two values: the resulting response, and the response body.")

(define-http-verb http-put
  'PUT
  "Put data at the given URI using the HTTP \"PUT\" method.

This function is similar to ‘http-get’, except it uses the \"PUT\"
method.  See ‘http-get’ for full documentation on the various keyword
arguments that are accepted by this function.

Returns two values: the resulting response, and the response body.")

(define-http-verb http-delete
  'DELETE
  "Delete data at the given URI using the HTTP \"DELETE\" method.

This function is similar to ‘http-get’, except it uses the \"DELETE\"
method.  See ‘http-get’ for full documentation on the various keyword
arguments that are accepted by this function.

Returns two values: the resulting response, and the response body.")

(define-http-verb http-trace
  'TRACE
  "Send an HTTP \"TRACE\" request.

This function is similar to ‘http-get’, except it uses the \"TRACE\"
method.  See ‘http-get’ for full documentation on the various keyword
arguments that are accepted by this function.

Returns two values: the resulting response, and the response body.")

(define-http-verb http-options
  'OPTIONS
  "Query characteristics of an HTTP resource using the HTTP \"OPTIONS\"
method.

This function is similar to ‘http-get’, except it uses the \"OPTIONS\"
method.  See ‘http-get’ for full documentation on the various keyword
arguments that are accepted by this function.

Returns two values: the resulting response, and the response body.")
