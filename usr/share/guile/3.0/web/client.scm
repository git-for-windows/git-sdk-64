;;; Web client

;; Copyright (C) 2011-2018, 2020-2022 Free Software Foundation, Inc.

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
  #:use-module (ice-9 copy-tree)
  #:use-module (ice-9 iconv)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (web http)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 ftw) (scandir)
  #:export (current-http-proxy
            current-https-proxy
            x509-certificate-directory
            open-socket-for-uri
            http-request
            http-get
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
;; variable" warnings, but these are OK: they'll be resolved at run time
;; thanks to 'load-gnutls'.

(define (load-gnutls)
  "Attempt to load the (gnutls) module.  Throw to 'gnutls-not-available
if it is unavailable."
  (catch 'misc-error
    (lambda ()
      ;; XXX: Use this hack instead of #:autoload to avoid compilation
      ;; errors.  See <http://bugs.gnu.org/12202>.
      (module-use! (resolve-module '(web client))
                   (resolve-interface '(gnutls))))
    (lambda _
      (throw 'gnutls-not-available "(gnutls) module not available")))
  (set! load-gnutls (const #t)))

(define current-http-proxy
  (make-parameter (let ((proxy (getenv "http_proxy")))
                    (and (not (equal? proxy ""))
                         proxy))))

(define current-https-proxy
  (make-parameter (let ((proxy (getenv "https_proxy")))
                    (and (not (equal? proxy ""))
                         proxy))))

(define x509-certificate-directory
  ;; The directory where X.509 authority PEM certificates are stored.
  (make-parameter (or (getenv "GUILE_TLS_CERTIFICATE_DIRECTORY")
                      (getenv "SSL_CERT_DIR")     ;like OpenSSL
                      "/etc/ssl/certs")))

(define (set-certificate-credentials-x509-trust-file!* cred file format)
  "Like 'set-certificate-credentials-x509-trust-file!', but without the file
name decoding bug described at
<https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26948#17>."
  (let ((data (call-with-input-file file get-bytevector-all)))
    (set-certificate-credentials-x509-trust-data! cred data format)))

(define (make-credendials-with-ca-trust-files directory)
  "Return certificate credentials with X.509 authority certificates read from
DIRECTORY.  Those authority certificates are checked when
'peer-certificate-status' is later called."
  (let ((cred  (make-certificate-credentials))
        (files (match (scandir directory (cut string-suffix? ".pem" <>))
                 ((or #f ())
                  ;; Some distros provide nothing but bundles (*.crt) under
                  ;; /etc/ssl/certs, so look for them.
                  (or (scandir directory (cut string-suffix? ".crt" <>))
                      '()))
                 (pem pem))))
    (for-each (lambda (file)
                (let ((file (string-append directory "/" file)))
                  ;; Protect against dangling symlinks.
                  (when (file-exists? file)
                    (set-certificate-credentials-x509-trust-file!*
                     cred file
                     x509-certificate-format/pem))))
              files)
    cred))

(define (peer-certificate session)
  "Return the certificate of the remote peer in SESSION."
  (match (session-peer-certificate-chain session)
    ((first _ ...)
     (import-x509-certificate first x509-certificate-format/der))))

(define (assert-valid-server-certificate session server)
  "Return #t if the certificate of the remote peer for SESSION is a valid
certificate for SERVER, where SERVER is the expected host name of peer."
  (define cert
    (peer-certificate session))

  ;; First check whether the server's certificate matches SERVER.
  (unless (x509-certificate-matches-hostname? cert server)
    (throw 'tls-certificate-error 'host-mismatch cert server))

  ;; Second check its validity and reachability from the set of authority
  ;; certificates loaded via 'set-certificate-credentials-x509-trust-file!'.
  (match (peer-certificate-status session)
    (()                                           ;certificate is valid
     #t)
    ((statuses ...)
     (throw 'tls-certificate-error 'invalid-certificate cert server
            statuses))))

(define (print-tls-certificate-error port key args default-printer)
  "Print the TLS certificate error represented by ARGS in an intelligible
way."
  (match args
    (('host-mismatch cert server)
     (format port
             "X.509 server certificate for '~a' does not match: ~a~%"
             server (x509-certificate-dn cert)))
    (('invalid-certificate cert server statuses)
     (format port
             "X.509 certificate of '~a' could not be verified:~%  ~a~%"
             server
             (string-join (map certificate-status->string statuses))))))

(set-exception-printer! 'tls-certificate-error
                        print-tls-certificate-error)

(define (wrap-record-port-for-gnutls<3.7.7 record port)
  "Return a port that wraps RECORD to ensure that closing it also closes PORT,
the actual socket port, and its file descriptor.  Make sure it does not
introduce extra buffering (custom ports are buffered by default as of Guile
3.0.5).

This wrapper is unnecessary with GnuTLS >= 3.7.7, which can automatically
close SESSION's file descriptor when RECORD is closed."
  (define (read! bv start count)
    (define read
      (catch 'gnutls-error
        (lambda ()
          (get-bytevector-n! record bv start count))
        (lambda (key err proc . rest)
          ;; When responding to "Connection: close" requests, some servers
          ;; close the connection abruptly after sending the response body,
          ;; without doing a proper TLS connection termination.  Treat it as
          ;; EOF.  This is fixed in GnuTLS 3.7.7.
          (if (eq? err error/premature-termination)
              the-eof-object
              (apply throw key err proc rest)))))

    (if (eof-object? read)
        0
        read))
  (define (write! bv start count)
    (put-bytevector record bv start count)
    (force-output record)
    count)
  (define (get-position)
    (port-position record))
  (define (set-position! new-position)
    (set-port-position! record new-position))
  (define (close)
    (unless (port-closed? port)
      (close-port port))
    (unless (port-closed? record)
      (close-port record)))

  (define (unbuffered port)
    (setvbuf port 'none)
    port)

  (unbuffered
   (make-custom-binary-input/output-port "gnutls wrapped port" read! write!
                                         get-position set-position!
                                         close)))

(define* (tls-wrap port server #:key (verify-certificate? #t))
  "Return PORT wrapped in a TLS connection to SERVER.  SERVER must be a DNS
host name without trailing dot."
  (define (log level str)
    (format (current-error-port)
            "gnutls: [~a|~a] ~a" (getpid) level str))

  (load-gnutls)

  (let ((session  (make-session connection-end/client))
        (ca-certs (x509-certificate-directory)))
    ;; Some servers such as 'cloud.github.com' require the client to support
    ;; the 'SERVER NAME' extension.  However, 'set-session-server-name!' is
    ;; not available in older GnuTLS releases.  See
    ;; <http://bugs.gnu.org/18526> for details.
    (if (module-defined? (resolve-interface '(gnutls))
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

    (set-session-credentials! session
                              (if verify-certificate?
                                  (make-credendials-with-ca-trust-files
                                   ca-certs)
                                  (make-certificate-credentials)))

    ;; Uncomment the following lines in case of debugging emergency.
    ;;(set-log-level! 10)
    ;;(set-log-procedure! log)

    (let loop ((retries 5))
      (catch 'gnutls-error
        (lambda ()
          (handshake session))
        (lambda (key err proc . rest)
          (cond ((eq? err error/warning-alert-received)
                 ;; Like Wget, do no stop upon non-fatal alerts such as
                 ;; 'alert-description/unrecognized-name'.
                 (format (current-error-port)
                         "warning: TLS warning alert received: ~a~%"
                         (alert-description->string (alert-get session)))
                 (handshake session))
                (else
                 (if (or (fatal-error? err) (zero? retries))
                     (apply throw key err proc rest)
                     (begin
                       ;; We got 'error/again' or similar; try again.
                       (format (current-error-port)
                               "warning: TLS non-fatal error: ~a~%"
                               (error->string err))
                       (loop (- retries 1)))))))))

    ;; Verify the server's certificate if needed.
    (when verify-certificate?
      (catch 'tls-certificate-error
        (lambda ()
          (assert-valid-server-certificate session server))
        (lambda args
          (close-port port)
          (apply throw args))))

    (let ((record (session-record-port session)))
      (setvbuf record 'block)
      (if (module-defined? (resolve-interface '(gnutls))
                           'set-session-record-port-close!) ;GnuTLS >= 3.7.7
          (let ((close-wrapped-port (lambda (_) (close-port port))))
            (set-session-record-port-close! record close-wrapped-port)
            record)
          (wrap-record-port-for-gnutls<3.7.7 record port)))))

(define (ensure-uri-reference uri-or-string)
  (cond
   ((string? uri-or-string) (string->uri-reference uri-or-string))
   ((uri-reference? uri-or-string) uri-or-string)
   (else (error "Invalid URI-reference" uri-or-string))))

(define (setup-http-tunnel port uri)
  "Establish over PORT an HTTP tunnel to the destination server of URI."
  (define target
    (string-append (uri-host uri) ":"
                   (number->string
                    (or (uri-port uri)
                        (match (uri-scheme uri)
                          ('http 80)
                          ('https 443))))))
  (format port "CONNECT ~a HTTP/1.1\r\n" target)
  (format port "Host: ~a\r\n\r\n" target)
  (force-output port)
  (read-response port))

(define* (open-socket-for-uri uri-or-string
                              #:key (verify-certificate? #t))
  "Return an open input/output port for a connection to URI-OR-STRING.
When VERIFY-CERTIFICATE? is true, verify HTTPS server certificates."
  (define uri
    (ensure-uri-reference uri-or-string))
  (define https?
    (eq? 'https (uri-scheme uri)))

  (define (open-socket)
    (define http-proxy
      (if https? (current-https-proxy) (current-http-proxy)))
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

  (let ((s (open-socket)))
    ;; Buffer input and output on this port.
    (setvbuf s 'block %http-receive-buffer-size)

    (when (and https? (current-https-proxy))
      (setup-http-tunnel s uri))

    (if https?
        (tls-wrap s (uri-host uri)
                  #:verify-certificate? verify-certificate?)
        s)))

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

(define* (http-request uri #:key
                       (body #f)
                       (verify-certificate? #t)
                       (port (open-socket-for-uri uri
                                                  #:verify-certificate?
                                                  verify-certificate?))
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
  "Connect to the server corresponding to URI and ask for the resource,
using METHOD, defaulting to ‘GET’.  If you already have a port open,
pass it as PORT.  The port will be closed at the end of the request
unless KEEP-ALIVE? is true.  Any extra headers in the alist HEADERS will
be added to the request.

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

If PORT is false, URI denotes an HTTPS URL, and VERIFY-CERTIFICATE? is
true, verify X.509 certificates against those available in
X509-CERTIFICATE-DIRECTORY.

Returns two values: the response read from the server, and the response
body as a string, bytevector, #f value, or as a port (if STREAMING? is
true)."
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

(define-syntax-rule (define-http-verb http-verb method doc)
  (define* (http-verb uri #:key
                      (body #f)
                      (verify-certificate? #t)
                      (port (open-socket-for-uri uri
                                                 #:verify-certificate?
                                                 verify-certificate?))
                      (version '(1 . 1))
                      (keep-alive? #f)
                      (headers '())
                      (decode-body? #t)
                      (streaming? #f))
    doc
    (http-request uri
                  #:body body #:method method
                  #:port port #:version version #:keep-alive? keep-alive?
                  #:headers headers #:decode-body? decode-body?
                  #:verify-certificate? verify-certificate?
                  #:streaming? streaming?)))

(define-http-verb http-get
  'GET
  "Fetch message headers for the given URI using the HTTP \"GET\"
method.

This function invokes ‘http-request’, with the \"GET\" method.  See
‘http-request’ for full documentation on the various keyword arguments
that are accepted by this function.

Returns two values: the resulting response, and the response body.")

(define-http-verb http-head
  'HEAD
  "Fetch message headers for the given URI using the HTTP \"HEAD\"
method.

This function invokes ‘http-request’, with the \"HEAD\" method.  See
‘http-request’ for full documentation on the various keyword arguments
that are accepted by this function.

Returns two values: the resulting response, and ‘#f’.  Responses to HEAD
requests do not have a body.  The second value is only returned so that
other procedures can treat all of the http-foo verbs identically.")

(define-http-verb http-post
  'POST
  "Post data to the given URI using the HTTP \"POST\" method.

This function invokes ‘http-request’, with the \"POST\" method.  See
‘http-request’ for full documentation on the various keyword arguments
that are accepted by this function.

Returns two values: the resulting response, and the response body.")

(define-http-verb http-put
  'PUT
  "Put data at the given URI using the HTTP \"PUT\" method.

This function invokes ‘http-request’, with the \"PUT\" method.  See
‘http-request’ for full documentation on the various keyword arguments
that are accepted by this function.

Returns two values: the resulting response, and the response body.")

(define-http-verb http-delete
  'DELETE
  "Delete data at the given URI using the HTTP \"DELETE\" method.

This function invokes ‘http-request’, with the \"DELETE\" method.  See
‘http-request’ for full documentation on the various keyword arguments
that are accepted by this function.

Returns two values: the resulting response, and the response body.")

(define-http-verb http-trace
  'TRACE
  "Send an HTTP \"TRACE\" request.

This function invokes ‘http-request’, with the \"TRACE\" method.  See
‘http-request’ for full documentation on the various keyword arguments
that are accepted by this function.

Returns two values: the resulting response, and the response body.")

(define-http-verb http-options
  'OPTIONS
  "Query characteristics of an HTTP resource using the HTTP \"OPTIONS\"
method.

This function invokes ‘http-request’, with the \"OPTIONS\" method.  See
‘http-request’ for full documentation on the various keyword arguments
that are accepted by this function.

Returns two values: the resulting response, and the response body.")
