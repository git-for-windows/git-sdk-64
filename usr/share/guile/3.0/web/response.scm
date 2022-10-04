;;; HTTP response objects

;; Copyright (C) 2010, 2011, 2012, 2013, 2014, 2015 Free Software Foundation, Inc.

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

(define-module (web response)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (web http)
  #:export (response?
            response-version
            response-code
            response-reason-phrase
            response-headers
            response-port
            read-response
            build-response
            adapt-response-version
            write-response

            response-must-not-include-body?
            response-body-port
            read-response-body
            write-response-body

            ;; General headers
            ;;
            response-cache-control
            response-connection
            response-date
            response-pragma
            response-trailer
            response-transfer-encoding
            response-upgrade
            response-via
            response-warning

            ;; Entity headers
            ;;
            response-allow
            response-content-encoding
            response-content-language
            response-content-length
            response-content-location
            response-content-md5
            response-content-range
            response-content-type
            text-content-type?
            response-expires
            response-last-modified

            ;; Response headers
            ;;
            response-accept-ranges
            response-age
            response-etag
            response-location
            response-proxy-authenticate
            response-retry-after
            response-server
            response-vary
            response-www-authenticate))


(define-record-type <response>
  (make-response version code reason-phrase headers port)
  response?
  (version response-version)
  (code response-code)
  (reason-phrase %response-reason-phrase)
  (headers response-headers)
  (port response-port))

(define (bad-response message . args)
  (throw 'bad-response message args))

(define (non-negative-integer? n)
  (and (number? n) (>= n 0) (exact? n) (integer? n)))
                                    
(define (validate-headers headers)
  (if (pair? headers)
      (let ((h (car headers)))
        (if (pair? h)
            (let ((k (car h)) (v (cdr h)))
              (if (valid-header? k v)
                  (validate-headers (cdr headers))
                  (bad-response "Bad value for header ~a: ~s" k v)))
            (bad-response "Header not a pair: ~a" h)))
      (if (not (null? headers))
          (bad-response "Headers not a list: ~a" headers))))

(define* (build-response #:key (version '(1 . 1)) (code 200) reason-phrase
                         (headers '()) port (validate-headers? #t))
  "Construct an HTTP response object. If VALIDATE-HEADERS? is true,
the headers are each run through their respective validators."
  (cond
   ((not (and (pair? version)
              (non-negative-integer? (car version))
              (non-negative-integer? (cdr version))))
    (bad-response "Bad version: ~a" version))
   ((not (and (non-negative-integer? code) (< code 600)))
    (bad-response "Bad code: ~a" code))
   ((and reason-phrase (not (string? reason-phrase)))
    (bad-response "Bad reason phrase" reason-phrase))
   (else
    (if validate-headers?
        (validate-headers headers))))
  (make-response version code reason-phrase headers port))

(define *reason-phrases*
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (200 . "OK")
    (201 . "Created")
    (202 . "Accepted")
    (203 . "Non-Authoritative Information")
    (204 . "No Content")
    (205 . "Reset Content")
    (206 . "Partial Content")
    (300 . "Multiple Choices")
    (301 . "Moved Permanently")
    (302 . "Found")
    (303 . "See Other")
    (304 . "Not Modified")
    (305 . "Use Proxy")
    (307 . "Temporary Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (402 . "Payment Required")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (406 . "Not Acceptable")
    (407 . "Proxy Authentication Required")
    (408 . "Request Timeout")
    (409 . "Conflict")
    (410 . "Gone")
    (411 . "Length Required")
    (412 . "Precondition Failed")
    (413 . "Request Entity Too Large")
    (414 . "Request-URI Too Long")
    (415 . "Unsupported Media Type")
    (416 . "Requested Range Not Satisfiable")
    (417 . "Expectation Failed")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout")
    (505 . "HTTP Version Not Supported")))

(define (code->reason-phrase code)
  (or (assv-ref *reason-phrases* code)
      "(Unknown)"))

(define (response-reason-phrase response)
  "Return the reason phrase given in RESPONSE, or the standard
reason phrase for the response's code."
  (or (%response-reason-phrase response)
      (code->reason-phrase (response-code response))))

(define (text-content-type? type)
  "Return #t if TYPE, a symbol as returned by `response-content-type',
represents a textual type such as `text/plain'."
  (let ((type (symbol->string type)))
    (or (string-prefix? "text/" type)
        (string-suffix? "/xml" type)
        (string-suffix? "+xml" type))))

(define (read-response port)
  "Read an HTTP response from PORT.

As a side effect, sets the encoding on PORT to
ISO-8859-1 (latin-1), so that reading one character reads one byte.  See
the discussion of character sets in \"HTTP Responses\" in the manual,
for more information."
  (set-port-encoding! port "ISO-8859-1")
  (call-with-values (lambda () (read-response-line port))
    (lambda (version code reason-phrase)
      (make-response version code reason-phrase (read-headers port) port))))

(define (adapt-response-version response version)
  "Adapt the given response to a different HTTP version.  Returns a new
HTTP response.

The idea is that many applications might just build a response for the
default HTTP version, and this method could handle a number of
programmatic transformations to respond to older HTTP versions (0.9 and
1.0).  But currently this function is a bit heavy-handed, just updating
the version field."
  (build-response #:code (response-code response)
                  #:version version
                  #:headers (response-headers response)
                  #:port (response-port response)))

(define (write-response r port)
  "Write the given HTTP response to PORT.

Returns a new response, whose ‘response-port’ will continue writing
on PORT, perhaps using some transfer encoding."
  (write-response-line (response-version r) (response-code r)
                       (response-reason-phrase r) port)
  (write-headers (response-headers r) port)
  (put-string port "\r\n")
  (if (eq? port (response-port r))
      r
      (make-response (response-version r) (response-code r)
                     (response-reason-phrase r) (response-headers r) port)))

(define (response-must-not-include-body? r)
  "Returns ‘#t’ if the response R is not permitted to have a body.

This is true for some response types, like those with code 304."
  ;; RFC 2616, section 4.3.
  (or (<= 100 (response-code r) 199)
      (= (response-code r) 204)
      (= (response-code r) 304)))

(define (make-delimited-input-port port len keep-alive?)
  "Return an input port that reads from PORT, and makes sure that
exactly LEN bytes are available from PORT.  Closing the returned port
closes PORT, unless KEEP-ALIVE? is true."
  (define bytes-read 0)

  (define (fail)
    (bad-response "EOF while reading response body: ~a bytes of ~a"
                  bytes-read len))

  (define (read! bv start count)
    ;; Read at most LEN bytes in total.  HTTP/1.1 doesn't say what to do
    ;; when a server provides more than the Content-Length, but it seems
    ;; wise to just stop reading at LEN.
    (let ((count (min count (- len bytes-read))))
      (let loop ((ret (get-bytevector-n! port bv start count)))
        (cond ((eof-object? ret)
               (if (= bytes-read len)
                   0                              ; EOF
                   (fail)))
              ((and (zero? ret) (> count 0))
               ;; Do not return zero since zero means EOF, so try again.
               (loop (get-bytevector-n! port bv start count)))
              (else
               (set! bytes-read (+ bytes-read ret))
               ret)))))

  (define close
    (and (not keep-alive?)
         (lambda ()
           (close-port port))))

  (make-custom-binary-input-port "delimited input port" read! #f #f close))

(define* (response-body-port r #:key (decode? #t) (keep-alive? #t))
  "Return an input port from which the body of R can be read.  The
encoding of the returned port is set according to R's ‘content-type’
header, when it's textual, except if DECODE? is ‘#f’.  Return #f when
no body is available.

When KEEP-ALIVE? is ‘#f’, closing the returned port also closes R's
response port."
  (define port
    (cond
     ((member '(chunked) (response-transfer-encoding r))
      (make-chunked-input-port (response-port r)
                               #:keep-alive? keep-alive?))
     ((response-content-length r)
      => (lambda (len)
           (make-delimited-input-port (response-port r)
                                      len keep-alive?)))
     ((response-must-not-include-body? r)
      #f)
     ((or (memq 'close (response-connection r))
          (and (equal? (response-version r) '(1 . 0))
               (not (memq 'keep-alive (response-connection r)))))
      (response-port r))
     (else
      ;; Here we have a message with no transfer encoding, no
      ;; content-length, and a response that won't necessarily be closed
      ;; by the server.  Not much we can do; assume that the client
      ;; knows how to handle it.
      (response-port r))))

  (when (and decode? port)
    (match (response-content-type r)
      (((? text-content-type?) . props)
       (set-port-encoding! port
                           (or (assq-ref props 'charset)
                               "ISO-8859-1")))
      (_ #f)))

  port)

(define (read-response-body r)
  "Reads the response body from R, as a bytevector.  Returns
‘#f’ if there was no response body."
  (let ((body (and=> (response-body-port r #:decode? #f)
                     get-bytevector-all)))
    ;; Reading a body of length 0 will result in get-bytevector-all
    ;; returning the EOF object.
    (if (eof-object? body)
        #vu8()
        body)))

(define (write-response-body r bv)
  "Write BV, a bytevector, to the port corresponding to the HTTP
response R."
  (put-bytevector (response-port r) bv))

(define-syntax define-response-accessor
  (lambda (x)
    (syntax-case x ()
      ((_ field)
       #'(define-response-accessor field #f))
      ((_ field def) (identifier? #'field)
       #`(define* (#,(datum->syntax
                      #'field
                      (symbol-append 'response- (syntax->datum #'field)))
                   response
                   #:optional (default def))
           (cond
            ((assq 'field (response-headers response)) => cdr)
            (else default)))))))

;; General headers
;;
(define-response-accessor cache-control '())
(define-response-accessor connection '())
(define-response-accessor date #f)
(define-response-accessor pragma '())
(define-response-accessor trailer '())
(define-response-accessor transfer-encoding '())
(define-response-accessor upgrade '())
(define-response-accessor via '())
(define-response-accessor warning '())

;; Entity headers
;;
(define-response-accessor allow '())
(define-response-accessor content-encoding '())
(define-response-accessor content-language '())
(define-response-accessor content-length #f)
(define-response-accessor content-location #f)
(define-response-accessor content-md5 #f)
(define-response-accessor content-range #f)
(define-response-accessor content-type #f)
(define-response-accessor expires #f)
(define-response-accessor last-modified #f)

;; Response headers
;;
(define-response-accessor accept-ranges #f)
(define-response-accessor age #f)
(define-response-accessor etag #f)
(define-response-accessor location #f)
(define-response-accessor proxy-authenticate #f)
(define-response-accessor retry-after #f)
(define-response-accessor server #f)
(define-response-accessor vary '())
(define-response-accessor www-authenticate #f)
