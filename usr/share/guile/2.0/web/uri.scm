;;;; (web uri) --- URI manipulation tools
;;;;
;;;; Copyright (C) 1997,2001,2002,2010,2011,2012,2013 Free Software Foundation, Inc.
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

;;; Commentary:

;; A data type for Universal Resource Identifiers, as defined in RFC
;; 3986.

;;; Code:

(define-module (web uri)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 control)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:export (uri?
            uri-scheme uri-userinfo uri-host uri-port
            uri-path uri-query uri-fragment

            build-uri
            declare-default-port!
            string->uri uri->string
            uri-decode uri-encode
            split-and-decode-uri-path
            encode-and-join-uri-path))

(define-record-type <uri>
  (make-uri scheme userinfo host port path query fragment)
  uri?
  (scheme uri-scheme)
  (userinfo uri-userinfo)
  (host uri-host)
  (port uri-port)
  (path uri-path)
  (query uri-query)
  (fragment uri-fragment))

(define (absolute-uri? obj)
  (and (uri? obj) (uri-scheme obj) #t))

(define (uri-error message . args)
  (throw 'uri-error message args))

(define (positive-exact-integer? port)
  (and (number? port) (exact? port) (integer? port) (positive? port)))

(define (validate-uri scheme userinfo host port path query fragment)
  (cond
   ((not (symbol? scheme))
    (uri-error "Expected a symbol for the URI scheme: ~s" scheme))
   ((and (or userinfo port) (not host))
    (uri-error "Expected a host, given userinfo or port"))
   ((and port (not (positive-exact-integer? port)))
    (uri-error "Expected port to be an integer: ~s" port))
   ((and host (or (not (string? host)) (not (valid-host? host))))
    (uri-error "Expected valid host: ~s" host))
   ((and userinfo (not (string? userinfo)))
    (uri-error "Expected string for userinfo: ~s" userinfo))
   ((not (string? path))
    (uri-error "Expected string for path: ~s" path))
   ((and host (not (string-null? path))
         (not (eqv? (string-ref path 0) #\/)))
    (uri-error "Expected path of absolute URI to start with a /: ~a" path))))

(define* (build-uri scheme #:key userinfo host port (path "") query fragment
                    (validate? #t))
  "Construct a URI object.  SCHEME should be a symbol, PORT
either a positive, exact integer or ‘#f’, and the rest of the
fields are either strings or ‘#f’.  If VALIDATE? is true,
also run some consistency checks to make sure that the constructed URI
is valid."
  (if validate?
      (validate-uri scheme userinfo host port path query fragment))
  (make-uri scheme userinfo host port path query fragment))

;; See RFC 3986 #3.2.2 for comments on percent-encodings, IDNA (RFC
;; 3490), and non-ASCII host names.
;;
(define ipv4-regexp
  (make-regexp "^([0-9.]+)$"))
(define ipv6-regexp
  (make-regexp "^([0-9a-fA-F:.]+)$"))
(define domain-label-regexp
  (make-regexp "^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?$"))
(define top-label-regexp
  (make-regexp "^[a-zA-Z]([a-zA-Z0-9-]*[a-zA-Z0-9])?$"))

(define (valid-host? host)
  (cond
   ((regexp-exec ipv4-regexp host)
    (false-if-exception (inet-pton AF_INET host)))
   ((regexp-exec ipv6-regexp host)
    (false-if-exception (inet-pton AF_INET6 host)))
   (else
    (let lp ((start 0))
      (let ((end (string-index host #\. start)))
        (if end
            (and (regexp-exec domain-label-regexp
                              (substring host start end))
                 (lp (1+ end)))
            (regexp-exec top-label-regexp host start)))))))

(define userinfo-pat
  "[a-zA-Z0-9_.!~*'();:&=+$,-]+")
(define host-pat
  "[a-zA-Z0-9.-]+")
(define ipv6-host-pat
  "[0-9a-fA-F:.]+")
(define port-pat
  "[0-9]*")
(define authority-regexp
  (make-regexp
   (format #f "^//((~a)@)?((~a)|(\\[(~a)\\]))(:(~a))?$"
           userinfo-pat host-pat ipv6-host-pat port-pat)))

(define (parse-authority authority fail)
  (if (equal? authority "//")
      ;; Allow empty authorities: file:///etc/hosts is a synonym of
      ;; file:/etc/hosts.
      (values #f #f #f)
      (let ((m (regexp-exec authority-regexp authority)))
        (if (and m (valid-host? (or (match:substring m 4)
                                    (match:substring m 6))))
            (values (match:substring m 2)
                    (or (match:substring m 4)
                        (match:substring m 6))
                    (let ((port (match:substring m 8)))
                      (and port (not (string-null? port))
                           (string->number port))))
            (fail)))))


;;; RFC 3986, #3.
;;;
;;;   URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
;;;
;;;   hier-part   = "//" authority path-abempty
;;;               / path-absolute
;;;               / path-rootless
;;;               / path-empty

(define scheme-pat
  "[a-zA-Z][a-zA-Z0-9+.-]*")
(define authority-pat
  "[^/?#]*")
(define path-pat
  "[^?#]*")
(define query-pat
  "[^#]*")
(define fragment-pat
  ".*")
(define uri-pat
  (format #f "^((~a):)?(//~a)?(~a)(\\?(~a))?(#(~a))?$"
          scheme-pat authority-pat path-pat query-pat fragment-pat))
(define uri-regexp
  (make-regexp uri-pat))

(define (string->uri* string)
  "Parse STRING into a URI object.  Return ‘#f’ if the string
could not be parsed."
  (% (let ((m (regexp-exec uri-regexp string)))
       (if (not m) (abort))
       (let ((scheme (let ((str (match:substring m 2)))
                       (and str (string->symbol (string-downcase str)))))
             (authority (match:substring m 3))
             (path (match:substring m 4))
             (query (match:substring m 6))
             (fragment (match:substring m 7)))
         (call-with-values
             (lambda ()
               (if authority
                   (parse-authority authority abort)
                   (values #f #f #f)))
           (lambda (userinfo host port)
             (make-uri scheme userinfo host port path query fragment)))))
     (lambda (k)
       #f)))

(define (string->uri string)
  "Parse STRING into a URI object.  Return ‘#f’ if the string
could not be parsed."
  (let ((uri (string->uri* string)))
    (and uri (uri-scheme uri) uri)))

(define *default-ports* (make-hash-table))

(define (declare-default-port! scheme port)
  "Declare a default port for the given URI scheme."
  (hashq-set! *default-ports* scheme port))

(define (default-port? scheme port)
  (or (not port)
      (eqv? port (hashq-ref *default-ports* scheme))))

(declare-default-port! 'http 80)
(declare-default-port! 'https 443)

(define (uri->string uri)
  "Serialize URI to a string.  If the URI has a port that is the
default port for its scheme, the port is not included in the
serialization."
  (let* ((scheme (uri-scheme uri))
         (userinfo (uri-userinfo uri))
         (host (uri-host uri))
         (port (uri-port uri))
         (path (uri-path uri))
         (query (uri-query uri))
         (fragment (uri-fragment uri)))
    (string-append
     (if scheme
         (string-append (symbol->string scheme) ":")
         "")
     (if host
         (string-append "//"
                        (if userinfo (string-append userinfo "@")
                            "")
                        (if (string-index host #\:)
                            (string-append "[" host "]")
                            host)
                        (if (default-port? (uri-scheme uri) port)
                            ""
                            (string-append ":" (number->string port))))
         "")
     path
     (if query
         (string-append "?" query)
         "")
     (if fragment
         (string-append "#" fragment)
         ""))))


;; like call-with-output-string, but actually closes the port (doh)
(define (call-with-output-string* proc)
  (let ((port (open-output-string)))
    (proc port)
    (let ((str (get-output-string port)))
      (close-port port)
      str)))

(define (call-with-output-bytevector* proc)
  (call-with-values
      (lambda ()
        (open-bytevector-output-port))
    (lambda (port get-bytevector)
      (proc port)
      (let ((bv (get-bytevector)))
        (close-port port)
        bv))))

(define (call-with-encoded-output-string encoding proc)
  (if (string-ci=? encoding "utf-8")
      (string->utf8 (call-with-output-string* proc))
      (call-with-output-bytevector*
       (lambda (port)
         (set-port-encoding! port encoding)
         (proc port)))))

(define (encode-string str encoding)
  (if (string-ci=? encoding "utf-8")
      (string->utf8 str)
      (call-with-encoded-output-string encoding
                                       (lambda (port)
                                         (display str port)))))

(define (decode-string bv encoding)
  (if (string-ci=? encoding "utf-8")
      (utf8->string bv)
      (let ((p (open-bytevector-input-port bv)))
        (set-port-encoding! p encoding)
        (let ((res (read-string p)))
          (close-port p)
          res))))


;; A note on characters and bytes: URIs are defined to be sequences of
;; characters in a subset of ASCII. Those characters may encode a
;; sequence of bytes (octets), which in turn may encode sequences of
;; characters in other character sets.
;;

;; Return a new string made from uri-decoding STR.  Specifically,
;; turn ‘+’ into space, and hex-encoded ‘%XX’ strings into
;; their eight-bit characters.
;;
(define hex-chars
  (string->char-set "0123456789abcdefABCDEF"))

(define* (uri-decode str #:key (encoding "utf-8") (decode-plus-to-space? #t))
  "Percent-decode the given STR, according to ENCODING,
which should be the name of a character encoding.

Note that this function should not generally be applied to a full URI
string. For paths, use ‘split-and-decode-uri-path’ instead. For query
strings, split the query on ‘&’ and ‘=’ boundaries, and decode
the components separately.

Note also that percent-encoded strings encode _bytes_, not characters.
There is no guarantee that a given byte sequence is a valid string
encoding. Therefore this routine may signal an error if the decoded
bytes are not valid for the given encoding. Pass ‘#f’ for ENCODING if
you want decoded bytes as a bytevector directly.  ‘set-port-encoding!’,
for more information on character encodings.

If DECODE-PLUS-TO-SPACE? is true, which is the default, also replace
instances of the plus character (+) with a space character.  This is
needed when parsing application/x-www-form-urlencoded data.

Returns a string of the decoded characters, or a bytevector if
ENCODING was ‘#f’."
  (let* ((len (string-length str))
         (bv
          (call-with-output-bytevector*
           (lambda (port)
             (let lp ((i 0))
               (if (< i len)
                   (let ((ch (string-ref str i)))
                     (cond
                      ((and (eqv? ch #\+) decode-plus-to-space?)
                       (put-u8 port (char->integer #\space))
                       (lp (1+ i)))
                      ((and (< (+ i 2) len) (eqv? ch #\%)
                            (let ((a (string-ref str (+ i 1)))
                                  (b (string-ref str (+ i 2))))
                              (and (char-set-contains? hex-chars a)
                                   (char-set-contains? hex-chars b)
                                   (string->number (string a b) 16))))
                       => (lambda (u8)
                            (put-u8 port u8)
                            (lp (+ i 3))))
                      ((< (char->integer ch) 128)
                       (put-u8 port (char->integer ch))
                       (lp (1+ i)))
                      (else
                       (uri-error "Invalid character in encoded URI ~a: ~s"
                                  str ch))))))))))
    (if encoding
        (decode-string bv encoding)
        ;; Otherwise return raw bytevector
        bv)))

(define ascii-alnum-chars
  (string->char-set
   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))

;; RFC 3986, #2.2.
(define gen-delims
  (string->char-set ":/?#[]@"))
(define sub-delims
  (string->char-set "!$&'()*+,l="))
(define reserved-chars
  (char-set-union gen-delims sub-delims))

;; RFC 3986, #2.3
(define unreserved-chars
  (char-set-union ascii-alnum-chars
                  (string->char-set "-._~")))

;; Return a new string made from uri-encoding STR, unconditionally
;; transforming any characters not in UNESCAPED-CHARS.
;;
(define* (uri-encode str #:key (encoding "utf-8")
                     (unescaped-chars unreserved-chars))
  "Percent-encode any character not in the character set,
UNESCAPED-CHARS.

The default character set includes alphanumerics from ASCII, as well as
the special characters ‘-’, ‘.’, ‘_’, and ‘~’.  Any other character will
be percent-encoded, by writing out the character to a bytevector within
the given ENCODING, then encoding each byte as ‘%HH’, where HH is the
uppercase hexadecimal representation of the byte."
  (define (needs-escaped? ch)
    (not (char-set-contains? unescaped-chars ch)))
  (if (string-index str needs-escaped?)
      (call-with-output-string*
       (lambda (port)
         (string-for-each
          (lambda (ch)
            (if (char-set-contains? unescaped-chars ch)
                (display ch port)
                (let* ((bv (encode-string (string ch) encoding))
                       (len (bytevector-length bv)))
                  (let lp ((i 0))
                    (if (< i len)
                        (let ((byte (bytevector-u8-ref bv i)))
                          (display #\% port)
                          (when (< byte 16)
                            (display #\0 port))
                          (display (string-upcase (number->string byte 16))
                                   port)
                          (lp (1+ i))))))))
          str)))
      str))

(define (split-and-decode-uri-path path)
  "Split PATH into its components, and decode each component,
removing empty components.

For example, ‘\"/foo/bar%20baz/\"’ decodes to the two-element list,
‘(\"foo\" \"bar baz\")’."
  (filter (lambda (x) (not (string-null? x)))
          (map (lambda (s) (uri-decode s #:decode-plus-to-space? #f))
               (string-split path #\/))))

(define (encode-and-join-uri-path parts)
  "URI-encode each element of PARTS, which should be a list of
strings, and join the parts together with ‘/’ as a delimiter.

For example, the list ‘(\"scrambled eggs\" \"biscuits&gravy\")’
encodes as ‘\"scrambled%20eggs/biscuits%26gravy\"’."
  (string-join (map uri-encode parts) "/"))
