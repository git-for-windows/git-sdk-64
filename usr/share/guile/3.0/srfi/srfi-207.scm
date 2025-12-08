;;;; SRFI 207: String-notated bytevectors
;;;;
;;;; Copyright (C) 2025 Free Software Foundation, Inc.
;;;;
;;;; This library is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public License
;;;; as published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this program.  If not, see
;;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; This is an implementation of SRFI 207: String-notated bytevectors.
;;;
;;; Code:

(define-module (srfi srfi-207)
  #:use-module ((ice-9 iconv) #:select (string->bytevector))
  #:use-module ((rnrs arithmetic bitwise) #:select (bitwise-and bitwise-ior))
  #:use-module ((rnrs bytevectors)
                #:select (bytevector->u8-list
                          bytevector-u8-ref
                          bytevector-u8-set!
                          string->utf8
                          u8-list->bytevector))
  #:use-module ((scheme base)
                #:select (bytevector-append
                          binary-port?
                          bytevector
                          bytevector-copy
                          bytevector-copy!
                          bytevector-length
                          bytevector-u8-ref
                          bytevector-u8-set!
                          bytevector?
                          define-record-type
                          eof-object
                          get-output-bytevector
                          let-values
                          make-bytevector
                          open-output-bytevector
                          read-string
                          utf8->string
                          write-bytevector
                          write-string
                          write-u8))
  #:use-module ((srfi srfi-1)
                #:select (append-map! circular-list fold list-tabulate unfold))
  #:use-module ((srfi srfi-43) #:select (vector-unfold))
  #:use-module ((srfi srfi-60) #:select (arithmetic-shift bit-field))
  #:export (base64->bytevector
            bytestring
            bytestring->list
            bytestring-break
            bytestring-error?
            bytestring-index
            bytestring-index-right
            bytestring-join
            bytestring-pad
            bytestring-pad-right
            bytestring-replace
            bytestring-span
            bytestring-split
            bytestring-trim
            bytestring-trim-both
            bytestring-trim-right
            bytestring<=?
            bytestring<?
            bytestring>=?
            bytestring>?
            bytevector->base64
            bytevector->hex-string
            hex-string->bytevector
            make-bytestring
            make-bytestring!
            make-bytestring-generator
            read-textual-bytestring
            write-binary-bytestring
            write-textual-bytestring))

(cond-expand-provide (current-module) '(srfi-207))

;; This awkwardness is because read.scm (not a module, included via
;; boot-9) also needs to be able to read bytestrings.
(define &bytestring-error
  (hashq-ref %boot-9-shared-internal-state '&bytestring-error))
(define bytestring-error
  (hashq-ref %boot-9-shared-internal-state 'bytestring-error))
(define read-bytestring-content
  (hashq-ref %boot-9-shared-internal-state 'read-bytestring-content))

(define bytestring-error? (exception-predicate &bytestring-error))

;; From the upstream 207.sld library definition
(define-syntax assume
  (syntax-rules ()
    ((_ pred) (unless pred (error "invalid assumption:" (quote pred))))
    ((_ pred msg ...) (unless pred (error msg ...)))))

(define common-base64-encoding
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

(define standard-base64-encode-table ; RFC 4648 section 4
  (bytestring common-base64-encoding "+/="))

(define url&filename-safe-base64-encode-table ; RFC 4648 section 5
  (bytestring common-base64-encoding "-_="))

(define (get-base64-encode-table digits)
  (cond
   ((string= "+/" digits) standard-base64-encode-table)
   ((string= "-_" digits) url&filename-safe-base64-encode-table)
   (else (bytestring common-base64-encoding
                     (string->bytevector digits "ASCII")
                     "="))))

(define outside-char 99) ; luft-balloons
(define pad-char 101)    ; dalmations

(define base64-common-decode-table
  ;; Everything except the digits
  (let ((bv (make-bytevector 256 outside-char)))
    (do ((i 0 (1+ i)))
        ((= i (string-length common-base64-encoding)))
      (let ((c (string-ref common-base64-encoding i)))
        (bytevector-u8-set! bv (char->integer c) i)))
    (bytevector-u8-set! bv 61 pad-char)
    bv))

(define (make-base64-decode-table digits)
  (let ((bv (bytevector-copy base64-common-decode-table)))
    (bytevector-u8-set! bv (char->integer (string-ref digits 0)) 62)
    (bytevector-u8-set! bv (char->integer (string-ref digits 1)) 63)
    bv))

;; RFC 4648 sections 4 and 5
(define standard-base64-decode-table (make-base64-decode-table "+/"))
(define url&filename-safe-base64-decode-table (make-base64-decode-table "-_"))

(define (get-base64-decode-table digits)
  (cond
   ((string= "+/" digits) standard-base64-decode-table)
   ((string= "-_" digits) url&filename-safe-base64-decode-table)
   (else (make-base64-decode-table digits))))

(include-from-path "srfi/srfi-207/upstream/base64.scm")
(include-from-path "srfi/srfi-207/upstream/bytestrings-impl.scm")

(define (make-bytestring! bvec at parts)
  (let lp ((parts parts)
           (i at))
    (unless (null? parts)
      (let ((x (car parts)))
        (cond
         ((and (exact-integer? x) (<= 0 x 255))
          (bytevector-u8-set! bvec i x)
          (lp (cdr parts) (1+ i)))
         ((and (char? x) (char<=? x #\delete))
          (bytevector-u8-set! bvec i (char->integer x))
          (lp (cdr parts) (1+ i)))
         ((bytevector? x)
          (bytevector-copy! bvec i x 0 (bytevector-length x))
          (lp (cdr parts) (+ i (bytevector-length x))))
         ((string? x)
          (let ((n (string-length x))
                (utf8 (string->utf8 x)))
            (unless (= n (bytevector-length utf8))
              (bytestring-error "bytestring string part is not ASCII" x))
            (bytevector-copy! bvec i utf8 0 n)
            (lp (cdr parts) (+ i (string-length x)))))
         (else
          (bytestring-error "invalid bytestring string part" x)))))))

(define (make-bytestring parts)
  (define (byte-len x)
    (cond
     ((and (integer? x) (<= 0 x 255)) 1)
     ((and (char? x) (char<=? #\delete)) 1)
     ((bytevector? x) (bytevector-length x))
     ((and (string? x) (string-every char-set:ascii x)) (string-length x))
     (else (bytestring-error "invalid bytestring argument" x))))
  (let* ((n (fold (λ (part total) (+ total (byte-len part)))
                  0 parts))
         (result (make-bytevector n)))
    (make-bytestring! result 0 parts)
    result))

(define (bytevector->hex-string bv)
  (assume (bytevector? bv))
  (define (integer->hex-char n)
    (case n
      ((0) #\0) ((1) #\1) ((2) #\2) ((3) #\3) ((4) #\4)
      ((5) #\5) ((6) #\6) ((7) #\7) ((8) #\8) ((9) #\9)
      ((10) #\a) ((11) #\b) ((12) #\c) ((13) #\d) ((14) #\e) ((15) #\f)
      (else (error "Should not be possible, half-byte out of hex range:" n))))
  (list->string
   (let loop ((i (1- (bytevector-length bv))) (result '()))
     (if (= i -1)
         result
         (let ((b (bytevector-u8-ref bv i)))
           (loop (1- i)
                 (cons* (integer->hex-char (ash b -4))
                        (integer->hex-char (logand b #x0f))
                        result)))))))

(define bytestring-join
  (case-lambda
   ((bstrings delimiter) (bytestring-join bstrings delimiter 'infix))
   ((bstrings delimiter grammar)
    (assume (or (pair? bstrings) (null? bstrings)))
    (let ((delim-bv (bytestring delimiter)))
      (define (alternate! l1 l2) (append-map! list l1 l2))
      (define (infix-join)
        (if (or (null? bstrings) (null? (cdr bstrings)))
            bstrings
            (cons (car bstrings)
                  (alternate! (circular-list delim-bv) (cdr bstrings)))))
      (apply bytevector-append
             (case grammar
               ((infix) (infix-join))
               ((prefix) (alternate! (circular-list delim-bv) bstrings))
               ((suffix) (alternate! bstrings (circular-list delim-bv)))
               ((strict-infix)
                (when (null? bstrings)
                  (bytestring-error "empty list with strict-infix grammar"))
                (infix-join))
               (else (bytestring-error "invalid grammar" grammar))))))))

(define read-textual-bytestring
  (case-lambda
   ((prefix) (read-textual-bytestring prefix (current-input-port)))
   ((prefix in)
    (unless (boolean? prefix)
      (scm-error 'wrong-type-arg "read-textual-bytestring"
                 "Non-boolean prefix argument: ~s" (list prefix) (list prefix)))
    (when prefix
      (let ((s (read-string 3 in)))
        (cond ((eof-object? s)
               (bytestring-error "end of input within bytestring content"))
              ((string=? s "#u8") #t)
              (else (bytestring-error "invalid bytestring prefix" s)))))
    (read-bytestring-content in))))
