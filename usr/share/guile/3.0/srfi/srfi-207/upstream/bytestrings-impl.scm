;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;; Utility

(define (ascii-char? x)
  (and (char? x) (char<=? x #\delete)))

(define (uint8? x)
  (and (exact-integer? x) (<= 0 x 255)))

(define (string-ascii? obj)
  (and (string? obj)
       (string-every (lambda (c) (char<=? c #\delete)) obj)
       #t))

;;;; Constructors

(define (%write-bytestring-segment obj port)
  ((cond ((and (exact-integer? obj) (<= 0 obj 255)) write-u8)
         ((and (char? obj) (char<=? obj #\delete)) write-char-binary)
         ((bytevector? obj) write-bytevector)
         ((string-ascii? obj) write-string-binary)
         (else
          (bytestring-error "invalid bytestring element" obj)))
   obj
   port))

;; If your Scheme allows binary ports to function as textual ports,
;; get rid of this dance.
(define (write-char-binary c port)
  (write-u8 (char->integer c) port))

(define (write-string-binary s port)
  (string-for-each (lambda (c)
                     (write-char-binary c port))
                   s))

(define (bytestring . args)
  (if (null? args) (bytevector) (make-bytestring args)))

;;;; Conversion

;;; Hex string conversion

(define (hex-string->bytevector hex-str)
  (unless (string? hex-str)
    (bytestring-error "invalid hex-str argument" hex-str))
  (let ((sn (string-length hex-str)))
    (unless (even? sn)
      (bytestring-error "incomplete hexadecimal string" hex-str))
    (let* ((result (make-bytevector (/ sn 2))))
      (do ((si 0 (+ si 2))
           (vi 0 (1+ vi)))
          ((= si sn) result)
        (let* ((s (substring hex-str si (+ si 2)))
               (n (string->number s 16)))
          (unless n
            (bytestring-error "invalid hexadecimal sequence in hex-str"
                              s hex-str))
          (bytevector-u8-set! result vi n))))))

(define bytevector->base64
  (case-lambda
    ((bvec) (bytevector->base64 bvec "+/"))
    ((bvec digits)
     (assume (bytevector? bvec))
     (assume (string? digits))
     (assume (= 2 (string-length digits)))
     (utf8->string (base64-encode-bytevector bvec digits)))))

(define base64->bytevector
  (case-lambda
    ((base64-string) (base64->bytevector base64-string "+/"))
    ((base64-string digits)
     (assume (string? base64-string))
     (assume (string? digits))
     (assume (= 2 (string-length digits)))
     (decode-base64-string base64-string digits))))

(define bytestring->list
  (case-lambda
    ((bstring) (bytestring->list bstring 0 (bytevector-length bstring)))
    ((bstring start)
     (bytestring->list bstring start (bytevector-length bstring)))
    ((bstring start end)
     (assume (bytevector? bstring))
     (assume (and (exact-integer? start) (>= start 0))
             "invalid start index"
             start
             bstring)
     (assume (and (exact-integer? end) (<= 0 end (bytevector-length bstring)))
             "invalid end index"
             end
             bstring)
     (assume (>= end start) "invalid indices" start end)
     (unfold (lambda (i) (= i end))
             (lambda (i)
               (let ((b (bytevector-u8-ref bstring i)))
                 (if (and (>= b #x20) (< b #x7f))
                     (integer->char b)
                     b)))
             1+
             start))))

(define (make-bytestring-generator . args)
  "Return a thunk that returns the consecutive bytes, one per
invocation, of the bytevector that (apply bytestring args) would
produce. The elements of args are validated before
make-bytestring-generator returns, and if invalid, an error satisfying
bytestring-error? is raised."
  (define (generate)
    (if (null? args)
        (eof-object)
        (let ((x (car args)))
          (cond
           ((integer? x)
            (set! args (cdr args))
            x)
           ((char? x)
            (set! args (cdr args))
            (char->integer x))
           ((bytevector? x)
            (set! args (append! (bytevector->u8-list x) (cdr args)))
            (generate))
           ((string? x)
            (set! args (append! (string->list x) (cdr args)))
            (generate))
           (else
            (bytestring-error "invalid bytestring segment" x))))))
  (for-each (λ (arg)
              (or (bytevector? arg)
                  (ascii-char? arg)
                  (uint8? arg)
                  (string-ascii? arg)
                  (bytestring-error "invalid bytestring segment" arg)))
            args)
  generate)

;;;; Selection

(define (%bytestring-pad-left-or-right bstring len char-or-u8 right)
  (assume (bytevector? bstring))
  (assume (and (exact-integer? len) (not (negative? len))))
  (assume (or (ascii-char? char-or-u8) (uint8? char-or-u8)))
  (let ((pad-len (- len (bytevector-length bstring)))
        (pad-byte (if (char? char-or-u8)
                      (char->integer char-or-u8)
                      char-or-u8)))
    (if (<= pad-len 0)
        (bytevector-copy bstring)
        (let ((padded (make-bytevector len pad-byte)))
          (bytevector-copy! padded (if right 0 pad-len) bstring)
          padded))))

(define (bytestring-pad bstring len char-or-u8)
  (%bytestring-pad-left-or-right bstring len char-or-u8 #f))

(define (bytestring-pad-right bstring len char-or-u8)
  (%bytestring-pad-left-or-right bstring len char-or-u8 #t))

(define (bytestring-trim bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (let ((new-start (bytestring-index bstring (negate pred))))
    (if new-start
        (bytevector-copy bstring new-start)
        (bytevector))))

(define (bytestring-trim-right bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (cond ((bytestring-index-right bstring (negate pred)) =>
         (lambda (end-1)
           (bytevector-copy bstring 0 (+ 1 end-1))))
        (else (bytevector))))

(define (bytestring-trim-both bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (let ((neg-pred (negate pred)))
    (cond ((bytestring-index bstring neg-pred) =>
           (lambda (start)
             (bytevector-copy bstring
                              start
                              (+ (bytestring-index-right bstring neg-pred)
                                 1))))
          (else (bytevector)))))

;;;; Replacement

(define bytestring-replace
  (case-lambda
    ((bstring1 bstring2 start end)
     (bytestring-replace bstring1
                         bstring2
                         start
                         end
                         0
                         (bytevector-length bstring2)))
    ((bstring1 bstring2 start1 end1 start2 end2)
     (assume (bytevector? bstring1))
     (assume (bytevector? bstring2))
     (assume (and (exact-integer? start1) (>= start1 0) (<= start1 end1))
             "invalid start index"
             start1)
     (assume (and (exact-integer? end1)
                  (<= 0 end1 (bytevector-length bstring1)))
             "invalid end index"
             bstring1)
     (assume (and (exact-integer? start2) (>= start2 0) (<= start2 end2))
             "invalid start index"
             start2)
     (assume (and (exact-integer? end2) (<= 0 end2 (bytevector-length bstring2)))
             "invalid end index"
             bstring2)
     (if (and (= start1 end1) (= start2 end2))
         (bytevector-copy bstring1)    ; replace no bits with no bits
         (let* ((b1-len (bytevector-length bstring1))
                (sub-len (- end2 start2))
                (new-len (+ sub-len (- b1-len (- end1 start1))))
                (bs-new (make-bytevector new-len)))
           (bytevector-copy! bs-new 0 bstring1 0 start1)
           (bytevector-copy! bs-new start1 bstring2 start2 end2)
           (bytevector-copy! bs-new (+ start1 sub-len) bstring1 end1 b1-len)
           bs-new)))))

;;;; Comparison

(define (%bytestring-prefix-length bstring1 bstring2)
  (let ((end (min (bytevector-length bstring1)
                  (bytevector-length bstring2))))
    (if (eq? bstring1 bstring2)  ; fast path
        end
        (let lp ((i 0))
          (if (or (>= i end)
                  (not (= (bytevector-u8-ref bstring1 i)
                          (bytevector-u8-ref bstring2 i))))
              i
              (lp (+ i 1)))))))

;;; Primitive bytevector comparison functions.

(define (%bytestring-compare bstring1 bstring2 res< res= res>)
  (let ((len1 (bytevector-length bstring1))
        (len2 (bytevector-length bstring2)))
    (let ((match (%bytestring-prefix-length bstring1 bstring2)))
      (if (= match len1)
          (if (= match len2) res= res<)
          (if (= match len2)
              res>
              (if (< (bytevector-u8-ref bstring1 match)
                     (bytevector-u8-ref bstring2 match))
                  res<
                  res>))))))

(define (bytestring<? bstring1 bstring2)
  (assume (bytevector? bstring1))
  (assume (bytevector? bstring2))
  (and (not (eq? bstring1 bstring2))
       (%bytestring-compare bstring1 bstring2 #t #f #f)))

(define (bytestring>? bstring1 bstring2)
  (assume (bytevector? bstring1))
  (assume (bytevector? bstring2))
  (and (not (eq? bstring1 bstring2))
       (%bytestring-compare bstring1 bstring2 #f #f #t)))

(define (bytestring<=? bstring1 bstring2)
  (assume (bytevector? bstring1))
  (assume (bytevector? bstring2))
  (or (eq? bstring1 bstring2)
      (%bytestring-compare bstring1 bstring2 #t #t #f)))

(define (bytestring>=? bstring1 bstring2)
  (assume (bytevector? bstring1))
  (assume (bytevector? bstring2))
  (or (eqv? bstring1 bstring2)
      (%bytestring-compare bstring1 bstring2 #f #t #t)))

;;;; Searching

(define bytestring-index
  (case-lambda
    ((bstring pred) (bytestring-index bstring pred 0))
    ((bstring pred start)
     (bytestring-index bstring pred start (bytevector-length bstring)))
    ((bstring pred start end)
     (assume (bytevector? bstring))
     (assume (procedure? pred))
     (assume (and (exact-integer? start) (not (negative? start))))
     (assume (and (exact-integer? end) (not (negative? end))))
     (let lp ((i start))
       (and (< i end)
            (if (pred (bytevector-u8-ref bstring i))
                i
                (lp (+ i 1))))))))

(define bytestring-index-right
  (case-lambda
    ((bstring pred) (bytestring-index-right bstring pred 0))
    ((bstring pred start)
     (bytestring-index-right bstring pred start (bytevector-length bstring)))
    ((bstring pred start end)
     (assume (bytevector? bstring))
     (assume (procedure? pred))
     (assume (and (exact-integer? start) (not (negative? start))))
     (assume (and (exact-integer? end) (not (negative? end))))
     (let lp ((i (- end 1)))
       (and (>= i start)
            (if (pred (bytevector-u8-ref bstring i))
                i
                (lp (- i 1))))))))

(define (bytestring-break bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (cond ((bytestring-index bstring pred) =>
         (lambda (len)
           (values (bytevector-copy bstring 0 len)
                   (bytevector-copy bstring len))))
        (else (values (bytevector-copy bstring) (bytevector)))))

(define (bytestring-span bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (cond ((bytestring-index bstring (negate pred)) =>
         (lambda (len)
           (values (bytevector-copy bstring 0 len)
                   (bytevector-copy bstring len))))
        (else (values (bytevector-copy bstring) (bytevector)))))

;;;; Splitting

(define (%find-right bstring byte end)
  (bytestring-index-right bstring (lambda (b) (= b byte)) 0 end))

(define (%bytestring-infix-split bstring delimiter)
  (let lp ((token-end (bytevector-length bstring)) (split '()))
    (cond ((< token-end 0) split)
          ((%find-right bstring delimiter token-end) =>
           (lambda (token-start-1)
             (lp token-start-1
                 (cons (bytevector-copy bstring (+ 1 token-start-1)
                                                token-end)
                       split))))
          (else (cons (bytevector-copy bstring 0 token-end) split)))))

(define (%trim-byte bstring byte)
  (bytestring-trim bstring (lambda (b) (= b byte))))

(define (%trim-right-byte bstring byte)
  (bytestring-trim-right bstring (lambda (b) (= b byte))))

(define (%bytestring-split/trim-outliers bstring delimiter grammar)
  (let ((trimmed (case grammar
                  ((infix strict-infix) bstring)
                  ((prefix) (%trim-byte bstring delimiter))
                  ((suffix) (%trim-right-byte bstring delimiter)))))
    (%bytestring-infix-split trimmed delimiter)))

(define bytestring-split
  (case-lambda
    ((bstring delimiter) (bytestring-split bstring delimiter 'infix))
    ((bstring delimiter grammar)
     (assume (bytevector? bstring))
     (assume (or (ascii-char? delimiter) (uint8? delimiter)))
     (unless (memv grammar '(infix strict-infix prefix suffix))
       (bytestring-error "invalid grammar" grammar))
     (if (zero? (bytevector-length bstring))
         '()
         (%bytestring-split/trim-outliers
          bstring
          (if (char? delimiter) (char->integer delimiter) delimiter)
          grammar)))))

;;;; I/O

(define backslash-codepoints
  '((7 . #\a) (8 . #\b) (9 . #\t) (10 . #\n) (13 . #\r)
    (34 . #\") (92 . #\\) (124 . #\|)))

(define write-textual-bytestring
  (case-lambda
   ((bstring)
    (write-textual-bytestring bstring (current-output-port)))
   ((bstring port)
    (parameterize ((current-output-port port))
      (write-string "#u8\"")
      (do ((i 0 (1+ i)))
          ((= i (bytevector-length bstring)))
        (let ((b (bytevector-u8-ref bstring i)))
          (cond ((assv b backslash-codepoints) =>
                 (lambda (p)
                   (write-char #\\)
                   (write-char (cdr p))))
                ((and (>= b #x20) (<= b #x7e))
                 (write-char (integer->char b)))
                (else
                 (write-string "\\x")
                 (write-string (number->string b 16))
                 (write-char #\;)))))
      (write-char #\")))))

(define (write-binary-bytestring port . args)
  (assume (binary-port? port))
  (for-each (lambda (seg) (%write-bytestring-segment seg port)) args))
