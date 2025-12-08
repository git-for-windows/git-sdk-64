;;;; Reduced and heavily modified base64 library from chibi-scheme.
;;;
;;; Copyright (c) 2009-2018 Alex Shinn
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; Decoding

(define (outside-char? x) (eqv? x outside-char))
(define (pad-char? x) (eqv? x pad-char))

(define (decode-base64-string src digits)
  (let ((table (get-base64-decode-table digits)))
    (call-with-port
     (open-output-bytevector)
     (lambda (out)
       (decode-base64-to-port src out table)
       (get-output-bytevector out)))))

;; Loop through src, writing decoded base64 data to port in chunks
;; of up to three bytes.
(define (decode-base64-to-port src port table)
  (let ((len (string-length src)))
    (let lp ((i 0) (b1 outside-char) (b2 outside-char) (b3 outside-char))
      (if (= i len)
          (decode-base64-trailing port b1 b2 b3)
          (let* ((c (string-ref src i))
                 (b (bytevector-u8-ref table (char->integer c))))
            (cond ((pad-char? b) (decode-base64-trailing port b1 b2 b3))
                  ((char-whitespace? c) (lp (+ i 1) b1 b2 b3))
                  ((outside-char? b)
                   (bytestring-error "invalid character in base64 string"
                                     c
                                     src))
                  ((outside-char? b1) (lp (+ i 1) b b2 b3))
                  ((outside-char? b2) (lp (+ i 1) b1 b b3))
                  ((outside-char? b3) (lp (+ i 1) b1 b2 b))
                  (else
                   (write-u8 (bitwise-ior (arithmetic-shift b1 2)
                                          (bit-field b2 4 6))
                             port)
                   (write-u8 (bitwise-ior
                              (arithmetic-shift (bit-field b2 0 4) 4)
                              (bit-field b3 2 6))
                             port)
                   (write-u8 (bitwise-ior
                              (arithmetic-shift (bit-field b3 0 2) 6)
                              b)
                             port)
                   (lp (+ i 1) outside-char outside-char outside-char))))))))

;; Flush any trailing bits accumulated in the decode loop to the
;; bytevector port `out', then return the finalized bytestring.
(define (decode-base64-trailing out b1 b2 b3)
  (cond ((outside-char? b1) #t)
        ((outside-char? b2) (write-u8 (arithmetic-shift b1 2) out))
        (else
         (write-u8 (bitwise-ior (arithmetic-shift b1 2) (bit-field b2 4 6))
                   out)
         (unless (outside-char? b3)
           (write-u8 (bitwise-ior (arithmetic-shift (bit-field b2 0 4) 4)
                                  (bit-field b3 2 6))
                     out)))))

;;;; Encoding

(define (base64-encode-bytevector bv digits)
  (let* ((len (bytevector-length bv))
         (quot (quotient len 3))
         (rem (- len (* quot 3)))
         (res-len (arithmetic-shift (+ quot (if (zero? rem) 0 1)) 2))
         (res (make-bytevector res-len))
         (table (get-base64-encode-table digits)))
    (base64-encode-bytevector! bv 0 len res table)
    res))

(define (base64-encode-bytevector! bv start end res table)
  (let ((limit (- end 2))
        (enc (lambda (i) (bytevector-u8-ref table i))))
    (let lp ((i start) (j 0))
      (if (>= i limit)
          (case (- end i)
            ((1)
             (let ((b1 (bytevector-u8-ref bv i)))
               (bytevector-u8-set! res j (enc (arithmetic-shift b1 -2)))
               (bytevector-u8-set!
                res
                (+ j 1)
                (enc (arithmetic-shift (bitwise-and #b11 b1) 4)))
               (bytevector-u8-set! res (+ j 2) (char->integer #\=))
               (bytevector-u8-set! res (+ j 3) (char->integer #\=))
               (+ j 4)))
            ((2)
             (let ((b1 (bytevector-u8-ref bv i))
                   (b2 (bytevector-u8-ref bv (+ i 1))))
               (bytevector-u8-set! res j (enc (arithmetic-shift b1 -2)))
               (bytevector-u8-set!
                res
                (+ j 1)
                (enc (bitwise-ior
                      (arithmetic-shift (bitwise-and #b11 b1) 4)
                      (bit-field b2 4 8))))
               (bytevector-u8-set!
                res
                (+ j 2)
                (enc (arithmetic-shift (bit-field b2 0 4) 2)))
               (bytevector-u8-set! res (+ j 3) (char->integer #\=))
               (+ j 4)))
            (else
             j))
          (let ((b1 (bytevector-u8-ref bv i))
                (b2 (bytevector-u8-ref bv (+ i 1)))
                (b3 (bytevector-u8-ref bv (+ i 2))))
            (bytevector-u8-set! res j (enc (arithmetic-shift b1 -2)))
            (bytevector-u8-set!
             res
             (+ j 1)
             (enc (bitwise-ior
                   (arithmetic-shift (bitwise-and #b11 b1) 4)
                   (bit-field b2 4 8))))
            (bytevector-u8-set!
             res
             (+ j 2)
             (enc (bitwise-ior
                   (arithmetic-shift (bit-field b2 0 4) 2)
                   (bit-field b3 6 8))))
            (bytevector-u8-set! res (+ j 3) (enc (bitwise-and #b111111 b3)))
            (lp (+ i 3) (+ j 4)))))))
