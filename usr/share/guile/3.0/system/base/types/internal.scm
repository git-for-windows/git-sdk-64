;;; Details on internal value representation.
;;; Copyright (C) 2014, 2015, 2017, 2018, 2020, 2021 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (system base types internal)
  #:export (;; Immediate tags.
            %tc2-fixnum
            %tc3-heap-object
            %tc8-char
            %tc16-false
            %tc16-nil
            %tc16-null
            %tc16-true
            %tc16-unspecified
            %tc16-undefined
            %tc16-eof
            visit-immediate-tags

            ;; Heap object tags (cell types).
            %tc1-pair
            %tc3-struct
            %tc7-symbol
            %tc7-variable
            %tc7-vector
            %tc8-immutable-vector
            %tc8-mutable-vector
            %tc7-weak-vector
            %tc7-string
            %tc7-heap-number
            %tc7-hash-table
            %tc7-pointer
            %tc7-fluid
            %tc7-stringbuf
            %tc7-dynamic-state
            %tc7-frame
            %tc7-keyword
            %tc7-atomic-box
            %tc7-syntax
            %tc7-program
            %tc7-vm-continuation
            %tc7-bytevector
            %tc7-weak-set
            %tc7-weak-table
            %tc7-array
            %tc7-bitvector
            %tc7-port
            %tc7-smob
            %tc16-bignum
            %tc16-flonum
            %tc16-complex
            %tc16-fraction
            visit-heap-tags

            scm->immediate-bits
            immediate-bits->scm
            truncate-bits
            sign-extend))

;;; Commentary:
;;;
;;; Tag values used to represent Scheme values, internally to Guile.
;;;
;;; Code:


;;;
;;; Tags---keep in sync with libguile/tags.h!
;;;

(define-syntax define-tags
  (lambda (x)
    (define (id-append ctx a b)
      (datum->syntax ctx (symbol-append (syntax->datum a) (syntax->datum b))))
    (syntax-case x ()
      ((_ tag-set (name pred mask tag) ...)
       #`(define-syntax #,(id-append #'tag-set #'visit- #'tag-set)
           (lambda (x)
             (define (introduce ctx id)
               (datum->syntax ctx (syntax->datum id)))
             (syntax-case x ()
               ((_ f)
                #`(begin
                    (f #,(introduce #'f #'name)
                       #,(introduce #'f #'pred)
                       mask
                       tag)
                    ...)))))))))

(define-tags immediate-tags
  ;;                                    321076543210    321076543210
  (fixnum           fixnum?                     #b11            #b10)
  (heap-object      heap-object?               #b111           #b000)
  (char             char?                 #b11111111      #b00001100)
  (undefined        undefined?        #b111111111111  #b100100000100)

  ;; To check for these values from Scheme, use eq?.  From assembler,
  ;; use eq-immediate?.
  (false            #f                #b111111111111  #b000000000100)
  (nil              #f                #b111111111111  #b000100000100)
  (null             #f                #b111111111111  #b001100000100)
  (true             #f                #b111111111111  #b010000000100)
  (unspecified      #f                #b111111111111  #b100000000100)
  (eof              #f                #b111111111111  #b101000000100)

  ;;(nil            eq-nil?           #b111111111111  #b000100000100)
  ;;(eol            eq-null?          #b111111111111  #b001100000100)
  ;;(false          eq-false?         #b111111111111  #b000000000100)
  (null+nil         null?             #b110111111111  #b000100000100)
  (false+nil        false?            #b111011111111  #b000000000100)
  (null+false+nil   nil?              #b110011111111  #b000000000100))

(define-tags heap-tags
  ;;                                    321076543210    321076543210
  (pair             pair?                        #b1             #b0)
  (struct           struct?                    #b111           #b001)
  ;; For tc7 values, low bits 2 and 0 must be 1.
  (symbol           symbol?                #b1111111       #b0000101)
  (variable         variable?              #b1111111       #b0000111)
  (vector           vector?                #b1111111       #b0001101)
  (immutable-vector immutable-vector?     #b11111111      #b10001101)
  (mutable-vector   mutable-vector?       #b11111111      #b00001101)
  (weak-vector      weak-vector?           #b1111111       #b0001111)
  (string           string?                #b1111111       #b0010101)
  (heap-number      heap-number?           #b1111111       #b0010111)
  (hash-table       hash-table?            #b1111111       #b0011101)
  (pointer          pointer?               #b1111111       #b0011111)
  (fluid            fluid?                 #b1111111       #b0100101)
  (stringbuf        stringbuf?             #b1111111       #b0100111)
  (dynamic-state    dynamic-state?         #b1111111       #b0101101)
  (frame            frame?                 #b1111111       #b0101111)
  (keyword          keyword?               #b1111111       #b0110101)
  (atomic-box       atomic-box?            #b1111111       #b0110111)
  (syntax           syntax?                #b1111111       #b0111101)
  ;;(unused         unused                 #b1111111       #b0111111)
  (program          program?               #b1111111       #b1000101)
  (vm-continuation  vm-continuation?       #b1111111       #b1000111)
  (bytevector       bytevector?            #b1111111       #b1001101)
  ;;(unused         unused                 #b1111111       #b1001111)
  (weak-set         weak-set?              #b1111111       #b1010101)
  (weak-table       weak-table?            #b1111111       #b1010111)
  (array            array?                 #b1111111       #b1011101)
  (bitvector        bitvector?             #b1111111       #b1011111)
  ;;(unused         unused                 #b1111111       #b1100101)
  ;;(unused         unused                 #b1111111       #b1100111)
  ;;(unused         unused                 #b1111111       #b1101101)
  ;;(unused         unused                 #b1111111       #b1101111)
  ;;(unused         unused                 #b1111111       #b1110101)
  (smob             smob?                  #b1111111       #b1110111)
  (port             port?                  #b1111111       #b1111101)
  ;;(unused         unused                 #b1111111       #b1111111)
  
  ;(heap-number     heap-number?           #b1111111       #b0010111)
  (bignum           bignum?           #b111111111111  #b000100010111)
  (flonum           flonum?           #b111111111111  #b001000010111)
  (complex          compnum?          #b111111111111  #b001100010111)
  (fraction         fracnum?          #b111111111111  #b010000010111))

(define-syntax define-tag
  (lambda (x)
    (define (id-append ctx a b)
      (datum->syntax ctx (symbol-append (syntax->datum a) (syntax->datum b))))
    (define (def prefix name tag)
      #`(define #,(id-append name prefix name) #,tag))
    (syntax-case x ()
      ((_ name pred #b1 tag)             (def #'%tc1- #'name #'tag))
      ((_ name pred #b11 tag)            (def #'%tc2- #'name #'tag))
      ((_ name pred #b111 tag)           (def #'%tc3- #'name #'tag))
      ((_ name pred #b1111111 tag)       (def #'%tc7- #'name #'tag))
      ((_ name pred #b11111111 tag)      (def #'%tc8- #'name #'tag))
      ;; Only 12 bits of mask but for historic reasons these are called
      ;; tc16 values.
      ((_ name pred #b111111111111 tag)  (def #'%tc16- #'name #'tag))
      ((_ name pred mask tag)
       #`(begin
           (define #,(id-append #'name #'name #'-mask) mask)
           (define #,(id-append #'name #'name #'-tag) tag))))))

(visit-immediate-tags define-tag)
(visit-heap-tags define-tag)

(define (scm->immediate-bits x)
  "If @var{x} is of a type that could be encoded as an immediate, return
that bit pattern, or @code{#f} otherwise..  Note that the immediate bits
may not fit into a word on the target platform."
  (cond
   ((exact-integer? x) (logior %tc2-fixnum (ash x 2)))
   ((char? x)          (logior %tc8-char (ash (char->integer x) 8)))
   ((eq? x #f)         %tc16-false)
   ((eq? x #nil)       %tc16-nil)
   ((eq? x '())        %tc16-null)
   ((eq? x #t)         %tc16-true)
   ((unspecified? x)   %tc16-unspecified)
   ;; FIXME: %tc16-undefined.
   ((eof-object? x)    %tc16-eof)
   (else #f)))

(define (immediate-bits->scm imm)
  "Return the SCM object corresponding to the immediate encoding
@code{imm}.  Note that this value should be sign-extended already."
  (define-syntax-rule (define-predicate name pred mask tag)
    (define (name) (eqv? (logand imm mask) tag)))
  (visit-immediate-tags define-predicate)
  (cond
   ((fixnum)      (ash imm -2))
   ((char)        (integer->char (ash imm -8)))
   ((false)       #f)
   ((nil)         #nil)
   ((null)        '())
   ((true)        #t)
   ((unspecified) (if #f #f))
   ((eof)         the-eof-object)
   (else (error "invalid immediate" imm))) )

(define (sign-extend x bits)
  (case (ash x (- 1 bits))
    ((0) x)
    ((1) (- x (ash 1 bits)))
    (else (error "value does not fit in bits" x bits))))

(define (truncate-bits x bits signed?)
  (define-syntax-rule (bits-case bits)
    (let ((umax (1- (ash 1 bits))))
      (and (if signed?
               (let ((smin (ash -1 (1- bits)))
                     (smax (1- (ash 1 (1- bits)))))
                 (<= smin x smax))
               (<= 0 x umax))
           (logand x umax))))
  (case bits
    ((16) (bits-case 16))
    ((32) (bits-case 32))
    ((64) (bits-case 64))
    (else (bits-case bits))))

;; See discussion in tags.h and boolean.h.
(eval-when (expand)
  (let ()
    (visit-immediate-tags define-tag)
    (define (exactly-one-bit-set? x)
      (and (not (zero? x)) (zero? (logand x (1- x)))))
    (define (exactly-two-bits-set? x)
      (exactly-one-bit-set? (logand x (1- x))))
    (define (bits-differ-in-exactly-one-bit-position? a b)
      (exactly-one-bit-set? (logxor a b)))
    (define (bits-differ-in-exactly-two-bit-positions? a b)
      (exactly-two-bits-set? (logxor a b)))
    (define (common-bits a b)
      (values (logand #xfff (lognot (logxor a b))) (logand a b)))

    (unless (bits-differ-in-exactly-one-bit-position? %tc16-null %tc16-nil)
      (error "expected #nil and '() to differ in exactly one bit position"))
    (unless (bits-differ-in-exactly-one-bit-position? %tc16-false %tc16-nil)
      (error "expected #f and '() to differ in exactly one bit position"))
    (unless (bits-differ-in-exactly-two-bit-positions? %tc16-false %tc16-null)
      (error "expected #f and '() to differ in exactly two bit positions"))
    (call-with-values (lambda () (common-bits %tc16-null %tc16-nil))
      (lambda (mask tag)
        (unless (= mask null+nil-mask) (error "unexpected mask for null?"))
        (unless (= tag null+nil-tag) (error "unexpected tag for null?"))))
    (call-with-values (lambda () (common-bits %tc16-false %tc16-nil))
      (lambda (mask tag)
        (unless (= mask false+nil-mask) (error "unexpected mask for false?"))
        (unless (= tag false+nil-tag) (error "unexpected tag for false?"))))
    (call-with-values (lambda () (common-bits %tc16-false %tc16-null))
      (lambda (mask tag)
        (unless (= mask null+false+nil-mask) (error "unexpected mask for nil?"))
        (unless (= tag null+false+nil-tag) (error "unexpected tag for nil?"))))))
