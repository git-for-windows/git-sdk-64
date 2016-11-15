;;; Guile VM code converters

;; Copyright (C) 2001, 2009, 2010, 2013 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language assembly decompile-bytecode)
  #:use-module (system vm instruction)
  #:use-module (system base pmatch)
  #:use-module (srfi srfi-4)
  #:use-module (rnrs bytevectors)
  #:use-module (language assembly)
  #:use-module ((system vm objcode) #:select (byte-order))
  #:export (decompile-bytecode))

(define (decompile-bytecode x env opts)
  (let ((i 0) (size (u8vector-length x)))
    (define (pop)
      (let ((b (cond ((< i size) (u8vector-ref x i))
                     ((= i size) #f)
                     (else (error "tried to decode too many bytes")))))
        (if b (set! i (1+ i)))
        b))
    (let ((ret (decode-load-program pop)))
      (if (= i size)
          (values ret env)
          (error "bad bytecode: only decoded ~a out of ~a bytes" i size)))))

(define (br-instruction? x)
  (memq x '(br br-if br-if-not br-if-eq br-if-not-eq br-if-null br-if-not-null)))
(define (br-nargs-instruction? x)
  (memq x '(br-if-nargs-ne br-if-nargs-lt br-if-nargs-gt br-if-nargs-lt/non-kw)))

(define (bytes->s24 a b c)
  (let ((x (+ (ash a 16) (ash b 8) c)))
    (if (zero? (logand (ash 1 23) x))
        x
        (- x (ash 1 24)))))

;; FIXME: this is a little-endian disassembly!!!
(define (decode-load-program pop)
  (let* ((a (pop)) (b (pop)) (c (pop)) (d (pop))
         (e (pop)) (f (pop)) (g (pop)) (h (pop))
         (len (+ a (ash b 8) (ash c 16) (ash d 24)))
         (metalen (+ e (ash f 8) (ash g 16) (ash h 24)))
         (labels '())
         (i 0))
    (define (ensure-label rel1 rel2 rel3)
      (let ((where (+ i (bytes->s24 rel1 rel2 rel3))))
        (or (assv-ref labels where)
            (begin
              (let ((l (gensym ":L")))
                (set! labels (acons where l labels))
                l)))))
    (define (sub-pop) ;; ...records. ha. ha.
      (let ((b (cond ((< i len) (pop))
                     ((= i len) #f)
                     (else (error "tried to decode too many bytes")))))
        (if b (set! i (1+ i)))
        b))
    (let lp ((out '()))
      (cond ((> i len)
             (error "error decoding program -- read too many bytes" out))
            ((= i len)
             `(load-program ,(map (lambda (x) (cons (cdr x) (car x)))
                                  (reverse labels))
                            ,len
                            ,(if (zero? metalen) #f (decode-load-program pop))
                            ,@(reverse! out)))
            (else
             (let ((exp (decode-bytecode sub-pop)))
               (pmatch exp
                 ((,br ,rel1 ,rel2 ,rel3) (guard (br-instruction? br))
                  (lp (cons `(,br ,(ensure-label rel1 rel2 rel3)) out)))
                 ((,br ,hi ,lo ,rel1 ,rel2 ,rel3) (guard (br-nargs-instruction? br))
                  (lp (cons `(,br ,hi ,lo ,(ensure-label rel1 rel2 rel3)) out)))
                 ((bind-optionals/shuffle-or-br ,nreq-hi ,nreq-lo
                                                ,nreq-and-nopt-hi ,nreq-and-nopt-lo
                                                ,ntotal-hi ,ntotal-lo
                                                ,rel1 ,rel2 ,rel3)
                  (lp (cons `(bind-optionals/shuffle-or-br
                              ,nreq-hi ,nreq-lo
                              ,nreq-and-nopt-hi ,nreq-and-nopt-lo
                              ,ntotal-hi ,ntotal-lo
                              ,(ensure-label rel1 rel2 rel3))
                            out)))
                 ((mv-call ,n ,rel1 ,rel2 ,rel3)
                  (lp (cons `(mv-call ,n ,(ensure-label rel1 rel2 rel3)) out)))
                 ((prompt ,n0 ,rel1 ,rel2 ,rel3)
                  (lp (cons `(prompt ,n0 ,(ensure-label rel1 rel2 rel3)) out)))
                 (else 
                  (lp (cons exp out))))))))))

(define (decode-bytecode pop)
  (and=> (pop)
         (lambda (opcode)
           (let ((inst (opcode->instruction opcode)))
             (cond
              ((eq? inst 'load-program)
               (decode-load-program pop))

              ((< (instruction-length inst) 0)
               ;; the negative length indicates a variable length
               ;; instruction
               (let* ((make-sequence
                       (if (or (memq inst '(load-array load-wide-string)))
                           make-bytevector
                           make-string))
                      (sequence-set!
                       (if (or (memq inst '(load-array load-wide-string)))
                           bytevector-u8-set!
                           (lambda (str pos value)
                             (string-set! str pos (integer->char value)))))
                      (len (let* ((a (pop)) (b (pop)) (c (pop)))
                             (+ (ash a 16) (ash b 8) c)))
                      (seq (make-sequence len)))
                 (let lp ((i 0))
                   (if (= i len)
                       `(,inst ,(if (eq? inst 'load-wide-string)
                                    (utf32->string seq (native-endianness))
                                    seq))
                       (begin
                         (sequence-set! seq i (pop))
                         (lp (1+ i)))))))
              (else
               ;; fixed length
               (let lp ((n (instruction-length inst)) (out (list inst)))
                 (if (zero? n)
                     (reverse! out)
                     (lp (1- n) (cons (pop) out))))))))))
