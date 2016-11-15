;;; Guile VM assembler

;; Copyright (C) 2001, 2009, 2010, 2011, 2013 Free Software Foundation, Inc.

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

(define-module (language assembly compile-bytecode)
  #:use-module (system base pmatch)
  #:use-module (system base target)
  #:use-module (language assembly)
  #:use-module (system vm instruction)
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export (compile-bytecode))

(define (compile-bytecode assembly env . opts)
  (define-syntax-rule (define-inline1 (proc arg) body body* ...)
    (define-syntax proc
      (syntax-rules ()
        ((_ (arg-expr (... ...)))
         (let ((x (arg-expr (... ...))))
           (proc x)))
        ((_ arg)
         (begin body body* ...)))))
       
  (define (fill-bytecode bv target-endianness)
    (let ((pos 0))
      (define-inline1 (write-byte b)
        (bytevector-u8-set! bv pos b)
        (set! pos (1+ pos)))
      (define u32-bv (make-bytevector 4))
      (define-inline1 (write-int24-be x)
        (bytevector-s32-set! u32-bv 0 x (endianness big))
        (bytevector-u8-set! bv pos (bytevector-u8-ref u32-bv 1))
        (bytevector-u8-set! bv (+ pos 1) (bytevector-u8-ref u32-bv 2))
        (bytevector-u8-set! bv (+ pos 2) (bytevector-u8-ref u32-bv 3))
        (set! pos (+ pos 3)))
      (define-inline1 (write-uint32-be x)
        (bytevector-u32-set! bv pos x (endianness big))
        (set! pos (+ pos 4)))
      (define-inline1 (write-uint32 x)
        (bytevector-u32-set! bv pos x target-endianness)
        (set! pos (+ pos 4)))
      (define-inline1 (write-loader-len len)
        (bytevector-u8-set! bv pos (ash len -16))
        (bytevector-u8-set! bv (+ pos 1) (logand (ash len -8) 255))
        (bytevector-u8-set! bv (+ pos 2) (logand len 255))
        (set! pos (+ pos 3)))
      (define-inline1 (write-latin1-string s)
        (let ((len (string-length s)))
          (write-loader-len len)
          (let lp ((i 0))
            (if (< i len)
                (begin
                  (bytevector-u8-set! bv (+ pos i)
                                      (char->integer (string-ref s i)))
                  (lp (1+ i)))))
          (set! pos (+ pos len))))
      (define-inline1 (write-bytevector bv*)
        (let ((len (bytevector-length bv*)))
          (write-loader-len len)
          (bytevector-copy! bv* 0 bv pos len)
          (set! pos (+ pos len))))
      (define-inline1 (write-wide-string s)
        (write-bytevector (string->utf32 s target-endianness)))
      (define-inline1 (write-break label)
        (let ((offset (- (assq-ref labels label) (+ (get-addr) 3))))
          (cond ((>= offset (ash 1 23)) (error "jump too far forward" offset))
                ((< offset (- (ash 1 23))) (error "jump too far backwards" offset))
                (else (write-int24-be offset)))))

      (define (write-bytecode asm labels address emit-opcode?)
        ;; Write ASM's bytecode to BV.  If EMIT-OPCODE? is false, don't
        ;; emit bytecode for the first opcode encountered.  Assume code
        ;; starts at ADDRESS (an integer).  LABELS is assumed to be an
        ;; alist mapping labels to addresses.
        (define get-addr
          (let ((start pos))
            (lambda ()
              (+ address (- pos start)))))
        (define (write-break label)
          (let ((offset (- (assq-ref labels label) (+ (get-addr) 3))))
            (cond ((>= offset (ash 1 23)) (error "jump too far forward" offset))
                  ((< offset (- (ash 1 23))) (error "jump too far backwards" offset))
                  (else (write-int24-be offset)))))
  
        (let ((inst (car asm))
              (args (cdr asm)))
          (let ((opcode (instruction->opcode inst))
                (len (instruction-length inst)))
            (if emit-opcode?
                (write-byte opcode))
            (pmatch asm
              ((load-program ,labels ,length ,meta . ,code)
               (write-uint32 length)
               (write-uint32 (if meta (1- (byte-length meta)) 0))
               (fold (lambda (asm address)
                       (let ((start pos))
                         (write-bytecode asm labels address #t)
                         (+ address (- pos start))))
                     0
                     code)
               (if meta
                   ;; Don't emit the `load-program' byte for metadata.  Note that
                   ;; META's bytecode meets the alignment requirements of
                   ;; `scm_objcode', thanks to the alignment computed in `(language
                   ;; assembly)'.
                   (write-bytecode meta '() 0 #f)))
              ((make-char32 ,x) (write-uint32-be x))
              ((load-number ,str) (write-latin1-string str))
              ((load-string ,str) (write-latin1-string str))
              ((load-wide-string ,str) (write-wide-string str))
              ((load-symbol ,str) (write-latin1-string str))
              ((load-array ,bv) (write-bytevector bv))
              ((br ,l) (write-break l))
              ((br-if ,l) (write-break l))
              ((br-if-not ,l) (write-break l))
              ((br-if-eq ,l) (write-break l))
              ((br-if-not-eq ,l) (write-break l))
              ((br-if-null ,l) (write-break l))
              ((br-if-not-null ,l) (write-break l))
              ((br-if-nargs-ne ,hi ,lo ,l) (write-byte hi) (write-byte lo) (write-break l))
              ((br-if-nargs-lt ,hi ,lo ,l) (write-byte hi) (write-byte lo) (write-break l))
              ((br-if-nargs-gt ,hi ,lo ,l) (write-byte hi) (write-byte lo) (write-break l))
              ((bind-optionals/shuffle-or-br ,nreq-hi ,nreq-lo
                                             ,nreq-and-nopt-hi ,nreq-and-nopt-lo
                                             ,ntotal-hi ,ntotal-lo
                                             ,l)
               (write-byte nreq-hi)
               (write-byte nreq-lo)
               (write-byte nreq-and-nopt-hi)
               (write-byte nreq-and-nopt-lo)
               (write-byte ntotal-hi)
               (write-byte ntotal-lo)
               (write-break l))
              ((mv-call ,n ,l) (write-byte n) (write-break l))
              ((prompt ,escape-only? ,l) (write-byte escape-only?) (write-break l))
              (else
               (cond
                ((< len 0)
                 (error "unhanded variable-length instruction" asm))
                ((not (= (length args) len))
                 (error "bad number of args to instruction" asm len))
                (else
                 (for-each (lambda (x) (write-byte x)) args))))))))

      ;; Don't emit the `load-program' byte.
      (write-bytecode assembly '() 0 #f)
      (if (= pos (bytevector-length bv))
          (values bv env env)
          (error "failed to fill bytevector" bv pos
                 (bytevector-length bv)))))

  (pmatch assembly
    ((load-program ,labels ,length ,meta . ,code)
     (fill-bytecode (make-bytevector (+ 4 4 length
                                        (if meta
                                            (1- (byte-length meta))
                                            0)))
                    (target-endianness)))
    
    (else (error "bad assembly" assembly))))
