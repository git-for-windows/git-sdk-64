;;; Guile Virtual Machine Assembly

;; Copyright (C) 2001, 2009, 2010, 2011 Free Software Foundation, Inc.

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

(define-module (language assembly)
  #:use-module (rnrs bytevectors)
  #:use-module (system base pmatch)
  #:use-module (system vm instruction)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export (byte-length
            addr+ align-program align-code align-block
            assembly-pack assembly-unpack
            object->assembly assembly->object))

;; len, metalen
(define *program-header-len* (+ 4 4))

;; lengths are encoded in 3 bytes
(define *len-len* 3)


(define (byte-length assembly)
  (pmatch assembly
    ((,inst . _) (guard (>= (instruction-length inst) 0))
     (+ 1 (instruction-length inst)))
    ((load-number ,str)
     (+ 1 *len-len* (string-length str)))
    ((load-string ,str)
     (+ 1 *len-len* (string-length str)))
    ((load-wide-string ,str)
     (+ 1 *len-len* (* 4 (string-length str))))
    ((load-symbol ,str)
     (+ 1 *len-len* (string-length str)))
    ((load-array ,bv)
     (+ 1 *len-len* (bytevector-length bv)))
    ((load-program ,labels ,len ,meta . ,code)
     (+ 1 *program-header-len* len (if meta (1- (byte-length meta)) 0)))
    (,label (guard (not (pair? label)))
     0)
    (else (error "unknown instruction" assembly))))


(define *program-alignment* 8)

(define (addr+ addr code)
  (fold (lambda (x len) (+ (byte-length x) len))
        addr
        code))

(define (code-alignment addr alignment header-len)
  (make-list (modulo (- alignment
                        (modulo (+ addr header-len) alignment))
                     alignment)
             '(nop)))

(define (align-block addr)
  '())

(define (align-code code addr alignment header-len)
  `(,@(code-alignment addr alignment header-len)
    ,code))

(define (align-program prog addr)
  (align-code prog addr *program-alignment* 1))

;;;
;;; Code compress/decompression
;;;

(define *abbreviations*
  '(((make-int8 0) . (make-int8:0))
    ((make-int8 1) . (make-int8:1))))
  
(define *expansions*
  (map (lambda (x) (cons (cdr x) (car x))) *abbreviations*))

(define (assembly-pack code)
  (or (assoc-ref *abbreviations* code)
      code))

(define (assembly-unpack code)
  (or (assoc-ref *expansions* code)
      code))


;;;
;;; Encoder/decoder
;;;

(define (object->assembly x)
  (cond ((eq? x #t) `(make-true))
	((eq? x #f) `(make-false))
        ((eq? x #nil) `(make-nil))
	((null? x) `(make-eol))
	((and (integer? x) (exact? x))
	 (cond ((and (<= -128 x) (< x 128))
		(assembly-pack `(make-int8 ,(modulo x 256))))
	       ((and (<= -32768 x) (< x 32768))
		(let ((n (if (< x 0) (+ x 65536) x)))
		  `(make-int16 ,(quotient n 256) ,(modulo n 256))))
               ((and (<= 0 x #xffffffffffffffff))
                `(make-uint64 ,@(bytevector->u8-list
                                 (let ((bv (make-bytevector 8)))
                                   (bytevector-u64-set! bv 0 x (endianness big))
                                   bv))))
	       ((and (<= 0 (+ x #x8000000000000000) #x7fffffffffffffff))
                `(make-int64 ,@(bytevector->u8-list
                                (let ((bv (make-bytevector 8)))
                                  (bytevector-s64-set! bv 0 x (endianness big))
                                  bv))))
	       (else #f)))
	((char? x)
         (cond ((<= (char->integer x) #xff)
                `(make-char8 ,(char->integer x)))
               (else
                `(make-char32 ,(char->integer x)))))
	(else #f)))

(define (assembly->object code)
  (pmatch code
    ((make-true) #t)
    ((make-false) #f) ;; FIXME: Same as the `else' case!
    ((make-nil) #nil)
    ((make-eol) '())
    ((make-int8 ,n)
     (if (< n 128) n (- n 256)))
    ((make-int16 ,n1 ,n2)
     (let ((n (+ (* n1 256) n2)))
       (if (< n 32768) n (- n 65536))))
    ((make-uint64 ,n1 ,n2 ,n3 ,n4 ,n5 ,n6 ,n7 ,n8)
     (bytevector-u64-ref
      (u8-list->bytevector (list n1 n2 n3 n4 n5 n6 n7 n8))
      0
      (endianness big)))
    ((make-int64 ,n1 ,n2 ,n3 ,n4 ,n5 ,n6 ,n7 ,n8)
     (bytevector-s64-ref
      (u8-list->bytevector (list n1 n2 n3 n4 n5 n6 n7 n8))
      0
      (endianness big)))
    ((make-char8 ,n)
     (integer->char n))
    ((make-char32 ,n1 ,n2 ,n3 ,n4)
     (integer->char (+ (* n1 #x1000000)
                       (* n2 #x10000)
                       (* n3 #x100)
                       n4)))
    ((load-string ,s) s)
    ((load-symbol ,s) (string->symbol s))
    (else #f)))
