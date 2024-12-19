;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2023 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; When targetting Guile's virtual machine, we can get maximum
;;; performance by expanding out some compound primcalls, both so that
;;; they are available to common subexpression elimination and so that
;;; their lowered forms can be implemented using Guile's low-level VM
;;; capabilities instead of by call-outs to library routines.
;;;
;;; Code:

(define-module (language cps guile-vm lower-primcalls)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (system base target)
  #:use-module (system base types internal)
  #:export (lower-primcalls))

(define *primcall-lowerers* (make-hash-table))
(define-syntax-rule (define-primcall-lowerer* name proc)
  (hashq-set! *primcall-lowerers* 'name proc))
(define-syntax-rule (define-primcall-lowerer (name cps k src param-pat args-pat)
                      body ...)
  (define-primcall-lowerer* name
    (lambda (cps k src param args)
      (match (cons param args)
        ((param-pat . args-pat)
         body ...)))))

(define *branching-primcall-lowerers* (make-hash-table))
(define-syntax-rule (define-branching-primcall-lowerer* name proc)
  (hashq-set! *branching-primcall-lowerers* 'name proc))
(define-syntax-rule (define-branching-primcall-lowerer
                      (name cps kf kt src param-pat args-pat)
                      body ...)
  (define-branching-primcall-lowerer* name
    (lambda (cps kf kt src param args)
      (match (cons param args)
        ((param-pat . args-pat)
         body ...)))))
(define-syntax-rule (define-branching-primcall-alias def use ...)
  (let ((proc (or (hashq-ref *branching-primcall-lowerers* 'def)
                  (error "def not found" 'def))))
    (hashq-set! *branching-primcall-lowerers* 'use proc)
    ...))

;; precondition: v is vector.  result is u64
(define-primcall-lowerer (vector-length cps k src #f (v))
  (with-cps cps
    (letv w0 ulen)
    (letk kassume
          ($kargs ('ulen) (ulen)
            ($continue k src
              ($primcall 'assume-u64 `(0 . ,(target-max-vector-length)) (ulen)))))
    (letk krsh
          ($kargs ('w0) (w0)
            ($continue kassume src ($primcall 'ursh/immediate 8 (w0)))))
    (build-term
      ($continue krsh src
        ($primcall 'word-ref/immediate '(vector . 0) (v))))))

;; precondition: v is vector, uidx is u64 in range
(define-primcall-lowerer (vector-ref cps k src #f (v uidx))
  (with-cps cps
    (letv upos)
    (letk kref ($kargs ('pos) (upos)
                 ($continue k src
                   ($primcall 'scm-ref 'vector (v upos)))))
    (build-term
      ($continue kref src
        ($primcall 'uadd/immediate 1 (uidx))))))

;; precondition: v is vector, idx is in range
(define-primcall-lowerer (vector-ref/immediate cps k src idx (v))
  (let ((pos (1+ idx)))
    (with-cps cps
      (build-term
        ($continue k src
          ($primcall 'scm-ref/immediate `(vector . ,pos) (v)))))))

;; precondition: v is vector, uidx is u64 and is in range
(define-primcall-lowerer (vector-set! cps k src #f (v uidx val))
  (with-cps cps
    (letv upos)
    (letk kset ($kargs ('pos) (upos)
                 ($continue k src
                   ($primcall 'scm-set! 'vector (v upos val)))))
    (build-term
      ($continue kset src
        ($primcall 'uadd/immediate 1 (uidx))))))

;; precondition: v is vector, idx is in range
(define-primcall-lowerer (vector-set!/immediate cps k src idx (v val))
  (let ((pos (1+ idx)))
    (with-cps cps
      (build-term
        ($continue k src
          ($primcall 'scm-set!/immediate `(vector . ,pos) (v val)))))))

(define-primcall-lowerer (allocate-vector/immediate cps k src size ())
  (define nwords (1+ size))
  (unless (and (exact-integer? size) (<= 0 size (target-max-vector-length)))
    (error "precondition failed" size))
  (with-cps cps
    (letv v w0)
    (letk kdone
          ($kargs () ()
            ($continue k src ($values (v)))))
    (letk ktag1
          ($kargs ('w0) (w0)
            ($continue kdone src
              ($primcall 'word-set!/immediate '(vector . 0) (v w0)))))
    (letk ktag0
          ($kargs ('v) (v)
            ($continue ktag1 src
              ($primcall 'load-u64 (+ %tc7-vector (ash size 8)) ()))))
    (build-term
      ($continue ktag0 src
        ($primcall 'allocate-words/immediate `(vector . ,nwords) ())))))

;; precondition: usize is u64 within range
(define-primcall-lowerer (allocate-vector cps k src #f (usize))
  (with-cps cps
    (letv nwords v w0-high w0)
    (letk kdone
          ($kargs () ()
            ($continue k src ($values (v)))))
    (letk ktag2
          ($kargs ('w0) (w0)
            ($continue kdone src
              ($primcall 'word-set!/immediate '(vector . 0) (v w0)))))
    (letk ktag1
          ($kargs ('w0-high) (w0-high)
            ($continue ktag2 src
              ($primcall 'uadd/immediate %tc7-vector (w0-high)))))
    (letk ktag0
          ($kargs ('v) (v)
            ($continue ktag1 src
              ($primcall 'ulsh/immediate 8 (usize)))))
    (letk kalloc
          ($kargs ('nwords) (nwords)
            ($continue ktag0 src
              ($primcall 'allocate-words 'vector (nwords)))))
    (build-term
      ($continue kalloc src
        ;; Header word.
        ($primcall 'uadd/immediate 1 (usize))))))

;; precondition: none
(define-primcall-lowerer (cons cps k src #f (head tail))
  (with-cps cps
    (letv pair)
    (letk kdone
          ($kargs () ()
            ($continue k src ($values (pair)))))
    (letk ktail
          ($kargs () ()
            ($continue kdone src
              ($primcall 'scm-set!/immediate '(pair . 1) (pair tail)))))
    (letk khead
          ($kargs ('pair) (pair)
            ($continue ktail src
              ($primcall 'scm-set!/immediate '(pair . 0) (pair head)))))
    (build-term
      ($continue khead src
        ($primcall 'allocate-words/immediate '(pair . 2) ())))))

;; precondition: pair is pair
(define-primcall-lowerer (car cps k src #f (pair))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'scm-ref/immediate '(pair . 0) (pair))))))

;; precondition: pair is pair
(define-primcall-lowerer (cdr cps k src #f (pair))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'scm-ref/immediate '(pair . 1) (pair))))))

;; precondition: pair is mutable pair
(define-primcall-lowerer (set-car! cps k src #f (pair val))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'scm-set!/immediate '(pair . 0) (pair val))))))

;; precondition: pair is mutable pair
(define-primcall-lowerer (set-cdr! cps k src #f (pair val))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'scm-set!/immediate '(pair . 1) (pair val))))))

;; precondition: none
(define-primcall-lowerer (box cps k src #f (val))
  (with-cps cps
    (letv obj tag)
    (letk kdone
          ($kargs () ()
            ($continue k src ($values (obj)))))
    (letk kval
          ($kargs () ()
            ($continue kdone src
              ($primcall 'scm-set!/immediate '(box . 1) (obj val)))))
    (letk ktag1
          ($kargs ('tag) (tag)
            ($continue kval src
              ($primcall 'word-set!/immediate '(box . 0) (obj tag)))))
    (letk ktag0
          ($kargs ('obj) (obj)
            ($continue ktag1 src
              ($primcall 'load-u64 %tc7-variable ()))))
    (build-term
      ($continue ktag0 src
        ($primcall 'allocate-words/immediate '(box . 2) ())))))

;; precondition: box is box.  note: no checking for unbound!
(define-primcall-lowerer (box-ref cps k src #f (box))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'scm-ref/immediate '(box . 1) (box))))))

;; precondition: box is box
(define-primcall-lowerer (box-set! cps k src #f (box val))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'scm-set!/immediate '(box . 1) (box val))))))

;; precondition: struct is a struct.
(define-primcall-lowerer (struct-vtable cps k src #f (struct))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'scm-ref/tag 'struct (struct))))))

;; precondition: vtable is a vtable.  result is u64
(define-primcall-lowerer (vtable-size cps k src #f (vtable))
  (define vtable-index-size 5) ; FIXME: pull from struct.h
  (define vtable-offset-size (1+ vtable-index-size))
  (with-cps cps
    (letv rfields)
    (letk kassume
          ($kargs ('rfields) (rfields)
            ($continue k src
              ($primcall 'assume-u64 `(0 . ,(target-max-size-t/scm))
                         (rfields)))))
    (build-term
      ($continue kassume src
        ($primcall 'word-ref/immediate
                   `(struct . ,vtable-offset-size) (vtable))))))

;; precondition: vtable is a vtable.
(define-branching-primcall-lowerer (vtable-vtable? cps kf kt src #f (vtable))
  (define vtable-index-flags 1)         ; FIXME: pull from struct.h
  (define vtable-offset-flags (1+ vtable-index-flags))
  (define vtable-validated-mask #b11)
  (define vtable-validated-value #b11)
  (with-cps cps
    (letv flags res)
    (letk ktest
          ($kargs ('res) (res)
            ($branch kf kt src
              'u64-imm-= vtable-validated-value (res))))
    (letk kflags
          ($kargs ('flags) (flags)
            ($continue ktest src
              ($primcall 'ulogand/immediate vtable-validated-mask (flags)))))
    (build-term
      ($continue kflags src
        ($primcall 'word-ref/immediate
                   `(struct . ,vtable-offset-flags) (vtable))))))

;; precondition: vtable is a vtable.
(define-branching-primcall-lowerer (vtable-has-unboxed-fields? cps kf kt src
                                                               nfields (vtable))
  (define vtable-index-unboxed-fields 6) ; FIXME: pull from struct.h
  (define vtable-offset-unboxed-fields (1+ vtable-index-unboxed-fields))
  (define (check-any-unboxed cps ptr word)
    (if (< (* word 32) nfields)
        (with-cps cps
          (letv idx bits)
          (let$ checkboxed (check-any-unboxed ptr (1+ word)))
          (letk kcheckboxed ($kargs () () ,checkboxed))
          (letk kcheck
                ($kargs ('bits) (bits)
                  ($branch kt kcheckboxed src 'u64-imm-= 0 (bits))))
          (letk kword
                ($kargs ('idx) (idx)
                  ($continue kcheck src
                    ($primcall 'u32-ref 'bitmask (vtable ptr idx)))))
          (build-term
            ($continue kword src
              ($primcall 'load-u64 word ()))))
        (with-cps cps
          (build-term ($continue kf src ($values ()))))))
  (with-cps cps
    (letv ptr)
    (let$ checkboxed (check-any-unboxed ptr 0))
    (letk kcheckboxed ($kargs ('ptr) (ptr) ,checkboxed))
    (build-term
      ($continue kcheckboxed src
        ($primcall 'pointer-ref/immediate
                   `(struct . ,vtable-offset-unboxed-fields)
                   (vtable))))))

;; precondition: vtable is a vtable, no unboxed fields, nfields matches
;; vtable size.
(define-primcall-lowerer (allocate-struct cps k src nfields (vtable))
  (define nwords (1+ nfields))
  (with-cps cps
    (letv s)
    (letk kdone
          ($kargs () () ($continue k src ($values (s)))))
    (letk ktag
          ($kargs ('s) (s)
            ($continue kdone src
              ($primcall 'scm-set!/tag 'struct (s vtable)))))
    (build-term
      ($continue ktag src
        ($primcall 'allocate-words/immediate `(struct . ,nwords) ())))))

;; precondition: vtable is vtable, idx less than vtable size
(define-branching-primcall-lowerer (vtable-field-boxed? cps kf kt src idx (vtable))
  (define vtable-index-unboxed-fields 6) ; FIXME: pull from struct.h
  (define vtable-offset-unboxed-fields (1+ vtable-index-unboxed-fields))
  (with-cps cps
    (letv ptr word bits res)
    (letk ktest
          ($kargs ('res) (res)
            ($branch kf kt src 'u64-imm-= 0 (res))))
    (letk kbits
          ($kargs ('bits) (bits)
            ($continue ktest src
              ($primcall 'ulogand/immediate (ash 1 (logand idx 31)) (bits)))))
    (letk kword
          ($kargs ('word) (word)
            ($continue kbits src
              ($primcall 'u32-ref 'bitmask (vtable ptr word)))))
    (letk kptr
          ($kargs ('ptr) (ptr)
            ($continue kword src
              ($primcall 'load-u64 (ash idx -5) ()))))
    (build-term
      ($continue kptr src
        ($primcall 'pointer-ref/immediate
                   `(struct . ,vtable-offset-unboxed-fields)
                   (vtable))))))

;; precondition: struct a struct, idx in range, field unboxed.
(define-primcall-lowerer (struct-ref cps k src idx (struct))
  (define pos (1+ idx)) ; get past vtable
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'scm-ref/immediate `(struct . ,pos) (struct))))))

;; precondition: struct a struct, idx in range, field unboxed.
(define-primcall-lowerer (struct-set! cps k src idx (struct val))
  (define pos (1+ idx)) ; get past vtable
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'scm-set!/immediate `(struct . ,pos) (struct val))))))

;; precondition: bv is bytevector.  result is ptr
(define-primcall-lowerer (bv-contents cps k src #f (bv))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'pointer-ref/immediate '(bytevector . 2) (bv))))))

;; precondition: bv is bytevector.  result u64
(define-primcall-lowerer (bv-length cps k src #f (bv))
  (with-cps cps
    (letv ulen)
    (letk kassume
          ($kargs ('ulen) (ulen)
            ($continue k src
              ($primcall 'assume-u64 `(0 . ,(target-max-size-t)) (ulen)))))
    (build-term
      ($continue kassume src
        ($primcall 'word-ref/immediate '(bytevector . 1) (bv))))))

;; precondition: str is a string.  result u64
(define-primcall-lowerer (string-length cps k src #f (str))
  (with-cps cps
    (letv ulen)
    (letk kassume
          ($kargs ('ulen) (ulen)
            ($continue k src
              ($primcall 'assume-u64 `(0 . ,(target-max-size-t)) (ulen)))))
    (build-term
      ($continue kassume src
        ($primcall 'word-ref/immediate '(string . 3) (str))))))

;; precondition: s a string, uidx in range.  result unboxed.
(define-primcall-lowerer (string-ref cps k src #f (s uidx))
  (define stringbuf-f-wide #x400)
  (with-cps cps
    (letv start upos buf ptr tag bits uwpos u32)
    (letk kassume
          ($kargs ('u32) (u32)
            ($continue k src
              ($primcall 'assume-u64 '(0 . #xffffff) (u32)))))
    (letk kwideref
          ($kargs ('uwpos) (uwpos)
            ($continue kassume src
              ($primcall 'u32-ref 'stringbuf (buf ptr uwpos)))))
    (letk kwide
          ($kargs () ()
            ($continue kwideref src
              ($primcall 'ulsh/immediate 2 (upos)))))
    (letk knarrow
          ($kargs () ()
            ($continue k src
              ($primcall 'u8-ref 'stringbuf (buf ptr upos)))))
    (letk kcmp
          ($kargs ('bits) (bits)
            ($branch kwide knarrow src 'u64-imm-= 0 (bits))))
    (letk ktag
          ($kargs ('tag) (tag)
            ($continue kcmp src
              ($primcall 'ulogand/immediate stringbuf-f-wide (tag)))))
    (letk kptr
          ($kargs ('ptr) (ptr)
            ($continue ktag src
              ($primcall 'word-ref/immediate '(stringbuf . 0) (buf)))))
    (letk kwidth
          ($kargs ('buf) (buf)
            ($continue kptr src
              ($primcall 'tail-pointer-ref/immediate '(stringbuf . 2) (buf)))))
    (letk kbuf
          ($kargs ('upos) (upos)
            ($continue kwidth src
              ($primcall 'scm-ref/immediate '(string . 1) (s)))))
    (letk kadd
          ($kargs ('start) (start)
            ($continue kbuf src
              ($primcall 'uadd #f (start uidx)))))
    (build-term
      ($continue kadd src
        ($primcall 'word-ref/immediate '(string . 2) (s))))))

;; precondition: sym is a symbol.
(define-primcall-lowerer (symbol-hash cps k src #f (sym))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'word-ref/immediate '(symbol . 2) (sym))))))

;; precondition: none.
(define-primcall-lowerer (make-atomic-box cps k src #f (val))
  (with-cps cps
    (letv obj tag)
    (letk kdone
          ($kargs () ()
            ($continue k src ($values (obj)))))
    (letk kval
          ($kargs () ()
            ($continue kdone src
              ($primcall 'atomic-scm-set!/immediate '(atomic-box . 1) (obj val)))))
    (letk ktag1
          ($kargs ('tag) (tag)
            ($continue kval src
              ($primcall 'word-set!/immediate '(atomic-box . 0) (obj tag)))))
    (letk ktag0
          ($kargs ('obj) (obj)
            ($continue ktag1 src
              ($primcall 'load-u64 %tc7-atomic-box ()))))
    (build-term
      ($continue ktag0 src
        ($primcall 'allocate-words/immediate '(atomic-box . 2) ())))))

;; precondition: x is atomic box
(define-primcall-lowerer (atomic-box-ref cps k src #f (x))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'atomic-scm-ref/immediate '(atomic-box . 1) (x))))))

;; precondition: x is atomic box
(define-primcall-lowerer (atomic-box-set! cps k src #f (x val))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'atomic-scm-set!/immediate '(atomic-box . 1)
                   (x val))))))

;; precondition: x is atomic box
(define-primcall-lowerer (atomic-box-swap! cps k src param (x val))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'atomic-scm-swap!/immediate '(atomic-box . 1)
                   (x val))))))

;; precondition: x is atomic box
(define-primcall-lowerer (atomic-box-compare-and-swap! cps k src param (x expected desired))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'atomic-scm-compare-and-swap!/immediate '(atomic-box . 1)
                   (x expected desired))))))

;; precondition: code is result of $code
(define-primcall-lowerer (make-closure cps k src nfree (code))
  (define nwords (+ nfree 2))
  (with-cps cps
    (letv closure tag)
    (letk kdone
          ($kargs () ()
            ($continue k src ($values (closure)))))
    (letk kinit
          ($kargs () ()
            ($continue kdone src
              ($primcall 'word-set!/immediate '(closure . 1) (closure code)))))
    (letk ktag1
          ($kargs ('tag) (tag)
            ($continue kinit src
              ($primcall 'word-set!/immediate '(closure . 0) (closure tag)))))
    (letk ktag0
          ($kargs ('closure) (closure)
            ($continue ktag1 src
              ($primcall 'load-u64 (+ %tc7-program (ash nfree 16)) ()))))

    (build-term
      ($continue ktag0 src
        ($primcall 'allocate-words/immediate `(closure . ,nwords) ())))))

;; precondition: closure is closure, idx is in range
(define-primcall-lowerer (closure-ref cps k src (idx . nfree) (closure))
  (let ((pos (+ idx 2)))
    (with-cps cps
      (build-term
        ($continue k src
          ($primcall 'scm-ref/immediate `(closure . ,pos) (closure)))))))

;; precondition: closure is clodure, idx is in range
(define-primcall-lowerer (closure-set! cps k src (idx . nfree) (closure val))
  (let ((pos (+ idx 2)))
    (with-cps cps
      (build-term
        ($continue k src
          ($primcall 'scm-set!/immediate `(closure . ,pos) (closure val)))))))

(define-primcall-lowerer (f64->scm cps k src #f (f64))
  (with-cps cps
    (letv scm tag ptr uidx)
    (letk kdone ($kargs () ()
                  ($continue k src ($values (scm)))))
    (letk kinit ($kargs ('uidx) (uidx)
                  ($continue kdone src
                    ($primcall 'f64-set! 'flonum (scm ptr uidx f64)))))
    (letk kidx ($kargs ('ptr) (ptr)
                 ($continue kinit src ($primcall 'load-u64 0 ()))))
    (letk kptr ($kargs () ()
                 ($continue kidx src
                   ($primcall 'tail-pointer-ref/immediate
                              `(flonum . ,(match (target-word-size)
                                            (4 2)
                                            (8 1)))
                              (scm)))))
    (letk ktag1 ($kargs ('tag) (tag)
                  ($continue kptr src
                    ($primcall 'word-set!/immediate '(flonum . 0) (scm tag)))))
    (letk ktag0 ($kargs ('scm) (scm)
                  ($continue ktag1 src
                    ($primcall 'load-u64 %tc16-flonum ()))))
    (build-term
      ($continue ktag0 src
        ($primcall 'allocate-pointerless-words/immediate
                   `(flonum . ,(match (target-word-size)
                                 (4 4)
                                 (8 2)))
                   ())))))

(define-primcall-lowerer (keyword->symbol cps k src #f (kw))
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall 'scm-ref/immediate '(keyword . 1) (kw))))))

(define-branching-primcall-lowerer (procedure? cps kf kt src #f (x))
  (with-cps cps
    (letv procedure? result)
    (letk kresult
          ($kargs ('result) (result)
            ($branch kt kf src 'eq-constant? #f (result))))
    (letk krecv
          ($kreceive '(result) '() kresult))
    (letk kcall
          ($kargs ('procedure?) (procedure?)
            ($continue krecv src
              ($call procedure? (x)))))
    (build-term
      ($continue kcall src ($prim 'procedure?)))))

(define-branching-primcall-lowerer (number? cps kf kt src #f (x))
  (with-cps cps
    (letk kheap-num
          ($kargs () ()
            ($branch kf kt src 'heap-number? #f (x))))
    (letk kheap
          ($kargs () ()
            ($branch kf kheap-num src 'heap-object? #f (x))))
    (build-term
      ($branch kheap kt src 'fixnum? #f (x)))))
(define-branching-primcall-alias number? complex?)

(define-branching-primcall-lowerer (real? cps kf kt src #f (x))
  (with-cps cps
    (letk kcomp
          ($kargs () ()
            ($branch kt kf src 'compnum? #f (x))))
    (letk kheap-num
          ($kargs () ()
            ($branch kf kcomp src 'heap-number? #f (x))))
    (letk kheap
          ($kargs () ()
            ($branch kf kheap-num src 'heap-object? #f (x))))
    (build-term
      ($branch kheap kt src 'fixnum? #f (x)))))

(define-branching-primcall-lowerer (rational? cps kf kt src #f (x))
  (with-cps cps
    (letv res prim)
    (letk ktest
          ($kargs ('res) (res)
            ($branch kt kf src 'false? #f (res))))
    (letk krecv
          ($kreceive '(val) #f ktest))
    (letk kcall
          ($kargs ('prim) (prim)
            ($continue krecv src ($call prim (x)))))
    (build-term
      ($continue kcall src ($prim 'rational?)))))

(define-branching-primcall-lowerer (integer? cps kf kt src #f (x))
  (with-cps cps
    (letv res prim)
    (letk ktest
          ($kargs ('res) (res)
            ($branch kt kf src 'false? #f (res))))
    (letk krecv
          ($kreceive '(val) #f ktest))
    (letk kcall
          ($kargs ('prim) (prim)
            ($continue krecv src ($call prim (x)))))
    (build-term
      ($continue kcall src ($prim 'integer?)))))

(define-branching-primcall-lowerer (exact-integer? cps kf kt src #f (x))
  (with-cps cps
    (letk kbig
          ($kargs () ()
            ($branch kf kt src 'bignum? #f (x))))
    (letk kheap
          ($kargs () ()
            ($branch kf kbig src 'heap-object? #f (x))))
    (build-term
      ($branch kheap kt src 'fixnum? #f (x)))))

(define-branching-primcall-lowerer (exact? cps kf kt src #f (x))
  (with-cps cps
    (letk kfrac
          ($kargs () ()
            ($branch kf kt src 'fracnum? #f (x))))
    (letk kbig
          ($kargs () ()
            ($branch kfrac kt src 'bignum? #f (x))))
    (build-term
      ($branch kbig kt src 'fixnum? #f (x)))))

(define-branching-primcall-lowerer (inexact? cps kf kt src #f (x))
  (with-cps cps
    (letk kcomp
          ($kargs () ()
            ($branch kf kt src 'compnum? #f (x))))
    (letk kflo
          ($kargs () ()
            ($branch kcomp kt src 'flonum? #f (x))))
    (build-term
      ($branch kflo kf src 'fixnum? #f (x)))))

(define (lower-primcalls cps)
  (with-fresh-name-state cps
    (persistent-intmap
     (intmap-fold
      (lambda (label cont cps)
        (match cont
          (($ $kargs names vars
              ($ $continue k src ($ $calli)))
           (error "$calli unsupported by guile-vm backend"))
          (($ $kargs names vars
              ($ $continue k src ($ $primcall op param args)))
           (match (hashq-ref *primcall-lowerers* op)
             (#f cps)
             (lower
              (with-cps cps
                (let$ term (lower k src param args))
                (setk label ($kargs names vars ,term))))))
          (($ $kargs names vars
              ($ $branch kf kt src op param args))
           (match (hashq-ref *branching-primcall-lowerers* op)
             (#f cps)
             (lower
              (with-cps cps
                (let$ term (lower kf kt src param args))
                (setk label ($kargs names vars ,term))))))
          (_ cps)))
      cps cps))))
