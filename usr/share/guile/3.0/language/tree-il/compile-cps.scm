;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2015,2017-2021 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; This pass converts Tree-IL to the continuation-passing style (CPS)
;;; language.
;;;
;;; CPS is a lower-level representation than Tree-IL.  Converting to
;;; CPS, beyond adding names for all control points and all values,
;;; simplifies expressions in the following ways, among others:
;;;
;;;   * Fixing the order of evaluation.
;;;
;;;   * Converting assigned variables to boxed variables.
;;;
;;;   * Requiring that Scheme's <letrec> has already been lowered to
;;;     <fix>.
;;;
;;;   * Inlining default-value initializers into lambda-case
;;;     expressions.
;;;
;;;   * Inlining prompt bodies.
;;;
;;;   * Turning toplevel and module references into primcalls.  This
;;;     involves explicitly modelling the "scope" of toplevel lookups
;;;     (indicating the module with respect to which toplevel bindings
;;;     are resolved).
;;;
;;; The utility of CPS is that it gives a name to everything: every
;;; intermediate value, and every control point (continuation).  As such
;;; it is more verbose than Tree-IL, but at the same time more simple as
;;; the number of concepts is reduced.
;;;
;;; Code:

(define-module (language tree-il compile-cps)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold filter-map))
  #:use-module (srfi srfi-26)
  #:use-module ((system foreign) #:select (make-pointer pointer->scm))
  #:use-module (system base target)
  #:use-module (system base types internal)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language tree-il cps-primitives)
  #:use-module (language tree-il)
  #:use-module (language cps intmap)
  #:export (compile-cps))

(define (convert-primcall/default cps k src op param . args)
  (with-cps cps
    (build-term
      ($continue k src ($primcall op param args)))))

(define *primcall-converters* (make-hash-table))
(define-syntax-rule (define-primcall-converter name proc)
  (hashq-set! *primcall-converters* 'name proc))

(define (convert-primcall* cps k src op param args)
  (let ((proc (hashq-ref *primcall-converters* op convert-primcall/default)))
    (apply proc cps k src op param args)))

(define (convert-primcall cps k src op param . args)
  (convert-primcall* cps k src op param args))

(define (ensure-vector cps src op pred v have-length)
  (define msg
    (match pred
      ('vector?
       "Wrong type argument in position 1 (expecting vector): ~S")
      ('mutable-vector?
       "Wrong type argument in position 1 (expecting mutable vector): ~S")))
  (define not-vector (vector 'wrong-type-arg (symbol->string op) msg))
  (with-cps cps
    (letv w0 slen ulen rlen)
    (letk knot-vector
          ($kargs () () ($throw src 'throw/value+data not-vector (v))))
    (let$ body (have-length slen))
    (letk k ($kargs ('slen) (slen) ,body))
    (letk kcast
          ($kargs ('rlen) (rlen)
            ($continue k src ($primcall 'u64->s64 #f (rlen)))))
    (letk kassume
          ($kargs ('ulen) (ulen)
            ($continue kcast src
              ($primcall 'assume-u64 `(0 . ,(target-max-vector-length)) (ulen)))))
    (letk krsh
          ($kargs ('w0) (w0)
            ($continue kassume src ($primcall 'ursh/immediate 8 (w0)))))
    (letk kv
          ($kargs () ()
            ($continue krsh src
              ($primcall 'word-ref/immediate '(vector . 0) (v)))))
    (letk kheap-object
          ($kargs () ()
            ($branch knot-vector kv src pred #f (v))))
    (build-term
      ($branch knot-vector kheap-object src 'heap-object? #f (v)))))

(define (untag-fixnum-index-in-range cps src op idx slen have-index-in-range)
  ;; Precondition: SLEN is a non-negative S64 that is representable as a
  ;; fixnum.
  (define not-fixnum
    (vector 'wrong-type-arg
            (symbol->string op)
            "Wrong type argument in position 2 (expecting small integer): ~S"))
  (define out-of-range
    (vector 'out-of-range
            (symbol->string op)
            "Argument 2 out of range: ~S"))
  (with-cps cps
    (letv sidx)
    (letk knot-fixnum
          ($kargs () () ($throw src 'throw/value+data not-fixnum (idx))))
    (letk kout-of-range
          ($kargs () () ($throw src 'throw/value+data out-of-range (idx))))
    (let$ body (have-index-in-range sidx))
    (letk k ($kargs () () ,body))
    (letk kboundlen
          ($kargs () ()
            ($branch kout-of-range k src 's64-< #f (sidx slen))))
    (letk kbound0
          ($kargs ('sidx) (sidx)
            ($branch kboundlen kout-of-range src 's64-imm-< 0 (sidx))))
    (letk kuntag
          ($kargs () ()
            ($continue kbound0 src ($primcall 'untag-fixnum #f (idx)))))
    (build-term ($branch knot-fixnum kuntag src 'fixnum? #f (idx)))))

(define (untag-fixnum-in-imm-range cps src op size min max have-int-in-range)
  (define not-fixnum
    (vector 'wrong-type-arg
            (symbol->string op)
            "Wrong type argument in position 2 (expecting small integer): ~S"))
  (define out-of-range
    (vector 'out-of-range
            (symbol->string op)
            "Argument 2 out of range: ~S"))
  (with-cps cps
    (letv ssize)
    (letk knot-fixnum
          ($kargs () () ($throw src 'throw/value+data not-fixnum (size))))
    (letk kout-of-range
          ($kargs () () ($throw src 'throw/value+data out-of-range (size))))
    (let$ body (have-int-in-range ssize))
    (letk k ($kargs () () ,body))
    (letk kboundlen
          ($kargs () ()
            ($branch k kout-of-range src 'imm-s64-< max (ssize))))
    (letk kbound0
          ($kargs ('ssize) (ssize)
            ($branch kboundlen kout-of-range src 's64-imm-< min (ssize))))
    (letk kuntag
          ($kargs () ()
            ($continue kbound0 src ($primcall 'untag-fixnum #f (size)))))
    (build-term ($branch knot-fixnum kuntag src 'fixnum? #f (size)))))

(define (compute-vector-access-pos cps src sidx have-pos)
  (with-cps cps
    (letv spos upos)
    (let$ body (have-pos upos))
    (letk kref ($kargs ('pos) (upos) ,body))
    (letk kcvt ($kargs ('pos) (spos)
                 ($continue kref src ($primcall 's64->u64 #f (spos)))))
    (build-term
      ($continue kcvt src ($primcall 'sadd/immediate 1 (sidx))))))

(define (prepare-vector-access cps src op pred v idx access)
  (ensure-vector
   cps src op pred v
   (lambda (cps slen)
     (untag-fixnum-index-in-range
      cps src op idx slen
      (lambda (cps sidx)
        (compute-vector-access-pos
         cps src sidx
         (lambda (cps pos)
           (access cps v pos))))))))

(define (prepare-vector-access/immediate cps src op pred v idx access)
  (unless (and (exact-integer? idx) (<= 0 idx (1- (target-max-vector-length))))
    (error "precondition failed" idx))
  (ensure-vector
   cps src op pred v
   (lambda (cps slen)
     (define out-of-range
       (vector 'out-of-range
               (symbol->string op)
               "Argument 2 out of range: ~S"))
     (with-cps cps
       (letv tidx)
       (letk kthrow
             ($kargs ('tidx) (tidx)
               ($throw src 'throw/value+data out-of-range (tidx))))
       (letk kout-of-range
             ($kargs () ()
               ($continue kthrow src ($const idx))))
       (let$ body (access v (1+ idx)))
       (letk k ($kargs () () ,body))
       (build-term
         ($branch kout-of-range k src 'imm-s64-< idx (slen)))))))

(define-primcall-converter vector-length
  (lambda (cps k src op param v)
    (ensure-vector
     cps src op 'vector? v
     (lambda (cps slen)
       (with-cps cps
         (build-term
           ($continue k src ($primcall 'tag-fixnum #f (slen)))))))))

(define-primcall-converter vector-ref
  (lambda (cps k src op param v idx)
    (prepare-vector-access
     cps src op 'vector? v idx
     (lambda (cps v upos)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'scm-ref 'vector (v upos)))))))))

(define-primcall-converter vector-ref/immediate
  (lambda (cps k src op param v)
    (prepare-vector-access/immediate
     cps src 'vector-ref 'vector? v param
     (lambda (cps v pos)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'scm-ref/immediate `(vector . ,pos) (v)))))))))

(define-primcall-converter vector-set!
  (lambda (cps k src op param v idx val)
    (prepare-vector-access
     cps src op 'mutable-vector? v idx
     (lambda (cps v upos)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'scm-set! 'vector (v upos val)))))))))

(define-primcall-converter vector-set!/immediate
  (lambda (cps k src op param v val)
    (prepare-vector-access/immediate
     cps src 'vector-set! 'mutable-vector? v param
     (lambda (cps v pos)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'scm-set!/immediate `(vector . ,pos) (v val)))))))))

(define-primcall-converter vector-init!
  (lambda (cps k src op param v val)
    (define pos (1+ param))
    (with-cps cps
      (build-term
        ($continue k src
          ($primcall 'scm-set!/immediate `(vector . ,pos) (v val)))))))

(define (emit-initializations-as-loop cps k src obj annotation start nwords init)
  (with-cps cps
    (letv pos)
    (letk kloop ,#f) ;; Patched later.
    (letk kback
          ($kargs () ()
            ($continue kloop src
              ($primcall 'uadd/immediate 1 (pos)))))
    (letk kinit
          ($kargs () ()
            ($continue kback src
              ($primcall 'scm-set! annotation (obj pos init)))))
    (setk kloop
          ($kargs ('pos) (pos)
            ($branch k kinit src 'u64-< #f (pos nwords))))
    (build-term
      ($continue kloop src
        ($primcall 'load-u64 start ())))))

(define-primcall-converter allocate-vector
  (lambda (cps k src op param)
    (define size param)
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
          ($primcall 'allocate-words/immediate `(vector . ,nwords) ()))))))

(define-primcall-converter make-vector
  (lambda (cps k src op param size init)
    (untag-fixnum-in-imm-range
     cps src op size 0 (target-max-vector-length)
     (lambda (cps ssize)
       (with-cps cps
         (letv usize nwords v w0-high w0)
         (letk kdone
               ($kargs () ()
                 ($continue k src ($values (v)))))
         (let$ init-loop
               (emit-initializations-as-loop kdone src v 'vector 1 nwords init))
         (letk kbody ($kargs () () ,init-loop))
         (letk ktag2
               ($kargs ('w0) (w0)
                 ($continue kbody src
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
         (letk kadd1
               ($kargs ('usize) (usize)
                 ($continue kalloc src
                   ;; Header word.
                   ($primcall 'uadd/immediate 1 (usize)))))
         (build-term
           ($continue kadd1 src
             ;; Header word.
             ($primcall 's64->u64 #f (ssize)))))))))

(define-primcall-converter make-vector/immediate
  (lambda (cps k src op param init)
    (define size param)
    (define nwords (1+ size))
    (define (init-fields cps v pos kdone)
      ;; Inline the initializations, up to vectors of size 32.  Above
      ;; that it's a bit of a waste, so reify a loop instead.
      (cond
       ((<= 32 nwords)
        (with-cps cps
          (letv unwords)
          (let$ init-loop
                (emit-initializations-as-loop kdone src v 'vector
                                              pos unwords init))
          (letk kinit ($kargs ('unwords) (unwords) ,init-loop))
          (letk kusize ($kargs () ()
                         ($continue kinit src
                           ($primcall 'load-u64 nwords ()))))
          kusize))
       ((< pos nwords)
        (with-cps cps
          (let$ knext (init-fields v (1+ pos) kdone))
          (letk kinit
                ($kargs () ()
                  ($continue knext src
                    ($primcall 'scm-set!/immediate `(vector . ,pos)
                               (v init)))))
          kinit))
       (else
        (with-cps cps
          kdone))))
    (unless (and (exact-integer? size) (<= 0 size (target-max-vector-length)))
      (error "precondition failed" size))
    (with-cps cps
      (letv v w0)
      (letk kdone
            ($kargs () ()
              ($continue k src ($values (v)))))
      (let$ kinit (init-fields v 1 kdone))
      (letk ktag1
            ($kargs ('w0) (w0)
              ($continue kinit src
                ($primcall 'word-set!/immediate '(vector . 0) (v w0)))))
      (letk ktag0
            ($kargs ('v) (v)
              ($continue ktag1 src
                ($primcall 'load-u64 (+ %tc7-vector (ash size 8)) ()))))
      (build-term
        ($continue ktag0 src
          ($primcall 'allocate-words/immediate `(vector . ,nwords) ()))))))

(define (ensure-pair cps src op pred x is-pair)
  (define msg
    (match pred
      ('pair?
       "Wrong type argument in position 1 (expecting pair): ~S")
      ('mutable-pair?
       "Wrong type argument in position 1 (expecting mutable pair): ~S")))
  (define not-pair (vector 'wrong-type-arg (symbol->string op) msg))
  (with-cps cps
    (letk knot-pair ($kargs () () ($throw src 'throw/value+data not-pair (x))))
    (let$ body (is-pair))
    (letk k ($kargs () () ,body))
    (letk kheap-object ($kargs () () ($branch knot-pair k src pred #f (x))))
    (build-term ($branch knot-pair kheap-object src 'heap-object? #f (x)))))

(define-primcall-converter cons
  (lambda (cps k src op param head tail)
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
          ($primcall 'allocate-words/immediate '(pair . 2) ()))))))

(define-primcall-converter car
  (lambda (cps k src op param pair)
    (ensure-pair
     cps src 'car 'pair? pair
     (lambda (cps)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'scm-ref/immediate '(pair . 0) (pair)))))))))

(define-primcall-converter cdr
  (lambda (cps k src op param pair)
    (ensure-pair
     cps src 'cdr 'pair? pair
     (lambda (cps)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'scm-ref/immediate '(pair . 1) (pair)))))))))

(define-primcall-converter set-car!
  (lambda (cps k src op param pair val)
    (ensure-pair
     ;; FIXME: Use mutable-pair? as predicate.
     cps src 'set-car! 'pair? pair
     (lambda (cps)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'scm-set!/immediate '(pair . 0) (pair val)))))))))

(define-primcall-converter set-cdr!
  (lambda (cps k src op param pair val)
    (ensure-pair
     ;; FIXME: Use mutable-pair? as predicate.
     cps src 'set-cdr! 'pair? pair
     (lambda (cps)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'scm-set!/immediate '(pair . 1) (pair val)))))))))

(define-primcall-converter box
  (lambda (cps k src op param val)
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
          ($primcall 'allocate-words/immediate '(box . 2) ()))))))

(define-primcall-converter %box-ref
  (lambda (cps k src op param box)
    (define unbound
      #(misc-error "variable-ref" "Unbound variable: ~S"))
    (with-cps cps
      (letv val)
      (letk kunbound ($kargs () () ($throw src 'throw/value unbound (box))))
      (letk kbound ($kargs () () ($continue k src ($values (val)))))
      (letk ktest
            ($kargs ('val) (val)
              ($branch kbound kunbound src 'undefined? #f (val))))
      (build-term
        ($continue ktest src
          ($primcall 'scm-ref/immediate '(box . 1) (box)))))))

(define-primcall-converter %box-set!
  (lambda (cps k src op param box val)
    (with-cps cps
      (build-term
        ($continue k src
          ($primcall 'scm-set!/immediate '(box . 1) (box val)))))))

(define (ensure-box cps src op x is-box)
  (define not-box
    (vector 'wrong-type-arg
            (symbol->string op)
            "Wrong type argument in position 1 (expecting box): ~S"))
  (with-cps cps
    (letk knot-box ($kargs () () ($throw src 'throw/value+data not-box (x))))
    (let$ body (is-box))
    (letk k ($kargs () () ,body))
    (letk kheap-object ($kargs () () ($branch knot-box k src 'variable? #f (x))))
    (build-term ($branch knot-box kheap-object src 'heap-object? #f (x)))))

(define-primcall-converter box-ref
  (lambda (cps k src op param box)
    (ensure-box
     cps src 'variable-ref box
     (lambda (cps)
       (convert-primcall cps k src '%box-ref param box)))))

(define-primcall-converter box-set!
  (lambda (cps k src op param box val)
    (ensure-box
     cps src 'variable-set! box
     (lambda (cps)
       (convert-primcall cps k src '%box-set! param box val)))))

(define (ensure-struct cps src op x have-vtable)
  (define not-struct
    (vector 'wrong-type-arg
            (symbol->string op)
            "Wrong type argument in position 1 (expecting struct): ~S"))
  (with-cps cps
    (letv vtable)
    (letk knot-struct
          ($kargs () () ($throw src 'throw/value+data not-struct (x))))
    (let$ body (have-vtable vtable))
    (letk k ($kargs ('vtable) (vtable) ,body))
    (letk kvtable ($kargs () ()
                    ($continue k src ($primcall 'scm-ref/tag 'struct (x)))))
    (letk kheap-object
          ($kargs () () ($branch knot-struct kvtable src 'struct? #f (x))))
    (build-term ($branch knot-struct kheap-object src 'heap-object? #f (x)))))

(define-primcall-converter struct-vtable
  (lambda (cps k src op param struct)
    (ensure-struct
     cps src 'struct-vtable struct
     (lambda (cps vtable)
       (with-cps cps
         (build-term
           ($continue k src ($values (vtable)))))))))

(define (ensure-vtable cps src op vtable is-vtable)
  (ensure-struct
   cps src op vtable
   (lambda (cps vtable-vtable)
     (define not-vtable
       (vector 'wrong-type-arg
               (symbol->string op)
               "Wrong type argument in position 1 (expecting vtable): ~S"))
     (define vtable-index-flags 1)    ; FIXME: pull from struct.h
     (define vtable-offset-flags (1+ vtable-index-flags))
     (define vtable-validated-mask #b11)
     (define vtable-validated-value #b11)
     (with-cps cps
       (letv flags mask res)
       (letk knot-vtable
             ($kargs () () ($throw src 'throw/value+data not-vtable (vtable))))
       (let$ body (is-vtable))
       (letk k ($kargs () () ,body))
       (letk ktest
             ($kargs ('res) (res)
               ($branch knot-vtable k src
                 'u64-imm-= vtable-validated-value (res))))
       (letk kand
             ($kargs ('mask) (mask)
               ($continue ktest src
                 ($primcall 'ulogand #f (flags mask)))))
       (letk kflags
             ($kargs ('flags) (flags)
               ($continue kand src
                 ($primcall 'load-u64 vtable-validated-mask ()))))
       (build-term
         ($continue kflags src
           ($primcall 'word-ref/immediate
                      `(struct . ,vtable-offset-flags) (vtable-vtable))))))))

(define-primcall-converter allocate-struct
  (lambda (cps k src op nwords vtable)
    (ensure-vtable
     cps src 'allocate-struct vtable
     (lambda (cps)
       (define vtable-index-size 5) ; FIXME: pull from struct.h
       (define vtable-index-unboxed-fields 6) ; FIXME: pull from struct.h
       (define vtable-offset-size (1+ vtable-index-size))
       (define vtable-offset-unboxed-fields (1+ vtable-index-unboxed-fields))
       (define wrong-number
         (vector 'wrong-number-of-args
                 (symbol->string op)
                 "Wrong number of initializers when instantiating ~A"))
       (define has-unboxed
         (vector 'wrong-type-arg
                 (symbol->string op)
                 "Expected vtable with no unboxed fields: ~A"))
       (define (check-all-boxed cps kf kt vtable ptr word)
         (if (< (* word 32) nwords)
             (with-cps cps
               (letv idx bits)
               (let$ checkboxed (check-all-boxed kf kt vtable ptr (1+ word)))
               (letk kcheckboxed ($kargs () () ,checkboxed))
               (letk kcheck
                     ($kargs ('bits) (bits)
                       ($branch kf kcheckboxed src 'u64-imm-= 0 (bits))))
               (letk kword
                     ($kargs ('idx) (idx)
                       ($continue kcheck src
                         ($primcall 'u32-ref 'bitmask (vtable ptr idx)))))
               (build-term
                 ($continue kword src
                   ($primcall 'load-u64 word ()))))
             (with-cps cps
               (build-term ($continue kt src ($values ()))))))
       (with-cps cps
         (letv rfields nfields ptr s)
         (letk kwna
               ($kargs () () ($throw src 'throw/value wrong-number (vtable))))
         (letk kunboxed
               ($kargs () () ($throw src 'throw/value+data has-unboxed (vtable))))
         (letk kdone
               ($kargs () () ($continue k src ($values (s)))))
         (letk ktag
               ($kargs ('s) (s)
                 ($continue kdone src
                   ($primcall 'scm-set!/tag 'struct (s vtable)))))
         (letk kalloc
               ($kargs () ()
                 ($continue ktag src
                   ($primcall 'allocate-words/immediate
                              `(struct . ,(1+ nwords)) ()))))
         (let$ checkboxed (check-all-boxed kunboxed kalloc vtable ptr 0))
         (letk kcheckboxed ($kargs ('ptr) (ptr) ,checkboxed))
         (letk kaccess
               ($kargs () ()
                 ($continue kcheckboxed src
                   ($primcall 'pointer-ref/immediate
                              `(struct . ,vtable-offset-unboxed-fields)
                              (vtable)))))
         (letk knfields
               ($kargs ('nfields) (nfields)
                 ($branch kwna kaccess src 'u64-imm-= nwords (nfields))))
         (letk kassume
               ($kargs ('rfields) (rfields)
                 ($continue knfields src
                   ($primcall 'assume-u64 `(0 . ,(target-max-size-t/scm))
                              (rfields)))))
         (build-term
           ($continue kassume src
             ($primcall 'word-ref/immediate
                        `(struct . ,vtable-offset-size) (vtable)))))))))

(define (ensure-struct-index-in-range cps src op vtable idx boxed? in-range)
  (define vtable-index-size 5)           ; FIXME: pull from struct.h
  (define vtable-index-unboxed-fields 6) ; FIXME: pull from struct.h
  (define vtable-offset-size (1+ vtable-index-size))
  (define vtable-offset-unboxed-fields (1+ vtable-index-unboxed-fields))
  (define bad-type
    (vector
     'wrong-type-arg
     (symbol->string op)
     (if boxed?
         "Wrong type argument in position 2 (expecting boxed field): ~S"
         "Wrong type argument in position 2 (expecting unboxed field): ~S")))
  (define out-of-range
    (vector 'out-of-range
            (symbol->string op)
            "Argument 2 out of range: ~S"))
  (with-cps cps
    (letv rfields nfields ptr word bits mask res throwval1 throwval2)
    (letk kthrow1
          ($kargs (#f) (throwval1)
            ($throw src 'throw/value+data out-of-range (throwval1))))
    (letk kthrow2
          ($kargs (#f) (throwval2)
            ($throw src 'throw/value+data bad-type (throwval2))))
    (letk kbadidx ($kargs () () ($continue kthrow1 src ($const idx))))
    (letk kbadtype ($kargs () () ($continue kthrow2 src ($const idx))))

    (let$ body (in-range))
    (letk k ($kargs () () ,body))
    (letk ktest
          ($kargs ('res) (res)
            ($branch (if boxed? kbadtype k) (if boxed? k kbadtype) src
              'u64-imm-= 0 (res))))
    (letk kand
          ($kargs ('mask) (mask)
            ($continue ktest src
              ($primcall 'ulogand #f (mask bits)))))
    (letk kbits
          ($kargs ('bits) (bits)
            ($continue kand src
              ($primcall 'load-u64 (ash 1 (logand idx 31)) ()))))
    (letk kword
          ($kargs ('word) (word)
            ($continue kbits src
              ($primcall 'u32-ref 'bitmask (vtable ptr word)))))
    (letk kptr
          ($kargs ('ptr) (ptr)
            ($continue kword src
              ($primcall 'load-u64 (ash idx -5) ()))))
    (letk kaccess
          ($kargs () ()
            ($continue kptr src
              ($primcall 'pointer-ref/immediate
                         `(struct . ,vtable-offset-unboxed-fields)
                         (vtable)))))
    (letk knfields
          ($kargs ('nfields) (nfields)
            ($branch kbadidx kaccess src 'imm-u64-< idx (nfields))))
    (letk kassume
          ($kargs ('rfields) (rfields)
            ($continue knfields src
              ($primcall 'assume-u64 `(0 . ,(target-max-size-t)) (rfields)))))
    (build-term
      ($continue kassume src
        ($primcall 'word-ref/immediate
                   `(struct . ,vtable-offset-size) (vtable))))))

(define (prepare-struct-scm-access cps src op struct idx boxed? have-pos)
  (define not-struct
    (vector 'wrong-type-arg
            (symbol->string op)
            "Wrong type argument in position 1 (expecting struct): ~S"))
  (ensure-struct
   cps src op struct
   (lambda (cps vtable)
     (ensure-struct-index-in-range
      cps src op vtable idx boxed?
      (lambda (cps) (have-pos cps (1+ idx)))))))

(define-primcall-converter struct-ref/immediate
  (lambda (cps k src op param struct)
    (prepare-struct-scm-access
     cps src op struct param #t
     (lambda (cps pos)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'scm-ref/immediate `(struct . ,pos) (struct)))))))))

(define-primcall-converter struct-set!/immediate
  (lambda (cps k src op param struct val)
    (prepare-struct-scm-access
     cps src op struct param #t
     (lambda (cps pos)
       (with-cps cps
         (letk k* ($kargs () () ($continue k src ($values (val)))))
         (build-term
           ($continue k* src
             ($primcall 'scm-set!/immediate `(struct . ,pos) (struct val)))))))))

(define-primcall-converter struct-init!
  (lambda (cps k src op param s val)
    (define pos (1+ param))
    (with-cps cps
      (build-term
        ($continue k src
          ($primcall 'scm-set!/immediate `(struct . ,pos) (s val)))))))

(define-primcall-converter struct-ref
  (lambda (cps k src op param struct idx)
    (with-cps cps
      (letv prim res)
      (letk krecv ($kreceive '(res) #f k))
      (letk kprim ($kargs ('prim) (prim)
                    ($continue krecv src ($call prim (struct idx)))))
      (build-term
        ($continue kprim src ($prim 'struct-ref))))))

(define-primcall-converter struct-set!
  (lambda (cps k src op param struct idx val)
    (with-cps cps
      (letv prim res)
      ;; struct-set! prim returns the value.
      (letk krecv ($kreceive '(res) #f k))
      (letk kprim ($kargs ('prim) (prim)
                    ($continue krecv src ($call prim (struct idx val)))))
      (build-term
        ($continue kprim src ($prim 'struct-set!))))))

(define (untag-bytevector-index cps src op idx ulen width have-uidx)
  (define not-fixnum
    (vector 'wrong-type-arg
            (symbol->string op)
            "Wrong type argument in position 2 (expecting small integer): ~S"))
  (define out-of-range
    (vector 'out-of-range
            (symbol->string op)
            "Argument 2 out of range: ~S"))
  (with-cps cps
    (letv sidx uidx maxidx+1)
    (letk knot-fixnum
          ($kargs () () ($throw src 'throw/value+data not-fixnum (idx))))
    (letk kout-of-range
          ($kargs () () ($throw src 'throw/value+data out-of-range (idx))))
    (let$ body (have-uidx uidx))
    (letk k ($kargs () () ,body))
    (letk ktestidx
          ($kargs ('maxidx+1) (maxidx+1)
            ($branch kout-of-range k src 'u64-< #f (uidx maxidx+1))))
    (letk kdeclen
          ($kargs () ()
            ($continue ktestidx src
              ($primcall 'usub/immediate (1- width) (ulen)))))
    (letk ktestlen
          ($kargs ('uidx) (uidx)
            ($branch kout-of-range kdeclen src 'imm-u64-< (1- width) (ulen))))
    (letk kcvt
          ($kargs () ()
            ($continue ktestlen src ($primcall 's64->u64 #f (sidx)))))
    (letk kbound0
          ($kargs ('sidx) (sidx)
            ($branch kcvt kout-of-range src 's64-imm-< 0 (sidx))))
    (letk kuntag
          ($kargs () ()
            ($continue kbound0 src ($primcall 'untag-fixnum #f (idx)))))
    (build-term ($branch knot-fixnum kuntag src 'fixnum? #f (idx)))))

(define (ensure-bytevector cps k src op pred x)
  (define msg
    (match pred
      ('bytevector?
       "Wrong type argument in position 1 (expecting bytevector): ~S")
      ('mutable-bytevector?
       "Wrong type argument in position 1 (expecting mutable bytevector): ~S")))
  (define bad-type (vector 'wrong-type-arg (symbol->string op) msg))
  (with-cps cps
    (letk kf ($kargs () () ($throw src 'throw/value+data bad-type (x))))
    (letk kheap-object ($kargs () () ($branch kf k src pred #f (x))))
    (build-term ($branch kf kheap-object src 'heap-object? #f (x)))))

(define (prepare-bytevector-access cps src op pred bv idx width
                                   have-ptr-and-uidx)
  (with-cps cps
    (letv ulen rlen)
    (let$ access
          (untag-bytevector-index
           src op idx rlen width
           (lambda (cps uidx)
             (with-cps cps
               (letv ptr)
               (let$ body (have-ptr-and-uidx ptr uidx))
               (letk k ($kargs ('ptr) (ptr) ,body))
               (build-term
                 ($continue k src
                   ($primcall 'pointer-ref/immediate '(bytevector . 2)
                              (bv))))))))
    (letk k ($kargs ('rlen) (rlen) ,access))
    (letk kassume
          ($kargs ('ulen) (ulen)
            ($continue k src
              ($primcall 'assume-u64 `(0 . ,(target-max-size-t)) (ulen)))))
    (letk klen
          ($kargs () ()
            ($continue kassume src
              ($primcall 'word-ref/immediate '(bytevector . 1) (bv)))))
    ($ (ensure-bytevector klen src op pred bv))))

(define (bytevector-ref-converter scheme-name ptr-op width kind)
  (define (tag cps k src val)
    (match kind
      ('unsigned
       (if (< (ash 1 (* width 8)) (target-most-positive-fixnum))
           (with-cps cps
             (letv s)
             (letk kcvt
                   ($kargs ('s) (s)
                           ($continue k src ($primcall 'tag-fixnum #f (s)))))
             (build-term
               ($continue kcvt src ($primcall 'u64->s64 #f (val)))))
           (with-cps cps
             (build-term
               ($continue k src ($primcall 'u64->scm #f (val)))))))
      ('signed
       (if (< (ash 1 (* width 8)) (target-most-positive-fixnum))
           (with-cps cps
             (build-term
               ($continue k src ($primcall 'tag-fixnum #f (val)))))
           (with-cps cps
             (build-term
               ($continue k src ($primcall 's64->scm #f (val)))))))
      ('float
       (with-cps cps
         (build-term
           ($continue k src ($primcall 'f64->scm #f (val))))))))
  (lambda (cps k src op param bv idx)
    (prepare-bytevector-access
     cps src scheme-name 'bytevector? bv idx width
     (lambda (cps ptr uidx)
       (with-cps cps
         (letv val)
         (let$ body (tag k src  val))
         (letk ktag ($kargs ('val) (val) ,body))
         (build-term
           ($continue ktag src
             ($primcall ptr-op 'bytevector (bv ptr uidx)))))))))

(define (bytevector-set-converter scheme-name ptr-op width kind)
  (define out-of-range
    (vector 'out-of-range
            (symbol->string scheme-name)
            "Argument 3 out of range: ~S"))
  (define (limit-urange cps src val uval hi in-range)
    (with-cps cps
      (letk kbad ($kargs () ()
                   ($throw src 'throw/value+data out-of-range (val))))
      (let$ body (in-range uval))
      (letk k ($kargs () () ,body))
      (build-term
        ($branch k kbad src 'imm-u64-< hi (uval)))))
  (define (limit-srange cps src val sval lo hi in-range)
    (with-cps cps
      (letk kbad ($kargs () ()
                   ($throw src 'throw/value+data out-of-range (val))))
      (let$ body (in-range sval))
      (letk k ($kargs () () ,body))
      (letk k' ($kargs () ()
                 ($branch k kbad src 's64-imm-< lo (sval))))
      (build-term
        ($branch k' kbad src 'imm-s64-< hi (sval)))))
  (define (integer-unboxer lo hi)
    (lambda (cps src val have-val)
      (cond
       ((<= hi (target-most-positive-fixnum))
        (let ((have-val (if (zero? lo)
                            (lambda (cps s)
                              (with-cps cps
                                (letv u)
                                (let$ body (have-val u))
                                (letk k ($kargs ('u) (u) ,body))
                                (build-term
                                  ($continue k src
                                    ($primcall 's64->u64 #f (s))))))
                            have-val)))
          (with-cps cps
            (letv sval)
            (letk kbad ($kargs () ()
                         ($throw src 'throw/value+data out-of-range (val))))
            (let$ body (have-val sval))
            (letk k ($kargs () () ,body))
            (letk khi ($kargs () ()
                       ($branch k kbad src 'imm-s64-< hi (sval))))
            (letk klo ($kargs ('sval) (sval)
                       ($branch khi kbad src 's64-imm-< lo (sval))))
            (letk kuntag
                  ($kargs () ()
                    ($continue klo src ($primcall 'untag-fixnum #f (val)))))
            (build-term
              ($branch kbad kuntag src 'fixnum? #f (val))))))
       ((zero? lo)
        (with-cps cps
          (letv u)
          (let$ body (limit-urange src val u hi have-val))
          (letk khi ($kargs ('u) (u) ,body))
          (build-term
            ($continue khi src ($primcall 'scm->u64 #f (val))))))
       (else
        (with-cps cps
          (letv s)
          (let$ body (limit-srange src val s lo hi have-val))
          (letk khi ($kargs ('s) (s) ,body))
          (build-term
            ($continue khi src ($primcall 'scm->s64 #f (val)))))))))
  (define untag
    (match kind
      ('unsigned (integer-unboxer 0 (1- (ash 1 (* width 8)))))
      ('signed   (integer-unboxer (ash -1 (1- (* width 8)))
                                  (1- (ash 1 (1- (* width 8))))))
      ('float
       (lambda (cps src val have-val)
         (with-cps cps
           (letv f)
           (let$ body (have-val f))
           (letk k ($kargs ('f) (f) ,body))
           (build-term
             ($continue k src ($primcall 'scm->f64 #f (val)))))))))
  (lambda (cps k src op param bv idx val)
    (prepare-bytevector-access
     cps src scheme-name 'bytevector? bv idx width
     (lambda (cps ptr uidx)
       (untag
        cps src val
        (lambda (cps uval)
          (with-cps cps
            (build-term
              ($continue k src
                ($primcall ptr-op 'bytevector (bv ptr uidx uval)))))))))))

(define-syntax-rule (define-bytevector-ref-converter
                      cps-name scheme-name op width kind)
  (define-primcall-converter cps-name
    (bytevector-ref-converter 'scheme-name 'op width 'kind)))
(define-syntax-rule (define-bytevector-ref-converters (cvt ...) ...)
  (begin
    (define-bytevector-ref-converter cvt ...)
    ...))

(define-syntax-rule (define-bytevector-set-converter
                      cps-name scheme-name op width kind)
  (define-primcall-converter cps-name
    (bytevector-set-converter 'scheme-name 'op width 'kind)))
(define-syntax-rule (define-bytevector-set-converters (cvt ...) ...)
  (begin
    (define-bytevector-set-converter cvt ...)
    ...))

(define-primcall-converter bv-length
  (lambda (cps k src op param bv)
    (with-cps cps
      (letv ulen rlen)
      (letk ktag ($kargs ('rlen) (rlen)
                   ($continue k src ($primcall 'u64->scm #f (rlen)))))
      (letk kassume
          ($kargs ('ulen) (ulen)
            ($continue ktag src
              ($primcall 'assume-u64 `(0 . ,(target-max-size-t)) (ulen)))))
      (letk klen
            ($kargs () ()
              ($continue kassume src
                ($primcall 'word-ref/immediate '(bytevector . 1) (bv)))))
      ($ (ensure-bytevector klen src op 'bytevector? bv)))))

(define-bytevector-ref-converters
  (bv-u8-ref   bytevector-u8-ref                  u8-ref   1 unsigned)
  (bv-u16-ref  bytevector-u16-native-ref          u16-ref  2 unsigned)
  (bv-u32-ref  bytevector-u32-native-ref          u32-ref  4 unsigned)
  (bv-u64-ref  bytevector-u64-native-ref          u64-ref  8 unsigned)
  (bv-s8-ref   bytevector-s8-ref                  s8-ref   1 signed)
  (bv-s16-ref  bytevector-s16-native-ref          s16-ref  2 signed)
  (bv-s32-ref  bytevector-s32-native-ref          s32-ref  4 signed)
  (bv-s64-ref  bytevector-s64-native-ref          s64-ref  8 signed)
  (bv-f32-ref  bytevector-ieee-single-native-ref  f32-ref  4 float)
  (bv-f64-ref  bytevector-ieee-double-native-ref  f64-ref  8 float))

(define-bytevector-set-converters
  (bv-u8-set!  bytevector-u8-set!                 u8-set!  1 unsigned)
  (bv-u16-set! bytevector-u16-native-set!         u16-set! 2 unsigned)
  (bv-u32-set! bytevector-u32-native-set!         u32-set! 4 unsigned)
  (bv-u64-set! bytevector-u64-native-set!         u64-set! 8 unsigned)
  (bv-s8-set!  bytevector-s8-set!                 s8-set!  1 signed)
  (bv-s16-set! bytevector-s16-native-set!         s16-set! 2 signed)
  (bv-s32-set! bytevector-s32-native-set!         s32-set! 4 signed)
  (bv-s64-set! bytevector-s64-native-set!         s64-set! 8 signed)
  (bv-f32-set! bytevector-ieee-single-native-set! f32-set! 4 float)
  (bv-f64-set! bytevector-ieee-double-native-set! f64-set! 8 float))

(define (ensure-string cps src op x have-length)
  (define msg "Wrong type argument in position 1 (expecting string): ~S")
  (define not-string (vector 'wrong-type-arg (symbol->string op) msg))
  (with-cps cps
    (letv ulen rlen)
    (letk knot-string
          ($kargs () () ($throw src 'throw/value+data not-string (x))))
    (let$ body (have-length rlen))
    (letk k ($kargs ('rlen) (rlen) ,body))
    (letk kassume
          ($kargs ('ulen) (ulen)
            ($continue k src
              ($primcall 'assume-u64 `(0 . ,(target-max-size-t)) (ulen)))))
    (letk ks
          ($kargs () ()
            ($continue kassume src
              ($primcall 'word-ref/immediate '(string . 3) (x)))))
    (letk kheap-object
          ($kargs () ()
            ($branch knot-string ks src 'string? #f (x))))
    (build-term
      ($branch knot-string kheap-object src 'heap-object? #f (x)))))

(define (ensure-char cps src op x have-char)
  (define msg "Wrong type argument (expecting char): ~S")
  (define not-char (vector 'wrong-type-arg (symbol->string op) msg))
  (with-cps cps
    (letv uchar)
    (letk knot-char
          ($kargs () () ($throw src 'throw/value+data not-char (x))))
    (let$ body (have-char uchar))
    (letk k ($kargs ('uchar) (uchar) ,body))
    (letk kchar
          ($kargs () () ($continue k src ($primcall 'untag-char #f (x)))))
    (build-term
      ($branch knot-char kchar src 'char? #f (x)))))

(define-primcall-converter string-length
  (lambda (cps k src op param x)
    (ensure-string
     cps src op x
     (lambda (cps ulen)
       (with-cps cps
         (build-term
           ($continue k src ($primcall 'u64->scm #f (ulen)))))))))

(define-primcall-converter string-ref
  (lambda (cps k src op param s idx)
    (define out-of-range
      #(out-of-range string-ref "Argument 2 out of range: ~S"))
    (define stringbuf-f-wide #x400)
    (ensure-string
     cps src op s
     (lambda (cps ulen)
       (with-cps cps
         (letv uidx start upos buf ptr tag mask bits uwpos u32 uchar)
         (letk kout-of-range
               ($kargs () ()
                 ($throw src 'throw/value+data out-of-range (idx))))
         (letk kchar
               ($kargs ('uchar) (uchar)
                 ($continue k src
                   ($primcall 'tag-char #f (uchar)))))
         (letk kassume
               ($kargs ('u32) (u32)
                 ($continue kchar src
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
                 ($continue kchar src
                   ($primcall 'u8-ref 'stringbuf (buf ptr upos)))))
         (letk kcmp
               ($kargs ('bits) (bits)
                 ($branch kwide knarrow src 'u64-imm-= 0 (bits))))
         (letk kmask
               ($kargs ('mask) (mask)
                 ($continue kcmp src
                   ($primcall 'ulogand #f (tag mask)))))
         (letk ktag
               ($kargs ('tag) (tag)
                 ($continue kmask src
                   ($primcall 'load-u64 stringbuf-f-wide ()))))
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
         (letk kstart
               ($kargs () ()
                 ($continue kadd src
                   ($primcall 'word-ref/immediate '(string . 2) (s)))))
         (letk krange
               ($kargs ('uidx) (uidx)
                 ($branch kout-of-range kstart src 'u64-< #f (uidx ulen))))
         (build-term
           ($continue krange src ($primcall 'scm->u64 #f (idx)))))))))

(define-primcall-converter string-set!
  (lambda (cps k src op param s idx ch)
    (define out-of-range
      #(out-of-range string-ref "Argument 2 out of range: ~S"))
    (define stringbuf-f-wide #x400)
    (ensure-string
     cps src op s
     (lambda (cps ulen)
       (ensure-char
        cps src op ch
        (lambda (cps uchar)
          (with-cps cps
            (letv uidx)
            (letk kout-of-range
                  ($kargs () ()
                    ($throw src 'throw/value+data out-of-range (idx))))
            (letk kuidx
                  ($kargs () ()
                    ($continue k src
                      ($primcall 'string-set! #f (s uidx uchar)))))
            (letk krange
                  ($kargs ('uidx) (uidx)
                    ($branch kout-of-range kuidx src 'u64-< #f (uidx ulen))))
            (build-term
              ($continue krange src ($primcall 'scm->u64 #f (idx)))))))))))

(define-primcall-converter integer->char
  (lambda (cps k src op param i)
    (define not-fixnum
      #(wrong-type-arg
        "integer->char"
        "Wrong type argument in position 1 (expecting small integer): ~S"))
    (define out-of-range
      #(out-of-range
        "integer->char"
        "Argument 1 out of range: ~S"))
    (define codepoint-surrogate-start #xd800)
    (define codepoint-surrogate-end #xdfff)
    (define codepoint-max #x10ffff)
    (with-cps cps
      (letv si ui)
      (letk knot-fixnum
            ($kargs () () ($throw src 'throw/value+data not-fixnum (i))))
      (letk kf
            ($kargs () () ($throw src 'throw/value+data out-of-range (i))))
      (letk ktag ($kargs ('ui) (ui)
                   ($continue k src ($primcall 'tag-char #f (ui)))))
      (letk kt ($kargs () ()
                 ($continue ktag src ($primcall 's64->u64 #f (si)))))
      (letk kmax
            ($kargs () ()
              ($branch kt kf src 'imm-s64-< codepoint-max (si))))
      (letk khi
            ($kargs () ()
              ($branch kf kmax src 'imm-s64-< codepoint-surrogate-end (si))))
      (letk klo
            ($kargs () ()
              ($branch khi kt src 's64-imm-< codepoint-surrogate-start (si))))
      (letk kbound0
            ($kargs ('si) (si)
              ($branch klo kf src 's64-imm-< 0 (si))))
      (letk kuntag
            ($kargs () ()
              ($continue kbound0 src ($primcall 'untag-fixnum #f (i)))))
      (build-term ($branch knot-fixnum kuntag src 'fixnum? #f (i))))))

(define-primcall-converter char->integer
  (lambda (cps k src op param ch)
    (define not-char
      #(wrong-type-arg
        "char->integer"
        "Wrong type argument in position 1 (expecting char): ~S"))
    (with-cps cps
      (letv ui si)
      (letk knot-char
            ($kargs () () ($throw src 'throw/value+data not-char (ch))))
      (letk ktag ($kargs ('si) (si)
                   ($continue k src ($primcall 'tag-fixnum #f (si)))))
      (letk kcvt ($kargs ('ui) (ui)
                   ($continue ktag src ($primcall 'u64->s64 #f (ui)))))
      (letk kuntag ($kargs () ()
                     ($continue kcvt src ($primcall 'untag-char #f (ch)))))
      (build-term
        ($branch knot-char kuntag src 'char? #f (ch))))))

(define (convert-shift cps k src op param obj idx)
  (with-cps cps
    (letv idx')
    (letk k' ($kargs ('idx) (idx')
               ($continue k src ($primcall op param (obj idx')))))
    (build-term ($continue k' src ($primcall 'scm->u64 #f (idx))))))

(define-primcall-converter rsh convert-shift)
(define-primcall-converter lsh convert-shift)

(define-primcall-converter make-atomic-box
  (lambda (cps k src op param val)
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
          ($primcall 'allocate-words/immediate '(atomic-box . 2) ()))))))

(define (ensure-atomic-box cps src op x is-atomic-box)
  (define bad-type
    (vector 'wrong-type-arg
            (symbol->string op)
            "Wrong type argument in position 1 (expecting atomic box): ~S"))
  (with-cps cps
    (letk kbad ($kargs () () ($throw src 'throw/value+data bad-type (x))))
    (let$ body (is-atomic-box))
    (letk k ($kargs () () ,body))
    (letk kheap-object ($kargs () () ($branch kbad k src 'atomic-box? #f (x))))
    (build-term ($branch kbad kheap-object src 'heap-object? #f (x)))))

(define-primcall-converter atomic-box-ref
  (lambda (cps k src op param x)
    (ensure-atomic-box
     cps src 'atomic-box-ref x
     (lambda (cps)
       (with-cps cps
         (letv val)
         (build-term
           ($continue k src
             ($primcall 'atomic-scm-ref/immediate '(atomic-box . 1) (x)))))))))

(define-primcall-converter atomic-box-set!
  (lambda (cps k src op param x val)
    (ensure-atomic-box
     cps src 'atomic-box-set! x
     (lambda (cps)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'atomic-scm-set!/immediate '(atomic-box . 1)
                        (x val)))))))))

(define-primcall-converter atomic-box-swap!
  (lambda (cps k src op param x val)
    (ensure-atomic-box
     cps src 'atomic-box-swap! x
     (lambda (cps)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'atomic-scm-swap!/immediate '(atomic-box . 1)
                        (x val)))))))))

(define-primcall-converter atomic-box-compare-and-swap!
  (lambda (cps k src op param x expected desired)
    (ensure-atomic-box
     cps src 'atomic-box-compare-and-swap! x
     (lambda (cps)
       (with-cps cps
         (build-term
           ($continue k src
             ($primcall 'atomic-scm-compare-and-swap!/immediate '(atomic-box . 1)
                        (x expected desired)))))))))

;;; Guile's semantics are that a toplevel lambda captures a reference on
;;; the current module, and that all contained lambdas use that module
;;; to resolve toplevel variables.  This parameter tracks whether or not
;;; we are in a toplevel lambda.  If we are in a lambda, the parameter
;;; is bound to a fresh name identifying the module that was current
;;; when the toplevel lambda is defined.
;;;
;;; This is more complicated than it need be.  Ideally we should resolve
;;; all toplevel bindings to bindings from specific modules, unless the
;;; binding is unbound.  This is always valid if the compilation unit
;;; sets the module explicitly, as when compiling a module, but it
;;; doesn't work for files auto-compiled for use with `load'.
;;;
(define current-topbox-scope (make-parameter #f))
(define scope-counter (make-parameter #f))

(define (fresh-scope-id)
  (let ((scope-id (scope-counter)))
    (scope-counter (1+ scope-id))
    scope-id))

;;; For calls to known imported values, we don't want to duplicate the
;;; "resolve the import" code at each call site.  Instead we generate a
;;; stub per callee, and have callers call-label the callees.
;;;
(define module-call-stubs (make-parameter #f))
(define (module-call-label cps mod name public? nargs)
  "Return three values: the new CPS, the label to call, and the value to
use as the proc slot."
  (define call-stub-key (list mod name public? nargs))
  (define var-cache-key (list mod name public?))
  (define var-cache
    (build-exp ($primcall 'cache-ref var-cache-key ())))
  (match (assoc-ref (module-call-stubs) call-stub-key)
    (#f
     (let* ((trampoline-name (string->symbol
                              (format #f "~a~a~a"
                                      name (if public? "@" "@@")
                                      (string-join (map symbol->string mod)
                                                   "/"))))
            (cached (fresh-var))
            (args (let lp ((n 0))
                    (if (< n nargs)
                        (cons (fresh-var) (lp (1+ n)))
                        '())))
            (argv (cons cached args))
            (names (let lp ((n 0))
                     (if (< n (1+ nargs))
                         (cons (string->symbol
                                (string-append "arg" (number->string n)))
                               (lp (1+ n)))
                         '()))))
       (with-cps cps
         (letv fresh-var var proc)
         (letk ktail ($ktail))
         (letk kcall
               ($kargs ('proc) (proc)
                 ($continue ktail #f ($call proc args))))
         (letk kref
               ($kargs ('var) (var)
                 ($continue kcall #f
                   ($primcall 'scm-ref/immediate '(box . 1) (var)))))
         (letk kcache2
               ($kargs () ()
                 ($continue kref #f ($values (fresh-var)))))
         (letk kcache
               ($kargs ('var) (fresh-var)
                 ($continue kcache2 #f
                   ($primcall 'cache-set! var-cache-key (fresh-var)))))
         (letk klookup
               ($kargs () ()
                 ($continue kcache #f
                   ($primcall (if public?
                                  'lookup-bound-public
                                  'lookup-bound-private)
                              (list mod name) ()))))
         (letk kcached
               ($kargs () ()
                 ($continue kref #f ($values (cached)))))
         (letk kentry
               ($kargs names argv
                 ($branch klookup kcached #f 'heap-object? #f (cached))))
         (letk kfun ($kfun #f `((name . ,trampoline-name)) #f ktail kentry))
         ($ ((lambda (cps)
               (module-call-stubs
                (acons call-stub-key kfun (module-call-stubs)))
               (values cps kfun var-cache)))))))
    (kfun
     (values cps kfun var-cache))))

(define (toplevel-box cps src name bound? have-var)
  (match (current-topbox-scope)
    (#f
     (with-cps cps
       (letv mod name-var box)
       (let$ body (have-var box))
       (letk kbox ($kargs ('box) (box) ,body))
       (letk kname ($kargs ('name) (name-var)
                     ($continue kbox src
                       ($primcall (if bound? 'lookup-bound 'lookup) #f
                                  (mod name-var)))))
       (letk kmod ($kargs ('mod) (mod)
                    ($continue kname src ($const name))))
       (build-term
         ($continue kmod src ($primcall 'current-module #f ())))))
    (scope
     (with-cps cps
       (letv box)
       (let$ body (have-var box))
       (letk kbox ($kargs ('box) (box) ,body))
       ($ (convert-primcall kbox src 'cached-toplevel-box
                            (list scope name bound?)))))))

(define (module-box cps src module name public? bound? val-proc)
  (with-cps cps
    (letv box)
    (let$ body (val-proc box))
    (letk kbox ($kargs ('box) (box) ,body))
    ($ (convert-primcall kbox src 'cached-module-box
                         (list module name public? bound?)))))

(define (capture-toplevel-scope cps src scope-id k)
  (with-cps cps
    (letv module)
    (let$ body (convert-primcall k src 'cache-current-module!
                                 (list scope-id) module))
    (letk kmodule ($kargs ('module) (module) ,body))
    ($ (convert-primcall kmodule src 'current-module #f))))

(define (fold-formals proc seed arity gensyms inits)
  (match arity
    (($ $arity req opt rest kw allow-other-keys?)
     (let ()
       (define (fold-req names gensyms seed)
         (match names
           (() (fold-opt opt gensyms inits seed))
           ((name . names)
            (proc name (car gensyms) #f
                  (fold-req names (cdr gensyms) seed)))))
       (define (fold-opt names gensyms inits seed)
         (match names
           (() (fold-rest rest gensyms inits seed))
           ((name . names)
            (proc name (car gensyms) (car inits)
                  (fold-opt names (cdr gensyms) (cdr inits) seed)))))
       (define (fold-rest rest gensyms inits seed)
         (match rest
           (#f (fold-kw kw gensyms inits seed))
           (name (proc name (car gensyms) #f
                       (fold-kw kw (cdr gensyms) inits seed)))))
       (define (fold-kw kw gensyms inits seed)
         (match kw
           (()
            (unless (null? gensyms)
              (error "too many gensyms"))
            (unless (null? inits)
              (error "too many inits"))
            seed)
           (((key name var) . kw)
            ;; Could be that var is not a gensym any more.
            (when (symbol? var)
              (unless (eq? var (car gensyms))
                (error "unexpected keyword arg order")))
            (proc name (car gensyms) (car inits)
                  (fold-kw kw (cdr gensyms) (cdr inits) seed)))))
       (fold-req req gensyms seed)))))

(define (init-default-value cps name sym subst init body)
  (match (hashq-ref subst sym)
    ((orig-var subst-var box?)
     (let ((src (tree-il-src init)))
       (define (maybe-box cps k make-body)
         (if box?
             (with-cps cps
               (letv phi)
               (let$ body (convert-primcall k src 'box #f phi))
               (letk kbox ($kargs (name) (phi) ,body))
               ($ (make-body kbox)))
             (make-body cps k)))
       (with-cps cps
         (letk knext ($kargs (name) (subst-var) ,body))
         ($ (maybe-box
             knext
             (lambda (cps k)
               (with-cps cps
                 (letk kbound ($kargs () () ($continue k src
                                              ($values (orig-var)))))
                 (letv val rest)
                 (letk krest ($kargs (name 'rest) (val rest)
                               ($continue k src ($values (val)))))
                 (letk kreceive ($kreceive (list name) 'rest krest))
                 (let$ init (convert init kreceive subst))
                 (letk kunbound ($kargs () () ,init))
                 (build-term
                   ($branch kbound kunbound src
                     'undefined? #f (orig-var))))))))))))

(define (build-list cps k src vals)
  (match vals
    (()
     (with-cps cps
       (build-term ($continue k src ($const '())))))
    ((v . vals)
     (with-cps cps
       (letv tail)
       (let$ head (convert-primcall k src 'cons #f v tail))
       (letk ktail ($kargs ('tail) (tail) ,head))
       ($ (build-list ktail src vals))))))

(define (sanitize-meta meta)
  (match meta
    (() '())
    (((k . v) . meta)
     (let ((meta (sanitize-meta meta)))
       (case k
         ((arg-representations noreturn return-type) meta)
         (else (acons k v meta)))))))

;;; The conversion from Tree-IL to CPS essentially wraps every
;;; expression in a $kreceive, which models the Tree-IL semantics that
;;; extra values are simply truncated.  In CPS, this means that the
;;; $kreceive has a rest argument after the required arguments, if any,
;;; and that the rest argument is unused.
;;;
;;; All CPS expressions that can return a variable number of values
;;; (i.e., $call and $abort) must continue to $kreceive, which checks
;;; the return arity and on success passes the parsed values along to a
;;; $kargs.  If the $call or $abort is in tail position they continue to
;;; $ktail instead, and then the values are parsed by the $kreceive of
;;; the non-tail caller.
;;;
;;; Other CPS terms like $values, $const, and the like all have a
;;; specific return arity, and must continue to $kargs instead of
;;; $kreceive or $ktail.  This allows the compiler to reason precisely
;;; about their result values.  To make sure that this is the case,
;;; whenever the CPS conversion would reify one of these terms it needs
;;; to ensure that the continuation actually accepts the return arity of
;;; the primcall.
;;;
;;; Some Tree-IL primcalls residualize CPS primcalls that return zero
;;; values, for example box-set!.  In this case the Tree-IL semantics
;;; are that the result of the expression is the undefined value.  That
;;; is to say, the result of this expression is #t:
;;;
;;;   (let ((x 30)) (eq? (set! x 10) (if #f #f)))
;;;
;;; So in the case that the continuation expects a value but the
;;; primcall produces zero values, we insert the "unspecified" value.
;;;
(define (adapt-arity cps k src nvals)
  (match nvals
    (0
     ;; As mentioned above, in the Tree-IL semantics the primcall
     ;; produces the unspecified value, but in CPS it produces no
     ;; values.  Therefore we plug the unspecified value into the
     ;; continuation.
     (match (intmap-ref cps k)
       (($ $ktail)
        (with-cps cps
          (let$ body (with-cps-constants ((unspecified *unspecified*))
                       (build-term
                         ($continue k src ($values (unspecified))))))
          (letk kvoid ($kargs () () ,body))
          kvoid))
       (($ $kargs ()) (with-cps cps k))
       (($ $kreceive arity kargs)
        (match arity
          (($ $arity () () (not #f) () #f)
           (with-cps cps
             (letk kvoid ($kargs () () ($continue kargs src ($const '()))))
             kvoid))
          (($ $arity (_) () #f () #f)
           (with-cps cps
             (letk kvoid ($kargs () ()
                           ($continue kargs src ($const *unspecified*))))
             kvoid))
          (($ $arity (_) () _ () #f)
           (with-cps cps
             (let$ void (with-cps-constants ((unspecified *unspecified*)
                                             (rest '()))
                          (build-term
                            ($continue kargs src
                              ($values (unspecified rest))))))
             (letk kvoid ($kargs () () ,void))
             kvoid))
          (_
           ;; Arity mismatch.  Serialize a values call.
           (with-cps cps
             (letv values)
             (let$ void (with-cps-constants ((unspecified *unspecified*))
                          (build-term
                            ($continue k src
                              ($call values (unspecified))))))
             (letk kvoid ($kargs ('values) (values) ,void))
             (letk kvalues ($kargs () ()
                             ($continue kvoid src ($prim 'values))))
             kvalues))))))
    (1
     (match (intmap-ref cps k)
       (($ $ktail)
        (with-cps cps
          (letv val)
          (letk kval ($kargs ('val) (val)
                       ($continue k src ($values (val)))))
          kval))
       (($ $kargs (_)) (with-cps cps k))
       (($ $kreceive arity kargs)
        (match arity
          (($ $arity () () (not #f) () #f)
           (with-cps cps
             (letv val)
             (let$ body (with-cps-constants ((nil '()))
                          ($ (convert-primcall kargs src 'cons #f
                                               val nil))))
             (letk kval ($kargs ('val) (val) ,body))
             kval))
          (($ $arity (_) () #f () #f)
           (with-cps cps
             kargs))
          (($ $arity (_) () _ () #f)
           (with-cps cps
             (letv val)
             (let$ body (with-cps-constants ((rest '()))
                          (build-term
                            ($continue kargs src ($values (val rest))))))
             (letk kval ($kargs ('val) (val) ,body))
             kval))
          (_
           ;; Arity mismatch.  Serialize a values call.
           (with-cps cps
             (letv val values)
             (letk kvalues ($kargs ('values) (values)
                             ($continue k src
                               ($call values (val)))))
             (letk kval ($kargs ('val) (val)
                          ($continue kvalues src ($prim 'values))))
             kval))))))))

;; cps exp k-name alist -> cps term
(define (convert cps exp k subst)
  (define (zero-valued? exp)
    (match exp
      ((or ($ <module-set>) ($ <toplevel-set>) ($ <toplevel-define>)
           ($ <lexical-set>))
       #t)
      (($ <let> src names syms vals body) (zero-valued? body))
      ;; Can't use <fix> here as the hack that <fix> uses to convert its
      ;; functions relies on continuation being single-valued.
      ;; (($ <fix> src names syms vals body) (zero-valued? body))
      (($ <let-values> src exp body) (zero-valued? body))
      (($ <seq> src head tail) (zero-valued? tail))
      (($ <primcall> src 'values args) (= (length args) 0))
      (($ <primcall> src name args)
       (match (tree-il-primitive->cps-primitive+nargs+nvalues name)
         (#f #f)
         (#(cps-prim nargs nvalues)
          (and (eqv? nvalues 0)
               (eqv? nargs (length args))))))
      (_ #f)))
  (define (single-valued? exp)
    (match exp
      ((or ($ <void>) ($ <const>) ($ <primitive-ref>) ($ <module-ref>)
           ($ <toplevel-ref>) ($ <lambda>))
       #t)
      (($ <let> src names syms vals body) (single-valued? body))
      (($ <fix> src names syms vals body) (single-valued? body))
      (($ <let-values> src exp body) (single-valued? body))
      (($ <seq> src head tail) (single-valued? tail))
      (($ <primcall> src 'values args) (= (length args) 1))
      (($ <primcall> src name args)
       (match (tree-il-primitive->cps-primitive+nargs+nvalues name)
         (#f #f)
         (#(cps-prim nargs nvalues)
          (and (eqv? nvalues 1)
               (eqv? nargs (length args))))))
      (_ #f)))
  ;; exp (v-name -> term) -> term
  (define (convert-arg cps exp k)
    (match exp
      (($ <lexical-ref> src name sym)
       (match (hashq-ref subst sym)
         ((orig-var box #t)
          (with-cps cps
            (letv unboxed)
            (let$ body (k unboxed))
            (letk kunboxed ($kargs ('unboxed) (unboxed) ,body))
            (build-term ($continue kunboxed src
                          ($primcall 'scm-ref/immediate '(box . 1) (box))))))
         ((orig-var subst-var #f) (k cps subst-var))
         (var (k cps var))))
      ((? single-valued?)
       (with-cps cps
         (letv arg)
         (let$ body (k arg))
         (letk karg ($kargs ('arg) (arg) ,body))
         ($ (convert exp karg subst))))
      (_
       (with-cps cps
         (letv arg rest)
         (let$ body (k arg))
         (letk karg ($kargs ('arg 'rest) (arg rest) ,body))
         (letk kreceive ($kreceive '(arg) 'rest karg))
         ($ (convert exp kreceive subst))))))
  ;; (exp ...) ((v-name ...) -> term) -> term
  (define (convert-args cps exps k)
    (match exps
      (() (k cps '()))
      ((exp . exps)
       (convert-arg cps exp
         (lambda (cps name)
           (convert-args cps exps
             (lambda (cps names)
               (k cps (cons name names)))))))))
  (define (box-bound-var cps name sym body)
    (match (hashq-ref subst sym)
      ((orig-var subst-var #t)
       (with-cps cps
         (letk k ($kargs (name) (subst-var) ,body))
         ($ (convert-primcall k #f 'box #f orig-var))))
      (else
       (with-cps cps body))))
  (define (box-bound-vars cps names syms body)
    (match (vector names syms)
      (#((name . names) (sym . syms))
       (with-cps cps
         (let$ body (box-bound-var name sym body))
         ($ (box-bound-vars names syms body))))
      (#(() ()) (with-cps cps body))))
  (define (bound-var sym)
    (match (hashq-ref subst sym)
      ((var . _) var)
      ((? exact-integer? var) var)))

  (match exp
    (($ <lexical-ref> src name sym)
     (with-cps cps
       (let$ k (adapt-arity k src 1))
       (rewrite-term (hashq-ref subst sym)
         ((orig-var box #t) ($continue k src
                              ($primcall 'scm-ref/immediate '(box . 1) (box))))
         ((orig-var subst-var #f) ($continue k src ($values (subst-var))))
         (var ($continue k src ($values (var)))))))

    (($ <void> src)
     (with-cps cps
       (let$ k (adapt-arity k src 1))
       (build-term ($continue k src ($const *unspecified*)))))

    (($ <const> src exp)
     (with-cps cps
       (let$ k (adapt-arity k src 1))
       (build-term ($continue k src ($const exp)))))

    (($ <primitive-ref> src name)
     (with-cps cps
       (let$ k (adapt-arity k src 1))
       (build-term ($continue k src ($prim name)))))

    (($ <lambda> fun-src meta body)
     (let ()
       (define (convert-clauses cps body ktail)
         (match body
           (#f (values cps #f))
           (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
            (let* ((arity (make-$arity req (or opt '()) rest
                                       (map (match-lambda
                                              ((kw name sym) 
                                               (list kw name (bound-var sym))))
                                            (if kw (cdr kw) '()))
                                       (and kw (car kw))))
                   (names (fold-formals (lambda (name sym init names)
                                          (cons name names))
                                        '()
                                        arity gensyms inits)))
              (define (fold-formals* cps f seed arity gensyms inits)
                (match (fold-formals
                        (lambda (name sym init cps+seed)
                          (match cps+seed
                            ((cps . seed)
                             (call-with-values (lambda ()
                                                 (f cps name sym init seed))
                               (lambda (cps seed) (cons cps seed))))))
                        (cons cps seed) arity gensyms inits)
                  ((cps . seed) (values cps seed))))
              (with-cps cps
                (let$ kalt (convert-clauses alternate ktail))
                (let$ body (convert body ktail subst))
                (let$ body
                      (fold-formals*
                       (lambda (cps name sym init body)
                         (if init
                             (init-default-value cps name sym subst init body)
                             (box-bound-var cps name sym body)))
                       body arity gensyms inits))
                (letk kargs ($kargs names (map bound-var gensyms) ,body))
                (letk kclause ($kclause ,arity kargs kalt))
                kclause)))))
       (if (current-topbox-scope)
           (with-cps cps
             (letv self)
             (letk ktail ($ktail))
             (let$ kclause (convert-clauses body ktail))
             (letk kfun ($kfun fun-src (sanitize-meta meta) self ktail kclause))
             (let$ k (adapt-arity k fun-src 1))
             (build-term ($continue k fun-src ($fun kfun))))
           (let ((scope-id (fresh-scope-id)))
             (with-cps cps
               (let$ body ((lambda (cps)
                             (parameterize ((current-topbox-scope scope-id))
                               (convert cps exp k subst)))))
               (letk kscope ($kargs () () ,body))
               ($ (capture-toplevel-scope fun-src scope-id kscope)))))))

    (($ <module-ref> src mod name public?)
     (module-box
      cps src mod name public? #t
      (lambda (cps box)
        (with-cps cps
          (let$ k (adapt-arity k src 1))
          (build-term ($continue k src
                        ($primcall 'scm-ref/immediate '(box . 1) (box))))))))

    (($ <module-set> src mod name public? exp)
     (convert-arg cps exp
       (lambda (cps val)
         (module-box
          cps src mod name public? #t
          (lambda (cps box)
            (with-cps cps
              (let$ k (adapt-arity k src 0))
              (build-term
                ($continue k src
                  ($primcall 'scm-set!/immediate '(box . 1) (box val))))))))))

    (($ <toplevel-ref> src mod name)
     (toplevel-box
      cps src name #t
      (lambda (cps box)
        (with-cps cps
          (let$ k (adapt-arity k src 1))
          (build-term
            ($continue k src
              ($primcall 'scm-ref/immediate '(box . 1) (box))))))))

    (($ <toplevel-set> src mod name exp)
     (convert-arg cps exp
       (lambda (cps val)
         (toplevel-box
          cps src name #f
          (lambda (cps box)
            (with-cps cps
              (let$ k (adapt-arity k src 0))
              (build-term
                ($continue k src
                  ($primcall 'scm-set!/immediate '(box . 1) (box val))))))))))

    (($ <toplevel-define> src modname name exp)
     (convert-arg cps exp
       (lambda (cps val)
         (with-cps cps
           (let$ k (adapt-arity k src 0))
           (letv box mod)
           (letk kset ($kargs ('box) (box)
                        ($continue k src
                          ($primcall 'scm-set!/immediate '(box . 1) (box val)))))
           ($ (with-cps-constants ((name name))
                (letk kmod
                      ($kargs ('mod) (mod)
                        ($continue kset src
                          ($primcall 'define! #f (mod name)))))
                (build-term
                  ($continue kmod src ($primcall 'current-module #f ())))))))))

    (($ <call> src ($ <module-ref> src2 mod name public?) args)
     (convert-args cps args
       (lambda (cps args)
         (call-with-values
             (lambda () (module-call-label cps mod name public? (length args)))
           (lambda (cps kfun proc-exp)
             (with-cps cps
               (letv cache)
               (letk kcall ($kargs ('cache) (cache)
                             ($continue k src
                               ($callk kfun #f ,(cons cache args)))))
               (build-term
                 ($continue kcall src2 ,proc-exp))))))))

    (($ <call> src proc args)
     (convert-args cps (cons proc args)
       (match-lambda*
         ((cps (proc . args))
          (with-cps cps
            (build-term ($continue k src ($call proc args))))))))

    (($ <primcall> src name args)
     (cond
      ((eq? name 'throw)
       (let ()
         (define (fallback)
           (convert-args cps args
             (lambda (cps args)
               (match args
                 ((key . args)
                  (with-cps cps
                    (letv arglist)
                    (letk kargs ($kargs ('arglist) (arglist)
                                  ($throw src 'throw #f (key arglist))))
                    ($ (build-list kargs src args))))))))
         (define (specialize op param . args)
           (convert-args cps args
             (lambda (cps args)
               (with-cps cps
                 (build-term
                   ($throw src op param args))))))
         (match args
           ((($ <const> _ key) ($ <const> _ subr) ($ <const> _ msg) args data)
            ;; Specialize `throw' invocations corresponding to common
            ;; "error" invocations.
            (let ()
              (match (vector args data)
                (#(($ <primcall> _ 'cons (x ($ <const> _ ())))
                   ($ <primcall> _ 'cons (x ($ <const> _ ()))))
                 (specialize 'throw/value+data `#(,key ,subr ,msg) x))
                (#(($ <primcall> _ 'cons (x ($ <const> _ ()))) ($ <const> _ #f))
                 (specialize 'throw/value `#(,key ,subr ,msg) x))
                (_ (fallback)))))
           (_ (fallback)))))
      ((eq? name 'values)
       (convert-args cps args
         (lambda (cps args)
           (match (intmap-ref cps k)
             (($ $ktail)
              (with-cps cps
                (build-term
                  ($continue k src ($values args)))))
             (($ $kargs names)
              ;; Can happen if continuation already saw we produced the
              ;; right number of values.
              (with-cps cps
                (build-term
                  ($continue k src ($values args)))))
             (($ $kreceive ($ $arity req () rest () #f) kargs)
              (cond
               ((and (not rest) (= (length args) (length req)))
                (with-cps cps
                  (build-term
                    ($continue kargs src ($values args)))))
               ((and rest (>= (length args) (length req)))
                (with-cps cps
                  (letv rest)
                  (letk krest ($kargs ('rest) (rest)
                                ($continue kargs src
                                  ($values ,(append (list-head args (length req))
                                                    (list rest))))))
                  ($ (build-list krest src (list-tail args (length req))))))
               (else
                ;; Number of values mismatch; reify a values call.
                (with-cps cps
                  (letv val values)
                  (letk kvalues ($kargs ('values) (values)
                                  ($continue k src ($call values args))))
                  (build-term ($continue kvalues src ($prim 'values)))))))))))
      ((tree-il-primitive->cps-primitive+nargs+nvalues name)
       =>
       (match-lambda
        (#(cps-prim nargs nvalues)
         (define (cvt cps k src op args)
           (define (default)
             (convert-args cps args
               (lambda (cps args)
                 (with-cps cps
                   ($ (convert-primcall* k src op #f args))))))
           (define-syntax-rule (specialize-case (pat (op c (arg ...))) ...
                                                (_ def))
             (match (cons cps-prim args)
               (pat
                (convert-args cps (list arg ...)
                  (lambda (cps args)
                    (with-cps cps
                      ($ (convert-primcall* k src 'op c args))))))
               ...
               (_ def)))
           (define (uint? val) (and (exact-integer? val) (<= 0 val)))
           (define (vector-index? val)
             (and (exact-integer? val)
                  (<= 0 val (1- (target-max-vector-length)))))
           (define (vector-size? val)
             (and (exact-integer? val)
                  (<= 0 val (target-max-vector-length))))
           (define (negint? val) (and (exact-integer? val) (< val 0)))
           ;; FIXME: Add case for mul
           (specialize-case
            (('allocate-vector ($ <const> _ n))
             (allocate-vector n ()))
            (('make-vector ($ <const> _ (? vector-size? n)) init)
             (make-vector/immediate n (init)))
            (('vector-ref v ($ <const> _ (? vector-index? n)))
             (vector-ref/immediate n (v)))
            (('vector-set! v ($ <const> _ (? vector-index? n)) x)
             (vector-set!/immediate n (v x)))
            (('vector-init! v ($ <const> _ n) x)
             (vector-init! n (v x)))
            (('allocate-struct v ($ <const> _ n))
             (allocate-struct n (v)))
            (('struct-ref s ($ <const> _ (? uint? n)))
             (struct-ref/immediate n (s)))
            (('struct-set! s ($ <const> _ (? uint? n)) x)
             (struct-set!/immediate n (s x)))
            (('struct-init! s ($ <const> _ n) x)
             (struct-init! n (s x)))
            (('add x ($ <const> _ (? number? y)))
             (add/immediate y (x)))
            (('add ($ <const> _ (? number? y)) x)
             (add/immediate y (x)))
            (('sub x ($ <const> _ (? number? y)))
             (sub/immediate y (x)))
            (('lsh x ($ <const> _ (? uint? y)))
             (lsh/immediate y (x)))
            (('rsh x ($ <const> _ (? uint? y)))
             (rsh/immediate y (x)))
            (_
             (default))))
         ;; Tree-IL primcalls are sloppy, in that it could be that
         ;; they are called with too many or too few arguments.  In
         ;; CPS we are more strict and only residualize a $primcall
         ;; if the argument count matches.
         (if (= nargs (length args))
             (with-cps cps
               (let$ k (adapt-arity k src nvalues))
               ($ (cvt k src cps-prim args)))
             (convert-args cps args
               (lambda (cps args)
                 (with-cps cps
                   (letv prim)
                   (letk kprim ($kargs ('prim) (prim)
                                 ($continue k src ($call prim args))))
                   (build-term ($continue kprim src ($prim name))))))))))
      (else
       ;; We have something that's a primcall for Tree-IL but not for
       ;; CPS; compile as a call.
       (convert-args cps args
         (lambda (cps args)
           (with-cps cps
             (letv prim)
             (letk kprim ($kargs ('prim) (prim)
                           ($continue k src ($call prim args))))
             (build-term ($continue kprim src ($prim name)))))))))

    ;; Prompts with inline handlers.
    (($ <prompt> src escape-only? tag body
        ($ <lambda> hsrc hmeta
           ($ <lambda-case> _ hreq #f hrest #f () hsyms hbody #f)))
     ;; Handler:
     ;;   khargs: check args returned to handler, -> khbody
     ;;   khbody: the handler, -> k
     ;;
     ;; Post-body:
     ;;   krest: collect return vals from body to list, -> kpop
     ;;   kpop: pop the prompt, -> kprim
     ;;   kprim: load the values primitive, -> kret
     ;;   kret: (apply values rvals), -> k
     ;;
     ;; Escape prompts evaluate the body with the continuation of krest.
     ;; Otherwise we do a no-inline call to body, continuing to krest.
     (convert-arg cps tag
       (lambda (cps tag)
         (let ((hnames (append hreq (if hrest (list hrest) '())))
               (bound-vars (map bound-var hsyms)))
           (define (convert-body cps khargs krest)
             (if escape-only?
                 (with-cps cps
                   (let$ body (convert body krest subst))
                   (letk kbody ($kargs () () ,body))
                   (build-term ($prompt kbody khargs src #t tag)))
                 (convert-arg cps body
                   (lambda (cps thunk)
                     (with-cps cps
                       (letk kbody ($kargs () ()
                                     ($continue krest (tree-il-src body)
                                       ($primcall 'call-thunk/no-inline #f
                                                  (thunk)))))
                       (build-term ($prompt kbody khargs (tree-il-src body)
                                     #f tag)))))))
           (with-cps cps
             (letv prim vals apply)
             (let$ hbody (convert hbody k subst))
             (let$ hbody (box-bound-vars hnames hsyms hbody))
             (letk khbody ($kargs hnames bound-vars ,hbody))
             (letk khargs ($kreceive hreq hrest khbody))
             (letk kapp ($kargs ('apply) (apply)
                          ($continue k src ($call apply (prim vals)))))
             (letk kprim ($kargs ('prim) (prim)
                           ($continue kapp src ($prim 'apply))))
             (letk kret ($kargs () ()
                          ($continue kprim src ($prim 'values))))
             (letk kpop ($kargs ('rest) (vals)
                          ($continue kret src ($primcall 'unwind #f ()))))
             ;; FIXME: Attach hsrc to $kreceive.
             (letk krest ($kreceive '() 'rest kpop))
             ($ (convert-body khargs krest)))))))

    (($ <abort> src tag args ($ <const> _ ()))
     (convert-args cps (cons tag args)
       (lambda (cps args*)
         (with-cps cps
           (letv abort)
           (letk kabort ($kargs ('abort) (abort)
                          ($continue k src ($call abort args*))))
           (build-term
             ($continue kabort src ($prim 'abort-to-prompt)))))))

    (($ <abort> src tag args tail)
     (convert-args cps
         (append (list (make-primitive-ref #f 'apply)
                       (make-primitive-ref #f 'abort-to-prompt)
                       tag)
                 args
                 (list tail))
       (lambda (cps args*)
         (match args*
           ((apply . apply-args)
            (with-cps cps
              (build-term ($continue k src ($call apply apply-args)))))))))

    (($ <conditional> src test consequent alternate)
     (define (convert-test cps test kt kf)
       (match test
         (($ <primcall> src 'eq? (a ($ <const> _ b)))
          (convert-arg cps a
            (lambda (cps a)
              (with-cps cps
                (build-term ($branch kf kt src 'eq-constant? b (a)))))))

         (($ <primcall> src 'eq? (($ <const> _ a) b))
          (convert-arg cps b
            (lambda (cps b)
              (with-cps cps
                (build-term ($branch kf kt src 'eq-constant? a (b)))))))

         (($ <primcall> src (? branching-primitive? name) args)
          (convert-args cps args
            (lambda (cps args)
              (if (heap-type-predicate? name)
                  (with-cps cps
                    (letk kt* ($kargs () ()
                                ($branch kf kt src name #f args)))
                    (build-term
                      ($branch kf kt* src 'heap-object? #f args)))
                  (with-cps cps
                    (build-term ($branch kf kt src name #f args)))))))
         (($ <conditional> src test consequent alternate)
          (with-cps cps
            (let$ t (convert-test consequent kt kf))
            (let$ f (convert-test alternate kt kf))
            (letk kt* ($kargs () () ,t))
            (letk kf* ($kargs () () ,f))
            ($ (convert-test test kt* kf*))))
         (($ <const> src c)
          (with-cps cps
            (build-term ($continue (if c kt kf) src ($values ())))))
         (_ (convert-arg cps test
              (lambda (cps test)
                (with-cps cps
                  (build-term ($branch kt kf src 'false? #f (test)))))))))
     (with-cps cps
       (let$ t (convert consequent k subst))
       (let$ f (convert alternate k subst))
       (letk kt ($kargs () () ,t))
       (letk kf ($kargs () () ,f))
       ($ (convert-test test kt kf))))

    (($ <lexical-set> src name gensym exp)
     (convert-arg cps exp
       (lambda (cps exp)
         (match (hashq-ref subst gensym)
           ((orig-var box #t)
            (with-cps cps
              (let$ k (adapt-arity k src 0))
              (build-term
                ($continue k src
                  ($primcall 'scm-set!/immediate '(box . 1) (box exp))))))))))

    (($ <seq> src head tail)
     (if (zero-valued? head)
         (with-cps cps
           (let$ tail (convert tail k subst))
           (letk kseq ($kargs () () ,tail))
           ($ (convert head kseq subst)))
         (with-cps cps
           (let$ tail (convert tail k subst))
           (letv vals)
           (letk kseq ($kargs ('vals) (vals) ,tail))
           (letk kreceive ($kreceive '() 'vals kseq))
           ($ (convert head kreceive subst)))))

    (($ <let> src names syms vals body)
     (let lp ((cps cps) (names names) (syms syms) (vals vals))
       (match (list names syms vals)
         ((() () ()) (convert cps body k subst))
         (((name . names) (sym . syms) (val . vals))
          (with-cps cps
            (let$ body (lp names syms vals))
            (let$ body (box-bound-var name sym body))
            ($ ((lambda (cps)
                  (if (single-valued? val)
                      (with-cps cps
                        (letk klet ($kargs (name) ((bound-var sym)) ,body))
                        ($ (convert val klet subst)))
                      (with-cps cps
                        (letv rest)
                        (letk klet ($kargs (name 'rest) ((bound-var sym) rest) ,body))
                        (letk kreceive ($kreceive (list name) 'rest klet))
                        ($ (convert val kreceive subst))))))))))))

    (($ <fix> src names gensyms funs body)
     ;; Some letrecs can be contified; that happens later.
     (define (convert-funs cps funs)
       (match funs
         (()
          (with-cps cps '()))
         ((fun . funs)
          (with-cps cps
            (let$ fun (convert fun k subst))
            (let$ funs (convert-funs funs))
            (cons (match fun
                    (($ $continue _ _ (and fun ($ $fun)))
                     fun))
                  funs)))))
     (if (current-topbox-scope)
         (let ((vars (map bound-var gensyms)))
           (with-cps cps
             (let$ body (convert body k subst))
             (letk krec ($kargs names vars ,body))
             (let$ funs (convert-funs funs))
             (build-term ($continue krec src ($rec names vars funs)))))
         (let ((scope-id (fresh-scope-id)))
           (with-cps cps
             (let$ body ((lambda (cps)
                           (parameterize ((current-topbox-scope scope-id))
                             (convert cps exp k subst)))))
             (letk kscope ($kargs () () ,body))
             ($ (capture-toplevel-scope src scope-id kscope))))))

    (($ <let-values> src exp
        ($ <lambda-case> lsrc req #f rest #f () syms body #f))
     (let ((names (append req (if rest (list rest) '())))
           (bound-vars (map bound-var syms)))
       (with-cps cps
         (let$ body (convert body k subst))
         (let$ body (box-bound-vars names syms body))
         (letk kargs ($kargs names bound-vars ,body))
         (letk kreceive ($kreceive req rest kargs))
         ($ (convert exp kreceive subst)))))))

(define (build-subst exp)
  "Compute a mapping from lexical gensyms to CPS variable indexes.  CPS
uses small integers to identify variables, instead of gensyms.

This subst table serves an additional purpose of mapping variables to
replacements.  The usual reason to replace one variable by another is
assignment conversion.  Default argument values is the other reason.

The result is a hash table mapping symbols to substitutions (in the case
that a variable is substituted) or to indexes.  A substitution is a list
of the form:

  (ORIG-INDEX SUBST-INDEX BOXED?)

A true value for BOXED?  indicates that the replacement variable is in a
box.  If a variable is not substituted, the mapped value is a small
integer."
  (let ((table (make-hash-table)))
    (define (down exp)
      (match exp
        (($ <lexical-set> src name sym exp)
         (match (hashq-ref table sym)
           ((orig subst #t) #t)
           ((orig subst #f) (hashq-set! table sym (list orig subst #t)))
           ((? number? idx) (hashq-set! table sym (list idx (fresh-var) #t)))))
        (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
         (fold-formals (lambda (name sym init seed)
                         (hashq-set! table sym
                                     (if init
                                         (list (fresh-var) (fresh-var) #f)
                                         (fresh-var))))
                       #f
                       (make-$arity req (or opt '()) rest
                                    (if kw (cdr kw) '()) (and kw (car kw)))
                       gensyms
                       inits))
        (($ <let> src names gensyms vals body)
         (for-each (lambda (sym)
                     (hashq-set! table sym (fresh-var)))
                   gensyms))
        (($ <fix> src names gensyms vals body)
         (for-each (lambda (sym)
                     (hashq-set! table sym (fresh-var)))
                   gensyms))
        (_ #t))
      (values))
    (define (up exp) (values))
    ((make-tree-il-folder) exp down up)
    table))

(define (cps-convert/thunk exp)
  (parameterize ((label-counter 0)
                 (var-counter 0)
                 (scope-counter 0)
                 (module-call-stubs '()))
    (with-cps empty-intmap
      (letv init)
      ;; Allocate kinit first so that we know that the entry point's
      ;; label is zero.  This simplifies data flow in the compiler if we
      ;; can just pass around the program as a map of continuations and
      ;; know that the entry point is label 0.
      (letk kinit ,#f)
      (letk ktail ($ktail))
      (let$ body (convert exp ktail (build-subst exp)))
      (letk kbody ($kargs () () ,body))
      (letk kclause ($kclause ('() '() #f '() #f) kbody #f))
      ($ ((lambda (cps)
            (let ((init (build-cont
                          ($kfun (tree-il-src exp) '() init ktail kclause))))
              (with-cps (persistent-intmap (intmap-replace! cps kinit init))
                kinit))))))))

(define *comp-module* (make-fluid))

(define (canonicalize exp)
  (define (reduce-conditional exp)
    (match exp
      (($ <conditional> src
          ($ <conditional> _ test ($ <const> _ t) ($ <const> _ f))
          consequent alternate)
       (cond
        ((and t (not f))
         (reduce-conditional (make-conditional src test consequent alternate)))
        ((and (not t) f)
         (reduce-conditional (make-conditional src test alternate consequent)))
        (else
         exp)))
      (_ exp)))
  (define (evaluate-args-eagerly-if-needed src inits k)
    ;; Some macros generate calls to "vector" or "list" with like 300
    ;; arguments.  Since we eventually compile to lower-level operations
    ;; like make-vector and vector-set! or cons, it reduces live
    ;; variable pressure to sink initializers if we can, if we can prove
    ;; that the initializer can't capture the continuation.  (More on
    ;; that caveat here:
    ;; http://wingolog.org/archives/2013/11/02/scheme-quiz-time).
    ;;
    ;; Normally we would do this transformation in the optimizer, but
    ;; it's quite tricky there and quite easy here, so we do it here.
    (match inits
      (() (k '()))
      ((init . inits)
       (match init
         ((or ($ <const>) ($ <void>) ($ <lambda>) ($ <lexical-ref>))
          (evaluate-args-eagerly-if-needed
           src inits (lambda (inits) (k (cons init inits)))))
         (_
          (with-lexicals src (init)
            (evaluate-args-eagerly-if-needed
             src inits (lambda (inits) (k (cons init inits))))))))))
  (post-order
   (lambda (exp)
     (match exp
       (($ <conditional>)
        (reduce-conditional exp))

       (($ <primcall> src 'exact-integer? (x))
        ;; Both fixnum? and bignum? are branching primitives.
        (with-lexicals src (x)
          (make-conditional
           src (make-primcall src 'fixnum? (list x))
           (make-const src #t)
           (make-conditional src (make-primcall src 'bignum? (list x))
                             (make-const src #t)
                             (make-const src #f)))))

       (($ <primcall> src '<= (a b))
        ;; No need to reduce as <= is a branching primitive.
        (make-conditional src (make-primcall src '<= (list a b))
                          (make-const src #t)
                          (make-const src #f)))

       (($ <primcall> src '>= (a b))
        ;; No need to reduce as < is a branching primitive.
        (make-conditional src (make-primcall src '<= (list b a))
                          (make-const src #t)
                          (make-const src #f)))

       (($ <primcall> src '> (a b))
        ;; No need to reduce as < is a branching primitive.
        (make-conditional src (make-primcall src '< (list b a))
                          (make-const src #t)
                          (make-const src #f)))

       (($ <primcall> src (? branching-primitive? name) args)
        ;; No need to reduce because test is not reducible: reifying
        ;; #t/#f is the right thing.
        (make-conditional src exp
                          (make-const src #t)
                          (make-const src #f)))

       (($ <primcall> src 'not (x))
        (reduce-conditional
         (make-conditional src x
                           (make-const src #f)
                           (make-const src #t))))

       (($ <primcall> src (or 'eqv? 'equal?) (a b))
        (let ()
          (define-syntax-rule (primcall name . args)
            (make-primcall src 'name (list . args)))
          (define-syntax primcall-chain
            (syntax-rules ()
              ((_ x) x)
              ((_ x . y)
               (make-conditional src (primcall . x) (primcall-chain . y)
                                 (make-const src #f)))))
          (define-syntax-rule (bool x)
            (make-conditional src x (make-const src #t) (make-const src #f)))
          (with-lexicals src (a b)
            (make-conditional
             src
             (primcall eq? a b)
             (make-const src #t)
             (match (primcall-name exp)
               ('eqv?
                ;; Completely inline.
                (primcall-chain (heap-number? a)
                                (heap-number? b)
                                (bool (primcall heap-numbers-equal? a b))))
               ('equal?
                ;; Partially inline.
                (primcall-chain (heap-object? a)
                                (heap-object? b)
                                (primcall equal? a b))))))))

       (($ <primcall> src 'vector args)
        ;; Expand to "allocate-vector" + "vector-init!".
        (evaluate-args-eagerly-if-needed
         src args
         (lambda (args)
           (define-syntax-rule (primcall name . args)
             (make-primcall src 'name (list . args)))
           (define-syntax-rule (const val)
             (make-const src val))
           (let ((v (primcall allocate-vector (const (length args)))))
             (with-lexicals src (v)
               (list->seq
                src
                (append (map (lambda (idx arg)
                               (primcall vector-init! v (const idx) arg))
                             (iota (length args))
                             args)
                        (list v))))))))

       (($ <primcall> src 'make-struct/simple (vtable . args))
        ;; Expand to "allocate-struct" + "struct-init!".
        (evaluate-args-eagerly-if-needed
         src args
         (lambda (args)
           (define-syntax-rule (primcall name . args)
             (make-primcall src 'name (list . args)))
           (define-syntax-rule (const val)
             (make-const src val))
           (let ((s (primcall allocate-struct vtable (const (length args)))))
             (with-lexicals src (s)
               (list->seq
                src
                (append (map (lambda (idx arg)
                               (primcall struct-init! s (const idx) arg))
                             (iota (length args))
                             args)
                        (list s))))))))

       (($ <primcall> src 'list args)
        ;; Expand to "cons".
        (evaluate-args-eagerly-if-needed
         src args
         (lambda (args)
           (define-syntax-rule (primcall name . args)
             (make-primcall src 'name (list . args)))
           (define-syntax-rule (const val)
             (make-const src val))
           (fold (lambda (arg tail) (primcall cons arg tail))
                 (const '())
                 (reverse args)))))

       ;; Lower (logand x (lognot y)) to (logsub x y).  We do it here
       ;; instead of in CPS because it gets rid of the lognot entirely;
       ;; if type folding can't prove Y to be an exact integer, then DCE
       ;; would have to leave it in the program for its possible
       ;; effects.
       (($ <primcall> src 'logand (x ($ <primcall> _ 'lognot (y))))
        (make-primcall src 'logsub (list x y)))
       (($ <primcall> src 'logand (($ <primcall> _ 'lognot (y)) x))
        (make-primcall src 'logsub (list x y)))

       (($ <primcall> src 'throw ())
        (make-call src (make-primitive-ref src 'throw) '()))

       (($ <prompt> src escape-only? tag body
           ($ <lambda> hsrc hmeta
              ($ <lambda-case> _ hreq #f hrest #f () hsyms hbody #f)))
        exp)

       (($ <primcall> src 'ash (a b))
        (match b
          (($ <const> src2 (? exact-integer? n))
           (if (< n 0)
               (make-primcall src 'rsh (list a (make-const src2 (- n))))
               (make-primcall src 'lsh (list a b))))
          (_
           (with-lexicals src (a b)
             (make-conditional
              src
              (make-primcall src '< (list b (make-const src 0)))
              (let ((n (make-primcall src '- (list (make-const src 0) b))))
                (make-primcall src 'rsh (list a n)))
              (make-primcall src 'lsh (list a b)))))))

       (_ exp)))
   exp))

(define (compile-cps exp env opts)
  (values (cps-convert/thunk (canonicalize exp)) env env))

;;; Local Variables:
;;; eval: (put 'convert-arg 'scheme-indent-function 2)
;;; eval: (put 'convert-args 'scheme-indent-function 2)
;;; End:
