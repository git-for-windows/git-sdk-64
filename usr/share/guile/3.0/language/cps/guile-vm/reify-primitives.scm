;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2021,2023 Free Software Foundation, Inc.

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
;;; A pass to reify lone $prim's that were never folded into a
;;; $primcall, and $primcall's to primitives that don't have a
;;; corresponding VM op.
;;;
;;; Code:

(define-module (language cps guile-vm reify-primitives)
  #:use-module (ice-9 match)
  #:use-module ((language tree-il primitives)
                #:select ((primitive-module . tree-il:primitive-module)))
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps intmap)
  #:use-module (language bytecode)
  #:use-module (system base target)
  #:use-module (system base types internal)
  #:export (reify-primitives))

(define (primitive-module name)
  (tree-il:primitive-module name))

(define (primitive-ref cps name k src)
  (with-cps cps
    (letv box)
    (letk kbox ($kargs ('box) (box)
                 ($continue k src
                   ($primcall 'scm-ref/immediate '(box . 1) (box)))))
    ($ ((hashq-ref *ephemeral-reifiers* 'cached-module-box)
        kbox src (list (primitive-module name) name #f #t) '()))))

(define (builtin-ref cps idx k src)
  (with-cps cps
    (build-term
      ($continue k src ($primcall 'builtin-ref idx ())))))

(define (reify-clause cps)
  (with-cps cps
    (let$ body
          (with-cps-constants ((wna 'wrong-number-of-args)
                               (args '(#f "Wrong number of arguments" () #f)))
            (build-term ($throw #f 'throw #f (wna args)))))
    (letk kbody ($kargs () () ,body))
    (letk kclause ($kclause ('() '() #f '() #f) kbody #f))
    kclause))

(define (wrap-unary cps k src wrap unwrap op param a)
  (with-cps cps
    (letv a* res*)
    (letk kres ($kargs ('res*) (res*)
                 ($continue k src
                   ($primcall 'u64->s64 #f (res*)))))
    (letk ka ($kargs ('a*) (a*)
               ($continue kres src
                 ($primcall op param (a*)))))
    (build-term
      ($continue ka src
        ($primcall 's64->u64 #f (a))))))

(define (wrap-binary cps k src wrap unwrap op param a b)
  (with-cps cps
    (letv a* b* res*)
    (letk kres ($kargs ('res*) (res*)
                 ($continue k src
                   ($primcall 'u64->s64 #f (res*)))))
    (letk kb ($kargs ('b*) (b*)
               ($continue kres src
                 ($primcall op param (a* b*)))))
    (letk ka ($kargs ('a*) (a*)
               ($continue kb src
                 ($primcall 's64->u64 #f (b)))))
    (build-term
      ($continue ka src
        ($primcall 's64->u64 #f (a))))))

(define (wrap-binary/exp cps k src wrap unwrap op param a b-exp)
  (with-cps cps
    (letv a* b* res*)
    (letk kres ($kargs ('res*) (res*)
                 ($continue k src
                   ($primcall 'u64->s64 #f (res*)))))
    (letk kb ($kargs ('b*) (b*)
               ($continue kres src
                 ($primcall op param (a* b*)))))
    (letk ka ($kargs ('a*) (a*)
               ($continue kb src ,b-exp)))
    (build-term
      ($continue ka src
        ($primcall 's64->u64 #f (a))))))

;; Primitives that we need to remove.
(define *ephemeral-reifiers* (make-hash-table))

(define-syntax-rule (define-ephemeral (name cps k src param arg ...)
                      . body)
  (hashq-set! *ephemeral-reifiers* 'name
              (lambda (cps k src param args)
                (match args ((arg ...) (let () . body))))))

(define-ephemeral (fadd/immediate cps k src param a)
  (with-cps cps
    (letv b)
    (letk kb ($kargs ('b) (b)
               ($continue k src
                 ($primcall 'fadd #f (a b)))))
    (build-term
      ($continue kb src
        ($primcall 'load-f64 param ())))))

(define-syntax-rule (define-binary-signed-ephemeral name uname)
  (define-ephemeral (name cps k src param a b)
    (wrap-binary cps k src 's64->u64 'u64->s64 'uname #f a b)))
(define-binary-signed-ephemeral sadd uadd)
(define-binary-signed-ephemeral ssub usub)
(define-binary-signed-ephemeral smul umul)

(define-syntax-rule (define-binary-signed-ephemeral/imm name/imm
                      uname/imm uname)
  (define-ephemeral (name/imm cps k src param a)
    (if (and (exact-integer? param) (<= 0 param 255))
        (wrap-unary cps k src 's64->u64 'u64->s64 'uname/imm param a)
        (wrap-binary/exp cps k src 's64->u64 'u64->s64 'uname #f a
                         (let ((param (logand param (1- (ash 1 64)))))
                           (build-exp ($primcall 'load-u64 param ())))))))
(define-binary-signed-ephemeral/imm sadd/immediate uadd/immediate uadd)
(define-binary-signed-ephemeral/imm ssub/immediate usub/immediate usub)
(define-binary-signed-ephemeral/imm smul/immediate umul/immediate umul)

(define-ephemeral (slsh cps k src param a b)
  (wrap-binary/exp cps k src 's64->u64 'u64->s64 'ulsh #f a
                   (build-exp ($values (b)))))
(define-ephemeral (slsh/immediate cps k src param a)
  (wrap-unary cps k src 's64->u64 'u64->s64 'ulsh/immediate param a))

(define (reify-lookup cps src mod-var name assert-bound? have-var)
  (with-cps cps
    (letv name-var var)
    (let$ body (have-var var))
    (letk kres ($kargs ('var) (var) ,body))
    (letk klookup ($kargs ('name) (name-var)
                    ($continue kres src
                      ($primcall (if assert-bound? 'lookup-bound 'lookup) #f
                                 (mod-var name-var)))))
    (build-term ($continue klookup src ($const name)))))

(define (reify-resolve-module cps k src module public?)
  (with-cps cps
    (letv mod-name)
    (letk kresolve
          ($kargs ('mod-name) (mod-name)
            ($continue k src
              ($primcall 'resolve-module public? (mod-name)))))
    (build-term
      ($continue kresolve src ($const module)))))

(define-ephemeral (cached-module-box cps k src param)
  (match param
    ((module name public? #t)
     (let ((cache-key param))
       (with-cps cps
         (letv cached var)
         (letk k* ($kargs () () ($continue k src ($values (var)))))
         (letk kcache ($kargs ('var) (var)
                        ($continue k* src
                          ($primcall 'cache-set! cache-key (var)))))
         (letk kinit ($kargs () ()
                       ($continue kcache src
                         ($primcall (if public?
                                        'lookup-bound-public
                                        'lookup-bound-private)
                                    (list module name) ()))))
         (letk kok ($kargs () ()
                     ($continue k src ($values (cached)))))
         (letk ktest
               ($kargs ('cached) (cached)
                 ($branch kinit kok src 'heap-object? #f (cached))))
         (build-term
           ($continue ktest src
             ($primcall 'cache-ref cache-key ()))))))
    ((module name public? bound?)
     (let ((cache-key param))
       (with-cps cps
         (letv mod cached)
         (let$ lookup
               (reify-lookup
                src mod name bound?
                (lambda (cps var)
                  (with-cps cps
                    (letk k* ($kargs () () ($continue k src ($values (var)))))
                    (build-term
                      ($continue k* src
                        ($primcall 'cache-set! cache-key (var))))))))
         (letk kmod ($kargs ('mod) (mod) ,lookup))
         (let$ module (reify-resolve-module kmod src module public?))
         (letk kinit ($kargs () () ,module))
         (letk kok ($kargs () () ($continue k src ($values (cached)))))
         (letk ktest
               ($kargs ('cached) (cached)
                 ($branch kinit kok src 'heap-object? #f (cached))))
         (build-term
           ($continue ktest src
             ($primcall 'cache-ref cache-key ()))))))))

(define-ephemeral (cache-current-module! cps k src param mod)
  (match param
    ((scope)
     (with-cps cps
       (build-term
         ($continue k src
           ($primcall 'cache-set! scope (mod))))))))

(define-ephemeral (cached-toplevel-box cps k src param)
  (match param
    ((scope name bound?)
     (let ((cache-key (cons scope name)))
       (with-cps cps
         (letv mod cached)
         (let$ lookup
               (reify-lookup
                src mod name bound?
                (lambda (cps var)
                  (with-cps cps
                    (letk k* ($kargs () () ($continue k src ($values (var)))))
                    (build-term
                      ($continue k* src
                        ($primcall 'cache-set! cache-key (var))))))))
         (letk kmod ($kargs ('mod) (mod) ,lookup))
         (letk kinit ($kargs () ()
                       ($continue kmod src ($primcall 'cache-ref scope ()))))
         (letk kok ($kargs () () ($continue k src ($values (cached)))))
         (letk ktest
               ($kargs ('cached) (cached)
                 ($branch kinit kok src 'heap-object? #f (cached))))
         (build-term
           ($continue ktest src
             ($primcall 'cache-ref cache-key ()))))))))

(define-ephemeral (mul/immediate cps k src param a)
  (with-cps cps
    (letv imm)
    (letk kop ($kargs ('imm) (imm)
                ($continue k src ($primcall 'mul #f (a imm)))))
    (build-term
      ($continue kop src ($const param)))))

(define-ephemeral (logand/immediate cps k src param a)
  (with-cps cps
    (letv imm)
    (letk kop ($kargs ('imm) (imm)
                ($continue k src ($primcall 'logand #f (a imm)))))
    (build-term
      ($continue kop src ($const param)))))

;; FIXME: Instead of having to check this, instead every primcall that's
;; not ephemeral should be handled by compile-bytecode.
(define (compute-known-primitives)
  (define *macro-instructions*
    '(add
      add/immediate
      sub
      sub/immediate
      mul
      div
      quo
      rem
      mod
      inexact
      sqrt
      abs
      floor
      ceiling
      sin
      cos
      tan
      asin
      acos
      atan
      atan2
      fsqrt
      fabs
      ffloor
      fceiling
      fsin
      fcos
      ftan
      fasin
      facos
      fatan
      fatan2
      logand
      logior
      logxor
      logsub
      string-set!
      string->number
      string->symbol
      symbol->keyword
      symbol->string
      string-utf8-length string->utf8 utf8->string
      class-of
      scm->f64
      s64->u64 s64->scm scm->s64
      u64->s64 u64->scm scm->u64 scm->u64/truncate
      wind unwind
      push-fluid pop-fluid fluid-ref fluid-set!
      push-dynamic-state pop-dynamic-state
      lsh rsh lsh/immediate rsh/immediate
      cache-ref cache-set!
      current-module resolve-module
      module-variable define!
      lookup lookup-bound lookup-bound-public lookup-bound-private))
  (let ((table (make-hash-table)))
    (for-each
     (match-lambda ((inst . _) (hashq-set! table inst #t)))
     (instruction-list))
    (for-each
     (lambda (prim) (hashq-set! table prim #t))
     *macro-instructions*)
    table))

(define *known-primitives* (delay (compute-known-primitives)))

(define (known-primitive? name)
  "Is @var{name} a primitive that can be lowered to bytecode?"
  (hashq-ref (force *known-primitives*) name))

(define (reify-primitives cps)
  (define (visit-cont label cont cps)
    (define (resolve-prim cps name k src)
      (cond
       ((builtin-name->index name)
        => (lambda (idx) (builtin-ref cps idx k src)))
       (else
        (primitive-ref cps name k src))))
    (match cont
      (($ $kfun src meta self tail #f)
       (with-cps cps
         (let$ clause (reify-clause))
         (setk label ($kfun src meta self tail clause))))
      (($ $kargs names vars ($ $throw src op param args))
       (match op
         ('raise-type-error
          (match (cons param args)
            ((#(proc-name pos what) val)
             (define msg
               (format #f
                       "Wrong type argument in position ~a (expecting ~a): ~~S"
                       pos what))
             (with-cps cps
               (setk label
                     ($kargs names vars
                       ($throw src 'throw/value+data
                               (vector 'wrong-type-arg proc-name msg)
                               (val))))))))
         ('raise-range-error
          (match (cons param args)
            ((#(proc-name pos) val)
             (define msg
               (format #f "Argument ~a out of range: ~~S" pos))
             (with-cps cps
               (setk label
                     ($kargs names vars
                       ($throw src 'throw/value+data
                               (vector 'out-of-range proc-name msg)
                               (val))))))))
         ('raise-arity-error
          (match (cons param args)
            ((#(proc-name) val)
             (define msg "Wrong number of arguments to ~A")
             (with-cps cps
               (setk label
                     ($kargs names vars
                       ($throw src 'throw/value
                               (vector 'wrong-number-of-args proc-name msg)
                               (val))))))))
         ('raise-exception
          (match (cons param args)
            ((#f exn)
             (with-cps cps
               (letv ignored prim)
               (letk kdie ($kargs (#f) (ignored)
                                    ($throw src 'unreachable #f ())))
               (letk kret ($kreceive '() 'rest kdie))
               (letk kcall ($kargs ('raise-exception) (prim)
                             ($continue kret src ($call prim (exn)))))
               (let$ body (resolve-prim 'raise-exception kcall src))
               (setk label ($kargs names vars ,body))))))
         ((or 'unreachable 'throw 'throw/value 'throw/value+data) cps)))
      (($ $kargs names vars ($ $continue k src ($ $prim name)))
       (with-cps cps
         (let$ body (resolve-prim name k src))
         (setk label ($kargs names vars ,body))))
      (($ $kargs names vars
          ($ $continue k src ($ $primcall 'call-thunk/no-inline #f (proc))))
       (with-cps cps
         (setk label ($kargs names vars ($continue k src ($call proc ()))))))
      (($ $kargs names vars
          ($ $continue k src ($ $primcall 'u64->scm/unlikely #f (u64))))
       (with-cps cps
         (setk label ($kargs names vars
                       ($continue k src ($primcall 'u64->scm #f (u64)))))))
      (($ $kargs names vars
          ($ $continue k src ($ $primcall 's64->scm/unlikely #f (s64))))
       (with-cps cps
         (setk label ($kargs names vars
                       ($continue k src ($primcall 's64->scm #f (s64)))))))
      (($ $kargs names vars
          ($ $continue k src ($ $primcall 'tag-fixnum/unlikely #f (s64))))
       (with-cps cps
         (setk label ($kargs names vars
                       ($continue k src ($primcall 'tag-fixnum #f (s64)))))))
      (($ $kargs names vars
          ($ $continue k src ($ $primcall 'load-const/unlikely val ())))
       (with-cps cps
         (setk label ($kargs names vars ($continue k src ($const val))))))
      (($ $kargs names vars
          ($ $continue k src
             ($ $primcall (or 'assume-u64 'assume-s64) (lo . hi) (val))))
       (with-cps cps
         (setk label ($kargs names vars
                       ($continue k src ($values (val)))))))
      (($ $kargs names vars ($ $continue k src ($ $primcall name param args)))
       (cond
        ((hashq-ref *ephemeral-reifiers* name)
         => (lambda (reify)
              (with-cps cps
                (let$ body (reify k src param args))
                (setk label ($kargs names vars ,body)))))
        ((known-primitive? name)
         ;; Assume arities are correct.
         (let ()
           (define (u6? val) (and (exact-integer? val) (<= 0 val 63)))
           (define (u8? val) (and (exact-integer? val) (<= 0 val 255)))
           (define-syntax-rule (reify-constants
                                wrap
                                ((op (pred? c) in ...) (op* out ...))
                                ...
                                (_ default))
             (match name
               ('op
                (if (pred? param)
                    cps
                    (match args
                      ((in ...)
                       (with-cps cps
                         (letv c)
                         (letk kconst ($kargs ('c) (c)
                                        ($continue k src
                                          ($primcall 'op* #f (out ...)))))
                         (setk label
                               ($kargs names vars
                                 ($continue kconst src wrap))))))))
               ...
               (_ default)))
           (define-syntax-rule (reify-scm-constants clause ...)
             (reify-constants ($const param) clause ...))
           (define-syntax-rule (reify-u64-constants clause ...)
             (reify-constants ($primcall 'load-u64 param ()) clause ...))
           (reify-scm-constants
            ((add/immediate (u8? y) x) (add x y))
            ((sub/immediate (u8? y) x) (sub x y))
            (_
             (reify-u64-constants
              ((uadd/immediate (u8? y) x) (uadd x y))
              ((usub/immediate (u8? y) x) (usub x y))
              ((umul/immediate (u8? y) x) (umul x y))
              ((rsh/immediate (u6? y) x) (rsh x y))
              ((lsh/immediate (u6? y) x) (lsh x y))
              ;; These should all be u6's by construction.
              ;; ((ursh/immediate (u6? y) x) (ursh x y))
              ;; ((srsh/immediate (u6? y) x) (srsh x y))
              ;; ((ulsh/immediate (u6? y) x) (ulsh x y))
              ((ulogand/immediate (u8? y) x) (ulogand x y))
              (_
               (match (cons name args)
                 (((or 'allocate-words/immediate
                       'allocate-pointerless-words/immediate))
                  (define op
                    (match name
                      ('allocate-words/immediate
                       'allocate-words)
                      ('allocate-pointerless-words/immediate
                       'allocate-pointerless-words)))
                  (match param
                    ((ann . n)
                     (if (u8? n)
                         cps
                         (with-cps cps
                           (letv n*)
                           (letk kop ($kargs ('n) (n*)
                                       ($continue k src
                                         ($primcall op ann (n*)))))
                           (setk label ($kargs names vars
                                         ($continue kop src
                                           ($primcall 'load-u64 n ())))))))))
                 ;; Assume (tail-)pointer-ref/immediate is within u8 range.
                 (((or 'word-ref/immediate 'scm-ref/immediate) obj)
                  (match param
                    ((ann . idx)
                     (if (u8? idx)
                         cps
                         (let ((op (match name
                                     ('word-ref/immediate 'word-ref)
                                     ('scm-ref/immediate 'scm-ref))))
                           (with-cps cps
                             (letv idx*)
                             (letk kop ($kargs ('idx) (idx*)
                                         ($continue k src
                                           ($primcall op ann (obj idx*)))))
                             (setk label ($kargs names vars
                                           ($continue kop src
                                             ($primcall 'load-u64 idx ()))))))))))
                 (((or 'word-set!/immediate 'scm-set!/immediate) obj val)
                  (match param
                    ((ann . idx)
                     (if (u8? idx)
                         cps
                         (let ((op (match name
                                     ('word-set!/immediate 'word-set!)
                                     ('scm-set!/immediate 'scm-set!))))
                           (with-cps cps
                             (letv idx*)
                             (letk kop ($kargs ('idx) (idx*)
                                         ($continue k src
                                           ($primcall op ann (obj idx* val)))))
                             (setk label ($kargs names vars
                                           ($continue kop src
                                             ($primcall 'load-u64 idx ()))))))))))
                 (_ cps))))))))
        (param (error "unexpected param to reified primcall" name))
        (else
         (with-cps cps
           (letv proc)
           (letk krecv ($kreceive '(res) #f k))
           (letk kproc ($kargs ('proc) (proc)
                         ($continue krecv src ($call proc args))))
           (let$ body (resolve-prim name kproc src))
           (setk label ($kargs names vars ,body))))))
      (($ $kargs names vars ($ $branch kf kt src name param args))
       (let ()
         (define (u11? val) (<= 0 val #x7ff))
         (define (u12? val) (<= 0 val #xfff))
         (define (s12? val) (<= (- #x800) val #x7ff))
         (define (imm16? val)
           (and=> (scm->immediate-bits val)
                  (lambda (bits)
                    (truncate-bits bits 16 #t))))
         (define (load-u64 k param)
           (build-term ($continue k src ($primcall 'load-u64 param ()))))
         (define (load-s64 k param)
           (build-term ($continue k src ($primcall 'load-s64 param ()))))
         (define (load-const k param)
           (build-term ($continue k src ($const param))))

         (define-syntax-rule (reify-constants ((op (pred? c) in ...)
                                               wrap (op* out ...))
                                              ...
                                              (_ default))
           (match name
             ('op
              (if (pred? param)
                  cps
                  (match args
                    ((in ...)
                     (with-cps cps
                       (letv c)
                       (letk kconst
                             ($kargs ('c) (c)
                               ($branch kf kt src 'op* #f (out ...))))
                       (setk label
                             ($kargs names vars ,(wrap kconst param))))))))
             ...
             (_ default)))
         (reify-constants
          ((u64-imm-= (u11? b) a) load-u64 (u64-= a b))
          ((u64-imm-< (u12? b) a) load-u64 (u64-< a b))
          ((imm-u64-< (u12? a) b) load-u64 (u64-< a b))
          ((s64-imm-= (s12? b) a) load-s64 (s64-= a b))
          ((s64-imm-< (s12? b) a) load-s64 (s64-< a b))
          ((imm-s64-< (s12? a) b) load-s64 (s64-< a b))
          ((eq-constant? (imm16? b) a) load-const (eq? a b))
          (_ cps))))
      (_ cps)))

  (with-fresh-name-state cps
    (persistent-intmap (intmap-fold visit-cont cps cps))))
