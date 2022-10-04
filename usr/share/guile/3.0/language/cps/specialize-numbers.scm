;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

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
;;; Some arithmetic operations have multiple implementations: one
;;; polymorphic implementation that works on all kinds of numbers, like
;;; `add', and one or more specialized variants for unboxed numbers of
;;; some kind, like `fadd'.  If we can replace a polymorphic
;;; implementation with a monomorphic implementation, we should do so --
;;; it will speed up the runtime and avoid boxing numbers.
;;;
;;; A polymorphic operation can be specialized if its result is
;;; specialized.  To specialize an operation, we manually unbox its
;;; arguments and box its return value, relying on CSE to remove boxes
;;; where possible.
;;;
;;; We also want to specialize phi variables.  A phi variable is bound
;;; by a continuation with more than one predecessor.  For example in
;;; this code:
;;;
;;;   (+ 1.0 (if a 2.0 3.0))
;;;
;;; We want to specialize this code to:
;;;
;;;   (f64->scm (fl+ (scm->f64 1.0) (if a (scm->f64 2.0) (scm->f64 3.0))))
;;;
;;; Hopefully later passes will remove the conversions.  In any case,
;;; specialization will likely result in a lower heap-number allocation
;;; rate, and that cost is higher than the extra opcodes to do
;;; conversions.  This transformation is especially important for loop
;;; variables.
;;;
;;; Code:

(define-module (language cps specialize-numbers)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (system base target)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps renumber)
  #:use-module (language cps types)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:export (specialize-numbers))

;; A note on how to represent unboxing and boxing operations.  We want
;; to avoid diamond control flows here, like:
;;
;;   s64 x = (if (fixnum? x*) (untag-fixnum x*) (untag-bignum x*))
;;
;; The reason is that the strategy that this specialize-numbers pass
;; uses to unbox values is to reify unboxing and boxing conversions
;; around every newly reified unboxed operation; it then relies heavily
;; on DCE and CSE to remove redundant conversions.  However DCE and CSE
;; really work best when there's a linear control flow, so instead we
;; use a mid-level primcall:
;;
;;   (define (scm->s64 x*)
;;     (if (fixnum? x*) (untag-fixnum x*) (untag-bignum x*)))
;;
;; Then, unless we know that we can reduce directly to `untag-fixnum`,
;; we do:
;;
;;   s64 x = (scm->s64 x*)
;;
;; That way we keep DCE and CSE happy.  We can inline scm->s64 at the
;; backend if we choose to (though we might choose to not do so, for
;; code size reasons).

(define (simple-primcall cps k src op arg)
  (with-cps cps
    (build-term
      ($continue k src
        ($primcall op #f (arg))))))

(define-syntax-rule (define-simple-primcall name)
  (define (name cps k src arg) (simple-primcall cps k src 'name arg)))

(define-simple-primcall untag-fixnum)
(define-simple-primcall scm->s64)
(define-simple-primcall tag-fixnum)
(define-simple-primcall s64->scm)
(define-simple-primcall tag-fixnum/unlikely)
(define-simple-primcall s64->scm/unlikely)

(define (fixnum->u64 cps k src fx)
  (with-cps cps
    (letv s64)
    (letk kcvt ($kargs ('s64) (s64)
                 ($continue k src ($primcall 's64->u64 #f (s64)))))
    ($ (untag-fixnum kcvt src fx))))
(define (u64->fixnum cps k src u64)
  (with-cps cps
    (letv s64)
    (let$ tag-body (tag-fixnum k src s64))
    (letk ks64 ($kargs ('s64) (s64) ,tag-body))
    (build-term
      ($continue ks64 src ($primcall 'u64->s64 #f (u64))))))
(define-simple-primcall scm->u64)
(define-simple-primcall u64->scm)
(define-simple-primcall u64->scm/unlikely)

(define-simple-primcall scm->f64)
(define-simple-primcall f64->scm)

(define (fixnum->f64 cps k src fx)
  (with-cps cps
    (letv s64)
    (letk kcvt ($kargs ('s64) (s64)
                 ($continue k src ($primcall 's64->f64 #f (s64)))))
    ($ (untag-fixnum kcvt src fx))))

(define (specialize-unop cps k src op param a unbox-a box-result)
  (with-cps cps
    (letv a* result)
    (let$ box-result-body (box-result k src result))
    (letk kbox ($kargs ('result) (result) ,box-result-body))
    (letk kop ($kargs ('a) (a*)
                ($continue kbox src ($primcall op param (a*)))))
    ($ (unbox-a kop src a))))

(define* (specialize-binop cps k src op a b
                           unbox-a unbox-b box-result)
  (with-cps cps
    (letv a* b* result)
    (let$ box-result-body (box-result k src result))
    (letk kbox ($kargs ('result) (result) ,box-result-body))
    (letk kop ($kargs ('b) (b*)
                ($continue kbox src ($primcall op #f (a* b*)))))
    (let$ unbox-b-body (unbox-b kop src b))
    (letk kunbox-b ($kargs ('a) (a*) ,unbox-b-body))
    ($ (unbox-a kunbox-b src a))))

(define (specialize-comparison cps kf kt src op a b unbox-a unbox-b)
  (with-cps cps
    (letv a* b*)
    (letk kop ($kargs ('b) (b*) ($branch kf kt src op #f (a* b*))))
    (let$ unbox-b-body (unbox-b kop src b))
    (letk kunbox-b ($kargs ('a) (a*) ,unbox-b-body))
    ($ (unbox-a kunbox-b src a))))

(define* (specialize-comparison/immediate cps kf kt src op a imm
                                          unbox-a)
  (with-cps cps
    (letv ia)
    (letk kop ($kargs ('ia) (ia) ($branch kf kt src op imm (ia))))
    ($ (unbox-a kop src a))))

(define (specialize-comparison/s64-integer cps kf kt src op a-s64 b-int
                                           unbox-a rebox-a)
  (let ((s64-op (match op ('= 's64-=) ('< 's64-<))))
    (with-cps cps
      (letv a b sunk)
      (letk kheap ($kargs ('sunk) (sunk)
                    ($branch kf kt src op #f (sunk b-int))))
      ;; Re-box the variable.  FIXME: currently we use a specially
      ;; marked s64->scm to avoid CSE from hoisting the allocation
      ;; again.  Instead we should just use a-s64 directly and implement
      ;; an allocation sinking pass that should handle this..
      (let$ rebox-a-body (rebox-a kheap src a))
      (letk kretag ($kargs () () ,rebox-a-body))
      (letk kb ($kargs ('b) (b) ($branch kf kt src s64-op #f (a b))))
      (letk kfix ($kargs () ()
                   ($continue kb src
                     ($primcall 'untag-fixnum #f (b-int)))))
      (letk ka ($kargs ('a) (a)
                 ($branch kretag kfix src 'fixnum? #f (b-int))))
      ($ (unbox-a ka src a-s64)))))

(define (specialize-comparison/integer-s64 cps kf kt src op a-int b-s64
                                           unbox-b rebox-b)
  (match op
    ('= (specialize-comparison/s64-integer cps kf kt src op b-s64 a-int
                                           unbox-b rebox-b))
    ('<
     (with-cps cps
       (letv a b sunk)
       (letk kheap ($kargs ('sunk) (sunk)
                     ($branch kf kt src '< #f (a-int sunk))))
       ;; FIXME: We should just use b-s64 directly and implement an
       ;; allocation sinking pass so that the box op that creates b-64
       ;; should float down here.  Instead, for now we just rebox the
       ;; variable, relying on the reboxing op not being available for
       ;; CSE.
       (let$ rebox-b-body (rebox-b kheap src b))
       (letk kretag ($kargs () () ,rebox-b-body))
       (letk ka ($kargs ('a) (a) ($branch kf kt src 's64-< #f (a b))))
       (letk kfix ($kargs () ()
                    ($continue ka src
                      ($primcall 'untag-fixnum #f (a-int)))))
       (letk kb ($kargs ('b) (b)
                  ($branch kretag kfix src 'fixnum? #f (a-int))))
       ($ (unbox-b kb src b-s64))))))

(define (specialize-comparison/immediate-s64-integer cps kf kt src op a b-int
                                                     compare-integers)
  (with-cps cps
    (letv b sunk)
    (letk kheap ($kargs ('sunk) (sunk) ,(compare-integers kf kt src sunk)))
    ;; Re-box the variable.  FIXME: currently we use a specially marked
    ;; load-const to avoid CSE from hoisting the constant.  Instead we
    ;; should just use a $const directly and implement an allocation
    ;; sinking pass that should handle this..
    (letk kretag ($kargs () ()
                   ($continue kheap src
                     ($primcall 'load-const/unlikely a ()))))
    (letk kb ($kargs ('b) (b)
               ($branch kf kt src op a (b))))
    (letk kfix ($kargs () ()
                 ($continue kb src
                   ($primcall 'untag-fixnum #f (b-int)))))
    (build-term ($branch kretag kfix src 'fixnum? #f (b-int)))))

;; compute-significant-bits solves a flow equation to compute a
;; least-fixed-point over the lattice VAR -> BITMASK, where X > Y if
;; X[VAR] > Y[VAR] for any VAR.  Adjoining VAR -> BITMASK to X results
;; in a distinct value X' (in the sense of eq?) if and only if X' > X.
;; This property is used in compute-significant-bits to know when to
;; stop iterating, and is ensured by intmaps, provided that the `meet'
;; function passed to `intmap-add' and so on also preserves this
;; property.
;;
;; The meet function for adding bits is `sigbits-union'; the first
;; argument is the existing value, and the second is the bitmask to
;; adjoin.  For fixnums, BITMASK' will indeed be distinct if and only if
;; bits were added.  However for bignums it's possible that (= X' X) but
;; not (eq? X' X).  This preserve-eq? helper does the impedance matching
;; for bignums, returning the first value if the values are =.
(define (preserve-eq? x x*)
  (if (= x x*)
      x
      x*))

(define (sigbits-union x y)
  (and x y
       (preserve-eq? x (logior x y))))

(define (sigbits-intersect x y)
  (cond
   ((not x) y)
   ((not y) x)
   (else (logand x y))))

(define (sigbits-intersect3 a b c)
  (sigbits-intersect a (sigbits-intersect b c)))

(define (next-power-of-two n)
  (let lp ((out 1))
    (if (< n out)
        out
        (lp (ash out 1)))))

(define (range->sigbits min max)
  (cond
   ((or (< min 0) (> max #xffffFFFFffffFFFF)) #f)
   ((eqv? min max) min)
   (else (1- (next-power-of-two max)))))

(define (inferred-sigbits types label var)
  (call-with-values (lambda () (lookup-pre-type types label var))
    (lambda (type min max)
      (and (type<=? type (logior &exact-integer &u64 &s64))
           (range->sigbits min max)))))

(define significant-bits-handlers (make-hash-table))
(define-syntax-rule (define-significant-bits-handler
                      ((primop label types out def ...) arg ...)
                      body ...)
  (hashq-set! significant-bits-handlers 'primop
              (lambda (label types out param args defs)
                (match args ((arg ...) (match defs ((def ...) body ...)))))))

(define-significant-bits-handler ((logand label types out res) a b)
  (let ((sigbits (sigbits-intersect3 (inferred-sigbits types label a)
                                     (inferred-sigbits types label b)
                                     (intmap-ref out res (lambda (_) 0)))))
    (intmap-add (intmap-add out a sigbits sigbits-union)
                b sigbits sigbits-union)))

(define (significant-bits-handler primop)
  (hashq-ref significant-bits-handlers primop))

(define (compute-significant-bits cps types kfun)
  "Given the locally inferred types @var{types}, compute a map of VAR ->
BITS indicating the significant bits needed for a variable.  BITS may be
#f to indicate all bits, or a non-negative integer indicating a bitmask."
  (let ((preds (invert-graph (compute-successors cps kfun))))
    (let lp ((worklist (intmap-keys preds)) (visited empty-intset)
             (out empty-intmap))
      (match (intset-prev worklist)
        (#f out)
        (label
         (let ((worklist (intset-remove worklist label))
               (visited* (intset-add visited label)))
           (define (continue out*)
             (if (and (eq? out out*) (eq? visited visited*))
                 (lp worklist visited out)
                 (lp (intset-union worklist (intmap-ref preds label))
                     visited* out*)))
           (define (add-def out var)
             (intmap-add out var 0 sigbits-union))
           (define (add-defs out vars)
             (match vars
               (() out)
               ((var . vars) (add-defs (add-def out var) vars))))
           (define (add-unknown-use out var)
             (intmap-add out var (inferred-sigbits types label var)
                         sigbits-union))
           (define (add-unknown-uses out vars)
             (match vars
               (() out)
               ((var . vars)
                (add-unknown-uses (add-unknown-use out var) vars))))
           (continue
            (match (intmap-ref cps label)
              (($ $kfun src meta self)
               (if self (add-def out self) out))
              (($ $kargs names vars term)
               (let ((out (add-defs out vars)))
                 (match term
                   (($ $continue k src exp)
                    (match exp
                      ((or ($ $const) ($ $prim) ($ $fun) ($ $const-fun)
                           ($ $code) ($ $rec))
                       ;; No uses, so no info added to sigbits.
                       out)
                      (($ $values args)
                       (match (intmap-ref cps k)
                         (($ $kargs _ vars)
                          (if (intset-ref visited k)
                              (fold (lambda (arg var out)
                                      (intmap-add out arg (intmap-ref out var)
                                                  sigbits-union))
                                    out args vars)
                              out))
                         (($ $ktail)
                          (add-unknown-uses out args))))
                      (($ $call proc args)
                       (add-unknown-use (add-unknown-uses out args) proc))
                      (($ $callk label proc args)
                       (let ((out (add-unknown-uses out args)))
                         (if proc
                             (add-unknown-use out proc)
                             out)))
                      (($ $primcall name param args)
                       (let ((h (significant-bits-handler name)))
                         (if h
                             (match (intmap-ref cps k)
                               (($ $kargs _ defs)
                                (h label types out param args defs)))
                             (add-unknown-uses out args))))))
                   (($ $branch kf kt src op param args)
                    (add-unknown-uses out args))
                   (($ $switch kf kt src arg)
                    (add-unknown-use out arg))
                   (($ $prompt k kh src escape? tag)
                    (add-unknown-use out tag))
                   (($ $throw src op param args)
                    (add-unknown-uses out args)))))
              (_ out)))))))))

(define (specialize-operations cps)
  (define (u6-parameter? param)
    (<= 0 param 63))
  (define (s64-parameter? param)
    (<= (ash -1 63) param (1- (ash 1 63))))
  (define (u64-parameter? param)
    (<= 0 param (1- (ash 1 64))))
  (define (visit-cont label cont cps types sigbits)
    (define (operand-in-range? var &type &min &max)
      (call-with-values (lambda ()
                          (lookup-pre-type types label var))
        (lambda (type min max)
          (and (type<=? type &type) (<= &min min max &max)))))
    (define (u64-operand? var)
      (operand-in-range? var &exact-integer 0 (1- (ash 1 64))))
    (define (u6-operand? var)
      ;; This predicate is only used for the "count" argument to
      ;; rsh/lsh, which is already unboxed to &u64.
      (operand-in-range? var &u64 0 63))
    (define (s64-operand? var)
      (operand-in-range? var &exact-integer (ash -1 63) (1- (ash 1 63))))
    (define (fixnum-operand? var)
      (operand-in-range? var &exact-integer
                         (target-most-negative-fixnum)
                         (target-most-positive-fixnum)))
    (define (exact-integer-operand? var)
      (operand-in-range? var &exact-integer -inf.0 +inf.0))
    (define (all-u64-bits-set? var)
      (operand-in-range? var &exact-integer (1- (ash 1 64)) (1- (ash 1 64))))
    (define (only-fixnum-bits-used? var)
      (let ((bits (intmap-ref sigbits var)))
        (and bits (= bits (logand bits (target-most-positive-fixnum))))))
    (define (fixnum-result? result)
      (or (only-fixnum-bits-used? result)
          (call-with-values
              (lambda ()
                (lookup-post-type types label result 0))
            (lambda (type min max)
              (and (type<=? type &exact-integer)
                   (<= (target-most-negative-fixnum)
                       min max
                       (target-most-positive-fixnum)))))))
    (define (only-u64-bits-used? var)
      (let ((bits (intmap-ref sigbits var)))
        (and bits (= bits (logand bits (1- (ash 1 64)))))))
    (define (u64-result? result)
      (or (only-u64-bits-used? result)
          (call-with-values
              (lambda ()
                (lookup-post-type types label result 0))
            (lambda (type min max)
              (and (type<=? type &exact-integer)
                   (<= 0 min max (1- (ash 1 64))))))))
    (define (s64-result? result)
      (call-with-values
          (lambda ()
            (lookup-post-type types label result 0))
        (lambda (type min max)
          (and (type<=? type &exact-integer)
               (<= (ash -1 63) min max (1- (ash 1 63)))))))
    (define (f64-result? result)
      (call-with-values
          (lambda ()
            (lookup-post-type types label result 0))
        (lambda (type min max)
          (eqv? type &flonum))))
    (define (f64-operands? vara varb)
      (let-values (((typea mina maxa) (lookup-pre-type types label vara))
                   ((typeb minb maxb) (lookup-pre-type types label varb)))
        (and (type<=? (logior typea typeb) &real)
             (or (eqv? typea &flonum)
                 (eqv? typeb &flonum)))))
    (define (constant-arg arg)
      (let-values (((type min max) (lookup-pre-type types label arg)))
        (and (= min max) min)))
    (define (fixnum-range? min max)
      (<= (target-most-negative-fixnum) min max (target-most-positive-fixnum)))
    (define (unbox-u64 arg)
      (if (fixnum-operand? arg) fixnum->u64 scm->u64))
    (define (unbox-s64 arg)
      (if (fixnum-operand? arg) untag-fixnum scm->s64))
    (define (rebox-s64 arg)
      (if (fixnum-operand? arg) tag-fixnum/unlikely s64->scm/unlikely))
    (define (unbox-f64 arg)
      ;; Could be more precise here.
      (if (fixnum-operand? arg) fixnum->f64 scm->f64))
    (define (box-s64 result)
      (if (fixnum-result? result) tag-fixnum s64->scm))
    (define (box-u64 result)
      (if (fixnum-result? result) u64->fixnum u64->scm))
    (define (box-f64 result)
      f64->scm)

    (define (specialize-primcall cps k src op param args)
      (match (intmap-ref cps k)
        (($ $kargs (_) (result))
         (match (cons* op result param args)
           (((or 'add 'sub 'mul 'div 'atan2)
             (? f64-result?) #f a b)
            (let ((op (match op
                        ('add 'fadd) ('sub 'fsub) ('mul 'fmul) ('div 'fdiv)
                        ('atan2 'fatan2))))
              (specialize-binop cps k src op a b
                                (unbox-f64 a) (unbox-f64 b) (box-f64 result))))

           (((or 'sqrt 'abs 'floor 'ceiling 'sin 'cos 'tan 'asin 'acos 'atan)
             (? f64-result?) #f a)
            (let ((op (match op
                        ('sqrt 'fsqrt) ('abs 'fabs)
                        ('floor 'ffloor) ('ceiling 'fceiling)
                        ('sin 'fsin) ('cos 'fcos) ('tan 'ftan)
                        ('asin 'fasin) ('acos 'facos) ('atan 'fatan))))
              (specialize-unop cps k src op #f a
                               (unbox-f64 a) (box-f64 result))))

           (((or 'add 'sub 'mul 'logand 'logior 'logxor 'logsub)
             (? u64-result?) #f (? u64-operand? a) (? u64-operand? b))
            (let ((op (match op
                        ('add 'uadd) ('sub 'usub) ('mul 'umul)
                        ('logand 'ulogand) ('logior 'ulogior)
                        ('logxor 'ulogxor) ('logsub 'ulogsub))))
              (specialize-binop cps k src op a b
                                (unbox-u64 a) (unbox-u64 b) (box-u64 result))))

           (((or 'logand 'logior 'logxor 'logsub)
             (? u64-result?) #f (? s64-operand? a) (? s64-operand? b))
            (let ((op (match op
                        ('logand 'ulogand) ('logior 'ulogior)
                        ('logxor 'ulogxor) ('logsub 'ulogsub))))
              (define (unbox-u64* x)
                (let ((unbox-s64 (unbox-s64 x)))
                  (lambda (cps k src x)
                    (with-cps cps
                      (letv s64)
                      (letk ks64 ($kargs ('s64) (s64)
                                         ($continue k src
                                                    ($primcall 's64->u64 #f (s64)))))
                      ($ (unbox-s64 k src x))))))
              (specialize-binop cps k src op a b
                                (unbox-u64* a) (unbox-u64* b) (box-u64 result))))

           (((or 'add 'sub 'mul)
             (? s64-result?) #f (? s64-operand? a) (? s64-operand? b))
            (let ((op (match op
                        ('add 'sadd) ('sub 'ssub) ('mul 'smul))))
              (specialize-binop cps k src op a b
                                (unbox-s64 a) (unbox-s64 b) (box-s64 result))))

           (('sub/immediate
             (? f64-result?) param a)
            (specialize-unop cps k src 'fadd/immediate (- param) a
                             (unbox-f64 a) (box-f64 result)))

           (((or 'add/immediate 'mul/immediate)
             (? f64-result?) param a)
            (let ((op (match op
                        ('add/immediate 'fadd/immediate)
                        ('mul/immediate 'fmul/immediate))))
              (specialize-unop cps k src op param a
                               (unbox-f64 a) (box-f64 result))))

           (((or 'add/immediate 'sub/immediate 'mul/immediate)
             (? u64-result?) (? u64-parameter?) (? u64-operand? a))
            (let ((op (match op
                        ('add/immediate 'uadd/immediate)
                        ('sub/immediate 'usub/immediate)
                        ('mul/immediate 'umul/immediate))))
              (specialize-unop cps k src op param a
                               (unbox-u64 a) (box-u64 result))))

           (((or 'add/immediate 'sub/immediate 'mul/immediate)
             (? s64-result?) (? s64-parameter?) (? s64-operand? a))
            (let ((op (match op
                        ('add/immediate 'sadd/immediate)
                        ('sub/immediate 'ssub/immediate)
                        ('mul/immediate 'smul/immediate))))
              (specialize-unop cps k src op param a
                               (unbox-s64 a) (box-s64 result))))

           (((or 'lsh 'rsh)
             (? u64-result?) #f (? u64-operand? a) (? u6-operand? b))
            (let ((op (match op ('lsh 'ulsh) ('rsh 'ursh))))
              (define (pass-u64 cps k src b)
                (with-cps cps
                  (build-term ($continue k src ($values (b))))))
              (specialize-binop cps k src op a b
                                (unbox-u64 a) pass-u64 (box-u64 result))))

           (((or 'lsh 'rsh)
             (? s64-result?) #f (? s64-operand? a) (? u6-operand? b))
            (let ((op (match op ('lsh 'slsh) ('rsh 'srsh))))
              (define (pass-u64 cps k src b)
                (with-cps cps
                  (build-term ($continue k src ($values (b))))))
              (specialize-binop cps k src op a b
                                (unbox-s64 a) pass-u64 (box-s64 result))))

           (((or 'lsh/immediate 'rsh/immediate)
             (? u64-result?) (? u6-parameter?) (? u64-operand? a))
            (let ((op (match op
                        ('lsh/immediate 'ulsh/immediate)
                        ('rsh/immediate 'ursh/immediate))))
              (specialize-unop cps k src op param a
                               (unbox-u64 a) (box-u64 result))))

           (((or 'lsh/immediate 'rsh/immediate)
             (? s64-result?) (? u6-parameter?) (? s64-operand? a))
            (let ((op (match op
                        ('lsh/immediate 'slsh/immediate)
                        ('rsh/immediate 'srsh/immediate))))
              (specialize-unop cps k src op param a
                               (unbox-s64 a) (box-s64 result))))

           (_ (with-cps cps #f))))
        (_ (with-cps cps #f))))

    (define (specialize-branch cps kf kt src op param args)
      (match (cons op args)
        (('<= a b)
         (cond
          ((f64-operands? a b)
           (specialize-comparison cps kf kt src 'f64-<= a b
                                  (unbox-f64 a) (unbox-f64 b)))
          ((and (exact-integer-operand? a) (exact-integer-operand? b))
           ;; If NaN is impossible, reduce (<= a b) to (not (< b a)) and
           ;; try again.
           (specialize-branch cps kt kf src '< param (list b a)))
          (else
           (with-cps cps #f))))
        (((or '< '=) a b)
         (cond
          ((f64-operands? a b)
           (let ((op (match op ('= 'f64-=) ('< 'f64-<))))
             (specialize-comparison cps kf kt src op a b
                                    (unbox-f64 a) (unbox-f64 b))))
          ((and (s64-operand? a) (s64-operand? b))
           (cond
            ((constant-arg a)
             => (lambda (a)
                  (let ((op (match op ('= 's64-imm-=) ('< 'imm-s64-<))))
                    (specialize-comparison/immediate cps kf kt src op b a
                                                     (unbox-s64 b)))))
            ((constant-arg b)
             => (lambda (b)
                  (let ((op (match op ('= 's64-imm-=) ('< 's64-imm-<))))
                    (specialize-comparison/immediate cps kf kt src op a b
                                                     (unbox-s64 a)))))
            (else
             (let ((op (match op ('= 's64-=) ('< 's64-<))))
               (specialize-comparison cps kf kt src op a b
                                      (unbox-s64 a) (unbox-s64 b))))))
          ((and (u64-operand? a) (u64-operand? b))
           (cond
            ((constant-arg a)
             => (lambda (a)
                  (let ((op (match op ('= 'u64-imm-=) ('< 'imm-u64-<))))
                    (specialize-comparison/immediate cps kf kt src op b a
                                                     (unbox-u64 b)))))
            ((constant-arg b)
             => (lambda (b)
                  (let ((op (match op ('= 'u64-imm-=) ('< 'u64-imm-<))))
                    (specialize-comparison/immediate cps kf kt src op a b
                                                     (unbox-u64 a)))))
            (else
             (let ((op (match op ('= 'u64-=) ('< 'u64-<))))
               (specialize-comparison cps kf kt src op a b
                                      (unbox-u64 a) (unbox-u64 b))))))
          ((and (exact-integer-operand? a) (exact-integer-operand? b))
           (cond
            ((s64-operand? a)
             (cond
              ((constant-arg a)
               => (lambda (a)
                    (let ((imm-op (match op ('= 's64-imm-=) ('< 'imm-s64-<))))
                      (specialize-comparison/immediate-s64-integer
                       cps kf kt src imm-op a b
                       (lambda (kf kt src a)
                         (build-term ($branch kf kt src op #f (a b))))))))
              (else
               (specialize-comparison/s64-integer cps kf kt src op a b
                                                  (unbox-s64 a)
                                                  (rebox-s64 a)))))
            ((s64-operand? b)
             (cond
              ((constant-arg b)
               => (lambda (b)
                    (let ((imm-op (match op ('= 's64-imm-=) ('< 's64-imm-<))))
                      (specialize-comparison/immediate-s64-integer
                       cps kf kt src imm-op b a
                       (lambda (kf kt src b)
                         (build-term ($branch kf kt src op #f (a b))))))))
              (else
               (specialize-comparison/integer-s64 cps kf kt src op a b
                                                  (unbox-s64 b)
                                                  (rebox-s64 b)))))
            (else (with-cps cps #f))))
          (else (with-cps cps #f))))
        (_ (with-cps cps #f))))

    (match cont
      (($ $kfun)
       (let* ((types (infer-types cps label))
              (sigbits (compute-significant-bits cps types label)))
         (values cps types sigbits)))

      (($ $kargs names vars ($ $continue k src ($ $primcall op param args)))
       (call-with-values
           (lambda () (specialize-primcall cps k src op param args))
         (lambda (cps term)
           (values (if term
                       (with-cps cps
                         (setk label ($kargs names vars ,term)))
                       cps)
                   types sigbits))))

      (($ $kargs names vars ($ $branch kf kt src op param args))
       (call-with-values
           (lambda () (specialize-branch cps kf kt src op param args))
         (lambda (cps term)
           (values (if term
                       (with-cps cps
                         (setk label ($kargs names vars ,term)))
                       cps)
                   types sigbits))))

      (_ (values cps types sigbits))))

  (values (intmap-fold visit-cont cps cps #f #f)))

;; Compute a map from VAR -> LABEL, where LABEL indicates the cont that
;; binds VAR.
(define (compute-defs conts labels)
  (intset-fold
   (lambda (label defs)
     (match (intmap-ref conts label)
       (($ $kfun src meta self tail clause)
        (if self (intmap-add defs self label) defs))
       (($ $kargs names vars)
        (fold1 (lambda (var defs)
                 (intmap-add defs var label))
               vars defs))
       (_ defs)))
   labels empty-intmap))

;; Compute vars whose definitions are all unboxable and whose uses
;; include an unbox operation.
(define (compute-specializable-vars cps body preds defs
                                    exp-result-unboxable?
                                    unbox-ops)
  ;; Compute a map of VAR->LABEL... indicating the set of labels that
  ;; define VAR with unboxable values, given the set of vars
  ;; UNBOXABLE-VARS which is known already to be unboxable.
  (define (collect-unboxable-def-labels unboxable-vars)
    (define (add-unboxable-def unboxable-defs var label)
      (intmap-add unboxable-defs var (intset label) intset-union))
    (intset-fold (lambda (label unboxable-defs)
                   (match (intmap-ref cps label)
                     (($ $kargs _ _ ($ $continue k _ exp))
                      (match exp
                        ((? exp-result-unboxable?)
                         (match (intmap-ref cps k)
                           (($ $kargs (_) (def))
                            (add-unboxable-def unboxable-defs def label))))
                        (($ $values vars)
                         (match (intmap-ref cps k)
                           (($ $kargs _ defs)
                            (fold
                             (lambda (var def unboxable-defs)
                               (if (intset-ref unboxable-vars var)
                                   (add-unboxable-def unboxable-defs def label)
                                   unboxable-defs))
                             unboxable-defs vars defs))
                           ;; Could be $ktail for $values.
                           (_ unboxable-defs)))
                        (_ unboxable-defs)))
                     (_ unboxable-defs)))
                 body empty-intmap))

  ;; Compute the set of vars which are always unboxable.
  (define (compute-unboxable-defs)
    (fixpoint
     (lambda (unboxable-vars)
       (intmap-fold
        (lambda (def unboxable-pred-labels unboxable-vars)
          (if (and (not (intset-ref unboxable-vars def))
                   ;; Are all defining expressions unboxable?
                   (and-map (lambda (pred)
                              (intset-ref unboxable-pred-labels pred))
                            (intmap-ref preds (intmap-ref defs def))))
              (intset-add unboxable-vars def)
              unboxable-vars))
        (collect-unboxable-def-labels unboxable-vars)
        unboxable-vars))
     empty-intset))

  ;; Compute the set of vars that may ever be unboxed.
  (define (compute-unbox-uses unboxable-defs)
    (intset-fold
     (lambda (label unbox-uses)
       (match (intmap-ref cps label)
         (($ $kargs _ _ ($ $continue k _ exp))
          (match exp
            (($ $primcall (? (lambda (op) (memq op unbox-ops))) #f (var))
             (intset-add unbox-uses var))
            (($ $values vars)
             (match (intmap-ref cps k)
               (($ $kargs _ defs)
                (fold (lambda (var def unbox-uses)
                        (if (intset-ref unboxable-defs def)
                            (intset-add unbox-uses var)
                            unbox-uses))
                      unbox-uses vars defs))
               (($ $ktail)
                ;; Assume return is rare and that any unboxable def can
                ;; be reboxed when leaving the procedure.
                (fold (lambda (var unbox-uses)
                        (intset-add unbox-uses var))
                      unbox-uses vars))))
            (_ unbox-uses)))
         (_ unbox-uses)))
     body empty-intset))

  (let ((unboxable-defs (compute-unboxable-defs)))
    (intset-intersect unboxable-defs (compute-unbox-uses unboxable-defs))))

;; Compute vars whose definitions are all inexact reals and whose uses
;; include an unbox operation.
(define (compute-specializable-f64-vars cps body preds defs)
  ;; Can the result of EXP definitely be unboxed as an f64?
  (define (exp-result-f64? exp)
    (match exp
      ((or ($ $primcall 'f64->scm #f (_))
           ($ $const (and (? number?) (? inexact?) (? real?))))
       #t)
      (_ #f)))
  (compute-specializable-vars cps body preds defs exp-result-f64? '(scm->f64)))

;; Compute vars whose definitions are all exact integers in the u64
;; range and whose uses include an unbox operation.
(define (compute-specializable-u64-vars cps body preds defs)
  ;; Can the result of EXP definitely be unboxed as a u64?
  (define (exp-result-u64? exp)
    (define (u64? n)
      (and (number? n) (exact-integer? n)
           (<= 0 n #xffffffffffffffff)))
    (match exp
      ((or ($ $primcall 'u64->scm #f (_))
           ($ $primcall 'u64->scm/unlikely #f (_))
           ($ $primcall 'load-const/unlikely (? u64?) ())
           ($ $const (? u64?)))
       #t)
      (_ #f)))

  (compute-specializable-vars cps body preds defs exp-result-u64?
                              '(scm->u64 'scm->u64/truncate)))

;; Compute vars whose definitions are all exact integers in the fixnum
;; range and whose uses include an untag operation.
(define (compute-specializable-fixnum-vars cps body preds defs)
  ;; Is the result of EXP definitely a fixnum?
  (define (exp-result-fixnum? exp)
    (define (fixnum? n)
      (and (number? n) (exact-integer? n)
           (<= (target-most-negative-fixnum)
               n
               (target-most-positive-fixnum))))
    (match exp
      ((or ($ $primcall 'tag-fixnum #f (_))
           ($ $primcall 'tag-fixnum/unlikely #f (_))
           ($ $const (? fixnum?))
           ($ $primcall 'load-const/unlikely (? fixnum?) ()))
       #t)
      (_ #f)))

  (compute-specializable-vars cps body preds defs exp-result-fixnum?
                              '(untag-fixnum)))

;; Compute vars whose definitions are all exact integers in the s64
;; range and whose uses include an untag operation.
(define (compute-specializable-s64-vars cps body preds defs)
  ;; Is the result of EXP definitely a fixnum?
  (define (exp-result-fixnum? exp)
    (define (s64? n)
      (and (number? n) (exact-integer? n)
           (<= (ash -1 63) n (1- (ash 1 63)))))
    (match exp
      ((or ($ $primcall 's64->scm #f (_))
           ($ $const (? s64?))
           ($ $primcall 'load-const/unlikely (? s64?) ()))
       #t)
      (_ #f)))

  (compute-specializable-vars cps body preds defs exp-result-fixnum?
                              '(scm->s64)))

(define (compute-phi-vars cps preds)
  (intmap-fold (lambda (label preds phis)
                 (match preds
                   (() phis)
                   ((_) phis)
                   (_
                    (match (intmap-ref cps label)
                      (($ $kargs names vars)
                       (fold1 (lambda (var phis)
                                (intset-add phis var))
                              vars phis))
                      (_ phis)))))
               preds empty-intset))

;; Compute the set of variables which have more than one definition,
;; whose definitions are always f64-valued or u64-valued, and which have
;; at least one use that is an unbox operation.
(define (compute-specializable-phis cps body preds defs)
  (let ((phi-vars (compute-phi-vars cps preds)))
    (fold1 (lambda (in out)
             (match in
               ((kind vars)
                (intset-fold
                 (lambda (var out)
                   (intmap-add out var kind (lambda (old new) old)))
                 (intset-intersect phi-vars vars)
                 out))))
           `((f64 ,(compute-specializable-f64-vars cps body preds defs))
             (fx ,(compute-specializable-fixnum-vars cps body preds defs))
             (s64 ,(compute-specializable-s64-vars cps body preds defs))
             (u64 ,(compute-specializable-u64-vars cps body preds defs)))
           empty-intmap)))

;; Each definition of a f64/u64 variable should unbox that variable.
;; The cont that binds the variable should re-box it under its original
;; name, and rely on CSE to remove the boxing as appropriate.
(define (apply-specialization cps kfun body preds defs phis)
  (define (compute-unbox-labels)
    (intmap-fold (lambda (phi kind labels)
                   (fold1 (lambda (pred labels)
                            (intset-add labels pred))
                          (intmap-ref preds (intmap-ref defs phi))
                          labels))
                 phis empty-intset))
  (define (unbox-op var)
    (match (intmap-ref phis var)
      ('f64 'scm->f64)
      ('fx 'untag-fixnum)
      ('s64 'scm->s64)
      ('u64 'scm->u64)))
  (define (box-op var)
    (match (intmap-ref phis var)
      ('f64 'f64->scm)
      ('fx 'tag-fixnum)
      ('s64 's64->scm)
      ('u64 'u64->scm)))
  (define (unbox-operands)
    (define (unbox-arg cps arg def-var have-arg)
      (if (intmap-ref phis def-var (lambda (_) #f))
          (with-cps cps
            (letv unboxed)
            (let$ body (have-arg unboxed))
            (letk kunboxed ($kargs ('unboxed) (unboxed) ,body))
            (build-term
              ($continue kunboxed #f ($primcall (unbox-op def-var) #f (arg)))))
          (have-arg cps arg)))
    (define (unbox-args cps args def-vars have-args)
      (match args
        (() (have-args cps '()))
        ((arg . args)
         (match def-vars
           ((def-var . def-vars)
            (unbox-arg cps arg def-var
                       (lambda (cps arg)
                         (unbox-args cps args def-vars
                                     (lambda (cps args)
                                       (have-args cps (cons arg args)))))))))))
    (intset-fold
     (lambda (label cps)
       (match (intmap-ref cps label)
         (($ $kargs names vars ($ $continue k src exp))
          (match (intmap-ref cps k)
            (($ $kargs _ defs)
             (match exp
               ;; For expressions that define a single value, we know we need
               ;; to unbox that value.  For $values though we might have to
               ;; unbox just a subset of values.
               (($ $values args)
                (with-cps cps
                  (let$ term (unbox-args
                              args defs
                              (lambda (cps args)
                                (with-cps cps
                                  (build-term
                                    ($continue k src ($values args)))))))
                  (setk label ($kargs names vars ,term))))
               (_
                (match defs
                  ((def)
                   (with-cps cps
                     (letv boxed)
                     (letk kunbox ($kargs ('boxed) (boxed)
                                    ($continue k src
                                      ($primcall (unbox-op def) #f (boxed)))))
                     (setk label ($kargs names vars
                                   ($continue kunbox src ,exp)))))))))))))
     (compute-unbox-labels)
     cps))
  (define (compute-box-labels)
    (intmap-fold (lambda (phi kind labels)
                   (intset-add labels (intmap-ref defs phi)))
                 phis empty-intset))
  (define (box-results cps)
    (intset-fold
     (lambda (label cps)
       (match (intmap-ref cps label)
         (($ $kargs names vars term)
          (let* ((boxed (fold1 (lambda (var boxed)
                                 (if (intmap-ref phis var (lambda (_) #f))
                                     (intmap-add boxed var (fresh-var))
                                     boxed))
                               vars empty-intmap))
                 (bound-vars (map (lambda (var)
                                    (intmap-ref boxed var (lambda (var) var)))
                                  vars)))
            (define (box-var cps name var done)
              (let ((unboxed (intmap-ref boxed var (lambda (_) #f))))
                (if unboxed
                    (with-cps cps
                      (let$ term (done))
                      (letk kboxed ($kargs (name) (var) ,term))
                      (build-term
                        ($continue kboxed #f
                          ($primcall (box-op var) #f (unboxed)))))
                    (done cps))))
            (define (box-vars cps names vars done)
              (match vars
                (() (done cps))
                ((var . vars)
                 (match names
                   ((name . names)
                    (box-var cps name var
                             (lambda (cps)
                               (box-vars cps names vars done))))))))
            (with-cps cps
              (let$ box-term (box-vars names vars
                                       (lambda (cps)
                                         (with-cps cps term))))
              (setk label ($kargs names bound-vars ,box-term)))))))
     (compute-box-labels)
     cps))
  (box-results (unbox-operands)))

(define (specialize-phis cps)
  (intmap-fold
   (lambda (kfun body cps)
     (let* ((preds (compute-predecessors cps kfun #:labels body))
            (defs (compute-defs cps body))
            (phis (compute-specializable-phis cps body preds defs)))
       (if (eq? phis empty-intmap)
           cps
           (apply-specialization cps kfun body preds defs phis))))
   (compute-reachable-functions cps)
   cps))

(define (specialize-numbers cps)
  ;; Type inference wants a renumbered graph; OK.
  (let ((cps (renumber cps)))
    (with-fresh-name-state cps
      (specialize-phis (specialize-operations cps)))))
