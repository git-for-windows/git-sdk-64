;;; Abstract constant folding on CPS
;;; Copyright (C) 2014-2020 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;; 
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; This pass uses the abstract interpretation provided by type analysis
;;; to fold constant values and type predicates.  It is most profitably
;;; run after CSE, to take advantage of scalar replacement.
;;;
;;; Code:

(define-module (language cps type-fold)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps renumber)
  #:use-module (language cps types)
  #:use-module (language cps with-cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (system base target)
  #:export (type-fold))




;; Branch folders.

(define &scalar-types
  (logior &fixnum &bignum &flonum &char &special-immediate))

(define (materialize-constant type min max kt kf)
  (cond
   ((zero? type) (kf))
   ((not (and (zero? (logand type (1- type)))
              (zero? (logand type (lognot &scalar-types)))
              (eqv? min max))) (kf))
   ((eqv? type &fixnum) (kt min))
   ((eqv? type &bignum) (kt min))
   ((eqv? type &flonum) (kt (exact->inexact min)))
   ((eqv? type &char) (kt (integer->char min)))
   ((eqv? type &special-immediate)
    (cond
     ((eqv? min &null) (kt '()))
     ((eqv? min &nil) (kt #nil))
     ((eqv? min &false) (kt #f))
     ((eqv? min &true) (kt #t))
     ((eqv? min &unspecified) (kt *unspecified*))
     ;; FIXME: &undefined here
     ((eqv? min &eof) (kt the-eof-object))
     (else (kf))))
   (else (kf))))

(define *branch-folders* (make-hash-table))

(define-syntax-rule (define-branch-folder op f)
  (hashq-set! *branch-folders* 'op f))

(define-syntax-rule (define-branch-folder-alias to from)
  (hashq-set! *branch-folders* 'to (hashq-ref *branch-folders* 'from)))

(define-syntax-rule (define-unary-branch-folder* (op param arg min max)
                      body ...)
  (define-branch-folder op (lambda (param arg min max) body ...)))

(define-syntax-rule (define-unary-branch-folder (op arg min max) body ...)
  (define-unary-branch-folder* (op param arg min max) body ...))

(define-syntax-rule (define-binary-branch-folder (op arg0 min0 max0
                                                       arg1 min1 max1)
                      body ...)
  (define-branch-folder op (lambda (param arg0 min0 max0 arg1 min1 max1) body ...)))

(define (fold-eq-constant? ctype cval type min max)
  (cond
   ((zero? (logand type ctype)) (values #t #f))
   ((eqv? type ctype)
    (cond
     ((or (< cval min) (< max cval)) (values #t #f))
     ((= cval min max) (values #t #t))
     (else (values #f #f))))
   (else (values #f #f))))
(define-unary-branch-folder* (eq-constant? param type min max)
  (call-with-values (lambda () (constant-type param))
    (lambda (ctype cval cval*)
      ;; cval either equals cval* or is meaningless.
      (fold-eq-constant? ctype cval type min max))))

(define-unary-branch-folder (undefined? type min max)
  (fold-eq-constant? &special-immediate &undefined type min max))

(define-syntax-rule (define-nullish-predicate-folder op imin imax)
  (define-unary-branch-folder (op type min max)
    (let ((type* (logand type &special-immediate)))
      (cond
       ((zero? (logand type &special-immediate)) (values #t #f))
       ((eqv? type &special-immediate)
        (cond
         ((or (< imax min) (< max imin)) (values #t #f))
         ((<= imin min max imax) (values #t #t))
         (else (values #f #f))))
       (else (values #f #f))))))

(define-nullish-predicate-folder null? &null &nil)
(define-nullish-predicate-folder false? &nil &false)
(define-nullish-predicate-folder nil? &null &false) ;; &nil in middle

(define-syntax-rule (define-unary-type-predicate-folder op &type)
  (define-unary-branch-folder (op type min max)
    (let ((type* (logand type &type)))
      (cond
       ((zero? type*) (values #t #f))
       ((eqv? type type*) (values #t #t))
       (else (values #f #f))))))

(define-unary-branch-folder (heap-object? type min max)
  (define &immediate-types (logior &fixnum &char &special-immediate))
  (cond
   ((zero? (logand type &immediate-types)) (values #t #t))
   ((type<=? type &immediate-types) (values #t #f))
   (else (values #f #f))))

(define-unary-branch-folder (heap-number? type min max)
  (define &types (logior &bignum &flonum &fraction &complex))
  (cond
   ((zero? (logand type &types)) (values #t #f))
   ((type<=? type &types) (values #t #t))
   (else (values #f #f))))

;; All the cases that are in compile-bytecode.
(define-unary-type-predicate-folder fixnum? &fixnum)
(define-unary-type-predicate-folder bignum? &bignum)
(define-unary-type-predicate-folder pair? &pair)
(define-unary-type-predicate-folder symbol? &symbol)
(define-unary-type-predicate-folder variable? &box)
(define-unary-type-predicate-folder mutable-vector? &mutable-vector)
(define-unary-type-predicate-folder immutable-vector? &immutable-vector)
(define-unary-type-predicate-folder struct? &struct)
(define-unary-type-predicate-folder string? &string)
(define-unary-type-predicate-folder number? &number)
(define-unary-type-predicate-folder char? &char)

(define-unary-branch-folder (vector? type min max)
  (cond
   ((zero? (logand type &vector)) (values #t #f))
   ((type<=? type &vector) (values #t #t))
   (else (values #f #f))))

(define-binary-branch-folder (eq? type0 min0 max0 type1 min1 max1)
  (cond
   ((or (zero? (logand type0 type1)) (< max0 min1) (< max1 min0))
    (values #t #f))
   ((and (eqv? type0 type1)
         (eqv? min0 min1 max0 max1)
         (zero? (logand type0 (1- type0)))
         (not (zero? (logand type0 &scalar-types))))
    (values #t #t))
   (else
    (values #f #f))))
(define-branch-folder-alias heap-numbers-equal? eq?)

(define (compare-exact-ranges min0 max0 min1 max1)
  (and (cond ((< max0 min1) '<)
             ((> min0 max1) '>)
             ((= min0 max0 min1 max1) '=)
             ((<= max0 min1) '<=)
             ((>= min0 max1) '>=)
             (else #f))))

(define-binary-branch-folder (< type0 min0 max0 type1 min1 max1)
  (if (type<=? (logior type0 type1) &exact-number)
      (case (compare-exact-ranges min0 max0 min1 max1)
        ((<) (values #t #t))
        ((= >= >) (values #t #f))
        (else (values #f #f)))
      (values #f #f)))
(define-binary-branch-folder (u64-< type0 min0 max0 type1 min1 max1)
  (case (compare-exact-ranges min0 max0 min1 max1)
    ((<) (values #t #t))
    ((= >= >) (values #t #f))
    (else (values #f #f))))
(define-branch-folder-alias s64-< u64-<)
;; We currently cannot define branch folders for floating point
;; comparison ops like the commented one below because we can't prove
;; there are no nans involved.
;;
;; (define-branch-folder-alias f64-< <)

(define-binary-branch-folder (<= type0 min0 max0 type1 min1 max1)
  (if (type<=? (logior type0 type1) &exact-number)
      (case (compare-exact-ranges min0 max0 min1 max1)
        ((< <= =) (values #t #t))
        ((>) (values #t #f))
        (else (values #f #f)))
      (values #f #f)))

(define-unary-branch-folder* (u64-imm-= c type min max)
  (cond
   ((= c min max) (values #t #t))
   ((<= min c max) (values #f #f))
   (else (values #t #f))))
(define-branch-folder-alias s64-imm-= u64-imm-=)

(define-unary-branch-folder* (u64-imm-< c type min max)
  (cond
   ((< max c) (values #t #t))
   ((>= min c) (values #t #f))
   (else (values #f #f))))
(define-branch-folder-alias s64-imm-< u64-imm-<)

(define-unary-branch-folder* (imm-u64-< c type min max)
  (cond
   ((< c min) (values #t #t))
   ((>= c max) (values #t #f))
   (else (values #f #f))))
(define-branch-folder-alias imm-s64-< imm-u64-<)

(define-binary-branch-folder (= type0 min0 max0 type1 min1 max1)
  (cond
   ((not (type<=? (logior type0 type1) &exact-number))
    (values #f #f))
   ((zero? (logand type0 type1))
    ;; If both values are exact but of different types, they are not
    ;; equal.
    (values #t #f))
   (else
    (case (compare-exact-ranges min0 max0 min1 max1)
      ((=) (values #t #t))
      ((< >) (values #t #f))
      (else (values #f #f))))))
(define-binary-branch-folder (u64-= type0 min0 max0 type1 min1 max1)
  (case (compare-exact-ranges min0 max0 min1 max1)
    ((=) (values #t #t))
    ((< >) (values #t #f))
    (else (values #f #f))))
(define-branch-folder-alias s64-= u64-=)




(define *branch-reducers* (make-hash-table))

(define-syntax-rule (define-branch-reducer op f)
  (hashq-set! *branch-reducers* 'op f))

(define-syntax-rule (define-binary-branch-reducer
                      (op cps kf kt src
                          arg0 type0 min0 max0
                          arg1 type1 min1 max1)
                      body ...)
  (define-branch-reducer op
    (lambda (cps kf kt src param arg0 type0 min0 max0 arg1 type1 min1 max1)
      body ...)))

(define-binary-branch-reducer (eq? cps kf kt src
                                   arg0 type0 min0 max0
                                   arg1 type1 min1 max1)
  (materialize-constant
   type0 min0 max0
   (lambda (const)
     (with-cps cps
       (build-term
         ($branch kf kt src 'eq-constant? const (arg1)))))
   (lambda ()
     (materialize-constant
      type1 min1 max1
      (lambda (const)
        (with-cps cps
          (build-term
            ($branch kf kt src 'eq-constant? const (arg0)))))
      (lambda () (with-cps cps #f))))))




;; Convert e.g. rsh to rsh/immediate.

(define *primcall-macro-reducers* (make-hash-table))

(define-syntax-rule (define-primcall-macro-reducer op f)
  (hashq-set! *primcall-macro-reducers* 'op f))

(define-syntax-rule (define-unary-primcall-macro-reducer (op cps k src
                                                             arg type min max)
                      body ...)
  (define-primcall-macro-reducer op
    (lambda (cps k src param arg type min max)
      body ...)))

(define-syntax-rule (define-binary-primcall-macro-reducer
                      (op cps k src
                          arg0 type0 min0 max0
                          arg1 type1 min1 max1)
                      body ...)
  (define-primcall-macro-reducer op
    (lambda (cps k src param arg0 type0 min0 max0 arg1 type1 min1 max1)
      body ...)))

(define-binary-primcall-macro-reducer (mul cps k src
                                           arg0 type0 min0 max0
                                           arg1 type1 min1 max1)
  (cond
   ((and (type<=? type0 &exact-integer) (= min0 max0))
    (with-cps cps
      (build-term
        ($continue k src ($primcall 'mul/immediate min0 (arg1))))))
   ((and (type<=? type1 &exact-integer) (= min1 max1))
    (with-cps cps
      (build-term
        ($continue k src ($primcall 'mul/immediate min1 (arg0))))))
   (else
    (with-cps cps #f))))

(define-binary-primcall-macro-reducer (lsh cps k src
                                           arg0 type0 min0 max0
                                           arg1 type1 min1 max1)
  (cond
   ((= min1 max1)
    (with-cps cps
      (build-term
        ($continue k src ($primcall 'lsh/immediate min1 (arg0))))))
   (else
    (with-cps cps #f))))

(define-binary-primcall-macro-reducer (rsh cps k src
                                           arg0 type0 min0 max0
                                           arg1 type1 min1 max1)
  (cond
   ((= min1 max1)
    (with-cps cps
      (build-term
        ($continue k src ($primcall 'rsh/immediate min1 (arg0))))))
   (else
    (with-cps cps #f))))



;; Strength reduction.

(define *primcall-reducers* (make-hash-table))

(define-syntax-rule (define-primcall-reducer op f)
  (hashq-set! *primcall-reducers* 'op f))

(define-syntax-rule (define-unary-primcall-reducer (op cps k src param
                                                    arg type min max)
                      body ...)
  (define-primcall-reducer op
    (lambda (cps k src param arg type min max)
      body ...)))

(define-syntax-rule (define-binary-primcall-reducer (op cps k src param
                                                     arg0 type0 min0 max0
                                                     arg1 type1 min1 max1)
                      body ...)
  (define-primcall-reducer op
    (lambda (cps k src param arg0 type0 min0 max0 arg1 type1 min1 max1)
      body ...)))

(define (power-of-two? constant)
  (and (positive? constant)
       (zero? (logand constant (1- constant)))))

(define-binary-primcall-reducer (quo cps k src param
                                     arg0 type0 min0 max0
                                     arg1 type1 min1 max1)
  (cond
   ((not (type<=? (logior type0 type1) &exact-integer))
    (with-cps cps #f))
   ((and (eqv? type1 &fixnum) (eqv? min1 max1) (power-of-two? min1))
    (with-cps cps
      (build-term
        ($continue k src
          ($primcall 'rsh/immediate (logcount (1- min1)) (arg0))))))
   (else
    (with-cps cps #f))))

(define-binary-primcall-reducer (rem cps k src param
                                     arg0 type0 min0 max0
                                     arg1 type1 min1 max1)
  (cond
   ((not (type<=? (logior type0 type1) &exact-integer))
    (with-cps cps #f))
   ((and (eqv? type1 &fixnum) (eqv? min1 max1) (power-of-two? min1)
         (<= 0 min0))
    (with-cps cps
      (letv mask)
      (letk kmask
            ($kargs ('mask) (mask)
              ($continue k src
                ($primcall 'logand #f (arg0 mask)))))
      (build-term
        ($continue kmask src ($const (1- min1))))))
   (else
    (with-cps cps #f))))

(define-binary-primcall-reducer (mod cps k src param
                                     arg0 type0 min0 max0
                                     arg1 type1 min1 max1)
  (cond
   ((not (type<=? (logior type0 type1) &exact-integer))
    (with-cps cps #f))
   ((and (eqv? type1 &fixnum) (eqv? min1 max1) (power-of-two? min1))
    (with-cps cps
      (letv mask)
      (letk kmask
            ($kargs ('mask) (mask)
              ($continue k src
                ($primcall 'logand #f (arg0 mask)))))
      (build-term
        ($continue kmask src ($const (1- min1))))))
   (else
    (with-cps cps #f))))

(define-unary-primcall-reducer (mul/immediate cps k src constant
                                              arg type min max)
  (cond
   ((not (type<=? type &number))
    (with-cps cps #f))
   ((eqv? constant -1)
    ;; (* arg -1) -> (- 0 arg)
    (with-cps cps
      ($ (with-cps-constants ((zero 0))
           (build-term
             ($continue k src ($primcall 'sub #f (zero arg))))))))
   ((and (eqv? constant 0) (type<=? type &exact-number))
    ;; (* arg 0) -> 0 if arg is exact
    (with-cps cps
      (build-term ($continue k src ($const 0)))))
   ((eqv? constant 1)
    ;; (* arg 1) -> arg
    (with-cps cps
      (build-term ($continue k src ($values (arg))))))
   ((eqv? constant 2)
    ;; (* arg 2) -> (+ arg arg)
    (with-cps cps
      (build-term ($continue k src ($primcall 'add #f (arg arg))))))
   ((and (type<=? type &exact-integer)
         (positive? constant)
         (zero? (logand constant (1- constant))))
    ;; (* arg power-of-2) -> (lsh arg (log2 power-of-2))
    (let ((n (let lp ((bits 0) (constant constant))
               (if (= constant 1) bits (lp (1+ bits) (ash constant -1))))))
      (with-cps cps
        (build-term ($continue k src ($primcall 'lsh/immediate n (arg)))))))
   (else
    (with-cps cps #f))))

(define-binary-primcall-reducer (logbit? cps k src param
                                         arg0 type0 min0 max0
                                         arg1 type1 min1 max1)
  (define (compute-mask cps kmask src)
    (if (eq? min0 max0)
        (with-cps cps
          (build-term
            ($continue kmask src ($const (ash 1 min0)))))
        (with-cps cps
          ($ (with-cps-constants ((one 1))
               (letv n)
               (letk kn ($kargs ('n) (n)
                          ($continue kmask src
                            ($primcall 'lsh #f (one n)))))
               (build-term
                 ($continue kn src ($primcall 'untag-fixnum #f (arg0)))))))))
  (cond
   ((and (type<=? type0 &exact-integer)
         (<= 0 min0 (target-most-positive-fixnum))
         (<= 0 max0 (target-most-positive-fixnum)))
    (with-cps cps
      (letv mask res u64)
      (letk kt ($kargs () () ($continue k src ($const #t))))
      (letk kf ($kargs () () ($continue k src ($const #f))))
      (letk ku64 ($kargs (#f) (u64)
                   ($branch kt kf src 's64-imm-= 0 (u64))))
      (letk kand ($kargs (#f) (res)
                   ($continue ku64 src ($primcall 'untag-fixnum #f (res)))))
      (letk kmask ($kargs (#f) (mask)
                    ($continue kand src
                      ($primcall 'logand #f (mask arg1)))))
      ($ (compute-mask kmask src))))
   (else
    (with-cps cps #f))))

(define-binary-primcall-reducer (logior cps k src param
                                        arg0 type0 min0 max0
                                        arg1 type1 min1 max1)
  (cond
   ((type<=? (logior type0 type1) &exact-integer)
    (cond
     ((= 0 min0 max0)
      (with-cps cps
        (build-term
          ($continue k src ($values (arg1))))))
     ((= 0 min1 max1)
      (with-cps cps
        (build-term
          ($continue k src ($values (arg0))))))
     (else
      (with-cps cps #f))))
   (else
    (with-cps cps #f))))

(define-unary-primcall-reducer (u64->scm cps k src constant arg type min max)
  (cond
   ((<= max (target-most-positive-fixnum))
    (with-cps cps
      (letv s64)
      (letk ks64 ($kargs ('s64) (s64)
                   ($continue k src
                     ($primcall 'tag-fixnum #f (s64)))))
      (build-term
        ($continue ks64 src
          ($primcall 'u64->s64 #f (arg))))))
   (else
    (with-cps cps #f))))

(define-unary-primcall-reducer (s64->scm cps k src constant arg type min max)
  (cond
   ((<= (target-most-negative-fixnum) min max (target-most-positive-fixnum))
    (with-cps cps
      (build-term
        ($continue k src
          ($primcall 'tag-fixnum #f (arg))))))
   (else
    (with-cps cps #f))))

(define-unary-primcall-reducer (scm->s64 cps k src constant arg type min max)
  (cond
   ((and (type<=? type &exact-integer)
         (<= (target-most-negative-fixnum) min max (target-most-positive-fixnum)))
    (with-cps cps
      (build-term
        ($continue k src
          ($primcall 'untag-fixnum #f (arg))))))
   (else
    (with-cps cps #f))))

(define-unary-primcall-reducer (scm->u64 cps k src constant arg type min max)
  (cond
   ((and (type<=? type &exact-integer)
         (<= 0 min max (target-most-positive-fixnum)))
    (with-cps cps
      (letv s64)
      (letk ks64 ($kargs ('s64) (s64)
                   ($continue k src
                     ($primcall 's64->u64 #f (s64)))))
      (build-term
        ($continue ks64 src
          ($primcall 'untag-fixnum #f (arg))))))
   (else
    (with-cps cps #f))))

(define-unary-primcall-reducer (scm->f64 cps k src constant arg type min max)
  (cond
   ((and (type<=? type &exact-integer)
         (<= (target-most-negative-fixnum) min max (target-most-positive-fixnum)))
    (with-cps cps
      (letv s64)
      (letk ks64 ($kargs ('s64) (s64)
                   ($continue k src
                     ($primcall 's64->f64 #f (s64)))))
      (build-term
        ($continue ks64 src
          ($primcall 'untag-fixnum #f (arg))))))
   (else
    (with-cps cps #f))))

(define-unary-primcall-reducer (inexact cps k src constant arg type min max)
  (cond
   ((and (type<=? type &exact-integer)
         (<= (target-most-negative-fixnum) min max (target-most-positive-fixnum)))
    (with-cps cps
      (letv s64 f64)
      (letk kf64 ($kargs ('f64) (f64)
                   ($continue k src
                     ($primcall 'f64->scm #f (f64)))))
      (letk ks64 ($kargs ('s64) (s64)
                   ($continue kf64 src
                     ($primcall 's64->f64 #f (s64)))))
      (build-term
        ($continue ks64 src
          ($primcall 'untag-fixnum #f (arg))))))
   ((type<=? type &flonum)
    (with-cps cps
      (build-term
        ($continue k src ($primcall 'values #f (arg))))))
   (else
    (with-cps cps #f))))



(define (local-type-fold start end cps)
  (let ((types (infer-types cps start)))
    (define (fold-primcall cps label names vars k src op param args def)
      (call-with-values (lambda () (lookup-post-type types label def 0))
        (lambda (type min max)
          (materialize-constant
           type min max
           (lambda (val)
             ;; (pk 'folded src op args val)
             (with-cps cps
               (letv v*)
               (letk k* ($kargs (#f) (v*)
                          ($continue k src ($const val))))
               ;; Rely on DCE to elide this expression, if possible.
               (setk label
                     ($kargs names vars
                       ($continue k* src ($primcall op param args))))))
           (lambda () #f)))))
    (define (transform-primcall f cps label names vars k src op param args)
      (and f
           (match args
             ((arg0)
              (call-with-values (lambda () (lookup-pre-type types label arg0))
                (lambda (type0 min0 max0)
                  (call-with-values (lambda ()
                                      (f cps k src param arg0 type0 min0 max0))
                    (lambda (cps term)
                      (and term
                           (with-cps cps
                             (setk label ($kargs names vars ,term)))))))))
             ((arg0 arg1)
              (call-with-values (lambda () (lookup-pre-type types label arg0))
                (lambda (type0 min0 max0)
                  (call-with-values (lambda () (lookup-pre-type types label arg1))
                    (lambda (type1 min1 max1)
                      (call-with-values (lambda ()
                                          (f cps k src param arg0 type0 min0 max0
                                             arg1 type1 min1 max1))
                        (lambda (cps term)
                          (and term
                               (with-cps cps
                                 (setk label ($kargs names vars ,term)))))))))))
             (_ #f))))
    (define (reduce-primcall cps label names vars k src op param args)
      (cond
       ((transform-primcall (hashq-ref *primcall-macro-reducers* op)
                            cps label names vars k src op param args)
        => (lambda (cps)
             (match (intmap-ref cps label)
               (($ $kargs names vars
                   ($ $continue k src ($ $primcall op param args)))
                (reduce-primcall cps label names vars k src op param args)))))
       ((transform-primcall (hashq-ref *primcall-reducers* op)
                            cps label names vars k src op param args))
       (else cps)))
    (define (reduce-branch cps label names vars kf kt src op param args)
      (and=>
       (hashq-ref *branch-reducers* op)
       (lambda (reducer)
         (match args
           ((arg0 arg1)
            (call-with-values (lambda () (lookup-pre-type types label arg0))
              (lambda (type0 min0 max0)
                (call-with-values (lambda () (lookup-pre-type types label arg1))
                  (lambda (type1 min1 max1)
                    (call-with-values (lambda ()
                                        (reducer cps kf kt src param
                                                 arg0 type0 min0 max0
                                                 arg1 type1 min1 max1))
                      (lambda (cps term)
                        (and term
                             (with-cps cps
                               (setk label
                                     ($kargs names vars ,term)))))))))))))))
    (define (branch-folded cps label names vars src k)
      (with-cps cps
        (setk label
              ($kargs names vars
                ($continue k src ($values ()))))))
    (define (fold-unary-branch cps label names vars kf kt src op param arg)
      (and=>
       (hashq-ref *branch-folders* op)
       (lambda (folder)
         (call-with-values (lambda () (lookup-pre-type types label arg))
           (lambda (type min max)
             (call-with-values (lambda () (folder param type min max))
               (lambda (f? v)
                 ;; (when f? (pk 'folded-unary-branch label op arg v))
                 (and f?
                      (branch-folded cps label names vars src
                                     (if v kt kf))))))))))
    (define (fold-binary-branch cps label names vars kf kt src op param arg0 arg1)
      (and=>
       (hashq-ref *branch-folders* op)
       (lambda (folder)
         (call-with-values (lambda () (lookup-pre-type types label arg0))
           (lambda (type0 min0 max0)
             (call-with-values (lambda () (lookup-pre-type types label arg1))
               (lambda (type1 min1 max1)
                 (call-with-values (lambda ()
                                     (folder param type0 min0 max0 type1 min1 max1))
                   (lambda (f? v)
                     ;; (when f? (pk 'folded-binary-branch label op arg0 arg1 v))
                     (and f?
                          (branch-folded cps label names vars src
                                         (if v kt kf))))))))))))
    (define (fold-branch cps label names vars kf kt src op param args)
      (match args
        ((x)
         (fold-unary-branch cps label names vars kf kt src op param x))
        ((x y)
         (fold-binary-branch cps label names vars kf kt src op param x y))))
    (define (visit-primcall cps label names vars k src op param args)
      ;; We might be able to fold primcalls that define a value.
      (match (intmap-ref cps k)
        (($ $kargs (_) (def))
         (or (fold-primcall cps label names vars k src op param args def)
             (reduce-primcall cps label names vars k src op param args)))
        (_
         (reduce-primcall cps label names vars k src op param args))))
    (define (visit-branch cps label names vars kf kt src op param args)
      ;; We might be able to fold primcalls that branch.
      (or (fold-branch cps label names vars kf kt src op param args)
          (reduce-branch cps label names vars kf kt src op param args)
          cps))
    (define (visit-switch cps label names vars kf kt* src arg)
      ;; We might be able to fold or reduce a switch.
      (let ((ntargets (length kt*)))
        (call-with-values (lambda () (lookup-pre-type types label arg))
          (lambda (type min max)
            (cond
             ((<= ntargets min)
              (branch-folded cps label names vars src kf))
             ((= min max)
              (branch-folded cps label names vars src (list-ref kt* min)))
             (else
              ;; There are two more optimizations we could do here: one,
              ;; if max is less than ntargets, we can prune targets at
              ;; the end of the switch, and perhaps reduce the switch
              ;; back to a branch; and two, if min is greater than 0,
              ;; then we can subtract off min and prune targets at the
              ;; beginning.  Not done yet though.
              cps))))))
    (let lp ((label start) (cps cps))
      (if (<= label end)
          (lp (1+ label)
              (match (intmap-ref cps label)
                (($ $kargs names vars ($ $continue k src
                                         ($ $primcall op param args)))
                 (visit-primcall cps label names vars k src op param args))
                (($ $kargs names vars ($ $branch kf kt src op param args))
                 (visit-branch cps label names vars kf kt src op param args))
                (($ $kargs names vars ($ $switch kf kt* src arg))
                 (visit-switch cps label names vars kf kt* src arg))
                (_ cps)))
          cps))))

(define (fold-functions-in-renumbered-program f conts seed)
  (let* ((conts (persistent-intmap conts))
         (end (1+ (intmap-prev conts))))
    (let lp ((label 0) (seed seed))
      (if (eqv? label end)
          seed
          (match (intmap-ref conts label)
            (($ $kfun src meta self tail clause)
             (lp (1+ tail) (f label tail seed))))))))

(define (type-fold conts)
  ;; Type analysis wants a program whose labels are sorted.
  (let ((conts (renumber conts)))
    (with-fresh-name-state conts
      (persistent-intmap
       (fold-functions-in-renumbered-program local-type-fold conts conts)))))
