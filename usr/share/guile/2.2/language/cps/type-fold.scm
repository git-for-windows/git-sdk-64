;;; Abstract constant folding on CPS
;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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
  (logior &exact-integer &flonum &char &unspecified &false &true &nil &null))

(define *branch-folders* (make-hash-table))

(define-syntax-rule (define-branch-folder name f)
  (hashq-set! *branch-folders* 'name f))

(define-syntax-rule (define-branch-folder-alias to from)
  (hashq-set! *branch-folders* 'to (hashq-ref *branch-folders* 'from)))

(define-syntax-rule (define-unary-branch-folder (name arg min max) body ...)
  (define-branch-folder name (lambda (arg min max) body ...)))

(define-syntax-rule (define-binary-branch-folder (name arg0 min0 max0
                                                       arg1 min1 max1)
                      body ...)
  (define-branch-folder name (lambda (arg0 min0 max0 arg1 min1 max1) body ...)))

(define-syntax-rule (define-unary-type-predicate-folder name &type)
  (define-unary-branch-folder (name type min max)
    (let ((type* (logand type &type)))
      (cond
       ((zero? type*) (values #t #f))
       ((eqv? type type*) (values #t #t))
       (else (values #f #f))))))

;; All the cases that are in compile-bytecode.
(define-unary-type-predicate-folder pair? &pair)
(define-unary-type-predicate-folder null? &null)
(define-unary-type-predicate-folder nil? &nil)
(define-unary-type-predicate-folder symbol? &symbol)
(define-unary-type-predicate-folder variable? &box)
(define-unary-type-predicate-folder vector? &vector)
(define-unary-type-predicate-folder struct? &struct)
(define-unary-type-predicate-folder string? &string)
(define-unary-type-predicate-folder number? &number)
(define-unary-type-predicate-folder char? &char)

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
(define-branch-folder-alias eqv? eq?)

(define (compare-ranges type0 min0 max0 type1 min1 max1)
  ;; Since &real, &u64, and &f64 are disjoint, we can compare once
  ;; against their mask instead of doing three "or" comparisons.
  (and (zero? (logand (logior type0 type1) (lognot (logior &real &f64 &u64))))
       (cond ((< max0 min1) '<)
             ((> min0 max1) '>)
             ((= min0 max0 min1 max1) '=)
             ((<= max0 min1) '<=)
             ((>= min0 max1) '>=)
             (else #f))))

(define-binary-branch-folder (< type0 min0 max0 type1 min1 max1)
  (case (compare-ranges type0 min0 max0 type1 min1 max1)
    ((<) (values #t #t))
    ((= >= >) (values #t #f))
    (else (values #f #f))))
(define-branch-folder-alias u64-< <)
(define-branch-folder-alias u64-<-scm <)
;; We currently cannot define branch folders for floating point
;; comparison ops like the commented one below because we can't prove
;; there are no nans involved.
;;
;; (define-branch-folder-alias f64-< <)

(define-binary-branch-folder (<= type0 min0 max0 type1 min1 max1)
  (case (compare-ranges type0 min0 max0 type1 min1 max1)
    ((< <= =) (values #t #t))
    ((>) (values #t #f))
    (else (values #f #f))))
(define-branch-folder-alias u64-<= <=)
(define-branch-folder-alias u64-<=-scm <=)

(define-binary-branch-folder (= type0 min0 max0 type1 min1 max1)
  (case (compare-ranges type0 min0 max0 type1 min1 max1)
    ((=) (values #t #t))
    ((< >) (values #t #f))
    (else (values #f #f))))
(define-branch-folder-alias u64-= =)
(define-branch-folder-alias u64-=-scm =)

(define-binary-branch-folder (>= type0 min0 max0 type1 min1 max1)
  (case (compare-ranges type0 min0 max0 type1 min1 max1)
    ((> >= =) (values #t #t))
    ((<) (values #t #f))
    (else (values #f #f))))
(define-branch-folder-alias u64->= >=)
(define-branch-folder-alias u64->=-scm >=)

(define-binary-branch-folder (> type0 min0 max0 type1 min1 max1)
  (case (compare-ranges type0 min0 max0 type1 min1 max1)
    ((>) (values #t #t))
    ((= <= <) (values #t #f))
    (else (values #f #f))))
(define-branch-folder-alias u64-> >)
(define-branch-folder-alias u64->-scm >)

(define-binary-branch-folder (logtest type0 min0 max0 type1 min1 max1)
  (define (logand-min a b)
    (if (< a b 0)
        (min a b)
        0))
  (define (logand-max a b)
    (if (< a b 0)
        0
        (max a b)))
  (if (and (= min0 max0) (= min1 max1) (eqv? type0 type1 &exact-integer))
      (values #t (logtest min0 min1))
      (values #f #f)))




;; Strength reduction.

(define *primcall-reducers* (make-hash-table))

(define-syntax-rule (define-primcall-reducer name f)
  (hashq-set! *primcall-reducers* 'name f))

(define-syntax-rule (define-unary-primcall-reducer (name cps k src
                                                    arg type min max)
                      body ...)
  (define-primcall-reducer name
    (lambda (cps k src arg type min max)
      body ...)))

(define-syntax-rule (define-binary-primcall-reducer (name cps k src
                                                     arg0 type0 min0 max0
                                                     arg1 type1 min1 max1)
                      body ...)
  (define-primcall-reducer name
    (lambda (cps k src arg0 type0 min0 max0 arg1 type1 min1 max1)
      body ...)))

(define-binary-primcall-reducer (mul cps k src
                                     arg0 type0 min0 max0
                                     arg1 type1 min1 max1)
  (define (fail) (with-cps cps #f))
  (define (negate arg)
    (with-cps cps
      ($ (with-cps-constants ((zero 0))
           (build-term
             ($continue k src ($primcall 'sub (zero arg))))))))
  (define (zero)
    (with-cps cps
      (build-term ($continue k src ($const 0)))))
  (define (identity arg)
    (with-cps cps
      (build-term ($continue k src ($values (arg))))))
  (define (double arg)
    (with-cps cps
      (build-term ($continue k src ($primcall 'add (arg arg))))))
  (define (power-of-two constant arg)
    (let ((n (let lp ((bits 0) (constant constant))
               (if (= constant 1) bits (lp (1+ bits) (ash constant -1))))))
      (with-cps cps
        ($ (with-cps-constants ((bits n))
             (build-term ($continue k src ($primcall 'ash (arg bits)))))))))
  (define (mul/constant constant constant-type arg arg-type)
    (cond
     ((not (or (= constant-type &exact-integer) (= constant-type arg-type)))
      (fail))
     ((eqv? constant -1)
      ;; (* arg -1) -> (- 0 arg)
      (negate arg))
     ((eqv? constant 0)
      ;; (* arg 0) -> 0 if arg is not a flonum or complex
      (and (= constant-type &exact-integer)
           (zero? (logand arg-type
                          (lognot (logior &flonum &complex))))
           (zero)))
     ((eqv? constant 1)
      ;; (* arg 1) -> arg
      (identity arg))
     ((eqv? constant 2)
      ;; (* arg 2) -> (+ arg arg)
      (double arg))
     ((and (= constant-type arg-type &exact-integer)
           (positive? constant)
           (zero? (logand constant (1- constant))))
      ;; (* arg power-of-2) -> (ash arg (log2 power-of-2
      (power-of-two constant arg))
     (else
      (fail))))
  (cond
   ((logtest (logior type0 type1) (lognot &number)) (fail))
   ((= min0 max0) (mul/constant min0 type0 arg1 type1))
   ((= min1 max1) (mul/constant min1 type1 arg0 type0))
   (else (fail))))

(define-binary-primcall-reducer (logbit? cps k src
                                         arg0 type0 min0 max0
                                         arg1 type1 min1 max1)
  (define (convert-to-logtest cps kbool)
    (define (compute-mask cps kmask src)
      (if (eq? min0 max0)
          (with-cps cps
            (build-term
              ($continue kmask src ($const (ash 1 min0)))))
          (with-cps cps
            ($ (with-cps-constants ((one 1))
                 (build-term
                   ($continue kmask src ($primcall 'ash (one arg0)))))))))
    (with-cps cps
      (letv mask)
      (letk kt ($kargs () ()
                 ($continue kbool src ($const #t))))
      (letk kf ($kargs () ()
                 ($continue kbool src ($const #f))))
      (letk kmask ($kargs (#f) (mask)
                    ($continue kf src
                      ($branch kt ($primcall 'logtest (mask arg1))))))
      ($ (compute-mask kmask src))))
  ;; Hairiness because we are converting from a primcall with unknown
  ;; arity to a branching primcall.
  (let ((positive-fixnum-bits (- (* (target-word-size) 8) 3)))
    (if (and (= type0 &exact-integer)
             (<= 0 min0 positive-fixnum-bits)
             (<= 0 max0 positive-fixnum-bits))
        (match (intmap-ref cps k)
          (($ $kreceive arity kargs)
           (match arity
             (($ $arity (_) () (not #f) () #f)
              (with-cps cps
                (letv bool)
                (let$ body (with-cps-constants ((nil '()))
                             (build-term
                               ($continue kargs src ($values (bool nil))))))
                (letk kbool ($kargs (#f) (bool) ,body))
                ($ (convert-to-logtest kbool))))
             (_
              (with-cps cps
                (letv bool)
                (letk kbool ($kargs (#f) (bool)
                              ($continue k src ($primcall 'values (bool)))))
                ($ (convert-to-logtest kbool))))))
          (($ $ktail)
           (with-cps cps
             (letv bool)
             (letk kbool ($kargs (#f) (bool)
                           ($continue k src ($values (bool)))))
             ($ (convert-to-logtest kbool)))))
        (with-cps cps #f))))




;;

(define (local-type-fold start end cps)
  (define (scalar-value type val)
    (cond
     ((eqv? type &exact-integer) val)
     ((eqv? type &flonum) (exact->inexact val))
     ((eqv? type &char) (integer->char val))
     ((eqv? type &unspecified) *unspecified*)
     ((eqv? type &false) #f)
     ((eqv? type &true) #t)
     ((eqv? type &nil) #nil)
     ((eqv? type &null) '())
     (else (error "unhandled type" type val))))
  (let ((types (infer-types cps start)))
    (define (fold-primcall cps label names vars k src name args def)
      (call-with-values (lambda () (lookup-post-type types label def 0))
        (lambda (type min max)
          (and (not (zero? type))
               (zero? (logand type (1- type)))
               (zero? (logand type (lognot &scalar-types)))
               (eqv? min max)
               (let ((val (scalar-value type min)))
                 ;; (pk 'folded src name args val)
                 (with-cps cps
                   (letv v*)
                   (letk k* ($kargs (#f) (v*)
                              ($continue k src ($const val))))
                   ;; Rely on DCE to elide this expression, if
                   ;; possible.
                   (setk label
                         ($kargs names vars
                           ($continue k* src ($primcall name args))))))))))
    (define (reduce-primcall cps label names vars k src name args)
      (and=>
       (hashq-ref *primcall-reducers* name)
       (lambda (reducer)
         (match args
           ((arg0)
            (call-with-values (lambda () (lookup-pre-type types label arg0))
              (lambda (type0 min0 max0)
                (call-with-values (lambda ()
                                    (reducer cps k src arg0 type0 min0 max0))
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
                                        (reducer cps k src arg0 type0 min0 max0
                                                 arg1 type1 min1 max1))
                      (lambda (cps term)
                        (and term
                             (with-cps cps
                               (setk label ($kargs names vars ,term)))))))))))
           (_ #f)))))
    (define (fold-unary-branch cps label names vars kf kt src name arg)
      (and=>
       (hashq-ref *branch-folders* name)
       (lambda (folder)
         (call-with-values (lambda () (lookup-pre-type types label arg))
           (lambda (type min max)
             (call-with-values (lambda () (folder type min max))
               (lambda (f? v)
                 ;; (when f? (pk 'folded-unary-branch label name arg v))
                 (and f?
                      (with-cps cps
                        (setk label
                              ($kargs names vars
                                ($continue (if v kt kf) src
                                  ($values ())))))))))))))
    (define (fold-binary-branch cps label names vars kf kt src name arg0 arg1)
      (and=>
       (hashq-ref *branch-folders* name)
       (lambda (folder)
         (call-with-values (lambda () (lookup-pre-type types label arg0))
           (lambda (type0 min0 max0)
             (call-with-values (lambda () (lookup-pre-type types label arg1))
               (lambda (type1 min1 max1)
                 (call-with-values (lambda ()
                                     (folder type0 min0 max0 type1 min1 max1))
                   (lambda (f? v)
                     ;; (when f? (pk 'folded-binary-branch label name arg0 arg1 v))
                     (and f?
                          (with-cps cps
                            (setk label
                                  ($kargs names vars
                                    ($continue (if v kt kf) src
                                      ($values ())))))))))))))))
    (define (visit-expression cps label names vars k src exp)
      (match exp
        (($ $primcall name args)
         ;; We might be able to fold primcalls that define a value.
         (match (intmap-ref cps k)
           (($ $kargs (_) (def))
            (or (fold-primcall cps label names vars k src name args def)
                (reduce-primcall cps label names vars k src name args)
                cps))
           (_
            (or (reduce-primcall cps label names vars k src name args)
                cps))))
        (($ $branch kt ($ $primcall name args))
         ;; We might be able to fold primcalls that branch.
         (match args
           ((x)
            (or (fold-unary-branch cps label names vars k kt src name x)
                cps))
           ((x y)
            (or (fold-binary-branch cps label names vars k kt src name x y)
                cps))))
        (($ $branch kt ($ $values (arg)))
         ;; We might be able to fold branches on values.
         (call-with-values (lambda () (lookup-pre-type types label arg))
           (lambda (type min max)
             (cond
              ((zero? (logand type (logior &false &nil)))
               (with-cps cps
                 (setk label
                       ($kargs names vars ($continue kt src ($values ()))))))
              ((zero? (logand type (lognot (logior &false &nil))))
               (with-cps cps
                 (setk label
                       ($kargs names vars ($continue k src ($values ()))))))
              (else cps)))))
        (_ cps)))
    (let lp ((label start) (cps cps))
      (if (<= label end)
          (lp (1+ label)
              (match (intmap-ref cps label)
                (($ $kargs names vars ($ $continue k src exp))
                 (visit-expression cps label names vars k src exp))
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
