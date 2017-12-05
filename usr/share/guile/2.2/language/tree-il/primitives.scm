;;; open-coding primitive procedures

;; Copyright (C) 2009-2015, 2017 Free Software Foundation, Inc.

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

(define-module (language tree-il primitives)
  #:use-module (system base pmatch)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (system base syntax)
  #:use-module (language tree-il)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-16)
  #:export (resolve-primitives add-interesting-primitive!
            expand-primcall expand-primitives
            effect-free-primitive? effect+exception-free-primitive?
            constructor-primitive?
            singly-valued-primitive? equality-primitive?
            bailout-primitive?
            negate-primitive))

;; When adding to this, be sure to update *multiply-valued-primitives*
;; if appropriate.
(define *interesting-primitive-names* 
  '(apply
    call-with-values
    call-with-current-continuation
    call/cc
    dynamic-wind
    values
    eq? eqv? equal?
    memq memv
    = < > <= >= zero? positive? negative?
    + * - / 1- 1+ quotient remainder modulo
    ash logand logior logxor lognot logtest logbit?
    sqrt abs
    not
    pair? null? list? symbol? vector? string? struct? number? char? nil?
    bytevector? keyword? bitvector?

    procedure? thunk?

    complex? real? rational? inf? nan? integer? exact? inexact? even? odd?

    char<? char<=? char>=? char>?

    integer->char char->integer number->string string->number

    acons cons cons*

    list vector

    car cdr
    set-car! set-cdr!

    caar cadr cdar cddr

    caaar caadr cadar caddr cdaar cdadr cddar cdddr

    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

    length

    make-vector vector-length vector-ref vector-set!
    variable? variable-ref variable-set!
    variable-bound?

    current-module define!

    current-thread fluid-ref fluid-set! with-fluid* with-dynamic-state

    call-with-prompt
    abort-to-prompt* abort-to-prompt
    make-prompt-tag

    throw error scm-error

    string-length string-ref string-set!

    allocate-struct struct-vtable make-struct/no-tail struct-ref struct-set!

    bytevector-length

    bytevector-u8-ref bytevector-u8-set!
    bytevector-s8-ref bytevector-s8-set!
    u8vector-ref u8vector-set! s8vector-ref s8vector-set!
    
    bytevector-u16-ref bytevector-u16-set!
    bytevector-u16-native-ref bytevector-u16-native-set!
    bytevector-s16-ref bytevector-s16-set!
    bytevector-s16-native-ref bytevector-s16-native-set!
    u16vector-ref u16vector-set! s16vector-ref s16vector-set!
    
    bytevector-u32-ref bytevector-u32-set!
    bytevector-u32-native-ref bytevector-u32-native-set!
    bytevector-s32-ref bytevector-s32-set!
    bytevector-s32-native-ref bytevector-s32-native-set!
    u32vector-ref u32vector-set! s32vector-ref s32vector-set!
    
    bytevector-u64-ref bytevector-u64-set!
    bytevector-u64-native-ref bytevector-u64-native-set!
    bytevector-s64-ref bytevector-s64-set!
    bytevector-s64-native-ref bytevector-s64-native-set!
    u64vector-ref u64vector-set! s64vector-ref s64vector-set!
    
    bytevector-ieee-single-ref bytevector-ieee-single-set!
    bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
    bytevector-ieee-double-ref bytevector-ieee-double-set!
    bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!
    f32vector-ref f32vector-set! f64vector-ref f64vector-set!))

(define (add-interesting-primitive! name)
  (hashq-set! *interesting-primitive-vars*
              (or (module-variable (current-module) name)
                  (error "unbound interesting primitive" name))
              name))

(define *interesting-primitive-vars* (make-hash-table))

(for-each add-interesting-primitive! *interesting-primitive-names*)

(define *primitive-constructors*
  ;; Primitives that return a fresh object.
  '(acons cons cons* list vector make-vector
    allocate-struct make-struct/no-tail
    make-prompt-tag))

(define *primitive-accessors*
  ;; Primitives that are pure, but whose result depends on the mutable
  ;; memory pointed to by their operands.
  ;;
  ;; Note: if you add an accessor here, be sure to add a corresponding
  ;; case in (language tree-il effects)!
  '(vector-ref
    car cdr
    memq memv
    struct-ref
    string-ref
    bytevector-u8-ref bytevector-s8-ref
    bytevector-u16-ref bytevector-u16-native-ref
    bytevector-s16-ref bytevector-s16-native-ref
    bytevector-u32-ref bytevector-u32-native-ref
    bytevector-s32-ref bytevector-s32-native-ref
    bytevector-u64-ref bytevector-u64-native-ref
    bytevector-s64-ref bytevector-s64-native-ref
    bytevector-ieee-single-ref bytevector-ieee-single-native-ref
    bytevector-ieee-double-ref bytevector-ieee-double-native-ref))

(define *effect-free-primitives*
  `(values
    eq? eqv? equal?
    = < > <= >= zero? positive? negative?
    ash logand logior logxor lognot logtest logbit?
    + * - / 1- 1+ sqrt abs quotient remainder modulo
    not
    pair? null? nil? list?
    symbol? variable? vector? struct? string? number? char?
    bytevector? keyword? bitvector? atomic-box?
    complex? real? rational? inf? nan? integer? exact? inexact? even? odd?
    char<? char<=? char>=? char>?
    integer->char char->integer number->string string->number
    struct-vtable
    length string-length vector-length bytevector-length
    ;; These all should get expanded out by expand-primitives.
    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    ,@*primitive-constructors*
    ,@*primitive-accessors*))

;; Like *effect-free-primitives* above, but further restricted in that they
;; cannot raise exceptions.
(define *effect+exception-free-primitives*
  '(values
    eq? eqv? equal?
    not
    pair? null? nil? list?
    symbol? variable? vector? struct? string? number? char?
    bytevector? keyword? bitvector?
    procedure? thunk? atomic-box?
    acons cons cons* list vector))

;; Primitives that don't always return one value.
(define *multiply-valued-primitives* 
  '(apply
    call-with-values
    call-with-current-continuation
    call/cc
    dynamic-wind
    values
    call-with-prompt
    @abort abort-to-prompt))

;; Procedures that cause a nonlocal, non-resumable abort.
(define *bailout-primitives*
  '(throw error scm-error))

;; Negatable predicates.
(define *negatable-primitives*
  '((even? . odd?)
    (exact? . inexact?)
    ;; (< <= > >=) are not negatable because of NaNs.
    (char<? . char>=?)
    (char>? . char<=?)))

(define *equality-primitives*
  '(eq? eqv? equal?))

(define *effect-free-primitive-table* (make-hash-table))
(define *effect+exceptions-free-primitive-table* (make-hash-table))
(define *equality-primitive-table* (make-hash-table))
(define *multiply-valued-primitive-table* (make-hash-table))
(define *bailout-primitive-table* (make-hash-table))
(define *negatable-primitive-table* (make-hash-table))

(for-each (lambda (x)
            (hashq-set! *effect-free-primitive-table* x #t))
          *effect-free-primitives*)
(for-each (lambda (x) 
            (hashq-set! *effect+exceptions-free-primitive-table* x #t))
          *effect+exception-free-primitives*)
(for-each (lambda (x)
            (hashq-set! *equality-primitive-table* x #t))
          *equality-primitives*)
(for-each (lambda (x) 
            (hashq-set! *multiply-valued-primitive-table* x #t))
          *multiply-valued-primitives*)
(for-each (lambda (x)
            (hashq-set! *bailout-primitive-table* x #t))
          *bailout-primitives*)
(for-each (lambda (x)
            (hashq-set! *negatable-primitive-table* (car x) (cdr x))
            (hashq-set! *negatable-primitive-table* (cdr x) (car x)))
          *negatable-primitives*)

(define (constructor-primitive? prim)
  (memq prim *primitive-constructors*))
(define (effect-free-primitive? prim)
  (hashq-ref *effect-free-primitive-table* prim))
(define (effect+exception-free-primitive? prim)
  (hashq-ref *effect+exceptions-free-primitive-table* prim))
(define (equality-primitive? prim)
  (hashq-ref *equality-primitive-table* prim))
(define (singly-valued-primitive? prim)
  (not (hashq-ref *multiply-valued-primitive-table* prim)))
(define (bailout-primitive? prim)
  (hashq-ref *bailout-primitive-table* prim))
(define (negate-primitive prim)
  (hashq-ref *negatable-primitive-table* prim))

(define (resolve-primitives x mod)
  (define local-definitions
    (make-hash-table))

  ;; Assume that any definitions with primitive names in the root module
  ;; have the same semantics as the primitives.
  (unless (eq? mod the-root-module)
    (let collect-local-definitions ((x x))
      (record-case x
        ((<toplevel-define> name)
         (hashq-set! local-definitions name #t))
        ((<seq> head tail)
         (collect-local-definitions head)
         (collect-local-definitions tail))
        (else #f))))
  
  (post-order
   (lambda (x)
     (or
      (record-case x
        ((<toplevel-ref> src name)
         (and=> (and (not (hashq-ref local-definitions name))
                     (hashq-ref *interesting-primitive-vars*
                                (module-variable mod name)))
                (lambda (name) (make-primitive-ref src name))))
        ((<module-ref> src mod name public?)
         ;; for the moment, we're disabling primitive resolution for
         ;; public refs because resolve-interface can raise errors.
         (and=> (and=> (resolve-module mod)
                       (if public?
                           module-public-interface
                           identity))
                (lambda (m)
                  (and=> (hashq-ref *interesting-primitive-vars*
                                    (module-variable m name))
                         (lambda (name)
                           (make-primitive-ref src name))))))
        ((<call> src proc args)
         (and (primitive-ref? proc)
              (make-primcall src (primitive-ref-name proc) args)))
        (else #f))
      x))
   x))



(define *primitive-expand-table* (make-hash-table))

(define (expand-primcall x)
  (record-case x
    ((<primcall> src name args)
     (let ((expand (hashq-ref *primitive-expand-table* name)))
       (or (and expand (apply expand src args))
           x)))
    (else x)))

(define (expand-primitives x)
  (pre-order expand-primcall x))

;;; I actually did spend about 10 minutes trying to redo this with
;;; syntax-rules. Patches appreciated.
;;;
(define-macro (define-primitive-expander sym . clauses)
  (define (inline-args args)
    (let lp ((in args) (out '()))
      (cond ((null? in) `(list ,@(reverse out)))
            ((symbol? in) `(cons* ,@(reverse out) ,in))
            ((pair? (car in))
             (lp (cdr in)
                 (cons (if (eq? (caar in) 'quote)
                           `(make-const src ,@(cdar in))
                           `(make-primcall src ',(caar in)
                                           ,(inline-args (cdar in))))
                       out)))
            ((symbol? (car in))
             ;; assume it's locally bound
             (lp (cdr in) (cons (car in) out)))
            ((self-evaluating? (car in))
             (lp (cdr in) (cons `(make-const src ,(car in)) out)))
            (else
             (error "what what" (car in))))))
  (define (consequent exp)
    (cond
     ((pair? exp)
      (pmatch exp
        ((if ,test ,then ,else)
         `(if ,test
              ,(consequent then)
              ,(consequent else)))
        (else
         `(make-primcall src ',(car exp)
                         ,(inline-args (cdr exp))))))
     ((symbol? exp)
      ;; assume locally bound
      exp)
     ((number? exp)
      `(make-const src ,exp))
     ((not exp)
      ;; failed match
      #f)
     (else (error "bad consequent yall" exp))))
  `(hashq-set! *primitive-expand-table*
               ',sym
               (match-lambda*
                ,@(let lp ((in clauses) (out '()))
                    (if (null? in)
                        (reverse (cons '(_ #f) out))
                        (lp (cddr in)
                            (cons `((src . ,(car in))
                                    ,(consequent (cadr in)))
                                  out)))))))

(define-primitive-expander zero? (x)
  (= x 0))

(define-primitive-expander positive? (x)
  (> x 0))

(define-primitive-expander negative? (x)
  (< x 0))

;; FIXME: All the code that uses `const?' is redundant with `peval'.

(define-primitive-expander 1+ (x)
  (+ x 1))

(define-primitive-expander 1- (x)
  (- x 1))

(define-primitive-expander +
  () 0
  (x) (values x)
  (x y) (+ x y)
  (x y z ... last) (+ (+ x y . z) last))

(define-primitive-expander *
  () 1
  (x) (values x)
  (x y z ... last) (* (* x y . z) last))
  
(define-primitive-expander -
  (x) (- 0 x)
  (x y) (- x y)
  (x y z ... last) (- (- x y . z) last))
  
(define-primitive-expander /
  (x) (/ 1 x)
  (x y z ... last) (/ (/ x y . z) last))
  
(define-primitive-expander logior
  () 0
  (x) (logior x 0)
  (x y) (logior x y)
  (x y z ... last) (logior (logior x y . z) last))

(define-primitive-expander logand
  () -1
  (x) (logand x -1)
  (x y) (logand x y)
  (x y z ... last) (logand (logand x y . z) last))

(define-primitive-expander caar (x) (car (car x)))
(define-primitive-expander cadr (x) (car (cdr x)))
(define-primitive-expander cdar (x) (cdr (car x)))
(define-primitive-expander cddr (x) (cdr (cdr x)))
(define-primitive-expander caaar (x) (car (car (car x))))
(define-primitive-expander caadr (x) (car (car (cdr x))))
(define-primitive-expander cadar (x) (car (cdr (car x))))
(define-primitive-expander caddr (x) (car (cdr (cdr x))))
(define-primitive-expander cdaar (x) (cdr (car (car x))))
(define-primitive-expander cdadr (x) (cdr (car (cdr x))))
(define-primitive-expander cddar (x) (cdr (cdr (car x))))
(define-primitive-expander cdddr (x) (cdr (cdr (cdr x))))
(define-primitive-expander caaaar (x) (car (car (car (car x)))))
(define-primitive-expander caaadr (x) (car (car (car (cdr x)))))
(define-primitive-expander caadar (x) (car (car (cdr (car x)))))
(define-primitive-expander caaddr (x) (car (car (cdr (cdr x)))))
(define-primitive-expander cadaar (x) (car (cdr (car (car x)))))
(define-primitive-expander cadadr (x) (car (cdr (car (cdr x)))))
(define-primitive-expander caddar (x) (car (cdr (cdr (car x)))))
(define-primitive-expander cadddr (x) (car (cdr (cdr (cdr x)))))
(define-primitive-expander cdaaar (x) (cdr (car (car (car x)))))
(define-primitive-expander cdaadr (x) (cdr (car (car (cdr x)))))
(define-primitive-expander cdadar (x) (cdr (car (cdr (car x)))))
(define-primitive-expander cdaddr (x) (cdr (car (cdr (cdr x)))))
(define-primitive-expander cddaar (x) (cdr (cdr (car (car x)))))
(define-primitive-expander cddadr (x) (cdr (cdr (car (cdr x)))))
(define-primitive-expander cdddar (x) (cdr (cdr (cdr (car x)))))
(define-primitive-expander cddddr (x) (cdr (cdr (cdr (cdr x)))))

(define-primitive-expander cons*
  (x) (values x)
  (x y) (cons x y)
  (x y . rest) (cons x (cons* y . rest)))

(define-primitive-expander acons (x y z)
  (cons (cons x y) z))

(define-primitive-expander call/cc (proc)
  (call-with-current-continuation proc))

(define-primitive-expander u8vector-ref (vec i)
  (bytevector-u8-ref vec i))
(define-primitive-expander u8vector-set! (vec i x)
  (bytevector-u8-set! vec i x))
(define-primitive-expander s8vector-ref (vec i)
  (bytevector-s8-ref vec i))
(define-primitive-expander s8vector-set! (vec i x)
  (bytevector-s8-set! vec i x))

(define-primitive-expander u16vector-ref (vec i)
  (bytevector-u16-native-ref vec (* i 2)))
(define-primitive-expander u16vector-set! (vec i x)
  (bytevector-u16-native-set! vec (* i 2) x))
(define-primitive-expander s16vector-ref (vec i)
  (bytevector-s16-native-ref vec (* i 2)))
(define-primitive-expander s16vector-set! (vec i x)
  (bytevector-s16-native-set! vec (* i 2) x))

(define-primitive-expander u32vector-ref (vec i)
  (bytevector-u32-native-ref vec (* i 4)))
(define-primitive-expander u32vector-set! (vec i x)
  (bytevector-u32-native-set! vec (* i 4) x))
(define-primitive-expander s32vector-ref (vec i)
  (bytevector-s32-native-ref vec (* i 4)))
(define-primitive-expander s32vector-set! (vec i x)
  (bytevector-s32-native-set! vec (* i 4) x))

(define-primitive-expander u64vector-ref (vec i)
  (bytevector-u64-native-ref vec (* i 8)))
(define-primitive-expander u64vector-set! (vec i x)
  (bytevector-u64-native-set! vec (* i 8) x))
(define-primitive-expander s64vector-ref (vec i)
  (bytevector-s64-native-ref vec (* i 8)))
(define-primitive-expander s64vector-set! (vec i x)
  (bytevector-s64-native-set! vec (* i 8) x))

(define-primitive-expander f32vector-ref (vec i)
  (bytevector-ieee-single-native-ref vec (* i 4)))
(define-primitive-expander f32vector-set! (vec i x)
  (bytevector-ieee-single-native-set! vec (* i 4) x))
(define-primitive-expander f32vector-ref (vec i)
  (bytevector-ieee-single-native-ref vec (* i 4)))
(define-primitive-expander f32vector-set! (vec i x)
  (bytevector-ieee-single-native-set! vec (* i 4) x))

(define-primitive-expander f64vector-ref (vec i)
  (bytevector-ieee-double-native-ref vec (* i 8)))
(define-primitive-expander f64vector-set! (vec i x)
  (bytevector-ieee-double-native-set! vec (* i 8) x))
(define-primitive-expander f64vector-ref (vec i)
  (bytevector-ieee-double-native-ref vec (* i 8)))
(define-primitive-expander f64vector-set! (vec i x)
  (bytevector-ieee-double-native-set! vec (* i 8) x))

(define (chained-comparison-expander prim-name)
  (case-lambda
    ((src) (make-const src #t))
    ((src a) #f)
    ((src a b) #f)
    ((src a b . rest)
     (let* ((b-sym (gensym "b"))
            (b* (make-lexical-ref src 'b b-sym)))
       (make-let src
                 '(b)
                 (list b-sym)
                 (list b)
                 (make-conditional src
                                   (make-primcall src prim-name (list a b*))
                                   (make-primcall src prim-name (cons b* rest))
                                   (make-const src #f)))))))

(for-each (lambda (prim-name)
            (hashq-set! *primitive-expand-table* prim-name
                        (chained-comparison-expander prim-name)))
          '(< > <= >= =))

(define (character-comparison-expander char< <)
  (lambda (src . args)
    (expand-primcall
     (make-primcall src <
                    (map (lambda (arg)
                           (make-primcall src 'char->integer (list arg)))
                         args)))))

(for-each (match-lambda
            ((char< . <)
             (hashq-set! *primitive-expand-table* char<
                         (character-comparison-expander char< <))))
          '((char<? . <)
            (char>? . >)
            (char<=? . <=)
            (char>=? . >=)
            (char=? . =)))

;; Appropriate for use with either 'eqv?' or 'equal?'.
(define (maybe-simplify-to-eq prim)
  (case-lambda
    ((src) (make-const src #t))
    ((src a) (make-const src #t))
    ((src a b)
     ;; Simplify cases where either A or B is constant.
     (define (maybe-simplify a b)
       (and (const? a)
            (let ((v (const-exp a)))
              (and (or (memq v '(#f #t () #nil))
                       (symbol? v)
                       (and (integer? v)
                            (exact? v)
                            (<= v most-positive-fixnum)
                            (>= v most-negative-fixnum)))
                   (make-primcall src 'eq? (list a b))))))
     (or (maybe-simplify a b) (maybe-simplify b a)))
    ((src a b . rest)
     (make-conditional src (make-primcall src prim (list a b))
                       (make-primcall src prim (cons b rest))
                       (make-const src #f)))
    (else #f)))

(hashq-set! *primitive-expand-table* 'eqv?   (maybe-simplify-to-eq 'eqv?))
(hashq-set! *primitive-expand-table* 'equal? (maybe-simplify-to-eq 'equal?))

(define (expand-chained-comparisons prim)
  (case-lambda
    ((src) (make-const src #t))
    ((src a)
     ;; (< x) -> (begin (< x 0) #t).  Residualizes side-effects from x
     ;; and, for numeric comparisons, checks that x is a number.
     (make-seq src
               (make-primcall src prim (list a (make-const src 0)))
               (make-const src #t)))
    ((src a b) #f)
    ((src a b . rest)
     (make-conditional src (make-primcall src prim (list a b))
                       (make-primcall src prim (cons b rest))
                       (make-const src #f)))
    (else #f)))

(for-each (lambda (prim)
            (hashq-set! *primitive-expand-table* prim
                        (expand-chained-comparisons prim)))
 '(< <= = >= > eq?))

(hashq-set! *primitive-expand-table*
            'call-with-prompt
            (case-lambda
              ((src tag thunk handler)
               (make-prompt src #f tag thunk handler))
              (else #f)))

(hashq-set! *primitive-expand-table*
            'abort-to-prompt*
            (case-lambda
              ((src tag tail-args)
               (make-abort src tag '() tail-args))
              (else #f)))
(hashq-set! *primitive-expand-table*
            'abort-to-prompt
            (case-lambda
              ((src tag . args)
               (make-abort src tag args (make-const #f '())))
              (else #f)))
