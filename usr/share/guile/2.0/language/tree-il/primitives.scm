;;; open-coding primitive procedures

;; Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

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
  #:use-module (rnrs bytevectors)
  #:use-module (system base syntax)
  #:use-module (language tree-il)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-16)
  #:export (resolve-primitives! add-interesting-primitive!
            expand-primitives!
            effect-free-primitive? effect+exception-free-primitive?
            constructor-primitive? accessor-primitive?
            singly-valued-primitive? bailout-primitive?
            negate-primitive))

;; When adding to this, be sure to update *multiply-valued-primitives*
;; if appropriate.
(define *interesting-primitive-names*
  '(apply @apply
    call-with-values @call-with-values
    call-with-current-continuation @call-with-current-continuation
    call/cc
    dynamic-wind
    @dynamic-wind
    values
    eq? eqv? equal?
    memq memv
    = < > <= >= zero? positive? negative?
    + * - / 1- 1+ quotient remainder modulo
    ash logand logior logxor lognot
    not
    pair? null? list? symbol? vector? string? struct? number? char?

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

    vector-ref vector-set!
    variable-ref variable-set!
    variable-bound?

    fluid-ref fluid-set!

    @prompt call-with-prompt @abort abort-to-prompt
    make-prompt-tag

    throw error scm-error

    string-length string-ref string-set!

    struct-vtable make-struct struct-ref struct-set!

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
  '(acons cons cons* list vector make-struct make-struct/no-tail
    make-prompt-tag))

(define *primitive-accessors*
  ;; Primitives that are pure, but whose result depends on the mutable
  ;; memory pointed to by their operands.
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
    ash logand logior logxor lognot
    + * - / 1- 1+ quotient remainder modulo
    not
    pair? null? list? symbol? vector? struct? string? number? char?
    complex? real? rational? inf? nan? integer? exact? inexact? even? odd?
    char<? char<=? char>=? char>?
    integer->char char->integer number->string string->number
    struct-vtable
    string-length
    ;; These all should get expanded out by expand-primitives!.
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
    pair? null? list? symbol? vector? struct? string? number? char?
    acons cons cons* list vector))

;; Primitives that don't always return one value.
(define *multiply-valued-primitives* 
  '(apply @apply
    call-with-values @call-with-values
    call-with-current-continuation @call-with-current-continuation
    call/cc
    dynamic-wind
    @dynamic-wind
    values
    @prompt call-with-prompt @abort abort-to-prompt))

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

(define *effect-free-primitive-table* (make-hash-table))
(define *effect+exceptions-free-primitive-table* (make-hash-table))
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
(define (accessor-primitive? prim)
  (memq prim *primitive-accessors*))
(define (effect-free-primitive? prim)
  (hashq-ref *effect-free-primitive-table* prim))
(define (effect+exception-free-primitive? prim)
  (hashq-ref *effect+exceptions-free-primitive-table* prim))
(define (singly-valued-primitive? prim)
  (not (hashq-ref *multiply-valued-primitive-table* prim)))
(define (bailout-primitive? prim)
  (hashq-ref *bailout-primitive-table* prim))
(define (negate-primitive prim)
  (hashq-ref *negatable-primitive-table* prim))

(define (resolve-primitives! x mod)
  (post-order!
   (lambda (x)
     (record-case x
       ((<toplevel-ref> src name)
        (and=> (hashq-ref *interesting-primitive-vars*
                          (module-variable mod name))
               (lambda (name) (make-primitive-ref src name))))
       ((<module-ref> src mod name public?)
        (and=> (and=> (resolve-module mod)
                      (if public?
                          module-public-interface
                          identity))
               (lambda (m)
                 (and=> (hashq-ref *interesting-primitive-vars*
                                   (module-variable m name))
                        (lambda (name)
                          (make-primitive-ref src name))))))
       (else #f)))
   x))



(define *primitive-expand-table* (make-hash-table))

(define (expand-primitives! x)
  (pre-order!
   (lambda (x)
     (record-case x
       ((<application> src proc args)
        (and (primitive-ref? proc)
             (let ((expand (hashq-ref *primitive-expand-table*
                                      (primitive-ref-name proc))))
               (and expand (apply expand src args)))))
       (else #f)))
   x))

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
                           `(make-application src (make-primitive-ref src ',(caar in))
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
         `(make-application src (make-primitive-ref src ',(car exp))
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

(define-primitive-expander +
  () 0
  (x) (values x)
  (x y) (if (and (const? y) (eqv? (const-exp y) 1))
            (1+ x)
            (if (and (const? y) (eqv? (const-exp y) -1))
                (1- x)
                (if (and (const? x) (eqv? (const-exp x) 1))
                    (1+ y)
                    (if (and (const? x) (eqv? (const-exp x) -1))
                        (1- y)
                        (+ x y)))))
  (x y z ... last) (+ (+ x y . z) last))

(define-primitive-expander *
  () 1
  (x) (values x)
  (x y z ... last) (* (* x y . z) last))
  
(define-primitive-expander -
  (x) (- 0 x)
  (x y) (if (and (const? y) (eqv? (const-exp y) 1))
            (1- x)
            (- x y))
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

(define-primitive-expander apply (f a0 . args)
  (@apply f a0 . args))

(define-primitive-expander call-with-values (producer consumer)
  (@call-with-values producer consumer))

(define-primitive-expander call-with-current-continuation (proc)
  (@call-with-current-continuation proc))

(define-primitive-expander call/cc (proc)
  (@call-with-current-continuation proc))

(define-primitive-expander make-struct (vtable tail-size . args)
  (if (and (const? tail-size)
           (let ((n (const-exp tail-size)))
             (and (number? n) (exact? n) (zero? n))))
      (make-struct/no-tail vtable . args)
      #f))

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
     (let* ((prim (make-primitive-ref src prim-name))
            (b-sym (gensym "b"))
            (b* (make-lexical-ref src 'b b-sym)))
       (make-let src
                 '(b)
                 (list b-sym)
                 (list b)
                 (make-conditional src
                                   (make-application src prim (list a b*))
                                   (make-application src prim (cons b* rest))
                                   (make-const src #f)))))))

(for-each (lambda (prim-name)
            (hashq-set! *primitive-expand-table* prim-name
                        (chained-comparison-expander prim-name)))
          '(< > <= >= =))

;; Appropriate for use with either 'eqv?' or 'equal?'.
(define maybe-simplify-to-eq
  (case-lambda
    ((src a b)
     ;; Simplify cases where either A or B is constant.
     (define (maybe-simplify a b)
       (and (const? a)
            (let ((v (const-exp a)))
              (and (or (memq v '(#f #t () #nil))
                       (symbol? v)
                       (and (integer? v)
                            (exact? v)
                            (<= most-negative-fixnum v most-positive-fixnum)))
                   (make-application src (make-primitive-ref #f 'eq?)
                                     (list a b))))))
     (or (maybe-simplify a b) (maybe-simplify b a)))
    (else #f)))

(hashq-set! *primitive-expand-table* 'eqv?   maybe-simplify-to-eq)
(hashq-set! *primitive-expand-table* 'equal? maybe-simplify-to-eq)

(hashq-set! *primitive-expand-table*
            'dynamic-wind
            (case-lambda
              ((src pre thunk post)
               (let ((PRE (gensym "pre-"))
                     (THUNK (gensym "thunk-"))
                     (POST (gensym "post-")))
                 (make-let
                  src
                  '(pre thunk post)
                  (list PRE THUNK POST)
                  (list pre thunk post)
                  (make-dynwind
                   src
                   (make-lexical-ref #f 'pre PRE)
                   (make-application #f (make-lexical-ref #f 'thunk THUNK) '())
                   (make-lexical-ref #f 'post POST)))))
              (else #f)))

(hashq-set! *primitive-expand-table*
            '@dynamic-wind
            (case-lambda
              ((src pre expr post)
               (let ((PRE (gensym "pre-"))
                     (POST (gensym "post-")))
                 (make-let
                  src
                  '(pre post)
                  (list PRE POST)
                  (list pre post)
                  (make-dynwind
                   src
                   (make-lexical-ref #f 'pre PRE)
                   expr
                   (make-lexical-ref #f 'post POST)))))))

(hashq-set! *primitive-expand-table*
            'fluid-ref
            (case-lambda
              ((src fluid) (make-dynref src fluid))
              (else #f)))

(hashq-set! *primitive-expand-table*
            'fluid-set!
            (case-lambda
              ((src fluid exp) (make-dynset src fluid exp))
              (else #f)))

(hashq-set! *primitive-expand-table*
            '@prompt
            (case-lambda
              ((src tag exp handler)
               (let ((args-sym (gensym)))
                 (make-prompt
                  src tag exp
                  ;; If handler itself is a lambda, the inliner can do some
                  ;; trickery here.
                  (make-lambda-case
                   (tree-il-src handler) '() #f 'args #f '() (list args-sym)
                   (make-application #f (make-primitive-ref #f 'apply)
                                     (list handler
                                           (make-lexical-ref #f 'args args-sym)))
                   #f))))
              (else #f)))

(hashq-set! *primitive-expand-table*
            'call-with-prompt
            (case-lambda
              ((src tag thunk handler)
               (let ((handler-sym (gensym))
                     (args-sym (gensym)))
                 (make-let
                  src '(handler) (list handler-sym) (list handler)
                  (make-prompt
                   src tag (make-application #f thunk '())
                   ;; If handler itself is a lambda, the inliner can do some
                   ;; trickery here.
                   (make-lambda-case
                    (tree-il-src handler) '() #f 'args #f '() (list args-sym)
                    (make-application
                     #f (make-primitive-ref #f 'apply)
                     (list (make-lexical-ref #f 'handler handler-sym)
                           (make-lexical-ref #f 'args args-sym)))
                    #f)))))
              (else #f)))

(hashq-set! *primitive-expand-table*
            '@abort
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
