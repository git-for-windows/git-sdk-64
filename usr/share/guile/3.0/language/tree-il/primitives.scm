;;; open-coding primitive procedures

;; Copyright (C) 2009-2015, 2017-2023 Free Software Foundation, Inc.

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
            negate-primitive
            primitive-module))

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
    + * - / 1- 1+ quotient remainder modulo exact->inexact
    expt
    ash logand logior logxor lognot logtest logbit?
    sqrt abs floor ceiling sin cos tan asin acos atan
    not
    pair? null? list? symbol? vector? string? struct? number? char? nil?
    eof-object?
    bytevector? keyword? bitvector?

    symbol->string string->symbol
    keyword->symbol symbol->keyword

    procedure? thunk?

    complex? real? rational? inf? nan? integer? exact? inexact? even? odd?
    exact-integer?

    char<? char<=? char>=? char>?

    integer->char char->integer number->string string->number

    acons cons cons* append

    list vector

    car cdr
    set-car! set-cdr!

    caar cadr cdar cddr

    caaar caadr cadar caddr cdaar cdadr cddar cdddr

    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

    length

    make-vector vector-length vector-ref vector-set!
    variable? make-variable variable-ref variable-set!
    variable-bound?

    current-module define!

    current-thread fluid-ref fluid-set! with-fluid* with-dynamic-state

    call-with-prompt
    abort-to-prompt* abort-to-prompt
    make-prompt-tag

    throw error scm-error raise-exception

    string-length string-ref string-set!

    string->utf8 string-utf8-length utf8->string

    make-struct/simple struct-vtable struct-ref struct-set!

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
  '(acons cons cons* append list vector make-vector
    make-struct/simple
    make-prompt-tag
    make-variable))

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
    string->utf8 string-utf8-length utf8->string
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
    expt ash logand logior logxor lognot logtest logbit?
    + * - / 1- 1+ sqrt abs quotient remainder modulo exact->inexact
    floor ceiling sin cos tan asin acos atan
    not
    pair? null? nil? list?
    symbol? variable? vector? struct? string? number? char?
    bytevector? keyword? bitvector? atomic-box?
    complex? real? rational? inf? nan? integer? exact? inexact? even? odd?
    exact-integer?
    char<? char<=? char>=? char>?
    integer->char char->integer number->string string->number
    symbol->string string->symbol
    keyword->symbol symbol->keyword
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
    symbol? variable? vector? struct? string? number? char? eof-object?
    exact-integer?
    bytevector? keyword? bitvector?
    procedure? thunk? atomic-box?
    acons cons cons* list vector make-variable))

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
      (match x
        (($ <toplevel-define> src mod name)
         (hashq-set! local-definitions name #t))
        (($ <seq> src head tail)
         (collect-local-definitions head)
         (collect-local-definitions tail))
        (_ #f))))
  
  (post-order
   (lambda (x)
     (or
      (match x
        ;; FIXME: Use `mod' field?
        (($ <toplevel-ref> src mod* name)
         (and=> (and (not (hashq-ref local-definitions name))
                     (hashq-ref *interesting-primitive-vars*
                                (module-variable mod name)))
                (lambda (name) (make-primitive-ref src name))))
        (($ <module-ref> src mod name public?)
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
        (($ <call> src proc args)
         (and (primitive-ref? proc)
              (make-primcall src (primitive-ref-name proc) args)))
        (_ #f))
      x))
   x))



(define (primitive-module name)
  (case name
    ((bytevector?
      bytevector-length

      bytevector-u8-ref bytevector-u8-set!
      bytevector-s8-ref bytevector-s8-set!

      bytevector-u16-ref bytevector-u16-set!
      bytevector-u16-native-ref bytevector-u16-native-set!
      bytevector-s16-ref bytevector-s16-set!
      bytevector-s16-native-ref bytevector-s16-native-set!

      bytevector-u32-ref bytevector-u32-set!
      bytevector-u32-native-ref bytevector-u32-native-set!
      bytevector-s32-ref bytevector-s32-set!
      bytevector-s32-native-ref bytevector-s32-native-set!

      bytevector-u64-ref bytevector-u64-set!
      bytevector-u64-native-ref bytevector-u64-native-set!
      bytevector-s64-ref bytevector-s64-set!
      bytevector-s64-native-ref bytevector-s64-native-set!

      bytevector-ieee-single-ref bytevector-ieee-single-set!
      bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
      bytevector-ieee-double-ref bytevector-ieee-double-set!
      bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!

      string->utf8 utf8->string)
     '(rnrs bytevectors))
    ((atomic-box?
      make-atomic-box atomic-box-ref atomic-box-set!
      atomic-box-swap! atomic-box-compare-and-swap!)
     '(ice-9 atomic))
    ((current-thread) '(ice-9 threads))
    ((class-of) '(oop goops))
    ((u8vector-ref
      u8vector-set! s8vector-ref s8vector-set!
      u16vector-ref u16vector-set! s16vector-ref s16vector-set!
      u32vector-ref u32vector-set! s32vector-ref s32vector-set!
      u64vector-ref u64vector-set! s64vector-ref s64vector-set!
      f32vector-ref f32vector-set! f64vector-ref f64vector-set!)
     '(srfi srfi-4))
    (else '(guile))))



(define *primitive-expand-table* (make-hash-table))

(define (expand-primcall x)
  (match x
    (($ <primcall> src name args)
     (let ((expand (hashq-ref *primitive-expand-table* name)))
       (or (and expand (apply expand src args))
           x)))
    (else x)))

(define (expand-primitives x)
  (pre-order expand-primcall x))

(define-syntax-rule (define-primitive-expander! sym proc)
  (hashq-set! *primitive-expand-table* sym proc))

(define-syntax primitive-expander
  (lambda (stx)
    (define (expand-args args)
      (syntax-case args ()
        (() #''())
        ((a . b) #`(cons #,(expand-expr #'a) #,(expand-args #'b)))
        (a (expand-expr #'a))))
    (define (expand-expr body)
      (syntax-case body (quote)
        (id (identifier? #'id) #'id)
        ((quote x) #'(make-const src 'x))
        ((op . args) #`(make-primcall src 'op #,(expand-args #'args)))
        (x (self-evaluating? (syntax->datum #'x)) #'(make-const src x))))
    (define (match-clauses args+body)
      (syntax-case args+body (if)
        (() '())
        ((args body . args+body)
         (cons #`(args #,(expand-expr #'body))
               (match-clauses #'args+body)))))
    (syntax-case stx ()
      ((_ args+body ...)
       #`(lambda (src . args)
           (match args
             #,@(match-clauses #'(args+body ...))
             (_ #f)))))))

(define-syntax-rule (define-primitive-expander sym . clauses)
  (define-primitive-expander! 'sym (primitive-expander . clauses)))

;; Oddly, scm-error is just an explicitly 5-argument `throw'.  Weird.
(define-primitive-expander scm-error (key who message args data)
  (throw key who message args data))

(define (escape-format-directives str)
  (string-join (string-split str #\~) "~~"))

(define-primitive-expander! 'error
  (match-lambda*
   ((src)
    (make-primcall src 'throw
                   (list (make-const src 'misc-error)
                         (make-const src #f)
                         (make-const src "?")
                         (make-const src #f)
                         (make-const src #f))))
   ((src ($ <const> src2 (? string? message)) . args)
    (let ((msg (string-join (cons (escape-format-directives message)
                                  (make-list (length args) "~S")))))
      (make-primcall src 'throw
                     (list (make-const src 'misc-error)
                           (make-const src #f)
                           (make-const src2 msg)
                           (make-primcall src 'list args)
                           (make-const src #f)))))
   ((src message . args)
    (let ((msg (string-join (cons "~A" (make-list (length args) "~S")))))
      (make-primcall src 'throw
                     (list (make-const src 'misc-error)
                           (make-const src #f)
                           (make-const src msg)
                           (make-primcall src 'list (cons message args))
                           (make-const src #f)))))))

(define-primitive-expander define! (sym val)
  (%variable-set! (module-ensure-local-variable! (current-module) sym) val))

(define-primitive-expander module-define! (mod sym val)
  (%variable-set! (module-ensure-local-variable! mod sym) val))

(define-primitive-expander! 'eof-object?
  (match-lambda*
   ((src obj)
    (make-primcall src 'eq? (list obj (make-const #f the-eof-object))))
   (_ #f)))

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
  
(define-primitive-expander atan
  (x) (atan x)
  (x y) (atan2 x y))

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

(define-primitive-expander! 'make-vector
  (match-lambda*
   ((src len)
    (make-primcall src 'make-vector (list len (make-const src *unspecified*))))
   ((src len init)
    (make-primcall src 'make-vector (list len init)))
   ((src . args)                              ;wrong number of arguments
    #f)))

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

(define-primitive-expander append
  () '()
  (x) (values x)
  (x y) (append x y)
  (x y . rest) (append x (append y . rest)))

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

(define (bind-lexicals src exps k)
  (match exps
    (() (k '()))
    ((exp . exps)
     (with-lexicals src (exp)
       (bind-lexicals src exps (lambda (exps) (k (cons exp exps))))))))

(define (expand-eq prim)
  (case-lambda
    ((src) (make-const src #t))
    ((src a) (make-const src #t))
    ((src a b) #f)
    ((src . args)
     (bind-lexicals
      src args
      (lambda (args)
        (match args
          ((a . args)
           (let lp ((args args))
             (match args
               ((b)
                (make-primcall src prim (list a b)))
               ((b . args)
                (make-conditional src (make-primcall src prim (list a b))
                                  (lp args)
                                  (make-const src #f))))))))))))

(define-primitive-expander! 'eq?    (expand-eq 'eq?))
(define-primitive-expander! 'eqv?   (expand-eq 'eqv?))
(define-primitive-expander! 'equal? (expand-eq 'equal?))

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
    ((src . args)
     (bind-lexicals
      src args
      (lambda (args)
        (let lp ((args args))
          (match args
            ((a b)
             (make-primcall src prim (list a b)))
            ((a b . args)
             (make-conditional src (make-primcall src prim (list a b))
                               (lp (cons b args))
                               (make-const src #f))))))))
    (else #f)))

(for-each (lambda (prim)
            (define-primitive-expander! prim
              (expand-chained-comparisons prim)))
 '(< <= = >= > eq?))

(define (character-comparison-expander char< <)
  (lambda (src . args)
    (expand-primcall
     (make-primcall src <
                    (map (lambda (arg)
                           (make-primcall src 'char->integer (list arg)))
                         args)))))

(for-each (match-lambda
            ((char< . <)
             (define-primitive-expander! char<
               (character-comparison-expander char< <))))
          '((char<? . <)
            (char>? . >)
            (char<=? . <=)
            (char>=? . >=)
            (char=? . =)))

(define-primitive-expander! 'call-with-prompt
  (case-lambda
   ((src tag thunk handler)
    (match handler
      (($ <lambda> _ _ ($ <lambda-case> _ _ #f _ #f () _ _ #f))
       (make-prompt src #f tag thunk handler))
      (_
       ;; Eta-convert prompts without inline handlers.
       (let ((h (gensym "h "))
             (args (gensym "args ")))
         (define-syntax-rule (primcall name . args)
           (make-primcall src 'name (list . args)))
         (define-syntax-rule (const val)
           (make-const src val))
         (with-lexicals src (handler)
           (make-conditional
            src
            (primcall procedure? handler)
            (make-prompt
             src #f tag thunk
             (make-lambda
              src '()
              (make-lambda-case
               src '() #f 'args #f '() (list args)
               (primcall apply handler (make-lexical-ref #f 'args args))
               #f)))
            (primcall raise-type-error
                      (const #("call-with-prompt" 3 "procedure"))
                      handler)))))))
   (else #f)))

(define-primitive-expander! 'abort-to-prompt*
  (case-lambda
   ((src tag tail-args)
    (make-abort src tag '() tail-args))
   (else #f)))
(define-primitive-expander! 'abort-to-prompt
  (case-lambda
   ((src tag . args)
    (make-abort src tag args (make-const #f '())))
   (else #f)))
