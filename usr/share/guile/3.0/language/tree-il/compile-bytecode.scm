;;; Lightweight compiler directly from Tree-IL to bytecode

;; Copyright (C) 2020-2021,2023 Free Software Foundation, Inc.

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
;;; This pass converts Tree-IL directly to bytecode.  Whereas first
;;; compiling to CPS will yield better-quality bytecode if the optimizer
;;; is on, this approach is much faster and less memory-hungry.  It's
;;; useful if it's more important to reduce time spent in the compiler
;;; than to have a fast program.
;;;
;;; Code:

(define-module (language tree-il compile-bytecode)
  #:use-module (ice-9 match)
  #:use-module (language bytecode)
  #:use-module (language tree-il)
  #:use-module ((language tree-il primitives) #:select (primitive-module))
  #:use-module ((srfi srfi-1) #:select (filter-map
                                        fold
                                        lset-adjoin lset-union lset-difference))
  #:use-module (srfi srfi-9)
  #:use-module (system base types internal)
  #:use-module (system vm assembler)
  #:export (compile-bytecode))

(define (u6? x)  (and (exact-integer? x) (<= 0 x #x3f)))
(define (u8? x)  (and (exact-integer? x) (<= 0 x #xff)))
(define (u12? x) (and (exact-integer? x) (<= 0 x #xfff)))

(define (emit-box asm dst src)
  (cond
   ((= src dst)
    (emit-mov asm 1 src)
    (emit-box asm dst 1))
   (else
    (let ((tmp 0))
      (emit-allocate-words/immediate asm dst 2)
      (emit-load-u64 asm tmp %tc7-variable)
      (emit-word-set!/immediate asm dst 0 tmp)
      (emit-word-set!/immediate asm dst 1 src)))))
(define (emit-box-set! asm loc val)
  (emit-scm-set!/immediate asm loc 1 val))
(define (emit-box-ref asm dst loc)
  (emit-scm-ref/immediate asm dst loc 1))
(define (emit-cons asm dst car cdr)
  (cond
   ((= car dst)
    (emit-mov asm 1 car)
    (emit-cons asm dst 1 (if (= cdr dst) 1 cdr)))
   ((= cdr dst)
    (emit-mov asm 1 cdr)
    (emit-cons asm dst car 1))
   (else
    (emit-allocate-words/immediate asm dst 2)
    (emit-scm-set!/immediate asm dst 0 car)
    (emit-scm-set!/immediate asm dst 1 cdr))))

(define (emit-cached-module-box asm dst mod name public? bound? tmp)
  (define key (cons mod name))
  (define cached (gensym "cached"))
  (emit-cache-ref asm dst key)
  (emit-heap-object? asm dst)
  (emit-je asm cached)
  (cond
   (bound?
    (let ((name (symbol->string name)))
      (if public?
          (emit-lookup-bound-public asm dst mod name)
          (emit-lookup-bound-private asm dst mod name))))
   (else
    (emit-load-constant asm dst mod)
    (emit-resolve-module asm dst dst public?)
    (emit-load-constant asm tmp name)
    (emit-lookup asm dst dst tmp)))
  (emit-cache-set! asm key dst)
  (emit-label asm cached))
(define (emit-cached-toplevel-box asm dst scope name bound? tmp)
  (define key (cons scope name))
  (define cached (gensym "cached"))
  (emit-cache-ref asm dst key)
  (emit-heap-object? asm dst)
  (emit-je asm cached)
  (emit-cache-ref asm dst scope)
  (emit-load-constant asm tmp name)
  (if bound?
      (emit-lookup-bound asm dst dst tmp)
      (emit-lookup asm dst dst tmp))
  (emit-cache-set! asm key dst)
  (emit-label asm cached))
(define (emit-toplevel-box asm dst name bound? tmp)
  (emit-current-module asm dst)
  (emit-load-constant asm tmp name)
  (if bound?
      (emit-lookup-bound asm dst dst tmp)
      (emit-lookup asm dst dst tmp)))

(define closure-header-words 2)
(define (emit-allocate-closure asm dst nfree label tmp)
  (let ((nwords (+ nfree closure-header-words)))
    (cond
     ((u12? nwords)
      (emit-allocate-words/immediate asm dst nwords))
     (else
      (emit-load-u64 asm tmp nwords)
      (emit-allocate-words asm dst tmp)))
    (emit-load-u64 asm tmp (+ %tc7-program (ash nfree 16)))
    (emit-word-set!/immediate asm dst 0 tmp)
    (emit-load-label asm tmp label)
    (emit-word-set!/immediate asm dst 1 tmp)))
(define (emit-maybe-allocate-closure asm dst nfree label tmp)
  (if (zero? nfree)
      (emit-load-static-procedure asm dst label)
      (emit-allocate-closure asm dst nfree label tmp)))
(define (emit-load-free-variable asm dst src idx tmp)
  (let ((idx (+ idx closure-header-words)))
    (cond
     ((u8? idx)
      (emit-scm-ref/immediate asm dst src idx))
     (else
      (emit-load-u64 asm tmp idx)
      (emit-scm-ref asm dst src tmp)))))
(define (emit-init-free-variable asm closure idx val tmp)
  (let ((idx (+ idx closure-header-words)))
    (cond
     ((u8? idx)
      (emit-scm-set!/immediate asm closure idx val))
     (else
      (emit-load-u64 asm tmp idx)
      (emit-scm-set! asm closure tmp val)))))

(define vector-header-words 1)
(define (emit-allocate-vector asm dst len tmp)
  (let ((nwords (+ len vector-header-words)))
    (cond
     ((u12? nwords)
      (emit-allocate-words/immediate asm dst nwords))
     (else
      (emit-load-u64 asm tmp nwords)
      (emit-allocate-words asm dst tmp)))
    (emit-load-u64 asm tmp (+ %tc7-vector (ash len 8)))
    (emit-word-set!/immediate asm dst 0 tmp)))
(define (emit-vector-init! asm v idx val tmp)
  (let ((idx (+ idx vector-header-words)))
    (cond
     ((u8? idx)
      (emit-scm-set!/immediate asm v idx val))
     (else
      (emit-load-u64 asm tmp idx)
      (emit-scm-set! asm v tmp val)))))

(define struct-header-words 1)
(define (emit-struct-init! asm s idx val tmp)
  (let ((idx (+ idx struct-header-words)))
    (cond
     ((u8? idx)
      (emit-scm-set!/immediate asm s idx val))
     (else
      (emit-load-u64 asm tmp idx)
      (emit-scm-set! asm s tmp val)))))

(define-syntax-rule (define-record-type/keywords rtd
                      make-rtd pred (field getter init) ...)
  (begin
    (define-record-type rtd (%make-rtd field ...) pred (field getter) ...)
    (define* (make-rtd #:key (field init) ...)
      (%make-rtd field ...))))

(define-record-type/keywords <primitive>
  make-primitive
  primitive?
  (name primitive-name (error "name required"))
  (nargs primitive-nargs (error "nargs required"))
  (has-result? primitive-has-result? #f)
  (predicate? primitive-predicate? #f)
  (emit primitive-emitter (error "emitter required"))
  (immediate-in-range? primitive-immediate-in-range-predicate #f)
  (emit/immediate primitive-emitter/immediate #f))

(define *primitives* (make-hash-table))
(define (lookup-primitive name) (hashq-ref *primitives* name))

(define-syntax-rule (define-primitive primitive kw ...)
  (hashq-set! *primitives* 'primitive
              (make-primitive #:name 'primitive kw ...)))

(define-syntax-rule (define-primitives (primitive kw ...) ...)
  (begin (define-primitive primitive kw ...) ...))

(define-primitives
  (+                #:nargs 2 #:has-result? #t #:emit emit-add
                    #:immediate-in-range? u8?
                    #:emit/immediate emit-add/immediate)
  (-                #:nargs 2 #:has-result? #t #:emit emit-sub
                    #:immediate-in-range? u8?
                    #:emit/immediate emit-sub/immediate)

  (*                #:nargs 2 #:has-result? #t #:emit emit-mul)
  (/                #:nargs 2 #:has-result? #t #:emit emit-div)
  (quotient         #:nargs 2 #:has-result? #t #:emit emit-quo)
  (remainder        #:nargs 2 #:has-result? #t #:emit emit-rem)
  (modulo           #:nargs 2 #:has-result? #t #:emit emit-mod)
  (exact->inexact   #:nargs 1 #:has-result? #t #:emit emit-inexact)

  (sqrt             #:nargs 1 #:has-result? #t #:emit emit-sqrt)
  (abs              #:nargs 1 #:has-result? #t #:emit emit-abs)
  (floor            #:nargs 1 #:has-result? #t #:emit emit-floor)
  (ceiling          #:nargs 1 #:has-result? #t #:emit emit-ceiling)
  (sin              #:nargs 1 #:has-result? #t #:emit emit-sin)
  (cos              #:nargs 1 #:has-result? #t #:emit emit-cos)
  (tan              #:nargs 1 #:has-result? #t #:emit emit-tan)
  (asin             #:nargs 1 #:has-result? #t #:emit emit-asin)
  (acos             #:nargs 1 #:has-result? #t #:emit emit-acos)
  (atan             #:nargs 1 #:has-result? #t #:emit emit-atan)
  (atan2            #:nargs 2 #:has-result? #t #:emit emit-atan2)

  (logand           #:nargs 2 #:has-result? #t #:emit emit-logand)
  (logior           #:nargs 2 #:has-result? #t #:emit emit-logior)
  (logxor           #:nargs 2 #:has-result? #t #:emit emit-logxor)
  (logsub           #:nargs 2 #:has-result? #t #:emit emit-logsub)

  (lsh              #:nargs 2 #:has-result? #t #:emit emit-lsh
                    #:immediate-in-range? u6?
                    #:emit/immediate emit-lsh/immediate)
  (rsh              #:nargs 2 #:has-result? #t #:emit emit-rsh
                    #:immediate-in-range? u6?
                    #:emit/immediate emit-rsh/immediate)

  (throw            #:nargs 2                  #:emit emit-throw)
  (throw/value      #:nargs 2                  #:emit #f
                    #:immediate-in-range? (lambda (_) #t)
                    #:emit/immediate emit-throw/value)
  (throw/value+data #:nargs 2                  #:emit #f
                    #:immediate-in-range? (lambda (_) #t)
                    #:emit/immediate emit-throw/value+data)

  (current-thread   #:nargs 2 #:has-result? #t #:emit emit-current-thread)
  (current-module   #:nargs 0 #:has-result? #t #:emit emit-current-module)
  (module-ensure-local-variable! #:nargs 2 #:has-result? #t #:emit emit-define!)
  (builtin-ref      #:nargs 1 #:has-result? #t #:emit #f
                    #:immediate-in-range? (lambda (_) #t)
                    #:emit/immediate emit-builtin-ref)

  (wind             #:nargs 2                  #:emit emit-wind)
  (unwind           #:nargs 0                  #:emit emit-unwind)
  (push-dynamic-state #:nargs 1                #:emit emit-push-dynamic-state)
  (pop-dynamic-state  #:nargs 0                #:emit emit-pop-dynamic-state)
  (push-fluid       #:nargs 2                  #:emit emit-push-fluid)
  (pop-fluid        #:nargs 0                  #:emit emit-pop-fluid)
  (pop-fluid-state  #:nargs 0                  #:emit emit-pop-dynamic-state)
  (fluid-ref        #:nargs 1 #:has-result? #t #:emit emit-fluid-ref)
  (fluid-set!       #:nargs 2                  #:emit emit-fluid-set!)

  (string->number   #:nargs 1 #:has-result? #t #:emit emit-string->number)
  (string->symbol   #:nargs 1 #:has-result? #t #:emit emit-string->symbol)
  (symbol->keyword  #:nargs 1 #:has-result? #t #:emit emit-symbol->keyword)
  (symbol->string   #:nargs 1 #:has-result? #t #:emit emit-symbol->string)

  (class-of         #:nargs 1 #:has-result? #t #:emit emit-class-of)

  (cons             #:nargs 2 #:has-result? #t #:emit emit-cons)
  (car              #:nargs 1 #:has-result? #t #:emit emit-$car)
  (cdr              #:nargs 1 #:has-result? #t #:emit emit-$cdr)
  (set-car!         #:nargs 2                  #:emit emit-$set-car!)
  (set-cdr!         #:nargs 2                  #:emit emit-$set-cdr!)
  
  (box              #:nargs 1 #:has-result? #t #:emit emit-box)
  (variable-ref     #:nargs 1 #:has-result? #t #:emit emit-$variable-ref)
  (variable-set!    #:nargs 2                  #:emit emit-$variable-set!)
  (%variable-ref    #:nargs 1 #:has-result? #t #:emit emit-$variable-ref)
  (%variable-set!   #:nargs 2                  #:emit emit-box-set!)

  (vector-length    #:nargs 1 #:has-result? #t #:emit emit-$vector-length)
  (vector-ref       #:nargs 2 #:has-result? #t #:emit emit-$vector-ref
                    #:immediate-in-range? u8?
                    #:emit/immediate emit-$vector-ref/immediate)
  (vector-set!      #:nargs 3                  #:emit emit-$vector-set!
                    #:immediate-in-range? u8?
                    #:emit/immediate emit-$vector-set!/immediate)
  
  (struct-vtable    #:nargs 1 #:has-result? #t #:emit emit-$struct-vtable)
  (struct-ref       #:nargs 2 #:has-result? #t #:emit emit-$struct-ref
                    #:immediate-in-range? u8?
                    #:emit/immediate emit-$struct-ref/immediate)
  (struct-set!      #:nargs 3                  #:emit emit-$struct-set!
                    #:immediate-in-range? u8?
                    #:emit/immediate emit-$struct-set!/immediate)

  (eq?              #:nargs 2 #:predicate? #t  #:emit (lambda (asm a b kf)
                                                        (emit-eq? asm a b)
                                                        (emit-jne asm kf))
                    #:immediate-in-range? (lambda (x)
                                            (and=>
                                             (scm->immediate-bits x)
                                             (lambda (bits)
                                               (truncate-bits bits 16 #t))))
                    #:emit/immediate (lambda (asm a b kf)
                                       (emit-eq-immediate? asm a b)
                                       (emit-jne asm kf)))
  (<                #:nargs 2 #:predicate? #t  #:emit (lambda (asm a b kf)
                                                        (emit-<? asm a b)
                                                        (emit-jnl asm kf)))
  (<=               #:nargs 2 #:predicate? #t  #:emit (lambda (asm a b kf)
                                                        (emit-<? asm b a)
                                                        (emit-jnge asm kf)))
  (=                #:nargs 2 #:predicate? #t  #:emit (lambda (asm a b kf)
                                                        (emit-=? asm a b)
                                                        (emit-jne asm kf))))

(define (variadic-constructor? name)
  (memq name '(vector list make-struct/simple)))

(define-syntax predicate-emitter
  (lambda (stx)
    (define (id-prepend pre id)
      (datum->syntax id (symbol-append pre (syntax->datum id))))
    (syntax-case stx ()
      ((_ pred)
       #`(lambda (asm a kf)
           (#,(id-prepend 'emit- #'pred) asm a)
           (emit-jne asm kf))))))
(define-syntax define-immediate-type-predicate
  (syntax-rules ()
    ((_ name #f mask tag) #f)
    ((_ name pred mask tag)
     (define-primitive pred #:nargs 1 #:predicate? #t
       #:emit (predicate-emitter pred)))))
(define-syntax-rule (define-heap-type-predicate name pred mask tag)
  (define-primitive pred #:nargs 1 #:predicate? #t
    #:emit (lambda (asm a kf)
             (emit-heap-object? asm a)
             (emit-jne asm kf)
             ((predicate-emitter pred) asm a kf))))

(visit-immediate-tags define-immediate-type-predicate)
(visit-heap-tags define-heap-type-predicate)

(define (canonicalize exp)
  (define (reify-primref src name)
    ;; some are builtin-ref
    (cond
     ((builtin-name->index name)
      => (lambda (idx)
           (make-primcall src 'builtin-ref (list (make-const #f idx)))))
     (else
      (make-module-ref src (primitive-module name) name #t))))
  (define (reify-primcall src name args)
    (make-call src (reify-primref src name) args))
  (define (reify-branch src name args)
    (make-conditional src
                      (make-primcall src name args)
                      (make-const src #t)
                      (make-const src #f)))
  (define (finish-conditional exp)
    (define (true? x) (match x (($ <const> _ val) val) (_ #f)))
    (define (false? x) (match x (($ <const> _ val) (not val)) (_ #f)))
    (define (predicate? name)
      (and=> (lookup-primitive name) primitive-predicate?))

    (match exp
      (($ <conditional> src ($ <conditional> _ test (? true?) (? false?))
          consequent alternate)
       (finish-conditional (make-conditional src test consequent alternate)))
      (($ <conditional> src ($ <conditional> _ test (? false?) (? true?))
          consequent alternate)
       (finish-conditional (make-conditional src test alternate consequent)))
      (($ <conditional> src ($ <primcall> _ (? predicate?)))
       exp)
      (($ <conditional> src test consequent alternate)
       (make-conditional src (make-primcall src 'false? (list test))
                         alternate consequent))))
  (post-order
   (lambda (exp)
     (match exp
       ;; Turn <void> into *unspecified*.
       (($ <void> src) (make-const src *unspecified*))

       ;; Ensure the test of a conditional is a branching primcall.
       (($ <conditional>) (finish-conditional exp))

       ;; Reify primitives.
       (($ <primitive-ref> src name) (reify-primref src name))

       ;; Invert >= and >.
       (($ <primcall> src '>= (a b)) (reify-branch src '<= (list b a)))
       (($ <primcall> src '>  (a b)) (reify-branch src '<  (list b a)))

       ;; For eq? on constants, make the second arg the constant.
       (($ <primcall> src 'eq? ((and a ($ <const>))
                                (and b (not ($ <const>)))))
        (reify-branch src 'eq? (list b a)))

       ;; Simplify "not".
       (($ <primcall> src 'not (x))
        (finish-conditional
         (make-conditional src x (make-const src #f) (make-const src #t))))

       ;; Special cases for variadic list, vector, make-struct/simple.
       (($ <primcall> src (? variadic-constructor?)) exp)

       ;; struct-set! needs to return its value.
       (($ <primcall> src 'struct-set! (x idx v))
        (with-lexicals src (v)
          (make-seq src
                    (make-primcall src 'struct-set! (list x idx v))
                    v)))

       ;; Transform "ash" to lsh / rsh.
       (($ <primcall> src 'ash (x ($ <const> src* (? exact-integer? y))))
        (if (negative? y)
            (make-primcall src 'rsh (list x (make-const src* (- y))))
            (make-primcall src 'lsh (list x (make-const src* y)))))

       ;; (throw key subr msg (list x) (list x))
       (($ <primcall> src 'throw
           (($ <const> _ key) ($ <const> _ subr) ($ <const> _ msg)
            ($ <primcall> _ 'list (x))
            ($ <primcall> _ 'list (x))))
        (make-primcall src 'throw/value+data
                       (list x (make-const #f `#(,key ,subr ,msg)))))

       ;; (throw key subr msg (list x) #f)
       (($ <primcall> src 'throw
           (($ <const> _ key) ($ <const> _ subr) ($ <const> _ msg)
            ($ <primcall> _ 'list (x))
            ($ <const> _ #f)))
        (make-primcall src 'throw/value
                       (list x (make-const #f `#(,key ,subr ,msg)))))

       ;; (throw key arg ...)
       (($ <primcall> src 'throw (key . args))
        (make-primcall src 'throw
                       (list key (make-primcall #f 'list args))))

       (($ <primcall> src 'raise-type-error (($ <const> _ #(subr pos what)) x))
        (define msg
          (format #f "Wrong type argument in position ~a (expecting ~a): ~~S"
                  pos what))
        (make-primcall src 'throw/value+data
                       (list x (make-const #f `#(wrong-type-arg ,subr ,msg)))))

       ;; Now that we handled special cases, ensure remaining primcalls
       ;; are understood by the code generator, and if not, reify them
       ;; as calls.
       (($ <primcall> src name args)
        (or (and=> (lookup-primitive name)
                   (lambda (prim)
                     (and (= (primitive-nargs prim) (length args))
                          (if (primitive-predicate? prim)
                              (reify-branch src name args)
                              exp))))
            (reify-primcall src name args)))

       ;; Add a clause to clauseless lambdas.
       (($ <lambda> src meta #f)
        (make-lambda src meta
                     (make-lambda-case
                      src '() #f #f #f '() '()
                      (make-primcall
                       src 'throw
                       (list (make-const src 'wrong-number-of-args)
                             (make-const src #f)
                             (make-const src "Wrong number of arguments")
                             (make-const src '())
                             (make-const src #f)))
                      #f)))

       ;; Turn <abort> into abort-to-prompt.
       (($ <abort> src tag args ($ <const> _ ()))
        (reify-primcall src 'abort-to-prompt (cons tag args)))
       (($ <abort> src tag args tail)
        (reify-primcall src 'apply
                        (cons* (reify-primref src 'abort-to-prompt)
                               tag
                               (append args (list tail)))))

       ;; Change non-escape-only prompt bodies from being thunks to
       ;; expressions.  (Escape-only prompt bodies are already
       ;; expressions.)
       (($ <prompt> src #f tag body handler)
        (make-prompt src #f tag (make-call src body '()) handler))

       (_ exp)))
   exp))

(define-record-type <closure>
  (make-closure label code module-scope free-vars)
  closure?
  (label closure-label)
  (code closure-code)
  (module-scope closure-module-scope)
  (free-vars closure-free-vars))

;; Identify closures and assigned variables within X.
(define (split-closures exp)
  (define closures '())
  (define assigned (make-hash-table))

  ;; Guile's current semantics are that a toplevel lambda captures a
  ;; reference on the current module, and that all contained lambdas use
  ;; that module to resolve toplevel variables.  The `module-scope'
  ;; parameter of `visit-closure' tracks whether or not we are in a
  ;; toplevel lambda.  Visiting a top-level lambda allocates a new
  ;; module-scope by incrementing this counter.  Visiting a nested
  ;; lambda re-uses the same module-scope.  The code generator will
  ;; associate these ID's with the module that was current at the point
  ;; the top-level lambda is created.
  (define scope-counter 0)

  ;; Compute free variables in X, adding entries to `free-vars' as
  ;; lambdas are seen, and adding set! vars to `assigned'.
  (define (visit-closure exp module-scope)
    (define (visit exp)
      (define (adjoin sym f) (lset-adjoin eq? f sym))
      (define (union f1 f2) (lset-union eq? f1 f2))
      (define (union3 f1 f2 f3) (union f1 (union f2 f3)))
      (define (difference f1 f2) (lset-difference eq? f1 f2))
      (define (visit* xs) (fold (lambda (x free) (union (visit x) free))
                                '() xs))

      (match exp
        (($ <lexical-ref> src name sym)
         (list sym))

        ((or ($ <const>) ($ <module-ref>) ($ <toplevel-ref>))
         '())

        (($ <lambda> src meta body)
         (let* ((module-scope (or module-scope
                                  (let ((scope scope-counter))
                                    (set! scope-counter (1+ scope-counter))
                                    scope)))
                (free (visit-closure body module-scope))
                (label (gensym "closure")))
           (set! closures
                 (cons (make-closure label exp module-scope free)
                       closures))
           free))

        (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
         (union (difference (union (visit* inits) (visit body))
                            gensyms)
                (if alternate
                    (visit alternate)
                    '())))

        (($ <module-set> src mod name public? exp)
         (visit exp))

        (($ <toplevel-set> src mod name exp)
         (visit exp))

        (($ <toplevel-define> src modname name exp)
         (visit exp))

        (($ <call> src proc args)
         (union (visit proc) (visit* args)))

        (($ <primcall> src name args)
         (visit* args))

        (($ <prompt> src escape-only? tag body
            ($ <lambda> hsrc hmeta hclause))
         (union3 (visit tag) (visit body) (visit hclause)))

        (($ <conditional> src test consequent alternate)
         (union3 (visit test) (visit consequent) (visit alternate)))

        (($ <lexical-set> src name gensym exp)
         (hashq-set! assigned gensym #t)
         (adjoin gensym (visit exp)))

        (($ <seq> src head tail)
         (union (visit head) (visit tail)))

        (($ <let> src names syms vals body)
         (union (visit* vals)
                (difference (visit body) syms)))

        (($ <fix> src names gensyms funs body)
         (difference (union (visit* funs) (visit body))
                     gensyms))

        (($ <let-values> src exp body)
         (union (visit exp) (visit body)))))

    (visit exp))

  (match (visit-closure exp #f)
    (()
     (let ()
       (define x-thunk
         (let ((src (tree-il-srcv exp)))
           (make-lambda src '()
                        (make-lambda-case src '() #f #f #f '() '() exp #f))))
       (values (cons (make-closure 'init x-thunk #f '())
                     (reverse closures))
               assigned)))
    (vars
     (error "unexpected free vars" vars))))

(define call-frame-size 3)

(define (compute-frame-size clause)
  "Compute a conservative count of how many stack slots will be needed
in the frame with for the lambda-case clause @var{clause}."
  (define (visit* xs)
    (fold (lambda (x size) (max (visit x) size)) 0 xs))
  (define (visit-args xs)
    (let lp ((i 0) (xs xs))
      (match xs
        (() i)
        ((x . xs)
         (max (+ i (visit x))
              (lp (+ i 1) xs))))))

  ;; Computing a value may require temporaries.  For example, for
  ;; module-ref, we may need a temporary for the module and a temporary
  ;; for the symbol.  Instead of trying to be extraordinarily precise
  ;; about temporary usage in all the different cases, let's just
  ;; reserve 3 temporaries.
  (define temporary-count 3)

  (define (visit exp)
    (match exp
      ((or ($ <const>) ($ <lexical-ref>) ($ <module-ref>) ($ <toplevel-ref>)
           ($ <lambda>))
       1)

      (($ <module-set> src mod name public? exp)
       (+ 1 (visit exp)))
      (($ <toplevel-set> src mod name exp)
       (+ 1 (visit exp)))
      (($ <toplevel-define> src modname name exp)
       (+ 1 (visit exp)))

      (($ <call> src proc args)
       (+ call-frame-size (visit-args (cons proc args))))

      (($ <primcall> src name args)
       (visit-args args))

      (($ <prompt> src escape-only? tag body
          ($ <lambda> hsrc hmeta
              ($ <lambda-case> _ hreq #f hrest #f () hsyms hbody #f)))
       (max (visit tag)
            (visit body)
            (+ (length hsyms) (visit hbody))))

      (($ <conditional> src test consequent alternate)
       (max (visit test) (visit consequent) (visit alternate)))

      (($ <lexical-set> src name gensym exp)
       (+ 1 (visit exp)))

      (($ <seq> src head tail)
       (max (visit head) (visit tail)))

      (($ <let> src names syms vals body)
       (max (visit-args vals)
            (+ (length vals) (visit body))))

      (($ <fix> src names gensyms funs body)
       (+ (length funs) (visit body)))

      (($ <let-values> src exp
          ($ <lambda-case> lsrc req #f rest #f () syms body #f))
       (max (visit exp)
            (+ (length syms) (visit body))))))

  (match clause
    (($ <lambda-case> src req opt rest kw inits syms body alt)
     (+ 1                   ; One slot for the closure.
        (length syms)       ; One slot for each arg.
        (max (visit* inits) ; Prologue.
             (visit body))  ; Body.
        temporary-count)))) ; Temporaries.

(define (sanitize-meta meta)
  (match meta
    (() '())
    (((k . v) . meta)
     (let ((meta (sanitize-meta meta)))
       (case k
         ((maybe-unused) meta)
         (else (acons k v meta)))))))

(define (compile-closure asm closure assigned? lookup-closure)
  (define-record-type <env>
    (make-env prev name id idx closure? boxed? next-local)
    env?

    ;; Outer <env>, or #f.
    (prev env-prev)

    ;; Pretty name of the binding, or #f.
    (name env-name)

    ;; For a lexical (local or closure), its sym.  For temporaries, #f.
    (id env-id)

    ;; For temporary or local, index from SP at which this value can be
    ;; loaded.  Otherwise index from closure.
    (idx env-idx)

    ;; True for closure vars, false otherwise.
    (closure? env-closure?)

    ;; True for boxed vars, false otherwise.  Only lexicals can be boxed.
    (boxed? env-boxed?)

    ;; If another local is pushed on inside this lexical environment,
    ;; where it should be written.  Usually the same as (1- idx) except
    ;; in the case of lexical aliases.  Invariant: no binding in the
    ;; <env> chain has an idx of next-local or lower.  For closure
    ;; bindings, #f.
    (next-local env-next-local))

  (define (lookup-lexical sym env)
    (match env
      (($ <env> prev _ id)
       (if (eq? id sym)
           env
           (lookup-lexical sym prev)))
      (_ (error "sym not found!" sym))))

  (define (compile-body clause module-scope free-vars frame-size)
    (define (push-free-var sym idx env)
      (make-env env sym sym idx #t (assigned? sym) (env-next-local env)))

    (define (push-local name sym env)
      (let ((idx (env-next-local env)))
        (emit-definition asm name (- frame-size idx 1) 'scm)
        (make-env env name sym idx #f (assigned? sym) (1- idx))))

    (define (push-closure env)
      (push-local 'closure #f env))

    (define (push-local-alias name sym idx env)
      (make-env env name sym idx #f #f (env-next-local env)))

    (define (push-temp env)
      (let ((idx (env-next-local env)))
        (make-env env #f #f idx #f #f (1- idx))))

    (define (push-frame env)
      (let lp ((i 0) (env env))
        (if (< i call-frame-size)
            (lp (1+ i) (push-temp env))
            env)))

    (define (create-initial-env names syms free-syms)
      (define (push-free-vars env)
        (let lp ((idx 0) (free free-syms) (env env))
          (match free
            (() env)
            ((sym . free)
             (lp (1+ idx) free
                 (push-free-var sym idx env))))))
      (define frame-base
        (make-env #f 'frame-base #f #f #f #f (- frame-size 1)))
      (fold push-local (push-closure (push-free-vars frame-base)) names syms))

    (define (stack-height-under-local idx)
      (- frame-size idx 1))

    (define (stack-height env)
      (stack-height-under-local (env-next-local env)))

    (define (maybe-cache-module! scope tmp)
      (unless module-scope
        (emit-current-module asm 0)
        (emit-cache-set! asm scope 0)))
            
    (define (maybe-emit-source source)
      (when source (emit-source asm source)))

    (define (init-free-vars dst free-vars env tmp0 tmp1)
      (let lp ((free-idx 0) (free-vars free-vars))
        (unless (null? free-vars)
          (let* ((loc (lookup-lexical (car free-vars) env))
                 (idx (env-idx loc)))
            (cond
             ((env-closure? loc)
              (emit-load-free-variable asm tmp0 (1- frame-size) idx tmp1)
              (emit-init-free-variable asm dst free-idx tmp0 tmp1))
             (else
              (emit-init-free-variable asm dst free-idx idx tmp0))))
          (lp (1+ free-idx) (cdr free-vars)))))
        
    ;; Visit let-values or prompt handler.
    (define (visit-values-handler src req rest syms body env ctx)
      (define (push-bindings names syms env)
        (fold (lambda (name sym env)
                (let ((env (push-local name sym env)))
                  (when (env-boxed? env)
                    (emit-box asm (env-idx env) (env-idx env)))
                  env))
              env names syms))
      (let ((proc-slot (stack-height env))
            (nreq (length req)))
        (maybe-emit-source src)
        (unless (and rest (zero? nreq))
          (emit-receive-values asm proc-slot (->bool rest) nreq))
        (when rest
          (emit-bind-rest asm (+ proc-slot nreq)))
        (emit-reset-frame asm frame-size)
        (let ((names (append req (if rest (list rest) '()))))
          (for-context body (push-bindings names syms env) ctx))))

    (define (visit-prompt exp env ctx)
      (match exp
        (($ <prompt> src escape-only? tag body
            ($ <lambda> hsrc hmeta
               ($ <lambda-case> _ hreq #f hrest #f () hsyms hbody #f)))
         (maybe-emit-source src)
         (let ((tag (env-idx (for-value tag env)))
               (proc-slot (stack-height env))
               (khandler (gensym "handler"))
               (done (gensym "done")))

           (emit-prompt asm tag escape-only? proc-slot khandler)
           (match ctx
             ('tail
              ;; Would be nice if we could invoke the body in true tail
              ;; context, but that's not how it currently is.
              (for-values-at body env 0)
              (emit-unwind asm)
              (emit-handle-interrupts asm)
              (emit-return-values asm))
             (_
              (for-context body env ctx)
              (emit-unwind asm)
              (emit-j asm done)))

           (emit-label asm khandler)
           (visit-values-handler hsrc hreq hrest hsyms hbody env ctx)

           (emit-label asm done)))))

    (define (visit-conditional exp env ctx)
      (match exp
        (($ <conditional> src ($ <primcall> tsrc name args)
            consequent alternate)
         (maybe-emit-source tsrc)
         (let ((prim (lookup-primitive name))
               (kf (gensym "false"))
               (kdone (gensym "done")))
           (define (emit/immediate? val)
             (and=> (primitive-immediate-in-range-predicate prim)
                    (lambda (pred) (pred val))))
           (match args
             ((a ($ <const> _ (? emit/immediate? b)))
              (let ((emit (primitive-emitter/immediate prim)))
                (match (for-args (list a) env)
                  ((a)
                   (maybe-emit-source src)
                   (emit asm a b kf)))))
             (_
              (let ((emit (primitive-emitter prim))
                    (args (for-args args env)))
                (maybe-emit-source src)
                (match args
                  ((a) (emit asm a kf))
                  ((a b) (emit asm a b kf))))))
           (for-context consequent env ctx)
           (unless (eq? ctx 'tail)
             (emit-j asm kdone))
           (emit-label asm kf)
           (for-context alternate env ctx)
           (emit-label asm kdone)))))

    (define (visit-seq exp env ctx)
      (match exp
        (($ <seq> src head tail)
         (maybe-emit-source src)
         (for-effect head env)
         (for-context tail env ctx))))

    (define (visit-let exp env ctx)
      (define (push-bindings names syms vals env)
        (fold (lambda (name sym val env)
                (for-push val env)
                (let ((env (push-local name sym env)))
                  (when (env-boxed? env)
                    (emit-box asm (env-idx env) (env-idx env)))
                  env))
              env names syms vals))
      (match exp
        (($ <let> src names syms vals body)
         (maybe-emit-source src)
         (for-context body (push-bindings names syms vals env) ctx))))

    (define (visit-fix exp env ctx)
      (define (push-bindings names syms vals env)
        (let* ((closures (map lookup-closure vals))
               (env (fold
                     (lambda (name sym closure env)
                       (let ((env (push-local name sym env)))
                         (match closure
                           (($ <closure> label code scope free-vars)
                            ;; FIXME: Allocate one scope per fix.
                            (maybe-cache-module! scope 0)
                            (emit-maybe-allocate-closure
                             asm (env-idx env) (length free-vars) label 0)
                            env))))
                     env names syms closures)))
          (for-each
           (lambda (sym closure)
             (let ((idx (env-idx (lookup-lexical sym env))))
               (match closure
                 (($ <closure> label code scope free-vars)
                  (init-free-vars idx free-vars env 0 1)))))
           syms closures)
          env))
      (match exp
        (($ <fix> src names syms vals body)
         (maybe-emit-source src)
         (for-context body (push-bindings names syms vals env) ctx))))

    (define (visit-let-values exp env ctx)
      (match exp
        (($ <let-values> src exp
            ($ <lambda-case> lsrc req #f rest #f () syms body #f))
         (maybe-emit-source src)
         (for-values exp env)
         (visit-values-handler lsrc req rest syms body env ctx))))

    (define (for-context exp env ctx)
      (match ctx
        ('effect (for-effect exp env))
        ('value (for-value exp env))
        ('tail (for-tail exp env))
        (('value-at . dst) (for-value-at exp env dst))
        (('values-at . height) (for-values-at exp env height))))

    (define (for-args exps env)
      (match exps
        (() '())
        ((exp . exps)
         (let ((env (for-value exp env)))
           (cons (env-idx env) (for-args exps env))))))

    (define (for-effect exp env)
      (match exp
        ((or ($ <lexical-ref>) ($ <const>) ($ <lambda>))
         ;; Nothing to do.
         (values))

        ((or ($ <module-ref>) ($ <toplevel-ref>)
             ($ <primcall> _ (? variadic-constructor?)))
         ;; Cause side effects but ignore value.
         (for-value exp env))

        (($ <lexical-set> src name sym exp)
         (let ((env (for-value exp env)))
           (maybe-emit-source src)
           (match (lookup-lexical sym env)
             (($ <env> _ _ _ idx #t #t) ;; Boxed closure.
              (emit-load-free-variable asm 0 (1- frame-size) idx 0)
              (emit-box-set! asm 0 (env-idx env)))
             (($ <env> _ _ _ idx #f #t) ;; Boxed local.
              (emit-box-set! asm idx (env-idx env))))))

        (($ <module-set> src mod name public? exp)
         (let ((env (for-value exp env)))
           (maybe-emit-source src)
           (emit-cached-module-box asm 0 mod name public? #f 1)
           (emit-box-set! asm 0 (env-idx env))))

        (($ <toplevel-set> src mod name exp)
         (let ((env (for-value exp env)))
           (maybe-emit-source src)
           (if module-scope
               (emit-cached-toplevel-box asm 0 module-scope name #f 1)
               (emit-toplevel-box asm 0 name #f 1))
           (emit-box-set! asm 0 (env-idx env))))

        (($ <toplevel-define> src mod name exp)
         (let ((env (for-value exp env)))
           (maybe-emit-source src)
           (emit-current-module asm 0)
           (emit-load-constant asm 1 name)
           (emit-define! asm 0 0 1)
           (emit-box-set! asm 0 (env-idx env))))

        (($ <call> src proc args)
         (let ((proc-slot (let ((env (push-frame env)))
                            (fold for-push (for-push proc env) args)
                            (stack-height env))))
           (maybe-emit-source src)
           (emit-handle-interrupts asm)
           (emit-call asm proc-slot (1+ (length args)))
           (emit-reset-frame asm frame-size)))

        (($ <primcall> src name args)
         (let ((prim (lookup-primitive name)))
           (define (emit/immediate? val)
             (and=> (primitive-immediate-in-range-predicate prim)
                    (lambda (pred) (pred val))))
           (cond
            ((primitive-has-result? prim)
             (for-value exp env))
            (else
             (match args
               ((a ($ <const> _ (? emit/immediate? b)))
                (let ((emit (primitive-emitter/immediate prim)))
                  (match (for-args (list a) env)
                    ((a)
                     (maybe-emit-source src)
                     (emit asm a b)))))
               ((a ($ <const> _ (? emit/immediate? b)) c)
                (let ((emit (primitive-emitter/immediate prim)))
                  (match (for-args (list a c) env)
                    ((a c)
                     (maybe-emit-source src)
                     (emit asm a b c)))))
               (_
                (let ((emit (primitive-emitter prim))
                      (args (for-args args env)))
                  (maybe-emit-source src)
                  (apply emit asm args))))))))

        (($ <prompt>)       (visit-prompt exp env 'effect))
        (($ <conditional>)  (visit-conditional exp env 'effect))
        (($ <seq>)          (visit-seq exp env 'effect))
        (($ <let>)          (visit-let exp env 'effect))
        (($ <fix>)          (visit-fix exp env 'effect))
        (($ <let-values>)   (visit-let-values exp env 'effect))))

    (define (for-value-at exp env dst)
      ;; The baseline compiler follows a stack discipline: compiling
      ;; temporaries pushes entries on an abstract compile-time stack
      ;; (the "env"), which are then popped as they are used.  Generally
      ;; speaking the "env" is compiled as stack slots: compiling an
      ;; operand pushes on an "env" entry, which increments the current
      ;; stack height, allocating a new slot that is in use by no live
      ;; value.  However since we're targetting a register VM though,
      ;; there are some important optimizations we should make.
      ;;
      ;;  1. In the case of (lambda (x) (+ x x)), we don't want to cause
      ;;     the references to "x" to allocate new stack slots.  We want
      ;;     to emit:
      ;;
      ;;       (add 0 0 0)
      ;;       (return-values)
      ;;
      ;;     and not:
      ;;
      ;;       (mov 1 0)
      ;;       (mov 2 0)
      ;;       (add 0 1 2)
      ;;       (return-values)
      ;;
      ;;     (These examples use FP-relative indexes.)
      ;;
      ;;     This optimization is handled by for-value, which can push
      ;;     on a special "env" that aliases a lexical binding.
      ;;
      ;;  2. Again for (lambda (x) (+ x x)), we want to write the result
      ;;     directly to its destination, which may alias an operand.
      ;;     So we want to avoid this:
      ;;
      ;;       (add 1 0 0)
      ;;       (mov 0 1)
      ;;       (return-values)
      ;;
      ;;     That optimization is implemented by for-value-at and
      ;;     for-values-at.  It works as long as long as the destination
      ;;     is clobbered only after operands are used, so each part of
      ;;     this function has to be careful not to do some kind of
      ;;     multi-part computation that first clobbers "dst" and then
      ;;     reads the operands.
      (match exp
        (($ <lexical-ref> src name sym)
         (maybe-emit-source src)
         (match (lookup-lexical sym env)
           (($ <env> _ _ _ idx #t #t)
            (emit-load-free-variable asm dst (1- frame-size) idx 0)
            (emit-box-ref asm dst dst))
           (($ <env> _ _ _ idx #t #f)
            (emit-load-free-variable asm dst (1- frame-size) idx 0))
           (($ <env> _ _ _ idx #f #t)
            (emit-box-ref asm dst idx))
           (($ <env> _ _ _ idx #f #f)
            (emit-mov asm dst idx))))

        (($ <const> src val)
         (maybe-emit-source src)
         (emit-load-constant asm dst val))

        (($ <module-ref> src mod name public?)
         (maybe-emit-source src)
         (emit-cached-module-box asm 0 mod name public? #t 1)
         (emit-box-ref asm dst 0))

        (($ <toplevel-ref> src mod name)
         (maybe-emit-source src)
         (if module-scope
             (emit-cached-toplevel-box asm 0 module-scope name #t 1)
             (emit-toplevel-box asm 0 name #t 1))
         (emit-box-ref asm dst 0))

        (($ <lambda> src)
         (maybe-emit-source src)
         (match (lookup-closure exp)
           (($ <closure> label code scope free-vars)
            (maybe-cache-module! scope 0)
            (match (length free-vars)
              (0 
               (emit-load-static-procedure asm dst label))
              (nfree
               ;; Stage closure in 0 to avoid stomping captured free
               ;; vars.
               (emit-allocate-closure asm 0 nfree label 1)
               (init-free-vars 0 free-vars env 1 2)
               (emit-mov asm dst 0))))))

        ((or ($ <lexical-set>)
             ($ <module-set>)
             ($ <toplevel-set>)
             ($ <toplevel-define>))
         (for-effect exp env)
         (emit-load-constant asm dst *unspecified*))

        (($ <call> src proc args)
         (let ((proc-slot (let ((env (push-frame env)))
                            (fold for-push (for-push proc env) args)
                            (stack-height env))))
           (maybe-emit-source src)
           (emit-handle-interrupts asm)
           (emit-call asm proc-slot (1+ (length args)))
           (emit-receive asm (stack-height-under-local dst) proc-slot
                         frame-size)))

        (($ <primcall> src (? variadic-constructor? name) args)
         ;; Stage result in 0 to avoid stomping args.
         (let ((args (for-args args env)))
           (maybe-emit-source src)
           (match name
             ('list
              (emit-load-constant asm 0 '())
              (for-each (lambda (arg)
                          (emit-cons asm 0 arg 0))
                        (reverse args)))
             ('vector
              (let ((len (length args)))
                (emit-allocate-vector asm 0 len 1)
                (let lp ((i 0) (args args))
                  (when (< i len)
                    (emit-vector-init! asm 0 i (car args) 1)
                    (lp (1+ i) (cdr args))))))
             ('make-struct/simple
              (match args
                ((vtable . args)
                 (emit-load-constant asm 0 (length args))
                 (emit-$allocate-struct asm 0 vtable 0)
                 (let lp ((i 0) (args args))
                   (match args
                     (() #t)
                     ((arg . args)
                      (emit-struct-init! asm 0 i arg 1)
                      (lp (1+ i) args))))))))
           (emit-mov asm dst 0)))

        (($ <primcall> src name args)
         (let ((prim (lookup-primitive name)))
           (define (emit/immediate? val)
             (and=> (primitive-immediate-in-range-predicate prim)
                    (lambda (pred) (pred val))))
           (cond
            ((not (primitive-has-result? prim))
             (for-effect exp env)
             (emit-load-constant asm dst *unspecified*))
            (else
             (match args
               ((($ <const> _ (? emit/immediate? a)))
                (let* ((emit (primitive-emitter/immediate prim)))
                  (maybe-emit-source src)
                  (emit asm dst a)))
               ((a ($ <const> _ (? emit/immediate? b)))
                (let* ((emit (primitive-emitter/immediate prim))
                       (a (for-value a env)))
                  (maybe-emit-source src)
                  (emit asm dst (env-idx a) b)))
               (_
                (let ((emit (primitive-emitter prim))
                      (args (for-args args env)))
                  (maybe-emit-source src)
                  (apply emit asm dst args))))))))

        (($ <prompt>)       (visit-prompt exp env `(value-at . ,dst)))
        (($ <conditional>)  (visit-conditional exp env `(value-at . ,dst)))
        (($ <seq>)          (visit-seq exp env `(value-at . ,dst)))
        (($ <let>)          (visit-let exp env `(value-at . ,dst)))
        (($ <fix>)          (visit-fix exp env `(value-at . ,dst)))
        (($ <let-values>)   (visit-let-values exp env `(value-at . ,dst)))))

    (define (for-value exp env)
      (match (and (lexical-ref? exp)
                  (lookup-lexical (lexical-ref-gensym exp) env))
        (($ <env> _ name sym idx #f #f)
         (push-local-alias name sym idx env))
        (_
         (for-push exp env))))

    (define (for-push exp env)
      (for-value-at exp env (env-next-local env))
      (push-temp env))

    (define (for-init sym init env)
      (match (lookup-lexical sym env)
        (($ <env> prev name sym idx #f boxed? next-local)
         (when init
           (let ((done (gensym "post-init")))
             (emit-undefined? asm idx)
             (emit-jne asm done)
             (for-value-at init env idx)
             (emit-label asm done)))
         (when boxed?
           (emit-box asm idx idx)))))

    (define (for-values-at exp env height)
      (match exp
        ((or ($ <const>)
             ($ <lexical-ref>)
             ($ <lexical-set>)
             ($ <toplevel-ref>)
             ($ <toplevel-set>)
             ($ <toplevel-define>)
             ($ <module-ref>)
             ($ <module-set>)
             ($ <lambda>)
             ($ <primcall>))
         (for-value-at exp env (- frame-size height 1))
         (emit-reset-frame asm (1+ height)))

        (($ <call> src proc args)
         (let* ((env (push-frame env))
                (from (stack-height env)))
           (fold for-push (for-push proc env) args)
           (maybe-emit-source src)
           (emit-handle-interrupts asm)
           (emit-call asm from (1+ (length args)))
           (unless (= from height)
             (emit-shuffle-down asm from height))))

        (($ <prompt>)       (visit-prompt exp env `(values-at . ,height)))
        (($ <conditional>)  (visit-conditional exp env `(values-at . ,height)))
        (($ <seq>)          (visit-seq exp env `(values-at . ,height)))
        (($ <let>)          (visit-let exp env `(values-at . ,height)))
        (($ <fix>)          (visit-fix exp env `(values-at . ,height)))
        (($ <let-values>)   (visit-let-values exp env `(values-at . ,height)))))

    (define (for-values exp env)
      (for-values-at exp env (stack-height env)))

    (define (for-tail exp env)
      (match exp
        ((or ($ <const>)
             ($ <lexical-ref>)
             ($ <lexical-set>)
             ($ <toplevel-ref>)
             ($ <toplevel-set>)
             ($ <toplevel-define>)
             ($ <module-ref>)
             ($ <module-set>)
             ($ <lambda>)
             ($ <primcall>))
         (for-values-at exp env 0)
         (emit-handle-interrupts asm)
         (emit-return-values asm))

        (($ <call> src proc args)
         (let* ((base (stack-height env))
                (env (fold for-push (for-push proc env) args)))
           (maybe-emit-source src)
           (let lp ((i (length args)) (env env))
             (when (<= 0 i)
               (lp (1- i) (env-prev env))
               (emit-mov asm (+ (env-idx env) base) (env-idx env))))
           (emit-reset-frame asm (+ 1 (length args)))
           (emit-handle-interrupts asm)
           (emit-tail-call asm)))

        (($ <prompt>)       (visit-prompt exp env 'tail))
        (($ <conditional>)  (visit-conditional exp env 'tail))
        (($ <seq>)          (visit-seq exp env 'tail))
        (($ <let>)          (visit-let exp env 'tail))
        (($ <fix>)          (visit-fix exp env 'tail))
        (($ <let-values>)   (visit-let-values exp env 'tail))))

    (match clause
      (($ <lambda-case> src req opt rest kw inits syms body alt)
       (let ((names (append req
                            (or opt '())
                            (if rest (list rest) '())
                            (match kw
                              ((aok? (key name sym) ...) name)
                              (#f '()))))
             (inits (append (make-list (length req) #f)
                            (list-head inits (if opt (length opt) 0))
                            (if rest '(#f) '())
                            (list-tail inits (if opt (length opt) 0)))))
         (unless (= (length names) (length syms) (length inits))
           (error "unexpected args" names syms inits))
         (maybe-emit-source src)
         (let ((env (create-initial-env names syms free-vars)))
           (for-each (lambda (sym init) (for-init sym init env)) syms inits)
           (for-tail body env))))))

  (define (emit-clause label clause module-scope free)
    (let ((frame-size (compute-frame-size clause)))
      (match clause
        (($ <lambda-case> src req opt rest kw inits syms body alt)
         (let ((alt-label (and alt (gensym "clause"))))
           (call-with-values
               (lambda ()
                 (match kw
                   (#f (values #f '()))
                   ((aok? . kw)
                    (values aok?
                            (map (match-lambda
                                  ((key name sym)
                                   (cons key (1+ (list-index syms sym)))))
                                 kw)))))
             (lambda (allow-other-keys? kw-indices)
               (when label (emit-label asm label))
               (let ((has-closure? #t) (opt (or opt '())))
                 (emit-begin-kw-arity asm has-closure? req opt rest kw-indices
                                      allow-other-keys? frame-size alt-label))
               (compile-body clause module-scope free frame-size)
               (emit-end-arity asm)
               (when alt
                 (emit-clause alt-label alt module-scope free)))))))))
  (match closure
    (($ <closure> label ($ <lambda> src meta body) module-scope free)
     (when src (emit-source asm src))
     (emit-begin-program asm label (sanitize-meta meta))
     (emit-clause #f body module-scope free)
     (emit-end-program asm))))

(define (kw-arg-ref args kw default)
  (match (memq kw args)
    ((_ val . _) val)
    (_ default)))

(define (compile-bytecode exp env opts)
  (let* ((exp (canonicalize exp))
         (asm (make-assembler)))
    (call-with-values (lambda () (split-closures exp))
      (lambda (closures assigned)
        (let ((by-code (make-hash-table)))
          (for-each (lambda (closure)
                      (hashq-set! by-code (closure-code closure) closure))
                    closures)
          (define (assigned? sym)
            (hashq-ref assigned sym))
          (define (lookup-closure x)
            (or (hashq-ref by-code x) (error "missing <closure>" x)))
          (for-each (lambda (closure)
                      (compile-closure asm closure assigned? lookup-closure))
                    closures))))
    (values (link-assembly asm #:page-aligned? (kw-arg-ref opts #:to-file? #f))
            env
            env)))
