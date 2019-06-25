;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015, 2017, 2018 Free Software Foundation, Inc.

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
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps primitives)
  #:use-module (language tree-il analyze)
  #:use-module (language tree-il optimize)
  #:use-module (language tree-il)
  #:use-module (language cps intmap)
  #:export (compile-cps))

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

(define (toplevel-box cps src name bound? val-proc)
  (define (lookup cps name bound? k)
    (match (current-topbox-scope)
      (#f
       (with-cps cps
         (build-term ($continue k src
                       ($primcall 'resolve (name bound?))))))
      (scope-id
       (with-cps cps
         ($ (with-cps-constants ((scope scope-id))
              (build-term
               ($continue k src
                 ($primcall 'cached-toplevel-box (scope name bound?))))))))))
  (with-cps cps
    (letv box)
    (let$ body (val-proc box))
    (letk kbox ($kargs ('box) (box) ,body))
    ($ (with-cps-constants ((name name)
                            (bound? bound?))
         ($ (lookup name bound? kbox))))))

(define (module-box cps src module name public? bound? val-proc)
  (with-cps cps
    (letv box)
    (let$ body (val-proc box))
    (letk kbox ($kargs ('box) (box) ,body))
    ($ (with-cps-constants ((module module)
                            (name name)
                            (public? public?)
                            (bound? bound?))
         (build-term ($continue kbox src
                       ($primcall 'cached-module-box
                                  (module name public? bound?))))))))

(define (capture-toplevel-scope cps src scope-id k)
  (with-cps cps
    (letv module)
    (let$ body (with-cps-constants ((scope scope-id))
                 (build-term
                   ($continue k src
                     ($primcall 'cache-current-module! (module scope))))))
    (letk kmodule ($kargs ('module) (module) ,body))
    (build-term ($continue kmodule src
                  ($primcall 'current-module ())))))

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

(define (unbound? cps src var kt kf)
  (define tc8-iflag 4)
  (define unbound-val 9)
  (define unbound-bits (logior (ash unbound-val 8) tc8-iflag))
  (with-cps cps
    ($ (with-cps-constants ((unbound (pointer->scm
                                      (make-pointer unbound-bits))))
         (build-term ($continue kf src
                       ($branch kt ($primcall 'eq? (var unbound)))))))))

(define (init-default-value cps name sym subst init body)
  (match (hashq-ref subst sym)
    ((orig-var subst-var box?)
     (let ((src (tree-il-src init)))
       (define (maybe-box cps k make-body)
         (if box?
             (with-cps cps
               (letv phi)
               (letk kbox ($kargs (name) (phi)
                            ($continue k src ($primcall 'box (phi)))))
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
                 ($ (unbound? src orig-var kunbound kbound)))))))))))

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
             (let$ void (with-cps-constants ((unspecified *unspecified*))
                          (build-term
                            ($continue k src
                              ($primcall 'values (unspecified))))))
             (letk kvoid ($kargs () () ,void))
             kvoid))))))
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
                          (build-term
                            ($continue kargs src ($primcall 'cons (val nil))))))
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
             (letv val)
             (letk kval ($kargs ('val) (val)
                          ($continue k src
                            ($primcall 'values (val)))))
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
      (($ <primcall> src name args)
       (match (prim-instruction name)
         (#f #f)
         (inst
          (match (prim-arity inst)
            ((out . in)
             (and (eqv? out 0)
                  (eqv? in (length args))))))))
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
      (($ <primcall> src name args)
       (match (prim-instruction name)
         (#f #f)
         (inst
          (match (prim-arity inst)
            ((out . in)
             (and (eqv? out 1)
                  (eqv? in (length args))))))))
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
            (build-term ($continue kunboxed src ($primcall 'box-ref (box))))))
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
         (build-term ($continue k #f ($primcall 'box (orig-var))))))
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
         ((orig-var box #t) ($continue k src ($primcall 'box-ref (box))))
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
             (letk kfun ($kfun fun-src meta self ktail kclause))
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
          (build-term ($continue k src ($primcall 'box-ref (box))))))))

    (($ <module-set> src mod name public? exp)
     (convert-arg cps exp
       (lambda (cps val)
         (module-box
          cps src mod name public? #t
          (lambda (cps box)
            (with-cps cps
              (let$ k (adapt-arity k src 0))
              (build-term
                ($continue k src ($primcall 'box-set! (box val))))))))))

    (($ <toplevel-ref> src name)
     (toplevel-box
      cps src name #t
      (lambda (cps box)
        (with-cps cps
          (let$ k (adapt-arity k src 1))
          (build-term ($continue k src ($primcall 'box-ref (box))))))))

    (($ <toplevel-set> src name exp)
     (convert-arg cps exp
       (lambda (cps val)
         (toplevel-box
          cps src name #f
          (lambda (cps box)
            (with-cps cps
              (let$ k (adapt-arity k src 0))
              (build-term
                ($continue k src ($primcall 'box-set! (box val))))))))))

    (($ <toplevel-define> src name exp)
     (convert-arg cps exp
       (lambda (cps val)
         (with-cps cps
           (let$ k (adapt-arity k src 0))
           (letv box)
           (letk kset ($kargs ('box) (box)
                        ($continue k src ($primcall 'box-set! (box val)))))
           ($ (with-cps-constants ((name name))
                (build-term
                  ($continue kset src ($primcall 'define! (name))))))))))

    (($ <call> src proc args)
     (convert-args cps (cons proc args)
       (match-lambda*
         ((cps (proc . args))
          (with-cps cps
            (build-term ($continue k src ($call proc args))))))))

    (($ <primcall> src name args)
     (cond
      ((eq? name 'equal?)
       (convert-args cps args
         (lambda (cps args)
           (with-cps cps
             (let$ k* (adapt-arity k src 1))
             (letk kt ($kargs () () ($continue k* src ($const #t))))
             (letk kf* ($kargs () ()
                         ;; Here we continue to the original $kreceive
                         ;; or $ktail, as equal? doesn't have a VM op.
                         ($continue k src ($primcall 'equal? args))))
             (build-term ($continue kf* src
                           ($branch kt ($primcall 'eqv? args))))))))
      ((branching-primitive? name)
       (convert-args cps args
         (lambda (cps args)
           (with-cps cps
             (let$ k (adapt-arity k src 1))
             (letk kt ($kargs () () ($continue k src ($const #t))))
             (letk kf ($kargs () () ($continue k src ($const #f))))
             (build-term ($continue kf src
                           ($branch kt ($primcall name args))))))))
      ((and (eq? name 'not) (match args ((_) #t) (_ #f)))
       (convert-args cps args
         (lambda (cps args)
           (with-cps cps
             (let$ k (adapt-arity k src 1))
             (letk kt ($kargs () () ($continue k src ($const #f))))
             (letk kf ($kargs () () ($continue k src ($const #t))))
             (build-term ($continue kf src
                           ($branch kt ($values args))))))))
      ((and (eq? name 'list)
            (and-map (match-lambda
                       ((or ($ <const>)
                            ($ <void>)
                            ($ <lambda>)
                            ($ <lexical-ref>)) #t)
                       (_ #f))
                     args))
       ;; See note below in `canonicalize' about `vector'.  The same
       ;; thing applies to `list'.
       (with-cps cps
         (let$ k (adapt-arity k src 1))
         ($ ((lambda (cps)
               (let lp ((cps cps) (args args) (k k))
                 (match args
                   (()
                    (with-cps cps
                      (build-term ($continue k src ($const '())))))
                   ((arg . args)
                    (with-cps cps
                      (letv tail)
                      (let$ body (convert-arg arg
                                   (lambda (cps head)
                                     (with-cps cps
                                       (build-term
                                         ($continue k src
                                           ($primcall 'cons (head tail))))))))
                      (letk ktail ($kargs ('tail) (tail) ,body))
                      ($ (lp args ktail)))))))))))
      ((prim-instruction name)
       => (lambda (instruction)
            (define (box+adapt-arity cps k src out)
              (case instruction
                ((bv-f32-ref bv-f64-ref)
                 (with-cps cps
                   (letv f64)
                   (let$ k (adapt-arity k src out))
                   (letk kbox ($kargs ('f64) (f64)
                                ($continue k src ($primcall 'f64->scm (f64)))))
                   kbox))
                ((char->integer
                  string-length vector-length
                  bv-length bv-u8-ref bv-u16-ref bv-u32-ref bv-u64-ref)
                 (with-cps cps
                   (letv u64)
                   (let$ k (adapt-arity k src out))
                   (letk kbox ($kargs ('u64) (u64)
                                ($continue k src ($primcall 'u64->scm (u64)))))
                   kbox))
                ((bv-s8-ref bv-s16-ref bv-s32-ref bv-s64-ref)
                 (with-cps cps
                   (letv s64)
                   (let$ k (adapt-arity k src out))
                   (letk kbox ($kargs ('s64) (s64)
                                ($continue k src ($primcall 's64->scm (s64)))))
                   kbox))
                (else
                 (adapt-arity cps k src out))))
            (define (unbox-arg cps arg unbox-op have-arg)
              (with-cps cps
                (letv unboxed)
                (let$ body (have-arg unboxed))
                (letk kunboxed ($kargs ('unboxed) (unboxed) ,body))
                (build-term
                  ($continue kunboxed src ($primcall unbox-op (arg))))))
            (define (unbox-args cps args have-args)
              (case instruction
                ((bv-f32-ref bv-f64-ref
                  bv-s8-ref bv-s16-ref bv-s32-ref bv-s64-ref
                  bv-u8-ref bv-u16-ref bv-u32-ref bv-u64-ref)
                 (match args
                   ((bv idx)
                    (unbox-arg
                     cps idx 'scm->u64
                     (lambda (cps idx)
                       (have-args cps (list bv idx)))))))
                ((bv-f32-set! bv-f64-set!)
                 (match args
                   ((bv idx val)
                    (unbox-arg
                     cps idx 'scm->u64
                     (lambda (cps idx)
                       (unbox-arg
                        cps val 'scm->f64
                        (lambda (cps val)
                          (have-args cps (list bv idx val)))))))))
                ((bv-s8-set! bv-s16-set! bv-s32-set! bv-s64-set!)
                 (match args
                   ((bv idx val)
                    (unbox-arg
                     cps idx 'scm->u64
                     (lambda (cps idx)
                       (unbox-arg
                        cps val 'scm->s64
                        (lambda (cps val)
                          (have-args cps (list bv idx val)))))))))
                ((bv-u8-set! bv-u16-set! bv-u32-set! bv-u64-set!)
                 (match args
                   ((bv idx val)
                    (unbox-arg
                     cps idx 'scm->u64
                     (lambda (cps idx)
                       (unbox-arg
                        cps val 'scm->u64
                        (lambda (cps val)
                          (have-args cps (list bv idx val)))))))))
                ((vector-ref struct-ref string-ref)
                 (match args
                   ((obj idx)
                    (unbox-arg
                     cps idx 'scm->u64
                     (lambda (cps idx)
                       (have-args cps (list obj idx)))))))
                ((vector-set! struct-set! string-set!)
                 (match args
                   ((obj idx val)
                    (unbox-arg
                     cps idx 'scm->u64
                     (lambda (cps idx)
                       (have-args cps (list obj idx val)))))))
                ((make-vector)
                 (match args
                   ((length init)
                    (unbox-arg
                     cps length 'scm->u64
                     (lambda (cps length)
                       (have-args cps (list length init)))))))
                ((allocate-struct)
                 (match args
                   ((vtable nfields)
                    (unbox-arg
                     cps nfields 'scm->u64
                     (lambda (cps nfields)
                       (have-args cps (list vtable nfields)))))))
                ((integer->char)
                 (match args
                   ((integer)
                    (unbox-arg
                     cps integer 'scm->u64
                     (lambda (cps integer)
                       (have-args cps (list integer)))))))
                (else (have-args cps args))))
            (convert-args cps args
              (lambda (cps args)
                ;; Tree-IL primcalls are sloppy, in that it could be
                ;; that they are called with too many or too few
                ;; arguments.  In CPS we are more strict and only
                ;; residualize a $primcall if the argument count
                ;; matches.
                (match (prim-arity instruction)
                  ((out . in)
                   (if (= in (length args))
                       (with-cps cps
                         (let$ k (box+adapt-arity k src out))
                         ($ (unbox-args
                             args
                             (lambda (cps args)
                               (with-cps cps
                                 (build-term
                                   ($continue k src
                                     ($primcall instruction args))))))))
                       (with-cps cps
                         (letv prim)
                         (letk kprim ($kargs ('prim) (prim)
                                       ($continue k src ($call prim args))))
                         (build-term ($continue kprim src ($prim name)))))))))))
      (else
       ;; We have something that's a primcall for Tree-IL but not for
       ;; CPS, which will get compiled as a call and so the right thing
       ;; to do is to continue to the given $ktail or $kreceive.
       (convert-args cps args
         (lambda (cps args)
           (with-cps cps
             (build-term
               ($continue k src ($primcall name args)))))))))

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
                   (build-term ($continue kbody src ($prompt #t tag khargs))))
                 (convert-arg cps body
                   (lambda (cps thunk)
                     (with-cps cps
                       (letk kbody ($kargs () ()
                                     ($continue krest (tree-il-src body)
                                       ($primcall 'call-thunk/no-inline
                                                  (thunk)))))
                       (build-term ($continue kbody (tree-il-src body)
                                     ($prompt #f tag khargs))))))))
           (with-cps cps
             (letv prim vals)
             (let$ hbody (convert hbody k subst))
             (let$ hbody (box-bound-vars hnames hsyms hbody))
             (letk khbody ($kargs hnames bound-vars ,hbody))
             (letk khargs ($kreceive hreq hrest khbody))
             (letk kprim ($kargs ('prim) (prim)
                           ($continue k src ($primcall 'apply (prim vals)))))
             (letk kret ($kargs () ()
                          ($continue kprim src ($prim 'values))))
             (letk kpop ($kargs ('rest) (vals)
                          ($continue kret src ($primcall 'unwind ()))))
             ;; FIXME: Attach hsrc to $kreceive.
             (letk krest ($kreceive '() 'rest kpop))
             ($ (convert-body khargs krest)))))))

    (($ <abort> src tag args ($ <const> _ ()))
     (convert-args cps (cons tag args)
       (lambda (cps args*)
         (with-cps cps
           (build-term
             ($continue k src ($primcall 'abort-to-prompt args*)))))))

    (($ <abort> src tag args tail)
     (convert-args cps
         (append (list (make-primitive-ref #f 'abort-to-prompt) tag)
                 args
                 (list tail))
       (lambda (cps args*)
         (with-cps cps
           (build-term ($continue k src ($primcall 'apply args*)))))))

    (($ <conditional> src test consequent alternate)
     (define (convert-test cps test kt kf)
       (match test
         (($ <primcall> src (? branching-primitive? name) args)
          (convert-args cps args
            (lambda (cps args)
              (with-cps cps
                (build-term ($continue kf src
                              ($branch kt ($primcall name args))))))))
         (($ <conditional> src test consequent alternate)
          (with-cps cps
            (let$ t (convert-test consequent kt kf))
            (let$ f (convert-test alternate kt kf))
            (letk kt* ($kargs () () ,t))
            (letk kf* ($kargs () () ,f))
            ($ (convert-test test kt* kf*))))
         (_ (convert-arg cps test
              (lambda (cps test)
                (with-cps cps
                  (build-term ($continue kf src
                                ($branch kt ($values (test)))))))))))
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
                ($continue k src ($primcall 'box-set! (box exp))))))))))

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
                 (scope-counter 0))
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

(define %warning-passes
  `((unused-variable             . ,unused-variable-analysis)
    (unused-toplevel             . ,unused-toplevel-analysis)
    (shadowed-toplevel           . ,shadowed-toplevel-analysis)
    (unbound-variable            . ,unbound-variable-analysis)
    (macro-use-before-definition . ,macro-use-before-definition-analysis)
    (arity-mismatch              . ,arity-analysis)
    (format                      . ,format-analysis)))

(define (optimize-tree-il x e opts)
  (define warnings
    (or (and=> (memq #:warnings opts) cadr)
        '()))

  ;; Go through the warning passes.
  (let ((analyses (filter-map (lambda (kind)
                                (assoc-ref %warning-passes kind))
                              warnings)))
    (analyze-tree analyses x e))

  (optimize x e opts))

(define (canonicalize exp)
  (post-order
   (lambda (exp)
     (match exp
       (($ <primcall> src 'vector
           (and args
                ((or ($ <const>) ($ <void>) ($ <lambda>) ($ <lexical-ref>))
                 ...)))
        ;; Some macros generate calls to "vector" with like 300
        ;; arguments.  Since we eventually compile to make-vector and
        ;; vector-set!, it reduces live variable pressure to allocate the
        ;; vector first, then set values as they are produced, if we can
        ;; prove that no value can capture the continuation.  (More on
        ;; that caveat here:
        ;; http://wingolog.org/archives/2013/11/02/scheme-quiz-time).
        ;;
        ;; Normally we would do this transformation in the compiler, but
        ;; it's quite tricky there and quite easy here, so hold your nose
        ;; while we drop some smelly code.
        (let ((len (length args))
              (v (gensym "v ")))
          (make-let src
                    (list 'v)
                    (list v)
                    (list (make-primcall src 'make-vector
                                         (list (make-const #f len)
                                               (make-const #f #f))))
                    (fold (lambda (arg n tail)
                            (make-seq
                             src
                             (make-primcall
                              src 'vector-set!
                              (list (make-lexical-ref src 'v v)
                                    (make-const #f n)
                                    arg))
                             tail))
                          (make-lexical-ref src 'v v)
                          (reverse args) (reverse (iota len))))))

       (($ <primcall> src 'struct-set! (struct index value))
        ;; Unhappily, and undocumentedly, struct-set! returns the value
        ;; that was set.  There is code that relies on this.  Hackety
        ;; hack...
        (let ((v (gensym "v ")))
          (make-let src
                    (list 'v)
                    (list v)
                    (list value)
                    (make-seq src
                              (make-primcall src 'struct-set!
                                             (list struct
                                                   index
                                                   (make-lexical-ref src 'v v)))
                              (make-lexical-ref src 'v v)))))

       ;; Lower (logand x (lognot y)) to (logsub x y).  We do it here
       ;; instead of in CPS because it gets rid of the lognot entirely;
       ;; if type folding can't prove Y to be an exact integer, then DCE
       ;; would have to leave it in the program for its possible
       ;; effects.
       (($ <primcall> src 'logand (x ($ <primcall> _ 'lognot (y))))
        (make-primcall src 'logsub (list x y)))
       (($ <primcall> src 'logand (($ <primcall> _ 'lognot (y)) x))
        (make-primcall src 'logsub (list x y)))

       (($ <prompt> src escape-only? tag body
           ($ <lambda> hsrc hmeta
              ($ <lambda-case> _ hreq #f hrest #f () hsyms hbody #f)))
        exp)

       ;; Eta-convert prompts without inline handlers.
       (($ <prompt> src escape-only? tag body handler)
        (let ((h (gensym "h "))
              (args (gensym "args ")))
          (make-let
           src (list 'h) (list h) (list handler)
           (make-seq
            src
            (make-conditional
             src
             (make-primcall src 'procedure? (list (make-lexical-ref #f 'h h)))
             (make-void src)
             (make-primcall
              src 'scm-error
              (list
               (make-const #f 'wrong-type-arg)
               (make-const #f "call-with-prompt")
               (make-const #f "Wrong type (expecting procedure): ~S")
               (make-primcall #f 'list (list (make-lexical-ref #f 'h h)))
               (make-primcall #f 'list (list (make-lexical-ref #f 'h h))))))
            (make-prompt
             src escape-only? tag body
             (make-lambda
              src '()
              (make-lambda-case
               src '() #f 'args #f '() (list args)
               (make-primcall
                src 'apply
                (list (make-lexical-ref #f 'h h)
                      (make-lexical-ref #f 'args args)))
               #f)))))))
       (_ exp)))
   exp))

(define (compile-cps exp env opts)
  (values (cps-convert/thunk
           (canonicalize (optimize-tree-il exp env opts)))
          env
          env))

;;; Local Variables:
;;; eval: (put 'convert-arg 'scheme-indent-function 2)
;;; eval: (put 'convert-args 'scheme-indent-function 2)
;;; End:
