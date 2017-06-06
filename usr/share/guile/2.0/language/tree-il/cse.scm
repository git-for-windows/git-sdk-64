;;; Common Subexpression Elimination (CSE) on Tree-IL

;; Copyright (C) 2011, 2012, 2013 Free Software Foundation, Inc.

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

(define-module (language tree-il cse)
  #:use-module (language tree-il)
  #:use-module (language tree-il primitives)
  #:use-module (language tree-il effects)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (cse))

;;;
;;; This pass eliminates common subexpressions in Tree-IL.  It works
;;; best locally -- within a function -- so it is meant to be run after
;;; partial evaluation, which usually inlines functions and so opens up
;;; a bigger space for CSE to work.
;;;
;;; The algorithm traverses the tree of expressions, returning two
;;; values: the newly rebuilt tree, and a "database".  The database is
;;; the set of expressions that will have been evaluated as part of
;;; evaluating an expression.  For example, in:
;;;
;;;   (1- (+ (if a b c) (* x y)))
;;;
;;; We can say that when it comes time to evaluate (1- <>), that the
;;; subexpressions +, x, y, and (* x y) must have been evaluated in
;;; values context.  We know that a was evaluated in test context, but
;;; we don't know if it was true or false.
;;;
;;; The expressions in the database /dominate/ any subsequent
;;; expression: FOO dominates BAR if evaluation of BAR implies that any
;;; effects associated with FOO have already occured.
;;;
;;; When adding expressions to the database, we record the context in
;;; which they are evaluated.  We treat expressions in test context
;;; specially: the presence of such an expression indicates that the
;;; expression is true.  In this way we can elide duplicate predicates.
;;;
;;; Duplicate predicates are not common in code that users write, but
;;; can occur quite frequently in macro-generated code.
;;;
;;; For example:
;;;
;;;   (and (foo? x) (foo-bar x))
;;;   => (if (and (struct? x) (eq? (struct-vtable x) <foo>))
;;;          (if (and (struct? x) (eq? (struct-vtable x) <foo>))
;;;              (struct-ref x 1)
;;;              (throw 'not-a-foo))
;;;          #f))
;;;   => (if (and (struct? x) (eq? (struct-vtable x) <foo>))
;;;          (struct-ref x 1)
;;;          #f)
;;;
;;; A conditional bailout in effect context also has the effect of
;;; adding predicates to the database:
;;;
;;;   (begin (foo-bar x) (foo-baz x))
;;;   => (begin
;;;        (if (and (struct? x) (eq? (struct-vtable x) <foo>))
;;;            (struct-ref x 1)
;;;            (throw 'not-a-foo))
;;;        (if (and (struct? x) (eq? (struct-vtable x) <foo>))
;;;            (struct-ref x 2)
;;;            (throw 'not-a-foo)))
;;;   => (begin
;;;        (if (and (struct? x) (eq? (struct-vtable x) <foo>))
;;;            (struct-ref x 1)
;;;            (throw 'not-a-foo))
;;;        (struct-ref x 2))
;;;
;;; When removing code, we have to ensure that the semantics of the
;;; source program and the residual program are the same.  It's easy to
;;; ensure that they have the same value, because those manipulations
;;; are just algebraic, but the tricky thing is to ensure that the
;;; expressions exhibit the same ordering of effects.  For that, we use
;;; the effects analysis of (language tree-il effects).  We only
;;; eliminate code if the duplicate code commutes with all of the
;;; dominators on the path from the duplicate to the original.
;;;
;;; The implementation uses vhashes as the fundamental data structure.
;;; This can be seen as a form of global value numbering.  This
;;; algorithm currently spends most of its time in vhash-assoc.  I'm not
;;; sure whether that is due to our bad hash function in Guile 2.0, an
;;; inefficiency in vhashes, or what.  Overall though the complexity
;;; should be linear, or N log N -- whatever vhash-assoc's complexity
;;; is.  Walking the dominators is nonlinear, but that only happens when
;;; we've actually found a common subexpression so that should be OK.
;;;

;; Logging helpers, as in peval.
;;
(define-syntax *logging* (identifier-syntax #f))
;; (define %logging #f)
;; (define-syntax *logging* (identifier-syntax %logging))
(define-syntax log
  (syntax-rules (quote)
    ((log 'event arg ...)
     (if (and *logging*
              (or (eq? *logging* #t)
                  (memq 'event *logging*)))
         (log* 'event arg ...)))))
(define (log* event . args)
  (let ((pp (module-ref (resolve-interface '(ice-9 pretty-print))
                        'pretty-print)))
    (pp `(log ,event . ,args))
    (newline)
    (values)))

;; A pre-pass on the source program to determine the set of assigned
;; lexicals.
;;
(define* (build-assigned-var-table exp #:optional (table vlist-null))
  (tree-il-fold
   (lambda (exp res)
     res)
   (lambda (exp res)
     (match exp
       (($ <lexical-set> src name gensym exp)
        (vhash-consq gensym #t res))
       (_ res)))
   (lambda (exp res) res)
   table exp))

(define (boolean-valued-primitive? primitive)
  (or (negate-primitive primitive)
      (eq? primitive 'not)
      (let ((chars (symbol->string primitive)))
        (eqv? (string-ref chars (1- (string-length chars)))
              #\?))))

(define (boolean-valued-expression? x ctx)
  (match x
    (($ <application> _
        ($ <primitive-ref> _ (? boolean-valued-primitive?))) #t)
    (($ <const> _ (? boolean?)) #t)
    (_ (eq? ctx 'test))))

(define (singly-valued-expression? x ctx)
  (match x
    (($ <const>) #t)
    (($ <lexical-ref>) #t)
    (($ <void>) #t)
    (($ <lexical-ref>) #t)
    (($ <primitive-ref>) #t)
    (($ <module-ref>) #t)
    (($ <toplevel-ref>) #t)
    (($ <application> _
        ($ <primitive-ref> _ (? singly-valued-primitive?))) #t)
    (($ <application> _ ($ <primitive-ref> _ 'values) (val)) #t)
    (($ <lambda>) #t)
    (_ (eq? ctx 'value))))

(define* (cse exp)
  "Eliminate common subexpressions in EXP."

  (define assigned-lexical?
    (let ((table (build-assigned-var-table exp)))
      (lambda (sym)
        (vhash-assq sym table))))

  (define %compute-effects
    (make-effects-analyzer assigned-lexical?))

  (define (negate exp ctx)
    (match exp
      (($ <const> src x)
       (make-const src (not x)))
      (($ <void> src)
       (make-const src #f))
      (($ <conditional> src test consequent alternate)
       (make-conditional src test (negate consequent ctx) (negate alternate ctx)))
      (($ <application> _ ($ <primitive-ref> _ 'not)
          ((and x (? (cut boolean-valued-expression? <> ctx)))))
       x)
      (($ <application> src
          ($ <primitive-ref> _ (and pred (? negate-primitive)))
          args)
       (make-application src
                         (make-primitive-ref #f (negate-primitive pred))
                         args))
      (_
       (make-application #f (make-primitive-ref #f 'not) (list exp)))))

  
  (define (hasher n)
    (lambda (x size) (modulo n size)))

  (define (add-to-db exp effects ctx db)
    (let ((v (vector exp effects ctx))
          (h (tree-il-hash exp)))
      (vhash-cons v h db (hasher h))))

  (define (control-flow-boundary db)
    (let ((h (hashq 'lambda most-positive-fixnum)))
      (vhash-cons 'lambda h db (hasher h))))

  (define (find-dominating-expression exp effects ctx db)
    (define (entry-matches? v1 v2)
      (match (if (vector? v1) v1 v2)
        (#(exp* effects* ctx*)
         (and (tree-il=? exp exp*)
              (or (not ctx) (eq? ctx* ctx))))
        (_ #f)))
      
    (let ((len (vlist-length db))
          (h (tree-il-hash exp)))
      (and (vhash-assoc #t db entry-matches? (hasher h))
           (let lp ((n 0))
             (and (< n len)
                  (match (vlist-ref db n)
                    (('lambda . h*)
                     ;; We assume that lambdas can escape and thus be
                     ;; called from anywhere.  Thus code inside a lambda
                     ;; only has a dominating expression if it does not
                     ;; depend on any effects.
                     (and (not (depends-on-effects? effects &all-effects))
                          (lp (1+ n))))
                    ((#(exp* effects* ctx*) . h*)
                     (log 'walk (unparse-tree-il exp) effects
                          (unparse-tree-il exp*) effects* ctx*)
                     (or (and (= h h*)
                              (or (not ctx) (eq? ctx ctx*))
                              (tree-il=? exp exp*))
                         (and (effects-commute? effects effects*)
                              (lp (1+ n)))))))))))

  ;; Return #t if EXP is dominated by an instance of itself.  In that
  ;; case, we can exclude *type-check* effects, because the first
  ;; expression already caused them if needed.
  (define (has-dominating-effect? exp effects db)
    (or (constant? effects)
        (and
         (effect-free?
          (exclude-effects effects
                           (logior &zero-values
                                   &allocation
                                   &type-check)))
         (find-dominating-expression exp effects #f db))))

  (define (find-dominating-test exp effects db)
    (and
     (effect-free?
      (exclude-effects effects (logior &allocation
                                       &type-check)))
     (match exp
       (($ <const> src val)
        (if (boolean? val)
            exp
            (make-const src (not (not val)))))
       ;; For (not FOO), try to prove FOO, then negate the result.
       (($ <application> src ($ <primitive-ref> _ 'not) (exp*))
        (match (find-dominating-test exp* effects db)
          (($ <const> _ val)
           (log 'inferring exp (not val))
           (make-const src (not val)))
          (_
           #f)))
       (_
        (cond
         ((find-dominating-expression exp effects 'test db)
          ;; We have an EXP fact, so we infer #t.
          (log 'inferring exp #t)
          (make-const (tree-il-src exp) #t))
         ((find-dominating-expression (negate exp 'test) effects 'test db)
          ;; We have a (not EXP) fact, so we infer #f.
          (log 'inferring exp #f)
          (make-const (tree-il-src exp) #f))
         (else
          ;; Otherwise we don't know.
          #f))))))

  (define (add-to-env exp name sym db env)
    (let* ((v (vector exp name sym (vlist-length db)))
           (h (tree-il-hash exp)))
      (vhash-cons v h env (hasher h))))

  (define (augment-env env names syms exps db)
    (if (null? names)
        env
        (let ((name (car names)) (sym (car syms)) (exp (car exps)))
          (augment-env (if (or (assigned-lexical? sym)
                               (lexical-ref? exp))
                           env
                           (add-to-env exp name sym db env))
                       (cdr names) (cdr syms) (cdr exps) db))))

  (define (find-dominating-lexical exp effects env db)
    (define (entry-matches? v1 v2)
      (match (if (vector? v1) v1 v2)
        (#(exp* name sym db)
         (tree-il=? exp exp*))
        (_ #f)))
      
    (define (unroll db base n)
      (or (zero? n)
          (match (vlist-ref db base)
            (('lambda . h*)
             ;; See note in find-dominating-expression.
             (and (not (depends-on-effects? effects &all-effects))
                  (unroll db (1+ base) (1- n))))
            ((#(exp* effects* ctx*) . h*)
             (and (effects-commute? effects effects*)
                  (unroll db (1+ base) (1- n)))))))

    (let ((h (tree-il-hash exp)))
      (and (effect-free? (exclude-effects effects &type-check))
           (vhash-assoc exp env entry-matches? (hasher h))
           (let ((env-len (vlist-length env))
                 (db-len (vlist-length db)))
             (let lp ((n 0) (m 0))
               (and (< n env-len)
                    (match (vlist-ref env n)
                      ((#(exp* name sym db-len*) . h*)
                       (let ((niter (- (- db-len db-len*) m)))
                         (and (unroll db m niter)
                              (if (and (= h h*) (tree-il=? exp* exp))
                                  (make-lexical-ref (tree-il-src exp) name sym)
                                  (lp (1+ n) (- db-len db-len*)))))))))))))

  (define (lookup-lexical sym env)
    (let ((env-len (vlist-length env)))
      (let lp ((n 0))
        (and (< n env-len)
             (match (vlist-ref env n)
               ((#(exp _ sym* _) . _)
                (if (eq? sym sym*)
                    exp
                    (lp (1+ n)))))))))

  (define (intersection db+ db-)
    (vhash-fold-right
     (lambda (k h out)
       (if (vhash-assoc k db- equal? (hasher h))
           (vhash-cons k h out (hasher h))
           out))
     vlist-null
     db+))

  (define (concat db1 db2)
    (vhash-fold-right (lambda (k h tail)
                        (vhash-cons k h tail (hasher h)))
                      db2 db1))

  (let visit ((exp   exp)
              (db vlist-null) ; dominating expressions: #(exp effects ctx) -> hash
              (env vlist-null) ; named expressions: #(exp name sym db) -> hash
              (ctx 'values)) ; test, effect, value, or values
    
    (define (parallel-visit exps db env ctx)
      (let lp ((in exps) (out '()) (db* vlist-null))
        (if (pair? in)
            (call-with-values (lambda () (visit (car in) db env ctx))
              (lambda (x db**)
                (lp (cdr in) (cons x out) (concat db** db*))))
            (values (reverse out) db*))))

    (define (compute-effects exp)
      (%compute-effects exp (lambda (sym) (lookup-lexical sym env))))

    (define (bailout? exp)
      (causes-effects? (compute-effects exp) &definite-bailout))

    (define (return exp db*)
      (let ((effects (compute-effects exp)))
        (cond
         ((and (eq? ctx 'effect)
               (not (lambda-case? exp))
               (or (effect-free?
                    (exclude-effects effects
                                     (logior &zero-values
                                             &allocation)))
                   (has-dominating-effect? exp effects db)))
          (cond
           ((void? exp)
            (values exp db*))
           (else
            (log 'elide ctx (unparse-tree-il exp))
            (values (make-void #f) db*))))
         ((and (boolean-valued-expression? exp ctx)
               (find-dominating-test exp effects db))
          => (lambda (exp)
               (log 'propagate-test ctx (unparse-tree-il exp))
               (values exp db*)))
         ((and (singly-valued-expression? exp ctx)
               (find-dominating-lexical exp effects env db))
          => (lambda (exp)
               (log 'propagate-value ctx (unparse-tree-il exp))
               (values exp db*)))
         ((and (constant? effects) (memq ctx '(value values)))
          ;; Adds nothing to the db.
          (values exp db*))
         (else
          (log 'return ctx effects (unparse-tree-il exp) db*)
          (values exp
                  (add-to-db exp effects ctx db*))))))

    (log 'visit ctx (unparse-tree-il exp) db env)

    (match exp
      (($ <const>)
       (return exp vlist-null))
      (($ <void>)
       (return exp vlist-null))
      (($ <lexical-ref> _ _ gensym)
       (return exp vlist-null))
      (($ <lexical-set> src name gensym exp)
       (let*-values (((exp db*) (visit exp db env 'value)))
         (return (make-lexical-set src name gensym exp)
                 db*)))
      (($ <let> src names gensyms vals body)
       (let*-values (((vals db*) (parallel-visit vals db env 'value))
                     ((body db**) (visit body (concat db* db)
                                         (augment-env env names gensyms vals db)
                                         ctx)))
         (return (make-let src names gensyms vals body)
                 (concat db** db*))))
      (($ <letrec> src in-order? names gensyms vals body)
       (let*-values (((vals db*) (parallel-visit vals db env 'value))
                     ((body db**) (visit body (concat db* db)
                                         (augment-env env names gensyms vals db)
                                         ctx)))
         (return (make-letrec src in-order? names gensyms vals body)
                 (concat db** db*))))
      (($ <fix> src names gensyms vals body)
       (let*-values (((vals db*) (parallel-visit vals db env 'value))
                     ((body db**) (visit body (concat db* db) env ctx)))
         (return (make-fix src names gensyms vals body)
                 (concat db** db*))))
      (($ <let-values> src producer consumer)
       (let*-values (((producer db*) (visit producer db env 'values))
                     ((consumer db**) (visit consumer (concat db* db) env ctx)))
         (return (make-let-values src producer consumer)
                 (concat db** db*))))
      (($ <dynwind> src winder body unwinder)
       (let*-values (((pre db*) (visit winder db env 'value))
                     ((body db**) (visit body (concat db* db) env ctx))
                     ((post db***) (visit unwinder db env 'value)))
         (return (make-dynwind src pre body post)
                 (concat db* (concat db** db***)))))
      (($ <dynlet> src fluids vals body)
       (let*-values (((fluids db*) (parallel-visit fluids db env 'value))
                     ((vals db**) (parallel-visit vals db env 'value))
                     ((body db***) (visit body (concat db** (concat db* db))
                                          env ctx)))
         (return (make-dynlet src fluids vals body)
                 (concat db*** (concat db** db*)))))
      (($ <dynref> src fluid)
       (let*-values (((fluid db*) (visit fluid db env 'value)))
         (return (make-dynref src fluid)
                 db*)))
      (($ <dynset> src fluid exp)
       (let*-values (((fluid db*) (visit fluid db env 'value))
                     ((exp db**) (visit exp db env 'value)))
         (return (make-dynset src fluid exp)
                 (concat db** db*))))
      (($ <toplevel-ref>)
       (return exp vlist-null))
      (($ <module-ref>)
       (return exp vlist-null))
      (($ <module-set> src mod name public? exp)
       (let*-values (((exp db*) (visit exp db env 'value)))
         (return (make-module-set src mod name public? exp)
                 db*)))
      (($ <toplevel-define> src name exp)
       (let*-values (((exp db*) (visit exp db env 'value)))
         (return (make-toplevel-define src name exp)
                 db*)))
      (($ <toplevel-set> src name exp)
       (let*-values (((exp db*) (visit exp db env 'value)))
         (return (make-toplevel-set src name exp)
                 db*)))
      (($ <primitive-ref>)
       (return exp vlist-null))
      (($ <conditional> src test consequent alternate)
       (let*-values
           (((test db+) (visit test db env 'test))
            ((converse db-) (visit (negate test 'test) db env 'test))
            ((consequent db++) (visit consequent (concat db+ db) env ctx))
            ((alternate db--) (visit alternate (concat db- db) env ctx)))
         (match (make-conditional src test consequent alternate)
           (($ <conditional> _ ($ <const> _ exp))
            (if exp
                (return consequent (concat db++ db+))
                (return alternate (concat db-- db-))))
           ;; (if FOO A A) => (begin FOO A)
           (($ <conditional> src _
               ($ <const> _ a) ($ <const> _ (? (cut equal? a <>))))
            (visit (make-sequence #f (list test (make-const #f a)))
                   db env ctx))
           ;; (if FOO #t #f) => FOO for boolean-valued FOO.
           (($ <conditional> src
               (? (cut boolean-valued-expression? <> ctx))
               ($ <const> _ #t) ($ <const> _ #f))
            (return test db+))
           ;; (if FOO #f #t) => (not FOO)
           (($ <conditional> src _ ($ <const> _ #f) ($ <const> _ #t))
            (visit (negate test ctx) db env ctx))

           ;; Allow "and"-like conditions to accumulate in test context.
           ((and c ($ <conditional> _ _ _ ($ <const> _ #f)))
            (return c (if (eq? ctx 'test) (concat db++ db+) vlist-null)))
           ((and c ($ <conditional> _ _ ($ <const> _ #f) _))
            (return c (if (eq? ctx 'test) (concat db-- db-) vlist-null)))

           ;; Conditional bailouts turn expressions into predicates.
           ((and c ($ <conditional> _ _ _ (? bailout?)))
            (return c (concat db++ db+)))
           ((and c ($ <conditional> _ _ (? bailout?) _))
            (return c (concat db-- db-)))

           (c
            (return c (intersection (concat db++ db+) (concat db-- db-)))))))
      (($ <application> src proc args)
       (let*-values (((proc db*) (visit proc db env 'value))
                     ((args db**) (parallel-visit args db env 'value)))
         (return (make-application src proc args)
                 (concat db** db*))))
      (($ <lambda> src meta body)
       (let*-values (((body _) (if body
                                   (visit body (control-flow-boundary db)
                                          env 'values)
                                   (values #f #f))))
         (return (make-lambda src meta body)
                 vlist-null)))
      (($ <lambda-case> src req opt rest kw inits gensyms body alt)
       (let*-values (((inits _) (parallel-visit inits db env 'value))
                     ((body db*) (visit body db env ctx))
                     ((alt _) (if alt
                                  (visit alt db env ctx)
                                  (values #f #f))))
         (return (make-lambda-case src req opt rest kw inits gensyms body alt)
                 (if alt vlist-null db*))))
      (($ <sequence> src exps)
       (let lp ((in exps) (out '()) (db* vlist-null))
         (match in
           ((last)
            (let*-values (((last db**) (visit last (concat db* db) env ctx)))
              (if (null? out)
                  (return last (concat db** db*))
                  (return (make-sequence src (reverse (cons last out)))
                          (concat db** db*)))))
           ((head . rest)
            (let*-values (((head db**) (visit head (concat db* db) env 'effect)))
              (cond
               ((sequence? head)
                (lp (append (sequence-exps head) rest) out db*))
               ((void? head)
                (lp rest out db*))
               (else
                (lp rest (cons head out) (concat db** db*)))))))))
      (($ <prompt> src tag body handler)
       (let*-values (((tag db*) (visit tag db env 'value))
                     ((body _) (visit body (concat db* db) env 'values))
                     ((handler _) (visit handler (concat db* db) env ctx)))
         (return (make-prompt src tag body handler)
                 db*)))
      (($ <abort> src tag args tail)
       (let*-values (((tag db*) (visit tag db env 'value))
                     ((args db**) (parallel-visit args db env 'value))
                     ((tail db***) (visit tail db env 'value)))
         (return (make-abort src tag args tail)
                 (concat db* (concat db** db***))))))))
