;;; Tree-IL partial evaluator

;; Copyright (C) 2011-2014,2017,2019-2024 Free Software Foundation, Inc.

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

(define-module (language tree-il peval)
  #:use-module (language tree-il)
  #:use-module (language tree-il primitives)
  #:use-module (language tree-il effects)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (system base target)
  #:use-module (ice-9 control)
  #:export (peval))

;;;
;;; Partial evaluation is Guile's most important source-to-source
;;; optimization pass.  It performs copy propagation, dead code
;;; elimination, inlining, and constant folding, all while preserving
;;; the order of effects in the residual program.
;;;
;;; For more on partial evaluation, see William Cook’s excellent
;;; tutorial on partial evaluation at DSL 2011, called “Build your own
;;; partial evaluator in 90 minutes”[0].
;;;
;;; Our implementation of this algorithm was heavily influenced by
;;; Waddell and Dybvig's paper, "Fast and Effective Procedure Inlining",
;;; IU CS Dept. TR 484.
;;;
;;; [0] http://www.cs.utexas.edu/~wcook/tutorial/.  
;;;

;; First, some helpers.
;;
(define-syntax *logging* (identifier-syntax #f))

;; For efficiency we define *logging* to inline to #f, so that the call
;; to log* gets optimized out.  If you want to log, uncomment these
;; lines:
;;
;; (define %logging #f)
;; (define-syntax *logging* (identifier-syntax %logging))
;;
;; Then you can change %logging at runtime.

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

(define (tree-il-any proc exp)
  (let/ec k
    (tree-il-fold (lambda (exp res)
                    (let ((res (proc exp)))
                      (if res (k res) #f)))
                  (lambda (exp res) #f)
                  #f exp)))

(define (vlist-any proc vlist)
  (let ((len (vlist-length vlist)))
    (let lp ((i 0))
      (and (< i len)
           (or (proc (vlist-ref vlist i))
               (lp (1+ i)))))))

(define (singly-valued-expression? exp)
  (match exp
    (($ <const>) #t)
    (($ <void>) #t)
    (($ <lexical-ref>) #t)
    (($ <primitive-ref>) #t)
    (($ <module-ref>) #t)
    (($ <toplevel-ref>) #t)
    (($ <primcall> _ (? singly-valued-primitive?)) #t)
    (($ <primcall> _ 'values (val)) #t)
    (($ <lambda>) #t)
    (($ <conditional> _ test consequent alternate)
     (and (singly-valued-expression? consequent)
          (singly-valued-expression? alternate)))
    (else #f)))

(define (truncate-values x)
  "Discard all but the first value of X."
  (if (singly-valued-expression? x)
      x
      (make-primcall (tree-il-srcv x) 'values (list x))))

;; Peval will do a one-pass analysis on the source program to determine
;; the set of assigned lexicals, and to identify unreferenced and
;; singly-referenced lexicals.
;;
(define-record-type <var>
  (make-var name gensym refcount set?)
  var?
  (name var-name)
  (gensym var-gensym)
  (refcount var-refcount set-var-refcount!)
  (set? var-set? set-var-set?!))

(define* (build-var-table exp #:optional (table vlist-null))
  (tree-il-fold
   (lambda (exp res)
     (match exp
       (($ <lexical-ref> src name gensym)
        (let ((var (cdr (vhash-assq gensym res))))
          (set-var-refcount! var (1+ (var-refcount var)))
          res))
       (($ <lambda-case> src req opt rest kw init gensyms body alt)
        (fold (lambda (name sym res)
                (vhash-consq sym (make-var name sym 0 #f) res))
              res
              (append req (or opt '()) (if rest (list rest) '())
                      (match kw
                        ((aok? (kw name sym) ...) name)
                        (_ '())))
              gensyms))
       (($ <let> src names gensyms vals body)
        (fold (lambda (name sym res)
                (vhash-consq sym (make-var name sym 0 #f) res))
              res names gensyms))
       (($ <letrec>)
        (error "unexpected letrec"))
       (($ <fix> src names gensyms vals body)
        (fold (lambda (name sym res)
                (vhash-consq sym (make-var name sym 0 #f) res))
              res names gensyms))
       (($ <lexical-set> src name gensym exp)
        (set-var-set?! (cdr (vhash-assq gensym res)) #t)
        res)
       (_ res)))
   (lambda (exp res) res)
   table exp))

(define (augment-var-table-with-externally-introduced-lexicals exp table)
  "Take the previously computed var table TABLE and the term EXP and
return a table augmented with the lexicals bound in EXP which are not
present in TABLE.  This is used for the result of `expand-primcalls`,
which may introduce new lexicals if a subexpression needs to be
referenced multiple times."
  (define (maybe-add-var name sym table)
    ;; Use a refcount of 2 to prevent the copy-single optimization.
    (define refcount 2)
    (define assigned? #f)
    (if (vhash-assq sym table)
        table
        (vhash-consq sym (make-var name sym refcount assigned?) table)))
  (tree-il-fold
   (lambda (exp table)
     (match exp
       (($ <lambda-case> src req opt rest kw init gensyms body alt)
        (fold maybe-add-var table
              (append req (or opt '()) (if rest (list rest) '())
                      (match kw
                        ((aok? (kw name sym) ...) name)
                        (_ '())))
              gensyms))
       (($ <let> src names gensyms vals body)
        (fold maybe-add-var table names gensyms))
       (($ <letrec>)
        (error "unexpected letrec"))
       (($ <fix> src names gensyms vals body)
        (fold maybe-add-var table names gensyms))
       (_ table)))
   (lambda (exp table) table)
   table exp))

;; Counters are data structures used to limit the effort that peval
;; spends on particular inlining attempts.  Each call site in the source
;; program is allocated some amount of effort.  If peval exceeds the
;; effort counter while attempting to inline a call site, it aborts the
;; inlining attempt and residualizes a call instead.
;;
;; As there is a fixed number of call sites, that makes `peval' O(N) in
;; the number of call sites in the source program.
;;
;; Counters should limit the size of the residual program as well, but
;; currently this is not implemented.
;;
;; At the top level, before seeing any peval call, there is no counter,
;; because inlining will terminate as there is no recursion.  When peval
;; sees a call at the top level, it will make a new counter, allocating
;; it some amount of effort and size.
;;
;; This top-level effort counter effectively "prints money".  Within a
;; toplevel counter, no more effort is printed ex nihilo; for a nested
;; inlining attempt to proceed, effort must be transferred from the
;; toplevel counter to the nested counter.
;;
;; Via `data' and `prev', counters form a linked list, terminating in a
;; toplevel counter.  In practice `data' will be the a pointer to the
;; source expression of the procedure being inlined.
;;
;; In this way peval can detect a recursive inlining attempt, by walking
;; back on the `prev' links looking for matching `data'.  Recursive
;; counters receive a more limited effort allocation, as we don't want
;; to spend all of the effort for a toplevel inlining site on loops.
;; Also, recursive counters don't need a prompt at each inlining site:
;; either the call chain folds entirely, or it will be residualized at
;; its original call.
;;
(define-record-type <counter>
  (%make-counter effort size continuation recursive? data prev)
  counter?
  (effort effort-counter)
  (size size-counter)
  (continuation counter-continuation)
  (recursive? counter-recursive? set-counter-recursive?!)
  (data counter-data)
  (prev counter-prev))

(define (abort-counter c)
  ((counter-continuation c)))

(define (record-effort! c)
  (let ((e (effort-counter c)))
    (if (zero? (variable-ref e))
        (abort-counter c)
        (variable-set! e (1- (variable-ref e))))))

(define (record-size! c)
  (let ((s (size-counter c)))
    (if (zero? (variable-ref s))
        (abort-counter c)
        (variable-set! s (1- (variable-ref s))))))

(define (find-counter data counter)
  (and counter
       (if (eq? data (counter-data counter))
           counter
           (find-counter data (counter-prev counter)))))

(define* (transfer! from to #:optional
                    (effort (variable-ref (effort-counter from)))
                    (size (variable-ref (size-counter from))))
  (define (transfer-counter! from-v to-v amount)
    (let* ((from-balance (variable-ref from-v))
           (to-balance (variable-ref to-v))
           (amount (min amount from-balance)))
      (variable-set! from-v (- from-balance amount))
      (variable-set! to-v (+ to-balance amount))))

  (transfer-counter! (effort-counter from) (effort-counter to) effort)
  (transfer-counter! (size-counter from) (size-counter to) size))

(define (make-top-counter effort-limit size-limit continuation data)
  (%make-counter (make-variable effort-limit)
                 (make-variable size-limit)
                 continuation
                 #t
                 data
                 #f))

(define (make-nested-counter continuation data current)
  (let ((c (%make-counter (make-variable 0)
                          (make-variable 0)
                          continuation
                          #f
                          data
                          current)))
    (transfer! current c)
    c))

(define (make-recursive-counter effort-limit size-limit orig current)
  (let ((c (%make-counter (make-variable 0)
                          (make-variable 0)
                          (counter-continuation orig)
                          #t
                          (counter-data orig)
                          current)))
    (transfer! current c effort-limit size-limit)
    c))

;; Operand structures allow bindings to be processed lazily instead of
;; eagerly.  By doing so, hopefully we can get process them in a way
;; appropriate to their use contexts.  Operands also prevent values from
;; being visited multiple times, wasting effort.
;;
;; TODO: Record value size in operand structure?
;; 
(define-record-type <operand>
  (%make-operand var sym visit source visit-count use-count
                 copyable? residual-value constant-value alias)
  operand?
  (var operand-var)
  (sym operand-sym)
  (visit %operand-visit)
  (source operand-source)
  (visit-count operand-visit-count set-operand-visit-count!)
  (use-count operand-use-count set-operand-use-count!)
  (copyable? operand-copyable? set-operand-copyable?!)
  (residual-value operand-residual-value %set-operand-residual-value!)
  (constant-value operand-constant-value set-operand-constant-value!)
  (alias operand-alias set-operand-alias!))

(define* (make-operand var sym #:optional source visit alias)
  ;; Bind SYM to VAR, with value SOURCE.  Unassigned bound operands are
  ;; considered copyable until we prove otherwise.  If we have a source
  ;; expression, truncate it to one value.  Copy propagation does not
  ;; work on multiply-valued expressions.
  (let ((source (and=> source truncate-values)))
    (%make-operand var sym visit source 0 0
                   (and source (not (var-set? var))) #f #f
                   (and (not (var-set? var)) alias))))

(define* (make-bound-operands vars syms sources visit #:optional aliases)
  (if aliases
      (map (lambda (name sym source alias)
             (make-operand name sym source visit alias))
           vars syms sources aliases)
      (map (lambda (name sym source)
             (make-operand name sym source visit #f))
           vars syms sources)))

(define (make-unbound-operands vars syms)
  (map make-operand vars syms))

(define (set-operand-residual-value! op val)
  (%set-operand-residual-value!
   op
   (match val
    (($ <primcall> src 'values (first))
     ;; The continuation of a residualized binding does not need the
     ;; introduced `values' node, so undo the effects of truncation.
     first)
    (else
     val))))

(define* (visit-operand op counter ctx #:optional effort-limit size-limit)
  ;; Peval is O(N) in call sites of the source program.  However,
  ;; visiting an operand can introduce new call sites.  If we visit an
  ;; operand outside a counter -- i.e., outside an inlining attempt --
  ;; this can lead to divergence.  So, if we are visiting an operand to
  ;; try to copy it, and there is no counter, make a new one.
  ;;
  ;; This will only happen at most as many times as there are lexical
  ;; references in the source program.
  (and (zero? (operand-visit-count op))
       (dynamic-wind
         (lambda ()
           (set-operand-visit-count! op (1+ (operand-visit-count op))))
         (lambda ()
           (and (operand-source op)
                (if (or counter (and (not effort-limit) (not size-limit)))
                    ((%operand-visit op) (operand-source op) counter ctx)
                    (let/ec k
                      (define (abort)
                        ;; If we abort when visiting the value in a
                        ;; fresh context, we won't succeed in any future
                        ;; attempt, so don't try to copy it again.
                        (set-operand-copyable?! op #f)
                        (k #f))
                      ((%operand-visit op)
                       (operand-source op) 
                       (make-top-counter effort-limit size-limit abort op)
                       ctx)))))
         (lambda ()
           (set-operand-visit-count! op (1- (operand-visit-count op)))))))

;; A helper for constant folding.
;;
(define (types-check? primitive-name args)
  (case primitive-name
    ((values) #t)
    ((not pair? null? list? symbol? vector? struct?)
     (= (length args) 1))
    ((eq? eqv? equal?)
     (= (length args) 2))
    ;; FIXME: add more cases?
    (else #f)))

(define* (peval exp #:optional (cenv (current-module)) (env vlist-null)
                #:key
                (operator-size-limit 40)
                (operand-size-limit 20)
                (value-size-limit 10)
                (effort-limit 500)
                (recursive-effort-limit 100)
                (cross-module-inlining? #f))
  "Partially evaluate EXP in compilation environment CENV, with
top-level bindings from ENV and return the resulting expression."

  ;; This is a simple partial evaluator.  It effectively performs
  ;; constant folding, copy propagation, dead code elimination, and
  ;; inlining.

  ;; TODO:
  ;;
  ;; Propagate copies across toplevel bindings, if we can prove the
  ;; bindings to be immutable.
  ;;
  ;; Specialize lambda expressions with invariant arguments.

  (define local-toplevel-env
    ;; The top-level environment of the module being compiled.
    (let ()
      (define (env-folder x env)
        (match x
          (($ <toplevel-define> _ _ name)
           (vhash-consq name #t env))
          (($ <seq> _ head tail)
           (env-folder tail (env-folder head env)))
          (_ env)))
      (env-folder exp vlist-null)))

  (define (local-toplevel? name)
    (vhash-assq name local-toplevel-env))

  ;; gensym -> <var>
  ;; renamed-term -> original-term
  ;;
  (define store (build-var-table exp))

  (define (record-new-temporary! name sym refcount)
    (set! store (vhash-consq sym (make-var name sym refcount #f) store)))

  (define (lookup-var sym)
    (let ((v (vhash-assq sym store)))
      (if v (cdr v) (error "unbound var" sym (vlist->list store)))))

  (define (fresh-gensyms vars)
    (map (lambda (var)
           (let ((new (gensym (string-append (symbol->string (var-name var))
                                             " "))))
             (set! store (vhash-consq new var store))
             new))
         vars))

  (define (fresh-temporaries ls)
    (map (lambda (elt)
           (let ((new (gensym "tmp ")))
             (record-new-temporary! 'tmp new 1)
             new))
         ls))

  (define (assigned-lexical? sym)
    (var-set? (lookup-var sym)))

  (define (lexical-refcount sym)
    (var-refcount (lookup-var sym)))

  (define (splice-expression exp)
    (define vars (make-hash-table))
    (define (rename! old*)
      (match old*
        (() '())
        ((old . old*)
         (cons (let ((new (gensym "t")))
                 (hashq-set! vars old new)
                 new)
               (rename! old*)))))
    (define (new-name old) (hashq-ref vars old))
    (define renamed
      (pre-order
       (match-lambda
         (($ <lexical-ref> src name gensym)
          (make-lexical-ref src name (new-name gensym)))
         (($ <lexical-set> src name gensym exp)
          (make-lexical-set src name (new-name gensym) exp))
         (($ <lambda-case> src req opt rest kw init gensyms body alt)
          (let ((gensyms (rename! gensyms)))
            (make-lambda-case src req opt rest
                              (match kw
                                ((aok? (kw name sym) ...)
                                 (cons aok?
                                       (map (lambda (kw name sym)
                                              (list kw name (new-name sym)))
                                            kw name sym)))
                                (#f #f))
                              init gensyms body alt)))
         (($ <let> src names gensyms vals body)
          (make-let src names (rename! gensyms) vals body))
         (($ <letrec>)
          (error "unexpected letrec"))
         (($ <fix> src names gensyms vals body)
          (make-fix src names (rename! gensyms) vals body))
         (exp exp))
       exp))
    (set! store (build-var-table renamed store))
    renamed)

  (define (with-temporaries src exps refcount can-copy? k)
    (let* ((pairs (map (match-lambda
                         ((and exp (? can-copy?))
                          (cons #f exp))
                         (exp
                          (let ((sym (gensym "tmp ")))
                            (record-new-temporary! 'tmp sym refcount)
                            (cons sym exp))))
                       exps))
           (tmps (filter car pairs)))
      (match tmps
        (() (k exps))
        (tmps
         (make-let src
                   (make-list (length tmps) 'tmp)
                   (map car tmps)
                   (map cdr tmps)
                   (k (map (match-lambda
                             ((#f . val) val)
                             ((sym . _)
                              (make-lexical-ref #f 'tmp sym)))
                           pairs)))))))

  (define (make-begin0 src first second)
    (make-let-values
     src
     first
     (let ((vals (gensym "vals ")))
       (record-new-temporary! 'vals vals 1)
       (make-lambda-case
        #f
        '() #f 'vals #f '() (list vals)
        (make-seq
         src
         second
         (make-primcall #f 'apply
                        (list
                         (make-primitive-ref #f 'values)
                         (make-lexical-ref #f 'vals vals))))
        #f))))

  ;; ORIG has been alpha-renamed to NEW.  Analyze NEW and record a link
  ;; from it to ORIG.
  ;;
  (define (record-source-expression! orig new)
    (set! store (vhash-consq new (source-expression orig) store))
    new)

  ;; Find the source expression corresponding to NEW.  Used to detect
  ;; recursive inlining attempts.
  ;;
  (define (source-expression new)
    (let ((x (vhash-assq new store)))
      (if x (cdr x) new)))

  (define (record-operand-use op)
    (set-operand-use-count! op (1+ (operand-use-count op))))

  (define (unrecord-operand-uses op n)
    (let ((count (- (operand-use-count op) n)))
      (when (zero? count)
        (set-operand-residual-value! op #f))
      (set-operand-use-count! op count)))

  (define* (residualize-lexical op #:optional ctx val)
    (log 'residualize op)
    (record-operand-use op)
    (if (memq ctx '(value values))
        (set-operand-residual-value! op val))
    (make-lexical-ref #f (var-name (operand-var op)) (operand-sym op)))

  (define (fold-constants src name args ctx)
    (define (apply-primitive name args)
      ;; todo: further optimize commutative primitives
      (catch #t
             (lambda ()
               (define mod (resolve-interface (primitive-module name)))
               (call-with-values
                   (lambda ()
                     (apply (module-ref mod name) args))
                 (lambda results
                   (values #t results))))
             (lambda _
               (values #f '()))))
    (define (make-values src values)
      (match values
        ((single) single)               ; 1 value
        ((_ ...)                        ; 0, or 2 or more values
         (make-primcall src 'values values))))
    (define (residualize-call)
      (make-primcall src name args))
    (cond
     ((every const? args)
      (let-values (((success? values)
                    (apply-primitive name (map const-exp args))))
        (log 'fold success? values name args)
        (if success?
            (case ctx
              ((effect) (make-void src))
              ((test)
               ;; Values truncation: only take the first
               ;; value.
               (if (pair? values)
                   (make-const src (car values))
                   (make-values src '())))
              (else
               (make-values src (map (cut make-const src <>) values))))
            (residualize-call))))
     ((and (eq? ctx 'effect) (types-check? name args))
      (make-void #f))
     (else
      (residualize-call))))

  (define (inline-values src exp nmin nmax consumer)
    (let loop ((exp exp))
      (match exp
        ;; Some expression types are always singly-valued.
        ((or ($ <const>)
             ($ <void>)
             ($ <lambda>)
             ($ <lexical-ref>)
             ($ <toplevel-ref>)
             ($ <module-ref>)
             ($ <primitive-ref>)
             ($ <lexical-set>)          ; FIXME: these set! expressions
             ($ <toplevel-set>)         ; could return zero values in
             ($ <toplevel-define>)      ; the future
             ($ <module-set>)           ;
             ($ <primcall> src (? singly-valued-primitive?)))
         (and (<= nmin 1) (or (not nmax) (>= nmax 1))
              (make-call src (make-lambda #f '() consumer) (list exp))))

        ;; Statically-known number of values.
        (($ <primcall> src 'values vals)
         (and (<= nmin (length vals)) (or (not nmax) (>= nmax (length vals)))
              (make-call src (make-lambda #f '() consumer) vals)))

        ;; Not going to copy code into both branches.
        (($ <conditional>) #f)

        ;; Bail on other applications.
        (($ <call>) #f)
        (($ <primcall>) #f)

        ;; Bail on prompt and abort.
        (($ <prompt>) #f)
        (($ <abort>) #f)
        
        ;; Propagate to tail positions.
        (($ <let> src names gensyms vals body)
         (let ((body (loop body)))
           (and body
                (make-let src names gensyms vals body))))
        (($ <fix> src names gensyms vals body)
         (let ((body (loop body)))
           (and body
                (make-fix src names gensyms vals body))))
        (($ <let-values> src exp
            ($ <lambda-case> src2 req opt rest kw inits gensyms body #f))
         (let ((body (loop body)))
           (and body
                (make-let-values src exp
                                 (make-lambda-case src2 req opt rest kw
                                                   inits gensyms body #f)))))
        (($ <seq> src head tail)
         (let ((tail (loop tail)))
           (and tail (make-seq src head tail)))))))

  (define compute-effects
    (make-effects-analyzer assigned-lexical?))

  (define (constant-expression? x)
    ;; Return true if X is constant, for the purposes of copying or
    ;; elision---i.e., if it is known to have no effects, does not
    ;; allocate storage for a mutable object, and does not access
    ;; mutable data (like `car' or toplevel references).
    (constant? (compute-effects x)))

  (define (prune-bindings ops in-order? body counter ctx build-result)
    ;; This helper handles both `let' and `letrec'/`fix'.  In the latter
    ;; cases we need to make sure that if referenced binding A needs
    ;; as-yet-unreferenced binding B, that B is processed for value.
    ;; Likewise if C, when processed for effect, needs otherwise
    ;; unreferenced D, then D needs to be processed for value too.
    ;;
    (define (referenced? op)
      ;; When we visit lambdas in operator context, we just copy them,
      ;; as we will process their body later.  However this does have
      ;; the problem that any free var referenced by the lambda is not
      ;; marked as needing residualization.  Here we hack around this
      ;; and treat all bindings as referenced if we are in operator
      ;; context.
      (or (eq? ctx 'operator)
          (not (zero? (operand-use-count op)))))
    
    ;; values := (op ...)
    ;; effects := (op ...)
    (define (residualize values effects)
      ;; Note, values and effects are reversed.
      (cond
       (in-order?
        (let ((values (filter operand-residual-value ops)))
          (if (null? values)
              body
              (build-result (map (compose var-name operand-var) values)
                            (map operand-sym values)
                            (map operand-residual-value values)
                            body))))
       (else
        (let ((body
               (if (null? effects)
                   body
                   (let ((effect-vals (map operand-residual-value effects)))
                     (list->seq #f (reverse (cons body effect-vals)))))))
          (if (null? values)
              body
              (let ((values (reverse values)))
                (build-result (map (compose var-name operand-var) values)
                              (map operand-sym values)
                              (map operand-residual-value values)
                              body)))))))

    ;; old := (bool ...)
    ;; values := (op ...)
    ;; effects := ((op . value) ...)
    (let prune ((old (map referenced? ops)) (values '()) (effects '()))
      (let lp ((ops* ops) (values values) (effects effects))
        (cond
         ((null? ops*)
          (let ((new (map referenced? ops)))
            (if (not (equal? new old))
                (prune new values '())
                (residualize values
                             (map (lambda (op val)
                                    (set-operand-residual-value! op val)
                                    op)
                                  (map car effects) (map cdr effects))))))
         (else
          (let ((op (car ops*)))
            (cond
             ((memq op values)
              (lp (cdr ops*) values effects))
             ((operand-residual-value op)
              (lp (cdr ops*) (cons op values) effects))
             ((referenced? op)
              (set-operand-residual-value! op (visit-operand op counter 'value))
              (lp (cdr ops*) (cons op values) effects))
             (else
              (lp (cdr ops*)
                  values
                  (let ((effect (visit-operand op counter 'effect)))
                    (if (void? effect)
                        effects
                        (acons op effect effects))))))))))))
  
  (define (small-expression? x limit)
    (let/ec k
      (tree-il-fold
       (lambda (x res)                  ; down
         (1+ res))
       (lambda (x res)                  ; up
         (if (< res limit)
             res
             (k #f)))
       0 x)
      #t))
  
  (define (extend-env sym op env)
    (vhash-consq (operand-sym op) op (vhash-consq sym op env)))
      
  (let loop ((exp   exp)
             (env   vlist-null)         ; vhash of gensym -> <operand>
             (counter #f)               ; inlined call stack
             (ctx 'values)) ; effect, value, values, test, operator, or call
    (define (lookup var)
      (cond 
       ((vhash-assq var env) => cdr)
       (else (error "unbound var" var))))

    ;; Find a value referenced a specific number of times.  This is a hack
    ;; that's used for propagating fresh data structures like rest lists and
    ;; prompt tags.  Usually we wouldn't copy consed data, but we can do so in
    ;; some special cases like `apply' or prompts if we can account
    ;; for all of its uses.
    ;;
    ;; You don't want to use this in general because it introduces a slight
    ;; nonlinearity by running peval again (though with a small effort and size
    ;; counter).
    ;;
    (define (find-definition x n-aliases)
      (cond
       ((lexical-ref? x)
        (cond
         ((lookup (lexical-ref-gensym x))
          => (lambda (op)
               (if (var-set? (operand-var op))
                   (values #f #f)
                   (let ((y (or (operand-residual-value op)
                                (visit-operand op counter 'value 10 10)
                                (operand-source op))))
                     (cond
                      ((and (lexical-ref? y)
                            (= (lexical-refcount (lexical-ref-gensym x)) 1))
                       ;; X is a simple alias for Y.  Recurse, regardless of
                       ;; the number of aliases we were expecting.
                       (find-definition y n-aliases))
                      ((= (lexical-refcount (lexical-ref-gensym x)) n-aliases)
                       ;; We found a definition that is aliased the right
                       ;; number of times.  We still recurse in case it is a
                       ;; lexical.
                       (values (find-definition y 1)
                               op))
                      (else
                       ;; We can't account for our aliases.
                       (values #f #f)))))))
         (else
          ;; A formal parameter.  Can't say anything about that.
          (values #f #f))))
       ((= n-aliases 1)
        ;; Not a lexical: success, but only if we are looking for an
        ;; unaliased value.
        (values x #f))
       (else (values #f #f))))

    (define (visit exp ctx)
      (loop exp env counter ctx))

    (define (for-value exp)    (visit exp 'value))
    (define (for-values exp)   (visit exp 'values))
    (define (for-test exp)     (visit exp 'test))
    (define (for-effect exp)   (visit exp 'effect))
    (define (for-call exp)     (visit exp 'call))
    (define (for-tail exp)     (visit exp ctx))

    (if counter
        (record-effort! counter))

    (log 'visit ctx (and=> counter effort-counter)
         (unparse-tree-il exp))

    (match exp
      (($ <const>)
       (case ctx
         ((effect) (make-void #f))
         (else exp)))
      (($ <void>)
       (case ctx
         ((test) (make-const #f #t))
         (else exp)))
      (($ <lexical-ref> _ _ gensym)
       (log 'begin-copy gensym)
       (let lp ((op (lookup gensym)))
         (cond
          ((eq? ctx 'effect)
           (log 'lexical-for-effect gensym)
           (make-void #f))
          ((operand-alias op)
           ;; This is an unassigned operand that simply aliases some
           ;; other operand.  Recurse to avoid residualizing the leaf
           ;; binding.
           => lp)
          ((eq? ctx 'call)
           ;; Don't propagate copies if we are residualizing a call.
           (log 'residualize-lexical-call gensym op)
           (residualize-lexical op))
          ((var-set? (operand-var op))
           ;; Assigned lexicals don't copy-propagate.
           (log 'assigned-var gensym op)
           (residualize-lexical op))
          ((not (operand-copyable? op))
           ;; We already know that this operand is not copyable.
           (log 'not-copyable gensym op)
           (residualize-lexical op))
          ((and=> (operand-constant-value op)
                  (lambda (x) (or (const? x) (void? x) (primitive-ref? x))))
           ;; A cache hit.
           (let ((val (operand-constant-value op)))
             (log 'memoized-constant gensym val)
             (for-tail val)))
          ((visit-operand op counter (if (eq? ctx 'values) 'value ctx)
                          recursive-effort-limit operand-size-limit)
           =>
           ;; If we end up deciding to residualize this value instead of
           ;; copying it, save that residualized value.
           (lambda (val)
             (cond
              ((not (constant-expression? val))
               (log 'not-constant gensym op)
               ;; At this point, ctx is operator, test, or value.  A
               ;; value that is non-constant in one context will be
               ;; non-constant in the others, so it's safe to record
               ;; that here, and avoid future visits.
               (set-operand-copyable?! op #f)
               (residualize-lexical op ctx val))
              ((or (const? val)
                   (void? val)
                   (primitive-ref? val))
               ;; Always propagate simple values that cannot lead to
               ;; code bloat.
               (log 'copy-simple gensym val)
               ;; It could be this constant is the result of folding.
               ;; If that is the case, cache it.  This helps loop
               ;; unrolling get farther.
               (if (or (eq? ctx 'value) (eq? ctx 'values))
                   (begin
                     (log 'memoize-constant gensym val)
                     (set-operand-constant-value! op val)))
               val)
              ((= 1 (var-refcount (operand-var op)))
               ;; Always propagate values referenced only once.
               (log 'copy-single gensym val)
               val)
              ;; FIXME: do demand-driven size accounting rather than
              ;; these heuristics.
              ((eq? ctx 'operator)
               ;; A pure expression in the operator position.  Inline
               ;; if it's a lambda that's small enough.
               (if (and (lambda? val)
                        (small-expression? val operator-size-limit))
                   (begin
                     (log 'copy-operator gensym val)
                     val)
                   (begin
                     (log 'too-big-for-operator gensym val)
                     (residualize-lexical op ctx val))))
              (else
               ;; A pure expression, processed for call or for value.
               ;; Don't inline lambdas, because they will probably won't
               ;; fold because we don't know the operator.
               (if (and (small-expression? val value-size-limit)
                        (not (tree-il-any lambda? val)))
                   (begin
                     (log 'copy-value gensym val)
                     val)
                   (begin
                     (log 'too-big-or-has-lambda gensym val)
                     (residualize-lexical op ctx val)))))))
          (else
           ;; Visit failed.  Either the operand isn't bound, as in
           ;; lambda formal parameters, or the copy was aborted.
           (log 'unbound-or-aborted gensym op)
           (residualize-lexical op)))))
      (($ <lexical-set> src name gensym exp)
       (let ((op (lookup gensym)))
         (if (zero? (var-refcount (operand-var op)))
             (let ((exp (for-effect exp)))
               (if (void? exp)
                   exp
                   (make-seq src exp (make-void #f))))
             (begin
               (record-operand-use op)
               (make-lexical-set src name (operand-sym op) (for-value exp))))))
      (($ <let> src
          (names ... rest)
          (gensyms ... rest-sym)
          (vals ... ($ <primcall> _ 'list rest-args))
          ($ <primcall> asrc 'apply
             (proc args ...
                   ($ <lexical-ref> _
                      (? (cut eq? <> rest))
                      (? (lambda (sym)
                           (and (eq? sym rest-sym)
                                (= (lexical-refcount sym) 1))))))))
       (let* ((tmps (make-list (length rest-args) 'tmp))
              (tmp-syms (fresh-temporaries tmps)))
         (for-tail
          (make-let src
                    (append names tmps)
                    (append gensyms tmp-syms)
                    (append vals rest-args)
                    (make-call
                     asrc
                     proc
                     (append args
                             (map (cut make-lexical-ref #f <> <>)
                                  tmps tmp-syms)))))))
      (($ <let> src names gensyms vals body)
       (define (lookup-alias exp)
         ;; It's very common for macros to introduce something like:
         ;;
         ;;   ((lambda (x y) ...) x-exp y-exp)
         ;;
         ;; In that case you might end up trying to inline something like:
         ;;
         ;;   (let ((x x-exp) (y y-exp)) ...)
         ;;
         ;; But if x-exp is itself a lexical-ref that aliases some much
         ;; larger expression, perhaps it will fail to inline due to
         ;; size.  However we don't want to introduce a useless alias
         ;; (in this case, x).  So if the RHS of a let expression is a
         ;; lexical-ref, we record that expression.  If we end up having
         ;; to residualize X, then instead we residualize X-EXP, as long
         ;; as it isn't assigned.
         ;;
         (match exp
           (($ <lexical-ref> _ _ sym)
            (let ((op (lookup sym)))
              (and (not (var-set? (operand-var op))) op)))
           (_ #f)))

       (let* ((vars (map lookup-var gensyms))
              (new (fresh-gensyms vars))
              (ops (make-bound-operands vars new vals
                                        (lambda (exp counter ctx)
                                          (loop exp env counter ctx))
                                        (map lookup-alias vals)))
              (env (fold extend-env env gensyms ops))
              (body (loop body env counter ctx)))
         (match body
           (($ <const>)
            (for-tail (list->seq src (append vals (list body)))))
           (($ <lexical-ref> _ _ (? (lambda (sym) (memq sym new)) sym))
            (let ((pairs (map cons new vals)))
              ;; (let ((x foo) (y bar) ...) x) => (begin bar ... foo)
              (for-tail
               (list->seq
                src
                (append (map cdr (alist-delete sym pairs eq?))
                        (list (assq-ref pairs sym)))))))
           ((and ($ <conditional> src*
                    ($ <lexical-ref> _ _ sym) ($ <lexical-ref> _ _ sym) alt)
                 (? (lambda (_)
                      (case ctx
                        ((test effect)
                         (and (equal? (list sym) new)
                              (= (lexical-refcount sym) 2)))
                        (else #f)))))
            ;; (let ((x EXP)) (if x x ALT)) -> (if EXP #t ALT) in test context
            (make-conditional src* (visit-operand (car ops) counter 'test)
                              (make-const src* #t) alt))
           (_
            ;; Only include bindings for which lexical references
            ;; have been residualized.
            (prune-bindings ops #f body counter ctx
                            (lambda (names gensyms vals body)
                              (if (null? names) (error "what!" names))
                              (make-let src names gensyms vals body)))))))
      (($ <fix> src names gensyms vals body)
       ;; Note the difference from the `let' case: here we use letrec*
       ;; so that the `visit' procedure for the new operands closes over
       ;; an environment that includes the operands.  Also we don't try
       ;; to elide aliases, because we can't sensibly reduce something
       ;; like (letrec ((a b) (b a)) a).
       (letrec* ((visit (lambda (exp counter ctx)
                          (loop exp env* counter ctx)))
                 (vars (map lookup-var gensyms))
                 (new (fresh-gensyms vars))
                 (ops (make-bound-operands vars new vals visit))
                 (env* (fold extend-env env gensyms ops))
                 (body* (visit body counter ctx)))
         (if (const? body*)
             body*
             (prune-bindings ops #f body* counter ctx
                             (lambda (names gensyms vals body)
                               (make-fix src names gensyms vals body))))))
      (($ <let-values> lv-src producer consumer)
       ;; Peval the producer, then try to inline the consumer into
       ;; the producer.  If that succeeds, peval again.  Otherwise
       ;; reconstruct the let-values, pevaling the consumer.
       (let ((producer (for-values producer)))
         (or (match consumer
               ((and ($ <lambda-case> src () #f rest #f () (rest-sym) body #f)
                     (? (lambda _ (singly-valued-expression? producer))))
                (let ((tmp (gensym "tmp ")))
                  (record-new-temporary! 'tmp tmp 1)
                  (for-tail
                   (make-let
                    src (list 'tmp) (list tmp) (list producer)
                    (make-let
                     src (list rest) (list rest-sym)
                     (list
                      (make-primcall #f 'list
                                     (list (make-lexical-ref #f 'tmp tmp))))
                     body)))))
               (($ <lambda-case> src req opt rest #f inits gensyms body #f)
                (let* ((nmin (length req))
                       (nmax (and (not rest) (+ nmin (if opt (length opt) 0)))))
                  (cond
                   ((inline-values lv-src producer nmin nmax consumer)
                    => for-tail)
                   (else #f))))
               (_ #f))
             (make-let-values lv-src producer (for-tail consumer)))))
      (($ <toplevel-ref> src mod (? effect-free-primitive? name))
       exp)
      (($ <toplevel-ref>)
       ;; todo: open private local bindings.
       exp)
      (($ <module-ref> src module (? effect-free-primitive? name) #f)
       (let ((module (false-if-exception
                      (resolve-module module #:ensure #f))))
         (if (module? module)
             (let ((var (module-variable module name)))
               (if (eq? var (module-variable the-scm-module name))
                   (make-primitive-ref src name)
                   exp))
             exp)))
      (($ <module-ref> src module name public?)
       (cond
        ((and cross-module-inlining?
              public?
              (and=> (resolve-module module #:ensure #f)
                     (lambda (module)
                       (and=> (module-public-interface module)
                              (lambda (iface)
                                (and=> (module-inlinable-exports iface)
                                       (lambda (proc) (proc name))))))))
         => (lambda (inlined)
              ;; Similar logic to lexical-ref, but we can't enumerate
              ;; uses, and don't know about aliases.
              (log 'begin-xm-copy exp inlined)
              (cond
               ((eq? ctx 'effect)
                (log 'xm-effect)
                (make-void #f))
               ((eq? ctx 'call)
                ;; Don't propagate copies if we are residualizing a call.
                (log 'residualize-xm-call exp)
                exp)
               ((or (const? inlined) (void? inlined) (primitive-ref? inlined))
                ;; Always propagate simple values that cannot lead to
                ;; code bloat.
                (log 'copy-xm-const)
                (for-tail inlined))
               ;; Inline in operator position if it's a lambda that's
               ;; small enough.  Normally the inlinable-exports pass
               ;; will only make small lambdas available for inlining,
               ;; but you never know.
               ((and (eq? ctx 'operator) (lambda? inlined)
                     (small-expression? inlined operator-size-limit))
                (log 'copy-xm-operator exp inlined)
                (splice-expression inlined))
               (else
                (log 'xm-copy-failed)
                ;; Could copy small lambdas in value context.  Something
                ;; to revisit.
                exp))))
        (else exp)))
      (($ <module-set> src mod name public? exp)
       (make-module-set src mod name public? (for-value exp)))
      (($ <toplevel-define> src mod name exp)
       (make-toplevel-define src mod name (for-value exp)))
      (($ <toplevel-set> src mod name exp)
       (make-toplevel-set src mod name (for-value exp)))
      (($ <primitive-ref>)
       (case ctx
         ((effect) (make-void #f))
         ((test) (make-const #f #t))
         (else exp)))
      (($ <conditional> src condition subsequent alternate)
       (define (call-with-failure-thunk exp proc)
         (match exp
           (($ <call> _ _ ()) (proc exp))
           (($ <primcall> _ _ ()) (proc exp))
           (($ <const>) (proc exp))
           (($ <void>) (proc exp))
           (($ <lexical-ref>) (proc exp))
           (_
            (let ((t (gensym "failure-")))
              (record-new-temporary! 'failure t 2)
              (make-let
               src (list 'failure) (list t)
               (list
                (make-lambda
                 #f '()
                 (make-lambda-case #f '() #f #f #f '() '() exp #f)))
               (proc (make-call #f (make-lexical-ref #f 'failure t)
                                '())))))))
       (define (simplify-conditional c)
         (match c
           ;; Swap the arms of (if (not FOO) A B), to simplify.
           (($ <conditional> src ($ <primcall> _ 'not (pred))
               subsequent alternate)
            (simplify-conditional
             (make-conditional src pred alternate subsequent)))
           ;; In the following four cases, we try to expose the test to
           ;; the conditional.  This will let the CPS conversion avoid
           ;; reifying boolean literals in some cases.
           (($ <conditional> src ($ <let> src* names vars vals body)
               subsequent alternate)
            (make-let src* names vars vals
                      (simplify-conditional
                       (make-conditional src body subsequent alternate))))
           (($ <conditional> src ($ <fix> src* names vars vals body)
               subsequent alternate)
            (make-fix src* names vars vals
                      (simplify-conditional
                       (make-conditional src body subsequent alternate))))
           (($ <conditional> src ($ <seq> src* head tail)
               subsequent alternate)
            (make-seq src* head
                      (simplify-conditional
                       (make-conditional src tail subsequent alternate))))
           ;; Special cases for common tests in the predicates of chains
           ;; of if expressions.
           (($ <conditional> src
               ($ <conditional> src* outer-test inner-test ($ <const> _ #f))
               inner-subsequent
               alternate)
            (let lp ((alternate alternate))
              (match alternate
                ;; Lift a common repeated test out of a chain of if
                ;; expressions.
                (($ <conditional> _ (? (cut tree-il=? outer-test <>))
                    other-subsequent alternate)
                 (make-conditional
                  src outer-test
                  (simplify-conditional
                   (make-conditional src* inner-test inner-subsequent
                                     other-subsequent))
                  alternate))
                ;; Likewise, but punching through any surrounding
                ;; failure continuations.
                (($ <let> let-src (name) (sym) ((and thunk ($ <lambda>))) body)
                 (make-let
                  let-src (list name) (list sym) (list thunk)
                  (lp body)))
                ;; Otherwise, rotate AND tests to expose a simple
                ;; condition in the front.  Although this may result in
                ;; lexically binding failure thunks, the thunks will be
                ;; compiled to labels allocation, so there's no actual
                ;; code growth.
                (_
                 (call-with-failure-thunk
                  alternate
                  (lambda (failure)
                    (make-conditional
                     src outer-test
                     (simplify-conditional
                      (make-conditional src* inner-test inner-subsequent failure))
                     failure)))))))
           (_ c)))
       (match (for-test condition)
         (($ <const> _ val)
          (if val
              (for-tail subsequent)
              (for-tail alternate)))
         (c
          (simplify-conditional
           (make-conditional src c (for-tail subsequent)
                             (for-tail alternate))))))
      (($ <primcall> src 'call-with-values
          (producer
           ($ <lambda> _ _
              (and consumer
                   ;; No optional or kwargs.
                   ($ <lambda-case>
                      _ req #f rest #f () gensyms body #f)))))
       (for-tail (make-let-values src (make-call src producer '())
                                  consumer)))
      (($ <primcall> src 'dynamic-wind (w thunk u))
       (for-tail
        (with-temporaries
         src (list w u) 2 constant-expression?
         (match-lambda
           ((w u)
            (make-seq
             src
             (make-seq
              src
              (make-conditional
               src
               ;; fixme: introduce logic to fold thunk?
               (make-primcall src 'thunk? (list u))
               (make-call src w '())
               (make-primcall src 'raise-type-error
                              (list (make-const #f #("dynamic-wind" 3 "thunk"))
                                    u)))
              (make-primcall src 'wind (list w u)))
             (make-begin0 src
                          (make-call src thunk '())
                          (make-seq src
                                    (make-primcall src 'unwind '())
                                    (make-call src u '())))))))))

      (($ <primcall> src 'with-fluid* (f v thunk))
       (for-tail
        (with-temporaries
         src (list f v thunk) 1 constant-expression?
         (match-lambda
           ((f v thunk)
            (make-seq src
                      (make-primcall src 'push-fluid (list f v))
                      (make-begin0 src
                                   (make-call src thunk '())
                                   (make-primcall src 'pop-fluid '()))))))))

      (($ <primcall> src 'with-dynamic-state (state thunk))
       (for-tail
        (with-temporaries
         src (list state thunk) 1 constant-expression?
         (match-lambda
           ((state thunk)
            (make-seq src
                      (make-primcall src 'push-dynamic-state (list state))
                      (make-begin0 src
                                   (make-call src thunk '())
                                   (make-primcall src 'pop-dynamic-state
                                                  '()))))))))

      (($ <primcall> src 'values exps)
       (match exps
         (()
          (case ctx
            ((effect) (make-void #f))
            ((values) exp)
            ;; Zero values returned to continuation expecting a value:
            ;; ensure that we raise an error.
            (else (make-primcall src 'values (list exp)))))
         ((($ <primcall> _ 'values ())) exp)
         (_
          (let ((vals (map for-value exps)))
            (if (and (case ctx
                       ((value test effect) #t)
                       (else (null? (cdr vals))))
                     (every singly-valued-expression? vals))
                (for-tail (list->seq src (append (cdr vals) (list (car vals)))))
                (make-primcall src 'values vals))))))

      (($ <primcall> src 'apply (proc args ... tail))
       (let lp ((tail* (find-definition tail 1)) (speculative? #t))
         (define (copyable? x)
           ;; Inlining a result from find-definition effectively copies it,
           ;; relying on the let-pruning to remove its original binding.  We
           ;; shouldn't copy non-constant expressions.
           (or (not speculative?) (constant-expression? x)))
         (match tail*
           (($ <const> _ (args* ...))
            (let ((args* (map (cut make-const #f <>) args*)))
              (for-tail (make-call src proc (append args args*)))))
           (($ <primcall> _ 'cons
               ((and head (? copyable?)) (and tail (? copyable?))))
            (for-tail (make-primcall src 'apply
                                     (cons proc
                                           (append args (list head tail))))))
           (($ <primcall> _ 'list
               (and args* ((? copyable?) ...)))
            (for-tail (make-call src proc (append args args*))))
           (tail*
            (if speculative?
                (lp (for-value tail) #f)
                (let ((args (append (map for-value args) (list tail*))))
                  (make-primcall src 'apply
                                 (cons (for-value proc) args))))))))

      (($ <primcall> src 'append (x z))
       (let ((x (for-value x)))
         (match x
           ((or ($ <const> _ ())
                ($ <primcall> _ 'list ()))
            (for-value z))
           ((or ($ <const> _ (_ . _))
                ($ <primcall> _ 'cons)
                ($ <primcall> _ 'list))
            (for-tail
             (let lp ((x x))
               (match x
                 ((or ($ <const> csrc ())
                      ($ <primcall> csrc 'list ()))
                  ;; Defer visiting z in value context to for-tail.
                  z)
                 (($ <const> csrc (x . y))
                  (let ((x (make-const csrc x))
                        (y (make-const csrc y)))
                    (make-primcall src 'cons (list x (lp y)))))
                 (($ <primcall> csrc 'cons (x y))
                  (make-primcall src 'cons (list x (lp y))))
                 (($ <primcall> csrc 'list (x . y))
                  (let ((y (make-primcall csrc 'list y)))
                    (make-primcall src 'cons (list x (lp y)))))
                 (x (make-primcall src 'append (list x z)))))))
           (else
            (make-primcall src 'append (list x (for-value z)))))))

      (($ <primcall> src (? constructor-primitive? name) args)
       (cond
        ((and (memq ctx '(effect test))
              (match (cons name args)
                ((or ('cons _ _)
                     ('list . _)
                     ('vector . _)
                     ('make-prompt-tag)
                     ('make-prompt-tag ($ <const> _ (? string?))))
                 #t)
                (_ #f)))
         (let ((res (if (eq? ctx 'effect)
                        (make-void #f)
                        (make-const #f #t))))
           (for-tail (list->seq src (append (map for-value args)
                                            (list res))))))
        (else
         (match (cons name (map for-value args))
           (('cons x ($ <const> _ (? (cut eq? <> '()))))
            (make-primcall src 'list (list x)))
           (('cons x ($ <primcall> _ 'list elts))
            (make-primcall src 'list (cons x elts)))
           (('list)
            (make-const src '()))
           (('vector)
            (make-const src '#()))
           ((name . args)
            (make-primcall src name args))))))

      (($ <primcall> src 'thunk? (proc))
       (case ctx
         ((effect)
          (for-tail (make-seq src proc (make-void src))))
         (else
          (match (for-value proc)
            (($ <lambda> _ _ ($ <lambda-case> _ req))
             (for-tail (make-const src (null? req))))
            (proc
             (match (find-definition proc 2)
               (($ <lambda> _ _ ($ <lambda-case> _ req))
                (for-tail (make-const src (null? req))))
               (_
                (make-primcall src 'thunk? (list proc)))))))))

      (($ <primcall> src name args)
       (match (cons name (map for-value args))
         ;; FIXME: these for-tail recursions could take place outside
         ;; an effort counter.
         (('car ($ <primcall> src 'cons (head tail)))
          (for-tail (make-seq src tail head)))
         (('cdr ($ <primcall> src 'cons (head tail)))
          (for-tail (make-seq src head tail)))
         (('car ($ <primcall> src 'list (head . tail)))
          (for-tail (list->seq src (append tail (list head)))))
         (('cdr ($ <primcall> src 'list (head . tail)))
          (for-tail (make-seq src head (make-primcall #f 'list tail))))
                  
         (('car ($ <const> src (head . tail)))
          (for-tail (make-const src head)))
         (('cdr ($ <const> src (head . tail)))
          (for-tail (make-const src tail)))
         (((or 'memq 'memv) k ($ <const> _ (elts ...)))
          ;; FIXME: factor 
          (case ctx
            ((effect)
             (for-tail
              (make-seq src k (make-void #f))))
            ((test)
             (cond
              ((const? k)
               ;; A shortcut.  The `else' case would handle it, but
               ;; this way is faster.
               (let ((member (case name ((memq) memq) ((memv) memv))))
                 (make-const #f (and (member (const-exp k) elts) #t))))
              ((null? elts)
               (for-tail
                (make-seq src k (make-const #f #f))))
              (else
               (let ((t (gensym "t "))
                     (eq (if (eq? name 'memq) 'eq? 'eqv?)))
                 (record-new-temporary! 't t (length elts))
                 (for-tail
                  (make-let
                   src (list 't) (list t) (list k)
                   (let lp ((elts elts))
                     (define test
                       (make-primcall #f eq
                                      (list (make-lexical-ref #f 't t)
                                            (make-const #f (car elts)))))
                     (if (null? (cdr elts))
                         test
                         (make-conditional src test
                                           (make-const #f #t)
                                           (lp (cdr elts)))))))))))
            (else
             (cond
              ((const? k)
               (let ((member (case name ((memq) memq) ((memv) memv))))
                 (make-const #f (member (const-exp k) elts))))
              ((null? elts)
               (for-tail (make-seq src k (make-const #f #f))))
              (else
               (make-primcall src name (list k (make-const #f elts))))))))

         (((? equality-primitive?) a (and b ($ <const> _ v)))
          (cond
           ((const? a)
            ;; Constants will be deduplicated later, but eq? folding can
            ;; happen now.  Anticipate the deduplication by using equal?
            ;; instead of eq? or eqv?.
            (for-tail (make-const src (equal? (const-exp a) v))))
           ((eq? name 'eq?)
            ;; Already in a reduced state.
            (make-primcall src 'eq? (list a b)))
           ((or (memq v '(#f #t () #nil)) (symbol? v) (char? v)
                ;; Only fold to eq? value is a fixnum on target and
                ;; host, as constant folding may have us compare on host
                ;; as well.
                (and (exact-integer? v)
                     (<= (max (target-most-negative-fixnum)
                              most-negative-fixnum)
                         v
                         (min (target-most-positive-fixnum)
                              most-positive-fixnum))))
            ;; Reduce to eq?.  Note that in Guile, characters are
            ;; comparable with eq?.
            (make-primcall src 'eq? (list a b)))
           ((number? v)
            ;; equal? and eqv? on non-fixnum numbers is the same as
            ;; eqv?, and can't be reduced beyond that.
            (make-primcall src 'eqv? (list a b)))
           ((eq? name 'eqv?)
            ;; eqv? on anything else is the same as eq?.
            (make-primcall src 'eq? (list a b)))
           (else
            ;; FIXME: inline a specialized implementation of equal? for
            ;; V here.
            (make-primcall src name (list a b)))))
         (((? equality-primitive?) (and a ($ <const>)) b)
          (for-tail (make-primcall src name (list b a))))
         (((? equality-primitive?) ($ <lexical-ref> _ _ sym)
           ($ <lexical-ref> _ _ sym))
          (for-tail (make-const src #t)))

         (('logbit? ($ <const> src2
                       (? (lambda (bit)
                            (and (exact-integer? bit)
                                 (<= 0 bit (logcount most-positive-fixnum))))
                          bit))
                    val)
          (for-tail
           (make-primcall src 'logtest
                          (list (make-const src2 (ash 1 bit)) val))))

         (('logtest a b)
          (for-tail
           (make-primcall
            src
            'not
            (list
             (make-primcall src 'eq?
                            (list (make-primcall src 'logand (list a b))
                                  (make-const src 0)))))))

         (((? effect-free-primitive?) . args)
          (fold-constants src name args ctx))

         ((name . args)
          (if (and (eq? ctx 'effect) (effect-free-primcall? name args))
              (if (null? args)
                  (make-void src)
                  (for-tail (list->seq src args)))
              (make-primcall src name args)))))

      (($ <call> src orig-proc orig-args)
       (define (residualize-call)
         (make-call src (for-call orig-proc) (map for-value orig-args)))

       (define (singly-referenced-lambda? proc)
         (match proc
           (($ <lambda>) #t)
           (($ <lexical-ref> _ _ sym)
            (and (not (assigned-lexical? sym))
                 (= (lexical-refcount sym) 1)
                 (singly-referenced-lambda?
                  (operand-source (lookup sym)))))
           (_ #f)))

       (define (attempt-inlining proc names syms vals body)
         (define inline-key (source-expression proc))
         (define existing-counter (find-counter inline-key counter))
         (define inlined-exp (make-let src names syms vals body))

         (cond
          ((and=> existing-counter counter-recursive?)
           ;; A recursive call.  Process again in tail context.

           ;; Mark intervening counters as recursive, so we can
           ;; handle a toplevel counter that recurses mutually with
           ;; some other procedure.  Otherwise, the next time we see
           ;; the other procedure, the effort limit would be clamped
           ;; to 100.
           (let lp ((counter counter))
             (unless (eq? counter existing-counter)
               (set-counter-recursive?! counter #t)
               (lp (counter-prev counter))))

           (log 'inline-recurse inline-key)
           (loop inlined-exp env counter ctx))
          ((singly-referenced-lambda? orig-proc)
           ;; A lambda in the operator position of the source
           ;; expression.  Process again in tail context.
           (log 'inline-beta inline-key)
           (loop inlined-exp env counter ctx))
          (else
           ;; An integration at the top-level, the first
           ;; recursion of a recursive procedure, or a nested
           ;; integration of a procedure that hasn't been seen
           ;; yet.
           (log 'inline-begin exp)
           (let/ec k
             (define (abort)
               (log 'inline-abort exp)
               (k (residualize-call)))
             (define new-counter
               (cond
                ;; These first two cases will transfer effort from
                ;; the current counter into the new counter.
                (existing-counter
                 (make-recursive-counter recursive-effort-limit
                                         operand-size-limit
                                         existing-counter counter))
                (counter
                 (make-nested-counter abort inline-key counter))
                ;; This case opens a new account, effectively
                ;; printing money.  It should only do so once for
                ;; each call site in the source program.
                (else
                 (make-top-counter effort-limit operand-size-limit
                                   abort inline-key))))
             (define result
               (loop inlined-exp env new-counter ctx))

             (when counter
               ;; The nested inlining attempt succeeded.  Deposit the
               ;; unspent effort and size back into the current
               ;; counter.
               (transfer! new-counter counter))

             (log 'inline-end result exp)
             result))))

       (let revisit-proc ((proc (visit orig-proc 'operator)))
         (match proc
           (($ <primitive-ref> _ name)
            (let ((exp (expand-primcall (make-primcall src name orig-args))))
              (set! store
                    (augment-var-table-with-externally-introduced-lexicals
                     exp store))
              (for-tail exp)))

           (($ <lambda> _ _ clause)
            ;; A lambda.  Attempt to find the matching clause, if
            ;; possible.
            (define (inline-clause req opt rest kw inits gensyms body
                                   arity-mismatch)
              (define (bind name sym val binds)
                (cons (vector name sym val) binds))
              (define (has-binding? binds sym)
                (match binds
                  (() #f)
                  ((#(n s v) . binds)
                   (or (eq? s sym) (has-binding? binds sym)))))

              ;; The basic idea is that we are going to transform an
              ;; expression like ((lambda (param ...) body) arg ...)
              ;; into (let ((param arg) ...) body).  However, we have to
              ;; consider order of effects and scope: the args are
              ;; logically parallel, whereas initializer expressions for
              ;; params that don't have arguments are evaluated in
              ;; order, after the arguments.  Therefore we have a set of
              ;; parallel bindings, abbreviated pbinds, which proceed
              ;; from the call site, and a set of serial bindings, the
              ;; sbinds, which result from callee initializers.  We
              ;; collect these in reverse order as we parse arguments.
              ;; The result is an outer let for the parallel bindings
              ;; containing a let* of the serial bindings and then the
              ;; body.

              (define (process-req req syms args pbinds sbinds)
                (match req
                  (() (process-opt (or opt '()) syms inits args pbinds sbinds))
                  ((name . req)
                   (match syms
                     ((sym . syms)
                      (match args
                        (() (arity-mismatch))
                        ((arg . args)
                         (process-req req syms args
                                      (bind name sym arg pbinds)
                                      sbinds))))))))

              (define (keyword-arg? exp)
                (match exp
                  (($ <const> _ (? keyword?)) #t)
                  (_ #f)))
              (define (not-keyword-arg? exp)
                (match exp
                  ((or ($ <const> _ (not (? keyword?)))
                       ($ <void>)
                       ($ <primitive-ref>)
                       ($ <lambda>))
                   #t)
                  (_ #f)))

              (define (process-opt opt syms inits args pbinds sbinds)
                (match opt
                  (() (process-rest syms inits args pbinds sbinds))
                  ((name . opt)
                   (match inits
                     ((init . inits)
                      (match syms
                        ((sym . syms)
                         (cond
                          (kw
                           (match args
                             ((or () ((? keyword-arg?) . _))
                              ;; Optargs and kwargs; stop optarg dispatch at
                              ;; first keyword.
                              (process-opt opt syms inits args pbinds
                                           (bind name sym init sbinds)))
                             (((? not-keyword-arg? arg) . args)
                              ;; Arg is definitely not a keyword; it is an
                              ;; optarg.
                              (process-opt opt syms inits args
                                           (bind name sym arg pbinds)
                                           sbinds))
                             (_
                              ;; We can't tell whether the arg is a keyword
                              ;; or not!  Annoying semantics, this.
                              (residualize-call))))
                          (else
                           ;; No kwargs.
                           (match args
                             (()
                              (process-opt opt syms inits args pbinds
                                           (bind name sym init sbinds)))
                             ((arg . args)
                              (process-opt opt syms inits args
                                           (bind name sym arg pbinds)
                                           sbinds))))))))))))

              (define (process-rest syms inits args pbinds sbinds)
                (match rest
                  (#f
                   (match kw
                     ((#f . kw)
                      (process-kw kw syms inits args pbinds sbinds))
                     (#f
                      (unless (and (null? syms) (null? inits))
                        (error "internal error"))
                      (match args
                        (() (finish pbinds sbinds body))
                        (_ (arity-mismatch))))))
                  (rest
                   (match syms
                     ((sym . syms)
                      (let ((rest-val (make-primcall src 'list args)))
                        (unless (and (null? syms) (null? inits))
                          (error "internal error"))
                        (finish pbinds (bind rest sym rest-val sbinds)
                                body)))))))

              (define (process-kw kw syms inits args pbinds sbinds)
                ;; Require that the ordered list of the keywords'
                ;; syms is the same as the remaining gensyms to bind.
                ;; Psyntax emits tree-il with this property, and it
                ;; is required by (and checked by) other parts of the
                ;; compiler, e.g. tree-il-to-cps lowering.
                (unless (equal? syms (match kw (((k name sym) ...) sym)))
                  (error "internal error: unexpected kwarg syms" kw syms))

                (define (process-kw-args positional? args pbinds)
                  (match args
                    (()
                     (process-kw-inits kw inits pbinds sbinds))
                    ((($ <const> _ (? keyword? keyword)) arg . args)
                     (match (assq keyword kw)
                       ((keyword name sym)
                        ;; Because of side effects, we don't
                        ;; optimize passing the same keyword arg
                        ;; multiple times.
                        (if (has-binding? pbinds sym)
                            (residualize-call)
                            (process-kw-args #f args
                                             (bind name sym arg pbinds))))
                       (#f (residualize-call))))
                    (((? not-keyword-arg?) . args)
                     (if positional?
                         (arity-mismatch)
                         (residualize-call)))
                    (_ (residualize-call))))

                (define (process-kw-inits kw inits pbinds sbinds)
                  (match kw
                    (()
                     (unless (null? inits) (error "internal error"))
                     (finish pbinds sbinds body))
                    (((keyword name sym) . kw)
                     (match inits
                       ((init . inits)
                        (process-kw-inits kw inits pbinds
                                          (if (has-binding? pbinds sym)
                                              sbinds
                                              (bind name sym init sbinds))))))))

                (process-kw-args #t args pbinds))

              (define (finish pbinds sbinds body)
                (match sbinds
                  (()
                   (match (reverse pbinds)
                     ((#(name sym val) ...)
                      (attempt-inlining proc name sym val body))))
                  ((#(name sym val) . sbinds)
                   (finish pbinds sbinds
                           (make-let src (list name) (list sym) (list val)
                                     body)))))

              ;; Limitations:
              ;;
              ;;  - #:key or #:rest, but not both.
              ;;  - #:allow-other-keys unsupported.
              (cond
               ((and kw (or rest (match kw ((aok? . _) aok?))))
                (residualize-call))
               (else
                (process-req req gensyms orig-args '() '()))))

            (let lp ((clause clause))
              (match clause
                ;; No clause matches.
                (#f (residualize-call))
                (($ <lambda-case> src req opt rest kw inits gensyms body alt)
                 (inline-clause req opt rest kw inits gensyms body
                                (lambda () (lp alt)))))))

           (($ <let> _ _ _ vals _)
            ;; Attempt to inline `let' in the operator position.
            ;;
            ;; We have to re-visit the proc in value mode, since the
            ;; `let' bindings might have been introduced or renamed,
            ;; whereas the lambda (if any) in operator position has not
            ;; been renamed.
            (if (or (and-map constant-expression? vals)
                    (and-map constant-expression? orig-args))
                ;; The arguments and the let-bound values commute.
                (match (for-value orig-proc)
                  (($ <let> lsrc names syms vals body)
                   (log 'inline-let orig-proc)
                   (for-tail
                    (make-let lsrc names syms vals
                              (make-call src body orig-args))))
                  ;; It's possible for a `let' to go away after the
                  ;; visit due to the fact that visiting a procedure in
                  ;; value context will prune unused bindings, whereas
                  ;; visiting in operator mode can't because it doesn't
                  ;; traverse through lambdas.  In that case re-visit
                  ;; the procedure.
                  (proc (revisit-proc proc)))
                (residualize-call)))

           (_ (residualize-call)))))

      (($ <lambda> src meta body)
       (case ctx
         ((effect) (make-void #f))
         ((test) (make-const #f #t))
         ((operator) exp)
         (else (record-source-expression!
                exp
                (make-lambda src meta (and body (for-values body)))))))
      (($ <lambda-case> src req opt rest kw inits gensyms body alt)
       (define (lift-applied-lambda body gensyms)
         (and (not opt) rest (not kw)
              (match body
                (($ <primcall> _ 'apply
                    (($ <lambda> _ _ (and lcase ($ <lambda-case> _ req1)))
                     ($ <lexical-ref> _ _ sym)
                     ...))
                 (and (equal? sym gensyms)
                      (not (lambda-case-alternate lcase))
                      (<= (length req) (length req1))
                      (every (lambda (s)
                               (= (lexical-refcount s) 1))
                             sym)
                      lcase))
                (_ #f))))
       (let* ((vars (map lookup-var gensyms))
              (new (fresh-gensyms vars))
              (env (fold extend-env env gensyms
                         (make-unbound-operands vars new)))
              (new-sym (lambda (old)
                         (operand-sym (cdr (vhash-assq old env)))))
              (body (loop body env counter ctx)))
         (or
          ;; (lambda args (apply (lambda ...) args)) => (lambda ...)
          (lift-applied-lambda body new)
          (make-lambda-case src req opt rest
                            (match kw
                              ((aok? (kw name old) ...)
                               (cons aok? (map list kw name (map new-sym old))))
                              (_ #f))
                            (map (cut loop <> env counter 'value) inits)
                            new
                            body
                            (and alt (for-tail alt))))))
      (($ <seq> src head tail)
       (let ((head (for-effect head))
             (tail (for-tail tail)))
         (if (void? head)
             tail
             (make-seq src
                       (if (and (seq? head)
                                (void? (seq-tail head)))
                           (seq-head head)
                           head)
                       tail))))
      (($ <prompt> src escape-only? tag body handler)
       (define (make-prompt-tag? x)
         (match x
           (($ <primcall> _ 'make-prompt-tag (or () ((? constant-expression?))))
            #t)
           (_ #f)))

       (let ((tag (for-value tag))
             (body (if escape-only? (for-tail body) (for-value body))))
         (cond
          ((find-definition tag 1)
           (lambda (val op)
             (make-prompt-tag? val))
           => (lambda (val op)
                ;; There is no way that an <abort> could know the tag
                ;; for this <prompt>, so we can elide the <prompt>
                ;; entirely.
                (when op (unrecord-operand-uses op 1))
                (for-tail (if escape-only? body (make-call src body '())))))
          (else
           (let ((handler (for-value handler)))
             (define (escape-only-handler? handler)
               (match handler
                 (($ <lambda> _ _
                     ($ <lambda-case> _ (_ . _) _ _ _ _ (k . _) body #f))
                  (not (tree-il-any
                        (match-lambda
                          (($ <lexical-ref> _ _ (? (cut eq? <> k))) #t)
                          (_ #f))
                        body)))
                 (else #f)))
             (if (and (not escape-only?) (escape-only-handler? handler))
                 ;; Prompt transitioning to escape-only; transition body
                 ;; to be an expression.
                 (for-tail
                  (make-prompt src #t tag (make-call #f body '()) handler))
                 (make-prompt src escape-only? tag body handler)))))))

      (($ <abort> src tag args tail)
       (make-abort src (for-value tag) (map for-value args)
                   (for-value tail))))))
