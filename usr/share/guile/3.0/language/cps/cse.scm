;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2021, 2023 Free Software Foundation, Inc.

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
;;; Common subexpression elimination for CPS.
;;;
;;; Code:

(define-module (language cps cse)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps effects-analysis)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps renumber)
  #:export (eliminate-common-subexpressions))

(define (intset-intersect* out out*)
  (if out (intset-intersect out out*) out*))

(define (compute-available-expressions succs kfun clobbers)
  "Compute and return a map of LABEL->ANCESTOR..., where ANCESTOR... is
an intset containing ancestor labels whose value is available at LABEL."
  (let ((init (intmap-map (lambda (label succs) #f) succs))
        (kill clobbers)
        (gen (intmap-map (lambda (label succs) (intset label)) succs))
        (subtract (lambda (in-1 kill-1)
                    (if in-1
                        (intset-subtract in-1 kill-1)
                        empty-intset)))
        (add intset-union)
        (meet intset-intersect*))
    (let ((in (intmap-replace init kfun empty-intset))
          (out init)
          (worklist (intset kfun)))
      (solve-flow-equations succs in out kill gen subtract add meet worklist))))

(define (intset-pop set)
  (match (intset-next set)
    (#f (values set #f))
    (i (values (intset-remove set i) i))))

(define-syntax-rule (make-worklist-folder* seed ...)
  (lambda (f worklist seed ...)
    (let lp ((worklist worklist) (seed seed) ...)
      (call-with-values (lambda () (intset-pop worklist))
        (lambda (worklist i)
          (if i
              (call-with-values (lambda () (f i seed ...))
                (lambda (i* seed ...)
                  (let add ((i* i*) (worklist worklist))
                    (match i*
                      (() (lp worklist seed ...))
                      ((i . i*) (add i* (intset-add worklist i)))))))
              (values seed ...)))))))

(define worklist-fold*
  (case-lambda
    ((f worklist seed)
     ((make-worklist-folder* seed) f worklist seed))))

(define-syntax-rule (true-idx idx) (ash idx 1))
(define-syntax-rule (false-idx idx) (1+ (ash idx 1)))

(define (compute-truthy-expressions conts kfun)
  "Compute a \"truth map\", indicating which expressions can be shown to
be true and/or false at each label in the function starting at KFUN.
Returns an intmap of intsets.  The even elements of the intset indicate
labels that may be true, and the odd ones indicate those that may be
false.  It could be that both true and false proofs are available."
  (define (propagate boolv succ out)
    (let* ((in (intmap-ref boolv succ (lambda (_) #f)))
           (in* (if in (intset-union in out) out)))
      (if (eq? in in*)
          (values '() boolv)
          (values (list succ)
                  (intmap-add boolv succ in* (lambda (old new) new))))))

  (define (visit-cont label boolv)
    (let ((in (intmap-ref boolv label)))
      (define (propagate0)
        (values '() boolv))
      (define (propagate1 succ)
        (propagate boolv succ in))
      (define (propagate2 succ0 succ1)
        (let*-values (((changed0 boolv) (propagate boolv succ0 in))
                      ((changed1 boolv) (propagate boolv succ1 in)))
          (values (append changed0 changed1) boolv)))
      (define (propagate-branch succ0 succ1)
        (let*-values (((changed0 boolv)
                       (propagate boolv succ0
                                  (intset-add in (false-idx label))))
                      ((changed1 boolv)
                       (propagate boolv succ1
                                  (intset-add in (true-idx label)))))
          (values (append changed0 changed1) boolv)))
      (define (propagate* succs)
        (fold2 (lambda (succ changed boolv)
                 (call-with-values (lambda () (propagate boolv succ in))
                   (lambda (changed* boolv)
                     (values (append changed* changed) boolv))))
               succs '() boolv))

      (match (intmap-ref conts label)
        (($ $kargs names vars term)
         (match term
           (($ $continue k)   (propagate1 k))
           (($ $branch kf kt) (propagate-branch kf kt))
           (($ $switch kf kt*) (propagate* (cons kf kt*)))
           (($ $prompt k kh)  (propagate2 k kh))
           (($ $throw)        (propagate0))))
        (($ $kreceive arity k)
         (propagate1 k))
        (($ $kfun src meta self tail clause)
         (if clause
             (propagate1 clause)
             (propagate0)))
        (($ $kclause arity kbody kalt)
         (if kalt
             (propagate2 kbody kalt)
             (propagate1 kbody)))
        (($ $ktail) (propagate0)))))

  (worklist-fold* visit-cont
                  (intset kfun)
                  (intmap-add empty-intmap kfun empty-intset)))

(define (lset-unionq old new)
  (lset-union eq? old new))
(define (meet-constants out out*)
  (if out (intmap-intersect out out* lset-unionq) out*))
(define (adjoin-constant in k v)
  (intmap-add in k (list v) lset-unionq))

(define (set-constants consts k in)
  (intmap-add consts k in (lambda (old new) new)))

(define (compute-consts conts kfun)
  "Compute a map of var to a list of constant values known to be bound
to variables at each label in CONTS.  If a var isn't present in the map
for a label, it isn't known to be constant at that label."
  (define (propagate consts succ out)
    (let* ((in (intmap-ref consts succ (lambda (_) #f)))
           (in* (meet-constants in out)))
      (if (eq? in in*)
          (values '() consts)
          (values (list succ) (set-constants consts succ in*)))))

  (define (visit-cont label consts)
    (let ((in (intmap-ref consts label)))
      (define (propagate0)
        (values '() consts))
      (define (propagate1 succ)
        (propagate consts succ in))
      (define (propagate2 succ0 succ1)
        (let*-values (((changed0 consts) (propagate consts succ0 in))
                      ((changed1 consts) (propagate consts succ1 in)))
          (values (append changed0 changed1) consts)))
      (define (propagate-branch succ0 succ1)
        (let*-values (((changed0 consts)
                       (propagate consts succ0
                                  (intset-add in (false-idx label))))
                      ((changed1 consts)
                       (propagate consts succ1
                                  (intset-add in (true-idx label)))))
          (values (append changed0 changed1) consts)))
      (define (propagate* succs)
        (fold2 (lambda (succ changed consts)
                 (call-with-values (lambda () (propagate consts succ in))
                   (lambda (changed* consts)
                     (values (append changed* changed) consts))))
               succs '() consts))
      (define (get-def k)
        (match (intmap-ref conts k)
          (($ $kargs (_) (v)) v)))
      (define (propagate-constant consts k v c)
        (propagate consts k (adjoin-constant in v c)))

      (match (intmap-ref conts label)
        (($ $kargs names vars term)
         (match term
           (($ $continue k src ($ $const c))
            (propagate-constant consts k (get-def k) c))
           (($ $continue k)
            (propagate1 k))
           (($ $branch kf kt src 'eq-constant? c (v))
            (let*-values (((changed0 consts) (propagate1 kf))
                          ((changed1 consts)
                           (propagate-constant consts kt v c)))
              (values (append changed0 changed1) consts)))
           (($ $branch kf kt)  (propagate2 kf kt))
           (($ $switch kf kt* src v)
            (let-values (((changed consts) (propagate1 kf)))
              (let lp ((i 0) (kt* kt*) (changed changed) (consts consts))
                (match kt*
                  (() (values changed consts))
                  ((k . kt*)
                   (call-with-values (lambda ()
                                       (propagate-constant consts k v i))
                     (lambda (changed* consts)
                       (lp (1+ i) kt* (append changed* changed) consts))))))))
           (($ $prompt k kh)   (propagate2 k kh))
           (($ $throw)         (propagate0))))
        (($ $kreceive arity k)
         (propagate1 k))
        (($ $kfun src meta self tail clause)
         (if clause
             (propagate1 clause)
             (propagate0)))
        (($ $kclause arity kbody kalt)
         (if kalt
             (propagate2 kbody kalt)
             (propagate1 kbody)))
        (($ $ktail) (propagate0)))))

  (worklist-fold* visit-cont
                  (intset kfun)
                  (intmap-add empty-intmap kfun empty-intmap)))

(define-record-type <analysis>
  (make-analysis effects clobbers preds avail truthy-labels consts)
  analysis?
  (effects analysis-effects)
  (clobbers analysis-clobbers)
  (preds analysis-preds)
  (avail analysis-avail)
  (truthy-labels analysis-truthy-labels)
  (consts analysis-consts))

;; When we determine that we can replace an expression with
;; already-bound variables, we change the expression to a $values.  At
;; its continuation, if it turns out that the $values expression is the
;; only predecessor, we elide the predecessor, to make redundant branch
;; folding easier.  Ideally, elision results in redundant branches
;; having multiple predecessors which already have values for the
;; branch.
;;
;; We could avoid elision, and instead search backwards when we get to a
;; branch that we'd like to elide.  However it's gnarly: branch elisions
;; reconfigure the control-flow graph, and thus affect the avail /
;; truthy maps.  If we forwarded such a distant predecessor, if there
;; were no intermediate definitions, we'd have to replay the flow
;; analysis from far away.  Maybe it's possible but it's not obvious.
;;
;; The elision mechanism is to rewrite predecessors to continue to the
;; successor.  We could have instead replaced the predecessor with the
;; body of the successor, but that would invalidate the values of the
;; avail / truthy maps, as well as the clobber sets.
;;
;; We can't always elide the predecessor though.  If any of the
;; predecessor's predecessors is a back-edge, it hasn't been
;; residualized yet and so we can't rewrite it.  This is an
;; implementation limitation.
;;
(define (forward-cont cont from to)
  (define (rename k) (if (eqv? k from) to k))
  (rewrite-cont cont
    (($ $kargs names vals ($ $continue k src exp))
     ($kargs names vals ($continue (rename k) src ,exp)))
    (($ $kargs names vals ($ $branch kf kt src op param args))
     ($kargs names vals ($branch (rename kf) (rename kt) src op param args)))
    (($ $kargs names vals ($ $switch kf kt* src arg))
     ($kargs names vals ($switch (rename kf) (map rename kt*) src arg)))
    (($ $kargs names vals ($ $prompt k kh src escape? tag))
     ($kargs names vals ($prompt (rename k) (rename kh) src escape? tag)))
    (($ $kreceive ($ $arity req () rest () #f) kbody)
     ($kreceive req rest (rename kbody)))
    (($ $kclause arity kbody kalternate)
     ;; Can only be a body continuation.
     ($kclause ,arity (rename kbody) kalternate))
    (($ $kfun src meta self tail kentry)
     ;; Can only be a $kargs clause continuation.
     ($kfun src meta self tail (rename kentry)))))

(define (elide-predecessor label pred out analysis)
  (match analysis
    (($ <analysis> effects clobbers preds avail truthy-labels consts)
     (let ((pred-preds (intmap-ref preds pred)))
       (and
        ;; Don't elide predecessors that are the targets of back-edges.
        (< (intset-prev pred-preds) pred)
        (cons
         (intset-fold
          (lambda (pred-pred out)
            (define (rename k) (if (eqv? k pred) label k))
            (intmap-replace!
             out pred-pred
             (forward-cont (intmap-ref out pred-pred) pred label)))
          pred-preds
          (intmap-remove out pred))
         (make-analysis effects
                        clobbers
                        (intmap-add (intmap-add preds label pred intset-remove)
                                    label pred-preds intset-union)
                        avail
                        truthy-labels
                        consts)))))))

(define (prune-branch analysis pred succ)
  (match analysis
    (($ <analysis> effects clobbers preds avail truthy-labels consts)
     (make-analysis effects
                    clobbers
                    (intmap-add preds succ pred intset-remove)
                    avail
                    truthy-labels
                    consts))))

(define (forward-branch analysis pred old-succ new-succ)
  (match analysis
    (($ <analysis> effects clobbers preds avail truthy-labels consts)
     (make-analysis effects
                    clobbers
                    (let ((preds (intmap-add preds old-succ pred
                                             intset-remove)))
                      (intmap-add preds new-succ pred intset-add))
                    avail
                    truthy-labels
                    consts))))

(define (prune-successors analysis pred succs)
  (intset-fold (lambda (succ analysis)
                 (prune-branch analysis pred succ))
               succs analysis))

(define (compute-out-edges analysis pred succ out)
  (match analysis
    (($ <analysis> effects clobbers preds avail truthy-labels consts)
     (let ((avail  (intmap-ref avail pred))
           (kill   (intmap-ref clobbers pred))
           (bool   (intmap-ref truthy-labels pred))
           (consts (intmap-ref consts pred)))
       (values (intset-add (intset-subtract avail kill) pred)
               (match (and (< pred succ) (intmap-ref out pred))
                 (($ $kargs _ _ ($ $branch kf kt))
                  (define (maybe-add bool k idx)
                    (if (eqv? k succ) (intset-add bool idx) bool))
                  (maybe-add (maybe-add bool kf (false-idx pred))
                             kt (true-idx pred)))
                 (_ bool))
               (match (and (< pred succ) (intmap-ref out pred))
                 (($ $kargs _ _ ($ $branch kf kt src 'eq-constant? c (v)))
                  (if (eqv? kf succ)
                      consts
                      (adjoin-constant consts v c)))
                 (_ consts)))))))

(define (propagate-analysis analysis label out)
  (match analysis
    (($ <analysis> effects clobbers preds avail truthy-labels consts)
     (call-with-values
         (lambda ()
           (intset-fold
            (lambda (pred avail-in bool-in consts-in)
              (call-with-values
                  (lambda ()
                    (compute-out-edges analysis pred label out))
                (lambda (avail-in* bool-in* consts-in*)
                  (values (intset-intersect* avail-in avail-in*)
                          (intset-union bool-in bool-in*)
                          (meet-constants consts-in consts-in*)))))
            (intmap-ref preds label) #f empty-intset #f))
       (lambda (avail-in bool-in consts-in)
         (make-analysis effects clobbers preds
                        (intmap-replace avail label avail-in)
                        (intmap-replace truthy-labels label bool-in)
                        (intmap-replace consts label consts-in)))))))

(define (term-successors term)
  (define (list->intset ls)
    (fold1 (lambda (elt set) (intset-add set elt)) ls empty-intset))
  (match term
    (($ $continue k) (intset k))
    (($ $branch kf kt) (intset kf kt))
    (($ $switch kf kt*) (list->intset (cons kf kt*)))
    (($ $prompt k kh) (intset k kh))
    (($ $throw) empty-intset)))

(define (intmap-select map keys)
  (persistent-intmap
   (intmap-fold (lambda (k v out)
                  (if (intset-ref keys k)
                      (intmap-add! out k v)
                      out))
                map empty-intmap)))

(define (make-equivalent-expression-table)
  ;; Table associating expressions with equivalent variables, indexed by
  ;; the label that defines them.
  (make-hash-table))
(define (add-equivalent-expression! table key label vars)
  (let ((equiv (hash-ref table key empty-intmap)))
    (define (allow-equal old new)
      (if (equal? old new)
          old
          (error "bad equiv var update" label old new)))
    (hash-set! table key
               (intmap-add equiv label vars allow-equal))))
(define (lookup-equivalent-expressions table key avail)
  (match (hash-ref table key)
    (#f empty-intmap)
    (equiv (intmap-select equiv avail))))

;; return #(taken not-taken), or #f if can't decide.
(define (fold-branch table key kf kt avail bool consts)
  (define (fold-constant-comparison)
    (match key
      (('eq-constant? c v)
       (match (intmap-ref consts v (lambda (v) #f))
         (#f   #f)
         ((c') (if (eq? c c')
                   (vector kt kf)
                   (vector kf kt)))
         (c*   (if (memq c c*)
                   #f
                   (vector kf kt)))))
      (_ #f)))
  (define (fold-redundant-branch)
    (let ((equiv (lookup-equivalent-expressions table key avail)))
      (let lp ((candidate (intmap-prev equiv)))
        (match candidate
          (#f #f)
          (_ (let ((t (intset-ref bool (true-idx candidate)))
                   (f (intset-ref bool (false-idx candidate))))
               (if (eqv? t f)
                   (lp (intmap-prev equiv (1- candidate)))
                   (if t
                       (vector kt kf)
                       (vector kf kt)))))))))
  (or (fold-constant-comparison)
      (fold-redundant-branch)))

(define (eliminate-common-subexpressions-in-fun kfun conts out substs)
  (define equivalent-expressions (make-equivalent-expression-table))
  (define (subst-var substs var)
    (intmap-ref substs var (lambda (var) var)))
  (define (subst-vars substs vars)
    (let lp ((vars vars))
      (match vars
        (() '())
        ((var . vars) (cons (subst-var substs var) (lp vars))))))

  (define (compute-branch-key branch)
    (match branch
      (($ $branch kf kt src op param args)  (cons* op param args))))
  (define (compute-expr-key expr)
    (match expr
      (($ $const val)                       (cons 'const val))
      (($ $prim name)                       (cons 'prim name))
      (($ $fun body)                        #f)
      (($ $rec names syms funs)             #f)
      (($ $const-fun label)                 #f)
      (($ $code label)                      (cons 'code label))
      (($ $call proc args)                  #f)
      (($ $callk k proc args)               #f)
      (($ $calli args callee)               #f)
      (($ $primcall name param args)        (cons* name param args))
      (($ $values args)                     #f)))
  (define (compute-term-key term)
    (match term
      (($ $continue k src exp)              (compute-expr-key exp))
      (($ $branch)                          (compute-branch-key term))
      (($ $switch)                          #f)
      (($ $prompt)                          #f)
      (($ $throw)                           #f)))

  (define (add-auxiliary-definitions! label defs substs term-key)
    (define (add-def! aux-key var)
      (add-equivalent-expression! equivalent-expressions aux-key label
                                  (list var)))
    (define-syntax add-definitions
      (syntax-rules (<-)
        ((add-definitions)
         #f)
        ((add-definitions
          ((def <- op arg ...) (aux <- op* arg* ...) ...)
          . clauses)
         (match term-key
           (('op arg ...)
            (match defs
              ((def) (add-def! (list 'op* arg* ...) aux) ...)))
           (_ (add-definitions . clauses))))
        ((add-definitions
          ((op arg ...) (aux <- op* arg* ...) ...)
          . clauses)
         (match term-key
           (('op arg ...)
            (add-def! (list 'op* arg* ...) aux) ...)
           (_ (add-definitions . clauses))))))
    (add-definitions
     ((scm-set! p s i x)               (x <- scm-ref p s i))
     ((scm-set!/tag p s x)             (x <- scm-ref/tag p s))
     ((scm-set!/immediate p s x)       (x <- scm-ref/immediate p s))
     ((word-set! p s i x)              (x <- word-ref p s i))
     ((word-set!/immediate p s x)      (x <- word-ref/immediate p s))
     ((pointer-set!/immediate p s x)   (x <- pointer-ref/immediate p s))

     ((p <- cons #f x y)               (x <- car #f p)
                                       (y <- cdr #f p))
     ((set-car! #f p x)                (x <- car #f p))
     ((set-cdr! #f p y)                (y <- cdr #f p))

     ((b <- box #f x)                  (x <- box-ref #f b))
     ((box-set! #f b x)                (x <- box-ref #f b))

     ((v <- allocate-vector #f n)      (n <- vector-length #f v))
     ((vector-set!/immediate p v x)    (x <- vector-ref/immediate p v))
     ((vector-set! #f v i x)           (x <- vector-ref #f v i))

     ((s <- allocate-struct n v)       (v <- struct-vtable #f s))
     ((struct-set! p s x)              (x <- struct-ref p s))

     ((u <- scm->f64 #f s)             (s <- f64->scm #f u))
     ((s <- f64->scm #f u)             (u <- scm->f64 #f s))
     ((u <- scm->u64 #f s)             (s <- u64->scm #f u))
     ((s <- u64->scm #f u)             (u <- scm->u64 #f s)
      (u <- scm->u64/truncate #f s))
     ((s <- u64->scm/unlikely #f u)    (u <- scm->u64 #f s)
      (u <- scm->u64/truncate #f s))
     ((u <- scm->s64 #f s)             (s <- s64->scm #f u))
     ((s <- s64->scm #f u)             (u <- scm->s64 #f s))
     ((s <- s64->scm/unlikely #f u)    (u <- scm->s64 #f s))
     ((u <- untag-fixnum #f s)         (s <- s64->scm #f u)
      (s <- tag-fixnum #f u))
     ;; NB: These definitions rely on U having top 2 bits equal to
     ;; 3rd (sign) bit.
     ((s <- tag-fixnum #f u)           (u <- scm->s64 #f s)
      (u <- untag-fixnum #f s))
     ((s <- u64->s64 #f u)             (u <- s64->u64 #f s))
     ((u <- s64->u64 #f s)             (s <- u64->s64 #f u))

     ((u <- untag-char #f s)           (s <- tag-char #f u))
     ((s <- tag-char #f u)             (u <- untag-char #f s))))

  (define (rename-uses term substs)
    (define (subst-var var)
      (intmap-ref substs var (lambda (var) var)))
    (define (rename-exp exp)
      (rewrite-exp exp
        ((or ($ $const) ($ $prim) ($ $fun) ($ $rec) ($ $const-fun) ($ $code))
         ,exp)
        (($ $call proc args)
         ($call (subst-var proc) ,(map subst-var args)))
        (($ $callk k proc args)
         ($callk k (and proc (subst-var proc)) ,(map subst-var args)))
        (($ $calli args callee)
         ($calli ,(map subst-var args) (subst-var callee)))
        (($ $primcall name param args)
         ($primcall name param ,(map subst-var args)))
        (($ $values args)
         ($values ,(map subst-var args)))))
    (rewrite-term term
      (($ $branch kf kt src op param args)
       ($branch kf kt src op param ,(map subst-var args)))
      (($ $switch kf kt* src arg)
       ($switch kf kt* src (subst-var arg)))
      (($ $continue k src exp)
       ($continue k src ,(rename-exp exp)))
      (($ $prompt k kh src escape? tag)
       ($prompt k kh src escape? (subst-var tag)))
      (($ $throw src op param args)
       ($throw src op param ,(map subst-var args)))))

  (define (visit-exp label exp analysis)
    (define (residualize) exp)
    (define (forward vals) (build-exp ($values vals)))
    (match (compute-expr-key exp)
      (#f (residualize))
      (key
       (match analysis
         (($ <analysis> effects clobbers preds avail truthy-labels consts)
          (match (lookup-equivalent-expressions equivalent-expressions
                                                key (intmap-ref avail label))
            ((? (lambda (x) (eq? x empty-intmap)))
             (residualize))
            (equiv
             (forward (intmap-ref equiv (intmap-next equiv))))))))))

  (define (maybe-forward-branch-predecessor label pred key kf kt out analysis)
    (cond
     ((<= label pred)
      ;; A backwards branch; punt.
      (values out analysis))
     (else
      (call-with-values (lambda ()
                          (compute-out-edges analysis pred label out))
        (lambda (pred-avail pred-bool pred-consts)
          (match (fold-branch equivalent-expressions key kf kt
                              pred-avail pred-bool pred-consts)
            (#(taken not-taken)
             (values (intmap-replace!
                      out pred
                      (forward-cont (intmap-ref out pred) label taken))
                     (forward-branch analysis pred label taken)))
            (#f
             (values out analysis))))))))

  (define (simplify-branch-predecessors label term out analysis)
    ;; if any predecessor's truthy-edge folds the branch, forward the
    ;; precedecessor.  may cause branch to become dead, or cause
    ;; remaining predecessor to eliminate.
    (match term
      (($ $branch kf kt)
       (let ((key (compute-branch-key term)))
         (match analysis
           (($ <analysis> effects clobbers preds avail truthy-labels consts)
            (call-with-values
                (lambda ()
                  (intset-fold
                   (lambda (pred out analysis)
                     (maybe-forward-branch-predecessor label pred
                                                       key kf kt out analysis))
                   (intmap-ref preds label) out analysis))
              (lambda (out* analysis*)
                (if (eq? analysis analysis*)
                    #f
                    (cons out* analysis*))))))))))

  (define (visit-branch label term analysis)
    (match term
      (($ $branch kf kt src)
       (match analysis
         (($ <analysis> effects clobbers preds avail truthy-labels consts)
          (let ((key (compute-branch-key term))
                (avail (intmap-ref avail label))
                (bool (intmap-ref truthy-labels label))
                (consts (intmap-ref consts label)))
            (match (fold-branch equivalent-expressions key kf kt avail bool
                                consts)
              (#(taken not-taken)
               (values (build-term ($continue taken src ($values ())))
                       (prune-branch analysis label not-taken)))
              (#f
               (values term analysis)))))))))

  (define (visit-term label names vars term out substs analysis)
    (let ((term (rename-uses term substs))
          (analysis (propagate-analysis analysis label out)))
      (match term
        (($ $branch)
         ;; Can only forward predecessors if this continuation binds no
         ;; values.
         (match (and (null? vars)
                     (simplify-branch-predecessors label term out analysis))
           (#f
            (call-with-values (lambda ()
                                (visit-branch label term analysis))
              (lambda (term analysis)
                (values (intmap-add! out label
                                     (build-cont ($kargs names vars ,term)))
                        substs
                        analysis))))
           ((out . analysis)
            ;; Recurse.
            (visit-label label (build-cont ($kargs names vars ,term))
                         out substs analysis))))
        (($ $continue k src exp)
         (values (intmap-add! out label
                              (build-cont
                                ($kargs names vars
                                  ($continue k src
                                    ,(visit-exp label exp analysis)))))
                 substs
                 analysis))
        ((or ($ $switch) ($ $prompt) ($ $throw))
         (values (intmap-add! out label (build-cont ($kargs names vars ,term)))
                 substs
                 analysis)))))

  (define (visit-label label cont out substs analysis)
    (match cont
      (($ $kargs names vars term)
       (define (visit-term-normally)
         (visit-term label names vars term out substs analysis))
       (match analysis
         (($ <analysis> effects clobbers preds avail truthy-labels consts)
          (let ((preds (intmap-ref preds label)))
            (cond
             ((eq? preds empty-intset)
              ;; Branch folding made this term unreachable.  Prune from
              ;; preds set.
              (values out substs
                      (prune-successors analysis label (term-successors term))))
             ((trivial-intset preds)
              => (lambda (pred)
                   (match (and (< pred label) (intmap-ref out pred))
                     (#f
                      ;; Orphan loop; branch folding must have removed
                      ;; entry.  Could still be alive though.
                      (visit-term-normally))
                     (($ $kargs names' vars' ($ $continue _ _ ($ $values vals)))
                      ;; Substitute dominating definitions, and try to elide the
                      ;; predecessor entirely.
                      (let ((substs (fold (lambda (var val substs)
                                            (intmap-add substs var val))
                                          substs vars vals)))
                        (match (elide-predecessor label pred out analysis)
                          (#f
                           ;; Can't elide; predecessor must be target of
                           ;; backwards branch.
                           (visit-term label names vars term out substs analysis))
                          ((out . analysis)
                           (visit-term label names' vars' term out substs analysis)))))
                     (($ $kargs _ _ term)
                      (match (compute-term-key term)
                        (#f #f)
                        (term-key
                         (let ((fx (intmap-ref effects pred)))
                           ;; Add residualized definition to the equivalence set.
                           ;; Note that expressions that allocate a fresh object
                           ;; or change the current fluid environment can't be
                           ;; eliminated by CSE (though DCE might do it if the
                           ;; value proves to be unused, in the allocation case).
                           (when (and (not (causes-effect? fx &allocation))
                                      (not (effect-clobbers? fx (&read-object &fluid))))
                             (add-equivalent-expression! equivalent-expressions term-key pred vars)))
                         ;; If the predecessor defines auxiliary definitions, as
                         ;; `cons' does for the results of `car' and `cdr', define
                         ;; those as well.
                         (add-auxiliary-definitions! pred vars substs term-key)))
                      (visit-term-normally))
                     ((or ($ $kclause) ($ $kfun) ($ $kreceive))
                      (visit-term-normally)))))
             (else
              (visit-term-normally)))))))
      (_ (values (intmap-add! out label cont) substs analysis))))

  ;; Because of the renumber pass, the labels are numbered in reverse
  ;; post-order, so the intmap-fold will visit definitions before
  ;; uses.
  (let* ((effects (synthesize-definition-effects (compute-effects conts)))
         (clobbers (compute-clobber-map conts effects))
         (succs (compute-successors conts kfun))
         (preds (invert-graph succs))
         (avail (compute-available-expressions succs kfun clobbers))
         (truthy-labels (compute-truthy-expressions conts kfun))
         (consts (compute-consts conts kfun)))
    (call-with-values
        (lambda ()
          (intmap-fold visit-label conts out substs
                       (make-analysis effects clobbers preds avail truthy-labels
                                      consts)))
      (lambda (out substs analysis)
        (values out substs)))))

(define (fold-renumbered-functions f conts . seeds)
  ;; Precondition: CONTS has been renumbered, and therefore functions
  ;; contained within it are topologically sorted, and the conts of each
  ;; function's body are numbered sequentially after the function's
  ;; $kfun.
  (define (next-function-body kfun)
    (match (intmap-ref conts kfun (lambda (_) #f))
      (#f #f)
      ((and cont ($ $kfun))
       (let lp ((k (1+ kfun)) (body (intmap-add! empty-intmap kfun cont)))
         (match (intmap-ref conts k (lambda (_) #f))
           ((or #f ($ $kfun))
            (persistent-intmap body))
           (cont
            (lp (1+ k) (intmap-add! body k cont))))))))

  (let fold ((kfun 0) (seeds seeds))
    (match (next-function-body kfun)
      (#f (apply values seeds))
      (conts
       (call-with-values (lambda () (apply f kfun conts seeds))
         (lambda seeds
           (fold (1+ (intmap-prev conts)) seeds)))))))

(define (eliminate-common-subexpressions conts)
  (let ((conts (renumber conts 0)))
    (persistent-intmap
     (fold-renumbered-functions eliminate-common-subexpressions-in-fun
                                conts empty-intmap empty-intmap))))
