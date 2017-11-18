;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.

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
  #:use-module (srfi srfi-11)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps effects-analysis)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:export (eliminate-common-subexpressions))

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

(define (compute-available-expressions conts kfun effects)
  "Compute and return a map of LABEL->ANCESTOR..., where ANCESTOR... is
an intset containing ancestor labels whose value is available at LABEL."
  (define (propagate avail succ out)
    (let* ((in (intmap-ref avail succ (lambda (_) #f)))
           (in* (if in (intset-intersect in out) out)))
      (if (eq? in in*)
          (values '() avail)
          (values (list succ)
                  (intmap-add avail succ in* (lambda (old new) new))))))

  (define (clobber label in)
    (let ((fx (intmap-ref effects label)))
      (cond
       ((not (causes-effect? fx &write))
        ;; Fast-path if this expression clobbers nothing.
        in)
       (else
        ;; Kill clobbered expressions.  FIXME: there is no need to check
        ;; on any label before than the last dominating label that
        ;; clobbered everything.  Another way to speed things up would
        ;; be to compute a clobber set per-effect, which we could
        ;; subtract from "in".
        (let lp ((label 0) (in in))
          (cond
           ((intset-next in label)
            => (lambda (label)
                 (if (effect-clobbers? fx (intmap-ref effects label))
                     (lp (1+ label) (intset-remove in label))
                     (lp (1+ label) in))))
           (else in)))))))

  (define (visit-cont label avail)
    (let* ((in (intmap-ref avail label))
           (out (intset-add (clobber label in) label)))
      (define (propagate0)
        (values '() avail))
      (define (propagate1 succ)
        (propagate avail succ out))
      (define (propagate2 succ0 succ1)
        (let*-values (((changed0 avail) (propagate avail succ0 out))
                      ((changed1 avail) (propagate avail succ1 out)))
          (values (append changed0 changed1) avail)))

      (match (intmap-ref conts label)
        (($ $kargs names vars ($ $continue k src exp))
         (match exp
           (($ $branch kt) (propagate2 k kt))
           (($ $prompt escape? tag handler) (propagate2 k handler))
           (_ (propagate1 k))))
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

(define (compute-truthy-expressions conts kfun)
  "Compute a \"truth map\", indicating which expressions can be shown to
be true and/or false at each label in the function starting at KFUN..
Returns an intmap of intsets.  The even elements of the intset indicate
labels that may be true, and the odd ones indicate those that may be
false.  It could be that both true and false proofs are available."
  (define (true-idx label) (ash label 1))
  (define (false-idx label) (1+ (ash label 1)))

  (define (propagate boolv succ out)
    (let* ((in (intmap-ref boolv succ (lambda (_) #f)))
           (in* (if in (intset-intersect in out) out)))
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

      (match (intmap-ref conts label)
        (($ $kargs names vars ($ $continue k src exp))
         (match exp
           (($ $branch kt) (propagate-branch k kt))
           (($ $prompt escape? tag handler) (propagate2 k handler))
           (_ (propagate1 k))))
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

  (intset-fold
   (lambda (kfun boolv)
     (worklist-fold* visit-cont
                     (intset kfun)
                     (intmap-add boolv kfun empty-intset)))
   (intmap-keys (compute-reachable-functions conts kfun))
   empty-intmap))

(define (intset-map f set)
  (persistent-intmap
   (intset-fold (lambda (i out) (intmap-add! out i (f i)))
                set
                empty-intmap)))

;; Returns a map of label-idx -> (var-idx ...) indicating the variables
;; defined by a given labelled expression.
(define (compute-defs conts kfun)
  (intset-map (lambda (label)
                (match (intmap-ref conts label)
                  (($ $kfun src meta self tail clause)
                   (list self))
                  (($ $kclause arity body alt)
                   (match (intmap-ref conts body)
                     (($ $kargs names vars) vars)))
                  (($ $kreceive arity kargs)
                   (match (intmap-ref conts kargs)
                     (($ $kargs names vars) vars)))
                  (($ $ktail)
                   '())
                  (($ $kargs names vars ($ $continue k))
                   (match (intmap-ref conts k)
                     (($ $kargs names vars) vars)
                     (_ #f)))))
               (compute-function-body conts kfun)))

(define (compute-singly-referenced succs)
  (define (visit label succs single multiple)
    (intset-fold (lambda (label single multiple)
                   (if (intset-ref single label)
                       (values single (intset-add! multiple label))
                       (values (intset-add! single label) multiple)))
                 succs single multiple))
  (call-with-values (lambda ()
                      (intmap-fold visit succs empty-intset empty-intset))
    (lambda (single multiple)
      (intset-subtract (persistent-intset single)
                       (persistent-intset multiple)))))

(define (compute-equivalent-subexpressions conts kfun effects)
  (define (visit-fun kfun equiv-labels var-substs)
    (let* ((succs (compute-successors conts kfun))
           (singly-referenced (compute-singly-referenced succs))
           (avail (compute-available-expressions conts kfun effects))
           (defs (compute-defs conts kfun))
           (equiv-set (make-hash-table)))
      (define (subst-var var-substs var)
        (intmap-ref var-substs var (lambda (var) var)))
      (define (subst-vars var-substs vars)
        (let lp ((vars vars))
          (match vars
            (() '())
            ((var . vars) (cons (subst-var var-substs var) (lp vars))))))

      (define (compute-exp-key var-substs exp)
        (match exp
          (($ $const val) (cons 'const val))
          (($ $prim name) (cons 'prim name))
          (($ $fun body) #f)
          (($ $rec names syms funs) #f)
          (($ $closure label nfree) #f)
          (($ $call proc args) #f)
          (($ $callk k proc args) #f)
          (($ $primcall name args)
           (cons* 'primcall name (subst-vars var-substs args)))
          (($ $branch _ ($ $primcall name args))
           (cons* 'primcall name (subst-vars var-substs args)))
          (($ $branch) #f)
          (($ $values args) #f)
          (($ $prompt escape? tag handler) #f)))

      (define (add-auxiliary-definitions! label var-substs exp-key)
        (define (subst var)
          (subst-var var-substs var))
        (let ((defs (intmap-ref defs label)))
          (define (add-def! aux-key var)
            (let ((equiv (hash-ref equiv-set aux-key '())))
              (hash-set! equiv-set aux-key
                         (acons label (list var) equiv))))
          (match exp-key
            (('primcall 'box val)
             (match defs
               ((box)
                (add-def! `(primcall box-ref ,(subst box)) val))))
            (('primcall 'box-set! box val)
             (add-def! `(primcall box-ref ,box) val))
            (('primcall 'cons car cdr)
             (match defs
               ((pair)
                (add-def! `(primcall car ,(subst pair)) car)
                (add-def! `(primcall cdr ,(subst pair)) cdr))))
            (('primcall 'set-car! pair car)
             (add-def! `(primcall car ,pair) car))
            (('primcall 'set-cdr! pair cdr)
             (add-def! `(primcall cdr ,pair) cdr))
            (('primcall (or 'make-vector 'make-vector/immediate) len fill)
             (match defs
               ((vec)
                (add-def! `(primcall vector-length ,(subst vec)) len))))
            (('primcall 'vector-set! vec idx val)
             (add-def! `(primcall vector-ref ,vec ,idx) val))
            (('primcall 'vector-set!/immediate vec idx val)
             (add-def! `(primcall vector-ref/immediate ,vec ,idx) val))
            (('primcall (or 'allocate-struct 'allocate-struct/immediate)
                        vtable size)
             (match defs
               ((struct)
                (add-def! `(primcall struct-vtable ,(subst struct))
                          vtable))))
            (('primcall 'struct-set! struct n val)
             (add-def! `(primcall struct-ref ,struct ,n) val))
            (('primcall 'struct-set!/immediate struct n val)
             (add-def! `(primcall struct-ref/immediate ,struct ,n) val))
            (('primcall 'scm->f64 scm)
             (match defs
               ((f64)
                (add-def! `(primcall f64->scm ,f64) scm))))
            (('primcall 'f64->scm f64)
             (match defs
               ((scm)
                (add-def! `(primcall scm->f64 ,scm) f64))))
            (('primcall 'scm->u64 scm)
             (match defs
               ((u64)
                (add-def! `(primcall u64->scm ,u64) scm))))
            (('primcall 'u64->scm u64)
             (match defs
               ((scm)
                (add-def! `(primcall scm->u64 ,scm) u64)
                (add-def! `(primcall scm->u64/truncate ,scm) u64))))
            (('primcall 'scm->s64 scm)
             (match defs
               ((s64)
                (add-def! `(primcall s64->scm ,s64) scm))))
            (('primcall 's64->scm s64)
             (match defs
               ((scm)
                (add-def! `(primcall scm->s64 ,scm) s64))))
            (_ #t))))

      (define (visit-label label equiv-labels var-substs)
        (match (intmap-ref conts label)
          (($ $kargs names vars ($ $continue k src exp))
           (let* ((exp-key (compute-exp-key var-substs exp))
                  (equiv (hash-ref equiv-set exp-key '()))
                  (fx (intmap-ref effects label))
                  (avail (intmap-ref avail label)))
             (define (finish equiv-labels var-substs)
               ;; If this expression defines auxiliary definitions,
               ;; as `cons' does for the results of `car' and `cdr',
               ;; define those.  Do so after finding equivalent
               ;; expressions, so that we can take advantage of
               ;; subst'd output vars.
               (add-auxiliary-definitions! label var-substs exp-key)
               (values equiv-labels var-substs))
             (let lp ((candidates equiv))
               (match candidates
                 (()
                  ;; No matching expressions.  Add our expression
                  ;; to the equivalence set, if appropriate.  Note
                  ;; that expressions that allocate a fresh object
                  ;; or change the current fluid environment can't
                  ;; be eliminated by CSE (though DCE might do it
                  ;; if the value proves to be unused, in the
                  ;; allocation case).
                  (when (and exp-key
                             (not (causes-effect? fx &allocation))
                             (not (effect-clobbers? fx (&read-object &fluid))))
                    (let ((defs (and (intset-ref singly-referenced k)
                                     (intmap-ref defs label))))
                      (when defs
                        (hash-set! equiv-set exp-key
                                   (acons label defs equiv)))))
                  (finish equiv-labels var-substs))
                 (((and head (candidate . vars)) . candidates)
                  (cond
                   ((not (intset-ref avail candidate))
                    ;; This expression isn't available here; try
                    ;; the next one.
                    (lp candidates))
                   (else
                    ;; Yay, a match.  Mark expression as equivalent.  If
                    ;; we provide the definitions for the successor, mark
                    ;; the vars for substitution.
                    (finish (intmap-add equiv-labels label head)
                            (let ((defs (and (intset-ref singly-referenced k)
                                             (intmap-ref defs label))))
                              (if defs
                                  (fold (lambda (def var var-substs)
                                          (intmap-add var-substs def var))
                                        var-substs defs vars)
                                  var-substs))))))))))
          (_ (values equiv-labels var-substs))))

      ;; Traverse the labels in fun in reverse post-order, which will
      ;; visit definitions before uses first.
      (fold2 visit-label
             (compute-reverse-post-order succs kfun)
             equiv-labels
             var-substs)))

  (intset-fold visit-fun
               (intmap-keys (compute-reachable-functions conts kfun))
               empty-intmap
               empty-intmap))

(define (apply-cse conts equiv-labels var-substs truthy-labels)
  (define (true-idx idx) (ash idx 1))
  (define (false-idx idx) (1+ (ash idx 1)))

  (define (subst-var var)
    (intmap-ref var-substs var (lambda (var) var)))

  (define (visit-exp exp)
    (rewrite-exp exp
      ((or ($ $const) ($ $prim) ($ $fun) ($ $rec) ($ $closure)) ,exp)
      (($ $call proc args)
       ($call (subst-var proc) ,(map subst-var args)))
      (($ $callk k proc args)
       ($callk k (subst-var proc) ,(map subst-var args)))
      (($ $primcall name args)
       ($primcall name ,(map subst-var args)))
      (($ $branch k exp)
       ($branch k ,(visit-exp exp)))
      (($ $values args)
       ($values ,(map subst-var args)))
      (($ $prompt escape? tag handler)
       ($prompt escape? (subst-var tag) handler))))

  (intmap-map
   (lambda (label cont)
     (match cont
       (($ $kargs names vars ($ $continue k src exp))
        (build-cont
          ($kargs names vars
            ,(match (intmap-ref equiv-labels label (lambda (_) #f))
               ((equiv . vars)
                (match exp
                  (($ $branch kt exp)
                   (let* ((bool (intmap-ref truthy-labels label))
                          (t (intset-ref bool (true-idx equiv)))
                          (f (intset-ref bool (false-idx equiv))))
                     (if (eqv? t f)
                         (build-term
                           ($continue k src
                             ($branch kt ,(visit-exp exp))))
                         (build-term
                           ($continue (if t kt k) src ($values ()))))))
                  (_
                   ;; For better or for worse, we only replace primcalls
                   ;; if they have an associated VM op, which allows
                   ;; them to continue to $kargs and thus we know their
                   ;; defs and can use a $values expression instead of a
                   ;; values primcall.
                   (build-term
                     ($continue k src ($values vars))))))
               (#f
                (build-term
                  ($continue k src ,(visit-exp exp))))))))
       (_ cont)))
   conts))

(define (eliminate-common-subexpressions conts)
  (call-with-values
      (lambda ()
        (let ((effects (synthesize-definition-effects (compute-effects conts))))
          (compute-equivalent-subexpressions conts 0 effects)))
    (lambda (equiv-labels var-substs)
      (let ((truthy-labels (compute-truthy-expressions conts 0)))
        (apply-cse conts equiv-labels var-substs truthy-labels)))))
