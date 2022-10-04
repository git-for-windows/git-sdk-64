;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015, 2017, 2018, 2019, 2020, 2021 Free Software Foundation, Inc.

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
;;; Helper facilities for working with CPS.
;;;
;;; Code:

(define-module (language cps utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (language cps)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:use-module (language cps graphs)
  #:export (;; Fresh names.
            label-counter var-counter
            fresh-label fresh-var
            with-fresh-name-state compute-max-label-and-var
            let-fresh

            ;; Graphs.
            compute-function-body
            compute-singly-referenced-labels
            compute-reachable-functions
            compute-successors
            compute-predecessors
            compute-idoms
            compute-dom-edges
            compute-defs-and-uses
            compute-var-representations)
  #:re-export (fold1 fold2
               trivial-intset
               intmap-map
               intmap-keys
               invert-bijection invert-partition
               intset->intmap
               intmap-select
               worklist-fold
               fixpoint

               ;; Flow analysis.
               invert-graph
               compute-reverse-post-order
               compute-strongly-connected-components
               compute-sorted-strongly-connected-components
               solve-flow-equations))

(define label-counter (make-parameter #f))
(define var-counter (make-parameter #f))

(define (fresh-label)
  (let ((count (or (label-counter)
                   (error "fresh-label outside with-fresh-name-state"))))
    (label-counter (1+ count))
    count))

(define (fresh-var)
  (let ((count (or (var-counter)
                   (error "fresh-var outside with-fresh-name-state"))))
    (var-counter (1+ count))
    count))

(define-syntax-rule (let-fresh (label ...) (var ...) body ...)
  (let* ((label (fresh-label)) ...
         (var (fresh-var)) ...)
    body ...))

(define-syntax-rule (with-fresh-name-state fun body ...)
  (call-with-values (lambda () (compute-max-label-and-var fun))
    (lambda (max-label max-var)
      (parameterize ((label-counter (1+ max-label))
                     (var-counter (1+ max-var)))
        body ...))))

(define (compute-max-label-and-var conts)
  (values (or (intmap-prev conts) -1)
          (intmap-fold (lambda (k cont max-var)
                         (match cont
                           (($ $kargs names syms body)
                            (apply max max-var syms))
                           (($ $kfun src meta (and self (not #f)))
                            (max max-var self))
                           (_ max-var)))
                       conts
                       -1)))

(define (compute-function-body conts kfun)
  (persistent-intset
   (let visit-cont ((label kfun) (labels empty-intset))
     (cond
      ((intset-ref labels label) labels)
      (else
       (let ((labels (intset-add! labels label)))
         (match (intmap-ref conts label)
           (($ $kreceive arity k) (visit-cont k labels))
           (($ $kfun src meta self ktail kclause)
            (let ((labels (visit-cont ktail labels)))
              (if kclause
                  (visit-cont kclause labels)
                  labels)))
           (($ $ktail) labels)
           (($ $kclause arity kbody kalt)
            (if kalt
                (visit-cont kalt (visit-cont kbody labels))
                (visit-cont kbody labels)))
           (($ $kargs names syms term)
            (match term
              (($ $continue k)
               (visit-cont k labels))
              (($ $branch kf kt)
               (visit-cont kf (visit-cont kt labels)))
              (($ $switch kf kt*)
               (visit-cont kf (fold1 visit-cont kt* labels)))
              (($ $prompt k kh)
               (visit-cont k (visit-cont kh labels)))
              (($ $throw)
               labels))))))))))

(define (compute-singly-referenced-labels conts)
  "Compute the set of labels in CONTS that have exactly one
predecessor."
  (define (add-ref label cont single multiple)
    (define (ref k single multiple)
      (if (intset-ref single k)
          (values single (intset-add! multiple k))
          (values (intset-add! single k) multiple)))
    (define (ref0) (values single multiple))
    (define (ref1 k) (ref k single multiple))
    (define (ref2 k k*)
      (if k*
          (let-values (((single multiple) (ref k single multiple)))
            (ref k* single multiple))
          (ref1 k)))
    (match cont
      (($ $kreceive arity k) (ref1 k))
      (($ $kfun src meta self ktail kclause) (ref2 ktail kclause))
      (($ $ktail) (ref0))
      (($ $kclause arity kbody kalt) (ref2 kbody kalt))
      (($ $kargs names syms ($ $continue k)) (ref1 k))
      (($ $kargs names syms ($ $branch kf kt)) (ref2 kf kt))
      (($ $kargs names syms ($ $switch kf kt*))
       (fold2 ref (cons kf kt*) single multiple))
      (($ $kargs names syms ($ $prompt k kh)) (ref2 k kh))
      (($ $kargs names syms ($ $throw)) (ref0))))
  (let*-values (((single multiple) (values empty-intset empty-intset))
                ((single multiple) (intmap-fold add-ref conts single multiple)))
    (intset-subtract (persistent-intset single)
                     (persistent-intset multiple))))

(define* (compute-reachable-functions conts #:optional (kfun 0))
  "Compute a mapping LABEL->LABEL..., where each key is a reachable
$kfun and each associated value is the body of the function, as an
intset."
  (define (intset-cons i set) (intset-add set i))
  (define (visit-fun kfun body to-visit)
    (intset-fold
     (lambda (label to-visit)
       (define (return kfun*) (fold intset-cons to-visit kfun*))
       (define (return1 kfun) (intset-add to-visit kfun))
       (define (return0) to-visit)
       (match (intmap-ref conts label)
         (($ $kargs _ _ ($ $continue _ _ exp))
          (match exp
            (($ $fun label) (return1 label))
            (($ $rec _ _ (($ $fun labels) ...)) (return labels))
            (($ $const-fun label) (return1 label))
            (($ $code label) (return1 label))
            (($ $callk label) (return1 label))
            (_ (return0))))
         (_ (return0))))
     body
     to-visit))
  (let lp ((to-visit (intset kfun)) (visited empty-intmap))
    (let ((to-visit (intset-subtract to-visit (intmap-keys visited))))
      (if (eq? to-visit empty-intset)
          visited
          (call-with-values
              (lambda ()
                (intset-fold
                 (lambda (kfun to-visit visited)
                   (let ((body (compute-function-body conts kfun)))
                     (values (visit-fun kfun body to-visit)
                             (intmap-add visited kfun body))))
                 to-visit
                 empty-intset
                 visited))
            lp)))))

(define* (compute-successors conts #:optional (kfun (intmap-next conts)))
  (define (visit label succs)
    (let visit ((label kfun) (succs empty-intmap))
      (define (propagate0)
        (intmap-add! succs label empty-intset))
      (define (propagate1 succ)
        (visit succ (intmap-add! succs label (intset succ))))
      (define (propagate2 succ0 succ1)
        (let ((succs (intmap-add! succs label (intset succ0 succ1))))
          (visit succ1 (visit succ0 succs))))
      (define (propagate* k*)
        (define (list->intset ls)
          (fold1 (lambda (elt set) (intset-add set elt)) ls empty-intset))
        (fold1 visit k* (intmap-add! succs label (list->intset k*))))
      (if (intmap-ref succs label (lambda (_) #f))
          succs
          (match (intmap-ref conts label)
            (($ $kargs names vars term)
             (match term
               (($ $continue k) (propagate1 k))
               (($ $branch kf kt) (propagate2 kf kt))
               (($ $switch kf kt*) (propagate* (cons kf kt*)))
               (($ $prompt k kh) (propagate2 k kh))
               (($ $throw) (propagate0))))
            (($ $kreceive arity k)
             (propagate1 k))
            (($ $kfun src meta self tail clause)
             (if clause
                 (propagate2 clause tail)
                 (propagate1 tail)))
            (($ $kclause arity kbody kalt)
             (if kalt
                 (propagate2 kbody kalt)
                 (propagate1 kbody)))
            (($ $ktail) (propagate0))))))
  (persistent-intmap (visit kfun empty-intmap)))

(define* (compute-predecessors conts kfun #:key
                               (labels (compute-function-body conts kfun)))
  (define (meet cdr car)
    (cons car cdr))
  (define (add-preds label preds)
    (define (add-pred k preds)
      (intmap-add! preds k label meet))
    (match (intmap-ref conts label)
      (($ $kreceive arity k)
       (add-pred k preds))
      (($ $kfun src meta self ktail kclause)
       (add-pred ktail (if kclause (add-pred kclause preds) preds)))
      (($ $ktail)
       preds)
      (($ $kclause arity kbody kalt)
       (add-pred kbody (if kalt (add-pred kalt preds) preds)))
      (($ $kargs names syms term)
       (match term
         (($ $continue k)   (add-pred k preds))
         (($ $branch kf kt) (add-pred kf (add-pred kt preds)))
         (($ $switch kf kt*) (fold1 add-pred (cons kf kt*) preds))
         (($ $prompt k kh)  (add-pred k (add-pred kh preds)))
         (($ $throw)        preds)))))
  (persistent-intmap
   (intset-fold add-preds labels
                (intset->intmap (lambda (label) '()) labels))))

;; Precondition: For each function in CONTS, the continuation names are
;; topologically sorted.
(define (compute-idoms conts kfun)
  ;; This is the iterative O(n^2) fixpoint algorithm, originally from
  ;; Allen and Cocke ("Graph-theoretic constructs for program flow
  ;; analysis", 1972).  See the discussion in Cooper, Harvey, and
  ;; Kennedy's "A Simple, Fast Dominance Algorithm", 2001.
  (let ((preds-map (compute-predecessors conts kfun)))
    (define (compute-idom idoms preds)
      (define (idom-ref label)
        (intmap-ref idoms label (lambda (_) #f)))
      (match preds
        (() -1)
        ((pred) pred)                   ; Shortcut.
        ((pred . preds)
         (define (common-idom d0 d1)
           ;; We exploit the fact that a reverse post-order is a
           ;; topological sort, and so the idom of a node is always
           ;; numerically less than the node itself.
           (let lp ((d0 d0) (d1 d1))
             (cond
              ;; d0 or d1 can be false on the first iteration.
              ((not d0) d1)
              ((not d1) d0)
              ((= d0 d1) d0)
              ((< d0 d1) (lp d0 (idom-ref d1)))
              (else (lp (idom-ref d0) d1)))))
         (fold1 common-idom preds pred))))
    (define (adjoin-idom label preds idoms)
      (let ((idom (compute-idom idoms preds)))
        ;; Don't use intmap-add! here.
        (intmap-add idoms label idom (lambda (old new) new))))
    (fixpoint (lambda (idoms)
                (intmap-fold adjoin-idom preds-map idoms))
              empty-intmap)))

;; Compute a vector containing, for each node, a list of the nodes that
;; it immediately dominates.  These are the "D" edges in the DJ tree.
(define (compute-dom-edges idoms)
  (define (snoc cdr car) (cons car cdr))
  (persistent-intmap
   (intmap-fold (lambda (label idom doms)
                  (let ((doms (intmap-add! doms label '())))
                    (cond
                     ((< idom 0) doms) ;; No edge to entry.
                     (else (intmap-add! doms idom label snoc)))))
                idoms
                empty-intmap)))

(define (compute-defs-and-uses cps)
  "Return two LABEL->VAR... maps indicating values defined at and used
by a label, respectively."
  (define (vars->intset vars)
    (fold (lambda (var set) (intset-add set var)) empty-intset vars))
  (define-syntax-rule (persistent-intmap2 exp)
    (call-with-values (lambda () exp)
      (lambda (a b)
        (values (persistent-intmap a) (persistent-intmap b)))))
  (persistent-intmap2
   (intmap-fold
    (lambda (label cont defs uses)
      (define (get-defs k)
        (match (intmap-ref cps k)
          (($ $kargs names vars) (vars->intset vars))
          (_ empty-intset)))
      (define (return d u)
        (values (intmap-add! defs label d)
                (intmap-add! uses label u)))
      (match cont
        (($ $kfun src meta self tail clause)
         (return (intset-union
                  (if clause (get-defs clause) empty-intset)
                  (if self (intset self) empty-intset))
                 empty-intset))
        (($ $kargs _ _ ($ $continue k src exp))
         (match exp
           ((or ($ $const) ($ $const-fun) ($ $code))
            (return (get-defs k) empty-intset))
           (($ $call proc args)
            (return (get-defs k) (intset-add (vars->intset args) proc)))
           (($ $callk _ proc args)
            (let ((args (vars->intset args)))
              (return (get-defs k) (if proc (intset-add args proc) args))))
           (($ $primcall name param args)
            (return (get-defs k) (vars->intset args)))
           (($ $values args)
            (return (get-defs k) (vars->intset args)))))
        (($ $kargs _ _ ($ $branch kf kt src op param args))
         (return empty-intset (vars->intset args)))
        (($ $kargs _ _ ($ $switch kf kt* src arg))
         (return empty-intset (intset arg)))
        (($ $kargs _ _ ($ $prompt k kh src escape? tag))
         (return empty-intset (intset tag)))
        (($ $kargs _ _ ($ $throw src op param args))
         (return empty-intset (vars->intset args)))
        (($ $kclause arity body alt)
         (return (get-defs body) empty-intset))
        (($ $kreceive arity kargs)
         (return (get-defs kargs) empty-intset))
        (($ $ktail)
         (return empty-intset empty-intset))))
    cps
    empty-intmap
    empty-intmap)))

(define (compute-var-representations cps)
  (define (get-defs k)
    (match (intmap-ref cps k)
      (($ $kargs names vars) vars)
      (_ '())))
  (intmap-fold
   (lambda (label cont representations)
     (match cont
       (($ $kargs _ _ ($ $continue k _ exp))
        (match (get-defs k)
          (() representations)
          ((var)
           (match exp
             (($ $values (arg))
              (intmap-add representations var
                          (intmap-ref representations arg)))
             (($ $callk)
              (intmap-add representations var 'scm))
             (($ $primcall (or 'scm->f64 'load-f64 's64->f64
                               'f32-ref 'f64-ref
                               'fadd 'fsub 'fmul 'fdiv 'fsqrt 'fabs
                               'ffloor 'fceiling
                               'fsin 'fcos 'ftan 'fasin 'facos 'fatan 'fatan2))
              (intmap-add representations var 'f64))
             (($ $primcall (or 'scm->u64 'scm->u64/truncate 'load-u64
                               's64->u64
                               'assume-u64
                               'uadd 'usub 'umul
                               'ulogand 'ulogior 'ulogxor 'ulogsub 'ursh 'ulsh
                               'uadd/immediate 'usub/immediate 'umul/immediate
                               'ursh/immediate 'ulsh/immediate
                               'u8-ref 'u16-ref 'u32-ref 'u64-ref
                               'word-ref 'word-ref/immediate
                               'untag-char))
              (intmap-add representations var 'u64))
             (($ $primcall (or 'untag-fixnum
                               'assume-s64
                               'scm->s64 'load-s64 'u64->s64
                               'srsh 'srsh/immediate
                               's8-ref 's16-ref 's32-ref 's64-ref))
              (intmap-add representations var 's64))
             (($ $primcall (or 'pointer-ref/immediate
                               'tail-pointer-ref/immediate))
              (intmap-add representations var 'ptr))
             (($ $code)
              (intmap-add representations var 'u64))
             (_
              (intmap-add representations var 'scm))))
          (vars
           (match exp
             (($ $values args)
              (fold (lambda (arg var representations)
                      (intmap-add representations var
                                  (intmap-ref representations arg)))
                    representations args vars))
             (($ $callk)
              (fold1 (lambda (var representations)
                      (intmap-add representations var 'scm))
                     vars representations))))))
       (($ $kargs _ _ (or ($ $branch) ($ $switch) ($ $prompt) ($ $throw)))
        representations)
       (($ $kfun src meta self tail entry)
        (let* ((representations (if self
                                   (intmap-add representations self 'scm)
                                   representations))
               (defs (get-defs entry))
               (reprs (or (assq-ref meta 'arg-representations)
                          (map (lambda (_) 'scm) defs))))
          (fold (lambda (var repr representations)
                   (intmap-add representations var repr))
                representations defs reprs)))
       (($ $kclause arity body alt)
        (fold1 (lambda (var representations)
                 (intmap-add representations var 'scm))
               (get-defs body) representations))
       (($ $kreceive arity kargs)
        (fold1 (lambda (var representations)
                 (intmap-add representations var 'scm))
               (get-defs kargs) representations))
       (($ $ktail) representations)))
   cps
   empty-intmap))
