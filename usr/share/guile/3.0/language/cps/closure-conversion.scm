;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2021 Free Software Foundation, Inc.

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
;;; This pass converts a CPS term in such a way that no function has any
;;; free variables.  Instead, closures are built explicitly as heap
;;; objects, and free variables are referenced through the closure.
;;;
;;; Closure conversion also removes any $rec expressions that
;;; contification did not handle.  See (language cps) for a further
;;; discussion of $rec.
;;;
;;; Before closure conversion, function self variables are always bound.
;;; After closure conversion, well-known functions with no free
;;; variables may have no self reference.
;;;
;;; Code:

(define-module (language cps closure-conversion)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold
                                        filter-map
                                        ))
  #:use-module (srfi srfi-11)
  #:use-module (system base types internal)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:export (convert-closures))

(define (compute-program-body functions)
  (intmap-fold (lambda (label body out) (intset-union body out))
               functions
               empty-intset))

(define (filter-reachable conts functions)
  (intmap-select conts (compute-program-body functions)))

(define (compute-non-operator-uses conts)
  (persistent-intset
   (intmap-fold
    (lambda (label cont uses)
      (define (add-use var uses) (intset-add! uses var))
      (define (add-uses vars uses)
        (match vars
          (() uses)
          ((var . vars) (add-uses vars (add-use var uses)))))
      (match cont
        (($ $kargs _ _ ($ $continue _ _ exp))
         (match exp
           ((or ($ $const) ($ $prim) ($ $fun) ($ $rec)) uses)
           (($ $values args)
            (add-uses args uses))
           (($ $call proc args)
            (add-uses args uses))
           (($ $callk label proc args)
            (let ((uses (add-uses args uses)))
              (if proc
                  (add-use proc uses)
                  uses)))
           (($ $primcall name param args)
            (add-uses args uses))))
        (($ $kargs _ _ ($ $branch kf kt src op param args))
         (add-uses args uses))
        (($ $kargs _ _ ($ $switch kf kt* src arg))
         (add-use arg uses))
        (($ $kargs _ _ ($ $prompt k kh src escape? tag))
         (add-use tag uses))
        (($ $kargs _ _ ($ $throw src op param args))
         (add-uses args uses))
        (_ uses)))
    conts
    empty-intset)))

(define (compute-function-names conts functions)
  "Compute a map of FUN-LABEL->BOUND-VAR... for each labelled function
whose bound vars we know."
  (define (add-named-fun var kfun out)
    (let ((self (match (intmap-ref conts kfun)
                  (($ $kfun src meta self) self))))
      (intmap-add out kfun (intset var self))))
  (intmap-fold
   (lambda (label body out)
     (let* ((conts (intmap-select conts body))
            (single (compute-singly-referenced-labels conts)))
       (intmap-fold
        (lambda (label cont out)
          (match cont
            (($ $kargs _ _ ($ $continue k _ ($ $fun kfun)))
             (if (intset-ref single k)
                 (match (intmap-ref conts k)
                   (($ $kargs (_) (var)) (add-named-fun var kfun out))
                   (_ out))
                 out))
            (($ $kargs _ _ ($ $continue k _ ($ $rec _ vars (($ $fun kfun) ...))))
             (unless (intset-ref single k)
               (error "$rec continuation has multiple predecessors??"))
             (fold add-named-fun out vars kfun))
            (_ out)))
        conts
        out)))
   functions
   empty-intmap))

(define (compute-well-known-functions conts bound->label)
  "Compute a set of labels indicating the well-known functions in
@var{conts}.  A well-known function is a function whose bound names we
know and which is never used in a non-operator position."
  (intset-subtract
   (persistent-intset
    (intmap-fold (lambda (bound label candidates)
                   (intset-add! candidates label))
                 bound->label
                 empty-intset))
   (persistent-intset
    (intset-fold (lambda (var not-well-known)
                   (match (intmap-ref bound->label var (lambda (_) #f))
                     (#f not-well-known)
                     (label (intset-add! not-well-known label))))
                 (compute-non-operator-uses conts)
                 empty-intset))))

(define (intset-cons i set)
  (intset-add set i))

(define (compute-shared-closures conts well-known)
  "Compute a map LABEL->VAR indicating the sets of functions that will
share a closure.  If a functions's label is in the map, it is shared.
The entries indicate the var of the shared closure, which will be one of
the bound vars of the closure."
  (intmap-fold
   (lambda (label cont out)
     (match cont
       (($ $kargs _ _
           ($ $continue _ _ ($ $rec names vars (($ $fun kfuns) ...))))
        ;; The split-rec pass should have ensured that this $rec forms a
        ;; strongly-connected component, so the free variables from all of
        ;; the functions will be alive as long as one of the closures is
        ;; alive.  For that reason we can consider storing all free
        ;; variables in one closure and sharing it.
        (let* ((kfuns-set (fold intset-cons empty-intset kfuns))
               (unknown-kfuns (intset-subtract kfuns-set well-known)))
          (cond
           ((or (eq? empty-intset kfuns-set) (trivial-intset kfuns-set))
            ;; There is only zero or one function bound here.  Trivially
            ;; shared already.
            out)
           ((eq? empty-intset unknown-kfuns)
            ;; All functions are well-known; we can share a closure.  Use
            ;; the first bound variable.
            (let ((closure (car vars)))
              (intset-fold (lambda (kfun out)
                             (intmap-add out kfun closure))
                           kfuns-set out)))
           ((trivial-intset unknown-kfuns)
            => (lambda (unknown-kfun)
                 ;; Only one function is not-well-known.  Use that
                 ;; function's closure as the shared closure.
                 (let ((closure (assq-ref (map cons kfuns vars) unknown-kfun)))
                   (intset-fold (lambda (kfun out)
                                  (intmap-add out kfun closure))
                                kfuns-set out))))
           (else
            ;; More than one not-well-known function means we need more
            ;; than one proper closure, so we can't share.
            out))))
       (_ out)))
   conts
   empty-intmap))

(define* (rewrite-shared-closure-calls cps functions label->bound shared kfun)
  "Rewrite CPS such that every call to a function with a shared closure
instead is a $callk to that label, but passing the shared closure as the
proc argument.  For recursive calls, use the appropriate 'self'
variable, if possible.  Also rewrite uses of the non-well-known but
shared closures to use the appropriate 'self' variable, if possible."
  ;; env := var -> (var . label)
  (define (visit-fun kfun cps env)
    (define (subst var)
      (match (intmap-ref env var (lambda (_) #f))
        (#f var)
        ((var . label) var)))

    (define (visit-exp exp)
      (rewrite-exp exp
        ((or ($ $const) ($ $prim)) ,exp)
        (($ $call proc args)
         ,(let ((args (map subst args)))
            (rewrite-exp (intmap-ref env proc (lambda (_) #f))
              (#f ($call proc ,args))
              ((closure . label) ($callk label closure ,args)))))
        (($ $callk label proc args)
         ($callk label (and proc (subst proc)) ,(map subst args)))
        (($ $primcall name param args)
         ($primcall name param ,(map subst args)))
        (($ $values args)
         ($values ,(map subst args)))))

    (define (visit-term term)
      (rewrite-term term
        (($ $continue k src exp)
         ($continue k src ,(visit-exp exp)))
        (($ $branch kf kt src op param args)
         ($branch kf kt src op param ,(map subst args)))
        (($ $switch kf kt* src arg)
         ($switch kf kt* src (subst arg)))
        (($ $prompt k kh src escape? tag)
         ($prompt k kh src escape? (subst tag)))
        (($ $throw src op param args)
         ($throw src op param ,(map subst args)))))

    (define (visit-rec labels vars cps)
      (define (compute-env label bound self rec-bound rec-labels env)
        (define (add-bound-var bound label env)
          (intmap-add env bound (cons self label) (lambda (old new) new)))
        (if (intmap-ref shared label (lambda (_) #f))
            ;; Within a function with a shared closure, rewrite
            ;; references to bound vars to use the "self" var.
            (fold add-bound-var env rec-bound rec-labels)
            ;; Otherwise be sure to use "self" references in any
            ;; closure.
            (add-bound-var bound label env)))
      (fold (lambda (label var cps)
              (match (intmap-ref cps label)
                (($ $kfun src meta self)
                 (visit-fun label cps
                            (compute-env label var self vars labels env)))))
            cps labels vars))

    (define (visit-cont label cps)
      (match (intmap-ref cps label)
        (($ $kargs names vars
            ($ $continue k src ($ $fun label)))
         (visit-fun label cps env))
        (($ $kargs _ _
            ($ $continue k src ($ $rec names vars (($ $fun labels) ...))))
         (visit-rec labels vars cps))
        (($ $kargs names vars term)
         (with-cps cps
           (setk label ($kargs names vars ,(visit-term term)))))
        (_ cps)))

    (intset-fold visit-cont (intmap-ref functions kfun) cps))

  ;; Initial environment is bound-var -> (shared-var . label) map for
  ;; functions with shared closures.
  (let ((env (intmap-fold (lambda (label shared env)
                            (intset-fold (lambda (bound env)
                                           (intmap-add env bound
                                                       (cons shared label)))
                                         (intset-remove
                                          (intmap-ref label->bound label)
                                          (match (intmap-ref cps label)
                                            (($ $kfun src meta self) self)))
                                         env))
                          shared
                          empty-intmap)))
    (persistent-intmap (visit-fun kfun cps env))))

(define (compute-free-vars conts kfun shared)
  "Compute a FUN-LABEL->FREE-VAR... map describing all free variable
references."
  (define (add-def var defs) (intset-add! defs var))
  (define (add-defs vars defs)
    (match vars
      (() defs)
      ((var . vars) (add-defs vars (add-def var defs)))))
  (define (add-use var uses)
    (intset-add! uses var))
  (define (add-uses vars uses)
    (match vars
      (() uses)
      ((var . vars) (add-uses vars (add-use var uses)))))
  (define (visit-nested-funs body)
    (intset-fold
     (lambda (label out)
       (match (intmap-ref conts label)
         (($ $kargs _ _ ($ $continue _ _ ($ $fun kfun)))
          (intmap-union out (visit-fun kfun)))
         ;; Convention is that functions not bound by $fun / $rec and
         ;; thus reachable only via $callk and such have no free
         ;; variables.
         (($ $kargs _ _ ($ $continue _ _
                           ($ $rec _ _ (($ $fun labels) ...))))
          (let* ((out (fold (lambda (kfun out)
                              (intmap-union out (visit-fun kfun)))
                            out labels))
                 (free (fold (lambda (kfun free)
                               (intset-union free (intmap-ref out kfun)))
                             empty-intset labels)))
            (fold (lambda (kfun out)
                    ;; For functions that share a closure, the free
                    ;; variables for one will be the union of the free
                    ;; variables for all.
                    (if (intmap-ref shared kfun (lambda (_) #f))
                        (intmap-replace out kfun free)
                        out))
                  out
                  labels)))
         (_ out)))
     body
     empty-intmap))
  (define (visit-fun kfun)
    (let* ((body (compute-function-body conts kfun))
           (free (visit-nested-funs body)))
      (call-with-values
          (lambda ()
            (intset-fold
             (lambda (label defs uses)
               (match (intmap-ref conts label)
                 (($ $kargs names vars term)
                  (values
                   (add-defs vars defs)
                   (match term
                     (($ $continue k src exp)
                      (match exp
                        ((or ($ $const) ($ $prim)) uses)
                        (($ $fun kfun)
                         (intset-union (persistent-intset uses)
                                       (intmap-ref free kfun)))
                        (($ $rec names vars (($ $fun kfun) ...))
                         (fold (lambda (kfun uses)
                                 (intset-union (persistent-intset uses)
                                               (intmap-ref free kfun)))
                               uses kfun))
                        (($ $values args)
                         (add-uses args uses))
                        (($ $call proc args)
                         (add-use proc (add-uses args uses)))
                        (($ $callk label proc args)
                         (let ((uses (add-uses args uses)))
                           (if proc
                               (add-use proc uses)
                               uses)))
                        (($ $primcall name param args)
                         (add-uses args uses))))
                     (($ $branch kf kt src op param args)
                      (add-uses args uses))
                     (($ $switch kf kt* src arg)
                      (add-use arg uses))
                     (($ $prompt k kh src escape? tag)
                      (add-use tag uses))
                     (($ $throw src op param args)
                      (add-uses args uses)))))
                 (($ $kfun src meta self)
                  (values (if self (add-def self defs) defs) uses))
                 (_ (values defs uses))))
             body empty-intset empty-intset))
        (lambda (defs uses)
          (intmap-add free kfun (intset-subtract
                                 (persistent-intset uses)
                                 (persistent-intset defs)))))))
  ;; Ensure that functions only reachable by $callk are present in the
  ;; free-vars map, albeit with empty-intset.  Note that if front-ends
  ;; start emitting $callk to targets with free variables, we will need
  ;; to do a better job here!
  (define (ensure-all-functions-have-free-vars free-vars)
    (intmap-fold
     (lambda (label cont out)
       (match cont
         (($ $kfun)
          (intmap-add out label empty-intset intset-union))
         (_ out)))
     conts
     free-vars))
  (ensure-all-functions-have-free-vars (visit-fun kfun)))

(define (eliminate-closure? label free-vars)
  (eq? (intmap-ref free-vars label) empty-intset))

(define (closure-label label shared bound->label)
  (cond
   ((intmap-ref shared label (lambda (_) #f))
    => (lambda (closure)
         (intmap-ref bound->label closure)))
   (else label)))

(define (closure-alias label well-known free-vars)
  (and (intset-ref well-known label)
       (trivial-intset (intmap-ref free-vars label))))

(define (prune-free-vars free-vars bound->label well-known shared)
  "Given the label->bound-var map @var{free-vars}, remove free variables
that are known functions with zero free variables, and replace
references to well-known functions with one free variable with that free
variable, until we reach a fixed point on the free-vars map."
  (define (prune-free in-label free free-vars)
    (intset-fold (lambda (var free)
                   (match (intmap-ref bound->label var (lambda (_) #f))
                     (#f free)
                     (label
                      (cond
                       ((eliminate-closure? label free-vars)
                        (intset-remove free var))
                       ((closure-alias (closure-label label shared bound->label)
                                       well-known free-vars)
                        => (lambda (alias)
                             ;; If VAR is free in LABEL, then ALIAS must
                             ;; also be free because its definition must
                             ;; precede VAR's definition.
                             (intset-add (intset-remove free var) alias)))
                       (else free)))))
                 free free))
  (fixpoint (lambda (free-vars)
              (intmap-fold (lambda (label free free-vars)
                             (intmap-replace free-vars label
                                             (prune-free label free free-vars)))
                           free-vars
                           free-vars))
            free-vars))

(define (intset-find set i)
  (let lp ((idx 0) (start #f))
    (let ((start (intset-next set start)))
      (cond
       ((not start) (error "not found" set i))
       ((= start i) idx)
       (else (lp (1+ idx) (1+ start)))))))

(define (intset-count set)
  (intset-fold (lambda (_ count) (1+ count)) set 0))

(define (compute-elidable-closures cps well-known shared free-vars)
  "Compute the set of well-known callees with no free variables.  Calls
to these functions can avoid passing a closure parameter.  Note however
that we have to exclude well-known callees that are part of a shared
closure that contains any not-well-known member."
  (define (intset-map f set)
    (persistent-intset
     (intset-fold (lambda (i out) (if (f i) (intset-add! out i) out))
                  set
                  empty-intset)))

  (let ((no-free-vars (persistent-intset
                       (intmap-fold (lambda (label free out)
                                      (if (eq? empty-intset free)
                                          (intset-add! out label)
                                          out))
                                    free-vars empty-intset)))
        (shared
         (intmap-fold
          (lambda (label cont out)
            (match cont
              (($ $kargs _ _
                  ($ $continue _ _ ($ $rec _ _ (($ $fun kfuns) ...))))
               ;; Either all of these functions share a closure, in
               ;; which all or all except one of them are well-known, or
               ;; none of the functions share a closure.
               (if (intmap-ref shared (car kfuns) (lambda (_) #f))
                   (let* ((scc (fold intset-cons empty-intset kfuns)))
                     (intset-fold (lambda (label out)
                                    (intmap-add out label scc))
                                  scc out))
                   out))
              (_ out)))
          cps
          empty-intmap)))
    (intmap-fold (lambda (label labels elidable)
                   (if (eq? labels (intset-intersect labels well-known))
                       elidable
                       (intset-subtract elidable labels)))
                 shared
                 (intset-intersect well-known no-free-vars))))

(define (convert-one cps label body free-vars bound->label well-known shared
                     elidable)
  (define (well-known? label)
    (intset-ref well-known label))

  (let* ((free (intmap-ref free-vars label))
         (nfree (intset-count free))
         (self-known? (well-known? (closure-label label shared bound->label)))
         (self (match (intmap-ref cps label) (($ $kfun _ _ self) self))))
    (define (convert-arg cps var k)
      "Convert one possibly free variable reference to a bound reference.

If @var{var} is free, it is replaced by a closure reference via a
@code{free-ref} primcall, and @var{k} is called with the new var.
Otherwise @var{var} is bound, so @var{k} is called with @var{var}."
      ;; We know that var is not the name of a well-known function.
      (cond
       ((and=> (intmap-ref bound->label var (lambda (_) #f))
               (lambda (kfun)
                 (and (eq? empty-intset (intmap-ref free-vars kfun))
                      kfun)))
        ;; A not-well-known function with zero free vars.  Copy as a
        ;; constant, relying on the linker to reify just one copy.
        => (lambda (kfun)
             ;; It may well be that "var" is the "self" of another
             ;; member of an SCC containing just one not-well-known
             ;; function.  But here we're asking for the value of the
             ;; closure, which is the $const-fun of the non-well-known
             ;; member.
             (let ((kfun (closure-label kfun shared bound->label)))
               (with-cps cps
                 (letv var*)
                 (let$ body (k var*))
                 (letk k* ($kargs (#f) (var*) ,body))
                 (build-term ($continue k* #f ($const-fun kfun)))))))
       ((intset-ref free var)
        (if (and self-known? (eqv? 1 nfree))
            ;; A reference to the one free var of a well-known function.
            (with-cps cps
              ($ (k self)))
            (let* ((idx (intset-find free var))
                   (param (cond
                           ((not self-known?) (cons 'closure (+ idx 2)))
                           ((= nfree 2)       (cons 'pair idx))
                           (else              (cons 'vector (+ idx 1))))))
              (with-cps cps
                (letv var*)
                (let$ body (k var*))
                (letk k* ($kargs (#f) (var*) ,body))
                (build-term
                  ($continue k* #f
                    ($primcall 'scm-ref/immediate param (self))))))))
       (else
        (with-cps cps
          ($ (k var))))))
  
    (define (convert-args cps vars k)
      "Convert a number of possibly free references to bound references.
@var{k} is called with the bound references, and should return the
term."
      (match vars
        (()
         (with-cps cps
           ($ (k '()))))
        ((var . vars)
         (convert-arg cps var
           (lambda (cps var)
             (convert-args cps vars
               (lambda (cps vars)
                 (with-cps cps
                   ($ (k (cons var vars)))))))))))
  
    (define (allocate-closure cps k src label known? nfree)
      "Allocate a new closure, and pass it to $var{k}."
      (match (vector known? nfree)
        (#(#f 0)
         ;; The call sites cannot be enumerated, but the closure has no
         ;; identity; statically allocate it.
         (with-cps cps
           (build-term ($continue k src ($const-fun label)))))
        (#(#f nfree)
         ;; The call sites cannot be enumerated; allocate a closure.
         (with-cps cps
           (letv closure tag code)
           (letk k* ($kargs () ()
                      ($continue k src ($values (closure)))))
           (letk kinit ($kargs ('code) (code)
                         ($continue k* src
                           ($primcall 'word-set!/immediate '(closure . 1)
                                      (closure code)))))
           (letk kcode ($kargs () ()
                         ($continue kinit src ($code label))))
           (letk ktag1
                 ($kargs ('tag) (tag)
                   ($continue kcode src
                     ($primcall 'word-set!/immediate '(closure . 0)
                                (closure tag)))))
           (letk ktag0
                 ($kargs ('closure) (closure)
                   ($continue ktag1 src
                     ($primcall 'load-u64 (+ %tc7-program (ash nfree 16)) ()))))
           (build-term
             ($continue ktag0 src
               ($primcall 'allocate-words/immediate `(closure . ,(+ nfree 2))
                          ())))))
        (#(#t 0)
         (with-cps cps
           (build-term ($continue k src ($const #f)))))
        (#(#t 1)
         ;; A well-known closure of one free variable is replaced
         ;; at each use with the free variable itself, so we don't
         ;; need a binding at all; and yet, the continuation
         ;; expects one value, so give it something.  DCE should
         ;; clean up later.
         (with-cps cps
           (build-term ($continue k src ($const #f)))))
        (#(#t 2)
         ;; Well-known closure with two free variables; the closure is a
         ;; pair.
         (with-cps cps
           (build-term
             ($continue k src
               ($primcall 'allocate-words/immediate `(pair . 2) ())))))
        ;; Well-known callee with more than two free variables; the closure
        ;; is a vector.
        (#(#t nfree)
         (unless (> nfree 2)
           (error "unexpected well-known nullary, unary, or binary closure"))
         (with-cps cps
           (letv v w0)
           (letk k* ($kargs () () ($continue k src ($values (v)))))
           (letk ktag1
                 ($kargs ('w0) (w0)
                   ($continue k* src
                     ($primcall 'word-set!/immediate '(vector . 0) (v w0)))))
           (letk ktag0
                 ($kargs ('v) (v)
                   ($continue ktag1 src
                     ($primcall 'load-u64 (+ %tc7-vector (ash nfree 8)) ()))))
           (build-term
             ($continue ktag0 src
               ($primcall 'allocate-words/immediate `(vector . ,(1+ nfree))
                          ())))))))

    (define (init-closure cps k src var known? free)
      "Initialize the free variables @var{closure-free} in a closure
bound to @var{var}, and continue to @var{k}."
      (let ((count (intset-count free)))
        (cond
         ((and known? (<= count 1))
          ;; Well-known callee with zero or one free variables; no
          ;; initialization necessary.
          (with-cps cps
            (build-term ($continue k src ($values ())))))
         (else
          ;; Otherwise residualize a sequence of scm-set!.
          (let-values (((kind offset)
                        ;; What are we initializing?  A closure if the
                        ;; procedure is not well-known; a pair if it has
                        ;; only 2 free variables; otherwise, a vector.
                        (cond
                         ((not known?) (values 'closure 2))
                         ((= count 2)  (values 'pair 0))
                         (else         (values 'vector 1)))))
            (let lp ((cps cps) (prev #f) (idx 0))
              (match (intset-next free prev)
                (#f (with-cps cps
                      (build-term ($continue k src ($values ())))))
                (v (with-cps cps
                     (let$ body (lp (1+ v) (1+ idx)))
                     (letk k ($kargs () () ,body))
                     ($ (convert-arg v
                          (lambda (cps v)
                            (with-cps cps
                              (build-term
                                ($continue k src
                                  ($primcall 'scm-set!/immediate
                                             (cons kind (+ offset idx))
                                             (var v)))))))))))))))))

    (define (make-single-closure cps k src kfun)
      (let ((free (intmap-ref free-vars kfun)))
        (match (vector (well-known? kfun) (intset-count free))
          (#(#f 0)
           (with-cps cps
             (build-term ($continue k src ($const-fun kfun)))))
          (#(well-known? nfree)
           ;; A bit of a mess, but beta conversion should remove the
           ;; final $values if possible.
           (with-cps cps
             (letv closure)
             (letk k* ($kargs () () ($continue k src ($values (closure)))))
             (let$ init (init-closure k* src closure well-known? free))
             (letk knew ($kargs (#f) (closure) ,init))
             ($ (allocate-closure knew src kfun well-known? nfree)))))))

    ;; The callee is known, but not necessarily well-known.
    (define (convert-known-proc-call cps k src label closure args)
      (define (have-closure cps closure)
        (convert-args cps args
          (lambda (cps args)
            (with-cps cps
              (build-term
                ($continue k src ($callk label closure args)))))))
      (cond
       ((not closure)
        ;; No closure to begin with; done.
        (have-closure cps #f))
       ((eq? (intmap-ref free-vars label) empty-intset)
        ;; Known call, no free variables; no closure needed.  If the
        ;; callee is well-known, elide the closure argument entirely.
        ;; Otherwise pass #f.
        (if (intset-ref elidable label)
            (have-closure cps #f)
            (with-cps cps
              ($ (with-cps-constants ((false #f))
                   ($ (have-closure false)))))))
       ((and (well-known? (closure-label label shared bound->label))
             (trivial-intset (intmap-ref free-vars label)))
        ;; Well-known closures with one free variable are
        ;; replaced at their use sites by uses of the one free
        ;; variable.
        => (lambda (var)
             (convert-arg cps var have-closure)))
       (else
        ;; Otherwise just load the proc.
        (convert-arg cps closure have-closure))))

    (define (visit-term cps term)
      (match term
        (($ $continue k src (or ($ $const) ($ $prim)))
         (with-cps cps
           term))

        (($ $continue k src ($ $fun kfun))
         (with-cps cps
           ($ (make-single-closure k src kfun))))

        ;; Remove letrec.
        (($ $continue k src ($ $rec names vars (($ $fun kfuns) ...)))
         (match (vector names vars kfuns)
           (#(() () ())
            ;; Trivial empty case.
            (with-cps cps
              (build-term ($continue k src ($values ())))))
           (#((name) (var) (kfun))
            ;; Trivial single case.  We have already proven that K has
            ;; only LABEL as its predecessor, so we have been able
            ;; already to rewrite free references to the bound name with
            ;; the self name.
            (with-cps cps
              ($ (make-single-closure k src kfun))))
           (#(_ _ (kfun0 . _))
            ;; A non-trivial strongly-connected component.  Does it have
            ;; a shared closure?
            (match (intmap-ref shared kfun0 (lambda (_) #f))
              (#f
               ;; Nope.  Allocate closures for each function.
               (let lp ((cps (match (intmap-ref cps k)
                               ;; Steal declarations from the continuation.
                               (($ $kargs names vals body)
                                (intmap-replace cps k
                                                (build-cont
                                                  ($kargs () () ,body))))))
                        (in (map vector names vars kfuns))
                        (init (lambda (cps)
                                (with-cps cps
                                  (build-term
                                    ($continue k src ($values ())))))))
                 (match in
                   (() (init cps))
                   ((#(name var kfun) . in)
                    (let* ((known? (well-known? kfun))
                           (free (intmap-ref free-vars kfun))
                           (nfree (intset-count free)))
                      (define (next-init cps)
                        (with-cps cps
                          (let$ body (init))
                          (letk k ($kargs () () ,body))
                          ($ (init-closure k src var known? free))))
                      (with-cps cps
                        (let$ body (lp in next-init))
                        (letk k ($kargs (name) (var) ,body))
                        ($ (allocate-closure k src kfun known? nfree))))))))
              (shared
               ;; If shared is in the bound->var map, that means one of
               ;; the functions is not well-known.  Otherwise use kfun0
               ;; as the function label, but just so make-single-closure
               ;; can find the free vars, not for embedding in the
               ;; closure.
               (let* ((kfun (intmap-ref bound->label shared (lambda (_) kfun0)))
                      (cps (match (intmap-ref cps k)
                             ;; Make continuation declare only the shared
                             ;; closure.
                             (($ $kargs names vals body)
                              (intmap-replace cps k
                                              (build-cont
                                                ($kargs (#f) (shared) ,body)))))))
                 (with-cps cps
                   ($ (make-single-closure k src kfun)))))))))

        (($ $continue k src ($ $call proc args))
         (match (intmap-ref bound->label proc (lambda (_) #f))
           (#f
            (convert-arg cps proc
              (lambda (cps proc)
                (convert-args cps args
                  (lambda (cps args)
                    (with-cps cps
                      (build-term
                        ($continue k src ($call proc args)))))))))
           (label
            (convert-known-proc-call cps k src label proc args))))

        (($ $continue k src ($ $callk label proc args))
         (convert-known-proc-call cps k src label proc args))

        (($ $continue k src ($ $primcall name param args))
         (convert-args cps args
           (lambda (cps args)
             (with-cps cps
               (build-term
                 ($continue k src ($primcall name param args)))))))

        (($ $continue k src ($ $values args))
         (convert-args cps args
           (lambda (cps args)
             (with-cps cps
               (build-term
                 ($continue k src ($values args)))))))

        (($ $branch kf kt src op param args)
         (convert-args cps args
           (lambda (cps args)
             (with-cps cps
               (build-term
                 ($branch kf kt src op param args))))))

        (($ $switch kf kt* src arg)
         (convert-arg cps arg
           (lambda (cps arg)
             (with-cps cps
               (build-term
                 ($switch kf kt* src arg))))))

        (($ $prompt k kh src escape? tag)
         (convert-arg cps tag
           (lambda (cps tag)
             (with-cps cps
               (build-term
                 ($prompt k kh src escape? tag))))))

        (($ $throw src op param args)
         (convert-args cps args
           (lambda (cps args)
             (with-cps cps
               (build-term
                 ($throw src op param args))))))))

    (intset-fold (lambda (label cps)
                   (match (intmap-ref cps label (lambda (_) #f))
                     (($ $kargs names vars term)
                      (with-cps cps
                        (let$ term (visit-term term))
                        (setk label ($kargs names vars ,term))))
                     (($ $kfun src meta self ktail kclause)
                      (if (intset-ref elidable label)
                          (with-cps cps
                            (setk label ($kfun src meta #f ktail kclause)))
                          cps))
                     (_ cps)))
                 body
                 cps)))

(define (convert-closures cps)
  "Convert free reference in @var{cps} to primcalls to @code{free-ref},
and allocate and initialize flat closures."
  (let* ((kfun 0) ;; Ass-u-me.
         ;; label -> body-label...
         (functions (compute-reachable-functions cps kfun))
         (cps (filter-reachable cps functions))
         ;; label -> bound-var...
         (label->bound (compute-function-names cps functions))
         ;; bound-var -> label
         (bound->label (invert-partition label->bound))
         ;; label...
         (well-known (compute-well-known-functions cps bound->label))
         ;; label -> closure-var
         (shared (compute-shared-closures cps well-known))
         (cps (rewrite-shared-closure-calls cps functions label->bound shared
                                            kfun))
         ;; label -> free-var...
         (free-vars (compute-free-vars cps kfun shared))
         (free-vars (prune-free-vars free-vars bound->label well-known shared))
         ;; label...
         (elidable (compute-elidable-closures cps well-known shared free-vars)))
    (let ((free-in-program (intmap-ref free-vars kfun)))
      (unless (eq? empty-intset free-in-program)
        (error "Expected no free vars in program" free-in-program)))
    (with-fresh-name-state cps
      (persistent-intmap
       (intmap-fold
        (lambda (label body cps)
          (convert-one cps label body free-vars bound->label well-known shared
                       elidable))
        functions
        cps)))))

;;; Local Variables:
;;; eval: (put 'convert-arg 'scheme-indent-function 2)
;;; eval: (put 'convert-args 'scheme-indent-function 2)
;;; End:
