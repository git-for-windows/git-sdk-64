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
;;; Loop peeling "peels off" one iteration of a loop.  When followed by
;;; common subexpression elimination, it has the effect of moving terms
;;; to the first peeled iteration, leaving the loop body with fewer
;;; terms.
;;;
;;; Loop peeling is complementary to loop-invariant code motion (LICM).
;;; LICM will hoist invariant terms that have no side effects, like
;;; $const, even if they are in branches that are not always taken.
;;; However LICM won't hoist expressions that might have side effects if
;;; it can't prove that they are reachable on every iteration.  Peeling
;;; on the other hand arranges for the body to be dominated by one loop
;;; iteration, so any effect that is reachable on one full iteration can
;;; be hoisted and eliminated, which is a big boon when we consider
;;; &type-check effects.  For example:
;;;
;;;    x = cached-toplevel-box map
;;;    y = box-ref x
;;;    z = cached-toplevel-box foo
;;;    w = box-ref z
;;;    ...
;;;
;;; In this example, LICM could hoist X, possibly Y as well if it can
;;; prove that the body doesn't write to variables, but it won't hoist
;;; Z.  In contrast, peeling + CSE will allow Z to be hoisted.
;;;
;;; Peeling does cause code growth.  If this becomes a problem we will
;;; need to apply heuristics to limit its applicability.
;;;
;;; Implementation-wise, things are complicated by values flowing out of
;;; the loop.  We actually perform this transformation only on loops
;;; that have a single exit continuation, so that we define values
;;; flowing out in one place.  We rename the loop variables in two
;;; places internally: one for the peeled iteration, and another for
;;; the body.  The loop variables' original names are then bound in a
;;; join continuation for use by successor code.
;;;
;;; Code:

(define-module (language cps peel-loops)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:export (peel-loops))

(define (intset-map f set)
  (persistent-intmap
   (intset-fold (lambda (i out) (intmap-add! out i (f i))) set empty-intmap)))

(define (loop-successors scc succs)
  (intset-subtract (intset-fold (lambda (label exits)
                                  (intset-union exits (intmap-ref succs label)))
                                scc empty-intset)
                   scc))

(define (find-exits scc succs)
  (intset-fold (lambda (label exits)
                 (if (eq? empty-intset
                          (intset-subtract (intmap-ref succs label) scc))
                     exits
                     (intset-add exits label)))
               scc
               empty-intset))

(define (find-entry scc preds)
  (trivial-intset (find-exits scc preds)))

(define (list->intset vars)
  (persistent-intset
   (fold1 (lambda (var set) (intset-add! set var)) vars empty-intset)))

(define (compute-live-variables cps entry body succs)
  (let* ((succs (intset-map (lambda (label)
                              (intset-intersect (intmap-ref succs label) body))
                            body))
         (init (intset-map (lambda (label) #f) body))
         (kill (intset-map (lambda (label) #f) body))
         (gen  (intset-map (lambda (label)
                             (match (intmap-ref cps label)
                               (($ $kargs names vars) (list->intset vars))
                               (_ empty-intset)))
                           body))
         (in (intmap-replace init entry (intmap-ref gen entry)))
         (out init))
    (define (subtract in kill) (or in empty-intset))
    (define (add in gen) (if in (intset-union in gen) gen))
    (define (meet in out) (if in (intset-intersect in out) out))
    (call-with-values (lambda ()
                        (solve-flow-equations succs in out kill gen
                                              subtract add meet
                                              (intset entry)))
      (lambda (in out)
        out))))

(define (compute-out-vars cps entry body succs exit)
  (let ((live (compute-live-variables cps entry body succs)))
    (intset-fold-right
     cons
     (intmap-fold (lambda (label succs live-out)
                    (if (intset-ref succs exit)
                        (if live-out
                            (intset-intersect live-out (intmap-ref live label))
                            (intmap-ref live label))
                        live-out))
                  succs #f)
     '())))

(define (rename-cont cont fresh-labels fresh-vars)
  (define (rename-label label)
    (intmap-ref fresh-labels label (lambda (label) label)))
  (define (rename-var var)
    (intmap-ref fresh-vars var (lambda (var) var)))
  (define (rename-exp exp)
    (rewrite-exp exp
      ((or ($ $const) ($ $prim) ($ $closure) ($ $rec ())) ,exp)
      (($ $values args)
       ($values ,(map rename-var args)))
      (($ $call proc args)
       ($call (rename-var proc) ,(map rename-var args)))
      (($ $callk k proc args)
       ($callk k (rename-var proc) ,(map rename-var args)))
      (($ $branch kt ($ $values (arg)))
       ($branch (rename-label kt) ($values ((rename-var arg)))))
      (($ $branch kt ($ $primcall name args))
       ($branch (rename-label kt) ($primcall name ,(map rename-var args))))
      (($ $primcall name args)
       ($primcall name ,(map rename-var args)))
      (($ $prompt escape? tag handler)
       ($prompt escape? (rename-var tag) (rename-label handler)))))
  (rewrite-cont cont
    (($ $kargs names vars ($ $continue k src exp))
     ($kargs names (map rename-var vars)
       ($continue (rename-label k) src ,(rename-exp exp))))
    (($ $kreceive ($ $arity req () rest) kargs)
     ($kreceive req rest (rename-label kargs)))))

(define (compute-var-names conts)
  (persistent-intmap
   (intmap-fold (lambda (label cont out)
                  (match cont
                    (($ $kargs names vars)
                     (fold (lambda (name var out)
                             (intmap-add! out var name))
                           out names vars))
                    (_ out)))
                conts empty-intmap)))

(define (peel-loop cps entry body-labels succs preds)
  (let* ((body-conts (intset-map (lambda (label) (intmap-ref cps label))
                                 body-labels))
         (var-names (compute-var-names body-conts))
         ;; All loop exits branch to this label.
         (exit (trivial-intset (loop-successors body-labels succs)))
         ;; The variables that flow out of the loop, as a list.
         (out-vars (compute-out-vars cps entry body-labels succs exit))
         (out-names (map (lambda (var) (intmap-ref var-names var)) out-vars))
         (join-label (fresh-label))
         (join-cont (build-cont
                      ($kargs out-names out-vars
                        ($continue exit #f ($values ())))))
         (trampoline-cont
          ;; A $values predecessor for the join, passing the out-vars
          ;; using their original names.  These will get renamed in
          ;; both the peeled iteration and the body.
          (build-cont
            ($kargs () ()
              ($continue join-label #f ($values out-vars)))))
         (fresh-body-labels
          ;; Fresh labels for the body.
          (intset-map (lambda (old) (fresh-label)) body-labels))
         (fresh-body-vars
          ;; Fresh vars for the body.
          (intmap-map (lambda (var name) (fresh-var)) var-names))
         (fresh-body-entry
          ;; The name of the entry, but in the body.
          (intmap-ref fresh-body-labels entry))
         (fresh-peeled-vars
          ;; Fresh names for variables that flow out of the peeled iteration.
          (fold1 (lambda (var out) (intmap-add out var (fresh-var)))
                 out-vars empty-intmap))
         (peeled-trampoline-label
          ;; Label for trampoline to pass values out of the peeled
          ;; iteration.
          (fresh-label))
         (peeled-trampoline-cont
          ;; Trampoline for the peeled iteration, ready to adjoin to
          ;; CPS.
          (rename-cont trampoline-cont empty-intmap fresh-peeled-vars))
         (peeled-labels
          ;; Exit goes to trampoline, back edges to body.
          (intmap-add (intmap-add empty-intmap exit peeled-trampoline-label)
                      entry fresh-body-entry))
         (peeled-iteration
          ;; The peeled iteration.
          (intmap-map (lambda (label cont)
                        (rename-cont cont peeled-labels fresh-peeled-vars))
                      body-conts))
         (body-trampoline-label
          ;; Label for trampoline to pass values out of the body.
          (fresh-label))
         (body-trampoline-cont
          ;; Trampoline for the body, ready to adjoin to CPS.
          (rename-cont trampoline-cont empty-intmap fresh-body-vars))
         (fresh-body
          ;; The body, renamed.
          (let ((label-map (intmap-add fresh-body-labels
                                       exit body-trampoline-label)))
            (persistent-intmap
             (intmap-fold
              (lambda (label new-label out)
                (intmap-add! out new-label
                             (rename-cont (intmap-ref body-conts label)
                                          label-map fresh-body-vars)))
              fresh-body-labels empty-intmap)))))

    (let* ((cps (intmap-add! cps join-label join-cont))
           (cps (intmap-add! cps peeled-trampoline-label
                             peeled-trampoline-cont))
           (cps (intmap-add! cps body-trampoline-label
                             body-trampoline-cont))
           (cps (intmap-fold (lambda (label cont cps)
                               (intmap-replace! cps label cont))
                             peeled-iteration cps))
           (cps (intmap-fold (lambda (label cont cps)
                               (intmap-add! cps label cont))
                             fresh-body cps)))
      cps)))

(define (peel-loops-in-function kfun body cps)
  (let* ((succs (compute-successors cps kfun))
         (preds (invert-graph succs)))
    ;; We can peel if there is one successor to the loop, and if the
    ;; loop has no nested functions.  (Peeling a nested function would
    ;; cause exponential code growth.)
    (define (can-peel? body)
      (and (trivial-intset (loop-successors body succs))
           (intset-fold (lambda (label peel?)
                          (match (intmap-ref cps label)
                            (($ $kargs _ _ ($ $continue _ _ exp))
                             (match exp
                               (($ $fun) #f)
                               (($ $rec (_ . _)) #f)
                               (_ peel?)))
                            (_ peel?)))
                        body #t)))
                         
    (intmap-fold
     (lambda (id scc cps)
       (cond
        ((trivial-intset scc) cps)
        ((find-entry scc preds)
         => (lambda (entry)
              (if (can-peel? scc)
                  (peel-loop cps entry scc succs preds)
                  cps)))
        (else cps)))
     (compute-strongly-connected-components succs kfun)
     cps)))

(define (peel-loops cps)
  (persistent-intmap
   (with-fresh-name-state cps
     (intmap-fold peel-loops-in-function
                  (compute-reachable-functions cps)
                  cps))))
