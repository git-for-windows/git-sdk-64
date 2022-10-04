;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2015, 2017-2021 Free Software Foundation, Inc.

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
;;; Helper facilities for working with graphs over intsets and intmaps.
;;;
;;; Code:

(define-module (language cps graphs)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:export (;; Various utilities.
            fold1 fold2
            trivial-intset
            intmap-map
            intmap-keys
            invert-bijection invert-partition
            rename-keys rename-intset rename-graph
            intset->intmap
            intmap-select
            worklist-fold
            fixpoint

            ;; Flow analysis.
            invert-graph
            compute-reverse-post-order
            compute-strongly-connected-components
            compute-sorted-strongly-connected-components
            compute-reverse-control-flow-order
            solve-flow-equations
            compute-live-variables))

(define-inlinable (fold1 f l s0)
  (let lp ((l l) (s0 s0))
    (match l
      (() s0)
      ((elt . l) (lp l (f elt s0))))))

(define-inlinable (fold2 f l s0 s1)
  (let lp ((l l) (s0 s0) (s1 s1))
    (match l
      (() (values s0 s1))
      ((elt . l)
       (call-with-values (lambda () (f elt s0 s1))
         (lambda (s0 s1)
           (lp l s0 s1)))))))

(define (trivial-intset set)
  "Returns the sole member of @var{set}, if @var{set} has exactly one
member, or @code{#f} otherwise."
  (let ((first (intset-next set)))
    (and first
         (not (intset-next set (1+ first)))
         first)))

(define (intmap-map proc map)
  (persistent-intmap
   (intmap-fold (lambda (k v out) (intmap-add! out k (proc k v)))
                map
                empty-intmap)))

(define (intmap-keys map)
  "Return an intset of the keys in @var{map}."
  (persistent-intset
   (intmap-fold (lambda (k v keys) (intset-add! keys k)) map empty-intset)))

(define (invert-bijection map)
  "Assuming the values of @var{map} are integers and are unique, compute
a map in which each value maps to its key.  If the values are not
unique, an error will be signalled."
  (persistent-intmap
   (intmap-fold (lambda (k v out) (intmap-add! out v k)) map empty-intmap)))

(define (invert-partition map)
  "Assuming the values of @var{map} are disjoint intsets, compute a map
in which each member of each set maps to its key.  If the values are not
disjoint, an error will be signalled."
  (intmap-fold (lambda (k v* out)
                 (intset-fold (lambda (v out) (intmap-add out v k)) v* out))
               map empty-intmap))

(define (intset->intmap f set)
  (persistent-intmap
   (intset-fold (lambda (label preds)
                  (intmap-add! preds label (f label)))
                set empty-intmap)))

(define (intmap-select map set)
  (persistent-intmap
   (intset-fold (lambda (label out)
                  (intmap-add! out label (intmap-ref map label)))
                set empty-intmap)))

(define worklist-fold
  (case-lambda
    ((f in out)
     (let lp ((in in) (out out))
       (if (eq? in empty-intset)
           out
           (call-with-values (lambda () (f in out)) lp))))
    ((f in out0 out1)
     (let lp ((in in) (out0 out0) (out1 out1))
       (if (eq? in empty-intset)
           (values out0 out1)
           (call-with-values (lambda () (f in out0 out1)) lp))))))

(define fixpoint
  (case-lambda
    ((f x)
     (let lp ((x x))
       (let ((x* (f x)))
         (if (eq? x x*) x* (lp x*)))))
    ((f x0 x1)
     (let lp ((x0 x0) (x1 x1))
       (call-with-values (lambda () (f x0 x1))
         (lambda (x0* x1*)
           (if (and (eq? x0 x0*) (eq? x1 x1*))
               (values x0* x1*)
               (lp x0* x1*))))))))

(define (compute-reverse-post-order succs start)
  "Compute a reverse post-order numbering for a depth-first walk over
nodes reachable from the start node."
  (let visit ((label start) (order '()) (visited empty-intset))
    (call-with-values
        (lambda ()
          (intset-fold (lambda (succ order visited)
                         (if (intset-ref visited succ)
                             (values order visited)
                             (visit succ order visited)))
                       (intmap-ref succs label)
                       order
                       (intset-add! visited label)))
      (lambda (order visited)
        ;; After visiting successors, add label to the reverse post-order.
        (values (cons label order) visited)))))

(define (invert-graph succs)
  "Given a graph PRED->SUCC..., where PRED is a label and SUCC... is an
intset of successors, return a graph SUCC->PRED...."
  (intmap-fold (lambda (pred succs preds)
                 (intset-fold
                  (lambda (succ preds)
                    (intmap-add preds succ pred intset-add))
                  succs
                  preds))
               succs
               (intmap-map (lambda (label _) empty-intset) succs)))

(define (rename-keys map old->new)
  "Return a fresh intmap containing F(K) -> V for K and V in MAP, where
F is looking up K in the intmap OLD->NEW."
  (persistent-intmap
   (intmap-fold (lambda (k v out)
                  (intmap-add! out (intmap-ref old->new k) v))
                map
                empty-intmap)))

(define (rename-intset set old->new)
  "Return a fresh intset of F(K) for K in SET, where F is looking up K
in the intmap OLD->NEW."
  (intset-fold (lambda (old set) (intset-add set (intmap-ref old->new old)))
               set empty-intset))

(define (rename-graph graph old->new)
  "Return a fresh intmap containing F(K) -> intset(F(V)...) for K and
intset(V...) in GRAPH, where F is looking up K in the intmap OLD->NEW."
  (persistent-intmap
   (intmap-fold (lambda (pred succs out)
                  (intmap-add! out
                               (intmap-ref old->new pred)
                               (rename-intset succs old->new)))
                graph
                empty-intmap)))

(define (compute-strongly-connected-components succs start)
  "Given a LABEL->SUCCESSOR... graph, compute a SCC->LABEL... map
partitioning the labels into strongly connected components (SCCs)."
  (let ((preds (invert-graph succs)))
    (define (visit-scc scc sccs-by-label)
      (let visit ((label scc) (sccs-by-label sccs-by-label))
        (if (intmap-ref sccs-by-label label (lambda (_) #f))
            sccs-by-label
            (intset-fold visit
                         (intmap-ref preds label)
                         (intmap-add sccs-by-label label scc)))))
    (intmap-fold
     (lambda (label scc sccs)
       (let ((labels (intset-add empty-intset label)))
         (intmap-add sccs scc labels intset-union)))
     (fold visit-scc empty-intmap (compute-reverse-post-order succs start))
     empty-intmap)))

(define (compute-sorted-strongly-connected-components edges)
  "Given a LABEL->SUCCESSOR... graph, return a list of strongly
connected components in sorted order."
  (define nodes
    (intmap-keys edges))
  ;; Add a "start" node that links to all nodes in the graph, and then
  ;; remove it from the result.
  (define start
    (if (eq? nodes empty-intset)
        0
        (1+ (intset-prev nodes))))
  (define components
    (intmap-remove
     (compute-strongly-connected-components (intmap-add edges start nodes)
                                            start)
     start))
  (define node-components
    (intmap-fold (lambda (id nodes out)
                   (intset-fold (lambda (node out) (intmap-add out node id))
                                nodes out))
                 components
                 empty-intmap))
  (define (node-component node)
    (intmap-ref node-components node))
  (define (component-successors id nodes)
    (intset-remove
     (intset-fold (lambda (node out)
                    (intset-fold
                     (lambda (successor out)
                       (intset-add out (node-component successor)))
                     (intmap-ref edges node)
                     out))
                  nodes
                  empty-intset)
     id))
  (define component-edges
    (intmap-map component-successors components))
  (define preds
    (invert-graph component-edges))
  (define roots
    (intmap-fold (lambda (id succs out)
                   (if (eq? empty-intset succs)
                       (intset-add out id)
                       out))
                 component-edges
                 empty-intset))
  ;; As above, add a "start" node that links to the roots, and remove it
  ;; from the result.
  (match (compute-reverse-post-order (intmap-add preds start roots) start)
    (((? (lambda (id) (eqv? id start))) . ids)
     (map (lambda (id) (intmap-ref components id)) ids))))

(define (compute-reverse-control-flow-order preds)
  "Return a LABEL->ORDER bijection where ORDER is a contiguous set of
integers starting from 0 and incrementing in sort order.  There is a
precondition that labels in PREDS are already renumbered in reverse post
order."
  (define (has-back-edge? preds)
    (let/ec return
      (intmap-fold (lambda (label labels)
                     (intset-fold (lambda (pred)
                                    (if (<= label pred)
                                        (return #t)
                                        (values)))
                                  labels)
                     (values))
                   preds)
      #f))
  (if (has-back-edge? preds)
      ;; This is more involved than forward control flow because not all
      ;; live labels are reachable from the tail.
      (persistent-intmap
       (fold2 (lambda (component order n)
                (intset-fold (lambda (label order n)
                               (values (intmap-add! order label n)
                                       (1+ n)))
                             component order n))
              (reverse (compute-sorted-strongly-connected-components preds))
              empty-intmap 0))
      ;; Just reverse forward control flow.
      (let ((max (intmap-prev preds)))
        (intmap-map (lambda (label labels) (- max label)) preds))))

(define (intset-pop set)
  (match (intset-next set)
    (#f (values set #f))
    (i (values (intset-remove set i) i))))

(define* (solve-flow-equations succs in out kill gen subtract add meet
                               #:optional (worklist (intmap-keys succs)))
  "Find a fixed point for flow equations for SUCCS, where INIT is the
initial state at each node in SUCCS.  KILL and GEN are intmaps
indicating the state that is killed or defined at every node, and
SUBTRACT, ADD, and MEET operates on that state."
  (define (visit label in out)
    (let* ((in-1 (intmap-ref in label))
           (kill-1 (intmap-ref kill label))
           (gen-1 (intmap-ref gen label))
           (out-1 (intmap-ref out label))
           (out-1* (add (subtract in-1 kill-1) gen-1)))
      (if (eq? out-1 out-1*)
          (values empty-intset in out)
          (let ((out (intmap-replace! out label out-1*)))
            (call-with-values
                (lambda ()
                  (intset-fold (lambda (succ in changed)
                                 (let* ((in-1 (intmap-ref in succ))
                                        (in-1* (meet in-1 out-1*)))
                                   (if (eq? in-1 in-1*)
                                       (values in changed)
                                       (values (intmap-replace! in succ in-1*)
                                               (intset-add changed succ)))))
                               (intmap-ref succs label) in empty-intset))
              (lambda (in changed)
                (values changed in out)))))))

  (let run ((worklist worklist) (in in) (out out))
    (call-with-values (lambda () (intset-pop worklist))
      (lambda (worklist popped)
        (if popped
            (call-with-values (lambda () (visit popped in out))
              (lambda (changed in out)
                (run (intset-union worklist changed) in out)))
            (values (persistent-intmap in)
                    (persistent-intmap out)))))))

(define (compute-live-variables preds defs uses)
  "Compute and return two values mapping LABEL->VAR..., where VAR... are
the definitions that are live before and after LABEL, as intsets."
  (let* ((old->new (compute-reverse-control-flow-order preds))
         (init (persistent-intmap (intmap-fold
                                   (lambda (old new init)
                                     (intmap-add! init new empty-intset))
                                   old->new empty-intmap))))
    (call-with-values
        (lambda ()
          (solve-flow-equations (rename-graph preds old->new)
                                init init
                                (rename-keys defs old->new)
                                (rename-keys uses old->new)
                                intset-subtract intset-union intset-union))
      (lambda (in out)
        ;; As a reverse control-flow problem, the values flowing into a
        ;; node are actually the live values after the node executes.
        ;; Funny, innit?  So we return them in the reverse order.
        (let ((new->old (invert-bijection old->new)))
          (values (rename-keys out new->old)
                  (rename-keys in new->old)))))))
