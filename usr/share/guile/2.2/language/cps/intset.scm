;;; Functional name maps
;;; Copyright (C) 2014, 2015, 2017 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;; 
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; A persistent, functional data structure representing a set of
;;; integers as a tree whose branches are vectors and whose leaves are
;;; fixnums.  Intsets are careful to preserve sub-structure, in the
;;; sense of eq?, whereever possible.
;;;
;;; Code:

(define-module (language cps intset)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 threads) #:select (current-thread))
  #:export (empty-intset
            intset?
            transient-intset?
            persistent-intset
            transient-intset
            intset
            intset-add
            intset-add!
            intset-remove
            intset-ref
            intset-next
            intset-prev
            intset-fold
            intset-fold-right
            intset-union
            intset-intersect
            intset-subtract
            bitvector->intset))

(define-syntax-rule (define-inline name val)
  (define-syntax name (identifier-syntax val)))

(eval-when (expand)
  (use-modules (system base target))
  (define-syntax compile-time-cond
    (lambda (x)
      (syntax-case x (else)
        ((_ (test body ...) rest ...)
         (if (primitive-eval (syntax->datum #'test))
             #'(begin body ...)
             #'(begin (compile-time-cond rest ...))))
        ((_ (else body ...))
         #'(begin body ...))
        ((_)
         (error "no compile-time-cond expression matched"))))))

(compile-time-cond
 ((eqv? (target-word-size) 4)
  (define-inline *leaf-bits* 4))
 ((eqv? (target-word-size) 8)
  (define-inline *leaf-bits* 5)))

;; FIXME: This should make an actual atomic reference.
(define-inlinable (make-atomic-reference value)
  (list value))
(define-inlinable (get-atomic-reference reference)
  (car reference))
(define-inlinable (set-atomic-reference! reference value)
  (set-car! reference value))

(define-inline *leaf-size* (ash 1 *leaf-bits*))
(define-inline *leaf-mask* (1- *leaf-size*))
(define-inline *branch-bits* 3)
(define-inline *branch-size* (ash 1 *branch-bits*))
(define-inline *branch-size-with-edit* (1+ *branch-size*))
(define-inline *edit-index* *branch-size*)
(define-inline *branch-mask* (1- *branch-size*))

(define-record-type <intset>
  (make-intset min shift root)
  intset?
  (min intset-min)
  (shift intset-shift)
  (root intset-root))

(define-record-type <transient-intset>
  (make-transient-intset min shift root edit)
  transient-intset?
  (min transient-intset-min set-transient-intset-min!)
  (shift transient-intset-shift set-transient-intset-shift!)
  (root transient-intset-root set-transient-intset-root!)
  (edit transient-intset-edit set-transient-intset-edit!))

(define-inlinable (clone-leaf-and-set leaf i val)
  (if val
      (if leaf
          (logior leaf (ash 1 i))
          (ash 1 i))
      (if leaf
          (logand leaf (lognot (ash 1 i)))
          #f)))
(define (leaf-empty? leaf)
  (zero? leaf))

(define-inlinable (new-branch edit)
  (let ((vec (make-vector *branch-size-with-edit* #f)))
    (when edit (vector-set! vec *edit-index* edit))
    vec))
(define-inlinable (clone-branch-and-set branch i elt)
  (let ((new (new-branch #f)))
    (when branch
      (let lp ((n 0))
        (when (< n *branch-size*)
          (vector-set! new n (vector-ref branch n))
          (lp (1+ n)))))
    (vector-set! new i elt)
    new))
(define-inlinable (assert-readable! root-edit)
  (unless (eq? (get-atomic-reference root-edit) (current-thread))
    (error "Transient intset owned by another thread" root-edit)))
(define-inlinable (writable-branch branch root-edit)
  (let ((edit (vector-ref branch *edit-index*)))
    (if (eq? root-edit edit)
        branch
        (clone-branch-and-set branch *edit-index* root-edit))))
(define (branch-empty? branch)
  (let lp ((i 0))
    (or (= i *branch-size*)
        (and (not (vector-ref branch i))
             (lp (1+ i))))))

(define-inlinable (round-down min shift)
  (logand min (lognot (1- (ash 1 shift)))))

(define empty-intset (make-intset 0 *leaf-bits* #f))

(define (add-level min shift root)
  (let* ((shift* (+ shift *branch-bits*))
         (min* (round-down min shift*))
         (idx (logand (ash (- min min*) (- shift)) *branch-mask*)))
    (make-intset min* shift* (clone-branch-and-set #f idx root))))

(define (make-intset/prune min shift root)
  (cond
   ((not root)
    empty-intset)
   ((= shift *leaf-bits*)
    (make-intset min shift root))
   (else
    (let lp ((i 0) (elt #f))
      (cond
       ((< i *branch-size*)
        (if (vector-ref root i)
            (if elt
                (make-intset min shift root)
                (lp (1+ i) i))
            (lp (1+ i) elt)))
       (elt
        (let ((shift (- shift *branch-bits*)))
          (make-intset/prune (+ min (ash elt shift))
                             shift
                             (vector-ref root elt))))
       ;; Shouldn't be reached...
       (else empty-intset))))))

(define* (transient-intset #:optional (source empty-intset))
  (match source
    (($ <transient-intset> min shift root edit)
     (assert-readable! edit)
     source)
    (($ <intset> min shift root)
     (let ((edit (make-atomic-reference (current-thread))))
       (make-transient-intset min shift root edit)))))

(define* (persistent-intset #:optional (source empty-intset))
  (match source
    (($ <transient-intset> min shift root edit)
     (assert-readable! edit)
     ;; Make a fresh reference, causing any further operations on this
     ;; transient to clone its root afresh.
     (set-transient-intset-edit! source
                                 (make-atomic-reference (current-thread)))
     ;; Clear the reference to the current thread, causing our edited
     ;; data structures to be persistent again.
     (set-atomic-reference! edit #f)
     (if min
         (make-intset min shift root)
         empty-intset))
    (($ <intset>)
     source)))

(define (intset-add! bs i)
  (define (adjoin-leaf i root)
    (clone-leaf-and-set root (logand i *leaf-mask*) #t))
  (define (ensure-branch! root idx)
    (let ((edit (vector-ref root *edit-index*)))
      (match (vector-ref root idx)
        (#f (let ((v (new-branch edit)))
              (vector-set! root idx v)
              v))
        (v (let ((v* (writable-branch v edit)))
             (unless (eq? v v*)
               (vector-set! root idx v*))
             v*)))))
  (define (adjoin-branch! i shift root)
    (let* ((shift (- shift *branch-bits*))
           (idx (logand (ash i (- shift)) *branch-mask*)))
      (cond
       ((= shift *leaf-bits*)
        (vector-set! root idx (adjoin-leaf i (vector-ref root idx))))
       (else
        (adjoin-branch! i shift (ensure-branch! root idx))))))
  (match bs
    (($ <transient-intset> min shift root edit)
     (assert-readable! edit)
     (cond
      ((< i 0)
       ;; The power-of-two spanning trick doesn't work across 0.
       (error "Intsets can only hold non-negative integers." i))
      ((not root)
       ;; Add first element.
       (let ((min (round-down i shift)))
         (set-transient-intset-min! bs min)
         (set-transient-intset-shift! bs *leaf-bits*)
         (set-transient-intset-root! bs (adjoin-leaf (- i min) root))))
      ((and (<= min i) (< i (+ min (ash 1 shift))))
       ;; Add element to set; level will not change.
       (if (= shift *leaf-bits*)
           (set-transient-intset-root! bs (adjoin-leaf (- i min) root))
           (let ((root* (writable-branch root edit)))
             (unless (eq? root root*)
               (set-transient-intset-root! bs root*))
             (adjoin-branch! (- i min) shift root*))))
      (else
       (let lp ((min min)
                (shift shift)
                (root (if (eqv? shift *leaf-bits*)
                          root
                          (writable-branch root edit))))
         (let* ((shift* (+ shift *branch-bits*))
                (min* (round-down min shift*))
                (idx (logand (ash (- min min*) (- shift)) *branch-mask*))
                (root* (new-branch edit)))
           (vector-set! root* idx root)
           (cond
            ((and (<= min* i) (< i (+ min* (ash 1 shift*))))
             (set-transient-intset-min! bs min*)
             (set-transient-intset-shift! bs shift*)
             (set-transient-intset-root! bs root*)
             (adjoin-branch! (- i min*) shift* root*))
            (else
             (lp min* shift* root*)))))))
     bs)
    (($ <intset>)
     (intset-add! (transient-intset bs) i))))

(define (intset-add bs i)
  (define (adjoin i shift root)
    (cond
     ((= shift *leaf-bits*)
      (let ((idx (logand i *leaf-mask*)))
        (if (and root (logbit? idx root))
            root
            (clone-leaf-and-set root idx #t))))
     (else
      (let* ((shift (- shift *branch-bits*))
             (idx (logand (ash i (- shift)) *branch-mask*))
             (node (and root (vector-ref root idx)))
             (new-node (adjoin i shift node)))
        (if (eq? node new-node)
            root
            (clone-branch-and-set root idx new-node))))))
  (match bs
    (($ <intset> min shift root)
     (cond
      ((< i 0)
       ;; The power-of-two spanning trick doesn't work across 0.
       (error "Intsets can only hold non-negative integers." i))
      ((not root)
       ;; Add first element.
       (let ((min (round-down i shift)))
         (make-intset min *leaf-bits*
                      (adjoin (- i min) *leaf-bits* root))))
      ((and (<= min i) (< i (+ min (ash 1 shift))))
       ;; Add element to set; level will not change.
       (let ((old-root root)
             (root (adjoin (- i min) shift root)))
         (if (eq? root old-root)
             bs
             (make-intset min shift root))))
      ((< i min)
       ;; Rebuild the tree by unioning two intsets.
       (intset-union (intset-add empty-intset i) bs))
      (else
       ;; Add a new level and try again.
       (intset-add (add-level min shift root) i))))))

(define-syntax intset
  (syntax-rules ()
    ((intset) empty-intset)
    ((intset x x* ...) (intset-add (intset x* ...) x))))

(define (intset-remove bs i)
  (define (remove i shift root)
    (cond
     ((= shift *leaf-bits*)
      (let ((idx (logand i *leaf-mask*)))
        (if (logbit? idx root)
            (let ((root (clone-leaf-and-set root idx #f)))
              (and (not (leaf-empty? root)) root))
            root)))
     (else
      (let* ((shift (- shift *branch-bits*))
             (idx (logand (ash i (- shift)) *branch-mask*)))
        (cond
         ((vector-ref root idx)
          => (lambda (node)
               (let ((new-node (remove i shift node)))
                 (if (eq? node new-node)
                     root
                     (let ((root (clone-branch-and-set root idx new-node)))
                       (and (or new-node (not (branch-empty? root)))
                            root))))))
         (else root))))))
  (match bs
    (($ <intset> min shift root)
     (cond
      ((not root) bs)
      ((and (<= min i) (< i (+ min (ash 1 shift))))
       (let ((old-root root)
             (root (remove (- i min) shift root)))
         (if (eq? root old-root)
             bs
             (make-intset/prune min shift root))))
      (else bs)))))

(define (intset-ref bs i)
  (define (ref min shift root)
    (and (<= min i) (< i (+ min (ash 1 shift)))
         (let ((i (- i min)))
           (let lp ((node root) (shift shift))
             (and node
                  (if (= shift *leaf-bits*)
                      (logbit? (logand i *leaf-mask*) node)
                      (let* ((shift (- shift *branch-bits*))
                             (idx (logand (ash i (- shift)) *branch-mask*)))
                        (lp (vector-ref node idx) shift))))))))
  (match bs
    (($ <intset> min shift root)
     (ref min shift root))
    (($ <transient-intset> min shift root edit)
     (assert-readable! edit)
     (ref min shift root))))

(define* (intset-next bs #:optional i)
  (define (visit-leaf node i)
    (let lp ((idx (logand i *leaf-mask*)))
      (if (logbit? idx node)
          (logior (logand i (lognot *leaf-mask*)) idx)
          (let ((idx (1+ idx)))
            (and (< idx *leaf-size*)
                 (lp idx))))))
  (define (visit-branch node shift i)
    (let lp ((i i) (idx (logand (ash i (- shift)) *branch-mask*)))
      (and (< idx *branch-size*)
           (or (let ((node (vector-ref node idx)))
                 (and node (visit-node node shift i)))
               (let ((inc (ash 1 shift)))
                 (lp (+ (round-down i shift) inc) (1+ idx)))))))
  (define (visit-node node shift i)
    (if (= shift *leaf-bits*)
        (visit-leaf node i)
        (visit-branch node (- shift *branch-bits*) i)))
  (define (next min shift root)
    (let ((i (if (and i (< min i))
                 (- i min)
                 0)))
      (and root (< i (ash 1 shift))
           (let ((i (visit-node root shift i)))
             (and i (+ min i))))))
  (match bs
    (($ <intset> min shift root)
     (next min shift root))
    (($ <transient-intset> min shift root edit)
     (assert-readable! edit)
     (next min shift root))))

(define* (intset-prev bs #:optional i)
  (define (visit-leaf node i)
    (let lp ((idx (logand i *leaf-mask*)))
      (if (logbit? idx node)
          (logior (logand i (lognot *leaf-mask*)) idx)
          (let ((idx (1- idx)))
            (and (<= 0 idx) (lp idx))))))
  (define (visit-branch node shift i)
    (let lp ((i i) (idx (logand (ash i (- shift)) *branch-mask*)))
      (and (<= 0 idx)
           (or (let ((node (vector-ref node idx)))
                 (and node (visit-node node shift i)))
               (lp (1- (round-down i shift)) (1- idx))))))
  (define (visit-node node shift i)
    (if (= shift *leaf-bits*)
        (visit-leaf node i)
        (visit-branch node (- shift *branch-bits*) i)))
  (define (prev min shift root)
    (let ((i (if (and i (<= i (+ min (ash 1 shift))))
                 (- i min)
                 (1- (ash 1 shift)))))
      (and root (<= 0 i)
           (let ((i (visit-node root shift i)))
             (and i (+ min i))))))
  (match bs
    (($ <intset> min shift root)
     (prev min shift root))
    (($ <transient-intset> min shift root edit)
     (assert-readable! edit)
     (prev min shift root))))

(define-syntax-rule (make-intset-folder forward? seed ...)
  (lambda (f set seed ...)
    (define (visit-branch node shift min seed ...)
      (cond
       ((= shift *leaf-bits*)
        (let lp ((i (if forward? 0 (1- *leaf-size*))) (seed seed) ...)
          (if (if forward? (< i *leaf-size*) (<= 0 i))
              (if (logbit? i node)
                  (call-with-values (lambda () (f (+ i min) seed ...))
                    (lambda (seed ...)
                      (lp (if forward? (1+ i) (1- i)) seed ...)))
                  (lp (if forward? (1+ i) (1- i)) seed ...))
              (values seed ...))))
       (else
        (let ((shift (- shift *branch-bits*)))
          (let lp ((i (if forward? 0 (1- *branch-size*))) (seed seed) ...)
            (if (if forward? (< i *branch-size*) (<= 0 i))
                (let ((elt (vector-ref node i)))
                  (if elt
                      (call-with-values
                          (lambda ()
                            (visit-branch elt shift (+ min (ash i shift)) seed ...))
                        (lambda (seed ...)
                          (lp (if forward? (1+ i) (1- i)) seed ...)))
                      (lp (if forward? (1+ i) (1- i)) seed ...)))
                (values seed ...)))))))
    (match set
      (($ <intset> min shift root)
       (cond
        ((not root) (values seed ...))
        (else (visit-branch root shift min seed ...))))
      (($ <transient-intset>)
       (intset-fold f (persistent-intset set) seed ...)))))

(define intset-fold
  (case-lambda
    ((f set)
     ((make-intset-folder #t) f set))
    ((f set seed)
     ((make-intset-folder #t seed) f set seed))
    ((f set s0 s1)
     ((make-intset-folder #t s0 s1) f set s0 s1))
    ((f set s0 s1 s2)
     ((make-intset-folder #t s0 s1 s2) f set s0 s1 s2))))

(define intset-fold-right
  (case-lambda
    ((f set)
     ((make-intset-folder #f) f set))
    ((f set seed)
     ((make-intset-folder #f seed) f set seed))
    ((f set s0 s1)
     ((make-intset-folder #f s0 s1) f set s0 s1))
    ((f set s0 s1 s2)
     ((make-intset-folder #f s0 s1 s2) f set s0 s1 s2))))

(define (intset-size shift root)
  (cond
   ((not root) 0)
   ((= *leaf-bits* shift) *leaf-size*)
   (else
    (let lp ((i (1- *branch-size*)))
      (let ((node (vector-ref root i)))
        (if node
            (let ((shift (- shift *branch-bits*)))
              (+ (intset-size shift node)
                 (* i (ash 1 shift))))
            (lp (1- i))))))))

(define (intset-union a b)
  ;; Union leaves.
  (define (union-leaves a b)
    (logior (or a 0) (or b 0)))
  ;; Union A and B from index I; the result will be fresh.
  (define (union-branches/fresh shift a b i fresh)
    (let lp ((i 0))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (vector-set! fresh i (union shift a-child b-child))
          (lp (1+ i))))
       (else fresh))))
  ;; Union A and B from index I; the result may be eq? to A.
  (define (union-branches/a shift a b i)
    (let lp ((i i))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (if (eq? a-child b-child)
              (lp (1+ i))
              (let ((child (union shift a-child b-child)))
                (cond
                 ((eq? a-child child)
                  (lp (1+ i)))
                 (else
                  (let ((result (clone-branch-and-set a i child)))
                    (union-branches/fresh shift a b (1+ i) result))))))))
       (else a))))
  ;; Union A and B; the may could be eq? to either.
  (define (union-branches shift a b)
    (let lp ((i 0))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (if (eq? a-child b-child)
              (lp (1+ i))
              (let ((child (union shift a-child b-child)))
                (cond
                 ((eq? a-child child)
                  (union-branches/a shift a b (1+ i)))
                 ((eq? b-child child)
                  (union-branches/a shift b a (1+ i)))
                 (else
                  (let ((result (clone-branch-and-set a i child)))
                    (union-branches/fresh shift a b (1+ i) result))))))))
       ;; Seems they are the same but not eq?.  Odd.
       (else a))))
  (define (union shift a-node b-node)
    (cond
     ((not a-node) b-node)
     ((not b-node) a-node)
     ((eq? a-node b-node) a-node)
     ((= shift *leaf-bits*) (union-leaves a-node b-node))
     (else (union-branches (- shift *branch-bits*) a-node b-node))))
  (match (cons a b)
    ((($ <intset> a-min a-shift a-root) . ($ <intset> b-min b-shift b-root))
     (cond
      ((not b-root) a)
      ((not a-root) b)
      ((not (= b-shift a-shift))
       ;; Hoist the set with the lowest shift to meet the one with the
       ;; higher shift.
       (if (< b-shift a-shift)
           (intset-union a (add-level b-min b-shift b-root))
           (intset-union (add-level a-min a-shift a-root) b)))
      ((not (= b-min a-min))
       ;; Nodes at the same shift but different minimums will cover
       ;; disjoint ranges (due to the round-down call on min).  Hoist
       ;; both until they cover the same range.
       (intset-union (add-level a-min a-shift a-root)
                     (add-level b-min b-shift b-root)))
      (else
       ;; At this point, A and B cover the same range.
       (let ((root (union a-shift a-root b-root)))
         (cond
          ((eq? root a-root) a)
          ((eq? root b-root) b)
          (else (make-intset a-min a-shift root)))))))))

(define (intset-intersect a b)
  ;; Intersect leaves.
  (define (intersect-leaves a b)
    (let ((leaf (logand a b)))
      (if (eqv? leaf 0) #f leaf)))
  ;; Intersect A and B from index I; the result will be fresh.
  (define (intersect-branches/fresh shift a b i fresh)
    (let lp ((i 0))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (vector-set! fresh i (intersect shift a-child b-child))
          (lp (1+ i))))
       ((branch-empty? fresh) #f)
       (else fresh))))
  ;; Intersect A and B from index I; the result may be eq? to A.
  (define (intersect-branches/a shift a b i)
    (let lp ((i i))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (if (eq? a-child b-child)
              (lp (1+ i))
              (let ((child (intersect shift a-child b-child)))
                (cond
                 ((eq? a-child child)
                  (lp (1+ i)))
                 (else
                  (let ((result (clone-branch-and-set a i child)))
                    (intersect-branches/fresh shift a b (1+ i) result))))))))
       (else a))))
  ;; Intersect A and B; the may could be eq? to either.
  (define (intersect-branches shift a b)
    (let lp ((i 0))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (if (eq? a-child b-child)
              (lp (1+ i))
              (let ((child (intersect shift a-child b-child)))
                (cond
                 ((eq? a-child child)
                  (intersect-branches/a shift a b (1+ i)))
                 ((eq? b-child child)
                  (intersect-branches/a shift b a (1+ i)))
                 (else
                  (let ((result (clone-branch-and-set a i child)))
                    (intersect-branches/fresh shift a b (1+ i) result))))))))
       ;; Seems they are the same but not eq?.  Odd.
       (else a))))
  (define (intersect shift a-node b-node)
    (cond
     ((or (not a-node) (not b-node)) #f)
     ((eq? a-node b-node) a-node)
     ((= shift *leaf-bits*) (intersect-leaves a-node b-node))
     (else (intersect-branches (- shift *branch-bits*) a-node b-node))))

  (define (different-mins lo-min lo-shift lo-root hi-min hi-shift hi lo-is-a?)
    (cond
     ((<= lo-shift hi-shift)
      ;; If LO has a lower shift and a lower min, it is disjoint.  If
      ;; it has the same shift and a different min, it is also
      ;; disjoint.
      empty-intset)
     (else
      (let* ((lo-shift (- lo-shift *branch-bits*))
             (lo-idx (ash (- hi-min lo-min) (- lo-shift))))
        (cond
         ((>= lo-idx *branch-size*)
          ;; HI has a lower shift, but it not within LO.
          empty-intset)
         ((vector-ref lo-root lo-idx)
          => (lambda (lo-root)
               (let ((lo (make-intset (+ lo-min (ash lo-idx lo-shift))
                                      lo-shift
                                      lo-root)))
                 (if lo-is-a?
                     (intset-intersect lo hi)
                     (intset-intersect hi lo)))))
         (else empty-intset))))))

  (define (different-shifts-same-min min hi-shift hi-root lo lo-is-a?)
    (cond
     ((vector-ref hi-root 0)
      => (lambda (hi-root)
           (let ((hi (make-intset min
                                  (- hi-shift *branch-bits*)
                                  hi-root)))
             (if lo-is-a?
                 (intset-intersect lo hi)
                 (intset-intersect hi lo)))))
     (else empty-intset)))

  (match (cons a b)
    ((($ <intset> a-min a-shift a-root) . ($ <intset> b-min b-shift b-root))
     (cond
      ((< a-min b-min)
       (different-mins a-min a-shift a-root b-min b-shift b #t))
      ((< b-min a-min)
       (different-mins b-min b-shift b-root a-min a-shift a #f))
      ((< a-shift b-shift)
       (different-shifts-same-min b-min b-shift b-root a #t))
      ((< b-shift a-shift)
       (different-shifts-same-min a-min a-shift a-root b #f))
      (else
       ;; At this point, A and B cover the same range.
       (let ((root (intersect a-shift a-root b-root)))
         (cond
          ((eq? root a-root) a)
          ((eq? root b-root) b)
          (else (make-intset/prune a-min a-shift root)))))))))

(define (intset-subtract a b)
  ;; Intersect leaves.
  (define (subtract-leaves a b)
    (let ((out (logand a (lognot b))))
      (if (zero? out) #f out)))
  ;; Subtract B from A starting at index I; the result will be fresh.
  (define (subtract-branches/fresh shift a b i fresh)
    (let lp ((i 0))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (vector-set! fresh i (subtract-nodes shift a-child b-child))
          (lp (1+ i))))
       ((branch-empty? fresh) #f)
       (else fresh))))
  ;; Subtract B from A.  The result may be eq? to A.
  (define (subtract-branches shift a b)
    (let lp ((i 0))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (let ((child (subtract-nodes shift a-child b-child)))
            (cond
             ((eq? a-child child)
              (lp (1+ i)))
             (else
              (let ((result (clone-branch-and-set a i child)))
                (subtract-branches/fresh shift a b (1+ i) result)))))))
       (else a))))
  (define (subtract-nodes shift a-node b-node)
    (cond
     ((or (not a-node) (not b-node)) a-node)
     ((eq? a-node b-node) #f)
     ((= shift *leaf-bits*) (subtract-leaves a-node b-node))
     (else (subtract-branches (- shift *branch-bits*) a-node b-node))))

  (match (cons a b)
    ((($ <intset> a-min a-shift a-root) . ($ <intset> b-min b-shift b-root))
     (define (return root)
       (cond
        ((eq? root a-root) a)
        (else (make-intset/prune a-min a-shift root))))
     (cond
      ((<= a-shift b-shift)
       (let lp ((b-min b-min) (b-shift b-shift) (b-root b-root))
         (if (= a-shift b-shift)
             (if (= a-min b-min)
                 (return (subtract-nodes a-shift a-root b-root))
                 a)
             (let* ((b-shift (- b-shift *branch-bits*))
                    (b-idx (ash (- a-min b-min) (- b-shift)))
                    (b-min (+ b-min (ash b-idx b-shift)))
                    (b-root (and b-root
                                 (<= 0 b-idx)
                                 (< b-idx *branch-size*)
                                 (vector-ref b-root b-idx))))
               (lp b-min b-shift b-root)))))
      (else
       (return
        (let lp ((a-min a-min) (a-shift a-shift) (a-root a-root))
          (if (= a-shift b-shift)
              (if (= a-min b-min)
                  (subtract-nodes a-shift a-root b-root)
                  a-root)
              (let* ((a-shift (- a-shift *branch-bits*))
                     (a-idx (ash (- b-min a-min) (- a-shift)))
                     (a-min (+ a-min (ash a-idx a-shift)))
                     (old (and a-root
                               (<= 0 a-idx)
                               (< a-idx *branch-size*)
                               (vector-ref a-root a-idx)))
                     (new (lp a-min a-shift old)))
                (if (eq? old new)
                    a-root
                    (let ((root (clone-branch-and-set a-root a-idx new)))
                      (and (or new (not (branch-empty? root)))
                           root))))))))))))

(define (bitvector->intset bv)
  (define (finish-tail out min tail)
    (if (zero? tail)
        out
        (intset-union out (make-intset min *leaf-bits* tail))))
  (let lp ((out empty-intset) (min 0) (pos 0) (tail 0))
    (let ((pos (bit-position #t bv pos)))
      (cond
       ((not pos)
        (finish-tail out min tail))
       ((< pos (+ min *leaf-size*))
        (lp out min (1+ pos) (logior tail (ash 1 (- pos min)))))
       (else
        (let ((min* (round-down pos *leaf-bits*)))
          (lp (finish-tail out min tail)
              min* pos (ash 1 (- pos min*)))))))))

(define (intset-key-ranges intset)
  (call-with-values
      (lambda ()
        (intset-fold (lambda (k start end closed)
                       (cond
                        ((not start) (values k k closed))
                        ((= k (1+ end)) (values start k closed))
                        (else (values k k (acons start end closed)))))
                     intset #f #f '()))
    (lambda (start end closed)
      (reverse (if start (acons start end closed) closed)))))

(define (range-string ranges)
  (string-join (map (match-lambda
                      ((start . start)
                       (format #f "~a" start))
                      ((start . end)
                       (format #f "~a-~a" start end)))
                    ranges)
               ","))

(define (print-helper port tag intset)
  (let ((ranges (intset-key-ranges intset)))
    (match ranges
      (()
       (format port "#<~a>" tag))
      (_
       (format port "#<~a ~a>" tag (range-string ranges))))))

(define (print-intset intset port)
  (print-helper port "intset" intset))
(define (print-transient-intset intset port)
  (print-helper port "transient-intset" intset))

(set-record-type-printer! <intset> print-intset)
(set-record-type-printer! <transient-intset> print-transient-intset)
