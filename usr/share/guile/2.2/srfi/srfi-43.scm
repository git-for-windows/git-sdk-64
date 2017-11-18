;;; srfi-43.scm -- SRFI 43 Vector library

;;      Copyright (C) 2014 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Author: Mark H Weaver <mhw@netris.org>

(define-module (srfi srfi-43)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:re-export (make-vector vector vector? vector-ref vector-set!
                           vector-length)
  #:replace (vector-copy vector-fill! list->vector vector->list)
  #:export (vector-empty? vector= vector-unfold vector-unfold-right
                          vector-reverse-copy
                          vector-append vector-concatenate
                          vector-fold vector-fold-right
                          vector-map vector-map!
                          vector-for-each vector-count
                          vector-index vector-index-right
                          vector-skip vector-skip-right
                          vector-binary-search
                          vector-any vector-every
                          vector-swap! vector-reverse!
                          vector-copy! vector-reverse-copy!
                          reverse-vector->list
                          reverse-list->vector))

(cond-expand-provide (current-module) '(srfi-43))

(define (error-from who msg . args)
  (apply error
         (string-append (symbol->string who) ": " msg)
         args))

(define-syntax-rule (assert-nonneg-exact-integer k who)
  (unless (and (exact-integer? k)
               (not (negative? k)))
    (error-from who "expected non-negative exact integer, got" k)))

(define-syntax-rule (assert-procedure f who)
  (unless (procedure? f)
    (error-from who "expected procedure, got" f)))

(define-syntax-rule (assert-vector v who)
  (unless (vector? v)
    (error-from who "expected vector, got" v)))

(define-syntax-rule (assert-valid-index i len who)
  (unless (and (exact-integer? i)
               (<= 0 i len))
    (error-from who "invalid index" i)))

(define-syntax-rule (assert-valid-start start len who)
  (unless (and (exact-integer? start)
               (<= 0 start len))
    (error-from who "invalid start index" start)))

(define-syntax-rule (assert-valid-range start end len who)
  (unless (and (exact-integer? start)
               (exact-integer? end)
               (<= 0 start end len))
    (error-from who "invalid index range" start end)))

(define-syntax-rule (assert-vectors vs who)
  (let loop ((vs vs))
    (unless (null? vs)
      (assert-vector (car vs) who)
      (loop (cdr vs)))))

;; Return the length of the shortest vector in VS.
;; VS must have at least one element.
(define (min-length vs)
  (let loop ((vs (cdr vs))
             (result (vector-length (car vs))))
    (if (null? vs)
        result
        (loop (cdr vs) (min result (vector-length (car vs)))))))

;; Return a list of the Ith elements of the vectors in VS.
(define (vectors-ref vs i)
  (let loop ((vs vs) (xs '()))
    (if (null? vs)
        (reverse! xs)
        (loop (cdr vs) (cons (vector-ref (car vs) i)
                             xs)))))

(define vector-unfold
  (case-lambda
    "(vector-unfold f length initial-seed ...) -> vector

The fundamental vector constructor.  Create a vector whose length is
LENGTH and iterates across each index k from 0 up to LENGTH - 1,
applying F at each iteration to the current index and current seeds, in
that order, to receive n + 1 values: the element to put in the kth slot
of the new vector, and n new seeds for the next iteration.  It is an
error for the number of seeds to vary between iterations."
    ((f len)
     (assert-procedure f 'vector-unfold)
     (assert-nonneg-exact-integer len 'vector-unfold)
     (let ((v (make-vector len)))
       (let loop ((i 0))
         (unless (= i len)
           (vector-set! v i (f i))
           (loop (+ i 1))))
       v))
    ((f len seed)
     (assert-procedure f 'vector-unfold)
     (assert-nonneg-exact-integer len 'vector-unfold)
     (let ((v (make-vector len)))
       (let loop ((i 0) (seed seed))
         (unless (= i len)
           (receive (x seed) (f i seed)
             (vector-set! v i x)
             (loop (+ i 1) seed))))
       v))
    ((f len seed1 seed2)
     (assert-procedure f 'vector-unfold)
     (assert-nonneg-exact-integer len 'vector-unfold)
     (let ((v (make-vector len)))
       (let loop ((i 0) (seed1 seed1) (seed2 seed2))
         (unless (= i len)
           (receive (x seed1 seed2) (f i seed1 seed2)
             (vector-set! v i x)
             (loop (+ i 1) seed1 seed2))))
       v))
    ((f len . seeds)
     (assert-procedure f 'vector-unfold)
     (assert-nonneg-exact-integer len 'vector-unfold)
     (let ((v (make-vector len)))
       (let loop ((i 0) (seeds seeds))
         (unless (= i len)
           (receive (x . seeds) (apply f i seeds)
             (vector-set! v i x)
             (loop (+ i 1) seeds))))
       v))))

(define vector-unfold-right
  (case-lambda
    "(vector-unfold-right f length initial-seed ...) -> vector

The fundamental vector constructor.  Create a vector whose length is
LENGTH and iterates across each index k from LENGTH - 1 down to 0,
applying F at each iteration to the current index and current seeds, in
that order, to receive n + 1 values: the element to put in the kth slot
of the new vector, and n new seeds for the next iteration.  It is an
error for the number of seeds to vary between iterations."
    ((f len)
     (assert-procedure f 'vector-unfold-right)
     (assert-nonneg-exact-integer len 'vector-unfold-right)
     (let ((v (make-vector len)))
       (let loop ((i (- len 1)))
         (unless (negative? i)
           (vector-set! v i (f i))
           (loop (- i 1))))
       v))
    ((f len seed)
     (assert-procedure f 'vector-unfold-right)
     (assert-nonneg-exact-integer len 'vector-unfold-right)
     (let ((v (make-vector len)))
       (let loop ((i (- len 1)) (seed seed))
         (unless (negative? i)
           (receive (x seed) (f i seed)
             (vector-set! v i x)
             (loop (- i 1) seed))))
       v))
    ((f len seed1 seed2)
     (assert-procedure f 'vector-unfold-right)
     (assert-nonneg-exact-integer len 'vector-unfold-right)
     (let ((v (make-vector len)))
       (let loop ((i (- len 1)) (seed1 seed1) (seed2 seed2))
         (unless (negative? i)
           (receive (x seed1 seed2) (f i seed1 seed2)
             (vector-set! v i x)
             (loop (- i 1) seed1 seed2))))
       v))
    ((f len . seeds)
     (assert-procedure f 'vector-unfold-right)
     (assert-nonneg-exact-integer len 'vector-unfold-right)
     (let ((v (make-vector len)))
       (let loop ((i (- len 1)) (seeds seeds))
         (unless (negative? i)
           (receive (x . seeds) (apply f i seeds)
             (vector-set! v i x)
             (loop (- i 1) seeds))))
       v))))

(define guile-vector-copy (@ (guile) vector-copy))

;; TODO: Enhance Guile core 'vector-copy' to do this.
(define vector-copy
  (case-lambda*
   "(vector-copy vec [start [end [fill]]]) -> vector

Allocate a new vector whose length is END - START and fills it with
elements from vec, taking elements from vec starting at index START
and stopping at index END.  START defaults to 0 and END defaults to
the value of (vector-length VEC).  If END extends beyond the length of
VEC, the slots in the new vector that obviously cannot be filled by
elements from VEC are filled with FILL, whose default value is
unspecified."
   ((v) (guile-vector-copy v))
   ((v start)
    (assert-vector v 'vector-copy)
    (let ((len (vector-length v)))
      (assert-valid-start start len 'vector-copy)
      (let ((result (make-vector (- len start))))
        (vector-move-left! v start len result 0)
        result)))
   ((v start end #:optional (fill *unspecified*))
    (assert-vector v 'vector-copy)
    (let ((len (vector-length v)))
      (unless (and (exact-integer? start)
                   (exact-integer? end)
                   (<= 0 start end))
        (error-from 'vector-copy "invalid index range" start end))
      (let ((result (make-vector (- end start) fill)))
        (vector-move-left! v start (min end len) result 0)
        result)))))

(define vector-reverse-copy
  (let ()
    (define (%vector-reverse-copy vec start end)
      (let* ((len (- end start))
             (result (make-vector len)))
        (let loop ((i 0) (j (- end 1)))
          (unless (= i len)
            (vector-set! result i (vector-ref vec j))
            (loop (+ i 1) (- j 1))))
        result))
    (case-lambda
      "(vector-reverse-copy vec [start [end]]) -> vector

Allocate a new vector whose length is END - START and fills it with
elements from vec, taking elements from vec in reverse order starting
at index START and stopping at index END.  START defaults to 0 and END
defaults to the value of (vector-length VEC)."
      ((vec)
       (assert-vector vec 'vector-reverse-copy)
       (%vector-reverse-copy vec 0 (vector-length vec)))
      ((vec start)
       (assert-vector vec 'vector-reverse-copy)
       (let ((len (vector-length vec)))
         (assert-valid-start start len 'vector-reverse-copy)
         (%vector-reverse-copy vec start len)))
      ((vec start end)
       (assert-vector vec 'vector-reverse-copy)
       (let ((len (vector-length vec)))
         (assert-valid-range start end len 'vector-reverse-copy)
         (%vector-reverse-copy vec start end))))))

(define (%vector-concatenate vs)
  (let* ((result-len (let loop ((vs vs) (len 0))
                       (if (null? vs)
                           len
                           (loop (cdr vs) (+ len (vector-length (car vs)))))))
         (result (make-vector result-len)))
    (let loop ((vs vs) (pos 0))
      (unless (null? vs)
        (let* ((v (car vs))
               (len (vector-length v)))
          (vector-move-left! v 0 len result pos)
          (loop (cdr vs) (+ pos len)))))
    result))

(define vector-append
  (case-lambda
    "(vector-append vec ...) -> vector

Return a newly allocated vector that contains all elements in order
from the subsequent locations in VEC ..."
    (() (vector))
    ((v)
     (assert-vector v 'vector-append)
     (guile-vector-copy v))
    ((v1 v2)
     (assert-vector v1 'vector-append)
     (assert-vector v2 'vector-append)
     (let ((len1 (vector-length v1))
           (len2 (vector-length v2)))
       (let ((result (make-vector (+ len1 len2))))
         (vector-move-left! v1 0 len1 result 0)
         (vector-move-left! v2 0 len2 result len1)
         result)))
    (vs
     (assert-vectors vs 'vector-append)
     (%vector-concatenate vs))))

(define (vector-concatenate vs)
  "(vector-concatenate list-of-vectors) -> vector

Append each vector in LIST-OF-VECTORS.  Equivalent to:
  (apply vector-append LIST-OF-VECTORS)"
  (assert-vectors vs 'vector-concatenate)
  (%vector-concatenate vs))

(define (vector-empty? vec)
  "(vector-empty? vec) -> boolean

Return true if VEC is empty, i.e. its length is 0, and false if not."
  (assert-vector vec 'vector-empty?)
  (zero? (vector-length vec)))

(define vector=
  (let ()
    (define (all-of-length? len vs)
      (or (null? vs)
          (and (= len (vector-length (car vs)))
               (all-of-length? len (cdr vs)))))
    (define (=up-to? i elt=? v1 v2)
      (or (negative? i)
          (let ((x1 (vector-ref v1 i))
                (x2 (vector-ref v2 i)))
            (and (or (eq? x1 x2) (elt=? x1 x2))
                 (=up-to? (- i 1) elt=? v1 v2)))))
    (case-lambda
      "(vector= elt=? vec ...) -> boolean

Return true if the vectors VEC ... have equal lengths and equal
elements according to ELT=?.  ELT=? is always applied to two
arguments.  Element comparison must be consistent with eq?, in the
following sense: if (eq? a b) returns true, then (elt=? a b) must also
return true.  The order in which comparisons are performed is
unspecified."
      ((elt=?)
       (assert-procedure elt=? 'vector=)
       #t)
      ((elt=? v)
       (assert-procedure elt=? 'vector=)
       (assert-vector v 'vector=)
       #t)
      ((elt=? v1 v2)
       (assert-procedure elt=? 'vector=)
       (assert-vector v1 'vector=)
       (assert-vector v2 'vector=)
       (let ((len (vector-length v1)))
         (and (= len (vector-length v2))
              (=up-to? (- len 1) elt=? v1 v2))))
      ((elt=? v1 . vs)
       (assert-procedure elt=? 'vector=)
       (assert-vector  v1 'vector=)
       (assert-vectors vs 'vector=)
       (let ((len (vector-length v1)))
         (and (all-of-length? len vs)
              (let loop ((vs vs))
                (or (null? vs)
                    (and (=up-to? (- len 1) elt=? v1 (car vs))
                         (loop (cdr vs)))))))))))

(define vector-fold
  (case-lambda
    "(vector-fold kons knil vec1 vec2 ...) -> value

The fundamental vector iterator.  KONS is iterated over each index in
all of the vectors, stopping at the end of the shortest; KONS is
applied as (KONS i state (vector-ref VEC1 i) (vector-ref VEC2 i) ...)
where STATE is the current state value, and I is the current index.
The current state value begins with KNIL, and becomes whatever KONS
returned at the respective iteration.  The iteration is strictly
left-to-right."
    ((kcons knil v)
     (assert-procedure kcons 'vector-fold)
     (assert-vector v 'vector-fold)
     (let ((len (vector-length v)))
       (let loop ((i 0) (state knil))
         (if (= i len)
             state
             (loop (+ i 1) (kcons i state (vector-ref v i)))))))
    ((kcons knil v1 v2)
     (assert-procedure kcons 'vector-fold)
     (assert-vector v1 'vector-fold)
     (assert-vector v2 'vector-fold)
     (let ((len (min (vector-length v1) (vector-length v2))))
       (let loop ((i 0) (state knil))
         (if (= i len)
             state
             (loop (+ i 1)
                   (kcons i state (vector-ref v1 i) (vector-ref v2 i)))))))
    ((kcons knil . vs)
     (assert-procedure kcons 'vector-fold)
     (assert-vectors vs 'vector-fold)
     (let ((len (min-length vs)))
       (let loop ((i 0) (state knil))
         (if (= i len)
             state
             (loop (+ i 1) (apply kcons i state (vectors-ref vs i)))))))))

(define vector-fold-right
  (case-lambda
    "(vector-fold-right kons knil vec1 vec2 ...) -> value

The fundamental vector iterator.  KONS is iterated over each index in
all of the vectors, starting at the end of the shortest; KONS is
applied as (KONS i state (vector-ref VEC1 i) (vector-ref VEC2 i) ...)
where STATE is the current state value, and I is the current index.
The current state value begins with KNIL, and becomes whatever KONS
returned at the respective iteration.  The iteration is strictly
right-to-left."
    ((kcons knil v)
     (assert-procedure kcons 'vector-fold-right)
     (assert-vector v 'vector-fold-right)
     (let ((len (vector-length v)))
       (let loop ((i (- len 1)) (state knil))
         (if (negative? i)
             state
             (loop (- i 1) (kcons i state (vector-ref v i)))))))
    ((kcons knil v1 v2)
     (assert-procedure kcons 'vector-fold-right)
     (assert-vector v1 'vector-fold-right)
     (assert-vector v2 'vector-fold-right)
     (let ((len (min (vector-length v1) (vector-length v2))))
       (let loop ((i (- len 1)) (state knil))
         (if (negative? i)
             state
             (loop (- i 1)
                   (kcons i state (vector-ref v1 i) (vector-ref v2 i)))))))
    ((kcons knil . vs)
     (assert-procedure kcons 'vector-fold-right)
     (assert-vectors vs 'vector-fold-right)
     (let ((len (min-length vs)))
       (let loop ((i (- len 1)) (state knil))
         (if (negative? i)
             state
             (loop (- i 1) (apply kcons i state (vectors-ref vs i)))))))))

(define vector-map
  (case-lambda
    "(vector-map f vec2 vec2 ...) -> vector

Return a new vector of the shortest size of the vector arguments.
Each element at index i of the new vector is mapped from the old
vectors by (F i (vector-ref VEC1 i) (vector-ref VEC2 i) ...).  The
dynamic order of application of F is unspecified."
    ((f v)
     (assert-procedure f 'vector-map)
     (assert-vector v 'vector-map)
     (let* ((len (vector-length v))
            (result (make-vector len)))
       (let loop ((i 0))
         (unless (= i len)
           (vector-set! result i (f i (vector-ref v i)))
           (loop (+ i 1))))
       result))
    ((f v1 v2)
     (assert-procedure f 'vector-map)
     (assert-vector v1 'vector-map)
     (assert-vector v2 'vector-map)
     (let* ((len (min (vector-length v1) (vector-length v2)))
            (result (make-vector len)))
       (let loop ((i 0))
         (unless (= i len)
           (vector-set! result i (f i (vector-ref v1 i) (vector-ref v2 i)))
           (loop (+ i 1))))
       result))
    ((f . vs)
     (assert-procedure f 'vector-map)
     (assert-vectors vs 'vector-map)
     (let* ((len (min-length vs))
            (result (make-vector len)))
       (let loop ((i 0))
         (unless (= i len)
           (vector-set! result i (apply f i (vectors-ref vs i)))
           (loop (+ i 1))))
       result))))

(define vector-map!
  (case-lambda
    "(vector-map! f vec2 vec2 ...) -> unspecified

Similar to vector-map, but rather than mapping the new elements into a
new vector, the new mapped elements are destructively inserted into
VEC1.  The dynamic order of application of F is unspecified."
    ((f v)
     (assert-procedure f 'vector-map!)
     (assert-vector v 'vector-map!)
     (let ((len (vector-length v)))
       (let loop ((i 0))
         (unless (= i len)
           (vector-set! v i (f i (vector-ref v i)))
           (loop (+ i 1))))))
    ((f v1 v2)
     (assert-procedure f 'vector-map!)
     (assert-vector v1 'vector-map!)
     (assert-vector v2 'vector-map!)
     (let ((len (min (vector-length v1) (vector-length v2))))
       (let loop ((i 0))
         (unless (= i len)
           (vector-set! v1 i (f i (vector-ref v1 i) (vector-ref v2 i)))
           (loop (+ i 1))))))
    ((f . vs)
     (assert-procedure f 'vector-map!)
     (assert-vectors vs 'vector-map!)
     (let ((len (min-length vs))
           (v1 (car vs)))
       (let loop ((i 0))
         (unless (= i len)
           (vector-set! v1 i (apply f i (vectors-ref vs i)))
           (loop (+ i 1))))))))

(define vector-for-each
  (case-lambda
    "(vector-for-each f vec1 vec2 ...) -> unspecified

Call (F i VEC1[i] VEC2[i] ...) for each index i less than the length
of the shortest vector passed.  The iteration is strictly
left-to-right."
    ((f v)
     (assert-procedure f 'vector-for-each)
     (assert-vector v 'vector-for-each)
     (let ((len (vector-length v)))
       (let loop ((i 0))
         (unless (= i len)
           (f i (vector-ref v i))
           (loop (+ i 1))))))
    ((f v1 v2)
     (assert-procedure f 'vector-for-each)
     (assert-vector v1 'vector-for-each)
     (assert-vector v2 'vector-for-each)
     (let ((len (min (vector-length v1)
                     (vector-length v2))))
       (let loop ((i 0))
         (unless (= i len)
           (f i (vector-ref v1 i) (vector-ref v2 i))
           (loop (+ i 1))))))
    ((f . vs)
     (assert-procedure f 'vector-for-each)
     (assert-vectors vs 'vector-for-each)
     (let ((len (min-length vs)))
       (let loop ((i 0))
         (unless (= i len)
           (apply f i (vectors-ref vs i))
           (loop (+ i 1))))))))

(define vector-count
  (case-lambda
    "(vector-count pred? vec1 vec2 ...) -> exact nonnegative integer

Count the number of indices i for which (PRED? VEC1[i] VEC2[i] ...)
returns true, where i is less than the length of the shortest vector
passed."
    ((pred? v)
     (assert-procedure pred? 'vector-count)
     (assert-vector v 'vector-count)
     (let ((len (vector-length v)))
       (let loop ((i 0) (count 0))
         (cond ((= i len) count)
               ((pred? i (vector-ref v i))
                (loop (+ i 1) (+ count 1)))
               (else
                (loop (+ i 1) count))))))
    ((pred? v1 v2)
     (assert-procedure pred? 'vector-count)
     (assert-vector v1 'vector-count)
     (assert-vector v2 'vector-count)
     (let ((len (min (vector-length v1)
                     (vector-length v2))))
       (let loop ((i 0) (count 0))
         (cond ((= i len) count)
               ((pred? i (vector-ref v1 i) (vector-ref v2 i))
                (loop (+ i 1) (+ count 1)))
               (else
                (loop (+ i 1) count))))))
    ((pred? . vs)
     (assert-procedure pred? 'vector-count)
     (assert-vectors vs 'vector-count)
     (let ((len (min-length vs)))
       (let loop ((i 0) (count 0))
         (cond ((= i len) count)
               ((apply pred? i (vectors-ref vs i))
                (loop (+ i 1) (+ count 1)))
               (else
                (loop (+ i 1) count))))))))

(define vector-index
  (case-lambda
    "(vector-index pred? vec1 vec2 ...) -> exact nonnegative integer or #f

Find and return the index of the first elements in VEC1 VEC2 ... that
satisfy PRED?.  If no matching element is found by the end of the
shortest vector, return #f."
    ((pred? v)
     (assert-procedure pred? 'vector-index)
     (assert-vector v 'vector-index)
     (let ((len (vector-length v)))
       (let loop ((i 0))
         (and (< i len)
              (if (pred? (vector-ref v i))
                  i
                  (loop (+ i 1)))))))
    ((pred? v1 v2)
     (assert-procedure pred? 'vector-index)
     (assert-vector v1 'vector-index)
     (assert-vector v2 'vector-index)
     (let ((len (min (vector-length v1)
                     (vector-length v2))))
       (let loop ((i 0))
         (and (< i len)
              (if (pred? (vector-ref v1 i)
                         (vector-ref v2 i))
                  i
                  (loop (+ i 1)))))))
    ((pred? . vs)
     (assert-procedure pred? 'vector-index)
     (assert-vectors vs 'vector-index)
     (let ((len (min-length vs)))
       (let loop ((i 0))
         (and (< i len)
              (if (apply pred? (vectors-ref vs i))
                  i
                  (loop (+ i 1)))))))))

(define vector-index-right
  (case-lambda
    "(vector-index-right pred? vec1 vec2 ...) -> exact nonnegative integer or #f

Find and return the index of the last elements in VEC1 VEC2 ... that
satisfy PRED?, searching from right-to-left.  If no matching element
is found before the end of the shortest vector, return #f."
    ((pred? v)
     (assert-procedure pred? 'vector-index-right)
     (assert-vector v 'vector-index-right)
     (let ((len (vector-length v)))
       (let loop ((i (- len 1)))
         (and (>= i 0)
              (if (pred? (vector-ref v i))
                  i
                  (loop (- i 1)))))))
    ((pred? v1 v2)
     (assert-procedure pred? 'vector-index-right)
     (assert-vector v1 'vector-index-right)
     (assert-vector v2 'vector-index-right)
     (let ((len (min (vector-length v1)
                     (vector-length v2))))
       (let loop ((i (- len 1)))
         (and (>= i 0)
              (if (pred? (vector-ref v1 i)
                         (vector-ref v2 i))
                  i
                  (loop (- i 1)))))))
    ((pred? . vs)
     (assert-procedure pred? 'vector-index-right)
     (assert-vectors vs 'vector-index-right)
     (let ((len (min-length vs)))
       (let loop ((i (- len 1)))
         (and (>= i 0)
              (if (apply pred? (vectors-ref vs i))
                  i
                  (loop (- i 1)))))))))

(define vector-skip
  (case-lambda
    "(vector-skip pred? vec1 vec2 ...) -> exact nonnegative integer or #f

Find and return the index of the first elements in VEC1 VEC2 ... that
do not satisfy PRED?.  If no matching element is found by the end of
the shortest vector, return #f."
    ((pred? v)
     (assert-procedure pred? 'vector-skip)
     (assert-vector v 'vector-skip)
     (let ((len (vector-length v)))
       (let loop ((i 0))
         (and (< i len)
              (if (pred? (vector-ref v i))
                  (loop (+ i 1))
                  i)))))
    ((pred? v1 v2)
     (assert-procedure pred? 'vector-skip)
     (assert-vector v1 'vector-skip)
     (assert-vector v2 'vector-skip)
     (let ((len (min (vector-length v1)
                     (vector-length v2))))
       (let loop ((i 0))
         (and (< i len)
              (if (pred? (vector-ref v1 i)
                         (vector-ref v2 i))
                  (loop (+ i 1))
                  i)))))
    ((pred? . vs)
     (assert-procedure pred? 'vector-skip)
     (assert-vectors vs 'vector-skip)
     (let ((len (min-length vs)))
       (let loop ((i 0))
         (and (< i len)
              (if (apply pred? (vectors-ref vs i))
                  (loop (+ i 1))
                  i)))))))

(define vector-skip-right
  (case-lambda
    "(vector-skip-right pred? vec1 vec2 ...) -> exact nonnegative integer or #f

Find and return the index of the last elements in VEC1 VEC2 ... that
do not satisfy PRED?, searching from right-to-left.  If no matching
element is found before the end of the shortest vector, return #f."
    ((pred? v)
     (assert-procedure pred? 'vector-skip-right)
     (assert-vector v 'vector-skip-right)
     (let ((len (vector-length v)))
       (let loop ((i (- len 1)))
         (and (not (negative? i))
              (if (pred? (vector-ref v i))
                  (loop (- i 1))
                  i)))))
    ((pred? v1 v2)
     (assert-procedure pred? 'vector-skip-right)
     (assert-vector v1 'vector-skip-right)
     (assert-vector v2 'vector-skip-right)
     (let ((len (min (vector-length v1)
                     (vector-length v2))))
       (let loop ((i (- len 1)))
         (and (not (negative? i))
              (if (pred? (vector-ref v1 i)
                         (vector-ref v2 i))
                  (loop (- i 1))
                  i)))))
    ((pred? . vs)
     (assert-procedure pred? 'vector-skip-right)
     (assert-vectors vs 'vector-skip-right)
     (let ((len (min-length vs)))
       (let loop ((i (- len 1)))
         (and (not (negative? i))
              (if (apply pred? (vectors-ref vs i))
                  (loop (- i 1))
                  i)))))))

(define vector-binary-search
  (let ()
    (define (%vector-binary-search vec value cmp start end)
      (let loop ((lo start) (hi end))
        (and (< lo hi)
             (let* ((i (quotient (+ lo hi) 2))
                    (x (vector-ref vec i))
                    (c (cmp x value)))
               (cond ((zero? c) i)
                     ((positive? c) (loop lo i))
                     ((negative? c) (loop (+ i 1) hi)))))))
    (case-lambda
      "(vector-binary-search vec value cmp [start [end]]) -> exact nonnegative integer or #f

Find and return an index of VEC between START and END whose value is
VALUE using a binary search.  If no matching element is found, return
#f.  The default START is 0 and the default END is the length of VEC.
CMP must be a procedure of two arguments such that (CMP A B) returns
a negative integer if A < B, a positive integer if A > B, or zero if
A = B.  The elements of VEC must be sorted in non-decreasing order
according to CMP."
      ((vec value cmp)
       (assert-vector vec 'vector-binary-search)
       (assert-procedure cmp 'vector-binary-search)
       (%vector-binary-search vec value cmp 0 (vector-length vec)))

      ((vec value cmp start)
       (assert-vector vec 'vector-binary-search)
       (let ((len (vector-length vec)))
         (assert-valid-start start len 'vector-binary-search)
         (%vector-binary-search vec value cmp start len)))

      ((vec value cmp start end)
       (assert-vector vec 'vector-binary-search)
       (let ((len (vector-length vec)))
         (assert-valid-range start end len 'vector-binary-search)
         (%vector-binary-search vec value cmp start end))))))

(define vector-any
  (case-lambda
    "(vector-any pred? vec1 vec2 ...) -> value or #f

Find the first parallel set of elements from VEC1 VEC2 ... for which
PRED? returns a true value.  If such a parallel set of elements
exists, vector-any returns the value that PRED? returned for that set
of elements.  The iteration is strictly left-to-right."
    ((pred? v)
     (assert-procedure pred? 'vector-any)
     (assert-vector v 'vector-any)
     (let ((len (vector-length v)))
       (let loop ((i 0))
         (and (< i len)
              (or (pred? (vector-ref v i))
                  (loop (+ i 1)))))))
    ((pred? v1 v2)
     (assert-procedure pred? 'vector-any)
     (assert-vector v1 'vector-any)
     (assert-vector v2 'vector-any)
     (let ((len (min (vector-length v1)
                     (vector-length v2))))
       (let loop ((i 0))
         (and (< i len)
              (or (pred? (vector-ref v1 i)
                         (vector-ref v2 i))
                  (loop (+ i 1)))))))
    ((pred? . vs)
     (assert-procedure pred? 'vector-any)
     (assert-vectors vs 'vector-any)
     (let ((len (min-length vs)))
       (let loop ((i 0))
         (and (< i len)
              (or (apply pred? (vectors-ref vs i))
                  (loop (+ i 1)))))))))

(define vector-every
  (case-lambda
    "(vector-every pred? vec1 vec2 ...) -> value or #f

If, for every index i less than the length of the shortest vector
argument, the set of elements VEC1[i] VEC2[i] ... satisfies PRED?,
vector-every returns the value that PRED? returned for the last set of
elements, at the last index of the shortest vector.  The iteration is
strictly left-to-right."
    ((pred? v)
     (assert-procedure pred? 'vector-every)
     (assert-vector v 'vector-every)
     (let ((len (vector-length v)))
       (or (zero? len)
           (let loop ((i 0))
             (let ((val (pred? (vector-ref v i)))
                   (next-i (+ i 1)))
               (if (or (not val) (= next-i len))
                   val
                   (loop next-i)))))))
    ((pred? v1 v2)
     (assert-procedure pred? 'vector-every)
     (assert-vector v1 'vector-every)
     (assert-vector v2 'vector-every)
     (let ((len (min (vector-length v1)
                     (vector-length v2))))
       (or (zero? len)
           (let loop ((i 0))
             (let ((val (pred? (vector-ref v1 i)
                               (vector-ref v2 i)))
                   (next-i (+ i 1)))
               (if (or (not val) (= next-i len))
                   val
                   (loop next-i)))))))
    ((pred? . vs)
     (assert-procedure pred? 'vector-every)
     (assert-vectors vs 'vector-every)
     (let ((len (min-length vs)))
       (or (zero? len)
           (let loop ((i 0))
             (let ((val (apply pred? (vectors-ref vs i)))
                   (next-i (+ i 1)))
               (if (or (not val) (= next-i len))
                   val
                   (loop next-i)))))))))

(define (vector-swap! vec i j)
  "(vector-swap! vec i j) -> unspecified

Swap the values of the locations in VEC at I and J."
  (assert-vector vec 'vector-swap!)
  (let ((len (vector-length vec)))
    (assert-valid-index i len 'vector-swap!)
    (assert-valid-index j len 'vector-swap!)
    (let ((tmp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j tmp))))

;; TODO: Enhance Guile core 'vector-fill!' to do this.
(define vector-fill!
  (let ()
    (define guile-vector-fill!
      (@ (guile) vector-fill!))
    (define (%vector-fill! vec fill start end)
      (let loop ((i start))
        (when (< i end)
          (vector-set! vec i fill)
          (loop (+ i 1)))))
    (case-lambda
      "(vector-fill! vec fill [start [end]]) -> unspecified

Assign the value of every location in VEC between START and END to
FILL.  START defaults to 0 and END defaults to the length of VEC."
      ((vec fill)
       (guile-vector-fill! vec fill))
      ((vec fill start)
       (assert-vector vec 'vector-fill!)
       (let ((len (vector-length vec)))
         (assert-valid-start start len 'vector-fill!)
         (%vector-fill! vec fill start len)))
      ((vec fill start end)
       (assert-vector vec 'vector-fill!)
       (let ((len (vector-length vec)))
         (assert-valid-range start end len 'vector-fill!)
         (%vector-fill! vec fill start end))))))

(define (%vector-reverse! vec start end)
  (let loop ((i start) (j (- end 1)))
    (when (< i j)
      (let ((tmp (vector-ref vec i)))
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j tmp)
        (loop (+ i 1) (- j 1))))))

(define vector-reverse!
  (case-lambda
    "(vector-reverse! vec [start [end]]) -> unspecified

Destructively reverse the contents of VEC between START and END.
START defaults to 0 and END defaults to the length of VEC."
    ((vec)
     (assert-vector vec 'vector-reverse!)
     (%vector-reverse! vec 0 (vector-length vec)))
    ((vec start)
     (assert-vector vec 'vector-reverse!)
     (let ((len (vector-length vec)))
       (assert-valid-start start len 'vector-reverse!)
       (%vector-reverse! vec start len)))
    ((vec start end)
     (assert-vector vec 'vector-reverse!)
     (let ((len (vector-length vec)))
       (assert-valid-range start end len 'vector-reverse!)
       (%vector-reverse! vec start end)))))

(define-syntax-rule (define-vector-copier! copy! docstring inner-proc)
  (define copy!
    (let ((%copy! inner-proc))
      (case-lambda
        docstring
        ((target tstart source)
         (assert-vector target 'copy!)
         (assert-vector source 'copy!)
         (let ((tlen (vector-length target))
               (slen (vector-length source)))
           (assert-valid-start tstart tlen 'copy!)
           (unless (>= tlen (+ tstart slen))
             (error-from 'copy! "would write past end of target"))
           (%copy! target tstart source 0 slen)))

        ((target tstart source sstart)
         (assert-vector target 'copy!)
         (assert-vector source 'copy!)
         (let ((tlen (vector-length target))
               (slen (vector-length source)))
           (assert-valid-start tstart tlen 'copy!)
           (assert-valid-start sstart slen 'copy!)
           (unless (>= tlen (+ tstart (- slen sstart)))
             (error-from 'copy! "would write past end of target"))
           (%copy! target tstart source sstart slen)))

        ((target tstart source sstart send)
         (assert-vector target 'copy!)
         (assert-vector source 'copy!)
         (let ((tlen (vector-length target))
               (slen (vector-length source)))
           (assert-valid-start tstart tlen 'copy!)
           (assert-valid-range sstart send slen 'copy!)
           (unless (>= tlen (+ tstart (- send sstart)))
             (error-from 'copy! "would write past end of target"))
           (%copy! target tstart source sstart send)))))))

(define-vector-copier! vector-copy!
  "(vector-copy! target tstart source [sstart [send]]) -> unspecified

Copy a block of elements from SOURCE to TARGET, both of which must be
vectors, starting in TARGET at TSTART and starting in SOURCE at
SSTART, ending when SEND - SSTART elements have been copied.  It is an
error for TARGET to have a length less than TSTART + (SEND - SSTART).
SSTART defaults to 0 and SEND defaults to the length of SOURCE."
  (lambda (target tstart source sstart send)
    (if (< tstart sstart)
        (vector-move-left!  source sstart send target tstart)
        (vector-move-right! source sstart send target tstart))))

(define-vector-copier! vector-reverse-copy!
  "(vector-reverse-copy! target tstart source [sstart [send]]) -> unspecified

Like vector-copy!, but copy the elements in the reverse order.  It is
an error if TARGET and SOURCE are identical vectors and the TARGET and
SOURCE ranges overlap; however, if TSTART = SSTART,
vector-reverse-copy! behaves as (vector-reverse! TARGET TSTART SEND)
would."
  (lambda (target tstart source sstart send)
    (if (and (eq? target source) (= tstart sstart))
        (%vector-reverse! target sstart send)
        (let loop ((i tstart) (j (- send 1)))
          (when (>= j sstart)
            (vector-set! target i (vector-ref source j))
            (loop (+ i 1) (- j 1)))))))

(define vector->list
  (let ()
    (define (%vector->list vec start end)
      (let loop ((i (- end 1))
                 (result '()))
        (if (< i start)
            result
            (loop (- i 1) (cons (vector-ref vec i) result)))))
    (case-lambda
      "(vector->list vec [start [end]]) -> proper-list

Return a newly allocated list containing the elements in VEC between
START and END.  START defaults to 0 and END defaults to the length of
VEC."
      ((vec)
       (assert-vector vec 'vector->list)
       (%vector->list vec 0 (vector-length vec)))
      ((vec start)
       (assert-vector vec 'vector->list)
       (let ((len (vector-length vec)))
         (assert-valid-start start len 'vector->list)
         (%vector->list vec start len)))
      ((vec start end)
       (assert-vector vec 'vector->list)
       (let ((len (vector-length vec)))
         (assert-valid-range start end len 'vector->list)
         (%vector->list vec start end))))))

(define reverse-vector->list
  (let ()
    (define (%reverse-vector->list vec start end)
      (let loop ((i start)
                 (result '()))
        (if (>= i end)
            result
            (loop (+ i 1) (cons (vector-ref vec i) result)))))
    (case-lambda
      "(reverse-vector->list vec [start [end]]) -> proper-list

Return a newly allocated list containing the elements in VEC between
START and END in reverse order.  START defaults to 0 and END defaults
to the length of VEC."
      ((vec)
       (assert-vector vec 'reverse-vector->list)
       (%reverse-vector->list vec 0 (vector-length vec)))
      ((vec start)
       (assert-vector vec 'reverse-vector->list)
       (let ((len (vector-length vec)))
         (assert-valid-start start len 'reverse-vector->list)
         (%reverse-vector->list vec start len)))
      ((vec start end)
       (assert-vector vec 'reverse-vector->list)
       (let ((len (vector-length vec)))
         (assert-valid-range start end len 'reverse-vector->list)
         (%reverse-vector->list vec start end))))))

;; TODO: change to use 'case-lambda' and improve error checking.
(define* (list->vector lst #:optional (start 0) (end (length lst)))
  "(list->vector proper-list [start [end]]) -> vector

Return a newly allocated vector of the elements from PROPER-LIST with
indices between START and END.  START defaults to 0 and END defaults
to the length of PROPER-LIST."
  (let* ((len (- end start))
         (result (make-vector len)))
    (let loop ((i 0) (lst (drop lst start)))
      (if (= i len)
          result
          (begin (vector-set! result i (car lst))
                 (loop (+ i 1) (cdr lst)))))))

;; TODO: change to use 'case-lambda' and improve error checking.
(define* (reverse-list->vector lst #:optional (start 0) (end (length lst)))
  "(reverse-list->vector proper-list [start [end]]) -> vector

Return a newly allocated vector of the elements from PROPER-LIST with
indices between START and END, in reverse order.  START defaults to 0
and END defaults to the length of PROPER-LIST."
  (let* ((len (- end start))
         (result (make-vector len)))
    (let loop ((i (- len 1)) (lst (drop lst start)))
      (if (negative? i)
          result
          (begin (vector-set! result i (car lst))
                 (loop (- i 1) (cdr lst)))))))
