;; 	Copyright (C) 2020 Free Software Foundation, Inc.
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

(define-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-43)  #:select (vector->list))
  #:use-module ((srfi srfi-69) #:prefix srfi69:)
  #:use-module ((rnrs hashtables) #:prefix rnrs:)
  #:use-module (srfi srfi-171 meta)
  #:export (rcons
            reverse-rcons
            rcount
            rany
            revery
            list-transduce
            vector-transduce
            string-transduce
            bytevector-u8-transduce
            port-transduce
            generator-transduce

            tmap
            tfilter
            tremove
            treplace
            tfilter-map
            tdrop
            tdrop-while
            ttake
            ttake-while
            tconcatenate
            tappend-map
            tdelete-neighbor-duplicates
            tdelete-duplicates
            tflatten
            tsegment
            tpartition
            tadd-between
            tenumerate
            tlog))
(cond-expand-provide (current-module) '(srfi-171))


;; A placeholder for a unique "nothing".
(define nothing (list 'nothing))
(define (nothing? val)
  (eq? val nothing))

;;; Reducing functions meant to be used at the end at the transducing process.
(define rcons
  (case-lambda
    "A transducer-friendly consing reducer with '() as identity."
    (() '())
    ((lst) (reverse! lst))
    ((lst x) (cons x lst))))

(define reverse-rcons
  (case-lambda
    "A transducer-friendly consing reducer with '() as identity.
The resulting list is in reverse order."
    (() '())
    ((lst) lst)
    ((lst x) (cons x lst))))

(define rcount
  (case-lambda
    "A counting reducer that counts any elements that made it through the
transduction.
@example
(transduce (tfilter odd?) tcount (list 1 2 3)) @result{} 2
@end example"
    (() 0)
    ((result) result)
    ((result input)
     (+ 1  result))))

(define (rany pred)
  (case-lambda
    "Return a reducer that tests input using @var{pred}. If any input satisfies
@var{pred}, return @code{(reduced value)}."
    (() #f)
    ((result) result)
    ((result input)
     (let ((test (pred input)))
       (if test
           (reduced test)
           #f)))))

(define (revery pred)
  (case-lambda
    "Returns a reducer that tests input using @var{pred}. If any input satisfies
@var{pred}, it returns @code{(reduced #f)}."
    (() #t)
    ((result) result)
    ((result input)
     (let ((test (pred input)))
       (if (and result test)
           test
           (reduced #f))))))


(define list-transduce
  (case-lambda
    ((xform f coll)
     (list-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (list-reduce xf init coll)))
       (xf result)))))

(define vector-transduce
  (case-lambda
    ((xform f coll)
     (vector-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (vector-reduce xf init coll)))
       (xf result)))))

(define string-transduce
  (case-lambda
    ((xform f coll)
     (string-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (string-reduce xf init coll)))
       (xf result)))))

(define bytevector-u8-transduce
  (case-lambda
    ((xform f coll)
     (bytevector-u8-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (bytevector-u8-reduce xf init coll)))
       (xf result)))))

(define port-transduce
  (case-lambda
    ((xform f by)
     (generator-transduce xform f by))
    ((xform f by port)
     (port-transduce xform f (f) by port))
    ((xform f init by port)
     (let* ((xf (xform f))
            (result (port-reduce xf init by port)))
       (xf result)))))

(define generator-transduce
  (case-lambda
    ((xform f gen)
     (generator-transduce xform f (f) gen))
    ((xform f init gen)
     (let* ((xf (xform f))
            (result (generator-reduce xf init gen)))
       (xf result)))))

;;; Transducers
(define (tmap f)
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result)) 
      ((result input)
       (reducer result (f input))))))

(define (tfilter pred)
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (if (pred input)
           (reducer result input)
           result)))))

(define (tremove pred)
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (if (not (pred input))
           (reducer result input)
           result)))))

(define (tfilter-map f) 
  (compose (tmap f) (tfilter values)))

(define (make-replacer map)
  (cond
   ((list? map)
    (lambda (x)
      (match (assoc x map)
        ((_ . replacer) replacer)
        (#f x))))
   ((srfi69:hash-table? map)
    (lambda (x)
      (srfi69:hash-table-ref/default map x x)))
   ((rnrs:hashtable? map)
    (lambda (x)
      (rnrs:hashtable-ref map x x)))
   ((hash-table? map)
    (lambda (x)
      (hash-ref map x x)))
   ((procedure? map) map)
   (else
    (error "Unsupported mapping in treplace" map))))


(define (treplace map)
  "Return a transducer that searches for any input in @var{map}, which may
be a guile native hashtable, an R6RS hashtable, a srfi-69 hashtable, an alist
or a one-argument procedure taking one value and producing either the same
value or a replacement one. Alists and guile-native hashtbles compare keys
using @code{equal?} whereas the other mappings use whatever equality predicate
they were created with."
  (tmap (make-replacer map)))

(define (tdrop n)
  (lambda (reducer)
    (let ((new-n (+ 1 n)))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (set! new-n (- new-n 1))
         (if (positive? new-n)
             result
             (reducer result input)))))))

(define (tdrop-while pred)
  (lambda (reducer)
    (let ((drop? #t))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (if (and (pred input) drop?)
             result
             (begin
               (set! drop? #f)
               (reducer result input))))))))

(define (ttake n)
  (lambda (reducer)
    ;; we need to reset new-n for every new transduction
    (let ((new-n n))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (let ((result (if (positive? new-n)
                           (reducer result input)
                           result)))
           (set! new-n (- new-n 1))
           (if (not (positive? new-n))
               (ensure-reduced result)
               result)))))))

(define ttake-while
  (case-lambda
    ((pred) (ttake-while pred (lambda (result input) result)))
    ((pred retf)
     (lambda (reducer)
       (let ((take? #t))
         (case-lambda
           (() (reducer))
           ((result) (reducer result))
           ((result input)
            (if (and take? (pred input))
                (reducer result input)
                (begin
                  (set! take? #f)
                  (ensure-reduced (retf result input)))))))))))

(define (tconcatenate reducer)
  (let ((preserving-reducer (preserving-reduced reducer)))
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (list-reduce preserving-reducer result input)))))

(define (tappend-map f)
  (compose (tmap f) tconcatenate))

(define (tflatten reducer)
  "tflatten is a transducer that flattens any list passed through it.
@example
(list-transduce tflatten conj (list 1 2 (list 3 4 '(5 6) 7 8)))
@result{} (1 2 3 4 5 6 7 8)
@end example"
  (case-lambda
    (() '())
    ((result) (reducer result))
    ((result input)
     (if (list? input)
         (list-reduce (preserving-reduced (tflatten reducer)) result input)
         (reducer result input)))))


(define tdelete-neighbor-duplicates
  (case-lambda
    (() (tdelete-neighbor-duplicates equal?))
    ((equality-pred?) 
     (lambda (reducer)
       (let ((prev nothing))
         (case-lambda
           (() (reducer))
           ((result) (reducer result))
           ((result input)
            (if (equality-pred? prev input)
                result
                (begin
                  (set! prev input)
                  (reducer result input))))))))))


(define* (tdelete-duplicates #:optional (equality-pred? equal?))
  "tdelede-duplicates is a transducer that deletes any subsequent duplicate
elements. Comparisons is done using @var{equality-pred?}, which defaults
to @code{equal?}."
  (lambda (reducer)
    (let ((already-seen (srfi69:make-hash-table equality-pred?)))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (if (srfi69:hash-table-exists? already-seen input)
             result
             (begin
               (srfi69:hash-table-set! already-seen input #t)
               (reducer result input))))))))

(define (tsegment n)
  "Return a transducer that partitions the input into
lists of @var{n} items. If the input stops it flushes any
accumulated state, which may be shorter than @var{n}."
  (if (not (and (integer? n) (positive? n)))
      (error "argument to tsegment must be a positive integer")
      (lambda (reducer)
        (let ((i 0)
              (collect (make-vector n)))
          (case-lambda
            (() (reducer))
            ((result)
             ;; if there is anything collected when we are asked to quit
             ;; we flush it to the remaining transducers
             (let ((result
                    (if (zero? i)
                        result
                        (reducer result (vector->list collect 0 i)))))
               (set! i 0)
               ;; now finally, pass it downstreams
               (if (reduced? result)
                   (reducer (unreduce result))
                   (reducer result))))
            ((result input)
             (vector-set! collect i input)
             (set! i (+ i 1))
             ;; If we have collected enough input we can pass it on downstream
             (if (< i n)
                 result
                 (let ((next-input (vector->list collect 0 i)))
                   (set! i 0)
                   (reducer result next-input)))))))))

(define (tpartition f)
  "Return a transducer that partitions any input by whenever
@code{(f input)} changes value. "
  (lambda (reducer)
    (let* ((prev nothing)
           (collect '()))
      (case-lambda
        (() (reducer))
        ((result)
         (let ((result
                (if (null? collect)
                    result
                    (reducer result (reverse! collect)))))
           (set! collect '())
           (if (reduced? result)
               (reducer (unreduce result))
               (reducer result))))
        ((result input)
         (let ((fout (f input)))
           (cond
            ((or (equal? fout prev) (nothing? prev)) ; collect
             (set! prev fout)
             (set! collect (cons input collect))
             result)
            (else ; flush what we collected already to the reducer
             (let ((next-input  (reverse! collect)))
               (set! prev fout)
               (set! collect (list input))
               (reducer result next-input))))))))))

(define (tadd-between elem)
  "Return a transducer that interposes @var{elem} between each value pushed
through the transduction."
  (lambda (reducer)
    (let ((send-elem? #f))
      (case-lambda
        (() (reducer))
        ((result)
         (reducer result))
        ((result input)
         (if send-elem?
             (let ((result (reducer result elem)))
               (if (reduced? result)
                   result
                   (reducer result input)))
             (begin
               (set! send-elem? #t)
               (reducer result input))))))))

(define* (tenumerate #:optional (n 0))
  "Return a transducer that indexes every value passed through into a cons
pair as @code{(index . value)}. Starts at @var{n} which defaults to 0."
  (lambda (reducer)
    (let ((n n))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (let ((input (cons n input)))
           (set! n (+ n 1))
           (reducer result input)))))))

(define* (tlog #:optional
               (log-function (lambda (result input) (write input) (newline))))
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (log-function result input)
       (reducer result input)))))




