;;; Type analysis on CPS
;;; Copyright (C) 2014-2021, 2023 Free Software Foundation, Inc.
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
;;; Type analysis computes the possible types and ranges that values may
;;; have at all program positions.  This analysis can help to prove that
;;; a primcall has no side-effects, if its arguments have the
;;; appropriate type and range.  It can also enable constant folding of
;;; type predicates and, in the future, enable the compiler to choose
;;; untagged, unboxed representations for numbers.
;;;
;;; For the purposes of this analysis, a "type" is an aspect of a value
;;; that will not change.  Guile's CPS intermediate language does not
;;; carry manifest type information that asserts properties about given
;;; values; instead, we recover this information via flow analysis,
;;; garnering properties from type predicates, constant literals,
;;; primcall results, and primcalls that assert that their arguments are
;;; of particular types.
;;;
;;; A range denotes a subset of the set of values in a type, bounded by
;;; a minimum and a maximum.  The precise meaning of a range depends on
;;; the type.  For real numbers, the range indicates an inclusive lower
;;; and upper bound on the integer value of a type.  For vectors, the
;;; range indicates the length of the vector.  The range is the union of
;;; the signed and unsigned 64-bit ranges.  Additionally, the minimum
;;; bound of a range may be -inf.0, and the maximum bound may be +inf.0.
;;; For some types, like pairs, the concept of "range" makes no sense.
;;; In these cases we consider the range to be -inf.0 to +inf.0.
;;;
;;; Types are represented as a bitfield.  Fewer bits means a more precise
;;; type.  Although normally only values that have a single type will
;;; have an associated range, this is not enforced.  The range applies
;;; to all types in the bitfield.  When control flow meets, the types and
;;; ranges meet with the union operator.
;;;
;;; It is not practical to precisely compute value ranges in all cases.
;;; For example, in the following case:
;;;
;;;   (let lp ((n 0)) (when (foo) (lp (1+ n))))
;;;
;;; The first time that range analysis visits the program, N is
;;; determined to be the exact integer 0.  The second time, it is an
;;; exact integer in the range [0, 1]; the third, [0, 2]; and so on.
;;; This analysis will terminate, but only after the positive half of
;;; the 64-bit range has been fully explored and we decide that the
;;; range of N is [0, +inf.0].  At the same time, we want to do range
;;; analysis and type analysis at the same time, as there are
;;; interactions between them, notably in the case of `sqrt' which
;;; returns a complex number if its argument cannot be proven to be
;;; non-negative.  So what we do instead is to precisely propagate types
;;; and ranges when propagating forward, but after the first backwards
;;; branch is seen, we cause backward branches that would expand the
;;; range of a value to saturate that range towards positive or negative
;;; infinity (as appropriate).
;;;
;;; A naive approach to type analysis would build up a table that has
;;; entries for all variables at all program points, but this has
;;; N-squared complexity and quickly grows unmanageable.  Instead, we
;;; use _intmaps_ from (language cps intmap) to share state between
;;; connected program points.
;;;
;;; Code:

(define-module (language cps types)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module ((system syntax internal) #:select (syntax?))
  #:use-module (system base target)
  #:export (;; Specific types.
            &fixnum
            &bignum
            &flonum
            &complex
            &fraction

            &char
            &special-immediate
            &symbol
            &keyword
            &procedure
            &pointer
            &fluid
            &pair
            &immutable-vector
            &mutable-vector
            &box
            &struct
            &string
            &bytevector
            &bitvector
            &array
            &syntax
            &other-heap-object

            ;; Special immediate values.
            &null &nil &false &true &unspecified &undefined &eof

            ;; Union types.
            &exact-integer &exact-number &real &number &vector

            ;; Untagged types.
            &f64
            &u64
            &s64

            ;; Helper.
            type<=?

            ;; Interface for type inference.
            constant-type
            infer-types
            lookup-pre-type
            lookup-post-type
            primcall-types-check?))

(define-syntax define-flags
  (lambda (x)
    (syntax-case x ()
      ((_ all shift name ...)
       (let ((count (length #'(name ...))))
         (with-syntax (((n ...) (iota count))
                       (count count))
           #'(begin
               (define-syntax name (identifier-syntax (ash 1 n)))
               ...
               (define-syntax all (identifier-syntax (1- (ash 1 count))))
               (define-syntax shift (identifier-syntax count)))))))))

;; More precise types have fewer bits.
(define-flags &all-types &type-bits
  &fixnum
  &bignum
  &flonum
  &complex
  &fraction

  &char
  &special-immediate

  &symbol
  &keyword
  &procedure
  &pointer
  &fluid
  &pair
  &immutable-vector
  &mutable-vector
  &box
  &struct
  &string
  &bytevector
  &bitvector
  &array
  &syntax
  &other-heap-object

  &f64
  &u64
  &s64)

(define-syntax &no-type (identifier-syntax 0))

;; Special immediate values.  Note that the values for the first 4 of
;; these are important; see uses below.
(define-syntax &null        (identifier-syntax 0))
(define-syntax &nil         (identifier-syntax 1))
(define-syntax &false       (identifier-syntax 2))
(define-syntax &true        (identifier-syntax 3))
(define-syntax &unspecified (identifier-syntax 4))
(define-syntax &undefined   (identifier-syntax 5))
(define-syntax &eof         (identifier-syntax 6))

(define-syntax &exact-integer
  (identifier-syntax (logior &fixnum &bignum)))
(define-syntax &exact-number
  (identifier-syntax (logior &fixnum &bignum &fraction)))
(define-syntax &real
  (identifier-syntax (logior &fixnum &bignum &flonum &fraction)))
(define-syntax &heap-number
  (identifier-syntax (logior &flonum &bignum &complex &fraction)))
(define-syntax &number
  (identifier-syntax (logior &fixnum &bignum &flonum &complex &fraction)))

(define-syntax &vector
  (identifier-syntax (logior &immutable-vector &mutable-vector)))

(define-syntax-rule (type<=? x type)
  (zero? (logand x (lognot type))))

;; Versions of min and max that do not coerce exact numbers to become
;; inexact.
(define min
  (case-lambda
    ((a b) (if (< a b) a b))
    ((a b c) (min (min a b) c))
    ((a b c d) (min (min a b) c d))))
(define max
  (case-lambda
    ((a b) (if (> a b) a b))
    ((a b c) (max (max a b) c))
    ((a b c d) (max (max a b) c d))))



(define-syntax-rule (define-compile-time-value name val)
  (define-syntax name
    (make-variable-transformer
     (lambda (x)
       (syntax-case x (set!)
         (var (identifier? #'var)
              (datum->syntax #'var val)))))))

(define-compile-time-value &fx32-min (- #x20000000))
(define-compile-time-value &fx32-max    #x1fffFFFF)
(define-compile-time-value &fx64-min (- #x2000000000000000))
(define-compile-time-value &fx64-max    #x1fffFFFFffffFFFF)
(define-compile-time-value &s64-min  (- #x8000000000000000))
(define-compile-time-value &s64-max     #x7fffFFFFffffFFFF)
(define-compile-time-value &u64-max     #xffffFFFFffffFFFF)

(define-syntax &range-min (identifier-syntax &s64-min))
(define-syntax &range-max (identifier-syntax &u64-max))

(define *max-codepoint* #x10ffff)

(define-inlinable (make-unclamped-type-entry type min max)
  (vector type min max))
(define-inlinable (type-entry-type tentry)
  (vector-ref tentry 0))
(define-inlinable (type-entry-min tentry)
  (vector-ref tentry 1))
(define-inlinable (type-entry-max tentry)
  (vector-ref tentry 2))

(define-inlinable (clamp-min val)
  (cond
   ;; Fast path to avoid comparisons with bignums.
   ((<= most-negative-fixnum val most-positive-fixnum) val)
   ((< val &range-min) -inf.0)
   ((< &range-max val) &range-max)
   (else val)))

(define-inlinable (clamp-max val)
  (cond
   ;; Fast path to avoid comparisons with bignums.
   ((<= most-negative-fixnum val most-positive-fixnum) val)
   ((< &range-max val) +inf.0)
   ((< val &range-min) &range-min)
   (else val)))

(define-inlinable (make-type-entry type min max)
  (vector type (clamp-min min) (clamp-max max)))

(define all-types-entry (make-type-entry &all-types -inf.0 +inf.0))

(define* (var-type-entry typeset var #:optional (default all-types-entry))
  (intmap-ref typeset var (lambda (_) default)))

(define (var-type typeset var)
  (type-entry-type (var-type-entry typeset var)))
(define (var-min typeset var)
  (type-entry-min (var-type-entry typeset var)))
(define (var-max typeset var)
  (type-entry-max (var-type-entry typeset var)))

;; Is the type entry A contained entirely within B?
(define (type-entry<=? a b)
  (match (cons a b)
    ((#(a-type a-min a-max) . #(b-type b-min b-max))
     (and (eqv? b-type (logior a-type b-type))
          (<= b-min a-min)
          (>= b-max a-max)))))

(define (type-entry-union a b)
  (cond
   ((type-entry<=? b a) a)
   ((type-entry<=? a b) b)
   (else (make-type-entry
          (logior (type-entry-type a) (type-entry-type b))
          (min (type-entry-min a) (type-entry-min b))
          (max (type-entry-max a) (type-entry-max b))))))

(define (type-entry-saturating-union a b)
  (cond
   ((type-entry<=? b a) a)
   (else
    (make-type-entry
     (logior (type-entry-type a) (type-entry-type b))
     (let ((a-min (type-entry-min a))
           (b-min (type-entry-min b)))
       (cond
        ((not (< b-min a-min)) a-min)
        ((< 0 b-min) 0)
        ((< &fx32-min b-min) &fx32-min)
        ((< &fx64-min b-min) &fx64-min)
        ((< &range-min b-min) &range-min)
        (else -inf.0)))
     (let ((a-max (type-entry-max a))
           (b-max (type-entry-max b)))
       (cond
        ((not (> b-max a-max)) a-max)
        ((> &fx32-max b-max) &fx32-max)
        ((> &fx64-max b-max) &fx64-max)
        ((> &s64-max b-max) &s64-max)
        ((> &range-max b-max) &range-max)
        (else +inf.0)))))))

(define (type-entry-intersection a b)
  (cond
   ((type-entry<=? a b) a)
   ((type-entry<=? b a) b)
   (else (make-type-entry
          (logand (type-entry-type a) (type-entry-type b))
          (max (type-entry-min a) (type-entry-min b))
          (min (type-entry-max a) (type-entry-max b))))))

(define (adjoin-var typeset var entry)
  (intmap-add typeset var entry type-entry-union))

(define (restrict-var typeset var entry)
  (intmap-add typeset var entry type-entry-intersection))

(define (constant-type val)
  "Compute the type and range of VAL.  Return three values: the type,
minimum, and maximum."
  (define (return type val)
    (if val
        (values type val val)
        (values type -inf.0 +inf.0)))
  (cond
   ((number? val)
    (cond
     ((exact-integer? val)
      (return (if (<= (target-most-negative-fixnum)
                      val
                      (target-most-positive-fixnum))
                  &fixnum
                  &bignum)
              val))
     ((eqv? (imag-part val) 0)
      (if (nan? val)
          (values &flonum -inf.0 +inf.0)
          (values
           (if (exact? val) &fraction &flonum)
           (if (rational? val) (inexact->exact (floor val)) val)
           (if (rational? val) (inexact->exact (ceiling val)) val))))
     (else (return &complex #f))))
   ((eq? val '()) (return &special-immediate &null))
   ((eq? val #nil) (return &special-immediate &nil))
   ((eq? val #t) (return &special-immediate &true))
   ((eq? val #f) (return &special-immediate &false))
   ((eqv? val *unspecified*) (return &special-immediate &unspecified))
   ((eof-object? val) (return &special-immediate &eof))
   ((char? val) (return &char (char->integer val)))
   ((symbol? val) (return &symbol #f))
   ((keyword? val) (return &keyword #f))
   ((pair? val) (return &pair #f))
   ((vector? val) (return &immutable-vector (vector-length val)))
   ((string? val) (return &string (string-length val)))
   ((bytevector? val) (return &bytevector (bytevector-length val)))
   ((bitvector? val) (return &bitvector (bitvector-length val)))
   ((array? val) (return &array (array-rank val)))
   ((syntax? val) (return &syntax 0))
   ((not (variable-bound? (make-variable val)))
    (return &special-immediate &undefined))

   (else (error "unhandled constant" val))))

(define (constant-type-entry val)
  "Compute the type and range of VAL.  Return three values: the type,
minimum, and maximum."
  (call-with-values (lambda () (constant-type val))
    (lambda (type min max)
      (make-type-entry type min max))))

(define *type-checkers* (make-hash-table))
(define *type-inferrers* (make-hash-table))

(define-syntax-rule (define-type-helper name)
  (define-syntax-parameter name
    (lambda (stx)
      (syntax-violation 'name
                        "macro used outside of define-type"
                        stx))))
(define-type-helper define!)
(define-type-helper restrict!)
(define-type-helper &type)
(define-type-helper &min)
(define-type-helper &max)

(define-syntax-rule (define-exact-integer! result min max)
  (let ((min* min) (max* max))
    (define! result
      (if (<= (target-most-negative-fixnum)
              min* max*
              (target-most-positive-fixnum))
          &fixnum
          &exact-integer)
      min* max*)))

;; Accessors to use in type inferrers where you know that the values
;; must be in some range for the computation to proceed (not throw an
;; error).  Note that these accessors should be used even for &u64 and
;; &s64 values, whose definitions you would think would be apparent
;; already.  However it could be that the graph isn't sorted, so we see
;; a use before a definition, in which case we need to clamp the generic
;; limits to the &u64/&s64 range.
(define-syntax-rule (&min/0 x) (max (&min x) 0))
(define-syntax-rule (&max/u64 x) (min (&max x) &u64-max))
(define-syntax-rule (&min/s64 x) (max (&min x) &s64-min))
(define-syntax-rule (&max/s64 x) (min (&max x) &s64-max))
(define-syntax-rule (&min/fixnum x) (max (&min x) (target-most-negative-fixnum)))
(define-syntax-rule (&max/fixnum x) (min (&max x) (target-most-positive-fixnum)))
(define-syntax-rule (&max/size x) (min (&max x) (target-max-size-t)))
(define-syntax-rule (&max/scm-size x) (min (&max x) (target-max-size-t/scm)))

(define-syntax-rule (define-type-checker/param (name param arg ...) body ...)
  (hashq-set!
   *type-checkers*
   'name
   (lambda (typeset param arg ...)
     (syntax-parameterize
         ((&type (syntax-rules () ((_ val) (var-type typeset val))))
          (&min  (syntax-rules () ((_ val) (var-min typeset val))))
          (&max  (syntax-rules () ((_ val) (var-max typeset val)))))
       body ...))))

(define-syntax-rule (define-type-checker (name arg ...) body ...)
  (define-type-checker/param (name param arg ...) body ...))

(define-syntax-rule (check-type arg type min max)
  ;; If the arg is negative, it is a closure variable.
  (and (>= arg 0)
       (zero? (logand (lognot type) (&type arg)))
       (<= min (&min arg))
       (<= (&max arg) max)))

(define-syntax-rule (define-type-inferrer* (name param succ var ...) body ...)
  (hashq-set!
   *type-inferrers*
   'name
   (lambda (in succ param var ...)
     (let ((out in))
       (syntax-parameterize
           ((define!
              (syntax-rules ()
                ((_ val type min max)
                 (set! out (adjoin-var out val
                                       (make-type-entry type min max))))))
            (restrict!
             (syntax-rules ()
               ((_ val type min max)
                (set! out (restrict-var out val
                                        (make-type-entry type min max))))))
            (&type (syntax-rules () ((_ val) (var-type in val))))
            (&min  (syntax-rules () ((_ val) (var-min in val))))
            (&max  (syntax-rules () ((_ val) (var-max in val)))))
         body ...
         out)))))

(define-syntax-rule (define-type-inferrer (name arg ...) body ...)
  (define-type-inferrer* (name param succ arg ...) body ...))

(define-syntax-rule (define-type-inferrer/param (name param arg ...) body ...)
  (define-type-inferrer* (name param succ arg ...) body ...))

(define-syntax-rule (define-predicate-inferrer (name arg ... true?) body ...)
  (define-type-inferrer* (name param succ arg ...)
    (let ((true? (not (zero? succ))))
      body ...)))

(define-syntax-rule (define-predicate-inferrer/param
                      (name param arg ... true?) body ...)
  (define-type-inferrer* (name param succ arg ...)
    (let ((true? (not (zero? succ))))
      body ...)))

(define-syntax define-simple-type-checker
  (lambda (x)
    (define (parse-spec l)
      (syntax-case l ()
        (() '())
        (((type min max) . l) (cons #'(type min max) (parse-spec #'l)))
        (((type min+max) . l) (cons #'(type min+max min+max) (parse-spec #'l)))
        ((type . l) (cons #'(type -inf.0 +inf.0) (parse-spec #'l)))))
    (syntax-case x ()
      ((_ (name arg-spec ...) result-spec ...)
       (with-syntax
           (((arg ...) (generate-temporaries #'(arg-spec ...)))
            (((arg-type arg-min arg-max) ...) (parse-spec #'(arg-spec ...))))
         #'(define-type-checker (name arg ...)
             (and (check-type arg arg-type arg-min arg-max)
                  ...)))))))

(define-syntax define-simple-type-inferrer
  (lambda (x)
    (define (parse-spec l)
      (syntax-case l ()
        (() '())
        (((type min max) . l) (cons #'(type min max) (parse-spec #'l)))
        (((type min+max) . l) (cons #'(type min+max min+max) (parse-spec #'l)))
        ((type . l) (cons #'(type -inf.0 +inf.0) (parse-spec #'l)))))
    (syntax-case x ()
      ((_ (name arg-spec ...) result-spec ...)
       (with-syntax
           (((arg ...) (generate-temporaries #'(arg-spec ...)))
            (((arg-type arg-min arg-max) ...) (parse-spec #'(arg-spec ...)))
            ((res ...) (generate-temporaries #'(result-spec ...)))
            (((res-type res-min res-max) ...) (parse-spec #'(result-spec ...))))
         #'(define-type-inferrer (name arg ... res ...)
             (restrict! arg arg-type arg-min arg-max)
             ...
             (define! res res-type res-min res-max)
             ...))))))

(define-syntax-rule (define-simple-type (name arg-spec ...) result-spec ...)
  (begin
    (define-simple-type-checker (name arg-spec ...))
    (define-simple-type-inferrer (name arg-spec ...) result-spec ...)))

(define-syntax-rule (define-simple-types
                      ((name arg-spec ...) result-spec ...)
                      ...)
  (begin
    (define-simple-type (name arg-spec ...) result-spec ...)
    ...))

(define-syntax-rule (define-type-checker-aliases orig alias ...)
  (let ((check (hashq-ref *type-checkers* 'orig)))
    (hashq-set! *type-checkers* 'alias check)
    ...))
(define-syntax-rule (define-type-inferrer-aliases orig alias ...)
  (let ((check (hashq-ref *type-inferrers* 'orig)))
    (hashq-set! *type-inferrers* 'alias check)
    ...))
(define-syntax-rule (define-type-aliases orig alias ...)
  (begin
    (define-type-checker-aliases orig alias ...)
    (define-type-inferrer-aliases orig alias ...)))




;;; This list of primcall type definitions follows the order of
;;; effects-analysis.scm; please keep it in a similar order.
;;;
;;; There is no need to add checker definitions for expressions that do
;;; not exhibit the &type-check effect, as callers should not ask if
;;; such an expression does or does not type-check.  For those that do
;;; exhibit &type-check, you should define a type inferrer unless the
;;; primcall will never typecheck.
;;;
;;; Likewise there is no need to define inferrers for primcalls which
;;; return &all-types values and which never raise exceptions from which
;;; we can infer the types of incoming values.




;;;
;;; Generic effect-free predicates.
;;;

(define-syntax-rule (infer-constant-comparison ctype cval val true?)
  (let ()
    (define (range-subtract lo hi x)
      (values (if (eqv? lo x) (1+ lo) lo)
              (if (eqv? hi x) (1- hi) hi)))
   (cond
    (true? (restrict! val ctype cval cval))
    (else
     (when (eqv? (&type val) ctype)
       (let-values (((lo hi) (range-subtract (&min val) (&max val) cval)))
         (restrict! val ctype lo hi)))))))

(define-predicate-inferrer/param (eq-constant? c val true?)
  (call-with-values (lambda () (constant-type c))
    (lambda (ctype cval cval*)
      ;; Either (= cval cval*), or the value is meaningless for this type.
      (infer-constant-comparison ctype cval val true?))))

;; Can't usefully pass undefined as a parameter to eq-constant?, so we
;; keep its special predicate.
(define-predicate-inferrer (undefined? val true?)
  (infer-constant-comparison &special-immediate &undefined val true?))

;; Various inferrers rely on these having contiguous values starting from 0.
(eval-when (expand)
  (unless (< -1 &null &nil &false &true 4)
    (error "unexpected special immediate values")))
(define-predicate-inferrer (null? val true?)
  (cond
   (true? (restrict! val &special-immediate &null &nil))
   (else
    (when (eqv? (&type val) &special-immediate)
      (restrict! val &special-immediate (1+ &nil) +inf.0)))))

(define-predicate-inferrer (false? val true?)
  (cond
   (true? (restrict! val &special-immediate &nil &false))
   (else
    (when (and (eqv? (&type val) &special-immediate) (> (&min val) &null))
      (restrict! val &special-immediate (1+ &false) +inf.0)))))

(define-predicate-inferrer (nil? val true?)
  (cond
   (true? (restrict! val &special-immediate &null &false))
   (else
    (when (eqv? (&type val) &special-immediate)
      (restrict! val &special-immediate (1+ &false) +inf.0)))))

(define-predicate-inferrer (heap-object? val true?)
  (define &immediate-types
    (logior &fixnum &char &special-immediate))
  (define &heap-object-types
    (logand &all-types (lognot &immediate-types)))
  (restrict! val (if true? &heap-object-types &immediate-types) -inf.0 +inf.0))

(define-predicate-inferrer (fixnum? val true?)
  (cond
   (true?
    (restrict! val &fixnum
               (target-most-negative-fixnum) (target-most-positive-fixnum)))
   ((type<=? (&type val) &exact-integer)
    (cond
     ((<= (&max val) (target-most-positive-fixnum))
      (restrict! val &bignum -inf.0 (1- (target-most-negative-fixnum))))
     ((>= (&min val) (target-most-negative-fixnum))
      (restrict! val &bignum (1+ (target-most-positive-fixnum)) +inf.0))
     (else
      (restrict! val &bignum -inf.0 +inf.0))))
   (else
    (restrict! val (logand &all-types (lognot &fixnum)) -inf.0 +inf.0))))

(define-predicate-inferrer (bignum? val true?)
  (cond
   (true?
    (cond
     ((<= (&max val) (target-most-positive-fixnum))
      (restrict! val &bignum -inf.0 (1- (target-most-negative-fixnum))))
     ((>= (&min val) (target-most-negative-fixnum))
      (restrict! val &bignum (1+ (target-most-positive-fixnum)) +inf.0))
     (else
      (restrict! val &bignum -inf.0 +inf.0))))
   ((type<=? (&type val) &exact-integer)
    (restrict! val &fixnum
               (target-most-negative-fixnum) (target-most-positive-fixnum)))
   (else
    (restrict! val (logand &all-types (lognot &bignum)) -inf.0 +inf.0))))

(define-syntax-rule (define-simple-predicate-inferrer predicate type)
  (define-predicate-inferrer (predicate val true?)
    (restrict! val (if true? type (lognot type)) -inf.0 +inf.0)))

(define-simple-predicate-inferrer bignum? &bignum)
(define-simple-predicate-inferrer bitvector? &bitvector)
(define-simple-predicate-inferrer bytevector? &bytevector)
(define-simple-predicate-inferrer char? &char)
(define-simple-predicate-inferrer compnum? &complex)
(define-simple-predicate-inferrer flonum? &flonum)
(define-simple-predicate-inferrer fixnum? &fixnum)
(define-simple-predicate-inferrer fluid? &fluid)
(define-simple-predicate-inferrer fracnum? &fraction)
(define-simple-predicate-inferrer immutable-vector? &immutable-vector)
(define-simple-predicate-inferrer keyword? &keyword)
(define-simple-predicate-inferrer mutable-vector? &mutable-vector)
(define-simple-predicate-inferrer pair? &pair)
(define-simple-predicate-inferrer pointer? &pointer)
(define-simple-predicate-inferrer program? &procedure)
(define-simple-predicate-inferrer string? &string)
(define-simple-predicate-inferrer struct? &struct)
(define-simple-predicate-inferrer symbol? &symbol)
(define-simple-predicate-inferrer syntax? &syntax)
(define-simple-predicate-inferrer variable? &box)

(define-simple-predicate-inferrer number? &number)
(define-type-inferrer-aliases number? rational? complex?)
(define-simple-predicate-inferrer heap-number? &heap-number)
(define-simple-predicate-inferrer real? &real)
(let ((&maybe-integer (logior &exact-integer &flonum &complex)))
  (define-simple-predicate-inferrer integer? &maybe-integer))
(define-simple-predicate-inferrer exact-integer? &exact-integer)
(define-simple-predicate-inferrer exact? &exact-number)
(let ((&inexact-number (logior &flonum &complex)))
  (define-simple-predicate-inferrer inexact? &inexact-number))

(define-type-inferrer-aliases eq? heap-numbers-equal?)

(define-predicate-inferrer (procedure? val true?)
  ;; Besides proper procedures, structs and smobs can also be applicable
  ;; in the guile-vm target.
  (define applicable-types (logior &procedure &struct &other-heap-object))
  (when true?
    (restrict! val (logand (&type val) applicable-types)
               (&min val) (&max val))))

(define-predicate-inferrer (vector? val true?)
  (define &not-vector (logand &all-types (lognot &vector)))
  (restrict! val (if true? &vector &not-vector) -inf.0 +inf.0))

(define-predicate-inferrer (eq? a b true?)
  ;; We can only propagate information down the true leg.
  (when true?
    (let ((type (logand (&type a) (&type b)))
          (min (max (&min a) (&min b)))
          (max (min (&max a) (&max b))))
      (restrict! a type min max)
      (restrict! b type min max))))
(define-type-inferrer-aliases eq? heap-numbers-equal?)



(define-type-inferrer/param (load-const/unlikely param result)
  (let ((ent (constant-type-entry param)))
    (define! result (type-entry-type ent)
      (type-entry-min ent) (type-entry-max ent))))

(define-type-inferrer (u64->s64 u64 s64)
  (if (<= (&max u64) &s64-max)
      (define! s64 &s64 (&min u64) (&max u64))
      (define! s64 &s64 &s64-min &s64-max)))

(define-type-inferrer (s64->u64 s64 u64)
  (if (<= 0 (&min s64))
      (define! u64 &u64 (&min s64) (&max s64))
      (define! u64 &u64 0 &u64-max)))



;;;
;;; High-level object representation.
;;;

(define-type-inferrer/param (allocate-vector param size result)
  (define! result &vector (&min/0 size) (&max/scm-size size)))
(define-type-inferrer/param (allocate-vector/immediate param result)
  (define size param)
  (define! result &vector size size))
(define-type-inferrer (vector-length v result)
  (define! result &u64 (&min/0 v) (&max/scm-size v)))
(define-type-inferrer (vector-ref v idx result)
  (restrict! v &vector (1+ (&min/0 idx)) (target-max-size-t/scm))
  (define! result &all-types -inf.0 +inf.0))
(define-type-inferrer/param (vector-ref/immediate param v result)
  (define idx param)
  (restrict! v &vector (1+ idx) (target-max-size-t/scm))
  (define! result &all-types -inf.0 +inf.0))
(define-type-inferrer (vector-set! v idx val)
  (restrict! v &vector (1+ (&min/0 idx)) (target-max-size-t/scm)))
(define-type-inferrer/param (vector-set!/immediate param v val)
  (define idx param)
  (restrict! v &vector (1+ param) (target-max-size-t/scm)))

(define-type-inferrer (cons head tail result)
  (define! result &pair -inf.0 +inf.0))
(define-type-inferrer (box val result)
  (define! result &box -inf.0 +inf.0))
;; No inferrers for pair or box accessors; because type checks dominate
;; these accessors, they would add no information.

(define-type-inferrer/param (allocate-struct param vtable result)
  (define nfields param)
  ;; It would be nice to be able to restrict the vtable-size of vtable,
  ;; but because vtables are themselves structs which have associated
  ;; size ranges, there's nowhere to put the vtable-size ranges.  Humm!
  (define! result &struct nfields nfields))
(define-type-inferrer (vtable-size vtable result)
  (define! result &u64 0 (target-max-size-t/scm)))
;; No predicate inferrers for vtable-has-unboxed-fields? and
;; vtable-field-boxed?, as there is nowhere to store this info.
(define-type-inferrer (struct-vtable struct result)
  (define! result &struct 0 (target-max-size-t/scm)))
(define-type-inferrer/param (struct-ref param struct result)
  (define idx param)
  (restrict! struct &struct (1+ idx) (target-max-size-t/scm))
  (define! result &all-types -inf.0 +inf.0))
(define-type-inferrer/param (struct-set! param struct val)
  (define idx param)
  (restrict! struct &struct (1+ idx) (target-max-size-t/scm)))

(define-type-inferrer (bv-contents bv result)
  (define! result &other-heap-object -inf.0 +inf.0))
(define-type-inferrer (bv-length bv result)
  (define! result &u64 (&min/0 bv) (&max/size bv)))

(define-type-inferrer (string-length str result)
  (define! result &u64 (&min/0 str) (&max/size str)))
(define-type-inferrer (string-ref str idx result)
  (define! result &u64 0 *max-codepoint*))

(define-type-inferrer (symbol-hash sym result)
  (define! result &u64 0 &u64-max))

(define-type-inferrer/param (make-closure param code result)
  (define nfree param)
  (define! result &procedure nfree nfree))
;; No information would be provided by closure-ref / closure-set!
;; inferrers.




;;;
;;; Low-level object representation.
;;;

(define (annotation->type ann)
  (match ann
    ('pair &pair)
    ('vector &vector)
    ('string &string)
    ('stringbuf &string)
    ('symbol &symbol)
    ('bytevector &bytevector)
    ('box &box)
    ('closure &procedure)
    ('struct &struct)
    ('atomic-box &all-types)
    ('keyword &keyword)))

(define (annotation->mutable-type ann)
  (match ann
    ('vector &mutable-vector)
    (_ (annotation->type ann))))

(define-type-inferrer/param (allocate-words param size result)
  (define! result (annotation->mutable-type param)
    (&min/0 size) (&max/scm-size size)))

(define-type-inferrer/param (allocate-words/immediate param result)
  (match param
    ((annotation . size)
     (define! result (annotation->mutable-type annotation)
       size size))))

(define-type-inferrer-aliases allocate-words allocate-pointerless-words)
(define-type-inferrer-aliases allocate-words/immediate
  allocate-pointerless-words/immediate)

(define-type-inferrer/param (scm-ref param obj idx result)
  (restrict! obj (annotation->type param)
             (1+ (&min/0 idx)) (target-max-size-t/scm))
  (define! result &all-types -inf.0 +inf.0))

(define-type-inferrer/param (scm-ref/immediate param obj result)
  (match param
    ((annotation . idx)
     (restrict! obj (annotation->type annotation) (1+ idx) +inf.0)
     (define! result &all-types -inf.0 +inf.0))))

(define-type-inferrer/param (scm-ref/tag param obj result)
  (restrict! obj (annotation->type param) -inf.0 +inf.0)
  (define! result &all-types -inf.0 +inf.0))
(define-type-inferrer/param (scm-set!/tag param obj val)
  (restrict! obj (annotation->mutable-type param) -inf.0 +inf.0))

(define-type-inferrer/param (scm-set! param obj idx val)
  (restrict! obj (annotation->mutable-type param) (1+ (&min/0 idx)) +inf.0))

(define-type-inferrer/param (scm-set!/immediate param obj val)
  (match param
    ((annotation . idx)
     (restrict! obj (annotation->mutable-type annotation) (1+ idx) +inf.0))))

(define-type-inferrer/param (word-ref param obj idx result)
  (restrict! obj (annotation->type param)
             (1+ (&min/0 idx)) (target-max-size-t/scm))
  (define! result &u64 0 &u64-max))

(define-type-inferrer/param (word-ref/immediate param obj result)
  (match param
    ((annotation . idx)
     (restrict! obj (annotation->type annotation) (1+ idx) +inf.0)
     (define! result &u64 0 &u64-max))))

(define-type-inferrer/param (word-set! param obj idx word)
  (restrict! obj (annotation->mutable-type param) (1+ (&min/0 idx)) +inf.0))

(define-type-inferrer/param (word-set!/immediate param obj word)
  (match param
    ((annotation . idx)
     (restrict! obj (annotation->mutable-type annotation) (1+ idx) +inf.0))))

(define-type-inferrer/param (pointer-ref/immediate param obj result)
  (define! result &other-heap-object -inf.0 +inf.0))
(define-type-inferrer/param (tail-pointer-ref/immediate param obj result)
  (define! result &other-heap-object -inf.0 +inf.0))

(define-type-inferrer/param (assume-u64 param val result)
  (match param
    ((lo . hi)
     (define! result &u64 (max lo (&min val)) (min hi (&max val))))))
(define-type-inferrer/param (assume-s64 param val result)
  (match param
    ((lo . hi)
     (define! result &s64 (max lo (&min val)) (min hi (&max val))))))




;;;
;;; Fluids.  Note that we can't track bound-ness of fluids, as pop-fluid
;;; can change boundness.
;;;

(define-simple-types
  ((fluid-ref (&fluid 1)) &all-types)
  ((fluid-set! (&fluid 0 1) &all-types))
  ((push-fluid (&fluid 0 1) &all-types))
  ((pop-fluid))
  ((push-dynamic-state &all-types))
  ((pop-dynamic-state)))




;;;
;;; Symbols and keywords
;;;
(define-simple-types
  ((symbol->keyword &symbol) &keyword)
  ((keyword->symbol &keyword) &symbol)
  ((symbol->string &symbol) &string)
  ((string->symbol &string) &symbol)
  ((string-utf8-length &string) &u64)
  ((utf8->string &bytevector) &string))




;;;
;;;  We don't currently track threads as an object type.
;;;

(define-simple-types
  ((current-thread) &all-types))




;;;
;;; Strings.
;;;

(define-simple-type (number->string &number) (&string 0 (target-max-size-t)))
(define-simple-type (string->number (&string 0 (target-max-size-t)))
  ((logior &number &special-immediate) -inf.0 +inf.0))




;;;
;;; Unboxed numbers.
;;;

(define-type-checker (scm->f64 scm)
  (check-type scm &real -inf.0 +inf.0))
(define-type-inferrer (scm->f64 scm result)
  (restrict! scm &real -inf.0 +inf.0)
  (define! result &f64 (&min scm) (&max scm)))
(define-type-inferrer/param (load-f64 param result)
  (define! result &f64 param param))

(define-type-checker (inexact scm)
  (check-type scm &number -inf.0 +inf.0))
(define-type-inferrer (inexact scm result)
  (restrict! scm &number -inf.0 +inf.0)
  (let* ((in (logand (&type &number)))
         (out (if (type<=? in &real)
                  &flonum
                  (logior &flonum &complex))))
    (define! result out (&min scm) (&max scm))))

(define-type-checker (s64->f64 s64) #t)
(define-type-inferrer (s64->f64 s64 result)
  (define! result &f64 (&min s64) (&max s64)))

(define-type-checker (f64->scm f64)
  #t)
(define-type-inferrer (f64->scm f64 result)
  (define! result &flonum (&min f64) (&max f64)))

(define-type-checker (scm->u64 scm)
  (check-type scm &exact-integer 0 &u64-max))
(define-type-inferrer (scm->u64 scm result)
  (restrict! scm &exact-integer 0 &u64-max)
  (define! result &u64 (&min/0 scm) (&max/u64 scm)))
(define-type-inferrer/param (load-u64 param result)
  (define! result &u64 param param))

(define-type-checker (scm->u64/truncate scm)
  (check-type scm &exact-integer &range-min &range-max))
(define-type-inferrer (scm->u64/truncate scm result)
  (restrict! scm &exact-integer &range-min &range-max)
  (define! result &u64 0 &u64-max))

(define-type-checker (u64->scm u64)
  #t)
(define-type-inferrer (u64->scm u64 result)
  (define-exact-integer! result (&min/0 u64) (&max/u64 u64)))
(define-type-aliases u64->scm u64->scm/unlikely)

(define-type-checker (scm->s64 scm)
  (check-type scm &exact-integer &s64-min &s64-max))
(define-type-inferrer (scm->s64 scm result)
  (restrict! scm &exact-integer &s64-min &s64-max)
  (define! result &s64 (&min/s64 scm) (&max/s64 scm)))
(define-type-aliases s64->scm s64->scm/unlikely)
(define-type-inferrer/param (load-s64 param result)
  (define! result &s64 param param))

(define-type-inferrer (untag-fixnum scm result)
  (define! result &s64 (&min/fixnum scm) (&max/fixnum scm)))

(define-type-inferrer (tag-fixnum s64 result)
  (define! result &fixnum (&min/fixnum s64) (&max/fixnum s64)))
(define-type-aliases tag-fixnum tag-fixnum/unlikely)




;;;
;;; Pointers
;;;

(define-syntax-rule (define-pointer-ref-inferrer ref type lo hi)
  (define-type-inferrer (ref obj bv idx result)
    (define! result type lo hi)))
(define-pointer-ref-inferrer u8-ref  &u64 0 #xff)
(define-pointer-ref-inferrer u16-ref &u64 0 #xffff)
(define-pointer-ref-inferrer u32-ref &u64 0 #xffffffff)
(define-pointer-ref-inferrer u64-ref &u64 0 &u64-max)

(define-pointer-ref-inferrer s8-ref  &s64 (- #x80) #x7f)
(define-pointer-ref-inferrer s16-ref &s64 (- #x8000) #x7fff)
(define-pointer-ref-inferrer s32-ref &s64 (- #x80000000) #x7fffffff)
(define-pointer-ref-inferrer s64-ref &s64 &s64-min &s64-max)

(define-pointer-ref-inferrer f32-ref &f64 -inf.0 +inf.0)
(define-pointer-ref-inferrer f64-ref &f64 -inf.0 +inf.0)



;;;
;;; Numbers.
;;;

(define-syntax-rule (infer-= a b true?)
  (when true?
    (let ((min (max (&min a) (&min b)))
          (max (min (&max a) (&max b))))
      (restrict! a &all-types min max)
      (restrict! b &all-types min max))))

(define-syntax-rule (infer-integer-< a b true?)
  (let ((min0 (&min a)) (max0 (&max a))
        (min1 (&min b)) (max1 (&max b)))
    (cond
     (true?
      (restrict! a &all-types min0 (min max0 (1- max1)))
      (restrict! b &all-types (max (1+ min0) min1) max1))
     (else
      (restrict! a &all-types (max min0 min1) max0)
      (restrict! b &all-types min1 (min max0 max1))))))

(define-simple-type-checker (= &number &number))
(define-predicate-inferrer (= a b true?)
  (let ((types (logior (&type a) (&type b))))
    (when (type<=? types &number)
      ;; OK if e.g. A is a NaN; in that case the range will be
      ;; -inf/+inf.
      (infer-= a b true?))))

(define-simple-type-checker (< &real &real))
(define-predicate-inferrer (< a b true?)
  (let ((types (logior (&type a) (&type b))))
    (cond
     ((type<=? types &exact-integer)
      (cond
       ((and (eqv? (&type a) &bignum) (eqv? (&type b) &fixnum))
        (if true?
            (restrict! a &bignum -inf.0 (1- (target-most-negative-fixnum)))
            (restrict! a &bignum (1+ (target-most-positive-fixnum)) +inf.0)))
       ((and (eqv? (&type a) &fixnum) (eqv? (&type b) &bignum))
        (if true?
            (restrict! b &bignum (1+ (target-most-positive-fixnum)) +inf.0)
            (restrict! b &bignum -inf.0 (1- (target-most-negative-fixnum)))))
       (else
        (infer-integer-< a b true?))))
     ;; Can't include &flonum because of NaN.  Perhaps we should model
     ;; NaN with a separate type bit.
     ((type<=? types &exact-number)
      (let ((min0 (&min a)) (max0 (&max a))
            (min1 (&min b)) (max1 (&max b)))
        (cond
         (true?
          (restrict! a &exact-number min0 (min max0 max1))
          (restrict! b &exact-number (max min0 min1) max1))
         (else
          (restrict! a &exact-number (max min0 min1) max0)
          (restrict! b &exact-number min1 (min max0 max1)))))))))

(define (infer-<= types succ param a b)
  ;; Infer "(<= a b)" as "(not (< b a))", knowing that we only make
  ;; inferences when NaN is impossible.
  ((hashq-ref *type-inferrers* '<) types (match succ (0 1) (1 0)) param b a))
(hashq-set! *type-inferrers* '<= infer-<=)

(define-predicate-inferrer (u64-= a b true?)
  (infer-= a b true?))
(define-predicate-inferrer (u64-< a b true?)
  (infer-integer-< a b true?))

(define-predicate-inferrer (s64-= a b true?)
  (infer-= a b true?))
(define-predicate-inferrer (s64-< a b true?)
  (infer-integer-< a b true?))

(define-predicate-inferrer/param (u64-imm-= b a true?)
  (when true?
    (restrict! a &u64 (max (&min a) b) (min (&max a) b))))
(define-predicate-inferrer/param (u64-imm-< b a true?)
  (if true?
      (restrict! a &u64 (&min a) (min (&max a) (1- b)))
      (restrict! a &u64 (max (&min a) b) (&max a))))
(define-predicate-inferrer/param (imm-u64-< b a true?)
  (if true?
      (restrict! a &u64 (max (&min a) (1+ b)) (&max a))
      (restrict! a &u64 (&min a) (min (&max a) b))))

(define-predicate-inferrer/param (s64-imm-= b a true?)
  (when true?
    (restrict! a &s64 (max (&min a) b) (min (&max a) b))))
(define-predicate-inferrer/param (s64-imm-< b a true?)
  (if true?
      (restrict! a &s64 (&min a) (min (&max a) (1- b)))
      (restrict! a &s64 (max (&min a) b) (&max a))))
(define-predicate-inferrer/param (imm-s64-< b a true?)
  (if true?
      (restrict! a &s64 (max (&min a) (1+ b)) (&max a))
      (restrict! a &s64 (&min a) (min (&max a) b))))


;; Unfortunately, we can't define f64 comparison inferrers because of
;; not-a-number values.

;; Arithmetic.
(define-syntax-rule (define-binary-result! a-type$ b-type$ result closed?
                      min$ max$)
  (let* ((min min$) (max max$) (a-type a-type$) (b-type b-type$)
         (type (logior a-type b-type)))
    (cond
     ((not (type<=? type &number))
      ;; One input not a number.  Perhaps we end up dispatching to
      ;; GOOPS.
      (define! result &all-types -inf.0 +inf.0))
     ;; Complex numbers are contagious.
     ((or (eqv? a-type &complex) (eqv? b-type &complex))
      (define! result &complex -inf.0 +inf.0))
     ((or (eqv? a-type &flonum) (eqv? b-type &flonum))
      ;; If one argument is a flonum, the result will be flonum or
      ;; possibly complex.
      (let ((result-type (logand type (logior &complex &flonum))))
        (define! result result-type min max)))
     ;; Exact integers are closed under some operations.
     ((and closed? (type<=? type &exact-integer))
      (define-exact-integer! result min max))
     (else
      (let* (;; Fractions may become integers.
             (type (if (zero? (logand type &fraction))
                       type
                       (logior type &exact-integer)))
             ;; Integers may become fractions under division.
             (type (if (or closed? (zero? (logand type &exact-integer)))
                       type
                       (logior type &fraction)))
             ;; Fixnums and bignums may become each other, depending on
             ;; the range.
             (type (cond
                    ((zero? (logand type &exact-integer))
                     type)
                    ((<= (target-most-negative-fixnum)
                         min max
                         (target-most-positive-fixnum))
                     (logand type (lognot &bignum)))
                    ((or (< max (target-most-negative-fixnum))
                         (> min (target-most-positive-fixnum)))
                     (logand type (lognot &fixnum)))
                    (else
                     (logior type &fixnum &bignum)))))
        (define! result type min max))))))

(define-simple-type-checker (add &number &number))
(define-simple-type-checker (add/immediate &number))
(define-type-inferrer (add a b result)
  (define-binary-result! (&type a) (&type b) result #t
                         (+ (&min a) (&min b))
                         (+ (&max a) (&max b))))
(define-type-inferrer/param (add/immediate param a result)
  (let ((b-type (type-entry-type (constant-type-entry param))))
    (define-binary-result! (&type a) b-type result #t
      (+ (&min a) param)
      (+ (&max a) param))))
(define-type-inferrer (fadd a b result)
  (define! result &f64
    (+ (&min a) (&min b))
    (+ (&max a) (&max b))))
(define-type-inferrer (uadd a b result)
  ;; Handle wraparound.
  (let ((max (+ (&max/u64 a) (&max/u64 b))))
    (if (<= max &u64-max)
        (define! result &u64 (+ (&min/0 a) (&min/0 b)) max)
        (define! result &u64 0 &u64-max))))
(define-type-inferrer (sadd a b result)
  ;; Handle wraparound.
  (let ((min (+ (&min/s64 a) (&min/s64 b)))
        (max (+ (&max/s64 a) (&max/s64 b))))
    (if (<= &s64-min min max &s64-max)
        (define! result &s64 min max)
        (define! result &s64 &s64-min &s64-max))))
(define-type-inferrer/param (uadd/immediate param a result)
  ;; Handle wraparound.
  (let ((max (+ (&max/u64 a) param)))
    (if (<= max &u64-max)
        (define! result &u64 (+ (&min/0 a) param) max)
        (define! result &u64 0 &u64-max))))
(define-type-inferrer/param (sadd/immediate param a result)
  ;; Handle wraparound.
  (let ((min (+ (&min/s64 a) param))
        (max (+ (&max/s64 a) param)))
    (if (<= &s64-min min max &s64-max)
        (define! result &s64 min max)
        (define! result &s64 &s64-min &s64-max))))

(define-simple-type-checker (sub &number &number))
(define-simple-type-checker (sub/immediate &number))
(define-type-checker (fsub a b) #t)
(define-type-checker (usub a b) #t)
(define-type-inferrer (sub a b result)
  (define-binary-result! (&type a) (&type b) result #t
                         (- (&min a) (&max b))
                         (- (&max a) (&min b))))
(define-type-inferrer/param (sub/immediate param a result)
  (let ((b-type (type-entry-type (constant-type-entry param))))
    (define-binary-result! (&type a) b-type result #t
      (- (&min a) param)
      (- (&max a) param))))
(define-type-inferrer (fsub a b result)
  (define! result &f64
    (- (&min a) (&max b))
    (- (&max a) (&min b))))
(define-type-inferrer (usub a b result)
  ;; Handle wraparound.
  (let ((min (- (&min/0 a) (&max/u64 b))))
    (if (< min 0)
        (define! result &u64 0 &u64-max)
        (define! result &u64 min (- (&max/u64 a) (&min/0 b))))))
(define-type-inferrer/param (usub/immediate param a result)
  ;; Handle wraparound.
  (let ((min (- (&min/0 a) param)))
    (if (< min 0)
        (define! result &u64 0 &u64-max)
        (define! result &u64 min (- (&max/u64 a) param)))))

(define-simple-type-checker (mul &number &number))
(define (mul-result-range same? nan-impossible? min-a max-a min-b max-b)
  (define (nan* a b)
    (if (and (or (and (inf? a) (zero? b))
                 (and (zero? a) (inf? b)))
             nan-impossible?)
        0 
        (* a b)))
  (let ((-- (nan* min-a min-b))
        (-+ (nan* min-a max-b))
        (++ (nan* max-a max-b))
        (+- (nan* max-a min-b)))
    (let ((has-nan? (or (nan? --) (nan? -+) (nan? ++) (nan? +-))))
      (values (cond
               (same? 0)
               (has-nan? -inf.0)
               (else (min -- -+ ++ +-)))
              (if has-nan?
                  +inf.0
                  (max -- -+ ++ +-))))))
(define-type-inferrer (mul a b result)
  (let ((min-a (&min a)) (max-a (&max a))
        (min-b (&min b)) (max-b (&max b))
        ;; We only really get +inf.0 at runtime for flonums and
        ;; compnums.  If we have inferred that the arguments are not
        ;; flonums and not compnums, then the result of (* +inf.0 0) at
        ;; range inference time is 0 and not +nan.0.
        (nan-impossible? (not (logtest (logior (&type a) (&type b))
                                       (logior &flonum &complex)))))
    (call-with-values (lambda ()
                        (mul-result-range (eqv? a b) nan-impossible?
                                          min-a max-a min-b max-b))
      (lambda (min max)
        (define-binary-result! (&type a) (&type b) result #t min max)))))
(define-type-inferrer (fmul a b result)
  (let ((min-a (&min a)) (max-a (&max a))
        (min-b (&min b)) (max-b (&max b))
        (nan-impossible? #f))
    (call-with-values (lambda ()
                        (mul-result-range (eqv? a b) nan-impossible?
                                          min-a max-a min-b max-b))
      (lambda (min max)
        (define! result &f64 min max)))))
(define-type-inferrer (umul a b result)
  ;; Handle wraparound.
  (let ((max (* (&max/u64 a) (&max/u64 b))))
    (if (<= max &u64-max)
        (define! result &u64 (* (&min/0 a) (&min/0 b)) max)
        (define! result &u64 0 &u64-max))))
(define-type-inferrer (smul a b result)
  (call-with-values (lambda ()
                      (mul-result-range (eqv? a b) #t
                                        (&min/s64 a) (&max/s64 a)
                                        (&min/s64 b) (&max/s64 b)))
    (lambda (min max)
      ;; Handle wraparound.
      (if (<= &s64-min min max &s64-max)
          (define! result &s64 min max)
          (define! result &s64 &s64-min &s64-max)))))
(define-type-inferrer/param (umul/immediate param a result)
  ;; Handle wraparound.
  (let ((max (* (&max/u64 a) param)))
    (if (<= max &u64-max)
        (define! result &u64 (* (&min/0 a) param) max)
        (define! result &u64 0 &u64-max))))
(define-type-inferrer/param (smul/immediate param a result)
  (call-with-values (lambda ()
                      (mul-result-range #f #t
                                        (&min/s64 a) (&max/s64 a)
                                        param param))
    (lambda (min max)
      ;; Handle wraparound.
      (if (<= &s64-min min max &s64-max)
          (define! result &s64 min max)
          (define! result &s64 &s64-min &s64-max)))))

(define-type-checker (div a b)
  (and (check-type a &number -inf.0 +inf.0)
       (check-type b &number -inf.0 +inf.0)
       ;; We only know that there will not be an exception if b is not
       ;; zero.
       (not (<= (&min b) 0 (&max b)))))
(define-type-checker (fdiv a b) #t)
(define (div-result-range min-a max-a min-b max-b)
  (if (or (<= min-b 0 max-b)
          (< max-b min-b))
      ;; If the range of the divisor crosses 0, or if we are in
      ;; unreachable code, the result spans the whole range.
      (values -inf.0 +inf.0)
      ;; Otherwise min-b and max-b have the same sign, and cannot both
      ;; be infinity.
      (let ((--- (if (inf? min-b) 0 (floor/ min-a min-b)))
            (-+- (if (inf? max-b) 0 (floor/ min-a max-b)))
            (++- (if (inf? max-b) 0 (floor/ max-a max-b)))
            (+-- (if (inf? min-b) 0 (floor/ max-a min-b)))
            (--+ (if (inf? min-b) 0 (ceiling/ min-a min-b)))
            (-++ (if (inf? max-b) 0 (ceiling/ min-a max-b)))
            (+++ (if (inf? max-b) 0 (ceiling/ max-a max-b)))
            (+-+ (if (inf? min-b) 0 (ceiling/ max-a min-b))))
        (values (min (min --- -+- ++- +--)
                     (min --+ -++ +++ +-+))
                (max (max --- -+- ++- +--)
                     (max --+ -++ +++ +-+))))))
(define-type-inferrer (div a b result)
  (let ((min-a (&min a)) (max-a (&max a))
        (min-b (&min b)) (max-b (&max b)))
    (call-with-values (lambda ()
                        (div-result-range min-a max-a min-b max-b))
      (lambda (min max)
        (define-binary-result! (&type a) (&type b) result #f min max)))))
(define-type-inferrer (fdiv a b result)
  (let ((min-a (&min a)) (max-a (&max a))
        (min-b (&min b)) (max-b (&max b)))
    (call-with-values (lambda ()
                        (div-result-range min-a max-a min-b max-b))
      (lambda (min max)
        (define! result &f64 min max)))))

(define &integer (logior &exact-integer &flonum))

(define-type-checker (quo a b)
  (and (check-type a &integer -inf.0 +inf.0)
       (check-type b &integer -inf.0 +inf.0)
       ;; We only know that there will not be an exception if b is not
       ;; zero.
       (not (<= (&min b) 0 (&max b)))))
(define-type-inferrer (quo a b result)
  (restrict! a &integer -inf.0 +inf.0)
  (restrict! b &integer -inf.0 +inf.0)
  (define! result (logand (logior (&type a) (&type b)) &integer)
    -inf.0 +inf.0))

(define-type-checker-aliases quo rem)
(define-type-inferrer (rem a b result)
  (restrict! a &integer -inf.0 +inf.0)
  (restrict! b &integer -inf.0 +inf.0)
  ;; Same sign as A.
  (let* ((max-abs-rem (1- (max (abs (&min b)) (abs (&max b)))))
         (t (logand (logior (&type a) (&type b)) &integer))
         (min-rem (if (< (&min a) 0) (- max-abs-rem) 0))
         (max-rem (if (< 0 (&max a)) max-abs-rem 0)))
    (if (type<=? t &exact-integer)
        (define-exact-integer! result min-rem max-rem)
        (define! result t min-rem max-rem))))

(define-type-checker-aliases quo mod)
(define-type-inferrer (mod a b result)
  (restrict! a &integer -inf.0 +inf.0)
  (restrict! b &integer -inf.0 +inf.0)
  ;; Same sign as B.
  (let* ((max-abs-mod (1- (max (abs (&min b)) (abs (&max b)))))
         (t (logand (logior (&type a) (&type b)) &integer))
         (min-mod (if (< (&min b) 0) (- max-abs-mod) 0))
         (max-mod (if (< 0 (&max b)) max-abs-mod 0)))
    (if (type<=? t &exact-integer)
        (define-exact-integer! result min-mod max-mod)
        (define! result t min-mod max-mod))))

;; Predicates.
(define-syntax-rule (define-type-predicate-result val result type)
  (cond
   ((zero? (logand (&type val) type))
    (define! result &special-immediate &false &false))
   ((zero? (logand (&type val) (lognot type)))
    (define! result &special-immediate &true &true))
   (else
    (define! result &special-immediate &false &true))))

(define-simple-type-checker (inf? &real))
(define-type-inferrer (inf? val result)
  (restrict! val &real -inf.0 +inf.0)
  (cond
   ((or (zero? (logand (&type val) (logior &flonum &complex)))
        (and (not (inf? (&min val))) (not (inf? (&max val)))))
    (define! result &special-immediate &false &false))
   (else
    (define! result &special-immediate &false &true))))

(define-type-aliases inf? nan?)

(define-simple-type (even? &integer)
  (&special-immediate &false &true))
(define-type-aliases even? odd?)

;; Bit operations.
(define-simple-type-checker (lsh &exact-integer &u64))
(define-simple-type-checker (rsh &exact-integer &u64))
(define (compute-ash-range min-val max-val min-shift max-shift)
  (define (ash* val count)
    ;; As we only precisely represent a 64-bit range, don't bother inferring
    ;; shifts that might exceed that range.
    (cond
     ((inf? val) val) ; Preserves sign.
     ((< count 64) (ash val count))
     ((zero? val) 0)
     ((positive? val) +inf.0)
     (else -inf.0)))
  (let ((-- (ash* min-val min-shift))
        (-+ (ash* min-val max-shift))
        (++ (ash* max-val max-shift))
        (+- (ash* max-val min-shift)))
    (values (min -- -+ ++ +-) (max -- -+ ++ +-))))
(define-type-inferrer (lsh val count result)
  (restrict! val &exact-integer -inf.0 +inf.0)
  (let-values (((min max) (compute-ash-range (&min val)
                                             (&max val)
                                             (&min/0 count)
                                             (&max/u64 count))))
    (define-exact-integer! result min max)))
(define-simple-type-checker (lsh/immediate &exact-integer))
(define-type-inferrer/param (lsh/immediate count val result)
  (restrict! val &exact-integer -inf.0 +inf.0)
  (let-values (((min max) (compute-ash-range (&min val)
                                             (&max val)
                                             count count)))
    (define-exact-integer! result min max)))
(define-type-inferrer (rsh val count result)
  (restrict! val &exact-integer -inf.0 +inf.0)
  (let-values (((min max) (compute-ash-range (&min val)
                                             (&max val)
                                             (- (&min/0 count))
                                             (- (&max/u64 count)))))
    (define-exact-integer! result min max)))
(define-simple-type-checker (rsh/immediate &exact-integer))
(define-type-inferrer/param (rsh/immediate count val result)
  (restrict! val &exact-integer -inf.0 +inf.0)
  (let-values (((min max) (compute-ash-range (&min val)
                                             (&max val)
                                             (- count) (- count))))
    (define-exact-integer! result min max)))

(define-type-inferrer (ursh a b result)
  (define! result &u64
    (ash (&min/0 a) (- (min 63 (&max/u64 b))))
    (ash (&max/u64 a) (- (min 63 (&min/0 b))))))
(define-type-inferrer/param (ursh/immediate param a result)
  (define! result &u64
    (ash (&min/0 a) (- param))
    (ash (&max/u64 a) (- param))))

(define-type-inferrer (srsh a b result)
  (let-values (((min max) (compute-ash-range (&min/s64 a)
                                             (&max/s64 a)
                                             (- (min 63 (&min/0 b)))
                                             (- (min 63 (&max/u64 b))))))
    (if (<= &s64-min min max &s64-max)
        (define! result &s64 min max)
        (define! result &s64 &s64-min &s64-max))))
(define-type-inferrer/param (srsh/immediate count val result)
  (let-values (((min max) (compute-ash-range (&min/s64 val)
                                             (&max/s64 val)
                                             (- count) (- count))))
    (if (<= &s64-min min max &s64-max)
        (define! result &s64 min max)
        (define! result &s64 &s64-min &s64-max))))

(define-type-inferrer (ulsh a b result)
  (if (and
       (or (zero? (&max/u64 a)) (< (&max/u64 b) 64)) ; don't even try
       (<= (ash (&max/u64 a) (&max/u64 b)) &u64-max))
      ;; No overflow; we can be precise.
      (define! result &u64
        (ash (&min/0 a) (&min/0 b))
        (ash (&max/u64 a) (&max/u64 b)))
      ;; Otherwise assume the whole range.
      (define! result &u64 0 &u64-max)))
(define-type-inferrer/param (ulsh/immediate param a result)
  (if (<= (ash (&max/u64 a) param) &u64-max)
      ;; No overflow; we can be precise.
      (define! result &u64
        (ash (&min/0 a) param)
        (ash (&max/u64 a) param))
      ;; Otherwise assume the whole range.
      (define! result &u64 0 &u64-max)))

(define-type-inferrer (slsh a b result)
  (let-values (((min max) (compute-ash-range (&min a) (&max a)
                                             (min 63 (&min/0 b))
                                             (min 63 (&max/u64 b)))))
    (if (<= &s64-min min max &s64-max)
        (define! result &s64 min max)
        (define! result &s64 &s64-min &s64-max))))
(define-type-inferrer/param (slsh/immediate param a result)
  (let-values (((min max) (compute-ash-range (&min a) (&max a)
                                             param param)))
    (if (<= &s64-min min max &s64-max)
        (define! result &s64 min max)
        (define! result &s64 &s64-min &s64-max))))

(define-inlinable (non-negative? n)
  "Return true if N is non-negative, otherwise return false."
  (not (negative? n)))

;; Like 'lognot', but handles infinities.
(define-inlinable (lognot* n)
  "Return the bitwise complement of N.  If N is infinite, return -N."
  (- -1 n))

(define saturate+
  (case-lambda
    "Let N be the least upper bound of the integer lengths of the
arguments.  Return the greatest integer whose integer length is N.
If any of the arguments are infinite, return positive infinity."
    ((a b)
     (if (or (inf? a) (inf? b))
         +inf.0
         (1- (ash 1 (max (integer-length a)
                         (integer-length b))))))
    ((a b c)
     (saturate+ (saturate+ a b) c))
    ((a b c d)
     (saturate+ (saturate+ a b) c d))))

(define saturate-
  (case-lambda
    "Let N be the least upper bound of the integer lengths of the
arguments.  Return the least integer whose integer length is N.
If any of the arguments are infinite, return negative infinity."
    ((a b)     (lognot* (saturate+ a b)))
    ((a b c)   (lognot* (saturate+ a b c)))
    ((a b c d) (lognot* (saturate+ a b c d)))))

(define (logand-bounds a0 a1 b0 b1)
  "Return two values: lower and upper bounds for (logand A B)
where (A0 <= A <= A1) and (B0 <= B <= B1)."
  ;; For each argument, we consider three cases: (1) the argument is
  ;; non-negative, (2) its sign is unknown, or (3) it is negative.
  ;; To handle both arguments, we must consider a total of 9 cases:
  ;;
  ;; -----------------------------------------------------------------------
  ;;    LOGAND      | non-negative B   | unknown-sign B | negative B
  ;; -----------------------------------------------------------------------
  ;; non-negative A | 0 .. (min A1 B1) | 0 .. A1        | 0 .. A1
  ;; -----------------------------------------------------------------------
  ;; unknown-sign A | 0 .. B1          | (sat- A0 B0)   | (sat- A0 B0)
  ;;                |                  |      ..        |    .. A1
  ;;                |                  | (sat+ A1 B1)   |
  ;; -----------------------------------------------------------------------
  ;;     negative A | 0 .. B1          | (sat- A0 B0)   | (sat- A0 B0)
  ;;                |                  |    .. B1       |    .. (min A1 B1)
  ;; -----------------------------------------------------------------------
  (values (if (or (non-negative? a0) (non-negative? b0))
              0
              (saturate- a0 b0))
          (cond ((or (and (non-negative? a0) (non-negative? b0))
                     (and (negative? a1) (negative? b1)))
                 (min a1 b1))
                ((or (non-negative? a0) (negative? b1))
                 a1)
                ((or (non-negative? b0) (negative? a1))
                 b1)
                (else
                 (saturate+ a1 b1)))))

(define-simple-type-checker (logand &exact-integer &exact-integer))
(define-type-inferrer (logand a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  (call-with-values (lambda ()
                      (logand-bounds (&min a) (&max a) (&min b) (&max b)))
    (lambda (min max)
      (define-exact-integer! result min max))))

(define-simple-type-checker (logand/immediate &exact-integer))
(define-type-inferrer/param (logand/immediate param a result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (call-with-values (lambda ()
                      (logand-bounds (&min a) (&max a) param param))
    (lambda (min max)
      (define-exact-integer! result min max))))

(define-type-inferrer (ulogand a b result)
  (restrict! a &u64 0 &u64-max)
  (restrict! b &u64 0 &u64-max)
  (define! result &u64 0 (min (&max/u64 a) (&max/u64 b))))
(define-type-inferrer/param (ulogand/immediate param a result)
  (restrict! a &u64 0 &u64-max)
  (call-with-values (lambda ()
                      (logand-bounds (&min a) (&max a) param param))
    (lambda (min max)
      (define! result &u64 min max))))

(define (logsub-bounds a0 a1 b0 b1)
  "Return two values: lower and upper bounds for (logsub A B),
i.e. (logand A (lognot B)), where (A0 <= A <= A1) and (B0 <= B <= B1)."
  ;; Here we use 'logand-bounds' to compute the bounds, after
  ;; computing the bounds of (lognot B) from the bounds of B.
  ;; From (B0 <= B <= B1) it follows that (~B1 <= ~B <= ~B0),
  ;; where ~X means (lognot X).
  (logand-bounds a0 a1 (lognot* b1) (lognot* b0)))

(define-simple-type-checker (logsub &exact-integer &exact-integer))
(define-type-inferrer (logsub a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  (call-with-values (lambda ()
                      (logsub-bounds (&min a) (&max a) (&min b) (&max b)))
    (lambda (min max)
      (define-exact-integer! result min max))))

(define-type-inferrer (ulogsub a b result)
  (restrict! a &u64 0 &u64-max)
  (restrict! b &u64 0 &u64-max)
  (define! result &u64 0 (&max/u64 a)))

(define (logior-bounds a0 a1 b0 b1)
  "Return two values: lower and upper bounds for (logior A B)
where (A0 <= A <= A1) and (B0 <= B <= B1)."
  ;; For each argument, we consider three cases: (1) the argument is
  ;; non-negative, (2) its sign is unknown, or (3) it is negative.
  ;; To handle both arguments, we must consider a total of 9 cases.
  ;;
  ;; ---------------------------------------------------------------------
  ;;    LOGIOR      | non-negative B | unknown-sign B | negative B
  ;; ---------------------------------------------------------------------
  ;; non-negative A | (max A0 B0)    | B0             | B0 .. -1
  ;;                |   ..           |   ..           |
  ;;                | (sat+ A1 B1)   | (sat+ A1 B1)   |
  ;; ---------------------------------------------------------------------
  ;; unknown-sign A | A0             | (sat- A0 B0)   | B0 .. -1
  ;;                |   ..           |        ..      |
  ;;                | (sat+ A1 B1)   | (sat+ A1 B1)   |
  ;; ---------------------------------------------------------------------
  ;;     negative A | A0 .. -1       | A0 .. -1       | (max A0 B0) .. -1
  ;; ---------------------------------------------------------------------
  (values (cond ((or (and (non-negative? a0) (non-negative? b0))
                     (and (negative? a1) (negative? b1)))
                 (max a0 b0))
                ((or (non-negative? a0) (negative? b1))
                 b0)
                ((or (non-negative? b0) (negative? a1))
                 a0)
                (else
                 (saturate- a0 b0)))
          (if (or (negative? a1) (negative? b1))
              -1
              (saturate+ a1 b1))))

(define-simple-type-checker (logior &exact-integer &exact-integer))
(define-type-inferrer (logior a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  (call-with-values (lambda ()
                      (logior-bounds (&min a) (&max a) (&min b) (&max b)))
    (lambda (min max)
      (define-exact-integer! result min max))))

(define-type-inferrer (ulogior a b result)
  (restrict! a &u64 0 &u64-max)
  (restrict! b &u64 0 &u64-max)
  (define! result &u64
    (max (&min/0 a) (&min/0 b))
    (saturate+ (&max/u64 a) (&max/u64 b))))

(define (logxor-bounds a0 a1 b0 b1)
  "Return two values: lower and upper bounds for (logxor A B)
where (A0 <= A <= A1) and (B0 <= B <= B1)."
  ;; For each argument, we consider three cases: (1) the argument is
  ;; non-negative, (2) its sign is unknown, or (3) it is negative.
  ;; To handle both arguments, we must consider a total of 9 cases.
  ;;
  ;; --------------------------------------------------------------------
  ;;    LOGXOR      | non-negative B | unknown-sign B     | negative B
  ;; --------------------------------------------------------------------
  ;; non-negative A | 0              |       (sat- A1 B0) | (sat- A1 B0)
  ;;                |   ..           |         ..         |   ..
  ;;                | (sat+ A1 B1)   | (sat+ A1 B1)       |     -1
  ;; --------------------------------------------------------------------
  ;; unknown-sign A | (sat- A0 B1)   | (sat- A0 B1 A1 B0) | (sat- A1 B0)
  ;;                |   ..           |   ..               |   ..
  ;;                | (sat+ A1 B1)   | (sat+ A1 B1 A0 B0) | (sat+ A0 B0)
  ;; --------------------------------------------------------------------
  ;;     negative A | (sat- A0 B1)   | (sat- A0 B1)       | 0
  ;;                |   ..           |    ..              |   ..
  ;;                |     -1         |       (sat+ A0 B0) | (sat+ A0 B0)
  ;; --------------------------------------------------------------------
  (values (cond ((or (and (non-negative? a0) (non-negative? b0))
                     (and (negative? a1) (negative? b1)))
                 0)
                ((or (non-negative? a0) (negative? b1))
                 (saturate- a1 b0))
                ((or (non-negative? b0) (negative? a1))
                 (saturate- a0 b1))
                (else
                 (saturate- a0 b1 a1 b0)))
          (cond ((or (and (non-negative? a0) (negative? b1))
                     (and (non-negative? b0) (negative? a1)))
                 -1)
                ((or (non-negative? a0) (non-negative? b0))
                 (saturate+ a1 b1))
                ((or (negative? a1) (negative? b1))
                 (saturate+ a0 b0))
                (else
                 (saturate+ a1 b1 a0 b0)))))

(define-simple-type-checker (logxor &exact-integer &exact-integer))
(define-type-inferrer (logxor a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  (call-with-values (lambda ()
                      (logxor-bounds (&min a) (&max a) (&min b) (&max b)))
    (lambda (min max)
      (define! result &exact-integer min max))))

(define-type-inferrer (ulogxor a b result)
  (restrict! a &u64 0 &u64-max)
  (restrict! b &u64 0 &u64-max)
  (define! result &u64 0 (saturate+ (&max/u64 a) (&max/u64 b))))

(define-simple-type-checker (lognot &exact-integer))
(define-type-inferrer (lognot a result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (define-exact-integer! result
    (lognot* (&max a))
    (lognot* (&min a))))

(define-simple-type-checker (logtest &exact-integer &exact-integer))
(define-type-inferrer (logtest a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  (define! result &special-immediate &false &true))

(define-simple-type-checker (logbit? (&exact-integer 0 +inf.0) &exact-integer))
(define-type-inferrer (logbit? a b result)
  (let ((a-min (&min a))
        (a-max (&max a))
        (b-min (&min b))
        (b-max (&max b)))
    (if (and (eqv? a-min a-max) (>= a-min 0) (not (inf? a-min))
             (eqv? b-min b-max) (>= b-min 0) (not (inf? b-min)))
        (let ((bool (if (logbit? a-min b-min) &true &false)))
          (define! result &special-immediate bool bool))
        (define! result &special-immediate &false &true))))

;; Flonums.
(define-simple-type-checker (sqrt &number))
(define-type-inferrer (sqrt x result)
  (let ((type (&type x)))
    (cond
     ((and (zero? (logand type &complex))
           (non-negative? (&min x)))
      (define! result
               (logior type &flonum)
               (exact-integer-sqrt (&min x))
               (if (inf? (&max x))
                   +inf.0
                   (call-with-values (lambda () (exact-integer-sqrt (&max x)))
                     (lambda (s r)
                       (if (zero? r) s (+ s 1)))))))
     (else
      (define! result (logior type &flonum &complex) -inf.0 +inf.0)))))
(define-type-checker (fsqrt x) #t)
(define-type-inferrer (fsqrt x result)
  (define! result
    &f64
    (exact-integer-sqrt (max (&min x) 0))
    (if (inf? (&max x))
        +inf.0
        (call-with-values (lambda () (exact-integer-sqrt (&max x)))
          (lambda (s r)
            (if (zero? r) s (+ s 1)))))))

(define-simple-type-checker (abs &real))
(define-type-inferrer (abs x result)
  (let ((type (&type x)))
    (cond
     ((type<=? type &exact-integer)
      (if (< (&min x) 0)
          (define-exact-integer! result 0 (max (abs (&min x)) (abs (&max x))))
          (define! result type (&min x) (&max x))))
     (else
      (when (type<=? type &number)
        (restrict! x &real -inf.0 +inf.0))
      (let* ((min (if (< (&min x) 0) 0 (&min x)))
             (max (max (abs (&min x)) (abs (&max x))))
             (type (cond
                    ((not (logtest type &exact-integer)) type)
                    ((< (target-most-positive-fixnum) min)
                     (logior &bignum (logand type (lognot &fixnum))))
                    ((<= max (target-most-positive-fixnum))
                     (logior &fixnum (logand type (lognot &bignum))))
                    (else (logior type &fixnum &bignum)))))
        (define! result (logior (logand type (lognot &number))
                                (logand type &real))
          min max))))))
(define-type-checker (fabs x) #t)
(define-type-inferrer (fabs x result)
  (let ((min (if (< (&min x) 0) 0 (&min x)))
        (max (max (abs (&min x)) (abs (&max x)))))
    (define! result &f64 min max)))

(define-simple-type-checker (floor &real))
(define-type-inferrer (floor x result)
  (restrict! x &real -inf.0 +inf.0)
  (let* ((in (logand (&type x) &real))
         (out (cond
               ((type<=? in &flonum) &flonum)
               ((type<=? in &exact-integer) in)
               ((logtest in &fraction)
                (logior (logand in (lognot &fraction)) &exact-integer)))))
    (define! result out (&min x) (&max x))))
(define-type-checker (ffloor x) #t)
(define-type-inferrer (ffloor x result)
  (define! result &f64 (&min x) (&max x)))

(define-type-aliases floor ceiling)
(define-type-aliases ffloor fceiling)

(define-simple-type-checker (sin &number))
(define-type-inferrer (sin x result)
  (let* ((in (&type x))
         (out (cond
               ((type<=? in &real) &flonum)
               ((type<=? in &complex) &complex)
               (else (logior &flonum &complex (logand in (lognot &number)))))))
    (define! result out -1 1)))
(define-type-checker (fsin x) #t)
(define-type-inferrer (fsin x result)
  (define! result &f64 -1 1))

(define-type-aliases sin cos)
(define-type-aliases fsin fcos)

(define-simple-type-checker (tan &number))
(define-type-inferrer (tan x result)
  (let* ((in (&type x))
         (out (cond
               ((type<=? in &real) &flonum)
               ((type<=? in &complex) &complex)
               (else (logior &flonum &complex (logand in (lognot &number)))))))
    (define! result out -inf.0 +inf.0)))
(define-type-checker (ftan x) #t)
(define-type-inferrer (ftan x result)
  (define! result &f64 -inf.0 +inf.0))

(define-simple-type-checker (asin &number))
(define-type-inferrer (asin x result)
  (define! result
    (logior &flonum &complex (logand (&type x) (lognot &number)))
    -inf.0 +inf.0))
(define-type-checker (fasin x) #t)
(define-type-inferrer (fasin x result)
  (define! result &f64 -2 2)) ; [-pi/2, pi/2]

(define-type-aliases asin acos)
(define-type-checker (facos x) #t)
(define-type-inferrer (facos x result)
  (define! result &f64 0 4)) ; [0, pi]

(define-simple-type-checker (atan &number))
(define-type-inferrer (atan x result)
  (let ((in (&type x)))
    (cond
     ((type<=? in &real)
      (define! result &flonum -2 2)) ; [-pi/2, pi/2]
     (else
      (define! result
        (logior &flonum &complex (logand in (lognot &number)))
        -inf.0 +inf.0)))))
(define-type-checker (fatan x) #t)
(define-type-inferrer (fatan x result)
  (define! result &f64 -2 2))

(define-simple-type-checker (atan2 &number &number))
(define-type-inferrer (atan2 x y result)
  (let* ((in (logior (&type x) (&type y))))
    (cond
     ((type<=? in &real)
      (define! result &flonum -4 4)) ; [-pi, pi]
     (else
      (define! result (logior &flonum &complex (logand in (lognot &number)))
        -inf.0 +inf.0)))))
(define-type-checker (fatan2 x y) #t)
(define-type-inferrer (fatan2 x y result)
  (define! result &f64 -4 4))





;;;
;;; Characters.
;;;

(define-type-inferrer (untag-char c result)
  (define! result &s64 0 (min (&max c) *max-codepoint*)))
(define-type-inferrer (tag-char u64 result)
  (define! result &char 0 (min (&max u64) *max-codepoint*)))




;;;
;;; Type flow analysis: the meet (ahem) of the algorithm.
;;;

(define (successor-count cont)
  (match cont
    (($ $kargs _ _ ($ $throw)) 0)
    (($ $kargs _ _ ($ $continue)) 1)
    (($ $kargs _ _ (or ($ $branch) ($ $prompt))) 2)
    (($ $kargs _ _ ($ $switch kf kt*)) (1+ (length kt*)))
    (($ $kfun src meta self tail clause) (if clause 1 0))
    (($ $kclause arity body alt) (if alt 2 1))
    (($ $kreceive) 1)
    (($ $ktail) 0)))

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

(define intmap-ensure
  (let* ((*absent* (list 'absent))
         (not-found (lambda (i) *absent*)))
    (lambda (map i ensure)
      (let ((val (intmap-ref map i not-found)))
        (if (eq? val *absent*)
            (let ((val (ensure i)))
              (values (intmap-add map i val) val))
            (values map val))))))

;; For best results, the labels in the function starting should be
;; topologically sorted (renumbered).  Otherwise the backward branch
;; detection mentioned in the module commentary will trigger for
;; ordinary forward branches.
(define (infer-types conts kfun)
  "Compute types for all variables bound in the function labelled
@var{kfun}, from @var{conts}.  Returns an intmap mapping labels to type
entries.

A type entry is a vector that describes the types of the values that
flow into and out of a labelled expression.  The first slot in the type
entry vector corresponds to the types that flow in, and the rest of the
slots correspond to the types that flow out.  Each element of the type
entry vector is an intmap mapping variable name to the variable's
inferred type.  An inferred type is a 3-vector of type, minimum, and
maximum, where type is a bitset as a fixnum."
  (define (get-entry typev label) (intmap-ref typev label))
  (define (entry-not-found label)
    (make-vector (1+ (successor-count (intmap-ref conts label))) #f))
  (define (ensure-entry typev label)
    (intmap-ensure typev label entry-not-found))

  (define (compute-initial-state)
    (let ((entry (entry-not-found kfun)))
      ;; Nothing flows in to the first label.
      (vector-set! entry 0 empty-intmap)
      (intmap-add empty-intmap kfun entry)))

  (define (adjoin-vars types vars entry)
    (match vars
      (() types)
      ((var . vars)
       (adjoin-vars (adjoin-var types var entry) vars entry))))

  (define (infer-primcall types succ name param args result)
    (cond
     ((hashq-ref *type-inferrers* name)
      => (lambda (inferrer)
           ;; FIXME: remove the apply?
           ;; (pk 'primcall name args result)
           (apply inferrer types succ param
                  (if result
                      (append args (list result))
                      args))))
     (result
      (adjoin-var types result all-types-entry))
     (else
      types)))

  (define (vector-replace vec idx val)
    (let ((vec (vector-copy vec)))
      (vector-set! vec idx val)
      vec))

  (define (update-out-types label typev types succ-idx)
    (let* ((entry (get-entry typev label))
           (old-types (vector-ref entry (1+ succ-idx))))
      (if (eq? types old-types)
          (values typev #f)
          (let ((entry (vector-replace entry (1+ succ-idx) types))
                (first? (not old-types)))
            (values (intmap-replace typev label entry) first?)))))

  (define (update-in-types label typev types saturate?)
    (let*-values (((typev entry) (ensure-entry typev label))
                  ((old-types) (vector-ref entry 0))
                  ;; TODO: If the label has only one predecessor, we can
                  ;; avoid the meet.
                  ((types) (if (not old-types)
                               types
                               (let ((meet (if saturate?
                                               type-entry-saturating-union
                                               type-entry-union)))
                                 (intmap-intersect old-types types meet)))))
      (if (eq? old-types types)
          (values typev #f)
          (let ((entry (vector-replace entry 0 types)))
            (values (intmap-replace typev label entry) #t)))))

  (define (propagate-types label typev succ-idx succ-label types)
    (let*-values
        (((typev first?) (update-out-types label typev types succ-idx))
         ((saturate?) (and (not first?) (<= succ-label label)))
         ((typev changed?) (update-in-types succ-label typev types saturate?)))
      (values (if changed? (list succ-label) '()) typev)))

  (define (visit-exp label typev k types exp)
    (define (propagate1 succ-label types)
      (propagate-types label typev 0 succ-label types))
    (define (propagate2 succ0-label types0 succ1-label types1)
      (let*-values (((changed0 typev)
                     (propagate-types label typev 0 succ0-label types0))
                    ((changed1 typev)
                     (propagate-types label typev 1 succ1-label types1)))
        (values (append changed0 changed1) typev)))
    ;; Each of these branches must propagate to its successors.
    (match exp
      (($ $primcall name param args)
       (propagate1 k
                   (match (intmap-ref conts k)
                     (($ $kargs _ defs)
                      (infer-primcall types 0 name param args
                                      (match defs ((var) var) (_ #f))))
                     (_
                      ;; (pk 'warning-no-restrictions name)
                      types))))
      (($ $values args)
       (match (intmap-ref conts k)
         (($ $kargs _ defs)
          (let ((in types))
            (let lp ((defs defs) (args args) (out types))
              (match (cons defs args)
                ((() . ())
                 (propagate1 k out))
                (((def . defs) . (arg . args))
                 (lp defs args
                     (adjoin-var out def (var-type-entry in arg))))))))
         (_
          (propagate1 k types))))
      ((or ($ $call) ($ $callk) ($ $calli))
       (propagate1 k types))
      (($ $rec names vars funs)
       (let ((proc-type (make-type-entry &procedure -inf.0 +inf.0)))
         (propagate1 k (adjoin-vars types vars proc-type))))
      (_
       (match (intmap-ref conts k)
         (($ $kargs (_) (var))
          (let ((entry (match exp
                         (($ $const val)
                          (constant-type-entry val))
                         ((or ($ $prim) ($ $fun) ($ $const-fun) ($ $code))
                          ;; Could be more precise here.
                          (make-type-entry &procedure -inf.0 +inf.0)))))
            (propagate1 k (adjoin-var types var entry))))))))

  (define (visit-cont label typev)
    (let ((types (vector-ref (intmap-ref typev label) 0)))
      (define (propagate0)
        (values '() typev))
      (define (propagate1 succ-label types)
        (propagate-types label typev 0 succ-label types))
      (define (propagate2 succ0-label types0 succ1-label types1)
        (let*-values (((changed0 typev)
                       (propagate-types label typev 0 succ0-label types0))
                      ((changed1 typev)
                       (propagate-types label typev 1 succ1-label types1)))
          (values (append changed0 changed1) typev)))
      
      ;; Add types for new definitions, and restrict types of
      ;; existing variables due to side effects.
      (match (intmap-ref conts label)
        (($ $kargs names vars ($ $continue k src exp))
         (visit-exp label typev k types exp))
        (($ $kargs names vars ($ $branch kf kt src op param args))
         ;; The "normal" continuation is the #f branch.
         (propagate2 kf (infer-primcall types 0 op param args #f)
                     kt (infer-primcall types 1 op param args #f)))
        (($ $kargs names vars ($ $switch kf kt* src arg))
         (define (restrict-index min max)
           (restrict-var types arg (make-type-entry &u64 min max)))
         (define (visit-default typev)
           (let ((types (restrict-index (length kt*) &u64-max)))
             (propagate-types label typev 0 kf types)))
         (define (visit-target typev k i)
           (let ((types (restrict-index i i)))
             (propagate-types label typev (1+ i) k types)))
         (call-with-values (lambda () (visit-default typev))
           (lambda (changed typev)
             (let lp ((kt* kt*) (i 0) (changed changed) (typev typev))
               (match kt*
                 (() (values changed typev))
                 ((kt . kt*)
                  (call-with-values (lambda () (visit-target typev kt i))
                    (lambda (changed* typev)
                      (lp kt* (1+ i) (append changed* changed) typev)))))))))
        (($ $kargs names vars ($ $prompt k kh src escape? tag))
         ;; The "normal" continuation enters the prompt.
         (propagate2 k types kh types))
        (($ $kargs names vars ($ $throw))
         (propagate0))
        (($ $kreceive arity k)
         (match (intmap-ref conts k)
           (($ $kargs names vars)
            (propagate1 k (adjoin-vars types vars all-types-entry)))))
        (($ $kfun src meta self tail clause)
         (if clause
             (let ((types (if self
                              (adjoin-var types self all-types-entry)
                              types)))
               (propagate1 clause
                           (match (intmap-ref conts clause)
                             (($ $kargs _ defs)
                              (adjoin-vars types defs all-types-entry))
                             (_ types))))
             (propagate0)))
        (($ $kclause arity kbody kalt)
         (match (intmap-ref conts kbody)
           (($ $kargs _ defs)
            (let ((body-types (adjoin-vars types defs all-types-entry)))
              (if kalt
                  (propagate2 kbody body-types kalt types)
                  (propagate1 kbody body-types))))))
        (($ $ktail) (propagate0)))))

  (worklist-fold* visit-cont
                  (intset-add empty-intset kfun)
                  (compute-initial-state)))

(define (lookup-pre-type types label def)
  (let* ((entry (intmap-ref types label))
         (tentry (var-type-entry (vector-ref entry 0) def)))
    (values (type-entry-type tentry)
            (type-entry-min tentry)
            (type-entry-max tentry))))

(define (lookup-post-type types label def succ-idx)
  (let* ((entry (intmap-ref types label))
         (tentry (var-type-entry (vector-ref entry (1+ succ-idx)) def)))
    (values (type-entry-type tentry)
            (type-entry-min tentry)
            (type-entry-max tentry))))

(define (primcall-types-check? types label name param args)
  (match (hashq-ref *type-checkers* name)
    (#f #f)
    (checker
     (let ((entry (intmap-ref types label)))
       (apply checker (vector-ref entry 0) param args)))))
