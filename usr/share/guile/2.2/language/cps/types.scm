;;; Type analysis on CPS
;;; Copyright (C) 2014, 2015, 2018 Free Software Foundation, Inc.
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
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module ((system syntax internal) #:select (syntax?))
  #:export (;; Specific types.
            &exact-integer
            &flonum
            &complex
            &fraction

            &char
            &unspecified
            &unbound
            &false
            &true
            &nil
            &null
            &symbol
            &keyword

            &procedure

            &pointer
            &fluid
            &pair
            &vector
            &box
            &struct
            &string
            &bytevector
            &bitvector
            &array
            &syntax

            ;; Union types.
            &number &real

            ;; Untagged types.
            &f64
            &u64
            &s64

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
  &exact-integer
  &flonum
  &complex
  &fraction

  &char
  &unspecified
  &unbound
  &false
  &true
  &nil
  &null
  &symbol
  &keyword

  &procedure

  &pointer
  &fluid
  &pair
  &vector
  &box
  &struct
  &string
  &bytevector
  &bitvector
  &array
  &syntax

  &f64
  &u64
  &s64)

(define-syntax &no-type (identifier-syntax 0))

(define-syntax &number
  (identifier-syntax (logior &exact-integer &flonum &complex &fraction)))
(define-syntax &real
  (identifier-syntax (logior &exact-integer &flonum &fraction)))

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

(define-compile-time-value &s64-min (- #x8000000000000000))
(define-compile-time-value &s64-max    #x7fffFFFFffffFFFF)
(define-compile-time-value &u64-max    #xffffFFFFffffFFFF)

(define-syntax &range-min (identifier-syntax &s64-min))
(define-syntax &range-max (identifier-syntax &u64-max))

;; This is a hack that takes advantage of knowing that
;; most-positive-fixnum is the size of a word, but with two tag bits and
;; one sign bit.  We also assume that the current common architectural
;; restriction of a maximum 48-bit address space means that we won't see
;; a size_t value above 2^48.
(define *max-size-t*
  (min (+ (ash most-positive-fixnum 3) #b111)
       (1- (ash 1 48))))
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
        ((< &range-min b-min) &range-min)
        (else -inf.0)))
     (let ((a-max (type-entry-max a))
           (b-max (type-entry-max b)))
       (cond
        ((not (> b-max a-max)) a-max)
        ((> *max-size-t* b-max) *max-size-t*)
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
        (make-type-entry type val val)
        (make-type-entry type -inf.0 +inf.0)))
  (cond
   ((number? val)
    (cond
     ((exact-integer? val) (return &exact-integer val))
     ((eqv? (imag-part val) 0)
      (if (nan? val)
          (make-type-entry &flonum -inf.0 +inf.0)
          (make-type-entry
           (if (exact? val) &fraction &flonum)
           (if (rational? val) (inexact->exact (floor val)) val)
           (if (rational? val) (inexact->exact (ceiling val)) val))))
     (else (return &complex #f))))
   ((eq? val '()) (return &null #f))
   ((eq? val #nil) (return &nil #f))
   ((eq? val #t) (return &true #f))
   ((eq? val #f) (return &false #f))
   ((char? val) (return &char (char->integer val)))
   ((eqv? val *unspecified*) (return &unspecified #f))
   ((symbol? val) (return &symbol #f))
   ((keyword? val) (return &keyword #f))
   ((pair? val) (return &pair #f))
   ((vector? val) (return &vector (vector-length val)))
   ((string? val) (return &string (string-length val)))
   ((bytevector? val) (return &bytevector (bytevector-length val)))
   ((bitvector? val) (return &bitvector (bitvector-length val)))
   ((array? val) (return &array (array-rank val)))
   ((syntax? val) (return &syntax 0))
   ((not (variable-bound? (make-variable val))) (return &unbound #f))

   (else (error "unhandled constant" val))))

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
(define-syntax-rule (&max/size x) (min (&max x) *max-size-t*))

(define-syntax-rule (define-type-checker (name arg ...) body ...)
  (hashq-set!
   *type-checkers*
   'name
   (lambda (typeset arg ...)
     (syntax-parameterize
         ((&type (syntax-rules () ((_ val) (var-type typeset val))))
          (&min  (syntax-rules () ((_ val) (var-min typeset val))))
          (&max  (syntax-rules () ((_ val) (var-max typeset val)))))
       body ...))))

(define-syntax-rule (check-type arg type min max)
  ;; If the arg is negative, it is a closure variable.
  (and (>= arg 0)
       (zero? (logand (lognot type) (&type arg)))
       (<= min (&min arg))
       (<= (&max arg) max)))

(define-syntax-rule (define-type-inferrer* (name succ var ...) body ...)
  (hashq-set!
   *type-inferrers*
   'name
   (lambda (in succ var ...)
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
  (define-type-inferrer* (name succ arg ...) body ...))

(define-syntax-rule (define-predicate-inferrer (name arg ... true?) body ...)
  (define-type-inferrer* (name succ arg ...)
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

(define-predicate-inferrer (eq? a b true?)
  ;; We can only propagate information down the true leg.
  (when true?
    (let ((type (logand (&type a) (&type b)))
          (min (max (&min a) (&min b)))
          (max (min (&max a) (&max b))))
      (restrict! a type min max)
      (restrict! b type min max))))
(define-type-inferrer-aliases eq? eqv?)

(define-syntax-rule (define-simple-predicate-inferrer predicate type)
  (define-predicate-inferrer (predicate val true?)
    (let ((type* (logand (&type val)
                         (if true?
                             type
                             (lognot type)))))
      (restrict! val type* -inf.0 +inf.0))))
(define-simple-predicate-inferrer pair? &pair)
(define-simple-predicate-inferrer null? (logior &nil &null))
(define-simple-predicate-inferrer nil? (logior &false &nil &null))
(define-simple-predicate-inferrer symbol? &symbol)
(define-simple-predicate-inferrer variable? &box)
(define-simple-predicate-inferrer vector? &vector)
(define-simple-predicate-inferrer struct? &struct)
(define-simple-predicate-inferrer string? &string)
(define-simple-predicate-inferrer bytevector? &bytevector)
(define-simple-predicate-inferrer bitvector? &bitvector)
(define-simple-predicate-inferrer keyword? &keyword)
(define-simple-predicate-inferrer number? &number)
(define-simple-predicate-inferrer char? &char)
(define-simple-predicate-inferrer procedure? &procedure)
(define-simple-predicate-inferrer thunk? &procedure)



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
;;; Threads.  We don't currently track threads as an object type.
;;;

(define-simple-types
  ((current-thread) &all-types))




;;;
;;; Prompts.  (Nothing to do.)
;;;




;;;
;;; Pairs.
;;;

(define-simple-types
  ((cons &all-types &all-types) &pair)
  ((car &pair) &all-types)
  ((set-car! &pair &all-types))
  ((cdr &pair) &all-types)
  ((set-cdr! &pair &all-types)))




;;;
;;; Variables.
;;;

(define-simple-types
  ((box &all-types) (&box 1))
  ((box-ref (&box 1)) &all-types))

(define-simple-type-checker (box-set! (&box 0 1) &all-types))
(define-type-inferrer (box-set! box val)
  (restrict! box &box 1 1))




;;;
;;; Vectors.
;;;

;; This max-vector-len computation is a hack.
(define *max-vector-len* (ash most-positive-fixnum -5))
(define-syntax-rule (&max/vector x) (min (&max x) *max-vector-len*))

(define-simple-type-checker (make-vector (&u64 0 *max-vector-len*)
                                         &all-types))
(define-type-inferrer (make-vector size init result)
  (restrict! size &u64 0 *max-vector-len*)
  (define! result &vector (&min/0 size) (&max/vector size)))

(define-type-checker (vector-ref v idx)
  (and (check-type v &vector 0 *max-vector-len*)
       (check-type idx &u64 0 (1- (&min v)))))
(define-type-inferrer (vector-ref v idx result)
  (restrict! v &vector (1+ (&min/0 idx)) *max-vector-len*)
  (restrict! idx &u64 0 (1- (&max/vector v)))
  (define! result &all-types -inf.0 +inf.0))

(define-type-checker (vector-set! v idx val)
  (and (check-type v &vector 0 *max-vector-len*)
       (check-type idx &u64 0 (1- (&min v)))))
(define-type-inferrer (vector-set! v idx val)
  (restrict! v &vector (1+ (&min/0 idx)) *max-vector-len*)
  (restrict! idx &u64 0 (1- (&max/vector v))))

(define-type-aliases make-vector make-vector/immediate)
(define-type-aliases vector-ref vector-ref/immediate)
(define-type-aliases vector-set! vector-set!/immediate)

(define-simple-type-checker (vector-length &vector))
(define-type-inferrer (vector-length v result)
  (restrict! v &vector 0 *max-vector-len*)
  (define! result &u64 (&min/0 v) (&max/vector v)))




;;;
;;; Structs.
;;;

;; No type-checker for allocate-struct, as we can't currently check that
;; vt is actually a vtable.
(define-type-inferrer (allocate-struct vt size result)
  (restrict! vt &struct vtable-offset-user *max-size-t*)
  (restrict! size &u64 0 *max-size-t*)
  (define! result &struct (&min/0 size) (&max/size size)))

(define-type-checker (struct-ref s idx)
  (and (check-type s &struct 0 *max-size-t*)
       (check-type idx &u64 0 *max-size-t*)
       ;; FIXME: is the field readable?
       (< (&max idx) (&min s))))
(define-type-inferrer (struct-ref s idx result)
  (restrict! s &struct (1+ (&min/0 idx)) *max-size-t*)
  (restrict! idx &u64 0 (1- (&max/size s)))
  (define! result &all-types -inf.0 +inf.0))

(define-type-checker (struct-set! s idx val)
  (and (check-type s &struct 0 *max-size-t*)
       (check-type idx &u64 0 *max-size-t*)
       ;; FIXME: is the field writable?
       (< (&max idx) (&min s))))
(define-type-inferrer (struct-set! s idx val)
  (restrict! s &struct (1+ (&min/0 idx)) *max-size-t*)
  (restrict! idx &u64 0 (1- (&max/size s))))

(define-type-aliases allocate-struct allocate-struct/immediate)
(define-type-aliases struct-ref struct-ref/immediate)
(define-type-aliases struct-set! struct-set!/immediate)

(define-simple-type (struct-vtable (&struct 0 *max-size-t*))
  (&struct vtable-offset-user *max-size-t*))




;;;
;;; Strings.
;;;

(define-type-checker (string-ref s idx)
  (and (check-type s &string 0 *max-size-t*)
       (check-type idx &u64 0 *max-size-t*)
       (< (&max idx) (&min s))))
(define-type-inferrer (string-ref s idx result)
  (restrict! s &string (1+ (&min/0 idx)) *max-size-t*)
  (restrict! idx &u64 0 (1- (&max/size s)))
  (define! result &char 0 *max-codepoint*))

(define-type-checker (string-set! s idx val)
  (and (check-type s &string 0 *max-size-t*)
       (check-type idx &u64 0 *max-size-t*)
       (check-type val &char 0 *max-codepoint*)
       (< (&max idx) (&min s))))
(define-type-inferrer (string-set! s idx val)
  (restrict! s &string (1+ (&min/0 idx)) *max-size-t*)
  (restrict! idx &u64 0 (1- (&max/size s)))
  (restrict! val &char 0 *max-codepoint*))

(define-simple-type-checker (string-length &string))
(define-type-inferrer (string-length s result)
  (restrict! s &string 0 *max-size-t*)
  (define! result &u64 (&min/0 s) (&max/size s)))

(define-simple-type (number->string &number) (&string 0 *max-size-t*))
(define-simple-type (string->number (&string 0 *max-size-t*))
  ((logior &number &false) -inf.0 +inf.0))




;;;
;;; Unboxed numbers.
;;;

(define-type-checker (scm->f64 scm)
  (check-type scm &real -inf.0 +inf.0))
(define-type-inferrer (scm->f64 scm result)
  (restrict! scm &real -inf.0 +inf.0)
  (define! result &f64 (&min scm) (&max scm)))
(define-type-aliases scm->f64 load-f64)

(define-type-checker (f64->scm f64)
  #t)
(define-type-inferrer (f64->scm f64 result)
  (define! result &flonum (&min f64) (&max f64)))

(define-type-checker (scm->u64 scm)
  (check-type scm &exact-integer 0 &u64-max))
(define-type-inferrer (scm->u64 scm result)
  (restrict! scm &exact-integer 0 &u64-max)
  (define! result &u64 (&min/0 scm) (&max/u64 scm)))
(define-type-aliases scm->u64 load-u64)

(define-type-checker (scm->u64/truncate scm)
  (check-type scm &exact-integer &range-min &range-max))
(define-type-inferrer (scm->u64/truncate scm result)
  (restrict! scm &exact-integer &range-min &range-max)
  (define! result &u64 0 &u64-max))

(define-type-checker (u64->scm u64)
  #t)
(define-type-inferrer (u64->scm u64 result)
  (define! result &exact-integer (&min/0 u64) (&max/u64 u64)))

(define-type-checker (scm->s64 scm)
  (check-type scm &exact-integer &s64-min &s64-max))
(define-type-inferrer (scm->s64 scm result)
  (restrict! scm &exact-integer &s64-min &s64-max)
  (define! result &s64 (&min/s64 scm) (&max/s64 scm)))
(define-type-aliases scm->s64 load-s64)

(define-type-checker (s64->scm s64)
  #t)
(define-type-inferrer (s64->scm s64 result)
  (define! result &exact-integer (&min/s64 s64) (&max/s64 s64)))




;;;
;;; Bytevectors.
;;;

(define-simple-type-checker (bv-length &bytevector))
(define-type-inferrer (bv-length bv result)
  (restrict! bv &bytevector 0 *max-size-t*)
  (define! result &u64 (&min/0 bv) (&max/size bv)))

(define-syntax-rule (define-bytevector-accessors ref set type size lo hi)
  (begin
    (define-type-checker (ref bv idx)
      (and (check-type bv &bytevector 0 *max-size-t*)
           (check-type idx &u64 0 *max-size-t*)
           (< (&max idx) (- (&min bv) size))))
    (define-type-inferrer (ref bv idx result)
      (restrict! bv &bytevector (+ (&min/0 idx) size) *max-size-t*)
      (restrict! idx &u64 0 (- (&max/size bv) size))
      (define! result type lo hi))
    (define-type-checker (set bv idx val)
      (and (check-type bv &bytevector 0 *max-size-t*)
           (check-type idx &u64 0 *max-size-t*)
           (check-type val type lo hi)
           (< (&max idx) (- (&min bv) size))))
    (define-type-inferrer (set! bv idx val)
      (restrict! bv &bytevector (+ (&min/0 idx) size) *max-size-t*)
      (restrict! idx &u64 0 (- (&max/size bv) size))
      (restrict! val type lo hi))))

(define-bytevector-accessors bv-u8-ref bv-u8-set! &u64 1 0 #xff)
(define-bytevector-accessors bv-s8-ref bv-s8-set! &s64 1 (- #x80) #x7f)

(define-bytevector-accessors bv-u16-ref bv-u16-set! &u64 2 0 #xffff)
(define-bytevector-accessors bv-s16-ref bv-s16-set! &s64 2 (- #x8000) #x7fff)

(define-bytevector-accessors bv-u32-ref bv-u32-set! &u64 4 0 #xffffffff)
(define-bytevector-accessors bv-s32-ref bv-s32-set! &s64 4
  (- #x80000000) #x7fffffff)

(define-bytevector-accessors bv-u64-ref bv-u64-set! &u64 8 0 &u64-max)
(define-bytevector-accessors bv-s64-ref bv-s64-set! &s64 8 &s64-min &s64-max)

(define-bytevector-accessors bv-f32-ref bv-f32-set! &f64 4 -inf.0 +inf.0)
(define-bytevector-accessors bv-f64-ref bv-f64-set! &f64 8 -inf.0 +inf.0)




;;;
;;; Numbers.
;;;

;; First, branching primitives with no results.
(define-simple-type-checker (= &number &number))
(define-predicate-inferrer (= a b true?)
  (when (and true?
             (zero? (logand (logior (&type a) (&type b)) (lognot &number))))
    (let ((min (max (&min a) (&min b)))
          (max (min (&max a) (&max b))))
      (restrict! a &number min max)
      (restrict! b &number min max))))

(define (restricted-comparison-ranges op type0 min0 max0 type1 min1 max1)
  (define (infer-integer-ranges)
    (match op
      ('< (values min0 (min max0 (1- max1)) (max (1+ min0) min1) max1))
      ('<= (values min0 (min max0 max1) (max min0 min1) max1))
      ('>= (values (max min0 min1) max0 min1 (min max0 max1)))
      ('> (values (max min0 (1+ min1)) max0 min1 (min (1- max0) max1)))))
  (define (infer-real-ranges)
    (match op
      ((or '< '<=) (values min0 (min max0 max1) (max min0 min1) max1))
      ((or '> '>=) (values (max min0 min1) max0 min1 (min max0 max1)))))
  (if (= (logior type0 type1) &exact-integer)
      (infer-integer-ranges)
      (infer-real-ranges)))

(define-syntax-rule (true-comparison-restrictions op a b a-type b-type)
  (call-with-values
      (lambda ()
        (restricted-comparison-ranges op
                                      (&type a) (&min a) (&max a)
                                      (&type b) (&min b) (&max b)))
    (lambda (min0 max0 min1 max1)
      (restrict! a a-type min0 max0)
      (restrict! b b-type min1 max1))))

(define-syntax-rule (define-comparison-inferrer (op inverse))
  (define-predicate-inferrer (op a b true?)
    (when (zero? (logand (logior (&type a) (&type b)) (lognot &number)))
      (true-comparison-restrictions (if true? 'op 'inverse) a b &real &real))))

(define-simple-type-checker (< &real &real))
(define-comparison-inferrer (< >=))

(define-simple-type-checker (<= &real &real))
(define-comparison-inferrer (<= >))

(define-simple-type-checker (>= &real &real))
(define-comparison-inferrer (>= <))

(define-simple-type-checker (> &real &real))
(define-comparison-inferrer (> <=))

(define-simple-type-checker (u64-= &u64 &u64))
(define-predicate-inferrer (u64-= a b true?)
  (when true?
    (let ((min (max (&min/0 a) (&min/0 b)))
          (max (min (&max/u64 a) (&max/u64 b))))
      (restrict! a &u64 min max)
      (restrict! b &u64 min max))))

(define-simple-type-checker (u64-=-scm &u64 &real))
(define-predicate-inferrer (u64-=-scm a b true?)
  (when (and true? (zero? (logand (&type b) (lognot &real))))
    (let ((min (max (&min/0 a) (&min/0 b)))
          (max (min (&max/u64 a) (&max/u64 b))))
      (restrict! a &u64 min max)
      (restrict! b &real min max))))

(define-simple-type-checker (u64-<-scm &u64 &real))
(define-predicate-inferrer (u64-<-scm a b true?)
  (when (and true? (zero? (logand (&type b) (lognot &real))))
    (true-comparison-restrictions '< a b &u64 &real)))

(define-simple-type-checker (u64-<=-scm &u64 &real))
(define-predicate-inferrer (u64-<=-scm a b true?)
  (when (and true? (zero? (logand (&type b) (lognot &real))))
    (true-comparison-restrictions '<= a b &u64 &real)))

(define-simple-type-checker (u64->=-scm &u64 &real))
(define-predicate-inferrer (u64->=-scm a b true?)
  (when (and true? (zero? (logand (&type b) (lognot &real))))
    (true-comparison-restrictions '>= a b &u64 &real)))

(define-simple-type-checker (u64->-scm &u64 &real))
(define-predicate-inferrer (u64->-scm a b true?)
  (when (and true? (zero? (logand (&type b) (lognot &real))))
    (true-comparison-restrictions '> a b &u64 &real)))

(define (infer-u64-comparison-ranges op min0 max0 min1 max1)
  (match op
    ('< (values min0 (min max0 (1- max1)) (max (1+ min0) min1) max1))
    ('<= (values min0 (min max0 max1) (max min0 min1) max1))
    ('>= (values (max min0 min1) max0 min1 (min max0 max1)))
    ('> (values (max min0 (1+ min1)) max0 min1 (min (1- max0) max1)))))
(define-syntax-rule (define-u64-comparison-inferrer (u64-op op inverse))
  (define-predicate-inferrer (u64-op a b true?)
    (call-with-values
        (lambda ()
          (infer-u64-comparison-ranges (if true? 'op 'inverse)
                                       (&min/0 a) (&max/u64 a)
                                       (&min/0 b) (&max/u64 b)))
      (lambda (min0 max0 min1 max1)
        (restrict! a &u64 min0 max0)
        (restrict! b &u64 min1 max1)))))

(define-simple-type-checker (u64-< &u64 &u64))
(define-u64-comparison-inferrer (u64-< < >=))

(define-simple-type-checker (u64-<= &u64 &u64))
(define-u64-comparison-inferrer (u64-<= <= >))

(define-simple-type-checker (u64->= &u64 &u64))
(define-u64-comparison-inferrer (u64-<= >= <))

(define-simple-type-checker (u64-> &u64 &u64))
(define-u64-comparison-inferrer (u64-> > <=))

;; Arithmetic.
(define-syntax-rule (define-unary-result! a result min max)
  (let ((min* min)
        (max* max)
        (type (logand (&type a) &number)))
    (cond
     ((not (= type (&type a)))
      ;; Not a number.  Punt and do nothing.
      (define! result &all-types -inf.0 +inf.0))
     ;; Complex numbers don't have a range.
     ((eqv? type &complex)
      (define! result &complex -inf.0 +inf.0))
     (else
      (define! result type min* max*)))))

(define-syntax-rule (define-binary-result! a b result closed? min max)
  (let ((min* min)
        (max* max)
        (a-type (logand (&type a) &number))
        (b-type (logand (&type b) &number)))
    (cond
     ((or (not (= a-type (&type a))) (not (= b-type (&type b))))
      ;; One input not a number.  Perhaps we end up dispatching to
      ;; GOOPS.
      (define! result &all-types -inf.0 +inf.0))
     ;; Complex numbers are contagious.
     ((or (eqv? a-type &complex) (eqv? b-type &complex))
      (define! result &complex -inf.0 +inf.0))
     ((or (eqv? a-type &flonum) (eqv? b-type &flonum))
      ;; If one argument is a flonum, the result will be flonum or
      ;; possibly complex.
      (let ((result-type (logand (logior a-type b-type)
                                 (logior &complex &flonum))))
        (define! result result-type min* max*)))
     ;; Exact integers are closed under some operations.
     ((and closed? (eqv? a-type &exact-integer) (eqv? b-type &exact-integer))
      (define! result &exact-integer min* max*))
     (else
      (let* ((type (logior a-type b-type))
             ;; Fractions may become integers.
             (type (if (zero? (logand type &fraction))
                       type
                       (logior type &exact-integer)))
             ;; Integers may become fractions under division.
             (type (if (or closed?
                           (zero? (logand type (logior &exact-integer))))
                       type
                       (logior type &fraction))))
        (define! result type min* max*))))))

(define-simple-type-checker (add &number &number))
(define-type-aliases add add/immediate)
(define-type-checker (fadd a b) #t)
(define-type-checker (uadd a b) #t)
(define-type-inferrer (add a b result)
  (define-binary-result! a b result #t
                         (+ (&min a) (&min b))
                         (+ (&max a) (&max b))))
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
(define-type-aliases uadd uadd/immediate)

(define-simple-type-checker (sub &number &number))
(define-type-aliases sub sub/immediate)
(define-type-checker (fsub a b) #t)
(define-type-checker (usub a b) #t)
(define-type-inferrer (sub a b result)
  (define-binary-result! a b result #t
                         (- (&min a) (&max b))
                         (- (&max a) (&min b))))
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
(define-type-aliases usub usub/immediate)

(define-simple-type-checker (mul &number &number))
(define-type-checker (fmul a b) #t)
(define-type-checker (umul a b) #t)
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
        (define-binary-result! a b result #t min max)))))
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
(define-type-aliases umul umul/immediate)

(define-type-checker (div a b)
  (and (check-type a &number -inf.0 +inf.0)
       (check-type b &number -inf.0 +inf.0)
       ;; We only know that there will not be an exception if b is not
       ;; zero.
       (not (<= (&min b) 0 (&max b)))))
(define-type-checker (fdiv a b) #t)
(define (div-result-range min-a max-a min-b max-b)
  (if (<= min-b 0 max-b)
      ;; If the range of the divisor crosses 0, the result spans
      ;; the whole range.
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
        (define-binary-result! a b result #f min max)))))
(define-type-inferrer (fdiv a b result)
  (let ((min-a (&min a)) (max-a (&max a))
        (min-b (&min b)) (max-b (&max b)))
    (call-with-values (lambda ()
                        (div-result-range min-a max-a min-b max-b))
      (lambda (min max)
        (define! result &f64 min max)))))

(define-type-checker (quo a b)
  (and (check-type a &exact-integer -inf.0 +inf.0)
       (check-type b &exact-integer -inf.0 +inf.0)
       ;; We only know that there will not be an exception if b is not
       ;; zero.
       (not (<= (&min b) 0 (&max b)))))
(define-type-inferrer (quo a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  (define! result &exact-integer -inf.0 +inf.0))

(define-type-checker-aliases quo rem)
(define-type-inferrer (rem a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  ;; Same sign as A.
  (let ((max-abs-rem (1- (max (abs (&min b)) (abs (&max b))))))
    (cond
     ((< (&min a) 0)
      (if (< 0 (&max a))
          (define! result &exact-integer (- max-abs-rem) max-abs-rem)
          (define! result &exact-integer (- max-abs-rem) 0)))
     (else
      (define! result &exact-integer 0 max-abs-rem)))))

(define-type-checker-aliases quo mod)
(define-type-inferrer (mod a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  ;; Same sign as B.
  (let ((max-abs-mod (1- (max (abs (&min b)) (abs (&max b))))))
    (cond
     ((< (&min b) 0)
      (if (< 0 (&max b))
          (define! result &exact-integer (- max-abs-mod) max-abs-mod)
          (define! result &exact-integer (- max-abs-mod) 0)))
     (else
      (define! result &exact-integer 0 max-abs-mod)))))

;; Predicates.
(define-syntax-rule (define-number-kind-predicate-inferrer name type)
  (define-type-inferrer (name val result)
    (cond
     ((zero? (logand (&type val) type))
      (define! result &false 0 0))
     ((zero? (logand (&type val) (lognot type)))
      (define! result &true 0 0))
     (else
      (define! result (logior &true &false) 0 0)))))
(define-number-kind-predicate-inferrer complex? &number)
(define-number-kind-predicate-inferrer real? &real)
(define-number-kind-predicate-inferrer rational?
  (logior &exact-integer &fraction))
(define-number-kind-predicate-inferrer integer?
  (logior &exact-integer &flonum))
(define-number-kind-predicate-inferrer exact-integer?
  &exact-integer)

(define-simple-type-checker (exact? &number))
(define-type-inferrer (exact? val result)
  (restrict! val &number -inf.0 +inf.0)
  (cond
   ((zero? (logand (&type val) (logior &exact-integer &fraction)))
    (define! result &false 0 0))
   ((zero? (logand (&type val) (lognot (logior &exact-integer &fraction))))
    (define! result &true 0 0))
   (else
    (define! result (logior &true &false) 0 0))))

(define-simple-type-checker (inexact? &number))
(define-type-inferrer (inexact? val result)
  (restrict! val &number -inf.0 +inf.0)
  (cond
   ((zero? (logand (&type val) (logior &flonum &complex)))
    (define! result &false 0 0))
   ((zero? (logand (&type val) (logand &number
                                       (lognot (logior &flonum &complex)))))
    (define! result &true 0 0))
   (else
    (define! result (logior &true &false) 0 0))))

(define-simple-type-checker (inf? &real))
(define-type-inferrer (inf? val result)
  (restrict! val &real -inf.0 +inf.0)
  (cond
   ((or (zero? (logand (&type val) (logior &flonum &complex)))
        (and (not (inf? (&min val))) (not (inf? (&max val)))))
    (define! result &false 0 0))
   (else
    (define! result (logior &true &false) 0 0))))

(define-type-aliases inf? nan?)

(define-simple-type (even? &exact-integer)
  ((logior &true &false) 0 0))
(define-type-aliases even? odd?)

;; Bit operations.
(define-simple-type-checker (ash &exact-integer &exact-integer))
(define-type-inferrer (ash val count result)
  (define (ash* val count)
    ;; As we only precisely represent a 64-bit range, don't bother inferring
    ;; shifts that might exceed that range.
    (cond
     ((inf? val) val) ; Preserves sign.
     ((< -64 count 64) (ash val count))
     ((zero? val) 0)
     ((positive? val) +inf.0)
     (else -inf.0)))
  (restrict! val &exact-integer -inf.0 +inf.0)
  (restrict! count &exact-integer -inf.0 +inf.0)
  (let ((-- (ash* (&min val) (&min count)))
        (-+ (ash* (&min val) (&max count)))
        (++ (ash* (&max val) (&max count)))
        (+- (ash* (&max val) (&min count))))
    (define! result &exact-integer
             (min -- -+ ++ +-)
             (max -- -+ ++ +-))))

(define-simple-type-checker (ursh &u64 &u64))
(define-type-inferrer (ursh a b result)
  (restrict! a &u64 0 &u64-max)
  (restrict! b &u64 0 &u64-max)
  (define! result &u64
    (ash (&min/0 a) (- (&max/u64 b)))
    (ash (&max/u64 a) (- (&min/0 b)))))
(define-type-aliases ursh ursh/immediate)

(define-simple-type-checker (ulsh &u64 &u64))
(define-type-inferrer (ulsh a b result)
  (restrict! a &u64 0 &u64-max)
  (restrict! b &u64 0 &u64-max)
  (if (and (< (&max/u64 b) 64)
           (<= (ash (&max/u64 a) (&max/u64 b)) &u64-max))
      ;; No overflow; we can be precise.
      (define! result &u64
        (ash (&min/0 a) (&min/0 b))
        (ash (&max/u64 a) (&max/u64 b)))
      ;; Otherwise assume the whole range.
      (define! result &u64 0 &u64-max)))
(define-type-aliases ulsh ulsh/immediate)

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
      (define! result &exact-integer min max))))

(define-simple-type-checker (ulogand &u64 &u64))
(define-type-inferrer (ulogand a b result)
  (restrict! a &u64 0 &u64-max)
  (restrict! b &u64 0 &u64-max)
  (define! result &u64 0 (min (&max/u64 a) (&max/u64 b))))

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
      (define! result &exact-integer min max))))

(define-simple-type-checker (ulogsub &u64 &u64))
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
      (define! result &exact-integer min max))))

(define-simple-type-checker (ulogior &u64 &u64))
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

(define-simple-type-checker (ulogxor &u64 &u64))
(define-type-inferrer (ulogxor a b result)
  (restrict! a &u64 0 &u64-max)
  (restrict! b &u64 0 &u64-max)
  (define! result &u64 0 (saturate+ (&max/u64 a) (&max/u64 b))))

(define-simple-type-checker (lognot &exact-integer))
(define-type-inferrer (lognot a result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (define! result &exact-integer
           (lognot* (&max a))
           (lognot* (&min a))))

(define-simple-type-checker (logtest &exact-integer &exact-integer))
(define-predicate-inferrer (logtest a b true?)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0))

(define-simple-type-checker (logbit? (&exact-integer 0 +inf.0) &exact-integer))
(define-type-inferrer (logbit? a b result)
  (let ((a-min (&min a))
        (a-max (&max a))
        (b-min (&min b))
        (b-max (&max b)))
    (if (and (eqv? a-min a-max) (>= a-min 0) (not (inf? a-min))
             (eqv? b-min b-max) (>= b-min 0) (not (inf? b-min)))
        (let ((type (if (logbit? a-min b-min) &true &false)))
          (define! result type 0 0))
        (define! result (logior &true &false) 0 0))))

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

(define-simple-type-checker (abs &real))
(define-type-inferrer (abs x result)
  (let ((type (&type x)))
    (cond
     ((eqv? type (logand type &number))
      (restrict! x &real -inf.0 +inf.0)
      (define! result (logand type &real)
        (min (abs (&min x)) (abs (&max x)))
        (max (abs (&min x)) (abs (&max x)))))
     (else
      (define! result (logior (logand (&type x) (lognot &number))
                              (logand (&type x) &real))
        (&min/0 x)
        (max (abs (&min x)) (abs (&max x))))))))




;;;
;;; Characters.
;;;

(define-simple-type-checker (integer->char (&u64 0 *max-codepoint*)))
(define-type-inferrer (integer->char i result)
  (restrict! i &u64 0 *max-codepoint*)
  (define! result &char (&min/0 i) (min (&max i) *max-codepoint*)))

(define-simple-type-checker (char->integer &char))
(define-type-inferrer (char->integer c result)
  (restrict! c &char 0 *max-codepoint*)
  (define! result &u64 (&min/0 c) (min (&max c) *max-codepoint*)))




;;;
;;; Type flow analysis: the meet (ahem) of the algorithm.
;;;

(define (successor-count cont)
  (match cont
    (($ $kargs _ _ ($ $continue k src exp))
     (match exp
       ((or ($ $branch) ($ $prompt)) 2)
       (_ 1)))
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

  (define (infer-primcall types succ name args result)
    (cond
     ((hashq-ref *type-inferrers* name)
      => (lambda (inferrer)
           ;; FIXME: remove the apply?
           ;; (pk 'primcall name args result)
           (apply inferrer types succ
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
      (($ $branch kt ($ $values (arg)))
       ;; The "normal" continuation is the #f branch.
       (let ((kf-types (restrict-var types arg
                                     (make-type-entry (logior &false &nil)
                                                      0
                                                      0)))
             (kt-types (restrict-var types arg
                                     (make-type-entry
                                      (logand &all-types 
                                              (lognot (logior &false &nil)))
                                      -inf.0 +inf.0))))
         (propagate2 k kf-types kt kt-types)))
      (($ $branch kt ($ $primcall name args))
       ;; The "normal" continuation is the #f branch.
       (let ((kf-types (infer-primcall types 0 name args #f))
             (kt-types (infer-primcall types 1 name args #f)))
         (propagate2 k kf-types kt kt-types)))
      (($ $prompt escape? tag handler)
       ;; The "normal" continuation enters the prompt.
       (propagate2 k types handler types))
      (($ $primcall name args)
       (propagate1 k
                   (match (intmap-ref conts k)
                     (($ $kargs _ defs)
                      (infer-primcall types 0 name args
                                      (match defs ((var) var) (() #f))))
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
      ((or ($ $call) ($ $callk))
       (propagate1 k types))
      (($ $rec names vars funs)
       (let ((proc-type (make-type-entry &procedure -inf.0 +inf.0)))
         (propagate1 k (adjoin-vars types vars proc-type))))
      (_
       (match (intmap-ref conts k)
         (($ $kargs (_) (var))
          (let ((entry (match exp
                         (($ $const val)
                          (constant-type val))
                         ((or ($ $prim) ($ $fun) ($ $closure))
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
        (($ $kreceive arity k)
         (match (intmap-ref conts k)
           (($ $kargs names vars)
            (propagate1 k (adjoin-vars types vars all-types-entry)))))
        (($ $kfun src meta self tail clause)
         (if clause
             (propagate1 clause (adjoin-var types self all-types-entry))
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

(define (primcall-types-check? types label name args)
  (match (hashq-ref *type-checkers* name)
    (#f #f)
    (checker
     (let ((entry (intmap-ref types label)))
       (apply checker (vector-ref entry 0) args)))))
