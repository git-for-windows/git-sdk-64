;;; Effects analysis on CPS

;; Copyright (C) 2011-2015,2017-2021,2023 Free Software Foundation, Inc.

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
;;; A helper module to compute the set of effects caused by an
;;; expression.  This information is useful when writing algorithms that
;;; move code around, while preserving the semantics of an input
;;; program.
;;;
;;; The effects set is represented as an integer with three parts.  The
;;; low 4 bits indicate effects caused by an expression, as a bitfield.
;;; The next 4 bits indicate the kind of memory accessed by the
;;; expression, if it accesses mutable memory.  Finally the rest of the
;;; bits indicate the field in the object being accessed, if known, or
;;; -1 for unknown.
;;;
;;; In this way we embed a coarse type-based alias analysis in the
;;; effects analysis.  For example, a "car" call is modelled as causing
;;; a read to field 0 on a &pair, and causing a &type-check effect.  If
;;; any intervening code sets the car of any pair, that will block
;;; motion of the "car" call, because any write to field 0 of a pair is
;;; seen by effects analysis as being a write to field 0 of all pairs.
;;;
;;; Code:

(define-module (language cps effects-analysis)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:use-module (ice-9 match)
  #:export (expression-effects
            compute-effects
            synthesize-definition-effects

            &allocation
            &type-check
            &read
            &write

            &fluid
            &prompt
            &vector
            &box
            &module
            &struct
            &string
            &thread
            &bytevector
            &closure
            &header

            &object
            &field

            &allocate
            &read-object
            &read-field
            &write-object
            &write-field

            &no-effects
            &all-effects

            causes-effect?
            causes-all-effects?
            effect-clobbers?
            compute-clobber-map))

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

(define-syntax define-enumeration
  (lambda (x)
    (define (count-bits n)
      (let lp ((out 1))
        (if (< n (ash 1 (1- out)))
            out
            (lp (1+ out)))))
    (syntax-case x ()
      ((_ mask shift name ...)
       (let* ((len (length #'(name ...)))
              (bits (count-bits len)))
         (with-syntax (((n ...) (iota len))
                       (bits bits))
           #'(begin
               (define-syntax name (identifier-syntax n))
               ...
               (define-syntax mask (identifier-syntax (1- (ash 1 bits))))
               (define-syntax shift (identifier-syntax bits)))))))))

(define-flags &all-effect-kinds &effect-kind-bits
  ;; Indicates that an expression may cause a type check.  A type check,
  ;; for the purposes of this analysis, is the possibility of throwing
  ;; an exception the first time an expression is evaluated.  If the
  ;; expression did not cause an exception to be thrown, users can
  ;; assume that evaluating the expression again will not cause an
  ;; exception to be thrown.
  ;;
  ;; For example, (+ x y) might throw if X or Y are not numbers.  But if
  ;; it doesn't throw, it should be safe to elide a dominated, common
  ;; subexpression (+ x y).
  &type-check

  ;; Indicates that an expression may return a fresh object.  The kind
  ;; of object is indicated in the object kind field.
  &allocation

  ;; Indicates that an expression may cause a read from memory.  The
  ;; kind of memory is given in the object kind field.  Some object
  ;; kinds have finer-grained fields; those are expressed in the "field"
  ;; part of the effects value.  -1 indicates "the whole object".
  &read

  ;; Indicates that an expression may cause a write to memory.
  &write)

(define-enumeration &memory-kind-mask &memory-kind-bits
  ;; Indicates than an expression may access unknown kinds of memory.
  &unknown-memory-kinds

  ;; Indicates that an expression depends on the value of a fluid
  ;; variable, or on the current fluid environment.
  &fluid

  ;; Indicates that an expression depends on the current prompt
  ;; stack.
  &prompt

  ;; Indicates that an expression depends on the value of the car or cdr
  ;; of a pair.
  &pair

  ;; Indicates that an expression depends on the value of a vector
  ;; field.  The effect field indicates the specific field, or zero for
  ;; an unknown field.
  &vector

  ;; Indicates that an expression depends on the value of a variable
  ;; cell.
  &box

  ;; Indicates that an expression depends on the current module.
  &module

  ;; Indicates that an expression depends on the current thread.
  &thread

  ;; Indicates that an expression depends on the value of a struct
  ;; field.  The effect field indicates the specific field, or zero for
  ;; an unknown field.
  &struct

  ;; Indicates that an expression depends on the contents of a string.
  &string

  ;; Indicates that an expression depends on the contents of a
  ;; bytevector.  We cannot be more precise, as bytevectors may alias
  ;; other bytevectors.
  &bytevector

  ;; Indicates a dependency on a free variable of a closure.
  &closure

  ;; Indicates a dependency on a raw bitmask, measured in 32-bit units.
  &bitmask

  ;; Indicates a dependency on the value of a cache cell.
  &cache

  ;; Indicates that an expression depends on a value extracted from the
  ;; fixed, unchanging part of an object -- for example the length of a
  ;; vector or the vtable of a struct.
  &header)

(define-inlinable (&field kind field)
  (ash (logior (ash field &memory-kind-bits) kind) &effect-kind-bits))
(define-inlinable (&object kind)
  (&field kind -1))

(define-inlinable (&allocate kind)
  (logior &allocation (&object kind)))
(define-inlinable (&read-field kind field)
  (logior &read (&field kind field)))
(define-inlinable (&read-object kind)
  (logior &read (&object kind)))
(define-inlinable (&write-field kind field)
  (logior &write (&field kind field)))
(define-inlinable (&write-object kind)
  (logior &write (&object kind)))

(define-syntax &no-effects (identifier-syntax 0))
(define-syntax &all-effects
  (identifier-syntax
   (logior &all-effect-kinds (&object &unknown-memory-kinds))))

(define-inlinable (causes-effect? x effects)
  (logtest x effects))

(define-inlinable (causes-all-effects? x)
  (eqv? x &all-effects))

(define (effect-clobbers? a b)
  "Return true if A clobbers B.  This is the case if A is a write, and B
is or might be a read or a write to the same location as A."
  (define (locations-same?)
    (let ((a (ash a (- &effect-kind-bits)))
          (b (ash b (- &effect-kind-bits))))
      (or (eqv? &unknown-memory-kinds (logand a &memory-kind-mask))
          (eqv? &unknown-memory-kinds (logand b &memory-kind-mask))
          (and (eqv? (logand a &memory-kind-mask) (logand b &memory-kind-mask))
               ;; A negative field indicates "the whole object".
               ;; Non-negative fields indicate only part of the object.
               (or (< a 0) (< b 0) (= a b))))))
  (and (logtest a &write)
       (logtest b (logior &read &write))
       (locations-same?)))

(define (compute-known-allocations conts effects)
  "Return a map of ACCESS-LABEL to ALLOC-LABEL, indicating stores to and
loads from objects created at known allocation sites."
  ;; VAR -> ALLOC map of defining allocations, where ALLOC is a label or
  ;; #f.  Possibly sparse.
  (define allocations
    (intmap-fold
     (lambda (label fx out)
       (match (intmap-ref conts label)
         (($ $kargs _ _ ($ $continue k))
          (match (intmap-ref conts k)
            (($ $kargs (_) (var))
             (intmap-add out var
                         (and (not (causes-all-effects? fx))
                              (logtest fx &allocation)
                              label)
                         (lambda (old new) #f)))
            (_ out)))
         (_ out)))
     effects empty-intmap))

  (persistent-intmap
   (intmap-fold
    (lambda (label fx out)
      (cond
       ((causes-all-effects? fx) out)
       ((logtest fx &allocation) out)
       ((logtest fx (logior &read &write))
        (match (intmap-ref conts label)
          ;; Assume that instructions which cause a known set of effects
          ;; and which
          (($ $kargs names vars
              ($ $continue k src
                 ($ $primcall name param (obj . args))))
           (match (intmap-ref allocations obj (lambda (_) #f))
             (#f out)
             (allocation-label
              (intmap-add! out label allocation-label))))
          (_ out)))
       (else out)))
    effects empty-intmap)))

(define (compute-clobber-map conts effects)
  "For the map LABEL->EFFECTS, compute a map LABEL->LABELS indicating
the LABELS that are clobbered by the effects of LABEL."
  (define known-allocations (compute-known-allocations conts effects))
  (define (filter-may-alias write-label clobbered-labels)
    ;; We may be able to remove some entries from CLOBBERED-LABELS, if
    ;; we can prove they are not aliased by WRITE-LABEL.
    (match (intmap-ref known-allocations write-label (lambda (_) #f))
      (#f
       ;; We don't know what object WRITE-LABEL refers to; can't refine.
       clobbered-labels)
      (clobber-alloc
       (intset-fold
        (lambda (clobbered-label clobbered-labels)
          (match (intmap-ref known-allocations clobbered-label (lambda (_) #f))
            (#f
             ;; We don't know what object CLOBBERED-LABEL refers to;
             ;; can't refine.
             clobbered-labels)
            (clobbered-alloc
             ;; We know that WRITE-LABEL and CLOBBERED-LABEL refer to
             ;; known allocations.  The write will only clobber the read
             ;; if the two allocations are the same.
             (if (eqv? clobber-alloc clobbered-alloc)
                 clobbered-labels
                 (intset-remove clobbered-labels clobbered-label)))))
        clobbered-labels clobbered-labels))))

  (define (make-clobber-vector) (make-vector &memory-kind-mask empty-intset))

  (define clobbered-by-write-to-unknown empty-intset)
  (define clobbered-by-write-to-any-field (make-clobber-vector))
  (define clobbered-by-write-to-all-fields (make-clobber-vector))
  (define clobbered-by-write-to-specific-field (make-hash-table))

  (define (adjoin-to-clobber-vector! v k id)
    (vector-set! v k (intset-union (vector-ref v k) (intset id))))
  (define (add-clobbered-by-write-to-any-field! kind label)
    (adjoin-to-clobber-vector! clobbered-by-write-to-any-field kind label))
  (define (add-clobbered-by-write-to-all-fields! kind label)
    (adjoin-to-clobber-vector! clobbered-by-write-to-all-fields kind label))
  (define (adjoin-to-clobber-hash! h k id)
    (hashv-set! h k (intset-union (hashv-ref h k empty-intset) (intset id))))
  (define (add-clobbered-by-write-to-specific-field! kind+field label)
    (adjoin-to-clobber-hash! clobbered-by-write-to-specific-field
                             kind+field label))

  (intmap-fold
   (lambda (label fx)
     ;; Unless an expression causes a read, it isn't clobbered by
     ;; anything.
     (when (causes-effect? fx &read)
       (define kind+field (ash fx (- &effect-kind-bits)))
       (define kind (logand &memory-kind-mask kind+field))
       (define field (ash kind+field (- &memory-kind-bits)))
       (cond
        ((eqv? field -1)
         ;; A read of the whole object is clobbered by a write to any
         ;; field.
         (add-clobbered-by-write-to-all-fields! kind label)
         (add-clobbered-by-write-to-any-field! kind label))
        ((negative? field) (error "unexpected field"))
        (else
         ;; A read of a specific field is clobbered by a write to that
         ;; specific field, or a write to all fields.
         (add-clobbered-by-write-to-all-fields! kind label)
         (add-clobbered-by-write-to-specific-field! kind+field label)))           

       ;; Also clobbered by write to any field of unknown memory kinds.
       (add-clobbered-by-write-to-any-field! &unknown-memory-kinds label))
     (values))
   effects)
  (define (lookup-clobbers fx)
    (define kind+field (ash fx (- &effect-kind-bits)))
    (define kind (logand &memory-kind-mask kind+field))
    (define field (ash kind+field (- &memory-kind-bits)))
    (cond
     ((eqv? field -1)
      ;; A write to the whole object.
      (intset-union
       (vector-ref clobbered-by-write-to-any-field kind)
       (vector-ref clobbered-by-write-to-all-fields kind)))
     ((negative? field) (error "unexpected field"))
     (else
      ;; A write to a specific field.  In addition to clobbering reads
      ;; of this specific field, we clobber reads of the whole object,
      ;; for example the ones that correspond to the synthesized "car"
      ;; and "cdr" definitions that are associated with a "cons" expr.
      (intset-union
       (vector-ref clobbered-by-write-to-any-field kind)
       (hashv-ref clobbered-by-write-to-specific-field kind+field)))))
  (intmap-map (lambda (label fx)
                (if (causes-effect? fx &write)
                    (filter-may-alias label (lookup-clobbers fx))
                    empty-intset))
              effects))

(define *primitive-effects* (make-hash-table))

(define-syntax-rule (define-primitive-effects* param
                      ((name . args) effects ...)
                      ...)
  (begin
    (hashq-set! *primitive-effects* 'name
                (case-lambda*
                 ((param . args) (logior effects ...))
                 (_ &all-effects)))
    ...))

(define-syntax-rule (define-primitive-effects ((name . args) effects ...) ...)
  (define-primitive-effects* param ((name . args) effects ...) ...))

;; Miscellaneous.
(define-primitive-effects
  ((load-const/unlikely))
  ((values . _)))

;; Generic effect-free predicates.
(define-primitive-effects
  ((eq? x y))
  ((equal? x y))
  ((bignum? arg))
  ((bitvector? arg))
  ((bytevector? arg))
  ((char? arg))
  ((compnum? arg))
  ((eq-constant? arg))
  ((false? arg))
  ((fixnum? arg))
  ((flonum? arg))
  ((fluid? arg))
  ((fracnum? arg))
  ((heap-number? arg))
  ((heap-object? arg))
  ((immutable-vector? arg))
  ((keyword? arg))
  ((nil? arg))
  ((null? arg))
  ((mutable-vector? arg))
  ((pair? arg))
  ((pointer? arg))
  ((procedure? arg))
  ((program? arg))
  ((string? arg))
  ((struct? arg))
  ((symbol? arg))
  ((syntax? arg))
  ((thunk? arg))
  ((undefined? arg))
  ((variable? arg))
  ((vector? arg)))

;; Fluids.
(define-primitive-effects
  ((fluid-ref f)                   (&read-object &fluid)       &type-check)
  ((fluid-set! f v)                (&write-object &fluid)      &type-check)
  ((push-fluid f v)                (&write-object &fluid)      &type-check)
  ((pop-fluid)                     (&write-object &fluid))
  ((push-dynamic-state state)      (&write-object &fluid)      &type-check)
  ((pop-dynamic-state)             (&write-object &fluid)))

(define-primitive-effects
  ((symbol->string x))             ;; CPS lowering includes symbol? type check.
  ((symbol->keyword))              ;; Same.
  ((keyword->symbol))              ;; Same, for keyword?.
  ((string->symbol)                (&read-object &string)      &type-check)
  ((string->utf8)                  (&read-object &string))
  ((utf8->string)                  (&read-object &bytevector)  &type-check)
  ((string-utf8-length)            (&read-object &string)))

;; Threads.  Calls cause &all-effects, which reflects the fact that any
;; call can capture a partial continuation and reinstate it on another
;; thread.
(define-primitive-effects
  ((current-thread)                (&read-object &thread)))

;; Prompts.
(define-primitive-effects
  ((make-prompt-tag #:optional arg) (&allocate &unknown-memory-kinds)))

;; Generic objects.
(define (annotation->memory-kind* annotation idx)
  ;; Lowering from Tree-IL to CPS reifies type-specific constructors and
  ;; accessors.  For these we can treat e.g. vector-length as completely
  ;; constant as it can commute with any other instruction: the header
  ;; initialization write of a vector is not visible.
  ;;
  ;; However when these instructions are later lowered to allocate-words
  ;; with explicit initializers, we need to model the header reads and
  ;; writes as non-commutative.
  (match (cons annotation idx)
    (('vector . 0) &header)
    (('string . (or 0 1 2 3)) &header)
    (('stringbuf . (or 0 1)) &header)
    (('bytevector . (or 0 1 2 3)) &header)
    (('symbol . (or 0 1 2)) &header)
    (('box . 0) &header)
    (('closure . (or 0 1)) &header)
    (('struct . 0) &header)
    (('atomic-box . 0) &header)
    (_ (annotation->memory-kind annotation))))

(define (annotation->memory-kind annotation)
  (match annotation
    ('pair &pair)
    ('vector &vector)
    ('string &string)
    ('stringbuf &string)
    ('symbol &unknown-memory-kinds)
    ('bytevector &bytevector)
    ('bitmask &bitmask)
    ('box &box)
    ('closure &closure)
    ('struct &struct)
    ('atomic-box &unknown-memory-kinds)
    ('keyword &unknown-memory-kinds)))

(define-primitive-effects* param
  ((allocate-vector size)          (&allocate &vector))
  ((allocate-vector/immediate)     (&allocate &vector))
  ((vector-length v))
  ((vector-ref/immediate v)        (&read-field &vector param))
  ((vector-ref v idx)              (&read-object &vector))
  ((vector-set!/immediate v val)   (&write-field &vector param))
  ((vector-set! v idx val)         (&write-object &vector))

  ((cons x y)                      (&allocate &pair))
  ((car pair)                      (&read-field &pair 0))
  ((cdr pair)                      (&read-field &pair 1))
  ((set-car! pair val)             (&write-field &pair 0))
  ((set-cdr! pair val)             (&write-field &pair 1))

  ((box val)                       (&allocate &box))
  ((box-ref b)                     (&read-object &box))
  ((box-set! b val)                (&write-object &box))

  ((allocate-struct vtable)        (&allocate &struct))
  ((vtable-size x))
  ((vtable-has-unboxed-fields? x))
  ((vtable-field-boxed? x))
  ((struct-vtable x))
  ((struct-ref x)                  (&read-field &struct param))
  ((struct-set! x y)               (&write-field &struct param))

  ((bv-contents bv))
  ((bv-length bv))

  ((string-length str))
  ((string-ref str idx)            (&read-object &string))
  ((string-set! str idx cp)        (&write-object &string))

  ((symbol-hash))

  ((make-closure code)             (&allocate &closure))
  ((closure-ref code)              (match param
                                     ((idx . nfree)
                                      (&read-field &closure idx))))
  ((closure-set! code)             (match param
                                     ((idx . nfree)
                                      (&write-field &closure idx)))))

(define-primitive-effects* param
  ((allocate-words size)           (&allocate (annotation->memory-kind param)))
  ((allocate-words/immediate)      (match param
                                     ((ann . size)
                                      (&allocate
                                       (annotation->memory-kind ann)))))
  ((allocate-pointerless-words size)
                                   (&allocate (annotation->memory-kind param)))
  ((allocate-pointerless-words/immediate)
                                   (match param
                                     ((ann . size)
                                      (&allocate
                                       (annotation->memory-kind ann)))))
  ((scm-ref obj idx)               (&read-object
                                    (annotation->memory-kind param)))
  ((scm-ref/tag obj)               (&read-field
                                    (annotation->memory-kind* param 0) 0))
  ((scm-ref/immediate obj)         (match param
                                     ((ann . idx)
                                      (&read-field
                                       (annotation->memory-kind* ann idx) idx))))
  ((scm-set! obj idx val)          (&write-object
                                    (annotation->memory-kind param)))
  ((scm-set/tag! obj val)          (&write-field
                                    (annotation->memory-kind* param 0) 0))
  ((scm-set!/immediate obj val)    (match param
                                     ((ann . idx)
                                      (&write-field
                                       (annotation->memory-kind* ann idx) idx))))
  ((word-ref obj idx)              (&read-object
                                    (annotation->memory-kind param)))
  ((word-ref/immediate obj)        (match param
                                     ((ann . idx)
                                      (&read-field
                                       (annotation->memory-kind* ann idx) idx))))
  ((word-set! obj idx val)         (&read-object
                                    (annotation->memory-kind param)))
  ((word-set!/immediate obj val)   (match param
                                     ((ann . idx)
                                      (&write-field
                                       (annotation->memory-kind* ann idx) idx))))
  ((pointer-ref/immediate obj)     (match param
                                     ((ann . idx)
                                      (&read-field
                                       (annotation->memory-kind* ann idx) idx))))
  ((pointer-set!/immediate obj val)
                                   (match param
                                     ((ann . idx)
                                      (&write-field
                                       (annotation->memory-kind* ann idx) idx))))
  ((tail-pointer-ref/immediate obj)))

;; Strings.
(define-primitive-effects
  ((string-set! s n c)             (&write-object &string)     &type-check)
  ((number->string _)              (&allocate &string)         &type-check)
  ((string->number _)              (&read-object &string)      &type-check))

;; Unboxed floats and integers.
(define-primitive-effects
  ((scm->f64 _)                                                &type-check)
  ((load-f64))
  ((f64->scm _))
  ((scm->u64 _)                                                &type-check)
  ((scm->u64/truncate _)                                       &type-check)
  ((load-u64))
  ((u64->scm _))
  ((u64->scm/unlikely _))
  ((scm->s64 _)                                                &type-check)
  ((load-s64))
  ((s64->scm _))
  ((s64->scm/unlikely _))
  ((u64->s64 _))
  ((s64->u64 _))
  ((assume-u64 _))
  ((assume-s64 _))
  ((untag-fixnum _))
  ((tag-fixnum _))
  ((tag-fixnum/unlikely _)))

;; Pointers.
(define-primitive-effects* param
  ((u8-ref obj bv n)               (&read-object (annotation->memory-kind param)))
  ((s8-ref obj bv n)               (&read-object (annotation->memory-kind param)))
  ((u16-ref obj bv n)              (&read-object (annotation->memory-kind param)))
  ((s16-ref obj bv n)              (&read-object (annotation->memory-kind param)))
  ((u32-ref obj bv n)              (&read-object (annotation->memory-kind param)))
  ((s32-ref obj bv n)              (&read-object (annotation->memory-kind param)))
  ((u64-ref obj bv n)              (&read-object (annotation->memory-kind param)))
  ((s64-ref obj bv n)              (&read-object (annotation->memory-kind param)))
  ((f32-ref obj bv n)              (&read-object (annotation->memory-kind param)))
  ((f64-ref obj bv n)              (&read-object (annotation->memory-kind param)))

  ((u8-set! obj bv n x)            (&write-object (annotation->memory-kind param)))
  ((s8-set! obj bv n x)            (&write-object (annotation->memory-kind param)))
  ((u16-set! obj bv n x)           (&write-object (annotation->memory-kind param)))
  ((s16-set! obj bv n x)           (&write-object (annotation->memory-kind param)))
  ((u32-set! obj bv n x)           (&write-object (annotation->memory-kind param)))
  ((s32-set! obj bv n x)           (&write-object (annotation->memory-kind param)))
  ((u64-set! obj bv n x)           (&write-object (annotation->memory-kind param)))
  ((s64-set! obj bv n x)           (&write-object (annotation->memory-kind param)))
  ((f32-set! obj bv n x)           (&write-object (annotation->memory-kind param)))
  ((f64-set! obj bv n x)           (&write-object (annotation->memory-kind param))))

;; Modules.
(define-primitive-effects
  ((current-module)                (&read-object &module))
  ((cache-current-module! m)       (&write-object &cache))
  ((resolve name)                  (&read-object &module)      &type-check)
  ((resolve-module mod)            (&read-object &module)      &type-check)
  ((module-variable mod name)      (&read-object &module)      &type-check)
  ((lookup mod name)               (&read-object &module)      &type-check)
  ((lookup-bound mod name)         (&read-object &module)      &type-check)
  ((lookup-bound-public)                                       &type-check)
  ((lookup-bound-private)                                      &type-check)
  ((cached-toplevel-box)                                       &type-check)
  ((cached-module-box)                                         &type-check)
  ((define! mod name)              (&read-object &module)))

;; Cache cells.
(define-primitive-effects
  ((cache-ref)                     (&read-object &cache))
  ((cache-set! x)                  (&write-object &cache)))

;; Numbers.
(define-primitive-effects
  ((heap-numbers-equal? . _))
  ((= . _)                         &type-check)
  ((<= . _)                         &type-check)
  ((< . _)                         &type-check)
  ((u64-= . _))
  ((u64-imm-= . _))
  ((u64-< . _))
  ((u64-imm-< . _))
  ((imm-u64-< . _))
  ((s64-= . _))
  ((s64-imm-= . _))
  ((s64-< . _))
  ((s64-imm-< . _))
  ((imm-s64-< . _))
  ((f64-= . _))
  ((f64-< . _))
  ((f64-<= . _))
  ((zero? . _)                     &type-check)
  ((add . _)                       &type-check)
  ((add/immediate . _)             &type-check)
  ((mul . _)                       &type-check)
  ((sub . _)                       &type-check)
  ((sub/immediate . _)             &type-check)
  ((div . _)                       &type-check)
  ((fadd . _))
  ((fsub . _))
  ((fmul . _))
  ((fdiv . _))
  ((uadd . _))
  ((usub . _))
  ((umul . _))
  ((uadd/immediate . _))
  ((usub/immediate . _))
  ((umul/immediate . _))
  ((sadd . _))
  ((ssub . _))
  ((smul . _))
  ((sadd/immediate . _))
  ((ssub/immediate . _))
  ((smul/immediate . _))
  ((quo . _)                       &type-check)
  ((rem . _)                       &type-check)
  ((mod . _)                       &type-check)
  ((inexact _)                     &type-check)
  ((s64->f64 _))
  ((number? _))
  ((complex? _))
  ((real? _))
  ((rational? _))
  ((integer? _))
  ((exact? _))
  ((inexact? _))
  ((inf? _)                        &type-check)
  ((nan? _)                        &type-check)
  ((even? _)                       &type-check)
  ((odd? _)                        &type-check)
  ((rsh n m)                       &type-check)
  ((lsh n m)                       &type-check)
  ((rsh/immediate n)               &type-check)
  ((lsh/immediate n)               &type-check)
  ((logand . _)                    &type-check)
  ((logand/immediate . _)          &type-check)
  ((logior . _)                    &type-check)
  ((logxor . _)                    &type-check)
  ((logsub . _)                    &type-check)
  ((lognot . _)                    &type-check)
  ((ulogand . _))
  ((ulogand/immediate . _))
  ((ulogior . _))
  ((ulogxor . _))
  ((ulogsub . _))
  ((ursh . _))
  ((srsh . _))
  ((ulsh . _))
  ((slsh . _))
  ((ursh/immediate . _))
  ((srsh/immediate . _))
  ((ulsh/immediate . _))
  ((slsh/immediate . _))
  ((logtest a b)                   &type-check)
  ((logbit? a b)                   &type-check)
  ((sqrt _)                        &type-check)
  ((abs _)                         &type-check)
  ((floor _)                       &type-check)
  ((ceiling _)                     &type-check)
  ((sin _)                         &type-check)
  ((cos _)                         &type-check)
  ((tan _)                         &type-check)
  ((asin _)                        &type-check)
  ((acos _)                        &type-check)
  ((atan _)                        &type-check)
  ((atan2 x y)                     &type-check)
  ((fsqrt _))
  ((fabs _))
  ((ffloor _))
  ((fceiling _))
  ((fsin _))
  ((fcos _))
  ((ftan _))
  ((fasin _))
  ((facos _))
  ((fatan _))
  ((fatan2 x y)))

;; Characters.
(define-primitive-effects
  ((untag-char _))
  ((tag-char _)))

;; Atomics are a memory and a compiler barrier; they cause all effects
;; so no need to have a case for them here.  (Though, see
;; https://jfbastien.github.io/no-sane-compiler/.)

(define (primitive-effects param name args)
  (let ((proc (hashq-ref *primitive-effects* name)))
    (if proc
        (apply proc param args)
        &all-effects)))

(define (expression-effects exp)
  (match exp
    ((or ($ $const) ($ $prim) ($ $values) ($ $code) ($ $const-fun))
     &no-effects)
    ((or ($ $fun) ($ $rec))
     (&allocate &unknown-memory-kinds))
    ((or ($ $call) ($ $callk) ($ $calli))
     &all-effects)
    (($ $primcall name param args)
     (primitive-effects param name args))))

(define (compute-effects conts)
  (intmap-map
   (lambda (label cont)
     (match cont
       (($ $kargs names syms ($ $continue k src exp))
        (expression-effects exp))
       (($ $kargs names syms ($ $branch kf kt src op param args))
        (primitive-effects param op args))
       (($ $kargs names syms ($ $switch)) &no-effects)
       (($ $kargs names syms ($ $prompt))
        ;; Although the "main" path just writes &prompt, we don't know
        ;; what nonlocal predecessors of the handler do, so we
        ;; conservatively assume &all-effects.
        &all-effects)
       (($ $kargs names syms ($ $throw))
        ;; A reachable "throw" term can never be elided.
        &all-effects)
       (($ $kreceive arity kargs)
        (match arity
          (($ $arity _ () #f () #f) &type-check)
          (($ $arity () () _ () #f) (&allocate &pair))
          (($ $arity _ () _ () #f) (logior (&allocate &pair) &type-check))))
       (($ $kfun) &type-check)
       (($ $kclause) &type-check)
       (($ $ktail) &no-effects)))
   conts))

;; There is a way to abuse effects analysis in CSE to also do scalar
;; replacement, effectively adding `car' and `cdr' expressions to `cons'
;; expressions, and likewise with other constructors and setters.  This
;; routine adds appropriate effects to `cons' and `set-car!' and the
;; like.
;;
;; This doesn't affect CSE's ability to eliminate expressions, given
;; that allocations aren't eliminated anyway, and the new effects will
;; just cause the allocations not to commute with e.g. set-car!  which
;; is what we want anyway.
(define (synthesize-definition-effects effects)
  (intmap-map (lambda (label fx)
                (if (logtest (logior &write &allocation) fx)
                    (logior fx &read)
                    fx))
              effects))
