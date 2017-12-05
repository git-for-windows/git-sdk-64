;;; Effects analysis on CPS

;; Copyright (C) 2011-2015, 2017 Free Software Foundation, Inc.

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
            &car
            &cdr
            &vector
            &box
            &module
            &struct
            &string
            &thread
            &bytevector
            &closure

            &object
            &field

            &allocate
            &read-object
            &read-field
            &write-object
            &write-field

            &no-effects
            &all-effects

            exclude-effects
            effect-free?
            constant?
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
  &closure)

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

(define-inlinable (constant? effects)
  (zero? effects))

(define-inlinable (causes-effect? x effects)
  (not (zero? (logand x effects))))

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
  (and (not (zero? (logand a &write)))
       (not (zero? (logand b (logior &read &write))))
       (locations-same?)))

(define (compute-clobber-map effects)
  "For the map LABEL->EFFECTS, compute a map LABEL->LABELS indicating
the LABELS that are clobbered by the effects of LABEL."
  (let ((clobbered-by-write (make-hash-table)))
    (intmap-fold
     (lambda (label fx)
       ;; Unless an expression causes a read, it isn't clobbered by
       ;; anything.
       (when (causes-effect? fx &read)
         (let ((me (intset label)))
           (define (add! kind field)
             (let* ((k (logior (ash field &memory-kind-bits) kind))
                    (clobber (hashv-ref clobbered-by-write k empty-intset)))
               (hashv-set! clobbered-by-write k (intset-union me clobber))))
           ;; Clobbered by write to specific field of this memory
           ;; kind, write to any field of this memory kind, or
           ;; write to any field of unknown memory kinds.
           (let* ((loc (ash fx (- &effect-kind-bits)))
                  (kind (logand loc &memory-kind-mask))
                  (field (ash loc (- &memory-kind-bits))))
             (add! kind field)
             (add! kind -1)
             (add! &unknown-memory-kinds -1))))
       (values))
     effects)
    (intmap-map (lambda (label fx)
                  (if (causes-effect? fx &write)
                      (hashv-ref clobbered-by-write
                                 (ash fx (- &effect-kind-bits))
                                 empty-intset)
                      empty-intset))
                effects)))

(define-inlinable (indexed-field kind var constants)
  (let ((val (intmap-ref constants var (lambda (_) #f))))
    (if (and (exact-integer? val) (<= 0 val))
        (&field kind val)
        (&object kind))))

(define *primitive-effects* (make-hash-table))

(define-syntax-rule (define-primitive-effects* constants
                      ((name . args) effects ...)
                      ...)
  (begin
    (hashq-set! *primitive-effects* 'name
                (case-lambda*
                 ((constants . args) (logior effects ...))
                 (_ &all-effects)))
    ...))

(define-syntax-rule (define-primitive-effects ((name . args) effects ...) ...)
  (define-primitive-effects* constants ((name . args) effects ...) ...))

;; Miscellaneous.
(define-primitive-effects
  ((values . _)))

;; Generic effect-free predicates.
(define-primitive-effects
  ((eq? . _))
  ((eqv? . _))
  ((equal? . _))
  ((pair? arg))
  ((null? arg))
  ((nil? arg ))
  ((symbol? arg))
  ((variable? arg))
  ((vector? arg))
  ((struct? arg))
  ((string? arg))
  ((number? arg))
  ((char? arg))
  ((bytevector? arg))
  ((keyword? arg))
  ((bitvector? arg))
  ((procedure? arg))
  ((thunk? arg)))

;; Fluids.
(define-primitive-effects
  ((fluid-ref f)                   (&read-object &fluid)       &type-check)
  ((fluid-set! f v)                (&write-object &fluid)      &type-check)
  ((push-fluid f v)                (&write-object &fluid)      &type-check)
  ((pop-fluid)                     (&write-object &fluid))
  ((push-dynamic-state state)      (&write-object &fluid)      &type-check)
  ((pop-dynamic-state)             (&write-object &fluid)))

;; Threads.  Calls cause &all-effects, which reflects the fact that any
;; call can capture a partial continuation and reinstate it on another
;; thread.
(define-primitive-effects
  ((current-thread)                (&read-object &thread)))

;; Prompts.
(define-primitive-effects
  ((make-prompt-tag #:optional arg) (&allocate &unknown-memory-kinds)))

;; Pairs.
(define-primitive-effects
  ((cons a b)                      (&allocate &pair))
  ((list . _)                      (&allocate &pair))
  ((car x)                         (&read-field &pair 0)       &type-check)
  ((set-car! x y)                  (&write-field &pair 0)      &type-check)
  ((cdr x)                         (&read-field &pair 1)       &type-check)
  ((set-cdr! x y)                  (&write-field &pair 1)      &type-check)
  ((memq x y)                      (&read-object &pair)        &type-check)
  ((memv x y)                      (&read-object &pair)        &type-check)
  ((list? arg)                     (&read-field &pair 1))
  ((length l)                      (&read-field &pair 1)       &type-check))

;; Variables.
(define-primitive-effects
  ((box v)                         (&allocate &box))
  ((box-ref v)                     (&read-object &box)         &type-check)
  ((box-set! v x)                  (&write-object &box)        &type-check))

;; Vectors.
(define (vector-field n constants)
  (indexed-field &vector n constants))
(define (read-vector-field n constants)
  (logior &read (vector-field n constants)))
(define (write-vector-field n constants)
  (logior &write (vector-field n constants)))
(define-primitive-effects* constants
  ((vector . _)                    (&allocate &vector))
  ((make-vector n init)            (&allocate &vector))
  ((make-vector/immediate n init)  (&allocate &vector))
  ((vector-ref v n)                (read-vector-field n constants) &type-check)
  ((vector-ref/immediate v n)      (read-vector-field n constants) &type-check)
  ((vector-set! v n x)             (write-vector-field n constants) &type-check)
  ((vector-set!/immediate v n x)   (write-vector-field n constants) &type-check)
  ((vector-length v)                                           &type-check))

;; Structs.
(define (struct-field n constants)
  (indexed-field &struct n constants))
(define (read-struct-field n constants)
  (logior &read (struct-field n constants)))
(define (write-struct-field n constants)
  (logior &write (struct-field n constants)))
(define-primitive-effects* constants
  ((allocate-struct vt n)          (&allocate &struct)         &type-check)
  ((allocate-struct/immediate v n) (&allocate &struct)         &type-check)
  ((make-struct/no-tail vt . _)    (&allocate &struct)         &type-check)
  ((struct-ref s n)                (read-struct-field n constants) &type-check)
  ((struct-ref/immediate s n)      (read-struct-field n constants) &type-check)
  ((struct-set! s n x)             (write-struct-field n constants) &type-check)
  ((struct-set!/immediate s n x)   (write-struct-field n constants) &type-check)
  ((struct-vtable s)                                           &type-check))

;; Strings.
(define-primitive-effects
  ((string-ref s n)                (&read-object &string)      &type-check)
  ((string-set! s n c)             (&write-object &string)     &type-check)
  ((number->string _)              (&allocate &string)         &type-check)
  ((string->number _)              (&read-object &string)      &type-check)
  ((string-length s)                                           &type-check))

;; Unboxed floats and integers.
(define-primitive-effects
  ((scm->f64 _)                                                &type-check)
  ((load-f64 _))
  ((f64->scm _))
  ((scm->u64 _)                                                &type-check)
  ((scm->u64/truncate _)                                       &type-check)
  ((load-u64 _))
  ((u64->scm _))
  ((scm->s64 _)                                                &type-check)
  ((load-s64 _))
  ((s64->scm _)))

;; Bytevectors.
(define-primitive-effects
  ((bv-length _)                                               &type-check)

  ((bv-u8-ref bv n)                (&read-object &bytevector)  &type-check)
  ((bv-s8-ref bv n)                (&read-object &bytevector)  &type-check)
  ((bv-u16-ref bv n)               (&read-object &bytevector)  &type-check)
  ((bv-s16-ref bv n)               (&read-object &bytevector)  &type-check)
  ((bv-u32-ref bv n)               (&read-object &bytevector)  &type-check)
  ((bv-s32-ref bv n)               (&read-object &bytevector)  &type-check)
  ((bv-u64-ref bv n)               (&read-object &bytevector)  &type-check)
  ((bv-s64-ref bv n)               (&read-object &bytevector)  &type-check)
  ((bv-f32-ref bv n)               (&read-object &bytevector)  &type-check)
  ((bv-f64-ref bv n)               (&read-object &bytevector)  &type-check)

  ((bv-u8-set! bv n x)             (&write-object &bytevector) &type-check)
  ((bv-s8-set! bv n x)             (&write-object &bytevector) &type-check)
  ((bv-u16-set! bv n x)            (&write-object &bytevector) &type-check)
  ((bv-s16-set! bv n x)            (&write-object &bytevector) &type-check)
  ((bv-u32-set! bv n x)            (&write-object &bytevector) &type-check)
  ((bv-s32-set! bv n x)            (&write-object &bytevector) &type-check)
  ((bv-u64-set! bv n x)            (&write-object &bytevector) &type-check)
  ((bv-s64-set! bv n x)            (&write-object &bytevector) &type-check)
  ((bv-f32-set! bv n x)            (&write-object &bytevector) &type-check)
  ((bv-f64-set! bv n x)            (&write-object &bytevector) &type-check))

;; Closures.
(define (closure-field n constants)
  (indexed-field &closure n constants))
(define (read-closure-field n constants)
  (logior &read (closure-field n constants)))
(define (write-closure-field n constants)
  (logior &write (closure-field n constants)))
(define-primitive-effects* constants
  ((free-ref closure idx)          (read-closure-field idx constants))
  ((free-set! closure idx val)     (write-closure-field idx constants)))

;; Modules.
(define-primitive-effects
  ((current-module)                (&read-object &module))
  ((cache-current-module! m scope) (&write-object &box))
  ((resolve name bound?)           (&read-object &module)      &type-check)
  ((cached-toplevel-box scope name bound?)                     &type-check)
  ((cached-module-box mod name public? bound?)                 &type-check)
  ((define! name)                  (&read-object &module)))

;; Numbers.
(define-primitive-effects
  ((= . _)                         &type-check)
  ((< . _)                         &type-check)
  ((> . _)                         &type-check)
  ((<= . _)                        &type-check)
  ((>= . _)                        &type-check)
  ((u64-= . _))
  ((u64-< . _))
  ((u64-> . _))
  ((u64-<= . _))
  ((u64->= . _))
  ((u64-<-scm . _)                 &type-check)
  ((u64-<=-scm . _)                &type-check)
  ((u64-=-scm . _)                 &type-check)
  ((u64->=-scm . _)                 &type-check)
  ((u64->-scm . _)                 &type-check)
  ((f64-= . _))
  ((f64-< . _))
  ((f64-> . _))
  ((f64-<= . _))
  ((f64->= . _))
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
  ((quo . _)                       &type-check)
  ((rem . _)                       &type-check)
  ((mod . _)                       &type-check)
  ((complex? _)                    &type-check)
  ((real? _)                       &type-check)
  ((rational? _)                   &type-check)
  ((inf? _)                        &type-check)
  ((nan? _)                        &type-check)
  ((integer? _)                    &type-check)
  ((exact? _)                      &type-check)
  ((inexact? _)                    &type-check)
  ((even? _)                       &type-check)
  ((odd? _)                        &type-check)
  ((ash n m)                       &type-check)
  ((logand . _)                    &type-check)
  ((logior . _)                    &type-check)
  ((logxor . _)                    &type-check)
  ((logsub . _)                    &type-check)
  ((lognot . _)                    &type-check)
  ((ulogand . _))
  ((ulogior . _))
  ((ulogxor . _))
  ((ulogsub . _))
  ((ursh . _))
  ((ulsh . _))
  ((ursh/immediate . _))
  ((ulsh/immediate . _))
  ((logtest a b)                   &type-check)
  ((logbit? a b)                   &type-check)
  ((sqrt _)                        &type-check)
  ((abs _)                         &type-check))

;; Characters.
(define-primitive-effects
  ((integer->char _)               &type-check)
  ((char->integer _)               &type-check))

;; Atomics are a memory and a compiler barrier; they cause all effects
;; so no need to have a case for them here.  (Though, see
;; https://jfbastien.github.io/no-sane-compiler/.)

(define (primitive-effects constants name args)
  (let ((proc (hashq-ref *primitive-effects* name)))
    (if proc
        (apply proc constants args)
        &all-effects)))

(define (expression-effects exp constants)
  (match exp
    ((or ($ $const) ($ $prim) ($ $values))
     &no-effects)
    (($ $closure _ 0)
     &no-effects)
    ((or ($ $fun) ($ $rec) ($ $closure))
     (&allocate &unknown-memory-kinds))
    (($ $prompt)
     ;; Although the "main" path just writes &prompt, we don't know what
     ;; nonlocal predecessors of the handler do, so we conservatively
     ;; assume &all-effects.
     &all-effects)
    ((or ($ $call) ($ $callk))
     &all-effects)
    (($ $branch k exp)
     (expression-effects exp constants))
    (($ $primcall name args)
     (primitive-effects constants name args))))

(define (compute-effects conts)
  (let ((constants (compute-constant-values conts)))
    (intmap-map
     (lambda (label cont)
       (match cont
         (($ $kargs names syms ($ $continue k src exp))
          (expression-effects exp constants))
         (($ $kreceive arity kargs)
          (match arity
            (($ $arity _ () #f () #f) &type-check)
            (($ $arity () () _ () #f) (&allocate &pair))
            (($ $arity _ () _ () #f) (logior (&allocate &pair) &type-check))))
         (($ $kfun) &type-check)
         (($ $kclause) &type-check)
         (($ $ktail) &no-effects)))
     conts)))

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
