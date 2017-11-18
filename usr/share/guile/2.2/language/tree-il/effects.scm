;;; Effects analysis on Tree-IL

;; Copyright (C) 2011, 2012, 2013 Free Software Foundation, Inc.

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

(define-module (language tree-il effects)
  #:use-module (language tree-il)
  #:use-module (language tree-il primitives)
  #:use-module (ice-9 match)
  #:export (make-effects-analyzer
            &mutable-lexical
            &toplevel
            &fluid
            &definite-bailout
            &possible-bailout
            &zero-values
            &allocation
            &type-check
            &all-effects
            effects-commute?
            exclude-effects
            effect-free?
            constant?
            depends-on-effects?
            causes-effects?))

;;;
;;; Hey, it's some effects analysis!  If you invoke
;;; `make-effects-analyzer', you get a procedure that computes the set
;;; of effects that an expression depends on and causes.  This
;;; information is useful when writing algorithms that move code around,
;;; while preserving the semantics of an input program.
;;;
;;; The effects set is represented by a bitfield, as a fixnum.  The set
;;; of possible effects is modelled rather coarsely.  For example, a
;;; toplevel reference to FOO is modelled as depending on the &toplevel
;;; effect, and causing a &type-check effect.  If any intervening code
;;; sets any toplevel variable, that will block motion of FOO.
;;;
;;; For each effect, two bits are reserved: one to indicate that an
;;; expression depends on the effect, and the other to indicate that an
;;; expression causes the effect.
;;;
;;; Since we have more bits in a fixnum on 64-bit systems, we can be
;;; more precise without losing efficiency.  On a 32-bit system, some of
;;; the more precise effects map to fewer bits.
;;;

(define-syntax define-effects
  (lambda (x)
    (syntax-case x ()
      ((_ all name ...)
       (with-syntax (((n ...) (iota (length #'(name ...)))))
         #'(begin
             (define-syntax name (identifier-syntax (ash 1 (* n 2))))
             ...
             (define-syntax all (identifier-syntax (logior name ...)))))))))

(define-syntax compile-time-cond
  (lambda (x)
    (syntax-case x (else)
      ((_ (else body ...))
       #'(begin body ...))
      ((_ (exp body ...) clause ...)
       (if (eval (syntax->datum #'exp) (current-module))
           #'(begin body ...)
           #'(compile-time-cond clause ...))))))

;; Here we define the effects, indicating the meaning of the effect.
;;
;; Effects that are described in a "depends on" sense can also be used
;; in the "causes" sense.
;;
;; Effects that are described as causing an effect are not usually used
;; in a "depends-on" sense.  Although the "depends-on" sense is used
;; when checking for the existence of the "causes" effect, the effects
;; analyzer will not associate the "depends-on" sense of these effects
;; with any expression.
;;
(compile-time-cond
 ((>= (logcount most-positive-fixnum) 60)
  (define-effects &all-effects
    ;; Indicates that an expression depends on the value of a mutable
    ;; lexical variable.
    &mutable-lexical

    ;; Indicates that an expression depends on the value of a toplevel
    ;; variable.
    &toplevel

    ;; Indicates that an expression depends on the value of a fluid
    ;; variable.
    &fluid

    ;; Indicates that an expression definitely causes a non-local,
    ;; non-resumable exit -- a bailout.  Only used in the "changes" sense.
    &definite-bailout

    ;; Indicates that an expression may cause a bailout.
    &possible-bailout

    ;; Indicates than an expression may return zero values -- a "causes"
    ;; effect.
    &zero-values

    ;; Indicates that an expression may return a fresh object -- a
    ;; "causes" effect.
    &allocation

    ;; Indicates that an expression depends on the value of the car of a
    ;; pair.
    &car

    ;; Indicates that an expression depends on the value of the cdr of a
    ;; pair.
    &cdr

    ;; Indicates that an expression depends on the value of a vector
    ;; field.  We cannot be more precise, as vectors may alias other
    ;; vectors.
    &vector

    ;; Indicates that an expression depends on the value of a variable
    ;; cell.
    &variable

    ;; Indicates that an expression depends on the value of a particular
    ;; struct field.
    &struct-0 &struct-1 &struct-2 &struct-3 &struct-4 &struct-5 &struct-6+

    ;; Indicates that an expression depends on the contents of a string.
    &string

    ;; Indicates that an expression depends on the contents of a
    ;; bytevector.  We cannot be more precise, as bytevectors may alias
    ;; other bytevectors.
    &bytevector

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
    &type-check)

  ;; Indicates that an expression depends on the contents of an unknown
  ;; struct field.
  (define-syntax &struct
    (identifier-syntax
     (logior &struct-1 &struct-2 &struct-3 &struct-4 &struct-5 &struct-6+))))

 (else
  ;; For systems with smaller fixnums, be less precise regarding struct
  ;; fields.
  (define-effects &all-effects
    &mutable-lexical
    &toplevel
    &fluid
    &definite-bailout
    &possible-bailout
    &zero-values
    &allocation
    &car
    &cdr
    &vector
    &variable
    &struct
    &string
    &bytevector
    &type-check)
  (define-syntax &struct-0 (identifier-syntax &struct))
  (define-syntax &struct-1 (identifier-syntax &struct))
  (define-syntax &struct-2 (identifier-syntax &struct))
  (define-syntax &struct-3 (identifier-syntax &struct))
  (define-syntax &struct-4 (identifier-syntax &struct))
  (define-syntax &struct-5 (identifier-syntax &struct))
  (define-syntax &struct-6+ (identifier-syntax &struct))))

(define-syntax &no-effects (identifier-syntax 0))

;; Definite bailout is an oddball effect.  Since it indicates that an
;; expression definitely causes bailout, it's not in the set of effects
;; of a call to an unknown procedure.  At the same time, it's also
;; special in that a definite bailout in a subexpression doesn't always
;; cause an outer expression to include &definite-bailout in its
;; effects.  For that reason we have to treat it specially.
;;
(define-syntax &all-effects-but-bailout
  (identifier-syntax
   (logand &all-effects (lognot &definite-bailout))))

(define-inlinable (cause effect)
  (ash effect 1))

(define-inlinable (&depends-on a)
  (logand a &all-effects))
(define-inlinable (&causes a)
  (logand a (cause &all-effects)))

(define (exclude-effects effects exclude)
  (logand effects (lognot (cause exclude))))
(define (effect-free? effects)
  (zero? (&causes effects)))
(define (constant? effects)
  (zero? effects))

(define-inlinable (depends-on-effects? x effects)
  (not (zero? (logand (&depends-on x) effects))))
(define-inlinable (causes-effects? x effects)
  (not (zero? (logand (&causes x) (cause effects)))))

(define-inlinable (effects-commute? a b)
  (and (not (causes-effects? a (&depends-on b)))
       (not (causes-effects? b (&depends-on a)))))

(define (make-effects-analyzer assigned-lexical?)
  "Returns a procedure of type EXP -> EFFECTS that analyzes the effects
of an expression."

  (let ((cache (make-hash-table)))
    (define* (compute-effects exp #:optional (lookup (lambda (x) #f)))
      (define (compute-effects exp)
        (or (hashq-ref cache exp)
            (let ((effects (visit exp)))
              (hashq-set! cache exp effects)
              effects)))

      (define (accumulate-effects exps)
        (let lp ((exps exps) (out &no-effects))
          (if (null? exps)
              out
              (lp (cdr exps) (logior out (compute-effects (car exps)))))))

      (define (visit exp)
        (match exp
          (($ <const>)
           &no-effects)
          (($ <void>)
           &no-effects)
          (($ <lexical-ref> _ _ gensym)
           (if (assigned-lexical? gensym)
               &mutable-lexical
               &no-effects))
          (($ <lexical-set> _ name gensym exp)
           (logior (cause &mutable-lexical)
                   (compute-effects exp)))
          (($ <let> _ names gensyms vals body)
           (logior (if (or-map assigned-lexical? gensyms)
                       (cause &allocation)
                       &no-effects)
                   (accumulate-effects vals)
                   (compute-effects body)))
          (($ <letrec> _ in-order? names gensyms vals body)
           (logior (if (or-map assigned-lexical? gensyms)
                       (cause &allocation)
                       &no-effects)
                   (accumulate-effects vals)
                   (compute-effects body)))
          (($ <fix> _ names gensyms vals body)
           (logior (if (or-map assigned-lexical? gensyms)
                       (cause &allocation)
                       &no-effects)
                   (accumulate-effects vals)
                   (compute-effects body)))
          (($ <let-values> _ producer consumer)
           (logior (compute-effects producer)
                   (compute-effects consumer)
                   (cause &type-check)))
          (($ <toplevel-ref>)
           (logior &toplevel
                   (cause &type-check)))
          (($ <module-ref>)
           (logior &toplevel
                   (cause &type-check)))
          (($ <module-set> _ mod name public? exp)
           (logior (cause &toplevel)
                   (cause &type-check)
                   (compute-effects exp)))
          (($ <toplevel-define> _ name exp)
           (logior (cause &toplevel)
                   (compute-effects exp)))
          (($ <toplevel-set> _ name exp)
           (logior (cause &toplevel)
                   (compute-effects exp)))
          (($ <primitive-ref>)
           &no-effects)
          (($ <conditional> _ test consequent alternate)
           (let ((tfx (compute-effects test))
                 (cfx (compute-effects consequent))
                 (afx (compute-effects alternate)))
             (if (causes-effects? (logior tfx (logand afx cfx))
                                  &definite-bailout)
                 (logior tfx cfx afx)
                 (exclude-effects (logior tfx cfx afx)
                                  &definite-bailout))))

          ;; Zero values.
          (($ <primcall> _ 'values ())
           (cause &zero-values))

          ;; Effect-free primitives.
          (($ <primcall> _ (or 'values 'eq? 'eqv? 'equal?) args)
           (accumulate-effects args))

          (($ <primcall> _ (or 'not 'pair? 'null? 'list? 'symbol?
                               'vector? 'struct? 'string? 'number?
                               'char?)
              (arg))
           (compute-effects arg))

          ;; Primitives that allocate memory.
          (($ <primcall> _ 'cons (x y))
           (logior (compute-effects x) (compute-effects y)
                   &allocation))

          (($ <primcall> _ (or 'list 'vector) args)
           (logior (accumulate-effects args) &allocation))

          (($ <primcall> _ 'make-prompt-tag ())
           &allocation)

          (($ <primcall> _ 'make-prompt-tag (arg))
           (logior (compute-effects arg) &allocation))

          (($ <primcall> _ 'fluid-ref (fluid))
           (logior (compute-effects fluid)
                   (cause &type-check)
                   &fluid))

          (($ <primcall> _ 'fluid-set! (fluid exp))
           (logior (compute-effects fluid)
                   (compute-effects exp)
                   (cause &type-check)
                   (cause &fluid)))

          (($ <primcall> _ 'push-fluid (fluid val))
           (logior (compute-effects fluid)
                   (compute-effects val)
                   (cause &type-check)
                   (cause &fluid)))

          (($ <primcall> _ 'pop-fluid ())
           (logior (cause &fluid)))

          (($ <primcall> _ 'push-dynamic-state (state))
           (logior (compute-effects state)
                   (cause &type-check)
                   (cause &fluid)))

          (($ <primcall> _ 'pop-dynamic-state ())
           (logior (cause &fluid)))

          (($ <primcall> _ 'car (x))
           (logior (compute-effects x)
                   (cause &type-check)
                   &car))
          (($ <primcall> _ 'set-car! (x y))
           (logior (compute-effects x)
                   (compute-effects y)
                   (cause &type-check)
                   (cause &car)))

          (($ <primcall> _ 'cdr (x))
           (logior (compute-effects x)
                   (cause &type-check)
                   &cdr))
          (($ <primcall> _ 'set-cdr! (x y))
           (logior (compute-effects x)
                   (compute-effects y)
                   (cause &type-check)
                   (cause &cdr)))

          (($ <primcall> _ (or 'memq 'memv) (x y))
           (logior (compute-effects x)
                   (compute-effects y)
                   (cause &type-check)
                   &car &cdr))

          (($ <primcall> _ 'vector-ref (v n))
           (logior (compute-effects v)
                   (compute-effects n)
                   (cause &type-check)
                   &vector))
          (($ <primcall> _ 'vector-set! (v n x))
           (logior (compute-effects v)
                   (compute-effects n)
                   (compute-effects x)
                   (cause &type-check)
                   (cause &vector)))

          (($ <primcall> _ 'variable-ref (v))
           (logior (compute-effects v)
                   (cause &type-check)
                   &variable))
          (($ <primcall> _ 'variable-set! (v x))
           (logior (compute-effects v)
                   (compute-effects x)
                   (cause &type-check)
                   (cause &variable)))

          (($ <primcall> _ 'struct-ref (s n))
           (logior (compute-effects s)
                   (compute-effects n)
                   (cause &type-check)
                   (match n
                     (($ <const> _ 0) &struct-0)
                     (($ <const> _ 1) &struct-1)
                     (($ <const> _ 2) &struct-2)
                     (($ <const> _ 3) &struct-3)
                     (($ <const> _ 4) &struct-4)
                     (($ <const> _ 5) &struct-5)
                     (($ <const> _ _) &struct-6+)
                     (_ &struct))))
          (($ <primcall> _ 'struct-set! (s n x))
           (logior (compute-effects s)
                   (compute-effects n)
                   (compute-effects x)
                   (cause &type-check)
                   (match n
                     (($ <const> _ 0) (cause &struct-0))
                     (($ <const> _ 1) (cause &struct-1))
                     (($ <const> _ 2) (cause &struct-2))
                     (($ <const> _ 3) (cause &struct-3))
                     (($ <const> _ 4) (cause &struct-4))
                     (($ <const> _ 5) (cause &struct-5))
                     (($ <const> _ _) (cause &struct-6+))
                     (_ (cause &struct)))))

          (($ <primcall> _ 'string-ref (s n))
           (logior (compute-effects s)
                   (compute-effects n)
                   (cause &type-check)
                   &string))
          (($ <primcall> _ 'string-set! (s n c))
           (logior (compute-effects s)
                   (compute-effects n)
                   (compute-effects c)
                   (cause &type-check)
                   (cause &string)))

          (($ <primcall> _
              (or 'bytevector-u8-ref 'bytevector-s8-ref
                  'bytevector-u16-ref 'bytevector-u16-native-ref
                  'bytevector-s16-ref 'bytevector-s16-native-ref
                  'bytevector-u32-ref 'bytevector-u32-native-ref
                  'bytevector-s32-ref 'bytevector-s32-native-ref
                  'bytevector-u64-ref 'bytevector-u64-native-ref
                  'bytevector-s64-ref 'bytevector-s64-native-ref
                  'bytevector-ieee-single-ref 'bytevector-ieee-single-native-ref
                  'bytevector-ieee-double-ref 'bytevector-ieee-double-native-ref)
              (bv n))
           (logior (compute-effects bv)
                   (compute-effects n)
                   (cause &type-check)
                   &bytevector))
          (($ <primcall> _
              (or 'bytevector-u8-set! 'bytevector-s8-set!
                  'bytevector-u16-set! 'bytevector-u16-native-set!
                  'bytevector-s16-set! 'bytevector-s16-native-set!
                  'bytevector-u32-set! 'bytevector-u32-native-set!
                  'bytevector-s32-set! 'bytevector-s32-native-set!
                  'bytevector-u64-set! 'bytevector-u64-native-set!
                  'bytevector-s64-set! 'bytevector-s64-native-set!
                  'bytevector-ieee-single-set! 'bytevector-ieee-single-native-set!
                  'bytevector-ieee-double-set! 'bytevector-ieee-double-native-set!)
              (bv n x))
           (logior (compute-effects bv)
                   (compute-effects n)
                   (compute-effects x)
                   (cause &type-check)
                   (cause &bytevector)))

          ;; Primitives that are normally effect-free, but which might
          ;; cause type checks or allocate memory.  Nota bene,
          ;; primitives that access mutable memory should be given their
          ;; own inline cases above!
          (($ <primcall> _ (and name (? effect-free-primitive?)) args)
           (logior (accumulate-effects args)
                   (cause &type-check)
                   (if (constructor-primitive? name)
                       (cause &allocation)
                       &no-effects)))
      
          ;; Lambda applications might throw wrong-number-of-args.
          (($ <call> _ ($ <lambda> _ _ body) args)
           (logior (accumulate-effects args)
                   (match body
                     (($ <lambda-case> _ req #f #f #f () syms body #f)
                      (logior (compute-effects body)
                              (if (= (length req) (length args))
                                  0
                                  (cause &type-check))))
                     (($ <lambda-case>)
                      (logior (compute-effects body)
                              (cause &type-check)))
                     (#f
                      ;; Calling a case-lambda with no clauses
                      ;; definitely causes bailout.
                      (logior (cause &definite-bailout)
                              (cause &possible-bailout))))))
        
          ;; Bailout primitives.
          (($ <primcall> _ (? bailout-primitive? name) args)
           (logior (accumulate-effects args)
                   (cause &definite-bailout)
                   (cause &possible-bailout)))
          (($ <call> _
              (and proc
                   ($ <module-ref> _ mod name public?)
                   (? (lambda (_)
                        (false-if-exception
                         (procedure-property
                          (module-ref (if public?
                                          (resolve-interface mod)
                                          (resolve-module mod))
                                      name)
                          'definite-bailout?)))))
              args)
           (logior (compute-effects proc)
                   (accumulate-effects args)
                   (cause &definite-bailout)
                   (cause &possible-bailout)))

          ;; A call to a lexically bound procedure, perhaps labels
          ;; allocated.
          (($ <call> _ (and proc ($ <lexical-ref> _ _ sym)) args)
           (cond
            ((lookup sym)
             => (lambda (proc)
                  (compute-effects (make-call #f proc args))))
            (else
             (logior &all-effects-but-bailout
                     (cause &all-effects-but-bailout)))))

          ;; A call to an unknown procedure can do anything.
          (($ <primcall> _ name args)
           (logior &all-effects-but-bailout
                   (cause &all-effects-but-bailout)))
          (($ <call> _ proc args)
           (logior &all-effects-but-bailout
                   (cause &all-effects-but-bailout)))

          (($ <lambda> _ meta body)
           &no-effects)
          (($ <lambda-case> _ req opt rest kw inits gensyms body alt)
           (logior (exclude-effects (accumulate-effects inits)
                                    &definite-bailout)
                   (if (or-map assigned-lexical? gensyms)
                       (cause &allocation)
                       &no-effects)
                   (compute-effects body)
                   (if alt (compute-effects alt) &no-effects)))

          (($ <seq> _ head tail)
           (logior
            ;; Returning zero values to a for-effect continuation is
            ;; not observable.
            (exclude-effects (compute-effects head)
                             (cause &zero-values))
            (compute-effects tail)))

          (($ <prompt> _ escape-only? tag body handler)
           (logior (compute-effects tag)
                   (compute-effects body)
                   (compute-effects handler)))

          (($ <abort> _ tag args tail)
           (logior &all-effects-but-bailout
                   (cause &all-effects-but-bailout)))))

      (compute-effects exp))

    compute-effects))
