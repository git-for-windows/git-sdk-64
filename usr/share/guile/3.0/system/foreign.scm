;;; Copyright (C) 2010-2011,2013-2014,2024 Free Software Foundation, Inc.
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


(define-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (system base target)
  #:export (void
            float double
            complex-float complex-double
            short
            unsigned-short
            int unsigned-int long unsigned-long size_t ssize_t ptrdiff_t
            int8 uint8
            uint16 int16
            uint32 int32
            uint64 int64
            intptr_t uintptr_t

            sizeof alignof

            %null-pointer
            null-pointer?
            pointer?
            make-pointer
            pointer->scm
            scm->pointer
            pointer-address

            pointer->bytevector
            bytevector->pointer
            set-pointer-finalizer!

            dereference-pointer
            string->pointer
            pointer->string

            pointer->procedure
            ;; procedure->pointer (see below)

            read-c-struct write-c-struct
            make-c-struct parse-c-struct

            define-wrapped-pointer-type))

(eval-when (expand load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_foreign"))


;;;
;;; Pointers.
;;;

(define (null-pointer? pointer)
  "Return true if POINTER is the null pointer."
  (= (pointer-address pointer) 0))

(if (defined? 'procedure->pointer)
    (export procedure->pointer))


;;;
;;; Structures.
;;;

(define-syntax compile-time-eval
  (lambda (stx)
    "Evaluate the target-dependent expression EXP at compile-time if we are
not cross-compiling; otherwise leave it to be evaluated at run-time."
    (syntax-case stx ()
      ((_ exp)
       (if (equal? (target-type) %host-type)
           #`(quote
              #,(datum->syntax #'here
                               (primitive-eval (syntax->datum #'exp))))
           #'exp)))))

;; Note that in a cross-compiled Guile, the host and the target may have
;; different values of, say, `long'.  However the explicitly-sized types
;; int8, float, etc have the same value on all platforms.  sizeof on
;; these types is also a target-invariant primitive.  alignof is notably
;; *not* target-invariant.

(define-syntax switch/compile-time-keys
  (syntax-rules (else)
    ((_ x (k expr) ... (else alt))
     (let ((t x))
       (cond
        ((eq? t (compile-time-eval k)) expr)
        ...
        (else alt))))))

(define-syntax-rule (align off alignment)
  (1+ (logior (1- off) (1- alignment))))

(define bytevector-pointer-ref
  (case (sizeof '*)
    ((8) (lambda (bv offset)
           (make-pointer (bytevector-u64-native-ref bv offset))))
    ((4) (lambda (bv offset)
           (make-pointer (bytevector-u32-native-ref bv offset))))
    (else (error "what machine is this?"))))

(define bytevector-pointer-set!
  (case (sizeof '*)
    ((8) (lambda (bv offset ptr)
           (bytevector-u64-native-set! bv offset (pointer-address ptr))))
    ((4) (lambda (bv offset ptr)
           (bytevector-u32-native-set! bv offset (pointer-address ptr))))
    (else (error "what machine is this?"))))

(define-syntax-rule (define-complex-accessors (read write) (%read %write size))
  (begin
    (define (read bv offset)
      (make-rectangular
       (%read bv offset)
       (%read bv (+ offset size))))
    (define (write bv offset val)
      (%write bv offset (real-part val))
      (%write bv (+ offset size) (imag-part val)))))

(define-complex-accessors
  (bytevector-complex-single-native-ref bytevector-complex-single-native-set!)
  (bytevector-ieee-single-native-ref bytevector-ieee-single-native-set! 4))

(define-complex-accessors
  (bytevector-complex-double-native-ref bytevector-complex-double-native-set!)
  (bytevector-ieee-double-native-ref bytevector-ieee-double-native-set! 8))

(define-syntax-rule (read-field %bv %offset %type)
  (let ((bv %bv)
        (offset %offset)
        (type %type))
    (define-syntax-rule (%read type reader)
      (let* ((offset (align offset (compile-time-eval (alignof type))))
             (val (reader bv offset)))
        (values val
                (+ offset (compile-time-eval (sizeof type))))))
    (define-syntax-rule (dispatch-read type (%%type reader) (... ...))
      (switch/compile-time-keys
       type
       (%%type (%read %%type reader))
       (... ...)
       (else
        (let ((offset (align offset (alignof type))))
          (values (%read-c-struct bv offset type)
                  (+ offset (sizeof type)))))))
    (dispatch-read
     type
     (int8 bytevector-s8-ref)
     (uint8 bytevector-u8-ref)
     (int16 bytevector-s16-native-ref)
     (uint16 bytevector-u16-native-ref)
     (int32 bytevector-s32-native-ref)
     (uint32 bytevector-u32-native-ref)
     (int64 bytevector-s64-native-ref)
     (uint64 bytevector-u64-native-ref)
     (float bytevector-ieee-single-native-ref)
     (double bytevector-ieee-double-native-ref)
     (complex-float bytevector-complex-single-native-ref)
     (complex-double bytevector-complex-double-native-ref)
     ('* bytevector-pointer-ref))))

(define-syntax-rule (read-c-struct %bv %offset ((field type) ...) k)
  (let ((bv %bv)
        (offset %offset)
        (size (compile-time-eval (sizeof (list type ...)))))
    (unless (<= (bytevector-length bv) (+ offset size))
      (error "destination bytevector too small"))
    (let*-values (((field offset)
                   (read-field bv offset (compile-time-eval type)))
                  ...)
      (k field ...))))

(define-syntax-rule (write-field %bv %offset %type %value)
  (let ((bv %bv)
        (offset %offset)
        (type %type)
        (value %value))
    (define-syntax-rule (%write type writer)
      (let ((offset (align offset (compile-time-eval (alignof type)))))
        (writer bv offset value)
        (+ offset (compile-time-eval (sizeof type)))))
    (define-syntax-rule (dispatch-write type (%%type writer) (... ...))
      (switch/compile-time-keys
       type
       (%%type (%write %%type writer))
       (... ...)
       (else
        (let ((offset (align offset (alignof type))))
          (%write-c-struct bv offset type value)
          (+ offset (sizeof type))))))
    (dispatch-write
     type
     (int8 bytevector-s8-set!)
     (uint8 bytevector-u8-set!)
     (int16 bytevector-s16-native-set!)
     (uint16 bytevector-u16-native-set!)
     (int32 bytevector-s32-native-set!)
     (uint32 bytevector-u32-native-set!)
     (int64 bytevector-s64-native-set!)
     (uint64 bytevector-u64-native-set!)
     (float bytevector-ieee-single-native-set!)
     (double bytevector-ieee-double-native-set!)
     (complex-float bytevector-complex-single-native-set!)
     (complex-double bytevector-complex-double-native-set!)
     ('* bytevector-pointer-set!))))

(define-syntax-rule (write-c-struct %bv %offset ((field type) ...))
  (let ((bv %bv)
        (offset %offset)
        (size (compile-time-eval (sizeof (list type ...)))))
    (unless (<= (bytevector-length bv) (+ offset size))
      (error "destination bytevector too small"))
    (let* ((offset (write-field bv offset (compile-time-eval type) field))
           ...)
      (values))))

;; Same as write-c-struct, but with run-time dispatch.
(define (%write-c-struct bv offset types vals)
  (let lp ((offset offset) (types types) (vals vals))
    (match types
      (() (match vals
            (() #t)
            (_ (error "too many values" vals))))
      ((type . types)
       (match vals
         ((val . vals)
          (lp (write-field bv offset type val) types vals))
         (() (error "too few values" vals)))))))

;; Same as read-c-struct, but with run-time dispatch.
(define (%read-c-struct bv offset types)
  (let lp ((offset offset) (types types))
    (match types
      (() '())
      ((type . types)
       (call-with-values (lambda () (read-field bv offset type))
         (lambda (val offset)
           (cons val (lp offset types))))))))

(define (make-c-struct types vals)
  (let ((bv (make-bytevector (sizeof types) 0)))
    (%write-c-struct bv 0 types vals)
    (bytevector->pointer bv)))

(define (parse-c-struct foreign types)
  (%read-c-struct (pointer->bytevector foreign (sizeof types)) 0 types))


;;;
;;; Wrapped pointer types.
;;;

(define-syntax define-wrapped-pointer-type
  (lambda (stx)
    "Define helper procedures to wrap pointer objects into Scheme
objects with a disjoint type.  Specifically, this macro defines PRED, a
predicate for the new Scheme type, WRAP, a procedure that takes a
pointer object and returns an object that satisfies PRED, and UNWRAP
which does the reverse.  PRINT must name a user-defined object printer."
    (syntax-case stx ()
      ((_ type-name pred wrap unwrap print)
       (with-syntax ((%wrap (datum->syntax #'wrap (gensym "wrap"))))
         #'(begin
             (define-record-type type-name
               (%wrap pointer)
               pred
               (pointer unwrap))
             (define wrap
               ;; Use a weak hash table to preserve pointer identity, i.e.,
               ;; PTR1 == PTR2 <-> (eq? (wrap PTR1) (wrap PTR2)).
               (let ((ptr->obj (make-weak-value-hash-table 3000)))
                 (lambda (ptr)
                   (or (hash-ref ptr->obj ptr)
                       (let ((o (%wrap ptr)))
                         (hash-set! ptr->obj ptr o)
                         o)))))
             (set-record-type-printer! type-name print)))))))
