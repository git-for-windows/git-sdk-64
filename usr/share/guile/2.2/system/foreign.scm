;;;; 	Copyright (C) 2010, 2011, 2013 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;


(define-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (void
            float double
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

(define *writers*
  `((,float . ,bytevector-ieee-single-native-set!)
    (,double . ,bytevector-ieee-double-native-set!)
    (,int8 . ,bytevector-s8-set!)
    (,uint8 . ,bytevector-u8-set!)
    (,int16 . ,bytevector-s16-native-set!)
    (,uint16 . ,bytevector-u16-native-set!)
    (,int32 . ,bytevector-s32-native-set!)
    (,uint32 . ,bytevector-u32-native-set!)
    (,int64 . ,bytevector-s64-native-set!)
    (,uint64 . ,bytevector-u64-native-set!)
    (* . ,bytevector-pointer-set!)))

(define *readers*
  `((,float . ,bytevector-ieee-single-native-ref)
    (,double . ,bytevector-ieee-double-native-ref)
    (,int8 . ,bytevector-s8-ref)
    (,uint8 . ,bytevector-u8-ref)
    (,int16 . ,bytevector-s16-native-ref)
    (,uint16 . ,bytevector-u16-native-ref)
    (,int32 . ,bytevector-s32-native-ref)
    (,uint32 . ,bytevector-u32-native-ref)
    (,int64 . ,bytevector-s64-native-ref)
    (,uint64 . ,bytevector-u64-native-ref)
    (* . ,bytevector-pointer-ref)))


(define (align off alignment)
  (1+ (logior (1- off) (1- alignment))))

(define (write-c-struct bv offset types vals)
  (let lp ((offset offset) (types types) (vals vals))
    (cond
     ((not (pair? types))
      (or (null? vals)
          (error "too many values" vals)))
     ((not (pair? vals))
      (error "too few values" types))
     (else
      ;; alignof will error-check
      (let* ((type (car types))
             (offset (align offset (alignof type))))
        (if (pair? type)
            (write-c-struct bv offset (car types) (car vals))
            ((assv-ref *writers* type) bv offset (car vals)))
        (lp (+ offset (sizeof type)) (cdr types) (cdr vals)))))))

(define (read-c-struct bv offset types)
  (let lp ((offset offset) (types types) (vals '()))
    (cond
     ((not (pair? types))
      (reverse vals))
     (else
      ;; alignof will error-check
      (let* ((type (car types))
             (offset (align offset (alignof type))))
        (lp (+ offset (sizeof type)) (cdr types)
            (cons (if (pair? type)
                      (read-c-struct bv offset (car types))
                      ((assv-ref *readers* type) bv offset))
                  vals)))))))

(define (make-c-struct types vals)
  (let ((bv (make-bytevector (sizeof types) 0)))
    (write-c-struct bv 0 types vals)
    (bytevector->pointer bv)))

(define (parse-c-struct foreign types)
  (let ((size (fold (lambda (type total)
                      (+ (sizeof type)
                         (align total (alignof type))))
                    0
                    types)))
    (read-c-struct (pointer->bytevector foreign size) 0 types)))


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
