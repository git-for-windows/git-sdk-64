;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2015, 2017-2018 Free Software Foundation, Inc.

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
;;; Some bytecode operations can encode an immediate as an operand.
;;; This pass tranforms generic primcalls to these specialized
;;; primcalls, if possible.
;;;
;;; Code:

(define-module (language cps specialize-primcalls)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:export (specialize-primcalls))

(define (compute-defining-expressions conts)
  (define (meet-defining-expressions old new)
    ;; If there are multiple definitions and they are different, punt
    ;; and record #f.
    (if (equal? old new)
        old
        #f))
  (persistent-intmap
   (intmap-fold (lambda (label cont defs)
                  (match cont
                    (($ $kargs _ _ ($ $continue k src exp))
                     (match (intmap-ref conts k)
                       (($ $kargs (_) (var))
                        (intmap-add! defs var exp meet-defining-expressions))
                       (_ defs)))
                    (_ defs)))
                conts
                empty-intmap)))

(define (compute-constant-values conts)
  (let ((defs (compute-defining-expressions conts)))
    (persistent-intmap
     (intmap-fold
      (lambda (var exp out)
        (match exp
          (($ $primcall (or 'load-f64 'load-u64 'load-s64) val ())
           (intmap-add! out var val))
          ;; Punch through type conversions to allow uadd to specialize
          ;; to uadd/immediate.
          (($ $primcall 'scm->f64 #f (val))
           (let ((f64 (intmap-ref out val (lambda (_) #f))))
             (if (and f64 (number? f64) (inexact? f64) (real? f64))
                 (intmap-add! out var f64)
                 out)))
          (($ $primcall (or 'scm->u64 'scm->u64/truncate) #f (val))
           (let ((u64 (intmap-ref out val (lambda (_) #f))))
             (if (and u64 (number? u64) (exact-integer? u64)
                      (<= 0 u64 #xffffFFFFffffFFFF))
                 (intmap-add! out var u64)
                 out)))
          (($ $primcall 'scm->s64 #f (val))
           (let ((s64 (intmap-ref out val (lambda (_) #f))))
             (if (and s64 (number? s64) (exact-integer? s64)
                      (<= (- #x8000000000000000) s64 #x7fffFFFFffffFFFF))
                 (intmap-add! out var s64)
                 out)))
          (_ out)))
      defs
      (intmap-fold (lambda (var exp out)
                     (match exp
                       (($ $const val)
                        (intmap-add! out var val))
                       (_ out)))
                   defs
                   empty-intmap)))))

(define (specialize-primcalls conts)
  (let ((constants (compute-constant-values conts)))
    (define (uint? var)
      (let ((val (intmap-ref constants var (lambda (_) #f))))
        (and (exact-integer? val) (<= 0 val))))
    (define (u64? var)
      (let ((val (intmap-ref constants var (lambda (_) #f))))
        (and (exact-integer? val) (<= 0 val #xffffFFFFffffFFFF))))
    (define (num? var)
      (number? (intmap-ref constants var (lambda (_) #f))))
    (define (s64? var)
      (let ((val (intmap-ref constants var (lambda (_) #f))))
        (and (exact-integer? val)
             (<= (- #x8000000000000000) val #x7fffFFFFffffFFFF))))
    (define (f64? var)
      (let ((val (intmap-ref constants var (lambda (_) #f))))
        (and (number? val) (inexact? val) (real? val))))
    (define (specialize-primcall name param args)
      (define (rename name)
        (build-exp ($primcall name param args)))
      (define-syntax compute-constant
        (syntax-rules ()
          ((_ (c exp) body)
           (let* ((c (intmap-ref constants c)) (c exp)) body))
          ((_ c body) (compute-constant (c c) body))))
      (define-syntax-rule (specialize-case (pat (op c (arg ...))) ...)
        (match (cons name args)
          (pat
           (let* ((param* (intmap-ref constants c))
                  (param (if param (cons param param*) param*)))
             (build-exp ($primcall 'op param (arg ...)))))
          ...
          (_ #f)))
      (specialize-case
        (('allocate-words (? uint? n)) (allocate-words/immediate n ()))
        (('allocate-pointerless-words (? uint? n))
         (allocate-pointerless-words/immediate n ()))
        (('scm-ref o (? uint? i)) (scm-ref/immediate i (o)))
        (('scm-set! o (? uint? i) x) (scm-set!/immediate i (o x)))
        ;; Assume (tail-)pointer-ref/immediate can always be emitted directly.
        (('word-ref o (? uint? i)) (word-ref/immediate i (o)))
        (('word-set! o (? uint? i) x) (word-set!/immediate i (o x)))
        (('add x (? num? y)) (add/immediate y (x)))
        (('add (? num? y) x) (add/immediate y (x)))
        (('sub x (? num? y)) (sub/immediate y (x)))
        (('uadd x (? uint? y)) (uadd/immediate y (x)))
        (('uadd (? uint? y) x) (uadd/immediate y (x)))
        (('usub x (? uint? y)) (usub/immediate y (x)))
        (('umul x (? uint? y)) (umul/immediate y (x)))
        (('umul (? uint? y) x) (umul/immediate y (x)))
        (('scm->f64 (? f64? var)) (load-f64 var ()))
        (('scm->u64 (? u64? var)) (load-u64 var ()))
        (('scm->u64/truncate (? u64? var)) (load-u64 var ()))
        (('scm->s64 (? s64? var)) (load-s64 var ()))
        (('untag-fixnum (? s64? var)) (load-s64 var ()))
        (('untag-char (? u64? var)) (load-u64 var ()))
        ;; FIXME: add support for tagging immediate chars
        ;; (('tag-char (? u64? var)) (load-const var ()))
        ))
    (intmap-map
     (lambda (label cont)
       (match cont
         (($ $kargs names vars ($ $continue k src ($ $primcall name param args)))
          (let ((exp* (specialize-primcall name param args)))
            (if exp*
                (build-cont
                  ($kargs names vars ($continue k src ,exp*)))
                cont)))
         (_ cont)))
     conts)))

;;; Local Variables:
;;; eval: (put 'specialize-case 'scheme-indent-function 0)
;;; End:
