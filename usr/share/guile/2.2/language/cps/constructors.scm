;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.

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
;;; Constructor inlining turns "list" primcalls into a series of conses,
;;; and does similar transformations for "vector".
;;;
;;; Code:

(define-module (language cps constructors)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps intmap)
  #:export (inline-constructors))

(define (inline-list out k src args)
  (define (build-list out args k)
    (match args
      (()
       (with-cps out
         (build-term ($continue k src ($const '())))))
      ((arg . args)
       (with-cps out
         (letv tail)
         (letk ktail ($kargs ('tail) (tail)
                       ($continue k src
                         ($primcall 'cons (arg tail)))))
         ($ (build-list args ktail))))))
  (with-cps out
    (letv val)
    (letk kvalues ($kargs ('val) (val)
                    ($continue k src
                      ($primcall 'values (val)))))
    ($ (build-list args kvalues))))

(define (inline-vector out k src args)
  (define (initialize out vec args n)
    (match args
      (()
       (with-cps out
         (build-term ($continue k src ($primcall 'values (vec))))))
      ((arg . args)
       (with-cps out
         (let$ next (initialize vec args (1+ n)))
         (letk knext ($kargs () () ,next))
         (letv u64)
         (letk kunbox ($kargs ('idx) (u64)
                        ($continue knext src
                          ($primcall 'vector-set! (vec u64 arg)))))
         ($ (with-cps-constants ((idx n))
              (build-term ($continue kunbox src
                            ($primcall 'scm->u64 (idx))))))))))
  (with-cps out
    (letv vec)
    (let$ body (initialize vec args 0))
    (letk kalloc ($kargs ('vec) (vec) ,body))
    ($ (with-cps-constants ((len (length args))
                            (init #f))
         (letv u64)
         (letk kunbox ($kargs ('len) (u64)
                        ($continue kalloc src
                          ($primcall 'make-vector (u64 init)))))
         (build-term ($continue kunbox src
                       ($primcall 'scm->u64 (len))))))))

(define (find-constructor-inliner name)
  (match name
    ('list inline-list)
    ('vector inline-vector)
    (_ #f)))

(define (inline-constructors conts)
  (with-fresh-name-state conts
    (persistent-intmap
     (intmap-fold
      (lambda (label cont out)
        (match cont
          (($ $kargs names vars ($ $continue k src ($ $primcall name args)))
           (let ((inline (find-constructor-inliner name)))
             (if inline
                 (call-with-values (lambda () (inline out k src args))
                   (lambda (out term)
                     (intmap-replace! out label
                                      (build-cont ($kargs names vars ,term)))))
                 out)))
          (_ out)))
      conts
      conts))))
