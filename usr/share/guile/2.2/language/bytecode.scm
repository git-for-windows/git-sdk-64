;;; Bytecode

;; Copyright (C) 2013 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language bytecode)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export (instruction-list
            instruction-arity
            builtin-name->index
            builtin-index->name))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_instructions")
(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_builtins")

(define (compute-instruction-arity name args)
  (define (first-word-arity word)
    (case word
      ((X32) 0)
      ((X8_S24) 1)
      ((X8_F24) 1)
      ((X8_C24) 1)
      ((X8_L24) 1)
      ((X8_S8_I16) 2)
      ((X8_S12_S12) 2)
      ((X8_S12_C12) 2)
      ((X8_C12_C12) 2)
      ((X8_F12_F12) 2)
      ((X8_S8_S8_S8) 3)
      ((X8_S8_S8_C8) 3)
      ((X8_S8_C8_S8) 3)))
  (define (tail-word-arity word)
    (case word
      ((C32) 1)
      ((I32) 1)
      ((A32 AU32 AS32 AF32) 1)
      ((B32 BF32 BS32 BU32) 0)
      ((N32) 1)
      ((R32) 1)
      ((L32) 1)
      ((LO32) 1)
      ((C8_C24) 2)
      ((B1_C7_L24) 3)
      ((B1_X7_S24) 2)
      ((B1_X7_F24) 2)
      ((B1_X7_C24) 2)
      ((B1_X7_L24) 2)
      ((B1_X31) 1)
      ((X8_S24) 1)
      ((X8_F24) 1)
      ((X8_C24) 1)
      ((X8_L24) 1)))
  (match args
    ((arg0 . args)
     (fold (lambda (arg arity)
             (+ (tail-word-arity arg) arity))
           (first-word-arity arg0)
           args))))

(define *macro-instruction-arities*
  '((cache-current-module! . (0 . 2))
    (cached-toplevel-box . (1 . 3))
    (cached-module-box . (1 . 4))))

(define (compute-instruction-arities)
  (let ((table (make-hash-table)))
    (for-each
     (match-lambda
      ;; Put special cases here.
      ((name op '! . args)
       (hashq-set! table name
                   (cons 0 (compute-instruction-arity name args))))
      ((name op '<- . args)
       (hashq-set! table name
                   (cons 1 (1- (compute-instruction-arity name args))))))
     (instruction-list))
    (for-each (match-lambda
               ((name . arity)
                (hashq-set! table name arity)))
              *macro-instruction-arities*)
    table))

(define *instruction-arities* (delay (compute-instruction-arities)))

(define (instruction-arity name)
  (hashq-ref (force *instruction-arities*) name))
