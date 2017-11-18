;;; Extensions to SRFI-9

;; 	Copyright (C) 2010, 2012 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:

;; Extensions to SRFI-9. Fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-1)
  #:use-module (system base ck)
  #:export (set-record-type-printer!
            define-immutable-record-type
            set-field
            set-fields))

(define (set-record-type-printer! type proc)
  "Set PROC as the custom printer for TYPE."
  (struct-set! type vtable-index-printer proc))

(define-syntax-rule (define-immutable-record-type name ctor pred fields ...)
  ((@@ (srfi srfi-9) %define-record-type)
   #t (define-immutable-record-type name ctor pred fields ...)
   name ctor pred fields ...))

(define-syntax-rule (set-field s (getter ...) expr)
  (%set-fields #t (set-field s (getter ...) expr) ()
               s ((getter ...) expr)))

(define-syntax-rule (set-fields s . rest)
  (%set-fields #t (set-fields s . rest) ()
               s . rest))

;;
;; collate-set-field-specs is a helper for %set-fields
;; thats combines all specs with the same head together.
;;
;; For example:
;;
;;   SPECS:  (((a b c) expr1)
;;            ((a d)   expr2)
;;            ((b c)   expr3)
;;            ((c)     expr4))
;;
;;  RESULT:  ((a ((b c) expr1)
;;               ((d)   expr2))
;;            (b ((c)   expr3))
;;            (c (()    expr4)))
;;
(define (collate-set-field-specs specs)
  (define (insert head tail expr result)
    (cond ((find (lambda (tree)
                   (free-identifier=? head (car tree)))
                 result)
           => (lambda (tree)
                `((,head (,tail ,expr)
                         ,@(cdr tree))
                  ,@(delq tree result))))
          (else `((,head (,tail ,expr))
                  ,@result))))
  (with-syntax (((((head . tail) expr) ...) specs))
    (fold insert '() #'(head ...) #'(tail ...) #'(expr ...))))

(define-syntax unknown-getter
  (lambda (x)
    (syntax-case x ()
      ((_ orig-form getter)
       (syntax-violation 'set-fields "unknown getter" #'orig-form #'getter)))))

(define-syntax c-list
  (lambda (x)
    (syntax-case x (quote)
      ((_ s 'v ...)
       #'(ck s '(v ...))))))

(define-syntax c-same-type-check
  (lambda (x)
    (syntax-case x (quote)
      ((_ s 'orig-form '(path ...)
          '(getter0 getter ...)
          '(type0 type ...)
          'on-success)
       (every (lambda (t g)
                (or (free-identifier=? t #'type0)
                    (syntax-violation
                     'set-fields
                     (format #f
                             "\
field paths ~a and ~a require one object to belong to two different record types (~a and ~a)"
                             (syntax->datum #`(path ... #,g))
                             (syntax->datum #'(path ... getter0))
                             (syntax->datum t)
                             (syntax->datum #'type0))
                     #'orig-form)))
              #'(type ...)
              #'(getter ...))
       #'(ck s 'on-success)))))

(define-syntax %set-fields
  (lambda (x)
    (with-syntax ((getter-type   #'(@@ (srfi srfi-9) getter-type))
                  (getter-index  #'(@@ (srfi srfi-9) getter-index))
                  (getter-copier #'(@@ (srfi srfi-9) getter-copier)))
      (syntax-case x ()
        ((_ check? orig-form (path-so-far ...)
            s)
         #'s)
        ((_ check? orig-form (path-so-far ...)
            s (() e))
         #'e)
        ((_ check? orig-form (path-so-far ...)
            struct-expr ((head . tail) expr) ...)
         (let ((collated-specs (collate-set-field-specs
                                #'(((head . tail) expr) ...))))
           (with-syntax (((getter0 getter ...)
                          (map car collated-specs)))
             (with-syntax ((err #'(unknown-getter
                                   orig-form getter0)))
               #`(ck
                  ()
                  (c-same-type-check
                   'orig-form
                   '(path-so-far ...)
                   '(getter0 getter ...)
                   (c-list (getter-type 'getter0 'err)
                           (getter-type 'getter 'err) ...)
                   '(let ((s struct-expr))
                      ((ck () (getter-copier 'getter0 'err))
                       check?
                       s
                       #,@(map (lambda (spec)
                                 (with-syntax (((head (tail expr) ...) spec))
                                   (with-syntax ((err #'(unknown-getter
                                                         orig-form head)))
                                     #'(head (%set-fields
                                              check?
                                              orig-form
                                              (path-so-far ... head)
                                              (struct-ref s (ck () (getter-index
                                                                    'head 'err)))
                                              (tail expr) ...)))))
                               collated-specs)))))))))
        ((_ check? orig-form (path-so-far ...)
            s (() e) (() e*) ...)
         (syntax-violation 'set-fields "duplicate field path"
                           #'orig-form #'(path-so-far ...)))
        ((_ check? orig-form (path-so-far ...)
            s ((getter ...) expr) ...)
         (syntax-violation 'set-fields "one field path is a prefix of another"
                           #'orig-form #'(path-so-far ...)))
        ((_ check? orig-form . rest)
         (syntax-violation 'set-fields "invalid syntax" #'orig-form))))))
