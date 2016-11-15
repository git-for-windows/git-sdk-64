;;; srfi-11.scm --- let-values and let*-values

;; Copyright (C) 2000, 2001, 2002, 2004, 2006, 2009 Free Software Foundation, Inc.
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

;; This module exports two syntax forms: let-values and let*-values.
;;
;; Sample usage:
;;
;;   (let-values (((x y . z) (foo a b))
;;                ((p q) (bar c)))
;;     (baz x y z p q))
;;
;; This binds `x' and `y' to the first to values returned by `foo',
;; `z' to the rest of the values from `foo', and `p' and `q' to the
;; values returned by `bar'.  All of these are available to `baz'.
;;
;; let*-values : let-values :: let* : let
;;
;; This module is fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-11)
  :export-syntax (let-values let*-values))

(cond-expand-provide (current-module) '(srfi-11))

;;;;;;;;;;;;;;
;; let-values
;;
;; Current approach is to translate
;;
;;   (let-values (((x y . z) (foo a b))
;;                ((p q) (bar c)))
;;     (baz x y z p q))
;;
;; into
;;
;;   (call-with-values (lambda () (foo a b))
;;     (lambda (<tmp-x> <tmp-y> . <tmp-z>)
;;       (call-with-values (lambda () (bar c))
;;         (lambda (<tmp-p> <tmp-q>)
;;           (let ((x <tmp-x>)
;;                 (y <tmp-y>)
;;                 (z <tmp-z>)
;;                 (p <tmp-p>)
;;                 (q <tmp-q>))
;;             (baz x y z p q))))))

;; We could really use quasisyntax here...
(define-syntax let-values
  (lambda (x)
    (syntax-case x ()
      ((_ ((binds exp)) b0 b1 ...)
       (syntax (call-with-values (lambda () exp)
                 (lambda binds b0 b1 ...))))
      ((_ (clause ...) b0 b1 ...)
       (let lp ((clauses (syntax (clause ...)))
                (ids '())
                (tmps '()))
         (if (null? clauses)
             (with-syntax (((id ...) ids)
                           ((tmp ...) tmps))
               (syntax (let ((id tmp) ...)
                         b0 b1 ...)))
             (syntax-case (car clauses) ()
               (((var ...) exp)
                (with-syntax (((new-tmp ...) (generate-temporaries 
                                              (syntax (var ...))))
                              ((id ...) ids)
                              ((tmp ...) tmps))
                  (with-syntax ((inner (lp (cdr clauses)
                                           (syntax (var ... id ...))
                                           (syntax (new-tmp ... tmp ...)))))
                    (syntax (call-with-values (lambda () exp)
                              (lambda (new-tmp ...) inner))))))
               ((vars exp)
                (with-syntax ((((new-tmp . new-var) ...)
                               (let lp ((vars (syntax vars)))
                                 (syntax-case vars ()
                                   ((id . rest)
                                    (acons (syntax id)
                                           (car
                                            (generate-temporaries (syntax (id))))
                                           (lp (syntax rest))))
                                   (id (acons (syntax id)
                                              (car
                                               (generate-temporaries (syntax (id))))
                                              '())))))
                              ((id ...) ids)
                              ((tmp ...) tmps))
                  (with-syntax ((inner (lp (cdr clauses)
                                           (syntax (new-var ... id ...))
                                           (syntax (new-tmp ... tmp ...))))
                                (args (let lp ((tmps (syntax (new-tmp ...))))
                                        (syntax-case tmps ()
                                          ((id) (syntax id))
                                          ((id . rest) (cons (syntax id)
                                                             (lp (syntax rest))))))))
                    (syntax (call-with-values (lambda () exp)
                              (lambda args inner)))))))))))))

;;;;;;;;;;;;;;
;; let*-values
;;
;; Current approach is to translate
;;
;;   (let*-values (((x y z) (foo a b))
;;                ((p q) (bar c)))
;;     (baz x y z p q))
;;
;; into
;;
;;   (call-with-values (lambda () (foo a b))
;;     (lambda (x y z)
;;       (call-with-values (lambda (bar c))
;;         (lambda (p q)
;;           (baz x y z p q)))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body ...)
     (let () body ...))
    ((let*-values ((vars-1 binding-1) (vars-2 binding-2) ...) body ...)
     (call-with-values (lambda () binding-1)
       (lambda vars-1
         (let*-values ((vars-2 binding-2) ...)
           body ...))))))

;;; srfi-11.scm ends here
