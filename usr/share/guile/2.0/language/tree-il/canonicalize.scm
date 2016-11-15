;;; Tree-il canonicalizer

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

;;; Code:

(define-module (language tree-il canonicalize)
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (canonicalize!))

(define (tree-il-any proc exp)
  (tree-il-fold (lambda (exp res)
                  (or res (proc exp)))
                (lambda (exp res)
                  (or res (proc exp)))
                (lambda (exp res) res)
                #f exp))

(define (canonicalize! x)
  (post-order!
   (lambda (x)
     (match x
       (($ <sequence> src (tail))
        tail)
       (($ <sequence> src exps)
        (and (any sequence? exps)
             (make-sequence src
                            (append-map (lambda (x)
                                          (if (sequence? x)
                                              (sequence-exps x)
                                              (list x)))
                                        exps))))
       (($ <let> src () () () body)
        body)
       (($ <letrec> src _ () () () body)
        body)
       (($ <fix> src () () () body)
        body)
       (($ <dynlet> src () () body)
        body)
       (($ <lambda> src meta #f)
        ;; Give a body to case-lambda with no clauses.
        (make-lambda
         src meta
         (make-lambda-case
          #f '() #f #f #f '() '()
          (make-application
           #f
           (make-primitive-ref #f 'throw)
           (list (make-const #f 'wrong-number-of-args)
                 (make-const #f #f)
                 (make-const #f "Wrong number of arguments")
                 (make-const #f '())
                 (make-const #f #f)))
          #f)))
       (($ <prompt> src tag body handler)
        (define (escape-only? handler)
          (match handler
            (($ <lambda-case> _ (_ . _) _ _ _ _ (cont . _) body #f)
             (not (tree-il-any (lambda (x)
                                 (and (lexical-ref? x)
                                      (eq? (lexical-ref-gensym x) cont)))
                               body)))
            (else #f)))
        (define (thunk-application? x)
          (match x
            (($ <application> _
                ($ <lambda> _ _ ($ <lambda-case> _ () #f #f #f))
                ()) #t)
            (_ #f)))
        (define (make-thunk-application body)
          (define thunk
            (make-lambda #f '()
                         (make-lambda-case #f '() #f #f #f '() '() body #f)))
          (make-application #f thunk '()))

        ;; This code has a nasty job to do: to ensure that either the
        ;; handler is escape-only, or the body is the application of a
        ;; thunk.  Sad but true.
        (if (or (escape-only? handler)
                (thunk-application? body))
            #f
            (make-prompt src tag (make-thunk-application body) handler)))
       (_ #f)))
   x))
