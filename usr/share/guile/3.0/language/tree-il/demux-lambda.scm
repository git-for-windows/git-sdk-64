;;; Expand case-lambda and lambda* into simple dispatchers
;;; Copyright (C) 2024 Free Software Foundation, Inc.
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

;;; Commentary:
;;;
;;; We can partition lambdas into simple and complex.  A simple lambda
;;; has just one clause and no optional, rest, or keyword arguments.
;;; Any other lambda is complex.  This pass aims to facilitate reduction
;;; of complex lambdas to simple lambdas.  It does so by eta-expanding
;;; lexically-bound complex lambdas into simple dispatchers that
;;; tail-call simple lambda body procedures.  This will allow peval to
;;; elide the complex lambdas in many cases.
;;;
;;; Code:

(define-module (language tree-il demux-lambda)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:export (demux-lambda))

(define (make-binding name sym val) (vector name sym val))

(define (demux-clause func-name clause)
  (match clause
    (#f (values '() clause))
    (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
     (call-with-values (lambda () (demux-clause func-name alternate))
       (lambda (bindings alternate)
         (define simple-req
           (append req (or opt '()) (if rest (list rest) '())
                   (match kw
                     ((aok? (kw name sym) ...) name)
                     (#f '()))))
         (define simple-clause
           (make-lambda-case src simple-req '() #f #f '() gensyms body #f))
         (define simple-func (make-lambda src '() simple-clause))
         (define simple-sym (gensym "demuxed"))
         (define simple-binding
           (make-binding func-name simple-sym simple-func))

         (define renamed-syms
           (map (lambda (_) (gensym "demux")) gensyms))
         (define rename-sym
           (let ((renamed (map cons gensyms renamed-syms)))
             (lambda (sym) (or (assq-ref renamed sym) sym))))
         (define renamed-kw
           (match kw
             ((aok? (kw name sym) ...)
              (cons aok? (map list kw name (map rename-sym sym))))
             (#f #f)))
         (define renamed-inits
           (map (lambda (init)
                  (post-order
                   (lambda (exp)
                     (match exp
                       (($ <lexical-ref> src name sym)
                        (make-lexical-ref src name (rename-sym sym)))
                       (($ <lexical-set> src name sym exp)
                        (make-lexical-set src name (rename-sym sym) exp))
                       (_ exp)))
                   init))
                inits))
         (define dispatch-call
           (make-call src (make-lexical-ref src func-name simple-sym)
                      (map (lambda (name sym)
                             (make-lexical-ref src name sym))
                           simple-req renamed-syms)))
         (define dispatch-clause
           (make-lambda-case src req opt rest renamed-kw renamed-inits
                             renamed-syms dispatch-call alternate))

         (values (cons simple-binding bindings)
                 dispatch-clause))))))

(define (demux-lambda exp)
  (define (complex-lambda? val)
    (match val
      (($ <lambda> src meta
          ($ <lambda-case> src req opt rest kw inits gensyms body alternate))
       (or (pair? opt) rest (pair? kw) alternate))
      (_ #f)))

  (define (demux-binding name gensym val)
    (if (complex-lambda? val)
        (match val
          (($ <lambda> src meta clause)
           (call-with-values (lambda () (demux-clause name clause))
             (lambda (extra-bindings clause)
               (let ((val (make-lambda src meta clause)))
                 (append extra-bindings
                         (list (make-binding name gensym val))))))))
        (list (make-binding name gensym val))))

  (define (demux-lexically-bound-complex-lambdas exp)
    (match exp
       (($ <letrec> src in-order? names gensyms vals body)
        (match (append-map demux-binding names gensyms vals)
          ((#(name gensym val) ...)
           (make-letrec src in-order? name gensym val body))))

       (($ <let> src names gensyms vals body)
        (if (or-map lambda? vals)
            (demux-lexically-bound-complex-lambdas
             (make-letrec src #f names gensyms vals body))
            exp))

       (_ exp)))

  (post-order demux-lexically-bound-complex-lambdas exp))
