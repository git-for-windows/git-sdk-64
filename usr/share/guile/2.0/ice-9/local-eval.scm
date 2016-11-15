;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2012, 2013 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (ice-9 local-eval)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system base compile)
  #:use-module (system syntax)
  #:export (the-environment local-eval local-compile))

(define-record-type lexical-environment-type
  (make-lexical-environment scope wrapper boxes patterns)
  lexical-environment?
  (scope             lexenv-scope)
  (wrapper           lexenv-wrapper)
  (boxes             lexenv-boxes)
  (patterns          lexenv-patterns))

(set-record-type-printer!
 lexical-environment-type
 (lambda (e port)
   (format port "#<lexical-environment ~S (~S bindings)>"
           (syntax-module (lexenv-scope e))
           (+ (length (lexenv-boxes e)) (length (lexenv-patterns e))))))

(define-syntax syntax-object-of
  (lambda (form)
    (syntax-case form ()
      ((_ x) #`(quote #,(datum->syntax #'x #'x))))))

(define-syntax-rule (make-box v)
  (case-lambda
   (() v)
   ((x) (set! v x))))

(define (make-transformer-from-box id trans)
  (set-procedure-property! trans 'identifier-syntax-box id)
  trans)

(define-syntax-rule (identifier-syntax-from-box box)
  (make-transformer-from-box
   (syntax-object-of box)
   (identifier-syntax (id          (box))
                      ((set! id x) (box x)))))

(define (unsupported-binding name)
  (make-variable-transformer
   (lambda (x)
     (syntax-violation
      'local-eval
      "unsupported binding captured by (the-environment)"
      x))))

(define (within-nested-ellipses id lvl)
  (let loop ((s id) (n lvl))
    (if (zero? n)
        s
        (loop #`(#,s (... ...)) (- n 1)))))

;; Analyze the set of bound identifiers IDS.  Return four values:
;;
;; capture: A list of forms that will be emitted in the expansion of
;; `the-environment' to capture lexical variables.
;;
;; formals: Corresponding formal parameters for use in the lambda that
;; re-introduces those variables.  These are temporary identifiers, and
;; as such if we have a nested `the-environment', there is no need to
;; capture them.  (See the notes on nested `the-environment' and
;; proxies, below.)
;;
;; wrappers: A list of procedures of type SYNTAX -> SYNTAX, used to wrap
;; the expression to be evaluated in forms that re-introduce the
;; variable.  The forms will be nested so that the variable shadowing
;; semantics of the original form are maintained.
;;
;; patterns: A terrible hack.  The issue is that for pattern variables,
;; we can't emit lexically nested with-syntax forms, like:
;;
;;   (with-syntax ((foo 1)) (the-environment))
;;   => (with-syntax ((foo 1))
;;        ... #'(with-syntax ((foo ...)) ... exp) ...)
;;
;; The reason is that the outer "foo" substitutes into the inner "foo",
;; yielding something like:
;;
;;   (with-syntax ((foo 1))
;;     ... (with-syntax ((1 ...)) ...)
;;            
;; Which ain't what we want.  So we hide the information needed to
;; re-make the inner pattern binding form in the lexical environment
;; object, and then introduce those identifiers via another with-syntax.
;;
;;
;; There are four different kinds of lexical bindings: normal lexicals,
;; macros, displaced lexicals, and pattern variables.  See the
;; documentation of syntax-local-binding for more info on these.
;;
;; We capture normal lexicals via `make-box', which creates a
;; case-lambda that can reference or set a variable.  These get
;; re-introduced with an identifier-syntax.
;;
;; We can't capture macros currently.  However we do recognize our own
;; macros that are actually proxying lexicals, so that nested
;; `the-environment' forms are possible.  In that case we drill down to
;; the identifier for the already-existing box, and just capture that
;; box.
;;
;; And that's it: we skip displaced lexicals, and the pattern variables
;; are discussed above.
;;
(define (analyze-identifiers ids)
  (define (mktmp)
    (datum->syntax #'here (gensym "t ")))
  (let lp ((ids ids) (capture '()) (formals '()) (wrappers '()) (patterns '()))
    (cond
     ((null? ids)
      (values capture formals wrappers patterns))
     (else
      (let ((id (car ids)) (ids (cdr ids)))
        (call-with-values (lambda () (syntax-local-binding id))
          (lambda (type val)
            (case type
              ((lexical)
               (if (or-map (lambda (x) (bound-identifier=? x id)) formals)
                   (lp ids capture formals wrappers patterns)
                   (let ((t (mktmp)))
                     (lp ids
                         (cons #`(make-box #,id) capture)
                         (cons t formals)
                         (cons (lambda (x)
                                 #`(let-syntax ((#,id (identifier-syntax-from-box #,t)))
                                     #,x))
                               wrappers)
                         patterns))))
              ((displaced-lexical)
               (lp ids capture formals wrappers patterns))
              ((macro)
               (let ((b (procedure-property val 'identifier-syntax-box)))
                 (if b
                     (lp ids (cons b capture) (cons b formals)
                         (cons (lambda (x)
                                 #`(let-syntax ((#,id (identifier-syntax-from-box #,b)))
                                     #,x))
                               wrappers)
                         patterns)
                     (lp ids capture formals
                         (cons (lambda (x)
                                 #`(let-syntax ((#,id (unsupported-binding '#,id)))
                                     #,x))
                               wrappers)
                         patterns))))
              ((pattern-variable)
               (let ((t (datum->syntax id (gensym "p ")))
                     (nested (within-nested-ellipses id (cdr val))))
                 (lp ids capture formals
                     (cons (lambda (x)
                             #`(with-syntax ((#,t '#,nested))
                                 #,x))
                           wrappers)
                     ;; This dance is to hide these pattern variables
                     ;; from the expander.
                     (cons (list (datum->syntax #'here (syntax->datum id))
                                 (cdr val)
                                 t)
                           patterns))))
              ((ellipsis)
               (lp ids capture formals
                   (cons (lambda (x)
                           #`(with-ellipsis #,val #,x))
                         wrappers)
                   patterns))
              (else
               (error "what" type val))))))))))

(define-syntax the-environment
  (lambda (x)
    (syntax-case x ()
      ((the-environment)
       #'(the-environment the-environment))
      ((the-environment scope)
       (call-with-values (lambda ()
                           (analyze-identifiers
                            (syntax-locally-bound-identifiers #'scope)))
         (lambda (capture formals wrappers patterns)
           (define (wrap-expression x)
             (let lp ((x x) (wrappers wrappers))
               (if (null? wrappers)
                   x
                   (lp ((car wrappers) x) (cdr wrappers)))))
           (with-syntax (((f ...) formals)
                         ((c ...) capture)
                         (((pname plvl pformal) ...) patterns)
                         (wrapped (wrap-expression #'(begin #f exp))))
             #'(make-lexical-environment
                #'scope
                (lambda (exp pformal ...)
                  (with-syntax ((exp exp)
                                (pformal pformal)
                                ...)
                    #'(lambda (f ...)
                        wrapped)))
                (list c ...)
                (list (list 'pname plvl #'pformal) ...)))))))))

(define (env-module e)
  (cond
   ((lexical-environment? e) (resolve-module (syntax-module (lexenv-scope e))))
   ((module? e) e)
   (else (error "invalid lexical environment" e))))

(define (env-boxes e)
  (cond
   ((lexical-environment? e) (lexenv-boxes e))
   ((module? e) '())
   (else (error "invalid lexical environment" e))))

(define (local-wrap x e)
  (cond
   ((lexical-environment? e)
    (apply (lexenv-wrapper e)
           (datum->syntax (lexenv-scope e) x)
           (map (lambda (l)
                  (let ((name (car l))
                        (lvl (cadr l))
                        (scope (caddr l)))
                    (within-nested-ellipses (datum->syntax scope name) lvl)))
                (lexenv-patterns e))))
   ((module? e) #`(lambda () #f #,x))
   (else (error "invalid lexical environment" e))))

(define (local-eval x e)
  "Evaluate the expression @var{x} within the lexical environment @var{e}."
  (apply (eval (local-wrap x e) (env-module e))
         (env-boxes e)))

(define* (local-compile x e #:key (opts '()))
  "Compile and evaluate the expression @var{x} within the lexical
environment @var{e}."
  (apply (compile (local-wrap x e) #:env (env-module e)
                  #:from 'scheme #:opts opts)
         (env-boxes e)))
