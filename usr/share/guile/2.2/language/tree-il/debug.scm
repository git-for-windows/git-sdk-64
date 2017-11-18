;;; Tree-IL verifier

;; Copyright (C) 2011, 2013 Free Software Foundation, Inc.

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

(define-module (language tree-il debug)
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (verify-tree-il))

(define (verify-tree-il exp)
  (define seen-gensyms (make-hash-table))
  (define (add sym env)
    (if (hashq-ref seen-gensyms sym)
        (error "duplicate gensym" sym)
        (begin
          (hashq-set! seen-gensyms sym #t)
          (cons sym env))))
  (define (add-env new env)
    (if (null? new)
        env
        (add-env (cdr new) (add (car new) env))))

  (let visit ((exp exp)
              (env '()))
    (match exp
      (($ <lambda-case> src req opt rest kw inits gensyms body alt)
       (cond
        ((not (and (list? req) (and-map symbol? req)))
         (error "bad required args (should be list of symbols)" exp))
        ((and opt (not (and (list? opt) (and-map symbol? opt))))
         (error "bad optionals (should be #f or list of symbols)" exp))
        ((and rest (not (symbol? rest)))
         (error "bad required args (should be #f or symbol)" exp))
        ((and kw (not (match kw
                        ((aok . kwlist)
                         (and (list? kwlist)
                              (and-map 
                               (lambda (x)
                                 (match x
                                   (((? keyword?) (? symbol?) (? symbol? sym))
                                    (memq sym gensyms))
                                   (_ #f)))
                               kwlist)))
                        (_ #f))))
         (error "bad keywords (should be #f or (aok (kw name sym) ...))" exp))
        ((not (and (list? gensyms) (and-map symbol? gensyms)))
         (error "bad gensyms (should be list of symbols)" exp))
        ((not (and (list? gensyms) (and-map symbol? gensyms)))
         (error "bad gensyms (should be list of symbols)" exp))
        ((not (= (length gensyms) 
                 (+ (length req)
                    (if opt (length opt) 0)
                    ;; FIXME: technically possible for kw gensyms to
                    ;; alias other gensyms
                    (if rest 1 0)
                    (if kw (1- (length kw)) 0))))
         (error "unexpected gensyms length" exp))
        (else
         (let lp ((env (add-env (take gensyms (length req)) env))
                  (nopt (if opt (length opt) 0))
                  (inits inits)
                  (tail (drop gensyms (length req))))
           (if (zero? nopt)
               (let lp ((env (if rest (add (car tail) env) env))
                        (inits inits)
                        (tail (if rest (cdr tail) tail)))
                 (if (pair? inits)
                     (begin
                       (visit (car inits) env)
                       (lp (add (car tail) env) (cdr inits) 
                           (cdr tail)))
                     (visit body env)))
               (begin
                 (visit (car inits) env)
                 (lp (add (car tail) env)
                     (1- nopt)
                     (cdr inits) 
                     (cdr tail)))))
         (if alt (visit alt env)))))
      (($ <lexical-ref> src name gensym)
       (cond
        ((not (symbol? name))
         (error "name should be a symbol" name))
        ((not (hashq-ref seen-gensyms gensym))
         (error "unbound lexical" exp))
        ((not (memq gensym env))
         (error "displaced lexical" exp))))
      (($ <lexical-set> src name gensym exp)
       (cond
        ((not (symbol? name))
         (error "name should be a symbol" name))
        ((not (hashq-ref seen-gensyms gensym))
         (error "unbound lexical" exp))
        ((not (memq gensym env))
         (error "displaced lexical" exp))
        (else
         (visit exp env))))
      (($ <lambda> src meta body)
       (cond
        ((and meta (not (and (list? meta) (and-map pair? meta))))
         (error "meta should be alist" meta))
        ((and body (not (lambda-case? body)))
         (error "lambda body should be lambda-case" exp))
        (else
         (if body
             (visit body env)))))
      (($ <let> src names gensyms vals body)
       (cond
        ((not (and (list? names) (and-map symbol? names)))
         (error "names should be list of syms" exp))
        ((not (and (list? gensyms) (and-map symbol? gensyms)))
         (error "gensyms should be list of syms" exp))
        ((not (list? vals))
         (error "vals should be list" exp))
        ((not (= (length names) (length gensyms) (length vals)))
         (error "names, syms, vals should be same length" exp))
        (else
         (for-each (cut visit <> env) vals)
         (visit body (add-env gensyms env)))))
      (($ <letrec> src in-order? names gensyms vals body)
       (cond
        ((not (and (list? names) (and-map symbol? names)))
         (error "names should be list of syms" exp))
        ((not (and (list? gensyms) (and-map symbol? gensyms)))
         (error "gensyms should be list of syms" exp))
        ((not (list? vals))
         (error "vals should be list" exp))
        ((not (= (length names) (length gensyms) (length vals)))
         (error "names, syms, vals should be same length" exp))
        (else
         (let ((env (add-env gensyms env)))
           (for-each (cut visit <> env) vals)
           (visit body env)))))
      (($ <fix> src names gensyms vals body)
       (cond
        ((not (and (list? names) (and-map symbol? names)))
         (error "names should be list of syms" exp))
        ((not (and (list? gensyms) (and-map symbol? gensyms)))
         (error "gensyms should be list of syms" exp))
        ((not (list? vals))
         (error "vals should be list" exp))
        ((not (= (length names) (length gensyms) (length vals)))
         (error "names, syms, vals should be same length" exp))
        (else
         (let ((env (add-env gensyms env)))
           (for-each (cut visit <> env) vals)
           (visit body env)))))
      (($ <let-values> src exp body)
       (cond
        ((not (lambda-case? body))
         (error "let-values body should be lambda-case" exp))
        (else
         (visit exp env)
         (visit body env))))
      (($ <const> src val) #t)
      (($ <void> src) #t)
      (($ <toplevel-ref> src name)
       (cond
        ((not (symbol? name))
         (error "name should be a symbol" name))))
      (($ <module-ref> src mod name public?)
       (cond
        ((not (and (list? mod) (and-map symbol? mod)))
         (error "module name should be list of symbols" exp))
        ((not (symbol? name))
         (error "name should be symbol" exp))))
      (($ <primitive-ref> src name)
       (cond
        ((not (symbol? name))
         (error "name should be symbol" exp))))
      (($ <toplevel-set> src name exp)
       (cond
        ((not (symbol? name))
         (error "name should be a symbol" name))
        (else
         (visit exp env))))
      (($ <toplevel-define> src name exp)
       (cond
        ((not (symbol? name))
         (error "name should be a symbol" name))
        (else
         (visit exp env))))
      (($ <module-set> src mod name public? exp)
       (cond
        ((not (and (list? mod) (and-map symbol? mod)))
         (error "module name should be list of symbols" exp))
        ((not (symbol? name))
         (error "name should be symbol" exp))
        (else
         (visit exp env))))
      (($ <conditional> src condition subsequent alternate)
       (visit condition env)
       (visit subsequent env)
       (visit alternate env))
      (($ <primcall> src name args)
       (cond
        ((not (symbol? name))
         (error "expected symbolic operator" exp))
        ((not (list? args))
         (error "expected list of args" args))
        (else
         (for-each (cut visit <> env) args))))
      (($ <call> src proc args)
       (cond
        ((not (list? args))
         (error "expected list of args" args))
        (else
         (visit proc env)
         (for-each (cut visit <> env) args))))
      (($ <seq> src head tail)
       (visit head env)
       (visit tail env))
      (($ <prompt> src escape-only? tag body handler)
       (unless (boolean? escape-only?)
         (error "escape-only? should be a bool" escape-only?))
       (visit tag env)
       (visit body env)
       (visit handler env))
      (($ <abort> src tag args tail)
       (visit tag env)
       (for-each (cut visit <> env) args)
       (visit tail env))
      (_
       (error "unexpected tree-il" exp)))
    (let ((src (tree-il-src exp)))
      (if (and src (not (and (list? src) (and-map pair? src)
                             (and-map symbol? (map car src)))))
          (error "bad src"))
      ;; Return it, why not.
      exp)))
