;;; transformation of letrec into simpler forms

;; Copyright (C) 2009-2013,2016,2019,2021 Free Software Foundation, Inc.

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

(define-module (language tree-il fix-letrec)
  #:use-module (system base syntax)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:use-module (language tree-il effects)
  #:use-module (language cps graphs)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:export (fix-letrec))

;; For a detailed discussion, see "Fixing Letrec: A Faithful Yet
;; Efficient Implementation of Scheme's Recursive Binding Construct", by
;; Oscar Waddell, Dipanwita Sarkar, and R. Kent Dybvig, as well as
;; "Fixing Letrec (reloaded)", by Abdulaziz Ghuloum and R. Kent Dybvig.

(define fix-fold (make-tree-il-folder))
(define (analyze-lexicals x)
  (define referenced (make-hash-table))
  (define assigned (make-hash-table))
  ;; Functional hash sets would be nice.
  (fix-fold x
            (lambda (x)
              (record-case x
                ((<lexical-ref> gensym)
                 (hashq-set! referenced gensym #t)
                 (values))
                ((<lexical-set> gensym)
                 (hashq-set! assigned gensym #t)
                 (values))
                (else
                 (values))))
            (lambda (x)
              (values)))
  (values referenced assigned))

(define (make-seq* src head tail)
  (record-case head
    ((<lambda>) tail)
    ((<const>) tail)
    ((<lexical-ref>) tail)
    ((<void>) tail)
    (else (make-seq src head tail))))

(define (free-variables expr cache)
  (define (adjoin elt set)
    (lset-adjoin eq? set elt))
  (define (union set1 set2)
    (lset-union eq? set1 set2))
  (define (difference set1 set2)
    (lset-difference eq? set1 set2))
  (define fix-fold (make-tree-il-folder))
  (define (recurse expr)
    (free-variables expr cache))
  (define (recurse* exprs)
    (fold (lambda (expr free)
            (union (recurse expr) free))
          '()
          exprs))
  (define (visit expr)
    (match expr
      ((or ($ <void>) ($ <const>) ($ <primitive-ref>)
           ($ <module-ref>) ($ <toplevel-ref>))
       '())
      (($ <lexical-ref> src name gensym)
       (list gensym))
      (($ <lexical-set> src name gensym exp)
       (adjoin gensym (recurse exp)))
      (($ <module-set> src mod name public? exp)
       (recurse exp))
      (($ <toplevel-set> src mod name exp)
       (recurse exp))
      (($ <toplevel-define> src mod name exp)
       (recurse exp))
      (($ <conditional> src test consequent alternate)
       (union (recurse test)
              (union (recurse consequent)
                     (recurse alternate))))
      (($ <call> src proc args)
       (recurse* (cons proc args)))
      (($ <primcall> src name args)
       (recurse* args))
      (($ <seq> src head tail)
       (union (recurse head)
              (recurse tail)))
      (($ <lambda> src meta body)
       (recurse body))
      (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
       (union (difference (union (recurse* inits)
                                 (recurse body))
                          gensyms)
              (if alternate
                  (recurse alternate)
                  '())))
      (($ <let> src names gensyms vals body)
       (union (recurse* vals)
              (difference (recurse body)
                          gensyms)))
      (($ <letrec> src in-order? names gensyms vals body)
       (difference (union (recurse* vals)
                          (recurse body))
                   gensyms))
      (($ <fix> src names gensyms vals body)
       (difference (union (recurse* vals)
                          (recurse body))
                   gensyms))
      (($ <let-values> src exp body)
       (union (recurse exp)
              (recurse body)))
      (($ <prompt> src escape-only? tag body handler)
       (union (recurse tag)
              (union (recurse body)
                     (recurse handler))))
      (($ <abort> src tag args tail)
       (union (recurse tag)
              (union (recurse* args)
                     (recurse tail))))))
  (or (hashq-ref cache expr)
      (let ((res (visit expr)))
        (hashq-set! cache expr res)
        res)))

(define (enumerate elts)
  (fold2 (lambda (x out id)
           (values (intmap-add out id x) (1+ id)))
         elts empty-intmap 0))

(define (compute-complex id->sym id->init assigned)
  (define compute-effects
    (make-effects-analyzer (lambda (x) (hashq-ref assigned x))))
  (intmap-fold
   (lambda (id sym complex)
     (if (or (hashq-ref assigned sym)
             (let ((effects (compute-effects (intmap-ref id->init id))))
               (not (constant? (exclude-effects effects &allocation)))))
         (intset-add complex id)
         complex))
   id->sym empty-intset))

(define (compute-sccs names syms inits in-order? fv-cache assigned)
  (define id->name (enumerate names))
  (define id->sym (enumerate syms))
  (define id->init (enumerate inits))
  (define sym->id (intmap-fold (lambda (id sym out) (acons sym id out))
                               id->sym '()))
  (define (var-list->intset vars)
    (fold1 (lambda (sym out)
             (intset-add out (assq-ref sym->id sym)))
           vars empty-intset))
  (define (free-in-init init)
    (var-list->intset
     (lset-intersection eq? syms (free-variables init fv-cache))))
  (define fv-edges
    (fold2 (lambda (init fv i)
             (values
              (intmap-add fv i (free-in-init init))
              (1+ i)))
           inits empty-intmap 0))
  (define order-edges
    (if in-order?
        (let ((complex (compute-complex id->sym id->init assigned)))
          (intmap-fold (lambda (id sym out prev)
                         (values
                          (intmap-add out id (intset-intersect complex prev))
                          (intset-add prev id)))
                       id->sym empty-intmap empty-intset))
        empty-intmap))
  (define sccs
    (reverse
     (compute-sorted-strongly-connected-components
      (invert-graph (intmap-union fv-edges order-edges intset-union)))))
  (map (lambda (ids)
         (intset-fold-right (lambda (id out)
                              (cons (list (intmap-ref id->name id)
                                          (intmap-ref id->sym id)
                                          (intmap-ref id->init id))
                                    out))
                            ids '()))
       sccs))

(define (fix-scc src binds body fv-cache referenced assigned)
  (match binds
    (((name sym init))
     ;; Case of an SCC containing just a single binding.
     (cond
      ((not (hashq-ref referenced sym))
       (make-seq* src init body))
      ((and (lambda? init) (not (hashq-ref assigned sym)))
       (make-fix src (list name) (list sym) (list init) body))
      ((memq sym (free-variables init fv-cache))
       (make-let src (list name) (list sym) (list (make-void src))
                 (make-seq src
                           (make-lexical-set src name sym init)
                           body)))
      (else
       (make-let src (list name) (list sym) (list init)
                 body))))
    (_
     (call-with-values (lambda ()
                         (partition
                          (lambda (bind)
                            (match bind
                              ((name sym init)
                               (and (lambda? init)
                                    (not (hashq-ref assigned sym))))))
                          binds))
       (lambda (l c)
         (define (bind-complex-vars body)
           (if (null? c)
               body
               (let ((inits (map (lambda (x) (make-void #f)) c)))
                 (make-let src (map car c) (map cadr c) inits body))))
         (define (bind-lambdas body)
           (if (null? l)
               body
               (make-fix src (map car l) (map cadr l) (map caddr l) body)))
         (define (initialize-complex body)
           (fold-right (lambda (bind body)
                         (match bind
                           ((name sym init)
                            (make-seq src
                                      (make-lexical-set src name sym init)
                                      body))))
                       body c))
         (bind-complex-vars
          (bind-lambdas
           (initialize-complex body))))))))

(define (fix-term src in-order? names gensyms vals body
                  fv-cache referenced assigned)
  (fold-right (lambda (binds body)
                (fix-scc src binds body fv-cache referenced assigned))
              body
              (compute-sccs names gensyms vals in-order? fv-cache
                            assigned)))

;; For letrec*, try to minimize false dependencies introduced by
;; ordering.
(define (reorder-bindings bindings)
  (define (possibly-references? expr bindings)
    (let visit ((expr expr))
      (match expr
        ((or ($ <void>) ($ <const>) ($ <primitive-ref>) ($ <module-ref>)) #f)
        (($ <lexical-ref> _ name var)
         (or-map (match-lambda (#(name var' val) (eq? var' var)))
                 bindings))
        (($ <seq> _ head tail)
         (or (visit head) (visit tail)))
        (($ <primcall> _ name args) (or-map visit args))
        (($ <conditional> _ test consequent alternate)
         (or (visit test) (visit consequent) (visit alternate)))
        (_ #t))))
  (let visit ((bindings bindings) (sunk-lambdas '()) (sunk-exprs '()))
    (match bindings
      (() (append sunk-lambdas (reverse sunk-exprs)))
      ((binding . bindings)
       (match binding
         (#(_ _ ($ <lambda>))
          (visit bindings (cons binding sunk-lambdas) sunk-exprs))
         (#(_ _ expr)
          (cond
           ((possibly-references? expr bindings)
            ;; Init expression might refer to later bindings.
            ;; Serialize.
            (append sunk-lambdas (reverse sunk-exprs)
                    (cons binding (visit bindings '() '()))))
           (else
            (visit bindings sunk-lambdas (cons binding sunk-exprs))))))))))

(define (fix-letrec x)
  (let-values (((referenced assigned) (analyze-lexicals x)))
    (define fv-cache (make-hash-table))
    (post-order
     (lambda (x)
       (record-case x

         ;; Sets to unreferenced variables may be replaced by their
         ;; expression, called for effect.
         ((<lexical-set> gensym exp)
          (if (hashq-ref referenced gensym)
              x
              (make-seq* #f exp (make-void #f))))

         ((<letrec> src in-order? names gensyms vals body)
          (if in-order?
              (match (reorder-bindings (map vector names gensyms vals))
                ((#(names gensyms vals) ...)
                 (fix-term src #t names gensyms vals body
                           fv-cache referenced assigned)))
              (fix-term src #f names gensyms vals body
                        fv-cache referenced assigned)))

         ((<let> src names gensyms vals body)
          ;; Apply the same algorithm to <let> that binds <lambda>
          (if (or-map lambda? vals)
              (fix-term src #f names gensyms vals body
                        fv-cache referenced assigned)
              x))
         
         (else x)))
     x)))

;;; Local Variables:
;;; eval: (put 'record-case 'scheme-indent-function 1)
;;; End:
