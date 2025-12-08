;;; transformation of letrec into simpler forms

;; Copyright (C) 2009-2013,2016,2019,2021,2023,2025 Free Software Foundation, Inc.

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
  #:use-module ((srfi srfi-1) #:select (fold fold-right partition))
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

(define (compute-ids expr)
  "For each lexical in @var{expr}, assign it an integer identifier.
Identifiers are assigned sequentially in pre-order, which allows later
code to sort letrec* bindings by identifier value."
  (define counter 0)
  (define sym->id (make-hash-table))
  (define (intern! sym)
    (let ((id counter))
      (set! counter (1+ id))
      (hashq-set! sym->id sym id)))
  (define (for-each/0 f l)
    (for-each f l)
    (values))
  (define tree-il-fold/0 (make-tree-il-folder))
  (tree-il-fold/0 expr
                  (match-lambda
                    (($ <lambda-case> src req opt rest kw inits syms body alt)
                     (for-each/0 intern! syms))
                    (($ <let> src names syms inits body)
                     (for-each/0 intern! syms))
                    (($ <letrec> src in-order? names syms inits body)
                     (for-each/0 intern! syms))
                    (($ <fix> src names syms inits body)
                     (for-each/0 intern! syms))
                    (_ (values)))
                  (lambda (_) (values)))
  (lambda (sym)
    (or (hashq-ref sym->id sym)
        (error "unknown binding" sym))))

(define (compute-referenced-and-assigned expr sym->id)
  (define tree-il-fold/2 (make-tree-il-folder referenced assigned))
  (tree-il-fold/2
   expr
   (lambda (expr referenced assigned)
     (match expr
       (($ <lexical-ref> src name gensym)
        (values (intset-add referenced (sym->id gensym))
                assigned))
       (($ <lexical-set> src name gensym val)
        (values referenced
                (intset-add assigned (sym->id gensym))))
       (_
        (values referenced assigned))))
   (lambda (term referenced assigned)
     (values referenced assigned))
   empty-intset
   empty-intset))

(define (make-seq* src head tail)
  (match head
    ((or ($ <lambda>) ($ <const>) ($ <lexical-ref>) ($ <void>)) tail)
    (else (make-seq src head tail))))

(define (make-compute-free-variables sym->id)
  (define (empty)
    empty-intset)
  (define (adjoin elt set)
    (intset-add set (sym->id elt)))
  (define (list->set elts)
    (fold adjoin (empty) elts))
  (define (union set1 set2)
    (intset-union set1 set2))
  (define (difference set1 set2)
    (intset-subtract set1 set2))
  (define (recurse expr)
    (visit expr))
  (define (recurse* exprs)
    (fold (lambda (expr free)
            (union (recurse expr) free))
          (empty)
          exprs))
  (define (visit expr)
    (match expr
      ((or ($ <void>) ($ <const>) ($ <primitive-ref>)
           ($ <module-ref>) ($ <toplevel-ref>))
       (empty))
      (($ <lexical-ref> src name gensym)
       (adjoin gensym (empty)))
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
                          (list->set gensyms))
              (if alternate
                  (recurse alternate)
                  (empty))))
      (($ <let> src names gensyms vals body)
       (union (recurse* vals)
              (difference (recurse body)
                          (list->set gensyms))))
      (($ <letrec> src in-order? names gensyms vals body)
       (difference (union (recurse* vals)
                          (recurse body))
                   (list->set gensyms)))
      (($ <fix> src names gensyms vals body)
       (difference (union (recurse* vals)
                          (recurse body))
                   (list->set gensyms)))
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
  (define cache (make-hash-table))
  (lambda (expr)
    (or (hashq-ref cache expr)
        (let ((res (visit expr)))
          (hashq-set! cache expr res)
          res))))

(define (compute-complex expr assigned sym->id)
  (define (assigned? id)
    (intset-ref assigned id))
  (define (assigned-sym? sym)
    (assigned? (sym->id sym)))
  (define compute-effects
    (make-effects-analyzer assigned-sym?))
  (define (compute-complex-bindings syms inits complex)
    (fold (lambda (sym init complex)
            (let ((id (sym->id sym)))
              (if (or (assigned? id)
                      (let ((effects (compute-effects init)))
                        (not (constant?
                              (exclude-effects effects &allocation)))))
                  (intset-add complex id)
                  complex)))
          complex syms inits))
  (define tree-il-fold/1 (make-tree-il-folder complex))
  (tree-il-fold/1
   expr
   (lambda (expr complex) complex)
   (lambda (expr complex)
     ;; Complex variables are bound by let or letrec.
     (match expr
       (($ <letrec> src in-order? names syms inits body)
        (compute-complex-bindings syms inits complex))
       (($ <let> src names syms inits body)
        (compute-complex-bindings syms inits complex))
       (_
        complex)))
   empty-intset))

(define (compute-sccs names syms inits in-order?
                      sym->id complex compute-free-variables)
  (define nodes
    (fold (lambda (name sym init nodes)
            (intmap-add nodes (sym->id sym) (list name sym init)))
          empty-intmap names syms inits))
  (define node-ids (intmap-keys nodes))
  (define fv-edges
    (intmap-fold (lambda (id node edges)
                   (match node
                     ((name sym init)
                      (let ((fv (compute-free-variables init)))
                        (intmap-add edges id
                                    (intset-intersect node-ids fv))))))
                 nodes empty-intmap))
  (define order-edges
    (if in-order?
        ;; Rely on identifier ordering.
        (intset-fold (lambda (id out prev)
                       (values
                        (intmap-add out id prev)
                        (if (intset-ref complex id)
                            (intset id)
                            prev)))
                     node-ids empty-intmap empty-intset)
        empty-intmap))
  (define sccs
    (reverse
     (compute-sorted-strongly-connected-components
      (invert-graph (intmap-union fv-edges order-edges intset-union)))))
  (map (lambda (ids)
         (intset-fold-right (lambda (id out)
                              (cons (intmap-ref nodes id) out))
                            ids '()))
       sccs))

(define (fix-scc src binds body unreferenced? unassigned? recursive?)
  (match binds
    (((name sym init))
     ;; Case of an SCC containing just a single binding.
     (cond
      ((unreferenced? sym)
       (make-seq* src init body))
      ((and (lambda? init) (unassigned? sym))
       (make-fix src (list name) (list sym) (list init) body))
      ((recursive? init sym)
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
                               (and (lambda? init) (unassigned? sym)))))
                          binds))
       (lambda (l c)
         (define (bind-complex-vars body)
           (match c
             (() body)
             (((names syms inits) ...)
              (let ((inits (map (lambda (x) (make-void #f)) inits)))
                (make-let src names syms inits body)))))
         (define (bind-lambdas body)
           (match l
             (() body)
             (((names syms inits) ...)
              (make-fix src names syms inits body))))
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
                  sym->id referenced assigned complex
                  compute-free-variables)
  (define (unreferenced? sym)
    (not (intset-ref referenced (sym->id sym))))
  (define (unassigned? sym)
    (not (intset-ref assigned (sym->id sym))))
  (define (recursive? expr sym)
    (intset-ref (compute-free-variables expr) (sym->id sym)))
  (fold-right (lambda (binds body)
                (fix-scc src binds body
                         unreferenced? unassigned? recursive?))
              body
              (compute-sccs names gensyms vals in-order?
                            sym->id complex compute-free-variables)))

;; For letrec*, try to minimize false dependencies introduced by
;; ordering.
(define (reorder-bindings bindings sym->id compute-free-variables)
  (define (possibly-references? expr remaining-ids)
    (not (eq? empty-intset
              (intset-intersect (compute-free-variables expr)
                                remaining-ids))))
  (define (binding-ids bindings)
    (fold (lambda (binding ids)
            (match binding
              (#(name sym val) (intset-add ids (sym->id sym)))))
          empty-intset bindings))
  (let visit ((bindings bindings) (remaining-ids (binding-ids bindings))
              (sunk-lambdas '()) (sunk-exprs '()))
    (match bindings
      (() (append sunk-lambdas (reverse sunk-exprs)))
      ((binding . bindings)
       (match binding
         (#(_ sym expr)
          (let ((remaining-ids (intset-remove remaining-ids (sym->id sym))))
            (cond
             ((lambda? expr)
              (visit bindings remaining-ids
                     (cons binding sunk-lambdas) sunk-exprs))
             ((possibly-references? expr remaining-ids)
              ;; Init expression might refer to later bindings.
              ;; Serialize.
              (append sunk-lambdas (reverse sunk-exprs)
                      (cons binding (visit bindings remaining-ids '() '()))))
             (else
              (visit bindings remaining-ids
                     sunk-lambdas (cons binding sunk-exprs)))))))))))

(define (fix-letrec x)
  (define sym->id
    (compute-ids x))
  (define-values (referenced assigned)
    (compute-referenced-and-assigned x sym->id))
  (define complex
    (compute-complex x assigned sym->id))
  (define compute-free-variables
    (make-compute-free-variables sym->id))
  (define (unreferenced? sym)
    (not (intset-ref referenced (sym->id sym))))

  (post-order
   (lambda (x)
     (match x
       ;; Sets to unreferenced variables may be replaced by their
       ;; expression, called for effect.
       (($ <lexical-set> src name (? unreferenced?) exp)
        (make-seq* #f exp (make-void #f)))

       (($ <letrec> src in-order? names gensyms vals body)
        (if in-order?
            (match (reorder-bindings (map vector names gensyms vals)
                                     sym->id compute-free-variables)
              ((#(names gensyms vals) ...)
               (fix-term src #t names gensyms vals body
                         sym->id referenced assigned complex
                         compute-free-variables)))
            (fix-term src #f names gensyms vals body
                      sym->id referenced assigned complex
                      compute-free-variables)))

       (($ <let> src names gensyms vals body)
        ;; Apply the same algorithm to <let> that binds <lambda>
        (if (or-map lambda? vals)
            (fix-term src #f names gensyms vals body
                      sym->id referenced assigned complex
                      compute-free-variables)
            x))

       (_ x)))
   x))
