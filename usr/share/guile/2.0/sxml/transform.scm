;;;; (sxml transform) -- pre- and post-order sxml transformation
;;;;
;;;; 	Copyright (C) 2009  Free Software Foundation, Inc.
;;;;    Modified 2004 by Andy Wingo <wingo at pobox dot com>.
;;;;    Written 2003 by Oleg Kiselyov <oleg at pobox dot com> as SXML-tree-trans.scm.
;;;; 
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
;;;; 

;;; Commentary:
;;
;;@heading SXML expression tree transformers
;
;@subheading Pre-Post-order traversal of a tree and creation of a new tree
;@smallexample
;pre-post-order:: <tree> x <bindings> -> <new-tree>
;@end smallexample
; where
;@smallexample
; <bindings> ::= (<binding> ...)
; <binding> ::= (<trigger-symbol> *preorder* . <handler>) |
;               (<trigger-symbol> *macro* . <handler>) |
;		(<trigger-symbol> <new-bindings> . <handler>) |
;		(<trigger-symbol> . <handler>)
; <trigger-symbol> ::= XMLname | *text* | *default*
; <handler> :: <trigger-symbol> x [<tree>] -> <new-tree>
;@end smallexample
;
; The pre-post-order function visits the nodes and nodelists
; pre-post-order (depth-first). For each @code{<Node>} of the form
; @code{(@var{name} <Node> ...)}, it looks up an association with the
; given @var{name} among its @var{<bindings>}. If failed,
; @code{pre-post-order} tries to locate a @code{*default*} binding. It's
; an error if the latter attempt fails as well. Having found a binding,
; the @code{pre-post-order} function first checks to see if the binding
; is of the form
;@smallexample
;	(<trigger-symbol> *preorder* . <handler>)
;@end smallexample
;
; If it is, the handler is 'applied' to the current node. Otherwise, the
; pre-post-order function first calls itself recursively for each child
; of the current node, with @var{<new-bindings>} prepended to the
; @var{<bindings>} in effect. The result of these calls is passed to the
; @var{<handler>} (along with the head of the current @var{<Node>}). To
; be more precise, the handler is _applied_ to the head of the current
; node and its processed children. The result of the handler, which
; should also be a @code{<tree>}, replaces the current @var{<Node>}. If
; the current @var{<Node>} is a text string or other atom, a special
; binding with a symbol @code{*text*} is looked up.
;
; A binding can also be of a form
;@smallexample
;	(<trigger-symbol> *macro* . <handler>)
;@end smallexample
; This is equivalent to @code{*preorder*} described above. However, the
; result is re-processed again, with the current stylesheet.
;;
;;; Code:

(define-module (sxml transform)
  #:export (SRV:send-reply
            foldts
            post-order
            pre-post-order
            replace-range))

;; Upstream version:
; $Id: SXML-tree-trans.scm,v 1.8 2003/04/24 19:39:53 oleg Exp oleg $

; Like let* but allowing for multiple-value bindings
(define-macro (let*-values bindings . body)
  (if (null? bindings) (cons 'begin body)
      (apply
       (lambda (vars initializer)
	 (let ((cont 
		(cons 'let*-values
		      (cons (cdr bindings) body))))
	   (cond
	    ((not (pair? vars))		; regular let case, a single var
	     `(let ((,vars ,initializer)) ,cont))
	    ((null? (cdr vars))		; single var, see the prev case
	     `(let ((,(car vars) ,initializer)) ,cont))
	   (else			; the most generic case
	    `(call-with-values (lambda () ,initializer)
	      (lambda ,vars ,cont))))))
       (car bindings))))

(define (SRV:send-reply . fragments)
  "Output the @var{fragments} to the current output port.

The fragments are a list of strings, characters, numbers, thunks,
@code{#f}, @code{#t} -- and other fragments. The function traverses the
tree depth-first, writes out strings and characters, executes thunks,
and ignores @code{#f} and @code{'()}. The function returns @code{#t} if
anything was written at all; otherwise the result is @code{#f} If
@code{#t} occurs among the fragments, it is not written out but causes
the result of @code{SRV:send-reply} to be @code{#t}."
  (let loop ((fragments fragments) (result #f))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((eq? #t (car fragments)) (loop (cdr fragments) #t))
      ((pair? (car fragments))
        (loop (cdr fragments) (loop (car fragments) result)))
      ((procedure? (car fragments))
        ((car fragments))
        (loop (cdr fragments) #t))
      (else
        (display (car fragments))
        (loop (cdr fragments) #t)))))



;------------------------------------------------------------------------
;	          Traversal of an SXML tree or a grove:
;			a <Node> or a <Nodelist>
;
; A <Node> and a <Nodelist> are mutually-recursive datatypes that
; underlie the SXML tree:
;	<Node> ::= (name . <Nodelist>) | "text string"
; An (ordered) set of nodes is just a list of the constituent nodes:
; 	<Nodelist> ::= (<Node> ...)
; Nodelists, and Nodes other than text strings are both lists. A
; <Nodelist> however is either an empty list, or a list whose head is
; not a symbol (an atom in general). A symbol at the head of a node is
; either an XML name (in which case it's a tag of an XML element), or
; an administrative name such as '@'.
; See SXPath.scm and SSAX.scm for more information on SXML.


;; see the commentary for docs
(define (pre-post-order tree bindings)
  (let* ((default-binding (assq '*default* bindings))
	 (text-binding (or (assq '*text* bindings) default-binding))
	 (text-handler			; Cache default and text bindings
	   (and text-binding
	     (if (procedure? (cdr text-binding))
	         (cdr text-binding) (cddr text-binding)))))
    (let loop ((tree tree))
      (cond
	((null? tree) '())
	((not (pair? tree))
	  (let ((trigger '*text*))
	    (if text-handler (text-handler trigger tree)
	      (error "Unknown binding for " trigger " and no default"))))
	((not (symbol? (car tree))) (map loop tree)) ; tree is a nodelist
	(else				; tree is an SXML node
	  (let* ((trigger (car tree))
		 (binding (or (assq trigger bindings) default-binding)))
	    (cond
	      ((not binding) 
		(error "Unknown binding for " trigger " and no default"))
	      ((not (pair? (cdr binding)))  ; must be a procedure: handler
		(apply (cdr binding) trigger (map loop (cdr tree))))
	      ((eq? '*preorder* (cadr binding))
		(apply (cddr binding) tree))
	      ((eq? '*macro* (cadr binding))
		(loop (apply (cddr binding) tree)))
	      (else			    ; (cadr binding) is a local binding
		(apply (cddr binding) trigger 
		  (pre-post-order (cdr tree) (append (cadr binding) bindings)))
		))))))))

; post-order is a strict subset of pre-post-order without *preorder*
; (let alone *macro*) traversals. 
; Now pre-post-order is actually faster than the old post-order.
; The function post-order is deprecated and is aliased below for
; backward compatibility.
(define post-order pre-post-order)

;------------------------------------------------------------------------
;			Extended tree fold
; tree = atom | (node-name tree ...)
;
; foldts fdown fup fhere seed (Leaf str) = fhere seed str
; foldts fdown fup fhere seed (Nd kids) =
;         fup seed $ foldl (foldts fdown fup fhere) (fdown seed) kids

; procedure fhere: seed -> atom -> seed
; procedure fdown: seed -> node -> seed
; procedure fup: parent-seed -> last-kid-seed -> node -> seed
; foldts returns the final seed

(define (foldts fdown fup fhere seed tree)
  (cond
   ((null? tree) seed)
   ((not (pair? tree))		; An atom
    (fhere seed tree))
   (else
    (let loop ((kid-seed (fdown seed tree)) (kids (cdr tree)))
      (if (null? kids)
	  (fup seed kid-seed tree)
	  (loop (foldts fdown fup fhere kid-seed (car kids))
		(cdr kids)))))))

;------------------------------------------------------------------------
; Traverse a forest depth-first and cut/replace ranges of nodes.
;
; The nodes that define a range don't have to have the same immediate
; parent, don't have to be on the same level, and the end node of a
; range doesn't even have to exist. A replace-range procedure removes
; nodes from the beginning node of the range up to (but not including)
; the end node of the range.  In addition, the beginning node of the
; range can be replaced by a node or a list of nodes. The range of
; nodes is cut while depth-first traversing the forest. If all
; branches of the node are cut a node is cut as well.  The procedure
; can cut several non-overlapping ranges from a forest.

;	replace-range:: BEG-PRED x END-PRED x FOREST -> FOREST
; where
;	type FOREST = (NODE ...)
;	type NODE = Atom | (Name . FOREST) | FOREST
;
; The range of nodes is specified by two predicates, beg-pred and end-pred.
;	beg-pred:: NODE -> #f | FOREST
;	end-pred:: NODE -> #f | FOREST
; The beg-pred predicate decides on the beginning of the range. The node
; for which the predicate yields non-#f marks the beginning of the range
; The non-#f value of the predicate replaces the node. The value can be a
; list of nodes. The replace-range procedure then traverses the tree and skips
; all the nodes, until the end-pred yields non-#f. The value of the end-pred
; replaces the end-range node. The new end node and its brothers will be
; re-scanned.
; The predicates are evaluated pre-order. We do not descend into a node that
; is marked as the beginning of the range.

(define (replace-range beg-pred end-pred forest)

  ; loop forest keep? new-forest
  ; forest is the forest to traverse
  ; new-forest accumulates the nodes we will keep, in the reverse
  ; order
  ; If keep? is #t, keep the curr node if atomic. If the node is not atomic,
  ; traverse its children and keep those that are not in the skip range.
  ; If keep? is #f, skip the current node if atomic. Otherwise,
  ; traverse its children. If all children are skipped, skip the node
  ; as well.

  (define (loop forest keep? new-forest)
    (if (null? forest) (values (reverse new-forest) keep?)
	(let ((node (car forest)))
	  (if keep?
	      (cond			; accumulate mode
	       ((beg-pred node) =>	; see if the node starts the skip range
		(lambda (repl-branches)	; if so, skip/replace the node
		  (loop (cdr forest) #f 
			(append (reverse repl-branches) new-forest))))
	       ((not (pair? node))	; it's an atom, keep it
		(loop (cdr forest) keep? (cons node new-forest)))
	       (else
		(let*-values
		 (((node?) (symbol? (car node))) ; or is it a nodelist?
		  ((new-kids keep?)		 ; traverse its children
		   (loop (if node? (cdr node) node) #t '())))
		 (loop (cdr forest) keep?
		       (cons 
			(if node? (cons (car node) new-kids) new-kids)
			new-forest)))))
	      ; skip mode
	      (cond
	       ((end-pred node) =>	; end the skip range
		(lambda (repl-branches)	; repl-branches will be re-scanned
		  (loop (append repl-branches (cdr forest)) #t
			new-forest)))
	       ((not (pair? node))	; it's an atom, skip it
		(loop (cdr forest) keep? new-forest))
	       (else
		(let*-values
		 (((node?) (symbol? (car node)))  ; or is it a nodelist?
		  ((new-kids keep?)		  ; traverse its children
		   (loop (if node? (cdr node) node) #f '())))
		 (loop (cdr forest) keep?
		       (if (or keep? (pair? new-kids))
			   (cons
			    (if node? (cons (car node) new-kids) new-kids)
			    new-forest)
			   new-forest)		; if all kids are skipped
		       ))))))))			; skip the node too
  
  (let*-values (((new-forest keep?) (loop forest #t '())))
     new-forest))

;;; arch-tag: 6c814f4b-38f7-42c1-b8ef-ce3447edefc7
;;; transform.scm ends here
