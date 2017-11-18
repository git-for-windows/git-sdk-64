;;;; (sxml apply-templates) -- xslt-like transformation for sxml
;;;;
;;;; 	Copyright (C) 2009 Free Software Foundation, Inc.
;;;;    Copyright 2004 by Andy Wingo <wingo at pobox dot com>.
;;;;    Written 2003 by Oleg Kiselyov <oleg at pobox dot com> as apply-templates.scm.
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
;; Pre-order traversal of a tree and creation of a new tree:
;;
;;@smallexample
;;	apply-templates:: tree x <templates> -> <new-tree>
;;@end smallexample
;; where
;;@smallexample
;; <templates> ::= (<template> ...)
;; <template>  ::= (<node-test> <node-test> ... <node-test> . <handler>)
;; <node-test> ::= an argument to node-typeof? above
;; <handler>   ::= <tree> -> <new-tree>
;;@end smallexample
;;
;; This procedure does a @emph{normal}, pre-order traversal of an SXML
;; tree.  It walks the tree, checking at each node against the list of
;; matching templates.
;;
;; If the match is found (which must be unique, i.e., unambiguous), the
;; corresponding handler is invoked and given the current node as an
;; argument. The result from the handler, which must be a @code{<tree>},
;; takes place of the current node in the resulting tree.
;; 
;; The name of the function is not accidental: it resembles rather
;; closely an @code{apply-templates} function of XSLT.
;;
;;; Code:

(define-module (sxml apply-templates)
  #:use-module (sxml ssax)
  #:use-module ((sxml xpath) :hide (filter))
                         
  #:export (apply-templates))

(define (apply-templates tree templates)

		; Filter the list of templates. If a template does not
		; contradict the given node (that is, its head matches
		; the type of the node), chop off the head and keep the
		; rest as the result. All contradicting templates are removed.
  (define (filter-templates node templates)
    (cond
     ((null? templates) templates)
     ((not (pair? (car templates)))  ; A good template must be a list
      (filter-templates node (cdr templates)))
     (((node-typeof? (caar templates)) node)
      (cons (cdar templates) (filter-templates node (cdr templates))))
     (else
      (filter-templates node (cdr templates)))))

		; Here <templates> ::= [<template> | <handler>]
		; If there is a <handler> in the above list, it must
		; be only one. If found, return it; otherwise, return #f
  (define (find-handler templates)
    (and (pair? templates)
	 (cond
	  ((procedure? (car templates))
	   (if (find-handler (cdr templates))
	       (error "ambiguous template match"))
	   (car templates))
	  (else (find-handler (cdr templates))))))

  (let loop ((tree tree) (active-templates '()))
   ;(cout "active-templates: " active-templates nl "tree: " tree nl)
    (if (nodeset? tree)
	(map-union (lambda (a-tree) (loop a-tree active-templates)) tree)
	(let ((still-active-templates 
	       (append 
		(filter-templates tree active-templates)
		(filter-templates tree templates))))
	  (cond 
	   ;((null? still-active-templates) '())
	   ((find-handler still-active-templates) =>
	    (lambda (handler) (handler tree)))
	   ((not (pair? tree)) '())
	   (else
	    (loop (cdr tree) still-active-templates)))))))

;;; arch-tag: 88cd87de-8825-4ab3-9721-cf99694fb787
;;; templates.scm ends here
