;;;; (sxml fold) -- transformation of sxml via fold operations
;;;;
;;;; 	Copyright (C) 2009, 2010  Free Software Foundation, Inc.
;;;;    Written 2007 by Andy Wingo <wingo at pobox dot com>.
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
;; @code{(sxml fold)} defines a number of variants of the @dfn{fold}
;; algorithm for use in transforming SXML trees. Additionally it defines
;; the layout operator, @code{fold-layout}, which might be described as
;; a context-passing variant of SSAX's @code{pre-post-order}.
;;
;;; Code:

(define-module (sxml fold)
  #:use-module (srfi srfi-1)
  #:export (foldt
            foldts
            foldts*
            fold-values
            foldts*-values
            fold-layout))

(define (atom? x)
  (not (pair? x)))

(define (foldt fup fhere tree)
  "The standard multithreaded tree fold.

@var{fup} is of type [a] -> a. @var{fhere} is of type object -> a.
"
  (if (atom? tree)
      (fhere tree)
      (fup (map (lambda (kid)
                  (foldt fup fhere kid))
                tree))))

(define (foldts fdown fup fhere seed tree)
  "The single-threaded tree fold originally defined in SSAX.
@xref{sxml ssax,,(sxml ssax)}, for more information."
  (if (atom? tree)
      (fhere seed tree)
      (fup seed
           (fold (lambda (kid kseed)
                  (foldts fdown fup fhere kseed kid))
                 (fdown seed tree)
                 tree)
           tree)))

(define (foldts* fdown fup fhere seed tree)
  "A variant of @ref{sxml fold foldts,,foldts} that allows pre-order
tree rewrites. Originally defined in Andy Wingo's 2007 paper,
@emph{Applications of fold to XML transformation}."
  (if (atom? tree)
      (fhere seed tree)
      (call-with-values
          (lambda () (fdown seed tree))
        (lambda (kseed tree)
          (fup seed
               (fold (lambda (kid kseed)
                       (foldts* fdown fup fhere
                                kseed kid))
                     kseed
                     tree)
               tree)))))

(define (fold-values proc list . seeds)
  "A variant of @ref{SRFI-1 Fold and Map, fold} that allows multi-valued
seeds. Note that the order of the arguments differs from that of
@code{fold}."
  (if (null? list)
      (apply values seeds)
      (call-with-values
          (lambda () (apply proc (car list) seeds))
        (lambda seeds
          (apply fold-values proc (cdr list) seeds)))))

(define (foldts*-values fdown fup fhere tree . seeds)
  "A variant of @ref{sxml fold foldts*,,foldts*} that allows
multi-valued seeds. Originally defined in Andy Wingo's 2007 paper,
@emph{Applications of fold to XML transformation}."
  (if (atom? tree)
      (apply fhere tree seeds)
      (call-with-values
          (lambda () (apply fdown tree seeds))
        (lambda (tree . kseeds)
          (call-with-values
              (lambda ()
                (apply fold-values
                       (lambda (tree . seeds)
                         (apply foldts*-values
                                fdown fup fhere tree seeds))
                       tree kseeds))
            (lambda kseeds
              (apply fup tree (append seeds kseeds))))))))

(define (assq-ref alist key default)
  (cond ((assq key alist) => cdr)
        (else default)))

(define (fold-layout tree bindings params layout stylesheet)
  "A traversal combinator in the spirit of SSAX's @ref{sxml transform
pre-post-order,,pre-post-order}.

@code{fold-layout} was originally presented in Andy Wingo's 2007 paper,
@emph{Applications of fold to XML transformation}.

@example
bindings := (<binding>...)
binding  := (<tag> <bandler-pair>...)
          | (*default* . <post-handler>)
          | (*text* . <text-handler>)
tag      := <symbol>
handler-pair := (pre-layout . <pre-layout-handler>)
          | (post . <post-handler>)
          | (bindings . <bindings>)
          | (pre . <pre-handler>)
          | (macro . <macro-handler>)
@end example

@table @var
@item pre-layout-handler
A function of three arguments:

@table @var
@item kids
the kids of the current node, before traversal
@item params
the params of the current node
@item layout
the layout coming into this node
@end table

@var{pre-layout-handler} is expected to use this information to return a
layout to pass to the kids. The default implementation returns the
layout given in the arguments.

@item post-handler
A function of five arguments:
@table @var
@item tag
the current tag being processed
@item params
the params of the current node
@item layout
the layout coming into the current node, before any kids were processed
@item klayout
the layout after processing all of the children
@item kids
the already-processed child nodes
@end table

@var{post-handler} should return two values, the layout to pass to the
next node and the final tree.

@item text-handler
@var{text-handler} is a function of three arguments:
@table @var
@item text
the string
@item params
the current params
@item layout
the current layout
@end table

@var{text-handler} should return two values, the layout to pass to the
next node and the value to which the string should transform.
@end table
"
  (define (err . args)
    (error "no binding available" args))
  (define (fdown tree bindings pcont params layout ret)
    (define (fdown-helper new-bindings new-layout cont)
      (let ((cont-with-tag (lambda args
                             (apply cont (car tree) args)))
            (bindings (if new-bindings
                          (append new-bindings bindings)
                          bindings))
            (style-params (assq-ref stylesheet (car tree) '())))
        (cond
         ((null? (cdr tree))
          (values
           '() bindings cont-with-tag (cons style-params params) new-layout '()))
         ((and (pair? (cadr tree)) (eq? (caadr tree) '@))
          (let ((params (cons (append (cdadr tree) style-params) params)))
            (values
             (cddr tree) bindings cont-with-tag params new-layout '())))
         (else
          (values
           (cdr tree) bindings cont-with-tag (cons style-params params) new-layout '())))))
    (define (no-bindings)
      (fdown-helper #f layout (assq-ref bindings '*default* err)))
    (define (macro macro-handler)
      (fdown (apply macro-handler tree)
             bindings pcont params layout ret))
    (define (pre pre-handler)
      (values '() bindings 
              (lambda (params layout old-layout kids)
                (values layout (reverse kids)))
              params layout (apply pre-handler tree)))
    (define (have-bindings tag-bindings)
      (fdown-helper
       (assq-ref tag-bindings 'bindings #f)
       ((assq-ref tag-bindings 'pre-layout
                  (lambda (tag params layout)
                    layout))
        tree params layout)
       (assq-ref tag-bindings 'post
                 (assq-ref bindings '*default* err))))
    (let ((tag-bindings (assq-ref bindings (car tree) #f)))
      (cond
       ((not tag-bindings) (no-bindings))
       ((assq-ref tag-bindings 'macro #f) => macro)
       ((assq-ref tag-bindings 'pre #f) => pre)
       (else (have-bindings tag-bindings)))))
  (define (fup tree bindings cont params layout ret
               kbindings kcont kparams klayout kret)
    (call-with-values
        (lambda ()
          (kcont kparams layout klayout (reverse kret)))
      (lambda (klayout kret)
        (values bindings cont params klayout (cons kret ret)))))
  (define (fhere tree bindings cont params layout ret)
    (call-with-values
        (lambda ()
          ((assq-ref bindings '*text* err) tree params layout))
      (lambda (tlayout tret)
        (values bindings cont params tlayout (cons tret ret)))))
  (call-with-values
      (lambda ()
        (foldts*-values
         fdown fup fhere tree bindings #f (cons params '()) layout '()))
    (lambda (bindings cont params layout ret)
      (values (car ret) layout))))
