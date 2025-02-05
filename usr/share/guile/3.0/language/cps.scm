;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2015,2017-2018,2020,2021 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; This is the continuation-passing style (CPS) intermediate language
;;; (IL) for Guile.
;;;
;;; In CPS, a term is a labelled expression that calls a continuation.
;;; A function is a collection of terms.  No term belongs to more than
;;; one function.  The function is identified by the label of its entry
;;; term, and its body is composed of those terms that are reachable
;;; from the entry term.  A program is a collection of functions,
;;; identified by the entry label of the entry function.
;;;
;;; Terms are themselves wrapped in continuations, which specify how
;;; predecessors may continue to them.  For example, a $kargs
;;; continuation specifies that the term may be called with a specific
;;; number of values, and that those values will then be bound to
;;; lexical variables.  $kreceive specifies that some number of values
;;; will be passed on the stack, as from a multiple-value return.  Those
;;; values will be passed to a $kargs, if the number of values is
;;; compatible with the $kreceive's arity.  $kfun is an entry point to a
;;; function, and receives arguments according to a well-known calling
;;; convention (currently, on the stack) and the stack before
;;; dispatching to a $kclause.  A $kclause is a case-lambda clause, and
;;; only appears within a $kfun; it checks the incoming values for the
;;; correct arity and dispatches to a $kargs, or to the next clause.
;;; Finally, $ktail is the tail continuation for a function, and
;;; contains no term.
;;;
;;; Each continuation has a label that is unique in the program.  As an
;;; implementation detail, the labels are integers, which allows us to
;;; easily sort them topologically.  A program is a map from integers to
;;; continuations, where continuation 0 in the map is the entry point
;;; for the program, and is a $kfun of no arguments.
;;;
;;; $continue nodes call continuations.  The expression contained in the
;;; $continue node determines the value or values that are passed to the
;;; target continuation: $const to pass a constant value, $values to
;;; pass multiple named values, etc.  $continue nodes also record the
;;; source location corresponding to the expression.
;;;
;;; As mentioned above, a $kargs continuation can bind variables, if it
;;; receives incoming values.  $kfun also binds a value, corresponding
;;; to the closure being called.  A traditional CPS implementation will
;;; nest terms in each other, binding them in "let" forms, ensuring that
;;; continuations are declared and bound within the scope of the values
;;; that they may use.  In this way, the scope tree is a proof that
;;; variables are defined before they are used.  However, this proof is
;;; conservative; it is possible for a variable to always be defined
;;; before it is used, but not to be in scope:
;;;
;;;   (letrec ((k1 (lambda (v1) (k2)))
;;;            (k2 (lambda () v1)))
;;;     (k1 0))
;;;
;;; This example is invalid, as v1 is used outside its scope.  However
;;; it would be perfectly fine for k2 to use v1 if k2 were nested inside
;;; k1:
;;;
;;;   (letrec ((k1 (lambda (v1)
;;;                  (letrec ((k2 (lambda () v1)))
;;;                    (k2))))
;;;     (k1 0))
;;;
;;; Because program transformation usually uses flow-based analysis,
;;; having to update the scope tree to manifestly prove a transformation
;;; that has already proven correct is needless overhead, and in the
;;; worst case can prevent optimizations from occuring.  For that
;;; reason, Guile's CPS language does not nest terms.  Instead, we use
;;; the invariant that definitions must dominate uses.  To check the
;;; validity of a CPS program is thus more involved than checking for a
;;; well-scoped tree; you have to do flow analysis to determine a
;;; dominator tree.  However the flexibility that this grants us is
;;; worth the cost of throwing away the embedded proof of the scope
;;; tree.
;;;
;;; This particular formulation of CPS was inspired by Andrew Kennedy's
;;; 2007 paper, "Compiling with Continuations, Continued".  All Guile
;;; hackers should read that excellent paper!  As in Kennedy's paper,
;;; continuations are second-class, and may be thought of as basic block
;;; labels.  All values are bound to variables using continuation calls:
;;; even constants!
;;;
;;; Finally, note that there are two flavors of CPS: higher-order and
;;; first-order.  By "higher-order", we mean that variables may be free
;;; across function boundaries.  Higher-order CPS contains $fun and $rec
;;; expressions that declare functions in the scope of their term.
;;; Closure conversion results in first-order CPS, where closure
;;; representations have been explicitly chosen, and all variables used
;;; in a function are bound.  Higher-order CPS is good for
;;; interprocedural optimizations like contification and beta reduction,
;;; while first-order CPS is better for instruction selection, register
;;; allocation, and code generation.
;;;
;;; See (language tree-il compile-cps) for details on how Tree-IL
;;; converts to CPS.
;;;
;;; Code:

(define-module (language cps)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:export (;; Helper.
            $arity
            make-$arity

            ;; Continuations.
            $kreceive $kargs $kfun $ktail $kclause

            ;; Terms.
            $continue $branch $switch $prompt $throw

            ;; Expressions.
            $const $prim $fun $rec $const-fun $code
            $call $callk $calli $primcall $values

            ;; Building macros.
            build-cont build-term build-exp
            rewrite-cont rewrite-term rewrite-exp

            ;; External representation.
            parse-cps unparse-cps))

;; FIXME: Use SRFI-99, when Guile adds it.
(define-syntax define-record-type*
  (lambda (x)
    (define (id-append ctx . syms)
      (datum->syntax ctx (apply symbol-append (map syntax->datum syms))))
    (syntax-case x ()
      ((_ name field ...)
       (and (identifier? #'name) (and-map identifier? #'(field ...)))
       (with-syntax ((cons (id-append #'name #'make- #'name))
                     (pred (id-append #'name #'name #'?))
                     ((getter ...) (map (lambda (f)
                                          (id-append f #'name #'- f))
                                        #'(field ...))))
         #'(define-record-type name
             (cons field ...)
             pred
             (field getter)
             ...))))))

(define-syntax-rule (define-cps-type name field ...)
  (begin
    (define-record-type* name field ...)
    (set-record-type-printer! name print-cps)))

(define (print-cps exp port)
  (format port "#<cps ~S>" (unparse-cps exp)))

;; Helper.
(define-record-type* $arity req opt rest kw allow-other-keys?)

;; Continuations
(define-cps-type $kreceive arity kbody)
(define-cps-type $kargs names syms term)
(define-cps-type $kfun src meta self ktail kentry)
(define-cps-type $ktail)
(define-cps-type $kclause arity kbody kalternate)

;; Terms.
(define-cps-type $continue k src exp)
(define-cps-type $branch kf kt src op param args)
(define-cps-type $switch kf kt* src arg)
(define-cps-type $prompt k kh src escape? tag)
(define-cps-type $throw src op param args)

;; Expressions.
(define-cps-type $const val)
(define-cps-type $prim name)
(define-cps-type $fun body) ; Higher-order.
(define-cps-type $rec names syms funs) ; Higher-order.
(define-cps-type $const-fun label) ; First-order.
(define-cps-type $code label) ; First-order.
(define-cps-type $call proc args)
(define-cps-type $callk k proc args) ; First-order.
(define-cps-type $calli args callee) ; First-order.
(define-cps-type $primcall name param args)
(define-cps-type $values args)

(define-syntax build-arity
  (syntax-rules (unquote)
    ((_ (unquote exp)) exp)
    ((_ (req opt rest kw allow-other-keys?))
     (make-$arity req opt rest kw allow-other-keys?))))

(define-syntax build-cont
  (syntax-rules (unquote $kreceive $kargs $kfun $ktail $kclause)
    ((_ (unquote exp))
     exp)
    ((_ ($kreceive req rest kargs))
     (make-$kreceive (make-$arity req '() rest '() #f) kargs))
    ((_ ($kargs (name ...) (unquote syms) body))
     (make-$kargs (list name ...) syms (build-term body)))
    ((_ ($kargs (name ...) (sym ...) body))
     (make-$kargs (list name ...) (list sym ...) (build-term body)))
    ((_ ($kargs names syms body))
     (make-$kargs names syms (build-term body)))
    ((_ ($kfun src meta self ktail kentry))
     (make-$kfun src meta self ktail kentry))
    ((_ ($ktail))
     (make-$ktail))
    ((_ ($kclause arity kbody kalternate))
     (make-$kclause (build-arity arity) kbody kalternate))))

(define-syntax build-term
  (syntax-rules (unquote $continue $branch $switch $prompt $throw)
    ((_ (unquote exp))
     exp)
    ((_ ($continue k src exp))
     (make-$continue k src (build-exp exp)))
    ((_ ($branch kf kt src op param (unquote args)))
     (make-$branch kf kt src op param args))
    ((_ ($branch kf kt src op param (arg ...)))
     (make-$branch kf kt src op param (list arg ...)))
    ((_ ($branch kf kt src op param args))
     (make-$branch kf kt src op param args))
    ((_ ($switch kf kt* src arg))
     (make-$switch kf kt* src arg))
    ((_ ($prompt k kh src escape? tag))
     (make-$prompt k kh src escape? tag))
    ((_ ($throw src op param (unquote args)))
     (make-$throw src op param args))
    ((_ ($throw src op param (arg ...)))
     (make-$throw src op param (list arg ...)))
    ((_ ($throw src op param args))
     (make-$throw src op param args))))

(define-syntax build-exp
  (syntax-rules (unquote
                 $const $prim $fun $rec $const-fun $code
                 $call $callk $calli $primcall $values)
    ((_ (unquote exp)) exp)
    ((_ ($const val)) (make-$const val))
    ((_ ($prim name)) (make-$prim name))
    ((_ ($fun kentry)) (make-$fun kentry))
    ((_ ($rec names gensyms funs)) (make-$rec names gensyms funs))
    ((_ ($const-fun k)) (make-$const-fun k))
    ((_ ($code k)) (make-$code k))
    ((_ ($call proc (unquote args))) (make-$call proc args))
    ((_ ($call proc (arg ...))) (make-$call proc (list arg ...)))
    ((_ ($call proc args)) (make-$call proc args))
    ((_ ($callk k proc (unquote args))) (make-$callk k proc args))
    ((_ ($callk k proc (arg ...))) (make-$callk k proc (list arg ...)))
    ((_ ($callk k proc args)) (make-$callk k proc args))
    ((_ ($calli (unquote args) callee)) (make-$calli args callee))
    ((_ ($calli (arg ...) callee)) (make-$calli (list arg ...) callee))
    ((_ ($calli args callee)) (make-$calli args callee))
    ((_ ($primcall name param (unquote args))) (make-$primcall name param args))
    ((_ ($primcall name param (arg ...))) (make-$primcall name param (list arg ...)))
    ((_ ($primcall name param args)) (make-$primcall name param args))
    ((_ ($values (unquote args))) (make-$values args))
    ((_ ($values (arg ...))) (make-$values (list arg ...)))
    ((_ ($values args)) (make-$values args))))

(define-syntax-rule (rewrite-cont x (pat cont) ...)
  (match x
    (pat (build-cont cont)) ...))
(define-syntax-rule (rewrite-term x (pat term) ...)
  (match x
    (pat (build-term term)) ...))
(define-syntax-rule (rewrite-exp x (pat body) ...)
  (match x
    (pat (build-exp body)) ...))

(define (parse-cps exp)
  (define (src exp)
    (let ((props (source-properties exp)))
      (and (pair? props) props)))
  (match exp
    ;; Continuations.
    (('kreceive req rest k)
     (build-cont ($kreceive req rest k)))
    (('kargs names syms body)
     (build-cont ($kargs names syms ,(parse-cps body))))
    (('kfun meta self ktail kentry)
     (build-cont ($kfun (src exp) meta self ktail kentry)))
    (('ktail)
     (build-cont ($ktail)))
    (('kclause (req opt rest kw allow-other-keys?) kbody)
     (build-cont ($kclause (req opt rest kw allow-other-keys?) kbody #f)))
    (('kclause (req opt rest kw allow-other-keys?) kbody kalt)
     (build-cont ($kclause (req opt rest kw allow-other-keys?) kbody kalt)))

    ;; Terms.
    (('continue k exp)
     (build-term ($continue k (src exp) ,(parse-cps exp))))
    (('branch kf kt op param arg ...)
     (build-term ($branch kf kt (src exp) op param arg)))
    (('switch kf (kt* ...) arg)
     (build-term ($switch kf kt* (src exp) arg)))
    (('prompt k kh escape? tag)
     (build-term ($prompt k kh (src exp) escape? tag)))
    (('throw op param arg ...)
     (build-term ($throw (src exp) op param arg)))

    ;; Expressions.
    (('unspecified)
     (build-exp ($const *unspecified*)))
    (('const exp)
     (build-exp ($const exp)))
    (('prim name)
     (build-exp ($prim name)))
    (('fun kbody)
     (build-exp ($fun kbody)))
    (('const-fun k)
     (build-exp ($const-fun k)))
    (('code k)
     (build-exp ($code k)))
    (('rec (name sym fun) ...)
     (build-exp ($rec name sym (map parse-cps fun))))
    (('call proc arg ...)
     (build-exp ($call proc arg)))
    (('callk k proc arg ...)
     (build-exp ($callk k proc arg)))
    (('calli arg ... callee)
     (build-exp ($calli arg callee)))
    (('primcall name param arg ...)
     (build-exp ($primcall name param arg)))
    (('values arg ...)
     (build-exp ($values arg)))
    (_
     (error "unexpected cps" exp))))

(define (unparse-cps exp)
  (match exp
    ;; Continuations.
    (($ $kreceive ($ $arity req () rest () #f) k)
     `(kreceive ,req ,rest ,k))
    (($ $kargs names syms body)
     `(kargs ,names ,syms ,(unparse-cps body)))
    (($ $kfun src meta self ktail kentry)
     `(kfun ,meta ,self ,ktail ,kentry))
    (($ $ktail)
     `(ktail))
    (($ $kclause ($ $arity req opt rest kw allow-other-keys?) kbody kalternate)
     `(kclause (,req ,opt ,rest ,kw ,allow-other-keys?) ,kbody
               . ,(if kalternate (list kalternate) '())))

    ;; Terms.
    (($ $continue k src exp)
     `(continue ,k ,(unparse-cps exp)))
    (($ $branch kf kt src op param args)
     `(branch ,kf ,kt ,op ,param ,@args))
    (($ $switch kf kt* src arg)
     `(switch ,kf ,kt* ,arg))
    (($ $prompt k kh src escape? tag)
     `(prompt ,k ,kh ,escape? ,tag))
    (($ $throw src op param args)
     `(throw ,op ,param ,@args))

    ;; Expressions.
    (($ $const val)
     (if (unspecified? val)
         '(unspecified)
         `(const ,val)))
    (($ $prim name)
     `(prim ,name))
    (($ $fun kbody)
     `(fun ,kbody))
    (($ $const-fun k)
     `(const-fun ,k))
    (($ $code k)
     `(code ,k))
    (($ $rec names syms funs)
     `(rec ,@(map (lambda (name sym fun)
                    (list name sym (unparse-cps fun)))
                  names syms funs)))
    (($ $call proc args)
     `(call ,proc ,@args))
    (($ $callk k proc args)
     `(callk ,k ,proc ,@args))
    (($ $calli args callee)
     `(callk ,@args ,callee))
    (($ $primcall name param args)
     `(primcall ,name ,param ,@args))
    (($ $values args)
     `(values ,@args))
    (_
     (error "unexpected cps" exp))))
