;;; Making lexically-bound procedures well-known

;; Copyright (C) 2020 Free Software Foundation, Inc.

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

(define-module (language tree-il eta-expand)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:export (eta-expand))

;; A lexically-bound procedure that is used only in operator position --
;; i.e. the F in (F ARG ...) -- is said to be "well-known" if all of
;; its use sites are calls and they can all be enumerated.  Well-known
;; procedures can be optimized in a number of important ways:
;; contification, call-by-label, shared closures, optimized closure
;; representation, and closure elision.
;;
;; All procedures in a source program can be converted to become
;; well-known by eta-expansion: wrapping them in a `lambda' that
;; dispatches to the target procedure.  However, reckless eta-expansion
;; has two downsides.  One drawback is that in some use cases,
;; eta-expansion just adds wrappers for no purpose: if there aren't
;; other uses of the procedure in operator position that could have
;; gotten the call-by-label treatment and closure optimization, there's
;; no point in making the closure well-known.
;;
;; The other drawback is that eta-expansion can confuse users who expect
;; a `lambda' term in a source program to have a unique object identity.
;; One might expect to associate a procedure with a value in an alist
;; and then look up that value later on, but if the looked-up procedure
;; is an eta-expanded wrapper, it won't be `eq?' to the previously-added
;; procedure.  While this behavior is permitted by the R6RS, it breaks
;; user expectations, often for no good reason due to the first problem.
;;
;; Therefore in Guile we have struck a balance: we will eta-expand
;; procedures that are:
;;   - lexically bound 
;;   - not assigned
;;   - referenced at least once in operator position
;;   - referenced at most once in value position
;;
;; These procedures will be eta-expanded in value position only.  (We do
;; this by eta-expanding all qualifying references, then reducing those
;; expanded in call position.)
;;
;; In this way eta-expansion avoids introducing new procedure
;; identities.
;;
;; Additionally, for implementation simplicity we restrict to procedures
;; that only have required and possibly rest arguments.

(define for-each-fold (make-tree-il-folder))
(define (tree-il-for-each f x)
  (for-each-fold x (lambda (x) (f x) (values)) (lambda (x) (values))))

(define (eta-expand expr)
  (define (analyze-procs)
    (define (proc-info proc)
      (vector 0 0 proc))
    (define (set-refcount! info count)
      (vector-set! info 0 count))
    (define (set-op-refcount! info count)
      (vector-set! info 1 count))
    (define proc-infos (make-hash-table))
    (define (maybe-add-proc! gensym val)
      (match val
        (($ <lambda> src1 meta
            ($ <lambda-case> src2 req #f rest #f () syms body #f))
         (hashq-set! proc-infos gensym (proc-info val)))
        (_ #f)))
    (tree-il-for-each
     (lambda (expr)
       (match expr
         (($ <lexical-ref> src name gensym)
          (match (hashq-ref proc-infos gensym)
            (#f #f)
            ((and info #(total op proc))
             (set-refcount! info (1+ total)))))

         (($ <lexical-set> src name gensym)
          (hashq-remove! proc-infos gensym))

         (($ <call> src1 ($ <lexical-ref> src2 name gensym) args)
          (match (hashq-ref proc-infos gensym)
            (#f #f)
            ((and info #(total op proc))
             (set-op-refcount! info (1+ op)))))

         (($ <let> src names gensyms vals body)
          (for-each maybe-add-proc! gensyms vals))

         (($ <letrec> src in-order? names gensyms vals body)
          (for-each maybe-add-proc! gensyms vals))

         (($ <fix> src names gensyms vals body)
          (for-each maybe-add-proc! gensyms vals))

         (_ #f)))
     expr)
    (define to-expand (make-hash-table))
    (hash-for-each (lambda (sym info)
                     (match info
                       (#(total op proc)
                        (when (and (not (zero? op))
                                   (= (- total op) 1))
                          (hashq-set! to-expand sym proc)))))
                   proc-infos)
    to-expand)

  (let ((to-expand (analyze-procs)))
    (define (eta-expand lexical)
      (match lexical
        (($ <lexical-ref> src name sym)
         (match (hashq-ref to-expand sym)
           (#f #f)
           (($ <lambda> src1 meta
               ($ <lambda-case> src2 req #f rest #f () syms body #f))
            (let* ((syms (map gensym (map symbol->string syms)))
                   (args (map (lambda (req sym) (make-lexical-ref src2 req sym))
                              (if rest (append req (list rest)) req)
                              syms))
                   (body (if rest
                             (make-primcall src 'apply (cons lexical args))
                             (make-call src lexical args))))
              (make-lambda src1 meta
                           (make-lambda-case src2 req #f rest #f '() syms
                                             body #f))))))))
    (define (eta-reduce proc)
      (match proc
        (($ <lambda> _ meta
            ($ <lambda-case> _ req #f #f #f () syms
               ($ <call> src ($ <lexical-ref> _ name sym)
                  (($ <lexical-ref> _ _ arg) ...))
               #f))
         (and (equal? arg syms)
              (make-lexical-ref src name sym)))
        (($ <lambda> _ meta
            ($ <lambda-case> _ req #f (not #f) #f () syms
               ($ <primcall> src 'apply 
                  (($ <lexical-ref> _ name sym) ($ <lexical-ref> _ _ arg) ...))
               #f))
         (and (equal? arg syms)
              (make-lexical-ref src name sym)))
        (_ #f)))
    (post-order
     (lambda (expr)
       (match expr
         (($ <lexical-ref>)
          (or (eta-expand expr)
              expr))

         (($ <call> src proc args)
          (match (eta-reduce proc)
            (#f expr)
            (proc (make-call src proc args))))

         (_ expr)))
     expr)))
