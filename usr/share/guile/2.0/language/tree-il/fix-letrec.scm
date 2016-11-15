;;; transformation of letrec into simpler forms

;; Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

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
  #:use-module (language tree-il)
  #:use-module (language tree-il effects)
  #:export (fix-letrec!))

;; For a detailed discussion, see "Fixing Letrec: A Faithful Yet
;; Efficient Implementation of Scheme's Recursive Binding Construct", by
;; Oscar Waddell, Dipanwita Sarkar, and R. Kent Dybvig.

(define fix-fold
  (make-tree-il-folder unref ref set simple lambda complex))

(define (simple-expression? x bound-vars simple-primcall?)
  (record-case x
    ((<void>) #t)
    ((<const>) #t)
    ((<lexical-ref> gensym)
     (not (memq gensym bound-vars)))
    ((<conditional> test consequent alternate)
     (and (simple-expression? test bound-vars simple-primcall?)
          (simple-expression? consequent bound-vars simple-primcall?)
          (simple-expression? alternate bound-vars simple-primcall?)))
    ((<sequence> exps)
     (and-map (lambda (x) (simple-expression? x bound-vars simple-primcall?))
              exps))
    ((<application> proc args)
     (and (primitive-ref? proc)
          (simple-primcall? x)
          (and-map (lambda (x)
                     (simple-expression? x bound-vars simple-primcall?))
                   args)))
    (else #f)))

(define (partition-vars x)
  (let-values
      (((unref ref set simple lambda* complex)
        (fix-fold x
                  (lambda (x unref ref set simple lambda* complex)
                    (record-case x
                      ((<lexical-ref> gensym)
                       (values (delq gensym unref)
                               (lset-adjoin eq? ref gensym)
                               set
                               simple
                               lambda*
                               complex))
                      ((<lexical-set> gensym)
                       (values unref
                               ref
                               (lset-adjoin eq? set gensym)
                               simple
                               lambda*
                               complex))
                      ((<letrec> gensyms)
                       (values (append gensyms unref)
                               ref
                               set
                               simple
                               lambda*
                               complex))
                      ((<let> gensyms)
                       (values (append gensyms unref)
                               ref
                               set
                               simple
                               lambda*
                               complex))
                      (else
                       (values unref ref set simple lambda* complex))))
                  (lambda (x unref ref set simple lambda* complex)
                    (record-case x
                      ((<letrec> in-order? (orig-gensyms gensyms) vals)
                       (define compute-effects
                         (make-effects-analyzer (lambda (x) (memq x set))))
                       (define (effect-free-primcall? x)
                         (let ((effects (compute-effects x)))
                           (effect-free?
                            (exclude-effects effects (logior &allocation
                                                             &type-check)))))
                       (define (effect+exception-free-primcall? x)
                         (let ((effects (compute-effects x)))
                           (effect-free?
                            (exclude-effects effects &allocation))))
                       (let lp ((gensyms orig-gensyms) (vals vals)
                                (s '()) (l '()) (c '()))
                         (cond
                          ((null? gensyms)
                           ;; Unreferenced complex vars are still
                           ;; complex for letrec*.  We need to update
                           ;; our algorithm to "Fixing letrec reloaded"
                           ;; to fix this.
                           (values (if in-order?
                                       (lset-difference eq? unref c)
                                       unref)
                                   ref
                                   set
                                   (append s simple)
                                   (append l lambda*)
                                   (append c complex)))
                          ((memq (car gensyms) unref)
                           ;; See above note about unref and letrec*.
                           (if (and in-order?
                                    (not (lambda? (car vals)))
                                    (not (simple-expression?
                                          (car vals) orig-gensyms
                                          effect+exception-free-primcall?)))
                               (lp (cdr gensyms) (cdr vals)
                                   s l (cons (car gensyms) c))
                               (lp (cdr gensyms) (cdr vals)
                                   s l c)))
                          ((memq (car gensyms) set)
                           (lp (cdr gensyms) (cdr vals)
                               s l (cons (car gensyms) c)))
                          ((lambda? (car vals))
                           (lp (cdr gensyms) (cdr vals)
                               s (cons (car gensyms) l) c))
                          ((simple-expression?
                            (car vals) orig-gensyms
                            (if in-order?
                                effect+exception-free-primcall?
                                effect-free-primcall?))
                           ;; For letrec*, we can't consider e.g. `car' to be
                           ;; "simple", as it could raise an exception. Hence
                           ;; effect+exception-free-primitive? above.
                           (lp (cdr gensyms) (cdr vals)
                               (cons (car gensyms) s) l c))
                          (else
                           (lp (cdr gensyms) (cdr vals)
                               s l (cons (car gensyms) c))))))
                      ((<let> (orig-gensyms gensyms) vals)
                       ;; The point is to compile let-bound lambdas as
                       ;; efficiently as we do letrec-bound lambdas, so
                       ;; we use the same algorithm for analyzing the
                       ;; gensyms. There is no problem recursing into the
                       ;; bindings after the let, because all variables
                       ;; have been renamed.
                       (let lp ((gensyms orig-gensyms) (vals vals)
                                (s '()) (l '()) (c '()))
                         (cond
                          ((null? gensyms)
                           (values unref
                                   ref
                                   set
                                   (append s simple)
                                   (append l lambda*)
                                   (append c complex)))
                          ((memq (car gensyms) unref)
                           (lp (cdr gensyms) (cdr vals)
                               s l c))
                          ((memq (car gensyms) set)
                           (lp (cdr gensyms) (cdr vals)
                               s l (cons (car gensyms) c)))
                          ((and (lambda? (car vals))
                                (not (memq (car gensyms) set)))
                           (lp (cdr gensyms) (cdr vals)
                               s (cons (car gensyms) l) c))
                          ;; There is no difference between simple and
                          ;; complex, for the purposes of let. Just lump
                          ;; them all into complex.
                          (else
                           (lp (cdr gensyms) (cdr vals)
                               s l (cons (car gensyms) c))))))
                      (else
                       (values unref ref set simple lambda* complex))))
                  '()
                  '()
                  '()
                  '()
                  '()
                  '())))
    (values unref simple lambda* complex)))

(define (make-sequence* src exps)
  (let lp ((in exps) (out '()))
    (if (null? (cdr in))
        (if (null? out)
            (car in)
            (make-sequence src (reverse (cons (car in) out))))
        (let ((head (car in)))
          (record-case head
            ((<lambda>) (lp (cdr in) out))
            ((<const>) (lp (cdr in) out))
            ((<lexical-ref>) (lp (cdr in) out))
            ((<void>) (lp (cdr in) out))
            (else (lp (cdr in) (cons head out))))))))

(define (fix-letrec! x)
  (let-values (((unref simple lambda* complex) (partition-vars x)))
    (post-order!
     (lambda (x)
       (record-case x

         ;; Sets to unreferenced variables may be replaced by their
         ;; expression, called for effect.
         ((<lexical-set> gensym exp)
          (if (memq gensym unref)
              (make-sequence* #f (list exp (make-void #f)))
              x))

         ((<letrec> src in-order? names gensyms vals body)
          (let ((binds (map list gensyms names vals)))
            ;; The bindings returned by this function need to appear in the same
            ;; order that they appear in the letrec.
            (define (lookup set)
              (let lp ((binds binds))
                (cond
                 ((null? binds) '())
                 ((memq (caar binds) set)
                  (cons (car binds) (lp (cdr binds))))
                 (else (lp (cdr binds))))))
            (let ((u (lookup unref))
                  (s (lookup simple))
                  (l (lookup lambda*))
                  (c (lookup complex)))
              ;; Bind "simple" bindings, and locations for complex
              ;; bindings.
              (make-let
               src
               (append (map cadr s) (map cadr c))
               (append (map car s) (map car c))
               (append (map caddr s) (map (lambda (x) (make-void #f)) c))
               ;; Bind lambdas using the fixpoint operator.
               (make-fix
                src (map cadr l) (map car l) (map caddr l)
                (make-sequence*
                 src
                 (append
                  ;; The right-hand-sides of the unreferenced
                  ;; bindings, for effect.
                  (map caddr u)
                  (cond
                   ((null? c)
                    ;; No complex bindings, just emit the body.
                    (list body))
                   (in-order?
                    ;; For letrec*, assign complex bindings in order, then the
                    ;; body.
                    (append
                     (map (lambda (c)
                            (make-lexical-set #f (cadr c) (car c)
                                              (caddr c)))
                          c)
                     (list body)))
                   (else
                    ;; Otherwise for plain letrec, evaluate the "complex"
                    ;; bindings, in a `let' to indicate that order doesn't
                    ;; matter, and bind to their variables.
                    (list
                     (let ((tmps (map (lambda (x) (gensym)) c)))
                       (make-let
                        #f (map cadr c) tmps (map caddr c)
                        (make-sequence
                         #f
                         (map (lambda (x tmp)
                                (make-lexical-set
                                 #f (cadr x) (car x)
                                 (make-lexical-ref #f (cadr x) tmp)))
                              c tmps))))
                     body))))))))))

         ((<let> src names gensyms vals body)
          (let ((binds (map list gensyms names vals)))
            (define (lookup set)
              (map (lambda (v) (assq v binds))
                   (lset-intersection eq? gensyms set)))
            (let ((u (lookup unref))
                  (l (lookup lambda*))
                  (c (lookup complex)))
              (make-sequence*
               src
               (append
                ;; unreferenced bindings, called for effect.
                (map caddr u)
                (list
                 ;; unassigned lambdas use fix.
                 (make-fix src (map cadr l) (map car l) (map caddr l)
                           ;; and the "complex" bindings.
                           (make-let src (map cadr c) (map car c) (map caddr c)
                                     body))))))))
         
         (else x)))
     x)))

;;; Local Variables:
;;; eval: (put 'record-case 'scheme-indent-function 1)
;;; End:
