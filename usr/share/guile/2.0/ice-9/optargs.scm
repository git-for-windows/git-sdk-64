;;;; optargs.scm -- support for optional arguments
;;;;
;;;; 	Copyright (C) 1997, 1998, 1999, 2001, 2002, 2004, 2006, 2009, 2010, 2011 Free Software Foundation, Inc.
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
;;;; Contributed by Maciej Stachowiak <mstachow@alum.mit.edu>



;;; Commentary:

;;; {Optional Arguments}
;;;
;;; The C interface for creating Guile procedures has a very handy
;;; "optional argument" feature. This module attempts to provide
;;; similar functionality for procedures defined in Scheme with
;;; a convenient and attractive syntax.
;;;
;;; exported macros are:
;;;   let-optional
;;;   let-optional*
;;;   let-keywords
;;;   let-keywords*
;;;   lambda*
;;;   define*
;;;   define*-public
;;;   defmacro*
;;;   defmacro*-public
;;;
;;;
;;; Summary of the lambda* extended parameter list syntax (brackets
;;; are used to indicate grouping only):
;;;
;;; ext-param-list ::= [identifier]* [#:optional [ext-var-decl]+]?
;;;   [#:key [ext-var-decl]+ [#:allow-other-keys]?]?
;;;   [[#:rest identifier]|[. identifier]]?
;;;
;;; ext-var-decl ::= identifier | ( identifier expression )
;;;
;;; The characters `*', `+' and `?' are not to be taken literally; they
;;; mean respectively, zero or more occurences, one or more occurences,
;;; and one or zero occurences.
;;;

;;; Code:

(define-module (ice-9 optargs)
  #:use-module (system base pmatch)
  #:re-export (lambda* define*)
  #:export (let-optional
            let-optional*
            let-keywords
            let-keywords*
            define*-public
            defmacro*
            defmacro*-public))

;; let-optional rest-arg (binding ...) . body
;; let-optional* rest-arg (binding ...) . body
;;   macros used to bind optional arguments
;;
;; These two macros give you an optional argument interface that is
;; very "Schemey" and introduces no fancy syntax. They are compatible
;; with the scsh macros of the same name, but are slightly
;; extended. Each of binding may be of one of the forms <var> or
;; (<var> <default-value>). rest-arg should be the rest-argument of
;; the procedures these are used from. The items in rest-arg are
;; sequentially bound to the variable namess are given. When rest-arg
;; runs out, the remaining vars are bound either to the default values
;; or to `#f' if no default value was specified. rest-arg remains
;; bound to whatever may have been left of rest-arg.
;;

(define (vars&inits bindings)
  (let lp ((bindings bindings) (vars '()) (inits '()))
    (syntax-case bindings ()
      (()
       (values (reverse vars) (reverse inits)))
      (((v init) . rest) (identifier? #'v)
       (lp #'rest (cons #'v vars) (cons #'init inits)))
      ((v . rest) (identifier? #'v)
       (lp #'rest (cons #'v vars) (cons #'#f inits))))))

(define-syntax let-optional
  (lambda (x)
    (syntax-case x ()
      ((_ rest-arg (binding ...) b0 b1 ...) (identifier? #'rest-arg)
       (call-with-values (lambda () (vars&inits #'(binding ...)))
         (lambda (vars inits)
           (with-syntax ((n (length vars))
                         (n+1 (1+ (length vars)))
                         (vars (append vars (list #'rest-arg)))
                         ((t ...) (generate-temporaries vars))
                         ((i ...) inits))
             #'(let ((t (lambda vars i))
                     ...)
                 (apply (lambda vars b0 b1 ...)
                        (or (parse-lambda-case '(0 n n n+1 #f '())
                                               (list t ...)
                                               rest-arg)
                            (error "sth" rest-arg)))))))))))

(define-syntax let-optional*
  (lambda (x)
    (syntax-case x ()
      ((_ rest-arg (binding ...) b0 b1 ...) (identifier? #'rest-arg)
       (call-with-values (lambda () (vars&inits #'(binding ...)))
         (lambda (vars inits)
           (with-syntax ((n (length vars))
                         (n+1 (1+ (length vars)))
                         (vars (append vars (list #'rest-arg)))
                         ((i ...) inits))
             #'(apply (lambda vars b0 b1 ...)
                      (or (parse-lambda-case '(0 n n n+1 #f '())
                                             (list (lambda vars i) ...)
                                             rest-arg)
                          (error "sth" rest-arg))))))))))


;; let-keywords rest-arg allow-other-keys? (binding ...) . body
;; let-keywords* rest-arg allow-other-keys? (binding ...) . body
;;   macros used to bind keyword arguments
;;
;; These macros pick out keyword arguments from rest-arg, but do not
;; modify it. This is consistent at least with Common Lisp, which
;; duplicates keyword args in the rest arg. More explanation of what
;; keyword arguments in a lambda list look like can be found below in
;; the documentation for lambda*.  Bindings can have the same form as
;; for let-optional. If allow-other-keys? is false, an error will be
;; thrown if anything that looks like a keyword argument but does not
;; match a known keyword parameter will result in an error.
;;


(define-syntax let-keywords
  (lambda (x)
    (syntax-case x ()
      ((_ rest-arg aok (binding ...) b0 b1 ...) (identifier? #'rest-arg)
       (call-with-values (lambda () (vars&inits #'(binding ...)))
         (lambda (vars inits)
           (with-syntax ((n (length vars))
                         (vars vars)
                         (ivars (generate-temporaries vars))
                         ((kw ...) (map symbol->keyword
                                        (map syntax->datum vars)))
                         ((idx ...) (iota (length vars)))
                         ((t ...) (generate-temporaries vars))
                         ((i ...) inits))
             #'(let ((t (lambda ivars i))
                     ...)
                 (apply (lambda vars b0 b1 ...)
                        (or (parse-lambda-case '(0 0 #f n aok ((kw . idx) ...))
                                               (list t ...)
                                               rest-arg)
                            (error "sth" rest-arg))))))))
      ((_ rest-arg aok (binding ...) b0 b1 ...)
       #'(let ((r rest-arg))
           (let-keywords r aok (binding ...) b0 b1 ...))))))

(define-syntax let-keywords*
  (lambda (x)
    (syntax-case x ()
      ((_ rest-arg aok (binding ...) b0 b1 ...) (identifier? #'rest-arg)
       (call-with-values (lambda () (vars&inits #'(binding ...)))
         (lambda (vars inits)
           (with-syntax ((n (length vars))
                         (vars vars)
                         ((kw ...) (map symbol->keyword
                                        (map syntax->datum vars)))
                         ((idx ...) (iota (length vars)))
                         ((i ...) inits))
             #'(apply (lambda vars b0 b1 ...)
                      (or (parse-lambda-case '(0 0 #f n aok ((kw . idx) ...))
                                             (list (lambda vars i) ...)
                                             rest-arg)
                          (error "sth" rest-arg)))))))
      ((_ rest-arg aok (binding ...) b0 b1 ...)
       #'(let ((r rest-arg))
           (let-keywords* r aok (binding ...) b0 b1 ...))))))

;; lambda* args . body
;;   lambda extended for optional and keyword arguments
;;
;; lambda* creates a procedure that takes optional arguments. These
;; are specified by putting them inside brackets at the end of the
;; paramater list, but before any dotted rest argument. For example,
;;   (lambda* (a b #:optional c d . e) '())
;; creates a procedure with fixed arguments a and b, optional arguments c
;; and d, and rest argument e. If the optional arguments are omitted
;; in a call, the variables for them are bound to `#f'.
;;
;; lambda* can also take keyword arguments. For example, a procedure
;; defined like this:
;;   (lambda* (#:key xyzzy larch) '())
;; can be called with any of the argument lists (#:xyzzy 11)
;; (#:larch 13) (#:larch 42 #:xyzzy 19) (). Whichever arguments
;; are given as keywords are bound to values.
;;
;; Optional and keyword arguments can also be given default values
;; which they take on when they are not present in a call, by giving a
;; two-item list in place of an optional argument, for example in:
;;   (lambda* (foo #:optional (bar 42) #:key (baz 73)) (list foo bar baz))
;; foo is a fixed argument, bar is an optional argument with default
;; value 42, and baz is a keyword argument with default value 73.
;; Default value expressions are not evaluated unless they are needed
;; and until the procedure is called.
;;
;; lambda* now supports two more special parameter list keywords.
;;
;; lambda*-defined procedures now throw an error by default if a
;; keyword other than one of those specified is found in the actual
;; passed arguments. However, specifying #:allow-other-keys
;; immediately after the keyword argument declarations restores the
;; previous behavior of ignoring unknown keywords. lambda* also now
;; guarantees that if the same keyword is passed more than once, the
;; last one passed is the one that takes effect. For example,
;;   ((lambda* (#:key (heads 0) (tails 0)) (display (list heads tails)))
;;    #:heads 37 #:tails 42 #:heads 99)
;; would result in (99 47) being displayed.
;;
;; #:rest is also now provided as a synonym for the dotted syntax rest
;; argument. The argument lists (a . b) and (a #:rest b) are equivalent in
;; all respects to lambda*. This is provided for more similarity to DSSSL,
;; MIT-Scheme and Kawa among others, as well as for refugees from other
;; Lisp dialects.


;; define* args . body
;; define*-public args . body
;;   define and define-public extended for optional and keyword arguments
;;
;; define* and define*-public support optional arguments with
;; a similar syntax to lambda*. Some examples:
;;   (define* (x y #:optional a (z 3) #:key w . u) (display (list y z u)))
;; defines a procedure x with a fixed argument y, an optional agument
;; a, another optional argument z with default value 3, a keyword argument w,
;; and a rest argument u.
;;
;; Of course, define*[-public] also supports #:rest and #:allow-other-keys
;; in the same way as lambda*.

(define-syntax define*-public
  (lambda (x)
    (syntax-case x ()
      ((_ (id . args) b0 b1 ...)
       #'(define-public id (lambda* args b0 b1 ...)))
      ((_ id val) (identifier? #'id)
       #'(define-public id val)))))


;; defmacro* name args . body
;; defmacro*-public args . body
;;   defmacro and defmacro-public extended for optional and keyword arguments
;;
;; These are just like defmacro and defmacro-public except that they
;; take lambda*-style extended paramter lists, where #:optional,
;; #:key, #:allow-other-keys and #:rest are allowed with the usual
;; semantics. Here is an example of a macro with an optional argument:
;;   (defmacro* transmogrify (a #:optional b)

(define-syntax defmacro*
  (lambda (x)
    (syntax-case x ()
      ((_ id args doc b0 b1 ...) (string? (syntax->datum #'doc))
       #'(define-macro id doc (lambda* args b0 b1 ...)))
      ((_ id args b0 b1 ...) 
       #'(define-macro id #f (lambda* args b0 b1 ...))))))
(define-syntax-rule (defmacro*-public id args b0 b1 ...)
  (begin
    (defmacro* id args b0 b1 ...)
    (export-syntax id)))

;;; Support for optional & keyword args with the interpreter.
(define *uninitialized* (list 'uninitialized))
(define (parse-lambda-case spec inits args)
  (pmatch spec
    ((,nreq ,nopt ,rest-idx ,nargs ,allow-other-keys? ,kw-indices)
     (define (req args prev tail n)
       (cond
        ((zero? n)
         (if prev (set-cdr! prev '()))
         (let ((slots-tail (make-list (- nargs nreq) *uninitialized*)))
           (opt (if prev (append! args slots-tail) slots-tail)
                slots-tail tail nopt inits)))
        ((null? tail)
         #f) ;; fail
        (else
         (req args tail (cdr tail) (1- n)))))
     (define (opt slots slots-tail args-tail n inits)
       (cond
        ((zero? n)
         (rest-or-key slots slots-tail args-tail inits rest-idx))
        ((null? args-tail)
         (set-car! slots-tail (apply (car inits) slots))
         (opt slots (cdr slots-tail) '() (1- n) (cdr inits)))
        (else
         (set-car! slots-tail (car args-tail))
         (opt slots (cdr slots-tail) (cdr args-tail) (1- n) (cdr inits)))))
     (define (rest-or-key slots slots-tail args-tail inits rest-idx)
       (cond
        (rest-idx
         ;; it has to be this way, vars are allocated in this order
         (set-car! slots-tail args-tail)
         (if (pair? kw-indices)
             (permissive-keys slots (cdr slots-tail) args-tail inits)
             (rest-or-key slots (cdr slots-tail) '() inits #f)))
        ((pair? kw-indices)
         ;; fail early here, because once we're in keyword land we throw
         ;; errors instead of failing
         (and (or (null? args-tail) rest-idx (keyword? (car args-tail)))
              (key slots slots-tail args-tail inits)))
        ((pair? args-tail)
         #f) ;; fail
        (else
         slots)))
     (define (permissive-keys slots slots-tail args-tail inits)
       (cond
        ((null? args-tail)
         (if (null? inits)
             slots
             (begin
               (if (eq? (car slots-tail) *uninitialized*)
                   (set-car! slots-tail (apply (car inits) slots)))
               (permissive-keys slots (cdr slots-tail) '() (cdr inits)))))
        ((not (keyword? (car args-tail)))
         (permissive-keys slots slots-tail (cdr args-tail) inits))
        ((and (keyword? (car args-tail))
              (pair? (cdr args-tail))
              (assq-ref kw-indices (car args-tail)))
         => (lambda (i)
              (list-set! slots i (cadr args-tail))
              (permissive-keys slots slots-tail (cddr args-tail) inits)))
        ((and (keyword? (car args-tail))
              (pair? (cdr args-tail))
              allow-other-keys?)
         (permissive-keys slots slots-tail (cddr args-tail) inits))
        (else (scm-error 'keyword-argument-error #f "Unrecognized keyword"
                         '() args-tail))))
     (define (key slots slots-tail args-tail inits)
       (cond
        ((null? args-tail)
         (if (null? inits)
             slots
             (begin
               (if (eq? (car slots-tail) *uninitialized*)
                   (set-car! slots-tail (apply (car inits) slots)))
               (key slots (cdr slots-tail) '() (cdr inits)))))
        ((not (keyword? (car args-tail)))
         (if rest-idx
             ;; no error checking, everything goes to the rest..
             (key slots slots-tail '() inits)
             (scm-error 'keyword-argument-error #f "Invalid keyword"
                        '() args-tail)))
        ((and (keyword? (car args-tail))
              (pair? (cdr args-tail))
              (assq-ref kw-indices (car args-tail)))
         => (lambda (i)
              (list-set! slots i (cadr args-tail))
              (key slots slots-tail (cddr args-tail) inits)))
        ((and (keyword? (car args-tail))
              (pair? (cdr args-tail))
              allow-other-keys?)
         (key slots slots-tail (cddr args-tail) inits))
        (else (scm-error 'keyword-argument-error #f "Unrecognized keyword"
                         '() args-tail))))
     (let ((args (list-copy args)))
       (req args #f args nreq)))
    (else (error "unexpected spec" spec))))
