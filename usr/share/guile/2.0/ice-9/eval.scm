;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 2009, 2010, 2012, 2013 Free Software Foundation, Inc.
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

;;; Scheme eval, written in Scheme.
;;;
;;; Expressions are first expanded, by the syntax expander (i.e.
;;; psyntax), then memoized into internal forms. The evaluator itself
;;; only operates on the internal forms ("memoized expressions").
;;;
;;; Environments are represented as linked lists of the form (VAL ... .
;;; MOD). If MOD is #f, it means the environment was captured before
;;; modules were booted. If MOD is the literal value '(), we are
;;; evaluating at the top level, and so should track changes to the
;;; current module.
;;;
;;; Evaluate this in Emacs to make code indentation work right:
;;;
;;;    (put 'memoized-expression-case 'scheme-indent-function 1)
;;;

;;; Code:



(eval-when (compile)
  (define-syntax capture-env
    (syntax-rules ()
      ((_ (exp ...))
       (let ((env (exp ...)))
         (capture-env env)))
      ((_ env)
       (if (null? env)
           (current-module)
           (if (not env)
               ;; the and current-module checks that modules are booted,
               ;; and thus the-root-module is defined
               (and (current-module) the-root-module)
               env)))))

  ;; Fast case for procedures with fixed arities.
  (define-syntax make-fixed-closure
    (lambda (x)
      (define *max-static-argument-count* 8)
      (define (make-formals n)
        (map (lambda (i)
               (datum->syntax
                x
                (string->symbol
                 (string (integer->char (+ (char->integer #\a) i))))))
             (iota n)))
      (syntax-case x ()
        ((_ eval nreq body env) (not (identifier? #'env))
         #'(let ((e env))
             (make-fixed-closure eval nreq body e)))
        ((_ eval nreq body env)
         #`(case nreq
             #,@(map (lambda (nreq)
                       (let ((formals (make-formals nreq)))
                         #`((#,nreq)
                            (lambda (#,@formals)
                              (eval body
                                    (cons* #,@(reverse formals) env))))))
                     (iota *max-static-argument-count*))
             (else
              #,(let ((formals (make-formals *max-static-argument-count*)))
                  #`(lambda (#,@formals . more)
                      (let lp ((new-env (cons* #,@(reverse formals) env))
                               (nreq (- nreq #,*max-static-argument-count*))
                               (args more))
                        (if (zero? nreq)
                            (eval body
                                  (if (null? args)
                                      new-env
                                      (scm-error 'wrong-number-of-args
                                                 "eval" "Wrong number of arguments"
                                                 '() #f)))
                            (if (null? args)
                                (scm-error 'wrong-number-of-args
                                           "eval" "Wrong number of arguments"
                                           '() #f)
                                (lp (cons (car args) new-env)
                                    (1- nreq)
                                    (cdr args)))))))))))))

  (define-syntax call
    (lambda (x)
      (define *max-static-call-count* 4)
      (syntax-case x ()
        ((_ eval proc nargs args env) (identifier? #'env)
         #`(case nargs
             #,@(map (lambda (nargs)
                       #`((#,nargs)
                          (proc
                           #,@(map
                               (lambda (n)
                                 (let lp ((n n) (args #'args))
                                   (if (zero? n)
                                       #`(eval (car #,args) env)
                                       (lp (1- n) #`(cdr #,args)))))
                               (iota nargs)))))
                     (iota *max-static-call-count*))
             (else
              (apply proc
                     #,@(map
                         (lambda (n)
                           (let lp ((n n) (args #'args))
                             (if (zero? n)
                                 #`(eval (car #,args) env)
                                 (lp (1- n) #`(cdr #,args)))))
                         (iota *max-static-call-count*))
                     (let lp ((exps #,(let lp ((n *max-static-call-count*)
                                               (args #'args))
                                        (if (zero? n)
                                            args
                                            (lp (1- n) #`(cdr #,args)))))
                              (args '()))
                       (if (null? exps)
                           (reverse args)
                           (lp (cdr exps)
                               (cons (eval (car exps) env) args)))))))))))

  ;; This macro could be more straightforward if the compiler had better
  ;; copy propagation. As it is we do some copy propagation by hand.
  (define-syntax mx-bind
    (lambda (x)
      (syntax-case x ()
        ((_ data () body)
         #'body)
        ((_ data (a . b) body) (and (identifier? #'a) (identifier? #'b))
         #'(let ((a (car data))
                 (b (cdr data)))
             body))
        ((_ data (a . b) body) (identifier? #'a)
         #'(let ((a (car data))
                 (xb (cdr data)))
             (mx-bind xb b body)))
        ((_ data (a . b) body) 
         #'(let ((xa (car data))
                 (xb (cdr data)))
             (mx-bind xa a (mx-bind xb b body))))
        ((_ data v body) (identifier? #'v)
         #'(let ((v data))
             body)))))
  
  ;; The resulting nested if statements will be an O(n) dispatch. Once
  ;; we compile `case' effectively, this situation will improve.
  (define-syntax mx-match
    (lambda (x)
      (syntax-case x (quote)
        ((_ mx data tag)
         #'(error "what" mx))
        ((_ mx data tag (('type pat) body) c* ...)
         #`(if (eqv? tag #,(or (memoized-typecode (syntax->datum #'type))
                               (error "not a typecode" #'type)))
               (mx-bind data pat body)
               (mx-match mx data tag c* ...))))))

  (define-syntax memoized-expression-case
    (lambda (x)
      (syntax-case x ()
        ((_ mx c ...)
         #'(let ((tag (memoized-expression-typecode mx))
                 (data (memoized-expression-data mx)))
             (mx-match mx data tag c ...)))))))


;;;
;;; On 18 Feb 2010, I did a profile of how often the various memoized expression
;;; types occur when getting to a prompt on a fresh build. Here are the numbers
;;; I got:
;;;
;;;      lexical-ref: 32933054
;;;             call: 20281547
;;;     toplevel-ref: 13228724
;;;               if: 9156156
;;;            quote: 6610137
;;;              let: 2619707
;;;           lambda: 1010921
;;;            begin: 948945
;;;      lexical-set: 509862
;;; call-with-values: 139668
;;;            apply: 49402
;;;       module-ref: 14468
;;;           define: 1259
;;;     toplevel-set: 328
;;;          dynwind: 162
;;;      with-fluids: 0
;;;          call/cc: 0
;;;       module-set: 0
;;;
;;; So until we compile `case' into a computed goto, we'll order the clauses in
;;; `eval' in this order, to put the most frequent cases first.
;;;

(define primitive-eval
  (let ()
    ;; We pre-generate procedures with fixed arities, up to some number of
    ;; arguments; see make-fixed-closure above.

    ;; A unique marker for unbound keywords.
    (define unbound-arg (list 'unbound-arg))

    ;; Procedures with rest, optional, or keyword arguments, potentially with
    ;; multiple arities, as with case-lambda.
    (define (make-general-closure env body nreq rest? nopt kw inits alt)
      (define alt-proc
        (and alt                             ; (body docstring nreq ...)
             (let* ((body (car alt))
                    (spec (cddr alt))
                    (nreq (car spec))
                    (rest (if (null? (cdr spec)) #f (cadr spec)))
                    (tail (and (pair? (cdr spec)) (pair? (cddr spec)) (cddr spec)))
                    (nopt (if tail (car tail) 0))
                    (kw (and tail (cadr tail)))
                    (inits (if tail (caddr tail) '()))
                    (alt (and tail (cadddr tail))))
               (make-general-closure env body nreq rest nopt kw inits alt))))
      (define (set-procedure-arity! proc)
        (let lp ((alt alt) (nreq nreq) (nopt nopt) (rest? rest?))
          (if (not alt)
              (begin
                (set-procedure-property! proc 'arglist
                                         (list nreq
                                               nopt
                                               (if kw (cdr kw) '())
                                               (and kw (car kw))
                                               (and rest? '_)))
                (set-procedure-minimum-arity! proc nreq nopt rest?))
              (let* ((spec (cddr alt))
                     (nreq* (car spec))
                     (rest?* (if (null? (cdr spec)) #f (cadr spec)))
                     (tail (and (pair? (cdr spec)) (pair? (cddr spec)) (cddr spec)))
                     (nopt* (if tail (car tail) 0))
                     (alt* (and tail (cadddr tail))))
                (if (or (< nreq* nreq)
                        (and (= nreq* nreq)
                             (if rest?
                                 (and rest?* (> nopt* nopt))
                                 (or rest?* (> nopt* nopt)))))
                    (lp alt* nreq* nopt* rest?*)
                    (lp alt* nreq nopt rest?)))))
        proc)
      (set-procedure-arity!
       (lambda %args
         (let lp ((env env)
                  (nreq* nreq)
                  (args %args))
           (if (> nreq* 0)
               ;; First, bind required arguments.
               (if (null? args)
                   (if alt
                       (apply alt-proc %args)
                       (scm-error 'wrong-number-of-args
                                  "eval" "Wrong number of arguments"
                                  '() #f))
                   (lp (cons (car args) env)
                       (1- nreq*)
                       (cdr args)))
               ;; Move on to optional arguments.
               (if (not kw)
                   ;; Without keywords, bind optionals from arguments.
                   (let lp ((env env)
                            (nopt nopt)
                            (args args)
                            (inits inits))
                     (if (zero? nopt)
                         (if rest?
                             (eval body (cons args env))
                             (if (null? args)
                                 (eval body env)
                                 (if alt
                                     (apply alt-proc %args)
                                     (scm-error 'wrong-number-of-args
                                                "eval" "Wrong number of arguments"
                                                '() #f))))
                         (if (null? args)
                             (lp (cons (eval (car inits) env) env)
                                 (1- nopt) args (cdr inits))
                             (lp (cons (car args) env)
                                 (1- nopt) (cdr args) (cdr inits)))))
                   (let lp ((env env)
                            (nopt* nopt)
                            (args args)
                            (inits inits))
                     (cond
                      ;; With keywords, we stop binding optionals at the
                      ;; first keyword.
                      ((> nopt* 0)
                       (if (or (null? args) (keyword? (car args)))
                           (lp (cons (eval (car inits) env) env)
                               (1- nopt*) args (cdr inits))
                           (lp (cons (car args) env)
                               (1- nopt*) (cdr args) (cdr inits))))
                      ;; Finished with optionals.
                      ((and alt (pair? args) (not (keyword? (car args)))
                            (not rest?))
                       ;; Too many positional args, no #:rest arg,
                       ;; and we have an alternate.
                       (apply alt-proc %args))
                      (else
                       (let* ((aok (car kw))
                              (kw (cdr kw))
                              (kw-base (+ nopt nreq (if rest? 1 0)))
                              (imax (let lp ((imax (1- kw-base)) (kw kw))
                                      (if (null? kw)
                                          imax
                                          (lp (max (cdar kw) imax)
                                              (cdr kw)))))
                              ;; Fill in kwargs  with "undefined" vals.
                              (env (let lp ((i kw-base)
                                            ;; Also, here we bind the rest
                                            ;; arg, if any.
                                            (env (if rest?
                                                     (cons args env)
                                                     env)))
                                     (if (<= i imax)
                                         (lp (1+ i) (cons unbound-arg env))
                                         env))))
                         ;; Now scan args for keywords.
                         (let lp ((args args))
                           (if (and (pair? args) (pair? (cdr args))
                                    (keyword? (car args)))
                               (let ((kw-pair (assq (car args) kw))
                                     (v (cadr args)))
                                 (if kw-pair
                                     ;; Found a known keyword; set its value.
                                     (list-set! env
                                                (- imax (cdr kw-pair)) v)
                                     ;; Unknown keyword.
                                     (if (not aok)
                                         (scm-error
                                          'keyword-argument-error
                                          "eval" "Unrecognized keyword"
                                          '() (list (car args)))))
                                 (lp (cddr args)))
                               (if (pair? args)
                                   (if rest?
                                       ;; Be lenient parsing rest args.
                                       (lp (cdr args))
                                       (scm-error 'keyword-argument-error
                                                  "eval" "Invalid keyword"
                                                  '() (list (car args))))
                                   ;; Finished parsing keywords. Fill in
                                   ;; uninitialized kwargs by evalling init
                                   ;; expressions in their appropriate
                                   ;; environment.
                                   (let lp ((i (- imax kw-base))
                                            (inits inits))
                                     (if (pair? inits)
                                         (let ((tail (list-tail env i)))
                                           (if (eq? (car tail) unbound-arg)
                                               (set-car! tail
                                                         (eval (car inits)
                                                               (cdr tail))))
                                           (lp (1- i) (cdr inits)))
                                         ;; Finally, eval the body.
                                         (eval body env))))))))))))))))

    ;; The "engine". EXP is a memoized expression.
    (define (eval exp env)
      (memoized-expression-case exp
        (('lexical-ref n)
         (list-ref env n))
        
        (('call (f nargs . args))
         (let ((proc (eval f env)))
           (call eval proc nargs args env)))
        
        (('toplevel-ref var-or-sym)
         (variable-ref
          (if (variable? var-or-sym)
              var-or-sym
              (memoize-variable-access! exp
                                        (capture-env (if (pair? env)
                                                         (cdr (last-pair env))
                                                         env))))))

        (('if (test consequent . alternate))
         (if (eval test env)
             (eval consequent env)
             (eval alternate env)))
      
        (('quote x)
         x)

        (('let (inits . body))
         (let lp ((inits inits) (new-env (capture-env env)))
           (if (null? inits)
               (eval body new-env)
               (lp (cdr inits)
                   (cons (eval (car inits) env) new-env)))))

        (('lambda (body docstring nreq . tail))
         (let ((proc
                (if (null? tail)
                    (make-fixed-closure eval nreq body (capture-env env))
                    (if (null? (cdr tail))
                        (make-general-closure (capture-env env) body
                                              nreq (car tail)
                                              0 #f '() #f)
                        (apply make-general-closure (capture-env env)
                               body nreq tail)))))
           (when docstring
             (set-procedure-property! proc 'documentation docstring))
           proc))

        (('begin (first . rest))
         (let lp ((first first) (rest rest))
           (if (null? rest)
               (eval first env)
               (begin
                 (eval first env)
                 (lp (car rest) (cdr rest))))))
      
        (('lexical-set! (n . x))
         (let ((val (eval x env)))
           (list-set! env n val)))
        
        (('call-with-values (producer . consumer))
         (call-with-values (eval producer env)
           (eval consumer env)))

        (('apply (f args))
         (apply (eval f env) (eval args env)))

        (('module-ref var-or-spec)
         (variable-ref
          (if (variable? var-or-spec)
              var-or-spec
              (memoize-variable-access! exp #f))))

        (('define (name . x))
         (let ((x (eval x env)))
           (if (and (procedure? x) (not (procedure-property x 'name)))
               (set-procedure-property! x 'name name))
           (define! name x)
           (if #f #f)))
      
        (('toplevel-set! (var-or-sym . x))
         (variable-set!
          (if (variable? var-or-sym)
              var-or-sym
              (memoize-variable-access! exp
                                        (capture-env (if (pair? env)
                                                         (cdr (last-pair env))
                                                         env))))
          (eval x env)))
      
        (('dynwind (in exp . out))
         (dynamic-wind (eval in env)
                       (lambda () (eval exp env))
                       (eval out env)))
        
        (('with-fluids (fluids vals . exp))
         (let* ((fluids (map (lambda (x) (eval x env)) fluids))
                (vals (map (lambda (x) (eval x env)) vals)))
           (let lp ((fluids fluids) (vals vals))
             (if (null? fluids)
                 (eval exp env)
                 (with-fluids (((car fluids) (car vals)))
                   (lp (cdr fluids) (cdr vals)))))))
        
        (('prompt (tag exp . handler))
         (@prompt (eval tag env)
                  (eval exp env)
                  (eval handler env)))
        
        (('call/cc proc)
         (call/cc (eval proc env)))

        (('module-set! (x . var-or-spec))
         (variable-set!
          (if (variable? var-or-spec)
              var-or-spec
              (memoize-variable-access! exp #f))
          (eval x env)))))
  
    ;; primitive-eval
    (lambda (exp)
      "Evaluate @var{exp} in the current module."
      (eval 
       (memoize-expression 
        (if (macroexpanded? exp)
            exp
            ((module-transformer (current-module)) exp)))
       '()))))
