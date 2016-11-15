;;; Guile Emacs Lisp

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (language elisp compile-tree-il)
  #:use-module (language elisp bindings)
  #:use-module (language elisp runtime)
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  #:use-module (system base compile)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (compile-tree-il
            compile-progn
            compile-if
            compile-defconst
            compile-defvar
            compile-setq
            compile-let
            compile-lexical-let
            compile-flet
            compile-let*
            compile-lexical-let*
            compile-flet*
            compile-without-void-checks
            compile-with-always-lexical
            compile-guile-ref
            compile-guile-primitive
            compile-while
            compile-function
            compile-defmacro
            compile-defun
            #{compile-`}#
            compile-quote))

;;; Certain common parameters (like the bindings data structure or
;;; compiler options) are not always passed around but accessed using
;;; fluids to simulate dynamic binding (hey, this is about elisp).

;;; The bindings data structure to keep track of symbol binding related
;;; data.

(define bindings-data (make-fluid))

;;; Store for which symbols (or all/none) void checks are disabled.

(define disable-void-check (make-fluid))

;;; Store which symbols (or all/none) should always be bound lexically,
;;; even with ordinary let and as lambda arguments.

(define always-lexical (make-fluid))

;;; Find the source properties of some parsed expression if there are
;;; any associated with it.

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
         (and (not (null? props))
              props))))

;;; Values to use for Elisp's nil and t.

(define (nil-value loc)
  (make-const loc (@ (language elisp runtime) nil-value)))

(define (t-value loc)
  (make-const loc (@ (language elisp runtime) t-value)))

;;; Modules that contain the value and function slot bindings.

(define runtime '(language elisp runtime))

(define value-slot (@ (language elisp runtime) value-slot-module))

(define function-slot (@ (language elisp runtime) function-slot-module))

;;; The backquoting works the same as quasiquotes in Scheme, but the
;;; forms are named differently; to make easy adaptions, we define these
;;; predicates checking for a symbol being the car of an
;;; unquote/unquote-splicing/backquote form.

(define (unquote? sym)
  (and (symbol? sym) (eq? sym '#{,}#)))

(define (unquote-splicing? sym)
  (and (symbol? sym) (eq? sym '#{,@}#)))

;;; Build a call to a primitive procedure nicely.

(define (call-primitive loc sym . args)
  (make-application loc (make-primitive-ref loc sym) args))

;;; Error reporting routine for syntax/compilation problems or build
;;; code for a runtime-error output.

(define (report-error loc . args)
  (apply error args))

(define (runtime-error loc msg . args)
  (make-application loc
                    (make-primitive-ref loc 'error)
                    (cons (make-const loc msg) args)))

;;; Generate code to ensure a global symbol is there for further use of
;;; a given symbol.  In general during the compilation, those needed are
;;; only tracked with the bindings data structure.  Afterwards, however,
;;; for all those needed symbols the globals are really generated with
;;; this routine.

(define (generate-ensure-global loc sym module)
  (make-application loc
                    (make-module-ref loc runtime 'ensure-fluid! #t)
                    (list (make-const loc module)
                          (make-const loc sym))))

(define (ensuring-globals loc bindings body)
  (make-sequence
   loc
   `(,@(map-globals-needed (fluid-ref bindings)
                           (lambda (mod sym)
                             (generate-ensure-global loc sym mod)))
     ,body)))

;;; Build a construct that establishes dynamic bindings for certain
;;; variables.  We may want to choose between binding with fluids and
;;; with-fluids* and using just ordinary module symbols and
;;; setting/reverting their values with a dynamic-wind.

(define (let-dynamic loc syms module vals body)
  (call-primitive
   loc
   'with-fluids*
   (make-application loc
                     (make-primitive-ref loc 'list)
                     (map (lambda (sym)
                            (make-module-ref loc module sym #t))
                          syms))
   (make-application loc (make-primitive-ref loc 'list) vals)
   (make-lambda loc
                '()
                (make-lambda-case #f '() #f #f #f '() '() body #f))))

;;; Handle access to a variable (reference/setting) correctly depending
;;; on whether it is currently lexically or dynamically bound.  lexical
;;; access is done only for references to the value-slot module!

(define (access-variable loc
                         sym
                         module
                         handle-global
                         handle-lexical
                         handle-dynamic)
  (let ((lexical (get-lexical-binding (fluid-ref bindings-data) sym)))
    (cond
     (lexical (handle-lexical lexical))
     ((equal? module function-slot) (handle-global))
     (else (handle-dynamic)))))

;;; Generate code to reference a variable.  For references in the
;;; value-slot module, we may want to generate a lexical reference
;;; instead if the variable has a lexical binding.

(define (reference-variable loc sym module)
  (access-variable
   loc
   sym
   module
   (lambda () (make-module-ref loc module sym #t))
   (lambda (lexical) (make-lexical-ref loc lexical lexical))
   (lambda ()
     (mark-global-needed! (fluid-ref bindings-data) sym module)
     (call-primitive loc
                     'fluid-ref
                     (make-module-ref loc module sym #t)))))

;;; Generate code to set a variable.  Just as with reference-variable, in
;;; case of a reference to value-slot, we want to generate a lexical set
;;; when the variable has a lexical binding.

(define (set-variable! loc sym module value)
  (access-variable
   loc
   sym
   module
   (lambda ()
     (make-application
      loc
      (make-module-ref loc runtime 'set-variable! #t)
      (list (make-const loc module) (make-const loc sym) value)))
   (lambda (lexical) (make-lexical-set loc lexical lexical value))
   (lambda ()
     (mark-global-needed! (fluid-ref bindings-data) sym module)
     (call-primitive loc
                     'fluid-set!
                     (make-module-ref loc module sym #t)
                     value))))

;;; Process the bindings part of a let or let* expression; that is,
;;; check for correctness and bring it to the form ((sym1 . val1) (sym2
;;; . val2) ...).

(define (process-let-bindings loc bindings)
  (map
   (lambda (b)
     (if (symbol? b)
         (cons b 'nil)
         (if (or (not (list? b))
                 (not (= (length b) 2)))
             (report-error
              loc
              "expected symbol or list of 2 elements in let")
             (if (not (symbol? (car b)))
                 (report-error loc "expected symbol in let")
                 (cons (car b) (cadr b))))))
   bindings))

;;; Split the let bindings into a list to be done lexically and one
;;; dynamically.  A symbol will be bound lexically if and only if: We're
;;; processing a lexical-let (i.e. module is 'lexical), OR we're
;;; processing a value-slot binding AND the symbol is already lexically
;;; bound or is always lexical, OR we're processing a function-slot
;;; binding.

(define (bind-lexically? sym module)
  (or (eq? module 'lexical)
      (eq? module function-slot)
      (and (equal? module value-slot)
           (let ((always (fluid-ref always-lexical)))
             (or (eq? always 'all)
                 (memq sym always)
                 (get-lexical-binding (fluid-ref bindings-data) sym))))))

(define (split-let-bindings bindings module)
  (let iterate ((tail bindings)
                (lexical '())
                (dynamic '()))
    (if (null? tail)
        (values (reverse lexical) (reverse dynamic))
        (if (bind-lexically? (caar tail) module)
            (iterate (cdr tail) (cons (car tail) lexical) dynamic)
            (iterate (cdr tail) lexical (cons (car tail) dynamic))))))

;;; Compile let and let* expressions.  The code here is used both for
;;; let/let* and flet/flet*, just with a different bindings module.
;;;
;;; A special module value 'lexical means that we're doing a lexical-let
;;; instead and the bindings should not be saved to globals at all but
;;; be done with the lexical framework instead.

;;; Let is done with a single call to let-dynamic binding them locally
;;; to new values all "at once".  If there is at least one variable to
;;; bind lexically among the bindings, we first do a let for all of them
;;; to evaluate all values before any bindings take place, and then call
;;; let-dynamic for the variables to bind dynamically.

(define (generate-let loc module bindings body)
  (let ((bind (process-let-bindings loc bindings)))
    (call-with-values
        (lambda () (split-let-bindings bind module))
      (lambda (lexical dynamic)
        (for-each (lambda (sym)
                    (mark-global-needed! (fluid-ref bindings-data)
                                         sym
                                         module))
                  (map car dynamic))
        (let ((make-values (lambda (for)
                             (map (lambda (el) (compile-expr (cdr el)))
                                  for)))
              (make-body (lambda ()
                           (make-sequence loc (map compile-expr body)))))
          (if (null? lexical)
              (let-dynamic loc (map car dynamic) module
                           (make-values dynamic) (make-body))
              (let* ((lexical-syms (map (lambda (el) (gensym)) lexical))
                     (dynamic-syms (map (lambda (el) (gensym)) dynamic))
                     (all-syms (append lexical-syms dynamic-syms))
                     (vals (append (make-values lexical)
                                   (make-values dynamic))))
                (make-let loc
                          all-syms
                          all-syms
                          vals
                          (with-lexical-bindings
                           (fluid-ref bindings-data)
                           (map car lexical) lexical-syms
                           (lambda ()
                             (if (null? dynamic)
                                 (make-body)
                                 (let-dynamic loc
                                              (map car dynamic)
                                              module
                                              (map
                                               (lambda (sym)
                                                 (make-lexical-ref loc
                                                                   sym
                                                                   sym))
                                               dynamic-syms)
                                              (make-body)))))))))))))

;;; Let* is compiled to a cascaded set of "small lets" for each binding
;;; in turn so that each one already sees the preceding bindings.

(define (generate-let* loc module bindings body)
  (let ((bind (process-let-bindings loc bindings)))
    (begin
      (for-each (lambda (sym)
                  (if (not (bind-lexically? sym module))
                      (mark-global-needed! (fluid-ref bindings-data)
                                           sym
                                           module)))
                (map car bind))
      (let iterate ((tail bind))
        (if (null? tail)
            (make-sequence loc (map compile-expr body))
            (let ((sym (caar tail))
                  (value (compile-expr (cdar tail))))
              (if (bind-lexically? sym module)
                  (let ((target (gensym)))
                    (make-let loc
                              `(,target)
                              `(,target)
                              `(,value)
                              (with-lexical-bindings
                               (fluid-ref bindings-data)
                               `(,sym)
                               `(,target)
                               (lambda () (iterate (cdr tail))))))
                  (let-dynamic loc
                               `(,(caar tail))
                               module
                               `(,value)
                               (iterate (cdr tail))))))))))

;;; Split the argument list of a lambda expression into required,
;;; optional and rest arguments and also check it is actually valid.
;;; Additionally, we create a list of all "local variables" (that is,
;;; required, optional and rest arguments together) and also this one
;;; split into those to be bound lexically and dynamically.  Returned is
;;; as multiple values: required optional rest lexical dynamic

(define (bind-arg-lexical? arg)
  (let ((always (fluid-ref always-lexical)))
    (or (eq? always 'all)
        (memq arg always))))

(define (split-lambda-arguments loc args)
  (let iterate ((tail args)
                (mode 'required)
                (required '())
                (optional '())
                (lexical '())
                (dynamic '()))
    (cond
     ((null? tail)
      (let ((final-required (reverse required))
            (final-optional (reverse optional))
            (final-lexical (reverse lexical))
            (final-dynamic (reverse dynamic)))
        (values final-required
                final-optional
                #f
                final-lexical
                final-dynamic)))
     ((and (eq? mode 'required)
           (eq? (car tail) '&optional))
      (iterate (cdr tail) 'optional required optional lexical dynamic))
     ((eq? (car tail) '&rest)
      (if (or (null? (cdr tail))
              (not (null? (cddr tail))))
          (report-error loc "expected exactly one symbol after &rest")
          (let* ((rest (cadr tail))
                 (rest-lexical (bind-arg-lexical? rest))
                 (final-required (reverse required))
                 (final-optional (reverse optional))
                 (final-lexical (reverse (if rest-lexical
                                             (cons rest lexical)
                                             lexical)))
                 (final-dynamic (reverse (if rest-lexical
                                             dynamic
                                             (cons rest dynamic)))))
            (values final-required
                    final-optional
                    rest
                    final-lexical
                    final-dynamic))))
     (else
      (if (not (symbol? (car tail)))
          (report-error loc
                        "expected symbol in argument list, got"
                        (car tail))
          (let* ((arg (car tail))
                 (bind-lexical (bind-arg-lexical? arg))
                 (new-lexical (if bind-lexical
                                  (cons arg lexical)
                                  lexical))
                 (new-dynamic (if bind-lexical
                                  dynamic
                                  (cons arg dynamic))))
            (case mode
              ((required) (iterate (cdr tail) mode
                                   (cons arg required) optional
                                   new-lexical new-dynamic))
              ((optional) (iterate (cdr tail) mode
                                   required (cons arg optional)
                                   new-lexical new-dynamic))
              (else
               (error "invalid mode in split-lambda-arguments"
                      mode)))))))))

;;; Compile a lambda expression.  One thing we have to be aware of is
;;; that lambda arguments are usually dynamically bound, even when a
;;; lexical binding is intact for a symbol.  For symbols that are marked
;;; as 'always lexical,' however, we lexically bind here as well, and
;;; thus we get them out of the let-dynamic call and register a lexical
;;; binding for them (the lexical target variable is already there,
;;; namely the real lambda argument from TreeIL).

(define (compile-lambda loc args body)
  (if (not (list? args))
      (report-error loc "expected list for argument-list" args))
  (if (null? body)
      (report-error loc "function body must not be empty"))
  (receive (required optional rest lexical dynamic)
           (split-lambda-arguments loc args)
    (define (process-args args)
      (define (find-pairs pairs filter)
        (lset-intersection (lambda (name+sym x)
                             (eq? (car name+sym) x))
                           pairs
                           filter))
      (let* ((syms (map (lambda (x) (gensym)) args))
             (pairs (map cons args syms))
             (lexical-pairs (find-pairs pairs lexical))
             (dynamic-pairs (find-pairs pairs dynamic)))
        (values syms pairs lexical-pairs dynamic-pairs)))
    (let*-values (((required-syms
                    required-pairs
                    required-lex-pairs
                    required-dyn-pairs)
                   (process-args required))
                  ((optional-syms
                    optional-pairs
                    optional-lex-pairs
                    optional-dyn-pairs)
                   (process-args optional))
                  ((rest-syms rest-pairs rest-lex-pairs rest-dyn-pairs)
                   (process-args (if rest (list rest) '())))
                  ((the-rest-sym) (if rest (car rest-syms) #f))
                  ((all-syms) (append required-syms
                                      optional-syms
                                      rest-syms))
                  ((all-lex-pairs) (append required-lex-pairs
                                           optional-lex-pairs
                                           rest-lex-pairs))
                  ((all-dyn-pairs) (append required-dyn-pairs
                                           optional-dyn-pairs
                                           rest-dyn-pairs)))
      (for-each (lambda (sym)
                  (mark-global-needed! (fluid-ref bindings-data)
                                       sym
                                       value-slot))
                dynamic)
      (with-dynamic-bindings
       (fluid-ref bindings-data)
       dynamic
       (lambda ()
         (with-lexical-bindings
          (fluid-ref bindings-data)
          (map car all-lex-pairs)
          (map cdr all-lex-pairs)
          (lambda ()
            (make-lambda
             loc
             '()
             (make-lambda-case
              #f
              required
              optional
              rest
              #f
              (map (lambda (x) (nil-value loc)) optional)
              all-syms
              (let ((compiled-body
                     (make-sequence loc (map compile-expr body))))
                (make-sequence
                 loc
                 (list
                  (if rest
                      (make-conditional
                       loc
                       (call-primitive loc
                                       'null?
                                       (make-lexical-ref loc
                                                         rest
                                                         the-rest-sym))
                       (make-lexical-set loc
                                         rest
                                         the-rest-sym
                                         (nil-value loc))
                       (make-void loc))
                      (make-void loc))
                  (if (null? dynamic)
                      compiled-body
                      (let-dynamic loc
                                   dynamic
                                   value-slot
                                   (map (lambda (name-sym)
                                          (make-lexical-ref
                                           loc
                                           (car name-sym)
                                           (cdr name-sym)))
                                        all-dyn-pairs)
                                   compiled-body)))))
              #f)))))))))

;;; Handle the common part of defconst and defvar, that is, checking for
;;; a correct doc string and arguments as well as maybe in the future
;;; handling the docstring somehow.

(define (handle-var-def loc sym doc)
  (cond
   ((not (symbol? sym)) (report-error loc "expected symbol, got" sym))
   ((> (length doc) 1) (report-error loc "too many arguments to defvar"))
   ((and (not (null? doc)) (not (string? (car doc))))
    (report-error loc "expected string as third argument of defvar, got"
                  (car doc)))
   ;; TODO: Handle doc string if present.
   (else #t)))

;;; Handle macro and special operator bindings.

(define (find-operator sym type)
  (and
   (symbol? sym)
   (module-defined? (resolve-interface function-slot) sym)
   (let* ((op (module-ref (resolve-module function-slot) sym))
          (op (if (fluid? op) (fluid-ref op) op)))
     (if (and (pair? op) (eq? (car op) type))
         (cdr op)
         #f))))

;;; See if a (backquoted) expression contains any unquotes.

(define (contains-unquotes? expr)
  (if (pair? expr)
      (if (or (unquote? (car expr)) (unquote-splicing? (car expr)))
          #t
          (or (contains-unquotes? (car expr))
              (contains-unquotes? (cdr expr))))
      #f))

;;; Process a backquoted expression by building up the needed
;;; cons/append calls.  For splicing, it is assumed that the expression
;;; spliced in evaluates to a list.  The emacs manual does not really
;;; state either it has to or what to do if it does not, but Scheme
;;; explicitly forbids it and this seems reasonable also for elisp.

(define (unquote-cell? expr)
  (and (list? expr) (= (length expr) 2) (unquote? (car expr))))

(define (unquote-splicing-cell? expr)
  (and (list? expr) (= (length expr) 2) (unquote-splicing? (car expr))))

(define (process-backquote loc expr)
  (if (contains-unquotes? expr)
      (if (pair? expr)
          (if (or (unquote-cell? expr) (unquote-splicing-cell? expr))
              (compile-expr (cadr expr))
              (let* ((head (car expr))
                     (processed-tail (process-backquote loc (cdr expr)))
                     (head-is-list-2 (and (list? head)
                                          (= (length head) 2)))
                     (head-unquote (and head-is-list-2
                                        (unquote? (car head))))
                     (head-unquote-splicing (and head-is-list-2
                                                 (unquote-splicing?
                                                  (car head)))))
                (if head-unquote-splicing
                    (call-primitive loc
                                    'append
                                    (compile-expr (cadr head))
                                    processed-tail)
                    (call-primitive loc 'cons
                                    (if head-unquote
                                        (compile-expr (cadr head))
                                        (process-backquote loc head))
                                    processed-tail))))
          (report-error loc
                        "non-pair expression contains unquotes"
                        expr))
      (make-const loc expr)))

;;; Temporarily update a list of symbols that are handled specially
;;; (disabled void check or always lexical) for compiling body.  We need
;;; to handle special cases for already all / set to all and the like.

(define (with-added-symbols loc fluid syms body)
  (if (null? body)
      (report-error loc "symbol-list construct has empty body"))
  (if (not (or (eq? syms 'all)
               (and (list? syms) (and-map symbol? syms))))
      (report-error loc "invalid symbol list" syms))
  (let ((old (fluid-ref fluid))
        (make-body (lambda ()
                     (make-sequence loc (map compile-expr body)))))
    (if (eq? old 'all)
        (make-body)
        (let ((new (if (eq? syms 'all)
                       'all
                       (append syms old))))
          (with-fluids ((fluid new))
            (make-body))))))

;;; Special operators

(defspecial progn (loc args)
  (make-sequence loc (map compile-expr args)))

(defspecial if (loc args)
  (pmatch args
    ((,cond ,then . ,else)
     (make-conditional loc
                       (compile-expr cond)
                       (compile-expr then)
                       (if (null? else)
                           (nil-value loc)
                           (make-sequence loc
                                          (map compile-expr else)))))))

(defspecial defconst (loc args)
  (pmatch args
    ((,sym ,value . ,doc)
     (if (handle-var-def loc sym doc)
         (make-sequence loc
                        (list (set-variable! loc
                                             sym
                                             value-slot
                                             (compile-expr value))
                              (make-const loc sym)))))))

(defspecial defvar (loc args)
  (pmatch args
    ((,sym) (make-const loc sym))
    ((,sym ,value . ,doc)
     (if (handle-var-def loc sym doc)
         (make-sequence
          loc
          (list
           (make-conditional
            loc
            (make-conditional
             loc
             (call-primitive
              loc
              'module-bound?
              (call-primitive loc
                              'resolve-interface
                              (make-const loc value-slot))
              (make-const loc sym))
             (call-primitive loc
                             'fluid-bound?
                             (make-module-ref loc value-slot sym #t))
             (make-const loc #f))
            (make-void loc)
            (set-variable! loc sym value-slot (compile-expr value)))
           (make-const loc sym)))))))

(defspecial setq (loc args)
  (define (car* x) (if (null? x) '() (car x)))
  (define (cdr* x) (if (null? x) '() (cdr x)))
  (define (cadr* x) (car* (cdr* x)))
  (define (cddr* x) (cdr* (cdr* x)))
  (make-sequence
   loc
   (let loop ((args args) (last (nil-value loc)))
     (if (null? args)
         (list last)
         (let ((sym (car args))
               (val (compile-expr (cadr* args))))
           (if (not (symbol? sym))
               (report-error loc "expected symbol in setq")
               (cons
                (set-variable! loc sym value-slot val)
                (loop (cddr* args)
                      (reference-variable loc sym value-slot)))))))))
  
(defspecial let (loc args)
  (pmatch args
    ((,bindings . ,body)
     (generate-let loc value-slot bindings body))))

(defspecial lexical-let (loc args)
  (pmatch args
    ((,bindings . ,body)
     (generate-let loc 'lexical bindings body))))

(defspecial flet (loc args)
  (pmatch args
    ((,bindings . ,body)
     (generate-let loc function-slot bindings body))))

(defspecial let* (loc args)
  (pmatch args
    ((,bindings . ,body)
     (generate-let* loc value-slot bindings body))))

(defspecial lexical-let* (loc args)
  (pmatch args
    ((,bindings . ,body)
     (generate-let* loc 'lexical bindings body))))

(defspecial flet* (loc args)
  (pmatch args
    ((,bindings . ,body)
     (generate-let* loc function-slot bindings body))))

;;; Temporarily set symbols as always lexical only for the lexical scope
;;; of a construct.

(defspecial with-always-lexical (loc args)
  (pmatch args
    ((,syms . ,body)
     (with-added-symbols loc always-lexical syms body))))

;;; guile-ref allows building TreeIL's module references from within
;;; elisp as a way to access data within the Guile universe.  The module
;;; and symbol referenced are static values, just like (@ module symbol)
;;; does!

(defspecial guile-ref (loc args)
  (pmatch args
    ((,module ,sym) (guard (and (list? module) (symbol? sym)))
     (make-module-ref loc module sym #t))))

;;; guile-primitive allows to create primitive references, which are
;;; still a little faster.

(defspecial guile-primitive (loc args)
  (pmatch args
    ((,sym)
     (make-primitive-ref loc sym))))

;;; A while construct is transformed into a tail-recursive loop like
;;; this:
;;;
;;; (letrec ((iterate (lambda ()
;;;                     (if condition
;;;                       (begin body
;;;                              (iterate))
;;;                       #nil))))
;;;   (iterate))
;;;
;;; As letrec is not directly accessible from elisp, while is
;;; implemented here instead of with a macro.

(defspecial while (loc args)
  (pmatch args
    ((,condition . ,body)
     (let* ((itersym (gensym))
            (compiled-body (map compile-expr body))
            (iter-call (make-application loc
                                         (make-lexical-ref loc
                                                           'iterate
                                                           itersym)
                                         (list)))
            (full-body (make-sequence loc
                                      `(,@compiled-body ,iter-call)))
            (lambda-body (make-conditional loc
                                           (compile-expr condition)
                                           full-body
                                           (nil-value loc)))
            (iter-thunk (make-lambda loc
                                     '()
                                     (make-lambda-case #f
                                                       '()
                                                       #f
                                                       #f
                                                       #f
                                                       '()
                                                       '()
                                                       lambda-body
                                                       #f))))
       (make-letrec loc
                    #f
                    '(iterate)
                    (list itersym)
                    (list iter-thunk)
                    iter-call)))))

(defspecial function (loc args)
  (pmatch args
    (((lambda ,args . ,body))
     (compile-lambda loc args body))
    ((,sym) (guard (symbol? sym))
     (reference-variable loc sym function-slot))))

(defspecial defmacro (loc args)
  (pmatch args
    ((,name ,args . ,body)
     (if (not (symbol? name))
         (report-error loc "expected symbol as macro name" name)
         (let* ((tree-il
                 (make-sequence
                  loc
                  (list
                   (set-variable!
                    loc
                    name
                    function-slot
                    (make-application
                     loc
                     (make-module-ref loc '(guile) 'cons #t)
                     (list (make-const loc 'macro)
                           (compile-lambda loc args body))))
                   (make-const loc name)))))
           (compile (ensuring-globals loc bindings-data tree-il)
                    #:from 'tree-il
                    #:to 'value)
           tree-il)))))

(defspecial defun (loc args)
  (pmatch args
    ((,name ,args . ,body)
     (if (not (symbol? name))
         (report-error loc "expected symbol as function name" name)
         (make-sequence loc
                        (list (set-variable! loc
                                             name
                                             function-slot
                                             (compile-lambda loc
                                                             args
                                                             body))
                              (make-const loc name)))))))

(defspecial #{`}# (loc args)
  (pmatch args
    ((,val)
     (process-backquote loc val))))

(defspecial quote (loc args)
  (pmatch args
    ((,val)
     (make-const loc val))))

;;; Compile a compound expression to Tree-IL.

(define (compile-pair loc expr)
  (let ((operator (car expr))
        (arguments (cdr expr)))
    (cond
     ((find-operator operator 'special-operator)
      => (lambda (special-operator-function)
           (special-operator-function loc arguments)))
     ((find-operator operator 'macro)
      => (lambda (macro-function)
           (compile-expr (apply macro-function arguments))))
     (else
      (make-application loc
                        (if (symbol? operator)
                            (reference-variable loc
                                                operator
                                                function-slot)
                            (compile-expr operator))
                        (map compile-expr arguments))))))

;;; Compile a symbol expression.  This is a variable reference or maybe
;;; some special value like nil.

(define (compile-symbol loc sym)
  (case sym
    ((nil) (nil-value loc))
    ((t) (t-value loc))
    (else (reference-variable loc sym value-slot))))

;;; Compile a single expression to TreeIL.

(define (compile-expr expr)
  (let ((loc (location expr)))
    (cond
     ((symbol? expr)
      (compile-symbol loc expr))
     ((pair? expr)
      (compile-pair loc expr))
     (else (make-const loc expr)))))

;;; Process the compiler options.
;;; FIXME: Why is '(()) passed as options by the REPL?

(define (valid-symbol-list-arg? value)
  (or (eq? value 'all)
      (and (list? value) (and-map symbol? value))))

(define (process-options! opt)
  (if (and (not (null? opt))
           (not (equal? opt '(()))))
      (if (null? (cdr opt))
          (report-error #f "Invalid compiler options" opt)
          (let ((key (car opt))
                (value (cadr opt)))
            (case key
              ((#:warnings)             ; ignore
               #f)
              ((#:always-lexical)
               (if (valid-symbol-list-arg? value)
                   (fluid-set! always-lexical value)
                   (report-error #f
                                 "Invalid value for #:always-lexical"
                                 value)))
              (else (report-error #f
                                  "Invalid compiler option"
                                  key)))))))

;;; Entry point for compilation to TreeIL.  This creates the bindings
;;; data structure, and after compiling the main expression we need to
;;; make sure all globals for symbols used during the compilation are
;;; created using the generate-ensure-global function.

(define (compile-tree-il expr env opts)
  (values
   (with-fluids ((bindings-data (make-bindings))
                 (disable-void-check '())
                 (always-lexical '()))
     (process-options! opts)
     (let ((compiled (compile-expr expr)))
      (ensuring-globals (location expr) bindings-data compiled)))
   env
   env))
