;;; Guile Emacs Lisp

;; Copyright (C) 2009-2011, 2013, 2018 Free Software Foundation, Inc.

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
  #:use-module (system base target)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (compile-tree-il
            compile-progn
            compile-eval-when-compile
            compile-if
            compile-defconst
            compile-defvar
            compile-setq
            compile-let
            compile-flet
            compile-labels
            compile-let*
            compile-guile-ref
            compile-guile-primitive
            compile-function
            compile-defmacro
            compile-defun
            #{compile-`}#
            compile-quote
            compile-%funcall
            compile-%set-lexical-binding-mode))

;;; Certain common parameters (like the bindings data structure or
;;; compiler options) are not always passed around but accessed using
;;; fluids to simulate dynamic binding (hey, this is about elisp).

;;; The bindings data structure to keep track of symbol binding related
;;; data.

(define bindings-data (make-fluid))

(define lexical-binding (make-fluid))

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
  (make-primcall loc sym args))

;;; Error reporting routine for syntax/compilation problems or build
;;; code for a runtime-error output.

(define (report-error loc . args)
  (apply error args))

(define (access-variable loc symbol handle-lexical handle-dynamic)
  (cond
   ((get-lexical-binding (fluid-ref bindings-data) symbol)
    => handle-lexical)
   (else
    (handle-dynamic))))

(define (reference-variable loc symbol)
  (access-variable
   loc
   symbol
   (lambda (lexical)
     (make-lexical-ref loc lexical lexical))
   (lambda ()
     (call-primitive loc
                     'fluid-ref
                     (make-module-ref loc value-slot symbol #t)))))

(define (global? module symbol)
  (module-variable module symbol))

(define (ensure-globals! loc names body)
  (if (and (every (cut global? (resolve-module value-slot) <>) names)
           (every symbol-interned? names))
      body
      (list->seq
       loc
       `(,@(map
            (lambda (name)
              (ensure-fluid! value-slot name)
              (make-call loc
                         (make-module-ref loc runtime 'ensure-fluid! #t)
                         (list (make-const loc value-slot)
                               (make-const loc name))))
            names)
         ,body))))

(define (set-variable! loc symbol value)
  (access-variable
   loc
   symbol
   (lambda (lexical)
     (make-lexical-set loc lexical lexical value))
   (lambda ()
     (ensure-globals!
      loc
      (list symbol)
      (call-primitive loc
                      'fluid-set!
                      (make-module-ref loc value-slot symbol #t)
                      value)))))

(define (access-function loc symbol handle-lexical handle-global)
  (cond
   ((get-function-binding (fluid-ref bindings-data) symbol)
    => handle-lexical)
   (else
    (handle-global))))

(define (reference-function loc symbol)
  (access-function
   loc
   symbol
   (lambda (gensym) (make-lexical-ref loc symbol gensym))
   (lambda () (make-module-ref loc function-slot symbol #t))))

(define (set-function! loc symbol value)
  (access-function
   loc
   symbol
   (lambda (gensym) (make-lexical-set loc symbol gensym value))
   (lambda ()
     (make-call
      loc
      (make-module-ref loc runtime 'set-symbol-function! #t)
      (list (make-const loc symbol) value)))))

(define (bind-lexically? sym module decls)
  (or (eq? module function-slot)
      (let ((decl (assq-ref decls sym)))
        (and (equal? module value-slot)
             (or
              (eq? decl 'lexical)
              (and
               (fluid-ref lexical-binding)
               (not (global? (resolve-module module) sym))))))))

(define (parse-let-binding loc binding)
  (pmatch binding
    ((unquote var)
     (guard (symbol? var))
     (cons var #nil))
    ((,var)
     (guard (symbol? var))
     (cons var #nil))
    ((,var ,val)
     (guard (symbol? var))
     (cons var val))
    (else
     (report-error loc "malformed variable binding" binding))))

(define (parse-flet-binding loc binding)
  (pmatch binding
    ((,var ,args . ,body)
     (guard (symbol? var))
     (cons var `(function (lambda ,args ,@body))))
    (else
     (report-error loc "malformed function binding" binding))))

(define (parse-declaration expr)
  (pmatch expr
    ((lexical . ,vars)
     (map (cut cons <> 'lexical) vars))
    (else
     '())))

(define (parse-body-1 body lambda?)
  (let loop ((lst body)
             (decls '())
             (intspec #f)
             (doc #f))
    (pmatch lst
      (((declare . ,x) . ,tail)
       (loop tail (append-reverse x decls) intspec doc))
      (((interactive . ,x) . ,tail)
       (guard lambda? (not intspec))
       (loop tail decls x doc))
      ((,x . ,tail)
       (guard lambda? (string? x) (not doc) (not (null? tail)))
       (loop tail decls intspec x))
      (else
       (values (append-map parse-declaration decls)
               intspec
               doc
               lst)))))

(define (parse-lambda-body body)
  (parse-body-1 body #t))

(define (parse-body body)
  (receive (decls intspec doc body) (parse-body-1 body #f)
    (values decls body)))

;;; Partition the argument list of a lambda expression into required,
;;; optional and rest arguments.

(define (parse-lambda-list lst)
  (define (%match lst null optional rest symbol)
    (pmatch lst
      (() (null))
      ((&optional . ,tail) (optional tail))
      ((&rest . ,tail) (rest tail))
      ((,arg . ,tail) (guard (symbol? arg)) (symbol arg tail))
      (else (fail))))
  (define (return rreq ropt rest)
    (values #t (reverse rreq) (reverse ropt) rest))
  (define (fail)
    (values #f #f #f #f))
  (define (parse-req lst rreq)
    (%match lst
            (lambda () (return rreq '() #f))
            (lambda (tail) (parse-opt tail rreq '()))
            (lambda (tail) (parse-rest tail rreq '()))
            (lambda (arg tail) (parse-req tail (cons arg rreq)))))
  (define (parse-opt lst rreq ropt)
    (%match lst
            (lambda () (return rreq ropt #f))
            (lambda (tail) (fail))
            (lambda (tail) (parse-rest tail rreq ropt))
            (lambda (arg tail) (parse-opt tail rreq (cons arg ropt)))))
  (define (parse-rest lst rreq ropt)
    (%match lst
            (lambda () (fail))
            (lambda (tail) (fail))
            (lambda (tail) (fail))
            (lambda (arg tail) (parse-post-rest tail rreq ropt arg))))
  (define (parse-post-rest lst rreq ropt rest)
    (%match lst
            (lambda () (return rreq ropt rest))
            (lambda () (fail))
            (lambda () (fail))
            (lambda (arg tail) (fail))))
  (parse-req lst '()))

(define (make-simple-lambda loc meta req opt init rest vars body)
  (make-lambda loc
               meta
               (make-lambda-case #f req opt rest #f init vars body #f)))

(define (make-dynlet src fluids vals body)
  (let ((f (map (lambda (x) (gensym "fluid ")) fluids))
        (v (map (lambda (x) (gensym "valud ")) vals)))
    (make-let src (map (lambda (_) 'fluid) fluids) f fluids
              (make-let src (map (lambda (_) 'val) vals) v vals
                        (let lp ((f f) (v v))
                          (if (null? f)
                              body
                              (make-primcall
                               src 'with-fluid*
                               (list (make-lexical-ref #f 'fluid (car f))
                                     (make-lexical-ref #f 'val (car v))
                                     (make-lambda
                                      src '()
                                      (make-lambda-case
                                       src '() #f #f #f '() '()
                                       (lp (cdr f) (cdr v))
                                       #f))))))))))

(define (compile-lambda loc meta args body)
  (receive (valid? req-ids opt-ids rest-id)
           (parse-lambda-list args)
    (if valid?
        (let* ((all-ids (append req-ids
                                opt-ids
                                (or (and=> rest-id list) '())))
               (all-vars (map (lambda (ignore) (gensym)) all-ids)))
          (let*-values (((decls intspec doc forms)
                         (parse-lambda-body body))
                        ((lexical dynamic)
                         (partition
                          (compose (cut bind-lexically? <> value-slot decls)
                                   car)
                          (map list all-ids all-vars)))
                        ((lexical-ids lexical-vars) (unzip2 lexical))
                        ((dynamic-ids dynamic-vars) (unzip2 dynamic)))
            (with-dynamic-bindings
             (fluid-ref bindings-data)
             dynamic-ids
             (lambda ()
               (with-lexical-bindings
                (fluid-ref bindings-data)
                lexical-ids
                lexical-vars
                (lambda ()
                  (ensure-globals!
                   loc
                   dynamic-ids
                   (let* ((tree-il
                           (compile-expr
                            (if rest-id
                                `(let ((,rest-id (if ,rest-id
                                                     ,rest-id
                                                     nil)))
                                   ,@forms)
                                `(progn ,@forms))))
                          (full-body
                           (if (null? dynamic)
                               tree-il
                               (make-dynlet
                                loc
                                (map (cut make-module-ref loc value-slot <> #t)
                                     dynamic-ids)
                                (map (cut make-lexical-ref loc <> <>)
                                     dynamic-ids
                                     dynamic-vars)
                                tree-il))))
                     (make-simple-lambda loc
                                         meta
                                         req-ids
                                         opt-ids
                                         (map (const (nil-value loc))
                                              opt-ids)
                                         rest-id
                                         all-vars
                                         full-body)))))))))
        (report-error "invalid function" `(lambda ,args ,@body)))))

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

(define (find-operator name type)
  (and
   (symbol? name)
   (module-defined? (resolve-interface function-slot) name)
   (let ((op (module-ref (resolve-module function-slot) name)))
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

;;; Special operators

(defspecial progn (loc args)
  (list->seq loc
             (if (null? args)
                 (list (nil-value loc))
                 (map compile-expr args))))

(defspecial eval-when-compile (loc args)
  (make-const loc (with-native-target
                   (lambda ()
                     (compile `(progn ,@args) #:from 'elisp #:to 'value)))))

(defspecial if (loc args)
  (pmatch args
    ((,cond ,then . ,else)
     (make-conditional
      loc
      (call-primitive loc 'not
       (call-primitive loc 'nil? (compile-expr cond)))
      (compile-expr then)
      (compile-expr `(progn ,@else))))))

(defspecial defconst (loc args)
  (pmatch args
    ((,sym ,value . ,doc)
     (if (handle-var-def loc sym doc)
         (make-seq loc
                   (set-variable! loc sym (compile-expr value))
                   (make-const loc sym))))))

(defspecial defvar (loc args)
  (pmatch args
    ((,sym) (make-const loc sym))
    ((,sym ,value . ,doc)
     (if (handle-var-def loc sym doc)
         (make-seq
          loc
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
           (set-variable! loc sym (compile-expr value)))
          (make-const loc sym))))))

(defspecial setq (loc args)
  (define (car* x) (if (null? x) '() (car x)))
  (define (cdr* x) (if (null? x) '() (cdr x)))
  (define (cadr* x) (car* (cdr* x)))
  (define (cddr* x) (cdr* (cdr* x)))
  (list->seq
   loc
   (let loop ((args args) (last (nil-value loc)))
     (if (null? args)
         (list last)
         (let ((sym (car args))
               (val (compile-expr (cadr* args))))
           (if (not (symbol? sym))
               (report-error loc "expected symbol in setq")
               (cons
                (set-variable! loc sym val)
                (loop (cddr* args)
                      (reference-variable loc sym)))))))))
  
(defspecial let (loc args)
  (pmatch args
    ((,varlist . ,body)
     (let ((bindings (map (cut parse-let-binding loc <>) varlist)))
       (receive (decls forms) (parse-body body)
         (receive (lexical dynamic)
                  (partition
                   (compose (cut bind-lexically? <> value-slot decls)
                            car)
                   bindings)
           (let ((make-values (lambda (for)
                                (map (lambda (el) (compile-expr (cdr el)))
                                     for)))
                 (make-body (lambda () (compile-expr `(progn ,@forms)))))
             (ensure-globals!
              loc
              (map car dynamic)
              (if (null? lexical)
                  (make-dynlet loc
                               (map (compose (cut make-module-ref
                                                  loc
                                                  value-slot
                                                  <>
                                                  #t)
                                             car)
                                    dynamic)
                               (map (compose compile-expr cdr)
                                    dynamic)
                               (make-body))
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
                               (map car lexical)
                               lexical-syms
                               (lambda ()
                                 (if (null? dynamic)
                                     (make-body)
                                     (make-dynlet loc
                                                  (map
                                                   (compose
                                                    (cut make-module-ref
                                                         loc
                                                         value-slot
                                                         <>
                                                         #t)
                                                    car)
                                                   dynamic)
                                                  (map
                                                   (lambda (sym)
                                                     (make-lexical-ref
                                                      loc
                                                      sym
                                                      sym))
                                                   dynamic-syms)
                                                  (make-body))))))))))))))))

(defspecial let* (loc args)
  (pmatch args
    ((,varlist . ,body)
     (let ((bindings (map (cut parse-let-binding loc <>) varlist)))
       (receive (decls forms) (parse-body body)
         (let iterate ((tail bindings))
           (if (null? tail)
               (compile-expr `(progn ,@forms))
               (let ((sym (caar tail))
                     (value (compile-expr (cdar tail))))
                 (if (bind-lexically? sym value-slot decls)
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
                     (ensure-globals!
                      loc
                      (list sym)
                      (make-dynlet loc
                                   (list (make-module-ref loc value-slot sym #t))
                                   (list value)
                                   (iterate (cdr tail)))))))))))))

(defspecial flet (loc args)
  (pmatch args
    ((,bindings . ,body)
     (let ((names+vals (map (cut parse-flet-binding loc <>) bindings)))
       (receive (decls forms) (parse-body body)
         (let ((names (map car names+vals))
               (vals (map cdr names+vals))
               (gensyms (map (lambda (x) (gensym)) names+vals)))
           (with-function-bindings
            (fluid-ref bindings-data)
            names
            gensyms
            (lambda ()
              (make-let loc
                        names
                        gensyms
                        (map compile-expr vals)
                        (compile-expr `(progn ,@forms)))))))))))

(defspecial labels (loc args)
  (pmatch args
    ((,bindings . ,body)
     (let ((names+vals (map (cut parse-flet-binding loc <>) bindings)))
       (receive (decls forms) (parse-body body)
         (let ((names (map car names+vals))
               (vals (map cdr names+vals))
               (gensyms (map (lambda (x) (gensym)) names+vals)))
           (with-function-bindings
            (fluid-ref bindings-data)
            names
            gensyms
            (lambda ()
              (make-letrec #f
                           loc
                           names
                           gensyms
                           (map compile-expr vals)
                           (compile-expr `(progn ,@forms)))))))))))

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

(defspecial function (loc args)
  (pmatch args
    (((lambda ,args . ,body))
     (compile-lambda loc '() args body))
    ((,sym) (guard (symbol? sym))
     (reference-function loc sym))))

(defspecial defmacro (loc args)
  (pmatch args
    ((,name ,args . ,body)
     (if (not (symbol? name))
         (report-error loc "expected symbol as macro name" name)
         (let* ((tree-il
                 (make-seq
                  loc
                  (set-function!
                   loc
                   name
                   (make-call
                    loc
                    (make-module-ref loc '(guile) 'cons #t)
                    (list (make-const loc 'macro)
                          (compile-lambda loc
                                          `((name . ,name))
                                          args
                                          body))))
                  (make-const loc name))))
           (with-native-target
            (lambda ()
              (compile tree-il #:from 'tree-il #:to 'value)))
           tree-il)))))

(defspecial defun (loc args)
  (pmatch args
    ((,name ,args . ,body)
     (if (not (symbol? name))
         (report-error loc "expected symbol as function name" name)
         (make-seq loc
                   (set-function! loc
                                  name
                                  (compile-lambda loc
                                                  `((name . ,name))
                                                  args
                                                  body))
                   (make-const loc name))))))

(defspecial #{`}# (loc args)
  (pmatch args
    ((,val)
     (process-backquote loc val))))

(defspecial quote (loc args)
  (pmatch args
    ((,val)
     (make-const loc val))))

(defspecial %funcall (loc args)
  (pmatch args
    ((,function . ,arguments)
     (make-call loc
                (compile-expr function)
                (map compile-expr arguments)))))

(defspecial %set-lexical-binding-mode (loc args)
  (pmatch args
    ((,val)
     (fluid-set! lexical-binding val)
     (make-void loc))))

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
      (compile-expr `(%funcall (function ,operator) ,@arguments))))))

;;; Compile a symbol expression.  This is a variable reference or maybe
;;; some special value like nil.

(define (compile-symbol loc sym)
  (case sym
    ((nil) (nil-value loc))
    ((t) (t-value loc))
    (else (reference-variable loc sym))))

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
              ((#:warnings #:to-file?)  ; ignore
               #f)
              (else (report-error #f
                                  "Invalid compiler option"
                                  key)))))))

(define (compile-tree-il expr env opts)
  (values
   (with-fluids ((bindings-data (make-bindings)))
     (process-options! opts)
     (compile-expr expr))
   env
   env))
