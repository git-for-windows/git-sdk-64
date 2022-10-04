;;; Diagnostic warnings for Tree-IL

;; Copyright (C) 2001,2008-2014,2016,2018-2022 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language tree-il analyze)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (system base syntax)
  #:use-module (system base message)
  #:use-module (system vm program)
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  #:export (analyze-tree
            unused-variable-analysis
            unused-toplevel-analysis
            shadowed-toplevel-analysis
            make-use-before-definition-analysis
            arity-analysis
            format-analysis
            make-analyzer))

;;;
;;; Tree analyses for warnings.
;;;

(define-record-type <tree-analysis>
  (make-tree-analysis down up post init)
  tree-analysis?
  (down tree-analysis-down)  ;; (lambda (x result env locs) ...)
  (up   tree-analysis-up)    ;; (lambda (x result env locs) ...)
  (post tree-analysis-post)  ;; (lambda (result env) ...)
  (init tree-analysis-init)) ;; arbitrary value

(define (analyze-tree analyses tree env)
  "Run all tree analyses listed in ANALYSES on TREE for ENV, using
`tree-il-fold'.  Return TREE.  The down and up procedures of each
analysis are passed a ``location stack', which is the stack of
`tree-il-src' values for each parent tree (a list); it can be used to
approximate source location when accurate information is missing from a
given `tree-il' element."

  (define (traverse proc update-locs)
    ;; Return a tree traversing procedure that returns a list of analysis
    ;; results prepended by the location stack.
    (lambda (x results)
      (let ((locs (update-locs x (car results))))
        (cons locs ;; the location stack
              (map (lambda (analysis result)
                     ((proc analysis) x result env locs))
                   analyses
                   (cdr results))))))

  ;; Extending and shrinking the location stack.
  (define (extend-locs x locs) (cons (tree-il-src x) locs))
  (define (shrink-locs x locs) (cdr locs))

  (let ((results
         (tree-il-fold (traverse tree-analysis-down extend-locs)
                       (traverse tree-analysis-up   shrink-locs)
                       (cons '() ;; empty location stack
                             (map tree-analysis-init analyses))
                       tree)))

    (for-each (lambda (analysis result)
                ((tree-analysis-post analysis) result env))
              analyses
              (cdr results)))

  tree)


;;;
;;; Unused variable analysis.
;;;

;; <binding-info> records are used during tree traversals in
;; `unused-variable-analysis'.  They contain a list of the local vars
;; currently in scope, and a list of locals vars that have been referenced.
(define-record-type <binding-info>
  (make-binding-info vars refs)
  binding-info?
  (vars binding-info-vars)  ;; ((GENSYM NAME LOCATION) ...)
  (refs binding-info-refs)) ;; (GENSYM ...)

(define (gensym? sym)
  ;; Return #t if SYM is (likely) a generated symbol.
  (string-any #\space (symbol->string sym)))

(define unused-variable-analysis
  ;; Report unused variables in the given tree.
  (make-tree-analysis
   (lambda (x info env locs)
     ;; Going down into X: extend INFO's variable list
     ;; accordingly.
     (let ((refs (binding-info-refs info))
           (vars (binding-info-vars info))
           (src  (tree-il-src x)))
       (define (extend inner-vars inner-names)
         (fold (lambda (var name vars)
                 (vhash-consq var (list name src) vars))
               vars
               inner-vars
               inner-names))

       (record-case x
         ((<lexical-ref> gensym)
          (make-binding-info vars (vhash-consq gensym #t refs)))
         ((<lexical-set> gensym)
          (make-binding-info vars (vhash-consq gensym #t refs)))
         ((<lambda-case> req opt inits rest kw gensyms)
          (let ((names `(,@req
                         ,@(or opt '())
                         ,@(if rest (list rest) '())
                         ,@(if kw (map cadr (cdr kw)) '()))))
            (make-binding-info (extend gensyms names) refs)))
         ((<let> gensyms names)
          (make-binding-info (extend gensyms names) refs))
         ((<letrec> gensyms names)
          (make-binding-info (extend gensyms names) refs))
         ((<fix> gensyms names)
          (make-binding-info (extend gensyms names) refs))
         (else info))))

   (lambda (x info env locs)
     ;; Leaving X's scope: shrink INFO's variable list
     ;; accordingly and reported unused nested variables.
     (let ((refs (binding-info-refs info))
           (vars (binding-info-vars info)))
       (define (shrink inner-vars refs)
         (vlist-for-each
          (lambda (var)
            (let ((gensym (car var)))
              ;; Don't report lambda parameters as unused.
              (if (and (memq gensym inner-vars)
                       (not (vhash-assq gensym refs))
                       (not (lambda-case? x)))
                  (let ((name (cadr var))
                        ;; We can get approximate source location by going up
                        ;; the LOCS location stack.
                        (loc  (or (caddr var)
                                  (find pair? locs))))
                    (if (and (not (gensym? name))
                             (not (eq? name '_)))
                        (warning 'unused-variable loc name))))))
          vars)
         (vlist-drop vars (length inner-vars)))

       ;; For simplicity, we leave REFS untouched, i.e., with
       ;; names of variables that are now going out of scope.
       ;; It doesn't hurt as these are unique names, it just
       ;; makes REFS unnecessarily fat.
       (record-case x
         ((<lambda-case> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         ((<let> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         ((<letrec> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         ((<fix> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         (else info))))

   (lambda (result env) #t)
   (make-binding-info vlist-null vlist-null)))


;;;
;;; Unused top-level variable analysis.
;;;

;; <reference-graph> record top-level definitions that are made, references to
;; top-level definitions and their context (the top-level definition in which
;; the reference appears), as well as the current context (the top-level
;; definition we're currently in).  The second part (`refs' below) is
;; effectively a graph from which we can determine unused top-level definitions.
(define-record-type <reference-graph>
  (make-reference-graph refs defs toplevel-context)
  reference-graph?
  (defs             reference-graph-defs) ;; ((NAME . LOC) ...)
  (refs             reference-graph-refs) ;; ((REF-CONTEXT REF ...) ...)
  (toplevel-context reference-graph-toplevel-context)) ;; NAME | #f

(define (graph-reachable-nodes root refs reachable)
  ;; Add to REACHABLE the nodes reachable from ROOT in graph REFS.  REFS is a
  ;; vhash mapping nodes to the list of their children: for instance,
  ;; ((A -> (B C)) (B -> (A)) (C -> ())) corresponds to
  ;;
  ;;  ,-------.
  ;;  v       |
  ;;  A ----> B
  ;;  |
  ;;  v
  ;;  C
  ;;
  ;; REACHABLE is a vhash of nodes known to be otherwise reachable.

  (let loop ((root   root)
             (path   vlist-null)
             (result reachable))
    (if (or (vhash-assq root path)
            (vhash-assq root result))
        result
        (let* ((children (or (and=> (vhash-assq root refs) cdr) '()))
               (path     (vhash-consq root #t path))
               (result   (fold (lambda (kid result)
                                 (loop kid path result))
                               result
                               children)))
          (fold (lambda (kid result)
                  (vhash-consq kid #t result))
                result
                children)))))

(define (graph-reachable-nodes* roots refs)
  ;; Return the list of nodes in REFS reachable from the nodes listed in ROOTS.
  (vlist-fold (lambda (root+true result)
                (let* ((root      (car root+true))
                       (reachable (graph-reachable-nodes root refs result)))
                  (vhash-consq root #t reachable)))
              vlist-null
              roots))

(define (partition* pred vhash)
  ;; Partition VHASH according to PRED.  Return the two resulting vhashes.
  (let ((result
         (vlist-fold (lambda (k+v result)
                       (let ((k  (car k+v))
                             (v  (cdr k+v))
                             (r1 (car result))
                             (r2 (cdr result)))
                         (if (pred k)
                             (cons (vhash-consq k v r1) r2)
                             (cons r1 (vhash-consq k v r2)))))
                     (cons vlist-null vlist-null)
                     vhash)))
    (values (car result) (cdr result))))

(define unused-toplevel-analysis
  ;; Report unused top-level definitions that are not exported.
  (let ((add-ref-from-context
         (lambda (graph name)
           ;; Add an edge CTX -> NAME in GRAPH.
           (let* ((refs     (reference-graph-refs graph))
                  (defs     (reference-graph-defs graph))
                  (ctx      (reference-graph-toplevel-context graph))
                  (ctx-refs (or (and=> (vhash-assq ctx refs) cdr) '())))
             (make-reference-graph (vhash-consq ctx (cons name ctx-refs) refs)
                                   defs ctx)))))
    (define (macro-variable? name env)
      (and (module? env)
           (let ((var (module-variable env name)))
             (and var (variable-bound? var)
                  (macro? (variable-ref var))))))

    (make-tree-analysis
     (lambda (x graph env locs)
       ;; Going down into X.
       (let ((ctx  (reference-graph-toplevel-context graph))
             (refs (reference-graph-refs graph))
             (defs (reference-graph-defs graph)))
         (record-case x
           ((<toplevel-ref> name src)
            (add-ref-from-context graph name))
           ((<toplevel-define> name src)
            (let ((refs refs)
                  (defs (vhash-consq name (or src (find pair? locs))
                                     defs)))
              (make-reference-graph refs defs name)))
           ((<toplevel-set> name src)
            (add-ref-from-context graph name))
           (else graph))))

     (lambda (x graph env locs)
       ;; Leaving X's scope.
       (record-case x
         ((<toplevel-define>)
          (let ((refs (reference-graph-refs graph))
                (defs (reference-graph-defs graph)))
            (make-reference-graph refs defs #f)))
         (else graph)))

     (lambda (graph env)
       ;; Process the resulting reference graph: determine all private definitions
       ;; not reachable from any public definition.  Macros
       ;; (syntax-transformers), which are globally bound, never considered
       ;; unused since we can't tell whether a macro is actually used; in
       ;; addition, macros are considered roots of the graph since they may use
       ;; private bindings.  FIXME: The `make-syntax-transformer' calls don't
       ;; contain any literal `toplevel-ref' of the global bindings they use so
       ;; this strategy fails.
       (define (exported? name)
         (if (module? env)
             (module-variable (module-public-interface env) name)
             #t))

       (let-values (((public-defs private-defs)
                     (partition* (lambda (name)
                                   (or (exported? name)
                                       (macro-variable? name env)))
                                 (reference-graph-defs graph))))
         (let* ((roots     (vhash-consq #f #t public-defs))
                (refs      (reference-graph-refs graph))
                (reachable (graph-reachable-nodes* roots refs))
                (unused    (vlist-filter (lambda (name+src)
                                           (not (vhash-assq (car name+src)
                                                            reachable)))
                                         private-defs)))
           (vlist-for-each (lambda (name+loc)
                             (let ((name (car name+loc))
                                   (loc  (cdr name+loc)))
                               (if (not (gensym? name))
                                   (warning 'unused-toplevel loc name))))
                           unused))))

     (make-reference-graph vlist-null vlist-null #f))))


;;;
;;; Shadowed top-level definition analysis.
;;;

(define shadowed-toplevel-analysis
  ;; Report top-level definitions that shadow previous top-level
  ;; definitions from the same compilation unit.
  (make-tree-analysis
   (lambda (x defs env locs)
     ;; Going down into X.
     (record-case x
                  ((<toplevel-define> name)
                   (match (vhash-assq name defs)
                     ((_ . previous-definition)
                      (warning 'shadowed-toplevel (tree-il-src x) name
                               (tree-il-src previous-definition))
                      defs)
                     (#f
                      (vhash-consq name x defs))))
                  (else defs)))

   (lambda (x defs env locs)
     ;; Leaving X's scope.
     defs)

   (lambda (defs env)
     #t)

   vlist-null))


;;;
;;; Use before definition analysis.
;;;
;;; This analysis collects all definitions of top-level variables, and
;;; references to top-level variables.  As it visits the term, it tries
;;; to match uses to the definition that corresponds to that program
;;; point.  For example, in this sample program:
;;;
;;;   (define a 42)
;;;   (define b a)
;;;
;;; The analysis will be able to know that the definition of "a"
;;; referred to when defining "b" is 42.
;;;
;;; In many cases this definition is conservative.  For example, in this
;;; code:
;;;
;;;   (define a 42)
;;;   (define b (lambda () a))
;;;
;;; We don't necessarily know that the "a" in the lambda is 42, as a
;;; further top-level definition could provide a different value.
;;; However, we do know that "a" is bound, unlike in this code:
;;;
;;;   (define b (lambda () a))
;;;
;;; Here we should issue a warning if no import provides an "a" binding.
;;;
;;; Use-before-def analysis also issues specialized warnings for some
;;; less common errors.  One relates specifically to macro use before
;;; definition.  If a compilation unit defines a macro and has some uses
;;; of the macro, usually the uses will be expanded out by the
;;; macro-expander.  If there is any reference to a macro as a value,
;;; that usually indicates a bug in the user's program.  Like in this
;;; program:
;;;
;;;   (define (a) (b))
;;;   (define-syntax-rule (b) 42)
;;;
;;; If this program is expanded one top-level expression at a time,
;;; which is Guile's default compilation mode, the expander will assume
;;; that the reference to (b) is a call to a top-level procedure, only
;;; to find out it's a macro later on.  Use-before-def analysis can warn
;;; for this case.
;;;
;;; Similarly, if a compilation unit uses an imported binding, then
;;; provides a local definition for the binding, this may cause problems
;;; if the module is re-loaded.  Consider:
;;;
;;;   (define-module (foo))
;;;   (define a +)
;;;   (define + -)
;;;
;;; In this fragment, we see the intention of the programmer is to
;;; locally redefine `+', but to preserve the previous definition in
;;; `a'.
;;;
;;; However, if the module is loaded twice, `a' will be bound not to the
;;; `(guile)' binding of `+', but rather to `-'.  This is because each
;;; module has a single global instance, and the first definition
;;; already bound `+' to `-'.  Use-before-def analysis can detect this
;;; situation as well.
;;;

;;; <use-before-def-info> records are used during tree traversal in
;;; search of possible uses of values before they are defined.  They
;;; contain a list of references to top-level variables, and a list of
;;; the top-level definitions that have been encountered.  Any definition
;;; which is a macro should in theory be expanded out already; if that's
;;; not the case, the program likely has a bug.
(define-record-type <use-before-def-info>
  (make-use-before-def-info depth uses defs)
  use-before-def-info?
  ;; LOCAL-DEF := #(MACRO? DEPTH LOCATION)
  ;; DEF := LOCAL-DEF           ; Defined in compilation unit already at use.
  ;;      | import              ; Def provided by imported module.
  ;;      | unknown-module      ; Module at use site not known.
  ;;      | unknown-declarative ; Defined, but def not within compilation unit.
  ;;      | unknown-imperative  ; Same as above, but in non-declarative module.
  ;;      | unbound             ; No top-level definition known at use
  ;; USE := #(MOD-NAME VAR-NAME DEPTH DEF LOCATION)
  (depth use-before-def-info-depth) ;; Zero if definitely evaluated
  (uses  use-before-def-info-uses)  ;; List of USE
  (defs  use-before-def-info-defs))  ;; Vhash of ((MOD . NAME) . LOCAL-DEF)

(define (goops-toplevel-definition proc args env)
  ;; If call of PROC to ARGS is a GOOPS top-level definition, return the
  ;; name of the variable being defined; otherwise return #f.  This
  ;; assumes knowledge of the current implementation of `define-class'
  ;; et al.
  (match (cons proc args)
    ((($ <module-ref> _ '(oop goops) 'toplevel-define! #f)
      ($ <const> _ (? symbol? name))
      exp)
     ;; We don't know the precise module in which we are defining the
     ;; variable :/  Guess that it's in `env'.
     (vector (module-name env) name exp))
    ((($ <toplevel-ref> _ '(oop goops) 'toplevel-define!)
      ($ <const> _ (? symbol? name))
      exp)
     (vector '(oop goops) name exp))
    (_ #f)))

(define* (make-use-before-definition-analysis #:key (warning-level 0)
                                              (enabled-warnings '()))
  ;; Report possibly unbound variables in the given tree.
  (define (enabled-for-level? level) (<= level warning-level))
  (define-syntax-rule (define-warning enabled
                        #:level level #:name warning-name)
    (define enabled
      (or (enabled-for-level? level)
          (memq 'warning-name enabled-warnings))))
  (define-warning use-before-definition-enabled
    #:level 1 #:name use-before-definition)
  (define-warning unbound-variable-enabled
    #:level 1 #:name unbound-variable)
  (define-warning macro-use-before-definition-enabled
    #:level 1 #:name macro-use-before-definition)
  (define-warning non-idempotent-definition-enabled
    #:level 1 #:name non-idempotent-definition)
  (define (resolve mod name defs)
    (match (vhash-assoc (cons mod name) defs)
      ((_ . local-def)
       ;; Top-level def present in this compilation unit, before this
       ;; use.
       local-def)
      (#f
       (let ((mod (and mod (resolve-module mod #f #:ensure #f))))
         (cond
          ((not mod)
           ;; We don't know the module with respect to which this var
           ;; is being resolved.
           'unknown-module)
          ((module-local-variable mod name)
           ;; The variable is locally bound in the module, but not by
           ;; any definition in the compilation unit; perhaps by load
           ;; or load-extension or something.
           (if (module-declarative? mod)
               'unknown-declarative
               'unknown-imperative))
          ((module-variable mod name)
           ;; The variable is an import.  At the time of use, the
           ;; name is bound to the import.
           'import)
          ((and=> (module-public-interface mod)
                  (lambda (interface)
                    (module-variable interface name)))
           ;; The variable is re-exported from another module.
           'import)
          (else
           ;; Variable unbound in the module.
           'unbound))))))

  (and
   (or use-before-definition-enabled
       unbound-variable-enabled
       macro-use-before-definition-enabled
       non-idempotent-definition-enabled)
   (make-tree-analysis
    (lambda (x info env locs)
      ;; Going down into X.
      (define (make-use mod name depth def src)
        (vector mod name depth def src))
      (define (make-def is-macro? depth src)
        (vector is-macro? depth src))
      (define (nearest-loc src)
        (or src (find pair? locs)))
      (define (add-use mod name src)
        (match info
          (($ <use-before-def-info> depth uses defs)
           (let* ((def (resolve mod name defs))
                  (use (make-use mod name depth def src)))
             (make-use-before-def-info depth (cons use uses) defs)))))
      (define (add-def mod name src is-macro?)
        (match info
          (($ <use-before-def-info> depth uses defs)
           (let ((def (make-def is-macro? depth src)))
             (make-use-before-def-info depth uses
                                       (vhash-cons (cons mod name) def
                                                   defs))))))
      (define (macro? x)
        (match x
          (($ <primcall> _ 'make-syntax-transformer) #t)
          (_ #f)))
      (match x
        (($ <toplevel-ref> src mod name)
         (add-use mod name (nearest-loc src)))
        (($ <toplevel-set> src mod name)
         (add-use mod name (nearest-loc src)))
        (($ <toplevel-define> src mod name exp)
         (add-def mod name (nearest-loc src) (macro? exp)))
        (($ <call> src proc args)
         ;; Check for a dynamic top-level definition, as is
         ;; done by code expanded from GOOPS macros.
         (match (goops-toplevel-definition proc args env)
           (#f info)
           (#(mod name exp) (add-def mod name (nearest-loc src) (macro? exp)))))
        ((or ($ <lambda>) ($ <conditional>))
         (match info
           (($ <use-before-def-info> depth uses defs)
            (make-use-before-def-info (1+ depth) uses defs))))
        (_ info)))

    (lambda (x info env locs)
      ;; Leaving X's scope.
      (match x
        ((or ($ <lambda>) ($ <conditional>))
         (match info
           (($ <use-before-def-info> depth uses defs)
            (make-use-before-def-info (1- depth) uses defs))))
        (_ info)))

    (lambda (info env)
      (define (compute-macros defs)
        (let ((macros (make-hash-table)))
          (vlist-for-each (match-lambda
                           ((mod+name . #(is-macro? depth src))
                            (when is-macro?
                              (hash-set! macros mod+name src))))
                          defs)
          macros))
      ;; Post-process the result.
      ;; FIXME: What to do with defs at nonzero depth?
      (match info
        (($ <use-before-def-info> 0 uses defs)
         ;; The way the traversal works is that we only add entries to
         ;; `defs' as we go, corresponding to local bindings.
         ;; Therefore the result of `resolve' can only go from being an
         ;; import, unbound, or top-level definition to being a
         ;; definition within the compilation unit.  It can't go from
         ;; e.g. being an import to being a top-level definition, for
         ;; the purposes of our analysis, without the definition being
         ;; local to the compilation unit.
         (let ((macros (compute-macros defs))
               (issued-unbound-warnings (make-hash-table)))
           (for-each
            (match-lambda
             (#(mod name use-depth def-at-use use-loc)
              (cond
               ((and (hash-ref macros (cons mod name))
                     macro-use-before-definition-enabled)
                ;; Something bound to this name is a macro, probably
                ;; later in the compilation unit.  Probably the author
                ;; made a mistake somewhere!
                (warning 'macro-use-before-definition use-loc name))
               (else
                (let ((def-at-end (resolve mod name defs)))
                  (match (cons def-at-use def-at-end)
                    (('import . 'import) #t)
                    (('import . #(is-macro? def-depth def-loc))
                     ;; At use, the binding was an import, but later
                     ;; had a local definition.  Warn as this could
                     ;; pose a hazard when reloading the module, as the
                     ;; initial binding wouldn't come from the import.
                     ;; If depth nonzero though, use might happen later
                     ;; as it might be in a lambda, so no warning in
                     ;; that case.
                     (when (and non-idempotent-definition-enabled
                                (zero? use-depth) (zero? def-depth))
                       (warning 'non-idempotent-definition use-loc name)))
                    (('unbound . 'unbound)
                     ;; No binding at all; probably an error at
                     ;; run-time, but we just warn at compile-time.
                     (when unbound-variable-enabled
                       (unless (hash-ref issued-unbound-warnings
                                         (cons mod name))
                         (hash-set! issued-unbound-warnings (cons mod name) #t)
                         (warning 'unbound-variable use-loc name))))
                    (('unbound . _)
                     ;; If the depth at the use is 0, then the use
                     ;; definitely occurs before the definition.
                     (when (and use-before-definition-enabled
                                (zero? use-depth))
                       (warning 'use-before-definition use-loc name)))
                    (('unknown-module . _)
                     ;; Could issue a warning here that for whatever
                     ;; reason, we weren't able to reason about what
                     ;; module was current!
                     #t)
                    (('unknown-declarative . 'unknown-declarative)
                     ;; FIXME: Probably we should emit a warning as in
                     ;; a declarative module perhaps this should not
                     ;; happen.
                     #t)
                    (('unknown-declarative . _)
                     ;; Def later in compilation unit than use; no
                     ;; problem.  Can occur when reloading declarative
                     ;; modules.
                     #t)
                    (('unknown-imperative . _)
                     ;; Def present and although not visible at the
                     ;; use, don't warn as use module is
                     ;; non-declarative.
                     #t)
                    (((? vector) . (? vector?))
                     ;; Def locally bound at use; no problem.
                     #t)))))))
            (reverse uses))))))

    (make-use-before-def-info 0 '() vlist-null))))


;;;
;;; Arity analysis.
;;;

;; <arity-info> records contain information about lexical definitions of
;; procedures currently in scope, top-level procedure definitions that have
;; been encountered, and calls to top-level procedures that have been
;; encountered.
(define-record-type <arity-info>
  (make-arity-info toplevel-calls lexical-lambdas toplevel-lambdas)
  arity-info?
  (toplevel-calls   toplevel-procedure-calls) ;; ((NAME . CALL) ...)
  (lexical-lambdas  lexical-lambdas)          ;; ((GENSYM . DEFINITION) ...)
  (toplevel-lambdas toplevel-lambdas))        ;; ((NAME . DEFINITION) ...)

(define (validate-arity proc call lexical?)
  ;; Validate the argument count of CALL, a tree-il call of
  ;; PROC, emitting a warning in case of argument count mismatch.

  (define (filter-keyword-args keywords allow-other-keys? args)
    ;; Filter keyword arguments from ARGS and return the resulting list.
    ;; KEYWORDS is the list of allowed keywords, and ALLOW-OTHER-KEYS?
    ;; specified whethere keywords not listed in KEYWORDS are allowed.
    (let loop ((args   args)
               (result '()))
      (if (null? args)
          (reverse result)
          (let ((arg (car args)))
            (if (and (const? arg)
                     (or (memq (const-exp arg) keywords)
                         (and allow-other-keys?
                              (keyword? (const-exp arg)))))
                (loop (if (pair? (cdr args))
                          (cddr args)
                          '())
                      result)
                (loop (cdr args)
                      (cons arg result)))))))

  (define (arities proc)
    ;; Return the arities of PROC, which can be either a tree-il or a
    ;; procedure.
    (define (len x)
      (or (and (or (null? x) (pair? x))
               (length x))
          0))
    (cond ((program? proc)
           (values (procedure-name proc)
                   (map (lambda (a)
                          (list (length (or (assq-ref a 'required) '()))
                                (length (or (assq-ref a 'optional) '()))
                                (and (assq-ref a 'rest) #t)
                                (map car (or (assq-ref a 'keyword) '()))
                                (assq-ref a 'allow-other-keys?)))
                        (program-arguments-alists proc))))
          ((procedure? proc)
           (if (struct? proc)
               ;; An applicable struct.
               (arities (struct-ref proc 0))
               ;; An applicable smob.
               (let ((arity (procedure-minimum-arity proc)))
                 (values (procedure-name proc)
                         (list (list (car arity) (cadr arity) (caddr arity)
                                     #f #f))))))
          (else
           (let loop ((name    #f)
                      (proc    proc)
                      (arities '()))
             (if (not proc)
                 (values name (reverse arities))
                 (record-case proc
                   ((<lambda-case> req opt rest kw alternate)
                    (loop name alternate
                          (cons (list (len req) (len opt) rest
                                      (and (pair? kw) (map car (cdr kw)))
                                      (and (pair? kw) (car kw)))
                                arities)))
                   ((<lambda> meta body)
                    (loop (assoc-ref meta 'name) body arities))
                   (else
                    (values #f #f))))))))

  (let ((args (call-args call))
        (src  (tree-il-src call)))
    (call-with-values (lambda () (arities proc))
      (lambda (name arities)
        (define matches?
          (find (lambda (arity)
                  (pmatch arity
                    ((,req ,opt ,rest? ,kw ,aok?)
                     (let ((args (if (pair? kw)
                                     (filter-keyword-args kw aok? args)
                                     args)))
                       (if (and req opt)
                           (let ((count (length args)))
                             (and (>= count req)
                                  (or rest?
                                      (<= count (+ req opt)))))
                           #t)))
                    (else #t)))
                arities))

        (if (not matches?)
            (warning 'arity-mismatch src
                     (or name (with-output-to-string (lambda () (write proc))))
                     lexical?)))))
  #t)

(define arity-analysis
  ;; Report arity mismatches in the given tree.
  (make-tree-analysis
   (lambda (x info env locs)
     ;; Down into X.
     (define (extend lexical-name val info)
       ;; If VAL is a lambda, add NAME to the lexical-lambdas of INFO.
       (let ((toplevel-calls   (toplevel-procedure-calls info))
             (lexical-lambdas  (lexical-lambdas info))
             (toplevel-lambdas (toplevel-lambdas info)))
         (record-case val
           ((<lambda> body)
            (make-arity-info toplevel-calls
                             (vhash-consq lexical-name val
                                          lexical-lambdas)
                             toplevel-lambdas))
           ((<lexical-ref> gensym)
            ;; lexical alias
            (let ((val* (vhash-assq gensym lexical-lambdas)))
              (if (pair? val*)
                  (extend lexical-name (cdr val*) info)
                  info)))
           ((<toplevel-ref> name)
            ;; top-level alias
            (make-arity-info toplevel-calls
                             (vhash-consq lexical-name val
                                          lexical-lambdas)
                             toplevel-lambdas))
           (else info))))

     (let ((toplevel-calls   (toplevel-procedure-calls info))
           (lexical-lambdas  (lexical-lambdas info))
           (toplevel-lambdas (toplevel-lambdas info)))

       (record-case x
         ((<toplevel-define> name exp)
          (record-case exp
            ((<lambda> body)
             (make-arity-info toplevel-calls
                              lexical-lambdas
                              (vhash-consq name exp toplevel-lambdas)))
            ((<toplevel-ref> name)
             ;; alias for another toplevel
             (let ((proc (vhash-assq name toplevel-lambdas)))
               (make-arity-info toplevel-calls
                                lexical-lambdas
                                (vhash-consq (toplevel-define-name x)
                                             (if (pair? proc)
                                                 (cdr proc)
                                                 exp)
                                             toplevel-lambdas))))
            (else info)))
         ((<let> gensyms vals)
          (fold extend info gensyms vals))
         ((<letrec> gensyms vals)
          (fold extend info gensyms vals))
         ((<fix> gensyms vals)
          (fold extend info gensyms vals))

         ((<call> proc args src)
          (record-case proc
            ((<lambda> body)
             (validate-arity proc x #t)
             info)
            ((<toplevel-ref> name)
             (make-arity-info (vhash-consq name x toplevel-calls)
                              lexical-lambdas
                              toplevel-lambdas))
            ((<lexical-ref> gensym)
             (let ((proc (vhash-assq gensym lexical-lambdas)))
               (if (pair? proc)
                   (record-case (cdr proc)
                     ((<toplevel-ref> name)
                      ;; alias to toplevel
                      (make-arity-info (vhash-consq name x toplevel-calls)
                                       lexical-lambdas
                                       toplevel-lambdas))
                     (else
                      (validate-arity (cdr proc) x #t)
                      info))

                   ;; If GENSYM wasn't found, it may be because it's an
                   ;; argument of the procedure being compiled.
                   info)))
            (else info)))
         (else info))))

   (lambda (x info env locs)
     ;; Up from X.
     (define (shrink name val info)
       ;; Remove NAME from the lexical-lambdas of INFO.
       (let ((toplevel-calls   (toplevel-procedure-calls info))
             (lexical-lambdas  (lexical-lambdas info))
             (toplevel-lambdas (toplevel-lambdas info)))
         (make-arity-info toplevel-calls
                          (if (vhash-assq name lexical-lambdas)
                              (vlist-tail lexical-lambdas)
                              lexical-lambdas)
                          toplevel-lambdas)))

     (let ((toplevel-calls   (toplevel-procedure-calls info))
           (lexical-lambdas  (lexical-lambdas info))
           (toplevel-lambdas (toplevel-lambdas info)))
       (record-case x
         ((<let> gensyms vals)
          (fold shrink info gensyms vals))
         ((<letrec> gensyms vals)
          (fold shrink info gensyms vals))
         ((<fix> gensyms vals)
          (fold shrink info gensyms vals))

         (else info))))

   (lambda (result env)
     ;; Post-processing: check all top-level procedure calls that have been
     ;; encountered.
     (let ((toplevel-calls   (toplevel-procedure-calls result))
           (toplevel-lambdas (toplevel-lambdas result)))
       (vlist-for-each
        (lambda (name+call)
          (let* ((name (car name+call))
                 (call (cdr name+call))
                 (proc
                  (or (and=> (vhash-assq name toplevel-lambdas) cdr)
                      (and (module? env)
                           (false-if-exception
                            (module-ref env name)))))
                 (proc*
                  ;; handle toplevel aliases
                  (if (toplevel-ref? proc)
                      (let ((name (toplevel-ref-name proc)))
                        (and (module? env)
                             (false-if-exception
                              (module-ref env name))))
                      proc)))
            (cond ((lambda? proc*)
                   (validate-arity proc* call #t))
                  ((procedure? proc*)
                   (validate-arity proc* call #f)))))
        toplevel-calls)))

   (make-arity-info vlist-null vlist-null vlist-null)))


;;;
;;; `format' argument analysis.
;;;

(define &syntax-error
  ;; The `throw' key for syntax errors.
  (gensym "format-string-syntax-error"))

(define (format-string-argument-count fmt)
  ;; Return the minimum and maxium number of arguments that should
  ;; follow format string FMT (or, ahem, a good estimate thereof) or
  ;; `any' if the format string can be followed by any number of
  ;; arguments.

  (define (drop-group chars end)
    ;; Drop characters from CHARS until "~END" is encountered.
    (let loop ((chars  chars)
               (tilde? #f))
      (if (null? chars)
          (throw &syntax-error 'unterminated-iteration)
          (if tilde?
              (if (eq? (car chars) end)
                  (cdr chars)
                  (loop (cdr chars) #f))
              (if (eq? (car chars) #\~)
                  (loop (cdr chars) #t)
                  (loop (cdr chars) #f))))))

  (define (digit? char)
    ;; Return true if CHAR is a digit, #f otherwise.
    (memq char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

  (define (previous-number chars)
    ;; Return the previous series of digits found in CHARS.
    (let ((numbers (take-while digit? chars)))
      (and (not (null? numbers))
           (string->number (list->string (reverse numbers))))))

  (let loop ((chars       (string->list fmt))
             (state       'literal)
             (params      '())
             (conditions  '())
             (end-group   #f)
             (min-count 0)
             (max-count 0))
    (if (null? chars)
        (if end-group
            (throw &syntax-error 'unterminated-conditional)
            (values min-count max-count))
        (case state
          ((tilde)
           (case (car chars)
             ((#\~ #\% #\& #\t #\T #\_ #\newline #\( #\) #\! #\| #\/ #\q #\Q)
                        (loop (cdr chars) 'literal '()
                              conditions end-group
                              min-count max-count))
             ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\, #\: #\@ #\+ #\- #\#)
                        (loop (cdr chars)
                              'tilde (cons (car chars) params)
                              conditions end-group
                              min-count max-count))
             ((#\v #\V) (loop (cdr chars)
                              'tilde (cons (car chars) params)
                              conditions end-group
                              (+ 1 min-count)
                              (+ 1 max-count)))
             ((#\p #\P) (let* ((colon?    (memq #\: params))
                               (min-count (if colon?
                                              (max 1 min-count)
                                              (+ 1 min-count))))
                          (loop (cdr chars) 'literal '()
                                conditions end-group
                                min-count
                                (if colon?
                                    (max max-count min-count)
                                    (+ 1 max-count)))))
             ((#\[)
              (loop chars 'literal '() '()
                    (let ((selector (previous-number params))
                          (at?      (memq #\@ params)))
                      (lambda (chars conds)
                        ;; end of group
                        (let ((mins (map car conds))
                              (maxs (map cdr conds))
                              (sel? (and selector
                                         (< selector (length conds)))))
                          (if (and (every number? mins)
                                   (every number? maxs))
                              (loop chars 'literal '() conditions end-group
                                    (+ min-count
                                       (if sel?
                                           (car (list-ref conds selector))
                                           (+ (if at? 0 1)
                                              (if (null? mins)
                                                  0
                                                  (apply min mins)))))
                                    (+ max-count
                                       (if sel?
                                           (cdr (list-ref conds selector))
                                           (+ (if at? 0 1)
                                              (if (null? maxs)
                                                  0
                                                  (apply max maxs))))))
                              (values 'any 'any))))) ;; XXX: approximation
                    0 0))
             ((#\;)
              (if end-group
                  (loop (cdr chars) 'literal '()
                        (cons (cons min-count max-count) conditions)
                        end-group
                        0 0)
                  (throw &syntax-error 'unexpected-semicolon)))
             ((#\])
              (if end-group
                  (end-group (cdr chars)
                             (reverse (cons (cons min-count max-count)
                                            conditions)))
                  (throw &syntax-error 'unexpected-conditional-termination)))
             ((#\{)     (if (memq #\@ params)
                            (values min-count 'any)
                            (loop (drop-group (cdr chars) #\})
                                  'literal '()
                                  conditions end-group
                                  (+ 1 min-count) (+ 1 max-count))))
             ((#\*)     (if (memq #\@ params)
                            (values 'any 'any) ;; it's unclear what to do here
                            (loop (cdr chars)
                                  'literal '()
                                  conditions end-group
                                  (+ (or (previous-number params) 1)
                                     min-count)
                                  (+ (or (previous-number params) 1)
                                     max-count))))
             ((#\? #\k #\K)
              ;; We don't have enough info to determine the exact number
              ;; of args, but we could determine a lower bound (TODO).
              (values 'any 'any))
             ((#\^)
              (values min-count 'any))
             ((#\h #\H)
                        (let ((argc (if (memq #\: params) 2 1)))
                          (loop (cdr chars) 'literal '()
                                conditions end-group
                                (+ argc min-count)
                                (+ argc max-count))))
             ((#\')
              (if (null? (cdr chars))
                  (throw &syntax-error 'unexpected-termination)
                  (loop (cddr chars) 'tilde (cons (cadr chars) params)
                        conditions end-group min-count max-count)))
             (else      (loop (cdr chars) 'literal '()
                              conditions end-group
                              (+ 1 min-count) (+ 1 max-count)))))
          ((literal)
           (case (car chars)
             ((#\~)     (loop (cdr chars) 'tilde '()
                              conditions end-group
                              min-count max-count))
             (else      (loop (cdr chars) 'literal '()
                              conditions end-group
                              min-count max-count))))
          (else (error "computer bought the farm" state))))))

(define (proc-ref? exp proc special-name env)
  "Return #t when EXP designates procedure PROC in ENV.  As a last
resort, return #t when EXP refers to the global variable SPECIAL-NAME."

  (define special?
    (cut eq? <> special-name))

  (match exp
    (($ <toplevel-ref> _ _ (? special?))
     ;; Allow top-levels like: (define G_ (cut gettext <> "my-domain")).
     #t)
    (($ <toplevel-ref> _ _ name)
     (let ((var (module-variable env name)))
       (and var (variable-bound? var)
            (eq? (variable-ref var) proc))))
    (($ <module-ref> _ _ (? special?))
     #t)
    (($ <module-ref> _ module name public?)
     (let* ((mod (if public?
                     (false-if-exception (resolve-interface module))
                     (resolve-module module #:ensure #f)))
            (var (and mod (module-variable mod name))))
       (and var (variable-bound? var) (eq? (variable-ref var) proc))))
    (($ <lexical-ref> _ (? special?))
     #t)
    (_ #f)))

(define gettext? (cut proc-ref? <> gettext 'G_ <>))
(define ngettext? (cut proc-ref? <> ngettext 'N_ <>))

(define (const-fmt x env)
  ;; Return the literal format string for X, or #f.
  (match x
    (($ <const> _ (? string? exp))
     exp)
    (($ <call> _ (? (cut gettext? <> env))
        (($ <const> _ (? string? fmt))))
     ;; Gettexted literals, like `(G_ "foo")'.
     fmt)
    (($ <call> _ (? (cut ngettext? <> env))
        (($ <const> _ (? string? fmt)) ($ <const> _ (? string?)) _ ..1))
     ;; Plural gettextized literals, like `(N_ "singular" "plural" n)'.

     ;; TODO: Check whether the singular and plural strings have the
     ;; same format escapes.
     fmt)
    (_ #f)))

(define format-analysis
  ;; Report arity mismatches in the given tree.
  (make-tree-analysis
   (lambda (x res env locs)
     ;; Down into X.
     (define (check-format-args args loc)
       (pmatch args
         ((,port ,fmt . ,rest)
          (guard (const-fmt fmt env))
          (if (and (const? port)
                   (not (boolean? (const-exp port))))
              (warning 'format loc 'wrong-port (const-exp port)))
          (let ((fmt   (const-fmt fmt env))
                (count (length rest)))
            (catch &syntax-error
              (lambda ()
                (let-values (((min max)
                              (format-string-argument-count fmt)))
                  (and min max
                       (or (and (or (eq? min 'any) (>= count min))
                                (or (eq? max 'any) (<= count max)))
                           (warning 'format loc 'wrong-format-arg-count
                                    fmt min max count)))))
              (lambda (_ key)
                (warning 'format loc 'syntax-error key fmt)))))
         ((,port ,fmt . ,rest)
          (if (and (const? port)
                   (not (boolean? (const-exp port))))
              (warning 'format loc 'wrong-port (const-exp port)))

          (match fmt
            (($ <const> loc* (? (negate string?) fmt))
             (warning 'format (or loc* loc) 'wrong-format-string fmt))

            ;; Warn on non-literal format strings, unless they refer to
            ;; a lexical variable named "fmt".
            (($ <lexical-ref> _ fmt)
             #t)
            ((? (negate const?))
             (warning 'format loc 'non-literal-format-string))))
         (else
          (warning 'format loc 'wrong-num-args (length args)))))

     (define (check-simple-format-args args loc)
       ;; Check the arguments to the `simple-format' procedure, which is
       ;; less capable than that of (ice-9 format).

       (define allowed-chars
         '(#\A #\S #\a #\s #\~ #\%))

       (define (format-chars fmt)
         (let loop ((chars  (string->list fmt))
                    (result '()))
           (match chars
             (()
              (reverse result))
             ((#\~ opt rest ...)
              (loop rest (cons opt result)))
             ((_ rest ...)
              (loop rest result)))))

       (match args
         ((port ($ <const> _ (? string? fmt)) _ ...)
          (let ((opts (format-chars fmt)))
            (or (every (cut memq <> allowed-chars) opts)
                (begin
                  (warning 'format loc 'simple-format fmt
                           (find (negate (cut memq <> allowed-chars)) opts))
                  #f))))
         ((port (= (cut const-fmt <> env) (? string? fmt)) args ...)
          (check-simple-format-args `(,port ,(make-const loc fmt) ,args) loc))
         (_ #t)))

     (define (resolve-toplevel name)
       (and (module? env)
            (false-if-exception (module-ref env name))))

     (match x
       (($ <call> src ($ <toplevel-ref> _ _ name) args)
        (let ((proc (resolve-toplevel name)))
          (if (or (and (eq? proc (@ (guile) simple-format))
                       (check-simple-format-args args
                                                 (or src (find pair? locs))))
                  (eq? proc (@ (ice-9 format) format)))
              (check-format-args args (or src (find pair? locs))))))
       (($ <call> src ($ <module-ref> _ '(ice-9 format) 'format) args)
        (check-format-args args (or src (find pair? locs))))
       (($ <call> src ($ <module-ref> _ '(guile)
                         (or 'format 'simple-format))
           args)
        (and (check-simple-format-args args
                                       (or src (find pair? locs)))
             (check-format-args args (or src (find pair? locs)))))
       (_ #t))
     #t)

   (lambda (x _ env locs)
     ;; Up from X.
     #t)

   (lambda (_ env)
     ;; Post-processing.
     #t)

   #t))

(begin-deprecated
 (define-syntax unbound-variable-analysis
   (identifier-syntax
    (begin
      (issue-deprecation-warning
       "`unbound-variable-analysis' is deprecated.  "
       "Use `make-use-before-definition-analysis' instead.")
      (make-use-before-definition-analysis
       #:enabled-warnings '(unbound-variable)))))
 (define-syntax macro-use-before-definition-analysis
   (identifier-syntax
    (begin
      (issue-deprecation-warning
       "`macro-use-before-definition-analysis' is deprecated.  "
       "Use `make-use-before-definition-analysis' instead.")
      (make-use-before-definition-analysis
       #:enabled-warnings '(macro-use-before-definition)))))
 (export unbound-variable-analysis
         macro-use-before-definition-analysis))

(define-syntax-rule (define-analysis make-analysis
                      #:level level #:kind kind #:analysis analysis)
  (define* (make-analysis #:key (warning-level 0) (enabled-warnings '()))
    (and (or (<= level warning-level)
             (memq 'kind enabled-warnings))
         analysis)))

(define-analysis make-unused-variable-analysis
  #:level 3 #:kind unused-variable #:analysis unused-variable-analysis)
(define-analysis make-unused-toplevel-analysis
  #:level 2 #:kind unused-toplevel #:analysis unused-toplevel-analysis)
(define-analysis make-shadowed-toplevel-analysis
  #:level 2 #:kind shadowed-toplevel #:analysis shadowed-toplevel-analysis)
(define-analysis make-arity-analysis
  #:level 1 #:kind arity-mismatch #:analysis arity-analysis)
(define-analysis make-format-analysis
  #:level 1 #:kind format #:analysis format-analysis)

(define (make-analyzer warning-level warnings)
  (define-syntax compute-analyses
    (syntax-rules ()
      ((_) '())
      ((_ make-analysis . make-analysis*)
       (let ((tail (compute-analyses . make-analysis*)))
         (match (make-analysis #:warning-level warning-level
                               #:enabled-warnings warnings)
           (#f tail)
           (analysis (cons analysis tail)))))))
  (let ((analyses (compute-analyses make-unused-variable-analysis
                                    make-unused-toplevel-analysis
                                    make-shadowed-toplevel-analysis
                                    make-arity-analysis
                                    make-format-analysis
                                    make-use-before-definition-analysis)))
    (lambda (exp env)
      (analyze-tree analyses exp env))))
