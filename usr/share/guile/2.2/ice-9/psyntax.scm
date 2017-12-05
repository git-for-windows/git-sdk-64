;;;; -*-scheme-*-
;;;;
;;;; Copyright (C) 2001, 2003, 2006, 2009, 2010, 2011,
;;;;   2012, 2013, 2015, 2016 Free Software Foundation, Inc.
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


;;; Portable implementation of syntax-case
;;; Originally extracted from Chez Scheme Version 5.9f
;;; Authors: R. Kent Dybvig, Oscar Waddell, Bob Hieb, Carl Bruggeman

;;; Copyright (c) 1992-1997 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.

;;; Modified by Mikael Djurfeldt <djurfeldt@nada.kth.se> according
;;; to the ChangeLog distributed in the same directory as this file:
;;; 1997-08-19, 1997-09-03, 1997-09-10, 2000-08-13, 2000-08-24,
;;; 2000-09-12, 2001-03-08

;;; Modified by Andy Wingo <wingo@pobox.com> according to the Git
;;; revision control logs corresponding to this file: 2009, 2010.

;;; Modified by Mark H Weaver <mhw@netris.org> according to the Git
;;; revision control logs corresponding to this file: 2012, 2013.


;;; This code is based on "Syntax Abstraction in Scheme"
;;; by R. Kent Dybvig, Robert Hieb, and Carl Bruggeman.
;;; Lisp and Symbolic Computation 5:4, 295-326, 1992.
;;; <http://www.cs.indiana.edu/~dyb/pubs/LaSC-5-4-pp295-326.pdf>


;;; This file defines the syntax-case expander, macroexpand, and a set
;;; of associated syntactic forms and procedures.  Of these, the
;;; following are documented in The Scheme Programming Language,
;;; Fourth Edition (R. Kent Dybvig, MIT Press, 2009), and in the 
;;; R6RS:
;;;
;;;   bound-identifier=?
;;;   datum->syntax
;;;   define-syntax
;;;   syntax-parameterize
;;;   free-identifier=?
;;;   generate-temporaries
;;;   identifier?
;;;   identifier-syntax
;;;   let-syntax
;;;   letrec-syntax
;;;   syntax
;;;   syntax-case
;;;   syntax->datum
;;;   syntax-rules
;;;   with-syntax
;;;
;;; Additionally, the expander provides definitions for a number of core
;;; Scheme syntactic bindings, such as `let', `lambda', and the like.

;;; The remaining exports are listed below:
;;;
;;;   (macroexpand datum)
;;;      if datum represents a valid expression, macroexpand returns an
;;;      expanded version of datum in a core language that includes no
;;;      syntactic abstractions.  The core language includes begin,
;;;      define, if, lambda, letrec, quote, and set!.
;;;   (eval-when situations expr ...)
;;;      conditionally evaluates expr ... at compile-time or run-time
;;;      depending upon situations (see the Chez Scheme System Manual,
;;;      Revision 3, for a complete description)
;;;   (syntax-violation who message form [subform])
;;;      used to report errors found during expansion
;;;   ($sc-dispatch e p)
;;;      used by expanded code to handle syntax-case matching

;;; This file is shipped along with an expanded version of itself,
;;; psyntax-pp.scm, which is loaded when psyntax.scm has not yet been
;;; compiled.  In this way, psyntax bootstraps off of an expanded
;;; version of itself.

;;; This implementation of the expander sometimes uses syntactic
;;; abstractions when procedural abstractions would suffice.  For
;;; example, we define top-wrap and top-marked? as
;;;
;;;   (define-syntax top-wrap (identifier-syntax '((top))))
;;;   (define-syntax top-marked?
;;;     (syntax-rules ()
;;;       ((_ w) (memq 'top (wrap-marks w)))))
;;;
;;; rather than
;;;
;;;   (define top-wrap '((top)))
;;;   (define top-marked?
;;;     (lambda (w) (memq 'top (wrap-marks w))))
;;;
;;; On the other hand, we don't do this consistently; we define
;;; make-wrap, wrap-marks, and wrap-subst simply as
;;;
;;;   (define make-wrap cons)
;;;   (define wrap-marks car)
;;;   (define wrap-subst cdr)
;;;
;;; In Chez Scheme, the syntactic and procedural forms of these
;;; abstractions are equivalent, since the optimizer consistently
;;; integrates constants and small procedures.  This will be true of
;;; Guile as well, once we implement a proper inliner.


;;; Implementation notes:

;;; Objects with no standard print syntax, including objects containing
;;; cycles and syntax object, are allowed in quoted data as long as they
;;; are contained within a syntax form or produced by datum->syntax.
;;; Such objects are never copied.

;;; All identifiers that don't have macro definitions and are not bound
;;; lexically are assumed to be global variables.

;;; Top-level definitions of macro-introduced identifiers are allowed.
;;; This may not be appropriate for implementations in which the
;;; model is that bindings are created by definitions, as opposed to
;;; one in which initial values are assigned by definitions.

;;; Identifiers and syntax objects are implemented as vectors for
;;; portability.  As a result, it is possible to "forge" syntax objects.

;;; The implementation of generate-temporaries assumes that it is
;;; possible to generate globally unique symbols (gensyms).

;;; The source location associated with incoming expressions is tracked
;;; via the source-properties mechanism, a weak map from expression to
;;; source information. At times the source is separated from the
;;; expression; see the note below about "efficiency and confusion".


;;; Bootstrapping:

;;; When changing syntax-object representations, it is necessary to support
;;; both old and new syntax-object representations in id-var-name.  It
;;; should be sufficient to recognize old representations and treat
;;; them as not lexically bound.



(eval-when (compile)
  (set-current-module (resolve-module '(guile))))

(let ((syntax? (module-ref (current-module) 'syntax?))
      (make-syntax (module-ref (current-module) 'make-syntax))
      (syntax-expression (module-ref (current-module) 'syntax-expression))
      (syntax-wrap (module-ref (current-module) 'syntax-wrap))
      (syntax-module (module-ref (current-module) 'syntax-module)))

  (define-syntax define-expansion-constructors
    (lambda (x)
      (syntax-case x ()
        ((_)
         (let lp ((n 0) (out '()))
           (if (< n (vector-length %expanded-vtables))
               (lp (1+ n)
                   (let* ((vtable (vector-ref %expanded-vtables n))
                          (stem (struct-ref vtable (+ vtable-offset-user 0)))
                          (fields (struct-ref vtable (+ vtable-offset-user 2)))
                          (sfields (map (lambda (f) (datum->syntax x f)) fields))
                          (ctor (datum->syntax x (symbol-append 'make- stem))))
                     (cons #`(define (#,ctor #,@sfields)
                               (make-struct/no-tail
                                (vector-ref %expanded-vtables #,n)
                                #,@sfields))
                           out)))
               #`(begin #,@(reverse out))))))))

  (define-syntax define-expansion-accessors
    (lambda (x)
      (syntax-case x ()
        ((_ stem field ...)
         (let lp ((n 0))
           (let ((vtable (vector-ref %expanded-vtables n))
                 (stem (syntax->datum #'stem)))
             (if (eq? (struct-ref vtable (+ vtable-offset-user 0)) stem)
                 #`(begin
                     (define (#,(datum->syntax x (symbol-append stem '?)) x)
                       (and (struct? x)
                            (eq? (struct-vtable x)
                                 (vector-ref %expanded-vtables #,n))))
                     #,@(map
                         (lambda (f)
                           (let ((get (datum->syntax x (symbol-append stem '- f)))
                                 (set (datum->syntax x (symbol-append 'set- stem '- f '!)))
                                 (idx (list-index (struct-ref vtable
                                                              (+ vtable-offset-user 2))
                                                  f)))
                             #`(begin
                                 (define (#,get x)
                                   (struct-ref x #,idx))
                                 (define (#,set x v)
                                   (struct-set! x #,idx v)))))
                         (syntax->datum #'(field ...))))
                 (lp (1+ n)))))))))

  (define-syntax define-structure
    (lambda (x)
      (define construct-name
        (lambda (template-identifier . args)
          (datum->syntax
           template-identifier
           (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string (syntax->datum x))))
                        args))))))
      (syntax-case x ()
        ((_ (name id1 ...))
         (and-map identifier? #'(name id1 ...))
         (with-syntax
             ((constructor (construct-name #'name "make-" #'name))
              (predicate (construct-name #'name #'name "?"))
              ((access ...)
               (map (lambda (x) (construct-name x #'name "-" x))
                    #'(id1 ...)))
              ((assign ...)
               (map (lambda (x)
                      (construct-name x "set-" #'name "-" x "!"))
                    #'(id1 ...)))
              (structure-length
               (+ (length #'(id1 ...)) 1))
              ((index ...)
               (let f ((i 1) (ids #'(id1 ...)))
                 (if (null? ids)
                     '()
                     (cons i (f (+ i 1) (cdr ids)))))))
           #'(begin
               (define constructor
                 (lambda (id1 ...)
                   (vector 'name id1 ... )))
               (define predicate
                 (lambda (x)
                   (and (vector? x)
                        (= (vector-length x) structure-length)
                        (eq? (vector-ref x 0) 'name))))
               (define access
                 (lambda (x)
                   (vector-ref x index)))
               ...
               (define assign
                 (lambda (x update)
                   (vector-set! x index update)))
               ...))))))

  (let ()
    (define-expansion-constructors)
    (define-expansion-accessors lambda meta)

    ;; hooks to nonportable run-time helpers
    (begin
      (define-syntax fx+ (identifier-syntax +))
      (define-syntax fx- (identifier-syntax -))
      (define-syntax fx= (identifier-syntax =))
      (define-syntax fx< (identifier-syntax <))

      (define top-level-eval-hook
        (lambda (x mod)
          (primitive-eval x)))

      (define local-eval-hook
        (lambda (x mod)
          (primitive-eval x)))
    
      ;; Capture syntax-session-id before we shove it off into a module.
      (define session-id
        (let ((v (module-variable (current-module) 'syntax-session-id)))
          (lambda ()
            ((variable-ref v)))))

      (define put-global-definition-hook
        (lambda (symbol type val)
          (module-define! (current-module)
                          symbol
                          (make-syntax-transformer symbol type val))))
    
      (define get-global-definition-hook
        (lambda (symbol module)
          (if (and (not module) (current-module))
              (warn "module system is booted, we should have a module" symbol))
          (and (not (equal? module '(primitive)))
               (let ((v (module-variable (if module
                                             (resolve-module (cdr module))
                                             (current-module))
                                         symbol)))
                 (and v (variable-bound? v)
                      (let ((val (variable-ref v)))
                        (and (macro? val) (macro-type val)
                             (cons (macro-type val)
                                   (macro-binding val))))))))))


    (define (decorate-source e s)
      (if (and s (supports-source-properties? e))
          (set-source-properties! e s))
      e)

    (define (maybe-name-value! name val)
      (if (lambda? val)
          (let ((meta (lambda-meta val)))
            (if (not (assq 'name meta))
                (set-lambda-meta! val (acons 'name name meta))))))

    ;; output constructors
    (define build-void
      (lambda (source)
        (make-void source)))

    (define build-call
      (lambda (source fun-exp arg-exps)
        (make-call source fun-exp arg-exps)))
  
    (define build-conditional
      (lambda (source test-exp then-exp else-exp)
        (make-conditional source test-exp then-exp else-exp)))
  
    (define build-lexical-reference
      (lambda (type source name var)
        (make-lexical-ref source name var)))
  
    (define build-lexical-assignment
      (lambda (source name var exp)
        (maybe-name-value! name exp)
        (make-lexical-set source name var exp)))
  
    (define (analyze-variable mod var modref-cont bare-cont)
      (if (not mod)
          (bare-cont var)
          (let ((kind (car mod))
                (mod (cdr mod)))
            (case kind
              ((public) (modref-cont mod var #t))
              ((private) (if (not (equal? mod (module-name (current-module))))
                             (modref-cont mod var #f)
                             (bare-cont var)))
              ((bare) (bare-cont var))
              ((hygiene) (if (and (not (equal? mod (module-name (current-module))))
                                  (module-variable (resolve-module mod) var))
                             (modref-cont mod var #f)
                             (bare-cont var)))
              ((primitive)
               (syntax-violation #f "primitive not in operator position" var))
              (else (syntax-violation #f "bad module kind" var mod))))))

    (define build-global-reference
      (lambda (source var mod)
        (analyze-variable
         mod var
         (lambda (mod var public?) 
           (make-module-ref source mod var public?))
         (lambda (var)
           (make-toplevel-ref source var)))))

    (define build-global-assignment
      (lambda (source var exp mod)
        (maybe-name-value! var exp)
        (analyze-variable
         mod var
         (lambda (mod var public?) 
           (make-module-set source mod var public? exp))
         (lambda (var)
           (make-toplevel-set source var exp)))))

    (define build-global-definition
      (lambda (source var exp)
        (maybe-name-value! var exp)
        (make-toplevel-define source var exp)))

    (define build-simple-lambda
      (lambda (src req rest vars meta exp)
        (make-lambda src
                     meta
                     ;; hah, a case in which kwargs would be nice.
                     (make-lambda-case
                      ;; src req opt rest kw inits vars body else
                      src req #f rest #f '() vars exp #f))))

    (define build-case-lambda
      (lambda (src meta body)
        (make-lambda src meta body)))

    (define build-lambda-case
      ;; req := (name ...)
      ;; opt := (name ...) | #f
      ;; rest := name | #f
      ;; kw := (allow-other-keys? (keyword name var) ...) | #f
      ;; inits: (init ...)
      ;; vars: (sym ...)
      ;; vars map to named arguments in the following order:
      ;;  required, optional (positional), rest, keyword.
      ;; the body of a lambda: anything, already expanded
      ;; else: lambda-case | #f
      (lambda (src req opt rest kw inits vars body else-case)
        (make-lambda-case src req opt rest kw inits vars body else-case)))

    (define build-primcall
      (lambda (src name args)
        (make-primcall src name args)))
    
    (define build-primref
      (lambda (src name)
        (make-primitive-ref src name)))
    
    (define (build-data src exp)
      (make-const src exp))

    (define build-sequence
      (lambda (src exps)
        (if (null? (cdr exps))
            (car exps)
            (make-seq src (car exps) (build-sequence #f (cdr exps))))))

    (define build-let
      (lambda (src ids vars val-exps body-exp)
        (for-each maybe-name-value! ids val-exps)
        (if (null? vars)
            body-exp
            (make-let src ids vars val-exps body-exp))))

    (define build-named-let
      (lambda (src ids vars val-exps body-exp)
        (let ((f (car vars))
              (f-name (car ids))
              (vars (cdr vars))
              (ids (cdr ids)))
          (let ((proc (build-simple-lambda src ids #f vars '() body-exp)))
            (maybe-name-value! f-name proc)
            (for-each maybe-name-value! ids val-exps)
            (make-letrec
             src #f
             (list f-name) (list f) (list proc)
             (build-call src (build-lexical-reference 'fun src f-name f)
                         val-exps))))))

    (define build-letrec
      (lambda (src in-order? ids vars val-exps body-exp)
        (if (null? vars)
            body-exp
            (begin
              (for-each maybe-name-value! ids val-exps)
              (make-letrec src in-order? ids vars val-exps body-exp)))))


    (define-syntax-rule (build-lexical-var src id)
      ;; Use a per-module counter instead of the global counter of
      ;; 'gensym' so that the generated identifier is reproducible.
      (module-gensym (symbol->string id)))

    (define (syntax-object? x)
      (or (syntax? x)
          (and (allow-legacy-syntax-objects?)
               (vector? x)
               (= (vector-length x) 4)
               (eqv? (vector-ref x 0) 'syntax-object))))
    (define (make-syntax-object expression wrap module)
      (make-syntax expression wrap module))
    (define (syntax-object-expression obj)
      (if (syntax? obj)
          (syntax-expression obj)
          (vector-ref obj 1)))
    (define (syntax-object-wrap obj)
      (if (syntax? obj)
          (syntax-wrap obj)
          (vector-ref obj 2)))
    (define (syntax-object-module obj)
      (if (syntax? obj)
          (syntax-module obj)
          (vector-ref obj 3)))

    (define-syntax no-source (identifier-syntax #f))

    (define source-annotation
      (lambda (x)
        (let ((props (source-properties
                      (if (syntax-object? x)
                          (syntax-object-expression x)
                          x))))
          (and (pair? props) props))))

    (define-syntax-rule (arg-check pred? e who)
      (let ((x e))
        (if (not (pred? x)) (syntax-violation who "invalid argument" x))))

    ;; compile-time environments

    ;; wrap and environment comprise two level mapping.
    ;;   wrap : id --> label
    ;;   env : label --> <element>

    ;; environments are represented in two parts: a lexical part and a global
    ;; part.  The lexical part is a simple list of associations from labels
    ;; to bindings.  The global part is implemented by
    ;; {put,get}-global-definition-hook and associates symbols with
    ;; bindings.

    ;; global (assumed global variable) and displaced-lexical (see below)
    ;; do not show up in any environment; instead, they are fabricated by
    ;; resolve-identifier when it finds no other bindings.

    ;; <environment>              ::= ((<label> . <binding>)*)

    ;; identifier bindings include a type and a value

    ;; <binding> ::= (macro . <procedure>)           macros
    ;;               (syntax-parameter . (<procedure>)) syntax parameters
    ;;               (core . <procedure>)            core forms
    ;;               (module-ref . <procedure>)      @ or @@
    ;;               (begin)                         begin
    ;;               (define)                        define
    ;;               (define-syntax)                 define-syntax
    ;;               (define-syntax-parameter)       define-syntax-parameter
    ;;               (local-syntax . rec?)           let-syntax/letrec-syntax
    ;;               (eval-when)                     eval-when
    ;;               (syntax . (<var> . <level>))    pattern variables
    ;;               (global)                        assumed global variable
    ;;               (lexical . <var>)               lexical variables
    ;;               (ellipsis . <identifier>)       custom ellipsis
    ;;               (displaced-lexical)             displaced lexicals
    ;; <level>   ::= <nonnegative integer>
    ;; <var>     ::= variable returned by build-lexical-var

    ;; a macro is a user-defined syntactic-form.  a core is a
    ;; system-defined syntactic form.  begin, define, define-syntax,
    ;; define-syntax-parameter, and eval-when are treated specially
    ;; since they are sensitive to whether the form is at top-level and
    ;; (except for eval-when) can denote valid internal definitions.

    ;; a pattern variable is a variable introduced by syntax-case and can
    ;; be referenced only within a syntax form.

    ;; any identifier for which no top-level syntax definition or local
    ;; binding of any kind has been seen is assumed to be a global
    ;; variable.

    ;; a lexical variable is a lambda- or letrec-bound variable.

    ;; an ellipsis binding is introduced by the 'with-ellipsis' special
    ;; form.

    ;; a displaced-lexical identifier is a lexical identifier removed from
    ;; it's scope by the return of a syntax object containing the identifier.
    ;; a displaced lexical can also appear when a letrec-syntax-bound
    ;; keyword is referenced on the rhs of one of the letrec-syntax clauses.
    ;; a displaced lexical should never occur with properly written macros.

    (define-syntax make-binding
      (syntax-rules (quote)
        ((_ type value) (cons type value))
        ((_ 'type) '(type))
        ((_ type) (cons type '()))))
    (define-syntax-rule (binding-type x)
      (car x))
    (define-syntax-rule (binding-value x)
      (cdr x))

    (define-syntax null-env (identifier-syntax '()))

    (define extend-env
      (lambda (labels bindings r) 
        (if (null? labels)
            r
            (extend-env (cdr labels) (cdr bindings)
                        (cons (cons (car labels) (car bindings)) r)))))

    (define extend-var-env
      ;; variant of extend-env that forms "lexical" binding
      (lambda (labels vars r)
        (if (null? labels)
            r
            (extend-var-env (cdr labels) (cdr vars)
                            (cons (cons (car labels) (make-binding 'lexical (car vars))) r)))))

    ;; we use a "macros only" environment in expansion of local macro
    ;; definitions so that their definitions can use local macros without
    ;; attempting to use other lexical identifiers.
    (define macros-only-env
      (lambda (r)
        (if (null? r)
            '()
            (let ((a (car r)))
              (if (memq (cadr a) '(macro syntax-parameter ellipsis))
                  (cons a (macros-only-env (cdr r)))
                  (macros-only-env (cdr r)))))))

    (define global-extend
      (lambda (type sym val)
        (put-global-definition-hook sym type val)))


    ;; Conceptually, identifiers are always syntax objects.  Internally,
    ;; however, the wrap is sometimes maintained separately (a source of
    ;; efficiency and confusion), so that symbols are also considered
    ;; identifiers by id?.  Externally, they are always wrapped.

    (define nonsymbol-id?
      (lambda (x)
        (and (syntax-object? x)
             (symbol? (syntax-object-expression x)))))

    (define id?
      (lambda (x)
        (cond
         ((symbol? x) #t)
         ((syntax-object? x) (symbol? (syntax-object-expression x)))
         (else #f))))

    (define-syntax-rule (id-sym-name e)
      (let ((x e))
        (if (syntax-object? x)
            (syntax-object-expression x)
            x)))

    (define id-sym-name&marks
      (lambda (x w)
        (if (syntax-object? x)
            (values
             (syntax-object-expression x)
             (join-marks (wrap-marks w) (wrap-marks (syntax-object-wrap x))))
            (values x (wrap-marks w)))))

    ;; syntax object wraps

    ;;      <wrap> ::= ((<mark> ...) . (<subst> ...))
    ;;     <subst> ::= shift | <subs>
    ;;      <subs> ::= #(ribcage #(<sym> ...) #(<mark> ...) #(<label> ...))
    ;;                 | #(ribcage (<sym> ...) (<mark> ...) (<label> ...))

    (define-syntax make-wrap (identifier-syntax cons))
    (define-syntax wrap-marks (identifier-syntax car))
    (define-syntax wrap-subst (identifier-syntax cdr))

    ;; labels must be comparable with "eq?", have read-write invariance,
    ;; and distinct from symbols.
    (define (gen-label)
      (symbol->string (module-gensym "l")))

    (define gen-labels
      (lambda (ls)
        (if (null? ls)
            '()
            (cons (gen-label) (gen-labels (cdr ls))))))

    (define-structure (ribcage symnames marks labels))

    (define-syntax empty-wrap (identifier-syntax '(())))

    (define-syntax top-wrap (identifier-syntax '((top))))

    (define-syntax-rule (top-marked? w)
      (memq 'top (wrap-marks w)))

    ;; Marks must be comparable with "eq?" and distinct from pairs and
    ;; the symbol top.  We do not use integers so that marks will remain
    ;; unique even across file compiles.

    (define-syntax the-anti-mark (identifier-syntax #f))

    (define anti-mark
      (lambda (w)
        (make-wrap (cons the-anti-mark (wrap-marks w))
                   (cons 'shift (wrap-subst w)))))

    (define-syntax-rule (new-mark)
      (module-gensym "m"))

    ;; make-empty-ribcage and extend-ribcage maintain list-based ribcages for
    ;; internal definitions, in which the ribcages are built incrementally
    (define-syntax-rule (make-empty-ribcage)
      (make-ribcage '() '() '()))

    (define extend-ribcage!
      ;; must receive ids with complete wraps
      (lambda (ribcage id label)
        (set-ribcage-symnames! ribcage
                               (cons (syntax-object-expression id)
                                     (ribcage-symnames ribcage)))
        (set-ribcage-marks! ribcage
                            (cons (wrap-marks (syntax-object-wrap id))
                                  (ribcage-marks ribcage)))
        (set-ribcage-labels! ribcage
                             (cons label (ribcage-labels ribcage)))))

    ;; make-binding-wrap creates vector-based ribcages
    (define make-binding-wrap
      (lambda (ids labels w)
        (if (null? ids)
            w
            (make-wrap
             (wrap-marks w)
             (cons
              (let ((labelvec (list->vector labels)))
                (let ((n (vector-length labelvec)))
                  (let ((symnamevec (make-vector n)) (marksvec (make-vector n)))
                    (let f ((ids ids) (i 0))
                      (if (not (null? ids))
                          (call-with-values
                              (lambda () (id-sym-name&marks (car ids) w))
                            (lambda (symname marks)
                              (vector-set! symnamevec i symname)
                              (vector-set! marksvec i marks)
                              (f (cdr ids) (fx+ i 1))))))
                    (make-ribcage symnamevec marksvec labelvec))))
              (wrap-subst w))))))

    (define smart-append
      (lambda (m1 m2)
        (if (null? m2)
            m1
            (append m1 m2))))

    (define join-wraps
      (lambda (w1 w2)
        (let ((m1 (wrap-marks w1)) (s1 (wrap-subst w1)))
          (if (null? m1)
              (if (null? s1)
                  w2
                  (make-wrap
                   (wrap-marks w2)
                   (smart-append s1 (wrap-subst w2))))
              (make-wrap
               (smart-append m1 (wrap-marks w2))
               (smart-append s1 (wrap-subst w2)))))))

    (define join-marks
      (lambda (m1 m2)
        (smart-append m1 m2)))

    (define same-marks?
      (lambda (x y)
        (or (eq? x y)
            (and (not (null? x))
                 (not (null? y))
                 (eq? (car x) (car y))
                 (same-marks? (cdr x) (cdr y))))))

    (define id-var-name
      ;; Syntax objects use wraps to associate names with marked
      ;; identifiers.  This function returns the name corresponding to
      ;; the given identifier and wrap, or the original identifier if no
      ;; corresponding name was found.
      ;;
      ;; The name may be a string created by gen-label, indicating a
      ;; lexical binding, or another syntax object, indicating a
      ;; reference to a top-level definition created during a previous
      ;; macroexpansion.
      ;;
      ;; For lexical variables, finding a label simply amounts to
      ;; looking for an entry with the same symbolic name and the same
      ;; marks.  Finding a toplevel definition is the same, except we
      ;; also have to compare modules, hence the `mod' parameter.
      ;; Instead of adding a separate entry in the ribcage for modules,
      ;; which wouldn't be used for lexicals, we arrange for the entry
      ;; for the name entry to be a pair with the module in its car, and
      ;; the name itself in the cdr.  So if the name that we find is a
      ;; pair, we have to check modules.
      ;;
      ;; The identifer may be passed in wrapped or unwrapped.  In any
      ;; case, this routine returns either a symbol, a syntax object, or
      ;; a string label.
      ;;
      (lambda (id w mod)
        (define-syntax-rule (first e)
          ;; Rely on Guile's multiple-values truncation.
          e)
        (define search
          (lambda (sym subst marks mod)
            (if (null? subst)
                (values #f marks)
                (let ((fst (car subst)))
                  (if (eq? fst 'shift)
                      (search sym (cdr subst) (cdr marks) mod)
                      (let ((symnames (ribcage-symnames fst)))
                        (if (vector? symnames)
                            (search-vector-rib sym subst marks symnames fst mod)
                            (search-list-rib sym subst marks symnames fst mod))))))))
        (define search-list-rib
          (lambda (sym subst marks symnames ribcage mod)
            (let f ((symnames symnames) (i 0))
              (cond
               ((null? symnames) (search sym (cdr subst) marks mod))
               ((and (eq? (car symnames) sym)
                     (same-marks? marks (list-ref (ribcage-marks ribcage) i)))
                (let ((n (list-ref (ribcage-labels ribcage) i)))
                  (if (pair? n)
                      (if (equal? mod (car n))
                          (values (cdr n) marks)
                          (f (cdr symnames) (fx+ i 1)))
                      (values n marks))))
               (else (f (cdr symnames) (fx+ i 1)))))))
        (define search-vector-rib
          (lambda (sym subst marks symnames ribcage mod)
            (let ((n (vector-length symnames)))
              (let f ((i 0))
                (cond
                 ((fx= i n) (search sym (cdr subst) marks mod))
                 ((and (eq? (vector-ref symnames i) sym)
                       (same-marks? marks (vector-ref (ribcage-marks ribcage) i)))
                  (let ((n (vector-ref (ribcage-labels ribcage) i)))
                    (if (pair? n)
                        (if (equal? mod (car n))
                            (values (cdr n) marks)
                            (f (fx+ i 1)))
                        (values n marks))))
                 (else (f (fx+ i 1))))))))
        (cond
         ((symbol? id)
          (or (first (search id (wrap-subst w) (wrap-marks w) mod)) id))
         ((syntax-object? id)
          (let ((id (syntax-object-expression id))
                (w1 (syntax-object-wrap id))
                (mod (syntax-object-module id)))
            (let ((marks (join-marks (wrap-marks w) (wrap-marks w1))))
              (call-with-values (lambda () (search id (wrap-subst w) marks mod))
                (lambda (new-id marks)
                  (or new-id
                      (first (search id (wrap-subst w1) marks mod))
                      id))))))
         (else (syntax-violation 'id-var-name "invalid id" id)))))

    ;; A helper procedure for syntax-locally-bound-identifiers, which
    ;; itself is a helper for transformer procedures.
    ;; `locally-bound-identifiers' returns a list of all bindings
    ;; visible to a syntax object with the given wrap.  They are in
    ;; order from outer to inner.
    ;;
    ;; The purpose of this procedure is to give a transformer procedure
    ;; references on bound identifiers, that the transformer can then
    ;; introduce some of them in its output.  As such, the identifiers
    ;; are anti-marked, so that rebuild-macro-output doesn't apply new
    ;; marks to them.
    ;;
    (define locally-bound-identifiers
      (lambda (w mod)
        (define scan
          (lambda (subst results)
            (if (null? subst)
                results
                (let ((fst (car subst)))
                  (if (eq? fst 'shift)
                      (scan (cdr subst) results)
                      (let ((symnames (ribcage-symnames fst))
                            (marks (ribcage-marks fst)))
                        (if (vector? symnames)
                            (scan-vector-rib subst symnames marks results)
                            (scan-list-rib subst symnames marks results))))))))
        (define scan-list-rib
          (lambda (subst symnames marks results)
            (let f ((symnames symnames) (marks marks) (results results))
              (if (null? symnames)
                  (scan (cdr subst) results)
                  (f (cdr symnames) (cdr marks)
                     (cons (wrap (car symnames)
                                 (anti-mark (make-wrap (car marks) subst))
                                 mod)
                           results))))))
        (define scan-vector-rib
          (lambda (subst symnames marks results)
            (let ((n (vector-length symnames)))
              (let f ((i 0) (results results))
                (if (fx= i n)
                    (scan (cdr subst) results)
                    (f (fx+ i 1)
                       (cons (wrap (vector-ref symnames i)
                                   (anti-mark (make-wrap (vector-ref marks i) subst))
                                   mod)
                             results)))))))
        (scan (wrap-subst w) '())))

    ;; Returns three values: binding type, binding value, the module (for
    ;; resolving toplevel vars).
    (define (resolve-identifier id w r mod resolve-syntax-parameters?)
      (define (resolve-syntax-parameters b)
        (if (and resolve-syntax-parameters?
                 (eq? (binding-type b) 'syntax-parameter))
            (or (assq-ref r (binding-value b))
                (make-binding 'macro (car (binding-value b))))
            b))
      (define (resolve-global var mod)
        (let ((b (resolve-syntax-parameters
                  (or (get-global-definition-hook var mod)
                      (make-binding 'global)))))
          (if (eq? (binding-type b) 'global)
              (values 'global var mod)
              (values (binding-type b) (binding-value b) mod))))
      (define (resolve-lexical label mod)
        (let ((b (resolve-syntax-parameters
                  (or (assq-ref r label)
                      (make-binding 'displaced-lexical)))))
          (values (binding-type b) (binding-value b) mod)))
      (let ((n (id-var-name id w mod)))
        (cond
         ((syntax-object? n)
          (cond
           ((not (eq? n id))
            ;; This identifier aliased another; recurse to allow
            ;; syntax-parameterize to override macro-introduced syntax
            ;; parameters.
            (resolve-identifier n w r mod resolve-syntax-parameters?))
           (else
            ;; Resolved to a free variable that was introduced by this
            ;; macro; continue to resolve this global by name.
            (resolve-identifier (syntax-object-expression n)
                                (syntax-object-wrap n)
                                r
                                (syntax-object-module n)
                                resolve-syntax-parameters?))))
         ((symbol? n)
          (resolve-global n (if (syntax-object? id)
                                (syntax-object-module id)
                                mod)))
         ((string? n)
          (resolve-lexical n (if (syntax-object? id)
                                 (syntax-object-module id)
                                 mod)))
         (else
          (error "unexpected id-var-name" id w n)))))

    (define transformer-environment
      (make-fluid
       (lambda (k)
         (error "called outside the dynamic extent of a syntax transformer"))))

    (define (with-transformer-environment k)
      ((fluid-ref transformer-environment) k))

    ;; free-id=? must be passed fully wrapped ids since (free-id=? x y)
    ;; may be true even if (free-id=? (wrap x w) (wrap y w)) is not.

    (define free-id=?
      (lambda (i j)
        (let* ((mi (and (syntax-object? i) (syntax-object-module i)))
               (mj (and (syntax-object? j) (syntax-object-module j)))
               (ni (id-var-name i empty-wrap mi))
               (nj (id-var-name j empty-wrap mj)))
          (define (id-module-binding id mod)
            (module-variable
             (if mod
                 ;; The normal case.
                 (resolve-module (cdr mod))
                 ;; Either modules have not been booted, or we have a
                 ;; raw symbol coming in, which is possible.
                 (current-module))
             (id-sym-name id)))
          (cond
           ((syntax-object? ni) (free-id=? ni j))
           ((syntax-object? nj) (free-id=? i nj))
           ((symbol? ni)
            ;; `i' is not lexically bound.  Assert that `j' is free,
            ;; and if so, compare their bindings, that they are either
            ;; bound to the same variable, or both unbound and have
            ;; the same name.
            (and (eq? nj (id-sym-name j))
                 (let ((bi (id-module-binding i mi)))
                   (if bi
                       (eq? bi (id-module-binding j mj))
                       (and (not (id-module-binding j mj))
                            (eq? ni nj))))
                 (eq? (id-module-binding i mi) (id-module-binding j mj))))
           (else
            ;; Otherwise `i' is bound, so check that `j' is bound, and
            ;; bound to the same thing.
            (equal? ni nj))))))
    
    ;; bound-id=? may be passed unwrapped (or partially wrapped) ids as
    ;; long as the missing portion of the wrap is common to both of the ids
    ;; since (bound-id=? x y) iff (bound-id=? (wrap x w) (wrap y w))

    (define bound-id=?
      (lambda (i j)
        (if (and (syntax-object? i) (syntax-object? j))
            (and (eq? (syntax-object-expression i)
                      (syntax-object-expression j))
                 (same-marks? (wrap-marks (syntax-object-wrap i))
                              (wrap-marks (syntax-object-wrap j))))
            (eq? i j))))

    ;; "valid-bound-ids?" returns #t if it receives a list of distinct ids.
    ;; valid-bound-ids? may be passed unwrapped (or partially wrapped) ids
    ;; as long as the missing portion of the wrap is common to all of the
    ;; ids.

    (define valid-bound-ids?
      (lambda (ids)
        (and (let all-ids? ((ids ids))
               (or (null? ids)
                   (and (id? (car ids))
                        (all-ids? (cdr ids)))))
             (distinct-bound-ids? ids))))

    ;; distinct-bound-ids? expects a list of ids and returns #t if there are
    ;; no duplicates.  It is quadratic on the length of the id list; long
    ;; lists could be sorted to make it more efficient.  distinct-bound-ids?
    ;; may be passed unwrapped (or partially wrapped) ids as long as the
    ;; missing portion of the wrap is common to all of the ids.

    (define distinct-bound-ids?
      (lambda (ids)
        (let distinct? ((ids ids))
          (or (null? ids)
              (and (not (bound-id-member? (car ids) (cdr ids)))
                   (distinct? (cdr ids)))))))

    (define bound-id-member?
      (lambda (x list)
        (and (not (null? list))
             (or (bound-id=? x (car list))
                 (bound-id-member? x (cdr list))))))

    ;; wrapping expressions and identifiers

    (define wrap
      (lambda (x w defmod)
        (cond
         ((and (null? (wrap-marks w)) (null? (wrap-subst w))) x)
         ((syntax-object? x)
          (make-syntax-object
           (syntax-object-expression x)
           (join-wraps w (syntax-object-wrap x))
           (syntax-object-module x)))
         ((null? x) x)
         (else (make-syntax-object x w defmod)))))

    (define source-wrap
      (lambda (x w s defmod)
        (wrap (decorate-source x s) w defmod)))

    ;; expanding

    (define expand-sequence
      (lambda (body r w s mod)
        (build-sequence s
                        (let dobody ((body body) (r r) (w w) (mod mod))
                          (if (null? body)
                              '()
                              (let ((first (expand (car body) r w mod)))
                                (cons first (dobody (cdr body) r w mod))))))))

    ;; At top-level, we allow mixed definitions and expressions.  Like
    ;; expand-body we expand in two passes.
    ;;
    ;; First, from left to right, we expand just enough to know what
    ;; expressions are definitions, syntax definitions, and splicing
    ;; statements (`begin').  If we anything needs evaluating at
    ;; expansion-time, it is expanded directly.
    ;;
    ;; Otherwise we collect expressions to expand, in thunks, and then
    ;; expand them all at the end.  This allows all syntax expanders
    ;; visible in a toplevel sequence to be visible during the
    ;; expansions of all normal definitions and expressions in the
    ;; sequence.
    ;;
    (define expand-top-sequence
      (lambda (body r w s m esew mod)
        (let* ((r (cons '("placeholder" . (placeholder)) r))
               (ribcage (make-empty-ribcage))
               (w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))))
          (define (record-definition! id var)
            (let ((mod (cons 'hygiene (module-name (current-module)))))
              ;; Ribcages map symbol+marks to names, mostly for
              ;; resolving lexicals.  Here to add a mapping for toplevel
              ;; definitions we also need to match the module.  So, we
              ;; put it in the name instead, and make id-var-name handle
              ;; the special case of names that are pairs.  See the
              ;; comments in id-var-name for more.
              (extend-ribcage! ribcage id
                               (cons (syntax-object-module id)
                                     (wrap var top-wrap mod)))))
          (define (macro-introduced-identifier? id)
            (not (equal? (wrap-marks (syntax-object-wrap id)) '(top))))
          (define (fresh-derived-name id orig-form)
            (symbol-append
             (syntax-object-expression id)
             '-
             (string->symbol
              ;; FIXME: `hash' currently stops descending into nested
              ;; data at some point, so it's less unique than we would
              ;; like.  Also this encodes hash values into the ABI of
              ;; compiled modules; a problem?
              (number->string
               (hash (syntax->datum orig-form) most-positive-fixnum)
               16))))
          (define (parse body r w s m esew mod)
            (let lp ((body body) (exps '()))
              (if (null? body)
                  exps
                  (lp (cdr body)
                      (append (parse1 (car body) r w s m esew mod)
                              exps)))))
          (define (parse1 x r w s m esew mod)
            (define (current-module-for-expansion mod)
              (case (car mod)
                ;; If the module was just put in place for hygiene, in a
                ;; top-level `begin' always recapture the current
                ;; module.  If a user wants to override, then we need to
                ;; use @@ or similar.
                ((hygiene) (cons 'hygiene (module-name (current-module))))
                (else mod)))
            (call-with-values
                (lambda ()
                  (let ((mod (current-module-for-expansion mod)))
                    (syntax-type x r w (source-annotation x) ribcage mod #f)))
              (lambda (type value form e w s mod)
                (case type
                  ((define-form)
                   (let* ((id (wrap value w mod))
                          (label (gen-label))
                          (var (if (macro-introduced-identifier? id)
                                   (fresh-derived-name id x)
                                   (syntax-object-expression id))))
                     (record-definition! id var)
                     (list
                      (if (eq? m 'c&e)
                          (let ((x (build-global-definition s var (expand e r w mod))))
                            (top-level-eval-hook x mod)
                            (lambda () x))
                          (call-with-values
                              (lambda () (resolve-identifier id empty-wrap r mod #t))
                            (lambda (type* value* mod*)
                              ;; If the identifier to be bound is currently bound to a
                              ;; macro, then immediately discard that binding.
                              (if (eq? type* 'macro)
                                  (top-level-eval-hook (build-global-definition
                                                        s var (build-void s))
                                                       mod))
                              (lambda ()
                                (build-global-definition s var (expand e r w mod)))))))))
                  ((define-syntax-form define-syntax-parameter-form)
                   (let* ((id (wrap value w mod))
                          (label (gen-label))
                          (var (if (macro-introduced-identifier? id)
                                   (fresh-derived-name id x)
                                   (syntax-object-expression id))))
                     (record-definition! id var)
                     (case m
                       ((c)
                        (cond
                         ((memq 'compile esew)
                          (let ((e (expand-install-global var type (expand e r w mod))))
                            (top-level-eval-hook e mod)
                            (if (memq 'load esew)
                                (list (lambda () e))
                                '())))
                         ((memq 'load esew)
                          (list (lambda ()
                                  (expand-install-global var type (expand e r w mod)))))
                         (else '())))
                       ((c&e)
                        (let ((e (expand-install-global var type (expand e r w mod))))
                          (top-level-eval-hook e mod)
                          (list (lambda () e))))
                       (else
                        (if (memq 'eval esew)
                            (top-level-eval-hook
                             (expand-install-global var type (expand e r w mod))
                             mod))
                        '()))))
                  ((begin-form)
                   (syntax-case e ()
                     ((_ e1 ...)
                      (parse #'(e1 ...) r w s m esew mod))))
                  ((local-syntax-form)
                   (expand-local-syntax value e r w s mod
                                        (lambda (forms r w s mod)
                                          (parse forms r w s m esew mod))))
                  ((eval-when-form)
                   (syntax-case e ()
                     ((_ (x ...) e1 e2 ...)
                      (let ((when-list (parse-when-list e #'(x ...)))
                            (body #'(e1 e2 ...)))
                        (define (recurse m esew)
                          (parse body r w s m esew mod))
                        (cond
                         ((eq? m 'e)
                          (if (memq 'eval when-list)
                              (recurse (if (memq 'expand when-list) 'c&e 'e)
                                       '(eval))
                              (begin
                                (if (memq 'expand when-list)
                                    (top-level-eval-hook
                                     (expand-top-sequence body r w s 'e '(eval) mod)
                                     mod))
                                '())))
                         ((memq 'load when-list)
                          (if (or (memq 'compile when-list)
                                  (memq 'expand when-list)
                                  (and (eq? m 'c&e) (memq 'eval when-list)))
                              (recurse 'c&e '(compile load))
                              (if (memq m '(c c&e))
                                  (recurse 'c '(load))
                                  '())))
                         ((or (memq 'compile when-list)
                              (memq 'expand when-list)
                              (and (eq? m 'c&e) (memq 'eval when-list)))
                          (top-level-eval-hook
                           (expand-top-sequence body r w s 'e '(eval) mod)
                           mod)
                          '())
                         (else
                          '()))))))
                  (else
                   (list
                    (if (eq? m 'c&e)
                        (let ((x (expand-expr type value form e r w s mod)))
                          (top-level-eval-hook x mod)
                          (lambda () x))
                        (lambda ()
                          (expand-expr type value form e r w s mod)))))))))
          (let ((exps (map (lambda (x) (x))
                           (reverse (parse body r w s m esew mod)))))
            (if (null? exps)
                (build-void s)
                (build-sequence s exps))))))
    
    (define expand-install-global
      (lambda (name type e)
        (build-global-definition
         no-source
         name
         (build-primcall
          no-source
          'make-syntax-transformer
          (if (eq? type 'define-syntax-parameter-form)
              (list (build-data no-source name)
                    (build-data no-source 'syntax-parameter)
                    (build-primcall no-source 'list (list e)))
              (list (build-data no-source name)
                    (build-data no-source 'macro)
                    e))))))
    
    (define parse-when-list
      (lambda (e when-list)
        ;; `when-list' is syntax'd version of list of situations.  We
        ;; could match these keywords lexically, via free-id=?, but then
        ;; we twingle the definition of eval-when to the bindings of
        ;; eval, load, expand, and compile, which is totally unintended.
        ;; So do a symbolic match instead.
        (let ((result (strip when-list empty-wrap)))
          (let lp ((l result))
            (if (null? l)
                result
                (if (memq (car l) '(compile load eval expand))
                    (lp (cdr l))
                    (syntax-violation 'eval-when "invalid situation" e
                                      (car l))))))))

    ;; syntax-type returns seven values: type, value, form, e, w, s, and
    ;; mod. The first two are described in the table below.
    ;;
    ;;    type                   value         explanation
    ;;    -------------------------------------------------------------------
    ;;    core                   procedure     core singleton
    ;;    core-form              procedure     core form
    ;;    module-ref             procedure     @ or @@ singleton
    ;;    lexical                name          lexical variable reference
    ;;    global                 name          global variable reference
    ;;    begin                  none          begin keyword
    ;;    define                 none          define keyword
    ;;    define-syntax          none          define-syntax keyword
    ;;    define-syntax-parameter none         define-syntax-parameter keyword
    ;;    local-syntax           rec?          letrec-syntax/let-syntax keyword
    ;;    eval-when              none          eval-when keyword
    ;;    syntax                 level         pattern variable
    ;;    displaced-lexical      none          displaced lexical identifier
    ;;    lexical-call           name          call to lexical variable
    ;;    global-call            name          call to global variable
    ;;    primitive-call         name          call to primitive
    ;;    call                   none          any other call
    ;;    begin-form             none          begin expression
    ;;    define-form            id            variable definition
    ;;    define-syntax-form     id            syntax definition
    ;;    define-syntax-parameter-form id      syntax parameter definition
    ;;    local-syntax-form      rec?          syntax definition
    ;;    eval-when-form         none          eval-when form
    ;;    constant               none          self-evaluating datum
    ;;    other                  none          anything else
    ;;
    ;; form is the entire form.  For definition forms (define-form,
    ;; define-syntax-form, and define-syntax-parameter-form), e is the
    ;; rhs expression.  For all others, e is the entire form.  w is the
    ;; wrap for both form and e.  s is the source for the entire form.
    ;; mod is the module for both form and e.
    ;;
    ;; syntax-type expands macros and unwraps as necessary to get to one
    ;; of the forms above.  It also parses definition forms, although
    ;; perhaps this should be done by the consumer.

    (define syntax-type
      (lambda (e r w s rib mod for-car?)
        (cond
         ((symbol? e)
          (call-with-values (lambda () (resolve-identifier e w r mod #t))
            (lambda (type value mod*)
              (case type
                ((macro)
                 (if for-car?
                     (values type value e e w s mod)
                     (syntax-type (expand-macro value e r w s rib mod)
                                  r empty-wrap s rib mod #f)))
                ((global)
                 ;; Toplevel definitions may resolve to bindings with
                 ;; different names or in different modules.
                 (values type value e value w s mod*))
                (else (values type value e e w s mod))))))
         ((pair? e)
          (let ((first (car e)))
            (call-with-values
                (lambda () (syntax-type first r w s rib mod #t))
              (lambda (ftype fval fform fe fw fs fmod)
                (case ftype
                  ((lexical)
                   (values 'lexical-call fval e e w s mod))
                  ((global)
                   (if (equal? fmod '(primitive))
                       (values 'primitive-call fval e e w s mod)
                       ;; If we got here via an (@@ ...) expansion, we
                       ;; need to make sure the fmod information is
                       ;; propagated back correctly -- hence this
                       ;; consing.
                       (values 'global-call (make-syntax-object fval w fmod)
                               e e w s mod)))
                  ((macro)
                   (syntax-type (expand-macro fval e r w s rib mod)
                                r empty-wrap s rib mod for-car?))
                  ((module-ref)
                   (call-with-values (lambda () (fval e r w mod))
                     (lambda (e r w s mod)
                       (syntax-type e r w s rib mod for-car?))))
                  ((core)
                   (values 'core-form fval e e w s mod))
                  ((local-syntax)
                   (values 'local-syntax-form fval e e w s mod))
                  ((begin)
                   (values 'begin-form #f e e w s mod))
                  ((eval-when)
                   (values 'eval-when-form #f e e w s mod))
                  ((define)
                   (syntax-case e ()
                     ((_ name val)
                      (id? #'name)
                      (values 'define-form #'name e #'val w s mod))
                     ((_ (name . args) e1 e2 ...)
                      (and (id? #'name)
                           (valid-bound-ids? (lambda-var-list #'args)))
                      ;; need lambda here...
                      (values 'define-form (wrap #'name w mod)
                              (wrap e w mod)
                              (decorate-source
                               (cons #'lambda (wrap #'(args e1 e2 ...) w mod))
                               s)
                              empty-wrap s mod))
                     ((_ name)
                      (id? #'name)
                      (values 'define-form (wrap #'name w mod)
                              (wrap e w mod)
                              #'(if #f #f)
                              empty-wrap s mod))))
                  ((define-syntax)
                   (syntax-case e ()
                     ((_ name val)
                      (id? #'name)
                      (values 'define-syntax-form #'name e #'val w s mod))))
                  ((define-syntax-parameter)
                   (syntax-case e ()
                     ((_ name val)
                      (id? #'name)
                      (values 'define-syntax-parameter-form #'name e #'val w s mod))))
                  (else
                   (values 'call #f e e w s mod)))))))
         ((syntax-object? e)
          (syntax-type (syntax-object-expression e)
                       r
                       (join-wraps w (syntax-object-wrap e))
                       (or (source-annotation e) s) rib
                       (or (syntax-object-module e) mod) for-car?))
         ((self-evaluating? e) (values 'constant #f e e w s mod))
         (else (values 'other #f e e w s mod)))))

    (define expand
      (lambda (e r w mod)
        (call-with-values
            (lambda () (syntax-type e r w (source-annotation e) #f mod #f))
          (lambda (type value form e w s mod)
            (expand-expr type value form e r w s mod)))))

    (define expand-expr
      (lambda (type value form e r w s mod)
        (case type
          ((lexical)
           (build-lexical-reference 'value s e value))
          ((core core-form)
           ;; apply transformer
           (value e r w s mod))
          ((module-ref)
           (call-with-values (lambda () (value e r w mod))
             (lambda (e r w s mod)
               (expand e r w mod))))
          ((lexical-call)
           (expand-call
            (let ((id (car e)))
              (build-lexical-reference 'fun (source-annotation id)
                                       (if (syntax-object? id)
                                           (syntax->datum id)
                                           id)
                                       value))
            e r w s mod))
          ((global-call)
           (expand-call
            (build-global-reference (source-annotation (car e))
                                    (if (syntax-object? value)
                                        (syntax-object-expression value)
                                        value)
                                    (if (syntax-object? value)
                                        (syntax-object-module value)
                                        mod))
            e r w s mod))
          ((primitive-call)
           (syntax-case e ()
             ((_ e ...)
              (build-primcall s
                              value
                              (map (lambda (e) (expand e r w mod))
                                   #'(e ...))))))
          ((constant) (build-data s (strip (source-wrap e w s mod) empty-wrap)))
          ((global) (build-global-reference s value mod))
          ((call) (expand-call (expand (car e) r w mod) e r w s mod))
          ((begin-form)
           (syntax-case e ()
             ((_ e1 e2 ...) (expand-sequence #'(e1 e2 ...) r w s mod))
             ((_)
              (syntax-violation #f "sequence of zero expressions"
                                (source-wrap e w s mod)))))
          ((local-syntax-form)
           (expand-local-syntax value e r w s mod expand-sequence))
          ((eval-when-form)
           (syntax-case e ()
             ((_ (x ...) e1 e2 ...)
              (let ((when-list (parse-when-list e #'(x ...))))
                (if (memq 'eval when-list)
                    (expand-sequence #'(e1 e2 ...) r w s mod)
                    (expand-void))))))
          ((define-form define-syntax-form define-syntax-parameter-form)
           (syntax-violation #f "definition in expression context, where definitions are not allowed,"
                             (source-wrap form w s mod)))
          ((syntax)
           (syntax-violation #f "reference to pattern variable outside syntax form"
                             (source-wrap e w s mod)))
          ((displaced-lexical)
           (syntax-violation #f "reference to identifier outside its scope"
                             (source-wrap e w s mod)))
          (else (syntax-violation #f "unexpected syntax"
                                  (source-wrap e w s mod))))))

    (define expand-call
      (lambda (x e r w s mod)
        (syntax-case e ()
          ((e0 e1 ...)
           (build-call s x
                       (map (lambda (e) (expand e r w mod)) #'(e1 ...)))))))

    ;; (What follows is my interpretation of what's going on here -- Andy)
    ;;
    ;; A macro takes an expression, a tree, the leaves of which are identifiers
    ;; and datums. Identifiers are symbols along with a wrap and a module. For
    ;; efficiency, subtrees that share wraps and modules may be grouped as one
    ;; syntax object.
    ;;
    ;; Going into the expansion, the expression is given an anti-mark, which
    ;; logically propagates to all leaves. Then, in the new expression returned
    ;; from the transfomer, if we see an expression with an anti-mark, we know it
    ;; pertains to the original expression; conversely, expressions without the
    ;; anti-mark are known to be introduced by the transformer.
    ;;
    ;; OK, good until now. We know this algorithm does lexical scoping
    ;; appropriately because it's widely known in the literature, and psyntax is
    ;; widely used. But what about modules? Here we're on our own. What we do is
    ;; to mark the module of expressions produced by a macro as pertaining to the
    ;; module that was current when the macro was defined -- that is, free
    ;; identifiers introduced by a macro are scoped in the macro's module, not in
    ;; the expansion's module. Seems to work well.
    ;;
    ;; The only wrinkle is when we want a macro to expand to code in another
    ;; module, as is the case for the r6rs `library' form -- the body expressions
    ;; should be scoped relative the the new module, the one defined by the macro.
    ;; For that, use `(@@ mod-name body)'.
    ;;
    ;; Part of the macro output will be from the site of the macro use and part
    ;; from the macro definition. We allow source information from the macro use
    ;; to pass through, but we annotate the parts coming from the macro with the
    ;; source location information corresponding to the macro use. It would be
    ;; really nice if we could also annotate introduced expressions with the
    ;; locations corresponding to the macro definition, but that is not yet
    ;; possible.
    (define expand-macro
      (lambda (p e r w s rib mod)
        (define rebuild-macro-output
          (lambda (x m)
            (cond ((pair? x)
                   (decorate-source 
                    (cons (rebuild-macro-output (car x) m)
                          (rebuild-macro-output (cdr x) m))
                    s))
                  ((syntax-object? x)
                   (let ((w (syntax-object-wrap x)))
                     (let ((ms (wrap-marks w)) (ss (wrap-subst w)))
                       (if (and (pair? ms) (eq? (car ms) the-anti-mark))
                           ;; output is from original text
                           (make-syntax-object
                            (syntax-object-expression x)
                            (make-wrap (cdr ms) (if rib (cons rib (cdr ss)) (cdr ss)))
                            (syntax-object-module x))
                           ;; output introduced by macro
                           (make-syntax-object
                            (decorate-source (syntax-object-expression x) s)
                            (make-wrap (cons m ms)
                                       (if rib
                                           (cons rib (cons 'shift ss))
                                           (cons 'shift ss)))
                            (syntax-object-module x))))))
                
                  ((vector? x)
                   (let* ((n (vector-length x))
                          (v (decorate-source (make-vector n) s)))
                     (do ((i 0 (fx+ i 1)))
                         ((fx= i n) v)
                       (vector-set! v i
                                    (rebuild-macro-output (vector-ref x i) m)))))
                  ((symbol? x)
                   (syntax-violation #f "encountered raw symbol in macro output"
                                     (source-wrap e w (wrap-subst w) mod) x))
                  (else (decorate-source x s)))))
        (with-fluids ((transformer-environment
                       (lambda (k) (k e r w s rib mod))))
          (rebuild-macro-output (p (source-wrap e (anti-mark w) s mod))
                                (new-mark)))))

    (define expand-body
      ;; In processing the forms of the body, we create a new, empty wrap.
      ;; This wrap is augmented (destructively) each time we discover that
      ;; the next form is a definition.  This is done:
      ;;
      ;;   (1) to allow the first nondefinition form to be a call to
      ;;       one of the defined ids even if the id previously denoted a
      ;;       definition keyword or keyword for a macro expanding into a
      ;;       definition;
      ;;   (2) to prevent subsequent definition forms (but unfortunately
      ;;       not earlier ones) and the first nondefinition form from
      ;;       confusing one of the bound identifiers for an auxiliary
      ;;       keyword; and
      ;;   (3) so that we do not need to restart the expansion of the
      ;;       first nondefinition form, which is problematic anyway
      ;;       since it might be the first element of a begin that we
      ;;       have just spliced into the body (meaning if we restarted,
      ;;       we'd really need to restart with the begin or the macro
      ;;       call that expanded into the begin, and we'd have to give
      ;;       up allowing (begin <defn>+ <expr>+), which is itself
      ;;       problematic since we don't know if a begin contains only
      ;;       definitions until we've expanded it).
      ;;
      ;; Before processing the body, we also create a new environment
      ;; containing a placeholder for the bindings we will add later and
      ;; associate this environment with each form.  In processing a
      ;; let-syntax or letrec-syntax, the associated environment may be
      ;; augmented with local keyword bindings, so the environment may
      ;; be different for different forms in the body.  Once we have
      ;; gathered up all of the definitions, we evaluate the transformer
      ;; expressions and splice into r at the placeholder the new variable
      ;; and keyword bindings.  This allows let-syntax or letrec-syntax
      ;; forms local to a portion or all of the body to shadow the
      ;; definition bindings.
      ;;
      ;; Subforms of a begin, let-syntax, or letrec-syntax are spliced
      ;; into the body.
      ;;
      ;; outer-form is fully wrapped w/source
      (lambda (body outer-form r w mod)
        (let* ((r (cons '("placeholder" . (placeholder)) r))
               (ribcage (make-empty-ribcage))
               (w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))))
          (let parse ((body (map (lambda (x) (cons r (wrap x w mod))) body))
                      (ids '()) (labels '())
                      (var-ids '()) (vars '()) (vals '()) (bindings '()))
            (if (null? body)
                (syntax-violation #f "no expressions in body" outer-form)
                (let ((e (cdar body)) (er (caar body)))
                  (call-with-values
                      (lambda () (syntax-type e er empty-wrap (source-annotation e) ribcage mod #f))
                    (lambda (type value form e w s mod)
                      (case type
                        ((define-form)
                         (let ((id (wrap value w mod)) (label (gen-label)))
                           (let ((var (gen-var id)))
                             (extend-ribcage! ribcage id label)
                             (parse (cdr body)
                                    (cons id ids) (cons label labels)
                                    (cons id var-ids)
                                    (cons var vars) (cons (cons er (wrap e w mod)) vals)
                                    (cons (make-binding 'lexical var) bindings)))))
                        ((define-syntax-form)
                         (let ((id (wrap value w mod))
                               (label (gen-label))
                               (trans-r (macros-only-env er)))
                           (extend-ribcage! ribcage id label)
                           ;; As required by R6RS, evaluate the right-hand-sides of internal
                           ;; syntax definition forms and add their transformers to the
                           ;; compile-time environment immediately, so that the newly-defined
                           ;; keywords may be used in definition context within the same
                           ;; lexical contour.
                           (set-cdr! r (extend-env
                                        (list label)
                                        (list (make-binding
                                               'macro
                                               (eval-local-transformer
                                                (expand e trans-r w mod)
                                                mod)))
                                        (cdr r)))
                           (parse (cdr body) (cons id ids) labels var-ids vars vals bindings)))
                        ((define-syntax-parameter-form)
                         ;; Same as define-syntax-form, but different format of the binding.
                         (let ((id (wrap value w mod))
                               (label (gen-label))
                               (trans-r (macros-only-env er)))
                           (extend-ribcage! ribcage id label)
                           (set-cdr! r (extend-env
                                        (list label)
                                        (list (make-binding
                                               'syntax-parameter
                                               (list (eval-local-transformer
                                                      (expand e trans-r w mod)
                                                      mod))))
                                        (cdr r)))
                           (parse (cdr body) (cons id ids) labels var-ids vars vals bindings)))
                        ((begin-form)
                         (syntax-case e ()
                           ((_ e1 ...)
                            (parse (let f ((forms #'(e1 ...)))
                                     (if (null? forms)
                                         (cdr body)
                                         (cons (cons er (wrap (car forms) w mod))
                                               (f (cdr forms)))))
                                   ids labels var-ids vars vals bindings))))
                        ((local-syntax-form)
                         (expand-local-syntax value e er w s mod
                                              (lambda (forms er w s mod)
                                                (parse (let f ((forms forms))
                                                         (if (null? forms)
                                                             (cdr body)
                                                             (cons (cons er (wrap (car forms) w mod))
                                                                   (f (cdr forms)))))
                                                       ids labels var-ids vars vals bindings))))
                        (else           ; found a non-definition
                         (if (null? ids)
                             (build-sequence no-source
                                             (map (lambda (x)
                                                    (expand (cdr x) (car x) empty-wrap mod))
                                                  (cons (cons er (source-wrap e w s mod))
                                                        (cdr body))))
                             (begin
                               (if (not (valid-bound-ids? ids))
                                   (syntax-violation
                                    #f "invalid or duplicate identifier in definition"
                                    outer-form))
                               (set-cdr! r (extend-env labels bindings (cdr r)))
                               (build-letrec no-source #t
                                             (reverse (map syntax->datum var-ids))
                                             (reverse vars)
                                             (map (lambda (x)
                                                    (expand (cdr x) (car x) empty-wrap mod))
                                                  (reverse vals))
                                             (build-sequence no-source
                                                             (map (lambda (x)
                                                                    (expand (cdr x) (car x) empty-wrap mod))
                                                                  (cons (cons er (source-wrap e w s mod))
                                                                        (cdr body)))))))))))))))))

    (define expand-local-syntax
      (lambda (rec? e r w s mod k)
        (syntax-case e ()
          ((_ ((id val) ...) e1 e2 ...)
           (let ((ids #'(id ...)))
             (if (not (valid-bound-ids? ids))
                 (syntax-violation #f "duplicate bound keyword" e)
                 (let ((labels (gen-labels ids)))
                   (let ((new-w (make-binding-wrap ids labels w)))
                     (k #'(e1 e2 ...)
                        (extend-env
                         labels
                         (let ((w (if rec? new-w w))
                               (trans-r (macros-only-env r)))
                           (map (lambda (x)
                                  (make-binding 'macro
                                                (eval-local-transformer
                                                 (expand x trans-r w mod)
                                                 mod)))
                                #'(val ...)))
                         r)
                        new-w
                        s
                        mod))))))
          (_ (syntax-violation #f "bad local syntax definition"
                               (source-wrap e w s mod))))))

    (define eval-local-transformer
      (lambda (expanded mod)
        (let ((p (local-eval-hook expanded mod)))
          (if (procedure? p)
              p
              (syntax-violation #f "nonprocedure transformer" p)))))

    (define expand-void
      (lambda ()
        (build-void no-source)))

    (define ellipsis?
      (lambda (e r mod)
        (and (nonsymbol-id? e)
             ;; If there is a binding for the special identifier
             ;; #{ $sc-ellipsis }# in the lexical environment of E,
             ;; and if the associated binding type is 'ellipsis',
             ;; then the binding's value specifies the custom ellipsis
             ;; identifier within that lexical environment, and the
             ;; comparison is done using 'bound-id=?'.
             (call-with-values
                 (lambda () (resolve-identifier
                             (make-syntax-object '#{ $sc-ellipsis }#
                                                 (syntax-object-wrap e)
                                                 (syntax-object-module e))
                             empty-wrap r mod #f))
               (lambda (type value mod)
                 (if (eq? type 'ellipsis)
                     (bound-id=? e value)
                     (free-id=? e #'(... ...))))))))

    (define lambda-formals
      (lambda (orig-args)
        (define (req args rreq)
          (syntax-case args ()
            (()
             (check (reverse rreq) #f))
            ((a . b) (id? #'a)
             (req #'b (cons #'a rreq)))
            (r (id? #'r)
               (check (reverse rreq) #'r))
            (else
             (syntax-violation 'lambda "invalid argument list" orig-args args))))
        (define (check req rest)
          (cond
           ((distinct-bound-ids? (if rest (cons rest req) req))
            (values req #f rest #f))
           (else
            (syntax-violation 'lambda "duplicate identifier in argument list"
                              orig-args))))
        (req orig-args '())))

    (define expand-simple-lambda
      (lambda (e r w s mod req rest meta body)
        (let* ((ids (if rest (append req (list rest)) req))
               (vars (map gen-var ids))
               (labels (gen-labels ids)))
          (build-simple-lambda
           s
           (map syntax->datum req) (and rest (syntax->datum rest)) vars
           meta
           (expand-body body (source-wrap e w s mod)
                        (extend-var-env labels vars r)
                        (make-binding-wrap ids labels w)
                        mod)))))

    (define lambda*-formals
      (lambda (orig-args)
        (define (req args rreq)
          (syntax-case args ()
            (()
             (check (reverse rreq) '() #f '()))
            ((a . b) (id? #'a)
             (req #'b (cons #'a rreq)))
            ((a . b) (eq? (syntax->datum #'a) #:optional)
             (opt #'b (reverse rreq) '()))
            ((a . b) (eq? (syntax->datum #'a) #:key)
             (key #'b (reverse rreq) '() '()))
            ((a b) (eq? (syntax->datum #'a) #:rest)
             (rest #'b (reverse rreq) '() '()))
            (r (id? #'r)
               (rest #'r (reverse rreq) '() '()))
            (else
             (syntax-violation 'lambda* "invalid argument list" orig-args args))))
        (define (opt args req ropt)
          (syntax-case args ()
            (()
             (check req (reverse ropt) #f '()))
            ((a . b) (id? #'a)
             (opt #'b req (cons #'(a #f) ropt)))
            (((a init) . b) (id? #'a)
             (opt #'b req (cons #'(a init) ropt)))
            ((a . b) (eq? (syntax->datum #'a) #:key)
             (key #'b req (reverse ropt) '()))
            ((a b) (eq? (syntax->datum #'a) #:rest)
             (rest #'b req (reverse ropt) '()))
            (r (id? #'r)
               (rest #'r req (reverse ropt) '()))
            (else
             (syntax-violation 'lambda* "invalid optional argument list"
                               orig-args args))))
        (define (key args req opt rkey)
          (syntax-case args ()
            (()
             (check req opt #f (cons #f (reverse rkey))))
            ((a . b) (id? #'a)
             (with-syntax ((k (symbol->keyword (syntax->datum #'a))))
               (key #'b req opt (cons #'(k a #f) rkey))))
            (((a init) . b) (id? #'a)
             (with-syntax ((k (symbol->keyword (syntax->datum #'a))))
               (key #'b req opt (cons #'(k a init) rkey))))
            (((a init k) . b) (and (id? #'a)
                                   (keyword? (syntax->datum #'k)))
             (key #'b req opt (cons #'(k a init) rkey)))
            ((aok) (eq? (syntax->datum #'aok) #:allow-other-keys)
             (check req opt #f (cons #t (reverse rkey))))
            ((aok a b) (and (eq? (syntax->datum #'aok) #:allow-other-keys)
                            (eq? (syntax->datum #'a) #:rest))
             (rest #'b req opt (cons #t (reverse rkey))))
            ((aok . r) (and (eq? (syntax->datum #'aok) #:allow-other-keys)
                            (id? #'r))
             (rest #'r req opt (cons #t (reverse rkey))))
            ((a b) (eq? (syntax->datum #'a) #:rest)
             (rest #'b req opt (cons #f (reverse rkey))))
            (r (id? #'r)
               (rest #'r req opt (cons #f (reverse rkey))))
            (else
             (syntax-violation 'lambda* "invalid keyword argument list"
                               orig-args args))))
        (define (rest args req opt kw)
          (syntax-case args ()
            (r (id? #'r)
               (check req opt #'r kw))
            (else
             (syntax-violation 'lambda* "invalid rest argument"
                               orig-args args))))
        (define (check req opt rest kw)
          (cond
           ((distinct-bound-ids?
             (append req (map car opt) (if rest (list rest) '())
                     (if (pair? kw) (map cadr (cdr kw)) '())))
            (values req opt rest kw))
           (else
            (syntax-violation 'lambda* "duplicate identifier in argument list"
                              orig-args))))
        (req orig-args '())))

    (define expand-lambda-case
      (lambda (e r w s mod get-formals clauses)
        (define (parse-req req opt rest kw body)
          (let ((vars (map gen-var req))
                (labels (gen-labels req)))
            (let ((r* (extend-var-env labels vars r))
                  (w* (make-binding-wrap req labels w)))
              (parse-opt (map syntax->datum req)
                         opt rest kw body (reverse vars) r* w* '() '()))))
        (define (parse-opt req opt rest kw body vars r* w* out inits)
          (cond
           ((pair? opt)
            (syntax-case (car opt) ()
              ((id i)
               (let* ((v (gen-var #'id))
                      (l (gen-labels (list v)))
                      (r** (extend-var-env l (list v) r*))
                      (w** (make-binding-wrap (list #'id) l w*)))
                 (parse-opt req (cdr opt) rest kw body (cons v vars)
                            r** w** (cons (syntax->datum #'id) out)
                            (cons (expand #'i r* w* mod) inits))))))
           (rest
            (let* ((v (gen-var rest))
                   (l (gen-labels (list v)))
                   (r* (extend-var-env l (list v) r*))
                   (w* (make-binding-wrap (list rest) l w*)))
              (parse-kw req (if (pair? out) (reverse out) #f)
                        (syntax->datum rest)
                        (if (pair? kw) (cdr kw) kw)
                        body (cons v vars) r* w* 
                        (if (pair? kw) (car kw) #f)
                        '() inits)))
           (else
            (parse-kw req (if (pair? out) (reverse out) #f) #f
                      (if (pair? kw) (cdr kw) kw)
                      body vars r* w*
                      (if (pair? kw) (car kw) #f)
                      '() inits))))
        (define (parse-kw req opt rest kw body vars r* w* aok out inits)
          (cond
           ((pair? kw)
            (syntax-case (car kw) ()
              ((k id i)
               (let* ((v (gen-var #'id))
                      (l (gen-labels (list v)))
                      (r** (extend-var-env l (list v) r*))
                      (w** (make-binding-wrap (list #'id) l w*)))
                 (parse-kw req opt rest (cdr kw) body (cons v vars)
                           r** w** aok
                           (cons (list (syntax->datum #'k)
                                       (syntax->datum #'id)
                                       v)
                                 out)
                           (cons (expand #'i r* w* mod) inits))))))
           (else
            (parse-body req opt rest
                        (if (or aok (pair? out)) (cons aok (reverse out)) #f)
                        body (reverse vars) r* w* (reverse inits) '()))))
        (define (parse-body req opt rest kw body vars r* w* inits meta)
          (syntax-case body ()
            ((docstring e1 e2 ...) (string? (syntax->datum #'docstring))
             (parse-body req opt rest kw #'(e1 e2 ...) vars r* w* inits
                         (append meta 
                                 `((documentation
                                    . ,(syntax->datum #'docstring))))))
            ((#((k . v) ...) e1 e2 ...) 
             (parse-body req opt rest kw #'(e1 e2 ...) vars r* w* inits
                         (append meta (syntax->datum #'((k . v) ...)))))
            ((e1 e2 ...)
             (values meta req opt rest kw inits vars
                     (expand-body #'(e1 e2 ...) (source-wrap e w s mod)
                                  r* w* mod)))))

        (syntax-case clauses ()
          (() (values '() #f))
          (((args e1 e2 ...) (args* e1* e2* ...) ...)
           (call-with-values (lambda () (get-formals #'args))
             (lambda (req opt rest kw)
               (call-with-values (lambda ()
                                   (parse-req req opt rest kw #'(e1 e2 ...)))
                 (lambda (meta req opt rest kw inits vars body)
                   (call-with-values
                       (lambda ()
                         (expand-lambda-case e r w s mod get-formals
                                             #'((args* e1* e2* ...) ...)))
                     (lambda (meta* else*)
                       (values
                        (append meta meta*)
                        (build-lambda-case s req opt rest kw inits vars
                                           body else*))))))))))))

    ;; data

    ;; strips syntax-objects down to top-wrap
    ;;
    ;; since only the head of a list is annotated by the reader, not each pair
    ;; in the spine, we also check for pairs whose cars are annotated in case
    ;; we've been passed the cdr of an annotated list

    (define strip
      (lambda (x w)
        (if (top-marked? w)
            x
            (let f ((x x))
              (cond
               ((syntax-object? x)
                (strip (syntax-object-expression x) (syntax-object-wrap x)))
               ((pair? x)
                (let ((a (f (car x))) (d (f (cdr x))))
                  (if (and (eq? a (car x)) (eq? d (cdr x)))
                      x
                      (cons a d))))
               ((vector? x)
                (let ((old (vector->list x)))
                  (let ((new (map f old)))
                    ;; inlined and-map with two args
                    (let lp ((l1 old) (l2 new))
                      (if (null? l1)
                          x
                          (if (eq? (car l1) (car l2))
                              (lp (cdr l1) (cdr l2))
                              (list->vector new)))))))
               (else x))))))

    ;; lexical variables

    (define gen-var
      (lambda (id)
        (let ((id (if (syntax-object? id) (syntax-object-expression id) id)))
          (build-lexical-var no-source id))))

    ;; appears to return a reversed list
    (define lambda-var-list
      (lambda (vars)
        (let lvl ((vars vars) (ls '()) (w empty-wrap))
          (cond
           ((pair? vars) (lvl (cdr vars) (cons (wrap (car vars) w #f) ls) w))
           ((id? vars) (cons (wrap vars w #f) ls))
           ((null? vars) ls)
           ((syntax-object? vars)
            (lvl (syntax-object-expression vars)
                 ls
                 (join-wraps w (syntax-object-wrap vars))))
           ;; include anything else to be caught by subsequent error
           ;; checking
           (else (cons vars ls))))))

    ;; core transformers

    (global-extend 'local-syntax 'letrec-syntax #t)
    (global-extend 'local-syntax 'let-syntax #f)

    (global-extend
     'core 'syntax-parameterize
     (lambda (e r w s mod)
       (syntax-case e ()
         ((_ ((var val) ...) e1 e2 ...)
          (valid-bound-ids? #'(var ...))
          (let ((names
                 (map (lambda (x)
                        (call-with-values
                            (lambda () (resolve-identifier x w r mod #f))
                          (lambda (type value mod)
                            (case type
                              ((displaced-lexical)
                               (syntax-violation 'syntax-parameterize
                                                 "identifier out of context"
                                                 e
                                                 (source-wrap x w s mod)))
                              ((syntax-parameter)
                               value)
                              (else
                               (syntax-violation 'syntax-parameterize
                                                 "invalid syntax parameter"
                                                 e
                                                 (source-wrap x w s mod)))))))
                      #'(var ...)))
                (bindings
                 (let ((trans-r (macros-only-env r)))
                   (map (lambda (x)
                          (make-binding
                           'macro
                           (eval-local-transformer (expand x trans-r w mod) mod)))
                        #'(val ...)))))
            (expand-body #'(e1 e2 ...)
                      (source-wrap e w s mod)
                      (extend-env names bindings r)
                      w
                      mod)))
         (_ (syntax-violation 'syntax-parameterize "bad syntax"
                              (source-wrap e w s mod))))))

    (global-extend 'core 'quote
                   (lambda (e r w s mod)
                     (syntax-case e ()
                       ((_ e) (build-data s (strip #'e w)))
                       (_ (syntax-violation 'quote "bad syntax"
                                            (source-wrap e w s mod))))))

    (global-extend
     'core 'syntax
     (let ()
       (define gen-syntax
         (lambda (src e r maps ellipsis? mod)
           (if (id? e)
               (call-with-values (lambda ()
                                   (resolve-identifier e empty-wrap r mod #f))
                 (lambda (type value mod)
                   (case type
                     ((syntax)
                      (call-with-values
                          (lambda () (gen-ref src (car value) (cdr value) maps))
                        (lambda (var maps)
                          (values `(ref ,var) maps))))
                     (else
                      (if (ellipsis? e r mod)
                          (syntax-violation 'syntax "misplaced ellipsis" src)
                          (values `(quote ,e) maps))))))
               (syntax-case e ()
                 ((dots e)
                  (ellipsis? #'dots r mod)
                  (gen-syntax src #'e r maps (lambda (e r mod) #f) mod))
                 ((x dots . y)
                  ;; this could be about a dozen lines of code, except that we
                  ;; choose to handle #'(x ... ...) forms
                  (ellipsis? #'dots r mod)
                  (let f ((y #'y)
                          (k (lambda (maps)
                               (call-with-values
                                   (lambda ()
                                     (gen-syntax src #'x r
                                                 (cons '() maps) ellipsis? mod))
                                 (lambda (x maps)
                                   (if (null? (car maps))
                                       (syntax-violation 'syntax "extra ellipsis"
                                                         src)
                                       (values (gen-map x (car maps))
                                               (cdr maps))))))))
                    (syntax-case y ()
                      ((dots . y)
                       (ellipsis? #'dots r mod)
                       (f #'y
                          (lambda (maps)
                            (call-with-values
                                (lambda () (k (cons '() maps)))
                              (lambda (x maps)
                                (if (null? (car maps))
                                    (syntax-violation 'syntax "extra ellipsis" src)
                                    (values (gen-mappend x (car maps))
                                            (cdr maps))))))))
                      (_ (call-with-values
                             (lambda () (gen-syntax src y r maps ellipsis? mod))
                           (lambda (y maps)
                             (call-with-values
                                 (lambda () (k maps))
                               (lambda (x maps)
                                 (values (gen-append x y) maps)))))))))
                 ((x . y)
                  (call-with-values
                      (lambda () (gen-syntax src #'x r maps ellipsis? mod))
                    (lambda (x maps)
                      (call-with-values
                          (lambda () (gen-syntax src #'y r maps ellipsis? mod))
                        (lambda (y maps) (values (gen-cons x y) maps))))))
                 (#(e1 e2 ...)
                  (call-with-values
                      (lambda ()
                        (gen-syntax src #'(e1 e2 ...) r maps ellipsis? mod))
                    (lambda (e maps) (values (gen-vector e) maps))))
                 (_ (values `(quote ,e) maps))))))

       (define gen-ref
         (lambda (src var level maps)
           (if (fx= level 0)
               (values var maps)
               (if (null? maps)
                   (syntax-violation 'syntax "missing ellipsis" src)
                   (call-with-values
                       (lambda () (gen-ref src var (fx- level 1) (cdr maps)))
                     (lambda (outer-var outer-maps)
                       (let ((b (assq outer-var (car maps))))
                         (if b
                             (values (cdr b) maps)
                             (let ((inner-var (gen-var 'tmp)))
                               (values inner-var
                                       (cons (cons (cons outer-var inner-var)
                                                   (car maps))
                                             outer-maps)))))))))))

       (define gen-mappend
         (lambda (e map-env)
           `(apply (primitive append) ,(gen-map e map-env))))

       (define gen-map
         (lambda (e map-env)
           (let ((formals (map cdr map-env))
                 (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
             (cond
              ((eq? (car e) 'ref)
               ;; identity map equivalence:
               ;; (map (lambda (x) x) y) == y
               (car actuals))
              ((and-map
                (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
                (cdr e))
               ;; eta map equivalence:
               ;; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
               `(map (primitive ,(car e))
                     ,@(map (let ((r (map cons formals actuals)))
                              (lambda (x) (cdr (assq (cadr x) r))))
                            (cdr e))))
              (else `(map (lambda ,formals ,e) ,@actuals))))))

       (define gen-cons
         (lambda (x y)
           (case (car y)
             ((quote)
              (if (eq? (car x) 'quote)
                  `(quote (,(cadr x) . ,(cadr y)))
                  (if (eq? (cadr y) '())
                      `(list ,x)
                      `(cons ,x ,y))))
             ((list) `(list ,x ,@(cdr y)))
             (else `(cons ,x ,y)))))

       (define gen-append
         (lambda (x y)
           (if (equal? y '(quote ()))
               x
               `(append ,x ,y))))

       (define gen-vector
         (lambda (x)
           (cond
            ((eq? (car x) 'list) `(vector ,@(cdr x)))
            ((eq? (car x) 'quote) `(quote #(,@(cadr x))))
            (else `(list->vector ,x)))))


       (define regen
         (lambda (x)
           (case (car x)
             ((ref) (build-lexical-reference 'value no-source (cadr x) (cadr x)))
             ((primitive) (build-primref no-source (cadr x)))
             ((quote) (build-data no-source (cadr x)))
             ((lambda)
              (if (list? (cadr x))
                  (build-simple-lambda no-source (cadr x) #f (cadr x) '() (regen (caddr x)))
                  (error "how did we get here" x)))
             (else (build-primcall no-source (car x) (map regen (cdr x)))))))

       (lambda (e r w s mod)
         (let ((e (source-wrap e w s mod)))
           (syntax-case e ()
             ((_ x)
              (call-with-values
                  (lambda () (gen-syntax e #'x r '() ellipsis? mod))
                (lambda (e maps) (regen e))))
             (_ (syntax-violation 'syntax "bad `syntax' form" e)))))))

    (global-extend 'core 'lambda
                   (lambda (e r w s mod)
                     (syntax-case e ()
                       ((_ args e1 e2 ...)
                        (call-with-values (lambda () (lambda-formals #'args))
                          (lambda (req opt rest kw)
                            (let lp ((body #'(e1 e2 ...)) (meta '()))
                              (syntax-case body ()
                                ((docstring e1 e2 ...) (string? (syntax->datum #'docstring))
                                 (lp #'(e1 e2 ...)
                                     (append meta
                                             `((documentation
                                                . ,(syntax->datum #'docstring))))))
                                ((#((k . v) ...) e1 e2 ...) 
                                 (lp #'(e1 e2 ...)
                                     (append meta (syntax->datum #'((k . v) ...)))))
                                (_ (expand-simple-lambda e r w s mod req rest meta body)))))))
                       (_ (syntax-violation 'lambda "bad lambda" e)))))
  
    (global-extend 'core 'lambda*
                   (lambda (e r w s mod)
                     (syntax-case e ()
                       ((_ args e1 e2 ...)
                        (call-with-values
                            (lambda ()
                              (expand-lambda-case e r w s mod
                                                  lambda*-formals #'((args e1 e2 ...))))
                          (lambda (meta lcase)
                            (build-case-lambda s meta lcase))))
                       (_ (syntax-violation 'lambda "bad lambda*" e)))))

    (global-extend 'core 'case-lambda
                   (lambda (e r w s mod)
                     (define (build-it meta clauses)
                       (call-with-values
                           (lambda ()
                             (expand-lambda-case e r w s mod
                                                 lambda-formals
                                                 clauses))
                         (lambda (meta* lcase)
                           (build-case-lambda s (append meta meta*) lcase))))
                     (syntax-case e ()
                       ((_ (args e1 e2 ...) ...)
                        (build-it '() #'((args e1 e2 ...) ...)))
                       ((_ docstring (args e1 e2 ...) ...)
                        (string? (syntax->datum #'docstring))
                        (build-it `((documentation
                                     . ,(syntax->datum #'docstring)))
                                  #'((args e1 e2 ...) ...)))
                       (_ (syntax-violation 'case-lambda "bad case-lambda" e)))))

    (global-extend 'core 'case-lambda*
                   (lambda (e r w s mod)
                     (define (build-it meta clauses)
                       (call-with-values
                           (lambda ()
                             (expand-lambda-case e r w s mod
                                                 lambda*-formals
                                                 clauses))
                         (lambda (meta* lcase)
                           (build-case-lambda s (append meta meta*) lcase))))
                     (syntax-case e ()
                       ((_ (args e1 e2 ...) ...)
                        (build-it '() #'((args e1 e2 ...) ...)))
                       ((_ docstring (args e1 e2 ...) ...)
                        (string? (syntax->datum #'docstring))
                        (build-it `((documentation
                                     . ,(syntax->datum #'docstring)))
                                  #'((args e1 e2 ...) ...)))
                       (_ (syntax-violation 'case-lambda "bad case-lambda*" e)))))

    (global-extend 'core 'with-ellipsis
                   (lambda (e r w s mod)
                     (syntax-case e ()
                       ((_ dots e1 e2 ...)
                        (id? #'dots)
                        (let ((id (if (symbol? #'dots)
                                      '#{ $sc-ellipsis }#
                                      (make-syntax-object '#{ $sc-ellipsis }#
                                                          (syntax-object-wrap #'dots)
                                                          (syntax-object-module #'dots)))))
                          (let ((ids (list id))
                                (labels (list (gen-label)))
                                (bindings (list (make-binding 'ellipsis (source-wrap #'dots w s mod)))))
                            (let ((nw (make-binding-wrap ids labels w))
                                  (nr (extend-env labels bindings r)))
                              (expand-body #'(e1 e2 ...) (source-wrap e nw s mod) nr nw mod)))))
                       (_ (syntax-violation 'with-ellipsis "bad syntax"
                                            (source-wrap e w s mod))))))

    (global-extend 'core 'let
                   (let ()
                     (define (expand-let e r w s mod constructor ids vals exps)
                       (if (not (valid-bound-ids? ids))
                           (syntax-violation 'let "duplicate bound variable" e)
                           (let ((labels (gen-labels ids))
                                 (new-vars (map gen-var ids)))
                             (let ((nw (make-binding-wrap ids labels w))
                                   (nr (extend-var-env labels new-vars r)))
                               (constructor s
                                            (map syntax->datum ids)
                                            new-vars
                                            (map (lambda (x) (expand x r w mod)) vals)
                                            (expand-body exps (source-wrap e nw s mod)
                                                         nr nw mod))))))
                     (lambda (e r w s mod)
                       (syntax-case e ()
                         ((_ ((id val) ...) e1 e2 ...)
                          (and-map id? #'(id ...))
                          (expand-let e r w s mod
                                      build-let
                                      #'(id ...)
                                      #'(val ...)
                                      #'(e1 e2 ...)))
                         ((_ f ((id val) ...) e1 e2 ...)
                          (and (id? #'f) (and-map id? #'(id ...)))
                          (expand-let e r w s mod
                                      build-named-let
                                      #'(f id ...)
                                      #'(val ...)
                                      #'(e1 e2 ...)))
                         (_ (syntax-violation 'let "bad let" (source-wrap e w s mod)))))))


    (global-extend 'core 'letrec
                   (lambda (e r w s mod)
                     (syntax-case e ()
                       ((_ ((id val) ...) e1 e2 ...)
                        (and-map id? #'(id ...))
                        (let ((ids #'(id ...)))
                          (if (not (valid-bound-ids? ids))
                              (syntax-violation 'letrec "duplicate bound variable" e)
                              (let ((labels (gen-labels ids))
                                    (new-vars (map gen-var ids)))
                                (let ((w (make-binding-wrap ids labels w))
                                      (r (extend-var-env labels new-vars r)))
                                  (build-letrec s #f
                                                (map syntax->datum ids)
                                                new-vars
                                                (map (lambda (x) (expand x r w mod)) #'(val ...))
                                                (expand-body #'(e1 e2 ...) 
                                                             (source-wrap e w s mod) r w mod)))))))
                       (_ (syntax-violation 'letrec "bad letrec" (source-wrap e w s mod))))))


    (global-extend 'core 'letrec*
                   (lambda (e r w s mod)
                     (syntax-case e ()
                       ((_ ((id val) ...) e1 e2 ...)
                        (and-map id? #'(id ...))
                        (let ((ids #'(id ...)))
                          (if (not (valid-bound-ids? ids))
                              (syntax-violation 'letrec* "duplicate bound variable" e)
                              (let ((labels (gen-labels ids))
                                    (new-vars (map gen-var ids)))
                                (let ((w (make-binding-wrap ids labels w))
                                      (r (extend-var-env labels new-vars r)))
                                  (build-letrec s #t
                                                (map syntax->datum ids)
                                                new-vars
                                                (map (lambda (x) (expand x r w mod)) #'(val ...))
                                                (expand-body #'(e1 e2 ...) 
                                                             (source-wrap e w s mod) r w mod)))))))
                       (_ (syntax-violation 'letrec* "bad letrec*" (source-wrap e w s mod))))))


    (global-extend
     'core 'set!
     (lambda (e r w s mod)
       (syntax-case e ()
         ((_ id val)
          (id? #'id)
          (call-with-values
              (lambda () (resolve-identifier #'id w r mod #t))
            (lambda (type value id-mod)
              (case type
                ((lexical)
                 (build-lexical-assignment s (syntax->datum #'id) value
                                           (expand #'val r w mod)))
                ((global)
                 (build-global-assignment s value (expand #'val r w mod) id-mod))
                ((macro)
                 (if (procedure-property value 'variable-transformer)
                     ;; As syntax-type does, call expand-macro with
                     ;; the mod of the expression. Hmm.
                     (expand (expand-macro value e r w s #f mod) r empty-wrap mod)
                     (syntax-violation 'set! "not a variable transformer"
                                       (wrap e w mod)
                                       (wrap #'id w id-mod))))
                ((displaced-lexical)
                 (syntax-violation 'set! "identifier out of context"
                                   (wrap #'id w mod)))
                (else
                 (syntax-violation 'set! "bad set!" (source-wrap e w s mod)))))))
         ((_ (head tail ...) val)
          (call-with-values
              (lambda () (syntax-type #'head r empty-wrap no-source #f mod #t))
            (lambda (type value ee* ee ww ss modmod)
              (case type
                ((module-ref)
                 (let ((val (expand #'val r w mod)))
                   (call-with-values (lambda () (value #'(head tail ...) r w mod))
                     (lambda (e r w s* mod)
                       (syntax-case e ()
                         (e (id? #'e)
                            (build-global-assignment s (syntax->datum #'e)
                                                     val mod)))))))
                (else
                 (build-call s
                             (expand #'(setter head) r w mod)
                             (map (lambda (e) (expand e r w mod))
                                  #'(tail ... val))))))))
         (_ (syntax-violation 'set! "bad set!" (source-wrap e w s mod))))))

    (global-extend 'module-ref '@
                   (lambda (e r w mod)
                     (syntax-case e ()
                       ((_ (mod ...) id)
                        (and (and-map id? #'(mod ...)) (id? #'id))
                        ;; Strip the wrap from the identifier and return top-wrap
                        ;; so that the identifier will not be captured by lexicals.
                        (values (syntax->datum #'id) r top-wrap #f
                                (syntax->datum
                                 #'(public mod ...)))))))

    (global-extend 'module-ref '@@
                   (lambda (e r w mod)
                     (define remodulate
                       (lambda (x mod)
                         (cond ((pair? x)
                                (cons (remodulate (car x) mod)
                                      (remodulate (cdr x) mod)))
                               ((syntax-object? x)
                                (make-syntax-object
                                 (remodulate (syntax-object-expression x) mod)
                                 (syntax-object-wrap x)
                                 ;; hither the remodulation
                                 mod))
                               ((vector? x)
                                (let* ((n (vector-length x)) (v (make-vector n)))
                                  (do ((i 0 (fx+ i 1)))
                                      ((fx= i n) v)
                                    (vector-set! v i (remodulate (vector-ref x i) mod)))))
                               (else x))))
                     (syntax-case e (@@ primitive)
                       ((_ primitive id)
                        (and (id? #'id)
                             (equal? (cdr (if (syntax-object? #'id)
                                              (syntax-object-module #'id)
                                              mod))
                                     '(guile)))
                        ;; Strip the wrap from the identifier and return top-wrap
                        ;; so that the identifier will not be captured by lexicals.
                        (values (syntax->datum #'id) r top-wrap #f '(primitive)))
                       ((_ (mod ...) id)
                        (and (and-map id? #'(mod ...)) (id? #'id))
                        ;; Strip the wrap from the identifier and return top-wrap
                        ;; so that the identifier will not be captured by lexicals.
                        (values (syntax->datum #'id) r top-wrap #f
                                (syntax->datum
                                 #'(private mod ...))))
                       ((_ @@ (mod ...) exp)
                        (and-map id? #'(mod ...))
                        ;; This is a special syntax used to support R6RS library forms.
                        ;; Unlike the syntax above, the last item is not restricted to
                        ;; be a single identifier, and the syntax objects are kept
                        ;; intact, with only their module changed.
                        (let ((mod (syntax->datum #'(private mod ...))))
                          (values (remodulate #'exp mod)
                                  r w (source-annotation #'exp)
                                  mod))))))
  
    (global-extend 'core 'if
                   (lambda (e r w s mod)
                     (syntax-case e ()
                       ((_ test then)
                        (build-conditional
                         s
                         (expand #'test r w mod)
                         (expand #'then r w mod)
                         (build-void no-source)))
                       ((_ test then else)
                        (build-conditional
                         s
                         (expand #'test r w mod)
                         (expand #'then r w mod)
                         (expand #'else r w mod))))))

    (global-extend 'begin 'begin '())

    (global-extend 'define 'define '())

    (global-extend 'define-syntax 'define-syntax '())
    (global-extend 'define-syntax-parameter 'define-syntax-parameter '())

    (global-extend 'eval-when 'eval-when '())

    (global-extend 'core 'syntax-case
                   (let ()
                     (define convert-pattern
                       ;; accepts pattern & keys
                       ;; returns $sc-dispatch pattern & ids
                       (lambda (pattern keys ellipsis?)
                         (define cvt*
                           (lambda (p* n ids)
                             (syntax-case p* ()
                               ((x . y)
                                (call-with-values
                                     (lambda () (cvt* #'y n ids))
                                   (lambda (y ids)
                                     (call-with-values
                                         (lambda () (cvt #'x n ids))
                                       (lambda (x ids)
                                         (values (cons x y) ids))))))
                               (_ (cvt p* n ids)))))
                         
                         (define (v-reverse x)
                           (let loop ((r '()) (x x))
                             (if (not (pair? x))
                                 (values r x)
                                 (loop (cons (car x) r) (cdr x)))))

                         (define cvt
                           (lambda (p n ids)
                             (if (id? p)
                                 (cond
                                  ((bound-id-member? p keys)
                                   (values (vector 'free-id p) ids))
                                  ((free-id=? p #'_)
                                   (values '_ ids))
                                  (else
                                   (values 'any (cons (cons p n) ids))))
                                 (syntax-case p ()
                                   ((x dots)
                                    (ellipsis? (syntax dots))
                                    (call-with-values
                                        (lambda () (cvt (syntax x) (fx+ n 1) ids))
                                      (lambda (p ids)
                                        (values (if (eq? p 'any) 'each-any (vector 'each p))
                                                ids))))
                                   ((x dots . ys)
                                    (ellipsis? (syntax dots))
                                    (call-with-values
                                        (lambda () (cvt* (syntax ys) n ids))
                                      (lambda (ys ids)
                                        (call-with-values
                                            (lambda () (cvt (syntax x) (+ n 1) ids))
                                          (lambda (x ids)
                                            (call-with-values
                                                (lambda () (v-reverse ys))
                                              (lambda (ys e)
                                                (values `#(each+ ,x ,ys ,e) 
                                                        ids))))))))
                                   ((x . y)
                                    (call-with-values
                                        (lambda () (cvt (syntax y) n ids))
                                      (lambda (y ids)
                                        (call-with-values
                                            (lambda () (cvt (syntax x) n ids))
                                          (lambda (x ids)
                                            (values (cons x y) ids))))))
                                   (() (values '() ids))
                                   (#(x ...)
                                    (call-with-values
                                        (lambda () (cvt (syntax (x ...)) n ids))
                                      (lambda (p ids) (values (vector 'vector p) ids))))
                                   (x (values (vector 'atom (strip p empty-wrap)) ids))))))
                         (cvt pattern 0 '())))

                     (define build-dispatch-call
                       (lambda (pvars exp y r mod)
                         (let ((ids (map car pvars)) (levels (map cdr pvars)))
                           (let ((labels (gen-labels ids)) (new-vars (map gen-var ids)))
                             (build-primcall
                              no-source
                              'apply
                              (list (build-simple-lambda no-source (map syntax->datum ids) #f new-vars '()
                                                         (expand exp
                                                              (extend-env
                                                               labels
                                                               (map (lambda (var level)
                                                                      (make-binding 'syntax `(,var . ,level)))
                                                                    new-vars
                                                                    (map cdr pvars))
                                                               r)
                                                              (make-binding-wrap ids labels empty-wrap)
                                                              mod))
                                    y))))))

                     (define gen-clause
                       (lambda (x keys clauses r pat fender exp mod)
                         (call-with-values
                             (lambda () (convert-pattern pat keys (lambda (e) (ellipsis? e r mod))))
                           (lambda (p pvars)
                             (cond
                              ((not (and-map (lambda (x) (not (ellipsis? (car x) r mod))) pvars))
                               (syntax-violation 'syntax-case "misplaced ellipsis" pat))
                              ((not (distinct-bound-ids? (map car pvars)))
                               (syntax-violation 'syntax-case "duplicate pattern variable" pat))
                              (else
                               (let ((y (gen-var 'tmp)))
                                 ;; fat finger binding and references to temp variable y
                                 (build-call no-source
                                             (build-simple-lambda no-source (list 'tmp) #f (list y) '()
                                                                  (let ((y (build-lexical-reference 'value no-source
                                                                                                    'tmp y)))
                                                                    (build-conditional no-source
                                                                                       (syntax-case fender ()
                                                                                         (#t y)
                                                                                         (_ (build-conditional no-source
                                                                                                               y
                                                                                                               (build-dispatch-call pvars fender y r mod)
                                                                                                               (build-data no-source #f))))
                                                                                       (build-dispatch-call pvars exp y r mod)
                                                                                       (gen-syntax-case x keys clauses r mod))))
                                             (list (if (eq? p 'any)
                                                       (build-primcall no-source 'list (list x))
                                                       (build-primcall no-source '$sc-dispatch
                                                                       (list x (build-data no-source p)))))))))))))

                     (define gen-syntax-case
                       (lambda (x keys clauses r mod)
                         (if (null? clauses)
                             (build-primcall no-source 'syntax-violation
                                             (list (build-data no-source #f)
                                                   (build-data no-source
                                                               "source expression failed to match any pattern")
                                                   x))
                             (syntax-case (car clauses) ()
                               ((pat exp)
                                (if (and (id? #'pat)
                                         (and-map (lambda (x) (not (free-id=? #'pat x)))
                                                  (cons #'(... ...) keys)))
                                    (if (free-id=? #'pat #'_)
                                        (expand #'exp r empty-wrap mod)
                                        (let ((labels (list (gen-label)))
                                              (var (gen-var #'pat)))
                                          (build-call no-source
                                                      (build-simple-lambda
                                                       no-source (list (syntax->datum #'pat)) #f (list var)
                                                       '()
                                                       (expand #'exp
                                                            (extend-env labels
                                                                        (list (make-binding 'syntax `(,var . 0)))
                                                                        r)
                                                            (make-binding-wrap #'(pat)
                                                                               labels empty-wrap)
                                                            mod))
                                                      (list x))))
                                    (gen-clause x keys (cdr clauses) r
                                                #'pat #t #'exp mod)))
                               ((pat fender exp)
                                (gen-clause x keys (cdr clauses) r
                                            #'pat #'fender #'exp mod))
                               (_ (syntax-violation 'syntax-case "invalid clause"
                                                    (car clauses)))))))

                     (lambda (e r w s mod)
                       (let ((e (source-wrap e w s mod)))
                         (syntax-case e ()
                           ((_ val (key ...) m ...)
                            (if (and-map (lambda (x) (and (id? x) (not (ellipsis? x r mod))))
                                         #'(key ...))
                                (let ((x (gen-var 'tmp)))
                                  ;; fat finger binding and references to temp variable x
                                  (build-call s
                                              (build-simple-lambda no-source (list 'tmp) #f (list x) '()
                                                                   (gen-syntax-case (build-lexical-reference 'value no-source
                                                                                                             'tmp x)
                                                                                    #'(key ...) #'(m ...)
                                                                                    r
                                                                                    mod))
                                              (list (expand #'val r empty-wrap mod))))
                                (syntax-violation 'syntax-case "invalid literals list" e))))))))

    ;; The portable macroexpand seeds expand-top's mode m with 'e (for
    ;; evaluating) and esew (which stands for "eval syntax expanders
    ;; when") with '(eval).  In Chez Scheme, m is set to 'c instead of e
    ;; if we are compiling a file, and esew is set to
    ;; (eval-syntactic-expanders-when), which defaults to the list
    ;; '(compile load eval).  This means that, by default, top-level
    ;; syntactic definitions are evaluated immediately after they are
    ;; expanded, and the expanded definitions are also residualized into
    ;; the object file if we are compiling a file.
    (set! macroexpand
          (lambda* (x #:optional (m 'e) (esew '(eval)))
            (expand-top-sequence (list x) null-env top-wrap #f m esew
                                 (cons 'hygiene (module-name (current-module))))))

    (set! identifier?
          (lambda (x)
            (nonsymbol-id? x)))

    (set! datum->syntax
          (lambda (id datum)
            (make-syntax-object datum (syntax-object-wrap id)
                                (syntax-object-module id))))

    (set! syntax->datum
          ;; accepts any object, since syntax objects may consist partially
          ;; or entirely of unwrapped, nonsymbolic data
          (lambda (x)
            (strip x empty-wrap)))

    (set! syntax-source
          (lambda (x) (source-annotation x)))

    (set! generate-temporaries
          (lambda (ls)
            (arg-check list? ls 'generate-temporaries)
            (let ((mod (cons 'hygiene (module-name (current-module)))))
              (map (lambda (x)
                     (wrap (module-gensym "t") top-wrap mod))
                   ls))))

    (set! free-identifier=?
          (lambda (x y)
            (arg-check nonsymbol-id? x 'free-identifier=?)
            (arg-check nonsymbol-id? y 'free-identifier=?)
            (free-id=? x y)))

    (set! bound-identifier=?
          (lambda (x y)
            (arg-check nonsymbol-id? x 'bound-identifier=?)
            (arg-check nonsymbol-id? y 'bound-identifier=?)
            (bound-id=? x y)))

    (set! syntax-violation
          (lambda* (who message form #:optional subform)
            (arg-check (lambda (x) (or (not x) (string? x) (symbol? x)))
                       who 'syntax-violation)
            (arg-check string? message 'syntax-violation)
            (throw 'syntax-error who message
                   (or (source-annotation subform)
                       (source-annotation form))
                   (strip form empty-wrap)
                   (and subform (strip subform empty-wrap)))))

    (let ()
      (define (%syntax-module id)
        (arg-check nonsymbol-id? id 'syntax-module)
        (let ((mod (syntax-object-module id)))
          (and (not (equal? mod '(primitive)))
               (cdr mod))))

      (define* (syntax-local-binding id #:key (resolve-syntax-parameters? #t))
        (arg-check nonsymbol-id? id 'syntax-local-binding)
        (with-transformer-environment
         (lambda (e r w s rib mod)
           (define (strip-anti-mark w)
             (let ((ms (wrap-marks w)) (s (wrap-subst w)))
               (if (and (pair? ms) (eq? (car ms) the-anti-mark))
                   ;; output is from original text
                   (make-wrap (cdr ms) (if rib (cons rib (cdr s)) (cdr s)))
                   ;; output introduced by macro
                   (make-wrap ms (if rib (cons rib s) s)))))
           (call-with-values (lambda ()
                               (resolve-identifier
                                (syntax-object-expression id)
                                (strip-anti-mark (syntax-object-wrap id))
                                r
                                (syntax-object-module id)
                                resolve-syntax-parameters?))
             (lambda (type value mod)
               (case type
                 ((lexical) (values 'lexical value))
                 ((macro) (values 'macro value))
                 ((syntax-parameter) (values 'syntax-parameter (car value)))
                 ((syntax) (values 'pattern-variable value))
                 ((displaced-lexical) (values 'displaced-lexical #f))
                 ((global)
                  (if (equal? mod '(primitive))
                      (values 'primitive value)
                      (values 'global (cons value (cdr mod)))))
                 ((ellipsis)
                  (values 'ellipsis
                          (make-syntax-object (syntax-object-expression value)
                                              (anti-mark (syntax-object-wrap value))
                                              (syntax-object-module value))))
                 (else (values 'other #f))))))))

      (define (syntax-locally-bound-identifiers id)
        (arg-check nonsymbol-id? id 'syntax-locally-bound-identifiers)
        (locally-bound-identifiers (syntax-object-wrap id)
                                   (syntax-object-module id)))

      ;; Using define! instead of set! to avoid warnings at
      ;; compile-time, after the variables are stolen away into (system
      ;; syntax).  See the end of boot-9.scm.
      ;;
      (define! '%syntax-module %syntax-module)
      (define! 'syntax-local-binding syntax-local-binding)
      (define! 'syntax-locally-bound-identifiers syntax-locally-bound-identifiers))
    
    ;; $sc-dispatch expects an expression and a pattern.  If the expression
    ;; matches the pattern a list of the matching expressions for each
    ;; "any" is returned.  Otherwise, #f is returned.  (This use of #f will
    ;; not work on r4rs implementations that violate the ieee requirement
    ;; that #f and () be distinct.)

    ;; The expression is matched with the pattern as follows:

    ;; pattern:                           matches:
    ;;   ()                                 empty list
    ;;   any                                anything
    ;;   (<pattern>1 . <pattern>2)          (<pattern>1 . <pattern>2)
    ;;   each-any                           (any*)
    ;;   #(free-id <key>)                   <key> with free-identifier=?
    ;;   #(each <pattern>)                  (<pattern>*)
    ;;   #(each+ p1 (p2_1 ... p2_n) p3)      (p1* (p2_n ... p2_1) . p3)
    ;;   #(vector <pattern>)                (list->vector <pattern>)
    ;;   #(atom <object>)                   <object> with "equal?"

    ;; Vector cops out to pair under assumption that vectors are rare.  If
    ;; not, should convert to:
    ;;   #(vector <pattern>*)               #(<pattern>*)

    (let ()

      (define match-each
        (lambda (e p w mod)
          (cond
           ((pair? e)
            (let ((first (match (car e) p w '() mod)))
              (and first
                   (let ((rest (match-each (cdr e) p w mod)))
                     (and rest (cons first rest))))))
           ((null? e) '())
           ((syntax-object? e)
            (match-each (syntax-object-expression e)
                        p
                        (join-wraps w (syntax-object-wrap e))
                        (syntax-object-module e)))
           (else #f))))

      (define match-each+
        (lambda (e x-pat y-pat z-pat w r mod)
          (let f ((e e) (w w))
            (cond
             ((pair? e)
              (call-with-values (lambda () (f (cdr e) w))
                (lambda (xr* y-pat r)
                  (if r
                      (if (null? y-pat)
                          (let ((xr (match (car e) x-pat w '() mod)))
                            (if xr
                                (values (cons xr xr*) y-pat r)
                                (values #f #f #f)))
                          (values
                           '()
                           (cdr y-pat)
                           (match (car e) (car y-pat) w r mod)))
                      (values #f #f #f)))))
             ((syntax-object? e)
              (f (syntax-object-expression e)
                 (join-wraps w (syntax-object-wrap e))))
             (else
              (values '() y-pat (match e z-pat w r mod)))))))

      (define match-each-any
        (lambda (e w mod)
          (cond
           ((pair? e)
            (let ((l (match-each-any (cdr e) w mod)))
              (and l (cons (wrap (car e) w mod) l))))
           ((null? e) '())
           ((syntax-object? e)
            (match-each-any (syntax-object-expression e)
                            (join-wraps w (syntax-object-wrap e))
                            mod))
           (else #f))))

      (define match-empty
        (lambda (p r)
          (cond
           ((null? p) r)
           ((eq? p '_) r)
           ((eq? p 'any) (cons '() r))
           ((pair? p) (match-empty (car p) (match-empty (cdr p) r)))
           ((eq? p 'each-any) (cons '() r))
           (else
            (case (vector-ref p 0)
              ((each) (match-empty (vector-ref p 1) r))
              ((each+) (match-empty (vector-ref p 1)
                                    (match-empty
                                     (reverse (vector-ref p 2))
                                     (match-empty (vector-ref p 3) r))))
              ((free-id atom) r)
              ((vector) (match-empty (vector-ref p 1) r)))))))

      (define combine
        (lambda (r* r)
          (if (null? (car r*))
              r
              (cons (map car r*) (combine (map cdr r*) r)))))

      (define match*
        (lambda (e p w r mod)
          (cond
           ((null? p) (and (null? e) r))
           ((pair? p)
            (and (pair? e) (match (car e) (car p) w
                             (match (cdr e) (cdr p) w r mod)
                             mod)))
           ((eq? p 'each-any)
            (let ((l (match-each-any e w mod))) (and l (cons l r))))
           (else
            (case (vector-ref p 0)
              ((each)
               (if (null? e)
                   (match-empty (vector-ref p 1) r)
                   (let ((l (match-each e (vector-ref p 1) w mod)))
                     (and l
                          (let collect ((l l))
                            (if (null? (car l))
                                r
                                (cons (map car l) (collect (map cdr l)))))))))
              ((each+)
               (call-with-values
                   (lambda ()
                     (match-each+ e (vector-ref p 1) (vector-ref p 2) (vector-ref p 3) w r mod))
                 (lambda (xr* y-pat r)
                   (and r
                        (null? y-pat)
                        (if (null? xr*)
                            (match-empty (vector-ref p 1) r)
                            (combine xr* r))))))
              ((free-id) (and (id? e) (free-id=? (wrap e w mod) (vector-ref p 1)) r))
              ((atom) (and (equal? (vector-ref p 1) (strip e w)) r))
              ((vector)
               (and (vector? e)
                    (match (vector->list e) (vector-ref p 1) w r mod))))))))

      (define match
        (lambda (e p w r mod)
          (cond
           ((not r) #f)
           ((eq? p '_) r)
           ((eq? p 'any) (cons (wrap e w mod) r))
           ((syntax-object? e)
            (match*
             (syntax-object-expression e)
             p
             (join-wraps w (syntax-object-wrap e))
             r
             (syntax-object-module e)))
           (else (match* e p w r mod)))))

      (set! $sc-dispatch
            (lambda (e p)
              (cond
               ((eq? p 'any) (list e))
               ((eq? p '_) '())
               ((syntax-object? e)
                (match* (syntax-object-expression e)
                        p (syntax-object-wrap e) '() (syntax-object-module e)))
               (else (match* e p empty-wrap '() #f))))))))


(define-syntax with-syntax
   (lambda (x)
      (syntax-case x ()
         ((_ () e1 e2 ...)
          #'(let () e1 e2 ...))
         ((_ ((out in)) e1 e2 ...)
          #'(syntax-case in ()
              (out (let () e1 e2 ...))))
         ((_ ((out in) ...) e1 e2 ...)
          #'(syntax-case (list in ...) ()
              ((out ...) (let () e1 e2 ...)))))))

(define-syntax syntax-error
  (lambda (x)
    (syntax-case x ()
      ;; Extended internal syntax which provides the original form
      ;; as the first operand, for improved error reporting.
      ((_ (keyword . operands) message arg ...)
       (string? (syntax->datum #'message))
       (syntax-violation (syntax->datum #'keyword)
                         (string-join (cons (syntax->datum #'message)
                                            (map (lambda (x)
                                                   (object->string
                                                    (syntax->datum x)))
                                                 #'(arg ...))))
                         (and (syntax->datum #'keyword)
                              #'(keyword . operands))))
      ;; Standard R7RS syntax
      ((_ message arg ...)
       (string? (syntax->datum #'message))
       #'(syntax-error (#f) message arg ...)))))

(define-syntax syntax-rules
  (lambda (xx)
    (define (expand-clause clause)
      ;; Convert a 'syntax-rules' clause into a 'syntax-case' clause.
      (syntax-case clause (syntax-error)
        ;; If the template is a 'syntax-error' form, use the extended
        ;; internal syntax, which adds the original form as the first
        ;; operand for improved error reporting.
        (((keyword . pattern) (syntax-error message arg ...))
         (string? (syntax->datum #'message))
         #'((dummy . pattern) #'(syntax-error (dummy . pattern) message arg ...)))
        ;; Normal case
        (((keyword . pattern) template)
         #'((dummy . pattern) #'template))))
    (define (expand-syntax-rules dots keys docstrings clauses)
      (with-syntax
          (((k ...) keys)
           ((docstring ...) docstrings)
           ((((keyword . pattern) template) ...) clauses)
           ((clause ...) (map expand-clause clauses)))
        (with-syntax
            ((form #'(lambda (x)
                       docstring ...        ; optional docstring
                       #((macro-type . syntax-rules)
                         (patterns pattern ...)) ; embed patterns as procedure metadata
                       (syntax-case x (k ...)
                         clause ...))))
          (if dots
              (with-syntax ((dots dots))
                #'(with-ellipsis dots form))
              #'form))))
    (syntax-case xx ()
      ((_ (k ...) ((keyword . pattern) template) ...)
       (expand-syntax-rules #f #'(k ...) #'() #'(((keyword . pattern) template) ...)))
      ((_ (k ...) docstring ((keyword . pattern) template) ...)
       (string? (syntax->datum #'docstring))
       (expand-syntax-rules #f #'(k ...) #'(docstring) #'(((keyword . pattern) template) ...)))
      ((_ dots (k ...) ((keyword . pattern) template) ...)
       (identifier? #'dots)
       (expand-syntax-rules #'dots #'(k ...) #'() #'(((keyword . pattern) template) ...)))
      ((_ dots (k ...) docstring ((keyword . pattern) template) ...)
       (and (identifier? #'dots) (string? (syntax->datum #'docstring)))
       (expand-syntax-rules #'dots #'(k ...) #'(docstring) #'(((keyword . pattern) template) ...))))))

(define-syntax define-syntax-rule
  (lambda (x)
    (syntax-case x ()
      ((_ (name . pattern) template)
       #'(define-syntax name
           (syntax-rules ()
             ((_ . pattern) template))))
      ((_ (name . pattern) docstring template)
       (string? (syntax->datum #'docstring))
       #'(define-syntax name
           (syntax-rules ()
             docstring
             ((_ . pattern) template)))))))

(define-syntax let*
  (lambda (x)
    (syntax-case x ()
      ((let* ((x v) ...) e1 e2 ...)
       (and-map identifier? #'(x ...))
       (let f ((bindings #'((x v)  ...)))
         (if (null? bindings)
             #'(let () e1 e2 ...)
             (with-syntax ((body (f (cdr bindings)))
                           (binding (car bindings)))
               #'(let (binding) body))))))))

(define-syntax quasiquote
  (let ()
    (define (quasi p lev)
      (syntax-case p (unquote quasiquote)
        ((unquote p)
         (if (= lev 0)
             #'("value" p)
             (quasicons #'("quote" unquote) (quasi #'(p) (- lev 1)))))
        ((quasiquote p) (quasicons #'("quote" quasiquote) (quasi #'(p) (+ lev 1))))
        ((p . q)
         (syntax-case #'p (unquote unquote-splicing)
           ((unquote p ...)
            (if (= lev 0)
                (quasilist* #'(("value" p) ...) (quasi #'q lev))
                (quasicons
                 (quasicons #'("quote" unquote) (quasi #'(p ...) (- lev 1)))
                 (quasi #'q lev))))
           ((unquote-splicing p ...)
            (if (= lev 0)
                (quasiappend #'(("value" p) ...) (quasi #'q lev))
                (quasicons
                 (quasicons #'("quote" unquote-splicing) (quasi #'(p ...) (- lev 1)))
                 (quasi #'q lev))))
           (_ (quasicons (quasi #'p lev) (quasi #'q lev)))))
        (#(x ...) (quasivector (vquasi #'(x ...) lev)))
        (p #'("quote" p))))
    (define (vquasi p lev)
      (syntax-case p ()
        ((p . q)
         (syntax-case #'p (unquote unquote-splicing)
           ((unquote p ...)
            (if (= lev 0)
                (quasilist* #'(("value" p) ...) (vquasi #'q lev))
                (quasicons
                 (quasicons #'("quote" unquote) (quasi #'(p ...) (- lev 1)))
                 (vquasi #'q lev))))
           ((unquote-splicing p ...)
            (if (= lev 0)
                (quasiappend #'(("value" p) ...) (vquasi #'q lev))
                (quasicons
                 (quasicons
                  #'("quote" unquote-splicing)
                  (quasi #'(p ...) (- lev 1)))
                 (vquasi #'q lev))))
           (_ (quasicons (quasi #'p lev) (vquasi #'q lev)))))
        (() #'("quote" ()))))
    (define (quasicons x y)
      (with-syntax ((x x) (y y))
        (syntax-case #'y ()
          (("quote" dy)
           (syntax-case #'x ()
             (("quote" dx) #'("quote" (dx . dy)))
             (_ (if (null? #'dy) #'("list" x) #'("list*" x y)))))
          (("list" . stuff) #'("list" x . stuff))
          (("list*" . stuff) #'("list*" x . stuff))
          (_ #'("list*" x y)))))
    (define (quasiappend x y)
      (syntax-case y ()
        (("quote" ())
         (cond
          ((null? x) #'("quote" ()))
          ((null? (cdr x)) (car x))
          (else (with-syntax (((p ...) x)) #'("append" p ...)))))
        (_
         (cond
          ((null? x) y)
          (else (with-syntax (((p ...) x) (y y)) #'("append" p ... y)))))))
    (define (quasilist* x y)
      (let f ((x x))
        (if (null? x)
            y
            (quasicons (car x) (f (cdr x))))))
    (define (quasivector x)
      (syntax-case x ()
        (("quote" (x ...)) #'("quote" #(x ...)))
        (_
         (let f ((y x) (k (lambda (ls) #`("vector" #,@ls))))
           (syntax-case y ()
             (("quote" (y ...)) (k #'(("quote" y) ...)))
             (("list" y ...) (k #'(y ...)))
             (("list*" y ... z) (f #'z (lambda (ls) (k (append #'(y ...) ls)))))
             (else #`("list->vector" #,x)))))))
    (define (emit x)
      (syntax-case x ()
        (("quote" x) #''x)
        (("list" x ...) #`(list #,@(map emit #'(x ...))))
        ;; could emit list* for 3+ arguments if implementation supports
        ;; list*
        (("list*" x ... y)
         (let f ((x* #'(x ...)))
           (if (null? x*)
               (emit #'y)
               #`(cons #,(emit (car x*)) #,(f (cdr x*))))))
        (("append" x ...) #`(append #,@(map emit #'(x ...))))
        (("vector" x ...) #`(vector #,@(map emit #'(x ...))))
        (("list->vector" x) #`(list->vector #,(emit #'x)))
        (("value" x) #'x)))
    (lambda (x)
      (syntax-case x ()
        ;; convert to intermediate language, combining introduced (but
        ;; not unquoted source) quote expressions where possible and
        ;; choosing optimal construction code otherwise, then emit
        ;; Scheme code corresponding to the intermediate language forms.
        ((_ e) (emit (quasi #'e 0))))))) 

(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn dir k)
        (let* ((p (open-input-file
                   (cond ((absolute-file-name? fn)
                          fn)
                         (dir
                          (in-vicinity dir fn))
                         (else
                          (syntax-violation
                           'include
                           "relative file name only allowed when the include form is in a file"
                           x)))))
               (enc (file-encoding p)))

          ;; Choose the input encoding deterministically.
          (set-port-encoding! p (or enc "UTF-8"))

          (let f ((x (read p))
                  (result '()))
            (if (eof-object? x)
                (begin
                  (close-port p)
                  (reverse result))
                (f (read p)
                   (cons (datum->syntax k x) result)))))))
    (let* ((src (syntax-source x))
           (file (and src (assq-ref src 'filename)))
           (dir (and (string? file) (dirname file))))
      (syntax-case x ()
        ((k filename)
         (let ((fn (syntax->datum #'filename)))
           (with-syntax (((exp ...) (read-file fn dir #'filename)))
             #'(begin exp ...))))))))

(define-syntax include-from-path
  (lambda (x)
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax->datum #'filename)))
         (with-syntax ((fn (datum->syntax
                            #'filename
                            (canonicalize-path
                             (or (%search-load-path fn)
                                 (syntax-violation 'include-from-path
                                                   "file not found in path"
                                                   x #'filename))))))
           #'(include fn)))))))

(define-syntax unquote
  (lambda (x)
    (syntax-violation 'unquote
                      "expression not valid outside of quasiquote"
                      x)))

(define-syntax unquote-splicing
  (lambda (x)
    (syntax-violation 'unquote-splicing
                      "expression not valid outside of quasiquote"
                      x)))

(define (make-variable-transformer proc)
  (if (procedure? proc)
      (let ((trans (lambda (x)
                     #((macro-type . variable-transformer))
                     (proc x))))
        (set-procedure-property! trans 'variable-transformer #t)
        trans)
      (error "variable transformer not a procedure" proc)))

(define-syntax identifier-syntax
  (lambda (xx)
    (syntax-case xx (set!)
      ((_ e)
       #'(lambda (x)
           #((macro-type . identifier-syntax))
           (syntax-case x ()
             (id
              (identifier? #'id)
              #'e)
             ((_ x (... ...))
              #'(e x (... ...))))))
      ((_ (id exp1) ((set! var val) exp2))
       (and (identifier? #'id) (identifier? #'var))
       #'(make-variable-transformer
          (lambda (x)
            #((macro-type . variable-transformer))
            (syntax-case x (set!)
              ((set! var val) #'exp2)
              ((id x (... ...)) #'(exp1 x (... ...)))
              (id (identifier? #'id) #'exp1))))))))

(define-syntax define*
  (lambda (x)
    (syntax-case x ()
      ((_ (id . args) b0 b1 ...)
       #'(define id (lambda* args b0 b1 ...)))
      ((_ id val) (identifier? #'id)
       #'(define id val)))))
