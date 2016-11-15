;;;; (texinfo reflection) -- documenting Scheme as stexinfo
;;;;
;;;; 	Copyright (C) 2009, 2010, 2011  Free Software Foundation, Inc.
;;;;    Copyright (C) 2003,2004,2009  Andy Wingo <wingo at pobox dot com>
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
;;Routines to generare @code{stexi} documentation for objects and
;;modules.
;;
;;Note that in this context, an @dfn{object} is just a value associated
;;with a location. It has nothing to do with GOOPS.
;;
;;; Code:

(define-module (texinfo reflection)
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:use-module (oop goops)
  #:use-module (texinfo)
  #:use-module (texinfo plain-text)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 session)
  #:use-module (ice-9 documentation)
  #:use-module (ice-9 optargs)
  #:use-module ((sxml transform) #:select (pre-post-order))
  #:export (module-stexi-documentation
            script-stexi-documentation
            object-stexi-documentation
            package-stexi-standard-copying
            package-stexi-standard-titlepage
            package-stexi-generic-menu
            package-stexi-standard-menu
            package-stexi-extended-menu
            package-stexi-standard-prologue
            package-stexi-documentation
            package-stexi-documentation-for-include))

;; List for sorting the definitions in a module
(define defs
  '(deftp defcv defivar deftypeivar defop deftypeop defmethod
    deftypemethod defopt defvr defvar deftypevr deftypevar deffn
    deftypefn defmac defspec defun deftypefun))

(define (sort-defs ordering a b)
  (define (def x)
    ;; a and b are lists of the form ((anchor ...) (def* ...)...)
    (cadr x))
  (define (name x)
    (cadr (assq 'name (cdadr (def x)))))
  (define (priority x)
    (list-index defs (car (def x))))
  (define (order x)
    (or (list-index ordering (string->symbol (name x)))
        ;; if the def is not in the list, a big number
        1234567890))
  (define (compare-in-order proc eq? < . args)
    (if (not (eq? (proc a) (proc b)))
        (< (proc a) (proc b))
        (or (null? args)
            (apply compare-in-order args))))
  (compare-in-order order = <
                    priority = <
                    name string=? string<=?))

(define (list*-join l infix restfix)
  (let lp ((in l) (out '()))
    (cond ((null? in) (reverse! out))
          ((symbol? in) (reverse! (cons* in restfix out)))
          (else (lp (cdr in) (if (null? out)
                                 (list (car in))
                                 (cons* (car in) infix out)))))))

(define (process-args args)
  (map (lambda (x) (if (string? x) x (object->string x)))
       (list*-join (or args '())
                   " " " . ")))

(define (get-proc-args proc)
  (cond
   ((procedure-arguments proc)
    => (lambda (args)
         (let ((required-args (assq-ref args 'required))
               (optional-args (assq-ref args 'optional))
               (keyword-args  (assq-ref args 'keyword))
               (rest-arg (assq-ref args 'rest)))
           (process-args 
            (append 
             ;; start with the required args...
             (map symbol->string required-args)

             ;; add any optional args if needed...
             (map (lambda (a)
                    (if (list? a)
                        (format #f "[~a = ~s]" (car a) (cadr a))
                        (format #f "[~a]" a)))
                  optional-args)
                    
             ;; now the keyword args..
             (map (lambda (a)
                    (if (pair? a)
                        (format #f "[~a]" (car a))
                        (format #f "[#:~a]" a)))
                  keyword-args)
                    
             ;; now the rest arg...
             (if rest-arg
                 (list "." (symbol->string rest-arg))
                 '()))))))))

(define (macro-arguments name type transformer)
  (process-args
   (case type
     ((syntax-rules)
      (let ((patterns (procedure-property transformer 'patterns)))
        (if (pair? patterns)
            (car patterns)
            '())))
     ((identifier-syntax)
      '())
     ((defmacro)
      (or (procedure-property transformer 'defmacro-args)
          '()))
     (else
      ;; a procedural (syntax-case) macro. how to document these?
      '()))))

(define (macro-additional-stexi name type transformer)
  (case type
    ((syntax-rules)
     (let ((patterns (procedure-property transformer 'patterns)))
       (if (pair? patterns)
           (map (lambda (x)
                  `(defspecx (% (name ,name)
                                (arguments ,@(process-args x)))))
                (cdr patterns))
           '())))
    (else
     '())))

(define many-space? (make-regexp "[[:space:]][[:space:]][[:space:]]"))
(define initial-space? (make-regexp "^[[:space:]]"))
(define (string->stexi str)
  (or (and (or (not str) (string-null? str))
           '(*fragment*))
      (and (or (string-index str #\@)
               (and (not (regexp-exec many-space? str))
                    (not (regexp-exec initial-space? str))))
           (false-if-exception
            (texi-fragment->stexi str)))
      `(*fragment* (verbatim ,str))))

(define method-formals
  (and (defined? 'method-formals) method-formals))

(define (method-stexi-arguments method)
  (cond
   (method-formals
    (let lp ((formals (method-formals method))
             (specializers (method-specializers method))
             (out '()))
      (define (arg-texinfo formal specializer)
        `(" (" (var ,(symbol->string formal)) " "
          (code ,(symbol->string (class-name specializer))) ")"))
      (cond
       ((null? formals) (reverse out))
       ((pair? formals)
        (lp (cdr formals) (cdr specializers)
            (append (reverse (arg-texinfo (car formals) (car specializers)))
                    out)))
       (else
        (append (reverse out) (arg-texinfo formals specializers)
                (list "..."))))))
   ((method-source method)
    (let lp ((bindings (cadr (method-source method))) (out '()))
      (define (arg-texinfo arg)
        `(" (" (var ,(symbol->string (car arg))) " "
          (code ,(symbol->string (cadr arg))) ")"))
      (cond
       ((null? bindings)
        (reverse out))
       ((not (pair? (car bindings)))
        (append (reverse out) (arg-texinfo bindings) (list "...")))
       (else
        (lp (cdr bindings)
            (append (reverse (arg-texinfo (car bindings))) out))))))
   (else (warn method) '())))

(define* (object-stexi-documentation object #:optional (name "[unknown]")
                                     #:key (force #f))
  (if (symbol? name)
      (set! name (symbol->string name)))
  (let ((stexi ((lambda (x)
                  (cond ((string? x) (string->stexi x))
                        ((and (pair? x) (eq? (car x) '*fragment*)) x)
                        (force `(*fragment*))
                        (else #f)))
                (object-documentation
                 (if (is-a? object <method>)
                     (method-procedure object)
                     object)))))
    (define (make-def type args)
      `(,type (% ,@args) ,@(cdr stexi)))
    (cond
     ((not stexi) #f)
     ;; stexi is now a list, headed by *fragment*.
     ((and (pair? (cdr stexi)) (pair? (cadr stexi))
           (memq (caadr stexi) defs))
      ;; it's already a deffoo.
      stexi)
     ((is-a? object <class>)
      (make-def 'deftp `((name ,name)
                         (category "Class"))))
     ((is-a? object <macro>)
      (let* ((proc (macro-transformer object))
             (type (and proc (procedure-property proc 'macro-type))))
        `(defspec (% (name ,name)
                     (arguments ,@(macro-arguments name type proc)))
           ,@(macro-additional-stexi name type proc)
           ,@(cdr stexi))))
     
     ((is-a? object <procedure>)
      (make-def 'defun `((name ,name)
                         (arguments ,@(get-proc-args object)))))
     ((is-a? object <method>)
      (make-def 'deffn `((category "Method")
                         (name ,name)
                         (arguments ,@(method-stexi-arguments object)))))
     ((is-a? object <generic>)
      `(*fragment*
        ,(make-def 'deffn `((name ,name)
                            (category "Generic")))
        ,@(map
           (lambda (method)
             (object-stexi-documentation method name #:force force))
           (generic-function-methods object))))
     (else
      (make-def 'defvar `((name ,name)))))))

(define (module-name->node-name sym-name)
  (string-join (map symbol->string sym-name) " "))

;; this copied from (ice-9 session); need to find a better way
(define (module-filename name)
  (let* ((name (map symbol->string name))
         (reverse-name (reverse name))
	 (leaf (car reverse-name))
	 (dir-hint-module-name (reverse (cdr reverse-name)))
	 (dir-hint (apply string-append
                          (map (lambda (elt)
                                 (string-append elt "/"))
                               dir-hint-module-name))))
    (%search-load-path (in-vicinity dir-hint leaf))))

(define (read-module name)
  (let ((filename (module-filename name)))
    (if filename
        (let ((port (open-input-file filename)))
          (let lp ((out '()) (form (read port)))
            (if (eof-object? form)
                (reverse out)
                (lp (cons form out) (read port)))))
        '())))

(define (module-export-list sym-name)
  (define (module-form-export-list form)
    (and (pair? form)
         (eq? (car form) 'define-module)
         (equal? (cadr form) sym-name)
         (and=> (memq #:export (cddr form)) cadr)))
  (let lp ((forms (read-module sym-name)))
    (cond ((null? forms) '())
          ((module-form-export-list (car forms)) => identity)
          (else (lp (cdr forms))))))

(define* (module-stexi-documentation sym-name
                                     #:optional %docs-resolver
                                     #:key (docs-resolver
                                            (or %docs-resolver
                                                (lambda (name def) def))))
  "Return documentation for the module named @var{sym-name}. The
documentation will be formatted as @code{stexi}
 (@pxref{texinfo,texinfo})."
  (if %docs-resolver
      (issue-deprecation-warning
       "module-stexi-documentation: use #:docs-resolver instead of a positional argument."))
  (let* ((commentary (and=> (module-commentary sym-name)
                            (lambda (x) (string-trim-both x #\newline))))
         (stexi (string->stexi commentary))
         (node-name (module-name->node-name sym-name))
         (name-str (with-output-to-string
                     (lambda () (display sym-name))))
         (module (resolve-interface sym-name))
         (export-list (module-export-list sym-name)))
    (define (anchor-name sym)
      (string-append node-name " " (symbol->string sym)))
    (define (make-defs)
      (sort!
       (module-map
        (lambda (sym var)
          `((anchor (% (name ,(anchor-name sym))))
            ,@((lambda (x)
                 (if (eq? (car x) '*fragment*)
                     (cdr x)
                     (list x)))
               (if (variable-bound? var)
                   (docs-resolver
                    sym
                    (object-stexi-documentation (variable-ref var) sym
                                                #:force #t))
                   (begin
                     (warn "variable unbound!" sym)
                     `(defvar (% (name ,(symbol->string sym)))
                        "[unbound!]"))))))
        module)
       (lambda (a b) (sort-defs export-list a b))))

    `(texinfo (% (title ,name-str))
              (node (% (name ,node-name)))
              (section "Overview")
              ,@(cdr stexi)
              (section "Usage")
              ,@(apply append! (make-defs)))))

(define (script-stexi-documentation scriptpath)
  "Return documentation for given script. The documentation will be
taken from the script's commentary, and will be returned in the
@code{stexi} format (@pxref{texinfo,texinfo})."
  (let ((commentary (file-commentary scriptpath)))
    `(texinfo (% (title ,(basename scriptpath)))
              (node (% (name ,(basename scriptpath))))
              ,@(if commentary
                    (cdr
                     (string->stexi
                      (string-trim-both commentary #\newline)))
                    '()))))

(cond
 ((defined? 'add-value-help-handler!)
  (add-value-help-handler! 
   (lambda (name value)
     (stexi->plain-text
      (object-stexi-documentation value name #:force #t))))
  (add-name-help-handler!
   (lambda (name)
    (and (list? name)
         (and-map symbol? name)
         (stexi->plain-text (module-stexi-documentation name)))))))

;; we could be dealing with an old (ice-9 session); fondle it to get
;; module-commentary
(define module-commentary (@@ (ice-9 session) module-commentary))

(define (package-stexi-standard-copying name version updated years
                                        copyright-holder permissions)
  "Create a standard texinfo @code{copying} section.

@var{years} is a list of years (as integers) in which the modules
being documented were released. All other arguments are strings."
  `(copying
    (para "This manual is for " ,name
          " (version " ,version ", updated " ,updated ")")
    (para "Copyright " ,(string-join (map number->string years) ",")
          " " ,copyright-holder)
    (quotation
     (para ,permissions))))

(define (package-stexi-standard-titlepage name version updated authors)
  "Create a standard GNU title page.

@var{authors} is a list of @code{(@var{name} . @var{email})}
pairs. All other arguments are strings.

Here is an example of the usage of this procedure:

@smallexample
 (package-stexi-standard-titlepage
  \"Foolib\"
  \"3.2\"
  \"26 September 2006\"
  '((\"Alyssa P Hacker\" . \"alyssa@@example.com\"))
  '(2004 2005 2006)
  \"Free Software Foundation, Inc.\"
  \"Standard GPL permissions blurb goes here\")
@end smallexample
"
  `(;(setchapternewpage (% (all "odd"))) makes manuals too long
    (titlepage
     (title ,name)
     (subtitle "version " ,version ", updated " ,updated)
     ,@(map (lambda (pair)
              `(author ,(car pair)
                       " (" (email ,(cdr pair)) ")"))
            authors)
     (page)
     (vskip (% (all "0pt plus 1filll")))
     (insertcopying))))

(define (package-stexi-generic-menu name entries)
  "Create a menu from a generic alist of entries, the car of which
should be the node name, and the cdr the description. As an exception,
an entry of @code{#f} will produce a separator."
  (define (make-entry node description)
    `("* " ,node "::"
      ,(make-string (max (- 21 (string-length node)) 2) #\space)
      ,@description "\n"))
  `((ifnottex
     (node (% (name "Top")))
     (top (% (title ,name)))
     (insertcopying)
     (menu
      ,@(apply
         append
         (map
          (lambda (entry)
            (if entry
                (make-entry (car entry) (cdr entry))
                '("\n")))
          entries))))
    (iftex
     (shortcontents))))


(define (package-stexi-standard-menu name modules module-descriptions
                                     extra-entries)
  "Create a standard top node and menu, suitable for processing
by makeinfo."
  (package-stexi-generic-menu
   name
   (let ((module-entries (map cons
                              (map module-name->node-name modules)
                              module-descriptions))
         (separate-sections (lambda (x) (if (null? x) x (cons #f x)))))
     `(,@module-entries
       ,@(separate-sections extra-entries)))))

(define (package-stexi-extended-menu name module-pairs script-pairs
                                     extra-entries)
  "Create an \"extended\" menu, like the standard menu but with a
section for scripts."
  (package-stexi-generic-menu
   name
   (let ((module-entries (map cons
                              (map module-name->node-name
                                   (map car module-pairs))
                              (map cdr module-pairs)))
         (script-entries (map cons
                              (map basename (map car script-pairs))
                              (map cdr script-pairs)))
         (separate-sections (lambda (x) (if (null? x) x (cons #f x)))))
     `(,@module-entries
       ,@(separate-sections script-entries)
       ,@(separate-sections extra-entries)))))

(define (package-stexi-standard-prologue name filename category
                                         description copying titlepage
                                         menu)
  "Create a standard prologue, suitable for later serialization
to texinfo and .info creation with makeinfo.

Returns a list of stexinfo forms suitable for passing to
@code{package-stexi-documentation} as the prologue. @xref{texinfo
reflection package-stexi-documentation}, @ref{texinfo reflection
package-stexi-standard-titlepage,package-stexi-standard-titlepage},
@ref{texinfo reflection
package-stexi-standard-copying,package-stexi-standard-copying},
and @ref{texinfo reflection
package-stexi-standard-menu,package-stexi-standard-menu}."
  `(,copying
    (dircategory (% (category ,category)))
    (direntry
     "* " ,name ": (" ,filename ").  " ,description ".")
    ,@titlepage
    ,@menu))

(define (stexi->chapter stexi)
  (pre-post-order
   stexi
   `((texinfo . ,(lambda (tag attrs node . body)
                   `(,node
                     (chapter ,@(assq-ref (cdr attrs) 'title))
                     ,@body)))
     (*text* . ,(lambda (tag text) text))
     (*default* . ,(lambda args args)))))

(define* (package-stexi-documentation modules name filename
                                      prologue epilogue
                                      #:key
                                      (module-stexi-documentation-args
                                       '())
                                      (scripts '()))
  "Create stexi documentation for a @dfn{package}, where a
package is a set of modules that is released together.

@var{modules} is expected to be a list of module names, where a
module name is a list of symbols. The stexi that is returned will
be titled @var{name} and a texinfo filename of @var{filename}.

@var{prologue} and @var{epilogue} are lists of stexi forms that
will be spliced into the output document before and after the
generated modules documentation, respectively.
@xref{texinfo reflection package-stexi-standard-prologue}, to
create a conventional GNU texinfo prologue.

@var{module-stexi-documentation-args} is an optional argument that, if
given, will be added to the argument list when
@code{module-texi-documentation} is called. For example, it might be
useful to define a @code{#:docs-resolver} argument."
  (define (verify-modules-list l)
    (define (all pred l)
      (and (pred (car l))
           (or (null? (cdr l)) (all pred (cdr l)))))
    (false-if-exception
     (all (lambda (x) (all symbol? x)) modules)))
  (if (not (verify-modules-list modules))
      (error "expected modules to be a list of a list of symbols"
             modules))

  `(texinfo
    (% (title ,name)
       (filename ,filename))
    ,@prologue
    ,@(append-map (lambda (mod)
                    (stexi->chapter
                     (apply module-stexi-documentation
                            mod module-stexi-documentation-args)))
                  modules)
    ,@(append-map (lambda (script)
                    (stexi->chapter
                     (script-stexi-documentation script)))
                  scripts)
    ,@epilogue))

(define* (package-stexi-documentation-for-include modules module-descriptions
                                                  #:key
                                                  (module-stexi-documentation-args '()))
  "Create stexi documentation for a @dfn{package}, where a
package is a set of modules that is released together.

@var{modules} is expected to be a list of module names, where a
module name is a list of symbols. Returns an stexinfo fragment.

Unlike @code{package-stexi-documentation}, this function simply produces
a menu and the module documentations instead of producing a full texinfo
document. This can be useful if you write part of your manual by hand,
and just use @code{@@include} to pull in the automatically generated
parts.

@var{module-stexi-documentation-args} is an optional argument that, if
given, will be added to the argument list when
@code{module-texi-documentation} is called. For example, it might be
useful to define a @code{#:docs-resolver} argument."
  (define (make-entry node description)
    `("* " ,node "::"
      ,(make-string (max (- 21 (string-length node)) 2) #\space)
      ,@description "\n"))
  `(*fragment*
    (menu
     ,@(append-map (lambda (modname desc)
                     (make-entry (module-name->node-name modname)
                                 desc))
                   modules
                   module-descriptions))
    ,@(append-map (lambda (modname)
                    (stexi->chapter
                     (apply module-stexi-documentation 
                            modname
                            module-stexi-documentation-args)))
                  modules)))

;;; arch-tag: bbe2bc03-e16d-4a9e-87b9-55225dc9836c
