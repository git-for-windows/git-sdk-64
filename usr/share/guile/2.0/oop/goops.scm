;;; installed-scm-file

;;;; Copyright (C) 1998,1999,2000,2001,2002, 2003, 2006, 2009, 2010, 2011, 2014, 2015 Free Software Foundation, Inc.
;;;; Copyright (C) 1993-1998 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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


;;;;
;;;; This file was based upon stklos.stk from the STk distribution
;;;; version 4.0.1 by Erick Gallesio <eg@unice.fr>.
;;;;

(define-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (oop goops util)
  #:export-syntax (define-class class standard-define-class
                   define-generic define-accessor define-method
                   define-extended-generic define-extended-generics
                   method)
  #:export (is-a? class-of
           ensure-metaclass ensure-metaclass-with-supers
	   make-class
	   make-generic ensure-generic
	   make-extended-generic
	   make-accessor ensure-accessor
	   add-method!
	   class-slot-ref class-slot-set! slot-unbound slot-missing 
	   slot-definition-name  slot-definition-options
	   slot-definition-allocation
	   slot-definition-getter slot-definition-setter
	   slot-definition-accessor
	   slot-definition-init-value slot-definition-init-form
	   slot-definition-init-thunk slot-definition-init-keyword 
	   slot-init-function class-slot-definition
	   method-source
	   compute-cpl compute-std-cpl compute-get-n-set compute-slots
	   compute-getter-method compute-setter-method
	   allocate-instance initialize make-instance make
	   no-next-method  no-applicable-method no-method
	   change-class update-instance-for-different-class
	   shallow-clone deep-clone
	   class-redefinition
	   apply-generic apply-method apply-methods
	   compute-applicable-methods %compute-applicable-methods
	   method-more-specific? sort-applicable-methods
	   class-subclasses class-methods
	   goops-error
	   min-fixnum max-fixnum
	   ;;; *fixme* Should go into goops.c
	   instance?  slot-ref-using-class
	   slot-set-using-class! slot-bound-using-class?
	   slot-exists-using-class? slot-ref slot-set! slot-bound?
	   class-name class-direct-supers class-direct-subclasses
	   class-direct-methods class-direct-slots class-precedence-list
	   class-slots
	   generic-function-name
	   generic-function-methods method-generic-function
	   method-specializers method-formals
	   primitive-generic-generic enable-primitive-generic!
	   method-procedure accessor-method-slot-definition
	   slot-exists? make find-method get-keyword))

(define *goops-module* (current-module))

;; First initialize the builtin part of GOOPS
(eval-when (expand load eval)
  (%init-goops-builtins))

(eval-when (expand load eval)
  (use-modules ((language tree-il primitives) :select (add-interesting-primitive!)))
  (add-interesting-primitive! 'class-of))

;; Then load the rest of GOOPS
(use-modules (oop goops dispatch))

;;;
;;; Compiling next methods into method bodies
;;;

;;; So, for the reader: there basic idea is that, given that the
;;; semantics of `next-method' depend on the concrete types being
;;; dispatched, why not compile a specific procedure to handle each type
;;; combination that we see at runtime.
;;;
;;; In theory we can do much better than a bytecode compilation, because
;;; we know the *exact* types of the arguments. It's ideal for native
;;; compilation. A task for the future.
;;;
;;; I think this whole generic application mess would benefit from a
;;; strict MOP.

(define (compute-cmethod methods types)
  (match methods
    ((method . methods)
     (let ((make-procedure (slot-ref method 'make-procedure)))
       (if make-procedure
           (make-procedure
            (if (null? methods)
                (lambda args
                  (no-next-method (method-generic-function method) args))
                (compute-cmethod methods types)))
           (method-procedure method))))))


(eval-when (expand load eval)
  (define min-fixnum (- (expt 2 29)))
  (define max-fixnum (- (expt 2 29) 1)))

;;
;; goops-error
;;
(define (goops-error format-string . args)
  (scm-error 'goops-error #f format-string args '()))

;;
;; is-a?
;;
(define (is-a? obj class)
  (and (memq class (class-precedence-list (class-of obj))) #t))


;;;
;;; {Meta classes}
;;;

(define ensure-metaclass-with-supers
  (let ((table-of-metas '()))
    (lambda (meta-supers)
      (let ((entry (assoc meta-supers table-of-metas)))
	(if entry
	    ;; Found a previously created metaclass
	    (cdr entry)
	    ;; Create a new meta-class which inherit from "meta-supers"
	    (let ((new (make <class> #:dsupers meta-supers
			             #:slots   '()
				     #:name   (gensym "metaclass"))))
	      (set! table-of-metas (cons (cons meta-supers new) table-of-metas))
	      new))))))

(define (ensure-metaclass supers)
  (if (null? supers)
      <class>
      (let* ((all-metas (map (lambda (x) (class-of x)) supers))
	     (all-cpls  (append-map (lambda (m)
                                      (cdr (class-precedence-list m))) 
                                    all-metas))
	     (needed-metas '()))
	;; Find the most specific metaclasses.  The new metaclass will be
	;; a subclass of these.
	(for-each
	 (lambda (meta)
	   (if (and (not (member meta all-cpls))
		      (not (member meta needed-metas)))
	     (set! needed-metas (append needed-metas (list meta)))))
	 all-metas)
	;; Now return a subclass of the metaclasses we found.
	(if (null? (cdr needed-metas))
	    (car needed-metas)  ; If there's only one, just use it.
	    (ensure-metaclass-with-supers needed-metas)))))

;;;
;;; {Classes}
;;;

;;; (define-class NAME (SUPER ...) SLOT-DEFINITION ... OPTION ...)
;;;
;;;   SLOT-DEFINITION ::= SLOT-NAME | (SLOT-NAME OPTION ...)
;;;   OPTION ::= KEYWORD VALUE
;;;

(define (kw-do-map mapper f kwargs)
  (define (keywords l)
    (cond
     ((null? l) '())
     ((or (null? (cdr l)) (not (keyword? (car l))))
      (goops-error "malformed keyword arguments: ~a" kwargs))
     (else (cons (car l) (keywords (cddr l))))))
  (define (args l)
    (if (null? l) '() (cons (cadr l) (args (cddr l)))))
  ;; let* to check keywords first
  (let* ((k (keywords kwargs))
         (a (args kwargs)))
    (mapper f k a)))

(define (make-class supers slots . options)
  (let* ((name (get-keyword #:name options (make-unbound)))
         (supers (if (not (or-map (lambda (class)
                                    (memq <object>
                                          (class-precedence-list class)))
                                  supers))
                     (append supers (list <object>))
                     supers))
         (metaclass (or (get-keyword #:metaclass options #f)
                        (ensure-metaclass supers))))

    ;; Verify that all direct slots are different and that we don't inherit
    ;; several time from the same class
    (let ((tmp1 (find-duplicate supers))
          (tmp2 (find-duplicate (map slot-definition-name slots))))
      (if tmp1
          (goops-error "make-class: super class ~S is duplicate in class ~S"
                       tmp1 name))
      (if tmp2
          (goops-error "make-class: slot ~S is duplicate in class ~S"
                       tmp2 name)))

    ;; Everything seems correct, build the class
    (apply make metaclass
           #:dsupers supers
           #:slots slots 
           #:name name
           options)))

;;; (class (SUPER ...) SLOT-DEFINITION ... OPTION ...)
;;;
;;;   SLOT-DEFINITION ::= SLOT-NAME | (SLOT-NAME OPTION ...)
;;;   OPTION ::= KEYWORD VALUE
;;;
(define-macro (class supers . slots)
  (define (make-slot-definition-forms slots)
    (map
     (lambda (def)
       (cond
        ((pair? def)
         `(list ',(car def)
                ,@(kw-do-map append-map
                             (lambda (kw arg)
                               (case kw
                                 ((#:init-form)
                                  `(#:init-form ',arg
                                    #:init-thunk (lambda () ,arg)))
                                 (else (list kw arg))))
                             (cdr def))))
        (else
         `(list ',def))))
     slots))
  (if (not (list? supers))
      (goops-error "malformed superclass list: ~S" supers))
  (let ((slots (take-while (lambda (x) (not (keyword? x))) slots))
        (options (or (find-tail keyword? slots) '())))
    `(make-class
      ;; evaluate super class variables
      (list ,@supers)
      ;; evaluate slot definitions, except the slot name!
      (list ,@(make-slot-definition-forms slots))
      ;; evaluate class options
      ,@options)))

(define-syntax define-class-pre-definition
  (lambda (x)
    (syntax-case x ()
      ((_ (k arg rest ...) out ...)
       (keyword? (syntax->datum #'k))
       (case (syntax->datum #'k)
         ((#:getter #:setter)
          #'(define-class-pre-definition (rest ...)
              out ...
              (if (or (not (defined? 'arg))
                      (not (is-a? arg <generic>)))
                  (toplevel-define!
                   'arg
                   (ensure-generic (if (defined? 'arg) arg #f) 'arg)))))
         ((#:accessor)
          #'(define-class-pre-definition (rest ...)
              out ...
              (if (or (not (defined? 'arg))
                      (not (is-a? arg <accessor>)))
                  (toplevel-define!
                   'arg
                   (ensure-accessor (if (defined? 'arg) arg #f) 'arg)))))
         (else
          #'(define-class-pre-definition (rest ...) out ...))))
      ((_ () out ...)
       #'(begin out ...)))))
       
;; Some slot options require extra definitions to be made. In
;; particular, we want to make sure that the generic function objects
;; which represent accessors exist before `make-class' tries to add
;; methods to them.
(define-syntax define-class-pre-definitions
  (lambda (x)
    (syntax-case x ()
      ((_ () out ...)
       #'(begin out ...))
      ((_ (slot rest ...) out ...)
       (keyword? (syntax->datum #'slot))
       #'(begin out ...))
      ((_ (slot rest ...) out ...)
       (identifier? #'slot)
       #'(define-class-pre-definitions (rest ...)
         out ...))
      ((_ ((slotname slotopt ...) rest ...) out ...)
       #'(define-class-pre-definitions (rest ...) 
         out ... (define-class-pre-definition (slotopt ...)))))))

(define-syntax-rule (define-class name supers slot ...)
  (begin
    (define-class-pre-definitions (slot ...))
    (if (and (defined? 'name)
             (is-a? name <class>)
             (memq <object> (class-precedence-list name)))
        (class-redefinition name
                            (class supers slot ... #:name 'name))
        (toplevel-define! 'name (class supers slot ... #:name 'name)))))
       
(define-syntax-rule (standard-define-class arg ...)
  (define-class arg ...))

;;;
;;; {Generic functions and accessors}
;;;

;; Apparently the desired semantics are that we extend previous
;; procedural definitions, but that if `name' was already a generic, we
;; overwrite its definition.
(define-macro (define-generic name)
  (if (not (symbol? name))
      (goops-error "bad generic function name: ~S" name))
  `(define ,name
     (if (and (defined? ',name) (is-a? ,name <generic>))
         (make <generic> #:name ',name)
         (ensure-generic (if (defined? ',name) ,name #f) ',name))))

(define-macro (define-extended-generic name val)
  (if (not (symbol? name))
      (goops-error "bad generic function name: ~S" name))
  `(define ,name (make-extended-generic ,val ',name)))

(define-macro (define-extended-generics names . args)
  (let ((prefixes (get-keyword #:prefix args #f)))
    (if prefixes
        `(begin
           ,@(map (lambda (name)
                    `(define-extended-generic ,name
                       (list ,@(map (lambda (prefix)
                                      (symbol-append prefix name))
                                    prefixes))))
                  names))
        (goops-error "no prefixes supplied"))))

(define* (make-generic #:optional name)
  (make <generic> #:name name))

(define* (make-extended-generic gfs #:optional name)
  (let* ((gfs (if (list? gfs) gfs (list gfs)))
	 (gws? (any (lambda (gf) (is-a? gf <generic-with-setter>)) gfs)))
    (let ((ans (if gws?
		   (let* ((sname (and name (make-setter-name name)))
			  (setters
			   (append-map (lambda (gf)
					 (if (is-a? gf <generic-with-setter>)
					     (list (ensure-generic (setter gf)
								   sname))
					     '()))
				       gfs))
			  (es (make <extended-generic-with-setter>
				#:name name
				#:extends gfs
				#:setter (make <extended-generic>
					   #:name sname
					   #:extends setters))))
		     (extended-by! setters (setter es))
		     es)
		   (make <extended-generic>
		     #:name name
		     #:extends gfs))))
      (extended-by! gfs ans)
      ans)))

(define (extended-by! gfs eg)
  (for-each (lambda (gf)
	      (slot-set! gf 'extended-by
			 (cons eg (slot-ref gf 'extended-by))))
	    gfs)
  (invalidate-method-cache! eg))

(define (not-extended-by! gfs eg)
  (for-each (lambda (gf)
	      (slot-set! gf 'extended-by
			 (delq! eg (slot-ref gf 'extended-by))))
	    gfs)
  (invalidate-method-cache! eg))

(define* (ensure-generic old-definition #:optional name)
  (cond ((is-a? old-definition <generic>) old-definition)
        ((procedure-with-setter? old-definition)
         (make <generic-with-setter>
           #:name name
           #:default (procedure old-definition)
           #:setter (setter old-definition)))
        ((procedure? old-definition)
         (if (generic-capability? old-definition) old-definition
             (make <generic> #:name name #:default old-definition)))
        (else (make <generic> #:name name))))

;; same semantics as <generic>
(define-syntax-rule (define-accessor name)
  (define name
    (cond ((not (defined? 'name))  (ensure-accessor #f 'name))
          ((is-a? name <accessor>) (make <accessor> #:name 'name))
          (else                    (ensure-accessor name 'name)))))

(define (make-setter-name name)
  (string->symbol (string-append "setter:" (symbol->string name))))

(define* (make-accessor #:optional name)
  (make <accessor>
    #:name name
    #:setter (make <generic>
               #:name (and name (make-setter-name name)))))

(define* (ensure-accessor proc #:optional name)
  (cond ((and (is-a? proc <accessor>)
              (is-a? (setter proc) <generic>))
         proc)
        ((is-a? proc <generic-with-setter>)
         (upgrade-accessor proc (setter proc)))
        ((is-a? proc <generic>)
         (upgrade-accessor proc (make-generic name)))
        ((procedure-with-setter? proc)
         (make <accessor>
           #:name name
           #:default (procedure proc)
           #:setter (ensure-generic (setter proc) name)))
        ((procedure? proc)
         (ensure-accessor (if (generic-capability? proc)
                              (make <generic> #:name name #:default proc)
                              (ensure-generic proc name))
                          name))
        (else
         (make-accessor name))))

(define (upgrade-accessor generic setter)
  (let ((methods (slot-ref generic 'methods))
	(gws (make (if (is-a? generic <extended-generic>)
		       <extended-generic-with-setter>
		       <accessor>)
		   #:name (generic-function-name generic)
		   #:extended-by (slot-ref generic 'extended-by)
		   #:setter setter)))
    (if (is-a? generic <extended-generic>)
	(let ((gfs (slot-ref generic 'extends)))
	  (not-extended-by! gfs generic)
	  (slot-set! gws 'extends gfs)
	  (extended-by! gfs gws)))
    ;; Steal old methods
    (for-each (lambda (method)
		(slot-set! method 'generic-function gws))
	      methods)
    (slot-set! gws 'methods methods)
    (invalidate-method-cache! gws)
    gws))

;;;
;;; {Methods}
;;;

(define (toplevel-define! name val)
  (module-define! (current-module) name val))

(define-syntax define-method
  (syntax-rules (setter)
    ((_ ((setter name) . args) body ...)
     (begin
       (if (or (not (defined? 'name))
               (not (is-a? name <accessor>)))
           (toplevel-define! 'name
                             (ensure-accessor
                              (if (defined? 'name) name #f) 'name)))
       (add-method! (setter name) (method args body ...))))
    ((_ (name . args) body ...)
     (begin
       ;; FIXME: this code is how it always was, but it's quite cracky:
       ;; it will only define the generic function if it was undefined
       ;; before (ok), or *was defined to #f*. The latter is crack. But
       ;; there are bootstrap issues about fixing this -- change it to
       ;; (is-a? name <generic>) and see.
       (if (or (not (defined? 'name))
               (not name))
           (toplevel-define! 'name (make <generic> #:name 'name)))
       (add-method! name (method args body ...))))))

(define-syntax method
  (lambda (x)
    (define (parse-args args)
      (let lp ((ls args) (formals '()) (specializers '()))
        (syntax-case ls ()
          (((f s) . rest)
           (and (identifier? #'f) (identifier? #'s))
           (lp #'rest
               (cons #'f formals)
               (cons #'s specializers)))
          ((f . rest)
           (identifier? #'f)
           (lp #'rest
               (cons #'f formals)
               (cons #'<top> specializers)))
          (()
           (list (reverse formals)
                 (reverse (cons #''() specializers))))
          (tail
           (identifier? #'tail)
           (list (append (reverse formals) #'tail)
                 (reverse (cons #'<top> specializers)))))))

    (define (find-free-id exp referent)
      (syntax-case exp ()
        ((x . y)
         (or (find-free-id #'x referent)
             (find-free-id #'y referent)))
        (x
         (identifier? #'x)
         (let ((id (datum->syntax #'x referent)))
           (and (free-identifier=? #'x id) id)))
        (_ #f)))

    (define (compute-procedure formals body)
      (syntax-case body ()
        ((body0 ...)
         (with-syntax ((formals formals))
           #'(lambda formals body0 ...)))))

    (define (->proper args)
      (let lp ((ls args) (out '()))
        (syntax-case ls ()
          ((x . xs)        (lp #'xs (cons #'x out)))
          (()              (reverse out))
          (tail            (reverse (cons #'tail out))))))

    (define (compute-make-procedure formals body next-method)
      (syntax-case body ()
        ((body ...)
         (with-syntax ((next-method next-method))
           (syntax-case formals ()
             ((formal ...)
              #'(lambda (real-next-method)
                  (lambda (formal ...)
                    (let ((next-method (lambda args
                                         (if (null? args)
                                             (real-next-method formal ...)
                                             (apply real-next-method args)))))
                      body ...))))
             (formals
              (with-syntax (((formal ...) (->proper #'formals)))
                #'(lambda (real-next-method)
                    (lambda formals
                      (let ((next-method (lambda args
                                           (if (null? args)
                                               (apply real-next-method formal ...)
                                               (apply real-next-method args)))))
                        body ...))))))))))

    (define (compute-procedures formals body)
      ;; So, our use of this is broken, because it operates on the
      ;; pre-expansion source code. It's equivalent to just searching
      ;; for referent in the datums. Ah well.
      (let ((id (find-free-id body 'next-method)))
        (if id
            ;; return a make-procedure
            (values #'#f
                    (compute-make-procedure formals body id))
            (values (compute-procedure formals body)
                    #'#f))))

    (syntax-case x ()
      ((_ args) #'(method args (if #f #f)))
      ((_ args body0 body1 ...)
       (with-syntax (((formals (specializer ...)) (parse-args #'args)))
         (call-with-values
             (lambda ()
               (compute-procedures #'formals #'(body0 body1 ...)))
           (lambda (procedure make-procedure)
             (with-syntax ((procedure procedure)
                           (make-procedure make-procedure))
               #'(make <method>
                   #:specializers (cons* specializer ...)
                   #:formals 'formals
                   #:body '(body0 body1 ...)
                   #:make-procedure make-procedure
                   #:procedure procedure)))))))))

;;;
;;; {add-method!}
;;;

(define (add-method-in-classes! m)
  ;; Add method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
	       (let ((dm (class-direct-methods x)))
		 (if (not (memq m dm))
		     (slot-set! x 'direct-methods (cons m dm)))))
	     (method-specializers m)))

(define (remove-method-in-classes! m)
  ;; Remove method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
	       (slot-set! x
			  'direct-methods
			  (delv! m (class-direct-methods x))))
	     (method-specializers m)))

(define (compute-new-list-of-methods gf new)
  (let ((new-spec (method-specializers new))
	(methods  (slot-ref gf 'methods)))
    (let loop ((l methods))
      (if (null? l)
	  (cons new methods)
	  (if (equal? (method-specializers (car l)) new-spec)
	      (begin 
		;; This spec. list already exists. Remove old method from dependents
		(remove-method-in-classes! (car l))
		(set-car! l new) 
		methods)
	      (loop (cdr l)))))))

(define (method-n-specializers m)
  (length* (slot-ref m 'specializers)))

(define (calculate-n-specialized gf)
  (fold (lambda (m n) (max n (method-n-specializers m)))
        0
        (generic-function-methods gf)))

(define (invalidate-method-cache! gf)
  (%invalidate-method-cache! gf)
  (slot-set! gf 'n-specialized (calculate-n-specialized gf))
  (for-each (lambda (gf) (invalidate-method-cache! gf))
            (slot-ref gf 'extended-by)))

(define internal-add-method!
  (method ((gf <generic>) (m <method>))
    (slot-set! m  'generic-function gf)
    (slot-set! gf 'methods (compute-new-list-of-methods gf m))
    (invalidate-method-cache! gf)
    (add-method-in-classes! m)
    *unspecified*))

(define-generic add-method!)

((method-procedure internal-add-method!) add-method! internal-add-method!)

(define-method (add-method! (proc <procedure>) (m <method>))
  (if (generic-capability? proc)
      (begin
	(enable-primitive-generic! proc)
	(add-method! proc m))
      (next-method)))

(define-method (add-method! (pg <primitive-generic>) (m <method>))
  (add-method! (primitive-generic-generic pg) m))

(define-method (add-method! obj (m <method>))
  (goops-error "~S is not a valid generic function" obj))

;;;
;;; {Access to meta objects}
;;;

;;;
;;; Methods
;;;
(define-method (method-source (m <method>))
  (let* ((spec (map* class-name (slot-ref m 'specializers)))
	 (src (procedure-source (slot-ref m 'procedure))))
    (and src
         (let ((args (cadr src))
               (body (cddr src)))
           (cons 'method
                 (cons (map* list args spec)
                       body))))))

(define-method (method-formals (m <method>))
  (slot-ref m 'formals))

;;;
;;; Slots
;;;
(define slot-definition-name car)

(define slot-definition-options cdr)

(define (slot-definition-allocation s)
  (get-keyword #:allocation (cdr s) #:instance))

(define (slot-definition-getter s)
  (get-keyword #:getter (cdr s) #f))

(define (slot-definition-setter s)
  (get-keyword #:setter (cdr s) #f))

(define (slot-definition-accessor s)
  (get-keyword #:accessor (cdr s) #f))

(define (slot-definition-init-value s)
  ;; can be #f, so we can't use #f as non-value
  (get-keyword #:init-value (cdr s) (make-unbound)))

(define (slot-definition-init-form s)
  (get-keyword #:init-form (cdr s) (make-unbound)))

(define (slot-definition-init-thunk s)
  (get-keyword #:init-thunk (cdr s) #f))

(define (slot-definition-init-keyword s)
  (get-keyword #:init-keyword (cdr s) #f))

(define (class-slot-definition class slot-name)
  (assq slot-name (class-slots class)))

(define (slot-init-function class slot-name)
  (cadr (assq slot-name (slot-ref class 'getters-n-setters))))

(define (accessor-method-slot-definition obj)
  "Return the slot definition of the accessor @var{obj}."
  (slot-ref obj 'slot-definition))


;;;
;;; {Standard methods used by the C runtime}
;;;

;;; Methods to compare objects
;;;

;; Have to do this in a strange order because equal? is used in the
;; add-method! implementation; we need to make sure that when the
;; primitive is extended, that the generic has a method. =
(define g-equal? (make-generic 'equal?))
;; When this generic gets called, we will have already checked eq? and
;; eqv? -- the purpose of this generic is to extend equality. So by
;; default, there is no extension, thus the #f return.
(add-method! g-equal? (method (x y) #f)) 
(set-primitive-generic! equal? g-equal?)

;;;
;;; methods to display/write an object
;;;

;     Code for writing objects must test that the slots they use are
;     bound. Otherwise a slot-unbound method will be called and will 
;     conduct to an infinite loop.

;; Write
(define (display-address o file)
  (display (number->string (object-address o) 16) file))

(define-method (write o file)
  (display "#<instance " file)
  (display-address o file)
  (display #\> file))

(define write-object (primitive-generic-generic write))

(define-method (write (o <object>) file)
  (let ((class (class-of o)))
    (if (slot-bound? class 'name)
	(begin
	  (display "#<" file)
	  (display (class-name class) file)
	  (display #\space file)
	  (display-address o file)
	  (display #\> file))
	(next-method))))

(define-method (write (class <class>) file)
  (let ((meta (class-of class)))
    (if (and (slot-bound? class 'name)
	     (slot-bound? meta 'name))
	(begin
	  (display "#<" file)
	  (display (class-name meta) file)
	  (display #\space file)
	  (display (class-name class) file)
	  (display #\space file)
	  (display-address class file)
	  (display #\> file))
	(next-method))))

(define-method (write (gf <generic>) file)
  (let ((meta (class-of gf)))
    (if (and (slot-bound? meta 'name)
	     (slot-bound? gf 'methods))
	(begin
	  (display "#<" file)
	  (display (class-name meta) file)
	  (let ((name (generic-function-name gf)))
	    (if name
		(begin
		  (display #\space file)
		  (display name file))))
	  (display " (" file)
	  (display (length (generic-function-methods gf)) file)
	  (display ")>" file))
	(next-method))))

(define-method (write (o <method>) file)
  (let ((meta (class-of o)))
    (if (and (slot-bound? meta 'name)
	     (slot-bound? o 'specializers))
	(begin
	  (display "#<" file)
	  (display (class-name meta) file)
	  (display #\space file)
	  (display (map* (lambda (spec)
			   (if (slot-bound? spec 'name)
			       (slot-ref spec 'name)
			       spec))
			 (method-specializers o))
		   file)
	  (display #\space file)
	  (display-address o file)
	  (display #\> file))
	(next-method))))

;; Display (do the same thing as write by default)
(define-method (display o file) 
  (write-object o file))

;;;
;;; Handling of duplicate bindings in the module system
;;;

(define-method (merge-generics (module <module>)
			       (name <symbol>)
			       (int1 <module>)
			       (val1 <top>)
			       (int2 <module>)
			       (val2 <top>)
			       (var <top>)
			       (val <top>))
  #f)

(define-method (merge-generics (module <module>)
			       (name <symbol>)
			       (int1 <module>)
			       (val1 <generic>)
			       (int2 <module>)
			       (val2 <generic>)
			       (var <top>)
			       (val <boolean>))
  (and (not (eq? val1 val2))
       (make-variable (make-extended-generic (list val2 val1) name))))

(define-method (merge-generics (module <module>)
			       (name <symbol>)
			       (int1 <module>)
			       (val1 <generic>)
			       (int2 <module>)
			       (val2 <generic>)
			       (var <top>)
			       (gf <extended-generic>))
  (and (not (memq val2 (slot-ref gf 'extends)))
       (begin
	 (slot-set! gf
		    'extends
		    (cons val2 (delq! val2 (slot-ref gf 'extends))))
	 (slot-set! val2
		    'extended-by
		    (cons gf (delq! gf (slot-ref val2 'extended-by))))
         (invalidate-method-cache! gf)
	 var)))

(module-define! duplicate-handlers 'merge-generics merge-generics)

(define-method (merge-accessors (module <module>)
				(name <symbol>)
				(int1 <module>)
				(val1 <top>)
				(int2 <module>)
				(val2 <top>)
				(var <top>)
				(val <top>))
  #f)

(define-method (merge-accessors (module <module>)
				(name <symbol>)
				(int1 <module>)
				(val1 <accessor>)
				(int2 <module>)
				(val2 <accessor>)
				(var <top>)
				(val <top>))
  (merge-generics module name int1 val1 int2 val2 var val))

(module-define! duplicate-handlers 'merge-accessors merge-accessors)

;;;
;;; slot access
;;;

(define (class-slot-g-n-s class slot-name)
  (let* ((this-slot (assq slot-name (slot-ref class 'slots)))
	 (g-n-s (cddr (or (assq slot-name (slot-ref class 'getters-n-setters))
			  (slot-missing class slot-name)))))
    (if (not (memq (slot-definition-allocation this-slot)
		   '(#:class #:each-subclass)))
	(slot-missing class slot-name))
    g-n-s))

(define (class-slot-ref class slot)
  (let ((x ((car (class-slot-g-n-s class slot)) #f)))
    (if (unbound? x)
	(slot-unbound class slot)
	x)))

(define (class-slot-set! class slot value)
  ((cadr (class-slot-g-n-s class slot)) #f value))

(define-method (slot-unbound (c <class>) (o <object>) s)
  (goops-error "Slot `~S' is unbound in object ~S" s o))

(define-method (slot-unbound (c <class>) s)
  (goops-error "Slot `~S' is unbound in class ~S" s c))

(define-method (slot-unbound (o <object>))
  (goops-error "Unbound slot in object ~S" o))

(define-method (slot-missing (c <class>) (o <object>) s)
  (goops-error "No slot with name `~S' in object ~S" s o))
  
(define-method (slot-missing (c <class>) s)
  (goops-error "No class slot with name `~S' in class ~S" s c))
  

(define-method (slot-missing (c <class>) (o <object>) s value)
  (slot-missing c o s))

;;; Methods for the possible error we can encounter when calling a gf

(define-method (no-next-method (gf <generic>) args)
  (goops-error "No next method when calling ~S\nwith arguments ~S" gf args))

(define-method (no-applicable-method (gf <generic>) args)
  (goops-error "No applicable method for ~S in call ~S"
	       gf (cons (generic-function-name gf) args)))

(define-method (no-method (gf <generic>) args)
  (goops-error "No method defined for ~S"  gf))

;;;
;;; {Cloning functions (from rdeline@CS.CMU.EDU)}
;;;

(define-method (shallow-clone (self <object>))
  (let ((clone (%allocate-instance (class-of self) '()))
	(slots (map slot-definition-name
		    (class-slots (class-of self)))))
    (for-each (lambda (slot)
		(if (slot-bound? self slot)
		    (slot-set! clone slot (slot-ref self slot))))
	      slots)
    clone))

(define-method (deep-clone  (self <object>))
  (let ((clone (%allocate-instance (class-of self) '()))
	(slots (map slot-definition-name
		    (class-slots (class-of self)))))
    (for-each (lambda (slot)
		(if (slot-bound? self slot)
		    (slot-set! clone slot
			       (let ((value (slot-ref self slot)))
				 (if (instance? value)
				     (deep-clone value)
				     value)))))
	      slots)
    clone))

;;;
;;; {Class redefinition utilities}
;;;

;;; (class-redefinition OLD NEW)
;;;

;;; Has correct the following conditions:

;;; Methods
;;; 
;;; 1. New accessor specializers refer to new header
;;; 
;;; Classes
;;; 
;;; 1. New class cpl refers to the new class header
;;; 2. Old class header exists on old super classes direct-subclass lists
;;; 3. New class header exists on new super classes direct-subclass lists

(define-method (class-redefinition (old <class>) (new <class>))
  ;; Work on direct methods:
  ;;		1. Remove accessor methods from the old class 
  ;;		2. Patch the occurences of new in the specializers by old
  ;;		3. Displace the methods from old to new
  (remove-class-accessors! old)					;; -1-
  (let ((methods (class-direct-methods new)))
    (for-each (lambda (m)
     	         (update-direct-method! m new old))	;; -2-
              methods)
    (slot-set! new
	       'direct-methods
	       (append methods (class-direct-methods old))))

  ;; Substitute old for new in new cpl
  (set-car! (slot-ref new 'cpl) old)
  
  ;; Remove the old class from the direct-subclasses list of its super classes
  (for-each (lambda (c) (slot-set! c 'direct-subclasses
				   (delv! old (class-direct-subclasses c))))
	    (class-direct-supers old))

  ;; Replace the new class with the old in the direct-subclasses of the supers
  (for-each (lambda (c)
	      (slot-set! c 'direct-subclasses
			 (cons old (delv! new (class-direct-subclasses c)))))
	    (class-direct-supers new))

  ;; Swap object headers
  (%modify-class old new)

  ;; Now old is NEW!

  ;; Redefine all the subclasses of old to take into account modification
  (for-each 
       (lambda (c)
	 (update-direct-subclass! c new old))
       (class-direct-subclasses new))

  ;; Invalidate class so that subsequent instances slot accesses invoke
  ;; change-object-class
  (slot-set! new 'redefined old)
  (%invalidate-class new) ;must come after slot-set!

  old)

;;;
;;; remove-class-accessors!
;;;

(define-method (remove-class-accessors! (c <class>))
  (for-each (lambda (m)
	      (if (is-a? m <accessor-method>)
		  (let ((gf (slot-ref m 'generic-function)))
		    ;; remove the method from its GF
		    (slot-set! gf 'methods
			       (delq1! m (slot-ref gf 'methods)))
		    (invalidate-method-cache! gf)
		    ;; remove the method from its specializers
		    (remove-method-in-classes! m))))
	    (class-direct-methods c)))

;;;
;;; update-direct-method!
;;;

(define-method (update-direct-method! (m  <method>)
				      (old <class>)
				      (new <class>))
  (let loop ((l (method-specializers m)))
    ;; Note: the <top> in dotted list is never used. 
    ;; So we can work as if we had only proper lists.
    (if (pair? l)       	  
	(begin
	  (if (eqv? (car l) old)  
	      (set-car! l new))
	  (loop (cdr l))))))

;;;
;;; update-direct-subclass!
;;;

(define-method (update-direct-subclass! (c <class>)
					(old <class>)
					(new <class>))
  (class-redefinition c
		      (make-class (class-direct-supers c)
				  (class-direct-slots c)
				  #:name (class-name c)
				  #:metaclass (class-of c))))

;;;
;;; {Utilities for INITIALIZE methods}
;;;

;;; compute-slot-accessors
;;;
(define (compute-slot-accessors class slots)
  (for-each
      (lambda (s g-n-s)
	(let ((getter-function (slot-definition-getter   s))
	      (setter-function (slot-definition-setter   s))
	      (accessor        (slot-definition-accessor s)))
	  (if getter-function
	      (add-method! getter-function
			   (compute-getter-method class g-n-s)))
	  (if setter-function
	      (add-method! setter-function
			   (compute-setter-method class g-n-s)))
	  (if accessor
	      (begin
		(add-method! accessor
			     (compute-getter-method class g-n-s))
		(add-method! (setter accessor)
			     (compute-setter-method class g-n-s))))))
      slots (slot-ref class 'getters-n-setters)))

(define-method (compute-getter-method (class <class>) g-n-s)
  (let ((init-thunk (cadr g-n-s))
        (g-n-s (cddr g-n-s)))
    (make <accessor-method>
          #:specializers (list class)
          #:procedure (cond ((pair? g-n-s)
                             (make-generic-bound-check-getter (car g-n-s)))
                            (init-thunk
                             (standard-get g-n-s))
                            (else
                             (bound-check-get g-n-s)))
          #:slot-definition g-n-s)))

(define-method (compute-setter-method (class <class>) g-n-s)
  (let ((init-thunk (cadr g-n-s))
        (g-n-s (cddr g-n-s)))
    (make <accessor-method>
      #:specializers (list class <top>)
      #:procedure (if (pair? g-n-s)
                      (cadr g-n-s)
                      (standard-set g-n-s))
      #:slot-definition g-n-s)))

(define (make-generic-bound-check-getter proc)
  (lambda (o) (assert-bound (proc o) o)))

;; the idea is to compile the index into the procedure, for fastest
;; lookup.

(eval-when (expand load eval)
  (define num-standard-pre-cache 20))

(define-macro (define-standard-accessor-method form . body)
  (let ((name (caar form))
        (n-var (cadar form))
        (args (cdr form)))
    (define (make-one x)
      (define (body-trans form)
        (cond ((not (pair? form)) form)
              ((eq? (car form) 'struct-ref)
               `(,(car form) ,(cadr form) ,x))
              ((eq? (car form) 'struct-set!)
               `(,(car form) ,(cadr form) ,x ,(cadddr form)))
              (else
               (map body-trans form))))
      `(lambda ,args ,@(map body-trans body)))
    `(define ,name
       (let ((cache (vector ,@(map make-one (iota num-standard-pre-cache)))))
         (lambda (n)
           (if (< n ,num-standard-pre-cache)
               (vector-ref cache n)
               ((lambda (,n-var) (lambda ,args ,@body)) n)))))))

(define-standard-accessor-method ((bound-check-get n) o)
  (let ((x (struct-ref o n)))
    (if (unbound? x)
        (slot-unbound o)
        x)))

(define-standard-accessor-method ((standard-get n) o)
  (struct-ref o n))

(define-standard-accessor-method ((standard-set n) o v)
  (struct-set! o n v))

;;; compute-getters-n-setters
;;;
(define (compute-getters-n-setters class slots)

  (define (compute-slot-init-function name s)
    (or (let ((thunk (slot-definition-init-thunk s)))
	  (and thunk
	       (if (thunk? thunk)
                   thunk
                   (goops-error "Bad init-thunk for slot `~S' in ~S: ~S"
                                name class thunk))))
	(let ((init (slot-definition-init-value s)))
	  (and (not (unbound? init))
	       (lambda () init)))))

  (define (verify-accessors slot l)
    (cond ((integer? l))
	  ((not (and (list? l) (= (length l) 2)))
	   (goops-error "Bad getter and setter for slot `~S' in ~S: ~S"
			slot class l))
	  (else
	   (let ((get (car l)) 
		 (set (cadr l)))
	     (if (not (procedure? get))
                 (goops-error "Bad getter closure for slot `~S' in ~S: ~S"
			      slot class get))
	     (if (not (procedure? set))
                 (goops-error "Bad setter closure for slot `~S' in ~S: ~S"
			      slot class set))))))

  (map (lambda (s)
	 ;; The strange treatment of nfields is due to backward compatibility.
	 (let* ((index (slot-ref class 'nfields))
		(g-n-s (compute-get-n-set class s))
		(size (- (slot-ref class 'nfields) index))
		(name  (slot-definition-name s)))
	   ;; NOTE: The following is interdependent with C macros
	   ;; defined above goops.c:scm_sys_prep_layout_x.
	   ;;
	   ;; For simple instance slots, we have the simplest form
	   ;; '(name init-function . index)
	   ;; For other slots we have
	   ;; '(name init-function getter setter . alloc)
	   ;; where alloc is:
	   ;;   '(index size) for instance allocated slots
	   ;;   '() for other slots
	   (verify-accessors name g-n-s)
           (case (slot-definition-allocation s)
             ((#:each-subclass #:class)
              (unless (and (zero? size) (pair? g-n-s))
                (error "Class-allocated slots should not reserve fields"))
              ;; Don't initialize the slot; that's handled when the slot
              ;; is allocated, in compute-get-n-set.
              (cons name (cons #f g-n-s)))
             (else
              (cons name
                    (cons (compute-slot-init-function name s)
                          (if (or (integer? g-n-s)
                                  (zero? size))
                              g-n-s
                              (append g-n-s (list index size)))))))))
       slots))

;;; compute-cpl
;;;
;;; Correct behaviour:
;;;
;;; (define-class food ())
;;; (define-class fruit (food))
;;; (define-class spice (food))
;;; (define-class apple (fruit))
;;; (define-class cinnamon (spice))
;;; (define-class pie (apple cinnamon))
;;; => cpl (pie) = pie apple fruit cinnamon spice food object top
;;;
;;; (define-class d ())
;;; (define-class e ())
;;; (define-class f ())
;;; (define-class b (d e))
;;; (define-class c (e f))
;;; (define-class a (b c))
;;; => cpl (a) = a b d c e f object top
;;;

(define-method (compute-cpl (class <class>))
  (compute-std-cpl class class-direct-supers))

;; Support

(define (only-non-null lst)
  (filter (lambda (l) (not (null? l))) lst))

(define (compute-std-cpl c get-direct-supers)
  (let ((c-direct-supers (get-direct-supers c)))
    (merge-lists (list c)
                 (only-non-null (append (map class-precedence-list
					     c-direct-supers)
                                        (list c-direct-supers))))))

(define (merge-lists reversed-partial-result inputs)
  (cond
   ((every null? inputs)
    (reverse! reversed-partial-result))
   (else
    (let* ((candidate (lambda (c)
                        (and (not (any (lambda (l)
                                         (memq c (cdr l)))
                                       inputs))
                             c)))
           (candidate-car (lambda (l)
                            (and (not (null? l))
                                 (candidate (car l)))))
           (next (any candidate-car inputs)))
      (if (not next)
          (goops-error "merge-lists: Inconsistent precedence graph"))
      (let ((remove-next (lambda (l)
                           (if (eq? (car l) next)
                               (cdr l)
                             l))))
        (merge-lists (cons next reversed-partial-result)
                     (only-non-null (map remove-next inputs))))))))

;; Modified from TinyClos:
;;
;; A simple topological sort.
;;
;; It's in this file so that both TinyClos and Objects can use it.
;;
;; This is a fairly modified version of code I originally got from Anurag
;; Mendhekar <anurag@moose.cs.indiana.edu>.
;;

(define (compute-clos-cpl c get-direct-supers)
  (top-sort ((build-transitive-closure get-direct-supers) c)
	    ((build-constraints get-direct-supers) c)
	    (std-tie-breaker get-direct-supers)))


(define (top-sort elements constraints tie-breaker)
  (let loop ((elements    elements)
	     (constraints constraints)
	     (result      '()))
    (if (null? elements)
	result
	(let ((can-go-in-now
	       (filter
		(lambda (x)
		  (every (lambda (constraint)
			   (or (not (eq? (cadr constraint) x))
			       (memq (car constraint) result)))
			 constraints))
		elements)))
	  (if (null? can-go-in-now)
	      (goops-error "top-sort: Invalid constraints")
	      (let ((choice (if (null? (cdr can-go-in-now))
				(car can-go-in-now)
				(tie-breaker result
					     can-go-in-now))))
		(loop
		 (filter (lambda (x) (not (eq? x choice)))
			 elements)
		 constraints
		 (append result (list choice)))))))))

(define (std-tie-breaker get-supers)
  (lambda (partial-cpl min-elts)
    (let loop ((pcpl (reverse partial-cpl)))
      (let ((current-elt (car pcpl)))
	(let ((ds-of-ce (get-supers current-elt)))
	  (let ((common (filter (lambda (x)
				      (memq x ds-of-ce))
				    min-elts)))
	    (if (null? common)
		(if (null? (cdr pcpl))
		    (goops-error "std-tie-breaker: Nothing valid")
		    (loop (cdr pcpl)))
		(car common))))))))


(define (build-transitive-closure get-follow-ons)
  (lambda (x)
    (let track ((result '())
		(pending (list x)))
      (if (null? pending)
	  result
	  (let ((next (car pending)))
	    (if (memq next result)
		(track result (cdr pending))
		(track (cons next result)
		       (append (get-follow-ons next)
			       (cdr pending)))))))))

(define (build-constraints get-follow-ons)
  (lambda (x)
    (let loop ((elements ((build-transitive-closure get-follow-ons) x))
	       (this-one '())
	       (result '()))
      (if (or (null? this-one) (null? (cdr this-one)))
	  (if (null? elements)
	      result
	      (loop (cdr elements)
		    (cons (car elements)
			  (get-follow-ons (car elements)))
		    result))
	  (loop elements
		(cdr this-one)
		(cons (list (car this-one) (cadr this-one))
		      result))))))

;;; compute-get-n-set
;;;
(define-method (compute-get-n-set (class <class>) s)
  (define (class-slot-init-value)
    (let ((thunk (slot-definition-init-thunk s)))
      (if thunk
          (thunk)
          (slot-definition-init-value s))))

  (case (slot-definition-allocation s)
    ((#:instance) ;; Instance slot
     ;; get-n-set is just its offset
     (let ((already-allocated (slot-ref class 'nfields)))
       (slot-set! class 'nfields (+ already-allocated 1))
       already-allocated))

    ((#:class)  ;; Class slot
     ;; Class-slots accessors are implemented as 2 closures around 
     ;; a Scheme variable. As instance slots, class slots must be
     ;; unbound at init time.
     (let ((name (slot-definition-name s)))
       (if (memq name (map slot-definition-name (class-direct-slots class)))
	   ;; This slot is direct; create a new shared variable
	   (make-closure-variable class (class-slot-init-value))
	   ;; Slot is inherited. Find its definition in superclass
	   (let loop ((l (cdr (class-precedence-list class))))
	     (let ((r (assoc name (slot-ref (car l) 'getters-n-setters))))
	       (if r
		   (cddr r)
		   (loop (cdr l))))))))

    ((#:each-subclass) ;; slot shared by instances of direct subclass.
     ;; (Thomas Buerger, April 1998)
     (make-closure-variable class (class-slot-init-value)))

    ((#:virtual) ;; No allocation
     ;; slot-ref and slot-set! function must be given by the user
     (let ((get (get-keyword #:slot-ref  (slot-definition-options s) #f))
	   (set (get-keyword #:slot-set! (slot-definition-options s) #f)))
       (if (not (and get set))
	   (goops-error "You must supply a #:slot-ref and a #:slot-set! in ~S"
			s))
       (list get set)))
    (else    (next-method))))

(define (make-closure-variable class value)
  (list (lambda (o) value)
        (lambda (o v) (set! value v))))

(define-method (compute-get-n-set (o <object>) s)
  (goops-error "Allocation \"~S\" is unknown" (slot-definition-allocation s)))

(define-method (compute-slots (class <class>))
  (%compute-slots class))

;;;
;;; {Initialize}
;;;

(define-method (initialize (object <object>) initargs)
  (%initialize-object object initargs))

(define-method (initialize (class <class>) initargs)
  (next-method)
  (let ((dslots (get-keyword #:slots initargs '()))
	(supers (get-keyword #:dsupers	  initargs '())))
    (slot-set! class 'name	  	(get-keyword #:name initargs '???))
    (slot-set! class 'direct-supers 	supers)
    (slot-set! class 'direct-slots  	dslots)
    (slot-set! class 'direct-subclasses '())
    (slot-set! class 'direct-methods    '())
    (slot-set! class 'cpl		(compute-cpl class))
    (slot-set! class 'redefined		#f)
    (let ((slots (compute-slots class)))
      (slot-set! class 'slots	  	  slots)
      (slot-set! class 'nfields	  	  0)
      (slot-set! class 'getters-n-setters (compute-getters-n-setters class 
								     slots))
      ;; Build getters - setters - accessors
      (compute-slot-accessors class slots))

    ;; Update the "direct-subclasses" of each inherited classes
    (for-each (lambda (x)
		(slot-set! x
			   'direct-subclasses 
			   (cons class (slot-ref x 'direct-subclasses))))
	      supers)

    ;; Support for the underlying structs:
    
    ;; Set the layout slot
    (%prep-layout! class)
    ;; Inherit class flags (invisible on scheme level) from supers
    (%inherit-magic! class supers)))

(define (initialize-object-procedure object initargs)
  (let ((proc (get-keyword #:procedure initargs #f)))
    (cond ((not proc))
	  ((pair? proc)
	   (apply slot-set! object 'procedure proc))
	  (else
           (slot-set! object 'procedure proc)))))

(define-method (initialize (applicable-struct <applicable-struct>) initargs)
  (next-method)
  (initialize-object-procedure applicable-struct initargs))

(define-method (initialize (generic <generic>) initargs)
  (let ((previous-definition (get-keyword #:default initargs #f))
	(name (get-keyword #:name initargs #f)))
    (next-method)
    (slot-set! generic 'methods (if (is-a? previous-definition <procedure>)
				    (list (method args
                                            (apply previous-definition args)))
				    '()))
    (if name
	(set-procedure-property! generic 'name name))
    ))

(define-method (initialize (gws <generic-with-setter>) initargs)
  (next-method)
  (%set-object-setter! gws (get-keyword #:setter initargs #f)))

(define-method (initialize (eg <extended-generic>) initargs)
  (next-method)
  (slot-set! eg 'extends (get-keyword #:extends initargs '())))

(define dummy-procedure (lambda args *unspecified*))

(define-method (initialize (method <method>) initargs)
  (next-method)
  (slot-set! method 'generic-function (get-keyword #:generic-function initargs #f))
  (slot-set! method 'specializers (get-keyword #:specializers initargs '()))
  (slot-set! method 'procedure
	     (get-keyword #:procedure initargs #f))
  (slot-set! method 'formals (get-keyword #:formals initargs '()))
  (slot-set! method 'body (get-keyword #:body initargs '()))
  (slot-set! method 'make-procedure (get-keyword #:make-procedure initargs #f)))
             

;;;
;;; {Change-class}
;;;

(define (change-object-class old-instance old-class new-class)
  (let ((new-instance (allocate-instance new-class '())))
    ;; Initialize the slots of the new instance
    (for-each (lambda (slot)
		(if (and (slot-exists-using-class? old-class old-instance slot)
			 (eq? (slot-definition-allocation
			       (class-slot-definition old-class slot))
			      #:instance)
			 (slot-bound-using-class? old-class old-instance slot))
		    ;; Slot was present and allocated in old instance; copy it 
		    (slot-set-using-class!
		     new-class 
		     new-instance 
		     slot 
		     (slot-ref-using-class old-class old-instance slot))
		    ;; slot was absent; initialize it with its default value
		    (let ((init (slot-init-function new-class slot)))
		      (if init
			  (slot-set-using-class!
			       new-class 
			       new-instance 
			       slot
			       (apply init '()))))))
	      (map slot-definition-name (class-slots new-class)))
    ;; Exchange old and new instance in place to keep pointers valid
    (%modify-instance old-instance new-instance)
    ;; Allow class specific updates of instances (which now are swapped)
    (update-instance-for-different-class new-instance old-instance)
    old-instance))


(define-method (update-instance-for-different-class (old-instance <object>)
						    (new-instance
						     <object>))
  ;;not really important what we do, we just need a default method
  new-instance)

(define-method (change-class (old-instance <object>) (new-class <class>))
  (change-object-class old-instance (class-of old-instance) new-class))

;;;
;;; {make}
;;;
;;; A new definition which overwrites the previous one which was built-in
;;;

(define-method (allocate-instance (class <class>) initargs)
  (%allocate-instance class initargs))

(define-method (make-instance (class <class>) . initargs)
  (let ((instance (allocate-instance class initargs)))
    (initialize instance initargs)
    instance))

(define make make-instance)

;;;
;;; {apply-generic}
;;;
;;; Protocol for calling standard generic functions.  This protocol is
;;; not used for real <generic> functions (in this case we use a
;;; completely C hard-coded protocol).  Apply-generic is used by
;;; goops for calls to subclasses of <generic> and <generic-with-setter>.
;;; The code below is similar to the first MOP described in AMOP. In
;;; particular, it doesn't used the currified approach to gf
;;; call. There are 2 reasons for that:
;;;   - the protocol below is exposed to mimic completely the one written in C
;;;   - the currified protocol would be imho inefficient in C.
;;;

(define-method (apply-generic (gf <generic>) args)
  (if (null? (slot-ref gf 'methods))
      (no-method gf args))
  (let ((methods (compute-applicable-methods gf args)))
    (if methods
	(apply-methods gf (sort-applicable-methods gf methods args) args)
	(no-applicable-method gf args))))

;; compute-applicable-methods is bound to %compute-applicable-methods.
;; *fixme* use let
(define %%compute-applicable-methods
  (make <generic> #:name 'compute-applicable-methods))

(define-method (%%compute-applicable-methods (gf <generic>) args)
  (%compute-applicable-methods gf args))

(set! compute-applicable-methods %%compute-applicable-methods)

(define-method (sort-applicable-methods (gf <generic>) methods args)
  (let ((targs (map class-of args)))
    (sort methods (lambda (m1 m2) (method-more-specific? m1 m2 targs)))))

(define-method (method-more-specific? (m1 <method>) (m2 <method>) targs)
  (%method-more-specific? m1 m2 targs))

(define-method (apply-method (gf <generic>) methods build-next args)
  (apply (method-procedure (car methods))
	 (build-next (cdr methods) args)
	 args))

(define-method (apply-methods (gf <generic>) (l <list>) args)
  (letrec ((next (lambda (procs args)
		   (lambda new-args
		     (let ((a (if (null? new-args) args new-args)))
		       (if (null? procs)
			   (no-next-method gf a)
			   (apply-method gf procs next a)))))))
    (apply-method gf l next args)))

;; We don't want the following procedure to turn up in backtraces:
(for-each (lambda (proc)
	    (set-procedure-property! proc 'system-procedure #t))
	  (list slot-unbound
		slot-missing
		no-next-method
		no-applicable-method
		no-method
		))

;;;
;;; {<composite-metaclass> and <active-metaclass>}
;;;

;(autoload "active-slot"    <active-metaclass>)
;(autoload "composite-slot" <composite-metaclass>)
;(export <composite-metaclass> <active-metaclass>)

;;;
;;; {Tools}
;;;

;; list2set
;;
;; duplicate the standard list->set function but using eq instead of
;; eqv which really sucks a lot, uselessly here
;;
(define (list2set l)	       
  (let loop ((l l)
	     (res '()))
    (cond		       
     ((null? l) res)
     ((memq (car l) res) (loop (cdr l) res))
     (else (loop (cdr l) (cons (car l) res))))))

(define (class-subclasses c)
  (letrec ((allsubs (lambda (c)
		      (cons c (mapappend allsubs
					 (class-direct-subclasses c))))))
    (list2set (cdr (allsubs c)))))

(define (class-methods c)
  (list2set (mapappend class-direct-methods
		       (cons c (class-subclasses c)))))

;;;
;;; {Final initialization}
;;;

;; Tell C code that the main bulk of Goops has been loaded
(%goops-loaded)
