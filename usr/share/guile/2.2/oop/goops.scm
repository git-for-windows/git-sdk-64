;;;; goops.scm -- The Guile Object-Oriented Programming System
;;;;
;;;; Copyright (C) 1998-2003, 2006, 2009-2011, 2013-2015, 2018
;;;;   Free Software Foundation, Inc.
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
  #:use-module ((language tree-il primitives)
                :select (add-interesting-primitive!))
  #:export-syntax (define-class class standard-define-class
                    define-generic define-accessor define-method
                    define-extended-generic define-extended-generics
                    method)
  #:export ( ;; The root of everything.
            <top>
            <class> <object>

            ;; Slot types.
            <slot>
            <foreign-slot> <protected-slot> <hidden-slot> <opaque-slot>
            <read-only-slot> <self-slot> <protected-opaque-slot>
            <protected-hidden-slot> <protected-read-only-slot>
            <scm-slot> <int-slot> <float-slot> <double-slot>

            ;; Methods are implementations of generic functions.
            <method> <accessor-method>

            ;; Applicable objects, either procedures or applicable structs.
            <procedure-class> <applicable>
            <procedure> <primitive-generic>

            ;; Applicable structs.
            <applicable-struct-class> <applicable-struct-with-setter-class>
            <applicable-struct> <applicable-struct-with-setter>
            <generic> <extended-generic>
            <generic-with-setter> <extended-generic-with-setter>
            <accessor> <extended-accessor>

            ;; Types with their own allocated typecodes.
            <boolean> <char> <list> <pair> <null> <string> <symbol>
            <vector> <bytevector> <uvec> <foreign> <hashtable>
            <fluid> <dynamic-state> <frame> <vm> <vm-continuation>
            <keyword> <syntax> <atomic-box>

            ;; Numbers.
            <number> <complex> <real> <integer> <fraction>

            ;; Unknown.
            <unknown>

            ;; Particular SMOB data types.  All SMOB types have
            ;; corresponding classes, which may be obtained via class-of,
            ;; once you have an instance.  Perhaps FIXME to provide a
            ;; smob-type-name->class procedure.
            <promise> <thread> <mutex> <condition-variable>
            <regexp> <hook> <bitvector> <random-state>
            <directory> <array> <character-set>
            <dynamic-object> <guardian> <macro>

            ;; Modules.
            <module>

            ;; Ports.
            <port> <input-port> <output-port> <input-output-port>

            ;; Like SMOB types, all port types have their own classes,
            ;; which can be accessed via `class-of' once you have an
            ;; instance.  Here we export bindings just for file ports.
            <file-port>
            <file-input-port> <file-output-port> <file-input-output-port>

            is-a? class-of
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

            instance?
            slot-ref slot-set! slot-bound? slot-exists?
            class-name class-direct-supers class-direct-subclasses
            class-direct-methods class-direct-slots class-precedence-list
            class-slots
            generic-function-name
            generic-function-methods method-generic-function
            method-specializers method-formals
            primitive-generic-generic enable-primitive-generic!
            method-procedure accessor-method-slot-definition
            make find-method get-keyword))


;;;
;;; Booting GOOPS is a tortuous process.  We begin by loading a small
;;; set of primitives from C.
;;;
(eval-when (expand load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_goops_builtins")
  (add-interesting-primitive! 'class-of))




;;;
;;; We then define the slots that must appear in all classes (<class>
;;; objects) and slot definitions (<slot> objects).  These slots must
;;; appear in order.  We'll use this list to statically compute offsets
;;; for the various fields, to compute the struct layout for <class>
;;; instances, and to compute the slot definition lists for <class>.
;;; Because the list is needed at expansion-time, we define it as a
;;; macro.
;;;
(define-syntax macro-fold-left
  (syntax-rules ()
    ((_ folder seed ()) seed)
    ((_ folder seed (head . tail))
     (macro-fold-left folder (folder head seed) tail))))

(define-syntax macro-fold-right
  (syntax-rules ()
    ((_ folder seed ()) seed)
    ((_ folder seed (head . tail))
     (folder head (macro-fold-right folder seed tail)))))

(define-syntax-rule (define-macro-folder macro-folder value ...)
  (define-syntax macro-folder
    (lambda (x)
      (syntax-case x ()
        ((_ fold visit seed)
         ;; The datum->syntax makes it as if each `value' were present
         ;; in the initial form, which allows them to be used as
         ;; (components of) introduced identifiers.
         #`(fold visit seed #,(datum->syntax #'visit '(value ...))))))))

(define-macro-folder fold-class-slots
  (layout #:class <protected-read-only-slot>)
  (flags #:class <hidden-slot>)
  (self #:class <self-slot>)
  (instance-finalizer #:class <hidden-slot>)
  (print)
  (name #:class <protected-hidden-slot>)
  (nfields #:class <hidden-slot>)
  (%reserved #:class <hidden-slot>)
  (redefined)
  (direct-supers)
  (direct-slots)
  (direct-subclasses)
  (direct-methods)
  (cpl)
  (slots))

(define-macro-folder fold-slot-slots
  (name #:init-keyword #:name)
  (allocation #:init-keyword #:allocation #:init-value #:instance)
  (init-keyword #:init-keyword #:init-keyword #:init-value #f)
  (init-form #:init-keyword #:init-form)
  (init-value #:init-keyword #:init-value)
  (init-thunk #:init-keyword #:init-thunk #:init-value #f)
  (options)
  (getter #:init-keyword #:getter #:init-value #f)
  (setter #:init-keyword #:setter #:init-value #f)
  (accessor #:init-keyword #:accessor #:init-value #f)
  ;; These last don't have #:init-keyword because they are meant to be
  ;; set by `allocate-slots', not in compute-effective-slot-definition.
  (slot-ref/raw #:init-value #f)
  (slot-ref #:init-value #f)
  (slot-set! #:init-value #f)
  (index #:init-value #f)
  (size #:init-value #f))

;;;
;;; Statically define variables for slot offsets: `class-index-layout'
;;; will be 0, `class-index-flags' will be 1, and so on, and the same
;;; for `slot-index-name' and such for <slot>.
;;;
(let-syntax ((define-slot-indexer
               (syntax-rules ()
                 ((_ define-index prefix)
                  (define-syntax define-index
                    (lambda (x)
                      (define (id-append ctx a b)
                        (datum->syntax ctx (symbol-append (syntax->datum a)
                                                          (syntax->datum b))))
                      (define (tail-length tail)
                        (syntax-case tail ()
                          ((begin) 0)
                          ((visit head tail) (1+ (tail-length #'tail)))))
                      (syntax-case x ()
                        ((_ (name . _) tail)
                         #`(begin
                             (define-syntax #,(id-append #'name #'prefix #'name)
                               (identifier-syntax #,(tail-length #'tail)))
                             tail)))))))))
  (define-slot-indexer define-class-index class-index-)
  (define-slot-indexer define-slot-index slot-index-)
  (fold-class-slots macro-fold-left define-class-index (begin))
  (fold-slot-slots macro-fold-left define-slot-index (begin)))

;;;
;;; Structs that are vtables have a "flags" slot, which corresponds to
;;; class-index-flags.  `vtable-flag-vtable' indicates that instances of
;;; a vtable are themselves vtables, and `vtable-flag-validated'
;;; indicates that the struct's layout has been validated.  goops.c
;;; defines a few additional flags: one to indicate that a vtable is
;;; actually a class, one to indicate that the class is "valid" (meaning
;;; that it hasn't been redefined), and one to indicate that instances
;;; of a class are slot definition objects (<slot> instances).
;;;
(define vtable-flag-goops-metaclass
  (logior vtable-flag-vtable vtable-flag-goops-class))

(define-inlinable (class-add-flags! class flags)
  (struct-set!/unboxed
   class
   class-index-flags
   (logior flags (struct-ref/unboxed class class-index-flags))))

(define-inlinable (class-clear-flags! class flags)
  (struct-set!/unboxed
   class
   class-index-flags
   (logand (lognot flags) (struct-ref/unboxed class class-index-flags))))

(define-inlinable (class-has-flags? class flags)
  (eqv? flags
        (logand (struct-ref/unboxed class class-index-flags) flags)))

(define-inlinable (class? obj)
  (class-has-flags? (struct-vtable obj) vtable-flag-goops-metaclass))

(define-inlinable (slot? obj)
  (and (struct? obj)
       (class-has-flags? (struct-vtable obj) vtable-flag-goops-slot)))

(define-inlinable (instance? obj)
  (and (struct? obj)
       (class-has-flags? (struct-vtable obj) vtable-flag-goops-class)))

(define (class-has-statically-allocated-slots? class)
  (class-has-flags? class vtable-flag-goops-static))

;;;
;;; Now that we know the slots that must be present in classes, and
;;; their offsets, we can create the root of the class hierarchy.
;;;
;;; Note that the `direct-supers', `direct-slots', `cpl', and `slots'
;;; fields will be updated later, once we can create slot definition
;;; objects and once we have definitions for <top> and <object>.
;;;
(define <class>
  (let-syntax ((cons-layout
                ;; A simple way to compute class layout for the concrete
                ;; types used in <class>.
                (syntax-rules (<protected-read-only-slot>
                               <self-slot>
                               <hidden-slot>
                               <protected-hidden-slot>)
                  ((_ (name) tail)
                   (string-append "pw" tail))
                  ((_ (name #:class <protected-read-only-slot>) tail)
                   (string-append "pr" tail))
                  ((_ (name #:class <self-slot>) tail)
                   (string-append "sr" tail))
                  ((_ (name #:class <hidden-slot>) tail)
                   (string-append "uh" tail))
                  ((_ (name #:class <protected-hidden-slot>) tail)
                   (string-append "ph" tail)))))
    (let* ((layout (fold-class-slots macro-fold-right cons-layout ""))
           (nfields (/ (string-length layout) 2))
           (<class> (%make-vtable-vtable layout)))
      (class-add-flags! <class> (logior vtable-flag-goops-class
                                        vtable-flag-goops-valid))
      (struct-set! <class> class-index-name '<class>)
      (struct-set!/unboxed <class> class-index-nfields nfields)
      (struct-set! <class> class-index-direct-supers '())
      (struct-set! <class> class-index-direct-slots '())
      (struct-set! <class> class-index-direct-subclasses '())
      (struct-set! <class> class-index-direct-methods '())
      (struct-set! <class> class-index-cpl '())
      (struct-set! <class> class-index-slots '())
      (struct-set! <class> class-index-redefined #f)
      <class>)))

;;;
;;; Accessors to fields of <class>.
;;;
(define-syntax-rule (define-class-accessor name docstring field)
  (define (name obj)
    docstring
    (let ((val obj))
      (unless (class? val)
        (scm-error 'wrong-type-arg #f "Not a class: ~S"
                   (list val) #f))
      (struct-ref val field))))

(define-class-accessor class-name
  "Return the class name of @var{obj}."
  class-index-name)
(define-class-accessor class-direct-supers
  "Return the direct superclasses of the class @var{obj}."
  class-index-direct-supers)
(define-class-accessor class-direct-slots
  "Return the direct slots of the class @var{obj}."
  class-index-direct-slots)
(define-class-accessor class-direct-subclasses
  "Return the direct subclasses of the class @var{obj}."
  class-index-direct-subclasses)
(define-class-accessor class-direct-methods
  "Return the direct methods of the class @var{obj}."
  class-index-direct-methods)
(define-class-accessor class-precedence-list
  "Return the class precedence list of the class @var{obj}."
  class-index-cpl)
(define-class-accessor class-slots
  "Return the slot list of the class @var{obj}."
  class-index-slots)

(define (class-subclasses c)
  "Compute a list of all subclasses of @var{c}, direct and indirect."
  (define (all-subclasses c)
    (cons c (append-map all-subclasses
                        (class-direct-subclasses c))))
  (delete-duplicates (cdr (all-subclasses c)) eq?))

(define (class-methods c)
  "Compute a list of all methods that specialize on @var{c} or
subclasses of @var{c}."
  (delete-duplicates (append-map class-direct-methods
                                 (cons c (class-subclasses c)))
                     eq?))

(define (is-a? obj class)
  "Return @code{#t} if @var{obj} is an instance of @var{class}, or
@code{#f} otherwise."
  (and (memq class (class-precedence-list (class-of obj))) #t))




;;;
;;; At this point, <class> is missing slot definitions, but we can't
;;; create slot definitions until we have a slot definition class.
;;; Continue with manual object creation until we're able to bootstrap
;;; more of the protocol.  Again, the CPL and class hierarchy slots
;;; remain uninitialized.
;;;
(define* (get-keyword key l #:optional default)
  "Determine an associated value for the keyword @var{key} from the list
@var{l}.  The list @var{l} has to consist of an even number of elements,
where, starting with the first, every second element is a keyword,
followed by its associated value.  If @var{l} does not hold a value for
@var{key}, the value @var{default} is returned."
  (unless (keyword? key)
    (scm-error 'wrong-type-arg #f "Not a keyword: ~S" (list key) #f))
  (let lp ((l l))
    (match l
      (() default)
      ((kw arg . l)
       (unless (keyword? kw)
         (scm-error 'wrong-type-arg #f "Not a keyword: ~S" (list kw) #f))
       (if (eq? kw key) arg (lp l))))))

(define *unbound* (list 'unbound))

(define-inlinable (unbound? x)
  (eq? x *unbound*))

(define (%allocate-instance class)
  (let ((obj (allocate-struct class
                              (struct-ref/unboxed class class-index-nfields))))
    (%clear-fields! obj *unbound*)
    obj))

(define <slot>
  (let-syntax ((cons-layout
                ;; All slots are "pw" in <slot>.
                (syntax-rules ()
                  ((_ _ tail) (string-append "pw" tail)))))
    (let* ((layout (fold-slot-slots macro-fold-right cons-layout ""))
           (nfields (/ (string-length layout) 2))
           (<slot> (make-struct/no-tail <class> (make-struct-layout layout))))
      (class-add-flags! <slot> (logior vtable-flag-goops-class
                                       vtable-flag-goops-slot
                                       vtable-flag-goops-valid))
      (struct-set! <slot> class-index-name '<slot>)
      (struct-set!/unboxed <slot> class-index-nfields nfields)
      (struct-set! <slot> class-index-direct-supers '())
      (struct-set! <slot> class-index-direct-slots '())
      (struct-set! <slot> class-index-direct-subclasses '())
      (struct-set! <slot> class-index-direct-methods '())
      (struct-set! <slot> class-index-cpl (list <slot>))
      (struct-set! <slot> class-index-slots '())
      (struct-set! <slot> class-index-redefined #f)
      <slot>)))

;;; Access to slot objects is performance-sensitive for slot-ref, so in
;;; addition to the type-checking accessors that we export, we also
;;; define some internal inlined helpers that just do an unchecked
;;; struct-ref in cases where we know the object must be a slot, as
;;; when accessing class-slots.
;;;
(define-syntax-rule (define-slot-accessor name docstring %name field)
  (begin
    (define-syntax-rule (%name obj)
      (struct-ref obj field))
    (define (name obj)
      docstring
      (unless (slot? obj)
        (scm-error 'wrong-type-arg #f "Not a slot: ~S"
                   (list obj) #f))
      (%name obj))))

(define-slot-accessor slot-definition-name
  "Return the name of @var{obj}."
   %slot-definition-name slot-index-name)
(define-slot-accessor slot-definition-allocation
  "Return the allocation of the slot @var{obj}."
   %slot-definition-allocation slot-index-allocation)
(define-slot-accessor slot-definition-init-keyword
  "Return the init keyword of the slot @var{obj}, or @code{#f}."
   %slot-definition-init-keyword slot-index-init-keyword)
(define-slot-accessor slot-definition-init-form
  "Return the init form of the slot @var{obj}, or the unbound value"
   %slot-definition-init-form slot-index-init-form)
(define-slot-accessor slot-definition-init-value
  "Return the init value of the slot @var{obj}, or the unbound value."
   %slot-definition-init-value slot-index-init-value)
(define-slot-accessor slot-definition-init-thunk
  "Return the init thunk of the slot @var{obj}, or @code{#f}."
   %slot-definition-init-thunk slot-index-init-thunk)
(define-slot-accessor slot-definition-options
  "Return the initargs given when creating the slot @var{obj}."
   %slot-definition-options slot-index-options)
(define-slot-accessor slot-definition-getter
  "Return the getter of the slot @var{obj}, or @code{#f}."
   %slot-definition-getter slot-index-getter)
(define-slot-accessor slot-definition-setter
  "Return the setter of the slot @var{obj}, or @code{#f}."
   %slot-definition-setter slot-index-setter)
(define-slot-accessor slot-definition-accessor
  "Return the accessor of the slot @var{obj}, or @code{#f}."
   %slot-definition-accessor slot-index-accessor)
(define-slot-accessor slot-definition-slot-ref/raw
  "Return the raw slot-ref procedure of the slot @var{obj}."
   %slot-definition-slot-ref/raw slot-index-slot-ref/raw)
(define-slot-accessor slot-definition-slot-ref
  "Return the slot-ref procedure of the slot @var{obj}."
   %slot-definition-slot-ref slot-index-slot-ref)
(define-slot-accessor slot-definition-slot-set!
  "Return the slot-set! procedure of the slot @var{obj}."
   %slot-definition-slot-set! slot-index-slot-set!)
(define-slot-accessor slot-definition-index
  "Return the allocated struct offset of the slot @var{obj}, or @code{#f}."
   %slot-definition-index slot-index-index)
(define-slot-accessor slot-definition-size
  "Return the number fields used by the slot @var{obj}, or @code{#f}."
   %slot-definition-size slot-index-size)

;; Boot definition.
(define (direct-slot-definition-class class initargs)
  (get-keyword #:class initargs <slot>))

;; Boot definition.
(define (make-slot class initargs)
  (let ((slot (make-struct/no-tail class)))
    (define-syntax-rule (init-slot offset kw default)
      (struct-set! slot offset (get-keyword kw initargs default)))
    (init-slot slot-index-name #:name #f)
    (init-slot slot-index-allocation #:allocation #:instance)
    (init-slot slot-index-init-keyword #:init-keyword #f)
    (init-slot slot-index-init-form #:init-form *unbound*)
    (init-slot slot-index-init-value #:init-value *unbound*)
    (struct-set! slot slot-index-init-thunk
                 (or (get-keyword #:init-thunk initargs #f)
                     (let ((val (%slot-definition-init-value slot)))
                       (if (unbound? val)
                           #f
                           (lambda () val)))))
    (struct-set! slot slot-index-options initargs)
    (init-slot slot-index-getter #:getter #f)
    (init-slot slot-index-setter #:setter #f)
    (init-slot slot-index-accessor #:accessor #f)
    (struct-set! slot slot-index-slot-ref/raw #f)
    (struct-set! slot slot-index-slot-ref #f)
    (struct-set! slot slot-index-slot-set! #f)
    (struct-set! slot slot-index-index #f)
    (struct-set! slot slot-index-size #f)
    slot))

;; Boot definition.
(define (make class . args)
  (unless (memq <slot> (class-precedence-list class))
    (error "Unsupported class: ~S" class))
  (make-slot class args))

;; Boot definition.
(define (compute-direct-slot-definition class initargs)
  (apply make (direct-slot-definition-class class initargs) initargs))

(define (compute-direct-slot-definition-initargs class slot-spec)
  (match slot-spec
    ((? symbol? name) (list #:name name))
    (((? symbol? name) . initargs)
     (cons* #:name name
            ;; If there is an #:init-form, the `class' macro will have
            ;; already added an #:init-thunk.  Still, if there isn't an
            ;; #:init-thunk already but we do have an #:init-value,
            ;; synthesize an #:init-thunk initarg.  This will ensure
            ;; that the #:init-thunk gets passed on to the effective
            ;; slot definition too.
            (if (get-keyword #:init-thunk initargs)
                initargs
                (let ((value (get-keyword #:init-value initargs *unbound*)))
                  (if (unbound? value)
                      initargs
                      (cons* #:init-thunk (lambda () value) initargs))))))))

(let ()
  (define-syntax cons-slot
    (syntax-rules ()
      ((_ (name #:class class) tail)
       ;; Special case to avoid referencing specialized <slot> kinds,
       ;; which are not defined yet.
       (cons (list 'name) tail))
      ((_ (name . initargs) tail)
       (cons (list 'name . initargs) tail))))
  (define-syntax-rule (initialize-direct-slots! class fold-slots)
    (let ((specs (fold-slots macro-fold-right cons-slot '())))
      (define (make-direct-slot-definition spec)
        (let ((initargs (compute-direct-slot-definition-initargs class spec)))
          (compute-direct-slot-definition class initargs)))
      (struct-set! class class-index-direct-slots
                   (map make-direct-slot-definition specs))))

  (initialize-direct-slots! <class> fold-class-slots)
  (initialize-direct-slots! <slot> fold-slot-slots))




;;;
;;; OK, at this point we have initialized `direct-slots' on both <class>
;;; and <slot>.  We need to define a standard way to make subclasses:
;;; how to compute the precedence list of subclasses, how to compute the
;;; list of slots in a subclass, and what layout to use for instances of
;;; those classes.
;;;
(define (compute-std-cpl c get-direct-supers)
  "The standard class precedence list computation algorithm."
  (define (only-non-null lst)
    (filter (lambda (l) (not (null? l))) lst))

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
        (unless next
          (goops-error "merge-lists: Inconsistent precedence graph"))
        (let ((remove-next (lambda (l)
                             (if (eq? (car l) next)
                                 (cdr l)
                                 l))))
          (merge-lists (cons next reversed-partial-result)
                       (only-non-null (map remove-next inputs))))))))
  (let ((c-direct-supers (get-direct-supers c)))
    (merge-lists (list c)
                 (only-non-null (append (map class-precedence-list
                                             c-direct-supers)
                                        (list c-direct-supers))))))

;; This version of compute-cpl is replaced with a generic function once
;; GOOPS has booted.
(define (compute-cpl class)
  (compute-std-cpl class class-direct-supers))

(define (effective-slot-definition-class class slot)
  (class-of slot))

(define (compute-effective-slot-definition class slot)
  ;; FIXME: Support slot being a list of slots, as in CLOS.
  (apply make
         (effective-slot-definition-class class slot)
         (slot-definition-options slot)))

(define (build-slots-list dslots cpl)
  (define (slot-memq slot slots)
    (let ((name (%slot-definition-name slot)))
      (let lp ((slots slots))
        (match slots
          (() #f)
          ((slot . slots)
           (or (eq? (%slot-definition-name slot) name) (lp slots)))))))
  (define (check-cpl slots static-slots)
    (match static-slots
      (() #t)
      ((static-slot . static-slots)
       (when (slot-memq static-slot slots)
         (scm-error 'misc-error #f
                    "statically allocated inherited field cannot be redefined: ~a"
                    (list (%slot-definition-name static-slot)) '()))
       (check-cpl slots static-slots))))
  (define (remove-duplicate-slots slots)
    (let lp ((slots (reverse slots)) (res '()) (seen '()))
      (match slots
        (() res)
        ((slot . slots)
         (let ((name (%slot-definition-name slot)))
           (if (memq name seen)
               (lp slots res seen)
               (lp slots (cons slot res) (cons name seen))))))))
  ;; For subclases of <class> and <slot>, we need to ensure that the
  ;; <class> or <slot> slots come first.
  (let ((static-slots
         (match (filter class-has-statically-allocated-slots? (cdr cpl))
           (() #f)
           ((class) (struct-ref class class-index-direct-slots))
           (classes
            (error "can't subtype multiple classes with static slot allocation"
                   classes)))))
    (when static-slots
      (check-cpl dslots static-slots))
    (let lp ((cpl (cdr cpl)) (res dslots) (static-slots '()))
      (match cpl
        (() (remove-duplicate-slots (append static-slots res)))
        ((head . cpl)
         (let ((new-slots (struct-ref head class-index-direct-slots)))
           (cond
            ((not static-slots)
             (lp cpl (append new-slots res) static-slots))
            ((class-has-statically-allocated-slots? head)
             ;; Move static slots to the head of the list.
             (lp cpl res new-slots))
            (else
             (check-cpl new-slots static-slots)
             (lp cpl (append new-slots res) static-slots)))))))))

;; Boot definition.
(define (compute-get-n-set class slot)
  (let ((index (struct-ref/unboxed class class-index-nfields)))
    (struct-set!/unboxed class class-index-nfields (1+ index))
    index))

;;; Pre-generate getters and setters for the first 20 slots.
(define-syntax define-standard-accessor-method
  (lambda (stx)
    (define num-standard-pre-cache 20)
    (syntax-case stx ()
      ((_ ((proc n) arg ...) body)
       #`(define proc
           (let ((cache (vector #,@(map (lambda (n*)
                                          #`(lambda (arg ...)
                                              (let ((n #,n*))
                                                body)))
                                        (iota num-standard-pre-cache)))))
             (lambda (n)
               (if (< n #,num-standard-pre-cache)
                   (vector-ref cache n)
                   (lambda (arg ...) body)))))))))

(define-standard-accessor-method ((bound-check-get n) o)
  (let ((x (struct-ref o n)))
    (if (unbound? x)
        (slot-unbound o)
        x)))

(define-standard-accessor-method ((standard-get n) o)
  (struct-ref o n))

(define-standard-accessor-method ((standard-set n) o v)
  (struct-set! o n v))

(define-standard-accessor-method ((unboxed-get n) o)
  (struct-ref/unboxed o n))

(define-standard-accessor-method ((unboxed-set n) o v)
  (struct-set!/unboxed o n v))

;; Boot definitions.
(define (opaque-slot? slot) #f)
(define (read-only-slot? slot) #f)
(define (unboxed-slot? slot)
  (memq (%slot-definition-name slot)
        '(flags instance-finalizer nfields %reserved)))

(define (allocate-slots class slots)
  "Transform the computed list of direct slot definitions @var{slots}
into a corresponding list of effective slot definitions, allocating
slots as we go."
  (define (make-effective-slot-definition slot)
    ;; `compute-get-n-set' is expected to mutate `nfields' if it
    ;; allocates a field to the object.  Pretty strange, but we preserve
    ;; the behavior for backward compatibility.
    (let* ((slot (compute-effective-slot-definition class slot))
           (name (%slot-definition-name slot))
           (index (struct-ref/unboxed class class-index-nfields))
           (g-n-s (compute-get-n-set class slot))
           (size (- (struct-ref/unboxed class class-index-nfields) index)))
      (call-with-values
          (lambda ()
            (match g-n-s
              ((? integer?)
               (unless (= size 1)
                 (error "unexpected return from compute-get-n-set"))
               (cond
                ((unboxed-slot? slot)
                 (let ((get (unboxed-get g-n-s)))
                   (values get get (unboxed-set g-n-s))))
                (else
                 (values (standard-get g-n-s)
                         (if (slot-definition-init-thunk slot)
                             (standard-get g-n-s)
                             (bound-check-get g-n-s))
                         (standard-set g-n-s)))))
              (((? procedure? get) (? procedure? set))
               (values get
                       (lambda (o)
                         (let ((value (get o)))
                           (if (unbound? value)
                               (slot-unbound class o name)
                               value)))
                       set))))
        (lambda (get/raw get set)
          (let ((get (if (opaque-slot? slot)
                         (lambda (o)
                           (error "Slot is opaque" name))
                         get))
                (set (cond
                      ((opaque-slot? slot)
                       (lambda (o v)
                         (error "Slot is opaque" name)))
                      ((read-only-slot? slot)
                       (if (unboxed-slot? slot)
                           (lambda (o v)
                             (let ((v* (get/raw o)))
                               (if (zero? v*)
                                   ;; Allow initialization.
                                   (set o v)
                                   (error "Slot is read-only" name))))
                           (lambda (o v)
                             (let ((v* (get/raw o)))
                               (if (unbound? v*)
                                   ;; Allow initialization.
                                   (set o v)
                                   (error "Slot is read-only" name))))))
                      (else set))))
            (struct-set! slot slot-index-slot-ref/raw get/raw)
            (struct-set! slot slot-index-slot-ref get)
            (struct-set! slot slot-index-slot-set! set)
            (struct-set! slot slot-index-index index)
            (struct-set! slot slot-index-size size))))
      slot))
  (struct-set!/unboxed class class-index-nfields 0)
  (map-in-order make-effective-slot-definition slots))

(define (%compute-layout slots nfields is-class?)
  (define (slot-protection-and-kind slot)
    (define (subclass? class parent)
      (memq parent (class-precedence-list class)))
    (let ((type (get-keyword #:class (%slot-definition-options slot))))
      (if (and type (subclass? type <foreign-slot>))
          (values (cond
                   ((subclass? type <self-slot>) #\s)
                   ((subclass? type <protected-slot>) #\p)
                   (else #\u))
                  (cond
                   ((subclass? type <read-only-slot>) #\r)
                   ((subclass? type <hidden-slot>) #\h)
                   (else #\w)))
          (values #\p #\w))))
  (let ((layout (make-string (* nfields 2))))
    (let lp ((n 0) (slots slots))
      (match slots
        (()
         (unless (= n nfields) (error "bad nfields"))
         (when is-class?
           (let ((class-layout (struct-ref <class> class-index-layout)))
             (unless (string-prefix? (symbol->string class-layout) layout)
               (error "bad layout for class"))))
         layout)
        ((slot . slots)
         (unless (= n (%slot-definition-index slot)) (error "bad allocation"))
         (call-with-values (lambda () (slot-protection-and-kind slot))
           (lambda (protection kind)
             (let init ((n n) (size (%slot-definition-size slot)))
               (cond
                ((zero? size) (lp n slots))
                (else
                 (unless (< n nfields) (error "bad nfields"))
                 (string-set! layout (* n 2) protection)
                 (string-set! layout (1+ (* n 2)) kind)
                 (init (1+ n) (1- size))))))))))))




;;;
;;; With all of this, we are now able to define subclasses of <class>.
;;;
(define (%prep-layout! class)
  (let* ((is-class? (and (memq <class> (struct-ref class class-index-cpl)) #t))
         (layout (%compute-layout (struct-ref class class-index-slots)
                                  (struct-ref/unboxed class class-index-nfields)
                                  is-class?)))
    (%init-layout! class layout)))

(define (make-standard-class class name dsupers dslots)
  (let ((z (make-struct/no-tail class)))
    (define (make-direct-slot-definition dslot)
      (let ((initargs (compute-direct-slot-definition-initargs z dslot)))
        (compute-direct-slot-definition z initargs)))

    (struct-set! z class-index-name name)
    (struct-set!/unboxed z class-index-nfields 0)
    (struct-set! z class-index-direct-supers dsupers)
    (struct-set! z class-index-direct-subclasses '())
    (struct-set! z class-index-direct-methods '())
    (struct-set! z class-index-redefined #f)
    (let ((cpl (compute-cpl z)))
      (struct-set! z class-index-cpl cpl)
      (when (memq <slot> cpl)
        (class-add-flags! z vtable-flag-goops-slot))
      (let* ((dslots (map make-direct-slot-definition dslots))
             (slots (allocate-slots z (build-slots-list dslots cpl))))
        (struct-set! z class-index-direct-slots dslots)
        (struct-set! z class-index-slots slots)))
    (for-each
     (lambda (super)
       (let ((subclasses (struct-ref super class-index-direct-subclasses)))
         (struct-set! super class-index-direct-subclasses
                      (cons z subclasses))))
     dsupers)
    (%prep-layout! z)
    z))

(define-syntax define-standard-class
  (syntax-rules ()
    ((define-standard-class name (super ...) #:metaclass meta slot ...)
     (define name
       (make-standard-class meta 'name (list super ...) '(slot ...))))
    ((define-standard-class name (super ...) slot ...)
     (define-standard-class name (super ...) #:metaclass <class> slot ...))))




;;;
;;; Sweet!  Now we can define <top> and <object>, and finish
;;; initializing the `direct-subclasses', `direct-supers', and `cpl'
;;; slots of <class>.
;;;
(define-standard-class <top> ())
(define-standard-class <object> (<top>))

;; The inheritance links for <top>, <object>, <class>, and <slot> were
;; partially initialized.  Correct them here.
(struct-set! <object> class-index-direct-subclasses (list <slot> <class>))
(struct-set! <class> class-index-direct-supers (list <object>))
(struct-set! <slot> class-index-direct-supers (list <object>))
(struct-set! <class> class-index-cpl (list <class> <object> <top>))
(struct-set! <slot> class-index-cpl (list <slot> <object> <top>))




;;;
;;; We can also define the various slot types, and finish initializing
;;; `direct-slots' and `slots' on <class> and <slot>.
;;;
(define-standard-class <foreign-slot> (<slot>))
(define-standard-class <protected-slot> (<foreign-slot>))
(define-standard-class <hidden-slot> (<foreign-slot>))
(define-standard-class <opaque-slot> (<foreign-slot>))
(define-standard-class <read-only-slot> (<foreign-slot>))
(define-standard-class <self-slot> (<read-only-slot>))
(define-standard-class <protected-opaque-slot> (<protected-slot>
                                                <opaque-slot>))
(define-standard-class <protected-hidden-slot> (<protected-slot>
                                                <hidden-slot>))
(define-standard-class <protected-read-only-slot> (<protected-slot>
                                                   <read-only-slot>))
(define-standard-class <scm-slot> (<protected-slot>))
(define-standard-class <int-slot> (<foreign-slot>))
(define-standard-class <float-slot> (<foreign-slot>))
(define-standard-class <double-slot> (<foreign-slot>))

(define (opaque-slot? slot) (is-a? slot <opaque-slot>))
(define (read-only-slot? slot) (is-a? slot <read-only-slot>))
(define (unboxed-slot? slot)
  (and (is-a? slot <foreign-slot>)
       (not (is-a? slot <self-slot>))
       (not (is-a? slot <protected-slot>))))



;;;
;;; Finally!  Initialize `direct-slots' and `slots' on <class>, and
;;; `slots' on <slot>.
;;;
(let ()
  (define-syntax-rule (cons-slot (name . initargs) tail)
    (cons (list 'name . initargs) tail))
  (define-syntax-rule (initialize-direct-slots! class fold-slots)
    (let ((specs (fold-slots macro-fold-right cons-slot '())))
      (define (make-direct-slot-definition spec)
        (let ((initargs (compute-direct-slot-definition-initargs class spec)))
          (compute-direct-slot-definition class initargs)))
      (struct-set! class class-index-direct-slots
                   (map make-direct-slot-definition specs))))
  (define (initialize-slots! class)
    (let ((slots (build-slots-list (class-direct-slots class)
                                   (class-precedence-list class))))
      (struct-set! class class-index-slots (allocate-slots class slots))))

  ;; Finish initializing <class> with the specialized slot kinds.
  (initialize-direct-slots! <class> fold-class-slots)

  (initialize-slots! <class>)
  (initialize-slots! <slot>)

  ;; Now that we're all done with that, mark <class> and <slot> as
  ;; static.
  (class-add-flags! <class> vtable-flag-goops-static)
  (class-add-flags! <slot> vtable-flag-goops-static))




;;;
;;; Now, to build out the class hierarchy.
;;;

(define-standard-class <procedure-class> (<class>))

(define-standard-class <applicable-struct-class>
  (<procedure-class>))
(class-add-flags! <applicable-struct-class>
                  vtable-flag-applicable-vtable)

(define-standard-class <applicable-struct-with-setter-class>
  (<applicable-struct-class>))
(class-add-flags! <applicable-struct-with-setter-class>
                  vtable-flag-setter-vtable)

(define-standard-class <applicable> (<top>))
(define-standard-class <applicable-struct> (<object> <applicable>)
  #:metaclass <applicable-struct-class>
  procedure)
(define-standard-class <applicable-struct-with-setter> (<applicable-struct>)
  #:metaclass <applicable-struct-with-setter-class>
  setter)
(define-standard-class <generic> (<applicable-struct>)
  #:metaclass <applicable-struct-class>
  methods
  (n-specialized #:init-value 0)
  (extended-by #:init-value ())
  effective-methods)
(define-standard-class <extended-generic> (<generic>)
  #:metaclass <applicable-struct-class>
  (extends #:init-value ()))
(define-standard-class <generic-with-setter> (<generic>
                                              <applicable-struct-with-setter>)
  #:metaclass <applicable-struct-with-setter-class>)
(define-standard-class <accessor> (<generic-with-setter>)
  #:metaclass <applicable-struct-with-setter-class>)
(define-standard-class <extended-generic-with-setter> (<extended-generic>
                                                       <generic-with-setter>)
  #:metaclass <applicable-struct-with-setter-class>)
(define-standard-class <extended-accessor> (<accessor>
                                            <extended-generic-with-setter>)
  #:metaclass <applicable-struct-with-setter-class>)

(define-standard-class <method> (<object>)
  generic-function
  specializers
  procedure
  formals
  body
  make-procedure)
(define-standard-class <accessor-method> (<method>)
  (slot-definition #:init-keyword #:slot-definition))

(define-standard-class <boolean> (<top>))
(define-standard-class <char> (<top>))
(define-standard-class <list> (<top>))
;; Not all pairs are lists, but there is code out there that relies on
;; (is-a? '(1 2 3) <list>) to work.  Terrible.  How to fix?
(define-standard-class <pair> (<list>))
(define-standard-class <null> (<list>))
(define-standard-class <string> (<top>))
(define-standard-class <symbol> (<top>))
(define-standard-class <vector> (<top>))
(define-standard-class <foreign> (<top>))
(define-standard-class <hashtable> (<top>))
(define-standard-class <fluid> (<top>))
(define-standard-class <dynamic-state> (<top>))
(define-standard-class <frame> (<top>))
(define-standard-class <vm-continuation> (<top>))
(define-standard-class <bytevector> (<top>))
(define-standard-class <uvec> (<bytevector>))
(define-standard-class <array> (<top>))
(define-standard-class <bitvector> (<top>))
(define-standard-class <number> (<top>))
(define-standard-class <complex> (<number>))
(define-standard-class <real> (<complex>))
(define-standard-class <integer> (<real>))
(define-standard-class <fraction> (<real>))
(define-standard-class <keyword> (<top>))
(define-standard-class <syntax> (<top>))
(define-standard-class <atomic-box> (<top>))
(define-standard-class <unknown> (<top>))
(define-standard-class <procedure> (<applicable>)
  #:metaclass <procedure-class>)
(define-standard-class <primitive-generic> (<procedure>)
  #:metaclass <procedure-class>)
(define-standard-class <port> (<top>))
(define-standard-class <input-port> (<port>))
(define-standard-class <output-port> (<port>))
(define-standard-class <input-output-port> (<input-port> <output-port>))

(define (inherit-applicable! class)
  "An internal routine to redefine a SMOB class that was added after
GOOPS was loaded, and on which scm_set_smob_apply installed an apply
function."
  ;; Why not use class-redefinition?  We would, except that loading the
  ;; compiler to compile effective methods can happen while GOOPS has
  ;; only been partially loaded, and loading the compiler might cause
  ;; SMOB types to be defined that need this facility.  Instead we make
  ;; a very specific hack, not a general solution.  Probably the right
  ;; solution is to avoid using the compiler, but that is another kettle
  ;; of fish.
  (unless (memq <applicable> (class-precedence-list class))
    (unless (null? (class-slots class))
      (error "SMOB object has slots?"))
    (for-each
     (lambda (super)
       (let ((subclasses (struct-ref super class-index-direct-subclasses)))
         (struct-set! super class-index-direct-subclasses
                      (delq class subclasses))))
     (struct-ref class class-index-direct-supers))
    (struct-set! class class-index-direct-supers (list <applicable>))
    (struct-set! class class-index-cpl (compute-cpl class))
    (let ((subclasses (struct-ref <applicable> class-index-direct-subclasses)))
      (struct-set! <applicable> class-index-direct-subclasses
                   (cons class subclasses)))))




;;;
;;; At this point we have defined the class hierarchy, and it's time to
;;; move on to instance allocation and generics.  Once we have generics,
;;; we'll fill out the metaobject protocol.
;;;
;;; Here we define a limited version of `make', so that we can allocate
;;; instances of specific classes.  This definition will be replaced
;;; later.
;;;
(define (%invalidate-method-cache! gf)
  (slot-set! gf 'effective-methods '())
  (recompute-generic-function-dispatch-procedure! gf))

;; Boot definition.
(define (invalidate-method-cache! gf)
  (%invalidate-method-cache! gf))

(define (make class . args)
  (cond
   ((or (eq? class <generic>) (eq? class <accessor>))
    (let ((z (make-struct/no-tail class #f '() 0 '())))
      (set-procedure-property! z 'name (get-keyword #:name args #f))
      (invalidate-method-cache! z)
      (when (eq? class <accessor>)
        (let ((setter (get-keyword #:setter args #f)))
          (when setter
            (slot-set! z 'setter setter))))
      z))
   (else
    (let ((z (%allocate-instance class)))
      (cond
       ((or (eq? class <method>) (eq? class <accessor-method>))
        (for-each (match-lambda
                   ((kw slot default)
                    (slot-set! z slot (get-keyword kw args default))))
                  '((#:generic-function generic-function #f)
                    (#:specializers specializers ())
                    (#:procedure procedure #f)
                    (#:formals formals ())
                    (#:body body ())
                    (#:make-procedure make-procedure #f))))
       ((memq <class> (class-precedence-list class))
        (class-add-flags! z (logior vtable-flag-goops-class
                                    vtable-flag-goops-valid))
        (for-each (match-lambda
                   ((kw slot default)
                    (slot-set! z slot (get-keyword kw args default))))
                  '((#:name name ???)
                    (#:dsupers direct-supers ())
                    (#:slots direct-slots ()))))
       (else
        (error "boot `make' does not support this class" class)))
      z))))




;;;
;;; Slot access.
;;;
;;; Before we go on, some notes about class redefinition.  In GOOPS,
;;; classes can be redefined.  Redefinition of a class marks the class
;;; as invalid, and instances will be lazily migrated over to the new
;;; representation as they are accessed.  Migration happens when
;;; `class-of' is called on an instance.  For more technical details on
;;; object redefinition, see struct.h.
;;;
;;; In the following interfaces, class-of handles the redefinition
;;; protocol.  I would think though that there is some thread-unsafety
;;; here though as the { class, object data } pair needs to be accessed
;;; atomically, not the { class, object } pair.
;;;
(define-inlinable (%class-slot-definition class slot-name kt kf)
  (let lp ((slots (struct-ref class class-index-slots)))
    (match slots
      ((slot . slots)
       (if (eq? (%slot-definition-name slot) slot-name)
           (kt slot)
           (lp slots)))
      (_ (kf)))))

(define (class-slot-definition class slot-name)
  (unless (class? class)
    (scm-error 'wrong-type-arg #f "Not a class: ~S" (list class) #f))
  (%class-slot-definition class slot-name
                          (lambda (slot) slot)
                          (lambda () #f)))

(define (slot-ref obj slot-name)
  "Return the value from @var{obj}'s slot with the nam var{slot_name}."
  (let ((class (class-of obj)))
    (define (have-slot slot)
      ((%slot-definition-slot-ref slot) obj))
    (define (no-slot)
      (unless (symbol? slot-name)
        (scm-error 'wrong-type-arg #f "Not a symbol: ~S"
                   (list slot-name) #f))
      (let ((val (slot-missing class obj slot-name)))
        (if (unbound? val)
            (slot-unbound class obj slot-name)
            val)))
    (%class-slot-definition class slot-name have-slot no-slot)))

(define (slot-set! obj slot-name value)
  "Set the slot named @var{slot_name} of @var{obj} to @var{value}."
  (let ((class (class-of obj)))
    (define (have-slot slot)
      ((%slot-definition-slot-set! slot) obj value))
    (define (no-slot)
      (unless (symbol? slot-name)
        (scm-error 'wrong-type-arg #f "Not a symbol: ~S"
                   (list slot-name) #f))
      (slot-missing class obj slot-name value))

    (%class-slot-definition class slot-name have-slot no-slot)))

(define (slot-bound? obj slot-name)
  "Return the value from @var{obj}'s slot with the nam var{slot_name}."
  (let ((class (class-of obj)))
    (define (have-slot slot)
      (not (unbound? ((%slot-definition-slot-ref/raw slot) obj))))
    (define (no-slot)
      (unless (symbol? slot-name)
        (scm-error 'wrong-type-arg #f "Not a symbol: ~S"
                   (list slot-name) #f))
      (not (unbound? (slot-missing class obj slot-name))))
    (%class-slot-definition class slot-name have-slot no-slot)))

(define (slot-exists? obj slot-name)
  "Return @code{#t} if @var{obj} has a slot named @var{slot_name}."
  (define (have-slot slot) #t)
  (define (no-slot)
    (unless (symbol? slot-name)
      (scm-error 'wrong-type-arg #f "Not a symbol: ~S"
                 (list slot-name) #f))
    #f)
  (%class-slot-definition (class-of obj) slot-name have-slot no-slot))

(begin-deprecated
 (define (check-slot-args class obj slot-name)
   (unless (eq? class (class-of obj))
     (scm-error 'wrong-type-arg #f "~S is not the class of ~S"
                (list class obj) #f))
   (unless (symbol? slot-name)
     (scm-error 'wrong-type-arg #f "Not a symbol: ~S"
                (list slot-name) #f)))

 (define (slot-ref-using-class class obj slot-name)
   (issue-deprecation-warning "slot-ref-using-class is deprecated.  "
                              "Use slot-ref instead.")
   (check-slot-args class obj slot-name)
   (slot-ref obj slot-name))

 (define (slot-set-using-class! class obj slot-name value)
   (issue-deprecation-warning "slot-set-using-class! is deprecated.  "
                              "Use slot-set! instead.")
   (check-slot-args class obj slot-name)
   (slot-set! obj slot-name value))

 (define (slot-bound-using-class? class obj slot-name)
   (issue-deprecation-warning "slot-bound-using-class? is deprecated.  "
                              "Use slot-bound? instead.")
   (check-slot-args class obj slot-name)
   (slot-bound? obj slot-name))

 (define (slot-exists-using-class? class obj slot-name)
   (issue-deprecation-warning "slot-exists-using-class? is deprecated.  "
                              "Use slot-exists? instead.")
   (check-slot-args class obj slot-name)
   (slot-exists? obj slot-name)))




;;;
;;; Method accessors.
;;;
(define (method-generic-function obj)
  "Return the generic function for the method @var{obj}."
  (unless (is-a? obj <method>)
    (scm-error 'wrong-type-arg #f "Not a method: ~S"
               (list obj) #f))
  (slot-ref obj 'generic-function))

(define (method-specializers obj)
  "Return specializers of the method @var{obj}."
  (unless (is-a? obj <method>)
    (scm-error 'wrong-type-arg #f "Not a method: ~S"
               (list obj) #f))
  (slot-ref obj 'specializers))

(define (method-procedure obj)
  "Return the procedure of the method @var{obj}."
  (unless (is-a? obj <method>)
    (scm-error 'wrong-type-arg #f "Not a method: ~S"
               (list obj) #f))
  (slot-ref obj 'procedure))




;;;
;;; Generic functions!
;;;
;;; Generic functions have an applicable-methods cache associated with
;;; them. Every distinct set of types that is dispatched through a
;;; generic adds an entry to the cache.  A composite dispatch procedure
;;; is recomputed every time an entry gets added to the cache, or when
;;; the cache is invalidated.
;;;
;;; In steady-state, this dispatch procedure is never regenerated; but
;;; during warm-up there is some churn.
;;;
;;; So what is the deal if warm-up happens in a multithreaded context?
;;; There is indeed a window between missing the cache for a certain set
;;; of arguments, and then updating the cache with the newly computed
;;; applicable methods. One of the updaters is liable to lose their new
;;; entry.
;;;
;;; This is actually OK though, because a subsequent cache miss for the
;;; race loser will just cause memoization to try again. The cache will
;;; eventually be consistent.  We're not mutating the old part of the
;;; cache, just consing on the new entry.
;;;
;;; It doesn't even matter if the dispatch procedure and the cache are
;;; inconsistent -- most likely the type-set that lost the dispatch
;;; procedure race will simply re-trigger a memoization, but since the
;;; winner isn't in the effective-methods cache, it will likely also
;;; re-trigger a memoization, and the cache will finally be consistent.
;;; As you can see there is a possibility for ping-pong effects, but
;;; it's unlikely given the shortness of the window between slot-set!
;;; invocations.
;;;
;;; We probably do need to use atomic access primitives to correctly
;;; handle concurrency, but that's a more general Guile concern.
;;;

(define-syntax arity-case
  (lambda (x)
    (syntax-case x ()
      ;; (arity-case n 2 foo bar)
      ;; => (case n
      ;;      ((0) (foo))
      ;;      ((1) (foo a))
      ;;      ((2) (foo a b))
      ;;      (else bar))
      ((arity-case n max form alternate)
       (let ((max (syntax->datum #'max)))
         #`(case n
             #,@(let lp ((n 0))
                  (let ((ids (map (lambda (n)
                                    (let* ((n (+ (char->integer #\a) n))
                                           (c (integer->char n)))
                                      (datum->syntax #'here (symbol c))))
                                  (iota n))))
                    #`(((#,n) (form #,@ids))
                       . #,(if (< n max)
                               (lp (1+ n))
                               #'()))))
             (else alternate)))))))

;;;
;;; These dispatchers are set as the "procedure" field of <generic>
;;; instances.  Unlike CLOS, in GOOPS a generic function can have
;;; multiple arities.
;;;
;;; We pre-generate fast dispatchers for applications of up to 20
;;; arguments.  More arguments than that will go through slower generic
;;; routines that cons arguments into a rest list.
;;;
(define (multiple-arity-dispatcher fv miss)
  (define-syntax dispatch
    (lambda (x)
      (define (build-clauses args)
        (let ((len (length (syntax->datum args))))
          #`((#,args ((vector-ref fv #,len) . #,args))
             . #,(syntax-case args ()
                   (() #'())
                   ((arg ... _) (build-clauses #'(arg ...)))))))
      (syntax-case x ()
        ((dispatch arg ...)
         #`(case-lambda
             #,@(build-clauses #'(arg ...))
             (args (apply miss args)))))))
  (arity-case (1- (vector-length fv)) 20 dispatch
              (lambda args
                (let ((nargs (length args)))
                  (if (< nargs (vector-length fv))
                      (apply (vector-ref fv nargs) args)
                      (apply miss args))))))

;;;
;;; The above multiple-arity-dispatcher is entirely sufficient, and
;;; should be fast enough.  Still, for no good reason we also have an
;;; arity dispatcher for generics that are only called with one arity.
;;;
(define (single-arity-dispatcher f nargs miss)
  (define-syntax-rule (dispatch arg ...)
    (case-lambda
      ((arg ...) (f arg ...))
      (args (apply miss args))))
  (arity-case nargs 20 dispatch
              (lambda args
                (if (eqv? (length args) nargs)
                    (apply f args)
                    (apply miss args)))))

;;;
;;; The guts of generic function dispatch are here.  Once we've selected
;;; an arity, we need to map from arguments to effective method.  Until
;;; we have `eqv?' specializers, this map is entirely a function of the
;;; types (classes) of the arguments.  So, we look in the cache to see
;;; if we have seen this set of concrete types, and if so we apply the
;;; previously computed effective method.  Otherwise we miss the cache,
;;; so we'll have to compute the right answer for this set of types, add
;;; the mapping to the cache, and apply the newly computed method.
;;;
;;; The cached mapping is invalidated whenever a new method is defined
;;; on this generic, or whenever the class hierarchy of any method
;;; specializer changes.
;;;
(define (single-arity-cache-dispatch cache nargs cache-miss)
  (match cache
    (() cache-miss)
    (((typev . cmethod) . cache)
     (cond
      ((eqv? nargs (vector-length typev))
       (let ((cache-miss (single-arity-cache-dispatch cache nargs cache-miss)))
         (define (type-ref n)
           (and (< n nargs) (vector-ref typev n)))
         (define-syntax args-match?
           (syntax-rules ()
             ((args-match?) #t)
             ((args-match? (arg type) (arg* type*) ...)
              ;; Check that the arg has the exact type that we saw.  It
              ;; could be that `type' is #f, which indicates the end of
              ;; the specializers list.  Once all specializers have been
              ;; examined, we don't need to look at any more arguments
              ;; to know that this is a cache hit.
              (or (not type)
                  (and (eq? (class-of arg) type)
                       (args-match? (arg* type*) ...))))))
         (define-syntax dispatch
           (lambda (x)
             (define (bind-types types k)
               (let lp ((types types) (n 0))
                 (syntax-case types ()
                   (() (k))
                   ((type . types)
                    #`(let ((type (type-ref #,n)))
                        #,(lp #'types (1+ n)))))))
             (syntax-case x ()
               ((dispatch arg ...)
                (with-syntax (((type ...) (generate-temporaries #'(arg ...))))
                  (bind-types
                   #'(type ...)
                   (lambda ()
                     #'(lambda (arg ...)
                         (if (args-match? (arg type) ...)
                             (cmethod arg ...)
                             (cache-miss arg ...))))))))))
         (arity-case nargs 20 dispatch
                     (lambda args
                       (define (args-match? args)
                         (let lp ((args args) (n 0))
                           (match args
                             ((arg . args)
                              (or (not (vector-ref typev n))
                                  (and (eq? (vector-ref typev n) (class-of arg))
                                       (lp args (1+ n)))))
                             (_ #t))))
                       (if (args-match? args)
                           (apply cmethod args)
                           (apply cache-miss args))))))
      (else
       (single-arity-cache-dispatch cache nargs cache-miss))))))

(define (compute-generic-function-dispatch-procedure gf)
  (define (seen-arities cache)
    (let lp ((arities 0) (cache cache))
      (match cache
        (() arities)
        (((typev . cmethod) . cache)
         (lp (logior arities (ash 1 (vector-length typev)))
             cache)))))
  (define (cache-miss . args)
    (memoize-generic-function-application! gf args)
    (apply gf args))
  (let* ((cache (slot-ref gf 'effective-methods))
         (arities (seen-arities cache))
         (max-arity (let lp ((max -1))
                      (if (< arities (ash 1 (1+ max)))
                          max
                          (lp (1+ max))))))
    (cond
     ((= max-arity -1)
      ;; Nothing in the cache.
      cache-miss)
     ((= arities (ash 1 max-arity))
      ;; Only one arity in the cache.
      (let* ((nargs max-arity)
             (f (single-arity-cache-dispatch cache nargs cache-miss)))
        (single-arity-dispatcher f nargs cache-miss)))
     (else
      ;; Multiple arities.
      (let ((fv (make-vector (1+ max-arity) #f)))
        (let lp ((n 0))
          (when (<= n max-arity)
            (let ((f (single-arity-cache-dispatch cache n cache-miss)))
              (vector-set! fv n f)
              (lp (1+ n)))))
        (multiple-arity-dispatcher fv cache-miss))))))

(define (recompute-generic-function-dispatch-procedure! gf)
  (slot-set! gf 'procedure
             (compute-generic-function-dispatch-procedure gf)))

(define (memoize-effective-method! gf args applicable)
  (define (record-types args)
    (let ((typev (make-vector (length args) #f)))
      (let lp ((n 0) (args args))
        (when (and (< n (slot-ref gf 'n-specialized))
                   (pair? args))
          (match args
            ((arg . args)
             (vector-set! typev n (class-of arg))
             (lp (1+ n) args)))))
      typev))
  (let* ((typev (record-types args))
         (compute-effective-method (if (eq? (class-of gf) <generic>)
                                       %compute-effective-method
                                       compute-effective-method))
         (cmethod (compute-effective-method gf applicable typev))
         (cache (acons typev cmethod (slot-ref gf 'effective-methods))))
    (slot-set! gf 'effective-methods cache)
    (recompute-generic-function-dispatch-procedure! gf)
    cmethod))

;;;
;;; If a method refers to `next-method' in its body, that method will be
;;; able to dispatch to the next most specific method.  The exact
;;; `next-method' implementation is only known at runtime, as it is a
;;; function of which precise argument types are being dispatched, which
;;; might be subclasses of the method's declared specializers.
;;;
;;; Guile implements `next-method' by binding it as a closure variable.
;;; An effective method is bound to a specific `next-method' by the
;;; `make-procedure' slot of a <method>, which returns the new closure.
;;;
(define (%compute-specialized-effective-method gf method types next-method)
  (match (slot-ref method 'make-procedure)
    (#f (method-procedure method))
    (make-procedure (make-procedure next-method))))

(define (compute-specialized-effective-method gf method types next-method)
  (%compute-specialized-effective-method gf method types next-method))

(define (%compute-effective-method gf methods types)
  (match methods
    ((method . methods)
     (let ((compute-specialized-effective-method
            (if (and (eq? (class-of gf) <generic>)
                     (eq? (class-of method) <method>))
                %compute-specialized-effective-method
                compute-specialized-effective-method)))
       (compute-specialized-effective-method
        gf method types
        (match methods
          (()
           (lambda args
             (no-next-method gf args)))
          (methods
           (let ((compute-effective-method (if (eq? (class-of gf) <generic>)
                                               %compute-effective-method
                                               compute-effective-method)))
             (compute-effective-method gf methods types)))))))))

;; Boot definition; overrided with a generic later.
(define (compute-effective-method gf methods types)
  (%compute-effective-method gf methods types))

;;;
;;; Memoization
;;;

(define (memoize-generic-function-application! gf args)
  (let ((applicable ((if (eq? (class-of gf) <generic>)
                         %compute-applicable-methods
                         compute-applicable-methods)
                     gf args)))
    (cond (applicable
           (memoize-effective-method! gf args applicable))
          (else
           (no-applicable-method gf args)))))

(define no-applicable-method
  (make <generic> #:name 'no-applicable-method))

(%goops-early-init)

;; Then load the rest of GOOPS


;; FIXME: deprecate.
(define min-fixnum (- (expt 2 29)))
(define max-fixnum (- (expt 2 29) 1))

;;
;; goops-error
;;
(define (goops-error format-string . args)
  (scm-error 'goops-error #f format-string args '()))

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
           (when (and (not (member meta all-cpls))
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
;;;   SLOT-DEFINITION ::= INSTANCE-OF-<SLOT> | (SLOT-NAME OPTION ...)
;;;   OPTION ::= KEYWORD VALUE
;;;

(define (make-class supers slots . options)
  (define (find-duplicate l)
    (match l
      (() #f)
      ((head . tail)
       (if (memq head tail)
           head
           (find-duplicate tail)))))
  (define (slot-spec->name slot-spec)
    (match slot-spec
      (((? symbol? name) . args) name)
      ;; We can get here when redefining classes.
      ((? slot? slot) (%slot-definition-name slot))))

  (let* ((name (get-keyword #:name options *unbound*))
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
          (tmp2 (find-duplicate (map slot-spec->name slots))))
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
(define-syntax class
  (lambda (x)
    (define (parse-options options)
      (syntax-case options ()
        (() #'())
        ((kw arg . options) (keyword? (syntax->datum #'kw))
         (with-syntax ((options (parse-options #'options)))
           (syntax-case #'kw ()
             (#:init-form
              #'(kw 'arg #:init-thunk (lambda () arg) . options))
             (_
              #'(kw arg . options)))))))
    (define (check-valid-kwargs args)
      (syntax-case args ()
        (() #'())
        ((kw arg . args) (keyword? (syntax->datum #'kw))
         #`(kw arg . #,(check-valid-kwargs #'args)))))
    (define (parse-slots-and-kwargs args)
      (syntax-case args ()
        (()
         #'(() ()))
        ((kw . _) (keyword? (syntax->datum #'kw))
         #`(() #,(check-valid-kwargs args)))
        (((name option ...) args ...)
         (with-syntax (((slots kwargs) (parse-slots-and-kwargs #'(args ...)))
                       ((option ...) (parse-options #'(option ...))))
           #'(((list 'name option ...) . slots) kwargs)))
        ((name args ...) (symbol? (syntax->datum #'name))
         (with-syntax (((slots kwargs) (parse-slots-and-kwargs #'(args ...))))
           #'(('(name) . slots) kwargs)))))
    (syntax-case x ()
      ((class (super ...) arg ...)
       (with-syntax ((((slot-def ...) (option ...))
                      (parse-slots-and-kwargs #'(arg ...))))
         #'(make-class (list super ...)
                       (list slot-def ...)
                       option ...))))))

(define-syntax define-class-pre-definition
  (lambda (x)
    (syntax-case x ()
      ((_ (k arg rest ...) out ...)
       (keyword? (syntax->datum #'k))
       (case (syntax->datum #'k)
         ((#:getter #:setter)
          #'(define-class-pre-definition (rest ...)
              out ...
              (when (or (not (defined? 'arg))
                        (not (is-a? arg <generic>)))
                (toplevel-define!
                 'arg
                 (ensure-generic (if (defined? 'arg) arg #f) 'arg)))))
         ((#:accessor)
          #'(define-class-pre-definition (rest ...)
              out ...
              (when (or (not (defined? 'arg))
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
(define-syntax define-generic
  (lambda (x)
    (syntax-case x ()
      ((define-generic name) (symbol? (syntax->datum #'name))
       #'(define name
           (if (and (defined? 'name) (is-a? name <generic>))
               (make <generic> #:name 'name)
               (ensure-generic (if (defined? 'name) name #f) 'name)))))))

(define-syntax define-extended-generic
  (lambda (x)
    (syntax-case x ()
      ((define-extended-generic name val) (symbol? (syntax->datum #'name))
       #'(define name (make-extended-generic val 'name))))))

(define-syntax define-extended-generics
  (lambda (x)
    (define (id-append ctx a b)
      (datum->syntax ctx (symbol-append (syntax->datum a) (syntax->datum b))))
    (syntax-case x ()
      ((define-extended-generic (name ...) #:prefix (prefix ...))
       (and (and-map symbol? (syntax->datum #'(name ...)))
            (and-map symbol? (syntax->datum #'(prefix ...))))
       (with-syntax ((((val ...)) (map (lambda (name)
                                         (map (lambda (prefix)
                                                (id-append name prefix name))
                                              #'(prefix ...)))
                                       #'(name ...))))
         #'(begin
             (define-extended-generic name (list val ...))
             ...))))))

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
    (when (is-a? generic <extended-generic>)
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

;; Note: `a' and `b' can have unequal lengths (i.e. one can be one
;; element longer than the other when we have a dotted parameter
;; list). For instance, with the call
;;
;;   (M 1)
;;
;; with
;;
;;   (define-method M (a . l) ....)
;;   (define-method M (a) ....)
;;
;; we consider that the second method is more specific.
;;
;; Precondition: `a' and `b' are methods and are applicable to `types'.
(define (%method-more-specific? a b types)
  (let lp ((a-specializers (method-specializers a))
           (b-specializers (method-specializers b))
           (types types))
    (cond
     ;; (a) less specific than (a b ...) or (a . b)
     ((null? a-specializers) #t)
     ;; (a b ...) or (a . b) less specific than (a)
     ((null? b-specializers) #f)
     ;; (a . b) less specific than (a b ...)
     ((not (pair? a-specializers)) #f)
     ;; (a b ...) more specific than (a . b)
     ((not (pair? b-specializers)) #t)
     (else
      (let ((a-specializer (car a-specializers))
            (b-specializer (car b-specializers))
            (a-specializers (cdr a-specializers))
            (b-specializers (cdr b-specializers))
            (type (car types))
            (types (cdr types)))
        (if (eq? a-specializer b-specializer)
            (lp a-specializers b-specializers types)
            (let lp ((cpl (class-precedence-list type)))
              (let ((elt (car cpl)))
                (cond
                 ((eq? a-specializer elt) #t)
                 ((eq? b-specializer elt) #f)
                 (else (lp (cdr cpl))))))))))))

(define (%sort-applicable-methods methods types)
  (sort methods (lambda (a b) (%method-more-specific? a b types))))

(define (generic-function-methods obj)
  "Return the methods of the generic function @var{obj}."
  (define (fold-upward method-lists gf)
    (cond
     ((is-a? gf <extended-generic>)
      (let lp ((method-lists method-lists) (gfs (slot-ref gf 'extends)))
        (match gfs
          (() method-lists)
          ((gf . gfs)
           (lp (fold-upward (cons (slot-ref gf 'methods) method-lists) gf)
               gfs)))))
     (else method-lists)))
  (define (fold-downward method-lists gf)
    (let lp ((method-lists (cons (slot-ref gf 'methods) method-lists))
             (gfs (slot-ref gf 'extended-by)))
      (match gfs
        (() method-lists)
        ((gf . gfs)
         (lp (fold-downward method-lists gf) gfs)))))
  (unless (is-a? obj <generic>)
    (scm-error 'wrong-type-arg #f "Not a generic: ~S"
               (list obj) #f))
  (concatenate (fold-downward (fold-upward '() obj) obj)))

(define (%compute-applicable-methods gf args)
  (define (method-applicable? m types)
    (let ((specs (method-specializers m)))
      (cond
       ((and (is-a? m <accessor-method>)
             (or (null? specs) (null? types)
                 (not (eq? (car specs) (car types)))))
        ;; Slot accessor methods are added to each subclass with the
        ;; slot.  They only apply to that specific concrete class, which
        ;; appears as the first argument.
        #f)
       (else
        (let lp ((specs specs) (types types))
          (cond
           ((null? specs) (null? types))
           ((not (pair? specs)) #t)
           ((null? types) #f)
           (else
            (and (memq (car specs) (class-precedence-list (car types)))
                 (lp (cdr specs) (cdr types))))))))))
  (let ((n (length args))
        (types (map class-of args)))
    (let lp ((methods (generic-function-methods gf))
             (applicable '()))
      (if (null? methods)
          (and (not (null? applicable))
               (%sort-applicable-methods applicable types))
          (let ((m (car methods)))
            (lp (cdr methods)
                (if (method-applicable? m types)
                    (cons m applicable)
                    applicable)))))))

(define compute-applicable-methods %compute-applicable-methods)

(define (toplevel-define! name val)
  (module-define! (current-module) name val))

(define-syntax define-method
  (syntax-rules (setter)
    ((_ ((setter name) . args) body ...)
     (begin
       (when (or (not (defined? 'name))
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
       (when (or (not (defined? 'name))
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
;;; {Utilities}
;;;
;;; These are useful when dealing with method specializers, which might
;;; have a rest argument.
;;;

(define (map* fn . l) 		; A map which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car l)) '())
   ((pair? (car l)) (cons (apply fn      (map car l))
			  (apply map* fn (map cdr l))))
   (else            (apply fn l))))

(define (for-each* fn . l) 	; A for-each which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car l)) '())
   ((pair? (car l)) (apply fn (map car l)) (apply for-each* fn (map cdr l)))
   (else            (apply fn l))))

(define (length* ls)
  (do ((n 0 (+ 1 n))
       (ls ls (cdr ls)))
      ((not (pair? ls)) n)))

;;;
;;; {add-method!}
;;;

(define (add-method-in-classes! m)
  ;; Add method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
               (let ((dm (class-direct-methods x)))
                 (unless (memq m dm)
                   (struct-set! x class-index-direct-methods (cons m dm)))))
             (method-specializers m)))

(define (remove-method-in-classes! m)
  ;; Remove method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
               (struct-set! x
                            class-index-direct-methods
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
  (slot-set! gf 'n-specialized (calculate-n-specialized gf))
  (%invalidate-method-cache! gf)
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
(define (slot-init-function class slot-name)
  (%slot-definition-init-thunk (or (class-slot-definition class slot-name)
                                   (error "slot not found" slot-name))))

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

(define-method (write (slot <slot>) file)
  (let ((class (class-of slot)))
    (if (and (slot-bound? class 'name)
             (slot-bound? slot 'name))
        (begin
          (display "#<" file)
          (display (class-name class) file)
          (display #\space file)
          (display (%slot-definition-name slot) file)
          (display #\space file)
          (display-address slot file)
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

(define (find-subclass super name)
  (let lp ((classes (class-direct-subclasses super)))
    (cond
     ((null? classes)
      (error "class not found" name))
     ((and (slot-bound? (car classes) 'name)
           (eq? (class-name (car classes)) name))
      (car classes))
     (else
      (lp (cdr classes))))))

;; A record type.
(define <module> (find-subclass <top> '<module>))

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

(define (class-slot-ref class slot-name)
  (let ((slot (class-slot-definition class slot-name)))
    (unless (memq (%slot-definition-allocation slot) '(#:class #:each-subclass))
      (slot-missing class slot-name))
    (let ((x ((%slot-definition-slot-ref/raw slot) #f)))
      (if (unbound? x)
          (slot-unbound class slot-name)
          x))))

(define (class-slot-set! class slot-name value)
  (let ((slot (class-slot-definition class slot-name)))
    (unless (memq (%slot-definition-allocation slot) '(#:class #:each-subclass))
      (slot-missing class slot-name))
    ((%slot-definition-slot-set! slot) #f value)))

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
  (let* ((class (class-of self))
         (clone (%allocate-instance class))
         (slots (map slot-definition-name (class-slots class))))
    (for-each (lambda (slot)
                (when (slot-bound? self slot)
                  (slot-set! clone slot (slot-ref self slot))))
              slots)
    clone))

(define-method (deep-clone  (self <object>))
  (let* ((class (class-of self))
         (clone (%allocate-instance class))
         (slots (map slot-definition-name (class-slots class))))
    (for-each (lambda (slot)
                (when (slot-bound? self slot)
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
  ;;            1. Remove accessor methods from the old class
  ;;            2. Patch the occurences of new in the specializers by old
  ;;            3. Displace the methods from old to new
  (remove-class-accessors! old)                                 ;; -1-
  (let ((methods (class-direct-methods new)))
    (for-each (lambda (m)
                 (update-direct-method! m new old))     ;; -2-
              methods)
    (struct-set! new
                 class-index-direct-methods
                 (append methods (class-direct-methods old))))

  ;; Substitute old for new in new cpl
  (set-car! (struct-ref new class-index-cpl) old)

  ;; Remove the old class from the direct-subclasses list of its super classes
  (for-each (lambda (c) (struct-set! c class-index-direct-subclasses
                                     (delv! old (class-direct-subclasses c))))
            (class-direct-supers old))

  ;; Replace the new class with the old in the direct-subclasses of the supers
  (for-each (lambda (c)
              (struct-set! c class-index-direct-subclasses
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
  (struct-set! new class-index-redefined old)
  (class-clear-flags! new vtable-flag-goops-valid) ;must come after slot-set!

  old)

;;;
;;; remove-class-accessors!
;;;

(define-method (remove-class-accessors! (c <class>))
  (for-each (lambda (m)
              (when (is-a? m <accessor-method>)
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
    (when (pair? l)
      (when (eqv? (car l) old)
        (set-car! l new))
      (loop (cdr l)))))

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
   (lambda (slot)
     (let ((getter (%slot-definition-getter slot))
           (setter (%slot-definition-setter slot))
           (accessor-setter setter)
           (accessor (%slot-definition-accessor slot)))
       (when getter
         (add-method! getter (compute-getter-method class slot)))
       (when setter
         (add-method! setter (compute-setter-method class slot)))
       (when accessor
         (add-method! accessor (compute-getter-method class slot))
         (add-method! (accessor-setter accessor)
                      (compute-setter-method class slot)))))
   slots))

(define-method (compute-getter-method (class <class>) slot)
  (make <accessor-method>
    #:specializers (list class)
    #:procedure (slot-definition-slot-ref slot)
    #:slot-definition slot))

(define-method (compute-setter-method (class <class>) slot)
  (make <accessor-method>
    #:specializers (list class <top>)
    #:procedure (slot-definition-slot-set! slot)
    #:slot-definition slot))

(define (make-generic-bound-check-getter proc)
  (lambda (o)
    (let ((val (proc o)))
      (if (unbound? val)
          (slot-unbound o)
          val))))

;;; compute-cpl
;;;

;; Replace the bootstrap compute-cpl with this definition.
(define compute-cpl
  (make <generic> #:name 'compute-cpl))

(define-method (compute-cpl (class <class>))
  (compute-std-cpl class class-direct-supers))

;;; compute-get-n-set
;;;
(define compute-get-n-set
  (make <generic> #:name 'compute-get-n-set))

(define-method (compute-get-n-set (class <class>) s)
  (define (class-slot-init-value)
    (let ((thunk (slot-definition-init-thunk s)))
      (if thunk
          (thunk)
          (slot-definition-init-value s))))

  (define (make-closure-variable class value)
    (list (lambda (o) value)
          (lambda (o v) (set! value v))))

  (case (slot-definition-allocation s)
    ((#:instance) ;; Instance slot
     ;; get-n-set is just its offset
     (let ((already-allocated (struct-ref/unboxed class class-index-nfields)))
       (struct-set!/unboxed class class-index-nfields (+ already-allocated 1))
       already-allocated))

    ((#:class) ;; Class slot
     ;; Class-slots accessors are implemented as 2 closures around
     ;; a Scheme variable. As instance slots, class slots must be
     ;; unbound at init time.
     (let ((name (slot-definition-name s)))
       (if (memq name (map slot-definition-name (class-direct-slots class)))
           ;; This slot is direct; create a new shared variable
           (make-closure-variable class (class-slot-init-value))
           ;; Slot is inherited. Find its definition in superclass
           (let lp ((cpl (cdr (class-precedence-list class))))
             (match cpl
               ((super . cpl)
                (let ((s (class-slot-definition super name)))
                  (if s
                      (list (slot-definition-slot-ref s)
                            (slot-definition-slot-set! s))
                      ;; Multiple inheritance means that we might have
                      ;; to look deeper in the CPL.
                      (lp cpl)))))))))

    ((#:each-subclass) ;; slot shared by instances of direct subclass.
     ;; (Thomas Buerger, April 1998)
     (make-closure-variable class (class-slot-init-value)))

    ((#:virtual) ;; No allocation
     ;; slot-ref and slot-set! function must be given by the user
     (let ((get (get-keyword #:slot-ref  (slot-definition-options s) #f))
           (set (get-keyword #:slot-set! (slot-definition-options s) #f)))
       (unless (and get set)
         (goops-error "You must supply a #:slot-ref and a #:slot-set! in ~S" s))
       (list get set)))
    (else    (next-method))))

(define-method (compute-get-n-set (o <object>) s)
  (goops-error "Allocation \"~S\" is unknown" (slot-definition-allocation s)))

(define-method (compute-slots (class <class>))
  (build-slots-list (class-direct-slots class)
                    (class-precedence-list class)))

;;;
;;; {Initialize}
;;;

;; FIXME: This could be much more efficient.
(define (%initialize-object obj initargs)
  "Initialize the object @var{obj} with the given arguments
var{initargs}."
  (define (valid-initargs? initargs)
    (match initargs
      (() #t)
      (((? keyword?) _ . initargs) (valid-initargs? initargs))
      (_ #f)))
  (unless (instance? obj)
    (scm-error 'wrong-type-arg #f "Not an object: ~S"
               (list obj) #f))
  (unless (valid-initargs? initargs)
    (scm-error 'wrong-type-arg #f "Invalid initargs: ~S"
               (list initargs) #f))
  (let ((class (class-of obj)))
    (define (get-initarg kw)
      (if kw
          ;; Inlined get-keyword to avoid checking initargs for validity
          ;; each time.
          (let lp ((initargs initargs))
            (match initargs
              ((kw* val . initargs)
               (if (eq? kw* kw)
                   val
                   (lp initargs)))
              (_ *unbound*)))
          *unbound*))
    (let lp ((slots (struct-ref class class-index-slots)))
      (match slots
        (() obj)
        ((slot . slots)
         (define (initialize-slot! value)
           ((%slot-definition-slot-set! slot) obj value))
         (let ((initarg (get-initarg (%slot-definition-init-keyword slot))))
           (cond
            ((not (unbound? initarg))
             (initialize-slot! initarg))
            ((%slot-definition-init-thunk slot)
             => (lambda (init-thunk)
                  (unless (memq (slot-definition-allocation slot)
                                '(#:class #:each-subclass))
                    (initialize-slot! (init-thunk)))))))
         (lp slots))))))

(define-method (initialize (object <object>) initargs)
  (%initialize-object object initargs))

(define-method (initialize (slot <slot>) initargs)
  (next-method)
  (struct-set! slot slot-index-options initargs)
  (let ((init-thunk (%slot-definition-init-thunk slot)))
    (when init-thunk
      (unless (thunk? init-thunk)
        (goops-error "Bad init-thunk for slot `~S': ~S"
                     (%slot-definition-name slot) init-thunk)))))

(define-method (initialize (class <class>) initargs)
  (define (make-direct-slot-definition dslot)
    (let ((initargs (compute-direct-slot-definition-initargs class dslot)))
      (compute-direct-slot-definition class initargs)))

  (next-method)
  (class-add-flags! class (logior vtable-flag-goops-class
                                  vtable-flag-goops-valid))
  (struct-set! class class-index-name (get-keyword #:name initargs '???))
  (struct-set!/unboxed class class-index-nfields 0)
  (struct-set! class class-index-direct-supers
               (get-keyword #:dsupers initargs '()))
  (struct-set! class class-index-direct-subclasses '())
  (struct-set! class class-index-direct-methods '())
  (struct-set! class class-index-redefined #f)
  (struct-set! class class-index-cpl (compute-cpl class))
  (when (get-keyword #:static-slot-allocation? initargs #f)
    (match (filter class-has-statically-allocated-slots?
                   (class-precedence-list class))
      (()
       (class-add-flags! class vtable-flag-goops-static))
      (classes
       (error "Class has superclasses with static slot allocation" classes))))
  (struct-set! class class-index-direct-slots
               (map (lambda (slot)
                      (if (slot? slot)
                          slot
                          (make-direct-slot-definition slot)))
                    (get-keyword #:slots initargs '())))
  (struct-set! class class-index-slots
               (allocate-slots class (compute-slots class)))

  ;; This is a hack.
  (when (memq <slot> (struct-ref class class-index-cpl))
    (class-add-flags! class vtable-flag-goops-slot))

  ;; Build getters - setters - accessors
  (compute-slot-accessors class (struct-ref class class-index-slots))

  ;; Update the "direct-subclasses" of each inherited classes
  (for-each (lambda (x)
              (let ((dsubs (struct-ref x class-index-direct-subclasses)))
                (struct-set! x class-index-direct-subclasses
                             (cons class dsubs))))
            (struct-ref class class-index-direct-supers))

  ;; Compute struct layout of instances, set the `layout' slot, and
  ;; update class flags.
  (%prep-layout! class))

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

(define-method (initialize (applicable-struct <applicable-struct-with-setter>)
                           initargs)
  (next-method)
  (slot-set! applicable-struct 'setter (get-keyword #:setter initargs #f)))

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
    (invalidate-method-cache! generic)))

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
    (for-each
     (lambda (slot)
       (if (and (slot-exists? old-instance slot)
                (eq? (%slot-definition-allocation
                      (class-slot-definition old-class slot))
                     #:instance)
                (slot-bound? old-instance slot))
           ;; Slot was present and allocated in old instance; copy it
           (slot-set! new-instance slot (slot-ref old-instance slot))
           ;; slot was absent; initialize it with its default value
           (let ((init (slot-init-function new-class slot)))
             (when init
               (slot-set! new-instance slot (init))))))
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
  (%allocate-instance class))

(define-method (make-instance (class <class>) . initargs)
  (let ((instance (allocate-instance class initargs)))
    (initialize instance initargs)
    instance))

(define make make-instance)

;;;
;;; {apply-generic}
;;;
;;; Protocol for calling generic functions, intended to be used when
;;; applying subclasses of <generic> and <generic-with-setter>.  The
;;; code below is similar to the first MOP described in AMOP.
;;;
;;; Note that standard generic functions dispatch only on the classes of
;;; the arguments, and the result of such dispatch can be memoized.  The
;;; `dispatch-generic-function-application-from-cache' routine
;;; implements this.  `apply-generic' isn't called currently; the
;;; generic function MOP was never fully implemented in GOOPS.  However
;;; now that GOOPS is implemented entirely in Scheme (2015) it's much
;;; easier to complete this work.  Contributions gladly accepted!
;;; Please read the AMOP first though :)
;;;
;;; The protocol is:
;;;
;;; 	+ apply-generic (gf args)
;;;		+ compute-applicable-methods (gf args ...)
;;;			+ sort-applicable-methods (gf methods args)
;;;		+ apply-methods (gf methods args)
;;;
;;; apply-methods calls make-next-method to build the "continuation" of
;;; a method.  Applying a next-method will call apply-next-method which
;;; in turn will call apply again to call effectively the following
;;; method.  (This paragraph is out of date but is kept so that maybe it
;;; illuminates some future hack.)
;;;

(define-method (apply-generic (gf <generic>) args)
  (when (null? (slot-ref gf 'methods))
    (no-method gf args))
  (let ((methods (compute-applicable-methods gf args)))
    (if methods
        (apply-methods gf (sort-applicable-methods gf methods args) args)
        (no-applicable-method gf args))))

;; compute-applicable-methods is bound to %compute-applicable-methods.
(define compute-applicable-methods
  (let ((gf (make <generic> #:name 'compute-applicable-methods)))
    (add-method! gf (method ((gf <generic>) args)
                      (%compute-applicable-methods gf args)))
    gf))

(define-method (sort-applicable-methods (gf <generic>) methods args)
  (%sort-applicable-methods methods (map class-of args)))

(define-method (method-more-specific? (m1 <method>) (m2 <method>) targs)
  (%method-more-specific? m1 m2 targs))

(define compute-effective-method
  (let ((gf (make <generic> #:name 'compute-effective-method)))
    (add-method! gf (method ((gf <generic>) methods typev)
                      (%compute-effective-method gf methods typev)))
    gf))

(define compute-specialized-effective-method
  (let ((gf (make <generic> #:name 'compute-specialized-effective-method)))
    (add-method!
     gf
     (method ((gf <generic>) (method <method>) typev next)
       (%compute-specialized-effective-method gf method typev next)))
    gf))

(define-method (compute-specialized-effective-method (gf <generic>)
                                                     (m <accessor-method>)
                                                     typev
                                                     next)
  (let ((name (slot-definition-name (accessor-method-slot-definition m))))
    (match typev
      (#(class)
       (slot-definition-slot-ref (class-slot-definition class name)))
      (#(class _)
       (slot-definition-slot-set! (class-slot-definition class name)))
      (_
       (next-method)))))

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
;;; {Final initialization}
;;;

;; Tell C code that the main bulk of Goops has been loaded
(%goops-loaded)




;;;
;;; {SMOB and port classes}
;;;

(begin-deprecated
 (define-public <arbiter> (find-subclass <top> '<arbiter>))
 (define-public <async> (find-subclass <top> '<async>)))

(define <promise> (find-subclass <top> '<promise>))
(define <thread> (find-subclass <top> '<thread>))
(define <mutex> (find-subclass <top> '<mutex>))
(define <condition-variable> (find-subclass <top> '<condition-variable>))
(define <regexp> (find-subclass <top> '<regexp>))
(define <hook> (find-subclass <top> '<hook>))
(define <bitvector> (find-subclass <top> '<bitvector>))
(define <random-state> (find-subclass <top> '<random-state>))
(define <directory> (find-subclass <top> '<directory>))
(define <array> (find-subclass <top> '<array>))
(define <character-set> (find-subclass <top> '<character-set>))
(define <dynamic-object> (find-subclass <top> '<dynamic-object>))
(define <guardian> (find-subclass <applicable> '<guardian>))
(define <macro> (find-subclass <top> '<macro>))

(define (define-class-subtree class)
  (define! (class-name class) class)
  (for-each define-class-subtree (class-direct-subclasses class)))

(define-class-subtree (find-subclass <port> '<file-port>))
