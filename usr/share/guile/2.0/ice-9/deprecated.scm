;;;; Copyright (C) 2003, 2005, 2006, 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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

(define-module (ice-9 deprecated)
  #:export (substring-move-left! substring-move-right!
            dynamic-maybe-call dynamic-maybe-link
            try-module-linked try-module-dynamic-link
            list* feature? eval-case unmemoize-expr
            $asinh
            $acosh
            $atanh
            $sqrt
            $abs
            $exp
            $expt
            $log
            $sin
            $cos
            $tan
            $asin
            $acos
            $atan
            $sinh
            $cosh
            $tanh
            closure?
            %nil
            @bind
            bad-throw
            error-catching-loop
            error-catching-repl
            scm-style-repl
            apply-to-args
            has-suffix?
            scheme-file-suffix
            get-option
            for-next-option
            display-usage-report
            transform-usage-lambda
            collect
            assert-repl-silence
            assert-repl-print-unspecified
            assert-repl-verbosity
            set-repl-prompt!
            set-batch-mode?!
            repl
            pre-unwind-handler-dispatch
            default-pre-unwind-handler
            handle-system-error
            stack-saved?
            the-last-stack
            save-stack
            named-module-use!
            top-repl
            turn-on-debugging
            read-hash-procedures
            process-define-module
            fluid-let-syntax
            set-system-module!
            char-code-limit
            generalized-vector?
            generalized-vector-length
            generalized-vector-ref
            generalized-vector-set!
            generalized-vector->list))


;;;; Deprecated definitions.

(define substring-move-left!
  (lambda args
    (issue-deprecation-warning
     "`substring-move-left!' is deprecated.  Use `substring-move!' instead.")
    (apply substring-move! args)))
(define substring-move-right!
  (lambda args
    (issue-deprecation-warning
     "`substring-move-right!' is deprecated.  Use `substring-move!' instead.")
    (apply substring-move! args)))



;; This method of dynamically linking Guile Extensions is deprecated.
;; Use `load-extension' explicitly from Scheme code instead.

(define (split-c-module-name str)
  (let loop ((rev '())
	     (start 0)
	     (pos 0)
	     (end (string-length str)))
    (cond
     ((= pos end)
      (reverse (cons (string->symbol (substring str start pos)) rev)))
     ((eq? (string-ref str pos) #\space)
      (loop (cons (string->symbol (substring str start pos)) rev)
	    (+ pos 1)
	    (+ pos 1)
	    end))
     (else
      (loop rev start (+ pos 1) end)))))

(define (convert-c-registered-modules dynobj)
  (let ((res (map (lambda (c)
		    (list (split-c-module-name (car c)) (cdr c) dynobj))
		  (c-registered-modules))))
    (c-clear-registered-modules)
    res))

(define registered-modules '())

(define (register-modules dynobj)
  (set! registered-modules
	(append! (convert-c-registered-modules dynobj)
		 registered-modules)))

(define (warn-autoload-deprecation modname)
  (issue-deprecation-warning
   "Autoloading of compiled code modules is deprecated."
   "Write a Scheme file instead that uses `load-extension'.")
  (issue-deprecation-warning
   (simple-format #f "(You just autoloaded module ~S.)" modname)))

(define (init-dynamic-module modname)
  ;; Register any linked modules which have been registered on the C level
  (register-modules #f)
  (or-map (lambda (modinfo)
	    (if (equal? (car modinfo) modname)
		(begin
		  (warn-autoload-deprecation modname)
		  (set! registered-modules (delq! modinfo registered-modules))
		  (let ((mod (resolve-module modname #f)))
		    (save-module-excursion
		     (lambda ()
		       (set-current-module mod)
		       (set-module-public-interface! mod mod)
		       (dynamic-call (cadr modinfo) (caddr modinfo))
		       ))
		    #t))
		#f))
	  registered-modules))

(define (dynamic-maybe-call name dynobj)
  (issue-deprecation-warning
   "`dynamic-maybe-call' is deprecated.  "
   "Wrap `dynamic-call' in a `false-if-exception' yourself.")
  (false-if-exception (dynamic-call name dynobj)))


(define (dynamic-maybe-link filename)
  (issue-deprecation-warning
   "`dynamic-maybe-link' is deprecated.  "
   "Wrap `dynamic-link' in a `false-if-exception' yourself.")
  (false-if-exception (dynamic-link filename)))

(define (find-and-link-dynamic-module module-name)
  (define (make-init-name mod-name)
    (string-append "scm_init"
		   (list->string (map (lambda (c)
					(if (or (char-alphabetic? c)
						(char-numeric? c))
					    c
					    #\_))
				      (string->list mod-name)))
		   "_module"))

  ;; Put the subdirectory for this module in the car of SUBDIR-AND-LIBNAME,
  ;; and the `libname' (the name of the module prepended by `lib') in the cdr
  ;; field.  For example, if MODULE-NAME is the list (inet tcp-ip udp), then
  ;; SUBDIR-AND-LIBNAME will be the pair ("inet/tcp-ip" . "libudp").
  (let ((subdir-and-libname
	 (let loop ((dirs "")
		    (syms module-name))
	   (if (null? (cdr syms))
	       (cons dirs (string-append "lib" (symbol->string (car syms))))
	       (loop (string-append dirs (symbol->string (car syms)) "/")
		     (cdr syms)))))
	(init (make-init-name (apply string-append
				     (map (lambda (s)
					    (string-append "_"
							   (symbol->string s)))
					  module-name)))))
    (let ((subdir (car subdir-and-libname))
	  (libname (cdr subdir-and-libname)))

      ;; Now look in each dir in %LOAD-PATH for `subdir/libfoo.la'.  If that
      ;; file exists, fetch the dlname from that file and attempt to link
      ;; against it.  If `subdir/libfoo.la' does not exist, or does not seem
      ;; to name any shared library, look for `subdir/libfoo.so' instead and
      ;; link against that.
      (let check-dirs ((dir-list %load-path))
	(if (null? dir-list)
	    #f
	    (let* ((dir (in-vicinity (car dir-list) subdir))
		   (sharlib-full
		    (or (try-using-libtool-name dir libname)
			(try-using-sharlib-name dir libname))))
	      (if (and sharlib-full (file-exists? sharlib-full))
		  (link-dynamic-module sharlib-full init)
		  (check-dirs (cdr dir-list)))))))))

(define (try-using-libtool-name libdir libname)
  (let ((libtool-filename (in-vicinity libdir
				       (string-append libname ".la"))))
    (and (file-exists? libtool-filename)
	 libtool-filename)))

(define (try-using-sharlib-name libdir libname)
  (in-vicinity libdir (string-append libname ".so")))

(define (link-dynamic-module filename initname)
  ;; Register any linked modules which have been registered on the C level
  (register-modules #f)
  (let ((dynobj (dynamic-link filename)))
    (dynamic-call initname dynobj)
    (register-modules dynobj)))

(define (try-module-linked module-name)
  (issue-deprecation-warning
   "`try-module-linked' is deprecated."
   "See the manual for how more on C extensions.")
  (init-dynamic-module module-name))

(define (try-module-dynamic-link module-name)
  (issue-deprecation-warning
   "`try-module-dynamic-link' is deprecated."
   "See the manual for how more on C extensions.")
  (and (find-and-link-dynamic-module module-name)
       (init-dynamic-module module-name)))


(define (list* . args)
  (issue-deprecation-warning "'list*' is deprecated.  Use 'cons*' instead.")
  (apply cons* args))

(define (feature? sym)
  (issue-deprecation-warning
   "`feature?' is deprecated.  Use `provided?' instead.")
  (provided? sym))

(define-macro (eval-case . clauses)
  (issue-deprecation-warning
   "`eval-case' is deprecated.  Use `eval-when' instead.")
  ;; Practically speaking, eval-case only had load-toplevel and else as
  ;; conditions.
  (cond
   ((assoc-ref clauses '(load-toplevel))
    => (lambda (exps)
         ;; the *unspecified so that non-toplevel definitions will be
         ;; caught
         `(begin *unspecified* . ,exps)))
   ((assoc-ref clauses 'else)
    => (lambda (exps)
         `(begin *unspecified* . ,exps)))
   (else
    `(begin))))

;; The strange prototype system for uniform arrays has been
;; deprecated.
(read-hash-extend
 #\y
 (lambda (c port)
   (issue-deprecation-warning
    "The `#y' bytevector syntax is deprecated.  Use `#s8' instead.")
   (let ((x (read port)))
     (cond
      ((list? x) (list->s8vector x))
      (else (error "#y needs to be followed by a list" x))))))

(define (unmemoize-expr . args)
  (issue-deprecation-warning
   "`unmemoize-expr' is deprecated. Use `unmemoize-expression' instead.")
  (apply unmemoize-expression args))

(define ($asinh z)
  (issue-deprecation-warning
   "`$asinh' is deprecated.  Use `asinh' instead.")
  (asinh z))
(define ($acosh z)
  (issue-deprecation-warning
   "`$acosh' is deprecated.  Use `acosh' instead.")
  (acosh z))
(define ($atanh z)
  (issue-deprecation-warning
   "`$atanh' is deprecated.  Use `atanh' instead.")
  (atanh z))
(define ($sqrt z)
  (issue-deprecation-warning
   "`$sqrt' is deprecated.  Use `sqrt' instead.")
  (sqrt z))
(define ($abs z)
  (issue-deprecation-warning
   "`$abs' is deprecated.  Use `abs' instead.")
  (abs z))
(define ($exp z)
  (issue-deprecation-warning
   "`$exp' is deprecated.  Use `exp' instead.")
  (exp z))
(define ($expt z1 z2)
  (issue-deprecation-warning
   "`$expt' is deprecated.  Use `expt' instead.")
  (expt z1 z2))
(define ($log z)
  (issue-deprecation-warning
   "`$log' is deprecated.  Use `log' instead.")
  (log z))
(define ($sin z)
  (issue-deprecation-warning
   "`$sin' is deprecated.  Use `sin' instead.")
  (sin z))
(define ($cos z)
  (issue-deprecation-warning
   "`$cos' is deprecated.  Use `cos' instead.")
  (cos z))
(define ($tan z)
  (issue-deprecation-warning
   "`$tan' is deprecated.  Use `tan' instead.")
  (tan z))
(define ($asin z)
  (issue-deprecation-warning
   "`$asin' is deprecated.  Use `asin' instead.")
  (asin z))
(define ($acos z)
  (issue-deprecation-warning
   "`$acos' is deprecated.  Use `acos' instead.")
  (acos z))
(define ($atan z)
  (issue-deprecation-warning
   "`$atan' is deprecated.  Use `atan' instead.")
  (atan z))
(define ($sinh z)
  (issue-deprecation-warning
   "`$sinh' is deprecated.  Use `sinh' instead.")
  (sinh z))
(define ($cosh z)
  (issue-deprecation-warning
   "`$cosh' is deprecated.  Use `cosh' instead.")
  (cosh z))
(define ($tanh z)
  (issue-deprecation-warning
   "`$tanh' is deprecated.  Use `tanh' instead.")
  (tanh z))

(define (closure? x)
  (issue-deprecation-warning
   "`closure?' is deprecated. Use `procedure?' instead.")
  (procedure? x))

(define %nil #nil)

;;; @bind is used by the old elisp code as a dynamic scoping mechanism.
;;; Please let the Guile developers know if you are using this macro.
;;;
(define-syntax @bind
  (lambda (x)
    (define (bound-member id ids)
      (cond ((null? ids) #f)
            ((bound-identifier=? id (car ids)) #t)
            ((bound-member (car ids) (cdr ids)))))
    
    (issue-deprecation-warning
     "`@bind' is deprecated. Use `with-fluids' instead.")

    (syntax-case x ()
      ((_ () b0 b1 ...)
       #'(let () b0 b1 ...))
      ((_ ((id val) ...) b0 b1 ...)
       (and-map identifier? #'(id ...))
       (if (let lp ((ids #'(id ...)))
             (cond ((null? ids) #f)
                   ((bound-member (car ids) (cdr ids)) #t)
                   (else (lp (cdr ids)))))
           (syntax-violation '@bind "duplicate bound identifier" x)
           (with-syntax (((old-v ...) (generate-temporaries #'(id ...)))
                         ((v ...) (generate-temporaries #'(id ...))))
             #'(let ((old-v id) ...
                     (v val) ...)
                 (dynamic-wind
                   (lambda ()
                     (set! id v) ...)
                   (lambda () b0 b1 ...)
                   (lambda ()
                     (set! id old-v) ...)))))))))

;; There are deprecated definitions for module-ref-submodule and
;; module-define-submodule! in boot-9.scm.

;; Define (%app) and (%app modules), and have (app) alias (%app). This
;; side-effects the-root-module, both to the submodules table and (through
;; module-define-submodule! above) the obarray.
;;
(let ((%app (make-module 31)))
  (set-module-name! %app '(%app))
  (module-define-submodule! the-root-module '%app %app)
  (module-define-submodule! the-root-module 'app %app)
  (module-define-submodule! %app 'modules (resolve-module '() #f)))

;; Allow code that poked %module-public-interface to keep on working.
;;
(set! module-public-interface
      (let ((getter module-public-interface))
        (lambda (mod)
          (or (getter mod)
              (cond
               ((and=> (module-local-variable mod '%module-public-interface)
                       variable-ref)
                => (lambda (iface)
                     (issue-deprecation-warning 
"Setting a module's public interface via munging %module-public-interface is
deprecated. Use set-module-public-interface! instead.")
                     (set-module-public-interface! mod iface)
                     iface))
               (else #f))))))

(set! set-module-public-interface!
      (let ((setter set-module-public-interface!))
        (lambda (mod iface)
          (setter mod iface)
          (module-define! mod '%module-public-interface iface))))

(define (bad-throw key . args)
  (issue-deprecation-warning 
   "`bad-throw' in the default environment is deprecated.
Find it in the `(ice-9 scm-style-repl)' module instead.")
  (apply (@ (ice-9 scm-style-repl) bad-throw) key args))

(define (error-catching-loop thunk)
  (issue-deprecation-warning 
   "`error-catching-loop' in the default environment is deprecated.
Find it in the `(ice-9 scm-style-repl)' module instead.")
  ((@ (ice-9 scm-style-repl) error-catching-loop) thunk))

(define (error-catching-repl r e p)
  (issue-deprecation-warning 
   "`error-catching-repl' in the default environment is deprecated.
Find it in the `(ice-9 scm-style-repl)' module instead.")
  ((@ (ice-9 scm-style-repl) error-catching-repl) r e p))

(define (scm-style-repl)
  (issue-deprecation-warning 
   "`scm-style-repl' in the default environment is deprecated.
Find it in the `(ice-9 scm-style-repl)' module instead, or
better yet, use the repl from `(system repl repl)'.")
  ((@ (ice-9 scm-style-repl) scm-style-repl)))


;;; Apply-to-args had the following comment attached to it in boot-9, but it's
;;; wrong-headed: in the mentioned case, a point should either be a record or
;;; multiple values.
;;;
;;; apply-to-args is functionally redundant with apply and, worse,
;;; is less general than apply since it only takes two arguments.
;;;
;;; On the other hand, apply-to-args is a syntacticly convenient way to
;;; perform binding in many circumstances when the "let" family of
;;; of forms don't cut it.  E.g.:
;;;
;;;     (apply-to-args (return-3d-mouse-coords)
;;;       (lambda (x y z)
;;;             ...))
;;;

(define (apply-to-args args fn)
  (issue-deprecation-warning 
   "`apply-to-args' is deprecated. Include a local copy in your program.")
  (apply fn args))

(define (has-suffix? str suffix)
  (issue-deprecation-warning 
   "`has-suffix?' is deprecated. Use `string-suffix?' instead (args reversed).")
  (string-suffix? suffix str))

(define scheme-file-suffix
  (lambda ()
    (issue-deprecation-warning
     "`scheme-file-suffix' is deprecated. Use `%load-extensions' instead.")
    ".scm"))



;;; {Command Line Options}
;;;

(define (get-option argv kw-opts kw-args return)
  (issue-deprecation-warning
   "`get-option' is deprecated. Use `(ice-9 getopt-long)' instead.")
  (cond
   ((null? argv)
    (return #f #f argv))

   ((or (not (eq? #\- (string-ref (car argv) 0)))
        (eq? (string-length (car argv)) 1))
    (return 'normal-arg (car argv) (cdr argv)))

   ((eq? #\- (string-ref (car argv) 1))
    (let* ((kw-arg-pos (or (string-index (car argv) #\=)
                           (string-length (car argv))))
           (kw (symbol->keyword (substring (car argv) 2 kw-arg-pos)))
           (kw-opt? (member kw kw-opts))
           (kw-arg? (member kw kw-args))
           (arg (or (and (not (eq? kw-arg-pos (string-length (car argv))))
                         (substring (car argv)
                                    (+ kw-arg-pos 1)
                                    (string-length (car argv))))
                    (and kw-arg?
                         (begin (set! argv (cdr argv)) (car argv))))))
      (if (or kw-opt? kw-arg?)
          (return kw arg (cdr argv))
          (return 'usage-error kw (cdr argv)))))

   (else
    (let* ((char (substring (car argv) 1 2))
           (kw (symbol->keyword char)))
      (cond

       ((member kw kw-opts)
        (let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
               (new-argv (if (= 0 (string-length rest-car))
                             (cdr argv)
                             (cons (string-append "-" rest-car) (cdr argv)))))
          (return kw #f new-argv)))

       ((member kw kw-args)
        (let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
               (arg (if (= 0 (string-length rest-car))
                        (cadr argv)
                        rest-car))
               (new-argv (if (= 0 (string-length rest-car))
                             (cddr argv)
                             (cdr argv))))
          (return kw arg new-argv)))

       (else (return 'usage-error kw argv)))))))

(define (for-next-option proc argv kw-opts kw-args)
  (issue-deprecation-warning
   "`for-next-option' is deprecated. Use `(ice-9 getopt-long)' instead.")
  (let loop ((argv argv))
    (get-option argv kw-opts kw-args
                (lambda (opt opt-arg argv)
                  (and opt (proc opt opt-arg argv loop))))))

(define (display-usage-report kw-desc)
  (issue-deprecation-warning
   "`display-usage-report' is deprecated. Use `(ice-9 getopt-long)' instead.")
  (for-each
   (lambda (kw)
     (or (eq? (car kw) #t)
         (eq? (car kw) 'else)
         (let* ((opt-desc kw)
                (help (cadr opt-desc))
                (opts (car opt-desc))
                (opts-proper (if (string? (car opts)) (cdr opts) opts))
                (arg-name (if (string? (car opts))
                              (string-append "<" (car opts) ">")
                              ""))
                (left-part (string-append
                            (with-output-to-string
                              (lambda ()
                                (map (lambda (x) (display (keyword->symbol x)) (display " "))
                                     opts-proper)))
                            arg-name))
                (middle-part (if (and (< (string-length left-part) 30)
                                      (< (string-length help) 40))
                                 (make-string (- 30 (string-length left-part)) #\ )
                                 "\n\t")))
           (display left-part)
           (display middle-part)
           (display help)
           (newline))))
   kw-desc))

(define (transform-usage-lambda cases)
  (issue-deprecation-warning
   "`display-usage-report' is deprecated. Use `(ice-9 getopt-long)' instead.")
  (let* ((raw-usage (delq! 'else (map car cases)))
         (usage-sans-specials (map (lambda (x)
                                    (or (and (not (list? x)) x)
                                        (and (symbol? (car x)) #t)
                                        (and (boolean? (car x)) #t)
                                        x))
                                  raw-usage))
         (usage-desc (delq! #t usage-sans-specials))
         (kw-desc (map car usage-desc))
         (kw-opts (apply append (map (lambda (x) (and (not (string? (car x))) x)) kw-desc)))
         (kw-args (apply append (map (lambda (x) (and (string? (car x)) (cdr x))) kw-desc)))
         (transmogrified-cases (map (lambda (case)
                                      (cons (let ((opts (car case)))
                                              (if (or (boolean? opts) (eq? 'else opts))
                                                  opts
                                                  (cond
                                                   ((symbol? (car opts))  opts)
                                                   ((boolean? (car opts)) opts)
                                                   ((string? (caar opts)) (cdar opts))
                                                   (else (car opts)))))
                                            (cdr case)))
                                    cases)))
    `(let ((%display-usage (lambda () (display-usage-report ',usage-desc))))
       (lambda (%argv)
         (let %next-arg ((%argv %argv))
           (get-option %argv
                       ',kw-opts
                       ',kw-args
                       (lambda (%opt %arg %new-argv)
                         (case %opt
                           ,@ transmogrified-cases))))))))



;;; {collect}
;;;
;;; Similar to `begin' but returns a list of the results of all constituent
;;; forms instead of the result of the last form.
;;;

(define-syntax collect
  (lambda (x)
    (issue-deprecation-warning
     "`collect' is deprecated. Define it yourself.")
    (syntax-case x ()
      ((_) #''())
      ((_ x x* ...)
       #'(let ((val x))
           (cons val (collect x* ...)))))))




(define (assert-repl-silence v)
  (issue-deprecation-warning
   "`assert-repl-silence' has moved to `(ice-9 scm-style-repl)'.")
  ((@ (ice-9 scm-style-repl) assert-repl-silence) v))

(define (assert-repl-print-unspecified v)
  (issue-deprecation-warning
   "`assert-repl-print-unspecified' has moved to `(ice-9 scm-style-repl)'.")
  ((@ (ice-9 scm-style-repl) assert-repl-print-unspecified) v))

(define (assert-repl-verbosity v)
  (issue-deprecation-warning
   "`assert-repl-verbosity' has moved to `(ice-9 scm-style-repl)'.")
  ((@ (ice-9 scm-style-repl) assert-repl-verbosity) v))

(define (set-repl-prompt! v)
  (issue-deprecation-warning
   "`set-repl-prompt!' is deprecated. Use `repl-default-prompt-set!' from
the `(system repl common)' module.")
  ;; Avoid @, as when bootstrapping it will cause the (system repl common)
  ;; module to be loaded at expansion time, which eventually loads srfi-1, but
  ;; that fails due to an unbuilt supporting lib... grrrrrrrrr.
  ((module-ref (resolve-interface '(system repl common))
               'repl-default-prompt-set!)
   v))

(define (set-batch-mode?! arg)
  (cond
   (arg
    (issue-deprecation-warning
     "`set-batch-mode?!' is deprecated. Use `ensure-batch-mode!' instead.")
    (ensure-batch-mode!))
   (else
    (issue-deprecation-warning
     "`set-batch-mode?!' with an argument of `#f' is deprecated. Use the
`*repl-stack*' fluid instead.")
    #t)))

(define (repl read evaler print)
  (issue-deprecation-warning
   "`repl' is deprecated. Define it yourself.")
  (let loop ((source (read (current-input-port))))
    (print (evaler source))
    (loop (read (current-input-port)))))

(define (pre-unwind-handler-dispatch key . args)
  (issue-deprecation-warning
   "`pre-unwind-handler-dispatch' is deprecated. Use
`default-pre-unwind-handler' from `(ice-9 scm-style-repl)' directly.")
  (apply (@ (ice-9 scm-style-repl) default-pre-unwind-handler) key args))

(define (default-pre-unwind-handler key . args)
  (issue-deprecation-warning
   "`default-pre-unwind-handler' is deprecated. Use it from 
`(ice-9 scm-style-repl)' if you need it.")
  (apply (@ (ice-9 scm-style-repl) default-pre-unwind-handler) key args))

(define (handle-system-error key . args)
  (issue-deprecation-warning
   "`handle-system-error' is deprecated. Use it from 
`(ice-9 scm-style-repl)' if you need it.")
  (apply (@ (ice-9 scm-style-repl) handle-system-error) key args))

(define-syntax stack-saved?
  (make-variable-transformer
   (lambda (x)
     (issue-deprecation-warning
      "`stack-saved?' is deprecated. Use it from
`(ice-9 save-stack)' if you need it.")
     (syntax-case x (set!)
       ((set! id val)
        (identifier? #'id)
        #'(set! (@ (ice-9 save-stack) stack-saved?) val))
       (id
        (identifier? #'id)
        #'(@ (ice-9 save-stack) stack-saved?))))))

(define-syntax the-last-stack
  (lambda (x)
    (issue-deprecation-warning
     "`the-last-stack' is deprecated. Use it from `(ice-9 save-stack)'
if you need it.")
    (syntax-case x ()
      (id
       (identifier? #'id)
       #'(@ (ice-9 save-stack) the-last-stack)))))

(define (save-stack . args)
  (issue-deprecation-warning
   "`save-stack' is deprecated. Use it from `(ice-9 save-stack)' if you need
it.")
  (apply (@ (ice-9 save-stack) save-stack) args))

(define (named-module-use! user usee)
  (issue-deprecation-warning
   "`named-module-use!' is deprecated. Define it yourself if you need it.")
  (module-use! (resolve-module user) (resolve-interface usee)))

(define (top-repl)
  (issue-deprecation-warning
   "`top-repl' has moved to the `(ice-9 top-repl)' module.")
  ((module-ref (resolve-module '(ice-9 top-repl)) 'top-repl)))

(set! debug-enable
      (let ((debug-enable debug-enable))
        (lambda opts
          (if (memq 'debug opts)
              (begin
                (issue-deprecation-warning
                 "`(debug-enable 'debug)' is obsolete and has no effect."
                 "Remove it from your code.")
                (apply debug-enable (delq 'debug opts)))
              (apply debug-enable opts)))))

(define (turn-on-debugging)
  (issue-deprecation-warning
   "`(turn-on-debugging)' is obsolete and usually has no effect."
   "Debugging capabilities are present by default.")
  (debug-enable 'backtrace)
  (read-enable 'positions))

(define (read-hash-procedures-warning)
  (issue-deprecation-warning
   "`read-hash-procedures' is deprecated."
   "Use the fluid `%read-hash-procedures' instead."))

(define-syntax read-hash-procedures
  (identifier-syntax
    (_
     (begin (read-hash-procedures-warning)
            (fluid-ref %read-hash-procedures)))
    ((set! _ expr)
     (begin (read-hash-procedures-warning)
            (fluid-set! %read-hash-procedures expr)))))

(define (process-define-module args)
  (define (missing kw)
    (error "missing argument to define-module keyword" kw))
  (define (unrecognized arg)
    (error "unrecognized define-module argument" arg))

  (issue-deprecation-warning
   "`process-define-module' is deprecated.  Use `define-module*' instead.")

  (let ((name (car args))
        (filename #f)
        (pure? #f)
        (version #f)
        (system? #f)
        (duplicates '())
        (transformer #f))
    (let loop ((kws (cdr args))
               (imports '())
               (exports '())
               (re-exports '())
               (replacements '())
               (autoloads '()))
      (if (null? kws)
          (define-module* name
            #:filename filename #:pure pure? #:version version
            #:duplicates duplicates #:transformer transformer
            #:imports (reverse! imports)
            #:exports exports
            #:re-exports re-exports
            #:replacements replacements
            #:autoloads autoloads)
          (case (car kws)
            ((#:use-module #:use-syntax)
             (or (pair? (cdr kws))
                 (missing (car kws)))
             (cond
              ((equal? (cadr kws) '(ice-9 syncase))
               (issue-deprecation-warning
                "(ice-9 syncase) is deprecated. Support for syntax-case is now in Guile core.")
               (loop (cddr kws)
                     imports exports re-exports replacements autoloads))
              (else
               (let ((iface-spec (cadr kws)))
                 (if (eq? (car kws) #:use-syntax)
                     (set! transformer iface-spec))
                 (loop (cddr kws)
                       (cons iface-spec imports) exports re-exports
                       replacements autoloads)))))
            ((#:autoload)
             (or (and (pair? (cdr kws)) (pair? (cddr kws)))
                 (missing (car kws)))
             (let ((name (cadr kws))
                   (bindings (caddr kws)))
               (loop (cdddr kws)
                     imports exports re-exports
                     replacements (cons* name bindings autoloads))))
            ((#:no-backtrace)
             ;; FIXME: deprecate?
             (set! system? #t)
             (loop (cdr kws)
                   imports exports re-exports replacements autoloads))
            ((#:pure)
             (set! pure? #t)
             (loop (cdr kws)
                   imports exports re-exports replacements autoloads))
            ((#:version)
             (or (pair? (cdr kws))
                 (missing (car kws)))
             (set! version (cadr kws))
             (loop (cddr kws)
                   imports exports re-exports replacements autoloads))
            ((#:duplicates)
             (if (not (pair? (cdr kws)))
                 (missing (car kws)))
             (set! duplicates (cadr kws))
             (loop (cddr kws)
                   imports exports re-exports replacements autoloads))
            ((#:export #:export-syntax)
             (or (pair? (cdr kws))
                 (missing (car kws)))
             (loop (cddr kws)
                   imports (append exports (cadr kws)) re-exports
                   replacements autoloads))
            ((#:re-export #:re-export-syntax)
             (or (pair? (cdr kws))
                 (missing (car kws)))
             (loop (cddr kws)
                   imports exports (append re-exports (cadr kws))
                   replacements autoloads))
            ((#:replace #:replace-syntax)
             (or (pair? (cdr kws))
                 (missing (car kws)))
             (loop (cddr kws)
                   imports exports re-exports
                   (append replacements (cadr kws)) autoloads))
            ((#:filename)
             (or (pair? (cdr kws))
                 (missing (car kws)))
             (set! filename (cadr kws))
             (loop (cddr kws)
                   imports exports re-exports replacements autoloads))
            (else
             (unrecognized kws)))))))

(define-syntax fluid-let-syntax
  (lambda (x)
    (issue-deprecation-warning
     "`fluid-let-syntax' is deprecated.  Use syntax parameters instead.")
    (syntax-case x ()
      ((_ ((k v) ...) body0 body ...)
       #'(syntax-parameterize ((k v) ...)
           body0 body ...)))))

(define (close-io-port port)
  (issue-deprecation-warning
   "`close-io-port' is deprecated.  Use `close-port' instead.")
  (close-port port))

(define (set-system-module! m s)
  (issue-deprecation-warning
   "`set-system-module!' is deprecated.  There is no need to use it.")
  (set-procedure-property! (module-eval-closure m) 'system-module s))

(set! module-eval-closure
      (lambda (m)
        (issue-deprecation-warning
         "`module-eval-closure' is deprecated.  Use module-variable or module-define! instead.")
        (standard-eval-closure m)))

;; Legacy definition.  We can't make it identifier-syntax yet though,
;; because compiled code might rely on it.
(define char-code-limit 256)
