;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 1995-2014, 2016-2017  Free Software Foundation, Inc.
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

;;; This file is the first thing loaded into Guile.  It adds many mundane
;;; definitions and a few that are interesting.
;;;
;;; The module system (hence the hierarchical namespace) are defined in this
;;; file.
;;;

;;; Code:



;; Before compiling, make sure any symbols are resolved in the (guile)
;; module, the primary location of those symbols, rather than in
;; (guile-user), the default module that we compile in.

(eval-when (compile)
  (set-current-module (resolve-module '(guile))))

;; Prevent this file being loaded more than once in a session.  Just
;; doesn't make sense!
(if (current-module)
    (error "re-loading ice-9/boot-9.scm not allowed"))



;;; {Language primitives}
;;;

;; These are are the procedural wrappers around the primitives of
;; Guile's language: apply, call-with-current-continuation, etc.
;;
;; Usually, a call to a primitive is compiled specially.  The compiler
;; knows about all these kinds of expressions.  But the primitives may
;; be referenced not only as operators, but as values as well.  These
;; stub procedures are the "values" of apply, dynamic-wind, and other
;; such primitives.
;;
(define apply
  (case-lambda
    ((fun args)
     ((@@ primitive apply) fun args))
    ((fun arg1 . args)
     (letrec ((append* (lambda (tail)
                         (let ((tail (car tail))
                               (tail* (cdr tail)))
                           (if (null? tail*)
                               tail
                               (cons tail (append* tail*)))))))
       (apply fun (cons arg1 (append* args)))))))
(define (call-with-current-continuation proc)
  ((@@ primitive call-with-current-continuation) proc))
(define (call-with-values producer consumer)
  ((@@ primitive call-with-values) producer consumer))
(define (dynamic-wind in thunk out)
  "All three arguments must be 0-argument procedures.
Guard @var{in} is called, then @var{thunk}, then
guard @var{out}.

If, any time during the execution of @var{thunk}, the
continuation of the @code{dynamic_wind} expression is escaped
non-locally, @var{out} is called.  If the continuation of
the dynamic-wind is re-entered, @var{in} is called.  Thus
@var{in} and @var{out} may be called any number of
times.
@lisp
 (define x 'normal-binding)
@result{} x
 (define a-cont
   (call-with-current-continuation
     (lambda (escape)
       (let ((old-x x))
         (dynamic-wind
           ;; in-guard:
           ;;
           (lambda () (set! x 'special-binding))

           ;; thunk
           ;;
           (lambda () (display x) (newline)
                   (call-with-current-continuation escape)
                   (display x) (newline)
                   x)

           ;; out-guard:
           ;;
           (lambda () (set! x old-x)))))))

;; Prints:
special-binding
;; Evaluates to:
@result{} a-cont
x
@result{} normal-binding
 (a-cont #f)
;; Prints:
special-binding
;; Evaluates to:
@result{} a-cont  ;; the value of the (define a-cont...)
x
@result{} normal-binding
a-cont
@result{} special-binding
@end lisp"
  ;; FIXME: Here we don't check that the out procedure is a thunk before
  ;; calling the in-guard, as dynamic-wind is called as part of loading
  ;; modules, but thunk? requires loading (system vm debug).  This is in
  ;; contrast to the open-coded version of dynamic-wind, which does
  ;; currently insert an eager thunk? check (but often optimizes it
  ;; out).  Not sure what the right thing to do is here -- make thunk?
  ;; callable before modules are loaded, live with this inconsistency,
  ;; or remove the thunk? check from the compiler?  Questions,
  ;; questions.
  #;
  (unless (thunk? out)
    (scm-error 'wrong-type-arg "dynamic-wind" "Not a thunk: ~S"
               (list out) #f))
  (in)
  ((@@ primitive wind) in out)
  (call-with-values thunk
    (lambda vals
      ((@@ primitive unwind))
      (out)
      (apply values vals))))

(define (with-fluid* fluid val thunk)
  "Set @var{fluid} to @var{value} temporarily, and call @var{thunk}.
@var{thunk} must be a procedure of no arguments."
  ((@@ primitive push-fluid) fluid val)
  (call-with-values thunk
    (lambda vals
      ((@@ primitive pop-fluid))
      (apply values vals))))

(define (with-dynamic-state state thunk)
  "Call @var{proc} while @var{state} is the current dynamic state object.
@var{thunk} must be a procedure of no arguments."
  ((@@ primitive push-dynamic-state) state)
  (call-with-values thunk
    (lambda vals
      ((@@ primitive pop-dynamic-state))
      (apply values vals))))



;;; {Simple Debugging Tools}
;;;

(define (peek . stuff)
  "Write arguments to the current output port, and return the last argument.

This is handy for tracing function calls, e.g.:

(+ 10 (troublesome-fn))
=> (+ 10 (pk 'troublesome-fn-returned (troublesome-fn)))"
  (newline)
  (display ";;; ")
  (write stuff)
  (newline)
  (car (last-pair stuff)))

(define pk peek)

(define (warn . stuff)
  (newline (current-warning-port))
  (display ";;; WARNING " (current-warning-port))
  (display stuff (current-warning-port))
  (newline (current-warning-port))
  (car (last-pair stuff)))



;;; {Features}
;;;

(define (provide sym)
  (if (not (memq sym *features*))
      (set! *features* (cons sym *features*))))

;; In SLIB, provided? also checks to see if the module is available.  We
;; should do that too, but don't.

(define (provided? feature)
  "Return #t iff FEATURE is available to this Guile interpreter."
  (and (memq feature *features*) #t))



;;; {map and for-each}
;;;

(define map
  (case-lambda
    ((f l)
     (if (not (list? l))
         (scm-error 'wrong-type-arg "map" "Not a list: ~S"
                    (list l) #f))
     (let map1 ((l l))
       (if (pair? l)
           (cons (f (car l)) (map1 (cdr l)))
           '())))

    ((f l1 l2)
     (if (not (= (length l1) (length l2)))
         (scm-error 'wrong-type-arg "map" "List of wrong length: ~S"
                    (list l2) #f))

     (let map2 ((l1 l1) (l2 l2))
       (if (pair? l1)
           (cons (f (car l1) (car l2))
                 (map2 (cdr l1) (cdr l2)))
           '())))

    ((f l1 . rest)
     (let ((len (length l1)))
       (let mapn ((rest rest))
         (or (null? rest)
             (if (= (length (car rest)) len)
                 (mapn (cdr rest))
                 (scm-error 'wrong-type-arg "map" "List of wrong length: ~S"
                            (list (car rest)) #f)))))
     (let mapn ((l1 l1) (rest rest))
       (if (pair? l1)
           (cons (apply f (car l1) (map car rest))
                 (mapn (cdr l1) (map cdr rest)))
           '())))))

(define map-in-order map)

(define for-each
  (case-lambda
    ((f l)
     (if (not (list? l))
         (scm-error 'wrong-type-arg "for-each" "Not a list: ~S" (list l) #f))
     (let for-each1 ((l l))
       (if (not (null? l))
           (begin
             (f (car l))
             (for-each1 (cdr l))))))

    ((f l1 l2)
     (if (not (= (length l1) (length l2)))
         (scm-error 'wrong-type-arg "for-each" "List of wrong length: ~S"
                    (list l2) #f))
     (let for-each2 ((l1 l1) (l2 l2))
       (if (not (null? l1))
           (begin
             (f (car l1) (car l2))
             (for-each2 (cdr l1) (cdr l2))))))

    ((f l1 . rest)
     (let ((len (length l1)))
       (let for-eachn ((rest rest))
         (or (null? rest)
             (if (= (length (car rest)) len)
                 (for-eachn (cdr rest))
                 (scm-error 'wrong-type-arg "for-each" "List of wrong length: ~S"
                            (list (car rest)) #f)))))

     (let for-eachn ((l1 l1) (rest rest))
       (if (pair? l1)
           (begin
             (apply f (car l1) (map car rest))
             (for-eachn (cdr l1) (map cdr rest))))))))


;; Temporary definitions used by `include'; replaced later.

(define (absolute-file-name? file-name) #t)
(define (open-input-file str) (open-file str "r"))

;; Temporary definition; replaced by a parameter later.
(define (allow-legacy-syntax-objects?) #f)

;;; {and-map and or-map}
;;;
;;; (and-map fn lst) is like (and (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;; (or-map fn lst) is like (or (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;;

(define (and-map f lst)
  "Apply F to successive elements of LST until exhaustion or F returns #f.
If returning early, return #f.  Otherwise, return the last value returned
by F.  If F has never been called because LST is empty, return #t."
  (let loop ((result #t)
             (l lst))
    (and result
         (or (and (null? l)
                  result)
             (loop (f (car l)) (cdr l))))))

(define (or-map f lst)
  "Apply F to successive elements of LST until exhaustion or while F returns #f.
If returning early, return the return value of F."
  (let loop ((result #f)
             (l lst))
    (or result
        (and (not (null? l))
             (loop (f (car l)) (cdr l))))))



;; let format alias simple-format until the more complete version is loaded

(define format simple-format)

;; this is scheme wrapping the C code so the final pred call is a tail call,
;; per SRFI-13 spec
(define string-any
  (lambda* (char_pred s #:optional (start 0) (end (string-length s)))
    (if (and (procedure? char_pred)
             (> end start)
             (<= end (string-length s))) ;; let c-code handle range error
        (or (string-any-c-code char_pred s start (1- end))
            (char_pred (string-ref s (1- end))))
        (string-any-c-code char_pred s start end))))

;; this is scheme wrapping the C code so the final pred call is a tail call,
;; per SRFI-13 spec
(define string-every
  (lambda* (char_pred s #:optional (start 0) (end (string-length s)))
    (if (and (procedure? char_pred)
             (> end start)
             (<= end (string-length s))) ;; let c-code handle range error
        (and (string-every-c-code char_pred s start (1- end))
             (char_pred (string-ref s (1- end))))
        (string-every-c-code char_pred s start end))))

(define (substring-fill! str start end fill)
  "A variant of string-fill! that we keep for compatibility."
  (string-fill! str fill start end))



;; Define a minimal stub of the module API for psyntax, before modules
;; have booted.
(define (module-name x)
  '(guile))
(define (module-add! module sym var)
  (hashq-set! (%get-pre-modules-obarray) sym var))
(define (module-define! module sym val)
  (let ((v (hashq-ref (%get-pre-modules-obarray) sym)))
    (if v
        (variable-set! v val)
        (module-add! (current-module) sym (make-variable val)))))
(define (module-ref module sym)
  (let ((v (module-variable module sym)))
    (if v (variable-ref v) (error "badness!" (pk module) (pk sym)))))
(define module-generate-unique-id!
  (let ((next-id 0))
    (lambda (m)
      (let ((i next-id))
        (set! next-id (+ i 1))
        i))))
(define module-gensym gensym)
(define (resolve-module . args)
  #f)

;; API provided by psyntax
(define syntax-violation #f)
(define datum->syntax #f)
(define syntax->datum #f)
(define syntax-source #f)
(define identifier? #f)
(define generate-temporaries #f)
(define bound-identifier=? #f)
(define free-identifier=? #f)

;; $sc-dispatch is an implementation detail of psyntax. It is used by
;; expanded macros, to dispatch an input against a set of patterns.
(define $sc-dispatch #f)

;; Load it up!
(primitive-load-path "ice-9/psyntax-pp")
;; The binding for `macroexpand' has now been overridden, making psyntax the
;; expander now.

(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ;; Avoid ellipsis, which would lead to quadratic expansion time.
    ((_ x . y) (if x (and . y) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ;; Avoid ellipsis, which would lead to quadratic expansion time.
    ((_ x . y) (let ((t x)) (if t t (or . y))))))

(include-from-path "ice-9/quasisyntax")

(define-syntax-rule (when test stmt stmt* ...)
  (if test (begin stmt stmt* ...)))

(define-syntax-rule (unless test stmt stmt* ...)
  (if (not test) (begin stmt stmt* ...)))

(define-syntax cond
  (lambda (whole-expr)
    (define (fold f seed xs)
      (let loop ((xs xs) (seed seed))
        (if (null? xs) seed
            (loop (cdr xs) (f (car xs) seed)))))
    (define (reverse-map f xs)
      (fold (lambda (x seed) (cons (f x) seed))
            '() xs))
    (syntax-case whole-expr ()
      ((_ clause clauses ...)
       #`(begin
           #,@(fold (lambda (clause-builder tail)
                      (clause-builder tail))
                    #'()
                    (reverse-map
                     (lambda (clause)
                       (define* (bad-clause #:optional (msg "invalid clause"))
                         (syntax-violation 'cond msg whole-expr clause))
                       (syntax-case clause (=> else)
                         ((else e e* ...)
                          (lambda (tail)
                            (if (null? tail)
                                #'((begin e e* ...))
                                (bad-clause "else must be the last clause"))))
                         ((else . _) (bad-clause))
                         ((test => receiver)
                          (lambda (tail)
                            #`((let ((t test))
                                 (if t
                                     (receiver t)
                                     #,@tail)))))
                         ((test => receiver ...)
                          (bad-clause "wrong number of receiver expressions"))
                         ((generator guard => receiver)
                          (lambda (tail)
                            #`((call-with-values (lambda () generator)
                                 (lambda vals
                                   (if (apply guard vals)
                                       (apply receiver vals)
                                       #,@tail))))))
                         ((generator guard => receiver ...)
                          (bad-clause "wrong number of receiver expressions"))
                         ((test)
                          (lambda (tail)
                            #`((let ((t test))
                                 (if t t #,@tail)))))
                         ((test e e* ...)
                          (lambda (tail)
                            #`((if test
                                   (begin e e* ...)
                                   #,@tail))))
                         (_ (bad-clause))))
                     #'(clause clauses ...))))))))

(define-syntax case
  (lambda (whole-expr)
    (define (fold f seed xs)
      (let loop ((xs xs) (seed seed))
        (if (null? xs) seed
            (loop (cdr xs) (f (car xs) seed)))))
    (define (fold2 f a b xs)
      (let loop ((xs xs) (a a) (b b))
        (if (null? xs) (values a b)
            (call-with-values
                (lambda () (f (car xs) a b))
              (lambda (a b)
                (loop (cdr xs) a b))))))
    (define (reverse-map-with-seed f seed xs)
      (fold2 (lambda (x ys seed)
               (call-with-values
                   (lambda () (f x seed))
                 (lambda (y seed)
                   (values (cons y ys) seed))))
             '() seed xs))
    (syntax-case whole-expr ()
      ((_ expr clause clauses ...)
       (with-syntax ((key #'key))
         #`(let ((key expr))
             #,@(fold
                 (lambda (clause-builder tail)
                   (clause-builder tail))
                 #'()
                 (reverse-map-with-seed
                  (lambda (clause seen)
                    (define* (bad-clause #:optional (msg "invalid clause"))
                      (syntax-violation 'case msg whole-expr clause))
                    (syntax-case clause ()
                      ((test . rest)
                       (with-syntax
                           ((clause-expr
                             (syntax-case #'rest (=>)
                               ((=> receiver) #'(receiver key))
                               ((=> receiver ...)
                                (bad-clause
                                 "wrong number of receiver expressions"))
                               ((e e* ...) #'(begin e e* ...))
                               (_ (bad-clause)))))
                         (syntax-case #'test (else)
                           ((datums ...)
                            (let ((seen
                                   (fold
                                    (lambda (datum seen)
                                      (define (warn-datum type)
                                        ((@ (system base message)
                                            warning)
                                         type
                                         (append (source-properties datum)
                                                 (source-properties
                                                  (syntax->datum #'test)))
                                         datum
                                         (syntax->datum clause)
                                         (syntax->datum whole-expr)))
                                      (when (memv datum seen)
                                        (warn-datum 'duplicate-case-datum))
                                      (when (or (pair? datum) (array? datum))
                                        (warn-datum 'bad-case-datum))
                                      (cons datum seen))
                                    seen
                                    (map syntax->datum #'(datums ...)))))
                              (values (lambda (tail)
                                        #`((if (memv key '(datums ...))
                                               clause-expr
                                               #,@tail)))
                                      seen)))
                           (else (values (lambda (tail)
                                           (if (null? tail)
                                               #'(clause-expr)
                                               (bad-clause
                                                "else must be the last clause")))
                                         seen))
                           (_ (bad-clause)))))
                      (_ (bad-clause))))
                  '() #'(clause clauses ...)))))))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step ...)
                       ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

(define-syntax define-values
  (lambda (orig-form)
    (syntax-case orig-form ()
      ((_ () expr)
       ;; XXX Work around the lack of hygienic top-level identifiers
       (with-syntax (((dummy) (generate-temporaries '(dummy))))
         #`(define dummy
             (call-with-values (lambda () expr)
               (lambda () #f)))))
      ((_ (var) expr)
       (identifier? #'var)
       #`(define var
           (call-with-values (lambda () expr)
             (lambda (v) v))))
      ((_ (var0 ... varn) expr)
       (and-map identifier? #'(var0 ... varn))
       ;; XXX Work around the lack of hygienic toplevel identifiers
       (with-syntax (((dummy) (generate-temporaries '(dummy))))
         #`(begin
             ;; Avoid mutating the user-visible variables
             (define dummy
               (call-with-values (lambda () expr)
                 (lambda (var0 ... varn)
                   (list var0 ... varn))))
             (define var0
               (let ((v (car dummy)))
                 (set! dummy (cdr dummy))
                 v))
             ...
             (define varn
               (let ((v (car dummy)))
                 (set! dummy #f)  ; blackhole dummy
                 v)))))
      ((_ var expr)
       (identifier? #'var)
       #'(define var
           (call-with-values (lambda () expr)
             list)))
      ((_ (var0 ... . varn) expr)
       (and-map identifier? #'(var0 ... varn))
       ;; XXX Work around the lack of hygienic toplevel identifiers
       (with-syntax (((dummy) (generate-temporaries '(dummy))))
         #`(begin
             ;; Avoid mutating the user-visible variables
             (define dummy
               (call-with-values (lambda () expr)
                 (lambda (var0 ... . varn)
                   (list var0 ... varn))))
             (define var0
               (let ((v (car dummy)))
                 (set! dummy (cdr dummy))
                 v))
             ...
             (define varn
               (let ((v (car dummy)))
                 (set! dummy #f)  ; blackhole dummy
                 v))))))))

(define-syntax-rule (delay exp)
  (make-promise (lambda () exp)))

(define-syntax with-fluids
  (lambda (stx)
    (define (emit-with-fluids bindings body)
      (syntax-case bindings ()
        (()
         body)
        (((f v) . bindings)
         #`(with-fluid* f v
             (lambda ()
               #,(emit-with-fluids #'bindings body))))))
    (syntax-case stx ()
      ((_ ((fluid val) ...) exp exp* ...)
       (with-syntax (((fluid-tmp ...) (generate-temporaries #'(fluid ...)))
                     ((val-tmp ...) (generate-temporaries #'(val ...))))
         #`(let ((fluid-tmp fluid) ...)
             (let ((val-tmp val) ...)
               #,(emit-with-fluids #'((fluid-tmp val-tmp) ...)
                                   #'(begin exp exp* ...)))))))))

(define-syntax current-source-location
  (lambda (x)
    (syntax-case x ()
      ((_)
       (with-syntax ((s (datum->syntax x (syntax-source x))))
         #''s)))))

;; We provide this accessor out of convenience.  current-line and
;; current-column aren't so interesting, because they distort what they
;; are measuring; better to use syntax-source from a macro.
;;
(define-syntax current-filename
  (lambda (x)
    "A macro that expands to the current filename: the filename that
the (current-filename) form appears in.  Expands to #f if this
information is unavailable."
    (false-if-exception
     (canonicalize-path (assq-ref (syntax-source x) 'filename)))))

(define-syntax-rule (define-once sym val)
  (define sym
    (if (module-locally-bound? (current-module) 'sym) sym val)))




;;; {Error handling}
;;;

;; Define delimited continuation operators, and implement catch and throw in
;; terms of them.

(define make-prompt-tag
  (lambda* (#:optional (stem "prompt"))
    ;; The only property that prompt tags need have is uniqueness in the
    ;; sense of eq?.  A one-element list will serve nicely.
    (list stem)))

(define default-prompt-tag
  ;; Redefined later to be a parameter.
  (let ((%default-prompt-tag (make-prompt-tag)))
    (lambda ()
      %default-prompt-tag)))

(define (call-with-prompt tag thunk handler)
  ((@@ primitive call-with-prompt) tag thunk handler))
(define (abort-to-prompt tag . args)
  (abort-to-prompt* tag args))

;; Define catch and with-throw-handler, using some common helper routines and a
;; shared fluid. Hide the helpers in a lexical contour.

(define with-throw-handler #f)
(let ((%eh (module-ref (current-module) '%exception-handler)))
  (define (make-exception-handler catch-key prompt-tag pre-unwind)
    (vector catch-key prompt-tag pre-unwind))
  (define (exception-handler-catch-key handler) (vector-ref handler 0))
  (define (exception-handler-prompt-tag handler) (vector-ref handler 1))
  (define (exception-handler-pre-unwind handler) (vector-ref handler 2))

  (define %running-pre-unwind (make-fluid #f))
  (define (pre-unwind-handler-running? handler)
    (let lp ((depth 0))
      (let ((running (fluid-ref* %running-pre-unwind depth)))
        (and running
             (or (eq? running handler) (lp (1+ depth)))))))

  (define (dispatch-exception depth key args)
    (cond
     ((fluid-ref* %eh depth)
      => (lambda (handler)
           (let ((catch-key (exception-handler-catch-key handler)))
             (if (or (eqv? catch-key #t) (eq? catch-key key))
                 (let ((prompt-tag (exception-handler-prompt-tag handler))
                       (pre-unwind (exception-handler-pre-unwind handler)))
                   (cond
                    ((and pre-unwind
                          (not (pre-unwind-handler-running? handler)))
                     ;; Prevent errors from within the pre-unwind
                     ;; handler's invocation from being handled by this
                     ;; handler.
                     (with-fluid* %running-pre-unwind handler
                       (lambda ()
                         ;; FIXME: Currently the "running" flag only
                         ;; applies to the pre-unwind handler; the
                         ;; post-unwind handler is still called if the
                         ;; error is explicitly rethrown.  Instead it
                         ;; would be better to cause a recursive throw to
                         ;; skip all parts of this handler.  Unfortunately
                         ;; that is incompatible with existing semantics.
                         ;; We'll see if we can change that later on.
                         (apply pre-unwind key args)
                         (dispatch-exception depth key args))))
                    (prompt-tag
                     (apply abort-to-prompt prompt-tag key args))
                    (else
                     (dispatch-exception (1+ depth) key args))))
                 (dispatch-exception (1+ depth) key args)))))
     ((eq? key 'quit)
      (primitive-exit (cond
                       ((not (pair? args)) 0)
                       ((integer? (car args)) (car args))
                       ((not (car args)) 1)
                       (else 0))))
     (else
      (format (current-error-port) "guile: uncaught throw to ~a: ~a\n"
              key args)
      (primitive-exit 1))))

  (define (throw key . args)
    "Invoke the catch form matching @var{key}, passing @var{args} to the
@var{handler}.

@var{key} is a symbol. It will match catches of the same symbol or of @code{#t}.

If there is no handler at all, Guile prints an error and then exits."
    (unless (symbol? key)
      (throw 'wrong-type-arg "throw" "Wrong type argument in position ~a: ~a"
             (list 1 key) (list key)))
    (dispatch-exception 0 key args))

  (define* (catch k thunk handler #:optional pre-unwind-handler)
    "Invoke @var{thunk} in the dynamic context of @var{handler} for
exceptions matching @var{key}.  If thunk throws to the symbol
@var{key}, then @var{handler} is invoked this way:
@lisp
 (handler key args ...)
@end lisp

@var{key} is a symbol or @code{#t}.

@var{thunk} takes no arguments.  If @var{thunk} returns
normally, that is the return value of @code{catch}.

Handler is invoked outside the scope of its own @code{catch}.
If @var{handler} again throws to the same key, a new handler
from further up the call chain is invoked.

If the key is @code{#t}, then a throw to @emph{any} symbol will
match this call to @code{catch}.

If a @var{pre-unwind-handler} is given and @var{thunk} throws
an exception that matches @var{key}, Guile calls the
@var{pre-unwind-handler} before unwinding the dynamic state and
invoking the main @var{handler}.  @var{pre-unwind-handler} should
be a procedure with the same signature as @var{handler}, that
is @code{(lambda (key . args))}.  It is typically used to save
the stack at the point where the exception occurred, but can also
query other parts of the dynamic state at that point, such as
fluid values.

A @var{pre-unwind-handler} can exit either normally or non-locally.
If it exits normally, Guile unwinds the stack and dynamic context
and then calls the normal (third argument) handler.  If it exits
non-locally, that exit determines the continuation."
    (define (wrong-type-arg n val)
      (scm-error 'wrong-type-arg "catch"
                 "Wrong type argument in position ~a: ~a"
                 (list n val) (list val)))
    (unless (or (symbol? k) (eqv? k #t))
      (wrong-type-arg 1 k))
    (unless (procedure? handler)
      (wrong-type-arg 3 handler))
    (unless (or (not pre-unwind-handler) (procedure? pre-unwind-handler))
      (wrong-type-arg 4 pre-unwind-handler))
    (let ((tag (make-prompt-tag "catch")))
      (call-with-prompt
       tag
       (lambda ()
         (with-fluid* %eh (make-exception-handler k tag pre-unwind-handler)
           thunk))
       (lambda (cont k . args)
         (apply handler k args)))))

  (define (with-throw-handler k thunk pre-unwind-handler)
    "Add @var{handler} to the dynamic context as a throw handler
for key @var{k}, then invoke @var{thunk}."
    (if (not (or (symbol? k) (eqv? k #t)))
        (scm-error 'wrong-type-arg "with-throw-handler"
                   "Wrong type argument in position ~a: ~a"
                   (list 1 k) (list k)))
    (with-fluid* %eh (make-exception-handler k #f pre-unwind-handler)
      thunk))

  (hashq-remove! (%get-pre-modules-obarray) '%exception-handler)
  (define! 'catch catch)
  (define! 'with-throw-handler with-throw-handler)
  (define! 'throw throw))




;;;
;;; Extensible exception printing.
;;;

(define set-exception-printer! #f)
;; There is already a definition of print-exception from backtrace.c
;; that we will override.

(let ((exception-printers '()))
  (define (print-location frame port)
    (let ((source (and=> frame frame-source)))
      ;; source := (addr . (filename . (line . column)))
      (if source
          (let ((filename (or (cadr source) "<unnamed port>"))
                (line (caddr source))
                (col (cdddr source)))
            (format port "~a:~a:~a: " filename (1+ line) col))
          (format port "ERROR: "))))

  (set! set-exception-printer!
        (lambda (key proc)
          (set! exception-printers (acons key proc exception-printers))))

  (set! print-exception
        (lambda (port frame key args)
          (define (default-printer)
            (format port "Throw to key `~a' with args `~s'." key args))

          (when frame
            (print-location frame port)
            ;; When booting, false-if-exception isn't defined yet.
            (let ((name (catch #t
                          (lambda () (frame-procedure-name frame))
                          (lambda _ #f))))
              (when name
                (format port "In procedure ~a:\n" name))))

          (catch #t
            (lambda ()
              (let ((printer (assq-ref exception-printers key)))
                (if printer
                    (printer port key args default-printer)
                    (default-printer))))
            (lambda (k . args)
              (format port "Error while printing exception.")))
          (newline port)
          (force-output port))))

;;;
;;; Printers for those keys thrown by Guile.
;;;
(let ()
  (define (scm-error-printer port key args default-printer)
    ;; Abuse case-lambda as a pattern matcher, given that we don't have
    ;; ice-9 match at this point.
    (apply (case-lambda
             ((subr msg args . rest)
              (if subr
                  (format port "In procedure ~a: " subr))
              (apply format port msg (or args '())))
             (_ (default-printer)))
           args))

  (define (syntax-error-printer port key args default-printer)
    (apply (case-lambda
             ((who what where form subform . extra)
              (format port "Syntax error:\n")
              (if where
                  (let ((file (or (assq-ref where 'filename) "unknown file"))
                        (line (and=> (assq-ref where 'line) 1+))
                        (col (assq-ref where 'column)))
                    (format port "~a:~a:~a: " file line col))
                  (format port "unknown location: "))
              (if who
                  (format port "~a: " who))
              (format port "~a" what)
              (if subform
                  (format port " in subform ~s of ~s" subform form)
                  (if form
                      (format port " in form ~s" form))))
             (_ (default-printer)))
           args))

  (define (keyword-error-printer port key args default-printer)
    (let ((message (cadr args))
          (faulty  (car (cadddr args)))) ; I won't do it again, I promise.
      (format port "~a: ~s" message faulty)))

  (define (getaddrinfo-error-printer port key args default-printer)
    (format port "In procedure getaddrinfo: ~a" (gai-strerror (car args))))

  (set-exception-printer! 'goops-error scm-error-printer)
  (set-exception-printer! 'host-not-found scm-error-printer)
  (set-exception-printer! 'keyword-argument-error keyword-error-printer)
  (set-exception-printer! 'misc-error scm-error-printer)
  (set-exception-printer! 'no-data scm-error-printer)
  (set-exception-printer! 'no-recovery scm-error-printer)
  (set-exception-printer! 'null-pointer-error scm-error-printer)
  (set-exception-printer! 'out-of-memory scm-error-printer)
  (set-exception-printer! 'out-of-range scm-error-printer)
  (set-exception-printer! 'program-error scm-error-printer)
  (set-exception-printer! 'read-error scm-error-printer)
  (set-exception-printer! 'regular-expression-syntax scm-error-printer)
  (set-exception-printer! 'signal scm-error-printer)
  (set-exception-printer! 'stack-overflow scm-error-printer)
  (set-exception-printer! 'system-error scm-error-printer)
  (set-exception-printer! 'try-again scm-error-printer)
  (set-exception-printer! 'unbound-variable scm-error-printer)
  (set-exception-printer! 'wrong-number-of-args scm-error-printer)
  (set-exception-printer! 'wrong-type-arg scm-error-printer)

  (set-exception-printer! 'syntax-error syntax-error-printer)

  (set-exception-printer! 'getaddrinfo-error getaddrinfo-error-printer))




;;; {Defmacros}
;;;

(define-syntax define-macro
  (lambda (x)
    "Define a defmacro."
    (syntax-case x ()
      ((_ (macro . args) doc body1 body ...)
       (string? (syntax->datum #'doc))
       #'(define-macro macro doc (lambda args body1 body ...)))
      ((_ (macro . args) body ...)
       #'(define-macro macro #f (lambda args body ...)))
      ((_ macro transformer)
       #'(define-macro macro #f transformer))
      ((_ macro doc transformer)
       (or (string? (syntax->datum #'doc))
           (not (syntax->datum #'doc)))
       #'(define-syntax macro
           (lambda (y)
             doc
             #((macro-type . defmacro)
               (defmacro-args args))
             (syntax-case y ()
               ((_ . args)
                (let ((v (syntax->datum #'args)))
                  (datum->syntax y (apply transformer v)))))))))))

(define-syntax defmacro
  (lambda (x)
    "Define a defmacro, with the old lispy defun syntax."
    (syntax-case x ()
      ((_ macro args doc body1 body ...)
       (string? (syntax->datum #'doc))
       #'(define-macro macro doc (lambda args body1 body ...)))
      ((_ macro args body ...)
       #'(define-macro macro #f (lambda args body ...))))))

(provide 'defmacro)



;;; {Deprecation}
;;;

(define-syntax begin-deprecated
  (lambda (x)
    (syntax-case x ()
      ((_ form form* ...)
       (if (include-deprecated-features)
           #'(begin form form* ...)
           #'(begin))))))



;;; {Trivial Functions}
;;;

(define (identity x) x)

(define (compose proc . rest)
  "Compose PROC with the procedures in REST, such that the last one in
REST is applied first and PROC last, and return the resulting procedure.
The given procedures must have compatible arity."
  (if (null? rest)
      proc
      (let ((g (apply compose rest)))
        (lambda args
          (call-with-values (lambda () (apply g args)) proc)))))

(define (negate proc)
  "Return a procedure with the same arity as PROC that returns the `not'
of PROC's result."
  (lambda args
    (not (apply proc args))))

(define (const value)
  "Return a procedure that accepts any number of arguments and returns
VALUE."
  (lambda _
    value))

(define (and=> value procedure)
  "When VALUE is #f, return #f.  Otherwise, return (PROC VALUE)."
  (and value (procedure value)))

(define call/cc call-with-current-continuation)

(define-syntax false-if-exception
  (syntax-rules ()
    ((false-if-exception expr)
     (catch #t
       (lambda () expr)
       (lambda args #f)))
    ((false-if-exception expr #:warning template arg ...)
     (catch #t
       (lambda () expr)
       (lambda (key . args)
         (for-each (lambda (s)
                     (if (not (string-null? s))
                         (format (current-warning-port) ";;; ~a\n" s)))
                   (string-split
                    (call-with-output-string
                     (lambda (port)
                       (format port template arg ...)
                       (print-exception port #f key args)))
                    #\newline))
         #f)))))



;;; {General Properties}
;;;

;; Properties are a lispy way to associate random info with random objects.
;; Traditionally properties are implemented as an alist or a plist actually
;; pertaining to the object in question.
;;
;; These "object properties" have the advantage that they can be associated with
;; any object, even if the object has no plist. Object properties are good when
;; you are extending pre-existing objects in unexpected ways. They also present
;; a pleasing, uniform procedure-with-setter interface. But if you have a data
;; type that always has properties, it's often still best to store those
;; properties within the object itself.

(define (make-object-property)
  ;; Weak tables are thread-safe.
  (let ((prop (make-weak-key-hash-table)))
    (make-procedure-with-setter
     (lambda (obj) (hashq-ref prop obj))
     (lambda (obj val) (hashq-set! prop obj val)))))




;;; {Symbol Properties}
;;;

;;; Symbol properties are something you see in old Lisp code. In most current
;;; Guile code, symbols are not used as a data structure -- they are used as
;;; keys into other data structures.

(define (symbol-property sym prop)
  (let ((pair (assoc prop (symbol-pref sym))))
    (and pair (cdr pair))))

(define (set-symbol-property! sym prop val)
  (let ((pair (assoc prop (symbol-pref sym))))
    (if pair
        (set-cdr! pair val)
        (symbol-pset! sym (acons prop val (symbol-pref sym))))))

(define (symbol-property-remove! sym prop)
  (let ((pair (assoc prop (symbol-pref sym))))
    (if pair
        (symbol-pset! sym (delq! pair (symbol-pref sym))))))



;;; {Arrays}
;;;

(define (array-shape a)
  (map (lambda (ind) (if (number? ind) (list 0 (+ -1 ind)) ind))
       (array-dimensions a)))



;;; {Keywords}
;;;

;;; It's much better if you can use lambda* / define*, of course.

(define (kw-arg-ref args kw)
  (let ((rem (member kw args)))
    (and rem (pair? (cdr rem)) (cadr rem))))



;;; {IOTA functions: generating lists of numbers}
;;;

(define (iota n)
  (let loop ((count (1- n)) (result '()))
    (if (< count 0) result
        (loop (1- count) (cons count result)))))



;;; {Structs}
;;;

(define (struct-layout s)
  (struct-ref (struct-vtable s) vtable-index-layout))



;;; {Records}
;;;

;; Printing records: by default, records are printed as
;;
;;   #<type-name field1: val1 field2: val2 ...>
;;
;; You can change that by giving a custom printing function to
;; MAKE-RECORD-TYPE (after the list of field symbols).  This function
;; will be called like
;;
;;   (<printer> object port)
;;
;; It should print OBJECT to PORT.

;; 0: type-name, 1: fields, 2: constructor
(define record-type-vtable
  (let ((s (make-vtable (string-append standard-vtable-fields "prprpw")
                        (lambda (s p)
                          (display "#<record-type " p)
                          (display (record-type-name s) p)
                          (display ">" p)))))
    (set-struct-vtable-name! s 'record-type)
    s))

(define (record-type? obj)
  (and (struct? obj) (eq? record-type-vtable (struct-vtable obj))))

(define* (make-record-type type-name fields #:optional printer)
  ;; Pre-generate constructors for nfields < 20.
  (define-syntax make-constructor
    (lambda (x)
      (define *max-static-argument-count* 20)
      (define (make-formals n)
        (let lp ((i 0))
          (if (< i n)
              (cons (datum->syntax
                     x 
                     (string->symbol
                      (string (integer->char (+ (char->integer #\a) i)))))
                    (lp (1+ i)))
              '())))
      (syntax-case x ()
        ((_ rtd exp) (not (identifier? #'exp))
         #'(let ((n exp))
             (make-constructor rtd n)))
        ((_ rtd nfields)
         #`(case nfields
             #,@(let lp ((n 0))
                  (if (< n *max-static-argument-count*)
                      (cons (with-syntax (((formal ...) (make-formals n))
                                          ((idx ...) (iota n))
                                          (n n))
                              #'((n)
                                 (lambda (formal ...)
                                   (let ((s (allocate-struct rtd n)))
                                     (struct-set! s idx formal)
                                     ...
                                     s))))
                            (lp (1+ n)))
                      '()))
             (else
              (lambda args
                (if (= (length args) nfields)
                    (apply make-struct/no-tail rtd args)
                    (scm-error 'wrong-number-of-args
                               (format #f "make-~a" type-name)
                               "Wrong number of arguments" '() #f)))))))))

  (define (default-record-printer s p)
    (display "#<" p)
    (display (record-type-name (record-type-descriptor s)) p)
    (let loop ((fields (record-type-fields (record-type-descriptor s)))
               (off 0))
      (cond
       ((not (null? fields))
        (display " " p)
        (display (car fields) p)
        (display ": " p)
        (display (struct-ref s off) p)
        (loop (cdr fields) (+ 1 off)))))
    (display ">" p))

  (let ((rtd (make-struct/no-tail
              record-type-vtable
              (make-struct-layout
               (apply string-append
                      (map (lambda (f) "pw") fields)))
              (or printer default-record-printer)
              type-name
              (copy-tree fields))))
    (struct-set! rtd (+ vtable-offset-user 2)
                 (make-constructor rtd (length fields)))
    ;; Temporary solution: Associate a name to the record type descriptor
    ;; so that the object system can create a wrapper class for it.
    (set-struct-vtable-name! rtd (if (symbol? type-name)
                                     type-name
                                     (string->symbol type-name)))
    rtd))

(define (record-type-name obj)
  (if (record-type? obj)
      (struct-ref obj vtable-offset-user)
      (error 'not-a-record-type obj)))

(define (record-type-fields obj)
  (if (record-type? obj)
      (struct-ref obj (+ 1 vtable-offset-user))
      (error 'not-a-record-type obj)))

(define* (record-constructor rtd #:optional field-names)
  (if (not field-names)
      (struct-ref rtd (+ 2 vtable-offset-user))
      (primitive-eval
       `(lambda ,field-names
          (make-struct/no-tail ',rtd
                               ,@(map (lambda (f)
                                        (if (memq f field-names)
                                            f
                                            #f))
                                      (record-type-fields rtd)))))))
          
(define (record-predicate rtd)
  (lambda (obj) (and (struct? obj) (eq? rtd (struct-vtable obj)))))

(define (%record-type-error rtd obj)  ;; private helper
  (or (eq? rtd (record-type-descriptor obj))
      (scm-error 'wrong-type-arg "%record-type-check"
                 "Wrong type record (want `~S'): ~S"
                 (list (record-type-name rtd) obj)
                 #f)))

(define (record-accessor rtd field-name)
  (let ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
        (error 'no-such-field field-name))
    (lambda (obj)
      (if (eq? (struct-vtable obj) rtd)
          (struct-ref obj pos)
          (%record-type-error rtd obj)))))

(define (record-modifier rtd field-name)
  (let ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
        (error 'no-such-field field-name))
    (lambda (obj val)
      (if (eq? (struct-vtable obj) rtd)
          (struct-set! obj pos val)
          (%record-type-error rtd obj)))))

(define (record? obj)
  (and (struct? obj) (record-type? (struct-vtable obj))))

(define (record-type-descriptor obj)
  (if (struct? obj)
      (struct-vtable obj)
      (error 'not-a-record obj)))

(provide 'record)



;;; {Parameters}
;;;

(define <parameter>
  ;; Three fields: the procedure itself, the fluid, and the converter.
  (make-struct/no-tail <applicable-struct-vtable> 'pwprpr))
(set-struct-vtable-name! <parameter> '<parameter>)

(define* (make-parameter init #:optional (conv (lambda (x) x)))
  "Make a new parameter.

A parameter is a dynamically bound value, accessed through a procedure.
To access the current value, apply the procedure with no arguments:

  (define p (make-parameter 10))
  (p) => 10

To provide a new value for the parameter in a dynamic extent, use
`parameterize':

  (parameterize ((p 20))
    (p)) => 20
  (p) => 10

The value outside of the dynamic extent of the body is unaffected.  To
update the current value, apply it to one argument:

  (p 20) => 10
  (p) => 20

As you can see, the call that updates a parameter returns its previous
value.

All values for the parameter are first run through the CONV procedure,
including INIT, the initial value.  The default CONV procedure is the
identity procedure.  CONV is commonly used to ensure some set of
invariants on the values that a parameter may have."
  (let ((fluid (make-fluid (conv init))))
    (make-struct/no-tail
     <parameter>
     (case-lambda
      (() (fluid-ref fluid))
      ((x) (let ((prev (fluid-ref fluid)))
             (fluid-set! fluid (conv x))
             prev)))
     fluid conv)))

(define (parameter? x)
  (and (struct? x) (eq? (struct-vtable x) <parameter>)))

(define (parameter-fluid p)
  (if (parameter? p)
      (struct-ref p 1)
      (scm-error 'wrong-type-arg "parameter-fluid"
                 "Not a parameter: ~S" (list p) #f)))

(define (parameter-converter p)
  (if (parameter? p)
      (struct-ref p 2)
      (scm-error 'wrong-type-arg "parameter-fluid"
                 "Not a parameter: ~S" (list p) #f)))

(define-syntax parameterize
  (lambda (x)
    (syntax-case x ()
      ((_ ((param value) ...) body body* ...)
       (with-syntax (((p ...) (generate-temporaries #'(param ...))))
         #'(let ((p param) ...)
             (if (not (parameter? p))
                        (scm-error 'wrong-type-arg "parameterize"
                                   "Not a parameter: ~S" (list p) #f))
             ...
             (with-fluids (((struct-ref p 1) ((struct-ref p 2) value))
                           ...)
               body body* ...)))))))

(define* (fluid->parameter fluid #:optional (conv (lambda (x) x)))
  "Make a parameter that wraps a fluid.

The value of the parameter will be the same as the value of the fluid.
If the parameter is rebound in some dynamic extent, perhaps via
`parameterize', the new value will be run through the optional CONV
procedure, as with any parameter.  Note that unlike `make-parameter',
CONV is not applied to the initial value."
  (make-struct/no-tail
   <parameter>
   (case-lambda
    (() (fluid-ref fluid))
    ((x) (let ((prev (fluid-ref fluid)))
           (fluid-set! fluid (conv x))
           prev)))
   fluid conv))



;;; Once parameters have booted, define the default prompt tag as being
;;; a parameter, and make allow-legacy-syntax-objects? a parameter.
;;;

(set! default-prompt-tag (make-parameter (default-prompt-tag)))

;; Because code compiled with Guile 2.2.0 embeds legacy syntax objects
;; into its compiled macros, we have to default to true, sadly.
(set! allow-legacy-syntax-objects? (make-parameter #t))



;;; {Languages}
;;;

;; The language can be a symbolic name or a <language> object from
;; (system base language).
;;
(define current-language (make-parameter 'scheme))




;;; {High-Level Port Routines}
;;;

(define (call-with-output-string proc)
  "Calls the one-argument procedure @var{proc} with a newly created output
port.  When the function returns, the string composed of the characters
written into the port is returned."
  (let ((port (open-output-string)))
    (proc port)
    (get-output-string port)))



;;; {Booleans}
;;;

(define (->bool x) (not (not x)))



;;; {Symbols}
;;;

(define (symbol-append . args)
  (string->symbol (apply string-append (map symbol->string args))))

(define (list->symbol . args)
  (string->symbol (apply list->string args)))

(define (symbol . args)
  (string->symbol (apply string args)))



;;; {Lists}
;;;

(define (list-index l k)
  (let loop ((n 0)
             (l l))
    (and (not (null? l))
         (if (eq? (car l) k)
             n
             (loop (+ n 1) (cdr l))))))



;; Load `posix.scm' even when not (provided? 'posix) so that we get the
;; `stat' accessors.
(primitive-load-path "ice-9/posix")

(if (provided? 'socket)
    (primitive-load-path "ice-9/networking"))

;; For reference, Emacs file-exists-p uses stat in this same way.
(define file-exists?
  (if (provided? 'posix)
      (lambda (str)
        (->bool (stat str #f)))
      (lambda (str)
        (let ((port (catch 'system-error (lambda () (open-input-file str))
                           (lambda args #f))))
          (if port (begin (close-port port) #t)
              #f)))))

(define file-is-directory?
  (if (provided? 'posix)
      (lambda (str)
        (eq? (stat:type (stat str)) 'directory))
      (lambda (str)
        (let ((port (catch 'system-error
                           (lambda ()
                             (open-input-file (string-append str "/.")))
                           (lambda args #f))))
          (if port (begin (close-port port) #t)
              #f)))))

(define (system-error-errno args)
  (if (eq? (car args) 'system-error)
      (car (list-ref args 4))
      #f))



;;; {Error Handling}
;;;

(define error
  (case-lambda
    (()
     (scm-error 'misc-error #f "?" #f #f))
    ((message . args)
     (let ((msg (string-join (cons "~A" (make-list (length args) "~S")))))
       (scm-error 'misc-error #f msg (cons message args) #f)))))



;;; {Time Structures}
;;;

(define (tm:sec obj) (vector-ref obj 0))
(define (tm:min obj) (vector-ref obj 1))
(define (tm:hour obj) (vector-ref obj 2))
(define (tm:mday obj) (vector-ref obj 3))
(define (tm:mon obj) (vector-ref obj 4))
(define (tm:year obj) (vector-ref obj 5))
(define (tm:wday obj) (vector-ref obj 6))
(define (tm:yday obj) (vector-ref obj 7))
(define (tm:isdst obj) (vector-ref obj 8))
(define (tm:gmtoff obj) (vector-ref obj 9))
(define (tm:zone obj) (vector-ref obj 10))

(define (set-tm:sec obj val) (vector-set! obj 0 val))
(define (set-tm:min obj val) (vector-set! obj 1 val))
(define (set-tm:hour obj val) (vector-set! obj 2 val))
(define (set-tm:mday obj val) (vector-set! obj 3 val))
(define (set-tm:mon obj val) (vector-set! obj 4 val))
(define (set-tm:year obj val) (vector-set! obj 5 val))
(define (set-tm:wday obj val) (vector-set! obj 6 val))
(define (set-tm:yday obj val) (vector-set! obj 7 val))
(define (set-tm:isdst obj val) (vector-set! obj 8 val))
(define (set-tm:gmtoff obj val) (vector-set! obj 9 val))
(define (set-tm:zone obj val) (vector-set! obj 10 val))

(define (tms:clock obj) (vector-ref obj 0))
(define (tms:utime obj) (vector-ref obj 1))
(define (tms:stime obj) (vector-ref obj 2))
(define (tms:cutime obj) (vector-ref obj 3))
(define (tms:cstime obj) (vector-ref obj 4))



;;; {C Environment}
;;;

(define (setenv name value)
  (if value
      (putenv (string-append name "=" value))
      (putenv name)))

(define (unsetenv name)
  "Remove the entry for NAME from the environment."
  (putenv name))



;;; {Load Paths}
;;;

(let-syntax ((compile-time-case
              (lambda (stx)
                (syntax-case stx ()
                  ((_ exp clauses ...)
                   (let ((val (primitive-eval (syntax->datum #'exp))))
                     (let next-clause ((clauses #'(clauses ...)))
                       (syntax-case clauses (else)
                         (()
                          (syntax-violation 'compile-time-case
                                            "all clauses failed to match" stx))
                         (((else form ...))
                          #'(begin form ...))
                         ((((k ...) form ...) clauses ...)
                          (if (memv val (syntax->datum #'(k ...)))
                              #'(begin form ...)
                              (next-clause #'(clauses ...))))))))))))
  ;; emacs: (put 'compile-time-case 'scheme-indent-function 1)
  (compile-time-case (system-file-name-convention)
    ((posix)
     (define (file-name-separator? c)
       (char=? c #\/))

     (define file-name-separator-string "/")

     (define (absolute-file-name? file-name)
       (string-prefix? "/" file-name)))

    ((windows)
     (define (file-name-separator? c)
       (or (char=? c #\/)
           (char=? c #\\)))

     (define file-name-separator-string "/")

     (define (absolute-file-name? file-name)
       (define (file-name-separator-at-index? idx)
         (and (> (string-length file-name) idx)
              (file-name-separator? (string-ref file-name idx))))
       (define (unc-file-name?)
         ;; Universal Naming Convention (UNC) file-names start with \\,
         ;; and are always absolute.  See:
         ;;   http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx#fully_qualified_vs._relative_paths
         (and (file-name-separator-at-index? 0)
              (file-name-separator-at-index? 1)))
       (define (has-drive-specifier?)
         (and (>= (string-length file-name) 2)
              (let ((drive (string-ref file-name 0)))
                (or (char<=? #\a drive #\z)
                    (char<=? #\A drive #\Z)))
              (eqv? (string-ref file-name 1) #\:)))
       (or (unc-file-name?)
           (if (has-drive-specifier?)
               (file-name-separator-at-index? 2)
               (file-name-separator-at-index? 0)))))))

(define (in-vicinity vicinity file)
  (let ((tail (let ((len (string-length vicinity)))
                (if (zero? len)
                    #f
                    (string-ref vicinity (- len 1))))))
    (string-append vicinity
                   (if (or (not tail) (file-name-separator? tail))
                       ""
                       file-name-separator-string)
                   file)))



;;; {Help for scm_shell}
;;;
;;; The argument-processing code used by Guile-based shells generates
;;; Scheme code based on the argument list.  This page contains help
;;; functions for the code it generates.
;;;

(define (command-line) (program-arguments))

;; This is mostly for the internal use of the code generated by
;; scm_compile_shell_switches.

(define (load-user-init)
  (let* ((home (or (getenv "HOME")
                   (false-if-exception (passwd:dir (getpwuid (getuid))))
                   file-name-separator-string))  ;; fallback for cygwin etc.
         (init-file (in-vicinity home ".guile")))
    (if (file-exists? init-file)
        (primitive-load init-file))))



;;; {The interpreter stack}
;;;

;; %stacks defined in stacks.c
(define (%start-stack tag thunk)
  (let ((prompt-tag (make-prompt-tag "start-stack")))
    (call-with-prompt
     prompt-tag
     (lambda ()
       (with-fluids ((%stacks (cons tag prompt-tag)))
         (thunk)))
     (lambda (k . args)
       (%start-stack tag (lambda () (apply k args)))))))

(define-syntax-rule (start-stack tag exp)
  (%start-stack tag (lambda () exp)))



;;; {Loading by paths}
;;;

(define (load-from-path name)
  "Load a Scheme source file named NAME, searching for it in the
directories listed in %load-path, and applying each of the file
name extensions listed in %load-extensions."
  (start-stack 'load-stack
               (primitive-load-path name)))

(define-syntax-rule (add-to-load-path elt)
  "Add ELT to Guile's load path, at compile-time and at run-time."
  (eval-when (expand load eval)
    (set! %load-path (cons elt (delete elt %load-path)))))

(define %load-verbosely #f)
(define (assert-load-verbosity v) (set! %load-verbosely v))

(define (%load-announce file)
  (if %load-verbosely
      (with-output-to-port (current-warning-port)
        (lambda ()
          (display ";;; ")
          (display "loading ")
          (display file)
          (newline)
          (force-output)))))

(set! %load-hook %load-announce)



;;; {Reader Extensions}
;;;
;;; Reader code for various "#c" forms.
;;;

(define read-eval? (make-fluid #f))
(read-hash-extend #\.
                  (lambda (c port)
                    (if (fluid-ref read-eval?)
                        (eval (read port) (interaction-environment))
                        (error
                         "#. read expansion found and read-eval? is #f."))))



;;; {Low Level Modules}
;;;
;;; These are the low level data structures for modules.
;;;
;;; Every module object is of the type 'module-type', which is a record
;;; consisting of the following members:
;;;
;;; - eval-closure: A deprecated field, to be removed in Guile 2.2.
;;;
;;; - obarray: a hash table that maps symbols to variable objects.  In this
;;;   hash table, the definitions are found that are local to the module (that
;;;   is, not imported from other modules).  When looking up bindings in the
;;;   module, this hash table is searched first.
;;;
;;; - binder: either #f or a function taking a module and a symbol argument.
;;;   If it is a function it is called after the obarray has been
;;;   unsuccessfully searched for a binding.  It then can provide bindings
;;;   that would otherwise not be found locally in the module.
;;;
;;; - uses: a list of modules from which non-local bindings can be inherited.
;;;   These modules are the third place queried for bindings after the obarray
;;;   has been unsuccessfully searched and the binder function did not deliver
;;;   a result either.
;;;
;;; - transformer: either #f or a function taking a scheme expression as
;;;   delivered by read.  If it is a function, it will be called to perform
;;;   syntax transformations (e. g. makro expansion) on the given scheme
;;;   expression. The output of the transformer function will then be passed
;;;   to Guile's internal memoizer.  This means that the output must be valid
;;;   scheme code.  The only exception is, that the output may make use of the
;;;   syntax extensions provided to identify the modules that a binding
;;;   belongs to.
;;;
;;; - name: the name of the module.  This is used for all kinds of printing
;;;   outputs.  In certain places the module name also serves as a way of
;;;   identification.  When adding a module to the uses list of another
;;;   module, it is made sure that the new uses list will not contain two
;;;   modules of the same name.
;;;
;;; - kind: classification of the kind of module.  The value is (currently?)
;;;   only used for printing.  It has no influence on how a module is treated.
;;;   Currently the following values are used when setting the module kind:
;;;   'module, 'directory, 'interface, 'custom-interface.  If no explicit kind
;;;   is set, it defaults to 'module.
;;;
;;; - duplicates-handlers: a list of procedures that get called to make a
;;;   choice between two duplicate bindings when name clashes occur.  See the
;;;   `duplicate-handlers' global variable below.
;;;
;;; - observers: a list of procedures that get called when the module is
;;;   modified.
;;;
;;; - weak-observers: a weak-key hash table of procedures that get called
;;;   when the module is modified.  See `module-observe-weak' for details.
;;;
;;; In addition, the module may (must?) contain a binding for
;;; `%module-public-interface'.  This variable should be bound to a module
;;; representing the exported interface of a module.  See the
;;; `module-public-interface' and `module-export!' procedures.
;;;
;;; !!! warning: The interface to lazy binder procedures is going
;;; to be changed in an incompatible way to permit all the basic
;;; module ops to be virtualized.
;;;
;;; (make-module size use-list lazy-binding-proc) => module
;;; module-{obarray,uses,binder}[|-set!]
;;; (module? obj) => [#t|#f]
;;; (module-locally-bound? module symbol) => [#t|#f]
;;; (module-bound? module symbol) => [#t|#f]
;;; (module-symbol-locally-interned? module symbol) => [#t|#f]
;;; (module-symbol-interned? module symbol) => [#t|#f]
;;; (module-local-variable module symbol) => [#<variable ...> | #f]
;;; (module-variable module symbol) => [#<variable ...> | #f]
;;; (module-symbol-binding module symbol opt-value)
;;;             => [ <obj> | opt-value | an error occurs ]
;;; (module-make-local-var! module symbol) => #<variable...>
;;; (module-add! module symbol var) => unspecified
;;; (module-remove! module symbol) =>  unspecified
;;; (module-for-each proc module) => unspecified
;;; (make-scm-module) => module ; a lazy copy of the symhash module
;;; (set-current-module module) => unspecified
;;; (current-module) => #<module...>
;;;
;;;



;;; {Printing Modules}
;;;

;; This is how modules are printed.  You can re-define it.
(define (%print-module mod port)
  (display "#<" port)
  (display (or (module-kind mod) "module") port)
  (display " " port)
  (display (module-name mod) port)
  (display " " port)
  (display (number->string (object-address mod) 16) port)
  (display ">" port))

(letrec-syntax
     ;; Locally extend the syntax to allow record accessors to be defined at
     ;; compile-time. Cache the rtd locally to the constructor, the getters and
     ;; the setters, in order to allow for redefinition of the record type; not
     ;; relevant in the case of modules, but perhaps if we make this public, it
     ;; could matter.

    ((define-record-type
       (lambda (x)
         (define (make-id scope . fragments)
           (datum->syntax scope
                          (apply symbol-append
                                 (map (lambda (x)
                                        (if (symbol? x) x (syntax->datum x)))
                                      fragments))))
         
         (define (getter rtd type-name field slot)
           #`(define #,(make-id rtd type-name '- field)
               (let ((rtd #,rtd))
                 (lambda (#,type-name)
                   (if (eq? (struct-vtable #,type-name) rtd)
                       (struct-ref #,type-name #,slot)
                       (%record-type-error rtd #,type-name))))))

         (define (setter rtd type-name field slot)
           #`(define #,(make-id rtd 'set- type-name '- field '!)
               (let ((rtd #,rtd))
                 (lambda (#,type-name val)
                   (if (eq? (struct-vtable #,type-name) rtd)
                       (struct-set! #,type-name #,slot val)
                       (%record-type-error rtd #,type-name))))))

         (define (accessors rtd type-name fields n exp)
           (syntax-case fields ()
             (() exp)
             (((field #:no-accessors) field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         exp))
             (((field #:no-setter) field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         #`(begin #,exp
                                  #,(getter rtd type-name #'field n))))
             (((field #:no-getter) field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         #`(begin #,exp
                                  #,(setter rtd type-name #'field n))))
             ((field field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         #`(begin #,exp
                                  #,(getter rtd type-name #'field n)
                                  #,(setter rtd type-name #'field n))))))

         (define (predicate rtd type-name fields exp)
           (accessors
            rtd type-name fields 0
            #`(begin
                #,exp
                (define (#,(make-id rtd type-name '?) obj)
                  (and (struct? obj) (eq? (struct-vtable obj) #,rtd))))))

         (define (field-list fields)
           (syntax-case fields ()
             (() '())
             (((f . opts) . rest) (identifier? #'f)
              (cons #'f (field-list #'rest)))
             ((f . rest) (identifier? #'f)
              (cons #'f (field-list #'rest)))))

         (define (constructor rtd type-name fields exp)
           (let* ((ctor (make-id rtd type-name '-constructor))
                  (args (field-list fields))
                  (n (length fields))
                  (slots (iota n)))
             (predicate rtd type-name fields
                        #`(begin #,exp
                                 (define #,ctor
                                   (let ((rtd #,rtd))
                                     (lambda #,args
                                       (let ((s (allocate-struct rtd #,n)))
                                         #,@(map
                                             (lambda (arg slot)
                                               #`(struct-set! s #,slot #,arg))
                                             args slots)
                                         s))))
                                 (struct-set! #,rtd (+ vtable-offset-user 2)
                                              #,ctor)))))

         (define (type type-name printer fields)
           (define (make-layout)
             (let lp ((fields fields) (slots '()))
               (syntax-case fields ()
                 (() (datum->syntax #'here
                                    (make-struct-layout
                                     (apply string-append slots))))
                 ((_ . rest) (lp #'rest (cons "pw" slots))))))

           (let ((rtd (make-id type-name type-name '-type)))
             (constructor rtd type-name fields
                          #`(begin
                              (define #,rtd
                                (make-struct/no-tail
                                 record-type-vtable
                                 '#,(make-layout)
                                 #,printer
                                 '#,type-name
                                 '#,(field-list fields)))
                              (set-struct-vtable-name! #,rtd '#,type-name)))))

         (syntax-case x ()
           ((_ type-name printer (field ...))
            (type #'type-name #'printer #'(field ...)))))))

  ;; module-type
  ;;
  ;; A module is characterized by an obarray in which local symbols
  ;; are interned, a list of modules, "uses", from which non-local
  ;; bindings can be inherited, and an optional lazy-binder which
  ;; is a (CLOSURE module symbol) which, as a last resort, can provide
  ;; bindings that would otherwise not be found locally in the module.
  ;;
  ;; NOTE: If you change the set of fields or their order, you also need to
  ;; change the constants in libguile/modules.h.
  ;;
  ;; NOTE: The getter `module-transfomer' is defined libguile/modules.c.
  ;; NOTE: The getter `module-name' is defined later, due to boot reasons.
  ;; NOTE: The getter `module-public-interface' is used in libguile/modules.c.
  ;;
  (define-record-type module
    (lambda (obj port) (%print-module obj port))
    (obarray
     uses
     binder
     eval-closure
     (transformer #:no-getter)
     (name #:no-getter)
     kind
     duplicates-handlers
     (import-obarray #:no-setter)
     observers
     (weak-observers #:no-setter)
     version
     submodules
     submodule-binder
     public-interface
     filename
     next-unique-id)))


;; make-module &opt size uses binder
;;
(define* (make-module #:optional (size 31) (uses '()) (binder #f))
  "Create a new module, perhaps with a particular size of obarray,
initial uses list, or binding procedure."
  (if (not (integer? size))
      (error "Illegal size to make-module." size))
  (if (not (and (list? uses)
                (and-map module? uses)))
      (error "Incorrect use list." uses))
  (if (and binder (not (procedure? binder)))
      (error
       "Lazy-binder expected to be a procedure or #f." binder))

  (module-constructor (make-hash-table size)
                      uses binder #f macroexpand
                      #f #f #f
                      (make-hash-table)
                      '()
                      (make-weak-key-hash-table 31) #f
                      (make-hash-table 7) #f #f #f 0))




;;; {Observer protocol}
;;;

(define (module-observe module proc)
  (set-module-observers! module (cons proc (module-observers module)))
  (cons module proc))

(define* (module-observe-weak module observer-id #:optional (proc observer-id))
  "Register PROC as an observer of MODULE under name OBSERVER-ID (which can
be any Scheme object).  PROC is invoked and passed MODULE any time
MODULE is modified.  PROC gets unregistered when OBSERVER-ID gets GC'd
(thus, it is never unregistered if OBSERVER-ID is an immediate value,
for instance).

The two-argument version is kept for backward compatibility: when called
with two arguments, the observer gets unregistered when closure PROC
gets GC'd (making it impossible to use an anonymous lambda for PROC)."
  (hashq-set! (module-weak-observers module) observer-id proc))

(define (module-unobserve token)
  (let ((module (car token))
        (id (cdr token)))
    (if (integer? id)
        (hash-remove! (module-weak-observers module) id)
        (set-module-observers! module (delq1! id (module-observers module)))))
  *unspecified*)

;; Hash table of module -> #t indicating modules that changed while
;; observers were deferred, or #f if observers are not being deferred.
(define module-defer-observers (make-parameter #f))

(define (module-modified m)
  (cond
   ((module-defer-observers) => (lambda (tab) (hashq-set! tab m #t)))
   (else (module-call-observers m))))

;;; This function can be used to delay calls to observers so that they
;;; can be called once only in the face of massive updating of modules.
;;;
(define (call-with-deferred-observers thunk)
  (cond
   ((module-defer-observers) (thunk))
   (else
    (let ((modules (make-hash-table)))
      (dynamic-wind (lambda () #t)
                    (lambda ()
                      (parameterize ((module-defer-observers modules))
                        (thunk)))
                    (lambda ()
                      (let ((changed (hash-map->list cons modules)))
                        (hash-clear! modules)
                        (for-each (lambda (pair)
                                    (module-call-observers (car pair)))
                                  changed))))))))

(define (module-call-observers m)
  (for-each (lambda (proc) (proc m)) (module-observers m))

  ;; We assume that weak observers don't (un)register themselves as they are
  ;; called since this would preclude proper iteration over the hash table
  ;; elements.
  (hash-for-each (lambda (id proc) (proc m)) (module-weak-observers m)))



;;; {Module Searching in General}
;;;
;;; We sometimes want to look for properties of a symbol
;;; just within the obarray of one module.  If the property
;;; holds, then it is said to hold ``locally'' as in, ``The symbol
;;; DISPLAY is locally rebound in the module `safe-guile'.''
;;;
;;;
;;; Other times, we want to test for a symbol property in the obarray
;;; of M and, if it is not found there, try each of the modules in the
;;; uses list of M.  This is the normal way of testing for some
;;; property, so we state these properties without qualification as
;;; in: ``The symbol 'fnord is interned in module M because it is
;;; interned locally in module M2 which is a member of the uses list
;;; of M.''
;;;

(define (module-search fn m v)
  "Return the first non-#f result of FN applied to M and then to
the modules in the uses of M, and so on recursively.  If all applications
return #f, then so does this function."
  (define (loop pos)
    (and (pair? pos)
         (or (module-search fn (car pos) v)
             (loop (cdr pos)))))
  (or (fn m v)
      (loop (module-uses m))))


;;; {Is a symbol bound in a module?}
;;;
;;; Symbol S in Module M is bound if S is interned in M and if the binding
;;; of S in M has been set to some well-defined value.
;;;

(define (module-locally-bound? m v)
  "Is symbol V bound (interned and defined) locally in module M?"
  (let ((var (module-local-variable m v)))
    (and var
         (variable-bound? var))))

(define (module-bound? m v)
  "Is symbol V bound (interned and defined) anywhere in module M or its
uses?"
  (let ((var (module-variable m v)))
    (and var
         (variable-bound? var))))

;;; {Is a symbol interned in a module?}
;;;
;;; Symbol S in Module M is interned if S occurs in
;;; of S in M has been set to some well-defined value.
;;;
;;; It is possible to intern a symbol in a module without providing
;;; an initial binding for the corresponding variable.  This is done
;;; with:
;;;       (module-add! module symbol (make-undefined-variable))
;;;
;;; In that case, the symbol is interned in the module, but not
;;; bound there.  The unbound symbol shadows any binding for that
;;; symbol that might otherwise be inherited from a member of the uses list.
;;;

(define (module-obarray-get-handle ob key)
  ((if (symbol? key) hashq-get-handle hash-get-handle) ob key))

(define (module-obarray-ref ob key)
  ((if (symbol? key) hashq-ref hash-ref) ob key))

(define (module-obarray-set! ob key val)
  ((if (symbol? key) hashq-set! hash-set!) ob key val))

(define (module-obarray-remove! ob key)
  ((if (symbol? key) hashq-remove! hash-remove!) ob key))

(define (module-symbol-locally-interned? m v)
  "Is symbol V interned (not neccessarily defined) locally in module M
or its uses?  Interned symbols shadow inherited bindings even if they
are not themselves bound to a defined value."
  (not (not (module-obarray-get-handle (module-obarray m) v))))

(define (module-symbol-interned? m v)
  "Is symbol V interned (not neccessarily defined) anywhere in module M
or its uses?  Interned symbols shadow inherited bindings even if they
are not themselves bound to a defined value."
  (module-search module-symbol-locally-interned? m v))


;;; {Mapping modules x symbols --> variables}
;;;

;; module-local-variable module symbol
;; return the local variable associated with a MODULE and SYMBOL.
;;
;;; This function is very important. It is the only function that can
;;; return a variable from a module other than the mutators that store
;;; new variables in modules.  Therefore, this function is the location
;;; of the "lazy binder" hack.
;;;
;;; If symbol is defined in MODULE, and if the definition binds symbol
;;; to a variable, return that variable object.
;;;
;;; If the symbols is not found at first, but the module has a lazy binder,
;;; then try the binder.
;;;
;;; If the symbol is not found at all, return #f.
;;;
;;; (This is now written in C, see `modules.c'.)
;;;

;;; {Mapping modules x symbols --> bindings}
;;;
;;; These are similar to the mapping to variables, except that the
;;; variable is dereferenced.
;;;

(define (module-symbol-local-binding m v . opt-val)
  "Return the binding of variable V specified by name within module M,
signalling an error if the variable is unbound.  If the OPT-VALUE is
passed, then instead of signalling an error, return OPT-VALUE."
  (let ((var (module-local-variable m v)))
    (if (and var (variable-bound? var))
        (variable-ref var)
        (if (not (null? opt-val))
            (car opt-val)
            (error "Locally unbound variable." v)))))

(define (module-symbol-binding m v . opt-val)
  "Return the binding of variable V specified by name within module M,
signalling an error if the variable is unbound.  If the OPT-VALUE is
passed, then instead of signalling an error, return OPT-VALUE."
  (let ((var (module-variable m v)))
    (if (and var (variable-bound? var))
        (variable-ref var)
        (if (not (null? opt-val))
            (car opt-val)
            (error "Unbound variable." v)))))




;;; {Adding Variables to Modules}
;;;

;; This function is used in modules.c.
;;
(define (module-make-local-var! m v)
  "Ensure a variable for V in the local namespace of M.
If no variable was already there, then create a new and uninitialized
variable."
  (or (let ((b (module-obarray-ref (module-obarray m) v)))
        (and (variable? b)
             (begin
               ;; Mark as modified since this function is called when
               ;; the standard eval closure defines a binding
               (module-modified m)
               b)))

      ;; Create a new local variable.
      (let ((local-var (make-undefined-variable)))
        (module-add! m v local-var)
        local-var)))

(define (module-ensure-local-variable! module symbol)
  "Ensure that there is a local variable in MODULE for SYMBOL.  If
there is no binding for SYMBOL, create a new uninitialized
variable.  Return the local variable."
  (or (module-local-variable module symbol)
      (let ((var (make-undefined-variable)))
        (module-add! module symbol var)
        var)))

;; module-add! module symbol var
;;
(define (module-add! m v var)
  "Ensure a particular variable for V in the local namespace of M."
  (if (not (variable? var))
      (error "Bad variable to module-add!" var))
  (if (not (symbol? v))
      (error "Bad symbol to module-add!" v))
  (module-obarray-set! (module-obarray m) v var)
  (module-modified m))

(define (module-remove! m v)
  "Make sure that symbol V is undefined in the local namespace of M."
  (module-obarray-remove! (module-obarray m) v)
  (module-modified m))

(define (module-clear! m)
  (hash-clear! (module-obarray m))
  (module-modified m))

;; MODULE-FOR-EACH -- exported
;;
(define (module-for-each proc module)
  "Call PROC on each symbol in MODULE, with arguments of (SYMBOL VARIABLE)."
  (hash-for-each proc (module-obarray module)))

(define (module-map proc module)
  (hash-map->list proc (module-obarray module)))

;; Submodules
;;
;; Modules exist in a separate namespace from values, because you generally do
;; not want the name of a submodule, which you might not even use, to collide
;; with local variables that happen to be named the same as the submodule.
;;
(define (module-ref-submodule module name)
  (or (hashq-ref (module-submodules module) name)
      (and (module-submodule-binder module)
           ((module-submodule-binder module) module name))))

(define (module-define-submodule! module name submodule)
  (hashq-set! (module-submodules module) name submodule))



;;; {Module-based Loading}
;;;

(define (save-module-excursion thunk)
  (let ((inner-module (current-module))
        (outer-module #f))
    (dynamic-wind (lambda ()
                    (set! outer-module (current-module))
                    (set-current-module inner-module)
                    (set! inner-module #f))
                  thunk
                  (lambda ()
                    (set! inner-module (current-module))
                    (set-current-module outer-module)
                    (set! outer-module #f)))))



;;; {MODULE-REF -- exported}
;;;
(define (module-ref module name . rest)
  "Returns the value of a variable called NAME in MODULE or any of its
used modules.  If there is no such variable, then if the optional third
argument DEFAULT is present, it is returned; otherwise an error is signaled."
  (let ((variable (module-variable module name)))
    (if (and variable (variable-bound? variable))
        (variable-ref variable)
        (if (null? rest)
            (error "No variable named" name 'in module)
            (car rest)                  ; default value
            ))))

;; MODULE-SET! -- exported
;;
(define (module-set! module name value)
  "Sets the variable called NAME in MODULE (or in a module that MODULE uses)
to VALUE; if there is no such variable, an error is signaled."
  (let ((variable (module-variable module name)))
    (if variable
        (variable-set! variable value)
        (error "No variable named" name 'in module))))

;; MODULE-DEFINE! -- exported
;;
(define (module-define! module name value)
  "Sets the variable called NAME in MODULE to VALUE; if there is no such
variable, it is added first."
  (let ((variable (module-local-variable module name)))
    (if variable
        (begin
          (variable-set! variable value)
          (module-modified module))
        (let ((variable (make-variable value)))
          (module-add! module name variable)))))

;; MODULE-DEFINED? -- exported
;;
(define (module-defined? module name)
  "Return #t iff NAME is defined in MODULE (or in a module that MODULE
uses)."
  (let ((variable (module-variable module name)))
    (and variable (variable-bound? variable))))

(define (module-use! module interface)
  "Add INTERFACE to the list of interfaces used by MODULE."
  (if (not (or (eq? module interface)
               (memq interface (module-uses module))))
      (begin
        ;; Newly used modules must be appended rather than consed, so that
        ;; `module-variable' traverses the use list starting from the first
        ;; used module.
        (set-module-uses! module (append (module-uses module)
                                         (list interface)))
        (hash-clear! (module-import-obarray module))
        (module-modified module))))

(define (module-use-interfaces! module interfaces)
  "Same as MODULE-USE!, but only notifies module observers after all
interfaces are added to the inports list."
  (let* ((cur (module-uses module))
         (new (let lp ((in interfaces) (out '()))
                (if (null? in)
                    (reverse out)
                    (lp (cdr in)
                        (let ((iface (car in)))
                          (if (or (memq iface cur) (memq iface out))
                              out
                              (cons iface out))))))))
    (set-module-uses! module (append cur new))
    (hash-clear! (module-import-obarray module))
    (module-modified module)))



;;; {Recursive Namespaces}
;;;
;;; A hierarchical namespace emerges if we consider some module to be
;;; root, and submodules of that module to be nested namespaces.
;;;
;;; The routines here manage variable names in hierarchical namespace.
;;; Each variable name is a list of elements, looked up in successively nested
;;; modules.
;;;
;;;             (nested-ref some-root-module '(foo bar baz))
;;;             => <value of a variable named baz in the submodule bar of
;;;                 the submodule foo of some-root-module>
;;;
;;;
;;; There are:
;;;
;;;     ;; a-root is a module
;;;     ;; name is a list of symbols
;;;
;;;     nested-ref a-root name
;;;     nested-set! a-root name val
;;;     nested-define! a-root name val
;;;     nested-remove! a-root name
;;;
;;; These functions manipulate values in namespaces. For referencing the
;;; namespaces themselves, use the following:
;;;
;;;     nested-ref-module a-root name
;;;     nested-define-module! a-root name mod
;;;
;;; (current-module) is a natural choice for a root so for convenience there are
;;; also:
;;;
;;;     local-ref name                ==  nested-ref (current-module) name
;;;     local-set! name val           ==  nested-set! (current-module) name val
;;;     local-define name val         ==  nested-define! (current-module) name val
;;;     local-remove name             ==  nested-remove! (current-module) name
;;;     local-ref-module name         ==  nested-ref-module (current-module) name
;;;     local-define-module! name m   ==  nested-define-module! (current-module) name m
;;;


(define (nested-ref root names)
  (if (null? names)
      root
      (let loop ((cur root)
                 (head (car names))
                 (tail (cdr names)))
        (if (null? tail)
            (module-ref cur head #f)
            (let ((cur (module-ref-submodule cur head)))
              (and cur
                   (loop cur (car tail) (cdr tail))))))))

(define (nested-set! root names val)
  (let loop ((cur root)
             (head (car names))
             (tail (cdr names)))
    (if (null? tail)
        (module-set! cur head val)
        (let ((cur (module-ref-submodule cur head)))
          (if (not cur)
              (error "failed to resolve module" names)
              (loop cur (car tail) (cdr tail)))))))

(define (nested-define! root names val)
  (let loop ((cur root)
             (head (car names))
             (tail (cdr names)))
    (if (null? tail)
        (module-define! cur head val)
        (let ((cur (module-ref-submodule cur head)))
          (if (not cur)
              (error "failed to resolve module" names)
              (loop cur (car tail) (cdr tail)))))))

(define (nested-remove! root names)
  (let loop ((cur root)
             (head (car names))
             (tail (cdr names)))
    (if (null? tail)
        (module-remove! cur head)
        (let ((cur (module-ref-submodule cur head)))
          (if (not cur)
              (error "failed to resolve module" names)
              (loop cur (car tail) (cdr tail)))))))


(define (nested-ref-module root names)
  (let loop ((cur root)
             (names names))
    (if (null? names)
        cur
        (let ((cur (module-ref-submodule cur (car names))))
          (and cur
               (loop cur (cdr names)))))))

(define (nested-define-module! root names module)
  (if (null? names)
      (error "can't redefine root module" root module)
      (let loop ((cur root)
                 (head (car names))
                 (tail (cdr names)))
        (if (null? tail)
            (module-define-submodule! cur head module)
            (let ((cur (or (module-ref-submodule cur head)
                           (let ((m (make-module 31)))
                             (set-module-kind! m 'directory)
                             (set-module-name! m (append (module-name cur)
                                                         (list head)))
                             (module-define-submodule! cur head m)
                             m))))
              (loop cur (car tail) (cdr tail)))))))


(define (local-ref names)
  (nested-ref (current-module) names))

(define (local-set! names val)
  (nested-set! (current-module) names val))

(define (local-define names val)
  (nested-define! (current-module) names val))

(define (local-remove names)
  (nested-remove! (current-module) names))

(define (local-ref-module names)
  (nested-ref-module (current-module) names))

(define (local-define-module names mod)
  (nested-define-module! (current-module) names mod))





;;; {The (guile) module}
;;;
;;; The standard module, which has the core Guile bindings. Also called the
;;; "root module", as it is imported by many other modules, but it is not
;;; necessarily the root of anything; and indeed, the module named '() might be
;;; better thought of as a root.
;;;

;; The root module uses the pre-modules-obarray as its obarray.  This
;; special obarray accumulates all bindings that have been established
;; before the module system is fully booted.
;;
;; (The obarray continues to be used by code that has been closed over
;;  before the module system has been booted.)
;;
(define the-root-module
  (let ((m (make-module 0)))
    (set-module-obarray! m (%get-pre-modules-obarray))
    (set-module-name! m '(guile))

    ;; Inherit next-unique-id from preliminary stub of
    ;; %module-get-next-unique-id! defined above.
    (set-module-next-unique-id! m (module-generate-unique-id! #f))

    m))

;; The root interface is a module that uses the same obarray as the
;; root module.  It does not allow new definitions, tho.
;;
(define the-scm-module
  (let ((m (make-module 0)))
    (set-module-obarray! m (%get-pre-modules-obarray))
    (set-module-name! m '(guile))
    (set-module-kind! m 'interface)

    ;; In Guile 1.8 and earlier M was its own public interface.
    (set-module-public-interface! m m)

    m))

(set-module-public-interface! the-root-module the-scm-module)



;; Now that we have a root module, even though modules aren't fully booted,
;; expand the definition of resolve-module.
;;
(define (resolve-module name . args)
  (if (equal? name '(guile))
      the-root-module
      (error "unexpected module to resolve during module boot" name)))

(define (module-generate-unique-id! m)
  (let ((i (module-next-unique-id m)))
    (set-module-next-unique-id! m (+ i 1))
    i))

;; Cheat.  These bindings are needed by modules.c, but we don't want
;; to move their real definition here because that would be unnatural.
;;
(define define-module* #f)
(define process-use-modules #f)
(define module-export! #f)
(define default-duplicate-binding-procedures #f)

;; This boots the module system.  All bindings needed by modules.c
;; must have been defined by now.
;;
(set-current-module the-root-module)




;; Now that modules are booted, give module-name its final definition.
;;
(define module-name
  (let ((accessor (record-accessor module-type 'name)))
    (lambda (mod)
      (or (accessor mod)
          (let ((name (list (gensym))))
            ;; Name MOD and bind it in the module root so that it's visible to
            ;; `resolve-module'. This is important as `psyntax' stores module
            ;; names and relies on being able to `resolve-module' them.
            (set-module-name! mod name)
            (nested-define-module! (resolve-module '() #f) name mod)
            (accessor mod))))))

(define* (module-gensym #:optional (id " mg") (m (current-module)))
  "Return a fresh symbol in the context of module M, based on ID (a
string or symbol).  As long as M is a valid module, this procedure is
deterministic."
  (define (->string number)
    (number->string number 16))

  (if m
      (string->symbol
       (string-append id "-"
                      (->string (hash (module-name m) most-positive-fixnum))
                      "-"
                      (->string (module-generate-unique-id! m))))
      (gensym id)))

(define (make-modules-in module name)
  (or (nested-ref-module module name)
      (let ((m (make-module 31)))
        (set-module-kind! m 'directory)
        (set-module-name! m (append (module-name module) name))
        (nested-define-module! module name m)
        m)))

(define (beautify-user-module! module)
  (let ((interface (module-public-interface module)))
    (if (or (not interface)
            (eq? interface module))
        (let ((interface (make-module 31)))
          (set-module-name! interface (module-name module))
          (set-module-version! interface (module-version module))
          (set-module-kind! interface 'interface)
          (set-module-public-interface! module interface))))
  (if (and (not (memq the-scm-module (module-uses module)))
           (not (eq? module the-root-module)))
      ;; Import the default set of bindings (from the SCM module) in MODULE.
      (module-use! module the-scm-module)))

(define (version-matches? version-ref target)
  (define (sub-versions-match? v-refs t)
    (define (sub-version-matches? v-ref t)
      (let ((matches? (lambda (v) (sub-version-matches? v t))))
        (cond
         ((number? v-ref) (eqv? v-ref t))
         ((list? v-ref)
          (case (car v-ref)
            ((>=)  (>= t (cadr v-ref)))
            ((<=)  (<= t (cadr v-ref)))
            ((and) (and-map matches? (cdr v-ref)))
            ((or)  (or-map matches? (cdr v-ref)))
            ((not) (not (matches? (cadr v-ref))))
            (else (error "Invalid sub-version reference" v-ref))))
         (else (error "Invalid sub-version reference" v-ref)))))
    (or (null? v-refs)
        (and (not (null? t))
             (sub-version-matches? (car v-refs) (car t))
             (sub-versions-match? (cdr v-refs) (cdr t)))))

  (let ((matches? (lambda (v) (version-matches? v target))))
    (or (null? version-ref)
        (case (car version-ref)
          ((and) (and-map matches? (cdr version-ref)))
          ((or)  (or-map matches? (cdr version-ref)))
          ((not) (not (matches? (cadr version-ref))))
          (else  (sub-versions-match? version-ref target))))))

(define (make-fresh-user-module)
  (let ((m (make-module)))
    (beautify-user-module! m)
    m))

;; NOTE: This binding is used in libguile/modules.c.
;;
(define resolve-module
  (let ((root (make-module)))
    (set-module-name! root '())
    ;; Define the-root-module as '(guile).
    (module-define-submodule! root 'guile the-root-module)

    (lambda* (name #:optional (autoload #t) (version #f) #:key (ensure #t))
      (let ((already (nested-ref-module root name)))
        (cond
         ((and already
               (or (not autoload) (module-public-interface already)))
          ;; A hit, a palpable hit.
          (if (and version
                   (not (version-matches? version (module-version already))))
              (error "incompatible module version already loaded" name))
          already)
         (autoload
          ;; Try to autoload the module, and recurse.
          (try-load-module name version)
          (resolve-module name #f #:ensure ensure))
         (else
          ;; No module found (or if one was, it had no public interface), and
          ;; we're not autoloading. Make an empty module if #:ensure is true.
          (or already
              (and ensure
                   (make-modules-in root name)))))))))


(define (try-load-module name version)
  (try-module-autoload name version))

(define (reload-module m)
  "Revisit the source file corresponding to the module @var{m}."
  (let ((f (module-filename m)))
    (if f
        (save-module-excursion
         (lambda () 
           ;; Re-set the initial environment, as in try-module-autoload.
           (set-current-module (make-fresh-user-module))
           (primitive-load-path f)
           m))
        ;; Though we could guess, we *should* know it.
        (error "unknown file name for module" m))))

(define (purify-module! module)
  "Removes bindings in MODULE which are inherited from the (guile) module."
  (let ((use-list (module-uses module)))
    (if (and (pair? use-list)
             (eq? (car (last-pair use-list)) the-scm-module))
        (set-module-uses! module (reverse (cdr (reverse use-list)))))))

(define* (resolve-interface name #:key
                            (select #f)
                            (hide '())
                            (prefix #f)
                            (renamer (if prefix
                                         (symbol-prefix-proc prefix)
                                         identity))
                            version)
  "Return a module that is an interface to the module designated by
NAME.

`resolve-interface' takes four keyword arguments:

  #:select SELECTION

SELECTION is a list of binding-specs to be imported; A binding-spec
is either a symbol or a pair of symbols (ORIG . SEEN), where ORIG
is the name in the used module and SEEN is the name in the using
module.  Note that SEEN is also passed through RENAMER, below.  The
default is to select all bindings.  If you specify no selection but
a renamer, only the bindings that already exist in the used module
are made available in the interface.  Bindings that are added later
are not picked up.

  #:hide BINDINGS

BINDINGS is a list of bindings which should not be imported.

  #:prefix PREFIX

PREFIX is a symbol that will be appended to each exported name.
The default is to not perform any renaming.

  #:renamer RENAMER

RENAMER is a procedure that takes a symbol and returns its new
name.  The default is not perform any renaming.

Signal \"no code for module\" error if module name is not resolvable
or its public interface is not available.  Signal \"no binding\"
error if selected binding does not exist in the used module."
  (let* ((module (resolve-module name #t version #:ensure #f))
         (public-i (and module (module-public-interface module))))
    (unless public-i
      (error "no code for module" name))
    (if (and (not select) (null? hide) (eq? renamer identity))
        public-i
        (let ((selection (or select (module-map (lambda (sym var) sym)
                                                public-i)))
              (custom-i (make-module 31)))
          (set-module-kind! custom-i 'custom-interface)
          (set-module-name! custom-i name)
          ;; XXX - should use a lazy binder so that changes to the
          ;; used module are picked up automatically.
          (for-each (lambda (bspec)
                      (let* ((direct? (symbol? bspec))
                             (orig (if direct? bspec (car bspec)))
                             (seen (if direct? bspec (cdr bspec)))
                             (var (or (module-local-variable public-i orig)
                                      (error
                                       ;; fixme: format manually for now
                                       (simple-format
                                        #f "no binding `~A' in module ~A"
                                        orig name)))))
                        (if (memq orig hide)
                            (set! hide (delq! orig hide))
                            (module-add! custom-i
                                         (renamer seen)
                                         var))))
                    selection)
          ;; Check that we are not hiding bindings which don't exist
          (for-each (lambda (binding)
                      (if (not (module-local-variable public-i binding))
                          (error
                           (simple-format
                            #f "no binding `~A' to hide in module ~A"
                            binding name))))
                    hide)
          custom-i))))

(define (symbol-prefix-proc prefix)
  (lambda (symbol)
    (symbol-append prefix symbol)))

;; This function is called from "modules.c".  If you change it, be
;; sure to update "modules.c" as well.

(define* (define-module* name
           #:key filename pure version (imports '()) (exports '())
           (replacements '()) (re-exports '()) (autoloads '())
           (duplicates #f) transformer)
  (define (list-of pred l)
    (or (null? l)
        (and (pair? l) (pred (car l)) (list-of pred (cdr l)))))
  (define (valid-import? x)
    (list? x))
  (define (valid-export? x)
    (or (symbol? x) (and (pair? x) (symbol? (car x)) (symbol? (cdr x)))))
  (define (valid-autoload? x)
    (and (pair? x) (list-of symbol? (car x)) (list-of symbol? (cdr x))))
  
  ;; We could add a #:no-check arg, set by the define-module macro, if
  ;; these checks are taking too much time.
  ;;
  (let ((module (resolve-module name #f)))
    (beautify-user-module! module)
    (when filename
      (set-module-filename! module filename))
    (when pure
      (purify-module! module))
    (when version
      (unless (list-of integer? version)
        (error "expected list of integers for version"))
      (set-module-version! module version)
      (set-module-version! (module-public-interface module) version))
    (call-with-deferred-observers
     (lambda ()
       (unless (list-of valid-import? imports)
         (error "expected imports to be a list of import specifications"))
       (unless (list-of valid-export? exports)
         (error "expected exports to be a list of symbols or symbol pairs"))
       (unless (list-of valid-export? replacements)
         (error "expected replacements to be a list of symbols or symbol pairs"))
       (unless (list-of valid-export? re-exports)
         (error "expected re-exports to be a list of symbols or symbol pairs"))
       (module-export! module exports)
       (module-replace! module replacements)
       (unless (null? imports)
         (let ((imports (map (lambda (import-spec)
                               (apply resolve-interface import-spec))
                             imports)))
           (module-use-interfaces! module imports)))
       (module-re-export! module re-exports)
       ;; FIXME: Avoid use of `apply'.
       (apply module-autoload! module autoloads)
       (let ((duplicates (or duplicates
                             ;; Avoid stompling a previously installed
                             ;; duplicates handlers if possible.
                             (and (not (module-duplicates-handlers module))
                                  ;; Note: If you change this default,
                                  ;; change it also in
                                  ;; `default-duplicate-binding-procedures'.
                                  '(replace warn-override-core warn last)))))
         (when duplicates
           (let ((handlers (lookup-duplicates-handlers duplicates)))
             (set-module-duplicates-handlers! module handlers))))))

    (when transformer
      (unless (and (pair? transformer) (list-of symbol? transformer))
        (error "expected transformer to be a module name" transformer))
      (let ((iface (resolve-interface transformer))
            (sym (car (last-pair transformer))))
        (set-module-transformer! module (module-ref iface sym))))
    
    (run-hook module-defined-hook module)
    module))

;; `module-defined-hook' is a hook that is run whenever a new module
;; is defined.  Its members are called with one argument, the new
;; module.
(define module-defined-hook (make-hook 1))



;;; {Autoload}
;;;

(define (make-autoload-interface module name bindings)
  (let ((b (lambda (a sym definep)
             (false-if-exception
              (and (memq sym bindings)
                   (let ((i (module-public-interface (resolve-module name))))
                     (if (not i)
                         (error "missing interface for module" name))
                     (let ((autoload (memq a (module-uses module))))
                       ;; Replace autoload-interface with actual interface if
                       ;; that has not happened yet.
                       (if (pair? autoload)
                           (set-car! autoload i)))
                     (module-local-variable i sym)))
              #:warning "Failed to autoload ~a in ~a:\n" sym name))))
    (module-constructor (make-hash-table 0) '() b #f #f name 'autoload #f
                        (make-hash-table 0) '() (make-weak-value-hash-table 31) #f
                        (make-hash-table 0) #f #f #f 0)))

(define (module-autoload! module . args)
  "Have @var{module} automatically load the module named @var{name} when one
of the symbols listed in @var{bindings} is looked up.  @var{args} should be a
list of module-name/binding-list pairs, e.g., as in @code{(module-autoload!
module '(ice-9 q) '(make-q q-length))}."
  (let loop ((args args))
    (cond ((null? args)
           #t)
          ((null? (cdr args))
           (error "invalid name+binding autoload list" args))
          (else
           (let ((name     (car args))
                 (bindings (cadr args)))
             (module-use! module (make-autoload-interface module
                                                          name bindings))
             (loop (cddr args)))))))




;;; {Autoloading modules}
;;;

;;; XXX FIXME autoloads-in-progress and autoloads-done
;;;           are not handled in a thread-safe way.

(define autoloads-in-progress '())

;; This function is called from scm_load_scheme_module in
;; "deprecated.c".  Please do not change its interface.
;;
(define* (try-module-autoload module-name #:optional version)
  "Try to load a module of the given name.  If it is not found, return
#f.  Otherwise return #t.  May raise an exception if a file is found,
but it fails to load."
  (let* ((reverse-name (reverse module-name))
         (name (symbol->string (car reverse-name)))
         (dir-hint-module-name (reverse (cdr reverse-name)))
         (dir-hint (apply string-append
                          (map (lambda (elt)
                                 (string-append (symbol->string elt)
                                                file-name-separator-string))
                               dir-hint-module-name))))
    (resolve-module dir-hint-module-name #f)
    (and (not (autoload-done-or-in-progress? dir-hint name))
         (let ((didit #f))
           (dynamic-wind
            (lambda () (autoload-in-progress! dir-hint name))
            (lambda ()
              (with-fluids ((current-reader #f))
                (save-module-excursion
                 (lambda () 
                   (define (call/ec proc)
                     (let ((tag (make-prompt-tag)))
                       (call-with-prompt
                        tag
                        (lambda ()
                          (proc (lambda () (abort-to-prompt tag))))
                        (lambda (k) (values)))))
                   ;; The initial environment when loading a module is a fresh
                   ;; user module.
                   (set-current-module (make-fresh-user-module))
                   ;; Here we could allow some other search strategy (other than
                   ;; primitive-load-path), for example using versions encoded
                   ;; into the file system -- but then we would have to figure
                   ;; out how to locate the compiled file, do auto-compilation,
                   ;; etc. Punt for now, and don't use versions when locating
                   ;; the file.
                   (call/ec
                    (lambda (abort)
                      (primitive-load-path (in-vicinity dir-hint name)
                                           abort)
                      (set! didit #t)))))))
            (lambda () (set-autoloaded! dir-hint name didit)))
           didit))))



;;; {Dynamic linking of modules}
;;;

(define autoloads-done '((guile . guile)))

(define (autoload-done-or-in-progress? p m)
  (let ((n (cons p m)))
    (->bool (or (member n autoloads-done)
                (member n autoloads-in-progress)))))

(define (autoload-done! p m)
  (let ((n (cons p m)))
    (set! autoloads-in-progress
          (delete! n autoloads-in-progress))
    (or (member n autoloads-done)
        (set! autoloads-done (cons n autoloads-done)))))

(define (autoload-in-progress! p m)
  (let ((n (cons p m)))
    (set! autoloads-done
          (delete! n autoloads-done))
    (set! autoloads-in-progress (cons n autoloads-in-progress))))

(define (set-autoloaded! p m done?)
  (if done?
      (autoload-done! p m)
      (let ((n (cons p m)))
        (set! autoloads-done (delete! n autoloads-done))
        (set! autoloads-in-progress (delete! n autoloads-in-progress)))))



;;; {Run-time options}
;;;

(define-syntax define-option-interface
  (syntax-rules ()
    ((_ (interface (options enable disable) (option-set!)))
     (begin
       (define options
        (case-lambda
          (() (interface))
          ((arg)
           (if (list? arg)
               (begin (interface arg) (interface))
               (for-each
                (lambda (option)
                  (apply (lambda (name value documentation)
                           (display name)
                           (let ((len (string-length (symbol->string name))))
                             (when (< len 16)
                               (display #\tab)
                               (when (< len 8)
                                 (display #\tab))))
                           (display #\tab)
                           (display value)
                           (display #\tab)
                           (display documentation)
                           (newline))
                         option))
                (interface #t))))))
       (define (enable . flags)
         (interface (append flags (interface)))
         (interface))
       (define (disable . flags)
         (let ((options (interface)))
           (for-each (lambda (flag) (set! options (delq! flag options)))
                     flags)
           (interface options)
           (interface)))
       (define-syntax-rule (option-set! opt val)
         (eval-when (expand load eval)
           (options (append (options) (list 'opt val)))))))))

(define-option-interface
  (debug-options-interface
   (debug-options debug-enable debug-disable)
   (debug-set!)))

(define-option-interface
  (read-options-interface
   (read-options read-enable read-disable)
   (read-set!)))

(define-option-interface
  (print-options-interface
   (print-options print-enable print-disable)
   (print-set!)))



;;; {The Unspecified Value}
;;;
;;; Currently Guile represents unspecified values via one particular value,
;;; which may be obtained by evaluating (if #f #f). It would be nice in the
;;; future if we could replace this with a return of 0 values, though.
;;;

(define-syntax *unspecified*
  (identifier-syntax (if #f #f)))

(define (unspecified? v) (eq? v *unspecified*))




;;; {Running Repls}
;;;

(define *repl-stack* (make-fluid '()))

;; Programs can call `batch-mode?' to see if they are running as part of a
;; script or if they are running interactively. REPL implementations ensure that
;; `batch-mode?' returns #f during their extent.
;;
(define (batch-mode?)
  (null? (fluid-ref *repl-stack*)))

;; Programs can re-enter batch mode, for example after a fork, by calling
;; `ensure-batch-mode!'. It's not a great interface, though; it would be better
;; to abort to the outermost prompt, and call a thunk there.
;;
(define (ensure-batch-mode!)
  (set! batch-mode? (lambda () #t)))

(define (quit . args)
  (apply throw 'quit args))

(define exit quit)

(define (gc-run-time)
  (cdr (assq 'gc-time-taken (gc-stats))))

(define abort-hook (make-hook))
(define before-error-hook (make-hook))
(define after-error-hook (make-hook))
(define before-backtrace-hook (make-hook))
(define after-backtrace-hook (make-hook))

(define before-read-hook (make-hook))
(define after-read-hook (make-hook))
(define before-eval-hook (make-hook 1))
(define after-eval-hook (make-hook 1))
(define before-print-hook (make-hook 1))
(define after-print-hook (make-hook 1))

;;; This hook is run at the very end of an interactive session.
;;;
(define exit-hook (make-hook))

;;; The default repl-reader function.  We may override this if we've
;;; the readline library.
(define repl-reader
  (lambda* (prompt #:optional (reader (fluid-ref current-reader)))
    (if (not (char-ready?))
        (begin
          (display (if (string? prompt) prompt (prompt)))
          ;; An interesting situation.  The printer resets the column to
          ;; 0 by printing a newline, but we then advance it by printing
          ;; the prompt.  However the port-column of the output port
          ;; does not typically correspond with the actual column on the
          ;; screen, because the input is echoed back!  Since the
          ;; input is line-buffered and thus ends with a newline, the
          ;; output will really start on column zero.  So, here we zero
          ;; it out.  See bug 9664.
          ;;
          ;; Note that for similar reasons, the output-line will not
          ;; reflect the actual line on the screen.  But given the
          ;; possibility of multiline input, the fix is not as
          ;; straightforward, so we don't bother.
          ;;
          ;; Also note that the readline implementation papers over
          ;; these concerns, because it's readline itself printing the
          ;; prompt, and not Guile.
          (set-port-column! (current-output-port) 0)))
    (force-output)
    (run-hook before-read-hook)
    ((or reader read) (current-input-port))))




;;; {While}
;;;
;;; with `continue' and `break'.
;;;

;; The inliner will remove the prompts at compile-time if it finds that
;; `continue' or `break' are not used.
;;
(define-syntax while
  (lambda (x)
    (syntax-case x ()
      ((while cond body ...)
       #`(let ((break-tag (make-prompt-tag "break"))
               (continue-tag (make-prompt-tag "continue")))
           (call-with-prompt
            break-tag
            (lambda ()
              (define-syntax #,(datum->syntax #'while 'break)
                (lambda (x)
                  (syntax-case x ()
                    ((_ arg (... ...))
                     #'(abort-to-prompt break-tag arg (... ...)))
                    (_
                     #'(lambda args
                         (apply abort-to-prompt break-tag args))))))
              (let lp ()
                (call-with-prompt
                 continue-tag
                 (lambda () 
                   (define-syntax #,(datum->syntax #'while 'continue)
                     (lambda (x)
                       (syntax-case x ()
                         ((_)
                          #'(abort-to-prompt continue-tag))
                         ((_ . args)
                          (syntax-violation 'continue "too many arguments" x))
                         (_
                          #'(lambda ()
                              (abort-to-prompt continue-tag))))))
                   (do () ((not cond) #f) body ...))
                 (lambda (k) (lp)))))
            (lambda (k . args)
              (if (null? args)
                  #t
                  (apply values args)))))))))




;;; {Module System Macros}
;;;

;; Return a list of expressions that evaluate to the appropriate
;; arguments for resolve-interface according to SPEC.

(eval-when (expand)
  (if (memq 'prefix (read-options))
      (error "boot-9 must be compiled with #:kw, not :kw")))

(define (keyword-like-symbol->keyword sym)
  (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))

(define-syntax define-module
  (lambda (x)
    (define (keyword-like? stx)
      (let ((dat (syntax->datum stx)))
        (and (symbol? dat)
             (eqv? (string-ref (symbol->string dat) 0) #\:))))
    (define (->keyword sym)
      (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))
    
    (define (parse-iface args)
      (let loop ((in args) (out '()))
        (syntax-case in ()
          (() (reverse! out))
          ;; The user wanted #:foo, but wrote :foo. Fix it.
          ((sym . in) (keyword-like? #'sym)
           (loop #`(#,(->keyword (syntax->datum #'sym)) . in) out))
          ((kw . in) (not (keyword? (syntax->datum #'kw)))
           (syntax-violation 'define-module "expected keyword arg" x #'kw))
          ((#:renamer renamer . in)
           (loop #'in (cons* #',renamer #:renamer out)))
          ((kw val . in)
           (loop #'in (cons* #'val #'kw out))))))

    (define (parse args imp exp rex rep aut)
      ;; Just quote everything except #:use-module and #:use-syntax.  We
      ;; need to know about all arguments regardless since we want to turn
      ;; symbols that look like keywords into real keywords, and the
      ;; keyword args in a define-module form are not regular
      ;; (i.e. no-backtrace doesn't take a value).
      (syntax-case args ()
        (()
         (let ((imp (if (null? imp) '() #`(#:imports `#,imp)))
               (exp (if (null? exp) '() #`(#:exports '#,exp)))
               (rex (if (null? rex) '() #`(#:re-exports '#,rex)))
               (rep (if (null? rep) '() #`(#:replacements '#,rep)))
               (aut (if (null? aut) '() #`(#:autoloads '#,aut))))
           #`(#,@imp #,@exp #,@rex #,@rep #,@aut)))
        ;; The user wanted #:foo, but wrote :foo. Fix it.
        ((sym . args) (keyword-like? #'sym)
         (parse #`(#,(->keyword (syntax->datum #'sym)) . args)
                  imp exp rex rep aut))
        ((kw . args) (not (keyword? (syntax->datum #'kw)))
         (syntax-violation 'define-module "expected keyword arg" x #'kw))
        ((#:no-backtrace . args)
         ;; Ignore this one.
         (parse #'args imp exp rex rep aut))
        ((#:pure . args)
         #`(#:pure #t . #,(parse #'args imp exp rex rep aut)))
        ((kw)
         (syntax-violation 'define-module "keyword arg without value" x #'kw))
        ((#:version (v ...) . args)
         #`(#:version '(v ...) . #,(parse #'args imp exp rex rep aut)))
        ((#:duplicates (d ...) . args)
         #`(#:duplicates '(d ...) . #,(parse #'args imp exp rex rep aut)))
        ((#:filename f . args)
         #`(#:filename 'f . #,(parse #'args imp exp rex rep aut)))
        ((#:use-module (name name* ...) . args)
         (and (and-map symbol? (syntax->datum #'(name name* ...))))
         (parse #'args #`(#,@imp ((name name* ...))) exp rex rep aut))
        ((#:use-syntax (name name* ...) . args)
         (and (and-map symbol? (syntax->datum #'(name name* ...))))
         #`(#:transformer '(name name* ...)
            . #,(parse #'args #`(#,@imp ((name name* ...))) exp rex rep aut)))
        ((#:use-module ((name name* ...) arg ...) . args)
         (and (and-map symbol? (syntax->datum #'(name name* ...))))
         (parse #'args
                #`(#,@imp ((name name* ...) #,@(parse-iface #'(arg ...))))
                exp rex rep aut))
        ((#:export (ex ...) . args)
         (parse #'args imp #`(#,@exp ex ...) rex rep aut))
        ((#:export-syntax (ex ...) . args)
         (parse #'args imp #`(#,@exp ex ...) rex rep aut))
        ((#:re-export (re ...) . args)
         (parse #'args imp exp #`(#,@rex re ...) rep aut))
        ((#:re-export-syntax (re ...) . args)
         (parse #'args imp exp #`(#,@rex re ...) rep aut))
        ((#:replace (r ...) . args)
         (parse #'args imp exp rex #`(#,@rep r ...) aut))
        ((#:replace-syntax (r ...) . args)
         (parse #'args imp exp rex #`(#,@rep r ...) aut))
        ((#:autoload name bindings . args)
         (parse #'args imp exp rex rep #`(#,@aut name bindings)))
        ((kw val . args)
         (syntax-violation 'define-module "unknown keyword or bad argument"
                           #'kw #'val))))
    
    (syntax-case x ()
      ((_ (name name* ...) arg ...)
       (and-map symbol? (syntax->datum #'(name name* ...)))
       (with-syntax (((quoted-arg ...)
                      (parse #'(arg ...) '() '() '() '() '()))
                     ;; Ideally the filename is either a string or #f;
                     ;; this hack is to work around a case in which
                     ;; port-filename returns a symbol (`socket') for
                     ;; sockets.
                     (filename (let ((f (assq-ref (or (syntax-source x) '())
                                                  'filename)))
                                 (and (string? f) f))))
         #'(eval-when (expand load eval)
             (let ((m (define-module* '(name name* ...)
                        #:filename filename quoted-arg ...)))
               (set-current-module m)
               m)))))))

;; The guts of the use-modules macro.  Add the interfaces of the named
;; modules to the use-list of the current module, in order.

;; This function is called by "modules.c".  If you change it, be sure
;; to change scm_c_use_module as well.

(define (process-use-modules module-interface-args)
  (let ((interfaces (map (lambda (mif-args)
                           (or (apply resolve-interface mif-args)
                               (error "no such module" mif-args)))
                         module-interface-args)))
    (call-with-deferred-observers
     (lambda ()
       (module-use-interfaces! (current-module) interfaces)))))

(define-syntax use-modules
  (lambda (x)
    (define (keyword-like? stx)
      (let ((dat (syntax->datum stx)))
        (and (symbol? dat)
             (eqv? (string-ref (symbol->string dat) 0) #\:))))
    (define (->keyword sym)
      (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))
    
    (define (quotify-iface args)
      (let loop ((in args) (out '()))
        (syntax-case in ()
          (() (reverse! out))
          ;; The user wanted #:foo, but wrote :foo. Fix it.
          ((sym . in) (keyword-like? #'sym)
           (loop #`(#,(->keyword (syntax->datum #'sym)) . in) out))
          ((kw . in) (not (keyword? (syntax->datum #'kw)))
           (syntax-violation 'define-module "expected keyword arg" x #'kw))
          ((#:renamer renamer . in)
           (loop #'in (cons* #'renamer #:renamer out)))
          ((kw val . in)
           (loop #'in (cons* #''val #'kw out))))))

    (define (quotify specs)
      (let lp ((in specs) (out '()))
        (syntax-case in ()
          (() (reverse out))
          (((name name* ...) . in)
           (and-map symbol? (syntax->datum #'(name name* ...)))
           (lp #'in (cons #''((name name* ...)) out)))
          ((((name name* ...) arg ...) . in)
           (and-map symbol? (syntax->datum #'(name name* ...)))
           (with-syntax (((quoted-arg ...) (quotify-iface #'(arg ...))))
             (lp #'in (cons #`(list '(name name* ...) quoted-arg ...)
                            out)))))))
    
    (syntax-case x ()
      ((_ spec ...)
       (with-syntax (((quoted-args ...) (quotify #'(spec ...))))
         #'(eval-when (expand load eval)
             (process-use-modules (list quoted-args ...))
             *unspecified*))))))

(include-from-path "ice-9/r6rs-libraries")

(define-syntax-rule (define-private foo bar)
  (define foo bar))

(define-syntax define-public
  (syntax-rules ()
    ((_ (name . args) . body)
     (begin
       (define (name . args) . body)
       (export name)))
    ((_ name val)
     (begin
       (define name val)
       (export name)))))

(define-syntax-rule (defmacro-public name args body ...)
  (begin
    (defmacro name args body ...)
    (export-syntax name)))

;; And now for the most important macro.
(define-syntax-rule (λ formals body ...)
  (lambda formals body ...))


;; This function is called from "modules.c".  If you change it, be
;; sure to update "modules.c" as well.

(define (module-export! m names)
  "Export a local variable."
  (let ((public-i (module-public-interface m)))
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-ensure-local-variable! m internal-name)))
                  (module-add! public-i external-name var)))
              names)))

(define (module-replace! m names)
  (let ((public-i (module-public-interface m)))
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-ensure-local-variable! m internal-name)))
                  ;; FIXME: use a bit on variables instead of object
                  ;; properties.
                  (set-object-property! var 'replace #t)
                  (module-add! public-i external-name var)))
              names)))

(define (module-export-all! mod)
  "Export all local variables from a module."
  (define (fresh-interface!)
    (let ((iface (make-module)))
      (set-module-name! iface (module-name mod))
      (set-module-version! iface (module-version mod))
      (set-module-kind! iface 'interface)
      (set-module-public-interface! mod iface)
      iface))
  (let ((iface (or (module-public-interface mod)
                   (fresh-interface!))))
    (set-module-obarray! iface (module-obarray mod))))

(define (module-re-export! m names)
  "Re-export an imported variable."
  (let ((public-i (module-public-interface m)))
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-variable m internal-name)))
                  (cond ((not var)
                         (error "Undefined variable:" internal-name))
                        ((eq? var (module-local-variable m internal-name))
                         (error "re-exporting local variable:" internal-name))
                        (else
                         (module-add! public-i external-name var)))))
              names)))

(define-syntax-rule (export name ...)
  (eval-when (expand load eval)
    (call-with-deferred-observers
     (lambda ()
       (module-export! (current-module) '(name ...))))))

(define-syntax-rule (re-export name ...)
  (eval-when (expand load eval)
    (call-with-deferred-observers
     (lambda ()
       (module-re-export! (current-module) '(name ...))))))

(define-syntax-rule (export! name ...)
  (eval-when (expand load eval)
    (call-with-deferred-observers
     (lambda ()
       (module-replace! (current-module) '(name ...))))))

(define-syntax-rule (export-syntax name ...)
  (export name ...))

(define-syntax-rule (re-export-syntax name ...)
  (re-export name ...))



;;; {Parameters}
;;;

(define* (make-mutable-parameter init #:optional (converter identity))
  (let ((fluid (make-fluid (converter init))))
    (case-lambda
      (() (fluid-ref fluid))
      ((val) (fluid-set! fluid (converter val))))))




;;; {Handling of duplicate imported bindings}
;;;

;; Duplicate handlers take the following arguments:
;;
;; module  importing module
;; name    conflicting name
;; int1    old interface where name occurs
;; val1    value of binding in old interface
;; int2    new interface where name occurs
;; val2    value of binding in new interface
;; var     previous resolution or #f
;; val     value of previous resolution
;;
;; A duplicate handler can take three alternative actions:
;;
;; 1. return #f => leave responsibility to next handler
;; 2. exit with an error
;; 3. return a variable resolving the conflict
;;

(define duplicate-handlers
  (let ((m (make-module 7)))
    
    (define (check module name int1 val1 int2 val2 var val)
      (scm-error 'misc-error
                 #f
                 "~A: `~A' imported from both ~A and ~A"
                 (list (module-name module)
                       name
                       (module-name int1)
                       (module-name int2))
                 #f))
    
    (define (warn module name int1 val1 int2 val2 var val)
      (format (current-warning-port)
              "WARNING: ~A: `~A' imported from both ~A and ~A\n"
              (module-name module)
              name
              (module-name int1)
              (module-name int2))
      #f)
     
    (define (replace module name int1 val1 int2 val2 var val)
      (let ((old (or (and var (object-property var 'replace) var)
                     (module-variable int1 name)))
            (new (module-variable int2 name)))
        (if (object-property old 'replace)
            (and (or (eq? old new)
                     (not (object-property new 'replace)))
                 old)
            (and (object-property new 'replace)
                 new))))
    
    (define (warn-override-core module name int1 val1 int2 val2 var val)
      (and (eq? int1 the-scm-module)
           (begin
             (format (current-warning-port)
                     "WARNING: ~A: imported module ~A overrides core binding `~A'\n"
                     (module-name module)
                     (module-name int2)
                     name)
             (module-local-variable int2 name))))
     
    (define (first module name int1 val1 int2 val2 var val)
      (or var (module-local-variable int1 name)))
     
    (define (last module name int1 val1 int2 val2 var val)
      (module-local-variable int2 name))
     
    (define (noop module name int1 val1 int2 val2 var val)
      #f)
    
    (set-module-name! m 'duplicate-handlers)
    (set-module-kind! m 'interface)
    (module-define! m 'check check)
    (module-define! m 'warn warn)
    (module-define! m 'replace replace)
    (module-define! m 'warn-override-core warn-override-core)
    (module-define! m 'first first)
    (module-define! m 'last last)
    (module-define! m 'merge-generics noop)
    (module-define! m 'merge-accessors noop)
    m))

(define (lookup-duplicates-handlers handler-names)
  (and handler-names
       (map (lambda (handler-name)
              (or (module-symbol-local-binding
                   duplicate-handlers handler-name #f)
                  (error "invalid duplicate handler name:"
                         handler-name)))
            (if (list? handler-names)
                handler-names
                (list handler-names)))))

(define default-duplicate-binding-procedures
  (case-lambda
    (()
     (or (module-duplicates-handlers (current-module))
         ;; Note: If you change this default, change it also in
         ;; `define-module*'.
         (lookup-duplicates-handlers
          '(replace warn-override-core warn last))))
    ((procs)
     (set-module-duplicates-handlers! (current-module) procs))))

(define default-duplicate-binding-handler
  (case-lambda
    (()
     (map procedure-name (default-duplicate-binding-procedures)))
    ((handlers)
     (default-duplicate-binding-procedures
       (lookup-duplicates-handlers handlers)))))



;;; {`load'.}
;;;
;;; Load is tricky when combined with relative file names, compilation,
;;; and the file system.  If a file name is relative, what is it
;;; relative to?  The name of the source file at the time it was
;;; compiled?  The name of the compiled file?  What if both or either
;;; were installed?  And how do you get that information?  Tricky, I
;;; say.
;;;
;;; To get around all of this, we're going to do something nasty, and
;;; turn `load' into a macro.  That way it can know the name of the
;;; source file with respect to which it was invoked, so it can resolve
;;; relative file names with respect to the original source file.
;;;
;;; There is an exception, and that is that if the source file was in
;;; the load path when it was compiled, instead of looking up against
;;; the absolute source location, we load-from-path against the relative
;;; source location.
;;;

(define %auto-compilation-options
  ;; Default `compile-file' option when auto-compiling.
  '(#:warnings (unbound-variable macro-use-before-definition arity-mismatch
                format duplicate-case-datum bad-case-datum)))

(define* (load-in-vicinity dir file-name #:optional reader)
  "Load source file FILE-NAME in vicinity of directory DIR.  Use a
pre-compiled version of FILE-NAME when available, and auto-compile one
when none is available, reading FILE-NAME with READER."

  ;; The auto-compilation code will residualize a .go file in the cache
  ;; dir: by default, $HOME/.cache/guile/2.0/ccache/PATH.go.  This
  ;; function determines the PATH to use as a key into the compilation
  ;; cache.
  (define (canonical->suffix canon)
    (cond
     ((and (not (string-null? canon))
           (file-name-separator? (string-ref canon 0)))
      canon)
     ((and (eq? (system-file-name-convention) 'windows)
           (absolute-file-name? canon))
      ;; An absolute file name that doesn't start with a separator
      ;; starts with a drive component.  Transform the drive component
      ;; to a file name element:  c:\foo -> \c\foo.
      (string-append file-name-separator-string
                     (substring canon 0 1)
                     (substring canon 2)))
     (else canon)))

  (define compiled-extension
    ;; File name extension of compiled files.
    (cond ((or (null? %load-compiled-extensions)
               (string-null? (car %load-compiled-extensions)))
           (warn "invalid %load-compiled-extensions"
                 %load-compiled-extensions)
           ".go")
          (else (car %load-compiled-extensions))))

  (define (more-recent? stat1 stat2)
    ;; Return #t when STAT1 has an mtime greater than that of STAT2.
    (or (> (stat:mtime stat1) (stat:mtime stat2))
        (and (= (stat:mtime stat1) (stat:mtime stat2))
             (>= (stat:mtimensec stat1)
                 (stat:mtimensec stat2)))))

  (define (fallback-file-name canon-file-name)
    ;; Return the in-cache compiled file name for source file
    ;; CANON-FILE-NAME.

    ;; FIXME: would probably be better just to append
    ;; SHA1(canon-file-name) to the %compile-fallback-path, to avoid
    ;; deep directory stats.
    (and %compile-fallback-path
         (string-append %compile-fallback-path
                        (canonical->suffix canon-file-name)
                        compiled-extension)))

  (define (compile file)
    ;; Compile source FILE, lazily loading the compiler.
    ((module-ref (resolve-interface '(system base compile))
                 'compile-file)
     file
     #:opts %auto-compilation-options
     #:env (current-module)))

  (define (load-thunk-from-file file)
    (let ((loader (resolve-interface '(system vm loader))))
      ((module-ref loader 'load-thunk-from-file) file)))

  ;; Returns a thunk loaded from the .go file corresponding to `name'.
  ;; Does not search load paths, only the fallback path.  If the .go
  ;; file is missing or out of date, and auto-compilation is enabled,
  ;; will try auto-compilation, just as primitive-load-path does
  ;; internally.  primitive-load is unaffected.  Returns #f if
  ;; auto-compilation failed or was disabled.
  ;;
  ;; NB: Unless we need to compile the file, this function should not
  ;; cause (system base compile) to be loaded up.  For that reason
  ;; compiled-file-name partially duplicates functionality from (system
  ;; base compile).

  (define (fresh-compiled-thunk name scmstat go-file-name)
    ;; Return GO-FILE-NAME after making sure that it contains a freshly
    ;; compiled version of source file NAME with stat SCMSTAT; return #f
    ;; on failure.
    (false-if-exception
     (let ((gostat (and (not %fresh-auto-compile)
                        (stat go-file-name #f))))
       (if (and gostat (more-recent? gostat scmstat))
           (load-thunk-from-file go-file-name)
           (begin
             (when gostat
               (format (current-warning-port)
                       ";;; note: source file ~a\n;;;       newer than compiled ~a\n"
                       name go-file-name))
             (cond
              (%load-should-auto-compile
               (%warn-auto-compilation-enabled)
               (format (current-warning-port) ";;; compiling ~a\n" name)
               (let ((cfn (compile name)))
                 (format (current-warning-port) ";;; compiled ~a\n" cfn)
                 (load-thunk-from-file cfn)))
              (else #f)))))
     #:warning "WARNING: compilation of ~a failed:\n" name))

  (define (sans-extension file)
    (let ((dot (string-rindex file #\.)))
      (if dot
          (substring file 0 dot)
          file)))

  (define (load-absolute abs-file-name)
    ;; Load from ABS-FILE-NAME, using a compiled file or auto-compiling
    ;; if needed.
    (define scmstat
      (false-if-exception
       (stat abs-file-name)
       #:warning "Stat of ~a failed:\n" abs-file-name))

    (define (pre-compiled)
      (or-map
       (lambda (dir)
         (or-map
          (lambda (ext)
            (let ((candidate (string-append (in-vicinity dir file-name) ext)))
              (let ((gostat (stat candidate #f)))
                (and gostat
                     (more-recent? gostat scmstat)
                     (false-if-exception
                      (load-thunk-from-file candidate)
                      #:warning "WARNING: failed to load compiled file ~a:\n"
                      candidate)))))
          %load-compiled-extensions))
       %load-compiled-path))

    (define (fallback)
      (and=> (false-if-exception (canonicalize-path abs-file-name))
             (lambda (canon)
               (and=> (fallback-file-name canon)
                      (lambda (go-file-name)
                        (fresh-compiled-thunk abs-file-name
                                              scmstat
                                              go-file-name))))))

    (let ((compiled (and scmstat
                         (or (and (not %fresh-auto-compile)
                                  (pre-compiled))
                             (fallback)))))
      (if compiled
          (begin
            (if %load-hook
                (%load-hook abs-file-name))
            (compiled))
          (start-stack 'load-stack
                       (primitive-load abs-file-name)))))

  (save-module-excursion
   (lambda ()
     (with-fluids ((current-reader reader)
                   (%file-port-name-canonicalization 'relative))
       (cond
        ((absolute-file-name? file-name)
         (load-absolute file-name))
        ((absolute-file-name? dir)
         (load-absolute (in-vicinity dir file-name)))
        (else
         (load-from-path (in-vicinity dir file-name))))))))

(define-syntax load
  (make-variable-transformer
   (lambda (x)
     (let* ((src (syntax-source x))
            (file (and src (assq-ref src 'filename)))
            (dir (and (string? file) (dirname file))))
       (syntax-case x ()
         ((_ arg ...)
          #`(load-in-vicinity #,(or dir #'(getcwd)) arg ...))
         (id
          (identifier? #'id)
          #`(lambda args
              (apply load-in-vicinity #,(or dir #'(getcwd)) args))))))))



;;; {`cond-expand' for SRFI-0 support.}
;;;
;;; This syntactic form expands into different commands or
;;; definitions, depending on the features provided by the Scheme
;;; implementation.
;;;
;;; Syntax:
;;;
;;; <cond-expand>
;;;   --> (cond-expand <cond-expand-clause>+)
;;;     | (cond-expand <cond-expand-clause>* (else <command-or-definition>))
;;; <cond-expand-clause>
;;;   --> (<feature-requirement> <command-or-definition>*)
;;; <feature-requirement>
;;;   --> <feature-identifier>
;;;     | (and <feature-requirement>*)
;;;     | (or <feature-requirement>*)
;;;     | (not <feature-requirement>)
;;; <feature-identifier>
;;;   --> <a symbol which is the name or alias of a SRFI>
;;;
;;; Additionally, this implementation provides the
;;; <feature-identifier>s `guile' and `r5rs', so that programs can
;;; determine the implementation type and the supported standard.
;;;
;;; Remember to update the features list when adding more SRFIs.
;;;

(define %cond-expand-features
  ;; This should contain only features that are present in core Guile,
  ;; before loading any modules.  Modular features are handled by
  ;; placing 'cond-expand-provide' in the relevant module.
  '(guile
    guile-2
    guile-2.2
    r5rs
    srfi-0   ;; cond-expand itself
    srfi-4   ;; homogeneous numeric vectors
    srfi-6   ;; string ports
    srfi-13  ;; string library
    srfi-14  ;; character sets
    srfi-16  ;; case-lambda
    srfi-23  ;; `error` procedure
    srfi-30  ;; nested multi-line comments
    srfi-39  ;; parameterize
    srfi-46  ;; basic syntax-rules extensions
    srfi-55  ;; require-extension
    srfi-61  ;; general cond clause
    srfi-62  ;; s-expression comments
    srfi-87  ;; => in case clauses
    srfi-105 ;; curly infix expressions
    ))

;; This table maps module public interfaces to the list of features.
;;
(define %cond-expand-table (make-hash-table 31))

;; Add one or more features to the `cond-expand' feature list of the
;; module `module'.
;;
(define (cond-expand-provide module features)
  (let ((mod (module-public-interface module)))
    (and mod
         (hashq-set! %cond-expand-table mod
                     (append (hashq-ref %cond-expand-table mod '())
                             features)))))

(define-syntax cond-expand
  (lambda (x)
    (define (module-has-feature? mod sym)
      (or-map (lambda (mod)
                (memq sym (hashq-ref %cond-expand-table mod '())))
              (module-uses mod)))

    (define (condition-matches? condition)
      (syntax-case condition (and or not)
        ((and c ...)
         (and-map condition-matches? #'(c ...)))
        ((or c ...)
         (or-map condition-matches? #'(c ...)))
        ((not c)
         (if (condition-matches? #'c) #f #t))
        (c
         (identifier? #'c)
         (let ((sym (syntax->datum #'c)))
           (if (memq sym %cond-expand-features)
               #t
               (module-has-feature? (current-module) sym))))))

    (define (match clauses alternate)
      (syntax-case clauses ()
        (((condition form ...) . rest)
         (if (condition-matches? #'condition)
             #'(begin form ...)
             (match #'rest alternate)))
        (() (alternate))))

    (syntax-case x (else)
      ((_ clause ... (else form ...))
       (match #'(clause ...)
         (lambda ()
           #'(begin form ...))))
      ((_ clause ...)
       (match #'(clause ...)
         (lambda ()
           (syntax-violation 'cond-expand "unfulfilled cond-expand" x)))))))

;; This procedure gets called from the startup code with a list of
;; numbers, which are the numbers of the SRFIs to be loaded on startup.
;;
(define (use-srfis srfis)
  (process-use-modules
   (map (lambda (num)
          (list (list 'srfi (string->symbol
                             (string-append "srfi-" (number->string num))))))
        srfis)))



;;; srfi-55: require-extension
;;;

(define-syntax require-extension
  (lambda (x)
    (syntax-case x (srfi)
      ((_ (srfi n ...))
       (and-map integer? (syntax->datum #'(n ...)))
       (with-syntax
           (((srfi-n ...)
             (map (lambda (n)
                    (datum->syntax x (symbol-append 'srfi- n)))
                  (map string->symbol
                       (map number->string (syntax->datum #'(n ...)))))))
         #'(use-modules (srfi srfi-n) ...)))
      ((_ (type arg ...))
       (identifier? #'type)
       (syntax-violation 'require-extension "Not a recognized extension type"
                         x)))))


;;; Defining transparently inlinable procedures
;;;

(define-syntax define-inlinable
  ;; Define a macro and a procedure such that direct calls are inlined, via
  ;; the macro expansion, whereas references in non-call contexts refer to
  ;; the procedure.  Inspired by the `define-integrable' macro by Dybvig et al.
  (lambda (x)
    ;; Use a space in the prefix to avoid potential -Wunused-toplevel
    ;; warning
    (define prefix (string->symbol "% "))
    (define (make-procedure-name name)
      (datum->syntax name
                     (symbol-append prefix (syntax->datum name)
                                    '-procedure)))

    (syntax-case x ()
      ((_ (name formals ...) body ...)
       (identifier? #'name)
       (with-syntax ((proc-name  (make-procedure-name #'name))
                     ((args ...) (generate-temporaries #'(formals ...))))
         #`(begin
             (define (proc-name formals ...)
               (syntax-parameterize ((name (identifier-syntax proc-name)))
                 body ...))
             (define-syntax-parameter name
               (lambda (x)
                 (syntax-case x ()
                   ((_ args ...)
                    #'((syntax-parameterize ((name (identifier-syntax proc-name)))
                         (lambda (formals ...)
                           body ...))
                       args ...))
                   ((_ a (... ...))
                    (syntax-violation 'name "Wrong number of arguments" x))
                   (_
                    (identifier? x)
                    #'proc-name))))))))))



(define using-readline?
  (let ((using-readline? (make-fluid)))
     (make-procedure-with-setter
      (lambda () (fluid-ref using-readline?))
      (lambda (v) (fluid-set! using-readline? v)))))



;;; {Deprecated stuff}
;;;

(begin-deprecated
 (module-use! the-scm-module (resolve-interface '(ice-9 deprecated))))



;;; {Ports}
;;;

;; Allow code in (guile) to use port bindings.
(module-use! the-root-module (resolve-interface '(ice-9 ports)))
;; Allow users of (guile) to see port bindings.
(module-use! the-scm-module (resolve-interface '(ice-9 ports)))



;;; {Threads}
;;;

;; Load (ice-9 threads), initializing some internal data structures.
(resolve-interface '(ice-9 threads))



;;; SRFI-4 in the default environment.  FIXME: we should figure out how
;;; to deprecate this.
;;;

;; FIXME:
(module-use! the-scm-module (resolve-interface '(srfi srfi-4)))



;;; A few identifiers that need to be defined in this file are really
;;; internal implementation details.  We shove them off into internal
;;; modules, removing them from the (guile) module.
;;;

(define-module (system syntax internal))

(let ()
  (define (steal-bindings! from to ids)
    (for-each
     (lambda (sym)
       (let ((v (module-local-variable from sym)))
         (module-remove! from sym)
         (module-add! to sym v)))
     ids)
    (module-export! to ids))

  (steal-bindings! the-root-module (resolve-module '(system syntax internal))
                   '(syntax?
                     syntax-local-binding
                     %syntax-module
                     syntax-locally-bound-identifiers
                     syntax-session-id
                     make-syntax
                     syntax-expression
                     syntax-wrap
                     syntax-module)))




;;; Place the user in the guile-user module.
;;;

;; Set filename to #f to prevent reload.
(define-module (guile-user)
  #:autoload (system base compile) (compile compile-file)
  #:filename #f)

;; Remain in the `(guile)' module at compilation-time so that the
;; `-Wunused-toplevel' warning works as expected.
(eval-when (compile) (set-current-module the-root-module))

;;; boot-9.scm ends here
