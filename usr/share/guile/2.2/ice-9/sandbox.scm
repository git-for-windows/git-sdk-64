;;; Sandboxed evaluation of Scheme code

;;; Copyright (C) 2017 Free Software Foundation, Inc.

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

;;; Commentary:
;;; 
;;; Code:

(define-module (ice-9 sandbox)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 threads) #:select (current-thread))
  #:use-module (system vm vm)
  #:export (call-with-time-limit
            call-with-allocation-limit
            call-with-time-and-allocation-limits

            eval-in-sandbox
            make-sandbox-module

            alist-bindings
            array-bindings
            bit-bindings
            bitvector-bindings
            char-bindings
            char-set-bindings
            clock-bindings
            core-bindings
            error-bindings
            fluid-bindings
            hash-bindings
            iteration-bindings
            keyword-bindings
            list-bindings
            macro-bindings
            nil-bindings
            number-bindings
            pair-bindings
            predicate-bindings
            procedure-bindings
            promise-bindings
            prompt-bindings
            regexp-bindings
            sort-bindings
            srfi-4-bindings
            string-bindings
            symbol-bindings
            unspecified-bindings
            variable-bindings
            vector-bindings
            version-bindings

            mutating-alist-bindings
            mutating-array-bindings
            mutating-bitvector-bindings
            mutating-fluid-bindings
            mutating-hash-bindings
            mutating-list-bindings
            mutating-pair-bindings
            mutating-sort-bindings
            mutating-srfi-4-bindings
            mutating-string-bindings
            mutating-variable-bindings
            mutating-vector-bindings

            all-pure-bindings
            all-pure-and-impure-bindings))


(define (call-with-time-limit limit thunk limit-reached)
  "Call @var{thunk}, but cancel it if @var{limit} seconds of wall-clock
time have elapsed.  If the computation is cancelled, call
@var{limit-reached} in tail position.  @var{thunk} must not disable
interrupts or prevent an abort via a @code{dynamic-wind} unwind
handler."
  ;; FIXME: use separate thread instead of sigalrm.  If rounded limit is
  ;; <= 0, make it 1 usec to signal immediately.
  (let ((limit-usecs (max (inexact->exact (round (* limit 1e6))) 1))
        (prev-sigalarm-handler #f)
        (tag (make-prompt-tag)))
    (call-with-prompt tag
      (lambda ()
        (dynamic-wind
          (lambda ()
            (set! prev-sigalarm-handler
              (sigaction SIGALRM (lambda (sig)
                                   ;; If signal handling is delayed
                                   ;; until after prompt, no worries;
                                   ;; the success path won the race.
                                   (false-if-exception
                                    (abort-to-prompt tag)))))
            (setitimer ITIMER_REAL 0 0 0 limit-usecs))
          thunk
          (lambda ()
            (setitimer ITIMER_REAL 0 0 0 0)
            (match prev-sigalarm-handler
              ((handler . flags)
               (sigaction SIGALRM handler flags))))))
      (lambda (k)
        (limit-reached)))))

(define (call-with-allocation-limit limit thunk limit-reached)
  "Call @var{thunk}, but cancel it if @var{limit} bytes have been
allocated.  If the computation is cancelled, call @var{limit-reached} in
tail position.  @var{thunk} must not disable interrupts or prevent an
abort via a @code{dynamic-wind} unwind handler.

This limit applies to both stack and heap allocation.  The computation
will not be aborted before @var{limit} bytes have been allocated, but
for the heap allocation limit, the check may be postponed until the next
garbage collection.

Note that as a current shortcoming, the heap size limit applies to all
threads; concurrent allocation by other unrelated threads counts towards
the allocation limit."
  (define (bytes-allocated) (assq-ref (gc-stats) 'heap-total-allocated))
  (let ((zero (bytes-allocated))
        (tag (make-prompt-tag))
        (thread (current-thread)))
    (define (check-allocation)
      (when (< limit (- (bytes-allocated) zero))
        (system-async-mark (lambda ()
                             (false-if-exception (abort-to-prompt tag)))
                           thread)))
    (call-with-prompt tag
      (lambda ()
        (dynamic-wind
          (lambda ()
            (add-hook! after-gc-hook check-allocation))
          (lambda ()
            (call-with-stack-overflow-handler
             ;; The limit is in "words", which used to be 4 or 8 but now
             ;; is always 8 bytes.
             (max (floor/ limit 8) 1)
             thunk
             (lambda () (abort-to-prompt tag))))
          (lambda ()
            (remove-hook! after-gc-hook check-allocation))))
      (lambda (k)
        (limit-reached)))))

(define (call-with-time-and-allocation-limits time-limit allocation-limit
                                              thunk)
  "Invoke @var{thunk} in a dynamic extent in which its execution is
limited to @var{time-limit} seconds of wall-clock time, and its
allocation to @var{allocation-limit} bytes.  @var{thunk} must not
disable interrupts or prevent an abort via a @code{dynamic-wind} unwind
handler.

If successful, return all values produced by invoking @var{thunk}.  Any
uncaught exception thrown by the thunk will propagate out.  If the time
or allocation limit is exceeded, an exception will be thrown to the
@code{limit-exceeded} key."
  (call-with-time-limit
   time-limit
   (lambda ()
     (call-with-allocation-limit
      allocation-limit
      thunk
      (lambda ()
        (scm-error 'limit-exceeded "with-resource-limits"
                   "Allocation limit exceeded" '() #f))))
   (lambda ()
     (scm-error 'limit-exceeded "with-resource-limits"
                "Time limit exceeded" '() #f))))

(define (sever-module! m)
  "Remove @var{m} from its container module."
  (match (module-name m)
    ((head ... tail)
     (let ((parent (resolve-module head #f)))
       (unless (eq? m (module-ref-submodule parent tail))
         (error "can't sever module?"))
       (hashq-remove! (module-submodules parent) tail)))))

;; bindings := module-binding-list ...
;; module-binding-list := interface-name import ...
;; import := name | (exported-name . imported-name)
;; name := symbol
(define (make-sandbox-module bindings)
  "Return a fresh module that only contains @var{bindings}.

The @var{bindings} should be given as a list of import sets.  One import
set is a list whose car names an interface, like @code{(ice-9 q)}, and
whose cdr is a list of imports.  An import is either a bare symbol or a
pair of @code{(@var{out} . @var{in})}, where @var{out} and @var{in} are
both symbols and denote the name under which a binding is exported from
the module, and the name under which to make the binding available,
respectively."
  (let ((m (make-fresh-user-module)))
    (purify-module! m)
    (module-use-interfaces! m
                            (map (match-lambda
                                   ((mod-name . bindings)
                                    (resolve-interface mod-name
                                                       #:select bindings)))
                                 bindings))
    m))

(define* (eval-in-sandbox exp #:key
                          (time-limit 0.1)
                          (allocation-limit #e10e6)
                          (bindings all-pure-bindings)
                          (module (make-sandbox-module bindings))
                          (sever-module? #t))
  "Evaluate the Scheme expression @var{exp} within an isolated
\"sandbox\".  Limit its execution to @var{time-limit} seconds of
wall-clock time, and limit its allocation to @var{allocation-limit}
bytes.

The evaluation will occur in @var{module}, which defaults to the result
of calling @code{make-sandbox-module} on @var{bindings}, which itself
defaults to @code{all-pure-bindings}.  This is the core of the
sandbox: creating a scope for the expression that is @dfn{safe}.

A safe sandbox module has two characteristics.  Firstly, it will not
allow the expression being evaluated to avoid being cancelled due to
time or allocation limits.  This ensures that the expression terminates
in a timely fashion.

Secondly, a safe sandbox module will prevent the evaluation from
receiving information from previous evaluations, or from affecting
future evaluations.  All combinations of binding sets exported by
@code{(ice-9 sandbox)} form safe sandbox modules.

The @var{bindings} should be given as a list of import sets.  One import
set is a list whose car names an interface, like @code{(ice-9 q)}, and
whose cdr is a list of imports.  An import is either a bare symbol or a
pair of @code{(@var{out} . @var{in})}, where @var{out} and @var{in} are
both symbols and denote the name under which a binding is exported from
the module, and the name under which to make the binding available,
respectively.  Note that @var{bindings} is only used as an input to the
default initializer for the @var{module} argument; if you pass
@code{#:module}, @var{bindings} is unused.  If @var{sever-module?} is
true (the default), the module will be unlinked from the global module
tree after the evaluation returns, to allow @var{mod} to be
garbage-collected.

If successful, return all values produced by @var{exp}.  Any uncaught
exception thrown by the expression will propagate out.  If the time or
allocation limit is exceeded, an exception will be thrown to the
@code{limit-exceeded} key."
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (call-with-time-and-allocation-limits
       time-limit allocation-limit
       (lambda ()
         ;; Prevent the expression from forging syntax objects.  See "Syntax
         ;; Transformer Helpers" in the manual.
         (parameterize ((allow-legacy-syntax-objects? #f))
           (eval exp module)))))
    (lambda () (when sever-module? (sever-module! module)))))


;; An evaluation-sandboxing facility is safe if:
;;
;;  (1) every evaluation will terminate in a timely manner
;;
;;  (2) no evaluation can affect future evaluations
;;
;; For (1), we impose a user-controllable time limit on the evaluation,
;; in wall-clock time.  When that limit is reached, Guile schedules an
;; asynchronous interrupt in the sandbox that aborts the computation.
;; For this to work, the sandboxed evaluation must not disable
;; interrupts, and it must not prevent timely aborts via malicious "out"
;; guards in dynamic-wind thunks.
;;
;; The sandbox also has an allocation limit that uses a similar cancel
;; mechanism, but this limit is less precise as it only runs at
;; garbage-collection time.
;;
;; The sandbox sets the allocation limit as the stack limit as well.
;;
;; For (2), the only way an evaluation can affect future evaluations is
;; if it causes a side-effect outside its sandbox.  That side effect
;; could change the way the host or future sandboxed evaluations
;; operate, or it could leak information to future evaluations.
;;
;; One means of information leakage would be the file system.  Although
;; one can imagine "safe" ways to access a file system, in practice we
;; just prevent all access to this and other operating system facilities
;; by not exposing the Guile primitives that access the file system,
;; connect to networking hosts, etc.  If we chose our set of bindings
;; correctly and it is impossible to access host values other than those
;; given to the evaluation, then we have succeeded in granting only a
;; limited set of capabilities to the guest.
;;
;; To prevent information leakage we also limit other information about
;; the host, like its hostname or the Guile build information.
;;
;; The guest must also not have the capability to mutate a location used
;; by the host or by future sandboxed evaluations.  Either you expose no
;; primitives to the evaluation that can mutate locations, or you expose
;; no mutable locations.  In this sandbox we opt for a combination of
;; the two, though the selection of bindings is up to the user.  "set!"
;; is always excluded, as Guile doesn't have a nice way to prevent set!
;; on imported bindings.  But variable-set! is included, as no set of
;; bindings from this module includes a variable or a capability to a
;; variable.  It's possible though to build sandbox modules with no
;; mutating primitives.  As far as we know, all possible combinations of
;; the binding sets listed below are safe.
;;
(define core-bindings
  '(((guile)
     and
     begin
     apply
     call-with-values
     values
     case
     case-lambda
     case-lambda*
     cond
     define
     define*
     define-values
     do
     if
     lambda
     lambda*
     let
     let*
     letrec
     letrec*
     or
     quasiquote
     quote
     ;; Can't allow mutation to globals.
     ;; set!
     unless
     unquote
     unquote-splicing
     when
     while
     λ)))

(define macro-bindings
  '(((guile)
     bound-identifier=?
     ;; Although these have "current" in their name, they are lexically
     ;; scoped, not dynamically scoped.
     current-filename
     current-source-location
     datum->syntax
     define-macro
     define-syntax
     define-syntax-parameter
     define-syntax-rule
     defmacro
     free-identifier=?
     generate-temporaries
     gensym
     identifier-syntax
     identifier?
     let-syntax
     letrec-syntax
     macroexpand
     macroexpanded?
     quasisyntax
     start-stack
     syntax
     syntax->datum
     syntax-case
     syntax-error
     syntax-parameterize
     syntax-rules
     syntax-source
     syntax-violation
     unsyntax
     unsyntax-splicing
     with-ellipsis
     with-syntax
     make-variable-transformer)))

(define iteration-bindings
  '(((guile)
     compose
     for-each
     identity
     iota
     map
     map-in-order
     const
     noop)))

(define clock-bindings
  '(((guile)
     get-internal-real-time
     internal-time-units-per-second
     sleep
     usleep)))

(define procedure-bindings
  '(((guile)
     procedure-documentation
     procedure-minimum-arity
     procedure-name
     procedure?
     thunk?)))

(define version-bindings
  '(((guile)
     effective-version
     major-version
     micro-version
     minor-version
     version
     version-matches?)))

(define nil-bindings
  '(((guile)
     nil?)))

(define unspecified-bindings
  '(((guile)
     unspecified?
     *unspecified*)))

(define predicate-bindings
  '(((guile)
     ->bool
     and-map
     and=>
     boolean?
     eq?
     equal?
     eqv?
     negate
     not
     or-map)))

;; The current ports (current-input-port et al) are dynamically scoped,
;; which is a footgun from a sandboxing perspective.  It's too easy for
;; a procedure that is the result of a sandboxed evaluation to be later
;; invoked in a different context and thereby be implicitly granted
;; capabilities to whatever port is then current.  This is compounded by
;; the fact that most Scheme i/o primitives allow the port to be omitted
;; and thereby default to whatever's current.  For now, sadly, we avoid
;; exposing any i/o primitive to the sandbox.
#;
(define i/o-bindings
  '(((guile)
     display
     eof-object?
     force-output
     format
     make-soft-port
     newline
     read
     simple-format
     write
     write-char)
    ((ice-9 ports)
     %make-void-port
     char-ready?
     ;; Note that these are mutable parameters.
     current-error-port
     current-input-port
     current-output-port
     current-warning-port
     drain-input
     eof-object?
     file-position
     force-output
     ftell
     input-port?
     output-port?
     peek-char
     port-closed?
     port-column
     port-conversion-strategy
     port-encoding
     port-filename
     port-line
     port-mode
     port?
     read-char
     the-eof-object
     ;; We don't provide open-output-string because it needs
     ;; get-output-string, and get-output-string provides a generic
     ;; capability on any output string port.  For consistency then we
     ;; don't provide open-input-string either; call-with-input-string
     ;; is sufficient.
     call-with-input-string
     call-with-output-string
     with-error-to-port
     with-error-to-string
     with-input-from-port
     with-input-from-string
     with-output-to-port
     with-output-to-string)))

;; If two evaluations are called with the same input port, unread-char
;; and unread-string can use a port as a mutable channel to pass
;; information from one to the other.
#;
(define mutating-i/o-bindings
  '(((guile)
     set-port-encoding!)
    ((ice-9 ports)
     close-input-port
     close-output-port
     close-port
     file-set-position
     seek
     set-port-column!
     set-port-conversion-strategy!
     set-port-encoding!
     set-port-filename!
     set-port-line!
     setvbuf
     unread-char
     unread-string)))

(define error-bindings
  '(((guile)
     error
     throw
     with-throw-handler
     catch
     ;; false-if-exception can cause i/o if the #:warning arg is passed.
     ;; false-if-exception

     ;; See notes on i/o-bindings.
     ;; peek
     ;; pk
     ;; print-exception
     ;; warn
     strerror
     scm-error
     )))

;; FIXME: Currently we can't expose anything that works on the current
;; module to the sandbox.  It could be that the sandboxed evaluation
;; returns a procedure, and that procedure may later be invoked in a
;; different context with a different current-module and it is unlikely
;; that the later caller will consider themselves as granting a
;; capability on whatever module is then current.  Likewise export (and
;; by extension, define-public and the like) also operate on the current
;; module.
;;
;; It could be that we could expose a statically scoped eval to the
;; sandbox.
#;
(define eval-bindings
  '(((guile)
     current-module
     module-name
     module?
     define-once
     define-private
     define-public
     defined?
     export
     defmacro-public
     ;; FIXME: single-arg eval?
     eval
     primitive-eval
     eval-string
     self-evaluating?
     ;; Can we?
     set-current-module)))

(define sort-bindings
  '(((guile)
     sort
     sorted?
     stable-sort
     sort-list)))

;; These can only form part of a safe binding set if no mutable pair or
;; vector is exposed to the sandbox.
(define mutating-sort-bindings
  '(((guile)
     sort!
     stable-sort!
     sort-list!
     restricted-vector-sort!)))

(define regexp-bindings
  '(((guile)
     make-regexp
     regexp-exec
     regexp/basic
     regexp/extended
     regexp/icase
     regexp/newline
     regexp/notbol
     regexp/noteol
     regexp?)))

(define alist-bindings
  '(((guile)
     acons
     assoc
     assoc-ref
     assq
     assq-ref
     assv
     assv-ref
     sloppy-assoc
     sloppy-assq
     sloppy-assv)))

;; These can only form part of a safe binding set if no mutable pair
;; is exposed to the sandbox.  Unfortunately all charsets in Guile are
;; mutable, currently, including the built-in charsets, so we can't
;; expose these primitives.
(define mutating-alist-bindings
  '(((guile)
     assoc-remove!
     assoc-set!
     assq-remove!
     assq-set!
     assv-remove!
     assv-set!)))

(define number-bindings
  '(((guile)
     *
     +
     -
     /
     1+
     1-
     <
     <=
     =
     >
     >=
     abs
     acos
     acosh
     angle
     asin
     asinh
     atan
     atanh
     ceiling
     ceiling-quotient
     ceiling-remainder
     ceiling/
     centered-quotient
     centered-remainder
     centered/
     complex?
     cos
     cosh
     denominator
     euclidean-quotient
     euclidean-remainder
     euclidean/
     even?
     exact->inexact
     exact-integer-sqrt
     exact-integer?
     exact?
     exp
     expt
     finite?
     floor
     floor-quotient
     floor-remainder
     floor/
     gcd
     imag-part
     inf
     inf?
     integer-expt
     integer-length
     integer?
     lcm
     log
     log10
     magnitude
     make-polar
     make-rectangular
     max
     min
     modulo
     modulo-expt
     most-negative-fixnum
     most-positive-fixnum
     nan
     nan?
     negative?
     numerator
     odd?
     positive?
     quotient
     rational?
     rationalize
     real-part
     real?
     remainder
     round
     round-quotient
     round-remainder
     round/
     sin
     sinh
     sqrt
     tan
     tanh
     truncate
     truncate-quotient
     truncate-remainder
     truncate/
     zero?
     number?
     number->string
     string->number)))

(define char-set-bindings
  '(((guile)
     ->char-set
     char-set
     char-set->list
     char-set->string
     char-set-adjoin
     char-set-any
     char-set-complement
     char-set-contains?
     char-set-copy
     char-set-count
     char-set-cursor
     char-set-cursor-next
     char-set-delete
     char-set-diff+intersection
     char-set-difference
     char-set-every
     char-set-filter
     char-set-fold
     char-set-for-each
     char-set-hash
     char-set-intersection
     char-set-map
     char-set-ref
     char-set-size
     char-set-unfold
     char-set-union
     char-set-xor
     char-set:ascii
     char-set:blank
     char-set:designated
     char-set:digit
     char-set:empty
     char-set:full
     char-set:graphic
     char-set:hex-digit
     char-set:iso-control
     char-set:letter
     char-set:letter+digit
     char-set:lower-case
     char-set:printing
     char-set:punctuation
     char-set:symbol
     char-set:title-case
     char-set:upper-case
     char-set:whitespace
     char-set<=
     char-set=
     char-set?
     end-of-char-set?
     list->char-set
     string->char-set
     ucs-range->char-set)))

;; These can only form part of a safe binding set if no mutable char-set
;; is exposed to the sandbox.  Unfortunately all charsets in Guile are
;; mutable, currently, including the built-in charsets, so we can't
;; expose these primitives.
#;
(define mutating-char-set-bindings
  '(((guile)
     char-set-adjoin!
     char-set-complement!
     char-set-delete!
     char-set-diff+intersection!
     char-set-difference!
     char-set-filter!
     char-set-intersection!
     char-set-unfold!
     char-set-union!
     char-set-xor!
     list->char-set!
     string->char-set!
     ucs-range->char-set!)))

(define array-bindings
  '(((guile)
     array->list
     array-cell-ref
     array-contents
     array-dimensions
     array-equal?
     array-for-each
     array-in-bounds?
     array-length
     array-rank
     array-ref
     array-shape
     array-slice
     array-slice-for-each
     array-slice-for-each-in-order
     array-type
     array-type-code
     array?
     list->array
     list->typed-array
     make-array
     make-shared-array
     make-typed-array
     shared-array-increments
     shared-array-offset
     shared-array-root
     transpose-array
     typed-array?)))

;; These can only form part of a safe binding set if no mutable vector,
;; bitvector, bytevector, srfi-4 vector, or array is exposed to the
;; sandbox.
(define mutating-array-bindings
  '(((guile)
     array-cell-set!
     array-copy!
     array-copy-in-order!
     array-fill!
     array-index-map!
     array-map!
     array-map-in-order!
     array-set!)))

(define hash-bindings
  '(((guile)
     doubly-weak-hash-table?
     hash
     hash-count
     hash-fold
     hash-for-each
     hash-for-each-handle
     hash-get-handle
     hash-map->list
     hash-ref
     hash-table?
     hashq
     hashq-get-handle
     hashq-ref
     hashv
     hashv-get-handle
     hashv-ref
     hashx-get-handle
     hashx-ref
     make-doubly-weak-hash-table
     make-hash-table
     make-weak-key-hash-table
     make-weak-value-hash-table
     weak-key-hash-table?
     weak-value-hash-table?)))

;; These can only form part of a safe binding set if no hash table is
;; exposed to the sandbox.
(define mutating-hash-bindings
  '(((guile)
     hash-clear!
     hash-create-handle!
     hash-remove!
     hash-set!
     hashq-create-handle!
     hashq-remove!
     hashq-set!
     hashv-create-handle!
     hashv-remove!
     hashv-set!
     hashx-create-handle!
     hashx-remove!
     hashx-set!)))

(define variable-bindings
  '(((guile)
     make-undefined-variable
     make-variable
     variable-bound?
     variable-ref
     variable?)))

;; These can only form part of a safe binding set if no mutable variable
;; is exposed to the sandbox; this applies particularly to variables
;; that are module bindings.
(define mutating-variable-bindings
  '(((guile)
     variable-set!
     variable-unset!)))

(define string-bindings
  '(((guile)
     absolute-file-name?
     file-name-separator-string
     file-name-separator?
     in-vicinity
     basename
     dirname

     list->string
     make-string
     object->string
     reverse-list->string
     string
     string->list
     string-any
     string-any-c-code
     string-append
     string-append/shared
     string-capitalize
     string-ci<
     string-ci<=
     string-ci<=?
     string-ci<>
     string-ci<?
     string-ci=
     string-ci=?
     string-ci>
     string-ci>=
     string-ci>=?
     string-ci>?
     string-compare
     string-compare-ci
     string-concatenate
     string-concatenate-reverse
     string-concatenate-reverse/shared
     string-concatenate/shared
     string-contains
     string-contains-ci
     string-copy
     string-count
     string-delete
     string-downcase
     string-drop
     string-drop-right
     string-every
     string-every-c-code
     string-filter
     string-fold
     string-fold-right
     string-for-each
     string-for-each-index
     string-hash
     string-hash-ci
     string-index
     string-index-right
     string-join
     string-length
     string-map
     string-normalize-nfc
     string-normalize-nfd
     string-normalize-nfkc
     string-normalize-nfkd
     string-null?
     string-pad
     string-pad-right
     string-prefix-ci?
     string-prefix-length
     string-prefix-length-ci
     string-prefix?
     string-ref
     string-replace
     string-reverse
     string-rindex
     string-skip
     string-skip-right
     string-split
     string-suffix-ci?
     string-suffix-length
     string-suffix-length-ci
     string-suffix?
     string-tabulate
     string-take
     string-take-right
     string-titlecase
     string-tokenize
     string-trim
     string-trim-both
     string-trim-right
     string-unfold
     string-unfold-right
     string-upcase
     string-utf8-length
     string<
     string<=
     string<=?
     string<>
     string<?
     string=
     string=?
     string>
     string>=
     string>=?
     string>?
     string?
     substring
     substring/copy
     substring/read-only
     substring/shared
     xsubstring)))

;; These can only form part of a safe binding set if no mutable string
;; is exposed to the sandbox.
(define mutating-string-bindings
  '(((guile)
     string-capitalize!
     string-copy!
     string-downcase!
     string-fill!
     string-map!
     string-reverse!
     string-set!
     string-titlecase!
     string-upcase!
     string-xcopy!
     substring-fill!
     substring-move!)))

(define symbol-bindings
  '(((guile)
     string->symbol
     string-ci->symbol
     symbol->string
     list->symbol
     make-symbol
     symbol
     symbol-append
     symbol-hash
     symbol-interned?
     symbol?)))

(define keyword-bindings
  '(((guile)
     keyword?
     keyword->symbol
     symbol->keyword)))

;; These can only form part of a safe binding set if no valid prompt tag
;; is ever exposed to the sandbox, or can be constructed by the sandbox.
(define prompt-bindings
  '(((guile)
     abort-to-prompt
     abort-to-prompt*
     call-with-prompt
     make-prompt-tag)))

(define bit-bindings
  '(((guile)
     ash
     round-ash
     logand
     logcount
     logior
     lognot
     logtest
     logxor
     logbit?)))

(define bitvector-bindings
  '(((guile)
     bit-count
     bit-count*
     bit-extract
     bit-position
     bitvector
     bitvector->list
     bitvector-length
     bitvector-ref
     bitvector?
     list->bitvector
     make-bitvector)))

;; These can only form part of a safe binding set if no mutable
;; bitvector is exposed to the sandbox.
(define mutating-bitvector-bindings
  '(((guile)
     bit-invert!
     bit-set*!
     bitvector-fill!
     bitvector-set!)))

(define fluid-bindings
  '(((guile)
     fluid-bound?
     fluid-ref
     ;; fluid-ref* could escape the sandbox and is not allowed.
     fluid-thread-local?
     fluid?
     make-fluid
     make-thread-local-fluid
     make-unbound-fluid
     with-fluid*
     with-fluids
     with-fluids*
     make-parameter
     parameter?
     parameterize)))

;; These can only form part of a safe binding set if no fluid is
;; directly exposed to the sandbox.
(define mutating-fluid-bindings
  '(((guile)
     fluid-set!
     fluid-unset!
     fluid->parameter)))

(define char-bindings
  '(((guile)
     char-alphabetic?
     char-ci<=?
     char-ci<?
     char-ci=?
     char-ci>=?
     char-ci>?
     char-downcase
     char-general-category
     char-is-both?
     char-lower-case?
     char-numeric?
     char-titlecase
     char-upcase
     char-upper-case?
     char-whitespace?
     char<=?
     char<?
     char=?
     char>=?
     char>?
     char?
     char->integer
     integer->char)))

(define list-bindings
  '(((guile)
     list
     list-cdr-ref
     list-copy
     list-head
     list-index
     list-ref
     list-tail
     list?
     null?
     make-list
     append
     delete
     delq
     delv
     filter
     length
     member
     memq
     memv
     merge
     reverse)))

;; These can only form part of a safe binding set if no mutable
;; pair is exposed to the sandbox.
(define mutating-list-bindings
  '(((guile)
     list-cdr-set!
     list-set!
     append!
     delete!
     delete1!
     delq!
     delq1!
     delv!
     delv1!
     filter!
     merge!
     reverse!)))

(define pair-bindings
  '(((guile)
     last-pair
     pair?
     caaaar
     caaadr
     caaar
     caadar
     caaddr
     caadr
     caar
     cadaar
     cadadr
     cadar
     caddar
     cadddr
     caddr
     cadr
     car
     cdaaar
     cdaadr
     cdaar
     cdadar
     cdaddr
     cdadr
     cdar
     cddaar
     cddadr
     cddar
     cdddar
     cddddr
     cdddr
     cddr
     cdr
     cons
     cons*)))

;; These can only form part of a safe binding set if no mutable
;; pair is exposed to the sandbox.
(define mutating-pair-bindings
  '(((guile)
     set-car!
     set-cdr!)))

(define vector-bindings
  '(((guile)
     list->vector
     make-vector
     vector
     vector->list
     vector-copy
     vector-length
     vector-ref
     vector?)))

;; These can only form part of a safe binding set if no mutable
;; vector is exposed to the sandbox.
(define mutating-vector-bindings
  '(((guile)
     vector-fill!
     vector-move-left!
     vector-move-right!
     vector-set!)))

(define promise-bindings
  '(((guile)
     force
     delay
     make-promise
     promise?)))

(define srfi-4-bindings
  '(((srfi srfi-4)
     f32vector
     f32vector->list
     f32vector-length
     f32vector-ref
     f32vector?
     f64vector
     f64vector->list
     f64vector-length
     f64vector-ref
     f64vector?
     list->f32vector
     list->f64vector
     list->s16vector
     list->s32vector
     list->s64vector
     list->s8vector
     list->u16vector
     list->u32vector
     list->u64vector
     list->u8vector
     make-f32vector
     make-f64vector
     make-s16vector
     make-s32vector
     make-s64vector
     make-s8vector
     make-u16vector
     make-u32vector
     make-u64vector
     make-u8vector
     s16vector
     s16vector->list
     s16vector-length
     s16vector-ref
     s16vector?
     s32vector
     s32vector->list
     s32vector-length
     s32vector-ref
     s32vector?
     s64vector
     s64vector->list
     s64vector-length
     s64vector-ref
     s64vector?
     s8vector
     s8vector->list
     s8vector-length
     s8vector-ref
     s8vector?
     u16vector
     u16vector->list
     u16vector-length
     u16vector-ref
     u16vector?
     u32vector
     u32vector->list
     u32vector-length
     u32vector-ref
     u32vector?
     u64vector
     u64vector->list
     u64vector-length
     u64vector-ref
     u64vector?
     u8vector
     u8vector->list
     u8vector-length
     u8vector-ref
     u8vector?)))

;; These can only form part of a safe binding set if no mutable
;; bytevector is exposed to the sandbox.
(define mutating-srfi-4-bindings
  '(((srfi srfi-4)
     f32vector-set!
     f64vector-set!
     s16vector-set!
     s32vector-set!
     s64vector-set!
     s8vector-set!
     u16vector-set!
     u32vector-set!
     u64vector-set!
     u8vector-set!)))

(define all-pure-bindings
  (append alist-bindings
          array-bindings
          bit-bindings
          bitvector-bindings
          char-bindings
          char-set-bindings
          clock-bindings
          core-bindings
          error-bindings
          fluid-bindings
          hash-bindings
          iteration-bindings
          keyword-bindings
          list-bindings
          macro-bindings
          nil-bindings
          number-bindings
          pair-bindings
          predicate-bindings
          procedure-bindings
          promise-bindings
          prompt-bindings
          regexp-bindings
          sort-bindings
          srfi-4-bindings
          string-bindings
          symbol-bindings
          unspecified-bindings
          variable-bindings
          vector-bindings
          version-bindings))


(define all-pure-and-impure-bindings
  (append all-pure-bindings
          mutating-alist-bindings
          mutating-array-bindings
          mutating-bitvector-bindings
          mutating-fluid-bindings
          mutating-hash-bindings
          mutating-list-bindings
          mutating-pair-bindings
          mutating-sort-bindings
          mutating-srfi-4-bindings
          mutating-string-bindings
          mutating-variable-bindings
          mutating-vector-bindings))
