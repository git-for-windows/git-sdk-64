;;; High-level compiler interface

;; Copyright (C) 2001,2005,2008-2013,2016,2020,2021 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(define-module (system base compile)
  #:use-module (system base language)
  #:use-module (system base message)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:export (compiled-file-name
            compile-file
            compile-and-load
            compute-compiler
            read-and-compile
            compile
            decompile
            default-warning-level
            default-optimization-level))


(define (level-validator x)
  (unless (and (exact-integer? x) (<= 0 x 9))
    (error
     "bad warning or optimization level: expected integer between 0 and 9"
     x))
  x)

(define default-warning-level (make-parameter 1 level-validator))
(define default-optimization-level (make-parameter 2 level-validator))

;;;
;;; Compiler
;;;

(define (call-once thunk)
  (let ((entered #f))
    (dynamic-wind
        (lambda ()
          (when entered
            (error "thunk may only be entered once: ~a" thunk))
          (set! entered #t))
        thunk
        (lambda () #t))))

;; emacs: (put 'call-with-output-file/atomic 'scheme-indent-function 1)
(define* (call-with-output-file/atomic filename proc #:optional reference)
  (let* ((tmp (mkstemp (string-append filename ".XXXXXX") "wb"))
         (tmpname (port-filename tmp)))
    (call-once
     (lambda ()
       (with-throw-handler #t
         (lambda ()
           (proc tmp)
           ;; Chmodding by name instead of by port allows this chmod to
           ;; work on systems without fchmod, like MinGW.
           (let ((perms (or (false-if-exception (stat:perms (stat reference)))
                            (lognot (umask)))))
             (chmod tmpname (logand #o0666 perms)))
           (close-port tmp)
           (rename-file tmpname filename))
         (lambda args
           (close-port tmp)
           (delete-file tmpname)))))))

(define (ensure-language x)
  (if (language? x)
      x
      (lookup-language x)))

;; Throws an exception if `dir' is not writable.  The mkdir occurs
;; before the check, so that we avoid races (possibly due to parallel
;; compilation).
;;
(define (ensure-directory dir)
  (catch 'system-error
    (lambda ()
      (mkdir dir))
    (lambda (k subr fmt args rest)
      (let ((errno (and (pair? rest) (car rest))))
        (cond
         ((eqv? errno EEXIST)
          ;; Assume it's a writable directory, to avoid TOCTOU errors,
          ;; as well as UID/EUID mismatches that occur with access(2).
          #t)
         ((eqv? errno ENOENT)
          (ensure-directory (dirname dir))
          (ensure-directory dir))
         (else
          (throw k subr fmt args rest)))))))

;;; This function is among the trickiest I've ever written. I tried many
;;; variants. In the end, simple is best, of course.
;;;
;;; After turning this around a number of times, it seems that the
;;; desired behavior is that .go files should exist in a path, for
;;; searching. That is orthogonal to this function. For writing .go
;;; files, either you know where they should go, in which case you tell
;;; compile-file explicitly, as in the srcdir != builddir case; or you
;;; don't know, in which case this function is called, and we just put
;;; them in your own ccache dir in ~/.cache/guile/ccache.
;;;
;;; See also boot-9.scm:load.
(define (compiled-file-name file)
  ;; FIXME: would probably be better just to append SHA1(canon-path)
  ;; to the %compile-fallback-path, to avoid deep directory stats.
  (define (canonical->suffix canon)
    (cond
     ((string-prefix? "/" canon) canon)
     ((and (> (string-length canon) 2)
           (eqv? (string-ref canon 1) #\:))
      ;; Paths like C:... transform to /C...
      (string-append "/" (substring canon 0 1) (substring canon 2)))
     (else canon)))
  (define (compiled-extension)
    (cond ((or (null? %load-compiled-extensions)
               (string-null? (car %load-compiled-extensions)))
           (warn "invalid %load-compiled-extensions"
                 %load-compiled-extensions)
           ".go")
          (else (car %load-compiled-extensions))))
  (and %compile-fallback-path
       (let ((f (string-append
                 %compile-fallback-path
                 (canonical->suffix (canonicalize-path file))
                 (compiled-extension))))
         (and (false-if-exception (ensure-directory (dirname f)))
              f))))

(define (validate-options opts)
  (define (validate-warnings warnings)
    (match warnings
      (() (values))
      ((w . warnings)
       (unless (lookup-warning-type w)
         (warning 'unsupported-warning #f w))
       (validate-warnings warnings))))
  (match opts
    (() (values))
    ((kw arg . opts)
     (match kw
       (#:warnings (validate-warnings arg))
       ((? keyword?) (values))
       (_
        ;; Programming error.
        (warn "malformed options list: not a keyword" kw)))
     (validate-options opts))
    (_
     ;; Programming error.
     (warn "malformed options list: expected keyword and arg pair" opts))))

(define* (compile-file file #:key
                       (output-file #f)
                       (from (current-language))
                       (to 'bytecode)
                       (env (default-environment from))
                       (optimization-level (default-optimization-level))
                       (warning-level (default-warning-level))
                       (opts '())
                       (canonicalization 'relative))
  (validate-options opts)
  (with-fluids ((%file-port-name-canonicalization canonicalization))
    (let* ((comp (or output-file (compiled-file-name file)
                     (error "failed to create path for auto-compiled file"
                            file)))
           (in (open-input-file file))
           (enc (file-encoding in)))
      ;; Choose the input encoding deterministically.
      (set-port-encoding! in (or enc "UTF-8"))

      (ensure-directory (dirname comp))
      (call-with-output-file/atomic comp
        (lambda (port)
          ((language-printer (ensure-language to))
           (read-and-compile in #:env env #:from from #:to to
                             #:optimization-level optimization-level
                             #:warning-level warning-level
                             #:opts (cons* #:to-file? #t opts))
           port))
        file)
      comp)))

(define* (compile-and-load file #:key (from (current-language)) (to 'value)
                           (env (current-module))
                           (optimization-level (default-optimization-level))
                           (warning-level (default-warning-level))
                           (opts '())
                           (canonicalization 'relative))
  (validate-options opts)
  (with-fluids ((%file-port-name-canonicalization canonicalization))
    (read-and-compile (open-input-file file)
                      #:from from #:to to #:opts opts
                      #:optimization-level optimization-level
                      #:warning-level warning-level
                      #:env env)))


;;;
;;; Compiler interface
;;;

(define (compute-analyzer lang warning-level opts)
  (level-validator warning-level)
  (match (language-analyzer lang)
    (#f (lambda (exp env) (values)))
    (proc (proc warning-level
                (let lp ((opts opts))
                  (match opts
                    (() '())
                    ((#:warnings warnings . _) warnings)
                    ((_ _ . opts) (lp opts))))))))

(define (compute-lowerer lang optimization-level opts)
  (level-validator optimization-level)
  (match (language-lowerer lang)
    (#f (lambda (exp env) exp))
    (proc (proc optimization-level opts))))

(define (next-pass from lang to optimization-level opts)
  (if (eq? lang to)
      #f ;; Done.
      (match (language-compilers lang)
        (((name . pass))
         (cons (lookup-language name) pass))
        (compilers
         (let ((chooser (language-compiler-chooser lang)))
           (unless chooser
             (if (null? compilers)
                 (error "no way to compile" from "to" to)
                 (error "multiple compilers; language should supply chooser")))
           (match (chooser to optimization-level opts)
             ((name . pass)
              (cons (lookup-language name) pass))))))))

(define (compute-compiler from to optimization-level warning-level opts)
  (let ((from (ensure-language from))
        (to (ensure-language to)))
    (let lp ((lang from))
      (match (next-pass from lang to optimization-level opts)
        (#f (lambda (exp env) (values exp env env)))
        ((next . pass)
         (let* ((analyze (compute-analyzer lang warning-level opts))
                (lower (compute-lowerer lang optimization-level opts))
                (compile (lambda (exp env)
                           (analyze exp env)
                           (pass (lower exp env) env opts)))
                (tail (lp next)))
           (lambda (exp env)
             (let*-values (((exp env cenv) (compile exp env))
                           ((exp env cenv*) (tail exp env)))
               ;; Return continuation environment from first pass, to
               ;; compile an additional expression in the same compilation
               ;; unit.
               (values exp env cenv)))))))))

(define (find-language-joint from to optimization-level opts)
  (let ((from (ensure-language from))
        (to (ensure-language to)))
    (let lp ((lang from))
      (match (next-pass from lang to optimization-level opts)
        (#f #f)
        ((next . pass)
         (or (lp next)
             (and (language-joiner next)
                  next)))))))

(define (default-language-joiner lang)
  (lambda (exps env)
    (match exps
      ((exp) exp)
      (_
       (error
        "Multiple expressions read and compiled, but language has no joiner"
        lang)))))

(define (read-and-parse lang port cenv)
  (let ((exp ((language-reader lang) port cenv)))
    (cond
     ((eof-object? exp) exp)
     ((language-parser lang) => (lambda (parse) (parse exp)))
     (else exp))))

(define* (read-and-compile port #:key
                           (from (current-language))
                           (to 'bytecode)
                           (env (default-environment from))
                           (optimization-level (default-optimization-level))
                           (warning-level (default-warning-level))
                           (opts '()))
  (let* ((from (ensure-language from))
         (to (ensure-language to))
         (joint (find-language-joint from to optimization-level opts)))
    (parameterize ((current-language from))
      (let lp ((exps '()) (env #f) (cenv env) (from #f) (compile1 #f))
        (match (read-and-parse (current-language) port cenv)
          ((? eof-object?)
           (close-port port)
           (if joint
               (compile ((or (language-joiner joint)
                             (default-language-joiner joint))
                         (reverse exps)
                         env)
                        #:from joint #:to to
                        ;; env can be false if no expressions were read.
                        #:env (or env (default-environment joint))
                        #:optimization-level optimization-level
                        #:warning-level warning-level
                        #:opts opts)
               ((default-language-joiner to)
                (reverse exps)
                env)))
          (exp
           (let with-compiler ((from from) (compile1 compile1))
             (cond
              ((eq? from (current-language))
               (receive (exp env cenv) (compile1 exp cenv)
                 (lp (cons exp exps) env cenv from compile1)))
              (else
               ;; compute-compiler instead of compile so we get the
               ;; env too.
               (let ((from (current-language)))
                 (with-compiler
                  from
                  (compute-compiler from (or joint to) optimization-level
                                    warning-level opts))))))))))))

(define* (compile x #:key
                  (from (current-language))
                  (to 'value)
                  (env (default-environment from))
                  (optimization-level (default-optimization-level))
                  (warning-level (default-warning-level))
                  (opts '()))
  (validate-options opts)
  (let ((compile1 (compute-compiler from to optimization-level
                                    warning-level opts)))
    (receive (exp env cenv) (compile1 x env)
      exp)))


;;;
;;; Decompiler interface
;;;

(define (decompile-passes from to opts)
  (match (lookup-decompilation-order from to)
    (((langs . passes) ...) passes)
    (_ (error "no way to decompile" from "to" to))))

(define (decompile-fold passes exp env opts)
  (match passes
    (() (values exp env))
    ((pass . passes)
     (receive (exp env) (pass exp env opts)
       (decompile-fold passes exp env opts)))))

(define* (decompile x #:key
                    (env #f)
                    (from 'tree-il)
                    (to 'scheme)
                    (opts '()))
  (decompile-fold (decompile-passes from to opts)
                  x
                  env
                  opts))
