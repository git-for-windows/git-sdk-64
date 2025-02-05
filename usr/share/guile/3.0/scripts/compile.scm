;;; Compile --- Command-line Guile Scheme compiler  -*- coding: iso-8859-1 -*-

;; Copyright 2005,2008-2011,2013-2015,2017-2020,2024 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this software; see the file COPYING.LESSER.  If
;; not, write to the Free Software Foundation, Inc., 51 Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;; Author: Andy Wingo <wingo@pobox.com>

;;; Commentary:

;; Usage: compile [ARGS]
;;
;; A command-line interface to the Guile compiler.

;;; Code:

(define-module (scripts compile)
  #:use-module ((system base compile) #:select (compute-compiler
                                                compile-file
                                                default-warning-level
                                                default-optimization-level))
  #:use-module (system base target)
  #:use-module (system base message)
  #:use-module (system base optimize)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (compile))

(define %summary "Compile a file.")


(define (fail message . args)
  (format (current-error-port) "error: ~?~%" message args)
  (exit 1))

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda (opt name arg result)
		  (alist-cons 'help? #t result)))
        (option '("version") #f #f
                (lambda (opt name arg result)
                  (show-version)
                  (exit 0)))

	(option '(#\L "load-path") #t #f
		(lambda (opt name arg result)
		  (let ((load-path (assoc-ref result 'load-path)))
		    (alist-cons 'load-path (cons arg load-path)
				result))))
	(option '(#\o "output") #t #f
		(lambda (opt name arg result)
		  (if (assoc-ref result 'output-file)
		      (fail "`-o' option cannot be specified more than once")
		      (alist-cons 'output-file arg result))))
        (option '("r6rs") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'install-r6rs? #t result)))
        (option '("r7rs") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'install-r7rs? #t result)))
        (option '(#\x) #t #f
                (lambda (opt name arg result)
                  (set! %load-extensions (cons arg %load-extensions))
                  result))

        (option '(#\W "warn") #t #f
                (lambda (opt name arg result)
                  (match arg
                    ("help"
                     (show-warning-help)
                     (exit 0))
                    ((? string->number)
                     (let ((n (string->number arg)))
                       (unless (and (exact-integer? n) (<= 0 n))
                         (fail "Bad warning level `~a'" n))
                       (alist-cons 'warning-level n
                                   (alist-delete 'warning-level result))))
                    (_
                     (let ((warnings (assoc-ref result 'warnings)))
                       (alist-cons 'warnings
                                   (cons (string->symbol arg) warnings)
                                   (alist-delete 'warnings result)))))))

	(option '(#\O "optimize") #t #f
		(lambda (opt name arg result)
                  (define (return val)
                    (alist-cons 'optimizations val result))
                  (define (return-option name val)
                    (let ((kw (symbol->keyword
                               (string->symbol (string-append name "?")))))
                      (unless (assq kw (available-optimizations))
                        (fail "Unknown optimization pass `~a'" name))
                      (return (list kw val))))
                  (cond
                   ((string=? arg "help")
                    (show-optimization-help)
                    (exit 0))
                   ((string->number arg)
                    => (lambda (level)
                         (unless (and (exact-integer? level) (<= 0 level 9))
                           (fail "Bad optimization level `~a'" level))
                         (alist-cons 'optimization-level level
                                     (alist-delete 'optimization-level result))))
                   ((string-prefix? "no-" arg)
                    (return-option (substring arg 3) #f))
                   (else
                    (return-option arg #t)))))
	(option '(#\f "from") #t #f
		(lambda (opt name arg result)
                  (if (assoc-ref result 'from)
                      (fail "`--from' option cannot be specified more than once")
                      (alist-cons 'from (string->symbol arg) result))))
	(option '(#\t "to") #t #f
		(lambda (opt name arg result)
                  (if (assoc-ref result 'to)
                      (fail "`--to' option cannot be specified more than once")
                      (alist-cons 'to (string->symbol arg) result))))
        (option '(#\T "target") #t #f
                (lambda (opt name arg result)
                  (if (assoc-ref result 'target)
                      (fail "`--target' option cannot be specified more than once")
                      (alist-cons 'target arg result))))))

(define (parse-args args)
  "Parse argument list @var{args} and return an alist with all the relevant
options."
  (args-fold args %options
             (lambda (opt name arg result)
               (format (current-error-port) "~A: unrecognized option~%" name)
	       (exit 1))
             (lambda (file result)
	       (let ((input-files (assoc-ref result 'input-files)))
		 (alist-cons 'input-files (cons file input-files)
			     result)))

	     ;; default option values
             `((input-files)
	       (load-path)
               (warning-level . ,(default-warning-level))
               (optimization-level . ,(default-optimization-level))
               (warnings unsupported-warning))))

(define (show-version)
  (format #t "compile (GNU Guile) ~A~%" (version))
  (format #t "Copyright (C) 2024 Free Software Foundation, Inc.
License LGPLv3+: GNU LGPL version 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.~%"))

(define (show-warning-help)
  (format #t "The available warning types are:~%~%")
  (for-each (lambda (wt)
              (format #t "  ~22A ~A~%"
                      (format #f "`~A'" (warning-type-name wt))
                      (warning-type-description wt)))
            %warning-types)
  (format #t "~%")
  (format #t "You may also specify warning levels as `-W0`, `-W1',~%")
  (format #t "`-W2', or `-W3'.  The default is `-W1'.~%"))

(define (show-optimization-help)
  (format #t "The available optimizations are:~%~%")
  (let lp ((options (available-optimizations)))
    (match options
      (() #t)
      (((kw level) . options)
       (let ((name (string-trim-right (symbol->string (keyword->symbol kw))
                                      #\?)))
         (format #t "  -O~a~%" name)
         (lp options)))))
  (format #t "~%")
  (format #t "To disable an optimization, prepend it with `no-', for example~%")
  (format #t "`-Ono-cse.'~%~%")
  (format #t "You may also specify optimization levels as `-O0', `-O1',~%")
  (format #t "`-O2', or `-O3'.  Currently `-O0' turns off all optimizations,~%")
  (format #t "`-O1' turns on partial evaluation, and `-O2' and `-O3' turn on~%")
  (format #t "everything.  The default is equivalent to `-O2'.")
  (format #t "~%"))


(define (compile . args)
  (let* ((options         (parse-args args))
         (help?           (assoc-ref options 'help?))
         (warning-level   (assoc-ref options 'warning-level))
         (optimization-level (assoc-ref options 'optimization-level))
         (compile-opts    `(#:warnings
                            ,(assoc-ref options 'warnings)
                            ,@(append-map
                               (lambda (opt)
                                 (match opt
                                   (('optimizations . opts) opts)
                                   (_ '())))
                               options)))
         (from            (or (assoc-ref options 'from) 'scheme))
         (to              (or (assoc-ref options 'to) 'bytecode))
         (target          (or (assoc-ref options 'target) %host-type))
	 (input-files     (assoc-ref options 'input-files))
	 (output-file     (assoc-ref options 'output-file))
	 (load-path       (assoc-ref options 'load-path)))
    (when (or help? (null? input-files))
      (format #t "Usage: compile [OPTION] FILE...
Compile each Guile source file FILE into a Guile object.

  -h, --help           print this help message

  -L, --load-path=DIR  add DIR to the front of the module load path
  -o, --output=OFILE   write output to OFILE
  -x EXTENSION         add EXTENSION to the set of source file extensions

  -W, --warn=WARNING   emit warnings of type WARNING; use `--warn=help'
                       for a list of available warnings
  -O, --optimize=OPT   specify optimization passes to run; use `-Ohelp'
                       for a list of available optimizations

  --r6rs, --r7rs       compile in an environment whose default bindings,
                       reader options, and load paths are adapted for
                       specific Scheme standards; see \"R6RS Support\"
                       and \"R7RS Support\" in the manual, for full details

  -f, --from=LANG      specify a source language other than `scheme'
  -t, --to=LANG        specify a target language other than `bytecode'
  -T, --target=TRIPLET produce bytecode for host TRIPLET

Note that auto-compilation will be turned off.

Report bugs to <~A>.~%"
              %guile-bug-report-address)
      (exit 0))

    (when (assoc-ref options 'install-r6rs?)
      (install-r6rs!))
    (when (assoc-ref options 'install-r7rs?)
      (install-r7rs!))

    ;; Compute a compiler before changing the load path, for its side
    ;; effects of loading compiler modules.  That way, when
    ;; cross-compiling Guile itself, we can be sure we're loading our
    ;; own language modules and not those of the Guile being compiled,
    ;; which may have incompatible .go files.
    (compute-compiler from to optimization-level warning-level compile-opts)

    (set! %load-path (append load-path %load-path))
    (set! %load-should-auto-compile #f)

    (when (and output-file
               (or (null? input-files)
                   (not (null? (cdr input-files)))))
      (fail "`-o' option can only be specified when compiling a single file"))

    ;; Install a SIGINT handler.  As a side effect, this gives unwind
    ;; handlers an opportunity to run upon SIGINT; this includes that of
    ;; 'call-with-output-file/atomic', called by 'compile-file', which
    ;; removes the temporary output file.
    (sigaction SIGINT
      (lambda args
        (fail "interrupted by the user")))

    (for-each (lambda (file)
                (format #t "wrote `~A'\n"
                        (with-fluids ((*current-warning-prefix* ""))
                          (with-target target
                            (lambda ()
                              (compile-file
                               file
                               #:output-file output-file
                               #:from from
                               #:to to
                               #:warning-level warning-level
                               #:optimization-level optimization-level
                               #:opts compile-opts))))))
              input-files)))

(define main compile)
