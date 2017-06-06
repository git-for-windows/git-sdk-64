;;; Compile --- Command-line Guile Scheme compiler  -*- coding: iso-8859-1 -*-

;; Copyright 2005, 2008, 2009, 2010, 2011, 2014 Free Software Foundation, Inc.
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
  #:use-module ((system base compile) #:select (compile-file))
  #:use-module (system base target)
  #:use-module (system base message)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 format)
  #:export (compile))

(define %summary "Compile a file.")


(define (fail . messages)
  (format (current-error-port) "error: ~{~a~}~%" messages)
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

        (option '(#\W "warn") #t #f
                (lambda (opt name arg result)
                  (if (string=? arg "help")
                      (begin
                        (show-warning-help)
                        (exit 0))
                      (let ((warnings (assoc-ref result 'warnings)))
                        (alist-cons 'warnings
                                    (cons (string->symbol arg) warnings)
                                    (alist-delete 'warnings result))))))

	(option '(#\O "optimize") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'optimize? #t result)))
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
               (format (current-error-port) "~A: unrecognized option" name)
	       (exit 1))
             (lambda (file result)
	       (let ((input-files (assoc-ref result 'input-files)))
		 (alist-cons 'input-files (cons file input-files)
			     result)))

	     ;; default option values
             '((input-files)
	       (load-path)
               (warnings unsupported-warning))))

(define (show-version)
  (format #t "compile (GNU Guile) ~A~%" (version))
  (format #t "Copyright (C) 2009, 2011 Free Software Foundation, Inc.
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
  (format #t "~%"))


(define (compile . args)
  (let* ((options         (parse-args args))
         (help?           (assoc-ref options 'help?))
         (compile-opts    (let ((o `(#:warnings
                                     ,(assoc-ref options 'warnings))))
                            (if (assoc-ref options 'optimize?)
                                (cons #:O o)
                                o)))
         (from            (or (assoc-ref options 'from) 'scheme))
         (to              (or (assoc-ref options 'to) 'objcode))
         (target          (or (assoc-ref options 'target) %host-type))
	 (input-files     (assoc-ref options 'input-files))
	 (output-file     (assoc-ref options 'output-file))
	 (load-path       (assoc-ref options 'load-path)))
    (if (or help? (null? input-files))
        (begin
          (format #t "Usage: compile [OPTION] FILE...
Compile each Guile source file FILE into a Guile object.

  -h, --help           print this help message

  -L, --load-path=DIR  add DIR to the front of the module load path
  -o, --output=OFILE   write output to OFILE

  -W, --warn=WARNING   emit warnings of type WARNING; use `--warn=help'
                       for a list of available warnings

  -f, --from=LANG      specify a source language other than `scheme'
  -t, --to=LANG        specify a target language other than `objcode'
  -T, --target=TRIPLET produce bytecode for host TRIPLET

Note that auto-compilation will be turned off.

Report bugs to <~A>.~%"
                  %guile-bug-report-address)
          (exit 0)))

    (set! %load-path (append load-path %load-path))
    (set! %load-should-auto-compile #f)

    (if (and output-file
             (or (null? input-files)
                 (not (null? (cdr input-files)))))
        (fail "`-o' option can only be specified "
              "when compiling a single file"))

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
                              (compile-file file
                                            #:output-file output-file
                                            #:from from
                                            #:to to
                                            #:opts compile-opts))))))
              input-files)))

(define main compile)
