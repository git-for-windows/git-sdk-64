;;; doc-snarf --- Extract documentation from source files

;; 	Copyright (C) 2001, 2006, 2011 Free Software Foundation, Inc.
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

;;; Author: Martin Grabmueller

;;; Commentary:

;; Usage: doc-snarf FILE
;;
;; This program reads in a Scheme source file and extracts docstrings
;; in the format specified below.  Additionally, a procedure prototype
;; is inferred from the procedure definition line starting with
;; (define... ).
;;
;; Currently, two output modi are implemented: texinfo and plaintext.
;; Default is plaintext, texinfo can be switched on with the
;; `--texinfo, -t' command line option.
;;
;; Format: A docstring can span multiple lines and a docstring line
;; begins with `;; ' (two semicolons and a space). A docstring is ended
;; by either a line beginning with (define ...) or one or more lines
;; beginning with `;;-' (two semicolons and a dash). These lines are
;; called `options' and begin with a keyword, followed by a colon and
;; a string.
;;
;; Additionally, "standard internal docstrings" (for Scheme source) are
;; recognized and output as "options".  The output formatting is likely
;; to change in the future.
;;
;; Example:

;; This procedure foos, or bars, depending on the argument @var{braz}.
;;-Author: Martin Grabmueller
(define (foo/bar braz)
  (if braz 'foo 'bar))

;;; Which results in the following docstring if texinfo output is
;;; enabled:
#!
foo/bar
@deffn procedure foo/bar braz
This procedure foos, or bars, depending on the argument @var{braz}.
@c Author: Martin Grabmueller
@end deffn
!#

;;; Or in this if plaintext output is used:
#!
Procedure: foo/bar braz
This procedure foos, or bars, depending on the argument @var{braz}.
;; Author: Martin Grabmueller
^L
!#

;; TODO: Convert option lines to alist.
;;       More parameterization.
;;       (maybe) Use in Guile build itself.

(define doc-snarf-version "0.0.2") ; please update before publishing!

;;; Code:

(define-module (scripts doc-snarf)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 regex)
  :use-module (ice-9 string-fun)
  :use-module (ice-9 rdelim)
  :export (doc-snarf))

(define %summary "Snarf out documentation from a file.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help    (single-char #\h) (value #f))
    (output  (single-char #\o) (value #t))
    (texinfo (single-char #\t) (value #f))
    (lang    (single-char #\l) (value #t))))

;; Display version information and exit.
;;-ttn-mod: use var
(define (display-version)
  (display "doc-snarf ") (display doc-snarf-version) (newline))

;; Display the usage help message and exit.
;;-ttn-mod: change option "source" to "lang"
(define (display-help)
  (display "Usage: doc-snarf [options...] inputfile\n")
  (display "  --help, -h              Show this usage information\n")
  (display "  --version, -v           Show version information\n")
  (display
   "  --output=FILE, -o       Specify output file [default=stdout]\n")
  (display "  --texinfo, -t           Format output as texinfo\n")
  (display "  --lang=[c,scheme], -l   Specify the input language\n"))

;; Main program.
;;-ttn-mod: canonicalize lang
(define (doc-snarf . args)
  (let ((options (getopt-long (cons "doc-snarf" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (texinfo-wanted (option-ref options 'texinfo #f))
	  (lang (string->symbol
                 (string-downcase (option-ref options 'lang "scheme")))))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(let ((input (option-ref options '() #f))
	      (output (option-ref options 'output #f)))
	  (if
           ;; Bonard B. Timmons III says `(pair? input)' alone is sufficient.
           ;; (and input (pair? input))
           (pair? input)
           (snarf-file (car input) output texinfo-wanted lang)
           (display-help))))))))

(define main doc-snarf)

;; Supported languages and their parameters.  Each element has form:
;; (LANG DOC-START DOC-END DOC-PREFIX OPT-PREFIX SIG-START STD-INT-DOC?)
;; LANG is a symbol, STD-INT-DOC? is a boolean indicating whether or not
;; LANG supports "standard internal docstring" (a string after the formals),
;; everything else is a string specifying a regexp.
;;-ttn-mod: new var
(define supported-languages
  '((c
     "^/\\*(.*)"
     "^ \\*/"
     "^ \\* (.*)"
     "^ \\*-(.*)"
     "NOTHING AT THIS TIME!!!"
     #f
     )
    (scheme
     "^;; (.*)"
     "^;;\\."
     "^;; (.*)"
     "^;;-(.*)"
     "^\\(define"
     #t
     )))

;; Get @var{lang}'s @var{parameter}.  Both args are symbols.
;;-ttn-mod: new proc
(define (lang-parm lang parm)
  (list-ref (assq-ref supported-languages lang)
            (case parm
              ((docstring-start)  0)
              ((docstring-end)    1)
              ((docstring-prefix) 2)
              ((option-prefix)    3)
              ((signature-start)  4)
              ((std-int-doc?)     5))))

;; Snarf all docstrings from the file @var{input} and write them to
;; file @var{output}.  Use texinfo format for the output if
;; @var{texinfo?} is true.
;;-ttn-mod: don't use string comparison, consult table instead
(define (snarf-file input output texinfo? lang)
  (or (memq lang (map car supported-languages))
      (error "doc-snarf: input language must be c or scheme."))
  (write-output (snarf input lang) output
                (if texinfo? format-texinfo format-plain)))

;; fixme: this comment is required to trigger standard internal
;; docstring snarfing...  ideally, it wouldn't be necessary.
;;-ttn-mod: new proc, from snarf-docs (aren't these names fun?)
(define (find-std-int-doc line input-port)
  "Unread @var{line} from @var{input-port}, then read in the entire form and
return the standard internal docstring if found.  Return #f if not."
  (unread-string line input-port)       ; ugh
  (let ((form (read input-port)))
    (cond ((and (list? form)            ; (define (PROC ARGS) "DOC" ...)
                (< 3 (length form))
                (eq? 'define (car form))
                (pair? (cadr form))
                (symbol? (caadr form))
                (string? (caddr form)))
           (caddr form))
          ((and (list? form)            ; (define VAR (lambda ARGS "DOC" ...))
                (< 2 (length form))
                (eq? 'define (car form))
                (symbol? (cadr form))
                (list? (caddr form))
                (< 3 (length (caddr form)))
                (eq? 'lambda (car (caddr form)))
                (string? (caddr (caddr form))))
           (caddr (caddr form)))
          (else #f))))

;; Split @var{string} into lines, adding @var{prefix} to each.
;;-ttn-mod: new proc
(define (split-prefixed string prefix)
  (separate-fields-discarding-char
   #\newline string
   (lambda lines
     (map (lambda (line)
            (string-append prefix line))
          lines))))

;; snarf input-file output-file
;; Extract docstrings from the input file @var{input}, presumed
;; to be written in language @var{lang}.
;;-Author: Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
;;-Created: 2001-02-17
;;-ttn-mod: regularize lang parm lookup, add "std int doc" snarfing (2 places)
(define (snarf input-file lang)
  (let* ((i-p (open-input-file input-file))
         (parm-regexp (lambda (parm) (make-regexp (lang-parm lang parm))))
         (docstring-start  (parm-regexp 'docstring-start))
         (docstring-end    (parm-regexp 'docstring-end))
         (docstring-prefix (parm-regexp 'docstring-prefix))
         (option-prefix    (parm-regexp 'option-prefix))
         (signature-start  (parm-regexp 'signature-start))
         (augmented-options
          (lambda (line i-p options)
            (let ((int-doc (and (lang-parm lang 'std-int-doc?)
                                (let ((d (find-std-int-doc line i-p)))
                                  (and d (split-prefixed d "internal: "))))))
              (if int-doc
                  (append (reverse int-doc) options)
                  options)))))

    (let lp ((line (read-line i-p)) (state 'neutral) (doc-strings '())
	     (options '()) (entries '()) (lno 0))
      (cond
       ((eof-object? line)
	(close-input-port i-p)
	(reverse entries))

       ;; State 'neutral: we're currently not within a docstring or
       ;; option section
       ((eq? state 'neutral)
	(let ((m (regexp-exec docstring-start line)))
	  (if m
	    (lp (read-line i-p) 'doc-string
		(list (match:substring m 1)) '() entries (+ lno 1))
	    (lp (read-line i-p) state '() '() entries (+ lno 1)))))

       ;; State 'doc-string: we have started reading a docstring and
       ;; are waiting for more, for options or for a define.
       ((eq? state 'doc-string)
	(let ((m0 (regexp-exec docstring-prefix line))
	      (m1 (regexp-exec option-prefix line))
	      (m2 (regexp-exec signature-start line))
	      (m3 (regexp-exec docstring-end line)))
	  (cond
	   (m0
	    (lp (read-line i-p) 'doc-string
		(cons (match:substring m0 1) doc-strings) '() entries
		(+ lno 1)))
	   (m1
	    (lp (read-line i-p) 'options
		doc-strings (cons (match:substring m1 1) options) entries
		(+ lno 1)))
	   (m2
            (let ((options (augmented-options line i-p options))) ; ttn-mod
              (lp (read-line i-p) 'neutral '() '()
                  (cons (parse-entry doc-strings options line input-file lno)
                        entries)
                  (+ lno 1))))
           (m3
	    (lp (read-line i-p) 'neutral '() '()
		(cons (parse-entry doc-strings options #f input-file lno)
		      entries)
		(+ lno 1)))
	   (else
	    (lp (read-line i-p) 'neutral '() '() entries (+ lno 1))))))

       ;; State 'options: We're waiting for more options or for a
       ;; define.
       ((eq? state 'options)
	(let ((m1 (regexp-exec option-prefix line))
	      (m2 (regexp-exec signature-start line))
	      (m3 (regexp-exec docstring-end line)))
	  (cond
	   (m1
	    (lp (read-line i-p) 'options
		doc-strings (cons (match:substring m1 1) options) entries
		(+ lno 1)))
	   (m2
            (let ((options (augmented-options line i-p options))) ; ttn-mod
              (lp (read-line i-p) 'neutral '() '()
                  (cons (parse-entry doc-strings options line input-file lno)
                        entries)
                  (+ lno 1))))
	   (m3
	    (lp (read-line i-p) 'neutral '() '()
		(cons (parse-entry doc-strings options #f input-file lno)
		      entries)
		(+ lno 1)))
	   (else
	    (lp (read-line i-p) 'neutral '() '() entries (+ lno 1))))))))))

(define (make-entry symbol signature docstrings options filename line)
  (vector 'entry symbol signature docstrings options filename line))
(define (entry-symbol e)
  (vector-ref e 1))
(define (entry-signature e)
  (vector-ref e 2))
(define (entry-docstrings e)
  (vector-ref e 3))
(define (entry-options e)
  (vector-ref e 4))
(define (entry-filename e)
  (vector-ref e 5))
(define (entry-line e)
  "This docstring will not be snarfed, unfortunately..."
  (vector-ref e 6))

;; Create a docstring entry from the docstring line list
;; @var{doc-strings}, the option line list @var{options} and the
;; define line @var{def-line}
(define (parse-entry docstrings options def-line filename line-no)
;  (write-line docstrings)
  (cond
   (def-line
     (make-entry (get-symbol def-line)
		 (make-prototype def-line) (reverse docstrings)
		 (reverse options) filename
		 (+ (- line-no (length docstrings) (length options)) 1)))
   ((> (length docstrings) 0)
    (make-entry (string->symbol (car (reverse docstrings)))
		(car (reverse docstrings))
		(cdr (reverse docstrings))
		(reverse options) filename
		(+ (- line-no (length docstrings) (length options)) 1)))
   (else
    (make-entry 'foo "" (reverse docstrings) (reverse options) filename
		(+ (- line-no (length docstrings) (length options)) 1)))))

;; Create a string which is a procedure prototype.  The necessary
;; information for constructing the prototype is taken from the line
;; @var{def-line}, which is a line starting with @code{(define...}.
(define (make-prototype def-line)
  (call-with-input-string
   def-line
   (lambda (s-p)
     (let* ((paren (read-char s-p))
	    (keyword (read s-p))
	    (tmp (read s-p)))
       (cond
	((pair? tmp)
	 (join-symbols tmp))
	((symbol? tmp)
	 (symbol->string tmp))
	(else
	 ""))))))

(define (get-symbol def-line)
  (call-with-input-string
   def-line
   (lambda (s-p)
     (let* ((paren (read-char s-p))
	    (keyword (read s-p))
	    (tmp (read s-p)))
       (cond
	((pair? tmp)
	 (car tmp))
	((symbol? tmp)
	 tmp)
	(else
	 'foo))))))

;; Append the symbols in the string list @var{s}, separated with a
;; space character.
(define (join-symbols s)
  (cond ((null? s)
	 "")
	((symbol? s)
	 (string-append ". " (symbol->string s)))
	((null? (cdr s))
	 (symbol->string (car s)))
	(else
	 (string-append (symbol->string (car s)) " " (join-symbols (cdr s))))))

;; Write @var{entries} to @var{output-file} using @var{writer}.
;; @var{writer} is a proc that takes one entry.
;; If @var{output-file} is #f, write to stdout.
;;-ttn-mod: new proc
(define (write-output entries output-file writer)
  (with-output-to-port (cond (output-file (open-output-file output-file))
                             (else (current-output-port)))
    (lambda () (for-each writer entries))))

;; Write an @var{entry} using texinfo format.
;;-ttn-mod: renamed from `texinfo-output', distilled
(define (format-texinfo entry)
  (display "\n\f")
  (display (entry-symbol entry))
  (newline)
  (display "@c snarfed from ")
  (display (entry-filename entry))
  (display ":")
  (display (entry-line entry))
  (newline)
  (display "@deffn procedure ")
  (display (entry-signature entry))
  (newline)
  (for-each (lambda (s) (write-line s))
            (entry-docstrings entry))
  (for-each (lambda (s) (display "@c ") (write-line s))
            (entry-options entry))
  (write-line "@end deffn"))

;; Write an @var{entry} using plain format.
;;-ttn-mod: renamed from `texinfo-output', distilled
(define (format-plain entry)
  (display "Procedure: ")
  (display (entry-signature entry))
  (newline)
  (for-each (lambda (s) (write-line s))
            (entry-docstrings entry))
  (for-each (lambda (s) (display ";; ") (write-line s))
            (entry-options entry))
  (display "Snarfed from ")
  (display (entry-filename entry))
  (display ":")
  (display (entry-line entry))
  (newline)
  (write-line "\f"))

;;; doc-snarf ends here
