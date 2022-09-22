;;;; readline.scm --- support functions for command-line editing
;;;;
;;;; Copyright (C) 1997, 1999, 2000, 2001, 2002, 2006, 2009, 2010, 2011,
;;;;   2013, 2014 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;;; Boston, MA 02110-1301 USA
;;;; 
;;;; Contributed by Daniel Risacher <risacher@worldnet.att.net>.
;;;; Extensions based upon code by
;;;; Andrew Archibald <aarchiba@undergrad.math.uwaterloo.ca>.



(define-module (ice-9 readline)
  #:use-module (ice-9 session)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 buffered-input)
  #:no-backtrace
  #:export (filename-completion-function
	    add-history
	    read-history
	    write-history
	    clear-history))



;;; Dynamically link the glue code for accessing the readline library,
;;; but only when it isn't already present.

(if (not (provided? 'readline))
    (load-extension "guile-readline" "scm_init_readline"))

(if (not (provided? 'readline))
    (scm-error 'misc-error
	       #f
	       "readline is not provided in this Guile installation"
	       '()
	       '()))



;;; Run-time options

(export
 readline-options
 readline-enable
 readline-disable)
(export-syntax
 readline-set!)

(define-option-interface
  (readline-options-interface
   (readline-options readline-enable readline-disable)
   (readline-set!)))



;;; MDJ 980513 <djurfeldt@nada.kth.se>:
;;; There should probably be low-level support instead of this code.

;;; Dirk:FIXME:: If the-readline-port, input-port or output-port are closed,
;;; guile will enter an endless loop or crash.

(define-once new-input-prompt "")
(define-once continuation-prompt "")
(define-once input-port (current-input-port))
(define-once output-port (current-output-port))
(define-once read-hook #f)

(define (make-readline-port)
  (let ((history-buffer #f))
    (make-line-buffered-input-port (lambda (continuation?)
                                     ;; When starting a new read, add
                                     ;; the previously read expression
                                     ;; to the history.
                                     (if (and (not continuation?)
                                              history-buffer)
                                         (begin
                                           (add-history history-buffer)
                                           (set! history-buffer #f)))
                                     ;; Set up prompts and read a line.
                                     (let* ((prompt (if continuation?
                                                        continuation-prompt
                                                        new-input-prompt))
                                            (str (%readline (if (string? prompt)
                                                                prompt
                                                                (prompt))
                                                            input-port
                                                            output-port
                                                            read-hook)))
                                       (or (eof-object? str)
                                           (string=? str "")
                                           (set! history-buffer
                                                 (if history-buffer
                                                     (string-append history-buffer
                                                                    "\n"
                                                                    str)
                                                     str)))
                                       str)))))

;;; We only create one readline port.  There's no point in having
;;; more, since they would all share the tty and history ---
;;; everything except the prompt.  And don't forget the
;;; compile/load/run phase distinctions.  Also, the readline library
;;; isn't reentrant.
(define-once the-readline-port #f)

(define-once history-variable "GUILE_HISTORY")
(define-once history-file
  (string-append (or (getenv "HOME") ".") "/.guile_history"))

(define-public readline-port
  (let ((do (lambda (r/w)
	      (if (memq 'history-file (readline-options-interface))
		  (r/w (or (getenv history-variable)
			   history-file))))))
    (lambda ()
      (if (not the-readline-port)
	  (begin
	    (do read-history) 
	    (set! the-readline-port (make-readline-port))
	    (add-hook! exit-hook (lambda () 
				   (do write-history)
				   (clear-history)))))
      the-readline-port)))

;;; The user might try to use readline in his programs.  It then
;;; becomes very uncomfortable that the current-input-port is the
;;; readline port...
;;;
;;; Here, we detect this situation and replace it with the
;;; underlying port.
;;;
;;; %readline is the low-level readline procedure.

(define-public (readline . args)
  (let ((prompt new-input-prompt)
	(inp input-port))
    (cond ((not (null? args))
	   (set! prompt (car args))
	   (set! args (cdr args))
	   (cond ((not (null? args))
		  (set! inp (car args))
		  (set! args (cdr args))))))
    (apply %readline
	   prompt
	   (if (eq? inp the-readline-port)
	       input-port
	       inp)
	   args)))

(define-public (set-readline-prompt! p . rest)
  (set! new-input-prompt p)
  (if (not (null? rest))
      (set! continuation-prompt (car rest))))

(define-public (set-readline-input-port! p)
  (cond ((or (not (file-port? p)) (not (input-port? p)))
	 (scm-error 'wrong-type-arg "set-readline-input-port!"
		    "Not a file input port: ~S" (list p) #f))
	((port-closed? p)
	 (scm-error 'misc-error "set-readline-input-port!"
		    "Port not open: ~S" (list p) #f))
	(else
	 (set! input-port p))))

(define-public (set-readline-output-port! p)
  (cond ((or (not (file-port? p)) (not (output-port? p)))
	 (scm-error 'wrong-type-arg "set-readline-input-port!"
		    "Not a file output port: ~S" (list p) #f))
	((port-closed? p)
	 (scm-error 'misc-error "set-readline-output-port!"
		    "Port not open: ~S" (list p) #f))
	(else
	 (set! output-port p))))

(define-public (set-readline-read-hook! h)
  (set! read-hook h))

(define-public apropos-completion-function
  (let ((completions '()))
    (lambda (text cont?)
      (if (not cont?)
          (set! completions
                (map symbol->string
                     (apropos-internal
                      (string-append "^" (regexp-quote text))))))
      (if (null? completions)
          #f
          (let ((retval (car completions)))
            (begin (set! completions (cdr completions))
                   retval))))))

(if (provided? 'regex)
    (set! *readline-completion-function* apropos-completion-function))

(define-public (with-readline-completion-function completer thunk)
  "With @var{completer} as readline completion function, call @var{thunk}."
  (let ((old-completer *readline-completion-function*))
    (dynamic-wind
	(lambda ()
	  (set! *readline-completion-function* completer))
	thunk
	(lambda ()
	  (set! *readline-completion-function* old-completer)))))

(define-once readline-repl-reader
  (let ((boot-9-repl-reader repl-reader))
    (lambda* (repl-prompt #:optional (reader (fluid-ref current-reader)))
      (let ((port (current-input-port)))
        (if (eq? port (readline-port))
            (let ((outer-new-input-prompt new-input-prompt)
                  (outer-continuation-prompt continuation-prompt)
                  (outer-read-hook read-hook))
              (dynamic-wind
                (lambda ()
                  (set-buffered-input-continuation?! port #f)
                  (set-readline-prompt! repl-prompt "... ")
                  (set-readline-read-hook! (lambda ()
                                             (run-hook before-read-hook))))
                (lambda () ((or reader read) port))
                (lambda ()
                  (set-readline-prompt! outer-new-input-prompt
                                        outer-continuation-prompt)
                  (set-readline-read-hook! outer-read-hook))))
            (boot-9-repl-reader repl-prompt reader))))))

(define-public (activate-readline)
  (if (isatty? (current-input-port))
      (begin
        (set-current-input-port (readline-port))
        (set! repl-reader readline-repl-reader)
        (set! (using-readline?) #t))))

(define-public (make-completion-function strings)
  "Construct and return a completion function for a list of strings.
The returned function is suitable for passing to
@code{with-readline-completion-function.  The argument @var{strings}
should be a list of strings, where each string is one of the possible
completions."
  (letrec ((strs '())
	   (regexp #f)
	   (completer (lambda (text continue?)
			(if continue?
			    (if (null? strs)
				#f
				(let ((str (car strs)))
				  (set! strs (cdr strs))
				  (if (string-match regexp str)
				      str
				      (completer text #t))))
			    (begin
			      (set! strs strings)
			      (set! regexp
				    (string-append "^" (regexp-quote text)))
			      (completer text #t))))))
    completer))
