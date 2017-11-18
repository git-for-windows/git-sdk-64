;;; Repl commands

;; Copyright (C) 2001, 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

(define-module (system repl command)
  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:use-module (system base compile)
  #:use-module (system repl common)
  #:use-module (system repl debug)
  #:use-module (system vm disassembler)
  #:use-module (system vm loader)
  #:use-module (system vm program)
  #:use-module (system vm trap-state)
  #:use-module (system vm vm)
  #:autoload (system base language) (lookup-language language-reader)
  #:autoload (system vm trace) (call-with-trace)
  #:use-module (ice-9 format)
  #:use-module (ice-9 session)
  #:use-module (ice-9 documentation)
  #:use-module (ice-9 and-let-star)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 control)
  #:use-module ((ice-9 pretty-print) #:select ((pretty-print . pp)))
  #:use-module ((system vm inspect) #:select ((inspect . %inspect)))
  #:use-module (rnrs bytevectors)
  #:use-module (statprof)
  #:export (meta-command define-meta-command))


;;;
;;; Meta command interface
;;;

(define *command-table*
  '((help     (help h) (show) (apropos a) (describe d))
    (module   (module m) (import use) (load l) (reload re) (binding b) (in))
    (language (language L))
    (compile  (compile c) (compile-file cc)
              (expand exp) (optimize opt)
	      (disassemble x) (disassemble-file xx))
    (profile  (time t) (profile pr) (trace tr))
    (debug    (backtrace bt) (up) (down) (frame fr)
              (locals) (error-message error)
              (break br bp) (break-at-source break-at bs)
              (step s) (step-instruction si)
              (next n) (next-instruction ni)
              (finish)
              (tracepoint tp)
              (traps) (delete del) (disable) (enable)
              (registers regs))
    (inspect  (inspect i) (pretty-print pp))
    (system   (gc) (statistics stat) (option o)
              (quit q continue cont))))

(define *show-table*
  '((show (warranty w) (copying c) (version v))))

(define (group-name g) (car g))
(define (group-commands g) (cdr g))

(define *command-infos* (make-hash-table))
(define (command-name c) (car c))
(define (command-abbrevs c) (cdr c))
(define (command-info c) (hashq-ref *command-infos* (command-name c)))
(define (command-procedure c) (command-info-procedure (command-info c)))
(define (command-doc c) (procedure-documentation (command-procedure c)))

(define (make-command-info proc arguments-reader)
  (cons proc arguments-reader))

(define (command-info-procedure info)
  (car info))

(define (command-info-arguments-reader info)
  (cdr info))

(define (command-usage c)
  (let ((doc (command-doc c)))
    (substring doc 0 (string-index doc #\newline))))

(define (command-summary c)
  (let* ((doc (command-doc c))
	 (start (1+ (string-index doc #\newline))))
    (cond ((string-index doc #\newline start)
	   => (lambda (end) (substring doc start end)))
	  (else (substring doc start)))))

(define (lookup-group name)
  (assq name *command-table*))

(define* (lookup-command key #:optional (table *command-table*))
  (let loop ((groups table) (commands '()))
    (cond ((and (null? groups) (null? commands)) #f)
	  ((null? commands)
	   (loop (cdr groups) (cdar groups)))
	  ((memq key (car commands)) (car commands))
	  (else (loop groups (cdr commands))))))

(define* (display-group group #:optional (abbrev? #t))
  (format #t "~:(~A~) Commands~:[~; [abbrev]~]:~2%" (group-name group) abbrev?)
  (for-each (lambda (c)
	      (display-summary (command-usage c)
			       (if abbrev? (command-abbrevs c) '())
			       (command-summary c)))
	    (group-commands group))
  (newline))

(define (display-command command)
  (display "Usage: ")
  (display (command-doc command))
  (newline))

(define (display-summary usage abbrevs summary)
  (let* ((usage-len (string-length usage))
         (abbrevs (if (pair? abbrevs)
                      (format #f "[,~A~{ ,~A~}]" (car abbrevs) (cdr abbrevs))
                      ""))
         (abbrevs-len (string-length abbrevs)))
    (format #t " ,~A~A~A - ~A\n"
            usage
            (cond
             ((> abbrevs-len 32)
              (error "abbrevs too long" abbrevs))
             ((> (+ usage-len abbrevs-len) 32)
              (format #f "~%~v_" (+ 2 (- 32 abbrevs-len))))
             (else
              (format #f "~v_" (- 32 abbrevs-len usage-len))))
            abbrevs
            summary)))

(define (read-command repl)
  (catch #t
    (lambda () (read))
    (lambda (key . args)
      (pmatch args
        ((,subr ,msg ,args . ,rest)
         (format #t "Throw to key `~a' while reading command:\n" key)
         (display-error #f (current-output-port) subr msg args rest))
        (else
         (format #t "Throw to key `~a' with args `~s' while reading command.\n"
                 key args)))
      (force-output)
      *unspecified*)))

(define (read-command-arguments c repl)
  ((command-info-arguments-reader (command-info c)) repl))

(define (meta-command repl)
  (let ((command (read-command repl)))
    (cond
     ((eq? command *unspecified*)) ; read error, already signalled; pass.
     ((not (symbol? command))
      (format #t "Meta-command not a symbol: ~s~%" command))
     ((lookup-command command)
      => (lambda (c)
           (and=> (read-command-arguments c repl)
                  (lambda (args) (apply (command-procedure c) repl args)))))
     (else
      (format #t "Unknown meta command: ~A~%" command)))))

(define (add-meta-command! name category proc argument-reader)
  (hashq-set! *command-infos* name (make-command-info proc argument-reader))
  (if category
      (let ((entry (assq category *command-table*)))
        (if entry
            (set-cdr! entry (append (cdr entry) (list (list name))))
            (set! *command-table*
                  (append *command-table*
                          (list (list category (list name)))))))))

(define-syntax define-meta-command
  (syntax-rules ()
    ((_ ((name category) repl (expression0 ...) . datums) docstring b0 b1 ...)
     (add-meta-command!
      'name
      'category
      (lambda* (repl expression0 ... . datums)
        docstring
        b0 b1 ...)
      (lambda (repl)
        (define (handle-read-error form-name key args)
          (pmatch args
            ((,subr ,msg ,args . ,rest)
             (format #t "Throw to key `~a' while reading ~@[argument `~A' of ~]command `~A':\n"
                     key form-name 'name)
             (display-error #f (current-output-port) subr msg args rest))
            (else
             (format #t "Throw to key `~a' with args `~s' while reading ~@[ argument `~A' of ~]command `~A'.\n"
                     key args form-name 'name)))
          (abort))
        (% (let* ((expression0
                   (catch #t
                          (lambda ()
                            (repl-reader
                             ""
                             (lambda* (#:optional (port (current-input-port)))
                               ((language-reader (repl-language repl))
                                port (current-module)))))
                          (lambda (k . args)
                            (handle-read-error 'expression0 k args))))
                  ...)
             (append
              (list expression0 ...)
              (catch #t
                     (lambda ()
                       (let ((port (open-input-string (read-line))))
                         (let lp ((out '()))
                           (let ((x (read port)))
                             (if (eof-object? x)
                                 (reverse out)
                                 (lp (cons x out)))))))
                     (lambda (k . args)
                       (handle-read-error #f k args)))))
           (lambda (k) #f)))))           ; the abort handler

    ((_ ((name category) repl . datums) docstring b0 b1 ...)
     (define-meta-command ((name category) repl () . datums)
       docstring b0 b1 ...))

    ((_ (name repl (expression0 ...) . datums) docstring b0 b1 ...)
     (define-meta-command ((name #f) repl (expression0 ...) . datums)
       docstring b0 b1 ...))

    ((_ (name repl . datums) docstring b0 b1 ...)
     (define-meta-command ((name #f) repl () . datums)
       docstring b0 b1 ...))))



;;;
;;; Help commands
;;;

(define-meta-command (help repl . args)
  "help [all | GROUP | [-c] COMMAND]
Show help.

With one argument, tries to look up the argument as a group name, giving
help on that group if successful. Otherwise tries to look up the
argument as a command, giving help on the command.

If there is a command whose name is also a group name, use the ,help
-c COMMAND form to give help on the command instead of the group.

Without any argument, a list of help commands and command groups
are displayed."
  (pmatch args
    (()
     (display-group (lookup-group 'help))
     (display "Command Groups:\n\n")
     (display-summary "help all" #f "List all commands")
     (for-each (lambda (g)
		 (let* ((name (symbol->string (group-name g)))
			(usage (string-append "help " name))
			(header (string-append "List " name " commands")))
		   (display-summary usage #f header)))
	       (cdr *command-table*))
     (newline)
     (display
      "Type `,help -c COMMAND' to show documentation of a particular command.")
     (newline))
    ((all)
     (for-each display-group *command-table*))
    ((,group) (guard (lookup-group group))
     (display-group (lookup-group group)))
    ((,command) (guard (lookup-command command))
     (display-command (lookup-command command)))
    ((-c ,command) (guard (lookup-command command))
     (display-command (lookup-command command)))
    ((,command)
     (format #t "Unknown command or group: ~A~%" command))
    ((-c ,command)
     (format #t "Unknown command: ~A~%" command))
    (else
     (format #t "Bad arguments: ~A~%" args))))

(define-meta-command (show repl . args)
  "show [TOPIC]
Gives information about Guile.

With one argument, tries to show a particular piece of information;

currently supported topics are `warranty' (or `w'), `copying' (or `c'),
and `version' (or `v').

Without any argument, a list of topics is displayed."
  (pmatch args
    (()
     (display-group (car *show-table*) #f)
     (newline))
    ((,topic) (guard (lookup-command topic *show-table*))
     ((command-procedure (lookup-command topic *show-table*)) repl))
    ((,command)
     (format #t "Unknown topic: ~A~%" command))
    (else
     (format #t "Bad arguments: ~A~%" args))))

;;; `warranty', `copying' and `version' are "hidden" meta-commands, only
;;; accessible via `show'. They have an entry in *command-infos* but not
;;; in *command-table*.

(define-meta-command (warranty repl)
  "show warranty
Details on the lack of warranty."
  (display *warranty*)
  (newline))

(define-meta-command (copying repl)
  "show copying
Show the LGPLv3."
  (display *copying*)
  (newline))

(define-meta-command (version repl)
  "show version
Version information."
  (display *version*)
  (newline))

(define-meta-command (apropos repl regexp)
  "apropos REGEXP
Find bindings/modules/packages."
  (apropos (->string regexp)))

(define-meta-command (describe repl (form))
  "describe OBJ
Show description/documentation."
  (display
    (object-documentation
      (let ((input (repl-parse repl form)))
        (if (symbol? input)
            (module-ref (current-module) input)
            (repl-eval repl input)))))
  (newline))

(define-meta-command (option repl . args)
  "option [NAME] [EXP]
List/show/set options."
  (pmatch args
    (()
     (for-each (lambda (spec)
		 (format #t "  ~A~24t~A\n" (car spec) (cadr spec)))
	       (repl-options repl)))
    ((,name)
     (display (repl-option-ref repl name))
     (newline))
    ((,name ,exp)
     ;; Would be nice to evaluate in the current language, but the REPL
     ;; option parser doesn't permit that, currently.
     (repl-option-set! repl name (eval exp (current-module))))))

(define-meta-command (quit repl)
  "quit
Quit this session."
  (throw 'quit))


;;;
;;; Module commands
;;;

(define-meta-command (module repl . args)
  "module [MODULE]
Change modules / Show current module."
  (pmatch args
    (() (puts (module-name (current-module))))
    ((,mod-name) (guard (list? mod-name))
     (set-current-module (resolve-module mod-name)))
    (,mod-name (set-current-module (resolve-module mod-name)))))

(define-meta-command (import repl . args)
  "import [MODULE ...]
Import modules / List those imported."
  (let ()
    (define (use name)
      (let ((mod (resolve-interface name)))
        (if mod
            (module-use! (current-module) mod)
            (format #t "No such module: ~A~%" name))))
    (if (null? args)
        (for-each puts (map module-name (module-uses (current-module))))
        (for-each use args))))

(define-meta-command (load repl file)
  "load FILE
Load a file in the current module."
  (load (->string file)))

(define-meta-command (reload repl . args)
  "reload [MODULE]
Reload the given module, or the current module if none was given."
  (pmatch args
    (() (reload-module (current-module)))
    ((,mod-name) (guard (list? mod-name))
     (reload-module (resolve-module mod-name)))
    (,mod-name (reload-module (resolve-module mod-name)))))

(define-meta-command (binding repl)
  "binding
List current bindings."
  (module-for-each (lambda (k v) (format #t "~23A ~A\n" k v))
                   (current-module)))

(define-meta-command (in repl module command-or-expression . args)
  "in MODULE COMMAND-OR-EXPRESSION
Evaluate an expression or command in the context of module."
  (let ((m (resolve-module module #:ensure #f)))
    (if m
        (pmatch command-or-expression
          (('unquote ,command) (guard (lookup-command command))
           (save-module-excursion
            (lambda ()
              (set-current-module m)
              (apply (command-procedure (lookup-command command)) repl args))))
          (,expression
           (guard (null? args))
           (repl-print repl (eval expression m)))
          (else
           (format #t "Invalid arguments to `in': expected a single expression or a command.\n")))
        (format #t "No such module: ~s\n" module))))


;;;
;;; Language commands
;;;

(define-meta-command (language repl name)
  "language LANGUAGE
Change languages."
  (let ((lang (lookup-language name))
        (cur (repl-language repl)))
    (format #t "Happy hacking with ~a!  To switch back, type `,L ~a'.\n"
            (language-title lang) (language-name cur))
    (current-language lang)
    (set! (repl-language repl) lang)))


;;;
;;; Compile commands
;;;

(define (load-image x)
  (let ((thunk (load-thunk-from-memory x)))
    (find-mapped-elf-image (program-code thunk))))

(define-meta-command (compile repl (form))
  "compile EXP
Generate compiled code."
  (let ((x (repl-compile repl (repl-parse repl form))))
    (cond ((bytevector? x) (disassemble-image (load-image x)))
          (else (repl-print repl x)))))

(define-meta-command (compile-file repl file . opts)
  "compile-file FILE
Compile a file."
  (compile-file (->string file) #:opts opts))

(define-meta-command (expand repl (form))
  "expand EXP
Expand any macros in a form."
  (let ((x (repl-expand repl (repl-parse repl form))))
    (run-hook before-print-hook x)
    (pp x)))

(define-meta-command (optimize repl (form))
  "optimize EXP
Run the optimizer on a piece of code and print the result."
  (let ((x (repl-optimize repl (repl-parse repl form))))
    (run-hook before-print-hook x)
    (pp x)))

(define-meta-command (disassemble repl (form))
  "disassemble EXP
Disassemble a compiled procedure."
  (let ((obj (repl-eval repl (repl-parse repl form))))
    (cond
     ((program? obj)
      (disassemble-program obj))
     ((bytevector? obj)
      (disassemble-image (load-image obj)))
     (else
      (format #t
              "Argument to ,disassemble not a procedure or a bytevector: ~a~%"
              obj)))))

(define-meta-command (disassemble-file repl file)
  "disassemble-file FILE
Disassemble a file."
  (disassemble-file (->string file)))


;;;
;;; Profile commands
;;;

(define-meta-command (time repl (form))
  "time EXP
Time execution."
  (let* ((gc-start (gc-run-time))
	 (real-start (get-internal-real-time))
	 (run-start (get-internal-run-time))
	 (result (repl-eval repl (repl-parse repl form)))
	 (run-end (get-internal-run-time))
	 (real-end (get-internal-real-time))
	 (gc-end (gc-run-time)))
    (define (diff start end)
      (/ (- end start) 1.0 internal-time-units-per-second))
    (repl-print repl result)
    (format #t ";; ~,6Fs real time, ~,6Fs run time.  ~,6Fs spent in GC.\n"
            (diff real-start real-end)
            (diff run-start run-end)
            (diff gc-start gc-end))
    result))

(define-meta-command (profile repl (form) . opts)
  "profile EXP
Profile execution."
  ;; FIXME opts
  (apply statprof
         (repl-prepare-eval-thunk repl (repl-parse repl form))
         opts))

(define-meta-command (trace repl (form) . opts)
  "trace EXP
Trace execution."
  ;; FIXME: doc options, or somehow deal with them better
  (apply call-with-trace
         (repl-prepare-eval-thunk repl (repl-parse repl form))
         (cons* #:width (terminal-width) opts)))


;;;
;;; Debug commands
;;;

(define-syntax define-stack-command
  (lambda (x)
    (syntax-case x ()
      ((_ (name repl . args) docstring body body* ...)
       #`(define-meta-command (name repl . args)
           docstring
           (let ((debug (repl-debug repl)))
             (if debug
                 (letrec-syntax
                     ((#,(datum->syntax #'repl 'frames)
                       (identifier-syntax (debug-frames debug)))
                      (#,(datum->syntax #'repl 'message)
                       (identifier-syntax (debug-error-message debug)))
                      (#,(datum->syntax #'repl 'index)
                       (identifier-syntax
                        (id (debug-index debug))
                        ((set! id exp) (set! (debug-index debug) exp))))
                      (#,(datum->syntax #'repl 'cur)
                       (identifier-syntax
                        (vector-ref #,(datum->syntax #'repl 'frames)
                                    #,(datum->syntax #'repl 'index)))))
                   body body* ...)
                 (format #t "Nothing to debug.~%"))))))))

(define-stack-command (backtrace repl #:optional count
                                 #:key (width (terminal-width)) full?)
  "backtrace [COUNT] [#:width W] [#:full? F]
Print a backtrace.

Print a backtrace of all stack frames, or innermost COUNT frames.
If COUNT is negative, the last COUNT frames will be shown."
  (print-frames frames
                #:count count
                #:width width
                #:full? full?))

(define-stack-command (up repl #:optional (count 1))
  "up [COUNT]
Select a calling stack frame.

Select and print stack frames that called this one.
An argument says how many frames up to go."
  (cond
   ((or (not (integer? count)) (<= count 0))
    (format #t "Invalid argument to `up': expected a positive integer for COUNT.~%"))
   ((>= (+ count index) (vector-length frames))
    (cond
     ((= index (1- (vector-length frames)))
      (format #t "Already at outermost frame.\n"))
     (else
      (set! index (1- (vector-length frames)))
      (print-frame cur #:index index))))
   (else
    (set! index (+ count index))
    (print-frame cur #:index index))))

(define-stack-command (down repl #:optional (count 1))
  "down [COUNT]
Select a called stack frame.

Select and print stack frames called by this one.
An argument says how many frames down to go."
  (cond
   ((or (not (integer? count)) (<= count 0))
    (format #t "Invalid argument to `down': expected a positive integer for COUNT.~%"))
   ((< (- index count) 0)
    (cond
     ((zero? index)
      (format #t "Already at innermost frame.\n"))
     (else
      (set! index 0)
      (print-frame cur #:index index))))
   (else
    (set! index (- index count))
    (print-frame cur #:index index))))

(define-stack-command (frame repl #:optional idx)
  "frame [IDX]
Show a frame.

Show the selected frame.
With an argument, select a frame by index, then show it."
  (cond
   (idx
    (cond
     ((or (not (integer? idx)) (< idx 0))
      (format #t "Invalid argument to `frame': expected a non-negative integer for IDX.~%"))
     ((< idx (vector-length frames))
      (set! index idx)
      (print-frame cur #:index index))
     (else
      (format #t "No such frame.~%"))))
   (else (print-frame cur #:index index))))

(define-stack-command (locals repl #:key (width (terminal-width)))
  "locals
Show local variables.

Show locally-bound variables in the selected frame."
  (print-locals cur #:width width))

(define-stack-command (error-message repl)
  "error-message
Show error message.

Display the message associated with the error that started the current
debugging REPL."
  (format #t "~a~%" (if (string? message) message "No error message")))

(define-meta-command (break repl (form))
  "break PROCEDURE
Break on calls to PROCEDURE.

Starts a recursive prompt when PROCEDURE is called."
  (let ((proc (repl-eval repl (repl-parse repl form))))
    (if (not (procedure? proc))
        (error "Not a procedure: ~a" proc)
        (let ((idx (add-trap-at-procedure-call! proc)))
          (format #t "Trap ~a: ~a.~%" idx (trap-name idx))))))

(define-meta-command (break-at-source repl file line)
  "break-at-source FILE LINE
Break when control reaches the given source location.

Starts a recursive prompt when control reaches line LINE of file FILE.
Note that the given source location must be inside a procedure."
  (let ((file (if (symbol? file) (symbol->string file) file)))
    (let ((idx (add-trap-at-source-location! file line)))
      (format #t "Trap ~a: ~a.~%" idx (trap-name idx)))))

(define (repl-pop-continuation-resumer repl msg)
  ;; Capture the dynamic environment with this prompt thing. The result
  ;; is a procedure that takes a frame and number of values returned.
  (% (call-with-values
         (lambda ()
           (abort
            (lambda (k)
              ;; Call frame->stack-vector before reinstating the
              ;; continuation, so that we catch the %stacks fluid at
              ;; the time of capture.
              (lambda (frame . values)
                (k frame
                   (frame->stack-vector
                    (frame-previous frame))
                   values)))))
       (lambda (from stack values)
         (format #t "~a~%" msg)
         (if (null? values)
             (format #t "No return values.~%")
             (begin
               (format #t "Return values:~%")
               (for-each (lambda (x) (repl-print repl x)) values)))
         ((module-ref (resolve-interface '(system repl repl)) 'start-repl)
          #:debug (make-debug stack 0 msg))))))

(define-stack-command (finish repl)
  "finish
Run until the current frame finishes.

Resume execution, breaking when the current frame finishes."
  (let ((handler (repl-pop-continuation-resumer
                  repl (format #f "Return from ~a" cur))))
    (add-ephemeral-trap-at-frame-finish! cur handler)
    (throw 'quit)))

(define (repl-next-resumer msg)
  ;; Capture the dynamic environment with this prompt thing. The
  ;; result is a procedure that takes a frame.
  (% (let ((stack (abort
                   (lambda (k)
                     ;; Call frame->stack-vector before reinstating the
                     ;; continuation, so that we catch the %stacks fluid
                     ;; at the time of capture.
                     (lambda (frame)
                       (k (frame->stack-vector frame)))))))
       (format #t "~a~%" msg)
       ((module-ref (resolve-interface '(system repl repl)) 'start-repl)
        #:debug (make-debug stack 0 msg)))))

(define-stack-command (step repl)
  "step
Step until control reaches a different source location.

Step until control reaches a different source location."
  (let ((msg (format #f "Step into ~a" cur)))
    (add-ephemeral-stepping-trap! cur (repl-next-resumer msg)
                                  #:into? #t #:instruction? #f)
    (throw 'quit)))

(define-stack-command (step-instruction repl)
  "step-instruction
Step until control reaches a different instruction.

Step until control reaches a different VM instruction."
  (let ((msg (format #f "Step into ~a" cur)))
    (add-ephemeral-stepping-trap! cur (repl-next-resumer msg)
                                  #:into? #t #:instruction? #t)
    (throw 'quit)))

(define-stack-command (next repl)
  "next
Step until control reaches a different source location in the current frame.

Step until control reaches a different source location in the current frame."
  (let ((msg (format #f "Step into ~a" cur)))
    (add-ephemeral-stepping-trap! cur (repl-next-resumer msg)
                                  #:into? #f #:instruction? #f)
    (throw 'quit)))

(define-stack-command (next-instruction repl)
  "next-instruction
Step until control reaches a different instruction in the current frame.

Step until control reaches a different VM instruction in the current frame."
  (let ((msg (format #f "Step into ~a" cur)))
    (add-ephemeral-stepping-trap! cur (repl-next-resumer msg)
                                  #:into? #f #:instruction? #t)
    (throw 'quit)))

(define-meta-command (tracepoint repl (form))
  "tracepoint PROCEDURE
Add a tracepoint to PROCEDURE.

A tracepoint will print out the procedure and its arguments, when it is
called, and its return value(s) when it returns."
  (let ((proc (repl-eval repl (repl-parse repl form))))
    (if (not (procedure? proc))
        (error "Not a procedure: ~a" proc)
        (let ((idx (add-trace-at-procedure-call! proc)))
          (format #t "Trap ~a: ~a.~%" idx (trap-name idx))))))

(define-meta-command (traps repl)
  "traps
Show the set of currently attached traps.

Show the set of currently attached traps (breakpoints and tracepoints)."
  (let ((traps (list-traps)))
    (if (null? traps)
        (format #t "No traps set.~%")
        (for-each (lambda (idx)
                    (format #t "  ~a: ~a~a~%"
                            idx (trap-name idx)
                            (if (trap-enabled? idx) "" " (disabled)")))
                  traps))))

(define-meta-command (delete repl idx)
  "delete IDX
Delete a trap.

Delete a trap."
  (if (not (integer? idx))
      (error "expected a trap index (a non-negative integer)" idx)
      (delete-trap! idx)))

(define-meta-command (disable repl idx)
  "disable IDX
Disable a trap.

Disable a trap."
  (if (not (integer? idx))
      (error "expected a trap index (a non-negative integer)" idx)
      (disable-trap! idx)))

(define-meta-command (enable repl idx)
  "enable IDX
Enable a trap.

Enable a trap."
  (if (not (integer? idx))
      (error "expected a trap index (a non-negative integer)" idx)
      (enable-trap! idx)))

(define-stack-command (registers repl)
  "registers
Print registers.

Print the registers of the current frame."
  (print-registers cur))

(define-meta-command (width repl #:optional x)
  "width [X]
Set debug output width.

Set the number of screen columns in the output from `backtrace' and
`locals'."
  (terminal-width x)
  (format #t "Set screen width to ~a columns.~%" (terminal-width)))



;;;
;;; Inspection commands
;;;

(define-meta-command (inspect repl (form))
  "inspect EXP
Inspect the result(s) of evaluating EXP."
  (call-with-values (repl-prepare-eval-thunk repl (repl-parse repl form))
    (lambda args
      (for-each %inspect args))))

(define-meta-command (pretty-print repl (form))
  "pretty-print EXP
Pretty-print the result(s) of evaluating EXP."
  (call-with-values (repl-prepare-eval-thunk repl (repl-parse repl form))
    (lambda args
      (for-each
       (lambda (x)
         (run-hook before-print-hook x)
         (pp x))
       args))))


;;;
;;; System commands
;;;

(define-meta-command (gc repl)
  "gc
Garbage collection."
  (gc))

(define-meta-command (statistics repl)
  "statistics
Display statistics."
  (let ((this-tms (times))
	(this-gcs (gc-stats))
	(last-tms (repl-tm-stats repl))
	(last-gcs (repl-gc-stats repl)))
    ;; GC times
    (let ((this-times  (assq-ref this-gcs 'gc-times))
	  (last-times  (assq-ref last-gcs 'gc-times)))
      (display-diff-stat "GC times:" #t this-times last-times "times")
      (newline))
    ;; Memory size
    (let ((this-heap  (assq-ref this-gcs 'heap-size))
	  (this-free   (assq-ref this-gcs 'heap-free-size)))
      (display-stat-title "Memory size:" "current" "limit")
      (display-stat "heap" #f (- this-heap this-free) this-heap "bytes")
      (newline))
    ;; Cells collected
    (let ((this-alloc (assq-ref this-gcs 'heap-total-allocated))
	  (last-alloc (assq-ref last-gcs 'heap-total-allocated)))
      (display-stat-title "Bytes allocated:" "diff" "total")
      (display-diff-stat "allocated" #f this-alloc last-alloc "bytes")
      (newline))
    ;; GC time taken
    (let ((this-total (assq-ref this-gcs 'gc-time-taken))
	  (last-total (assq-ref last-gcs 'gc-time-taken)))
      (display-stat-title "GC time taken:" "diff" "total")
      (display-time-stat "total" this-total last-total)
      (newline))
    ;; Process time spent
    (let ((this-utime  (tms:utime this-tms))
	  (last-utime  (tms:utime last-tms))
	  (this-stime  (tms:stime this-tms))
	  (last-stime  (tms:stime last-tms))
	  (this-cutime (tms:cutime this-tms))
	  (last-cutime (tms:cutime last-tms))
	  (this-cstime (tms:cstime this-tms))
	  (last-cstime (tms:cstime last-tms)))
      (display-stat-title "Process time spent:" "diff" "total")
      (display-time-stat "user" this-utime last-utime)
      (display-time-stat "system" this-stime last-stime)
      (display-time-stat "child user" this-cutime last-cutime)
      (display-time-stat "child system" this-cstime last-cstime)
      (newline))
    ;; Save statistics
    ;; Save statistics
    (set! (repl-tm-stats repl) this-tms)
    (set! (repl-gc-stats repl) this-gcs)))

(define (display-stat title flag field1 field2 unit)
  (let ((fmt (format #f "~~20~AA ~~10@A /~~10@A ~~A~~%" (if flag "" "@"))))
    (format #t fmt title field1 field2 unit)))

(define (display-stat-title title field1 field2)
  (display-stat title #t field1 field2 ""))

(define (display-diff-stat title flag this last unit)
  (display-stat title flag (- this last) this unit))

(define (display-time-stat title this last)
  (define (conv num)
    (format #f "~10,2F" (exact->inexact (/ num internal-time-units-per-second))))
  (display-stat title #f (conv (- this last)) (conv this) "s"))

(define (display-mips-stat title this-time this-clock last-time last-clock)
  (define (mips time clock)
    (if (= time 0) "----" (format #f "~10,2F" (/ clock time 1000000.0))))
  (display-stat title #f
		(mips (- this-time last-time) (- this-clock last-clock))
		(mips this-time this-clock) "mips"))
