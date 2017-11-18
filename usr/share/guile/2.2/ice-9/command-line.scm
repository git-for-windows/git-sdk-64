;;; Parsing Guile's command-line

;;; Copyright (C) 1994-1998, 2000-2017 Free Software Foundation, Inc.

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

;;; Code:

;;;
;;; Please be careful not to load up other modules in this file, unless
;;; they are explicitly requested.  Loading modules currently imposes a
;;; speed penalty of a few stats, an mmap, and some allocation, which
;;; can range from 1 to 20ms, depending on the state of your disk cache.
;;; Since `compile-shell-switches' is called even for the most transient
;;; of command-line programs, we need to keep it lean.
;;;
;;; Generally speaking, the goal is for Guile to boot and execute simple
;;; expressions like "1" within 20ms or less, measured using system time
;;; from the time of the `guile' invocation to exit.
;;;

(define-module (ice-9 command-line)
  #:autoload (system vm vm) (set-default-vm-engine! set-vm-engine!)
  #:export (compile-shell-switches
            version-etc
            *GPLv3+*
            *LGPLv3+*
            emit-bug-reporting-address))

;; An initial stab at i18n.
(define _ gettext)

(define *GPLv3+*
  (_ "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law."))

(define *LGPLv3+*
  (_ "License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law."))

;; Display the --version information in the
;; standard way: command and package names, package version, followed
;; by a short license notice and a list of up to 10 author names.
;; If COMMAND_NAME is NULL, the PACKAGE is asumed to be the name of
;; the program.  The formats are therefore:
;; PACKAGE VERSION
;; or
;; COMMAND_NAME (PACKAGE) VERSION.
;;
;; Based on the version-etc gnulib module.
;;
(define* (version-etc package version #:key
                      (port (current-output-port))
                      ;; FIXME: authors
                      (copyright-year 2017)
                      (copyright-holder "Free Software Foundation, Inc.")
                      (copyright (format #f "Copyright (C) ~a ~a"
                                         copyright-year copyright-holder))
                      (license *GPLv3+*)
                      command-name
                      packager packager-version)
  (if command-name
      (format port "~a (~a) ~a\n" command-name package version)
      (format port "~a ~a\n" package version))

  (if packager
      (if packager-version
          (format port (_ "Packaged by ~a (~a)\n") packager packager-version)
          (format port (_ "Packaged by ~a\n") packager)))
  
  (display copyright port)
  (newline port)
  (newline port)
  (display license port)
  (newline port))


;; Display the usual `Report bugs to' stanza.
;;
(define* (emit-bug-reporting-address package bug-address #:key
                                     (port (current-output-port))
                                     (url (string-append
                                           "http://www.gnu.org/software/"
                                           package
                                           "/"))
                                     packager packager-bug-address)
  (format port (_ "\nReport bugs to: ~a\n") bug-address)
  (if (and packager packager-bug-address)
      (format port (_ "Report ~a bugs to: ~a\n") packager packager-bug-address))
  (format port (_ "~a home page: <~a>\n") package url)
  (format port
          (_ "General help using GNU software: <http://www.gnu.org/gethelp/>\n")))

(define *usage*
  (_ "Evaluate code with Guile, interactively or from a script.

  [-s] FILE      load source code from FILE, and exit
  -c EXPR        evalute expression EXPR, and exit
  --             stop scanning arguments; run interactively

The above switches stop argument processing, and pass all
remaining arguments as the value of (command-line).
If FILE begins with `-' the -s switch is mandatory.

  -L DIRECTORY   add DIRECTORY to the front of the module load path
  -C DIRECTORY   like -L, but for compiled files
  -x EXTENSION   add EXTENSION to the front of the load extensions
  -l FILE        load source code from FILE
  -e FUNCTION    after reading script, apply FUNCTION to
                 command line arguments
  --language=LANG  change language; default: scheme
  -ds            do -s script at this point
  --debug        start with the \"debugging\" VM engine
  --no-debug     start with the normal VM engine (backtraces but
                 no breakpoints); default is --debug for interactive
                 use, but not for `-s' and `-c'.
  --auto-compile compile source files automatically
  --fresh-auto-compile  invalidate auto-compilation cache
  --no-auto-compile  disable automatic source file compilation;
                 default is to enable auto-compilation of source
                 files.
  --listen[=P]   listen on a local port or a path for REPL clients;
                 if P is not given, the default is local port 37146
  -q             inhibit loading of user init file
  --use-srfi=LS  load SRFI modules for the SRFIs in LS,
                 which is a list of numbers like \"2,13,14\"
  -h, --help     display this help and exit
  -v, --version  display version information and exit
  \\              read arguments from following script lines"))


(define* (shell-usage name fatal? #:optional fmt . args)
  (let ((port (if fatal?
                  (current-error-port)
                  (current-output-port))))
    (when fmt
      (apply format port fmt args)
      (newline port))

    (format port (_ "Usage: ~a [OPTION]... [FILE]...\n") name)
    (display *usage* port)
    (newline port)

    (emit-bug-reporting-address
     "GNU Guile" "bug-guile@gnu.org"
     #:port port
     #:url "http://www.gnu.org/software/guile/"
     #:packager (assq-ref %guile-build-info 'packager)
     #:packager-bug-address
     (assq-ref %guile-build-info 'packager-bug-address))

    (if fatal?
        (exit 1))))

;; Try to avoid loading (ice-9 eval-string) and (system base compile) if
;; possible.
(define (eval-string/lang str)
  (case (current-language)
    ((scheme)
     (call-with-input-string
      str
      (lambda (port)
        (let lp ()
          (let ((exp (read port)))
            (if (not (eof-object? exp))
                (begin
                  (eval exp (current-module))
                  (lp))))))))
    (else
     ((module-ref (resolve-module '(ice-9 eval-string)) 'eval-string) str))))

(define (load/lang f)
  (case (current-language)
    ((scheme)
     (load-in-vicinity (getcwd) f))
    (else
     ((module-ref (resolve-module '(system base compile)) 'compile-file)
      f #:to 'value))))

(define* (compile-shell-switches args #:optional (usage-name "guile"))
  (let ((arg0 "guile")
        (script-cell #f)
        (entry-point #f)
        (user-load-path '())
        (user-load-compiled-path '())
        (user-extensions '())
        (interactive? #t)
        (inhibit-user-init? #f)
        (turn-on-debugging? #f)
        (turn-off-debugging? #f))

    (define (error fmt . args)
      (apply shell-usage usage-name #t
             (string-append "error: " fmt "~%") args))

    (define (parse args out)
      (cond
       ((null? args)
        (finish args out))
       (else
        (let ((arg (car args))
              (args (cdr args)))
          (cond
           ((not (string-prefix? "-" arg)) ; foo
            ;; If we specified the -ds option, script-cell is the cdr of
            ;; an expression like (load #f).  We replace the car (i.e.,
            ;; the #f) with the script name.
            (set! arg0 arg)
            (set! interactive? #f)
            (if script-cell
                (begin
                  (set-car! script-cell arg0)
                  (finish args out))
                (finish args
                        (cons `((@@ (ice-9 command-line) load/lang) ,arg0)
                              out))))

           ((string=? arg "-s")         ; foo
            (if (null? args)
                (error "missing argument to `-s' switch"))
            (set! arg0 (car args))
            (set! interactive? #f)
            (if script-cell
                (begin
                  (set-car! script-cell arg0)
                  (finish (cdr args) out))
                (finish (cdr args)
                        (cons `((@@ (ice-9 command-line) load/lang) ,arg0)
                              out))))
           
           ((string=? arg "-c")         ; evaluate expr
            (if (null? args)
                (error "missing argument to `-c' switch"))
            (set! interactive? #f)
            (finish (cdr args)
                    (cons `((@@ (ice-9 command-line) eval-string/lang)
                            ,(car args))
                          out)))

           ((string=? arg "--")         ; end args go interactive
            (finish args out))

           ((string=? arg "-l")         ; load a file
            (if (null? args)
                (error "missing argument to `-l' switch"))
            (parse (cdr args)
                   (cons `((@@ (ice-9 command-line) load/lang) ,(car args))
                         out)))

           ((string=? arg "-L")         ; add to %load-path
            (if (null? args)
                (error "missing argument to `-L' switch"))
            (set! user-load-path (cons (car args) user-load-path))
            (parse (cdr args)
                   out))

           ((string=? arg "-C")         ; add to %load-compiled-path
            (if (null? args)
                (error "missing argument to `-C' switch"))
            (set! user-load-compiled-path
                  (cons (car args) user-load-compiled-path))
            (parse (cdr args)
                   out))

           ((string=? arg "-x")         ; add to %load-extensions
            (if (null? args)
                (error "missing argument to `-x' switch"))
            (set! user-extensions (cons (car args) user-extensions))
            (parse (cdr args)
                   out))

           ((string=? arg "-e")         ; entry point
            (if (null? args)
                (error "missing argument to `-e' switch"))
            (let* ((port (open-input-string (car args)))
                   (arg1 (read port))
                   (arg2 (read port)))
              ;; Recognize syntax of certain versions of guile 1.4 and
              ;; transform to (@ MODULE-NAME FUNC).
              (set! entry-point
                    (cond
                     ((not (eof-object? arg2))
                      `(@ ,arg1 ,arg2))
                     ((and (pair? arg1)
                           (not (memq (car arg1) '(@ @@)))
                           (and-map symbol? arg1))
                      `(@ ,arg1 main))
                     (else
                      arg1))))
            (parse (cdr args)
                   out))

           ((string-prefix? "--language=" arg) ; language
            (parse args
                   (cons `(current-language
                           ',(string->symbol
                              (substring arg (string-length "--language="))))
                         out)))

           ((string=? "--language" arg) ; language
            (when (null? args)
              (error "missing argument to `--language' option"))
            (parse (cdr args)
                   (cons `(current-language ',(string->symbol (car args)))
                         out)))

           ((string=? arg "-ds")        ; do script here
            ;; We put a dummy "load" expression, and let the -s put the
            ;; filename in.
            (when script-cell
              (error "the -ds switch may only be specified once"))
            (set! script-cell (list #f))
            (parse args
                   (acons '(@@ (ice-9 command-line) load/lang)
                          script-cell
                          out)))

           ((string=? arg "--debug")
            (set! turn-on-debugging? #t)
            (set! turn-off-debugging? #f)
            (parse args out))

           ((string=? arg "--no-debug")
            (set! turn-off-debugging? #t)
            (set! turn-on-debugging? #f)
            (parse args out))

           ;; Do auto-compile on/off now, because the form itself might
           ;; need this decision.
           ((string=? arg "--auto-compile")
            (set! %load-should-auto-compile #t)
            (parse args out))

           ((string=? arg "--fresh-auto-compile")
            (set! %load-should-auto-compile #t)
            (set! %fresh-auto-compile #t)
            (parse args out))

           ((string=? arg "--no-auto-compile")
            (set! %load-should-auto-compile #f)
            (parse args out))

           ((string=? arg "-q")         ; don't load user init
            (set! inhibit-user-init? #t)
            (parse args out))

           ((string-prefix? "--use-srfi=" arg)
            (let ((srfis (map (lambda (x)
                                (let ((n (string->number x)))
                                  (if (and n (exact? n) (integer? n) (>= n 0))
                                      n
                                      (error "invalid SRFI specification"))))
                              (string-split (substring arg 11) #\,))))
              (if (null? srfis)
                  (error "invalid SRFI specification"))
              (parse args
                     (cons `(use-srfis ',srfis) out))))

           ((string=? arg "--listen")   ; start a repl server
            (parse args
                   (cons '((@@ (system repl server) spawn-server)) out)))
           
           ((string-prefix? "--listen=" arg) ; start a repl server
            (parse
             args
             (cons
              (let ((where (substring arg 9)))
                (cond
                 ((string->number where) ; --listen=PORT
                  => (lambda (port)
                       (if (and (integer? port) (exact? port) (>= port 0))
                           `((@@ (system repl server) spawn-server)
                             ((@@ (system repl server) make-tcp-server-socket) #:port ,port))
                           (error "invalid port for --listen"))))
                 ((string-prefix? "/" where) ; --listen=/PATH/TO/SOCKET
                  `((@@ (system repl server) spawn-server)
                    ((@@ (system repl server) make-unix-domain-server-socket) #:path ,where)))
                 (else
                  (error "unknown argument to --listen"))))
              out)))

           ((or (string=? arg "-h") (string=? arg "--help"))
            (shell-usage usage-name #f)
            (exit 0))

           ((or (string=? arg "-v") (string=? arg "--version"))
            (version-etc "GNU Guile" (version)
                         #:license *LGPLv3+*
                         #:command-name "guile"
                         #:packager (assq-ref %guile-build-info 'packager)
                         #:packager-version
                         (assq-ref %guile-build-info 'packager-version))
            (exit 0))

           (else
            (error "unrecognized switch ~a" arg)))))))

    (define (finish args out)
      ;; Check to make sure the -ds got a -s.
      (when (and script-cell (not (car script-cell)))
        (error "the `-ds' switch requires the use of `-s' as well"))

      ;; Make any remaining arguments available to the
      ;; script/command/whatever.
      (set-program-arguments (cons arg0 args))

      ;; If debugging was requested, or we are interactive and debugging
      ;; was not explicitly turned off, use the debug engine.
      (if (or turn-on-debugging?
              (and interactive? (not turn-off-debugging?)))
          (begin
            (set-default-vm-engine! 'debug)
            (set-vm-engine! 'debug)))
      
      ;; Return this value.
      `(;; It would be nice not to load up (ice-9 control), but the
        ;; default-prompt-handler is nontrivial.
        (@ (ice-9 control) %)
        (begin
          ;; If we didn't end with a -c or a -s and didn't supply a -q, load
          ;; the user's customization file.
          ,@(if (and interactive? (not inhibit-user-init?))
                '((load-user-init))
                '())

          ;; Use-specified extensions.
          ,@(map (lambda (ext)
                   `(set! %load-extensions (cons ,ext %load-extensions)))
                 user-extensions)

          ;; Add the user-specified load paths here, so they won't be in
          ;; effect during the loading of the user's customization file.
          ,@(map (lambda (path)
                   `(set! %load-path (cons ,path %load-path)))
                 user-load-path)
          ,@(map (lambda (path)
                   `(set! %load-compiled-path
                          (cons ,path %load-compiled-path)))
                 user-load-compiled-path)

          ;; Put accumulated actions in their correct order.
          ,@(reverse! out)

          ;; Handle the `-e' switch, if it was specified.
          ,@(if entry-point
                `((,entry-point (command-line)))
                '())
          ,(if interactive?
               ;; If we didn't end with a -c or a -s, start the
               ;; repl.
               '((@ (ice-9 top-repl) top-repl))
               ;; Otherwise, after doing all the other actions
               ;; prescribed by the command line, quit.
               '(quit)))))

      (if (pair? args)
          (begin
            (set! arg0 (car args))
            (let ((slash (string-rindex arg0 #\/)))
              (set! usage-name
                    (if slash (substring arg0 (1+ slash)) arg0)))
            (parse (cdr args) '()))
          (parse args '()))))
