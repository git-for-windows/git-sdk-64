;;;; Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2014
;;;; Free Software Foundation, Inc.
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

(define-module (ice-9 scm-style-repl)
  #:use-module (ice-9 save-stack)

  #:export (scm-repl-silent
            scm-repl-print-unspecified
            scm-repl-verbose
            scm-repl-prompt
            assert-repl-silence
            assert-repl-print-unspecified
            assert-repl-verbosity

            default-pre-unwind-handler
            bad-throw
            error-catching-loop
            error-catching-repl
            scm-style-repl
            handle-system-error))

(define scm-repl-silent #f)
(define (assert-repl-silence v) (set! scm-repl-silent v))

(define scm-repl-print-unspecified #f)
(define (assert-repl-print-unspecified v) (set! scm-repl-print-unspecified v))

(define scm-repl-verbose #f)
(define (assert-repl-verbosity v) (set! scm-repl-verbose v))

(define scm-repl-prompt "guile> ")



;; bad-throw is the hook that is called upon a throw to a an unhandled
;; key (unless the throw has four arguments, in which case
;; it's usually interpreted as an error throw.)
;; If the key has a default handler (a throw-handler-default property),
;; it is applied to the throw.
;;
(define (bad-throw key . args)
  (let ((default (symbol-property key 'throw-handler-default)))
    (or (and default (apply default key args))
        (apply error "unhandled-exception:" key args))))



(define (default-pre-unwind-handler key . args)
  ;; Narrow by two more frames: this one, and the throw handler.
  (save-stack 2)
  (apply throw key args))



(define has-shown-debugger-hint? #f)

(define (error-catching-loop thunk)
  (let ((status #f)
        (interactive #t))
    (define (loop first)
      (let ((next
             (catch #t

                    (lambda ()
                      (call-with-unblocked-asyncs
                       (lambda ()
                         (first)

                         ;; This line is needed because mark
                         ;; doesn't do closures quite right.
                         ;; Unreferenced locals should be
                         ;; collected.
                         (set! first #f)
                         (let loop ((v (thunk)))
                           (loop (thunk)))
                         #f)))

                    (lambda (key . args)
                      (case key
                        ((quit)
                         (set! status args)
                         #f)

                        ((switch-repl)
                         (apply throw 'switch-repl args))

                        ((abort)
                         ;; This is one of the closures that require
                         ;; (set! first #f) above
                         ;;
                         (lambda ()
                           (run-hook abort-hook)
                           (force-output (current-output-port))
                           (display "ABORT: "  (current-error-port))
                           (write args (current-error-port))
                           (newline (current-error-port))
                           (if interactive
                               (begin
                                 (if (and
                                      (not has-shown-debugger-hint?)
                                      (not (memq 'backtrace
                                                 (debug-options-interface)))
                                      (stack? (fluid-ref the-last-stack)))
                                     (begin
                                       (newline (current-error-port))
                                       (display
                                        "Type \"(backtrace)\" to get more information or \"(debug)\" to enter the debugger.\n"
                                        (current-error-port))
                                       (set! has-shown-debugger-hint? #t)))
                                 (force-output (current-error-port)))
                               (begin
                                 (primitive-exit 1)))
                           (set! stack-saved? #f)))

                        (else
                         ;; This is the other cons-leak closure...
                         (lambda ()
                           (cond ((= (length args) 4)
                                  (apply handle-system-error key args))
                                 (else
                                  (apply bad-throw key args)))))))

                    default-pre-unwind-handler)))

        (if next (loop next) status)))
    (set! ensure-batch-mode! (lambda ()
                               (set! interactive #f)
                               (restore-signals)))
    (set! batch-mode? (lambda () (not interactive)))
    (call-with-blocked-asyncs
     (lambda () (loop (lambda () #t))))))

(define (error-catching-repl r e p)
  (error-catching-loop
   (lambda ()
     (call-with-values (lambda () (e (r)))
       (lambda the-values (for-each p the-values))))))

(define (scm-style-repl)
  (letrec (
           (start-gc-rt #f)
           (start-rt #f)
           (repl-report-start-timing (lambda ()
                                       (set! start-gc-rt (gc-run-time))
                                       (set! start-rt (get-internal-run-time))))
           (repl-report (lambda ()
                          (display ";;; ")
                          (display (inexact->exact
                                    (* 1000 (/ (- (get-internal-run-time) start-rt)
                                               internal-time-units-per-second))))
                          (display "  msec  (")
                          (display  (inexact->exact
                                     (* 1000 (/ (- (gc-run-time) start-gc-rt)
                                                internal-time-units-per-second))))
                          (display " msec in gc)\n")))

           (consume-trailing-whitespace
            (lambda ()
              (let ((ch (peek-char)))
                (cond
                 ((eof-object? ch))
                 ((or (char=? ch #\space) (char=? ch #\tab))
                  (read-char)
                  (consume-trailing-whitespace))
                 ((char=? ch #\newline)
                  (read-char))))))
           (-read (lambda ()
                    (let ((val
                           (let ((prompt (cond ((string? scm-repl-prompt)
                                                scm-repl-prompt)
                                               ((thunk? scm-repl-prompt)
                                                (scm-repl-prompt))
                                               (scm-repl-prompt "> ")
                                               (else ""))))
                             (repl-reader prompt))))

                      ;; As described in R4RS, the READ procedure updates the
                      ;; port to point to the first character past the end of
                      ;; the external representation of the object.  This
                      ;; means that it doesn't consume the newline typically
                      ;; found after an expression.  This means that, when
                      ;; debugging Guile with GDB, GDB gets the newline, which
                      ;; it often interprets as a "continue" command, making
                      ;; breakpoints kind of useless.  So, consume any
                      ;; trailing newline here, as well as any whitespace
                      ;; before it.
                      ;; But not if EOF, for control-D.
                      (if (not (eof-object? val))
                          (consume-trailing-whitespace))
                      (run-hook after-read-hook)
                      (if (eof-object? val)
                          (begin
                            (repl-report-start-timing)
                            (if scm-repl-verbose
                                (begin
                                  (newline)
                                  (display ";;; EOF -- quitting")
                                  (newline)))
                            (quit 0)))
                      val)))

           (-eval (lambda (sourc)
                    (repl-report-start-timing)
                    (run-hook before-eval-hook sourc)
                    (let ((val (start-stack 'repl-stack
                                            ;; If you change this procedure
                                            ;; (primitive-eval), please also
                                            ;; modify the repl-stack case in
                                            ;; save-stack so that stack cutting
                                            ;; continues to work.
                                            (primitive-eval sourc))))
                      (run-hook after-eval-hook sourc)
                      val)))


           (-print (let ((maybe-print (lambda (result)
                                        (if (or scm-repl-print-unspecified
                                                (not (unspecified? result)))
                                            (begin
                                              (write result)
                                              (newline))))))
                     (lambda (result)
                       (if (not scm-repl-silent)
                           (begin
                             (run-hook before-print-hook result)
                             (maybe-print result)
                             (run-hook after-print-hook result)
                             (if scm-repl-verbose
                                 (repl-report))
                             (force-output))))))

           (-quit (lambda (args)
                    (if scm-repl-verbose
                        (begin
                          (display ";;; QUIT executed, repl exitting")
                          (newline)
                          (repl-report)))
                    args)))

    (let ((status (error-catching-repl -read
                                       -eval
                                       -print)))
      (-quit status))))

(define (handle-system-error key . args)
  (let ((cep (current-error-port)))
    (cond ((not (stack? (fluid-ref the-last-stack))))
          ((memq 'backtrace (debug-options-interface))
           (let ((highlights (if (or (eq? key 'wrong-type-arg)
                                     (eq? key 'out-of-range))
                                 (list-ref args 3)
                                 '())))
             (run-hook before-backtrace-hook)
             (newline cep)
             (display "Backtrace:\n")
             (display-backtrace (fluid-ref the-last-stack) cep
                                #f #f highlights)
             (newline cep)
             (run-hook after-backtrace-hook))))
    (run-hook before-error-hook)
    (apply display-error (fluid-ref the-last-stack) cep args)
    (run-hook after-error-hook)
    (force-output cep)
    (throw 'abort key)))
