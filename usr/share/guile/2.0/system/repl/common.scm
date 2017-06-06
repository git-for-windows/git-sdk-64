;;; Repl common routines

;; Copyright (C) 2001, 2008-2016  Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (system repl common)
  #:use-module (system base syntax)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (system base message)
  #:use-module (system vm program)
  #:autoload (language tree-il optimize) (optimize!)
  #:use-module (ice-9 control)
  #:use-module (ice-9 history)
  #:export (<repl> make-repl repl-language repl-options
            repl-tm-stats repl-gc-stats repl-debug
            repl-welcome repl-prompt
            repl-read repl-compile repl-prepare-eval-thunk repl-eval
            repl-expand repl-optimize
            repl-parse repl-print repl-option-ref repl-option-set!
            repl-default-option-set! repl-default-prompt-set!
            puts ->string user-error
            *warranty* *copying* *version*))

(define *version*
  (format #f "GNU Guile ~A
Copyright (C) 1995-2016 Free Software Foundation, Inc.

Guile comes with ABSOLUTELY NO WARRANTY; for details type `,show w'.
This program is free software, and you are welcome to redistribute it
under certain conditions; type `,show c' for details." (version)))

(define *copying*
"Guile is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

Guile is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program. If not, see
<http://www.gnu.org/licenses/lgpl.html>.")

(define *warranty*
"Guile is distributed WITHOUT ANY WARRANTY. The following
sections from the GNU General Public License, version 3, should
make that clear.

  15. Disclaimer of Warranty.

  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY
OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

  16. Limitation of Liability.

  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.

  17. Interpretation of Sections 15 and 16.

  If the disclaimer of warranty and limitation of liability provided
above cannot be given local legal effect according to their terms,
reviewing courts shall apply local law that most closely approximates
an absolute waiver of all civil liability in connection with the
Program, unless a warranty or assumption of liability accompanies a
copy of the Program in return for a fee.

See <http://www.gnu.org/licenses/lgpl.html>, for more details.")


;;;
;;; Repl type
;;;

(define-record/keywords <repl>
  language options tm-stats gc-stats debug)

(define repl-default-options
  (copy-tree
   `((compile-options ,%auto-compilation-options #f)
     (trace #f #f)
     (interp #f #f)
     (prompt #f ,(lambda (prompt)
                   (cond
                    ((not prompt) #f)
                    ((string? prompt) (lambda (repl) prompt))
                    ((thunk? prompt) (lambda (repl) (prompt)))
                    ((procedure? prompt) prompt)
                    (else (error "Invalid prompt" prompt)))))
     (print #f ,(lambda (print)
                  (cond
                   ((not print) #f)
                   ((procedure? print) print)
                   (else (error "Invalid print procedure" print)))))
     (value-history
      ,(value-history-enabled?)
      ,(lambda (x)
         (if x (enable-value-history!) (disable-value-history!))
         (->bool x)))
     (on-error
      debug
      ,(let ((vals '(debug backtrace report pass)))
         (lambda (x)
           (if (memq x vals)
               x
               (error "Bad on-error value ~a; expected one of ~a" x vals))))))))

(define %make-repl make-repl)
(define* (make-repl lang #:optional debug)
  (%make-repl #:language (if (language? lang)
                             lang
                             (lookup-language lang))
              #:options (copy-tree repl-default-options)
              #:tm-stats (times)
              #:gc-stats (gc-stats)
              #:debug debug))

(define (repl-welcome repl)
  (display *version*)
  (newline)
  (newline)
  (display "Enter `,help' for help.\n"))

(define (repl-prompt repl)
  (cond
   ((repl-option-ref repl 'prompt)
    => (lambda (prompt) (prompt repl)))
   (else
    (format #f "~A@~A~A> " (language-name (repl-language repl))
            (module-name (current-module))
            (let ((level (length (cond
                                  ((fluid-ref *repl-stack*) => cdr)
                                  (else '())))))
              (if (zero? level) "" (format #f " [~a]" level)))))))

(define (repl-read repl)
  (let ((reader (language-reader (repl-language repl))))
    (reader (current-input-port) (current-module))))

(define (repl-compile-options repl)
  (repl-option-ref repl 'compile-options))

(define (repl-compile repl form)
  (let ((from (repl-language repl))
        (opts (repl-compile-options repl)))
    (compile form #:from from #:to 'objcode #:opts opts
             #:env (current-module))))

(define (repl-expand repl form)
  (let ((from (repl-language repl))
        (opts (repl-compile-options repl)))
    (decompile (compile form #:from from #:to 'tree-il #:opts opts
                        #:env (current-module))
               #:from 'tree-il #:to from)))

(define (repl-optimize repl form)
  (let ((from (repl-language repl))
        (opts (repl-compile-options repl)))
    (decompile (optimize! (compile form #:from from #:to 'tree-il #:opts opts
                                   #:env (current-module))
                          (current-module)
                          opts)
               #:from 'tree-il #:to from)))

(define (repl-parse repl form)
  (let ((parser (language-parser (repl-language repl))))
    (if parser (parser form) form)))

(define (repl-prepare-eval-thunk repl form)
  (let* ((eval (language-evaluator (repl-language repl))))
    (if (and eval
             (or (null? (language-compilers (repl-language repl)))
                 (repl-option-ref repl 'interp)))
        (lambda () (eval form (current-module)))
        (make-program (repl-compile repl form)))))

(define (repl-eval repl form)
  (let ((thunk (repl-prepare-eval-thunk repl form)))
    (% (thunk))))

(define (repl-print repl val)
  (if (not (eq? val *unspecified*))
      (begin
        (run-hook before-print-hook val)
        (cond
         ((repl-option-ref repl 'print)
          => (lambda (print) (print repl val)))
         (else
          ;; The result of an evaluation is representable in scheme, and
          ;; should be printed with the generic printer, `write'. The
          ;; language-printer is something else: it prints expressions of
          ;; a given language, not the result of evaluation.
          (write val)
          (newline))))))

(define (repl-option-ref repl key)
  (cadr (or (assq key (repl-options repl))
            (error "unknown repl option" key))))

(define (repl-option-set! repl key val)
  (let ((spec (or (assq key (repl-options repl))
                  (error "unknown repl option" key))))
    (set-car! (cdr spec)
              (if (procedure? (caddr spec))
                  ((caddr spec) val)
                  val))))

(define (repl-default-option-set! key val)
  (let ((spec (or (assq key repl-default-options)
                  (error "unknown repl option" key))))
    (set-car! (cdr spec)
              (if (procedure? (caddr spec))
                  ((caddr spec) val)
                  val))))

(define (repl-default-prompt-set! prompt)
  (repl-default-option-set! 'prompt prompt))


;;;
;;; Utilities
;;;

(define (puts x) (display x) (newline))

(define (->string x)
  (object->string x display))

(define (user-error msg . args)
  (throw 'user-error #f msg args #f))
