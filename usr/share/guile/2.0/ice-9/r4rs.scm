;;;; r4rs.scm --- definitions needed for libguile to be R4RS compliant
;;;; Jim Blandy <jimb@cyclic.com> --- October 1996

;;;; 	Copyright (C) 1996, 1997, 1998, 2000, 2001, 2006, 2010, 2011, 2012 Free Software Foundation, Inc.
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

(eval-when (compile)
  (set-current-module (resolve-module '(guile))))


;;;; apply and call-with-current-continuation

;;; The deal with these is that they are the procedural wrappers around the
;;; primitives of Guile's language. There are about 20 different kinds of
;;; expression in Guile, and e.g. @apply is one of them. (It has to be that way
;;; to preserve tail recursion.)
;;;
;;; Usually we recognize (apply foo bar) to be an instance of @apply, but in the
;;; case that apply is passed to apply, or we're bootstrapping, we need a
;;; trampoline -- and here they are.
(define (apply fun . args)
  (@apply fun (apply:nconc2last args)))
(define (call-with-current-continuation proc)
  (@call-with-current-continuation proc))
(define (call-with-values producer consumer)
  (@call-with-values producer consumer))
(define (dynamic-wind in thunk out)
  "All three arguments must be 0-argument procedures.
Guard @var{in} is called, then @var{thunk}, then
guard @var{out}.

If, any time during the execution of @var{thunk}, the
continuation of the @code{dynamic_wind} expression is escaped
non-locally, @var{out} is called.  If the continuation of
the dynamic-wind is re-entered, @var{in} is called.  Thus
@var{in} and @var{out} may be called any number of
times.
@lisp
 (define x 'normal-binding)
@result{} x
 (define a-cont
   (call-with-current-continuation
     (lambda (escape)
       (let ((old-x x))
         (dynamic-wind
           ;; in-guard:
           ;;
           (lambda () (set! x 'special-binding))

           ;; thunk
           ;;
           (lambda () (display x) (newline)
                   (call-with-current-continuation escape)
                   (display x) (newline)
                   x)

           ;; out-guard:
           ;;
           (lambda () (set! x old-x)))))))

;; Prints:
special-binding
;; Evaluates to:
@result{} a-cont
x
@result{} normal-binding
 (a-cont #f)
;; Prints:
special-binding
;; Evaluates to:
@result{} a-cont  ;; the value of the (define a-cont...)
x
@result{} normal-binding
a-cont
@result{} special-binding
@end lisp"
  (@dynamic-wind in (thunk) out))


;;;; Basic Port Code

;;; Specifically, the parts of the low-level port code that are written in 
;;; Scheme rather than C.
;;;
;;; WARNING: the parts of this interface that refer to file ports
;;; are going away.   It would be gone already except that it is used
;;; "internally" in a few places.


;;; OPEN_READ, OPEN_WRITE, and OPEN_BOTH are used to request the
;;; proper mode to open files in.
;;;
;;; If we want to support systems that do CRLF->LF translation, like
;;; Windows, then we should have a symbol in scmconfig.h made visible
;;; to the Scheme level that we can test here, and autoconf magic to
;;; #define it when appropriate.  Windows will probably just have a
;;; hand-generated scmconfig.h file.
(define OPEN_READ "r")
(define OPEN_WRITE "w")
(define OPEN_BOTH "r+")

(define *null-device* "/dev/null")

(define (open-input-file str)
  "Takes a string naming an existing file and returns an input port
capable of delivering characters from the file.  If the file
cannot be opened, an error is signalled."
  (open-file str OPEN_READ))

(define (open-output-file str)
  "Takes a string naming an output file to be created and returns an
output port capable of writing characters to a new file by that
name.  If the file cannot be opened, an error is signalled.  If a
file with the given name already exists, the effect is unspecified."
  (open-file str OPEN_WRITE))

(define (open-io-file str) 
  "Open file with name STR for both input and output."
  (open-file str OPEN_BOTH))

(define (call-with-input-file str proc)
  "PROC should be a procedure of one argument, and STR should be a
string naming a file.  The file must
already exist. These procedures call PROC
with one argument: the port obtained by opening the named file for
input or output.  If the file cannot be opened, an error is
signalled.  If the procedure returns, then the port is closed
automatically and the values yielded by the procedure are returned.
If the procedure does not return, then the port will not be closed
automatically unless it is possible to prove that the port will
never again be used for a read or write operation."
  (let ((p (open-input-file str)))
    (call-with-values
      (lambda () (proc p))
      (lambda vals
        (close-input-port p)
        (apply values vals)))))

(define (call-with-output-file str proc)
  "PROC should be a procedure of one argument, and STR should be a
string naming a file.  The behaviour is unspecified if the file 
already exists. These procedures call PROC
with one argument: the port obtained by opening the named file for
input or output.  If the file cannot be opened, an error is
signalled.  If the procedure returns, then the port is closed
automatically and the values yielded by the procedure are returned.
If the procedure does not return, then the port will not be closed
automatically unless it is possible to prove that the port will
never again be used for a read or write operation."
  (let ((p (open-output-file str)))
    (call-with-values
      (lambda () (proc p))
      (lambda vals
        (close-output-port p)
        (apply values vals)))))

(define (with-input-from-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-input-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-output-to-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-output-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-error-to-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-error-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-input-from-file file thunk)
  "THUNK must be a procedure of no arguments, and FILE must be a
string naming a file.  The file must already exist. The file is opened for
input, an input port connected to it is made
the default value returned by `current-input-port', 
and the THUNK is called with no arguments.
When the THUNK returns, the port is closed and the previous
default is restored.  Returns the values yielded by THUNK.  If an
escape procedure is used to escape from the continuation of these
procedures, their behavior is implementation dependent."
  (call-with-input-file file
   (lambda (p) (with-input-from-port p thunk))))

(define (with-output-to-file file thunk)
  "THUNK must be a procedure of no arguments, and FILE must be a
string naming a file.  The effect is unspecified if the file already exists. 
The file is opened for output, an output port connected to it is made
the default value returned by `current-output-port', 
and the THUNK is called with no arguments.
When the THUNK returns, the port is closed and the previous
default is restored.  Returns the values yielded by THUNK.  If an
escape procedure is used to escape from the continuation of these
procedures, their behavior is implementation dependent."
  (call-with-output-file file
   (lambda (p) (with-output-to-port p thunk))))

(define (with-error-to-file file thunk)
  "THUNK must be a procedure of no arguments, and FILE must be a
string naming a file.  The effect is unspecified if the file already exists. 
The file is opened for output, an output port connected to it is made
the default value returned by `current-error-port', 
and the THUNK is called with no arguments.
When the THUNK returns, the port is closed and the previous
default is restored.  Returns the values yielded by THUNK.  If an
escape procedure is used to escape from the continuation of these
procedures, their behavior is implementation dependent."
  (call-with-output-file file
   (lambda (p) (with-error-to-port p thunk))))

(define (with-input-from-string string thunk)
  "THUNK must be a procedure of no arguments.
The test of STRING  is opened for
input, an input port connected to it is made, 
and the THUNK is called with no arguments.
When the THUNK returns, the port is closed.
Returns the values yielded by THUNK.  If an
escape procedure is used to escape from the continuation of these
procedures, their behavior is implementation dependent."
  (call-with-input-string string
   (lambda (p) (with-input-from-port p thunk))))

(define (with-output-to-string thunk)
  "Calls THUNK and returns its output as a string."
  (call-with-output-string
   (lambda (p) (with-output-to-port p thunk))))

(define (with-error-to-string thunk)
  "Calls THUNK and returns its error output as a string."
  (call-with-output-string
   (lambda (p) (with-error-to-port p thunk))))

(define the-eof-object (call-with-input-string "" (lambda (p) (read-char p))))
