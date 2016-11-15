;;; Guile VM debugging facilities

;;; Copyright (C) 2001, 2009, 2010, 2011 Free Software Foundation, Inc.
;;;
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

(define-module (system vm inspect)
  #:use-module (system base pmatch)
  #:use-module (system base syntax)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module ((language assembly disassemble)
                #:select ((disassemble . %disassemble)))
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 format)
  #:use-module (system vm program)
  #:export (inspect))


(define (reverse-hashq h)
  (let ((ret (make-hash-table)))
    (hash-for-each
     (lambda (k v)
       (hashq-set! ret v (cons k (hashq-ref ret v '()))))
     h)
    ret))

(define (catch-bad-arguments thunk bad-args-thunk)
  (catch 'wrong-number-of-args
    (lambda ()
      (catch 'keyword-argument-error
        thunk
        (lambda (k . args)
          (bad-args-thunk))))
    (lambda (k . args)
      (bad-args-thunk))))

(define (read-args prompt)
  (define (read* reader)
    (repl-reader prompt reader))
  (define (next)
    (read* read-char))
  (define (cmd chr)
    (cond
     ((eof-object? chr) (list chr))
     ((char=? chr #\newline) (cmd (next)))
     ((char-whitespace? chr) (cmd (next)))
     (else
      (unread-char chr)
      (let ((tok (read* read)))
        (args (list tok) (next))))))
  (define (args out chr)
    (cond
     ((eof-object? chr) (reverse out))
     ((char=? chr #\newline) (reverse out))
     ((char-whitespace? chr) (args out (next)))
     (else
      (unread-char chr)
      (let ((tok (read* read)))
        (args (cons tok out) (next))))))
  (cmd (next)))


;;;
;;; Inspector
;;;

(define (inspect x)
  (define-syntax-rule (define-command ((mod cname alias ...) . args)
                        body ...)
    (define cname
      (let ((c (lambda* args body ...)))
        (set-procedure-property! c 'name 'cname)
        (module-define! mod 'cname c)
        (module-add! mod 'alias (module-local-variable mod 'cname))
        ...
        c)))

  (let ((commands (make-module)))
    (define (prompt)
      (format #f "~20@y inspect> " x))
      
    (define-command ((commands quit q continue cont c))
      "Quit the inspector."
      (throw 'quit))
      
    (define-command ((commands print p))
      "Print the current object using `pretty-print'."
      (pretty-print x))
      
    (define-command ((commands write w))
      "Print the current object using `write'."
      (write x))
      
    (define-command ((commands display d))
      "Print the current object using `display'."
      (display x))
      
    (define-command ((commands disassemble x))
      "Disassemble the current object, which should be objcode or a procedure."
      (catch #t
        (lambda ()
          (%disassemble x))
        (lambda args
          (format #t "Error disassembling object: ~a\n" args))))
    
    (define-command ((commands help h ?) #:optional cmd)
      "Show this help message."
      (let ((rhash (reverse-hashq (module-obarray commands))))
        (define (help-cmd cmd)
          (let* ((v (module-local-variable commands cmd))
                 (p (variable-ref v))
                 (canonical-name (procedure-name p)))
            ;; la la la
            (format #t "~a~{ ~:@(~a~)~}~?~%~a~&~%"
                    canonical-name (program-lambda-list p)
                    "~#[~:;~40t(aliases: ~@{~a~^, ~})~]"
                    (delq canonical-name (hashq-ref rhash v))
                    (procedure-documentation p))))
        (cond
         (cmd
          (cond
           ((and (symbol? cmd) (module-local-variable commands cmd))
            (help-cmd cmd))
           (else
            (format #t "Invalid command ~s.~%" cmd)
            (format #t "Try `help' for a list of commands~%"))))
         (else
          (let ((names (sort
                        (hash-map->list
                         (lambda (k v)
                           (procedure-name (variable-ref k)))
                         rhash)
                        (lambda (x y)
                          (string<? (symbol->string x)
                                    (symbol->string y))))))
            (format #t "Available commands:~%~%")
            (for-each help-cmd names))))))

    (define (handle cmd . args)
      (cond
       ((and (symbol? cmd)
             (module-local-variable commands cmd))
        => (lambda (var)
             (let ((proc (variable-ref var)))
               (catch-bad-arguments
                (lambda ()
                  (apply (variable-ref var) args))
                (lambda ()
                  (format (current-error-port)
                          "Invalid arguments to ~a. Try `help ~a'.~%"
                          (procedure-name proc) (procedure-name proc)))))))
       ; ((and (integer? cmd) (exact? cmd))
       ;  (nth cmd))
       ((eof-object? cmd)
        (newline)
        (throw 'quit))
       (else
        (format (current-error-port)
                "~&Unknown command: ~a. Try `help'.~%" cmd)
        *unspecified*)))

    (catch 'quit
      (lambda ()
        (let loop ()
          (apply
           handle
           (save-module-excursion
            (lambda ()
              (set-current-module commands)
              (read-args prompt))))
          (loop)))
      (lambda (k . args)
        (apply values args)))))
