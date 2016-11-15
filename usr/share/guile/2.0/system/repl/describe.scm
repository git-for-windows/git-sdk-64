;;; Describe objects

;; Copyright (C) 2001, 2009, 2011 Free Software Foundation, Inc.

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

(define-module (system repl describe)
  #:use-module (oop goops)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 and-let-star)
  #:export (describe))

(define-method (describe (symbol <symbol>))
  (format #t "`~s' is " symbol)
  (if (not (defined? symbol))
      (display "not defined in the current module.\n")
      (describe-object (module-ref (current-module) symbol))))


;;;
;;; Display functions
;;;

(define (safe-class-name class)
  (if (slot-bound? class 'name)
      (class-name class)
      class))

(define-method (display-class class . args)
  (let* ((name (safe-class-name class))
	 (desc (if (pair? args) (car args) name)))
    (if (eq? *describe-format* 'tag)
	(format #t "@class{~a}{~a}" name desc)
	(format #t "~a" desc))))

(define (display-list title list)
  (if title (begin (display title) (display ":\n\n")))
  (if (null? list)
      (display "(not defined)\n")
      (for-each display-summary list)))

(define (display-slot-list title instance list)
  (if title (begin (display title) (display ":\n\n")))
  (if (null? list)
      (display "(not defined)\n")
      (for-each (lambda (slot)
		  (let ((name (slot-definition-name slot)))
		    (display "Slot: ")
		    (display name)
		    (if (and instance (slot-bound? instance name))
			(begin
			  (display " = ")
			  (display (slot-ref instance name))))
		    (newline)))
		list)))

(define (display-file location)
  (display "Defined in ")
  (if (eq? *describe-format* 'tag)
      (format #t "@location{~a}.\n" location)
      (format #t "`~a'.\n" location)))

(define (format-documentation doc)
  (with-current-buffer (make-buffer #:text doc)
    (lambda ()
      (let ((regexp (make-regexp "@([a-z]*)(\\{([^}]*)\\})?")))
	(do-while (match (re-search-forward regexp))
	  (let ((key (string->symbol (match:substring match 1)))
		(value (match:substring match 3)))
	    (case key
	      ((deffnx)
	       (delete-region! (match:start match)
			       (begin (forward-line) (point))))
	      ((var)
	       (replace-match! match 0 (string-upcase value)))
	      ((code)
	       (replace-match! match 0 (string-append "`" value "'")))))))
      (display (string (current-buffer)))
      (newline))))


;;;
;;; Top
;;;

(define description-table
  (list
   (cons <boolean>   "a boolean")
   (cons <null>      "an empty list")
   (cons <integer>   "an integer")
   (cons <real>      "a real number")
   (cons <complex>   "a complex number")
   (cons <char>      "a character")
   (cons <symbol>    "a symbol")
   (cons <keyword>   "a keyword")
   (cons <promise>   "a promise")
   (cons <hook>      "a hook")
   (cons <fluid>     "a fluid")
   (cons <stack>     "a stack")
   (cons <variable>  "a variable")
   (cons <regexp>    "a regexp object")
   (cons <module>    "a module object")
   (cons <unknown>   "an unknown object")))

(define-generic describe-object)
(export describe-object)

(define-method (describe-object (obj <top>))
  (display-type obj)
  (display-location obj)
  (newline)
  (display-value obj)
  (newline)
  (display-documentation obj))

(define-generic display-object)
(define-generic display-summary)
(define-generic display-type)
(define-generic display-value)
(define-generic display-location)
(define-generic display-description)
(define-generic display-documentation)
(export display-object display-summary display-type display-value
	display-location display-description display-documentation)

(define-method (display-object (obj <top>))
  (write obj))

(define-method (display-summary (obj <top>))
  (display "Value: ")
  (display-object obj)
  (newline))

(define-method (display-type (obj <top>))
  (cond
   ((eof-object? obj) (display "the end-of-file object"))
   ((unspecified? obj) (display "unspecified"))
   (else (let ((class (class-of obj)))
	   (display-class class (or (assq-ref description-table class)
				    (safe-class-name class))))))
  (display ".\n"))

(define-method (display-value (obj <top>))
  (if (not (unspecified? obj))
      (begin (display-object obj) (newline))))

(define-method (display-location (obj <top>))
  *unspecified*)

(define-method (display-description (obj <top>))
  (let* ((doc (with-output-to-string (lambda () (display-documentation obj))))
	 (index (string-index doc #\newline)))
    (display (substring doc 0 (1+ index)))))

(define-method (display-documentation (obj <top>))
  (display "Not documented.\n"))


;;;
;;; Pairs
;;;

(define-method (display-type (obj <pair>))
  (cond
   ((list? obj) (display-class <list> "a list"))
   ((pair? (cdr obj)) (display "an improper list"))
   (else (display-class <pair> "a pair")))
  (display ".\n"))


;;;
;;; Strings
;;;

(define-method (display-type (obj <string>))
  (if (read-only-string? 'obj)
      (display "a read-only string")
      (display-class <string> "a string"))
  (display ".\n"))


;;;
;;; Procedures
;;;

(define-method (display-object (obj <procedure>))
  (cond
   ;; FIXME: VM programs, ...
   (else
    ;; Primitive procedure.  Let's lookup the dictionary.
    (and-let* ((entry (lookup-procedure obj)))
      (let ((name (entry-property entry 'name))
	    (print-arg (lambda (arg)
			 (display " ")
			 (display (string-upcase (symbol->string arg))))))
	(display "(")
	(display name)
	(and-let* ((args (entry-property entry 'args)))
	  (for-each print-arg args))
	(and-let* ((opts (entry-property entry 'opts)))
	  (display " &optional")
	  (for-each print-arg opts))
	(and-let* ((rest (entry-property entry 'rest)))
	  (display " &rest")
	  (print-arg rest))
	(display ")"))))))

(define-method (display-summary (obj <procedure>))
  (display "Procedure: ")
  (display-object obj)
  (newline)
  (display "  ")
  (display-description obj))

(define-method (display-type (obj <procedure>))
  (cond
   ((and (thunk? obj) (not (procedure-name obj))) (display "a thunk"))
   ((procedure-with-setter? obj)
    (display-class <procedure-with-setter> "a procedure with setter"))
   (else (display-class <procedure> "a procedure")))
  (display ".\n"))

(define-method (display-location (obj <procedure>))
  (and-let* ((entry (lookup-procedure obj)))
    (display-file (entry-file entry))))

(define-method (display-documentation (obj <procedure>))
  (cond ((or (procedure-documentation obj)
             (and=> (lookup-procedure obj) entry-text))
	 => format-documentation)
	(else (next-method))))


;;;
;;; Classes
;;;

(define-method (describe-object (obj <class>))
  (display-type obj)
  (display-location obj)
  (newline)
  (display-documentation obj)
  (newline)
  (display-value obj))

(define-method (display-summary (obj <class>))
  (display "Class: ")
  (display-class obj)
  (newline)
  (display "  ")
  (display-description obj))

(define-method (display-type (obj <class>))
  (display-class <class> "a class")
  (if (not (eq? (class-of obj) <class>))
      (begin (display " of ") (display-class (class-of obj))))
  (display ".\n"))

(define-method (display-value (obj <class>))
  (display-list "Class precedence list" (class-precedence-list obj))
  (newline)
  (display-list "Direct superclasses" (class-direct-supers obj))
  (newline)
  (display-list "Direct subclasses" (class-direct-subclasses obj))
  (newline)
  (display-slot-list "Direct slots" #f (class-direct-slots obj))
  (newline)
  (display-list "Direct methods" (class-direct-methods obj)))


;;;
;;; Instances
;;;

(define-method (display-type (obj <object>))
  (display-class <object> "an instance")
  (display " of class ")
  (display-class (class-of obj))
  (display ".\n"))

(define-method (display-value (obj <object>))
  (display-slot-list #f obj (class-slots (class-of obj))))


;;;
;;; Generic functions
;;;

(define-method (display-type (obj <generic>))
  (display-class <generic> "a generic function")
  (display " of class ")
  (display-class (class-of obj))
  (display ".\n"))

(define-method (display-value (obj <generic>))
  (display-list #f (generic-function-methods obj)))


;;;
;;; Methods
;;;

(define-method (display-object (obj <method>))
  (display "(")
  (let ((gf (method-generic-function obj)))
    (display (if gf (generic-function-name gf) "#<anonymous>")))
  (let loop ((args (method-specializers obj)))
    (cond
     ((null? args))
     ((pair? args)
      (display " ")
      (display-class (car args))
      (loop (cdr args)))
     (else (display " . ") (display-class args))))
  (display ")"))

(define-method (display-summary (obj <method>))
  (display "Method: ")
  (display-object obj)
  (newline)
  (display "  ")
  (display-description obj))

(define-method (display-type (obj <method>))
  (display-class <method> "a method")
  (display " of class ")
  (display-class (class-of obj))
  (display ".\n"))

(define-method (display-documentation (obj <method>))
  (let ((doc (procedure-documentation (method-procedure obj))))
    (if doc (format-documentation doc) (next-method))))
