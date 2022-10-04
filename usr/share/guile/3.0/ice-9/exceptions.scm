;;; Exceptions
;;; Copyright (C) 2019-2020 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Definition of the standard exception types.
;;;
;;; Code:


(define-module (ice-9 exceptions)
  #:re-export (&exception
               make-exception
               make-exception-type
               simple-exceptions
               exception?
               exception-type?
               exception-predicate
               exception-accessor

               exception-kind
               exception-args

               &error
               &programming-error
               &quit-exception
               &non-continuable

               raise-exception
               with-exception-handler)
  #:export (define-exception-type

            &message
            make-exception-with-message
            exception-with-message?
            exception-message

            &warning
            make-warning
            warning?

            make-error
            error?

            &external-error
	    make-external-error
	    external-error?

            make-quit-exception
            quit-exception?

            make-programming-error
	    programming-error?

	    &assertion-failure
	    make-assertion-failure
	    assertion-failure?

	    &irritants
	    make-exception-with-irritants
            exception-with-irritants?
	    exception-irritants

	    &origin
	    make-exception-with-origin
            exception-with-origin?
	    exception-origin

            make-non-continuable-error
            non-continuable-error?

            &implementation-restriction
            make-implementation-restriction-error
            implementation-restriction-error?

            &lexical
            make-lexical-error
            lexical-error?

            &syntax
            make-syntax-error
            syntax-error?
            syntax-error-form
            syntax-error-subform

            &undefined-variable
            make-undefined-variable-error
            undefined-variable-error?

            raise-continuable

            guard))

(define-syntax define-exception-type-procedures
  (syntax-rules ()
    ((_ exception-type supertype constructor predicate
	(field accessor) ...)
     (begin
       (define constructor (record-constructor exception-type))
       (define predicate (exception-predicate exception-type))
       (define accessor
         (exception-accessor exception-type
                             (record-accessor exception-type 'field)))
       ...))))

(define-syntax define-exception-type
  (syntax-rules ()
    ((_ exception-type supertype constructor predicate
	(field accessor) ...)
     (begin
       (define exception-type
         (make-record-type 'exception-type '((immutable field) ...)
                           #:parent supertype #:extensible? #t))
       (define-exception-type-procedures exception-type supertype
         constructor predicate (field accessor) ...)))))

(define-exception-type-procedures &error &exception
  make-error error?)
(define-exception-type-procedures &programming-error &error
  make-programming-error programming-error?)

(define-exception-type &assertion-failure &programming-error
  make-assertion-failure assertion-failure?)

(define-exception-type &message &exception 
  make-exception-with-message exception-with-message? 
  (message exception-message))

(define-exception-type &warning &exception
  make-warning warning?)

(define-exception-type &external-error &error
  make-external-error external-error?)

(define-exception-type &irritants &exception
  make-exception-with-irritants exception-with-irritants?
  (irritants exception-irritants))

(define-exception-type &origin &exception
  make-exception-with-origin exception-with-origin?
  (origin exception-origin))

(define-exception-type-procedures &non-continuable &programming-error
  make-non-continuable-error
  non-continuable-error?)

(define-exception-type &implementation-restriction &programming-error
  make-implementation-restriction-error
  implementation-restriction-error?)

(define-exception-type &lexical &programming-error
  make-lexical-error lexical-error?)

(define-exception-type &syntax &programming-error
  make-syntax-error syntax-error?
  (form syntax-error-form)
  (subform syntax-error-subform))

(define-exception-type &undefined-variable &programming-error
  make-undefined-variable-error undefined-variable-error?)

(define make-exception-with-kind-and-args
  (record-constructor &exception-with-kind-and-args))
(define make-quit-exception
  (record-constructor &quit-exception))
(define quit-exception?
  (exception-predicate &quit-exception))

(define (default-guile-exception-converter key args)
  (make-exception (make-error)
                  (guile-common-exceptions key args)))

(define (guile-common-exceptions key args)
  (apply (case-lambda
          ((subr msg margs . _)
           (make-exception
            (make-exception-with-origin subr)
            (make-exception-with-message msg)
            (make-exception-with-irritants margs)))
          (_ (make-exception-with-irritants args)))
         args))

(define (convert-guile-exception key args)
  (let ((converter (assv-ref guile-exception-converters key)))
    (make-exception (or (and converter (converter key args))
                        (default-guile-exception-converter key args))
                    (make-exception-with-kind-and-args key args))))

(define (raise-continuable obj)
  (raise-exception obj #:continuable? #t))

;;; Exception printing

(define (exception-printer port key args punt)
  (cond ((and (= 1 (length args))
              (exception? (car args)))
         (display "ERROR:\n" port)
         (format-exception port (car args)))
        (else
         (punt))))

(define (format-exception port exception)
  (let ((components (simple-exceptions exception)))
    (if (null? components)
        (format port "Empty exception object")
        (let loop ((i 1) (components components))
          (cond ((pair? components)
                 (format port "  ~a. " i)
                 (format-simple-exception port (car components))
                 (when (pair? (cdr components))
                   (newline port))
                 (loop (+ i 1) (cdr components))))))))

(define (format-simple-exception port exception)
  (let* ((type (struct-vtable exception))
         (name (record-type-name type))
         (fields (record-type-fields type)))
    (cond
     ((null? fields)
      (format port "~a" name))
     ((null? (cdr fields))
      (format port "~a: ~s" name (struct-ref exception 0)))
     (else
      (format port "~a:\n" name)
      (let lp ((fields fields) (i 0))
        (let ((field (car fields))
              (fields (cdr fields)))
          (format port "      ~a: ~s" field (struct-ref exception i))
          (unless (null? fields)
            (newline port)
            (lp fields (+ i 1)))))))))

(set-exception-printer! '%exception exception-printer)

;; Guile exception converters
;;
;; Each converter is a procedure (converter KEY ARGS) that returns
;; either an exception object or #f.  If #f is returned,
;; 'default-guile-exception-converter' will be used.

(define (guile-syntax-error-converter key args)
  (apply (case-lambda
          ((who what where form subform . extra)
           (make-exception (make-syntax-error form subform)
                           (make-exception-with-origin who)
                           (make-exception-with-message what)))
          (_ #f))
         args))

(define make-quit-exception (record-constructor &quit-exception))
(define (guile-quit-exception-converter key args)
  (define code
    (cond
     ((not (pair? args)) 0)
     ((integer? (car args)) (car args))
     ((not (car args)) 1)
     (else 0)))
  (make-exception (make-quit-exception code)
                  (guile-common-exceptions key args)))

(define (guile-lexical-error-converter key args)
  (make-exception (make-lexical-error)
                  (guile-common-exceptions key args)))

(define (guile-assertion-failure-converter key args)
  (make-exception (make-assertion-failure)
                  (guile-common-exceptions key args)))

(define (guile-undefined-variable-error-converter key args)
  (make-exception (make-undefined-variable-error)
                  (guile-common-exceptions key args)))

(define (guile-implementation-restriction-converter key args)
  (make-exception (make-implementation-restriction-error)
                  (guile-common-exceptions key args)))

(define (guile-external-error-converter key args)
  (make-exception (make-external-error)
                  (guile-common-exceptions key args)))

(define (guile-system-error-converter key args)
  (apply (case-lambda
          ((subr msg msg-args errno . rest)
           ;; XXX TODO we should return a more specific error
           ;; (usually an I/O error) as expected by R6RS programs.
           ;; Unfortunately this often requires the 'filename' (or
           ;; other?) which is not currently provided by the native
           ;; Guile exceptions.
           (make-exception (make-external-error)
                           (guile-common-exceptions key args)))
          (_ (guile-external-error-converter key args)))
         args))

;; TODO: Arrange to have the needed information included in native
;;       Guile I/O exceptions, and arrange here to convert them to the
;;       proper exceptions.  Remove the earlier exception conversion
;;       mechanism: search for 'with-throw-handler' in the 'rnrs'
;;       tree, e.g. 'with-i/o-filename-exceptions' and
;;       'with-i/o-port-error' in (rnrs io ports).

;; XXX TODO: How should we handle the 'misc-error', 'vm-error', and
;;           'signal' native Guile exceptions?

;; XXX TODO: Should we handle the 'quit' exception specially?

;; An alist mapping native Guile exception keys to converters.
(define guile-exception-converters
  `((quit                      . ,guile-quit-exception-converter)
    (read-error                . ,guile-lexical-error-converter)
    (syntax-error              . ,guile-syntax-error-converter)
    (unbound-variable          . ,guile-undefined-variable-error-converter)
    (wrong-number-of-args      . ,guile-assertion-failure-converter)
    (wrong-type-arg            . ,guile-assertion-failure-converter)
    (keyword-argument-error    . ,guile-assertion-failure-converter)
    (out-of-range              . ,guile-assertion-failure-converter)
    (regular-expression-syntax . ,guile-assertion-failure-converter)
    (program-error             . ,guile-assertion-failure-converter)
    (goops-error               . ,guile-assertion-failure-converter)
    (null-pointer-error        . ,guile-assertion-failure-converter)
    (system-error              . ,guile-system-error-converter)
    (host-not-found            . ,guile-external-error-converter)
    (getaddrinfo-error         . ,guile-external-error-converter)
    (no-data                   . ,guile-external-error-converter)
    (no-recovery               . ,guile-external-error-converter)
    (try-again                 . ,guile-external-error-converter)
    (stack-overflow            . ,guile-implementation-restriction-converter)
    (numerical-overflow        . ,guile-implementation-restriction-converter)
    (memory-allocation-error   . ,guile-implementation-restriction-converter)))

(define (set-guile-exception-converter! key proc)
  (set! guile-exception-converters
        (acons key proc guile-exception-converters)))

;; Override core definition.
(set! make-exception-from-throw convert-guile-exception)

(define-syntax guard
  (lambda (stx)
    "Establish an exception handler during the evaluation of an expression.

@example
(guard (@var{exn} @var{clause1} @var{clause2} ...)
  @var{body} @var{body*} ...)
@end example

Each @var{clause} should have the same form as a @code{cond} clause.

The @code{(begin body body* ...)} is evaluated with an exception
handler that binds the raised object to @var{exn} and within the scope of
that binding evaluates the clauses as if they were the clauses of a cond
expression.

When a clause of that implicit cond expression matches, its consequent
is evaluated with the continuation and dynamic environment of the
@code{guard} expression.

If every clause's test evaluates to false and there is no @code{else}
clause, then @code{raise-continuable} is re-invoked on the raised
object, within the dynamic environment of the original call to raise
except that the current exception handler is that of the guard
expression.

Note that in a slight deviation from SRFI-34, R6RS, and R7RS, Guile
evaluates the clause tests within the continuation of the exception
handler, not the continuation of the @code{guard}.  This allows
unhandled exceptions to continue to dispatch within the original
continuation, without unwinding then rewinding any intermediate
@code{dynamic-wind} invocations."
    (define (dispatch tag exn clauses)
      (define (build-clause test handler clauses)
        #`(let ((t #,test))
            (if t
                (abort-to-prompt #,tag #,handler t)
                #,(dispatch tag exn clauses))))
      (syntax-case clauses (=> else)
        (() #`(raise-continuable #,exn))
        (((test => f) . clauses)
         (build-clause #'test #'(lambda (res) (f res)) #'clauses))
        (((else e e* ...) . clauses)
         (build-clause #'#t #'(lambda (res) e e* ...) #'clauses))
        (((test) . clauses)
         (build-clause #'test #'(lambda (res) res) #'clauses))
        (((test e* ...) . clauses)
         (build-clause #'test #'(lambda (res) e* ...) #'clauses))))
    (syntax-case stx ()
      ((guard (exn clause clause* ...) body body* ...)
       (identifier? #'exn)
       #`(let ((tag (make-prompt-tag)))
           (call-with-prompt
            tag
            (lambda ()
              (with-exception-handler
               (lambda (exn)
                 #,(dispatch #'tag #'exn #'(clause clause* ...)))
               (lambda () body body* ...)))
            (lambda (_ h v)
              (h v))))))))
