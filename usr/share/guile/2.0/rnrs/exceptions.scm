;;; exceptions.scm --- The R6RS exceptions library

;;      Copyright (C) 2010, 2011, 2013 Free Software Foundation, Inc.
;;
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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(library (rnrs exceptions (6))
  (export guard with-exception-handler raise raise-continuable)
  (import (rnrs base (6))
          (rnrs control (6))
          (rnrs conditions (6))
	  (rnrs records procedural (6))
	  (rnrs records inspection (6))
	  (only (guile)
                format
                newline
                display
                filter
                acons
                assv-ref
                throw
                set-exception-printer!
                with-throw-handler
                *unspecified*
                @@))

  ;; When a native guile exception is caught by an R6RS exception
  ;; handler, we convert it to an R6RS compound condition that includes
  ;; not only the standard condition objects expected by R6RS code, but
  ;; also a special &guile condition that preserves the original KEY and
  ;; ARGS passed to the native Guile catch handler.

  (define-condition-type &guile &condition
    make-guile-condition guile-condition?
    (key  guile-condition-key)
    (args guile-condition-args))

  (define (default-guile-condition-converter key args)
    (condition (make-serious-condition)
               (guile-common-conditions key args)))

  (define (guile-common-conditions key args)
    (apply (case-lambda
             ((subr msg margs . _)
              (condition (make-who-condition subr)
                         (make-message-condition msg)
                         (make-irritants-condition margs)))
             (_ (make-irritants-condition args)))
           args))

  (define (convert-guile-condition key args)
    (let ((converter (assv-ref guile-condition-converters key)))
      (condition (or (and converter (converter key args))
                     (default-guile-condition-converter key args))
                 ;; Preserve the original KEY and ARGS in the R6RS
                 ;; condition object.
                 (make-guile-condition key args))))

  ;; If an R6RS exception handler chooses not to handle a given
  ;; condition, it will re-raise the condition to pass it on to the next
  ;; handler.  If the condition was converted from a native Guile
  ;; exception, we must re-raise using the native Guile facilities and
  ;; the original exception KEY and ARGS.  We arrange for this in
  ;; 'raise' so that native Guile exception handlers will continue to
  ;; work when mixed with R6RS code.

  (define (raise obj)
    (if (guile-condition? obj)
        (apply throw (guile-condition-key obj) (guile-condition-args obj))
        ((@@ (rnrs records procedural) r6rs-raise) obj)))
  (define raise-continuable
    (@@ (rnrs records procedural) r6rs-raise-continuable))

  (define raise-object-wrapper? 
    (@@ (rnrs records procedural) raise-object-wrapper?))
  (define raise-object-wrapper-obj
    (@@ (rnrs records procedural) raise-object-wrapper-obj))
  (define raise-object-wrapper-continuation
    (@@ (rnrs records procedural) raise-object-wrapper-continuation))

  (define (with-exception-handler handler thunk)
    (with-throw-handler #t
     thunk
     (lambda (key . args)
       (cond ((not (eq? key 'r6rs:exception))
              (let ((obj (convert-guile-condition key args)))
                (handler obj)
                (raise (make-non-continuable-violation))))
             ((and (not (null? args))
                   (raise-object-wrapper? (car args)))
              (let* ((cargs (car args))
                     (obj (raise-object-wrapper-obj cargs))
                     (continuation (raise-object-wrapper-continuation cargs))
                     (handler-return (handler obj)))
                (if continuation
                    (continuation handler-return)
                    (raise (make-non-continuable-violation)))))))))

  (define-syntax guard0
    (syntax-rules ()
      ((_ (variable cond-clause ...) . body)
       (call/cc (lambda (continuation)
		  (with-exception-handler
		   (lambda (variable)
		     (continuation (cond cond-clause ...)))
		   (lambda () . body)))))))

  (define-syntax guard
    (syntax-rules (else)
      ((_ (variable cond-clause ... . ((else else-clause ...))) . body)
       (guard0 (variable cond-clause ... (else else-clause ...)) . body))
      ((_ (variable cond-clause ...) . body)
       (guard0 (variable cond-clause ... (else (raise variable))) . body))))

  ;;; Exception printing

  (define (exception-printer port key args punt)
    (cond ((and (= 1 (length args))
                (raise-object-wrapper? (car args)))
           (let ((obj (raise-object-wrapper-obj (car args))))
             (cond ((condition? obj)
                    (display "ERROR: R6RS exception:\n" port)
                    (format-condition port obj))
                   (else
                    (format port "ERROR: R6RS exception: `~s'" obj)))))
          (else
           (punt))))

  (define (format-condition port condition)
    (let ((components (simple-conditions condition)))
      (if (null? components)
          (format port "Empty condition object")
          (let loop ((i 1) (components components))
            (cond ((pair? components)
                   (format port "  ~a. " i)
                   (format-simple-condition port (car components))
                   (when (pair? (cdr components))
                     (newline port))
                   (loop (+ i 1) (cdr components))))))))

  (define (format-simple-condition port condition)
    (define (print-rtd-fields rtd field-names)
      (let ((n-fields (vector-length field-names)))
        (do ((i 0 (+ i 1)))
            ((>= i n-fields))
          (format port "      ~a: ~s"
                  (vector-ref field-names i)
                  ((record-accessor rtd i) condition))
          (unless (= i (- n-fields 1))
            (newline port)))))
    (let ((condition-name (record-type-name (record-rtd condition))))
      (let loop ((rtd (record-rtd condition))
                 (rtd.fields-list '())
                 (n-fields 0))
        (cond (rtd
               (let ((field-names (record-type-field-names rtd)))
                 (loop (record-type-parent rtd)
                       (cons (cons rtd field-names) rtd.fields-list)
                       (+ n-fields (vector-length field-names)))))
              (else
               (let ((rtd.fields-list
                      (filter (lambda (rtd.fields)
                                (not (zero? (vector-length (cdr rtd.fields)))))
                              (reverse rtd.fields-list))))
                 (case n-fields
                   ((0) (format port "~a" condition-name))
                   ((1) (format port "~a: ~s"
                                condition-name
                                ((record-accessor (caar rtd.fields-list) 0)
                                 condition)))
                   (else
                    (format port "~a:\n" condition-name)
                    (let loop ((lst rtd.fields-list))
                      (when (pair? lst)
                        (let ((rtd.fields (car lst)))
                          (print-rtd-fields (car rtd.fields) (cdr rtd.fields))
                          (when (pair? (cdr lst))
                            (newline port))
                          (loop (cdr lst)))))))))))))

  (set-exception-printer! 'r6rs:exception exception-printer)

  ;; Guile condition converters
  ;;
  ;; Each converter is a procedure (converter KEY ARGS) that returns
  ;; either an R6RS condition or #f.  If #f is returned,
  ;; 'default-guile-condition-converter' will be used.

  (define (guile-syntax-violation-converter key args)
    (apply (case-lambda
             ((who what where form subform . extra)
              (condition (make-syntax-violation form subform)
                         (make-who-condition who)
                         (make-message-condition what)))
             (_ #f))
           args))

  (define (guile-lexical-violation-converter key args)
    (condition (make-lexical-violation) (guile-common-conditions key args)))

  (define (guile-assertion-violation-converter key args)
    (condition (make-assertion-violation) (guile-common-conditions key args)))

  (define (guile-undefined-violation-converter key args)
    (condition (make-undefined-violation) (guile-common-conditions key args)))

  (define (guile-implementation-restriction-converter key args)
    (condition (make-implementation-restriction-violation)
               (guile-common-conditions key args)))

  (define (guile-error-converter key args)
    (condition (make-error) (guile-common-conditions key args)))

  (define (guile-system-error-converter key args)
    (apply (case-lambda
             ((subr msg msg-args errno . rest)
              ;; XXX TODO we should return a more specific error
              ;; (usually an I/O error) as expected by R6RS programs.
              ;; Unfortunately this often requires the 'filename' (or
              ;; other?) which is not currently provided by the native
              ;; Guile exceptions.
              (condition (make-error) (guile-common-conditions key args)))
             (_ (guile-error-converter key args)))
           args))

  ;; TODO: Arrange to have the needed information included in native
  ;;       Guile I/O exceptions, and arrange here to convert them to the
  ;;       proper conditions.  Remove the earlier exception conversion
  ;;       mechanism: search for 'with-throw-handler' in the 'rnrs'
  ;;       tree, e.g. 'with-i/o-filename-conditions' and
  ;;       'with-i/o-port-error' in (rnrs io ports).

  ;; XXX TODO: How should we handle the 'misc-error', 'vm-error', and
  ;;           'signal' native Guile exceptions?

  ;; XXX TODO: Should we handle the 'quit' exception specially?

  ;; An alist mapping native Guile exception keys to converters.
  (define guile-condition-converters
    `((read-error                . ,guile-lexical-violation-converter)
      (syntax-error              . ,guile-syntax-violation-converter)
      (unbound-variable          . ,guile-undefined-violation-converter)
      (wrong-number-of-args      . ,guile-assertion-violation-converter)
      (wrong-type-arg            . ,guile-assertion-violation-converter)
      (keyword-argument-error    . ,guile-assertion-violation-converter)
      (out-of-range              . ,guile-assertion-violation-converter)
      (regular-expression-syntax . ,guile-assertion-violation-converter)
      (program-error             . ,guile-assertion-violation-converter)
      (goops-error               . ,guile-assertion-violation-converter)
      (null-pointer-error        . ,guile-assertion-violation-converter)
      (system-error              . ,guile-system-error-converter)
      (host-not-found            . ,guile-error-converter)
      (getaddrinfo-error         . ,guile-error-converter)
      (no-data                   . ,guile-error-converter)
      (no-recovery               . ,guile-error-converter)
      (try-again                 . ,guile-error-converter)
      (stack-overflow            . ,guile-implementation-restriction-converter)
      (numerical-overflow        . ,guile-implementation-restriction-converter)
      (memory-allocation-error   . ,guile-implementation-restriction-converter)))

  (define (set-guile-condition-converter! key proc)
    (set! guile-condition-converters
          (acons key proc guile-condition-converters))))
