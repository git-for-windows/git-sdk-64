;;;; 	Copyright (C) 1996, 1998, 2001, 2002, 2003, 2006, 2010, 2011,
;;;;      2012 Free Software Foundation, Inc.
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
;;;; ----------------------------------------------------------------
;;;; threads.scm -- User-level interface to Guile's thread system
;;;; 4 March 1996, Anthony Green <green@cygnus.com>
;;;; Modified 5 October 1996, MDJ <djurfeldt@nada.kth.se>
;;;; Modified 6 April 2001, ttn
;;;; ----------------------------------------------------------------
;;;;

;;; Commentary:

;; This module is documented in the Guile Reference Manual.
;; Briefly, one procedure is exported: `%thread-handler';
;; as well as four macros: `make-thread', `begin-thread',
;; `with-mutex' and `monitor'.

;;; Code:

(define-module (ice-9 threads)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 match)
  #:export (begin-thread
            parallel
            letpar
            make-thread
            with-mutex
            monitor

            par-map
            par-for-each
            n-par-map
            n-par-for-each
            n-for-each-par-map
            %thread-handler))



;;; Macros first, so that the procedures expand correctly.

(define-syntax-rule (begin-thread e0 e1 ...)
  (call-with-new-thread
   (lambda () e0 e1 ...)
   %thread-handler))

(define-syntax parallel
  (lambda (x)
    (syntax-case x ()
      ((_ e0 ...)
       (with-syntax (((tmp0 ...) (generate-temporaries (syntax (e0 ...)))))
         #'(let ((tmp0 (future e0))
                 ...)
             (values (touch tmp0) ...)))))))

(define-syntax-rule (letpar ((v e) ...) b0 b1 ...)
  (call-with-values
      (lambda () (parallel e ...))
    (lambda (v ...)
      b0 b1 ...)))

(define-syntax-rule (make-thread proc arg ...)
  (call-with-new-thread
   (lambda () (proc arg ...))
   %thread-handler))

(define-syntax-rule (with-mutex m e0 e1 ...)
  (let ((x m))
    (dynamic-wind
      (lambda () (lock-mutex x))
      (lambda () (begin e0 e1 ...))
      (lambda () (unlock-mutex x)))))

(define-syntax-rule (monitor first rest ...)
  (with-mutex (make-mutex)
    first rest ...))

(define (par-mapper mapper cons)
  (lambda (proc . lists)
    (let loop ((lists lists))
      (match lists
        (((heads tails ...) ...)
         (let ((tail (future (loop tails)))
               (head (apply proc heads)))
           (cons head (touch tail))))
        (_
         '())))))

(define par-map (par-mapper map cons))
(define par-for-each (par-mapper for-each (const *unspecified*)))

(define (n-par-map n proc . arglists)
  (let* ((m (make-mutex))
	 (threads '())
	 (results (make-list (length (car arglists))))
	 (result results))
    (do ((i 0 (+ 1 i)))
	((= i n)
	 (for-each join-thread threads)
	 results)
      (set! threads
	    (cons (begin-thread
		   (let loop ()
		     (lock-mutex m)
		     (if (null? result)
			 (unlock-mutex m)
			 (let ((args (map car arglists))
			       (my-result result))
			   (set! arglists (map cdr arglists))
			   (set! result (cdr result))
			   (unlock-mutex m)
			   (set-car! my-result (apply proc args))
			   (loop)))))
		  threads)))))

(define (n-par-for-each n proc . arglists)
  (let ((m (make-mutex))
	(threads '()))
    (do ((i 0 (+ 1 i)))
	((= i n)
	 (for-each join-thread threads))
      (set! threads
	    (cons (begin-thread
		   (let loop ()
		     (lock-mutex m)
		     (if (null? (car arglists))
			 (unlock-mutex m)
			 (let ((args (map car arglists)))
			   (set! arglists (map cdr arglists))
			   (unlock-mutex m)
			   (apply proc args)
			   (loop)))))
		  threads)))))

;;; The following procedure is motivated by the common and important
;;; case where a lot of work should be done, (not too much) in parallel,
;;; but the results need to be handled serially (for example when
;;; writing them to a file).
;;;
(define (n-for-each-par-map n s-proc p-proc . arglists)
  "Using N parallel processes, apply S-PROC in serial order on the results
of applying P-PROC on ARGLISTS."
  (let* ((m (make-mutex))
	 (threads '())
	 (no-result '(no-value))
	 (results (make-list (length (car arglists)) no-result))
	 (result results))
    (do ((i 0 (+ 1 i)))
	((= i n)
	 (for-each join-thread threads))
      (set! threads
	    (cons (begin-thread
		   (let loop ()
		     (lock-mutex m)
		     (cond ((null? results)
			    (unlock-mutex m))
			   ((not (eq? (car results) no-result))
			    (let ((arg (car results)))
			      ;; stop others from choosing to process results
			      (set-car! results no-result)
			      (unlock-mutex m)
			      (s-proc arg)
			      (lock-mutex m)
			      (set! results (cdr results))
			      (unlock-mutex m)
			      (loop)))
			   ((null? result)
			    (unlock-mutex m))
			   (else
			    (let ((args (map car arglists))
				  (my-result result))
			      (set! arglists (map cdr arglists))
			      (set! result (cdr result))
			      (unlock-mutex m)
			      (set-car! my-result (apply p-proc args))
			      (loop))))))
		  threads)))))

(define (thread-handler tag . args)
  (let ((n (length args))
	(p (current-error-port)))
    (display "In thread:" p)
    (newline p)
    (if (>= n 3)
        (display-error #f
                       p
                       (car args)
                       (cadr args)
                       (caddr args)
                       (if (= n 4)
                           (cadddr args)
                           '()))
        (begin
          (display "uncaught throw to " p)
          (display tag p)
          (display ": " p)
          (display args p)
          (newline p)))
    #f))

;;; Set system thread handler
(define %thread-handler thread-handler)

;;; threads.scm ends here
