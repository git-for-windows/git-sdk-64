;;; srfi-18.scm --- Multithreading support

;; Copyright (C) 2008, 2009, 2010, 2014 Free Software Foundation, Inc.
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

;;; Author: Julian Graham <julian.graham@aya.yale.edu>
;;; Date: 2008-04-11

;;; Commentary:

;; This is an implementation of SRFI-18 (Multithreading support).
;;
;; All procedures defined in SRFI-18, which are not already defined in
;; the Guile core library, are exported.
;;
;; This module is fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-18)
  :use-module (srfi srfi-34)
  :export (

;;; Threads
 ;; current-thread			<= in the core
 ;; thread?				<= in the core
 make-thread
 thread-name
 thread-specific
 thread-specific-set!
 thread-start!
 thread-yield!
 thread-sleep!
 thread-terminate!
 thread-join!

;;; Mutexes
 ;; mutex?				<= in the core
 make-mutex
 mutex-name
 mutex-specific
 mutex-specific-set!
 mutex-state
 mutex-lock!
 mutex-unlock!

;;; Condition variables
 ;; condition-variable?			<= in the core
 make-condition-variable
 condition-variable-name
 condition-variable-specific
 condition-variable-specific-set!
 condition-variable-signal!
 condition-variable-broadcast!
 condition-variable-wait!

;;; Time
 current-time
 time?
 time->seconds
 seconds->time
 
 current-exception-handler
 with-exception-handler
 raise
 join-timeout-exception?
 abandoned-mutex-exception?
 terminated-thread-exception?
 uncaught-exception?
 uncaught-exception-reason
 )
  :re-export (current-thread thread? mutex? condition-variable?)
  :replace (current-time 
	    make-thread 
	    make-mutex 
	    make-condition-variable
	    raise))

(if (not (provided? 'threads))
    (error "SRFI-18 requires Guile with threads support"))

(cond-expand-provide (current-module) '(srfi-18))

(define (check-arg-type pred arg caller)
  (if (pred arg)
      arg
      (scm-error 'wrong-type-arg caller
		 "Wrong type argument: ~S" (list arg) '())))

(define abandoned-mutex-exception (list 'abandoned-mutex-exception))
(define join-timeout-exception (list 'join-timeout-exception))
(define terminated-thread-exception (list 'terminated-thread-exception))
(define uncaught-exception (list 'uncaught-exception))

(define object-names (make-weak-key-hash-table))
(define object-specifics (make-weak-key-hash-table))
(define thread-start-conds (make-weak-key-hash-table))
(define thread-exception-handlers (make-weak-key-hash-table))

;; EXCEPTIONS

(define raise (@ (srfi srfi-34) raise))
(define (initial-handler obj) 
  (srfi-18-exception-preserver (cons uncaught-exception obj)))

(define thread->exception (make-object-property))

(define (srfi-18-exception-preserver obj)
  (if (or (terminated-thread-exception? obj)
          (uncaught-exception? obj))
      (set! (thread->exception (current-thread)) obj)))

(define (srfi-18-exception-handler key . args)

  ;; SRFI 34 exceptions continue to bubble up no matter who handles them, so
  ;; if one is caught at this level, it has already been taken care of by
  ;; `initial-handler'.

  (and (not (eq? key 'srfi-34))
       (srfi-18-exception-preserver (if (null? args) 
					(cons uncaught-exception key)
					(cons* uncaught-exception key args)))))

(define (current-handler-stack)
  (let ((ct (current-thread)))
    (or (hashq-ref thread-exception-handlers ct)
	(hashq-set! thread-exception-handlers ct (list initial-handler)))))

(define (with-exception-handler handler thunk)
  (let ((ct (current-thread))
        (hl (current-handler-stack)))
    (check-arg-type procedure? handler "with-exception-handler") 
    (check-arg-type thunk? thunk "with-exception-handler")
    (hashq-set! thread-exception-handlers ct (cons handler hl))
    (apply (@ (srfi srfi-34) with-exception-handler) 
           (list (lambda (obj)
                   (hashq-set! thread-exception-handlers ct hl) 
                   (handler obj))
                 (lambda () 
                   (call-with-values thunk
                     (lambda res
                       (hashq-set! thread-exception-handlers ct hl)
                       (apply values res))))))))

(define (current-exception-handler)
  (car (current-handler-stack)))

(define (join-timeout-exception? obj) (eq? obj join-timeout-exception))
(define (abandoned-mutex-exception? obj) (eq? obj abandoned-mutex-exception))
(define (uncaught-exception? obj) 
  (and (pair? obj) (eq? (car obj) uncaught-exception)))
(define (uncaught-exception-reason exc)
  (cdr (check-arg-type uncaught-exception? exc "uncaught-exception-reason")))
(define (terminated-thread-exception? obj) 
  (eq? obj terminated-thread-exception))

;; THREADS

;; Create a new thread and prevent it from starting using a condition variable.
;; Once started, install a top-level exception handler that rethrows any 
;; exceptions wrapped in an uncaught-exception wrapper. 

(define make-thread 
  (let ((make-cond-wrapper (lambda (thunk lcond lmutex scond smutex)
			     (lambda () 
			       (lock-mutex lmutex)
			       (signal-condition-variable lcond)
			       (lock-mutex smutex)
			       (unlock-mutex lmutex)
			       (wait-condition-variable scond smutex)
			       (unlock-mutex smutex)
			       (with-exception-handler initial-handler 
						       thunk)))))
    (lambda (thunk . name)
      (let ((n (and (pair? name) (car name)))

	    (lm (make-mutex 'launch-mutex))
	    (lc (make-condition-variable 'launch-condition-variable))
	    (sm (make-mutex 'start-mutex))
	    (sc (make-condition-variable 'start-condition-variable)))
	
	(lock-mutex lm)
	(let ((t (call-with-new-thread (make-cond-wrapper thunk lc lm sc sm)
				       srfi-18-exception-handler)))
	  (hashq-set! thread-start-conds t (cons sm sc))
	  (and n (hashq-set! object-names t n))
	  (wait-condition-variable lc lm)
	  (unlock-mutex lm)
	  t)))))

(define (thread-name thread)
  (hashq-ref object-names (check-arg-type thread? thread "thread-name")))

(define (thread-specific thread)
  (hashq-ref object-specifics 
	     (check-arg-type thread? thread "thread-specific")))

(define (thread-specific-set! thread obj)
  (hashq-set! object-specifics
	      (check-arg-type thread? thread "thread-specific-set!")
	      obj)
  *unspecified*)

(define (thread-start! thread)
  (let ((x (hashq-ref thread-start-conds
		      (check-arg-type thread? thread "thread-start!"))))
    (and x (let ((smutex (car x))
		 (scond (cdr x)))
	     (hashq-remove! thread-start-conds thread)
	     (lock-mutex smutex)
	     (signal-condition-variable scond)
	     (unlock-mutex smutex)))
    thread))

(define (thread-yield!) (yield) *unspecified*)

(define (thread-sleep! timeout)
  (let* ((ct (time->seconds (current-time)))
	 (t (cond ((time? timeout) (- (time->seconds timeout) ct))
		  ((number? timeout) (- timeout ct))
		  (else (scm-error 'wrong-type-arg "thread-sleep!"
				   "Wrong type argument: ~S" 
				   (list timeout) 
				   '()))))
	 (secs (inexact->exact (truncate t)))
	 (usecs (inexact->exact (truncate (* (- t secs) 1000000)))))
    (and (> secs 0) (sleep secs))
    (and (> usecs 0) (usleep usecs))
    *unspecified*))

;; A convenience function for installing exception handlers on SRFI-18 
;; primitives that resume the calling continuation after the handler is 
;; invoked -- this resolves a behavioral incompatibility with Guile's
;; implementation of SRFI-34, which uses lazy-catch and rethrows handled
;; exceptions.  (SRFI-18, "Primitives and exceptions")

(define (wrap thunk)
  (lambda (continuation)
    (with-exception-handler (lambda (obj)
			      ((current-exception-handler) obj)
			      (continuation))
			    thunk)))

;; A pass-thru to cancel-thread that first installs a handler that throws
;; terminated-thread exception, as per SRFI-18, 

(define (thread-terminate! thread)
  (define (thread-terminate-inner!)
    (let ((current-handler (thread-cleanup thread)))
      (if (thunk? current-handler)
	  (set-thread-cleanup! thread 
			       (lambda ()
				 (with-exception-handler initial-handler
							 current-handler) 
				 (srfi-18-exception-preserver
				  terminated-thread-exception)))
	  (set-thread-cleanup! thread
			       (lambda () (srfi-18-exception-preserver
					   terminated-thread-exception))))
      (cancel-thread thread)
      *unspecified*))
  (thread-terminate-inner!))

(define (thread-join! thread . args) 
  (define thread-join-inner!
    (wrap (lambda ()
	    (let ((v (apply join-thread (cons thread args)))
		  (e (thread->exception thread)))
	      (if (and (= (length args) 1) (not v))
		  (raise join-timeout-exception))
	      (if e (raise e))
	      v))))
  (call/cc thread-join-inner!))

;; MUTEXES
;; These functions are all pass-thrus to the existing Guile implementations.

(define make-mutex
  (lambda name
    (let ((n (and (pair? name) (car name)))
	  (m ((@ (guile) make-mutex) 
	      'unchecked-unlock 
	      'allow-external-unlock 
	      'recursive)))
      (and n (hashq-set! object-names m n)) m)))

(define (mutex-name mutex)
  (hashq-ref object-names (check-arg-type mutex? mutex "mutex-name")))

(define (mutex-specific mutex)
  (hashq-ref object-specifics 
	     (check-arg-type mutex? mutex "mutex-specific")))

(define (mutex-specific-set! mutex obj)
  (hashq-set! object-specifics
	      (check-arg-type mutex? mutex "mutex-specific-set!")
	      obj)
  *unspecified*)

(define (mutex-state mutex)
  (let ((owner (mutex-owner mutex)))
    (if owner
	(if (thread-exited? owner) 'abandoned owner)
	(if (> (mutex-level mutex) 0) 'not-owned 'not-abandoned))))

(define (mutex-lock! mutex . args) 
  (define mutex-lock-inner!
    (wrap (lambda ()
	    (catch 'abandoned-mutex-error
		   (lambda () (apply lock-mutex (cons mutex args)))
		   (lambda (key . args) (raise abandoned-mutex-exception))))))
  (call/cc mutex-lock-inner!))

(define (mutex-unlock! mutex . args) 
  (apply unlock-mutex (cons mutex args)))

;; CONDITION VARIABLES
;; These functions are all pass-thrus to the existing Guile implementations.

(define make-condition-variable
  (lambda name
    (let ((n (and (pair? name) (car name)))
	  (m ((@ (guile) make-condition-variable))))
      (and n (hashq-set! object-names m n)) m)))

(define (condition-variable-name condition-variable)
  (hashq-ref object-names (check-arg-type condition-variable? 
					  condition-variable
					  "condition-variable-name")))

(define (condition-variable-specific condition-variable)
  (hashq-ref object-specifics (check-arg-type condition-variable? 
					      condition-variable 
					      "condition-variable-specific")))

(define (condition-variable-specific-set! condition-variable obj)
  (hashq-set! object-specifics
	      (check-arg-type condition-variable? 
			      condition-variable 
			      "condition-variable-specific-set!")
	      obj)
  *unspecified*)

(define (condition-variable-signal! cond) 
  (signal-condition-variable cond) 
  *unspecified*)

(define (condition-variable-broadcast! cond)
  (broadcast-condition-variable cond)
  *unspecified*)

;; TIME

(define current-time gettimeofday)
(define (time? obj)
  (and (pair? obj)
       (let ((co (car obj))) (and (integer? co) (>= co 0)))
       (let ((co (cdr obj))) (and (integer? co) (>= co 0)))))

(define (time->seconds time) 
  (and (check-arg-type time? time "time->seconds")
       (+ (car time) (/ (cdr time) 1000000))))

(define (seconds->time x)
  (and (check-arg-type number? x "seconds->time")
       (let ((fx (truncate x)))
	 (cons (inexact->exact fx)
	       (inexact->exact (truncate (* (- x fx) 1000000)))))))

;; srfi-18.scm ends here
