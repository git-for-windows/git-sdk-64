;;; srfi-18.scm --- Multithreading support

;; Copyright (C) 2008, 2009, 2010, 2012, 2014 Free Software Foundation, Inc.
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
  #:use-module ((ice-9 threads) #:prefix threads:)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-34) #:prefix srfi-34:)
  #:use-module ((srfi srfi-35) #:select (define-condition-type
                                          &error
                                          condition))
  #:export (;; Threads
            make-thread
            thread-name
            thread-specific
            thread-specific-set!
            thread-start!
            thread-yield!
            thread-sleep!
            thread-terminate!
            thread-join!

            ;; Mutexes
            make-mutex
            mutex
            mutex-name
            mutex-specific
            mutex-specific-set!
            mutex-state
            mutex-lock!
            mutex-unlock!

            ;; Condition variables
            make-condition-variable
            condition-variable-name
            condition-variable-specific
            condition-variable-specific-set!
            condition-variable-signal!
            condition-variable-broadcast!

            ;; Time
            current-time
            time?
            time->seconds
            seconds->time
 
            current-exception-handler
            with-exception-handler
            join-timeout-exception?
            abandoned-mutex-exception?
            terminated-thread-exception?
            uncaught-exception?
            uncaught-exception-reason)
  #:re-export ((srfi-34:raise . raise))
  #:replace (current-time
             current-thread
             thread?
             make-thread
             make-mutex
             mutex?
             make-condition-variable
             condition-variable?))

(unless (provided? 'threads)
  (error "SRFI-18 requires Guile with threads support"))

(cond-expand-provide (current-module) '(srfi-18))

(define (check-arg-type pred arg caller)
  (if (pred arg)
      arg
      (scm-error 'wrong-type-arg caller
		 "Wrong type argument: ~S" (list arg) '())))

(define-condition-type &abandoned-mutex-exception &error
  abandoned-mutex-exception?)
(define-condition-type &join-timeout-exception &error
  join-timeout-exception?)
(define-condition-type &terminated-thread-exception &error
  terminated-thread-exception?)
(define-condition-type &uncaught-exception &error
  uncaught-exception?
  (reason uncaught-exception-reason))

(define-record-type <mutex>
  (%make-mutex prim name specific owner abandoned?)
  mutex?
  (prim mutex-prim)
  (name mutex-name)
  (specific mutex-specific mutex-specific-set!)
  (owner mutex-owner set-mutex-owner!)
  (abandoned? mutex-abandoned? set-mutex-abandoned?!))

(define-record-type <condition-variable>
  (%make-condition-variable prim name specific)
  condition-variable?
  (prim condition-variable-prim)
  (name condition-variable-name)
  (specific condition-variable-specific condition-variable-specific-set!))

(define-record-type <thread>
  (%make-thread prim name specific start-conds exception)
  thread?
  (prim thread-prim set-thread-prim!)
  (name thread-name)
  (specific thread-specific thread-specific-set!)
  (start-conds thread-start-conds set-thread-start-conds!)
  (exception thread-exception set-thread-exception!))

(define current-thread (make-parameter (%make-thread #f #f #f #f #f)))
(define thread-mutexes (make-parameter #f))

;; EXCEPTIONS

;; All threads created by SRFI-18 have an initial handler installed that
;; will squirrel away an uncaught exception to allow it to bubble out to
;; joining threads.  However for the main thread and other threads not
;; created by SRFI-18, just let the exception bubble up by passing on
;; doing anything with the exception.
(define (exception-handler-for-foreign-threads obj)
  (values))

(define current-exception-handler
  (make-parameter exception-handler-for-foreign-threads))

(define (with-exception-handler handler thunk)
  (check-arg-type procedure? handler "with-exception-handler")
  (check-arg-type thunk? thunk "with-exception-handler")
  (srfi-34:with-exception-handler
   (let ((prev-handler (current-exception-handler)))
     (lambda (obj)
       (parameterize ((current-exception-handler prev-handler))
         (handler obj))))
   (lambda ()
     (parameterize ((current-exception-handler handler))
       (thunk)))))

;; THREADS

;; Create a new thread and prevent it from starting using a condition variable.
;; Once started, install a top-level exception handler that rethrows any 
;; exceptions wrapped in an uncaught-exception wrapper. 

(define (with-thread-mutex-cleanup thunk)
  (let ((mutexes (make-weak-key-hash-table)))
    (dynamic-wind
      values
      (lambda ()
        (parameterize ((thread-mutexes mutexes))
          (thunk)))
      (lambda ()
        (let ((thread (current-thread)))
          (hash-for-each (lambda (mutex _)
                           (when (eq? (mutex-owner mutex) thread)
                             (abandon-mutex! mutex)))
                         mutexes))))))

(define* (make-thread thunk #:optional name)
  (let* ((sm (make-mutex 'start-mutex))
         (sc (make-condition-variable 'start-condition-variable))
         (thread (%make-thread #f name #f (cons sm sc) #f)))
    (mutex-lock! sm)
    (let ((prim (threads:call-with-new-thread
                 (lambda ()
                   (catch #t
                     (lambda ()
                       (parameterize ((current-thread thread))
                         (with-thread-mutex-cleanup
                          (lambda ()
                            (mutex-lock! sm)
                            (condition-variable-signal! sc)
                            (mutex-unlock! sm sc)
                            (thunk)))))
                     (lambda (key . args)
                       (set-thread-exception!
                        thread
                        (condition (&uncaught-exception
                                    (reason
                                     (match (cons key args)
                                       (('srfi-34 obj) obj)
                                       (obj obj))))))))))))
      (set-thread-prim! thread prim)
      (mutex-unlock! sm sc)
      thread)))

(define (thread-start! thread)
  (match (thread-start-conds thread)
    ((smutex . scond)
     (set-thread-start-conds! thread #f)
     (mutex-lock! smutex)
     (condition-variable-signal! scond)
     (mutex-unlock! smutex))
    (#f #f))
  thread)

(define (thread-yield!) (threads:yield) *unspecified*)

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
    (when (> secs 0) (sleep secs))
    (when (> usecs 0) (usleep usecs))
    *unspecified*))

;; Whereas SRFI-34 leaves the continuation of a call to an exception
;; handler unspecified, SRFI-18 has this to say:
;;
;;   When one of the primitives defined in this SRFI raises an exception
;;   defined in this SRFI, the exception handler is called with the same
;;   continuation as the primitive (i.e. it is a tail call to the
;;   exception handler).
;;
;; Therefore arrange for exceptions thrown by SRFI-18 primitives to run
;; handlers with the continuation of the primitive call, for those
;; primitives that throw exceptions.

(define (with-exception-handlers-here thunk)
  (let ((tag (make-prompt-tag)))
    (call-with-prompt tag
      (lambda ()
        (with-exception-handler (lambda (exn) (abort-to-prompt tag exn))
          thunk))
      (lambda (k exn)
        ((current-exception-handler) exn)))))

;; A unique value.
(define %cancel-sentinel (list 'cancelled))
(define (thread-terminate! thread)
  (threads:cancel-thread (thread-prim thread) %cancel-sentinel)
  *unspecified*)

;; A unique value.
(define %timeout-sentinel (list 1))
(define* (thread-join! thread #:optional (timeout %timeout-sentinel)
                       (timeoutval %timeout-sentinel))
  (let ((t (thread-prim thread)))
    (with-exception-handlers-here
     (lambda ()
       (let* ((v (if (eq? timeout %timeout-sentinel)
                     (threads:join-thread t)
                     (threads:join-thread t timeout %timeout-sentinel))))
         (cond
          ((eq? v %timeout-sentinel)
           (if (eq? timeoutval %timeout-sentinel)
               (srfi-34:raise (condition (&join-timeout-exception)))
               timeoutval))
          ((eq? v %cancel-sentinel)
           (srfi-34:raise (condition (&terminated-thread-exception))))
          ((thread-exception thread) => srfi-34:raise)
          (else v)))))))

;; MUTEXES

(define* (make-mutex #:optional name)
  (%make-mutex (threads:make-mutex 'allow-external-unlock) name #f #f #f))

(define (mutex-state mutex)
  (cond
   ((mutex-abandoned? mutex) 'abandoned)
   ((mutex-owner mutex))
   ((> (threads:mutex-level (mutex-prim mutex)) 0) 'not-owned)
   (else 'not-abandoned)))

(define (abandon-mutex! mutex)
  (set-mutex-abandoned?! mutex #t)
  (threads:unlock-mutex (mutex-prim mutex)))

(define* (mutex-lock! mutex #:optional timeout (thread (current-thread)))
  (let ((mutexes (thread-mutexes)))
    (when mutexes
      (hashq-set! mutexes mutex #t)))
  (with-exception-handlers-here
   (lambda ()
     (cond
      ((threads:lock-mutex (mutex-prim mutex) timeout)
       (set-mutex-owner! mutex thread)
       (when (mutex-abandoned? mutex)
         (set-mutex-abandoned?! mutex #f)
         (srfi-34:raise
          (condition (&abandoned-mutex-exception))))
       #t)
      (else #f)))))

(define %unlock-sentinel (list 'unlock))
(define* (mutex-unlock! mutex #:optional (cond-var %unlock-sentinel)
                        (timeout %unlock-sentinel))
  (when (mutex-owner mutex)
    (set-mutex-owner! mutex #f)
    (cond
     ((eq? cond-var %unlock-sentinel)
      (threads:unlock-mutex (mutex-prim mutex)))
     ((eq? timeout %unlock-sentinel)
      (threads:wait-condition-variable (condition-variable-prim cond-var)
                                       (mutex-prim mutex))
      (threads:unlock-mutex (mutex-prim mutex)))
     ((threads:wait-condition-variable (condition-variable-prim cond-var)
                                       (mutex-prim mutex)
                                       timeout)
      (threads:unlock-mutex (mutex-prim mutex)))
     (else #f))))

;; CONDITION VARIABLES
;; These functions are all pass-thrus to the existing Guile implementations.

(define* (make-condition-variable #:optional name)
  (%make-condition-variable (threads:make-condition-variable) name #f))

(define (condition-variable-signal! cond) 
  (threads:signal-condition-variable (condition-variable-prim cond))
  *unspecified*)

(define (condition-variable-broadcast! cond)
  (threads:broadcast-condition-variable (condition-variable-prim cond))
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
