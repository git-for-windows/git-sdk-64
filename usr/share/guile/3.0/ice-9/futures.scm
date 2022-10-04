;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2010, 2011, 2012 Free Software Foundation, Inc.
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

(define-module (ice-9 futures)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 q)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (ice-9 threads)
  #:export (future make-future future? touch))

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;;
;;; Commentary:
;;;
;;; This module provides an implementation of futures, a mechanism for
;;; fine-grain parallelism.  Futures were first described by Henry Baker
;;; in ``The Incremental Garbage Collection of Processes'', 1977, and
;;; then implemented in MultiLisp (an implicit variant thereof, i.e.,
;;; without `touch'.)
;;;
;;; This modules uses a fixed thread pool, normally one per CPU core.
;;; Futures are off-loaded to these threads, when they are idle.
;;;
;;; Code:


;;;
;;; Futures.
;;;

(define-record-type <future>
  (%make-future thunk state mutex completion)
  future?
  (thunk        future-thunk  set-future-thunk!)
  (state        future-state  set-future-state!)  ; done | started | queued
  (result       future-result set-future-result!)
  (mutex        future-mutex)
  (completion   future-completion))               ; completion cond. var.

(set-record-type-printer!
 <future>
 (lambda (future port)
   (simple-format port "#<future ~a ~a ~s>"
                  (number->string (object-address future) 16)
                  (future-state future)
                  (future-thunk future))))

(define (make-future thunk)
  "Return a new future for THUNK.  Execution may start at any point
concurrently, or it can start at the time when the returned future is
touched."
  (create-workers!)
  (let ((future (%make-future thunk 'queued
                              (make-mutex) (make-condition-variable))))
    (register-future! future)
    future))


;;;
;;; Future queues.
;;;

;; Global queue of pending futures.
;; TODO: Use per-worker queues to reduce contention.
(define %futures (make-q))

;; Lock for %FUTURES and %FUTURES-WAITING.
(define %futures-mutex (make-mutex))
(define %futures-available (make-condition-variable))

;; A mapping of nested futures to futures waiting for them to complete.
(define %futures-waiting '())

;; Whether currently running within a future.
(define %within-future? (make-parameter #f))

(define-syntax-rule (with-mutex m e0 e1 ...)
  ;; Copied from (ice-9 threads) to avoid circular dependency.
  (let ((x m))
    (dynamic-wind
      (lambda () (lock-mutex x))
      (lambda () (begin e0 e1 ...))
      (lambda () (unlock-mutex x)))))

(define %future-prompt
  ;; The prompt futures abort to when they want to wait for another
  ;; future.
  (make-prompt-tag))


(define (register-future! future)
  ;; Register FUTURE as being processable.
  (lock-mutex %futures-mutex)
  (enq! %futures future)
  (signal-condition-variable %futures-available)
  (unlock-mutex %futures-mutex))

(define (process-future! future)
  "Process FUTURE.  When FUTURE completes, return #t and update its
result; otherwise, when FUTURE touches a nested future that has not
completed yet, then suspend it and return #f.  Suspending a future
consists in capturing its continuation, marking it as `queued', and
adding it to the waiter queue."
  (let/ec return
    (let* ((suspend
            (lambda (cont future-to-wait)
              ;; FUTURE wishes to wait for the completion of FUTURE-TO-WAIT.
              ;; At this point, FUTURE is unlocked and in `started' state,
              ;; and FUTURE-TO-WAIT is unlocked.
              (with-mutex %futures-mutex
                (with-mutex (future-mutex future)
                  (set-future-thunk! future cont)
                  (set-future-state! future 'queued))

                (with-mutex (future-mutex future-to-wait)
                  ;; If FUTURE-TO-WAIT completed in the meantime, then
                  ;; reschedule FUTURE directly; otherwise, add it to the
                  ;; waiter queue.
                  (if (eq? 'done (future-state future-to-wait))
                      (begin
                        (enq! %futures future)
                        (signal-condition-variable %futures-available))
                      (set! %futures-waiting
                            (alist-cons future-to-wait future
                                        %futures-waiting))))

                (return #f))))
           (thunk (lambda ()
                    (call-with-prompt %future-prompt
                                      (lambda ()
                                        (parameterize ((%within-future? #t))
                                          ((future-thunk future))))
                                      suspend))))
      (set-future-result! future
                          (catch #t
                            (lambda ()
                              (call-with-values thunk
                                (lambda results
                                  (lambda ()
                                    (apply values results)))))
                            (lambda args
                              (lambda ()
                                (apply throw args)))))
      #t)))

(define (process-one-future)
  "Attempt to pick one future from the queue and process it."
  ;; %FUTURES-MUTEX must be locked on entry, and is locked on exit.
  (or (q-empty? %futures)
      (let ((future (deq! %futures)))
        (lock-mutex (future-mutex future))
        (case (future-state future)
          ((done started)
           ;; Nothing to do.
           (unlock-mutex (future-mutex future)))
          (else
           ;; Do the actual work.

           ;; We want to release %FUTURES-MUTEX so that other workers can
           ;; progress.  However, to avoid deadlocks, we have to unlock
           ;; FUTURE as well, to preserve lock ordering.
           (unlock-mutex (future-mutex future))
           (unlock-mutex %futures-mutex)

           (lock-mutex (future-mutex future))
           (if (eq? (future-state future) 'queued) ; lost the race?
               (begin                          ; no, so let's process it
                 (set-future-state! future 'started)
                 (unlock-mutex (future-mutex future))

                 (let ((done? (process-future! future)))
                   (when done?
                     (with-mutex %futures-mutex
                       (with-mutex (future-mutex future)
                         (set-future-state! future 'done)
                         (notify-completion future))))))
               (unlock-mutex (future-mutex future))) ; yes

           (lock-mutex %futures-mutex))))))

(define (process-futures)
  "Continuously process futures from the queue."
  (lock-mutex %futures-mutex)
  (let loop ()
    (when (q-empty? %futures)
      (wait-condition-variable %futures-available
                               %futures-mutex))

    (process-one-future)
    (loop)))

(define (notify-completion future)
  "Notify futures and callers waiting that FUTURE completed."
  ;; FUTURE and %FUTURES-MUTEX are locked.
  (broadcast-condition-variable (future-completion future))
  (let-values (((waiting remaining)
                (partition (match-lambda          ; TODO: optimize
                            ((waitee . _)
                             (eq? waitee future)))
                           %futures-waiting)))
    (set! %futures-waiting remaining)
    (for-each (match-lambda
               ((_ . waiter)
                (enq! %futures waiter)))
              waiting)))

(define (touch future)
  "Return the result of FUTURE, computing it if not already done."
  (define (work)
    ;; Do some work while waiting for FUTURE to complete.
    (lock-mutex %futures-mutex)
    (if (q-empty? %futures)
        (begin
          (unlock-mutex %futures-mutex)
          (with-mutex (future-mutex future)
            (unless (eq? 'done (future-state future))
              (wait-condition-variable (future-completion future)
                                       (future-mutex future)))))
        (begin
          (process-one-future)
          (unlock-mutex %futures-mutex))))

  (let loop ()
    (lock-mutex (future-mutex future))
    (case (future-state future)
      ((done)
       (unlock-mutex (future-mutex future)))
      ((started)
       (unlock-mutex (future-mutex future))
       (if (%within-future?)
           (abort-to-prompt %future-prompt future)
           (begin
             (work)
             (loop))))
      (else
       (unlock-mutex (future-mutex future))
       (work)
       (loop))))
  ((future-result future)))


;;;
;;; Workers.
;;;

(define %worker-count
  (if (provided? 'threads)
      (- (current-processor-count) 1)
      0))

;; A dock of workers that stay here forever.

;; TODO
;; 1. Allow the pool to be shrunk, as in libgomp (though that we'd
;;    need semaphores, which aren't yet in libguile!).
;; 2. Provide a `worker-count' fluid.
(define %workers '())

(define (%create-workers!)
  (with-mutex
   %futures-mutex
   ;; Setting 'create-workers!' to a no-op is an optimization, but it is
   ;; still possible for '%create-workers!' to be called more than once
   ;; from different threads.  Therefore, to avoid creating %workers more
   ;; than once (and thus creating too many threads), we check to make
   ;; sure %workers is empty within the critical section.
   (when (null? %workers)
     (set! %workers
           (unfold (lambda (i) (>= i %worker-count))
                   (lambda (i) (call-with-new-thread process-futures))
                   1+
                   0))
     (set! create-workers! (lambda () #t)))))

(define create-workers!
  (lambda () (%create-workers!)))


;;;
;;; Syntax.
;;;

(define-syntax-rule (future body)
  "Return a new future for BODY."
  (make-future (lambda () body)))

;;; Local Variables:
;;; eval: (put 'with-mutex 'scheme-indent-function 1)
;;; End:
