;;; Error handling in the REPL

;; Copyright (C) 2001, 2009, 2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

(define-module (system repl error-handling)
  #:use-module (system base pmatch)
  #:use-module (system vm trap-state)
  #:use-module (system repl debug)
  #:use-module (ice-9 format)
  #:export (call-with-error-handling
            with-error-handling))




;;;
;;; Error handling via repl debugging
;;;

(define (error-string stack key args)
  (call-with-output-string
   (lambda (port)
     (let ((frame (and (< 0 (vector-length stack)) (vector-ref stack 0))))
       (print-exception port frame key args)))))
                  
(define* (call-with-error-handling thunk #:key
                                   (on-error 'debug) (post-error 'catch)
                                   (pass-keys '(quit)) (trap-handler 'debug)
                                   (report-keys '(stack-overflow out-of-memory)))
  (let ((in (current-input-port))
        (out (current-output-port))
        (err (current-error-port)))
    (define (with-saved-ports thunk)
      (with-input-from-port in
        (lambda ()
          (with-output-to-port out
            (lambda ()
              (with-error-to-port err
                thunk))))))

    (define (debug-trap-handler frame trap-idx trap-name)
      (let* ((tag (and (pair? (fluid-ref %stacks))
                       (cdr (fluid-ref %stacks))))
             (stack (narrow-stack->vector
                     (make-stack frame)
                     ;; Take the stack from the given frame, cutting 0
                     ;; frames.
                     0
                     ;; Narrow the end of the stack to the most recent
                     ;; start-stack.
                     tag
                     ;; And one more frame, because %start-stack
                     ;; invoking the start-stack thunk has its own frame
                     ;; too.
                     0 (and tag 1)))
             (error-msg (if trap-idx
                            (format #f "Trap ~d: ~a" trap-idx trap-name)
                            trap-name))
             (debug (make-debug stack 0 error-msg)))
        (with-saved-ports
         (lambda ()
           (if trap-idx
               (begin
                 (format #t "~a~%" error-msg)
                 (format #t "Entering a new prompt.  ")
                 (format #t "Type `,bt' for a backtrace or `,q' to continue.\n")))
           ((@ (system repl repl) start-repl) #:debug debug)))))

    (define (null-trap-handler frame trap-idx trap-name)
      #t)

    (define le-trap-handler
      (case trap-handler
        ((debug) debug-trap-handler)
        ((pass) null-trap-handler)
        ((disabled) #f)
        (else (error "Unknown trap-handler strategy" trap-handler))))

    (define (report-error key args)
      (with-saved-ports
       (lambda ()
         (run-hook before-error-hook)
         (print-exception err #f key args)
         (run-hook after-error-hook)
         (force-output err))))

    (catch #t
      (lambda () 
        (with-default-trap-handler le-trap-handler
          (lambda () (%start-stack #t thunk))))

      (case post-error
        ((report)
         (lambda (key . args)
           (if (memq key pass-keys)
               (apply throw key args)
               (begin
                 (report-error key args)
                 (if #f #f)))))
        ((catch)
         (lambda (key . args)
           (when (memq key pass-keys)
             (apply throw key args))
           (when (memq key report-keys)
             (report-error key args))
           (if #f #f)))
        (else
         (if (procedure? post-error)
             (lambda (k . args)
               (apply (if (memq k pass-keys) throw post-error) k args))
             (error "Unknown post-error strategy" post-error))))

      (case on-error
        ((debug)
         (lambda (key . args)
           (if (not (memq key pass-keys))
               (let* ((tag (and (pair? (fluid-ref %stacks))
                                (cdr (fluid-ref %stacks))))
                      (stack (narrow-stack->vector
                              (make-stack #t)
                              ;; Cut three frames from the top of the stack:
                              ;; make-stack, this one, and the throw handler.
                              3
                              ;; Narrow the end of the stack to the most recent
                              ;; start-stack.
                              tag
                              ;; And one more frame, because %start-stack invoking
                              ;; the start-stack thunk has its own frame too.
                              0 (and tag 1)))
                      (error-msg (error-string stack key args))
                      (debug (make-debug stack 0 error-msg)))
                 (with-saved-ports
                  (lambda ()
                    (format #t "~a~%" error-msg)
                    (format #t "Entering a new prompt.  ")
                    (format #t "Type `,bt' for a backtrace or `,q' to continue.\n")
                    ((@ (system repl repl) start-repl) #:debug debug)))))))
        ((report)
         (lambda (key . args)
           (unless (memq key pass-keys)
             (report-error key args))
           (if #f #f)))
        ((backtrace)
         (lambda (key . args)
           (if (not (memq key pass-keys))
               (let* ((tag (and (pair? (fluid-ref %stacks))
                                (cdr (fluid-ref %stacks))))
                      (frames (narrow-stack->vector
                               (make-stack #t)
                               ;; Narrow as above, for the debugging case.
                               3 tag 0 (and tag 1))))
                 (with-saved-ports (lambda () (print-frames frames)))
                 (report-error key args)
                 (if #f #f)))))
        ((pass)
         (lambda (key . args)
           ;; fall through to rethrow
           #t))
        (else
         (if (procedure? on-error)
             (lambda (k . args)
               (apply (if (memq k pass-keys) throw on-error) k args))
             (error "Unknown on-error strategy" on-error)))))))

(define-syntax-rule (with-error-handling form)
  (call-with-error-handling (lambda () form)))
