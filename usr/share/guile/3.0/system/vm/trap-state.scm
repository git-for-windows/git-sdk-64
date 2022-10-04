;;; trap-state.scm: a set of traps

;; Copyright (C)  2010, 2013 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; Code:

(define-module (system vm trap-state)
  #:use-module (system base syntax)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (system vm vm)
  #:use-module (system vm traps)
  #:use-module (system vm trace)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:export (add-trap!
            list-traps
            trap-enabled?
            trap-name
            enable-trap!
            disable-trap!
            delete-trap!
            
            with-default-trap-handler
            install-trap-handler!

            add-trap-at-procedure-call!
            add-trace-at-procedure-call!
            add-trap-at-source-location!
            add-ephemeral-trap-at-frame-finish!
            add-ephemeral-stepping-trap!))

(define %default-trap-handler (make-fluid))

(define (default-trap-handler frame idx trap-name)
  (let ((default-handler (fluid-ref %default-trap-handler)))
    (if default-handler
        (default-handler frame idx trap-name)
        (warn "Trap with no handler installed" frame idx trap-name))))

(define-record <trap-wrapper>
  index
  enabled?
  trap
  name)

(define-record <trap-state>
  (handler default-trap-handler)
  (next-idx 0)
  (next-ephemeral-idx -1)
  (wrappers '()))

(define (trap-wrapper<? t1 t2)
  (< (trap-wrapper-index t1) (trap-wrapper-index t2)))

;; The interface that a trap provides to the outside world is that of a
;; procedure, which when called disables the trap, and returns a
;; procedure to enable the trap. Perhaps this is a bit too odd and we
;; should fix this.
(define (enable-trap-wrapper! wrapper)
  (if (trap-wrapper-enabled? wrapper)
      (error "Trap already enabled" (trap-wrapper-index wrapper))
      (let ((trap (trap-wrapper-trap wrapper)))
        (set! (trap-wrapper-trap wrapper) (trap))
        (set! (trap-wrapper-enabled? wrapper) #t))))

(define (disable-trap-wrapper! wrapper)
  (if (not (trap-wrapper-enabled? wrapper))
      (error "Trap already disabled" (trap-wrapper-index wrapper))
      (let ((trap (trap-wrapper-trap wrapper)))
        (set! (trap-wrapper-trap wrapper) (trap))
        (set! (trap-wrapper-enabled? wrapper) #f))))

(define (add-trap-wrapper! trap-state wrapper)
  (set! (trap-state-wrappers trap-state)
        (append (trap-state-wrappers trap-state) (list wrapper)))
  (trap-wrapper-index wrapper))

(define (remove-trap-wrapper! trap-state wrapper)
  (set! (trap-state-wrappers trap-state)
        (delq wrapper (trap-state-wrappers trap-state))))

(define (trap-state->trace-level trap-state)
  (fold (lambda (wrapper level)
          (if (trap-wrapper-enabled? wrapper)
              (1+ level)
              level))
        0
        (trap-state-wrappers trap-state)))

(define (wrapper-at-index trap-state idx)
  (let lp ((wrappers (trap-state-wrappers trap-state)))
    (cond
     ((null? wrappers)
      (warn "no wrapper found with index in trap-state" idx)
      #f)
     ((eqv? (trap-wrapper-index (car wrappers)) idx)
      (car wrappers))
     (else
      (lp (cdr wrappers))))))

(define (next-index! trap-state)
  (let ((idx (trap-state-next-idx trap-state)))
    (set! (trap-state-next-idx trap-state) (1+ idx))
    idx))

(define (next-ephemeral-index! trap-state)
  (let ((idx (trap-state-next-ephemeral-idx trap-state)))
    (set! (trap-state-next-ephemeral-idx trap-state) (1- idx))
    idx))

(define (handler-for-index trap-state idx)
  (lambda (frame)
    (let ((wrapper (wrapper-at-index trap-state idx))
          (handler (trap-state-handler trap-state)))
      (if wrapper
          (handler frame
                   (trap-wrapper-index wrapper)
                   (trap-wrapper-name wrapper))))))

(define (ephemeral-handler-for-index trap-state idx handler)
  (lambda (frame)
    (let ((wrapper (wrapper-at-index trap-state idx)))
      (if wrapper
          (begin
            (if (trap-wrapper-enabled? wrapper)
                (disable-trap-wrapper! wrapper))
            (remove-trap-wrapper! trap-state wrapper)
            (handler frame))))))



;;;
;;; Per-thread trap states
;;;

;; FIXME: This should be thread-local -- not something you can inherit
;; from a dynamic state.

(define %trap-state (make-parameter #f))

(define (the-trap-state)
  (or (%trap-state)
      (let ((ts (make-trap-state)))
        (%trap-state ts)
        ts)))



;;;
;;; API
;;;

(define* (with-default-trap-handler handler thunk
                                    #:optional (trap-state (the-trap-state)))
  (with-fluids ((%default-trap-handler handler))
    (dynamic-wind
      (lambda ()
        ;; Don't enable hooks if the handler is #f.
        (if handler
            (set-vm-trace-level! (trap-state->trace-level trap-state))))
      thunk
      (lambda ()
        (if handler
            (set-vm-trace-level! 0))))))

(define* (list-traps #:optional (trap-state (the-trap-state)))
  (map trap-wrapper-index (trap-state-wrappers trap-state)))

(define* (trap-name idx #:optional (trap-state (the-trap-state)))
  (and=> (wrapper-at-index trap-state idx)
         trap-wrapper-name))

(define* (trap-enabled? idx #:optional (trap-state (the-trap-state)))
  (and=> (wrapper-at-index trap-state idx)
         trap-wrapper-enabled?))

(define* (enable-trap! idx #:optional (trap-state (the-trap-state)))
  (and=> (wrapper-at-index trap-state idx)
         enable-trap-wrapper!))

(define* (disable-trap! idx #:optional (trap-state (the-trap-state)))
  (and=> (wrapper-at-index trap-state idx)
         disable-trap-wrapper!))

(define* (delete-trap! idx #:optional (trap-state (the-trap-state)))
  (and=> (wrapper-at-index trap-state idx)
         (lambda (wrapper)
           (if (trap-wrapper-enabled? wrapper)
               (disable-trap-wrapper! wrapper))
           (remove-trap-wrapper! trap-state wrapper))))

(define* (install-trap-handler! handler #:optional (trap-state (the-trap-state)))
  (set! (trap-state-handler trap-state) handler))

(define* (add-trap-at-procedure-call! proc #:optional (trap-state (the-trap-state)))
  (let* ((idx (next-index! trap-state))
         (trap (trap-at-procedure-call
                proc
                (handler-for-index trap-state idx))))
    (add-trap-wrapper!
     trap-state
     (make-trap-wrapper
      idx #t trap
      (format #f "Breakpoint at ~a" proc)))))

(define* (add-trace-at-procedure-call! proc
                                       #:optional (trap-state (the-trap-state)))
  (let* ((idx (next-index! trap-state))
         (trap (trace-calls-to-procedure
                proc
                #:prefix (format #f "Trap ~a: " idx))))
    (add-trap-wrapper!
     trap-state
     (make-trap-wrapper
      idx #t trap
      (format #f "Tracepoint at ~a" proc)))))

(define* (add-trap-at-source-location! file user-line
                                       #:optional (trap-state (the-trap-state)))
  (let* ((idx (next-index! trap-state))
         (trap (trap-at-source-location file user-line
                                        (handler-for-index trap-state idx))))
    (add-trap-wrapper!
     trap-state
     (make-trap-wrapper
      idx #t trap
      (format #f "Breakpoint at ~a:~a" file user-line)))))

;; handler := frame -> nothing
(define* (add-ephemeral-trap-at-frame-finish! frame handler
                                              #:optional (trap-state
                                                          (the-trap-state)))
  (let* ((idx (next-ephemeral-index! trap-state))
         (trap (trap-frame-finish
                frame
                (ephemeral-handler-for-index trap-state idx handler)
                (lambda (frame) (delete-trap! idx trap-state)))))
    (add-trap-wrapper!
     trap-state
     (make-trap-wrapper
      idx #t trap
      (format #f "Return from ~a" frame)))))

(define (source-string source)
  (if source
      (format #f "~a:~a:~a" (or (source:file source) "unknown file")
              (source:line-for-user source) (source:column source))
      "unknown source location"))

(define* (add-ephemeral-stepping-trap! frame handler
                                       #:optional (trap-state
                                                   (the-trap-state))
                                       #:key (into? #t) (instruction? #f))
  (define (wrap-predicate-according-to-into predicate)
    (if into?
        predicate
        (let ((fp (frame-address frame)))
          (lambda (f)
            (and (<= (frame-address f) fp)
                 (predicate f))))))
  
  (let* ((source (frame-source frame))
         (idx (next-ephemeral-index! trap-state))
         (trap (trap-matching-instructions
                (wrap-predicate-according-to-into
                 (if instruction?
                     (lambda (f) #t)
                     (lambda (f) (not (equal? (frame-source f) source)))))
                (ephemeral-handler-for-index trap-state idx handler))))
    (add-trap-wrapper!
     trap-state
     (make-trap-wrapper
      idx #t trap
      (if instruction?
          (if into?
              "Step to different instruction"
              (format #f "Step to different instruction in ~a" frame))
          (if into?
              (format #f "Step into ~a" (source-string source)) 
              (format #f "Step out of ~a" (source-string source))))))))

(define* (add-trap! trap name #:optional (trap-state (the-trap-state)))
  (let* ((idx (next-index! trap-state)))
    (add-trap-wrapper!
     trap-state
     (make-trap-wrapper idx #t trap name))))
