;;; Traps: stepping, breakpoints, and such.

;; Copyright (C)  2010, 2012, 2013, 2014, 2017 Free Software Foundation, Inc.

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
;;; Guile's debugging capabilities come from the hooks that its VM
;;; provides. For example, there is a hook that is fired when a function
;;; is called, and even a hook that gets fired at every retired
;;; instruction.
;;;
;;; But as the firing of these hooks is interleaved with the program
;;; execution, if we want to debug a program, we have to write an
;;; imperative program that mutates the state of these hooks, and to
;;; dispatch the hooks to a more semantic context.
;;;
;;; For example if we have placed a breakpoint at foo.scm:38, and
;;; determined that that location maps to the 18th instruction in
;;; procedure `bar', then we will need per-instruction hooks within
;;; `bar' -- but when running other procedures, we can have the
;;; per-instruction hooks off.
;;;
;;; Our approach is to define "traps". The behavior of a trap is
;;; specified when the trap is created. After creation, traps expose a
;;; limited, uniform interface: they are either on or off.
;;;
;;; To take our foo.scm:38 example again, we can define a trap that
;;; calls a function when control transfers to that source line --
;;; trap-at-source-location below. Calling the trap-at-source-location
;;; function adds to the VM hooks in such at way that it can do its job.
;;; The result of calling the function is a "disable-hook" closure that,
;;; when called, will turn off that trap.
;;;
;;; The result of calling the "disable-hook" closure, in turn, is an
;;; "enable-hook" closure, which when called turns the hook back on, and
;;; returns a "disable-hook" closure.
;;;
;;; It's a little confusing. The summary is, call these functions to add
;;; a trap; and call their return value to disable the trap.
;;;
;;; Code:

(define-module (system vm traps)
  #:use-module (system base pmatch)
  #:use-module (system vm vm)
  #:use-module (system vm debug)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:use-module (system xref)
  #:use-module (rnrs bytevectors)
  #:export (trap-at-procedure-call
            trap-in-procedure
            trap-instructions-in-procedure
            trap-at-procedure-ip-in-range
            trap-at-source-location
            trap-frame-finish
            trap-in-dynamic-extent
            trap-calls-in-dynamic-extent
            trap-instructions-in-dynamic-extent
            trap-calls-to-procedure
            trap-matching-instructions))

(define-syntax arg-check
  (syntax-rules ()
    ((_ arg predicate? message)
     (if (not (predicate? arg))
         (error "bad argument ~a: ~a" 'arg message)))
    ((_ arg predicate?)
     (if (not (predicate? arg))
         (error "bad argument ~a: expected ~a" 'arg 'predicate?)))))

(define (new-disabled-trap enable disable)
  (let ((enabled? #f))
    (define-syntax disabled?
      (identifier-syntax
       (disabled? (not enabled?))
       ((set! disabled? val) (set! enabled? (not val)))))
    
    (define* (enable-trap #:optional frame)
      (if enabled? (error "trap already enabled"))
      (enable frame)
      (set! enabled? #t)
      disable-trap)
    
    (define* (disable-trap #:optional frame)
      (if disabled? (error "trap already disabled"))
      (disable frame)
      (set! disabled? #t)
      enable-trap)

    enable-trap))

(define (new-enabled-trap frame enable disable)
  ((new-disabled-trap enable disable) frame))

;; Returns an absolute IP.
(define (program-last-ip prog)
  (let ((pdi (find-program-debug-info (program-code prog))))
    (and pdi
         (+ (program-debug-info-addr pdi)
            (program-debug-info-size pdi)))))

(define (frame-matcher proc)
  (let ((proc (if (struct? proc)
                  (procedure proc)
                  proc)))
    (cond
     ((program? proc)
      (let ((start (program-code proc))
            (end (program-last-ip proc)))
        (lambda (frame)
          (let ((ip (frame-instruction-pointer frame)))
            (and (<= start ip)
                 end (< ip end))))))
     ((struct? proc)
      (frame-matcher (procedure proc)))
     (else
      (error "Not a VM program" proc)))))

;; A basic trap, fires when a procedure is called.
;;
(define* (trap-at-procedure-call proc handler #:key
                                 (our-frame? (frame-matcher proc)))
  (arg-check proc procedure?)
  (arg-check handler procedure?)
  (let ()
    (define (apply-hook frame)
      (if (our-frame? frame)
          (handler frame)))

    (new-enabled-trap
     #f
     (lambda (frame)
       (add-hook! (vm-apply-hook) apply-hook))
     (lambda (frame)
       (remove-hook! (vm-apply-hook) apply-hook)))))

;; A more complicated trap, traps when control enters a procedure.
;;
;; Control can enter a procedure via:
;;  * A procedure call.
;;  * A return to a procedure's frame on the stack.
;;  * A continuation returning directly to an application of this
;;    procedure.
;;
;; Control can leave a procedure via:
;;  * A normal return from the procedure.
;;  * An application of another procedure.
;;  * An invocation of a continuation.
;;  * An abort.
;;
(define* (trap-in-procedure proc enter-handler exit-handler
                            #:key current-frame
                            (our-frame? (frame-matcher proc)))
  (arg-check proc procedure?)
  (arg-check enter-handler procedure?)
  (arg-check exit-handler procedure?)
  (let ((in-proc? #f))
    (define (enter-proc frame)
      (if in-proc?
          (warn "already in proc" frame)
          (begin
            (enter-handler frame)
            (set! in-proc? #t))))

    (define (exit-proc frame)
      (if in-proc?
          (begin
            (exit-handler frame)
            (set! in-proc? #f))
          (warn "not in proc" frame)))
    
    (define (apply-hook frame)
      (if in-proc?
          (exit-proc frame))
      (if (our-frame? frame)
          (enter-proc frame)))

    (define (pop-cont-hook frame . values)
      (if in-proc?
          (exit-proc frame))
      (if (our-frame? frame)
          (enter-proc frame)))

    (define (abort-hook frame . values)
      (if in-proc?
          (exit-proc frame))
      (if (our-frame? frame)
          (enter-proc frame)))

    (new-enabled-trap
     current-frame
     (lambda (frame)
       (add-hook! (vm-apply-hook) apply-hook)
       (add-hook! (vm-pop-continuation-hook) pop-cont-hook)
       (add-hook! (vm-abort-continuation-hook) abort-hook)
       (if (and frame (our-frame? frame))
           (enter-proc frame)))
     (lambda (frame)
       (if in-proc?
           (exit-proc frame))
       (remove-hook! (vm-apply-hook) apply-hook)
       (remove-hook! (vm-pop-continuation-hook) pop-cont-hook)
       (remove-hook! (vm-abort-continuation-hook) abort-hook)))))

;; Building on trap-in-procedure, we have trap-instructions-in-procedure
;;
(define* (trap-instructions-in-procedure proc next-handler exit-handler
                                         #:key current-frame
                                         (our-frame? (frame-matcher proc)))
  (arg-check proc procedure?)
  (arg-check next-handler procedure?)
  (arg-check exit-handler procedure?)
  (let ()
    (define (next-hook frame)
      (if (our-frame? frame)
          (next-handler frame)))
    
    (define (enter frame)
      (add-hook! (vm-next-hook) next-hook)
      (if frame (next-hook frame)))

    (define (exit frame)
      (exit-handler frame)
      (remove-hook! (vm-next-hook) next-hook))

    (trap-in-procedure proc enter exit
                       #:current-frame current-frame
                       #:our-frame? our-frame?)))

(define (non-negative-integer? x)
  (and (number? x) (integer? x) (exact? x) (not (negative? x))))

(define (positive-integer? x)
  (and (number? x) (integer? x) (exact? x) (positive? x)))

(define (range? x)
  (and (list? x)
       (and-map (lambda (x)
                  (and (pair? x)
                       (non-negative-integer? (car x))
                       (non-negative-integer? (cdr x))))
                x)))

(define (in-range? range i)
  (or-map (lambda (bounds)
            (and (<= (car bounds) i)
                 (< i (cdr bounds))))
          range))

;; Building on trap-instructions-in-procedure, we have
;; trap-at-procedure-ip-in-range.
;;
(define* (trap-at-procedure-ip-in-range proc range handler
                                        #:key current-frame
                                        (our-frame? (frame-matcher proc)))
  (arg-check proc procedure?)
  (arg-check range range?)
  (arg-check handler procedure?)
  (let ((fp-stack '()))
    (define (cull-frames! fp)
      (let lp ((frames fp-stack))
        (if (and (pair? frames) (< (car frames) fp))
            (lp (cdr frames))
            (set! fp-stack frames))))

    (define (next-handler frame)
      (let ((fp (frame-address frame))
            (ip (frame-instruction-pointer frame)))
        (cull-frames! fp)
        (let ((now-in-range? (in-range? range ip))
              (was-in-range? (and (pair? fp-stack) (= (car fp-stack) fp))))
          (cond
           (was-in-range?
            (if (not now-in-range?)
                (set! fp-stack (cdr fp-stack))))
           (now-in-range?
            (set! fp-stack (cons fp fp-stack))
            (handler frame))))))
    
    (define (exit-handler frame)
      (if (and (pair? fp-stack)
               (= (car fp-stack) (frame-address frame)))
          (set! fp-stack (cdr fp-stack))))
    
    (trap-instructions-in-procedure proc next-handler exit-handler
                                    #:current-frame current-frame
                                    #:our-frame? our-frame?)))

(define (program-sources-by-line proc file)
  (cond
   ((program? proc)
    (let ((code (program-code proc)))
      (let lp ((sources (program-sources proc))
               (out '()))
        (if (pair? sources)
            (lp (cdr sources)
                (pmatch (car sources)
                  ((,start-ip ,start-file ,start-line . ,start-col)
                   (if (equal? start-file file)
                       (acons start-line
                              (if (pair? (cdr sources))
                                  (pmatch (cadr sources)
                                    ((,end-ip . _)
                                     (cons (+ start-ip code)
                                           (+ end-ip code)))
                                    (else (error "unexpected")))
                                  (cons (+ start-ip code)
                                        (program-last-ip proc)))
                              out)
                       out))
                  (else (error "unexpected"))))
            (let ((alist '()))
              (for-each
               (lambda (pair)
                 (set! alist
                       (assv-set! alist (car pair)
                                  (cons (cdr pair)
                                        (or (assv-ref alist (car pair))
                                            '())))))
               out)
              (sort! alist (lambda (x y) (< (car x) (car y))))
              alist)))))
   (else '())))

(define (source->ip-range proc file line)
  (or (or-map (lambda (line-and-ranges)
                (cond
                 ((= (car line-and-ranges) line)
                  (cdr line-and-ranges))
                 ((> (car line-and-ranges) line)
                  (warn "no instructions found at" file ":" line
                        "; using line" (car line-and-ranges) "instead")
                  (cdr line-and-ranges))
                 (else #f)))
              (program-sources-by-line proc file))
      (begin
        (warn "no instructions found for" file ":" line)
        '())))

(define (source-closures-or-procedures file line)
  (let ((closures (source-closures file line)))
    (if (pair? closures)
        (values closures #t)
        (values (source-procedures file line) #f))))

;; Building on trap-on-instructions-in-procedure, we have
;; trap-at-source-location. The parameter `user-line' is one-indexed, as
;; a user counts lines, instead of zero-indexed, as Guile counts lines.
;;
(define* (trap-at-source-location file user-line handler #:key current-frame)
  (arg-check file string?)
  (arg-check user-line positive-integer?)
  (arg-check handler procedure?)
  (let ((traps #f))
    (call-with-values
        (lambda () (source-closures-or-procedures file (1- user-line)))
      (lambda (procs closures?)
        (new-enabled-trap
         current-frame
         (lambda (frame)
           (set! traps
                 (map
                  (lambda (proc)
                    (let ((range (source->ip-range proc file (1- user-line))))
                      (trap-at-procedure-ip-in-range proc range handler
                                                     #:current-frame
                                                     current-frame)))
                  procs))
           (if (null? traps)
               (error "No procedures found at ~a:~a." file user-line)))
         (lambda (frame)
           (for-each (lambda (trap) (trap frame)) traps)
           (set! traps #f)))))))



;; On a different tack, now we're going to build up a set of traps that
;; do useful things during the dynamic extent of a procedure's
;; application. First, a trap for when a frame returns.
;;
(define (trap-frame-finish frame return-handler abort-handler)
  (arg-check frame frame?)
  (arg-check return-handler procedure?)
  (arg-check abort-handler procedure?)
  (let ((fp (frame-address frame)))
    (define (pop-cont-hook frame . values)
      (if (and fp (< (frame-address frame) fp))
          (begin
            (set! fp #f)
            (apply return-handler frame values))))
    
    (define (abort-hook frame . values)
      (if (and fp (< (frame-address frame) fp))
          (begin
            (set! fp #f)
            (apply abort-handler frame values))))
    
    (new-enabled-trap
     frame
     (lambda (frame)
       (if (not fp)
           (error "return-or-abort traps may only be enabled once"))
       (add-hook! (vm-pop-continuation-hook) pop-cont-hook)
       (add-hook! (vm-abort-continuation-hook) abort-hook))
     (lambda (frame)
       (set! fp #f)
       (remove-hook! (vm-pop-continuation-hook) pop-cont-hook)
       (remove-hook! (vm-abort-continuation-hook) abort-hook)))))

;; A more traditional dynamic-wind trap. Perhaps this should not be
;; based on the above trap-frame-finish?
;;
(define* (trap-in-dynamic-extent proc enter-handler return-handler abort-handler
                                 #:key current-frame
                                 (our-frame? (frame-matcher proc)))
  (arg-check proc procedure?)
  (arg-check enter-handler procedure?)
  (arg-check return-handler procedure?)
  (arg-check abort-handler procedure?)
  (let ((exit-trap #f))
    (define (return-hook frame . values)
      (exit-trap frame) ; disable the return/abort trap.
      (set! exit-trap #f)
      (return-handler frame))
    
    (define (abort-hook frame . values)
      (exit-trap frame) ; disable the return/abort trap.
      (set! exit-trap #f)
      (abort-handler frame))
    
    (define (apply-hook frame)
      (if (and (not exit-trap) (our-frame? frame))
          (begin
            (enter-handler frame)
            (set! exit-trap
                  (trap-frame-finish frame return-hook abort-hook)))))
    
    (new-enabled-trap
     current-frame
     (lambda (frame)
       (add-hook! (vm-apply-hook) apply-hook))
     (lambda (frame)
       (if exit-trap
           (abort-hook frame))
       (set! exit-trap #f)
       (remove-hook! (vm-apply-hook) apply-hook)))))

;; Trapping all procedure calls within a dynamic extent, recording the
;; depth of the call stack relative to the original procedure.
;;
(define* (trap-calls-in-dynamic-extent proc apply-handler return-handler
                                       #:key current-frame
                                       (our-frame? (frame-matcher proc)))
  (arg-check proc procedure?)
  (arg-check apply-handler procedure?)
  (arg-check return-handler procedure?)
  (let ((*call-depth* 0))
    (define (trace-push frame)
      (set! *call-depth* (1+ *call-depth*)))
  
    (define (trace-pop frame . values)
      (apply return-handler frame *call-depth* values)
      (set! *call-depth* (1- *call-depth*)))
  
    (define (trace-apply frame)
      (apply-handler frame *call-depth*))
  
    ;; FIXME: recalc depth on abort

    (define (enter frame)
      (add-hook! (vm-push-continuation-hook) trace-push)
      (add-hook! (vm-pop-continuation-hook) trace-pop)
      (add-hook! (vm-apply-hook) trace-apply))
  
    (define (leave frame)
      (remove-hook! (vm-push-continuation-hook) trace-push)
      (remove-hook! (vm-pop-continuation-hook) trace-pop)
      (remove-hook! (vm-apply-hook) trace-apply))
  
    (define (return frame)
      (leave frame))
  
    (define (abort frame)
      (leave frame))

    (trap-in-dynamic-extent proc enter return abort
                            #:current-frame current-frame
                            #:our-frame? our-frame?)))

;; Trapping all retired intructions within a dynamic extent.
;;
(define* (trap-instructions-in-dynamic-extent proc next-handler
                                              #:key current-frame
                                              (our-frame? (frame-matcher proc)))
  (arg-check proc procedure?)
  (arg-check next-handler procedure?)
  (let ()
    (define (trace-next frame)
      (next-handler frame))
  
    (define (enter frame)
      (add-hook! (vm-next-hook) trace-next))
  
    (define (leave frame)
      (remove-hook! (vm-next-hook) trace-next))
  
    (define (return frame)
      (leave frame))
  
    (define (abort frame)
      (leave frame))

    (trap-in-dynamic-extent proc enter return abort
                            #:current-frame current-frame
                            #:our-frame? our-frame?)))

;; Traps calls and returns for a given procedure, keeping track of the call depth.
;;
(define (trap-calls-to-procedure proc apply-handler return-handler)
  (arg-check proc procedure?)
  (arg-check apply-handler procedure?)
  (arg-check return-handler procedure?)
  (let ((pending-finish-traps '())
        (last-fp #f))
    (define (apply-hook frame)
      (let ((depth (length pending-finish-traps)))

        (apply-handler frame depth)

        (if (not (eqv? (frame-address frame) last-fp))
            (let ((finish-trap #f))
              (define (frame-finished frame)
                (finish-trap frame) ;; disables the trap.
                (set! pending-finish-traps
                      (delq finish-trap pending-finish-traps))
                (set! finish-trap #f))
              
              (define (return-hook frame . values)
                (frame-finished frame)
                (apply return-handler frame depth values))
        
              ;; FIXME: abort handler?
              (define (abort-hook frame . values)
                (frame-finished frame))
        
              (set! finish-trap
                    (trap-frame-finish frame return-hook abort-hook))
              (set! pending-finish-traps
                    (cons finish-trap pending-finish-traps))))))

    ;; The basic idea is that we install one trap that fires for calls,
    ;; but that each call installs its own finish trap. Those finish
    ;; traps remove themselves as their frames finish or abort.
    ;;
    ;; However since to the outside world we present the interface of
    ;; just being one trap, disabling this calls-to-procedure trap
    ;; should take care of disabling all of the pending finish traps. We
    ;; keep track of pending traps through the pending-finish-traps
    ;; list.
    ;;
    ;; So since we know that the trap-at-procedure will be enabled, and
    ;; thus returning a disable closure, we make sure to wrap that
    ;; closure in something that will disable pending finish traps.
    (define (with-pending-finish-disablers trap)
      (define (with-pending-finish-enablers trap)
        (lambda* (#:optional frame)
          (with-pending-finish-disablers (trap frame))))
      
      (lambda* (#:optional frame)
        (for-each (lambda (disable) (disable frame))
                  pending-finish-traps)
        (set! pending-finish-traps '())
        (with-pending-finish-enablers (trap frame))))

    (with-pending-finish-disablers
     (trap-at-procedure-call proc apply-hook))))

;; Trap when the source location changes.
;;
(define (trap-matching-instructions frame-pred handler)
  (arg-check frame-pred procedure?)
  (arg-check handler procedure?)
  (let ()
    (define (next-hook frame)
      (if (frame-pred frame)
          (handler frame)))
  
    (new-enabled-trap
     #f
     (lambda (frame)
       (add-hook! (vm-next-hook) next-hook))
     (lambda (frame)
       (remove-hook! (vm-next-hook) next-hook)))))
