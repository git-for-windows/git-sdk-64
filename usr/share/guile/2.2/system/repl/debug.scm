;;; Guile VM debugging facilities

;;; Copyright (C) 2001, 2009, 2010, 2011, 2013, 2014, 2015 Free Software Foundation, Inc.
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

;;; Code:

(define-module (system repl debug)
  #:use-module (system base pmatch)
  #:use-module (system base syntax)
  #:use-module (system base language)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (system vm debug)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 pretty-print)
  #:use-module ((system vm inspect) #:select ((inspect . %inspect)))
  #:use-module (system vm program)
  #:export (<debug>
            make-debug debug?
            debug-frames debug-index debug-error-message
            terminal-width
            print-registers print-locals print-frame print-frames
            stack->vector narrow-stack->vector
            frame->stack-vector))

;; TODO:
;;
;; eval expression in context of frame
;; set local variable in frame
;; step until greater source line
;; watch expression
;; set printing width
;; disassemble the current function
;; inspect any object

;;;
;;; Debugger
;;;
;;; The actual interaction loop of the debugger is run by the repl. This module
;;; simply exports a data structure to hold the debugger state, along with its
;;; accessors, and provides some helper functions.
;;;

(define-record <debug> frames index error-message)



;; A fluid, because terminals are usually implicitly associated with
;; threads.
;;
(define terminal-width
  (let ((set-width (make-fluid)))
    (case-lambda
      (()
       (or (fluid-ref set-width)
           (let ((w (false-if-exception (string->number (getenv "COLUMNS")))))
             (and (integer? w) (exact? w) (> w 0) w))
           72))
      ((w)
       (if (or (not w) (and (integer? w) (exact? w) (> w 0)))
           (fluid-set! set-width w)
           (error "Expected a column number (a positive integer)" w))))))




(define (reverse-hashq h)
  (let ((ret (make-hash-table)))
    (hash-for-each
     (lambda (k v)
       (hashq-set! ret v (cons k (hashq-ref ret v '()))))
     h)
    ret))

(define* (print-registers frame #:optional (port (current-output-port))
                          #:key (per-line-prefix "  "))
  (define (print fmt val)
    (display per-line-prefix port)
    (run-hook before-print-hook val)
    (format port fmt val))
  
  (format port "~aRegisters:~%" per-line-prefix)
  (let ((ip (frame-instruction-pointer frame)))
    (print "ip = #x~x" ip)
    (let ((info (find-program-debug-info ip)))
      (when info
        (let ((addr (program-debug-info-addr info)))
          (format port " (#x~x + ~d * 4)" addr (/ (- ip addr) 4)))))
    (newline port))
  (print "sp = ~a\n" (frame-stack-pointer frame))
  (print "fp = ~a\n" (frame-address frame)))

(define* (print-locals frame #:optional (port (current-output-port))
                       #:key (width (terminal-width)) (per-line-prefix "  "))
  (let ((bindings (frame-bindings frame)))
    (cond
     ((null? bindings)
      (format port "~aNo local variables.~%" per-line-prefix))
     (else
      (format port "~aLocal variables:~%" per-line-prefix)
      (for-each
       (lambda (binding)
         (let ((v (binding-ref binding)))
           (display per-line-prefix port)
           (run-hook before-print-hook v)
           (format port "~a = ~v:@y\n" (binding-name binding) width v)))
       (frame-bindings frame))))))

(define* (print-frame frame #:optional (port (current-output-port))
                      #:key index (width (terminal-width)) (full? #f)
                      (last-source #f) next-source?)
  (define (source:pretty-file source)
    (if source
        (or (source:file source) "current input")
        "unknown file"))
  (let* ((source (frame-source frame))
         (file (source:pretty-file source))
         (line (and=> source source:line-for-user))
         (col (and=> source source:column)))
    (if (and file (not (equal? file (source:pretty-file last-source))))
        (format port "~&In ~a:~&" file))
    (format port "~9@a~:[~*~3_~;~3d~] ~v:@y~%"
            (if line (format #f "~a:~a" line col) "")
            index index width
            (frame-call-representation frame #:top-frame? (zero? index)))
    (if full?
        (print-locals frame #:width width
                      #:per-line-prefix "     "))))

(define* (print-frames frames
                       #:optional (port (current-output-port))
                       #:key (width (terminal-width)) (full? #f)
                       (forward? #f) count)
  (let* ((len (vector-length frames))
         (lower-idx (if (or (not count) (positive? count))
                        0
                        (max 0 (+ len count))))
         (upper-idx (if (and count (negative? count))
                        (1- len)
                        (1- (if count (min count len) len))))
         (inc (if forward? 1 -1)))
    (let lp ((i (if forward? lower-idx upper-idx))
             (last-source #f))
      (if (<= lower-idx i upper-idx)
          (let* ((frame (vector-ref frames i)))
            (print-frame frame port #:index i #:width width #:full? full?
                         #:last-source last-source)
            (lp (+ i inc)
                (frame-source frame)))))))

(define (stack->vector stack)
  (let* ((len (stack-length stack))
         (v (make-vector len)))
    (if (positive? len)
        (let lp ((i 0) (frame (stack-ref stack 0)))
          (if (< i len)
              (begin
                (vector-set! v i frame)
                (lp (1+ i) (frame-previous frame))))))
    v))

(define (narrow-stack->vector stack . args)
  (let ((narrowed (apply make-stack (stack-ref stack 0) args)))
    (if narrowed
        (stack->vector narrowed)
        #()))) ; ? Can be the case for a tail-call to `throw' tho

(define (frame->stack-vector frame)
  (let ((stack (make-stack frame)))
    (match (fluid-ref %stacks)
      ((stack-tag . prompt-tag)
       (narrow-stack->vector
        stack
        ;; Take the stack from the given frame, cutting 0 frames.
        0
        ;; Narrow the end of the stack to the most recent start-stack.
        prompt-tag
        ;; And one more frame, because %start-stack invoking the
        ;; start-stack thunk has its own frame too.
        0 (and prompt-tag 1)))
      (_
       ;; Otherwise take the whole stack.
       (stack->vector stack)))))

;; (define (debug)
;;   (run-debugger
;;    (narrow-stack->vector
;;     (make-stack #t)
;;     ;; Narrow the `make-stack' frame and the `debug' frame
;;     2
;;     ;; Narrow the end of the stack to the most recent start-stack.
;;     (and (pair? (fluid-ref %stacks))
;;          (cdr (fluid-ref %stacks))))))

