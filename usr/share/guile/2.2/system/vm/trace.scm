;;; Guile VM tracer

;; Copyright (C) 2001, 2009, 2010, 2012, 2013, 2014 Free Software Foundation, Inc.

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

(define-module (system vm trace)
  #:use-module (system base syntax)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:use-module (system vm traps)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:export (trace-calls-in-procedure
            trace-calls-to-procedure
            trace-instructions-in-procedure
            call-with-trace))

(define (build-prefix prefix depth infix numeric-format max-indent)
  (let lp ((indent "") (n 0))
    (cond
     ((= n depth)
      (string-append prefix indent))
     ((< (+ (string-length indent) (string-length infix)) max-indent)
      (lp (string-append indent infix) (1+ n)))
     (else
      (string-append prefix indent (format #f numeric-format depth))))))

(define (print-application frame depth width prefix max-indent)
  (let ((prefix (build-prefix prefix depth "|  " "~d> " max-indent)))
    (format (current-error-port) "~a~v:@y\n"
            prefix
            width
            (frame-call-representation frame #:top-frame? #t))))

(define (print-return depth width prefix max-indent values)
  (let ((prefix (build-prefix prefix depth "|  " "~d< "max-indent)))
    (case (length values)
      ((0)
       (format (current-error-port) "~ano values\n" prefix))
      ((1)
       (format (current-error-port) "~a~v:@y\n"
               prefix
               width
               (car values)))
      (else
       ;; this should work, but there appears to be a bug
       ;; "~a~d values:~:{ ~v:@y~}\n"
       (format (current-error-port) "~a~d values:~{ ~a~}\n"
               prefix (length values)
               (map (lambda (val)
                      (format #f "~v:@y" width val))
                    values))))))

(define* (trace-calls-to-procedure proc #:key (width 80)
                                   (prefix "trace: ")
                                   (max-indent (- width 40)))
  (define (apply-handler frame depth)
    (print-application frame depth width prefix max-indent))
  (define (return-handler frame depth . values)
    (print-return depth width prefix max-indent values))
  (trap-calls-to-procedure proc apply-handler return-handler))

(define* (trace-calls-in-procedure proc #:key (width 80)
                                   (prefix "trace: ")
                                   (max-indent (- width 40)))
  (define (apply-handler frame depth)
    (print-application frame depth width prefix max-indent))
  (define (return-handler frame depth . values)
    (print-return depth width prefix max-indent values))
  (trap-calls-in-dynamic-extent proc apply-handler return-handler))

(define* (trace-instructions-in-procedure proc #:key (width 80)
                                          (max-indent (- width 40)))
  (define (trace-next frame)
    ;; FIXME: We could disassemble this instruction here.
    (let ((ip (frame-instruction-pointer frame)))
      (format #t "0x~x\n" ip)))
  
  (trap-instructions-in-dynamic-extent proc trace-next))

;; Note that because this procedure manipulates the VM trace level
;; directly, it doesn't compose well with traps at the REPL.
;;
(define* (call-with-trace thunk #:key (calls? #t) (instructions? #f) 
                          (width 80) (max-indent (- width 40)))
  (let ((call-trap #f)
        (inst-trap #f))
    (dynamic-wind
      (lambda ()
        (if calls?
            (set! call-trap
                  (trace-calls-in-procedure thunk #:width width
                                            #:max-indent max-indent)))
        (if instructions?
            (set! inst-trap
                  (trace-instructions-in-procedure thunk #:width width 
                                                   #:max-indent max-indent)))
        (set-vm-trace-level! (1+ (vm-trace-level))))
      thunk
      (lambda ()
        (set-vm-trace-level! (1- (vm-trace-level)))
        (if call-trap (call-trap))
        (if inst-trap (inst-trap))
        (set! call-trap #f)
        (set! inst-trap #f)))))
