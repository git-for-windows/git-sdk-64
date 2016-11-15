;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010
;;;; Free Software Foundation, Inc.
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



;;; Commentary:

;;; An older approack to debugging, in which the user installs a pre-unwind
;;; handler that saves the stack at the time of the error. The last stack can
;;; then be debugged later.
;;;

;;; Code:

(define-module (ice-9 save-stack)
  ;; Replace deprecated root-module bindings, if present.
  #:replace (stack-saved?
             the-last-stack
             save-stack))

;; FIXME: stack-saved? is broken in the presence of threads.
(define stack-saved? #f)

(define the-last-stack (make-fluid))

(define (save-stack . narrowing)
  (if (not stack-saved?)
      (begin
        (let ((stacks (fluid-ref %stacks)))
          (fluid-set! the-last-stack
                      ;; (make-stack obj inner outer inner outer ...)
                      ;;
                      ;; In this case, cut away the make-stack frame, the
                      ;; save-stack frame, and then narrow as specified by the
                      ;; user, delimited by the nearest start-stack invocation,
                      ;; if any.
                      (apply make-stack #t
                             2
                             (if (pair? stacks) (cdar stacks) 0)
                             narrowing)))
        (set! stack-saved? #t))))
