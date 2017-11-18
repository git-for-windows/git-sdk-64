;;;; 	Copyright (C) 1997, 2000, 2001, 2002, 2003, 2006, 2010 Free Software Foundation, Inc.
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


(define-module (ice-9 syncase)
  ;; FIXME re-export other procs
  #:export (datum->syntax-object syntax-object->datum
            sc-expand))

(issue-deprecation-warning
 "Syntax-case macros are now a part of Guile core; importing (ice-9 syncase) is no longer necessary.")

(define datum->syntax-object datum->syntax)
(define syntax-object->datum syntax->datum)
(define sc-expand macroexpand)

;;; Hack to make syncase macros work in the slib module
;; FIXME wingo is this still necessary?
;; (let ((m (nested-ref the-root-module '(%app modules ice-9 slib))))
;;   (if m
;;       (set-object-property! (module-local-variable m 'define)
;; 			    '*sc-expander*
;; 			    '(define))))
