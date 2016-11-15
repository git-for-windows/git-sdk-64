;;;; 	Copyright (C) 1999, 2001, 2006, 2009 Free Software Foundation, Inc.
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


;; There are circularities here; you can't import (oop goops compile)
;; before (oop goops). So when compiling, make sure that things are
;; kosher.
(eval-when (expand) (resolve-module '(oop goops)))

(define-module (oop goops compile)
  :use-module (oop goops)
  :use-module (oop goops util)
  :export (compute-cmethod)
  :no-backtrace
  )

;;;
;;; Compiling next methods into method bodies
;;;

;;; So, for the reader: there basic idea is that, given that the
;;; semantics of `next-method' depend on the concrete types being
;;; dispatched, why not compile a specific procedure to handle each type
;;; combination that we see at runtime.
;;;
;;; In theory we can do much better than a bytecode compilation, because
;;; we know the *exact* types of the arguments. It's ideal for native
;;; compilation. A task for the future.
;;;
;;; I think this whole generic application mess would benefit from a
;;; strict MOP.

(define (compute-cmethod methods types)
  (let ((make-procedure (slot-ref (car methods) 'make-procedure)))
    (if make-procedure
        (make-procedure
         (if (null? (cdr methods))
             (lambda args
               (no-next-method (method-generic-function (car methods)) args))
             (compute-cmethod (cdr methods) types)))
        (method-procedure (car methods)))))
