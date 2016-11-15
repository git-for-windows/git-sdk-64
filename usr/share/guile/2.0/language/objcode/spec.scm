;;; Guile Lowlevel Intermediate Language

;; Copyright (C) 2001, 2009, 2010, 2011, 2013 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language objcode spec)
  #:use-module (system base language)
  #:use-module (system vm objcode)
  #:use-module (system vm program)
  #:export (objcode))

(define (objcode->value x e opts)
  (let ((thunk (make-program x #f #f)))
    (if (eq? e (current-module))
        ;; save a cons in this case
        (values (thunk) e e)
        (save-module-excursion
         (lambda ()
           (set-current-module e)
           (values (thunk) e e))))))

;; since locals are allocated on the stack and can have limited scope,
;; in many cases we use one local for more than one lexical variable. so
;; the returned locals set is a list, where element N of the list is
;; itself a list of bindings for local variable N.
(define (collapse-locals locs)
  (let lp ((ret '()) (locs locs))
    (if (null? locs)
        (map cdr (sort! ret 
                        (lambda (x y) (< (car x) (car y)))))
        (let ((b (car locs)))
          (cond
           ((assv-ref ret (binding:index b))
            => (lambda (bindings)
                 (append! bindings (list b))
                 (lp ret (cdr locs))))
           (else
            (lp (acons (binding:index b) (list b) ret)
                (cdr locs))))))))

(define (decompile-value x env opts)
  (cond
   ((program? x)
    (let ((objs  (program-objects x))
          (meta  (program-meta x))
          (free-vars  (program-free-variables x))
          (binds (program-bindings x))
          (srcs  (program-sources x)))
      (let ((blocs (and binds (collapse-locals binds))))
        (values (program-objcode x)
                `((objects . ,objs)
                  (meta    . ,(and meta (meta)))
                  (free-vars . ,free-vars)
                  (blocs   . ,blocs)
                  (sources . ,srcs))))))
   ((objcode? x)
    (values x #f))
   (else
    (error "Object for disassembly not a program or objcode" x))))

(define-language objcode
  #:title	"Guile Object Code"
  #:reader	#f
  #:printer	write-objcode
  #:compilers   `((value . ,objcode->value))
  #:decompilers `((value . ,decompile-value))
  #:for-humans? #f
  )
