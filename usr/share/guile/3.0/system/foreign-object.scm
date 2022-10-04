;;; Wrapping foreign objects in Scheme

;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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
;;; 

;;; Commentary:
;;
;;
;;; Code:

(define-module (system foreign-object)
  #:use-module (oop goops)
  #:export     (make-foreign-object-type
                define-foreign-object-type))

(eval-when (eval load expand)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_foreign_object"))

(define-class <foreign-class> (<class>))

(define-class <foreign-class-with-finalizer> (<foreign-class>)
  (finalizer #:init-keyword #:finalizer #:init-value #f
             #:getter finalizer))

(define-method (allocate-instance (class <foreign-class-with-finalizer>)
                                  initargs)
  (let ((instance (next-method))
        (finalizer (finalizer class)))
    (when finalizer
      (%add-finalizer! instance finalizer))
    instance))

(define* (make-foreign-object-type name slots #:key finalizer
                                   (getters (map (const #f) slots)))
  (unless (symbol? name)
    (error "type name should be a symbol" name))
  (unless (or (not finalizer) (procedure? finalizer))
    (error "finalizer should be a procedure" finalizer))
  (let ((dslots (map (lambda (slot getter)
                       (unless (symbol? slot)
                         (error "slot name should be a symbol" slot))
                       (cons* slot #:class <foreign-slot>
                              #:init-keyword (symbol->keyword slot)
                              #:init-value 0
                              (if getter (list #:getter getter) '())))
                     slots
                     getters)))
    (if finalizer
        (make-class '() dslots #:name name
                    #:finalizer finalizer
                    #:static-slot-allocation? #t
                    #:metaclass <foreign-class-with-finalizer>)
        (make-class '() dslots #:name name
                    #:static-slot-allocation? #t
                    #:metaclass <foreign-class>))))

(define-syntax define-foreign-object-type
  (lambda (x)
    (define (kw-apply slots)
      (syntax-case slots ()
        (() #'())
        ((slot . slots)
         (let ((kw (symbol->keyword (syntax->datum #'slot))))
           #`(#,kw slot . #,(kw-apply #'slots))))))

    (syntax-case x ()
      ((_ name constructor (slot ...) kwarg ...)
       #`(begin
           (define slot (ensure-generic 'slot (and (defined? 'slot) slot)))
           ...
           (define name
             (make-foreign-object-type 'name '(slot ...) kwarg ...
                                       #:getters (list slot ...)))
           (define constructor
             (lambda (slot ...)
               (make name #,@(kw-apply #'(slot ...))))))))))
