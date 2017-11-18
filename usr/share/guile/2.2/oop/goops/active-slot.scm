;;; installed-scm-file

;;;; Copyright (C) 1999, 2001, 2006, 2009, 2015 Free Software Foundation, Inc.
;;;; Copyright (C) 1993-1998 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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


;;;;
;;;; This file was based upon active-slot.stklos from the STk distribution
;;;; version 4.0.1 by Erick Gallesio <eg@unice.fr>.
;;;;

(define-module (oop goops active-slot)
  :use-module (oop goops internal)
  :export (<active-class>))

(define-class <active-class> (<class>))

(define-method (compute-get-n-set (class <active-class>) slot)
  (if (eq? (slot-definition-allocation slot) #:active)
      (let* ((index 	  (slot-ref class 'nfields))
	     (s		  (slot-definition-options slot))
	     (before-ref  (get-keyword #:before-slot-ref  s #f))
	     (after-ref   (get-keyword #:after-slot-ref   s #f))
	     (before-set! (get-keyword #:before-slot-set! s #f))
	     (after-set!  (get-keyword #:after-slot-set!  s #f))
	     (unbound	  *unbound*))
	(slot-set! class 'nfields (+ index 1))
	(list (lambda (o)
		(if before-ref
		    (if (before-ref o)
			(let ((res (struct-ref o index)))
			  (and after-ref (not (eqv? res unbound)) (after-ref o))
			  res)
			*unbound*)
		    (let ((res (struct-ref o index)))
		      (and after-ref (not (eqv? res unbound)) (after-ref o))
		      res)))

	      (lambda (o v) 
		(if before-set!
		    (if (before-set! o v)
			(begin
			  (struct-set! o index v)
			  (and after-set! (after-set! o v))))
		    (begin
		      (struct-set! o index v)
		      (and after-set! (after-set! o v)))))))
      (next-method)))
