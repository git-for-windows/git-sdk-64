;;; inspection.scm --- Inspection support for R6RS records

;;      Copyright (C) 2010, 2019 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(library (rnrs records inspection (6))
  (export record? 
          record-rtd 
	  record-type-name 
	  record-type-parent 
	  record-type-uid 
	  record-type-generative? 
	  record-type-sealed? 
	  record-type-opaque? 
	  record-type-field-names 
	  record-field-mutable?)
  (import (rnrs arithmetic bitwise (6))
          (rnrs base (6))
	  (rnrs records procedural (6))
	  (rnrs exceptions (6))
	  (rnrs conditions (6))
	  (rename (only (guile)
                        unless
                        logbit?
                        record?
                        record-type-name
                        record-type-parent
                        record-type-fields
                        record-type-opaque?
                        record-type-extensible?
                        record-type-uid
                        record-type-mutable-fields
                        struct-vtable)
                  (record? guile:record?)))

  (define (record? obj)
    (and (guile:record? obj)
	 (not (record-type-opaque? (struct-vtable obj)))))

  (define (record-rtd record)
    (unless (record? record)
      (assertion-violation 'record-rtd "not a record" record))
    (struct-vtable record))

  (define (record-type-generative? rtd) 
    (not (record-type-uid rtd)))
  (define (record-type-sealed? rtd) 
    (not (record-type-extensible? rtd)))
  (define (record-type-field-names rtd)
    (let ((parent (record-type-parent rtd))
          (fields (record-type-fields rtd)))
      (list->vector
       (if parent
           (list-tail fields (length (record-type-fields parent)))
           fields))))
  (define (record-field-mutable? rtd k)
    (let* ((parent (record-type-parent rtd))
           (parent-nfields (if parent
                               (length (record-type-fields parent))
                               0))
           (k (+ k parent-nfields)))
      (unless (and (<= parent-nfields k)
                   (< k (length (record-type-fields rtd))))
        (raise (make-assertion-violation)))
      (logbit? k (record-type-mutable-fields rtd)))))
