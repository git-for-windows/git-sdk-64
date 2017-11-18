;;; inspection.scm --- Inspection support for R6RS records

;;      Copyright (C) 2010 Free Software Foundation, Inc.
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
	  (only (guile) struct-ref struct-vtable vtable-index-layout @@))

  (define record-internal? (@@ (rnrs records procedural) record-internal?))

  (define rtd-index-name (@@ (rnrs records procedural) rtd-index-name))
  (define rtd-index-parent (@@ (rnrs records procedural) rtd-index-parent))
  (define rtd-index-uid (@@ (rnrs records procedural) rtd-index-uid))
  (define rtd-index-sealed? (@@ (rnrs records procedural) rtd-index-sealed?))
  (define rtd-index-opaque? (@@ (rnrs records procedural) rtd-index-opaque?))
  (define rtd-index-field-names 
    (@@ (rnrs records procedural) rtd-index-field-names))
  (define rtd-index-field-bit-field
    (@@ (rnrs records procedural) rtd-index-field-bit-field))

  (define (record? obj)
    (and (record-internal? obj)
	 (not (record-type-opaque? (struct-vtable obj)))))

  (define (record-rtd record)
    (or (and (record-internal? record)
	     (let ((rtd (struct-vtable record)))
	       (and (not (struct-ref rtd rtd-index-opaque?)) rtd)))
	(assertion-violation 'record-rtd "not a record" record)))

  (define (guarantee-rtd who rtd)
    (if (record-type-descriptor? rtd)
        rtd
        (assertion-violation who "not a record type descriptor" rtd)))

  (define (record-type-name rtd) 
    (struct-ref (guarantee-rtd 'record-type-name rtd) rtd-index-name))
  (define (record-type-parent rtd) 
    (struct-ref (guarantee-rtd 'record-type-parent rtd) rtd-index-parent))
  (define (record-type-uid rtd)
    (struct-ref (guarantee-rtd 'record-type-uid rtd) rtd-index-uid))
  (define (record-type-generative? rtd) 
    (not (record-type-uid (guarantee-rtd 'record-type-generative? rtd))))
  (define (record-type-sealed? rtd) 
    (struct-ref (guarantee-rtd 'record-type-sealed? rtd) rtd-index-sealed?))
  (define (record-type-opaque? rtd) 
    (struct-ref (guarantee-rtd 'record-type-opaque? rtd) rtd-index-opaque?))
  (define (record-type-field-names rtd)
    (struct-ref (guarantee-rtd 'record-type-field-names rtd) rtd-index-field-names))
  (define (record-field-mutable? rtd k)
    (bitwise-bit-set? (struct-ref (guarantee-rtd 'record-field-mutable? rtd)
                                  rtd-index-field-bit-field)
                      k))
)
