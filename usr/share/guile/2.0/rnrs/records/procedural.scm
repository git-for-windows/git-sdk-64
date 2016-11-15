;;; procedural.scm --- Procedural interface to R6RS records

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


(library (rnrs records procedural (6))
  (export make-record-type-descriptor 
	  record-type-descriptor?
	  make-record-constructor-descriptor
	  
	  record-constructor
	  record-predicate
	  record-accessor	  
	  record-mutator)
	  
  (import (rnrs base (6))
          (only (guile) cons*
                        logand 
                        logior
                        ash

                        and=>
			throw
			display
		        make-struct 
			make-vtable 
			map
			simple-format
			string-append 
                        symbol-append
			
			struct? 
                        struct-layout
			struct-ref 
			struct-set! 
			struct-vtable
			vtable-index-layout

                        make-hash-table
			hashq-ref
			hashq-set!

			vector->list)
	  (ice-9 receive)
	  (only (srfi :1) fold split-at take))

  (define (record-internal? obj)
    (and (struct? obj) (record-type-descriptor? (struct-vtable obj))))

  (define rtd-index-name 8)
  (define rtd-index-uid 9)
  (define rtd-index-parent 10)
  (define rtd-index-sealed? 11)
  (define rtd-index-opaque? 12)
  (define rtd-index-predicate 13)
  (define rtd-index-field-names 14)
  (define rtd-index-field-bit-field 15)
  (define rtd-index-field-binder 16)

  (define rctd-index-rtd 0)
  (define rctd-index-parent 1)
  (define rctd-index-protocol 2)

  (define vtable-base-layout (symbol->string (struct-layout (make-vtable ""))))

  (define record-type-vtable 
    (make-vtable (string-append vtable-base-layout "prprprprprprprprprpr")
		 (lambda (obj port) 
		   (simple-format port "#<r6rs:record-type:~A>"
				  (struct-ref obj rtd-index-name)))))

  (define record-constructor-vtable 
    (make-vtable "prprpr"
		 (lambda (obj port) 
		   (simple-format port "#<r6rs:record-constructor:~A>" 
				  (struct-ref (struct-ref obj rctd-index-rtd)
					      rtd-index-name)))))

  (define uid-table (make-hash-table))    

  (define (make-record-type-descriptor name parent uid sealed? opaque? fields)
    (define fields-pair
      (let loop ((field-list (vector->list fields))
                 (layout-sym 'pr)
                 (layout-bit-field 0)
                 (counter 0))
        (if (null? field-list)
            (cons layout-sym layout-bit-field)
            (case (caar field-list)
              ((immutable) 
               (loop (cdr field-list)
                     (symbol-append layout-sym 'pr) 
                     layout-bit-field 
                     (+ counter 1)))
              ((mutable)
               (loop (cdr field-list)
                     (symbol-append layout-sym 'pw)
                     (logior layout-bit-field (ash 1 counter))
                     (+ counter 1)))
              (else (r6rs-raise (make-assertion-violation)))))))

    (define fields-layout (car fields-pair))
    (define fields-bit-field (cdr fields-pair))

    (define field-names (list->vector (map cadr (vector->list fields))))
    (define late-rtd #f)

    (define (private-record-predicate obj)       
      (and (record-internal? obj)
           (or (eq? (struct-vtable obj) late-rtd)
               (and=> (struct-ref obj 0) private-record-predicate))))

    (define (field-binder parent-struct . args)
      (apply make-struct (cons* late-rtd 0 parent-struct args)))

    (if (and parent (struct-ref parent rtd-index-sealed?))
	(r6rs-raise (make-assertion-violation)))

    (let ((matching-rtd (and uid (hashq-ref uid-table uid)))
	  (opaque? (or opaque? (and parent (struct-ref 
					    parent rtd-index-opaque?)))))
      (if matching-rtd
	  (if (equal? (list name 
			    parent 
			    sealed? 
			    opaque?                            
			    field-names
                            fields-bit-field)
		      (list (struct-ref matching-rtd rtd-index-name)
			    (struct-ref matching-rtd rtd-index-parent)
			    (struct-ref matching-rtd rtd-index-sealed?)
			    (struct-ref matching-rtd rtd-index-opaque?)
			    (struct-ref matching-rtd rtd-index-field-names)
                            (struct-ref matching-rtd 
                                        rtd-index-field-bit-field)))
	      matching-rtd
	      (r6rs-raise (make-assertion-violation)))
          
	  (let ((rtd (make-struct record-type-vtable 0

                                  fields-layout
                                  (lambda (obj port)
                                    (simple-format 
                                     port "#<r6rs:record:~A>" name))
				  
				  name
				  uid
				  parent 
				  sealed? 
				  opaque?
				  
				  private-record-predicate
				  field-names
                                  fields-bit-field
				  field-binder)))
	    (set! late-rtd rtd)
	    (if uid (hashq-set! uid-table uid rtd))
	    rtd))))

  (define (record-type-descriptor? obj)
    (and (struct? obj) (eq? (struct-vtable obj) record-type-vtable)))

  (define (make-record-constructor-descriptor rtd 
					      parent-constructor-descriptor
					      protocol)
    (define rtd-arity (vector-length (struct-ref rtd rtd-index-field-names)))
    (define (default-inherited-protocol n)
      (lambda args
	(receive 
          (n-args p-args) 
	  (split-at args (- (length args) rtd-arity))
	  (let ((p (apply n n-args)))
	    (apply p p-args)))))
    (define (default-protocol p) p)
    
    (let* ((prtd (struct-ref rtd rtd-index-parent))
	   (pcd (or parent-constructor-descriptor
		    (and=> prtd (lambda (d) (make-record-constructor-descriptor 
					     prtd #f #f)))))
	   (prot (or protocol (if pcd 
				  default-inherited-protocol 
				  default-protocol))))
      (make-struct record-constructor-vtable 0 rtd pcd prot)))

  (define (record-constructor rctd)
    (let* ((rtd (struct-ref rctd rctd-index-rtd))
	   (parent-rctd (struct-ref rctd rctd-index-parent))
	   (protocol (struct-ref rctd rctd-index-protocol)))
      (protocol 
       (if parent-rctd
	   (let ((parent-record-constructor (record-constructor parent-rctd))
		 (parent-rtd (struct-ref parent-rctd rctd-index-rtd)))
	     (lambda args
	       (let ((struct (apply parent-record-constructor args)))
		 (lambda args
		   (apply (struct-ref rtd rtd-index-field-binder)
			  (cons struct args))))))
	   (lambda args (apply (struct-ref rtd rtd-index-field-binder)
			       (cons #f args)))))))
		    
  (define (record-predicate rtd) (struct-ref rtd rtd-index-predicate))

  (define (record-accessor rtd k)
    (define (record-accessor-inner obj)
      (if (eq? (struct-vtable obj) rtd)
	  (struct-ref obj (+ k 1))
          (and=> (struct-ref obj 0) record-accessor-inner)))
    (lambda (obj) 
      (if (not (record-internal? obj))
          (r6rs-raise (make-assertion-violation)))
      (record-accessor-inner obj)))

  (define (record-mutator rtd k)
    (define (record-mutator-inner obj val)
      (and obj (or (and (eq? (struct-vtable obj) rtd)
                        (struct-set! obj (+ k 1) val))
                   (record-mutator-inner (struct-ref obj 0) val))))
    (let ((bit-field (struct-ref rtd rtd-index-field-bit-field)))
      (if (zero? (logand bit-field (ash 1 k)))
	  (r6rs-raise (make-assertion-violation))))
    (lambda (obj val) (record-mutator-inner obj val)))

  ;; Condition types that are used in the current library.  These are defined
  ;; here and not in (rnrs conditions) to avoid a circular dependency.

  (define &condition (make-record-type-descriptor '&condition #f #f #f #f '#()))
  (define &condition-constructor-descriptor 
    (make-record-constructor-descriptor &condition #f #f))

  (define &serious (make-record-type-descriptor 
		    '&serious &condition #f #f #f '#()))
  (define &serious-constructor-descriptor
    (make-record-constructor-descriptor 
     &serious &condition-constructor-descriptor #f))

  (define make-serious-condition 
    (record-constructor &serious-constructor-descriptor))

  (define &violation (make-record-type-descriptor
		      '&violation &serious #f #f #f '#()))
  (define &violation-constructor-descriptor
    (make-record-constructor-descriptor 
     &violation &serious-constructor-descriptor #f))
  (define make-violation (record-constructor &violation-constructor-descriptor))

  (define &assertion (make-record-type-descriptor
		      '&assertion &violation #f #f #f '#()))
  (define make-assertion-violation 
    (record-constructor 
     (make-record-constructor-descriptor
      &assertion &violation-constructor-descriptor #f)))

  ;; Exception wrapper type, along with a wrapping `throw' implementation.
  ;; These are used in the current library, and so they are defined here and not
  ;; in (rnrs exceptions) to avoid a circular dependency.

  (define &raise-object-wrapper
    (make-record-type-descriptor '&raise-object-wrapper #f #f #f #f
				 '#((immutable obj) (immutable continuation))))
  (define make-raise-object-wrapper 
    (record-constructor (make-record-constructor-descriptor 
			 &raise-object-wrapper #f #f)))
  (define raise-object-wrapper? (record-predicate &raise-object-wrapper))
  (define raise-object-wrapper-obj (record-accessor &raise-object-wrapper 0))
  (define raise-object-wrapper-continuation 
    (record-accessor &raise-object-wrapper 1))

  (define (r6rs-raise obj) 
    (throw 'r6rs:exception (make-raise-object-wrapper obj #f)))
  (define (r6rs-raise-continuable obj)
    (define (r6rs-raise-continuable-internal continuation)
      (throw 'r6rs:exception (make-raise-object-wrapper obj continuation)))
    (call/cc r6rs-raise-continuable-internal))
)
