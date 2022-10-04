;;; procedural.scm --- Procedural interface to R6RS records

;;      Copyright (C) 2010, 2017, 2019-2020 Free Software Foundation, Inc.
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
          (rename (record-type? record-type-descriptor?))
	  make-record-constructor-descriptor
	  
	  record-constructor
	  record-predicate
	  record-accessor	  
	  record-mutator)
	  
  (import (rnrs base (6))
          (rnrs conditions (6))
          (rnrs exceptions (6))
          (only (rename (guile)
                        (record-accessor guile:record-accessor))
                logbit?
                when
                unless
                struct-ref
                struct-set!
                make-record-type
                record-type?
                record-type-name
                record-type-fields
                record-type-constructor
                record-type-mutable-fields
                record-type-parent
                record-type-opaque?
                record-predicate
                guile:record-accessor
                record-modifier
                vector->list))

  (define (make-record-type-descriptor name parent uid sealed? opaque? fields)
    (make-record-type name (vector->list fields) #:parent parent #:uid uid
                      #:extensible? (not sealed?)
                      #:allow-duplicate-field-names? #t
                      #:opaque? (or opaque?
                                    (and parent (record-type-opaque? parent)))))

  (define record-constructor-descriptor
    (make-record-type 'record-constructor-descriptor
                      '((immutable rtd)
                        (immutable parent)
                        (immutable protocol))))
  (define rcd-rtd
    (guile:record-accessor record-constructor-descriptor 'rtd))
  (define rcd-parent
    (guile:record-accessor record-constructor-descriptor 'parent))
  (define rcd-protocol
    (guile:record-accessor record-constructor-descriptor 'protocol))

  (define (make-record-constructor-descriptor rtd parent-rcd protocol)
    (unless (record-type? rtd)
      (raise (make-assertion-violation)))
    (when protocol
      (unless (procedure? protocol)
        (raise (make-assertion-violation))))
    (when parent-rcd
      (unless (eq? (rcd-rtd parent-rcd)
                   (record-type-parent rtd))
        (when protocol
          (raise (make-assertion-violation)))))
    ((record-type-constructor record-constructor-descriptor)
     rtd parent-rcd protocol))

  (define (record-constructor rcd)
    ;; The protocol facility allows users to define constructors whose
    ;; arguments don't directly correspond to the fields of the record
    ;; type; instead, the protocol managed a mapping from "args" to
    ;; "inits", where args are constructor args, and inits are the
    ;; resulting set of initial field values.
    (define-syntax if*
      (syntax-rules (=>)
        ((if* (exp => id) consequent alternate)
         (cond (exp => (lambda (id) consequent)) (else alternate)))))
    (define raw-constructor
      (record-type-constructor (rcd-rtd rcd)))
    (if* ((rcd-protocol rcd) => protocol)
         (protocol
          (if* ((rcd-parent rcd) => parent)
               (lambda parent-args
                 (lambda inits
                   (let collect-inits ((parent parent)
                                       (parent-args parent-args)
                                       (inits inits))
                     (apply
                      (if* ((and parent (rcd-protocol parent)) => protocol)
                           (protocol
                            (if* ((rcd-parent parent) => parent)
                                 ;; Parent has a protocol too; collect
                                 ;; inits from parent.
                                 (lambda parent-args
                                   (lambda parent-inits
                                     (collect-inits parent parent-args
                                                    (append parent-inits
                                                            inits))))
                                 ;; Default case: parent args correspond
                                 ;; to inits.
                                 (lambda parent-args
                                   (apply raw-constructor
                                          (append parent-args inits)))))
                           ;; Default case: parent args correspond to inits.
                           (lambda parent-args
                             (apply raw-constructor
                                    (append parent-args inits))))
                      parent-args))))
               raw-constructor))
         raw-constructor))
		    
  (define (record-accessor rtd k)
    (define pred (record-predicate rtd))
    
    (let* ((parent (record-type-parent rtd))
           (parent-nfields (if parent
                               (length (record-type-fields parent))
                               0))
           (k (+ k parent-nfields)))
      (unless (and (<= parent-nfields k)
                   (< k (length (record-type-fields rtd))))
        (raise (make-assertion-violation)))
      (lambda (obj)
        (unless (pred obj)
          (raise (make-assertion-violation)))
        (struct-ref obj k))))

  (define (record-mutator rtd k)
    (define pred (record-predicate rtd))
    (let* ((parent (record-type-parent rtd))
           (parent-nfields (if parent
                               (length (record-type-fields parent))
                               0))
           (k (+ k parent-nfields)))
      (unless (and (<= parent-nfields k)
                   (< k (length (record-type-fields rtd))))
        (raise (make-assertion-violation)))
      (unless (logbit? k (record-type-mutable-fields rtd))
        (raise (make-assertion-violation)))
      (lambda (obj val)
        (unless (pred obj)
          (raise (make-assertion-violation)))
        (struct-set! obj k val))))

  )
