;;; installed-scm-file

;;;; 	Copyright (C) 1996, 2001, 2006, 2013 Free Software Foundation, Inc.
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



(define-module (ice-9 mapping)
  :use-module (ice-9 poe)
  :export (mapping-hooks-type make-mapping-hooks mapping-hooks?
	   mapping-hooks-get-handle mapping-hooks-create-handle
	   mapping-hooks-remove mapping-type make-mapping mapping?
	   mapping-hooks mapping-data set-mapping-hooks! set-mapping-data!
	   mapping-get-handle mapping-create-handle! mapping-remove!
	   mapping-ref mapping-set!  hash-table-mapping-hooks
	   make-hash-table-mapping hash-table-mapping))

(issue-deprecation-warning
 "(ice-9 mapping) is deprecated.  Use srfi-69 or rnrs hash tables instead.")

(define mapping-hooks-type (make-record-type 'mapping-hooks '(get-handle
							      create-handle
							      remove)))


(define make-mapping-hooks (perfect-funcq 17 (record-constructor mapping-hooks-type)))
(define mapping-hooks? (record-predicate mapping-hooks-type))
(define mapping-hooks-get-handle (record-accessor mapping-hooks-type 'get-handle))
(define mapping-hooks-create-handle (record-accessor mapping-hooks-type 'create-handle))
(define mapping-hooks-remove (record-accessor mapping-hooks-type 'remove))

(define mapping-type (make-record-type 'mapping '(hooks data)))
(define make-mapping (record-constructor mapping-type))
(define mapping? (record-predicate mapping-type))
(define mapping-hooks (record-accessor mapping-type 'hooks))
(define mapping-data (record-accessor mapping-type 'data))
(define set-mapping-hooks! (record-modifier mapping-type 'hooks))
(define set-mapping-data! (record-modifier mapping-type 'data))

(define (mapping-get-handle map key)
  ((mapping-hooks-get-handle (mapping-hooks map)) map key))
(define (mapping-create-handle! map key init)
  ((mapping-hooks-create-handle (mapping-hooks map)) map key init))
(define (mapping-remove! map key)
  ((mapping-hooks-remove (mapping-hooks map)) map key))

(define* (mapping-ref map key #:optional dflt)
  (cond
   ((mapping-get-handle map key) => cdr)
   (else dflt)))

(define (mapping-set! map key val)
  (set-cdr! (mapping-create-handle! map key #f) val))



(define hash-table-mapping-hooks
  (let ((wrap (lambda (proc) (lambda (1st . rest) (apply proc (mapping-data 1st) rest)))))

    (perfect-funcq 17
		   (lambda (hash-proc assoc-proc)
		     (let ((procs (list hash-proc assoc-proc)))
		       (cond
			((equal? procs `(,hashq ,assq))
			 (make-mapping-hooks (wrap hashq-get-handle)
					     (wrap hashq-create-handle!)
					     (wrap hashq-remove!)))
			((equal? procs `(,hashv ,assv))
			 (make-mapping-hooks (wrap hashv-get-handle)
					     (wrap hashv-create-handle!)
					     (wrap hashv-remove!)))
			((equal? procs `(,hash ,assoc))
			 (make-mapping-hooks (wrap hash-get-handle)
					     (wrap hash-create-handle!)
					     (wrap hash-remove!)))
			(else
			 (make-mapping-hooks (wrap
					      (lambda (table key)
						(hashx-get-handle hash-proc assoc-proc table key)))
					     (wrap
					      (lambda (table key init)
						(hashx-create-handle! hash-proc assoc-proc table key init)))
					     (wrap
					      (lambda (table key)
						(hashx-remove! hash-proc assoc-proc table key)))))))))))

(define (make-hash-table-mapping table hash-proc assoc-proc)
  (make-mapping (hash-table-mapping-hooks hash-proc assoc-proc) table))

(define* (hash-table-mapping #:optional (size 71) #:key
                             (hash-proc hash)
                             (assoc-proc
                              (or (assq-ref `((,hashq . ,assq)
                                              (,hashv . ,assv)
                                              (,hash . ,assoc))
                                            hash-proc)
                                  (error 'hash-table-mapping
                                         "Hash-procedure specified with no known assoc function."
                                         hash-proc)))
                             (table-constructor
                              (lambda (len) (make-vector len '()))))
  (make-hash-table-mapping (table-constructor size)
                           hash-proc
                           assoc-proc))
