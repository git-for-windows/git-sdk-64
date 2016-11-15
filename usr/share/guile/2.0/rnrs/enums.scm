;;; enums.scm --- The R6RS enumerations library

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


(library (rnrs enums (6))
  (export make-enumeration enum-set-universe enum-set-indexer 
	  enum-set-constructor enum-set->list enum-set-member? enum-set-subset?
	  enum-set=? enum-set-union enum-set-intersection enum-set-difference
	  enum-set-complement enum-set-projection define-enumeration)
  (import (only (guile) and=>)
	  (rnrs base (6))
	  (rnrs conditions (6))
	  (rnrs exceptions (6))
          (rnrs records procedural (6))
	  (rnrs syntax-case (6))
	  (srfi :1))

  (define enum-set-rtd (make-record-type-descriptor 
			'enum-set #f #f #f #f '#((mutable universe)
						 (immutable set))))
  
  (define make-enum-set
    (record-constructor 
     (make-record-constructor-descriptor enum-set-rtd #f #f)))

  (define enum-set-universe-internal (record-accessor enum-set-rtd 0))
  (define enum-set-universe-set! (record-mutator enum-set-rtd 0))

  (define enum-set-set (record-accessor enum-set-rtd 1))

  (define (make-enumeration symbol-list) 
    (let ((es (make-enum-set #f symbol-list)))
      (enum-set-universe-set! es es)))

  (define (enum-set-universe enum-set)
    (or (enum-set-universe-internal enum-set) 
	enum-set))
  
  (define (enum-set-indexer enum-set)
    (let* ((symbols (enum-set->list (enum-set-universe enum-set)))
	   (cardinality (length symbols)))
      (lambda (x)
	(and=> (memq x symbols) 
	       (lambda (probe) (- cardinality (length probe)))))))

  (define (enum-set-constructor enum-set)
    (lambda (symbol-list)
      (make-enum-set (enum-set-universe enum-set) 
		     (list-copy symbol-list))))

  (define (enum-set->list enum-set)
    (lset-intersection eq? 
		       (enum-set-set (enum-set-universe enum-set))
		       (enum-set-set enum-set)))

  (define (enum-set-member? symbol enum-set)
    (and (memq symbol (enum-set-set enum-set)) #t))

  (define (enum-set-subset? enum-set-1 enum-set-2)
    (and (lset<= eq? 
		 (enum-set-set (enum-set-universe enum-set-1))
		 (enum-set-set (enum-set-universe enum-set-2)))
	 (lset<= eq? (enum-set-set enum-set-1) (enum-set-set enum-set-2))))

  (define (enum-set=? enum-set-1 enum-set-2)
    (and (enum-set-subset? enum-set-1 enum-set-2)
	 (enum-set-subset? enum-set-2 enum-set-1)))

  (define (enum-set-union enum-set-1 enum-set-2)
    (if (equal? (enum-set-universe enum-set-1) 
                (enum-set-universe enum-set-2))
	(make-enum-set (enum-set-universe enum-set-1)
		       (lset-union eq? 
				   (enum-set-set enum-set-1) 
				   (enum-set-set enum-set-2)))
	(raise (make-assertion-violation))))

  (define (enum-set-intersection enum-set-1 enum-set-2)
    (if (equal? (enum-set-universe enum-set-1) 
                (enum-set-universe enum-set-2))
	(make-enum-set (enum-set-universe enum-set-1)
		       (lset-intersection eq? 
					  (enum-set-set enum-set-1) 
					  (enum-set-set enum-set-2)))
	(raise (make-assertion-violation))))

  (define (enum-set-difference enum-set-1 enum-set-2)
    (if (equal? (enum-set-universe enum-set-1) 
                (enum-set-universe enum-set-2))
	(make-enum-set (enum-set-universe enum-set-1)
		       (lset-difference eq? 
					(enum-set-set enum-set-1) 
					(enum-set-set enum-set-2)))
	(raise (make-assertion-violation))))
  
  (define (enum-set-complement enum-set)
    (let ((universe (enum-set-universe enum-set)))
      (make-enum-set universe 
		     (lset-difference 
		      eq? (enum-set->list universe) (enum-set-set enum-set)))))

  (define (enum-set-projection enum-set-1 enum-set-2)
    (make-enum-set (enum-set-universe enum-set-2)
		   (lset-intersection eq?
				      (enum-set-set enum-set-1)
				      (enum-set->list 
				       (enum-set-universe enum-set-2)))))

  (define-syntax define-enumeration
    (syntax-rules ()
      ((_ type-name (symbol ...) constructor-syntax)
       (begin
	 (define-syntax type-name
	   (lambda (s) 
	     (syntax-case s ()
	       ((type-name sym)
		(if (memq (syntax->datum #'sym) '(symbol ...))
		    #'(quote sym)
		    (syntax-violation (symbol->string 'type-name) 
				      "not a member of the set"
				      #f))))))
	 (define-syntax constructor-syntax
	   (lambda (s)
	     (syntax-case s ()
	       ((_ sym (... ...))
		(let* ((universe '(symbol ...))
		       (syms (syntax->datum #'(sym (... ...))))
		       (quoted-universe 
			(datum->syntax s (list 'quote universe)))
		       (quoted-syms (datum->syntax s (list 'quote syms))))
		  (or (every (lambda (x) (memq x universe)) syms)
		      (syntax-violation (symbol->string 'constructor-syntax)
					"not a subset of the universe"
					#f))
		  #`((enum-set-constructor (make-enumeration #,quoted-universe))
		     #,quoted-syms))))))))))
)
