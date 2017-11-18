;;; syntactic.scm --- Syntactic support for R6RS records

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


(library (rnrs records syntactic (6))
  (export define-record-type 
	  record-type-descriptor 
	  record-constructor-descriptor)
  (import (only (guile) and=> gensym)
          (rnrs base (6))
	  (rnrs conditions (6))
	  (rnrs exceptions (6))
	  (rnrs hashtables (6))
	  (rnrs lists (6))
	  (rnrs records procedural (6))
	  (rnrs syntax-case (6))
	  (only (srfi :1) take))

  (define record-type-registry (make-eq-hashtable))

  (define (guess-constructor-name record-name)
    (string->symbol (string-append "make-" (symbol->string record-name))))
  (define (guess-predicate-name record-name)
    (string->symbol (string-append (symbol->string record-name) "?")))
  (define (register-record-type name rtd rcd)
    (hashtable-set! record-type-registry name (cons rtd rcd)))
  (define (lookup-record-type-descriptor name)
    (and=> (hashtable-ref record-type-registry name #f) car))
  (define (lookup-record-constructor-descriptor name)
    (and=> (hashtable-ref record-type-registry name #f) cdr))
  
  (define-syntax define-record-type
    (lambda (stx)
      (syntax-case stx ()
	((_ (record-name constructor-name predicate-name) record-clause ...)
	 #'(define-record-type0 
	     (record-name constructor-name predicate-name)
	     record-clause ...))
	((_ record-name record-clause ...)
	 (let* ((record-name-sym (syntax->datum #'record-name))
		(constructor-name 
		 (datum->syntax 
		  #'record-name (guess-constructor-name record-name-sym)))
		(predicate-name 
		 (datum->syntax 
		  #'record-name (guess-predicate-name record-name-sym))))
	   #`(define-record-type0 
	       (record-name #,constructor-name #,predicate-name) 
	       record-clause ...))))))

  (define (sequence n)
    (define (seq-inner n) (if (= n 0) '(0) (cons n (seq-inner (- n 1)))))
    (reverse (seq-inner n)))
  (define (number-fields fields)
    (define (number-fields-inner fields counter)
      (if (null? fields)
	  '()
	  (cons (cons fields counter) 
		(number-fields-inner (cdr fields) (+ counter 1)))))
    (number-fields-inner fields 0))
  
  (define (process-fields record-name fields)
    (define (wrap x) (datum->syntax record-name x))
    (define (id->string x)
      (symbol->string (syntax->datum x)))
    (define record-name-str (id->string record-name))
    (define (guess-accessor-name field-name)
      (wrap
       (string->symbol (string-append
                        record-name-str "-" (id->string field-name)))))
    (define (guess-mutator-name field-name)
      (wrap
       (string->symbol
        (string-append
         record-name-str "-" (id->string field-name) "-set!"))))
    (define (f x)
      (syntax-case x (immutable mutable)
        [(immutable name)
         (list (wrap `(immutable ,(syntax->datum #'name)))
               (guess-accessor-name #'name)
               #f)]
        [(immutable name accessor)
         (list (wrap `(immutable ,(syntax->datum #'name))) #'accessor #f)]
        [(mutable name)
         (list (wrap `(mutable ,(syntax->datum #'name)))
               (guess-accessor-name #'name)
               (guess-mutator-name #'name))]
        [(mutable name accessor mutator)
         (list (wrap `(mutable ,(syntax->datum #'name))) #'accessor #'mutator)]
        [name
         (identifier? #'name)
         (list (wrap `(immutable ,(syntax->datum #'name)))
               (guess-accessor-name #'name)
               #f)]
        [else
         (syntax-violation 'define-record-type "invalid field specifier" x)]))
    (map f fields))
  
  (define-syntax define-record-type0
    (lambda (stx)	  
      (define *unspecified* (cons #f #f))
      (define (unspecified? obj)
        (eq? *unspecified* obj))
      (syntax-case stx ()
        ((_ (record-name constructor-name predicate-name) record-clause ...)
         (let loop ((_fields *unspecified*)
                    (_parent *unspecified*)
                    (_protocol *unspecified*)
                    (_sealed *unspecified*)
                    (_opaque *unspecified*)
                    (_nongenerative *unspecified*)
                    (_constructor *unspecified*)
                    (_parent-rtd *unspecified*)
                    (record-clauses #'(record-clause ...)))
           (syntax-case record-clauses
               (fields parent protocol sealed opaque nongenerative
                       constructor parent-rtd)
             [()
              (let* ((fields (if (unspecified? _fields) '() _fields))
                     (field-names (list->vector (map car fields)))
                     (field-accessors
                      (fold-left (lambda (lst x c)
                                   (cons #`(define #,(cadr x)
                                             (record-accessor record-name #,c))
                                         lst))
                                 '() fields (sequence (length fields))))
                     (field-mutators
                      (fold-left (lambda (lst x c)
                                   (if (caddr x)
                                       (cons #`(define #,(caddr x)
                                                 (record-mutator record-name
                                                                 #,c))
                                             lst)
                                       lst))
                                 '() fields (sequence (length fields))))
                     (parent-cd (cond ((not (unspecified? _parent))
                                       #`(record-constructor-descriptor
                                          #,_parent))
                                      ((not (unspecified? _parent-rtd))
                                       (cadr _parent-rtd))
                                      (else #f)))
                     (parent-rtd (cond ((not (unspecified? _parent))
                                        #`(record-type-descriptor #,_parent))
                                       ((not (unspecified? _parent-rtd))
                                        (car _parent-rtd))
                                       (else #f)))
                     (protocol (if (unspecified? _protocol) #f _protocol))
                     (uid (if (unspecified? _nongenerative) #f _nongenerative))
                     (sealed? (if (unspecified? _sealed) #f _sealed))
                     (opaque? (if (unspecified? _opaque) #f _opaque)))
                #`(begin
                    (define record-name
                      (make-record-type-descriptor
                       (quote record-name)
                       #,parent-rtd #,uid #,sealed? #,opaque?
                       #,field-names))
                    (define constructor-name
                      (record-constructor
                       (make-record-constructor-descriptor
                        record-name #,parent-cd #,protocol)))
                    (define dummy
                      (let ()
                        (register-record-type 
                         (quote record-name)
                         record-name (make-record-constructor-descriptor 
                                      record-name #,parent-cd #,protocol))
                        'dummy))
                    (define predicate-name (record-predicate record-name))
                    #,@field-accessors
                    #,@field-mutators))]
             [((fields record-fields ...) . rest)
              (if (unspecified? _fields)
                  (loop (process-fields #'record-name #'(record-fields ...))
                        _parent _protocol _sealed _opaque _nongenerative
                        _constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((parent parent-name) . rest)
              (if (not (unspecified? _parent-rtd))
                  (raise (make-assertion-violation))
                  (if (unspecified? _parent)
                      (loop _fields #'parent-name _protocol _sealed _opaque
                            _nongenerative _constructor _parent-rtd #'rest)
                      (raise (make-assertion-violation))))]
             [((protocol expression) . rest)
              (if (unspecified? _protocol)
                  (loop _fields _parent #'expression _sealed _opaque
                        _nongenerative _constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((sealed sealed?) . rest)
              (if (unspecified? _sealed)
                  (loop _fields _parent _protocol #'sealed? _opaque
                        _nongenerative _constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((opaque opaque?) . rest)
              (if (unspecified? _opaque)
                  (loop _fields _parent _protocol _sealed #'opaque?
                        _nongenerative _constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((nongenerative) . rest)
              (if (unspecified? _nongenerative)
                  (loop _fields _parent _protocol _sealed _opaque
                        #`(quote #,(datum->syntax #'record-name (gensym)))
                        _constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((nongenerative uid) . rest)
              (if (unspecified? _nongenerative)
                  (loop _fields _parent _protocol _sealed
                        _opaque #''uid _constructor
                        _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((parent-rtd rtd cd) . rest)
              (if (not (unspecified? _parent))
                  (raise (make-assertion-violation))
                  (if (unspecified? _parent-rtd)
                      (loop _fields _parent _protocol _sealed _opaque
                            _nongenerative _constructor #'(rtd cd)
                            #'rest)
                      (raise (make-assertion-violation))))]))))))

  (define-syntax record-type-descriptor
    (lambda (stx)
      (syntax-case stx ()
	((_ name) #`(lookup-record-type-descriptor 
		     #,(datum->syntax 
			stx (list 'quote (syntax->datum #'name))))))))

  (define-syntax record-constructor-descriptor
    (lambda (stx)
      (syntax-case stx ()
	((_ name) #`(lookup-record-constructor-descriptor 
		     #,(datum->syntax 
			stx (list 'quote (syntax->datum #'name))))))))
)
