;;; srfi-9.scm --- define-record-type

;; Copyright (C) 2001, 2002, 2006, 2009, 2010, 2011, 2012,
;;   2013, 2014 Free Software Foundation, Inc.
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

;;; Commentary:

;; This module exports the syntactic form `define-record-type', which
;; is the means for creating record types defined in SRFI-9.
;;
;; The syntax of a record type definition is:
;;
;;  <record type definition>
;;    -> (define-record-type <type name>
;;         (<constructor name> <field tag> ...)
;;         <predicate name>
;;         <field spec> ...)
;;
;;  <field spec> -> (<field tag> <getter name>)
;;               -> (<field tag> <getter name> <setter name>)
;;
;;  <field tag> -> <identifier>
;;  <... name>  -> <identifier>
;;
;; Usage example:
;;
;; guile> (use-modules (srfi srfi-9))
;; guile> (define-record-type :foo (make-foo x) foo?
;;                            (x get-x) (y get-y set-y!))
;; guile> (define f (make-foo 1))
;; guile> f
;; #<:foo x: 1 y: #f>
;; guile> (get-x f)
;; 1
;; guile> (set-y! f 2)
;; 2
;; guile> (get-y f)
;; 2
;; guile> f
;; #<:foo x: 1 y: 2>
;; guile> (foo? f)
;; #t
;; guile> (foo? 1)
;; #f

;;; Code:

(define-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (system base ck)
  #:export (define-record-type))

(cond-expand-provide (current-module) '(srfi-9))

;; Roll our own instead of using the public `define-inlinable'.  This is
;; because the public one has a different `make-procedure-name', so
;; using it would require users to recompile code that uses SRFI-9.  See
;; <http://lists.gnu.org/archive/html/guile-devel/2011-04/msg00111.html>.
;;

(define-syntax-rule (define-inlinable (name formals ...) body ...)
  (define-tagged-inlinable () (name formals ...) body ...))

;; 'define-tagged-inlinable' has an additional feature: it stores a map
;; of keys to values that can be retrieved at expansion time.  This is
;; currently used to retrieve the rtd id, field index, and record copier
;; macro for an arbitrary getter.

(define-syntax-rule (%%on-error err) err)

(define %%type #f)   ; a private syntax literal
(define-syntax getter-type
  (syntax-rules (quote)
    ((_ s 'getter 'err)
     (getter (%%on-error err) %%type s))))

(define %%index #f)  ; a private syntax literal
(define-syntax getter-index
  (syntax-rules (quote)
   ((_ s 'getter 'err)
    (getter (%%on-error err) %%index s))))

(define %%copier #f) ; a private syntax literal
(define-syntax getter-copier
  (syntax-rules (quote)
   ((_ s 'getter 'err)
    (getter (%%on-error err) %%copier s))))

(define-syntax define-tagged-inlinable
  (lambda (x)
    (define (make-procedure-name name)
      (datum->syntax name
                     (symbol-append '% (syntax->datum name)
                                    '-procedure)))

    (syntax-case x ()
      ((_ ((key value) ...) (name formals ...) body ...)
       (identifier? #'name)
       (with-syntax ((proc-name  (make-procedure-name #'name))
                     ((args ...) (generate-temporaries #'(formals ...))))
         #`(begin
             (define (proc-name formals ...)
               body ...)
             (define-syntax name
               (lambda (x)
                 (syntax-case x (%%on-error key ...)
                   ((_ (%%on-error err) key s) #'(ck s 'value)) ...
                   ((_ args ...)
                    #'((lambda (formals ...)
                         body ...)
                       args ...))
                   ((_ a (... ...))
                    (syntax-violation 'name "Wrong number of arguments" x))
                   (_
                    (identifier? x)
                    #'proc-name))))))))))

(define (default-record-printer s p)
  (display "#<" p)
  (display (record-type-name (record-type-descriptor s)) p)
  (let loop ((fields (record-type-fields (record-type-descriptor s)))
             (off 0))
    (cond
     ((not (null? fields))
      (display " " p)
      (display (car fields) p)
      (display ": " p)
      (write (struct-ref s off) p)
      (loop (cdr fields) (+ 1 off)))))
  (display ">" p))

(define-syntax-rule (throw-bad-struct s who)
  (let ((s* s))
    (throw 'wrong-type-arg who
           "Wrong type argument: ~S" (list s*)
           (list s*))))

(define (make-copier-id type-name)
  (datum->syntax type-name
                 (symbol-append '%% (syntax->datum type-name)
                                '-set-fields)))

(define-syntax %%set-fields
  (lambda (x)
    (syntax-case x ()
      ((_ type-name (getter-id ...) check? s (getter expr) ...)
       (every identifier? #'(getter ...))
       (let ((copier-name (syntax->datum (make-copier-id #'type-name)))
             (getter+exprs #'((getter expr) ...))
             (nfields (length #'(getter-id ...))))
         (define (lookup id default-expr)
           (let ((results
                  (filter (lambda (g+e)
                            (free-identifier=? id (car g+e)))
                          getter+exprs)))
             (case (length results)
               ((0) default-expr)
               ((1) (cadar results))
               (else (syntax-violation
                      copier-name "duplicate getter" x id)))))
         (for-each (lambda (id)
                     (or (find (lambda (getter-id)
                                 (free-identifier=? id getter-id))
                               #'(getter-id ...))
                         (syntax-violation
                          copier-name "unknown getter" x id)))
                   #'(getter ...))
         (with-syntax ((unsafe-expr
                        #`(let ((new (allocate-struct type-name #,nfields)))
                            #,@(map (lambda (getter index)
                                      #`(struct-set!
                                         new
                                         #,index
                                         #,(lookup getter
                                                   #`(struct-ref s #,index))))
                                    #'(getter-id ...)
                                    (iota nfields))
                            new)))
           (if (syntax->datum #'check?)
               #`(if (eq? (struct-vtable s) type-name)
                     unsafe-expr
                     (throw-bad-struct
                      s '#,(datum->syntax #'here copier-name)))
               #'unsafe-expr)))))))

(define-syntax %define-record-type
  (lambda (x)
    (define (field-identifiers field-specs)
      (map (lambda (field-spec)
             (syntax-case field-spec ()
               ((name getter) #'name)
               ((name getter setter) #'name)))
           field-specs))

    (define (getter-identifiers field-specs)
      (map (lambda (field-spec)
             (syntax-case field-spec ()
               ((name getter) #'getter)
               ((name getter setter) #'getter)))
           field-specs))

    (define (constructor form type-name constructor-spec field-ids)
      (syntax-case constructor-spec ()
        ((ctor field ...)
         (every identifier? #'(field ...))
         (let ((slots (map (lambda (field)
                             (or (list-index (lambda (x)
                                               (free-identifier=? x field))
                                             field-ids)
                                 (syntax-violation
                                  (syntax-case form ()
                                    ((macro . args)
                                     (syntax->datum #'macro)))
                                  "unknown field in constructor spec"
                                  form field)))
                           #'(field ...))))
           #`(define-inlinable #,constructor-spec
               (let ((s (allocate-struct #,type-name #,(length field-ids))))
                 #,@(map (lambda (arg slot)
                           #`(struct-set! s #,slot #,arg))
                         #'(field ...) slots)
                 s))))))

    (define (getters type-name getter-ids copier-id)
      (map (lambda (getter index)
             #`(define-tagged-inlinable
                 ((%%type   #,type-name)
                  (%%index  #,index)
                  (%%copier #,copier-id))
                 (#,getter s)
                 (if (eq? (struct-vtable s) #,type-name)
                     (struct-ref s #,index)
                     (throw-bad-struct s '#,getter))))
           getter-ids
           (iota (length getter-ids))))

    (define (copier type-name getter-ids copier-id)
      #`(define-syntax-rule
          (#,copier-id check? s (getter expr) (... ...))
          (%%set-fields #,type-name #,getter-ids
                        check? s (getter expr) (... ...))))

    (define (setters type-name field-specs)
      (filter-map (lambda (field-spec index)
                    (syntax-case field-spec ()
                      ((name getter) #f)
                      ((name getter setter)
                       #`(define-inlinable (setter s val)
                           (if (eq? (struct-vtable s) #,type-name)
                               (struct-set! s #,index val)
                               (throw-bad-struct s 'setter))))))
                  field-specs
                  (iota (length field-specs))))

    (define (functional-setters copier-id field-specs)
      (filter-map (lambda (field-spec index)
                    (syntax-case field-spec ()
                      ((name getter) #f)
                      ((name getter setter)
                       #`(define-inlinable (setter s val)
                           (#,copier-id #t s (getter val))))))
                  field-specs
                  (iota (length field-specs))))

    (define (record-layout immutable? count)
      ;; Mutability is expressed on the record level; all structs in the
      ;; future will be mutable.
      (string-concatenate (make-list count "pw")))

    (syntax-case x ()
      ((_ immutable? form type-name constructor-spec predicate-name
          field-spec ...)
       (let ()
         (define (syntax-error message subform)
           (syntax-violation (syntax-case #'form ()
                               ((macro . args) (syntax->datum #'macro)))
                             message #'form subform))
         (and (boolean? (syntax->datum #'immutable?))
              (or (identifier? #'type-name)
                  (syntax-error "expected type name" #'type-name))
              (syntax-case #'constructor-spec ()
                ((ctor args ...)
                 (every identifier? #'(ctor args ...))
                 #t)
                (_ (syntax-error "invalid constructor spec"
                                 #'constructor-spec)))
              (or (identifier? #'predicate-name)
                  (syntax-error "expected predicate name" #'predicate-name))
              (every (lambda (spec)
                       (syntax-case spec ()
                         ((field getter) #t)
                         ((field getter setter) #t)
                         (_ (syntax-error "invalid field spec" spec))))
                     #'(field-spec ...))))
       (let* ((field-ids   (field-identifiers  #'(field-spec ...)))
              (getter-ids  (getter-identifiers #'(field-spec ...)))
              (field-count (length field-ids))
              (immutable?  (syntax->datum #'immutable?))
              (layout      (record-layout immutable? field-count))
              (ctor-name   (syntax-case #'constructor-spec ()
                             ((ctor args ...) #'ctor)))
              (copier-id   (make-copier-id #'type-name)))
         #`(begin
             #,(constructor #'form #'type-name #'constructor-spec field-ids)

             (define type-name
               (let ((rtd (make-struct/no-tail
                           record-type-vtable
                           '#,(datum->syntax #'here (make-struct-layout layout))
                           default-record-printer
                           'type-name
                           '#,field-ids)))
                 (set-struct-vtable-name! rtd 'type-name)
                 (struct-set! rtd (+ 2 vtable-offset-user) #,ctor-name)
                 rtd))

             (define-inlinable (predicate-name obj)
               (and (struct? obj)
                    (eq? (struct-vtable obj) type-name)))

             #,@(getters #'type-name getter-ids copier-id)
             #,(copier #'type-name getter-ids copier-id)
             #,@(if immutable?
                    (functional-setters copier-id #'(field-spec ...))
                    (setters #'type-name #'(field-spec ...))))))
      ((_ immutable? form . rest)
       (syntax-violation
        (syntax-case #'form ()
          ((macro . args) (syntax->datum #'macro)))
        "invalid record definition syntax"
        #'form)))))

(define-syntax-rule (define-record-type name ctor pred fields ...)
  (%define-record-type #f (define-record-type name ctor pred fields ...)
                       name ctor pred fields ...))

;;; srfi-9.scm ends here
