;;; Guile VM specific syntaxes and utilities

;; Copyright (C) 2001, 2009 Free Software Foundation, Inc

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

;;; Code:

(define-module (system base syntax)
  #:export (%compute-initargs)
  #:export-syntax (define-type define-record define-record/keywords
                   record-case transform-record))

(define (symbol-trim-both sym pred)
  (string->symbol (string-trim-both (symbol->string sym) pred)))
(define (trim-brackets sym)
  (symbol-trim-both sym (list->char-set '(#\< #\>))))


;;;
;;; Type
;;;

(define-macro (define-type name . rest)
  (let ((name (if (pair? name) (car name) name))
        (opts (if (pair? name) (cdr name) '())))
    (let ((printer (kw-arg-ref opts #:printer))
          (common-slots (or (kw-arg-ref opts #:common-slots) '())))
      `(begin ,@(map (lambda (def)
                       `(define-record ,(if printer
                                            `(,(car def) ,printer)
                                            (car def))
                          ,@common-slots
                          ,@(cdr def)))
                     rest)
              ,@(map (lambda (common-slot i)
                       `(define ,(symbol-append (trim-brackets name)
                                                '- common-slot)
                          (make-procedure-with-setter
                           (lambda (x) (struct-ref x ,i))
                           (lambda (x v) (struct-set! x ,i v)))))
                     common-slots (iota (length common-slots)))))))


;;;
;;; Record
;;;

(define-macro (define-record name-form . slots)
  (let* ((name (if (pair? name-form) (car name-form) name-form))
         (printer (and (pair? name-form) (cadr name-form)))
         (slot-names (map (lambda (slot) (if (pair? slot) (car slot) slot))
                          slots))
         (stem (trim-brackets name)))
    `(begin
       (define ,name (make-record-type ,(symbol->string name) ',slot-names
                                       ,@(if printer (list printer) '())))
       ,(let* ((reqs (let lp ((slots slots))
                       (if (or (null? slots) (not (symbol? (car slots))))
                           '()
                           (cons (car slots) (lp (cdr slots))))))
               (opts (list-tail slots (length reqs)))
               (tail (gensym)))
          `(define (,(symbol-append 'make- stem) ,@reqs . ,tail)
             (let ,(map (lambda (o)
                          `(,(car o) (cond ((null? ,tail) ,(cadr o))
                                           (else (let ((_x (car ,tail)))
                                                   (set! ,tail (cdr ,tail))
                                                   _x)))))
                        opts)
               (make-struct ,name 0 ,@slot-names))))
       (define ,(symbol-append stem '?) (record-predicate ,name))
       ,@(map (lambda (sname)
                `(define ,(symbol-append stem '- sname)
                   (make-procedure-with-setter
                    (record-accessor ,name ',sname)
                    (record-modifier ,name ',sname))))
              slot-names))))

;; like the former, but accepting keyword arguments in addition to
;; optional arguments
(define-macro (define-record/keywords name-form . slots)
  (let* ((name (if (pair? name-form) (car name-form) name-form))
         (printer (and (pair? name-form) (cadr name-form)))
         (slot-names (map (lambda (slot) (if (pair? slot) (car slot) slot))
                          slots))
         (stem (trim-brackets name)))
    `(begin
       (define ,name (make-record-type ,(symbol->string name) ',slot-names
                                       ,@(if printer (list printer) '())))
       (define ,(symbol-append 'make- stem)
         (let ((slots (list ,@(map (lambda (slot)
                                     (if (pair? slot)
                                         `(cons ',(car slot) ,(cadr slot))
                                         `',slot))
                                   slots)))
               (constructor (record-constructor ,name)))
           (lambda args
             (apply constructor (%compute-initargs args slots)))))
       (define ,(symbol-append stem '?) (record-predicate ,name))
       ,@(map (lambda (sname)
                `(define ,(symbol-append stem '- sname)
                   (make-procedure-with-setter
                    (record-accessor ,name ',sname)
                    (record-modifier ,name ',sname))))
              slot-names))))

(define (%compute-initargs args slots)
  (define (finish out)
    (map (lambda (slot)
           (let ((name (if (pair? slot) (car slot) slot)))
             (cond ((assq name out) => cdr)
                   ((pair? slot) (cdr slot))
                   (else (error "unbound slot" args slots name)))))
         slots))
  (let lp ((in args) (positional slots) (out '()))
    (cond
     ((null? in)
      (finish out))
     ((keyword? (car in))
      (let ((sym (keyword->symbol (car in))))
        (cond
         ((and (not (memq sym slots))
               (not (assq sym (filter pair? slots))))
          (error "unknown slot" sym))
         ((assq sym out) (error "slot already set" sym out))
         (else (lp (cddr in) '() (acons sym (cadr in) out))))))
     ((null? positional)
      (error "too many initargs" args slots))
     (else
      (lp (cdr in) (cdr positional)
          (let ((slot (car positional)))
            (acons (if (pair? slot) (car slot) slot)
                   (car in)
                   out)))))))

;; So, dear reader. It is pleasant indeed around this fire or at this
;; cafe or in this room, is it not? I think so too.
;;
;; This macro used to generate code that looked like this:
;;
;;  `(((record-predicate ,record-type) ,r)
;;    (let ,(map (lambda (slot)
;;                 (if (pair? slot)
;;                     `(,(car slot) ((record-accessor ,record-type ',(cadr slot)) ,r))
;;                     `(,slot ((record-accessor ,record-type ',slot) ,r))))
;;               slots)
;;      ,@body)))))
;;
;; But this was a hot spot, so computing all those predicates and
;; accessors all the time was getting expensive, so we did a terrible
;; thing: we decided that since above we're already defining accessors
;; and predicates with computed names, we might as well just rely on that fact here.
;;
;; It's a bit nasty, I agree. But it is fast.
;;
;;scheme@(guile-user)> (with-statprof #:hz 1000 #:full-stacks? #t (resolve-module '(oop goops)))%     cumulative   self             
;; time   seconds     seconds      name
;;   8.82      0.03      0.01  glil->assembly
;;   8.82      0.01      0.01  record-type-fields
;;   5.88      0.01      0.01  %compute-initargs
;;   5.88      0.01      0.01  list-index


;;; So ugly... but I am too ignorant to know how to make it better.
(define-syntax record-case
  (lambda (x)
    (syntax-case x ()
      ((_ record clause ...)
       (let ((r (syntax r))
             (rtd (syntax rtd)))
         (define (process-clause tag fields exprs)
           (let ((infix (trim-brackets (syntax->datum tag))))
             (with-syntax ((tag tag)
                           (((f . accessor) ...)
                            (let lp ((fields fields))
                              (syntax-case fields ()
                                (() (syntax ()))
                                (((v0 f0) f1 ...)
                                 (acons (syntax v0)
                                        (datum->syntax x 
                                                       (symbol-append infix '- (syntax->datum
                                                                                (syntax f0))))
                                        (lp (syntax (f1 ...)))))
                                ((f0 f1 ...)
                                 (acons (syntax f0)
                                        (datum->syntax x 
                                                       (symbol-append infix '- (syntax->datum
                                                                                (syntax f0))))
                                        (lp (syntax (f1 ...))))))))
                           ((e0 e1 ...)
                            (syntax-case exprs ()
                              (() (syntax (#t)))
                              ((e0 e1 ...) (syntax (e0 e1 ...))))))
               (syntax
                ((eq? rtd tag)
                 (let ((f (accessor r))
                       ...)
                   e0 e1 ...))))))
         (with-syntax
             ((r r)
              (rtd rtd)
              ((processed ...)
               (let lp ((clauses (syntax (clause ...)))
                        (out '()))
                 (syntax-case clauses (else)
                   (()
                    (reverse! (cons (syntax
                                     (else (error "unhandled record" r)))
                                    out)))
                   (((else e0 e1 ...))
                    (reverse! (cons (syntax (else e0 e1 ...)) out)))
                   (((else e0 e1 ...) . rest)
                    (syntax-violation 'record-case
                                      "bad else clause placement"
                                      (syntax x)
                                      (syntax (else e0 e1 ...))))
                   ((((<foo> f0 ...) e0 ...) . rest)
                    (lp (syntax rest)
                        (cons (process-clause (syntax <foo>)
                                              (syntax (f0 ...))
                                              (syntax (e0 ...)))
                              out)))))))
           (syntax
            (let* ((r record)
                   (rtd (struct-vtable r)))
              (cond processed ...)))))))))


;; Here we take the terrorism to another level. Nasty, but the client
;; code looks good.

(define-macro (transform-record type-and-common record . clauses)
  (let ((r (gensym))
        (rtd (gensym))
        (type-stem (trim-brackets (car type-and-common))))
    (define (make-stem s)
      (symbol-append type-stem '- s))
    (define (further-predicates x record-stem slots)
      (define (access slot)
        `(,(symbol-append (make-stem record-stem) '- slot) ,x))
      (let lp ((in slots) (out '()))
        (cond ((null? in) out)
              ((pair? (car in))
               (let ((slot (caar in))
                     (arg (cadar in)))
                 (cond ((symbol? arg)
                        (lp (cdr in) out))
                       ((pair? arg)
                        (lp (cdr in)
                            (append (further-predicates (access slot)
                                                        (car arg)
                                                        (cdr arg))
                                    out)))
                       (else (lp (cdr in) (cons `(eq? ,(access slot) ',arg)
                                                out))))))
              (else (lp (cdr in) out)))))
    (define (let-clauses x record-stem slots)
      (define (access slot)
        `(,(symbol-append (make-stem record-stem) '- slot) ,x))
      (let lp ((in slots) (out '()))
        (cond ((null? in) out)
              ((pair? (car in))
               (let ((slot (caar in))
                     (arg (cadar in)))
                 (cond ((symbol? arg)
                        (lp (cdr in)
                            (cons `(,arg ,(access slot)) out)))
                       ((pair? arg)
                        (lp (cdr in)
                            (append (let-clauses (access slot)
                                                 (car arg)
                                                 (cdr arg))
                                    out)))
                       (else
                        (lp (cdr in) out)))))
              (else
               (lp (cdr in)
                   (cons `(,(car in) ,(access (car in))) out))))))
    (define (transform-expr x)
      (cond ((not (pair? x)) x)
            ((eq? (car x) '->)
             (if (= (length x) 2)
                 (let ((form (cadr x)))
                   `(,(symbol-append 'make- (make-stem (car form)))
                     ,@(cdr type-and-common)
                     ,@(map (lambda (y)
                              (if (and (pair? y) (eq? (car y) 'unquote))
                                  (transform-expr (cadr y))
                                  y))
                            (cdr form))))
                 (error "bad -> form" x)))
            (else (cons (car x) (map transform-expr (cdr x))))))
    (define (process-clause clause)
      (if (eq? (car clause) 'else)
          clause
          (let ((stem (caar clause))
                (slots (cdar clause))
                (body (cdr clause)))
            (let ((record-type (symbol-append '< (make-stem stem) '>)))
              `((and (eq? ,rtd ,record-type)
                     ,@(reverse (further-predicates r stem slots)))
                (let ,(reverse (let-clauses r stem slots))
                  ,@(if (pair? body)
                        (map transform-expr body)
                        '((if #f #f)))))))))
    `(let* ((,r ,record)
            (,rtd (struct-vtable ,r))
            ,@(map (lambda (slot)
                     `(,slot (,(make-stem slot) ,r)))
                   (cdr type-and-common)))
       (cond ,@(let ((clauses (map process-clause clauses)))
                 (if (assq 'else clauses)
                     clauses
                     (append clauses `((else (error "unhandled record" ,r))))))))))
