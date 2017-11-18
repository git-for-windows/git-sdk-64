;;; base.scm --- The R6RS base library

;;      Copyright (C) 2010, 2011 Free Software Foundation, Inc.
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


(library (rnrs base (6))
  (export boolean? symbol? char? vector? null? pair? number? string? procedure?
	 
	  define define-syntax syntax-rules lambda let let* let-values 
	  let*-values letrec letrec* begin 

	  quote lambda if set! cond case 
	 
	  or and not
	 
	  eqv? equal? eq?
	 
	  + - * / max min abs numerator denominator gcd lcm floor ceiling
	  truncate round rationalize real-part imag-part make-rectangular angle
	  div mod div-and-mod div0 mod0 div0-and-mod0
	  
	  expt exact-integer-sqrt sqrt exp log sin cos tan asin acos atan 
	  make-polar magnitude angle
	 
	  complex? real? rational? integer? exact? inexact? real-valued?
	  rational-valued? integer-valued? zero? positive? negative? odd? even?
	  nan? finite? infinite?

	  exact inexact = < > <= >= 

	  number->string string->number

          boolean=?

	  cons car cdr caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr 
	  cddar cdddr caaaar caaadr caadar cadaar cdaaar cddaar cdadar cdaadr 
	  cadadr caaddr caddar cadddr cdaddr cddadr cdddar cddddr

	  list? list length append reverse list-tail list-ref map for-each

	  symbol->string string->symbol symbol=?

	  char->integer integer->char char=? char<? char>? char<=? char>=?

	  make-string string string-length string-ref string=? string<? string>?
	  string<=? string>=? substring string-append string->list list->string
	  string-for-each string-copy

	  vector? make-vector vector vector-length vector-ref vector-set! 
	  vector->list list->vector vector-fill! vector-map vector-for-each

	  error assertion-violation assert

	  call-with-current-continuation call/cc call-with-values dynamic-wind
	  values apply

	  quasiquote unquote unquote-splicing

	  let-syntax letrec-syntax

	  syntax-rules identifier-syntax)
  (import (rename (except (guile) error raise map string-for-each)
                  (log log-internal)
                  (euclidean-quotient div)
                  (euclidean-remainder mod)
                  (euclidean/ div-and-mod)
                  (centered-quotient div0)
                  (centered-remainder mod0)
                  (centered/ div0-and-mod0)
                  (inf? infinite?)
                  (exact->inexact inexact)
                  (inexact->exact exact))
          (srfi srfi-11))

 (define string-for-each
   (case-lambda
     ((proc string)
      (let ((end (string-length string)))
        (let loop ((i 0))
          (unless (= i end)
            (proc (string-ref string i))
            (loop (+ i 1))))))
     ((proc string1 string2)
      (let ((end1 (string-length string1))
            (end2 (string-length string2)))
        (unless (= end1 end2)
          (assertion-violation 'string-for-each
                               "string arguments must all have the same length"
                               string1 string2))
        (let loop ((i 0))
          (unless (= i end1)
            (proc (string-ref string1 i)
                  (string-ref string2 i))
            (loop (+ i 1))))))
     ((proc string . strings)
      (let ((end (string-length string))
            (ends (map string-length strings)))
        (for-each (lambda (x)
                    (unless (= end x)
                      (apply assertion-violation
                             'string-for-each
                             "string arguments must all have the same length"
                             string strings)))
                  ends)
        (let loop ((i 0))
          (unless (= i end)
            (apply proc
                   (string-ref string i)
                   (map (lambda (s) (string-ref s i)) strings))
            (loop (+ i 1))))))))

 (define map
   (case-lambda
     ((f l)
      (let map1 ((hare l) (tortoise l) (move? #f) (out '()))
        (if (pair? hare)
            (if move?
                (if (eq? tortoise hare)
                    (scm-error 'wrong-type-arg "map" "Circular list: ~S"
                               (list l) #f)
                    (map1 (cdr hare) (cdr tortoise) #f
                          (cons (f (car hare)) out)))
                (map1 (cdr hare) tortoise #t
                      (cons (f (car hare)) out)))
            (if (null? hare)
                (reverse out)
                (scm-error 'wrong-type-arg "map" "Not a list: ~S"
                           (list l) #f)))))
    
     ((f l1 l2)
      (let map2 ((h1 l1) (h2 l2) (t1 l1) (t2 l2) (move? #f) (out '()))
        (cond
         ((pair? h1)
          (cond
           ((not (pair? h2))
            (scm-error 'wrong-type-arg "map"
                       (if (list? h2)
                           "List of wrong length: ~S"
                           "Not a list: ~S")
                       (list l2) #f))
           ((not move?)
            (map2 (cdr h1) (cdr h2) t1 t2 #t
                  (cons (f (car h1) (car h2)) out)))
           ((eq? t1 h1)
            (scm-error 'wrong-type-arg "map" "Circular list: ~S"
                       (list l1) #f))
           ((eq? t2 h2)
            (scm-error 'wrong-type-arg "map" "Circular list: ~S"
                       (list l2) #f))
           (else
            (map2 (cdr h1) (cdr h2) (cdr t1) (cdr t2) #f
                  (cons (f (car h1) (car h2)) out)))))

         ((and (null? h1) (null? h2))
          (reverse out))
        
         ((null? h1)
          (scm-error 'wrong-type-arg "map"
                     (if (list? h2)
                         "List of wrong length: ~S"
                         "Not a list: ~S")
                     (list l2) #f))
         (else
          (scm-error 'wrong-type-arg "map"
                     "Not a list: ~S"
                     (list l1) #f)))))

     ((f l1 . rest)
      (let ((len (length l1)))
        (let mapn ((rest rest))
          (or (null? rest)
              (if (= (length (car rest)) len)
                  (mapn (cdr rest))
                  (scm-error 'wrong-type-arg "map" "List of wrong length: ~S"
                             (list (car rest)) #f)))))
      (let mapn ((l1 l1) (rest rest) (out '()))
        (if (null? l1)
            (reverse out)
            (mapn (cdr l1) (map cdr rest)
                  (cons (apply f (car l1) (map car rest)) out)))))))

 (define log
   (case-lambda
     ((n)
      (log-internal n))
     ((n base)
      (/ (log n)
         (log base)))))

 (define (boolean=? . bools)
   (define (boolean=?-internal lst last)
     (or (null? lst)
         (let ((bool (car lst))) 
           (and (eqv? bool last) (boolean=?-internal (cdr lst) bool)))))
   (or (null? bools)
       (let ((bool (car bools)))
         (and (boolean? bool) (boolean=?-internal (cdr bools) bool)))))

 (define (symbol=? . syms)
   (define (symbol=?-internal lst last)
     (or (null? lst)
         (let ((sym (car lst))) 
           (and (eq? sym last) (symbol=?-internal (cdr lst) sym)))))
   (or (null? syms)
       (let ((sym (car syms)))
         (and (symbol? sym) (symbol=?-internal (cdr syms) sym)))))

 (define (real-valued? x)
   (and (complex? x)
        (zero? (imag-part x))))

 (define (rational-valued? x)
   (and (real-valued? x)
        (rational? (real-part x))))

 (define (integer-valued? x)
   (and (rational-valued? x)
        (= x (floor (real-part x)))))

 (define (vector-for-each proc . vecs)
   (apply for-each (cons proc (map vector->list vecs))))
 (define (vector-map proc . vecs)
   (list->vector (apply map (cons proc (map vector->list vecs)))))

 (define-syntax define-proxy
   (syntax-rules (@)
     ;; Define BINDING to point to (@ MODULE ORIGINAL).  This hack is to
     ;; make sure MODULE is loaded lazily, at run-time, when BINDING is
     ;; encountered, rather than being loaded while compiling and
     ;; loading (rnrs base).
     ;; This avoids circular dependencies among modules and makes
     ;; (rnrs base) more lightweight.
     ((_ binding (@ module original))
      (define-syntax binding
        (identifier-syntax
         (module-ref (resolve-interface 'module) 'original))))))

 (define-proxy raise
   (@ (rnrs exceptions) raise))

 (define-proxy condition
   (@ (rnrs conditions) condition))
 (define-proxy make-error
   (@ (rnrs conditions) make-error))
 (define-proxy make-assertion-violation
   (@ (rnrs conditions) make-assertion-violation))
 (define-proxy make-who-condition
   (@ (rnrs conditions) make-who-condition))
 (define-proxy make-message-condition
   (@ (rnrs conditions) make-message-condition))
 (define-proxy make-irritants-condition
   (@ (rnrs conditions) make-irritants-condition))

 (define (error who message . irritants)
   (raise (apply condition
                 (append (list (make-error))
                         (if who (list (make-who-condition who)) '())
                         (list (make-message-condition message)
                               (make-irritants-condition irritants))))))
 
 (define (assertion-violation who message . irritants)
   (raise (apply condition
                 (append (list (make-assertion-violation))
                         (if who (list (make-who-condition who)) '())
                         (list (make-message-condition message)
                               (make-irritants-condition irritants))))))

 (define-syntax assert
   (syntax-rules ()
     ((_ expression)
      (or expression
          (raise (condition
                  (make-assertion-violation)
                  (make-message-condition
                   (format #f "assertion failed: ~s" 'expression))))))))

)
