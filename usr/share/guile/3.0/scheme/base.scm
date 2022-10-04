;;; R7RS compatibility libraries
;;; Copyright (C) 2019-2021 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Based on code from https://gitlab.com/akku/akku-scm, written
;;; 2018-2019 by Göran Weinholt <goran@weinholt.se>, as well as
;;; https://github.com/okuoku/yuni, written 2014-2018 by OKUMURA Yuki
;;; <mjt@cltn.org>.  This code was originally released under the
;;; following terms:
;;;
;;;     To the extent possible under law, the author(s) have dedicated
;;;     all copyright and related and neighboring rights to this
;;;     software to the public domain worldwide. This software is
;;;     distributed without any warranty.
;;;
;;;     See <http://creativecommons.org/publicdomain/zero/1.0/>, for a
;;;     copy of the CC0 Public Domain Dedication.

(define-module (scheme base)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 exceptions)
  #:use-module ((srfi srfi-34) #:select (guard))
  #:use-module (ice-9 ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (error-object-message error-object-irritants
            file-error?
            (r7:error . error)
            (r7:cond-expand . cond-expand)
            (r7:include . include)
            (r7:include-ci . include-ci)
            (r7:let-syntax . let-syntax)
            member assoc list-copy map for-each
            binary-port? textual-port?
            open-input-bytevector
            open-output-bytevector get-output-bytevector
            peek-u8 read-u8 read-bytevector read-bytevector!
            read-string read-line
            write-u8 write-bytevector write-string flush-output-port
            (r7:string-map . string-map)
            bytevector bytevector-append
            string->vector vector->string
            (r7:string->utf8 . string->utf8)
            (r7:vector->list . vector->list)
            vector-append vector-for-each vector-map
            (r7:bytevector-copy . bytevector-copy)
            (r7:bytevector-copy! . bytevector-copy!)
            (r7:utf8->string . utf8->string)
            square
            (r7:expt . expt)
            boolean=? symbol=?
            features
            input-port-open? output-port-open?)
  #:re-export
  (_
   ... => else
   * + - / < <= = > >= abs and append apply assq assv begin
   boolean?
   bytevector-length
   bytevector-u8-ref bytevector-u8-set! bytevector? caar cadr
   call-with-current-continuation call-with-port call-with-values
   call/cc car case cdar cddr cdr ceiling char->integer char-ready?
   char<=? char<? char=? char>=? char>? char? close-input-port
   close-output-port close-port complex? cond cons
   current-error-port current-input-port current-output-port define
   define-record-type define-syntax define-values denominator do
   dynamic-wind eof-object eof-object? eq? equal? eqv?
   (exception? . error-object?)
   even?
   (inexact->exact . exact)
   (exact->inexact . inexact)
   exact-integer-sqrt exact-integer? exact?
   floor floor-quotient floor-remainder floor/
   gcd
   get-output-string guard if inexact?
   input-port? integer->char integer? lambda lcm
   length let let* let*-values let-values letrec letrec*
   letrec-syntax list list->string list->vector list-ref
   list-set! list-tail list? make-bytevector make-list make-parameter
   make-string make-vector max memq memv min modulo
   negative? newline not null? number->string number? numerator odd?
   open-input-string
   open-output-string or output-port? pair?
   parameterize peek-char port? positive? procedure?
   quasiquote quote quotient
   (raise-exception . raise)
   raise-continuable
   rational?
   rationalize read-char
   (lexical-error? . read-error?)
   real? remainder reverse round set!
   set-car! set-cdr! string string->list string->number
   string->symbol string-append
   string-copy string-copy! string-fill! string-for-each
   string-length string-ref string-set! string<=? string<?
   string=? string>=? string>? string? substring symbol->string
   symbol? syntax-error syntax-rules truncate
   truncate-quotient truncate-remainder truncate/
   (char-ready? . u8-ready?)
   unless
   unquote unquote-splicing values
   vector vector-copy vector-copy! vector-fill!
   vector-length vector-ref vector-set! vector?
   when with-exception-handler write-char
   zero?))

(define* (member x ls #:optional (= equal?))
  (cond
   ((eq? = eq?) (memq x ls))
   ((eq? = eqv?) (memv x ls))
   (else 
    (unless (procedure? =)
      (error "not a procedure" =))
    (let lp ((ls ls))
      (cond
       ((null? ls) #f)
       ((= (car ls) x) ls)
       (else (lp (cdr ls))))))))

(define* (assoc x ls #:optional (= equal?))
  (cond
   ((eq? = eq?) (assq x ls))
   ((eq? = eqv?) (assv x ls))
   (else 
    (unless (procedure? =)
      (error "not a procedure" =))
    (let lp ((ls ls))
      (cond
       ((null? ls) #f)
       ((= (caar ls) x) (car ls))
       (else (lp (cdr ls))))))))

(define (list-copy x)
  (if (pair? x)
      (cons (car x) (list-copy (cdr x)))
      x))

(define (circular-list? x)
  (and (pair? x)
       (let lp ((hare (cdr x)) (tortoise x))
         (and (pair? hare)
              (let ((hare (cdr hare)))
	        (and (pair? hare)
                     (or (eq? hare tortoise)
                         (lp (cdr hare) (cdr tortoise)))))))))

(define map
  (case-lambda
    ((f l)
     (unless (or (list? l)
                 (circular-list? l))
       (scm-error 'wrong-type-arg "map" "Not a list: ~S"
                  (list l) #f))
     (let map1 ((l l))
       (if (pair? l)
           (cons (f (car l)) (map1 (cdr l)))
           '())))

    ((f l1 l2)
     (cond
      ((list? l1)
       (unless (or (list? l2) (circular-list? l2))
         (scm-error 'wrong-type-arg "map" "Not a list: ~S"
                    (list l2) #f)))
      ((circular-list? l1)
       (unless (list? l2)
         (scm-error 'wrong-type-arg "map" "Not a finite list: ~S"
                    (list l2) #f)))
      (else
       (scm-error 'wrong-type-arg "map" "Not a list: ~S"
                  (list l1) #f)))
     (let map2 ((l1 l1) (l2 l2))
       (if (and (pair? l1) (pair? l2))
           (cons (f (car l1) (car l2))
                 (map2 (cdr l1) (cdr l2)))
           '())))

    ((f l1 . rest)
     (let ((lists (cons l1 rest)))
       (unless (and-map list? lists)
         (unless (or-map list? lists)
           (scm-error 'wrong-type-arg "map"
                      "Arguments do not contain a finite list" '() #f))
         (for-each (lambda (x)
                     (unless (or (list? x) (circular-list? x))
                       (scm-error 'wrong-type-arg "map" "Not a list: ~S"
                                  (list x) #f)))
                   lists))
       (let mapn ((lists lists))
         (if (and-map pair? lists)
             (cons (apply f (map car lists)) (mapn (map cdr lists)))
             '()))))))

(define for-each
  (case-lambda
    ((f l)
     (unless (or (list? l)
                 (circular-list? l))
       (scm-error 'wrong-type-arg "for-each" "Not a list: ~S"
                  (list l) #f))
     (let for-each1 ((l l))
       (when (pair? l)
         (f (car l))
         (for-each1 (cdr l)))))

    ((f l1 l2)
     (cond
      ((list? l1)
       (unless (or (list? l2) (circular-list? l2))
         (scm-error 'wrong-type-arg "for-each" "Not a list: ~S"
                    (list l2) #f)))
      ((circular-list? l1)
       (unless (list? l2)
         (scm-error 'wrong-type-arg "for-each" "Not a finite list: ~S"
                    (list l2) #f)))
      (else
       (scm-error 'wrong-type-arg "for-each" "Not a list: ~S"
                  (list l1) #f)))
     (let for-each2 ((l1 l1) (l2 l2))
       (when (and (pair? l1) (pair? l2))
         (f (car l1) (car l2))
         (for-each2 (cdr l1) (cdr l2)))))

    ((f l1 . rest)
     (let ((lists (cons l1 rest)))
       (unless (and-map list? lists)
         (unless (or-map list? lists)
           (scm-error 'wrong-type-arg "for-each"
                      "Arguments do not contain a finite list" '() #f))
         (for-each (lambda (x)
                     (unless (or (list? x) (circular-list? x))
                       (scm-error 'wrong-type-arg "for-each" "Not a list: ~S"
                                  (list x) #f)))
                   lists))
       (let for-eachn ((lists lists))
         (when (and-map pair? lists)
           (apply f (map car lists))
           (for-eachn (map cdr lists))))))))

;; FIXME.
(define (file-error? x) #f)

(define (error-object-message obj)
  (and (exception-with-message? obj)
       (exception-message obj)))

(define (error-object-irritants obj)
  (and (exception-with-irritants? obj)
       (exception-irritants obj)))

(define (r7:error message . irritants)
  (raise-exception
   (let ((exn (make-exception-with-message message)))
     (if (null? irritants)
         exn
         (make-exception exn
                         (make-exception-with-irritants irritants))))))

(define-syntax r7:cond-expand
  (lambda (x)
    (define (has-req? req)
      (syntax-case req (and or not library)
        ((and req ...)
         (and-map has-req? #'(req ...)))
        ((or req ...)
         (or-map has-req? #'(req ...)))
        ((not req)
         (not (has-req? #'req)))
        ((library lib-name)
         (->bool (resolve-interface (syntax->datum #'lib-name))))
        (id
         (identifier? #'id)
         (memq (syntax->datum #'id) (features)))))
    (syntax-case x (else)
      ((_)
       (syntax-violation 'cond-expand "Unfulfilled cond-expand" x))
      ((_ (else body ...))
       #'(begin body ...))
      ((_ (req body ...) more-clauses ...)
       (if (has-req? #'req)
           #'(begin body ...)
           #'(r7:cond-expand more-clauses ...))))))

(define-syntax-rule (r7:include fn* ...)
  (begin (include fn*) ...))

(define-syntax-rule (r7:include-ci fn* ...)
  (begin (include-ci fn*) ...))

(define-syntax-rule (r7:let-syntax ((vars trans) ...) . expr)
  (let-syntax ((vars trans) ...)
    (let () . expr)))

(define (boolean=? x y . y*)
  (unless (boolean? x) (error "not a boolean" x))
  (unless (boolean? y) (error "not a boolean" y))
  (and (eq? x y)
       (or (null? y*)
           (apply boolean=? x y*))))

(define (symbol=? x y . y*)
  (unless (symbol? x) (error "not a symbol" x))
  (unless (symbol? y) (error "not a symbol" y))
  (and (symbol? x)
       (eq? x y)
       (or (null? y*)
           (apply symbol=? x y*))))

(define (binary-port? p) (port? p))
(define (textual-port? p) (port? p))

(define (open-input-bytevector bv) (open-bytevector-input-port bv))

(define (open-output-bytevector)
  (let-values (((p extract) (open-bytevector-output-port)))
    (define pos 0)
    (define buf #vu8())
    (define (read! target target-start count)
      (when (zero? (- (bytevector-length buf) pos))
        (set! buf (bytevector-append buf (extract))))  ;resets p
      (let ((count (min count (- (bytevector-length buf) pos))))
        (bytevector-copy! buf pos
                          target target-start count)
        (set! pos (+ pos count))
        count))
    (define (write! bv start count)
      (put-bytevector p bv start count)
      (set! pos (+ pos count))
      count)
    (define (get-position)
      pos)
    (define (set-position! new-pos)
      (set! pos new-pos))
    (define (close)
      (close-port p))
    ;; It's actually an input/output port, but only
    ;; get-output-bytevector should ever read from it. If it was just
    ;; an output port then there would be no good way for
    ;; get-output-bytevector to read the data. -weinholt
    (make-custom-binary-input/output-port
     "bytevector" read! write! get-position set-position! close)))

(define (get-output-bytevector port)
  ;; R7RS says "It is an error if port was not created with
  ;; open-output-bytevector.", so we can safely assume that the port
  ;; was created by open-output-bytevector. -weinholt
  (seek port 0 SEEK_SET)
  (let ((bv (get-bytevector-all port)))
    (if (eof-object? bv)
        #vu8()
        bv)))

(define* (peek-u8 #:optional (port (current-input-port)))
  (lookahead-u8 port))

(define* (read-u8 #:optional (port (current-output-port)))
  (get-u8 port))

(define* (read-bytevector len #:optional (port (current-input-port)))
  (get-bytevector-n port len))

(define* (read-string len #:optional (port (current-input-port)))
  (get-string-n port len))

(define* (read-bytevector! bv #:optional (port (current-input-port))
                           (start 0) (end (bytevector-length bv)))
  (get-bytevector-n! port bv start (- end start)))

(define* (read-line #:optional (port (current-input-port)))
  (get-line port))

(define* (write-u8 obj #:optional (port (current-output-port)))
  (put-u8 port obj))

(define* (write-bytevector bv #:optional (port (current-output-port))
                           (start 0) (end (bytevector-length bv)))
  (put-bytevector port bv start (- end start)))

(define* (write-string str #:optional (port (current-output-port))
                       (start 0) (end (string-length str)))
  (put-string port str start (- end start)))

(define* (flush-output-port #:optional (port (current-output-port)))
  (force-output port))

(define (r7:string-map proc s . s*)
  (if (null? s*)
      (string-map proc s)
      (list->string (apply map proc (string->list s) (map string->list s*)))))

(define (bytevector . lis)
  (u8-list->bytevector lis))

(define (call-with-bytevector-output-port proc)
  (call-with-values (lambda () (open-bytevector-output-port))
    (lambda (port get)
      (proc port)
      (get))))

(define (bytevector-append . bvs)
  (call-with-bytevector-output-port
    (lambda (p)
      (for-each (lambda (bv) (put-bytevector p bv)) bvs))))

(define string->vector
  (case-lambda
    ((str) (list->vector (string->list str)))
    ((str start) (string->vector (substring str start)))
    ((str start end) (string->vector (substring str start end)))))

(define r7:string->utf8
  (case-lambda
    ((str) (string->utf8 str))
    ((str start) (string->utf8 (substring str start)))
    ((str start end) (string->utf8 (substring str start end)))))

;;; vector

(define r7:vector->list
  (case-lambda*
    ((v) (vector->list v))
    ((v start #:optional (end (vector-length v)))
     (vector->list (vector-copy v start end)))))

(define vector-map
  (case-lambda*
   ((f v)
    (let* ((len (vector-length v))
           (out (make-vector len #f)))
      (let lp ((i 0))
        (when (< i len)
          (vector-set! out i (f (vector-ref v i)))
          (lp (1+ i))))
      out))
   ((f v . v*)
    (list->vector (apply map f (map vector->list (cons v v*)))))))

(define vector-for-each
  (case-lambda*
   ((f v)
    (let lp ((i 0))
      (when (< i (vector-length v))
        (f (vector-ref v i))
        (lp (1+ i)))))
   ((f v . v*)
    (let ((len (apply min (vector-length v) (map vector-length v*))))
      (let lp ((i 0))
        (when (< i len)
          (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) v*))
          (lp (1+ i))))))))

(define (vector-append . vectors)
  (if (null? vectors)
      #()
      (let* ((len (let lp ((vectors vectors))
                    (if (null? vectors)
                        0
                        (+ (vector-length (car vectors)) (lp (cdr vectors))))))
             (out (make-vector len #f)))
        (let lp ((i 0) (j 0) (v (car vectors)) (v* (cdr vectors)))
          (cond
           ((< j (vector-length v))
            (vector-set! out i (vector-ref v j))
            (lp (1+ i) (1+ j) v v*))
           ((null? v*)
            out)
           (else
            (lp i 0 (car v*) (cdr v*))))))))

(define vector->string
  (case-lambda*
    ((v) (list->string (vector->list v)))
    ((v start #:optional (end (vector-length v)))
     (vector->string (vector-copy v start end)))))

(define (%subbytevector bv start end)
  (define mlen (- end start))
  (define out (make-bytevector mlen))
  (bytevector-copy! bv start out 0 mlen)
  out)

(define (%subbytevector1 bv start)
  (%subbytevector bv start (bytevector-length bv)))

(define r7:bytevector-copy!
  (case-lambda*
   ((to at from #:optional
        (start 0)
        (end (+ start
                (min (- (bytevector-length from) start)
                     (- (bytevector-length to) at)))))
    (bytevector-copy! from start to at (- end start)))))

(define r7:bytevector-copy
  (case-lambda*
    ((bv) (bytevector-copy bv))
    ((bv start #:optional (end (bytevector-length bv)))
     (%subbytevector bv start end))))

(define r7:utf8->string
  (case-lambda*
    ((bv) (utf8->string bv))
    ((bv start #:optional (end (bytevector-length bv)))
     (utf8->string (%subbytevector bv start end)))))

(define (square x) (* x x))

(define (r7:expt x y)
  (if (eqv? x 0.0)
      (exact->inexact (expt x y))
      (expt x y)))

(define (features)
  (append
   (case (native-endianness)
     ((big) '(big-endian))
     ((little) '(little-endian))
     (else '()))
   %cond-expand-features))

(define (input-port-open? port)
  (and (not (port-closed? port)) (input-port? port)))

(define (output-port-open? port)
  (and (not (port-closed? port)) (output-port? port)))
