;;;; -*- coding: utf-8; mode: scheme -*-
;;;;
;;;; 	Copyright (C) 2001, 2004, 2006, 2009, 2010,
;;;;      2012, 2013, 2014 Free Software Foundation, Inc.
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
(define-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:export (pretty-print
            truncated-print))


;; From SLIB.

;;"genwrite.scm" generic write used by pretty-print and truncated-print.
;; Copyright (c) 1991, Marc Feeley
;; Author: Marc Feeley (feeley@iro.umontreal.ca)
;; Distribution restrictions: none

(define genwrite:newline-str (make-string 1 #\newline))

(define (generic-write
         obj display? width max-expr-width per-line-prefix output)

  (define (read-macro? l)
    (define (length1? l) (and (pair? l) (null? (cdr l))))
    (let ((head (car l)) (tail (cdr l)))
      (case head
        ((quote quasiquote unquote unquote-splicing) (length1? tail))
        (else                                        #f))))

  (define (read-macro-body l)
    (cadr l))

  (define (read-macro-prefix l)
    (let ((head (car l)))
      (case head
        ((quote)            "'")
        ((quasiquote)       "`")
        ((unquote)          ",")
        ((unquote-splicing) ",@"))))

  (define (out str col)
    (and col (output str) (+ col (string-length str))))

  (define (wr obj col)
    (let loop ((obj obj)
               (col col))
      (match obj
        (((or 'quote 'quasiquote 'unquote 'unquote-splicing) body)
         (wr body (out (read-macro-prefix obj) col)))
        ((head . (rest ...))
         ;; A proper list: do our own list printing so as to catch read
         ;; macros that appear in the middle of the list.
         (let ((col (loop head (out "(" col))))
           (out ")"
                (fold (lambda (i col)
                        (loop i (out " " col)))
                      col rest))))
        (_
         (out (object->string obj (if display? display write)) col)))))

  (define (pp obj col)

    (define (spaces n col)
      (if (> n 0)
        (if (> n 7)
          (spaces (- n 8) (out "        " col))
          (out (substring "        " 0 n) col))
        col))

    (define (indent to col)
      (and col
           (if (< to col)
             (and (out genwrite:newline-str col)
		  (out per-line-prefix 0)
		  (spaces to 0))
             (spaces (- to col) col))))

    (define (pr obj col extra pp-pair)
      (if (or (pair? obj) (vector? obj)) ; may have to split on multiple lines
        (let ((result '())
              (left (min (+ (- (- width col) extra) 1) max-expr-width)))
          (generic-write obj display? #f max-expr-width ""
            (lambda (str)
              (set! result (cons str result))
              (set! left (- left (string-length str)))
              (> left 0)))
          (if (> left 0) ; all can be printed on one line
            (out (reverse-string-append result) col)
            (if (pair? obj)
              (pp-pair obj col extra)
              (pp-list (vector->list obj) (out "#" col) extra pp-expr))))
        (wr obj col)))

    (define (pp-expr expr col extra)
      (if (read-macro? expr)
        (pr (read-macro-body expr)
            (out (read-macro-prefix expr) col)
            extra
            pp-expr)
        (let ((head (car expr)))
          (if (symbol? head)
            (let ((proc (style head)))
              (if proc
                (proc expr col extra)
                (if (> (string-length (symbol->string head))
                       max-call-head-width)
                  (pp-general expr col extra #f #f #f pp-expr)
                  (pp-call expr col extra pp-expr))))
            (pp-list expr col extra pp-expr)))))

    ; (head item1
    ;       item2
    ;       item3)
    (define (pp-call expr col extra pp-item)
      (let ((col* (wr (car expr) (out "(" col))))
        (and col
             (pp-down (cdr expr) col* (+ col* 1) extra pp-item))))

    ; (item1
    ;  item2
    ;  item3)
    (define (pp-list l col extra pp-item)
      (let ((col (out "(" col)))
        (pp-down l col col extra pp-item)))

    (define (pp-down l col1 col2 extra pp-item)
      (let loop ((l l) (col col1))
        (and col
             (cond ((pair? l)
                    (let ((rest (cdr l)))
                      (let ((extra (if (null? rest) (+ extra 1) 0)))
                        (loop rest
                              (pr (car l) (indent col2 col) extra pp-item)))))
                   ((null? l)
                    (out ")" col))
                   (else
                    (out ")"
                         (pr l
                             (indent col2 (out "." (indent col2 col)))
                             (+ extra 1)
                             pp-item)))))))

    (define (pp-general expr col extra named? pp-1 pp-2 pp-3)

      (define (tail1 rest col1 col2 col3)
        (if (and pp-1 (pair? rest))
          (let* ((val1 (car rest))
                 (rest (cdr rest))
                 (extra (if (null? rest) (+ extra 1) 0)))
            (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
          (tail2 rest col1 col2 col3)))

      (define (tail2 rest col1 col2 col3)
        (if (and pp-2 (pair? rest))
          (let* ((val1 (car rest))
                 (rest (cdr rest))
                 (extra (if (null? rest) (+ extra 1) 0)))
            (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2)))
          (tail3 rest col1 col2)))

      (define (tail3 rest col1 col2)
        (pp-down rest col2 col1 extra pp-3))

      (let* ((head (car expr))
             (rest (cdr expr))
             (col* (wr head (out "(" col))))
        (if (and named? (pair? rest))
          (let* ((name (car rest))
                 (rest (cdr rest))
                 (col** (wr name (out " " col*))))
            (tail1 rest (+ col indent-general) col** (+ col** 1)))
          (tail1 rest (+ col indent-general) col* (+ col* 1)))))

    (define (pp-expr-list l col extra)
      (pp-list l col extra pp-expr))

    (define (pp-LAMBDA expr col extra)
      (pp-general expr col extra #f pp-expr-list #f pp-expr))

    (define (pp-IF expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr))

    (define (pp-COND expr col extra)
      (pp-call expr col extra pp-expr-list))

    (define (pp-CASE expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr-list))

    (define (pp-AND expr col extra)
      (pp-call expr col extra pp-expr))

    (define (pp-LET expr col extra)
      (let* ((rest (cdr expr))
             (named? (and (pair? rest) (symbol? (car rest)))))
        (pp-general expr col extra named? pp-expr-list #f pp-expr)))

    (define (pp-BEGIN expr col extra)
      (pp-general expr col extra #f #f #f pp-expr))

    (define (pp-DO expr col extra)
      (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

    (define (pp-SYNTAX-CASE expr col extra)
      (pp-general expr col extra #t pp-expr-list #f pp-expr))

    ; define formatting style (change these to suit your style)

    (define indent-general 2)

    (define max-call-head-width 5)

    (define (style head)
      (case head
        ((lambda lambda* let* letrec define define* define-public
                 define-syntax let-syntax letrec-syntax with-syntax)
                                     pp-LAMBDA)
        ((if set!)                   pp-IF)
        ((cond)                      pp-COND)
        ((case)                      pp-CASE)
        ((and or)                    pp-AND)
        ((let)                       pp-LET)
        ((begin)                     pp-BEGIN)
        ((do)                        pp-DO)
        ((syntax-rules)              pp-LAMBDA)
        ((syntax-case)               pp-SYNTAX-CASE)
        (else                        #f)))

    (pr obj col 0 pp-expr))

  (out per-line-prefix 0)
  (if width
    (out genwrite:newline-str (pp obj 0))
    (wr obj 0))
  ;; Return `unspecified'
  (if #f #f))

; (reverse-string-append l) = (apply string-append (reverse l))

(define (reverse-string-append l)

  (define (rev-string-append l i)
    (if (pair? l)
      (let* ((str (car l))
             (len (string-length str))
             (result (rev-string-append (cdr l) (+ i len))))
        (let loop ((j 0) (k (- (- (string-length result) i) len)))
          (if (< j len)
            (begin
              (string-set! result k (string-ref str j))
              (loop (+ j 1) (+ k 1)))
            result)))
      (make-string i)))

  (rev-string-append l 0))

(define* (pretty-print obj #:optional port*
                       #:key 
                       (port (or port* (current-output-port)))
                       (width 79)
                       (max-expr-width 50)
                       (display? #f)
                       (per-line-prefix ""))
  "Pretty-print OBJ on PORT, which is a keyword argument defaulting to
the current output port.  Formatting can be controlled by a number of
keyword arguments: Each line in the output is preceded by the string
PER-LINE-PREFIX, which is empty by default.  The output lines will be
at most WIDTH characters wide; the default is 79.  If DISPLAY? is
true, display rather than write representation will be used.

Instead of with a keyword argument, you can also specify the output
port directly after OBJ, like (pretty-print OBJ PORT)."
  (generic-write obj display?
		 (- width (string-length per-line-prefix))
                 max-expr-width
		 per-line-prefix
		 (lambda (s) (display s port) #t)))


;; `truncated-print' was written in 2009 by Andy Wingo, and is not from
;; genwrite.scm.
(define* (truncated-print x #:optional port*
                          #:key
                          (port (or port* (current-output-port)))
                          (width 79)
                          (display? #f)
                          (breadth-first? #f))
  "Print @var{x}, truncating the output, if necessary, to make it fit
into @var{width} characters. By default, @var{x} will be printed using
@code{write}, though that behavior can be overriden via the
@var{display?} keyword argument.

The default behaviour is to print depth-first, meaning that the entire
remaining width will be available to each sub-expression of @var{x} --
e.g., if @var{x} is a vector, each member of @var{x}. One can attempt to
\"ration\" the available width, trying to allocate it equally to each
sub-expression, via the @var{breadth-first?} keyword argument."

  (define ellipsis
    ;; Choose between `HORIZONTAL ELLIPSIS' (U+2026) and three dots, depending
    ;; on the encoding of PORT.
    (let ((e "…"))
      (catch 'encoding-error
        (lambda ()
          (with-fluids ((%default-port-conversion-strategy 'error))
            (call-with-output-string
             (lambda (p)
               (set-port-encoding! p (port-encoding port))
               (display e p)))))
        (lambda (key . args)
          "..."))))

  (let ((ellipsis-width (string-length ellipsis)))

    (define* (print-sequence x width len ref next #:key inner?)
      (let lp ((x x)
               (width width)
               (i 0))
        (if (> i 0)
            (display #\space))
        (cond
         ((= i len)) ; catches 0-length case
         ((and (= i (1- len)) (or (zero? i) (> width 1)))
          (print (ref x i) (if (zero? i) width (1- width)) #:inner? inner?))
         ((<= width (+ 1 ellipsis-width))
          (display ellipsis))
         (else
          (let ((str (with-output-to-string
                       (lambda ()
                         (print (ref x i)
                                (if breadth-first?
                                    (max 1
                                         (1- (floor (/ width (- len i)))))
                                    (- width (+ 1 ellipsis-width)))
                                #:inner? inner?)))))
            (display str)
            (lp (next x) (- width 1 (string-length str)) (1+ i)))))))

    (define (print-tree x width)
      ;; width is >= the width of # . #, which is 5
      (let lp ((x x)
               (width width))
        (cond
         ((or (not (pair? x)) (<= width 4))
          (display ". ")
          (print x (- width 2)))
         (else
          ;; width >= 5
          (let ((str (with-output-to-string
                       (lambda ()
                         (print (car x)
                                (if breadth-first?
                                    (floor (/ (- width 3) 2))
                                    (- width 4)))))))
            (display str)
            (display " ")
            (lp (cdr x) (- width 1 (string-length str))))))))

    (define (truncate-string str width)
      ;; width is < (string-length str)
      (let lp ((fixes '(("#<" . ">")
                        ("#(" . ")")
                        ("(" . ")")
                        ("\"" . "\""))))
        (cond
         ((null? fixes)
          "#")
         ((and (string-prefix? (caar fixes) str)
               (string-suffix? (cdar fixes) str)
               (>= (string-length str)
                   width
                   (+ (string-length (caar fixes))
                      (string-length (cdar fixes))
                      ellipsis-width)))
          (format #f "~a~a~a~a"
                  (caar fixes)
                  (substring str (string-length (caar fixes))
                             (- width (string-length (cdar fixes))
                                ellipsis-width))
                  ellipsis
                  (cdar fixes)))
         (else
          (lp (cdr fixes))))))

    (define* (print x width #:key inner?)
      (cond
       ((<= width 0)
        (error "expected a positive width" width))
       ((list? x)
        (cond
         ((>= width (+ 2 ellipsis-width))
          (display "(")
          (print-sequence x (- width 2) (length x)
                          (lambda (x i) (car x)) cdr)
          (display ")"))
         (else
          (display "#"))))
       ((vector? x)
        (cond
         ((>= width (+ 3 ellipsis-width))
          (display "#(")
          (print-sequence x (- width 3) (vector-length x)
                          vector-ref identity)
          (display ")"))
         (else
          (display "#"))))
       ((bytevector? x)
        (cond
         ((>= width 9)
          (format #t  "#~a(" (array-type x))
          (print-sequence x (- width 6) (array-length x)
                          array-ref identity)
          (display ")"))
         (else
          (display "#"))))
       ((bitvector? x)
        (cond
         ((>= width (+ 2 (array-length x)))
          (format #t "~a" x))
         ;; the truncated bitvector would print as #1b(...), so we print by hand.
         ((>= width (+ 2 ellipsis-width))
          (format #t "#*")
          (array-for-each (lambda (xi) (format #t (if xi "1" "0")))
                          (make-shared-array x list (- width 2 ellipsis-width)))
          (format #t ellipsis))
         (else
          (display "#"))))
       ((and (array? x) (not (string? x)))
        (let* ((type (array-type x))
               (prefix
                (if inner?
                  ""
                  (call-with-output-string
                   (lambda (s) ((@@ (ice-9 arrays) array-print-prefix) x s)))))
               (width-prefix (string-length prefix)))
          (cond
           ((>= width (+ 2 width-prefix ellipsis-width))
            (format #t  "~a(" prefix)
            (if (zero? (array-rank x))
              (print (array-ref x) (- width width-prefix 2))
              (print-sequence x (- width width-prefix 2) (array-length x)
                              (let ((base (caar (array-shape x))))
                                (lambda (x i) (array-cell-ref x (+ base i))))
                              identity
                              #:inner? (< 1 (array-rank x))))
            (display ")"))
           (else
            (display "#")))))
       ((pair? x)
        (cond
         ((>= width (+ 4 ellipsis-width))
          (display "(")
          (print-tree x (- width 2))
          (display ")"))
         (else
          (display "#"))))
       (else
        (let* ((str (with-output-to-string
                      (lambda () (if display? (display x) (write x)))))
               (len (string-length str)))
          (display (if (<= (string-length str) width)
                       str
                       (truncate-string str width)))))))

    (with-output-to-port port
      (lambda ()
        (print x width)))))
