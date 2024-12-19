;;;; -*- coding: utf-8; mode: scheme -*-
;;;;
;;;; 	Copyright (C) 2001, 2004, 2006, 2009, 2010,
;;;;      2012, 2013, 2014, 2023 Free Software Foundation, Inc.
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
  #:use-module (ice-9 soft-ports)
  #:use-module (ice-9 textual-ports)
  #:export (pretty-print
            truncated-print))

(define* (call-with-truncating-output-string proc success failure #:key
                                             (initial-column 0)
                                             (max-column 79)
                                             (allow-newline? #f))
  (define length 0)
  (define strs '())
  (define tag (make-prompt-tag))
  (define (write-string str)
    (set! length (+ length (string-length str)))
    (set! strs (cons str strs))
    (when (or (< (- max-column initial-column) length)
              (and (not allow-newline?)
                   (not (zero? (port-line port)))))
      (abort-to-prompt tag)))
  (define port
    (make-soft-port #:id "truncating-output-port"
                    #:write-string write-string))
  (call-with-prompt
   tag
   (lambda ()
     (proc port)
     (close port)
     (success (string-concatenate-reverse strs)))
   (lambda (_)
     (failure (string-concatenate-reverse strs)))))




;; Parts of pretty-print derived from "genwrite.scm", from SLIB.
;; Copyright (c) 1991, Marc Feeley
;; Author: Marc Feeley (feeley@iro.umontreal.ca)
;; Distribution restrictions: none

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
  (define (wr obj port)
    (define (wr-read-macro prefix x)
      (put-string port prefix)
      (wr x port))
    (match obj
      (('quote x)            (wr-read-macro "'" x))
      (('quasiquote x)       (wr-read-macro "`" x))
      (('unquote x)          (wr-read-macro "," x))
      (('unquote-splicing x) (wr-read-macro ",@" x))
      ((head . (rest ...))
       ;; A proper list: do our own list printing so as to catch read
       ;; macros that appear in the middle of the list.
       (put-string port "(")
       (wr head port)
       (for-each (lambda (x)
                   (put-string port " ")
                   (wr x port))
                 rest)
       (put-string port ")"))
      (_
       ((if display? display write) obj port))))

                                        ; define formatting style (change these to suit your style)
  (define indent-general 2)
  (define max-call-head-width 5)

  (define (spaces n)
    (when (< 0 n)
      (put-string port "        " 0 (min 8 n))
      (when (< 8 n)
        (spaces (- n 8)))))

  (define (indent to)
    (let ((col (port-column port)))
      (cond
       ((< to col)
        (put-string port "\n")
        (put-string port per-line-prefix)
        (spaces (- to (string-length per-line-prefix))))
       (else
        (spaces (- to col))))))

  (define (pr obj pp-pair)
    (match obj
      ((? vector?)
       (put-string port "#")
       (pr (vector->list obj) pp-pair))
      ((not (? pair?))
       (wr obj port))
      (('quote x)            (put-string port "'") (pr x pp-pair))
      (('quasiquote x)       (put-string port "`") (pr x pp-pair))
      (('unquote x)          (put-string port ",") (pr x pp-pair))
      (('unquote-splicing x) (put-string port ",@") (pr x pp-pair))
      (_
       ;; A pair (and possibly a list).  May have to split on multiple
       ;; lines.
       (call-with-truncating-output-string
        (lambda (port) (wr obj port))
        (lambda (full-str) (put-string port full-str))
        (lambda (partial-str) (pp-pair obj))
        #:initial-column (port-column port)
        #:max-column (- width (string-length per-line-prefix))
        #:allow-newline? #f))))

  (define (pp-expr expr)
    (match expr
      (((or 'quote 'quasiquote 'unquote 'unquote-splicing) _)
       (pp-quote expr))
      (('lambda _ _ . _)         (pp-lambda expr))
      (('lambda* _ _ . _)        (pp-lambda expr))
      (('let (? symbol?) _ _ . _) (pp-named-let expr))
      (('let _ _ . _)            (pp-let expr))
      (('let* _ _ . _)           (pp-let expr))
      (('letrec _ _ . _)         (pp-let expr))
      (('letrec* _ _ . _)        (pp-let expr))
      (('let-syntax _ _ . _)     (pp-let expr))
      (('letrec-syntax _ _ . _)  (pp-let expr))
      (('define _ _ . _)         (pp-define expr))
      (('define* _ _ . _)        (pp-define expr))
      (('define-public _ _ . _)  (pp-define expr))
      (('define-syntax _ _ . _)  (pp-define expr))
      (('if _ _ . (or () (_)))   (pp-if expr))
      (('cond . _)               (pp-cond expr))
      (('case _ . _)             (pp-case expr))
      (('begin . _)              (pp-begin expr))
      (('do _ _ . _)             (pp-do expr))
      (('syntax-rules _ . _)     (pp-syntax-rules expr))
      (('syntax-case _ _ . _)    (pp-syntax-case expr))
      (((? symbol? head) . _)
       (if (< max-call-head-width (string-length (symbol->string head)))
           (pp-list expr pp-expr)
           (pp-call expr pp-expr)))
      (_ (pp-list expr pp-expr))))

  (define (pp0 head body)
    (let ((body-col (+ (port-column port) indent-general)))
      (put-string port "(")
      (wr head port)
      (pp-down body body-col pp-expr)))

  (define (pp1 head param0 body pp-param0)
    (let ((body-col (+ (port-column port) indent-general)))
      (put-string port "(")
      (wr head port)
      (put-string port " ")
      (pr param0 pp-param0)
      (pp-down body body-col pp-expr)))

  (define (pp2 head param0 param1 body pp-param0 pp-param1)
    (let ((body-col (+ (port-column port) indent-general)))
      (put-string port "(")
      (wr head port)
      (put-string port " ")
      (pr param0 pp-param0)
      (put-string port " ")
      (pr param1 pp-param1)
      (pp-down body body-col pp-expr)))

  (define (pp-quote expr)
    (match obj
      ((head x)
       (put-string port
                   (match x
                     ('quote "'")
                     ('quasiquote "`")
                     ('unquote ",")
                     ('unquote-splicing ",@")))
       (pr x pp-expr))))

  (define (pp-lambda expr)
    (match expr
      ((head args . body)
       (pp1 head args body pp-expr-list))))

  (define (pp-let expr)
    (match expr
      ((head bindings . body)
       (pp1 head bindings body pp-expr-list))))

  (define (pp-named-let expr)
    (match expr
      ((head name bindings . body)
       (pp2 head name bindings body pp-expr pp-expr-list))))

  (define (pp-define expr)
    (match expr
      ((head args . body)
       (pp1 head args body pp-expr-list))))

  (define (pp-if expr)
    (match expr
      ((head test . body)
       ;; "if" indent is 4.
       (put-string port "(")
       (wr head port)
       (put-string port " ")
       (let ((body-col (port-column port)))
         (pr test pp-expr)
         (pp-down body body-col pp-expr)))))

  (define (pp-cond expr)
    (match expr
      ((head . clauses)
       (pp0 head clauses))))

  (define (pp-case expr)
    (match expr
      ((head x . clauses)
       (pp1 head x clauses pp-expr))))

  (define (pp-begin expr)
    (match expr
      ((head . body) (pp0 head body))))

  (define (pp-do expr)
    (match expr
      ((head bindings exit . body)
       (pp2 head bindings exit body pp-expr-list pp-expr-list))))

  (define (pp-syntax-rules expr)
    (match expr
      ((head literals . clauses)
       (pp1 head literals clauses pp-expr-list))))

  (define (pp-syntax-case expr)
    (match expr
      ((head stx literals . clauses)
       (pp2 head stx literals clauses pp-expr pp-expr-list))))

                                        ; (head item1
                                        ;       item2
                                        ;       item3)
  (define (pp-call expr pp-item)
    (match expr
      ((head . tail)
       (put-string port "(")
       (wr head port)
       (pp-down tail (+ (port-column port) 1) pp-item))))

                                        ; (item1
                                        ;  item2
                                        ;  item3)
  (define (pp-list l pp-item)
    (put-string port "(")
    (pp-down l (port-column port) pp-item))

  (define (pp-down l item-indent pp-item)
    (let loop ((l l))
      (match l
        (() (put-string port ")"))
        ((head . tail)
         (indent item-indent)
         (pr head pp-item)
         (loop tail))
        (improper-tail
         (indent item-indent)
         (put-string port ".")
         (indent item-indent)
         (pr improper-tail pp-item)
         (put-string port ")")))))

  (define (pp-expr-list l)
    (pp-list l pp-expr))

  (put-string port per-line-prefix)
  (pr obj pp-expr)
  (newline port)
  ;; Return `unspecified'
  (if #f #f))




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

The default behavior is to print depth-first, meaning that the entire
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
      (unless (< width (string-length str))
        (error "precondition failed"))
      (or (or-map (match-lambda
                    ((prefix . suffix)
                     (and (string-prefix? prefix str)
                          (<= (+ (string-length prefix)
                                 (string-length suffix)
                                 ellipsis-width)
                              width)
                          (format #f "~a~a~a"
                                  (substring str 0
                                             (- width (string-length suffix)
                                                ellipsis-width))
                                  ellipsis
                                  suffix))))
                  '(("#<" . ">")
                    ("#(" . ")")
                    ("(" . ")")
                    ("\"" . "\"")))
          "#"))

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
          (array-for-each (lambda (xi) (display (if xi "1" "0")))
                          (make-shared-array x list (- width 2 ellipsis-width)))
          (display ellipsis))
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
        (call-with-truncating-output-string
         (lambda (port)
           (if display? (display x port) (write x port)))
         (lambda (full-str)
           (display full-str))
         (lambda (partial-str)
           (display (truncate-string partial-str width)))
         #:max-column width
         #:allow-newline? #f))))

    (with-output-to-port port
      (lambda ()
        (print x width)))))
