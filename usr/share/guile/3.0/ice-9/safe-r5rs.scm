;;;; Copyright (C) 2000-2001,2004,2006,2008-2010,2019
;;;;   Free Software Foundation, Inc.
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

;;;; Safe subset of R5RS bindings

(define-module (ice-9 safe-r5rs)
  #:pure
  #:use-module ((guile) #:hide (case cond syntax-rules _ => else ...))
  #:use-module (ice-9 ports)
  #:use-module ((guile) #:select ((_ . ^_)
                                  (... . ^...)))
  #:re-export (quote
               quasiquote
               unquote unquote-splicing
               define-syntax let-syntax letrec-syntax
               define lambda let let* letrec begin do
               if set! delay and or

               eqv? eq? equal?
               number? complex? real? rational? integer?
               exact? inexact?
               = < > <= >=
               zero? positive? negative? odd? even?
               max min
               + * - /
               abs
               quotient remainder modulo
               gcd lcm
               numerator denominator
               rationalize
               floor ceiling truncate round
               exp log sin cos tan asin acos atan
               sqrt
               expt
               make-rectangular make-polar real-part imag-part magnitude angle
               exact->inexact inexact->exact

               number->string string->number

               boolean?
               not

               pair?
               cons car cdr
               set-car! set-cdr!
               caar cadr cdar cddr
               caaar caadr cadar caddr cdaar cdadr cddar cdddr
               caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
               cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
               null?
               list?
               list
               length
               append
               reverse
               list-tail list-ref
               memq memv member
               assq assv assoc

               symbol?
               symbol->string string->symbol

               char?
               char=? char<? char>? char<=? char>=?
               char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
               char-alphabetic? char-numeric? char-whitespace?
               char-upper-case? char-lower-case?
               char->integer integer->char
               char-upcase
               char-downcase

               string?
               make-string
               string
               string-length
               string-ref string-set!
               string=? string-ci=?
               string<? string>? string<=? string>=?
               string-ci<? string-ci>? string-ci<=? string-ci>=?
               substring
               string-length
               string-append
               string->list list->string
               string-copy string-fill!

               vector?
               make-vector
               vector
               vector-length
               vector-ref vector-set!
               vector->list list->vector
               vector-fill!

               procedure?
               apply
               map
               for-each
               force

               call-with-current-continuation

               values
               call-with-values
               dynamic-wind

               eval

               input-port? output-port?
               current-input-port current-output-port

               read
               read-char
               peek-char
               eof-object?
               char-ready?

               write
               display
               newline
               write-char

               ;;transcript-on
               ;;transcript-off
               )

  #:export (null-environment
            syntax-rules cond case))

;;; These definitions of `cond', `case', and `syntax-rules' differ from
;;; the ones in Guile in that they expect their auxiliary syntax (`_',
;;; `...', `else', and `=>') to be unbound.  They also don't support
;;; some extensions from Guile (e.g. `=>' in `case'.).

(define-syntax syntax-rules
  (lambda (x)
    (define (replace-underscores pattern)
      (syntax-case pattern (_)
        (_ #'^_)
        ((x . y)
         (with-syntax ((x (replace-underscores #'x))
                       (y (replace-underscores #'y)))
           #'(x . y)))
        ((x . y)
         (with-syntax ((x (replace-underscores #'x))
                       (y (replace-underscores #'y)))
           #'(x . y)))
        (#(x ^...)
         (with-syntax (((x ^...) (map replace-underscores #'(x ^...))))
           #'#(x ^...)))
        (x #'x)))
    (syntax-case x ()
      ((^_ dots (k ^...) . clauses)
       (identifier? #'dots)
       #'(with-ellipsis dots (syntax-rules (k ^...) . clauses)))
      ((^_ (k ^...) ((keyword . pattern) template) ^...)
       (with-syntax (((pattern ^...) (replace-underscores #'(pattern ^...))))
         #`(lambda (x)
             (syntax-case x (k ^...)
               ((dummy . pattern) #'template)
               ^...)))))))

(define-syntax case
  (lambda (stx)
    (let lp ((stx stx))
      (syntax-case stx (else)
        (("case" x)
         #'(if #f #f))
        (("case" x ((y ^...) expr ^...) clause ^...)
         #`(if (memv x '(y ^...))
               (begin expr ^...)
               #,(lp #'("case" x clause ^...))))
        (("case" x (else expr ^...))
         #'(begin expr ^...))
        (("case" x clause . ^_)
         (syntax-violation 'case "bad 'case' clause" #'clause))
        ((^_ x clause clause* ^...)
         #`(let ((t x))
             #,(lp #'("case" t clause clause* ^...))))))))

(define-syntax cond
  (lambda (stx)
    (let lp ((stx stx))
      (syntax-case stx (else =>)
        (("cond")
         #'(if #f #f))
        (("cond" (else expr ^...))
         #'(begin expr ^...))
        (("cond" (test => expr) clause ^...)
         #`(let ((t test))
             (if t
                 (expr t)
                 #,(lp #'("cond" clause ^...)))))
        (("cond" (test) clause ^...)
         #`(or test #,(lp #'("cond" clause ^...))))
        (("cond" (test expr ^...) clause ^...)
         #`(if test
               (begin expr ^...)
               #,(lp #'("cond" clause ^...))))
        (("cond" clause . ^_)
         (syntax-violation 'cond "bad 'cond' clause" #'clause))
        ((^_ clause clause* ^...)
         (lp #'("cond" clause clause* ^...)))))))

(define (null-environment n)
  (unless (eqv? n 5)
    (scm-error 'misc-error 'null-environment
               "~A is not a valid version" (list n) '()))
  ;; Note that we need to create a *fresh* interface
  (let ((interface (make-module)))
    (set-module-kind! interface 'interface)
    (define bindings
      '(define quote lambda if set! cond case and or let let* letrec
	 begin do delay quasiquote unquote
         define-syntax let-syntax letrec-syntax syntax-rules))
    (module-use! interface
                 (resolve-interface '(ice-9 safe-r5rs) #:select bindings))
    interface))
