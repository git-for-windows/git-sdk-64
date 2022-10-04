;;; R7RS compatibility libraries
;;; Copyright (C) 2019 Free Software Foundation, Inc.
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

(define-module (scheme r5rs)
  #:use-module ((scheme base) #:select ((expt . r7:expt)))
  #:use-module ((ice-9 safe-r5rs) #:select (null-environment))
  #:use-module ((ice-9 r5rs) #:select (scheme-report-environment
                                       interaction-environment))
  #:re-export (quote
               quasiquote
               unquote unquote-splicing
               define-syntax let-syntax letrec-syntax
               define lambda let let* letrec begin do
               if set! delay and or
               syntax-rules _ ... else =>

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
               (r7:expt . expt)
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

               null-environment
               scheme-report-environment interaction-environment))
