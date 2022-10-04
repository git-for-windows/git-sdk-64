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

(define-module (scheme char)
  #:use-module ((srfi srfi-43) #:select (vector-binary-search))
  #:use-module (ice-9 i18n)
  #:export (char-foldcase
            string-foldcase
            digit-value)
  #:re-export (char-alphabetic?
               char-ci<=? char-ci<? char-ci=? char-ci>=?
               char-ci>? char-downcase char-lower-case?
               char-numeric? char-upcase char-upper-case? char-whitespace?
               string-ci<=? string-ci<? string-ci=?
               string-ci>=? string-ci>?
               (string-locale-downcase . string-downcase)
               (string-locale-upcase . string-upcase)))

(define (char-foldcase char)
    (if (or (eqv? char #\460) (eqv? char #\461))
	char
        (char-downcase (char-upcase char))))

(define (string-foldcase str)
  (string-locale-downcase (string-locale-upcase str)))

;; The table can be extracted with:
;; awk -F ';' '/ZERO;Nd/ {print "#x"$1}' UnicodeData.txt
;; Up to date with Unicode 11.0.0

(define *decimal-zeroes* '#(#x0030 #x0660 #x06F0 #x07C0 #x0966 #x09E6
  #x0A66 #x0AE6 #x0B66 #x0BE6 #x0C66 #x0CE6 #x0D66 #x0DE6 #x0E50
  #x0ED0 #x0F20 #x1040 #x1090 #x17E0 #x1810 #x1946 #x19D0 #x1A80
  #x1A90 #x1B50 #x1BB0 #x1C40 #x1C50 #xA620 #xA8D0 #xA900 #xA9D0
  #xA9F0 #xAA50 #xABF0 #xFF10 #x104A0 #x10D30 #x11066 #x110F0 #x11136
  #x111D0 #x112F0 #x11450 #x114D0 #x11650 #x116C0 #x11730 #x118E0
  #x11C50 #x11D50 #x11DA0 #x16A60 #x16B50 #x1D7CE #x1D7D8 #x1D7E2
  #x1D7EC #x1D7F6 #x1E950))

(define (digit-value char)
  (define (cmp zero ch)
    (if (integer? ch)
        (- (cmp zero ch))
        (let ((i (char->integer ch)))
          (cond ((< i zero) 1)
                ((> i (+ zero 9)) -1)
                (else 0)))))
  (unless (char? char)
    (error "Expected a char" char))
  (cond
    ((char<=? #\0 char #\9)             ;fast case
     (- (char->integer char) (char->integer #\0)))
    ((vector-binary-search *decimal-zeroes* char cmp)
     => (lambda (zero)
          (- (char->integer char)
             (vector-ref *decimal-zeroes* zero))))
    (else #f)))
