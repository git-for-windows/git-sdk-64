;;;; i18n.scm --- internationalization support    -*- coding: utf-8 -*-

;;;;	Copyright (C) 2006, 2007, 2009, 2010, 2012 Free Software Foundation, Inc.
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

;;; Author: Ludovic Courtès <ludo@gnu.org>

;;; Commentary:
;;;
;;; This module provides a number of routines that support
;;; internationalization (e.g., locale-dependent text collation, character
;;; mapping, etc.).  It also defines `locale' objects, representing locale
;;; settings, that may be passed around to most of these procedures.
;;;

;;; Code:

(define-module (ice-9 i18n)
  :use-module (ice-9 optargs)
  :export (;; `locale' type
           make-locale locale?
           %global-locale

           ;; text collation
           string-locale<? string-locale>?
           string-locale-ci<? string-locale-ci>? string-locale-ci=?

           char-locale<? char-locale>?
           char-locale-ci<? char-locale-ci>? char-locale-ci=?

           ;; character mapping
           char-locale-downcase char-locale-upcase char-locale-titlecase
           string-locale-downcase string-locale-upcase string-locale-titlecase

           ;; reading numbers
           locale-string->integer locale-string->inexact

           ;; charset/encoding
           locale-encoding

           ;; days and months
           locale-day-short locale-day locale-month-short locale-month

           ;; date and time
           locale-am-string locale-pm-string
           locale-date+time-format locale-date-format locale-time-format
           locale-time+am/pm-format
           locale-era locale-era-year
           locale-era-date-format locale-era-date+time-format
           locale-era-time-format

           ;; monetary
           locale-currency-symbol
           locale-monetary-decimal-point locale-monetary-thousands-separator
           locale-monetary-grouping locale-monetary-fractional-digits
           locale-currency-symbol-precedes-positive?
           locale-currency-symbol-precedes-negative?
           locale-positive-separated-by-space?
           locale-negative-separated-by-space?
           locale-monetary-positive-sign locale-monetary-negative-sign
           locale-positive-sign-position locale-negative-sign-position
           monetary-amount->locale-string

           ;; number formatting
           locale-digit-grouping locale-decimal-point
           locale-thousands-separator
           number->locale-string

           ;; miscellaneous
           locale-yes-regexp locale-no-regexp))


(eval-when (expand load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_i18n"))


;;;
;;; Charset/encoding.
;;;

(define (locale-encoding . locale)
  (apply nl-langinfo CODESET locale))


;;;
;;; Months and days.
;;;

;; Helper macro: Define a procedure named NAME that maps its argument to
;; NL-ITEMS.  Gnulib guarantees that these items are available.
(define-macro (define-vector-langinfo-mapping name nl-items)
  (let* ((item-count (length nl-items))
         (defines   `(define %nl-items (vector #f ,@nl-items)))
         (make-body (lambda (result)
                      `(if (and (integer? item) (exact? item))
                           (if (and (>= item 1) (<= item ,item-count))
                               ,result
                               (throw 'out-of-range "out of range" item))
                           (throw 'wrong-type-arg "wrong argument type" item)))))
    `(define (,name item . locale)
       ,defines
       ,(make-body '(apply nl-langinfo (vector-ref %nl-items item) locale)))))


(define-vector-langinfo-mapping locale-day-short
  (ABDAY_1 ABDAY_2 ABDAY_3 ABDAY_4 ABDAY_5 ABDAY_6 ABDAY_7))

(define-vector-langinfo-mapping locale-day
  (DAY_1 DAY_2 DAY_3 DAY_4 DAY_5 DAY_6 DAY_7))

(define-vector-langinfo-mapping locale-month-short
  (ABMON_1 ABMON_2 ABMON_3 ABMON_4 ABMON_5 ABMON_6
   ABMON_7 ABMON_8 ABMON_9 ABMON_10 ABMON_11 ABMON_12))

(define-vector-langinfo-mapping locale-month
  (MON_1 MON_2 MON_3 MON_4 MON_5 MON_6 MON_7 MON_8 MON_9 MON_10 MON_11 MON_12))



;;;
;;; Date and time.
;;;

;; Define a procedure NAME that gets langinfo item ITEM.  Gnulib's
;; `nl_langinfo' does not guarantee that all these items are supported
;; (for instance, `GROUPING' is lacking on Darwin and Gnulib provides no
;; replacement), so use DEFAULT as the default value when ITEM is not
;; available.
(define-macro (define-simple-langinfo-mapping name item default)
  (let ((body (if (defined? item)
                  `(apply nl-langinfo ,item locale)
                  default)))
    `(define (,name . locale)
       ,body)))

(define-simple-langinfo-mapping locale-am-string
  AM_STR "AM")
(define-simple-langinfo-mapping locale-pm-string
  PM_STR "PM")
(define-simple-langinfo-mapping locale-date+time-format
  D_T_FMT "%a %b %e %H:%M:%S %Y")
(define-simple-langinfo-mapping locale-date-format
  D_FMT   "%m/%d/%y")
(define-simple-langinfo-mapping locale-time-format
  T_FMT   "%H:%M:%S")
(define-simple-langinfo-mapping locale-time+am/pm-format
  T_FMT_AMPM "%I:%M:%S %p")
(define-simple-langinfo-mapping locale-era
  ERA        "")
(define-simple-langinfo-mapping locale-era-year
  ERA_YEAR   "")
(define-simple-langinfo-mapping locale-era-date+time-format
  ERA_D_T_FMT "")
(define-simple-langinfo-mapping locale-era-date-format
  ERA_D_FMT   "")
(define-simple-langinfo-mapping locale-era-time-format
  ERA_T_FMT   "")



;;;
;;; Monetary information.
;;;

;; Define a procedure NAME that gets item LOCAL-ITEM or INTL-ITEM,
;; depending on whether the caller asked for the international version
;; or not.  Since Gnulib's `nl_langinfo' module doesn't guarantee that
;; all these items are available, use DEFAULT/LOCAL and DEFAULT/INTL as
;; default values when the system does not support them.
(define-macro (define-monetary-langinfo-mapping name local-item intl-item
                                                default/local default/intl)
  (let ((body
         (let ((intl  (if (defined? intl-item)
                          `(apply nl-langinfo ,intl-item locale)
                          default/intl))
               (local (if (defined? local-item)
                          `(apply nl-langinfo ,local-item locale)
                          default/local)))
           `(if intl? ,intl ,local))))

    `(define (,name intl? . locale)
       ,body)))

;; FIXME: How can we use ALT_DIGITS?
(define-monetary-langinfo-mapping locale-currency-symbol
  CRNCYSTR           INT_CURR_SYMBOL
  "-"                "")
(define-monetary-langinfo-mapping locale-monetary-fractional-digits
  FRAC_DIGITS        INT_FRAC_DIGITS
  2                  2)

(define-simple-langinfo-mapping locale-monetary-positive-sign
  POSITIVE_SIGN        "+")
(define-simple-langinfo-mapping locale-monetary-negative-sign
  NEGATIVE_SIGN        "-")
(define-simple-langinfo-mapping locale-monetary-decimal-point
  MON_DECIMAL_POINT    "")
(define-simple-langinfo-mapping locale-monetary-thousands-separator
  MON_THOUSANDS_SEP    "")
(define-simple-langinfo-mapping locale-monetary-digit-grouping
  MON_GROUPING         '())

(define-monetary-langinfo-mapping locale-currency-symbol-precedes-positive?
  P_CS_PRECEDES       INT_P_CS_PRECEDES
  #t                  #t)
(define-monetary-langinfo-mapping locale-currency-symbol-precedes-negative?
  N_CS_PRECEDES       INT_N_CS_PRECEDES
  #t                  #t)


(define-monetary-langinfo-mapping locale-positive-separated-by-space?
  ;; Whether a space should be inserted between a positive amount and the
  ;; currency symbol.
  P_SEP_BY_SPACE      INT_P_SEP_BY_SPACE
  #t                  #t)
(define-monetary-langinfo-mapping locale-negative-separated-by-space?
  ;; Whether a space should be inserted between a negative amount and the
  ;; currency symbol.
  N_SEP_BY_SPACE      INT_N_SEP_BY_SPACE
  #t                  #t)

(define-monetary-langinfo-mapping locale-positive-sign-position
  ;; Position of the positive sign wrt. currency symbol and quantity in a
  ;; monetary amount.
  P_SIGN_POSN         INT_P_SIGN_POSN
  'unspecified        'unspecified)
(define-monetary-langinfo-mapping locale-negative-sign-position
  ;; Position of the negative sign wrt. currency symbol and quantity in a
  ;; monetary amount.
  N_SIGN_POSN         INT_N_SIGN_POSN
  'unspecified        'unspecified)


(define (%number-integer-part int grouping separator)
  ;; Process INT (a string denoting a number's integer part) and return a new
  ;; string with digit grouping and separators according to GROUPING (a list,
  ;; potentially circular) and SEPARATOR (a string).

  ;; Process INT from right to left.
  (let loop ((int      int)
             (grouping grouping)
             (result   '()))
    (cond ((string=? int "") (apply string-append result))
          ((null? grouping)  (apply string-append int result))
          (else
           (let* ((len (string-length int))
                  (cut (min (car grouping) len)))
             (loop (substring int 0 (- len cut))
                   (cdr grouping)
                   (let ((sub (substring int (- len cut) len)))
                     (if (> len cut)
                         (cons* separator sub result)
                         (cons sub result)))))))))

(define (add-monetary-sign+currency amount figure intl? locale)
  ;; Add a sign and currency symbol around FIGURE.  FIGURE should be a
  ;; formatted unsigned amount (a string) representing AMOUNT.
  (let* ((positive? (> amount 0))
         (sign
          (cond ((> amount 0) (locale-monetary-positive-sign locale))
                ((< amount 0) (locale-monetary-negative-sign locale))
                (else         "")))
         (currency (locale-currency-symbol intl? locale))
         (currency-precedes?
          (if positive?
              locale-currency-symbol-precedes-positive?
              locale-currency-symbol-precedes-negative?))
         (separated?
          (if positive?
              locale-positive-separated-by-space?
              locale-negative-separated-by-space?))
         (sign-position
          (if positive?
              locale-positive-sign-position
              locale-negative-sign-position))
         (currency-space
          (if (separated? intl? locale) " " ""))
         (append-currency
          (lambda (amt)
            (if (currency-precedes? intl? locale)
                (string-append currency currency-space amt)
                (string-append amt currency-space currency)))))

    (case (sign-position intl? locale)
      ((parenthesize)
       (string-append "(" (append-currency figure) ")"))
      ((sign-before)
       (string-append sign (append-currency figure)))
      ((sign-after unspecified)
       ;; following glibc's recommendation for `unspecified'.
       (if (currency-precedes? intl? locale)
           (string-append currency currency-space sign figure)
           (string-append figure currency-space currency sign)))
      ((sign-before-currency-symbol)
       (if (currency-precedes? intl? locale)
           (string-append sign currency currency-space figure)
           (string-append figure currency-space sign currency))) ;; unlikely
      ((sign-after-currency-symbol)
       (if (currency-precedes? intl? locale)
           (string-append currency sign currency-space figure)
           (string-append figure currency-space currency sign)))
      (else
       (error "unsupported sign position" (sign-position intl? locale))))))


(define* (monetary-amount->locale-string amount intl?
                                         #:optional (locale %global-locale))
  "Convert @var{amount} (an inexact) into a string according to the cultural
conventions of either @var{locale} (a locale object) or the current locale.
If @var{intl?} is true, then the international monetary format for the given
locale is used."

  (let* ((fraction-digits
          (or (locale-monetary-fractional-digits intl? locale) 2))
         (decimal-part
          (lambda (dec)
            (if (or (string=? dec "") (eq? 0 fraction-digits))
                ""
                (string-append (locale-monetary-decimal-point locale)
                               (if (< fraction-digits (string-length dec))
                                   (substring dec 0 fraction-digits)
                                   dec)))))

         (external-repr (number->string (if (> amount 0) amount (- amount))))
         (int+dec   (string-split external-repr #\.))
         (int       (car int+dec))
         (dec       (decimal-part (if (null? (cdr int+dec))
                                      ""
                                      (cadr int+dec))))
         (grouping  (locale-monetary-digit-grouping locale))
         (separator (locale-monetary-thousands-separator locale)))

      (add-monetary-sign+currency amount
                                  (string-append
                                   (%number-integer-part int grouping
                                                         separator)
                                   dec)
                                  intl? locale)))



;;;
;;; Number formatting.
;;;

(define-simple-langinfo-mapping locale-digit-grouping
  GROUPING             '())
(define-simple-langinfo-mapping locale-decimal-point
  RADIXCHAR            ".")
(define-simple-langinfo-mapping locale-thousands-separator
  THOUSEP              "")

(define* (number->locale-string number
                                #:optional (fraction-digits #t)
                                           (locale %global-locale))
  "Convert @var{number} (an inexact) into a string according to the cultural
conventions of either @var{locale} (a locale object) or the current locale.
Optionally, @var{fraction-digits} may be bound to an integer specifying the
number of fractional digits to be displayed."

  (let* ((sign
          (cond ((> number 0) "")
                ((< number 0) "-")
                (else         "")))
         (decimal-part
          (lambda (dec)
            (if (or (string=? dec "") (eq? 0 fraction-digits))
                ""
                (string-append (locale-decimal-point locale)
                               (if (and (integer? fraction-digits)
                                        (< fraction-digits
                                           (string-length dec)))
                                   (substring dec 0 fraction-digits)
                                   dec))))))

    (let* ((external-repr (number->string (if (> number 0)
                                              number
                                              (- number))))
           (int+dec   (string-split external-repr #\.))
           (int       (car int+dec))
           (dec       (decimal-part (if (null? (cdr int+dec))
                                        ""
                                        (cadr int+dec))))
           (grouping  (locale-digit-grouping locale))
           (separator (locale-thousands-separator locale)))

      (string-append sign
                     (%number-integer-part int grouping separator)
                     dec))))


;;;
;;; Miscellaneous.
;;;

(define-simple-langinfo-mapping locale-yes-regexp
  YESEXPR              "^[yY]")
(define-simple-langinfo-mapping locale-no-regexp
  NOEXPR               "^[nN]")

;; `YESSTR' and `NOSTR' are considered deprecated so we don't provide them.

;;; i18n.scm ends here
