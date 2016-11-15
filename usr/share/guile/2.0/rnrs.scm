;;; rnrs.scm --- The R6RS composite library

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


(library (rnrs (6))
  (export ;; (rnrs arithmetic bitwise)

          bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-if 
	  bitwise-bit-count bitwise-length bitwise-first-bit-set 
	  bitwise-bit-set? bitwise-copy-bit bitwise-bit-field 
	  bitwise-copy-bit-field bitwise-arithmetic-shift 
	  bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
	  bitwise-rotate-bit-field bitwise-reverse-bit-field
          
	  ;; (rnrs arithmetic fixnums)

	  fixnum? fixnum-width least-fixnum greatest-fixnum fx=? fx>? fx<? fx>=?
	  fx<=? fxzero? fxpositive? fxnegative? fxodd? fxeven? fxmax fxmin fx+
	  fx* fx- fxdiv-and-mod fxdiv fxmod fxdiv0-and-mod0 fxdiv0 fxmod0
	  fx+/carry fx-/carry fx*/carry fxnot fxand fxior fxxor fxif fxbit-count
	  fxlength fxfirst-bit-set fxbit-set? fxcopy-bit fxbit-field
	  fxcopy-bit-field fxarithmetic-shift fxarithmetic-shift-left
	  fxarithmetic-shift-right fxrotate-bit-field fxreverse-bit-field

	  ;; (rnrs arithmetic flonums)

	  flonum? real->flonum fl=? fl<? fl<=? fl>? fl>=? flinteger? flzero? 
	  flpositive? flnegative? flodd? fleven? flfinite? flinfinite? flnan?
	  flmax flmin fl+ fl* fl- fl/ flabs fldiv-and-mod fldiv flmod
	  fldiv0-and-mod0 fldiv0 flmod0 flnumerator fldenominator flfloor 
	  flceiling fltruncate flround flexp fllog flsin flcos fltan flacos 
	  flasin flatan flsqrt flexpt &no-infinities
	  make-no-infinities-violation no-infinities-violation? &no-nans
	  make-no-nans-violation no-nans-violation? fixnum->flonum
	  
	  ;; (rnrs base)

	  boolean? symbol? char? vector? null? pair? number? string? procedure?
	  define define-syntax syntax-rules lambda let let* let-values
	  let*-values letrec letrec* begin quote lambda if set! cond case or 
          and not eqv? equal? eq? + - * / max min abs numerator denominator gcd
          lcm floor ceiling truncate round rationalize real-part imag-part 
	  make-rectangular angle div mod div-and-mod div0 mod0 div0-and-mod0
	  expt exact-integer-sqrt sqrt exp log sin cos tan asin acos atan 
	  make-polar magnitude angle complex? real? rational? integer? exact? 
	  inexact? real-valued? rational-valued? integer-valued? zero? 
	  positive? negative? odd? even? nan? finite? infinite? exact inexact =
	  < > <= >= number->string string->number boolean=? cons car cdr caar 
          cadr cdar cddr caaar caadr cadar cdaar caddr cdadr cddar cdddr caaaar
          caaadr caadar cadaar cdaaar cddaar cdadar cdaadr cadadr caaddr caddar
          cadddr cdaddr cddadr cdddar cddddr list? list length append reverse 
	  list-tail list-ref map for-each symbol->string string->symbol symbol=?
	  char->integer integer->char char=? char<? char>? char<=? char>=?
	  make-string string string-length string-ref string=? string<? string>?
	  string<=? string>=? substring string-append string->list list->string
	  string-for-each string-copy vector? make-vector vector vector-length 
	  vector-ref vector-set! vector->list list->vector vector-fill! 
	  vector-map vector-for-each error assertion-violation assert
	  call-with-current-continuation call/cc call-with-values dynamic-wind
	  values apply quasiquote unquote unquote-splicing let-syntax 
	  letrec-syntax syntax-rules identifier-syntax

	  ;; (rnrs bytevectors)
	  
	  endianness native-endianness bytevector? make-bytevector 
	  bytevector-length bytevector=? bytevector-fill! bytevector-copy! 
	  bytevector-copy uniform-array->bytevector bytevector-u8-ref 
	  bytevector-s8-ref bytevector-u8-set! bytevector-s8-set! 
	  bytevector->u8-list u8-list->bytevector bytevector-uint-ref 
	  bytevector-uint-set! bytevector-sint-ref bytevector-sint-set!
	  bytevector->sint-list bytevector->uint-list uint-list->bytevector 
	  sint-list->bytevector bytevector-u16-ref bytevector-s16-ref
	  bytevector-u16-set! bytevector-s16-set! bytevector-u16-native-ref 
	  bytevector-s16-native-ref bytevector-u16-native-set! 
	  bytevector-s16-native-set! bytevector-u32-ref bytevector-s32-ref
	  bytevector-u32-set! bytevector-s32-set! bytevector-u32-native-ref 
	  bytevector-s32-native-ref bytevector-u32-native-set! 
	  bytevector-s32-native-set! bytevector-u64-ref bytevector-s64-ref
	  bytevector-u64-set! bytevector-s64-set! bytevector-u64-native-ref 
	  bytevector-s64-native-ref bytevector-u64-native-set! 
	  bytevector-s64-native-set! bytevector-ieee-single-ref
	  bytevector-ieee-single-set! bytevector-ieee-single-native-ref
	  bytevector-ieee-single-native-set! bytevector-ieee-double-ref
	  bytevector-ieee-double-set! bytevector-ieee-double-native-ref
	  bytevector-ieee-double-native-set! string->utf8 string->utf16 
	  string->utf32 utf8->string utf16->string utf32->string

	  ;; (rnrs conditions)

	  &condition condition simple-conditions condition? condition-predicate
	  condition-accessor define-condition-type &message
	  make-message-condition message-condition? condition-message &warning
	  make-warning warning? &serious make-serious-condition
	  serious-condition? &error make-error error? &violation make-violation
	  violation? &assertion make-assertion-violation assertion-violation?
	  &irritants make-irritants-condition irritants-condition?
	  condition-irritants &who make-who-condition who-condition?
	  condition-who &non-continuable make-non-continuable-violation
	  non-continuable-violation? &implementation-restriction
	  make-implementation-restriction-violation
	  implementation-restriction-violation? &lexical make-lexical-violation
	  lexical-violation? &syntax make-syntax-violation syntax-violation?
	  syntax-violation-form syntax-violation-subform &undefined
	  make-undefined-violation undefined-violation?

	  ;; (rnrs control)

	  when unless do case-lambda

	  ;; (rnrs enums)

	  make-enumeration enum-set-universe enum-set-indexer 
	  enum-set-constructor enum-set->list enum-set-member? enum-set-subset?
	  enum-set=? enum-set-union enum-set-intersection enum-set-difference
	  enum-set-complement enum-set-projection define-enumeration

	  ;; (rnrs exceptions)

	  guard with-exception-handler raise raise-continuable

	  ;; (rnrs files)

	  file-exists? delete-file &i/o make-i/o-error i/o-error? &i/o-read 
	  make-i/o-read-error i/o-read-error? &i/o-write make-i/o-write-error 
	  i/o-write-error? &i/o-invalid-position 
	  make-i/o-invalid-position-error i/o-invalid-position-error? 
	  i/o-error-position &i/o-filename make-i/o-filename-error
	  i/o-filename-error? i/o-error-filename &i/o-file-protection 
	  make-i/o-file-protection-error i/o-file-protection-error?
	  &i/o-file-is-read-only make-i/o-file-is-read-only-error
	  i/o-file-is-read-only-error? &i/o-file-already-exists
	  make-i/o-file-already-exists-error i/o-file-already-exists-error?
	  &i/o-file-does-not-exist make-i/o-file-does-not-exist-error
	  i/o-file-does-not-exist-error? &i/o-port make-i/o-port-error
	  i/o-port-error? i/o-error-port

	  ;; (rnrs hashtables)

	  make-eq-hashtable make-eqv-hashtable make-hashtable hashtable?
	  hashtable-size hashtable-ref hashtable-set! hashtable-delete!
	  hashtable-contains? hashtable-update! hashtable-copy hashtable-clear!
	  hashtable-keys hashtable-entries hashtable-equivalence-function
	  hashtable-hash-function hashtable-mutable? equal-hash string-hash
	  string-ci-hash symbol-hash

	  ;; (rnrs io ports)

	  file-options buffer-mode buffer-mode?
	  eol-style native-eol-style error-handling-mode
	  make-transcoder transcoder-codec transcoder-eol-style
          transcoder-error-handling-mode native-transcoder
	  latin-1-codec utf-8-codec utf-16-codec
	  
	  eof-object? port? input-port? output-port? eof-object port-eof?
	  port-transcoder
	  binary-port? textual-port? transcoded-port
	  port-position set-port-position!
	  port-has-port-position? port-has-set-port-position!?
          close-port call-with-port
	  open-bytevector-input-port make-custom-binary-input-port get-u8 
	  lookahead-u8 get-bytevector-n get-bytevector-n! get-bytevector-some 
	  get-bytevector-all open-bytevector-output-port
	  make-custom-binary-output-port put-u8 put-bytevector
          open-string-input-port open-string-output-port
          call-with-bytevector-output-port
          call-with-string-output-port
          latin-1-codec utf-8-codec utf-16-codec
          open-file-input-port open-file-output-port open-file-input/output-port
          make-custom-textual-output-port
          call-with-string-output-port
	  flush-output-port put-string
          get-char get-datum get-line get-string-all get-string-n get-string-n!
          lookahead-char
          put-char put-datum put-string
          standard-input-port standard-output-port standard-error-port
          
	  ;; (rnrs io simple)
	  
	  call-with-input-file call-with-output-file current-input-port
	  current-output-port current-error-port with-input-from-file
	  with-output-to-file open-input-file open-output-file close-input-port
	  close-output-port read-char peek-char read write-char newline display
	  write

	  ;; (rnrs lists)

	  find for-all exists filter partition fold-left fold-right remp remove 
	  remv remq memp member memv memq assp assoc assv assq cons*

	  ;; (rnrs programs)

	  command-line exit

	  ;; (rnrs records inspection)

	  record? record-rtd record-type-name record-type-parent
	  record-type-uid record-type-generative? record-type-sealed? 
	  record-type-opaque? record-type-field-names record-field-mutable?

	  ;; (rnrs records procedural)

	  make-record-type-descriptor record-type-descriptor?
	  make-record-constructor-descriptor record-constructor record-predicate
	  record-accessor record-mutator

	  ;; (rnrs records syntactic)

	  define-record-type record-type-descriptor 
	  record-constructor-descriptor

	  ;; (rnrs sorting)
	  
	  list-sort vector-sort vector-sort!

	  ;; (rnrs syntax-case)

	  make-variable-transformer syntax
          ;; Until the deprecated support for a unified modules and
          ;; bindings namespace is removed, we need to manually resolve
          ;; a conflict between two bindings: that of the (rnrs
          ;; syntax-case) module, and the imported `syntax-case'
          ;; binding. We do so here and below by renaming the macro
          ;; import.
          (rename (syntax-case-hack syntax-case))
          identifier?  bound-identifier=? free-identifier=?
          syntax->datum datum->syntax generate-temporaries with-syntax
          quasisyntax unsyntax unsyntax-splicing syntax-violation

	  ;; (rnrs unicode)
	  
	  char-upcase char-downcase char-titlecase char-foldcase
	  char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	  char-alphabetic? char-numeric? char-whitespace? char-upper-case?
	  char-lower-case? char-title-case? char-general-category
	  string-upcase string-downcase string-titlecase string-foldcase
	  string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
	  string-normalize-nfd string-normalize-nfkd string-normalize-nfc
	  string-normalize-nfkc)

  (import (rnrs arithmetic bitwise (6))
	  (rnrs arithmetic fixnums (6))
	  (rnrs arithmetic flonums (6))
	  (rnrs base (6))

	  (rnrs bytevectors (6))
          
	  (rnrs conditions (6))
	  (rnrs control (6))
	  (rnrs enums (6))
	  (rnrs exceptions (6))

          (rnrs files (6))

	  (rnrs hashtables (6))

	  (rnrs io ports (6))

	  (rnrs io simple (6))
	  (rnrs lists (6))
	  (rnrs programs (6))
	  (rnrs records inspection (6))
	  (rnrs records procedural (6))
	  (rnrs records syntactic (6))
	  (rnrs sorting (6))
          ;; See note above on exporting syntax-case.
	  (rename (rnrs syntax-case (6))
                  (syntax-case syntax-case-hack))
	  (rnrs unicode (6))))
