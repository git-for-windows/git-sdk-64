;;; unicode.scm --- The R6RS Unicode library

;;      Copyright (C) 2010 Free Software Foundation, Inc.
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


(library (rnrs unicode (6))
  (export char-upcase
	  char-downcase
	  char-titlecase
	  char-foldcase
	  
	  char-ci=?
	  char-ci<?
	  char-ci>?
	  char-ci<=?
	  char-ci>=?
	   
	  char-alphabetic?
	  char-numeric?
	  char-whitespace?
	  char-upper-case?
	  char-lower-case?
	  char-title-case?
	  
	  char-general-category
	  
	  string-upcase
	  string-downcase
	  string-titlecase
	  string-foldcase
	  
	  string-ci=?
	  string-ci<?
	  string-ci>?
	  string-ci<=?
	  string-ci>=?
	  
	  string-normalize-nfd
	  string-normalize-nfkd
	  string-normalize-nfc
	  string-normalize-nfkc)
  (import (only (guile) char-upcase
		        char-downcase
			char-titlecase

			char-ci=?
			char-ci<?
			char-ci>?
			char-ci<=?
			char-ci>=?

			char-alphabetic?
			char-numeric?
			char-whitespace?
			char-upper-case?
			char-lower-case?

			char-set-contains?
			char-set:title-case

			char-general-category

			char-upcase
			char-downcase
			char-titlecase

			string-upcase
			string-downcase
			string-titlecase
	  
			string-ci=?
			string-ci<?
			string-ci>?
			string-ci<=?
			string-ci>=?
	  
			string-normalize-nfd
			string-normalize-nfkd
			string-normalize-nfc
			string-normalize-nfkc)
	  (rnrs base (6)))

  (define (char-foldcase char)
    (if (or (eqv? char #\460) (eqv? char #\461))
	char (char-downcase (char-upcase char))))

  (define (char-title-case? char) (char-set-contains? char-set:title-case char))

  (define (string-foldcase str) (string-downcase (string-upcase str)))
)
