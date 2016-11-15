;;; Brainfuck for GNU Guile.

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

(define-module (language brainfuck spec)
  #:use-module (language brainfuck compile-tree-il)
  #:use-module (language brainfuck compile-scheme)
  #:use-module (language brainfuck parse)
  #:use-module (system base language)
  #:export (brainfuck))


; The new language is integrated into Guile via this (define-language)
; specification in the special module (language [lang] spec).
; Provided is a parser-routine in #:reader, a output routine in #:printer
; and one or more compiler routines (as target-language - routine pairs)
; in #:compilers.  This is the basic set of fields needed to specify a new
; language.

(define-language brainfuck
  #:title	"Brainfuck"
  #:reader	(lambda (port env) (read-brainfuck port))
  #:compilers	`((tree-il . ,compile-tree-il)
                  (scheme . ,compile-scheme))
  #:printer	write
  )
