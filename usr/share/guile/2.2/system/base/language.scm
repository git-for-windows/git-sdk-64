;;; Multi-language support

;; Copyright (C) 2001, 2009, 2010, 2011, 2013 Free Software Foundation, Inc.

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

(define-module (system base language)
  #:use-module (system base syntax)
  #:export (define-language language? lookup-language make-language
            language-name language-title language-reader
            language-printer language-parser 
            language-compilers language-decompilers language-evaluator
            language-joiner language-for-humans?
            language-make-default-environment

            lookup-compilation-order lookup-decompilation-order
            invalidate-compilation-cache! default-environment

            *current-language*)

  #:re-export (current-language))


;;;
;;; Language class
;;;

(define-record/keywords <language>
  name
  title
  reader
  printer
  (parser #f)
  (compilers '())
  (decompilers '())
  (evaluator #f)
  (joiner #f)
  (for-humans? #t)
  (make-default-environment make-fresh-user-module))

(define-macro (define-language name . spec)
  `(begin
     (invalidate-compilation-cache!)
     (define ,name (make-language #:name ',name ,@spec))))

(define (lookup-language name)
  (let ((m (resolve-module `(language ,name spec))))
    (if (module-bound? m name)
	(module-ref m name)
	(error "no such language" name))))

(define *compilation-cache* '())
(define *decompilation-cache* '())

(define (invalidate-compilation-cache!)
  (set! *decompilation-cache* '())
  (set! *compilation-cache* '()))

(define (compute-translation-order from to language-translators)
  (cond
   ((not (language? to))
    (compute-translation-order from (lookup-language to) language-translators))
   (else
    (let lp ((from from) (seen '()))
      (cond
       ((not (language? from))
        (lp (lookup-language from) seen))
       ((eq? from to) (reverse! seen))
       ((memq from seen) #f)
       (else (or-map (lambda (pair)
                       (lp (car pair) (acons from (cdr pair) seen)))
                     (language-translators from))))))))

(define (lookup-compilation-order from to)
  (let ((key (cons from to)))
    (or (assoc-ref *compilation-cache* key)
        (let ((order (compute-translation-order from to language-compilers)))
          (set! *compilation-cache*
                (acons key order *compilation-cache*))
          order))))

(define (lookup-decompilation-order from to)
  (let ((key (cons from to)))
    (or (assoc-ref *decompilation-cache* key)
        ;; trickery!
        (let ((order (and=>
                      (compute-translation-order to from language-decompilers)
                      reverse!)))
          (set! *decompilation-cache* (acons key order *decompilation-cache*))
          order))))

(define (default-environment lang)
  "Return the default compilation environment for source language LANG."
  ((language-make-default-environment
    (if (language? lang) lang (lookup-language lang)))))



;;;
;;; Current language
;;;

;; Deprecated; use current-language instead.
(define *current-language* (parameter-fluid current-language))
