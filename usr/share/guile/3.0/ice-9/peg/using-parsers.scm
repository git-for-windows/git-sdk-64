;;;; using-parsers.scm --- utilities to make using parsers easier
;;;;
;;;; 	Copyright (C) 2010, 2011 Free Software Foundation, Inc.
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

(define-module (ice-9 peg using-parsers)
  #:use-module (ice-9 peg simplify-tree)
  #:use-module (ice-9 peg codegen)
  #:use-module (ice-9 peg cache)
  #:use-module (srfi srfi-9)
  #:export (match-pattern define-peg-pattern search-for-pattern
            prec make-prec peg:start peg:end peg:string
            peg:tree peg:substring peg-record?))

(define-record-type prec
  (make-prec start end string tree)
  peg-record?
  (start prec-start)
  (end prec-end)
  (string prec-string)
  (tree prec-tree))

(define (peg:start pm)
  (and pm (prec-start pm)))
(define (peg:end pm)
  (and pm (prec-end pm)))
(define (peg:string pm)
  (and pm (prec-string pm)))
(define (peg:tree pm)
  (and pm (prec-tree pm)))
(define (peg:substring pm)
  (and pm (substring (prec-string pm) (prec-start pm) (prec-end pm))))

;;;
;;; Helper Macros
;;;

(define-syntax until
  (syntax-rules ()
    "Evaluate TEST.  If it is true, return its value.  Otherwise,
execute the STMTs and try again."
    ((_ test stmt stmt* ...)
     (let lp ()
       (or test
           (begin stmt stmt* ... (lp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; FOR DEFINING AND USING NONTERMINALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parses STRING using NONTERM
(define (match-pattern nonterm string)
  ;; We copy the string before using it because it might have been modified
  ;; in-place since the last time it was parsed, which would invalidate the
  ;; cache.  Guile uses copy-on-write for strings, so this is fast.
  (let ((res (nonterm (string-copy string) (string-length string) 0)))
    (if (not res)
        #f
        (make-prec 0 (car res) string (string-collapse (cadr res))))))

;; Defines a new nonterminal symbol accumulating with ACCUM.
(define-syntax define-peg-pattern
  (lambda (x)
    (syntax-case x ()
      ((_ sym accum pat)
       (let ((matchf (compile-peg-pattern #'pat (syntax->datum #'accum)))
             (accumsym (syntax->datum #'accum)))
         ;; CODE is the code to parse the string if the result isn't cached.
         (let ((syn (wrap-parser-for-users x matchf accumsym #'sym)))
           #`(define sym #,(cg-cached-parser syn))))))))

(define (peg-like->peg pat)
  (syntax-case pat ()
    (str (string? (syntax->datum #'str)) #'(peg str))
    (else pat)))

;; Searches through STRING for something that parses to PEG-MATCHER.  Think
;; regexp search.
(define-syntax search-for-pattern
  (lambda (x)
    (syntax-case x ()
      ((_ pattern string-uncopied)
       (let ((pmsym (syntax->datum #'pattern)))
         (let ((matcher (compile-peg-pattern (peg-like->peg #'pattern) 'body)))
           ;; We copy the string before using it because it might have been
           ;; modified in-place since the last time it was parsed, which would
           ;; invalidate the cache.  Guile uses copy-on-write for strings, so
           ;; this is fast.
           #`(let ((string (string-copy string-uncopied))
                   (strlen (string-length string-uncopied))
                   (at 0))
               (let ((ret (until (or (>= at strlen)
                                     (#,matcher string strlen at))
                                 (set! at (+ at 1)))))
                 (if (eq? ret #t) ;; (>= at strlen) succeeded
                     #f
                     (let ((end (car ret))
                           (match (cadr ret)))
                       (make-prec
                        at end string
                        (string-collapse match))))))))))))
