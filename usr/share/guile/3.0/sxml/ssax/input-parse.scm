;;;; (sxml ssax input-parse) -- a simple lexer
;;;;
;;;; 	Copyright (C) 2009  Free Software Foundation, Inc.
;;;;    Modified 2004 by Andy Wingo <wingo at pobox dot com>.
;;;;    Written 2003 by Oleg Kiselyov <oleg at pobox dot com> as input-parse.scm.
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

;;; Commentary:
;;
;; A simple lexer.
;;
;; The procedures in this module surprisingly often suffice to parse an
;; input stream. They either skip, or build and return tokens, according
;; to inclusion or delimiting semantics. The list of characters to
;; expect, include, or to break at may vary from one invocation of a
;; function to another. This allows the functions to easily parse even
;; context-sensitive languages.
;;
;; EOF is generally frowned on, and thrown up upon if encountered.
;; Exceptions are mentioned specifically. The list of expected
;; characters (characters to skip until, or break-characters) may
;; include an EOF "character", which is to be coded as the symbol,
;; @code{*eof*}.
;;
;; The input stream to parse is specified as a @dfn{port}, which is
;; usually the last (and optional) argument. It defaults to the current
;; input port if omitted.
;;
;; If the parser encounters an error, it will throw an exception to the
;; key @code{parser-error}. The arguments will be of the form
;; @code{(@var{port} @var{message} @var{specialising-msg}*)}.
;;
;; The first argument is a port, which typically points to the offending
;; character or its neighborhood. You can then use @code{port-column}
;; and @code{port-line} to query the current position. @var{message} is
;; the description of the error. Other arguments supply more details
;; about the problem.
;;
;;; Code:

(define-module (sxml ssax input-parse)
  #:use-module (ice-9 rdelim)
  #:export (peek-next-char
            assert-curr-char
            skip-until
            skip-while
            next-token
            next-token-of
            read-text-line
            read-string
            find-string-from-port?))

(define ascii->char integer->char)
(define char->ascii char->integer)
(define char-newline #\newline)
(define char-return #\return)
(define inc 1+)
(define dec 1-)

;; rewrite oleg's define-opt into define* style
(define-macro (define-opt bindings body . body-rest)
  (let* ((rev-bindings (reverse bindings))
         (opt-bindings
          (and (pair? rev-bindings) (pair? (car rev-bindings))
               (eq? 'optional (caar rev-bindings))
               (cdar rev-bindings))))
    (if opt-bindings
	`(define* ,(append (reverse (cons #:optional (cdr rev-bindings)))
			  opt-bindings)
	   ,body ,@body-rest)
	`(define* ,bindings ,body ,@body-rest))))

(define (parser-error port message . rest)
  (apply throw 'parser-error port message rest))

(include-from-path "sxml/upstream/input-parse.scm")

;; This version for guile is quite speedy, due to read-delimited (which
;; is implemented in C).
(define-opt (next-token prefix-skipped-chars break-chars
			(optional (comment "") (port (current-input-port))) )
  (let ((delims (list->string (delete '*eof* break-chars))))
    (if (eof-object? (if (null? prefix-skipped-chars)
                         (peek-char port)
                         (skip-while prefix-skipped-chars port)))
        (if (memq '*eof* break-chars)
            ""
            (parser-error port "EOF while reading a token " comment))
        (let ((token (read-delimited delims port 'peek)))
          (if (and (eof-object? (peek-char port))
                   (not (memq '*eof* break-chars)))
              (parser-error port "EOF while reading a token " comment)
              token)))))

(define-opt (read-text-line (optional (port (current-input-port))) )
  (read-line port))

;; Written 1995, 1996 by Oleg Kiselyov (oleg@acm.org)
;; Modified 1996, 1997, 1998, 2001 by A. Jaffer (agj@alum.mit.edu)
;; Modified 2003 by Steve VanDevender (stevev@hexadecimal.uoregon.edu)
;; Modified 2004 Andy Wingo <wingo at pobox dot com>
;; This function is from SLIB's strsrch.scm, and is in the public domain.
(define (find-string-from-port? str <input-port> . max-no-char)
  "Looks for @var{str} in @var{<input-port>}, optionally within the
first @var{max-no-char} characters."
  (set! max-no-char (if (null? max-no-char) #f (car max-no-char)))
  (letrec
      ((no-chars-read 0)
       (peeked? #f)
       (my-peek-char			; Return a peeked char or #f
        (lambda () (and (or (not (number? max-no-char))
                            (< no-chars-read max-no-char))
                        (let ((c (peek-char <input-port>)))
                          (cond (peeked? c)
                                ((eof-object? c) #f)
                                ((procedure? max-no-char)
                                 (set! peeked? #t)
                                 (if (max-no-char c) #f c))
                                ((eqv? max-no-char c) #f)
                                (else c))))))
       (next-char (lambda () (set! peeked? #f) (read-char <input-port>)
                          (set! no-chars-read  (+ 1 no-chars-read))))
       (match-1st-char                  ; of the string str
        (lambda ()
          (let ((c (my-peek-char)))
            (and c
                 (begin (next-char)
                        (if (char=? c (string-ref str 0))
                            (match-other-chars 1)
                            (match-1st-char)))))))
       ;; There has been a partial match, up to the point pos-to-match
       ;; (for example, str[0] has been found in the stream)
       ;; Now look to see if str[pos-to-match] for would be found, too
       (match-other-chars
        (lambda (pos-to-match)
          (if (>= pos-to-match (string-length str))
              no-chars-read             ; the entire string has matched
              (let ((c (my-peek-char)))
                (and c
                     (if (not (char=? c (string-ref str pos-to-match)))
                         (backtrack 1 pos-to-match)
                         (begin (next-char)
                                (match-other-chars (+ 1 pos-to-match)))))))))

       ;; There had been a partial match, but then a wrong char showed up.
       ;; Before discarding previously read (and matched) characters, we check
       ;; to see if there was some smaller partial match. Note, characters read
       ;; so far (which matter) are those of str[0..matched-substr-len - 1]
       ;; In other words, we will check to see if there is such i>0 that
       ;; substr(str,0,j) = substr(str,i,matched-substr-len)
       ;; where j=matched-substr-len - i
       (backtrack
        (lambda (i matched-substr-len)
          (let ((j (- matched-substr-len i)))
            (if (<= j 0)
                ;; backed off completely to the begining of str
                (match-1st-char)
                (let loop ((k 0))
                  (if (>= k j)
                      (match-other-chars j) ; there was indeed a shorter match
                      (if (char=? (string-ref str k)
                                  (string-ref str (+ i k)))
                          (loop (+ 1 k))
                          (backtrack (+ 1 i) matched-substr-len))))))))
       )
    (match-1st-char)))
