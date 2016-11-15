;;; ECMAScript for Guile

;; Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language ecmascript tokenize)
  #:use-module (ice-9 rdelim)
  #:use-module ((srfi srfi-1) #:select (unfold-right))
  #:use-module (system base lalr)
  #:export (next-token make-tokenizer make-tokenizer/1 tokenize tokenize/1))

(define (syntax-error what loc form . args)
  (throw 'syntax-error #f what
         (and=> loc source-location->source-properties)
         form #f args))

(define (port-source-location port)
  (make-source-location (port-filename port)
                        (port-line port)
                        (port-column port)
                        (false-if-exception (ftell port))
                        #f))

;; taken from SSAX, sorta
(define (read-until delims port loc)
  (if (eof-object? (peek-char port))
      (syntax-error "EOF while reading a token" loc #f)
      (let ((token (read-delimited delims port 'peek)))
        (if (eof-object? (peek-char port))
            (syntax-error "EOF while reading a token" loc token)
            token))))

(define (char-hex? c)
  (and (not (eof-object? c))
       (or (char-numeric? c)
           (memv c '(#\a #\b #\c #\d #\e #\f))
           (memv c '(#\A #\B #\C #\D #\E #\F)))))

(define (digit->number c)
  (- (char->integer c) (char->integer #\0)))

(define (hex->number c)
  (if (char-numeric? c)
      (digit->number c)
      (+ 10 (- (char->integer (char-downcase c)) (char->integer #\a)))))

(define (read-slash port loc div?)
  (let ((c1 (begin
              (read-char port)
              (peek-char port))))
    (cond
     ((eof-object? c1)
      ;; hmm. error if we're not looking for a div? ?
      (make-lexical-token '/ loc #f))
     ((char=? c1 #\/)
      (read-line port)
      (next-token port div?))
     ((char=? c1 #\*)
      (read-char port)
      (let lp ((c (read-char port)))
        (cond
         ((eof-object? c)
          (syntax-error "EOF while in multi-line comment" loc #f))
         ((char=? c #\*)
          (if (eqv? (peek-char port) #\/)
              (begin
                (read-char port)
                (next-token port div?))
              (lp (read-char port))))
         (else
          (lp (read-char port))))))
     (div?
      (case c1
        ((#\=) (read-char port) (make-lexical-token '/= loc #f))
        (else (make-lexical-token '/ loc #f))))
     (else
      (read-regexp port loc)))))

(define (read-regexp port loc)
  ;; first slash already read
  (let ((terms (string #\/ #\\ #\nl #\cr)))
    (let lp ((str (read-until terms port loc)) (head ""))
      (let ((terminator (peek-char port)))
        (cond
         ((char=? terminator #\/)
          (read-char port)
          ;; flags
          (let lp ((c (peek-char port)) (flags '()))
            (if (or (eof-object? c)
                    (not (or (char-alphabetic? c)
                             (char-numeric? c)
                             (char=? c #\$)
                             (char=? c #\_))))
                (make-lexical-token 'RegexpLiteral loc
                                    (cons (string-append head str)
                                          (reverse flags)))
                (begin (read-char port)
                       (lp (peek-char port) (cons c flags))))))
         ((char=? terminator #\\)
          (read-char port)
          (let ((echar (read-char port)))
            (lp (read-until terms port loc)
                (string-append head str (string #\\ echar)))))
         (else
          (syntax-error "regexp literals may not contain newlines"
                        loc str)))))))

(define (read-string port loc)
  (let ((c (read-char port)))
    (let ((terms (string c #\\ #\nl #\cr)))
      (define (read-escape port)
        (let ((c (read-char port)))
          (case c
            ((#\' #\" #\\) c)
            ((#\b) #\bs)
            ((#\f) #\np)
            ((#\n) #\nl)
            ((#\r) #\cr)
            ((#\t) #\tab)
            ((#\v) #\vt)
            ((#\0)
             (let ((next (peek-char port)))
               (cond
                ((eof-object? next) #\nul)
                ((char-numeric? next)
                 (syntax-error "octal escape sequences are not supported"
                               loc #f))
                (else #\nul))))
            ((#\x)
             (let* ((a (read-char port))
                    (b (read-char port)))
               (cond
                ((and (char-hex? a) (char-hex? b))
                 (integer->char (+ (* 16 (hex->number a)) (hex->number b))))
                (else
                 (syntax-error "bad hex character escape" loc (string a b))))))
            ((#\u)
             (let* ((a (read-char port))
                    (b (read-char port))
                    (c (read-char port))
                    (d (read-char port)))
               (integer->char (string->number (string a b c d) 16))))
            (else
             c))))
      (let lp ((str (read-until terms port loc)))
        (let ((terminator (peek-char port)))
          (cond
           ((char=? terminator c)
            (read-char port)
            (make-lexical-token 'StringLiteral loc str))
           ((char=? terminator #\\)
            (read-char port)
            (let ((echar (read-escape port)))
              (lp (string-append str (string echar)
                                 (read-until terms port loc)))))
           (else
            (syntax-error "string literals may not contain newlines"
                          loc str))))))))

(define *keywords*
  '(("break" . break)
    ("else" . else)
    ("new" . new)
    ("var" . var)
    ("case" . case)
    ("finally" . finally)
    ("return" . return)
    ("void" . void)
    ("catch" . catch)
    ("for" . for)
    ("switch" . switch)
    ("while" . while)
    ("continue" . continue)
    ("function" . function)
    ("this" . this)
    ("with" . with)
    ("default" . default)
    ("if" . if)
    ("throw" . throw)
    ("delete" . delete)
    ("in" . in)
    ("try" . try)
    ("do" . do)
    ("instanceof" . instanceof)
    ("typeof" . typeof)

    ;; these aren't exactly keywords, but hey
    ("null" . null)
    ("true" . true)
    ("false" . false)))

(define *future-reserved-words*
  '(("abstract" . abstract)
    ("enum" . enum)
    ("int" . int)
    ("short" . short)
    ("boolean" . boolean)
    ("export" . export)
    ("interface" . interface)
    ("static" . static)
    ("byte" . byte)
    ("extends" . extends)
    ("long" . long)
    ("super" . super)
    ("char" . char)
    ("final" . final)
    ("native" . native)
    ("synchronized" . synchronized)
    ("class" . class)
    ("float" . float)
    ("package" . package)
    ("throws" . throws)
    ("const" . const)
    ("goto" . goto)
    ("private" . private)
    ("transient" . transient)
    ("debugger" . debugger)
    ("implements" . implements)
    ("protected" . protected)
    ("volatile" . volatile)
    ("double" . double)
    ("import" . import)
    ("public" . public)))

(define (read-identifier port loc)
  (let lp ((c (peek-char port)) (chars '()))
    (if (or (eof-object? c)
            (not (or (char-alphabetic? c)
                     (char-numeric? c)
                     (char=? c #\$)
                     (char=? c #\_))))
        (let ((word (list->string (reverse chars))))
          (cond ((assoc-ref *keywords* word)
                 => (lambda (x) (make-lexical-token x loc #f)))
                ((assoc-ref *future-reserved-words* word)
                 (syntax-error "word is reserved for the future, dude."
                               loc word))
                (else (make-lexical-token 'Identifier loc
                                          (string->symbol word)))))
        (begin (read-char port)
               (lp (peek-char port) (cons c chars))))))

(define (read-numeric port loc)
  (let* ((c0 (if (char=? (peek-char port) #\.)
                 #\0
                 (read-char port)))
         (c1 (peek-char port)))
    (cond
     ((eof-object? c1) (digit->number c0))
     ((and (char=? c0 #\0) (or (char=? c1 #\x) (char=? c1 #\X)))
      (read-char port)
      (let ((c (peek-char port)))
        (if (not (char-hex? c))
            (syntax-error "bad digit reading hexadecimal number"
                          loc c))
        (let lp ((c c) (acc 0))
          (cond ((char-hex? c)
                 (read-char port)
                 (lp (peek-char port)
                     (+ (* 16 acc) (hex->number c))))
                (else
                 acc)))))
     ((and (char=? c0 #\0) (char-numeric? c1))
      (let lp ((c c1) (acc 0))
        (cond ((eof-object? c) acc)
              ((char-numeric? c)
               (if (or (char=? c #\8) (char=? c #\9))
                   (syntax-error "invalid digit in octal sequence"
                                 loc c))
               (read-char port)
               (lp (peek-char port)
                   (+ (* 8 acc) (digit->number c))))
              (else
               acc))))
     (else
      (let lp ((c1 c1) (acc (digit->number c0)))
        (cond
         ((eof-object? c1) acc)
         ((char-numeric? c1)
          (read-char port)
          (lp (peek-char port)
              (+ (* 10 acc) (digit->number c1))))
         ((or (char=? c1 #\e) (char=? c1 #\E))
          (read-char port)
          (let ((add (let ((c (peek-char port)))
                       (cond ((eof-object? c)
                              (syntax-error "error reading exponent: EOF"
                                            loc #f))
                             ((char=? c #\+) (read-char port) +)
                             ((char=? c #\-) (read-char port) -)
                             ((char-numeric? c) +)
                             (else
                              (syntax-error "error reading exponent: non-digit"
                                            loc c))))))
            (let lp ((c (peek-char port)) (e 0))
              (cond ((and (not (eof-object? c)) (char-numeric? c))
                     (read-char port)
                     (lp (peek-char port) (add (* 10 e) (digit->number c))))
                    (else
                     (* (if (negative? e) (* acc 1.0) acc) (expt 10 e)))))))
         ((char=? c1 #\.)
          (read-char port)
          (let lp2 ((c (peek-char port)) (dec 0.0) (n -1))
            (cond ((and (not (eof-object? c)) (char-numeric? c))
                   (read-char port)
                   (lp2 (peek-char port)
                        (+ dec (* (digit->number c) (expt 10 n)))
                        (1- n)))
                  (else
                   ;; loop back to catch an exponential part
                   (lp c (+ acc dec))))))
         (else
          acc)))))))
           
(define *punctuation*
  '(("{" . lbrace)
    ("}" . rbrace)
    ("(" . lparen)
    (")" . rparen)
    ("[" . lbracket)
    ("]" . rbracket)
    ("." . dot)
    (";" . semicolon)
    ("," . comma)
    ("<" . <)
    (">" . >)
    ("<=" . <=)
    (">=" . >=)
    ("==" . ==)
    ("!=" . !=)
    ("===" . ===)
    ("!==" . !==)
    ("+" . +)
    ("-" . -)
    ("*" . *)
    ("%" . %)
    ("++" . ++)
    ("--" . --)
    ("<<" . <<)
    (">>" . >>)
    (">>>" . >>>)
    ("&" . &)
    ("|" . bor)
    ("^" . ^)
    ("!" . !)
    ("~" . ~)
    ("&&" . &&)
    ("||" . or)
    ("?" . ?)
    (":" . colon)
    ("=" . =)
    ("+=" . +=)
    ("-=" . -=)
    ("*=" . *=)
    ("%=" . %=)
    ("<<=" . <<=)
    (">>=" . >>=)
    (">>>=" . >>>=)
    ("&=" . &=)
    ("|=" . bor=)
    ("^=" . ^=)))

(define *div-punctuation*
  '(("/" . /)
    ("/=" . /=)))

;; node ::= (char (symbol | #f) node*)
(define read-punctuation
  (let ((punc-tree (let lp ((nodes '()) (puncs *punctuation*))
                     (cond ((null? puncs)
                            nodes)
                           ((assv-ref nodes (string-ref (caar puncs) 0))
                            => (lambda (node-tail)
                                 (if (= (string-length (caar puncs)) 1)
                                     (set-car! node-tail (cdar puncs))
                                     (set-cdr! node-tail
                                               (lp (cdr node-tail)
                                                   `((,(substring (caar puncs) 1)
                                                      . ,(cdar puncs))))))
                                 (lp nodes (cdr puncs))))
                           (else
                            (lp (cons (list (string-ref (caar puncs) 0) #f) nodes)
                                puncs))))))
    (lambda (port loc)
      (let lp ((c (peek-char port)) (tree punc-tree) (candidate #f))
        (cond
         ((assv-ref tree c)
          => (lambda (node-tail)
               (read-char port)
               (lp (peek-char port) (cdr node-tail) (car node-tail))))
         (candidate
          (make-lexical-token candidate loc #f))
         (else
          (syntax-error "bad syntax: character not allowed" loc c)))))))

(define (next-token port div?)
  (let ((c   (peek-char port))
        (loc (port-source-location port)))
    (case c
      ((#\ht #\vt #\np #\space #\x00A0) ; whitespace
       (read-char port)
       (next-token port div?))
      ((#\newline #\cr)                 ; line break
       (read-char port)
       (next-token port div?))
      ((#\/)
       ;; division, single comment, double comment, or regexp
       (read-slash port loc div?))
      ((#\" #\')                        ; string literal
       (read-string port loc))
      (else
       (cond
        ((eof-object? c)
         '*eoi*)
        ((or (char-alphabetic? c)
             (char=? c #\$)
             (char=? c #\_))
         ;; reserved word or identifier
         (read-identifier port loc))
        ((char-numeric? c)
         ;; numeric -- also accept . FIXME, requires lookahead
         (make-lexical-token 'NumericLiteral loc (read-numeric port loc)))
        (else
         ;; punctuation
         (read-punctuation port loc)))))))

(define (make-tokenizer port)
  (let ((div? #f))
    (lambda ()
      (let ((tok (next-token port div?)))
        (set! div? (and (lexical-token? tok)
                        (let ((cat (lexical-token-category tok)))
                          (or (eq? cat 'Identifier)
                              (eq? cat 'NumericLiteral)
                              (eq? cat 'StringLiteral)))))
        tok))))

(define (make-tokenizer/1 port)
  (let ((div? #f)
        (eoi? #f)
        (stack '()))
    (lambda ()
      (if eoi?
          '*eoi*
          (let ((tok (next-token port div?)))
            (case (if (lexical-token? tok) (lexical-token-category tok) tok)
              ((lparen)
               (set! stack (cons tok stack)))
              ((rparen)
               (if (and (pair? stack)
                        (eq? (lexical-token-category (car stack)) 'lparen))
                   (set! stack (cdr stack))
                   (syntax-error "unexpected right parenthesis"
                                 (lexical-token-source tok)
                                 #f)))
              ((lbracket)
               (set! stack (cons tok stack)))
              ((rbracket)
               (if (and (pair? stack)
                        (eq? (lexical-token-category (car stack)) 'lbracket))
                   (set! stack (cdr stack))
                   (syntax-error "unexpected right bracket"
                                 (lexical-token-source tok)
                                 #f)))
              ((lbrace)
               (set! stack (cons tok stack)))
              ((rbrace)
               (if (and (pair? stack)
                        (eq? (lexical-token-category (car stack)) 'lbrace))
                   (set! stack (cdr stack))
                   (syntax-error "unexpected right brace"
                                 (lexical-token-source tok)
                                 #f)))
              ((semicolon)
               (set! eoi? (null? stack))))
            (set! div? (and (lexical-token? tok)
                            (let ((cat (lexical-token-category tok)))
                              (or (eq? cat 'Identifier)
                                  (eq? cat 'NumericLiteral)
                                  (eq? cat 'StringLiteral)))))
            tok)))))

(define (tokenize port)
  (let ((next (make-tokenizer port)))
    (let lp ((out '()))
      (let ((tok (next)))
        (if (eq? tok '*eoi*)
            (reverse! out)
            (lp (cons tok out)))))))

(define (tokenize/1 port)
  (let ((next (make-tokenizer/1 port)))
    (let lp ((out '()))
      (let ((tok (next)))
        (if (eq? tok '*eoi*)
            (reverse! out)
            (lp (cons tok out)))))))

