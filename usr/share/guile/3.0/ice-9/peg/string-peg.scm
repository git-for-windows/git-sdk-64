;;;; string-peg.scm --- representing PEG grammars as strings
;;;;
;;;; 	Copyright (C) 2010, 2011, Free Software Foundation, Inc.
;;;; 	Copyright (C) 2024 Ekaitz Zarraga <ekaitz@elenq.tech>
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

(define-module (ice-9 peg string-peg)
  #:export (peg-as-peg
            define-peg-string-patterns
            peg-grammar)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg using-parsers)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 peg codegen)
  #:use-module (ice-9 peg simplify-tree))

;; This module provides support for PEG as described in:
;;   <https://bford.info/pub/lang/peg.pdf>

;; Gets the left-hand depth of a list.
(define (depth lst)
  (if (or (not (list? lst)) (null? lst))
      0
      (+ 1 (depth (car lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parse string PEGs using sexp PEGs.
;; See the variable PEG-AS-PEG for an easier-to-read syntax.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grammar for PEGs in PEG grammar.
(define peg-as-peg
"# Hierarchical syntax
Grammar <-- Spacing Definition+ EndOfFile
Definition <-- Identifier LEFTARROW Expression

Expression <-- Sequence (SLASH Sequence)*
Sequence <-- Prefix*
Prefix <-- (AND / NOT)? Suffix
Suffix <-- Primary (QUESTION / STAR / PLUS)?
Primary <-- Identifier !LEFTARROW
           / OPEN Expression CLOSE
           / Literal / Class / NotInClass / DOT

# Lexical syntax
Identifier <-- IdentStart IdentCont* Spacing
# NOTE: `-` is an extension
IdentStart <- [a-zA-Z_] / '-'
IdentCont <- IdentStart / [0-9]

Literal <-- SQUOTE (!SQUOTE Char)* SQUOTE Spacing
        / DQUOTE (!DQUOTE Char)* DQUOTE Spacing
NotInClass <-- OPENBRACKET NOTIN  (!CLOSEBRACKET Range)* CLOSEBRACKET Spacing
Class <-- OPENBRACKET !NOTIN  (!CLOSEBRACKET Range)* CLOSEBRACKET Spacing
Range <-- Char DASH Char / Char
Char <-- '\\\\' [nrtf'\"\\[\\]\\\\]
       / '\\\\' [0-7][0-7][0-7]
       / '\\\\' [0-7][0-7]?
       / '\\\\' 'u' HEX HEX HEX HEX
       / !'\\\\' .

# NOTE: `<--` and `<` are extensions
LEFTARROW <- ('<--' / '<-' / '<') Spacing
SQUOTE < [']
DQUOTE < [\"]
DASH < '-'
OPENBRACKET < '['
CLOSEBRACKET < ']'
HEX <- [0-9a-fA-F]
NOTIN < '^'
SLASH < '/' Spacing
AND <-- '&' Spacing
NOT <-- '!' Spacing
QUESTION <-- '?' Spacing
STAR <-- '*' Spacing
PLUS <-- '+' Spacing
OPEN < '(' Spacing
CLOSE < ')' Spacing
DOT <-- '.' Spacing

Spacing < (Space / Comment)*
Comment < '#' (!EndOfLine .)* EndOfLine
Space < ' ' / '\\t' / EndOfLine
EndOfLine < '\\r\\n' / '\\n' / '\\r'
EndOfFile < !.
")


(define-syntax define-sexp-parser
  (lambda (x)
    (syntax-case x ()
      ((_ sym accum pat)
       (let* ((matchf (compile-peg-pattern #'pat (syntax->datum #'accum)))
              (accumsym (syntax->datum #'accum))
              (syn (wrap-parser-for-users x matchf accumsym #'sym)))
           #`(define sym #,syn))))))

(define-sexp-parser Grammar all
  (and Spacing (+ Definition) EndOfFile))
(define-sexp-parser Definition all
  (and Identifier LEFTARROW Expression))
(define-sexp-parser Expression all
  (and Sequence (* (and SLASH Sequence))))
(define-sexp-parser Sequence all
  (* Prefix))
(define-sexp-parser Prefix all
  (and (? (or AND NOT)) Suffix))
(define-sexp-parser Suffix all
  (and Primary (? (or QUESTION STAR PLUS))))
(define-sexp-parser Primary all
  (or (and Identifier (not-followed-by LEFTARROW))
      (and OPEN Expression CLOSE)
      Literal
      Class
      NotInClass
      DOT))
(define-sexp-parser Identifier all
  (and IdentStart (* IdentCont) Spacing))
(define-sexp-parser IdentStart body
  (or (or (range #\a #\z) (range #\A #\Z) "_") "-")) ; NOTE: - is an extension
(define-sexp-parser IdentCont body
  (or IdentStart (range #\0 #\9)))
(define-sexp-parser Literal all
  (or (and SQUOTE (* (and (not-followed-by SQUOTE) Char)) SQUOTE Spacing)
      (and DQUOTE (* (and (not-followed-by DQUOTE) Char)) DQUOTE Spacing)))
(define-sexp-parser Class all
  (and OPENBRACKET (not-followed-by NOTIN)
       (* (and (not-followed-by CLOSEBRACKET) Range)) CLOSEBRACKET Spacing))
(define-sexp-parser NotInClass all
  (and OPENBRACKET NOTIN
       (* (and (not-followed-by CLOSEBRACKET) Range)) CLOSEBRACKET Spacing))
(define-sexp-parser Range all
  (or (and Char DASH Char) Char))
(define-sexp-parser Char all
  (or (and "\\" (or "n" "r" "t" "f" "'" "\"" "[" "]" "\\"))
      (and "\\" (range #\0 #\7) (range #\0 #\7) (range #\0 #\7))
      (and "\\" (range #\0 #\7) (? (range #\0 #\7)))
      (and "\\" "u" HEX HEX HEX HEX)
      (and (not-followed-by "\\") peg-any)))
(define-sexp-parser LEFTARROW body
  (and (or "<--" "<-" "<") Spacing)) ; NOTE: <-- and < are extensions
(define-sexp-parser HEX body
  (or (range #\0 #\9) (range #\a #\f) (range #\A #\F)))
(define-sexp-parser NOTIN none
  (and "^"))
(define-sexp-parser SLASH none
  (and "/" Spacing))
(define-sexp-parser AND all
  (and "&" Spacing))
(define-sexp-parser NOT all
  (and "!" Spacing))
(define-sexp-parser QUESTION all
  (and "?" Spacing))
(define-sexp-parser STAR all
  (and "*" Spacing))
(define-sexp-parser PLUS all
  (and "+" Spacing))
(define-sexp-parser OPEN none
  (and "(" Spacing))
(define-sexp-parser CLOSE none
  (and ")" Spacing))
(define-sexp-parser DOT all
  (and "." Spacing))
(define-sexp-parser SQUOTE none "'")
(define-sexp-parser DQUOTE none "\"")
(define-sexp-parser OPENBRACKET none "[")
(define-sexp-parser CLOSEBRACKET none "]")
(define-sexp-parser DASH none "-")
(define-sexp-parser Spacing none
  (* (or Space Comment)))
(define-sexp-parser Comment none
  (and "#" (* (and (not-followed-by EndOfLine) peg-any)) EndOfLine))
(define-sexp-parser Space none
  (or " " "\t" EndOfLine))
(define-sexp-parser EndOfLine none
  (or "\r\n" "\n" "\r"))
(define-sexp-parser EndOfFile none
  (not-followed-by peg-any))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; PARSE STRING PEGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Takes a string representing a PEG grammar and returns syntax that
;; will define all of the nonterminals in the grammar with equivalent
;; PEG s-expressions.
(define (peg-parser str for-syntax)
  (let ((parsed (match-pattern Grammar str)))
    (if (not parsed)
        (begin
          ;; (display "Invalid PEG grammar!\n")
          #f)
        (let ((lst (peg:tree parsed)))
          (cond
           ((or (not (list? lst)) (null? lst))
            lst)
           ((eq? (car lst) 'Grammar)
            (Grammar->defn lst for-syntax)))))))

;; (Grammar (Definition ...) (Definition ...))
(define (Grammar->defn lst for-syntax)
  #`(begin
      #,@(map (lambda (x) (Definition->defn x for-syntax))
              (context-flatten (lambda (lst) (<= (depth lst) 1))
                               (cdr lst)))))

;; (Definition (Identifier "Something") "<-" (Expression ...))
;;  `-> (define-peg-pattern Something 'all ...)
(define (Definition->defn lst for-syntax)
  (match lst
    (('Definition ('Identifier identifier) grabber expression)
     #`(define-peg-pattern
         #,(datum->syntax for-syntax (string->symbol identifier))
         #,(match grabber
                  ("<--" (datum->syntax for-syntax 'all))
                  ("<-"  (datum->syntax for-syntax 'body))
                  ("<"   (datum->syntax for-syntax 'none)))
         #,(compressor
             (Expression->defn expression for-syntax)
             for-syntax)))))

;; (Expression X)
;;  `-> (or X)
;; (Expression X Y)
;;  `-> (or X Y)
;; (Expression X (Y Z ...))
;;  `-> (or X Y Z ...)
(define (Expression->defn lst for-syntax)
  (match lst
    (('Expression seq ...)
     #`(or #,@(map (lambda (x) (Sequence->defn x for-syntax))
                   (keyword-flatten '(Sequence) seq))))))

;; (Sequence X)
;;  `-> (and X)
;; (Sequence X Y)
;;  `-> (and X Y)
;; (Sequence X (Y Z ...))
;;  `-> (and X Y Z ...)
(define (Sequence->defn lst for-syntax)
  (match lst
    (('Sequence pre ...)
     #`(and #,@(map (lambda (x) (Prefix->defn x for-syntax))
                    (keyword-flatten '(Prefix) pre))))))

;; (Prefix (Suffix ...))
;;  `-> (...)
;; (Prefix (NOT "!") (Suffix ...))
;;  `-> (not-followed-by ...)
;; (Prefix (AND "&") (Suffix ...))
;;  `-> (followed-by ...)
(define (Prefix->defn lst for-syntax)
  (match lst
    (('Prefix ('AND _) su) #`(followed-by     #,(Suffix->defn su for-syntax)))
    (('Prefix ('NOT _) su) #`(not-followed-by #,(Suffix->defn su for-syntax)))
    (('Prefix suffix) (Suffix->defn suffix for-syntax))))

;; (Suffix (Primary ...))
;;  `-> (...)
;; (Suffix (Primary ...) (STAR "*"))
;;  `-> (* ...)
;; (Suffix (Primary ...) (QUESTION "?"))
;;  `-> (? ...)
;; (Suffix (Primary ...) (PLUS "+"))
;;  `-> (+ ...)
(define (Suffix->defn lst for-syntax)
  (match lst
    (('Suffix prim)               (Primary->defn prim for-syntax))
    (('Suffix prim ('STAR     _)) #`(* #,(Primary->defn prim for-syntax)))
    (('Suffix prim ('QUESTION _)) #`(? #,(Primary->defn prim for-syntax)))
    (('Suffix prim ('PLUS     _)) #`(+ #,(Primary->defn prim for-syntax)))))


(define (Primary->defn lst for-syntax)
  (let ((value (second lst)))
    (match (car value)
      ('DOT        #'peg-any)
      ('Identifier (Identifier->defn value for-syntax))
      ('Expression (Expression->defn value for-syntax))
      ('Literal    (Literal->defn value for-syntax))
      ('NotInClass (NotInClass->defn value for-syntax))
      ('Class      (Class->defn value for-syntax)))))

;; (Identifier "hello")
;;  `-> hello
(define (Identifier->defn lst for-syntax)
  (datum->syntax for-syntax (string->symbol (second lst))))

;; (Literal (Char "a") (Char "b") (Char "c"))
;;  `-> "abc"
(define (Literal->defn lst for-syntax)
  (apply string (map (lambda (x) (Char->defn x for-syntax)) (cdr lst))))

;; (NotInClass (Range ...) (Range ...))
;;  `-> (and (followed-by (not-in-range ...))
;;           (followed-by (not-in-range ...))
;;           ...
;;           (not-in-range ...))
;; NOTE: the order doesn't matter, because all `not-in-range`s will always
;; parse exactly one character, but all the elements but the last need not to
;; consume the input.
(define (NotInClass->defn lst for-syntax)
  #`(and
      #,@(map (lambda (x) #`(followed-by #,(NotInRange->defn x for-syntax)))
              (cddr lst))
      #,(NotInRange->defn (cadr lst) for-syntax)))

;; (Class ...)
;;  `-> (or ...)
(define (Class->defn lst for-syntax)
  #`(or #,@(map (lambda (x) (Range->defn x for-syntax))
                (cdr lst))))

;; NOTE: It's coming from NotInClass.
;; For one character:
;; (Range (Char "a"))
;;  `-> (not-in-range #\a #\a)
;; Or for a range:
;; (Range (Char "a") (Char "b"))
;;  `-> (not-in-range #\a #\b)
(define (NotInRange->defn lst for-syntax)
  (match lst
    (('Range c)
     (let ((ch (Char->defn c for-syntax)))
       #`(not-in-range #,ch #,ch)))
    (('Range range-beginning range-end)
     #`(not-in-range
         #,(Char->defn range-beginning for-syntax)
         #,(Char->defn range-end       for-syntax)))))

;; For one character:
;; (Range (Char "a"))
;;  `-> "a"
;; Or for a range:
;; (Range (Char "a") (Char "b"))
;;  `-> (range #\a #\b)
(define (Range->defn lst for-syntax)
  (match lst
    (('Range ch)
     (string (Char->defn ch for-syntax)))
    (('Range range-beginning range-end)
     #`(range
         #,(Char->defn range-beginning for-syntax)
         #,(Char->defn range-end       for-syntax)))))

;; (Char "a")
;;  `-> #\a
;; (Char "\\n")
;;  `-> #\newline
;; (Char "\\135")
;;  `-> #\]
(define (Char->defn lst for-syntax)
  (let* ((charstr (second lst))
         (first   (string-ref charstr 0)))
    (cond
      ((= 1 (string-length charstr)) first)
      ((char-numeric? (string-ref charstr 1))
       (integer->char
         (reduce + 0
                 (map
                   (lambda (x y)
                     (* (- (char->integer x) (char->integer #\0)) y))
                   (reverse (string->list charstr 1))
                   '(1 8 64)))))
      ((char=? #\u (string-ref charstr 1))
       (integer->char
         (reduce + 0
                 (map
                   (lambda (x y)
                     (* (cond
                          ((char-numeric? x)
                           (- (char->integer x) (char->integer #\0)))
                          ((char-alphabetic? x)
                           (+ 10 (- (char->integer x) (char->integer #\a)))))
                        y))
                   (reverse (string->list (string-downcase charstr) 2))
                   '(1 16 256 4096)))))
      (else
        (case (string-ref charstr 1)
          ((#\n) #\newline)
          ((#\r) #\return)
          ((#\t) #\tab)
          ((#\f) #\page)
          ((#\') #\')
          ((#\") #\")
          ((#\]) #\])
          ((#\\) #\\)
          ((#\[) #\[))))))

(define peg-grammar Grammar)

;; Macro wrapper for PEG-PARSER.  Parses PEG grammars expressed as strings and
;; defines all the appropriate nonterminals.
(define-syntax define-peg-string-patterns
  (lambda (x)
    (syntax-case x ()
      ((_ str)
       (peg-parser (syntax->datum #'str) x)))))

;; Compresses a list to save the optimizer work.
;; e.g. (or (and a)) -> a
(define (compressor-core lst)
  (if (or (not (list? lst)) (null? lst))
      lst
      (cond
       ((and (or (eq? (car lst) 'or) (eq? (car lst) 'and))
             (null? (cddr lst)))
        (compressor-core (cadr lst)))
       ((and (eq? (car lst) 'body)
             (eq? (cadr lst) 'lit)
             (eq? (cadddr lst) 1))
        (compressor-core (caddr lst)))
       (else (map compressor-core lst)))))

(define (compressor syn for-syntax)
  (datum->syntax for-syntax
                 (compressor-core (syntax->datum syn))))

;; Builds a lambda-expressions for the pattern STR using accum.
(define (peg-string-compile args accum)
  (syntax-case args ()
    ((str-stx) (string? (syntax->datum #'str-stx))
     (let ((string (syntax->datum #'str-stx)))
       (compile-peg-pattern
        (compressor
         (Expression->defn
          (peg:tree (match-pattern Expression string)) #'str-stx)
         #'str-stx)
        (if (eq? accum 'all) 'body accum))))
     (else (error "Bad embedded PEG string" args))))

(add-peg-compiler! 'peg peg-string-compile)
