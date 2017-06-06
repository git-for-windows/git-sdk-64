;;;; (texinfo string-utils) -- text filling and wrapping 
;;;;
;;;;    Copyright (C) 2009, 2013  Free Software Foundation, Inc.
;;;;    Copyright (C) 2003  Richard Todd
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
;; Module @samp{(texinfo string-utils)} provides various string-related
;; functions useful to Guile's texinfo support.
;;; Code:

(define-module (texinfo string-utils)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:export (escape-special-chars
            transform-string
            expand-tabs
            center-string
            left-justify-string
            right-justify-string
            collapse-repeated-chars
            make-text-wrapper
            fill-string
            string->wrapped-lines))

(define* (transform-string str match? replace #:optional (start #f) (end #f))
"Uses @var{match?} against each character in @var{str}, and performs a
replacement on each character for which matches are found.

@var{match?} may either be a function, a character, a string, or
@code{#t}.  If @var{match?}  is a function, then it takes a single
character as input, and should return @samp{#t} for matches.
@var{match?} is a character, it is compared to each string character
using @code{char=?}.  If @var{match?} is a string, then any character
in that string will be considered a match.  @code{#t} will cause 
every character to be a match.

If @var{replace} is a function, it is called with the matched
character as an argument, and the returned value is sent to the output
string via @samp{display}.  If @var{replace} is anything else, it is
sent through the output string via @samp{display}.

Note that te replacement for the matched characters does not need to
be a single character.  That is what differentiates this function from
@samp{string-map}, and what makes it useful for applications such as
converting @samp{#\\&} to @samp{\"&amp;\"} in web page text.  Some other
functions in this module are just wrappers around common uses of
@samp{transform-string}.  Transformations not possible with this
function should probably be done with regular expressions.

If @var{start} and @var{end} are given, they control which portion
of the string undergoes transformation.  The entire input string
is still output, though.  So, if @var{start} is @samp{5}, then the
first five characters of @var{str} will still appear in the returned
string.

@lisp
; these two are equivalent...
 (transform-string str #\\space #\\-) ; change all spaces to -'s
 (transform-string str (lambda (c) (char=? #\\space c)) #\\-)
@end lisp"
  ;;  I had implemented this with string-fold, but it was
  ;; slower...
  (let* ((os (open-output-string))
         (matcher (cond ((char? match?)
                         (lambda (c) (char=? match? c)))
                        ((procedure? match?)
                         match?)
                        ((string? match?)
                         (lambda (c) (string-index match? c)))
                        ((boolean? match?)
                         (lambda (c) match?))
                        (else (throw 'bad-type "expected #t, char, string, or procedure"))))
         (replacer (if (procedure? replace)
                       (lambda (c) (display (replace c) os))
                       (lambda (c) (display replace os)))))

    ;; put the first part in, un-transformed if they asked for it...
    (if (and start (<= start (string-length str)))
        (display (substring str 0 start) os))

    ;; process the portion they want processed....
    (string-for-each
     (lambda (c)
       (if (matcher c)
           ;; we have a match! replace the char as directed...
           (replacer c)

           ;; not a match, just insert the character itself...
           (write-char c os)))
     str
     (or start 0)
     (or end (string-length str)))

    ;; if there was any at the end, tack it on...
    (if (and end (< end (string-length str)))
        (display (substring str end) os))

    (get-output-string os)))

(define* (expand-tabs str #:optional (tab-size 8))
"Returns a copy of @var{str} with all tabs expanded to spaces.  @var{tab-size} defaults to 8.

Assuming tab size of 8, this is equivalent to: @lisp
 (transform-string str #\\tab \"        \")
@end lisp"
  (transform-string str 
                    #\tab
                    (make-string tab-size #\space)))

(define (escape-special-chars str special-chars escape-char)
"Returns a copy of @var{str} with all given special characters preceded
by the given @var{escape-char}.

@var{special-chars} can either be a single character, or a string consisting
of all the special characters.

@lisp
;; make a string regexp-safe...
 (escape-special-chars \"***(Example String)***\"  
                      \"[]()/*.\" 
                      #\\\\)
=> \"\\\\*\\\\*\\\\*\\\\(Example String\\\\)\\\\*\\\\*\\\\*\"

;; also can escape a singe char...
 (escape-special-chars \"richardt@@vzavenue.net\"
                      #\\@@
                      #\\@@)
=> \"richardt@@@@vzavenue.net\"
@end lisp"
  (transform-string str
                    (if (char? special-chars)
                        ;; if they gave us a char, use char=?
                        (lambda (c) (char=? c special-chars))

                        ;; if they gave us a string, see if our character is in it
                        (lambda (c) (string-index special-chars c)))

                    ;; replace matches with the character preceded by the escape character
                    (lambda (c) (string escape-char c))))

(define* (center-string str #:optional (width 80) (chr #\space) (rchr #f))
"Returns a copy of @var{str} centered in a field of @var{width}
characters.  Any needed padding is done by character @var{chr}, which
defaults to @samp{#\\space}.  If @var{rchr} is provided, then the
padding to the right will use it instead.  See the examples below.
left and @var{rchr} on the right.  The default @var{width} is 80.  The
default @var{chr} and @var{rchr} is @samp{#\\space}.  The string is
never truncated.
@lisp
 (center-string \"Richard Todd\" 24)
=> \"      Richard Todd      \"

 (center-string \" Richard Todd \" 24 #\\=)
=> \"===== Richard Todd =====\"

 (center-string \" Richard Todd \" 24 #\\< #\\>)
=> \"<<<<< Richard Todd >>>>>\"
@end lisp"
  (let* ((len (string-length str))
         (lpad (make-string (max (quotient (- width len) 2) 0) chr))
         ;; right-char == char unless it has been provided by the user
         (right-chr (or rchr chr))
         (rpad (if (char=? right-chr chr)
                   lpad
                   (make-string (max (quotient (- width len) 2) 0) right-chr))))
    (if (>= len width)
        str
        (string-append lpad str rpad (if (odd? (- width len)) (string right-chr) "")))))

(define* (left-justify-string str #:optional (width 80) (chr #\space))
"@code{left-justify-string str [width chr]}.  
Returns a copy of @var{str} padded with @var{chr} such that it is left
justified in a field of @var{width} characters.  The default
@var{width} is 80.  Unlike @samp{string-pad} from srfi-13, the string
is never truncated."
  (let* ((len (string-length str))
         (pad (make-string (max (- width len) 0) chr)))
    (if (>= len width)
        str
        (string-append str pad))))

(define* (right-justify-string str #:optional (width 80) (chr #\space))
"Returns a copy of @var{str} padded with @var{chr} such that it is
right justified in a field of @var{width} characters.  The default
@var{width} is 80.  The default @var{chr} is @samp{#\\space}.  Unlike
@samp{string-pad} from srfi-13, the string is never truncated."
  (let* ((len (string-length str))
         (pad (make-string (max (- width len) 0) chr)))
    (if (>= len width)
        str
        (string-append pad str))))

 (define* (collapse-repeated-chars str #:optional (chr #\space) (num 1))
"Returns a copy of @var{str} with all repeated instances of 
@var{chr} collapsed down to at most @var{num} instances.
The default value for @var{chr} is @samp{#\\space}, and 
the default value for @var{num} is 1.

@lisp
 (collapse-repeated-chars \"H  e  l  l  o\")
=> \"H e l l o\"
 (collapse-repeated-chars \"H--e--l--l--o\" #\\-)
=> \"H-e-l-l-o\"
 (collapse-repeated-chars \"H-e--l---l----o\" #\\- 2)
=> \"H-e--l--l--o\"
@end lisp"
   ;; define repeat-locator as a stateful match? function which remembers
   ;; the last character it had seen.
   (let ((repeat-locator
          ;; initialize prev-chr to something other than what we're seeking...
          (let ((prev-chr (if (char=? chr #\space) #\A #\space))
                (match-count 0))
            (lambda (c)
              (if (and (char=? c prev-chr)
                       (char=? prev-chr chr))
                  ;; found enough duplicates if the match-count is high enough
                  (begin
                    (set! match-count (+ 1 match-count))
                    (>= match-count num))

                  ;; did not find a duplicate
                  (begin (set! match-count 0) 
                         (set! prev-chr c) 
                         #f))))))

     ;; transform the string with our stateful matcher...
     ;; deleting matches...
     (transform-string str repeat-locator "")))

;; split a text string into segments that have the form...
;;  <ws non-ws>  <ws non-ws> etc..
(define (split-by-single-words str)
  (let ((non-wschars (char-set-complement char-set:whitespace)))
    (let loop ((ans '())
               (index 0))
      (let ((next-non-ws (string-index str non-wschars index)))
        (if next-non-ws
          ;; found non-ws...look for ws following...
          (let ((next-ws (string-index str char-set:whitespace next-non-ws)))
            (if next-ws
                ;; found the ws following...
                (loop (cons (substring str index next-ws) ans)
                      next-ws)
                ;; did not find ws...must be the end...
                (reverse (cons (substring str index) ans))))
          ;; did not find non-ws... only ws at end of the string...
          (reverse ans))))))

(define (end-of-sentence? str)
  "Return #t when STR likely denotes the end of sentence."
  (let ((len (string-length str)))
    (and (> len 1)
         (eqv? #\. (string-ref str (- len 1)))
         (not (eqv? #\. (string-ref str (- len 2)))))))

(define* (make-text-wrapper #:key
                            (line-width 80)
                            (expand-tabs? #t)
                            (tab-width 8)
                            (collapse-whitespace? #t)
                            (subsequent-indent "")
                            (initial-indent "")
                            (break-long-words? #t))
  "Returns a procedure that will split a string into lines according to the
given parameters.

@table @code
@item #:line-width
This is the target length used when deciding where to wrap lines.
Default is 80.

@item #:expand-tabs?
Boolean describing whether tabs in the input should be expanded. Default
is #t.

@item #:tab-width
If tabs are expanded, this will be the number of spaces to which they
expand. Default is 8.

@item #:collapse-whitespace?
Boolean describing whether the whitespace inside the existing text
should be removed or not.  Default is #t.

If text is already well-formatted, and is just being wrapped to fit in a
different width, then set this to @samp{#f}. This way, many common text
conventions (such as two spaces between sentences) can be preserved if
in the original text. If the input text spacing cannot be trusted, then
leave this setting at the default, and all repeated whitespace will be
collapsed down to a single space.

@item #:initial-indent
Defines a string that will be put in front of the first line of wrapped
text. Default is the empty string, ``''.

@item #:subsequent-indent
Defines a string that will be put in front of all lines of wrapped
text, except the first one.  Default is the empty string, ``''.

@item #:break-long-words?
If a single word is too big to fit on a line, this setting tells the
wrapper what to do.  Defaults to #t, which will break up long words.
When set to #f, the line will be allowed, even though it is longer
than the defined @code{#:line-width}.
@end table

The return value is a procedure of one argument, the input string, which
returns a list of strings, where each element of the list is one line."
  (lambda (str)
    ;; replace newlines with spaces
    (set! str (transform-string str (lambda (c) (char=? c #\nl)) #\space))

    ;; expand tabs if they wanted us to...
    (if expand-tabs?
        (set! str (expand-tabs str tab-width)))

    ;; collapse whitespace if they wanted us to...
    (if collapse-whitespace?
        (set! str (collapse-repeated-chars str)))
  
    ;; drop any whitespace from the front...
    (set! str (string-trim str))

    ;; now start breaking the text into lines...
    (let loop ((ans '())
               (words (split-by-single-words str))
               (line initial-indent)
               (count 0))
      (if (null? words)
          ;; out of words? ...done!
          (reverse (if (> count 0)
                       (cons line ans)
                       ans))
        
          ;; not out of words...keep going...
          (let ((length-left (- line-width
                                (string-length line)))
                (next-word (if (= count 0)
                               (string-trim (car words))
                               (car words))))
            (cond 
             ;; does the next entry fit?
             ((<= (string-length next-word)
                  length-left)
              (loop ans
                    (cdr words)
                    (if (and collapse-whitespace?
                             (end-of-sentence? line))
                        ;; Add an extra space after the period.
                        (string-append line " " next-word)
                        (string-append line next-word))
                    (+ count 1)))

             ;; ok, it didn't fit...is there already at least one word on the line?
             ((> count 0)
              ;; try to use it for the next line, then...
              (loop (cons line ans)
                    words
                    subsequent-indent
                    0))
           
             ;; ok, it didn't fit...and it's the first word. 
             ;; were we told to break up long words?
             (break-long-words?
              ;; break the like at the limit, since the user wants us to...
              (loop (cons (string-append line (substring next-word 0 length-left))
                          ans)
                    (cons (substring next-word length-left)
                          (cdr words))
                    subsequent-indent
                    0))

             ;; well, then is it the first word and we *shouldn't* break long words, then...
             (else
              (loop (cons (string-append line next-word)
                          ans)
                    (cdr words)
                    subsequent-indent
                    0))))))))

(define (string->wrapped-lines str . kwargs)
  "@code{string->wrapped-lines str keywds ...}. Wraps the text given in
string @var{str} according to the parameters provided in @var{keywds},
or the default setting if they are not given. Returns a list of strings
representing the formatted lines. Valid keyword arguments are discussed
in @code{make-text-wrapper}."
  ((apply make-text-wrapper kwargs) str))

(define (fill-string str . kwargs)
  "Wraps the text given in string @var{str} according to the parameters
provided in @var{kwargs}, or the default setting if they are not
given.  Returns a single string with the wrapped text.  Valid keyword
arguments are discussed in @code{make-text-wrapper}."
  (string-join (apply string->wrapped-lines str kwargs)
               "\n"
               'infix))
