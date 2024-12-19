;;; Wisp

;; Copyright (C) 2013, 2017, 2018, 2020, 2024 Free Software Foundation, Inc.
;; Copyright (C) 2014--2023 Arne Babenhauserheide.
;; Copyright (C) 2023 Maxime Devos <maximedevos@telenet.be>

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

;;; Commentary:

;; Scheme-only implementation of a wisp-preprocessor which output a
;; Scheme code tree to feed to a Scheme interpreter instead of a
;; preprocessed file.

;; Limitations:
;; - in some cases the source line information is missing in backtraces.
;;   check for set-source-property!

;;; Code:

(define-module (language wisp)
  #:export (wisp-scheme-read-chunk wisp-scheme-read-all
                                   wisp-scheme-read-file-chunk wisp-scheme-read-file
                                   wisp-scheme-read-string)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11); for let-values
  #:use-module (srfi srfi-9); for records
  #:use-module (ice-9 rw); for write-string/partial
  #:use-module (ice-9 match))

;; use curly-infix by default
(eval-when (expand load eval)
  (read-enable 'curly-infix))


;; Helpers to preserver source properties

(define (wisp-add-source-properties-from source target)
  "Copy the source properties from source into the target and return the target."
  (catch #t
    (lambda ()
      (set-source-properties! target (source-properties source)))
    (lambda (key . arguments)
      #f))
  target)

(define (wisp-add-source-properties-from/when-required source target)
  "Copy the source properties if target has none."
  (if (null? (source-properties target))
      (wisp-add-source-properties-from source target)
      target))


;; Helper functions for the indent-and-symbols data structure: '((indent token token ...) ...)
(define make-line list)

(define (line-indent line)
  (car line))

(define (line-real-indent line)
  "Get the indentation without the comment-marker for unindented lines (-1 is treated as 0)."
  (let ((indent (line-indent line)))
    (if (= -1 indent)
        0
        indent)))

(define (line-code line)
  "Strip the indentation markers from the beginning of the line and preserve source-properties"
  (let ((code (cdr line)))
    ;; propagate source properties
    (when (not (null? code))
      (wisp-add-source-properties-from/when-required line code))
    code))

;; literal values I need
(define readcolon
  (string->symbol ":"))

;; define an intermediate dot replacement with UUID to avoid clashes.
(define repr-dot ; .
  (make-symbol "wisp-dot"))

;; allow using reader additions as the first element on a line to prefix the list
(define repr-quote ; '
  (make-symbol "wisp-quote"))
(define repr-unquote ; ,
  (make-symbol "wisp-unquote"))
(define repr-quasiquote ; `
  (make-symbol "wisp-quasiquote"))
(define repr-unquote-splicing ; ,@
  (make-symbol "wisp-unquote-splicing"))

(define repr-syntax ; #'
  (make-symbol "wisp-syntax"))
(define repr-unsyntax ; #,
  (make-symbol "wisp-unsyntax"))
(define repr-quasisyntax ; #`
  (make-symbol "wisp-quasisyntax"))
(define repr-unsyntax-splicing ; #,@
  (make-symbol "wisp-unsyntax-splicing"))

;; TODO: wrap the reader to return the repr of the syntax reader
;; additions

(define (equal-rest? chars . args)
  (equal? chars args))

(define (match-charlist-to-repr char-list)
  (let ((chars (reverse char-list)))
    (cond
     ((equal-rest? chars #\.) repr-dot)
     ((equal-rest? chars #\') repr-quote)
     ((equal-rest? chars #\,) repr-unquote)
     ((equal-rest? chars #\`) repr-quasiquote)
     ((equal-rest? chars #\, #\@) repr-unquote-splicing)
     ((equal-rest? chars #\# #\') repr-syntax)
     ((equal-rest? chars #\# #\,) repr-unsyntax)
     ((equal-rest? chars #\# #\`) repr-quasisyntax)
     ((equal-rest? chars #\# #\, #\@) repr-unsyntax-splicing)
     (else #f))))

(define (wisp-read port)
  "Wrap read to catch list prefixes: read one or several chars from PORT and return read symbols or replacement-symbols as representation for special forms."
  (let ((prefix-maxlen 4))
    (let longpeek ((peeked '()) (repr-symbol #f))
      (cond
       ((or (< prefix-maxlen (length peeked))
            (eof-object? (peek-char port))
            (equal? #\space (peek-char port))
            (equal? #\newline (peek-char port)))
        (if repr-symbol ; found a special symbol, return it.
            repr-symbol
            (let unpeek ((remaining peeked))
              (cond
               ((equal? '() remaining)
                (read port)); let read to the work
               (else
                (unread-char (car remaining) port)
                (unpeek (cdr remaining)))))))
       (else
        (let* ((next-char (read-char port))
               (peeked (cons next-char peeked)))
          (longpeek
           peeked
           (match-charlist-to-repr peeked))))))))



(define (line-continues? line)
  (eq? repr-dot (car (line-code line))))

(define (line-only-colon? line)
  (and
   (equal? ":" (car (line-code line)))
   (null? (cdr (line-code line)))))

(define (line-empty-code? line)
  (null? (line-code line)))

(define (line-empty? line)
  (and
   ;; if indent is -1, we stripped a comment, so the line was not really empty.
   (= 0 (line-indent line))
   (line-empty-code? line)))

(define (line-strip-continuation line)
  (if (line-continues? line)
      (apply make-line
             (line-indent line)
             (cdr (line-code line)))
      line))

(define (line-strip-indentation-marker line)
  "Strip the indentation markers from the beginning of the line for line-finalize without propagating source-properties (those are propagated in a second step)"
  (cdr line))

(define (indent-level-reduction indentation-levels level select-fun)
  "Reduce the INDENTATION-LEVELS to the given LEVEL and return the value selected by SELECT-FUN"
  (let loop ((newlevels indentation-levels)
             (diff 0))
    (cond
     ((= level (car newlevels))
      (select-fun (list diff indentation-levels)))
     ((< level (car newlevels))
      (loop
       (cdr newlevels)
       (1+ diff)))
     (else
      (raise-exception (make-exception-from-throw 'wisp-syntax-error (list (format #f "Level ~A not found in the indentation-levels ~A." level indentation-levels))))))))

(define (indent-level-difference indentation-levels level)
  "Find how many indentation levels need to be popped off to find the given level."
  (indent-level-reduction indentation-levels level
                          (lambda (x); get the count
                            (car x))))

(define (indent-reduce-to-level indentation-levels level)
  "Find how many indentation levels need to be popped off to find the given level."
  (indent-level-reduction indentation-levels level
                          (lambda (x); get the levels
                            (car (cdr x)))))

(define (chunk-ends-with-period currentsymbols next-char)
  "Check whether indent-and-symbols ends with a period, indicating the end of a chunk."
  (and (not (null? currentsymbols))
       (equal? #\newline next-char)
       (eq? repr-dot
            (list-ref currentsymbols (- (length currentsymbols) 1)))))


(define (wisp-scheme-read-chunk-lines port)
  ;; the line number for this chunk is the line number when starting to read it
  ;; a top-level form stops processing, so we only need to retrieve this here.
  (define line-number (port-line port))
  (let loop ((indent-and-symbols (list)); '((5 "(foobar)" "\"yobble\"")(3 "#t"))
             (in-indent? #t)
             (in-underscoreindent? (equal? #\_ (peek-char port)))
             (in-comment? #f)
             (currentindent 0)
             (currentsymbols '())
             (emptylines 0))
    (cond
     ((>= emptylines 2)
      ;; the chunk end has to be checked
      ;; before we look for new chars in the
      ;; port to make execution in the REPL
      ;; after two empty lines work
      ;; (otherwise it shows one more line).
      indent-and-symbols)
     (else
      (let ((next-char (peek-char port)))
        (cond
         ((eof-object? next-char)
          (let ((line (apply make-line currentindent currentsymbols)))
            (set-source-property! line 'filename (port-filename port))
            (set-source-property! line 'line line-number)
            (append indent-and-symbols (list line))))
         ((and in-indent?
               (zero? currentindent)
               (not in-comment?)
               (not (null? indent-and-symbols))
               (not in-underscoreindent?)
               (not (or (equal? #\space next-char)
                        (equal? #\newline next-char)
                        (equal? (string-ref ";" 0) next-char))))
          (append indent-and-symbols)); top-level form ends chunk
         ((chunk-ends-with-period currentsymbols next-char)
          ;; the line ends with a period. This is forbidden in
          ;; SRFI-119. Use it to end the line in the REPL without
          ;; showing continuation dots (...).
          (append indent-and-symbols (list (apply make-line currentindent (drop-right currentsymbols 1)))))
         ((and in-indent? (equal? #\space next-char))
          (read-char port); remove char
          (loop
           indent-and-symbols
           #t ; in-indent?
           #f ; in-underscoreindent?
           #f ; in-comment?
           (1+ currentindent)
           currentsymbols
           emptylines))
         ((and in-underscoreindent? (equal? #\_ next-char))
          (read-char port); remove char
          (loop
           indent-and-symbols
           #t ; in-indent?
           #t ; in-underscoreindent?
           #f ; in-comment?
           (1+ currentindent)
           currentsymbols
           emptylines))
         ;; any char but whitespace *after* underscoreindent is
         ;; an error. This is stricter than the current wisp
         ;; syntax definition.
         ;; TODO: Fix the definition. Better start too strict.
         ;; FIXME: breaks on lines with only underscores which should be
         ;; empty lines.
         ((and in-underscoreindent? (and (not (equal? #\space next-char)) (not (equal? #\newline next-char))))
          (raise-exception (make-exception-from-throw 'wisp-syntax-error (list "initial underscores without following whitespace at beginning of the line after" (last indent-and-symbols)))))
         ((equal? #\newline next-char)
          (read-char port); remove the newline
          (let*
              ;; distinguish pure whitespace lines and lines
              ;; with comment by giving the former zero
              ;; indent. Lines with a comment at zero indent
              ;; get indent -1 for the same reason - meaning
              ;; not actually empty.
              ((indent
                (cond
                 (in-comment?
                  (if (= 0 currentindent); specialcase
                      -1
                      currentindent))
                 ((not (null? currentsymbols)); pure whitespace
                  currentindent)
                 (else
                  0)))
               (parsedline (apply make-line indent currentsymbols))
               (emptylines
                (if (not (line-empty? parsedline))
                    0
                    (1+ emptylines))))
            (when (not (= 0 (length (line-code parsedline))))
              ;; set the source properties to parsedline so we can try to add them later.
              (set-source-property! parsedline 'filename (port-filename port))
              (set-source-property! parsedline 'line line-number))
            ;; TODO: If the line is empty. Either do it here and do not add it, just
            ;; increment the empty line counter, or strip it later. Replace indent
            ;; -1 by indent 0 afterwards.
            (loop
             (append indent-and-symbols (list parsedline))
             #t ; in-indent?
             (if (<= 2 emptylines)
                 #f ; chunk ends here
                 (equal? #\_ (peek-char port))); are we in underscore indent?
             #f ; in-comment?
             0
             '()
             emptylines)))
         ((equal? #t in-comment?)
          (read-char port); remove one comment character
          (loop
           indent-and-symbols
           #f ; in-indent?
           #f ; in-underscoreindent?
           #t ; in-comment?
           currentindent
           currentsymbols
           emptylines))
         ((or (equal? #\space next-char) (equal? #\tab next-char) (equal? #\return next-char)); remove whitespace when not in indent
          (read-char port); remove char
          (loop
           indent-and-symbols
           #f ; in-indent?
           #f ; in-underscoreindent?
           #f ; in-comment?
           currentindent
           currentsymbols
           emptylines))
         ;; | cludge to appease the former wisp parser
         ;; | used for bootstrapping which has a
         ;; v problem with the literal comment char
         ((equal? (string-ref ";" 0) next-char)
          (loop
           indent-and-symbols
           #f ; in-indent?
           #f ; in-underscoreindent?
           #t ; in-comment?
           currentindent
           currentsymbols
           emptylines))
         (else ; use the reader
          (loop
           indent-and-symbols
           #f ; in-indent?
           #f ; in-underscoreindent?
           #f ; in-comment?
           currentindent
           ;; this also takes care of the hashbang and leading comments.
           (append currentsymbols (list (wisp-read port)))
           emptylines))))))))


(define (line-code-replace-inline-colons line)
  "Replace inline colons by opening parens which close at the end of the line"
  ;; format #t "replace inline colons for line ~A\n" line
  (let loop ((processed '())
             (unprocessed line))
    (cond
     ((null? unprocessed)
      ;; format #t "inline-colons processed line: ~A\n" processed
      processed)
     ;; replace : . with nothing
     ((and (<= 2 (length unprocessed))
           (equal? readcolon (car unprocessed))
           (eq? repr-dot (car (cdr unprocessed))))
      (loop
       (append processed
               (loop '() (cdr (cdr unprocessed))))
       '()))
     ((equal? readcolon (car unprocessed))
      (loop
       (append processed
               (list (loop '() (cdr unprocessed))))
       '()))
     (else
      (loop
       (append processed
               (list (car unprocessed)))
       (cdr unprocessed))))))

(define (line-replace-inline-colons line)
  (cons
   (line-indent line)
   (line-code-replace-inline-colons (line-code line))))

(define (line-strip-lone-colon line)
  "A line consisting only of a colon is just a marked indentation level. We need to kill the colon before replacing inline colons."
  (if (equal? (line-code line) (list readcolon))
      (make-line (line-indent line))
      line))

(define (line-finalize line)
  "Process all wisp-specific information in a line and strip it"
  (let ((l (line-code-replace-inline-colons
            (line-strip-indentation-marker
             (line-strip-lone-colon
              (line-strip-continuation line))))))
    (when (not (null? (source-properties line)))
      (catch #t
        (lambda ()
          (set-source-properties! l (source-properties line)))
        (lambda (key . arguments)
          #f)))
    l))

(define (wisp-propagate-source-properties code)
  "Propagate the source properties from the sourrounding list into every part of the code."
  (let loop ((processed '())
             (unprocessed code))
    (cond
     ((and (null? processed) (not (pair? unprocessed)) (not (list? unprocessed)))
      unprocessed)
     ((and (pair? unprocessed) (not (list? unprocessed)))
      (cons
       (wisp-propagate-source-properties (car unprocessed))
       (wisp-propagate-source-properties (cdr unprocessed))))
     ((null? unprocessed)
      processed)
     (else
      (let ((line (car unprocessed)))
        (wisp-add-source-properties-from/when-required line unprocessed)
        (wisp-add-source-properties-from/when-required code unprocessed)
        (wisp-add-source-properties-from/when-required unprocessed line)
        (wisp-add-source-properties-from/when-required unprocessed code)
        (let ((processed (append processed (list (wisp-propagate-source-properties line)))))
          ;; must propagate from line, because unprocessed and code can be null, then they cannot keep source-properties.
          (wisp-add-source-properties-from/when-required line processed)
          (loop processed
                (cdr unprocessed))))))))

(define* (wisp-scheme-indentation-to-parens lines)
  "Add parentheses to lines and remove the indentation markers"
  (when
      (and
       (not (null? lines))
       (not (line-empty-code? (car lines)))
       (not (= 0 (line-real-indent (car lines))))); -1 is a line with a comment
    (if (= 1 (line-real-indent (car lines)))
        ;; accept a single space as indentation of the first line (and ignore the indentation) to support meta commands
        (set! lines
              (cons
               (cons 0 (cdr (car lines)))
               (cdr lines)))
        (raise-exception
         (make-exception-from-throw
          'wisp-syntax-error
          (list
           (format #f "The first symbol in a chunk must start at zero indentation. Indentation and line: ~A"
                   (car lines)))))))
  (let loop ((processed '())
             (unprocessed lines)
             (indentation-levels '(0)))
    (let* ((current-line
            (if (<= 1 (length unprocessed))
                (car unprocessed)
                (make-line 0))); empty code
           (next-line
            (if (<= 2 (length unprocessed))
                (car (cdr unprocessed))
                (make-line 0))); empty code
           (current-indentation
            (car indentation-levels))
           (current-line-indentation (line-real-indent current-line)))
      ;; format #t "processed: ~A\ncurrent-line: ~A\nnext-line: ~A\nunprocessed: ~A\nindentation-levels: ~A\ncurrent-indentation: ~A\n\n"
      ;;     . processed current-line next-line unprocessed indentation-levels current-indentation
      (cond
       ;; the real end: this is reported to the outside world.
       ((and (null? unprocessed) (not (null? indentation-levels)) (null? (cdr indentation-levels)))
        ;; reverse the processed lines, because I use cons.
        processed)
       ;; the recursion end-condition
       ((and (null? unprocessed))
        ;; this is the last step. Nothing more to do except
        ;; for rolling up the indentation levels.  return the
        ;; new processed and unprocessed lists: this is a
        ;; side-recursion
        (values processed unprocessed))
       ((null? indentation-levels)
        (raise-exception
         (make-exception-from-throw
          'wisp-programming-error
          (list
           "The indentation-levels are null but the current-line is null: Something killed the indentation-levels."))))
       (else ; now we come to the line-comparisons and indentation-counting.
        (cond
         ((line-empty-code? current-line)
          ;; We cannot process indentation without
          ;; code. Just switch to the next line. This should
          ;; only happen at the start of the recursion.
          (loop
           processed
           (cdr unprocessed)
           indentation-levels))
         ((and (line-empty-code? next-line) (<= 2 (length unprocessed)))
          ;; take out the next-line from unprocessed.
          (loop
           processed
           (cons current-line
                 (cdr (cdr unprocessed)))
           indentation-levels))
         ((> current-indentation current-line-indentation)
          ;; this just steps back one level via the side-recursion.
          (let ((previous-indentation (car (cdr indentation-levels))))
            (if (<= current-line-indentation previous-indentation)
                (values processed unprocessed)
                (begin ;; not yet used level! TODO: maybe throw an error here instead of a warning.
                  (let ((linenumber (- (length lines) (length unprocessed))))
                    (format (current-error-port) ";;; WARNING:~A: used lower but undefined indentation level (line ~A of the current chunk: ~S). This makes refactoring much more error-prone, therefore it might become an error in a later version of Wisp.\n" (source-property current-line 'line) linenumber (cdr current-line)))
                  (loop
                   processed
                   unprocessed
                   (cons ; recursion via the indentation-levels
                    current-line-indentation
                    (cdr indentation-levels)))))))
         ((= current-indentation current-line-indentation)
          (let ((line (line-finalize current-line))
                (next-line-indentation (line-real-indent next-line)))
            (cond
             ((>= current-line-indentation next-line-indentation)
              ;; simple recursiive step to the next line
              (loop
               (append processed
                       (if (line-continues? current-line)
                           line
                           (wisp-add-source-properties-from line (list line))))
               (cdr unprocessed); recursion here
               indentation-levels))
             ((< current-line-indentation next-line-indentation)
              ;; side-recursion via a sublist
              (let-values
                  (((sub-processed sub-unprocessed)
                    (loop
                     line
                     (cdr unprocessed); recursion here
                     indentation-levels)))
                (loop
                 (append processed (list sub-processed))
                 sub-unprocessed ; simply use the recursion from the sub-recursion
                 indentation-levels))))))
         ((< current-indentation current-line-indentation)
          (loop
           processed
           unprocessed
           (cons ; recursion via the indentation-levels
            current-line-indentation
            indentation-levels)))
         (else
          (raise-exception
           (make-exception-from-throw
            'wisp-not-implemented
            (list
             (format #f "Need to implement further line comparison: current: ~A, next: ~A, processed: ~A."
                     current-line next-line processed)))))))))))


(define (wisp-scheme-replace-inline-colons lines)
  "Replace inline colons by opening parens which close at the end of the line"
  (let loop ((processed '())
             (unprocessed lines))
    (if (null? unprocessed)
        processed
        (loop
         (append processed (list (line-replace-inline-colons (car unprocessed))))
         (cdr unprocessed)))))


(define (wisp-scheme-strip-indentation-markers lines)
  "Strip the indentation markers from the beginning of the lines"
  (let loop ((processed '())
             (unprocessed lines))
    (if (null? unprocessed)
        processed
        (loop
         (append processed (cdr (car unprocessed)))
         (cdr unprocessed)))))

(define (wisp-unescape-underscore-and-colon code)
  "replace \\_ and \\: by _ and :"
  (wisp-add-source-properties-from/when-required
   code
   (cond ((list? code) (map wisp-unescape-underscore-and-colon code))
         ((eq? code '\:) ':)
         ;; Look for symbols like \____ and remove the \.
         ((symbol? code)
          (let ((as-string (symbol->string code)))
            (if (and (>= (string-length as-string) 2) ; at least a single underscore
                     (char=? (string-ref as-string 0) #\\)
                     (string-every #\_ (substring as-string 1)))
                (string->symbol (substring as-string 1))
                code)))
         (#t code))))


(define (wisp-replace-empty-eof code)
  "replace ((#<eof>)) by ()"
  ;; This is a hack which fixes a bug when the
  ;; parser hits files with only hashbang and comments.
  (if (and (not (null? code)) (pair? (car code)) (eof-object? (car (car code))) (null? (cdr code)) (null? (cdr (car code))))
      (wisp-add-source-properties-from code (list))
      code))


(define (wisp-replace-paren-quotation-repr code)
  "Replace lists starting with a quotation symbol by quoted lists."
  (define (pred value)
    (lambda (x)
      (eq? x value)))

  (wisp-add-source-properties-from/when-required
   code
   (match code
     (((? (pred repr-quote)) a ...)
      (list 'quote (map wisp-replace-paren-quotation-repr a)))
     ((a ... (? (pred repr-quote)) b); this is the quoted empty list
      (append
       (map wisp-replace-paren-quotation-repr a)
       (list (list 'quote (map wisp-replace-paren-quotation-repr b)))))
     (((? (pred repr-quasiquote)) (? (pred repr-unquote)) a ...)
      (list 'quasiquote (list 'unquote (map wisp-replace-paren-quotation-repr a))))
     (((? (pred repr-unquote)) a ...)
      (list 'unquote (map wisp-replace-paren-quotation-repr a)))
     ((a ... (? (pred repr-unquote)) b)
      (append
       (map wisp-replace-paren-quotation-repr a)
       (list (list 'unquote (map wisp-replace-paren-quotation-repr b)))))
     (((? (pred repr-quasiquote)) a ...)
      (list 'quasiquote (map wisp-replace-paren-quotation-repr a)))
     ((a ... (? (pred repr-quasiquote)) b) ;this is the quoted empty list
      (append
       (map wisp-replace-paren-quotation-repr a)
       (list (list 'quasiquote (map wisp-replace-paren-quotation-repr b)))))
     (((? (pred repr-unquote-splicing)) a ...)
      (list 'unquote-splicing (map wisp-replace-paren-quotation-repr a)))
     (((? (pred repr-syntax)) a ...)
      (list 'syntax (map wisp-replace-paren-quotation-repr a)))
     (((? (pred repr-unsyntax)) a ...)
      (list 'unsyntax (map wisp-replace-paren-quotation-repr a)))
     (((? (pred repr-quasisyntax)) a ...)
      (list 'quasisyntax (map wisp-replace-paren-quotation-repr a)))
     (((? (pred repr-unsyntax-splicing)) a ...)
      (list 'unsyntax-splicing (map wisp-replace-paren-quotation-repr a)))
     ;; literal array as start of a line: # (a b) c -> (#(a b) c)
     ((#\# a ...)
      (with-input-from-string ;; hack to defer to read
          (string-append "#"
                         (with-output-to-string
                           (λ ()
                             (write (map wisp-replace-paren-quotation-repr a)
                                    (current-output-port)))))
        read))
     ((a ...)
      (map wisp-replace-paren-quotation-repr a))
     (a
      a))))

(define (wisp-make-improper code)
  "Turn (a #{.}# b) into the correct (a . b).

read called on a single dot creates a variable named #{.}# (|.|
in r7rs). Due to parsing the indentation before the list
structure is known, the reader cannot create improper lists
when it reads a dot. So we have to take another pass over the
code to recreate the improper lists.

Match is awesome!"
  (define (dot? x)
    (eq? repr-dot x))

  (define is-proper? #t)
  ;; local alias
  (define (add-prop/req form)
    (wisp-add-source-properties-from/when-required code form))

  (wisp-add-source-properties-from/when-required
   code
   (let ((improper
          (match code
            ((a ... b (? dot?) c)
             (set! is-proper? #f)
             (wisp-add-source-properties-from/when-required
              code
              (append (map wisp-make-improper (map add-prop/req a))
                      (cons (wisp-make-improper (add-prop/req b))
                            (wisp-make-improper (add-prop/req c))))))
            ((a ...)
             (add-prop/req
              (map wisp-make-improper (map add-prop/req a))))
            (a
             a))))
     (define (syntax-error li msg)
       (raise-exception
        (make-exception-from-throw
         'wisp-syntax-error
         (list (format #f "incorrect dot-syntax #{.}# in code: ~A: ~A" msg li)))))

     (if is-proper?
         improper
         (let check ((tocheck improper))
           (match tocheck
             ;; lists with only one member
             (((? dot?))
              (syntax-error tocheck "list with the period as only member"))
             ;; list with remaining dot.
             ((a ...)
              (if (and (member repr-dot a))
                  (syntax-error tocheck "leftover period in list")
                  (map check a)))
             ;; simple pair - this and the next do not work when parsed from wisp-scheme itself. Why?
             (((? dot?) . c)
              (syntax-error tocheck "dot as first element in already improper pair"))
             ;; simple pair, other way round
             ((a . (? dot?))
              (syntax-error tocheck "dot as last element in already improper pair"))
             ;; more complex pairs
             ((? pair? a)
              (let ((head (drop-right a 1))
                    (tail (last-pair a)))
                (cond
                 ((eq? repr-dot (car tail))
                  (syntax-error tocheck "equal? repr-dot : car tail"))
                 ((eq? repr-dot (cdr tail))
                  (syntax-error tocheck "equal? repr-dot : cdr tail"))
                 ((memq repr-dot head)
                  (syntax-error tocheck "member repr-dot head"))
                 (else
                  a))))
             (a
              a)))))))

(define (wisp-scheme-read-chunk port)
  "Read and parse one chunk of wisp-code"
  (with-fluids ((%read-hash-procedures (fluid-ref %read-hash-procedures)))
    (read-hash-extend #\# (lambda args #\#))
    (let ((lines (wisp-scheme-read-chunk-lines port)))
      (wisp-make-improper
       (wisp-replace-empty-eof
        (wisp-unescape-underscore-and-colon
         (wisp-replace-paren-quotation-repr
          (wisp-propagate-source-properties
           (wisp-scheme-indentation-to-parens lines)))))))))

(define (wisp-scheme-read-all port)
  "Read all chunks from the given port"
  (let loop ((tokens '()))
    (cond
     ((eof-object? (peek-char port))
      tokens)
     (else
      (loop
       (append tokens (wisp-scheme-read-chunk port)))))))

(define (wisp-scheme-read-file path)
  (call-with-input-file path wisp-scheme-read-all))

(define (wisp-scheme-read-file-chunk path)
  (call-with-input-file path wisp-scheme-read-chunk))

(define (wisp-scheme-read-string str)
  (call-with-input-string str wisp-scheme-read-all))

(define (wisp-scheme-read-string-chunk str)
  (call-with-input-string str wisp-scheme-read-chunk))
