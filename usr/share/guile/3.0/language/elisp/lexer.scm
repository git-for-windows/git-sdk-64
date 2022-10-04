;;; Guile Emacs Lisp

;;; Copyright (C) 2009, 2010, 2013 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language elisp lexer)
  #:use-module (ice-9 regex)
  #:export (get-lexer get-lexer/1))

;;; This is the lexical analyzer for the elisp reader.  It is
;;; hand-written instead of using some generator.  I think this is the
;;; best solution because of all that fancy escape sequence handling and
;;; the like.
;;;
;;; Characters are handled internally as integers representing their
;;; code value.  This is necessary because elisp allows a lot of fancy
;;; modifiers that set certain high-range bits and the resulting values
;;; would not fit into a real Scheme character range.  Additionally,
;;; elisp wants characters as integers, so we just do the right thing...
;;;
;;; TODO: #@count comments

;;; Report an error from the lexer (that is, invalid input given).

(define (lexer-error port msg . args)
  (apply error msg args))

;;; In a character, set a given bit.  This is just some bit-wise or'ing
;;; on the characters integer code and converting back to character.

(define (set-char-bit chr bit)
  (logior chr (ash 1 bit)))

;;; Check if a character equals some other.  This is just like char=?
;;; except that the tested one could be EOF in which case it simply
;;; isn't equal.

(define (is-char? tested should-be)
  (and (not (eof-object? tested))
       (char=? tested should-be)))

;;; For a character (as integer code), find the real character it
;;; represents or #\nul if out of range.  This is used to work with
;;; Scheme character functions like char-numeric?.

(define (real-character chr)
  (if (< chr 256)
      (integer->char chr)
      #\nul))

;;; Return the control modified version of a character.  This is not
;;; just setting a modifier bit, because ASCII conrol characters must be
;;; handled as such, and in elisp C-? is the delete character for
;;; historical reasons.  Otherwise, we set bit 26.

(define (add-control chr)
  (let ((real (real-character chr)))
    (if (char-alphabetic? real)
        (- (char->integer (char-upcase real)) (char->integer #\@))
        (case real
          ((#\?) 127)
          ((#\@) 0)
          (else (set-char-bit chr 26))))))

;;; Parse a charcode given in some base, basically octal or hexadecimal
;;; are needed.  A requested number of digits can be given (#f means it
;;; does not matter and arbitrary many are allowed), and additionally
;;; early return allowed (if fewer valid digits are found).  These
;;; options are all we need to handle the \u, \U, \x and \ddd (octal
;;; digits) escape sequences.

(define (charcode-escape port base digits early-return)
  (let iterate ((result 0)
                (procdigs 0))
    (if (and digits (>= procdigs digits))
        result
        (let* ((cur (read-char port))
               (value (cond
                       ((char-numeric? cur)
                        (- (char->integer cur) (char->integer #\0)))
                       ((char-alphabetic? cur)
                        (let ((code (- (char->integer (char-upcase cur))
                                       (char->integer #\A))))
                          (if (< code 0)
                              #f
                              (+ code 10))))
                       (else #f)))
               (valid (and value (< value base))))
          (if (not valid)
              (if (or (not digits) early-return)
                  (begin
                    (unread-char cur port)
                    result)
                  (lexer-error port
                               "invalid digit in escape-code"
                               base
                               cur))
              (iterate (+ (* result base) value) (1+ procdigs)))))))

;;; Read a character and process escape-sequences when necessary.  The
;;; special in-string argument defines if this character is part of a
;;; string literal or a single character literal, the difference being
;;; that in strings the meta modifier sets bit 7, while it is bit 27 for
;;; characters.

(define basic-escape-codes
  '((#\a . 7)
    (#\b . 8)
    (#\t . 9)
    (#\n . 10)
    (#\v . 11)
    (#\f . 12)
    (#\r . 13)
    (#\e . 27)
    (#\s . 32)
    (#\d . 127)))

(define (get-character port in-string)
  (let ((meta-bits `((#\A . 22)
                     (#\s . 23)
                     (#\H . 24)
                     (#\S . 25)
                     (#\M . ,(if in-string 7 27))))
        (cur (read-char port)))
    (if (char=? cur #\\)
        ;; Handle an escape-sequence.
        (let* ((escaped (read-char port))
               (esc-code (assq-ref basic-escape-codes escaped))
               (meta (assq-ref meta-bits escaped)))
          (cond
           ;; Meta-check must be before esc-code check because \s- must
           ;; be recognized as the super-meta modifier if a - follows.
           ;; If not, it will be caught as \s -> space escape code.
           ((and meta (is-char? (peek-char port) #\-))
            (if (not (char=? (read-char port) #\-))
                (error "expected - after control sequence"))
            (set-char-bit (get-character port in-string) meta))
           ;; One of the basic control character escape names?
           (esc-code esc-code)
           ;; Handle \ddd octal code if it is one.
           ((and (char>=? escaped #\0) (char<? escaped #\8))
            (begin
              (unread-char escaped port)
              (charcode-escape port 8 3 #t)))
           ;; Check for some escape-codes directly or otherwise use the
           ;; escaped character literally.
           (else
            (case escaped
              ((#\^) (add-control (get-character port in-string)))
              ((#\C)
               (if (is-char? (peek-char port) #\-)
                   (begin
                     (if (not (char=? (read-char port) #\-))
                         (error "expected - after control sequence"))
                     (add-control (get-character port in-string)))
                   escaped))
              ((#\x) (charcode-escape port 16 #f #t))
              ((#\u) (charcode-escape port 16 4 #f))
              ((#\U) (charcode-escape port 16 8 #f))
              (else (char->integer escaped))))))
        ;; No escape-sequence, just the literal character.  But remember
        ;; to get the code instead!
        (char->integer cur))))

;;; Read a symbol or number from a port until something follows that
;;; marks the start of a new token (like whitespace or parentheses).
;;; The data read is returned as a string for further conversion to the
;;; correct type, but we also return what this is
;;; (integer/float/symbol).  If any escaped character is found, it must
;;; be a symbol.  Otherwise we at the end check the result-string
;;; against regular expressions to determine if it is possibly an
;;; integer or a float.

(define integer-regex (make-regexp "^[+-]?[0-9]+\\.?$"))

(define float-regex
  (make-regexp
   "^[+-]?([0-9]+\\.?[0-9]*|[0-9]*\\.?[0-9]+)(e[+-]?[0-9]+)?$"))

;;; A dot is also allowed literally, only a single dort alone is parsed
;;; as the 'dot' terminal for dotted lists.

(define no-escape-punctuation (string->char-set "-+=*/_~!@$%^&:<>{}?."))

(define (get-symbol-or-number port)
  (let iterate ((result-chars '())
                (had-escape #f))
    (let* ((c (read-char port))
           (finish (lambda ()
                     (let ((result (list->string
                                    (reverse result-chars))))
                       (values
                        (cond
                         ((and (not had-escape)
                               (regexp-exec integer-regex result))
                          'integer)
                         ((and (not had-escape)
                               (regexp-exec float-regex result))
                          'float)
                         (else 'symbol))
                        result))))
           (need-no-escape? (lambda (c)
                              (or (char-numeric? c)
                                  (char-alphabetic? c)
                                  (char-set-contains?
                                   no-escape-punctuation
                                   c)))))
      (cond
       ((eof-object? c) (finish))
       ((need-no-escape? c) (iterate (cons c result-chars) had-escape))
       ((char=? c #\\) (iterate (cons (read-char port) result-chars) #t))
       (else
        (unread-char c port)
        (finish))))))

;;; Parse a circular structure marker without the leading # (which was
;;; already read and recognized), that is, a number as identifier and
;;; then either = or #.

(define (get-circular-marker port)
  (call-with-values
      (lambda ()
        (let iterate ((result 0))
          (let ((cur (read-char port)))
            (if (char-numeric? cur)
                (let ((val (- (char->integer cur) (char->integer #\0))))
                  (iterate (+ (* result 10) val)))
                (values result cur)))))
    (lambda (id type)
      (case type
        ((#\#) `(circular-ref . ,id))
        ((#\=) `(circular-def . ,id))
        (else (lexer-error port
                           "invalid circular marker character"
                           type))))))

;;; Main lexer routine, which is given a port and does look for the next
;;; token.

(define lexical-binding-regexp
  (make-regexp
   "-\\*-(|.*;)[ \t]*lexical-binding:[ \t]*([^;]*[^ \t;]).*-\\*-"))

(define (lex port)
  (define (lexical-binding-value string)
    (and=> (regexp-exec lexical-binding-regexp string)
           (lambda (match)
             (not (member (match:substring match 2) '("nil" "()"))))))
  (let* ((return (let ((file (if (file-port? port)
                                 (port-filename port)
                                 #f))
                       (line (1+ (port-line port)))
                       (column (1+ (port-column port))))
                   (lambda (token value)
                     (let ((obj (cons token value)))
                       (set-source-property! obj 'filename file)
                       (set-source-property! obj 'line line)
                       (set-source-property! obj 'column column)
                       obj))))
         ;; Read afterwards so the source-properties are correct above
         ;; and actually point to the very character to be read.
         (c (read-char port)))
    (cond
     ;; End of input must be specially marked to the parser.
     ((eof-object? c) (return 'eof c))
     ;; Whitespace, just skip it.
     ((char-whitespace? c) (lex port))
     ;; The dot is only the one for dotted lists if followed by
     ;; whitespace.  Otherwise it is considered part of a number of
     ;; symbol.
     ((and (char=? c #\.)
           (char-whitespace? (peek-char port)))
      (return 'dot #f))
     ;; Continue checking for literal character values.
     (else
      (case c
        ;; A line comment, skip until end-of-line is found.
        ((#\;)
         (if (= (port-line port) 0)
             (let iterate ((chars '()))
               (let ((cur (read-char port)))
                 (if (or (eof-object? cur) (char=? cur #\newline))
                     (let ((string (list->string (reverse chars))))
                       (return 'set-lexical-binding-mode!
                               (lexical-binding-value string)))
                     (iterate (cons cur chars)))))
             (let iterate ()
               (let ((cur (read-char port)))
                 (if (or (eof-object? cur) (char=? cur #\newline))
                     (lex port)
                     (iterate))))))
        ;; A character literal.
        ((#\?)
         (return 'character (get-character port #f)))
        ;; A literal string.  This is mainly a sequence of characters
        ;; just as in the character literals, the only difference is
        ;; that escaped newline and space are to be completely ignored
        ;; and that meta-escapes set bit 7 rather than bit 27.
        ((#\")
         (let iterate ((result-chars '()))
           (let ((cur (read-char port)))
             (case cur
               ((#\")
                (return 'string (list->string (reverse result-chars))))
               ((#\\)
                (let ((escaped (read-char port)))
                  (case escaped
                    ((#\newline #\space)
                     (iterate result-chars))
                    (else
                     (unread-char escaped port)
                     (unread-char cur port)
                     (iterate
                      (cons (integer->char (get-character port #t))
                            result-chars))))))
               (else (iterate (cons cur result-chars)))))))
        ((#\#)
         (let ((c (read-char port)))
          (case c
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
             (unread-char c port)
             (let ((mark (get-circular-marker port)))
               (return (car mark) (cdr mark))))
            ((#\')
             (return 'function #f))
            ((#\:)
             (call-with-values
                 (lambda () (get-symbol-or-number port))
               (lambda (type str)
                 (return 'symbol (make-symbol str))))))))
        ;; Parentheses and other special-meaning single characters.
        ((#\() (return 'paren-open #f))
        ((#\)) (return 'paren-close #f))
        ((#\[) (return 'square-open #f))
        ((#\]) (return 'square-close #f))
        ((#\') (return 'quote #f))
        ((#\`) (return 'backquote #f))
        ;; Unquote and unquote-splicing.
        ((#\,)
         (if (is-char? (peek-char port) #\@)
             (if (not (char=? (read-char port) #\@))
                 (error "expected @ in unquote-splicing")
                 (return 'unquote-splicing #f))
             (return 'unquote #f)))
        ;; Remaining are numbers and symbols.  Process input until next
        ;; whitespace is found, and see if it looks like a number
        ;; (float/integer) or symbol and return accordingly.
        (else
         (unread-char c port)
         (call-with-values
             (lambda () (get-symbol-or-number port))
           (lambda (type str)
             (case type
               ((symbol)
                ;; str could be empty if the first character is already
                ;; something not allowed in a symbol (and not escaped)!
                ;; Take care about that, it is an error because that
                ;; character should have been handled elsewhere or is
                ;; invalid in the input.
                (if (zero? (string-length str))
                    (begin
                      ;; Take it out so the REPL might not get into an
                      ;; infinite loop with further reading attempts.
                      (read-char port)
                      (error "invalid character in input" c))
                    (return 'symbol (string->symbol str))))
               ((integer)
                ;; In elisp, something like "1." is an integer, while
                ;; string->number returns an inexact real.  Thus we need
                ;; a conversion here, but it should always result in an
                ;; integer!
                (return
                 'integer
                 (let ((num (inexact->exact (string->number str))))
                   (if (not (integer? num))
                       (error "expected integer" str num))
                   num)))
               ((float)
                (return 'float (let ((num (string->number str)))
                                 (if (exact? num)
                                     (error "expected inexact float"
                                            str
                                            num))
                                 num)))
               (else (error "wrong number/symbol type" type)))))))))))

;;; Build a lexer thunk for a port.  This is the exported routine which
;;; can be used to create a lexer for the parser to use.

(define (get-lexer port)
  (lambda () (lex port)))

;;; Build a special lexer that will only read enough for one expression
;;; and then always return end-of-input.  If we find one of the quotation
;;; stuff, one more expression is needed in any case.

(define (get-lexer/1 port)
  (let ((lex (get-lexer port))
        (finished #f)
        (paren-level 0))
    (lambda ()
      (if finished
          (cons 'eof ((@ (ice-9 binary-ports) eof-object)))
          (let ((next (lex))
                (quotation #f))
            (case (car next)
              ((paren-open square-open)
               (set! paren-level (1+ paren-level)))
              ((paren-close square-close)
               (set! paren-level (1- paren-level)))
              ((quote backquote unquote unquote-splicing circular-def)
               (set! quotation #t)))
            (if (and (not quotation) (<= paren-level 0))
                (set! finished #t))
            next)))))
