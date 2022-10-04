;;; Guile Emacs Lisp

;;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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

(define-module (language elisp parser)
  #:use-module (language elisp lexer)
  #:export (read-elisp))

;;; The parser (reader) for elisp expressions.
;;;
;;; It is hand-written (just as the lexer is) instead of using some
;;; parser generator because this allows easier transfer of source
;;; properties from the lexer ((text parse-lalr) seems not to allow
;;; access to the original lexer token-pair) and is easy enough anyways.

;;; Report a parse error.  The first argument is some current lexer
;;; token where source information is available should it be useful.

(define (parse-error token msg . args)
  (apply error msg args))

;;; For parsing circular structures, we keep track of definitions in a
;;; hash-map that maps the id's to their values.  When defining a new
;;; id, though, we immediatly fill the slot with a promise before
;;; parsing and setting the real value, because it must already be
;;; available at that time in case of a circular reference.  The promise
;;; refers to a local variable that will be set when the real value is
;;; available through a closure.  After parsing the expression is
;;; completed, we work through it again and force all promises we find.
;;; The definitions themselves are stored in a fluid and their scope is
;;; one call to read-elisp (but not only the currently parsed
;;; expression!).

(define circular-definitions (make-fluid))

(define (make-circular-definitions)
  (make-hash-table))

(define (circular-ref token)
  (if (not (eq? (car token) 'circular-ref))
      (error "invalid token for circular-ref" token))
  (let* ((id (cdr token))
         (value (hashq-ref (fluid-ref circular-definitions) id)))
    (if value
        value
        (parse-error token "undefined circular reference" id))))

;;; Returned is a closure that, when invoked, will set the final value.
;;; This means both the variable the promise will return and the
;;; hash-table slot so we don't generate promises any longer.

(define (circular-define! token)
  (if (not (eq? (car token) 'circular-def))
      (error "invalid token for circular-define!" token))
  (let ((value #f)
        (table (fluid-ref circular-definitions))
        (id (cdr token)))
    (hashq-set! table id (delay value))
    (lambda (real-value)
      (set! value real-value)
      (hashq-set! table id real-value))))

;;; Work through a parsed data structure and force the promises there.
;;; After a promise is forced, the resulting value must not be recursed
;;; on; this may lead to infinite recursion with a circular structure,
;;; and additionally this value was already processed when it was
;;; defined.  All deep data structures that can be parsed must be
;;; handled here!

(define (force-promises! data)
  (cond
   ((pair? data)
    (begin
      (if (promise? (car data))
          (set-car! data (force (car data)))
          (force-promises! (car data)))
      (if (promise? (cdr data))
          (set-cdr! data (force (cdr data)))
          (force-promises! (cdr data)))))
   ((vector? data)
    (let ((len (vector-length data)))
      (let iterate ((i 0))
        (if (< i len)
            (let ((el (vector-ref data i)))
              (if (promise? el)
                  (vector-set! data i (force el))
                  (force-promises! el))
              (iterate (1+ i)))))))
   ;; Else nothing needs to be done.
   ))

;;; We need peek-functionality for the next lexer token, this is done
;;; with some single token look-ahead storage.  This is handled by a
;;; closure which allows getting or peeking the next token.  When one
;;; expression is fully parsed, we don't want a look-ahead stored here
;;; because it would miss from future parsing.  This is verified by the
;;; finish action.

(define (make-lexer-buffer lex)
  (let ((look-ahead #f))
    (lambda (action)
      (if (eq? action 'finish)
          (if look-ahead
              (error "lexer-buffer is not empty when finished")
              #f)
          (begin
            (if (not look-ahead)
                (set! look-ahead (lex)))
            (case action
              ((peek) look-ahead)
              ((get)
               (let ((result look-ahead))
                 (set! look-ahead #f)
                 result))
              (else (error "invalid lexer-buffer action" action))))))))

;;; Get the contents of a list, where the opening parentheses has
;;; already been found.  The same code is used for vectors and lists,
;;; where lists allow the dotted tail syntax and vectors not;
;;; additionally, the closing parenthesis must of course match.  The
;;; implementation here is not tail-recursive, but I think it is clearer
;;; and simpler this way.

(define (get-list lex allow-dot close-square)
  (let* ((next (lex 'peek))
         (type (car next)))
    (cond
     ((eq? type (if close-square 'square-close 'paren-close))
      (begin
        (if (not (eq? (car (lex 'get)) type))
            (error "got different token than peeked"))
        '()))
     ((and allow-dot (eq? type 'dot))
      (begin
        (if (not (eq? (car (lex 'get)) type))
            (error "got different token than peeked"))
        (let ((tail (get-list lex #f close-square)))
          (if (not (= (length tail) 1))
              (parse-error next
                           "expected exactly one element after dot"))
          (car tail))))
     (else
      ;; Do both parses in exactly this sequence!
      (let* ((head (get-expression lex))
             (tail (get-list lex allow-dot close-square)))
        (cons head tail))))))

;;; Parse a single expression from a lexer-buffer.  This is the main
;;; routine in our recursive-descent parser.

(define quotation-symbols '((quote . quote)
                            (backquote . #{`}#)
                            (unquote . #{,}#)
                            (unquote-splicing . #{,@}#)))

(define (get-expression lex)
  (let* ((token (lex 'get))
         (type (car token))
         (return (lambda (result)
                   (if (pair? result)
                       (set-source-properties!
                        result
                        (source-properties token)))
                   result)))
    (case type
      ((eof)
       (parse-error token "end of file during parsing"))
      ((integer float symbol character string)
       (return (cdr token)))
      ((function)
       (return `(function ,(get-expression lex))))
      ((quote backquote unquote unquote-splicing)
       (return (list (assq-ref quotation-symbols type)
                     (get-expression lex))))
      ((paren-open)
       (return (get-list lex #t #f)))
      ((square-open)
       (return (list->vector (get-list lex #f #t))))
      ((circular-ref)
       (circular-ref token))
      ((circular-def)
       ;; The order of definitions is important!
       (let* ((setter (circular-define! token))
              (expr (get-expression lex)))
         (setter expr)
         (force-promises! expr)
         expr))
      ((set-lexical-binding-mode!)
       (return `(%set-lexical-binding-mode ,(cdr token))))
      (else
       (parse-error token "expected expression, got" token)))))

;;; Define the reader function based on this; build a lexer, a
;;; lexer-buffer, and then parse a single expression to return.  We also
;;; define a circular-definitions data structure to use.

(define (read-elisp port)
  (with-fluids ((circular-definitions (make-circular-definitions)))
    (let* ((lexer (get-lexer port))
           (lexbuf (make-lexer-buffer lexer))
           (next (lexbuf 'peek)))
      (if (eq? (car next) 'eof)
          (cdr next)
          (let ((result (get-expression lexbuf)))
            (lexbuf 'finish)
            result)))))
