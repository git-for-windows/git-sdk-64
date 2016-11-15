;;;; (texinfo) -- parsing of texinfo into SXML
;;;;
;;;; 	Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014  Free Software Foundation, Inc.
;;;;    Copyright (C) 2004, 2009 Andy Wingo <wingo at pobox dot com>
;;;;    Copyright (C) 2001,2002 Oleg Kiselyov <oleg at pobox dot com>
;;;;
;;;; This file is based on SSAX's SSAX.scm.
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

;;; Commentary:
;;
;; @subheading Texinfo processing in scheme
;; 
;; This module parses texinfo into SXML. TeX will always be the
;; processor of choice for print output, of course. However, although
;; @code{makeinfo} works well for info, its output in other formats is
;; not very customizable, and the program is not extensible as a whole.
;; This module aims to provide an extensible framework for texinfo
;; processing that integrates texinfo into the constellation of SXML
;; processing tools.
;; 
;; @subheading Notes on the SXML vocabulary
;;
;; Consider the following texinfo fragment:
;; 
;;@example
;; @@deffn Primitive set-car! pair value
;; This function...
;; @@end deffn
;;@end example
;; 
;; Logically, the category (Primitive), name (set-car!), and arguments
;; (pair value) are ``attributes'' of the deffn, with the description as
;; the content. However, texinfo allows for @@-commands within the
;; arguments to an environment, like @code{@@deffn}, which means that
;; texinfo ``attributes'' are PCDATA. XML attributes, on the other hand,
;; are CDATA. For this reason, ``attributes'' of texinfo @@-commands are
;; called ``arguments'', and are grouped under the special element, `%'.
;;
;; Because `%' is not a valid NCName, stexinfo is a superset of SXML. In
;; the interests of interoperability, this module provides a conversion
;; function to replace the `%' with `texinfo-arguments'.
;; 
;;; Code:

;; Comparison to xml output of texinfo (which is rather undocumented):
;;  Doesn't conform to texinfo dtd
;;  No DTD at all, in fact :-/
;;  Actually outputs valid xml, after transforming %
;;  Slower (although with caching the SXML that problem can go away)
;;  Doesn't parse menus (although menus are shite)
;;  Args go in a dedicated element, FBOFW
;;  Definitions are handled a lot better
;;  Does parse comments
;;  Outputs only significant line breaks (a biggie!)
;;  Nodes are treated as anchors, rather than content organizers (a biggie)
;;    (more book-like, less info-like)

;; TODO
;; Integration: help, indexing, plain text

(define-module (texinfo)
  #:use-module (sxml simple)
  #:use-module (sxml transform)
  #:use-module (sxml ssax input-parse)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:export (call-with-file-and-dir
            texi-command-specs
            texi-command-depth
            texi-fragment->stexi
            texi->stexi
            stexi->sxml))

;; Some utilities

(define (parser-error port message . rest)
  (apply throw 'parser-error port message rest))

(define (call-with-file-and-dir filename proc)
  "Call the one-argument procedure @var{proc} with an input port that
reads from @var{filename}. During the dynamic extent of @var{proc}'s
execution, the current directory will be @code{(dirname
@var{filename})}. This is useful for parsing documents that can include
files by relative path name."
  (let ((current-dir (getcwd)))
    (dynamic-wind
        (lambda () (chdir (dirname filename)))
        (lambda ()
          (call-with-input-file (basename filename) proc))
        (lambda () (chdir current-dir)))))

;;========================================================================
;;            Reflection on the XML vocabulary

(define texi-command-specs
  #;
"A list of (@var{name} @var{content-model} . @var{args})

@table @var
@item name 
The name of an @@-command, as a symbol.

@item content-model
A symbol indicating the syntactic type of the @@-command:
@table @code
@item EMPTY-COMMAND
No content, and no @code{@@end} is coming
@item EOL-ARGS
Unparsed arguments until end of line
@item EOL-TEXT
Parsed arguments until end of line
@item INLINE-ARGS
Unparsed arguments ending with @code{#\\@}}
@item INLINE-TEXT
Parsed arguments ending with @code{#\\@}}
@item INLINE-TEXT-ARGS
Parsed arguments ending with @code{#\\@}}
@item ENVIRON
The tag is an environment tag, expect @code{@@end foo}.
@item TABLE-ENVIRON
Like ENVIRON, but with special parsing rules for its arguments.
@item FRAGMENT
For @code{*fragment*}, the command used for parsing fragments of
texinfo documents.
@end table

@code{INLINE-TEXT} commands will receive their arguments within their
bodies, whereas the @code{-ARGS} commands will receive them in their
attribute list.

@code{EOF-TEXT} receives its arguments in its body.

@code{ENVIRON} commands have both: parsed arguments until the end of
line, received through their attribute list, and parsed text until the
@code{@@end}, received in their bodies.

@code{EOF-TEXT-ARGS} receives its arguments in its attribute list, as in
@code{ENVIRON}.

In addition, @code{ALIAS} can alias one command to another.  The alias
will never be seen in parsed stexinfo.

There are four @@-commands that are treated specially. @code{@@include}
is a low-level token that will not be seen by higher-level parsers, so
it has no content-model. @code{@@para} is the paragraph command, which
is only implicit in the texinfo source. @code{@@item} has special
syntax, as noted above, and @code{@@entry} is how this parser treats
@code{@@item} commands within @code{@@table}, @code{@@ftable}, and
@code{@@vtable}.

Also, indexing commands (@code{@@cindex}, etc.) are treated specially.
Their arguments are parsed, but they are needed before entering the
element so that an anchor can be inserted into the text before the index
entry.

@item args
Named arguments to the command, in the same format as the formals for a
lambda. Only present for @code{INLINE-ARGS}, @code{EOL-ARGS},
@code{INLINE-TEXT-ARGS}, @code{ENVIRON}, @code{TABLE-ENVIRON} commands.
@end table"
  '(;; Special commands
    (include            #f) ;; this is a low-level token
    (para               PARAGRAPH)
    (item               ITEM)
    (entry              ENTRY . heading)
    (noindent           EMPTY-COMMAND)
    (*fragment*         FRAGMENT)

    ;; Inline text commands
    (*braces*           INLINE-TEXT) ;; FIXME: make me irrelevant
    (bold               INLINE-TEXT)
    (sample             INLINE-TEXT)
    (samp               INLINE-TEXT)
    (code               INLINE-TEXT)
    (math               INLINE-TEXT)
    (kbd                INLINE-TEXT)
    (key                INLINE-TEXT)
    (var                INLINE-TEXT)
    (env                INLINE-TEXT)
    (file               INLINE-TEXT)
    (command            INLINE-TEXT)
    (option             INLINE-TEXT)
    (dfn                INLINE-TEXT)
    (cite               INLINE-TEXT)
    (acro               INLINE-TEXT)
    (email              INLINE-TEXT)
    (emph               INLINE-TEXT)
    (strong             INLINE-TEXT)
    (sample             INLINE-TEXT)
    (sc                 INLINE-TEXT)
    (titlefont          INLINE-TEXT)
    (asis               INLINE-TEXT)
    (b                  INLINE-TEXT)
    (i                  INLINE-TEXT)
    (r                  INLINE-TEXT)
    (sansserif          INLINE-TEXT)
    (slanted            INLINE-TEXT)
    (t                  INLINE-TEXT)

    ;; Inline args commands
    (value              INLINE-ARGS . (key))
    (ref                INLINE-ARGS . (node #:opt name section info-file manual))
    (xref               INLINE-ARGS . (node #:opt name section info-file manual))
    (pxref              INLINE-TEXT-ARGS
                        . (node #:opt name section info-file manual))
    (url                ALIAS       . uref)
    (uref               INLINE-ARGS . (url #:opt title replacement))
    (anchor             INLINE-ARGS . (name))
    (dots               INLINE-ARGS . ())
    (result             INLINE-ARGS . ())
    (bullet             INLINE-ARGS . ())
    (copyright          INLINE-ARGS . ())
    (tie                INLINE-ARGS . ())
    (image              INLINE-ARGS . (file #:opt width height alt-text extension))

    ;; Inline parsed args commands
    (acronym            INLINE-TEXT-ARGS . (acronym #:opt meaning))

    ;; EOL args elements
    (node               EOL-ARGS . (name #:opt next previous up))
    (c                  EOL-ARGS . all)
    (comment            EOL-ARGS . all)
    (setchapternewpage  EOL-ARGS . all)
    (sp                 EOL-ARGS . all)
    (page               EOL-ARGS . ())
    (vskip              EOL-ARGS . all)
    (syncodeindex       EOL-ARGS . all)
    (contents           EOL-ARGS . ())
    (shortcontents      EOL-ARGS . ())
    (summarycontents    EOL-ARGS . ())
    (insertcopying      EOL-ARGS . ())
    (dircategory        EOL-ARGS . (category))
    (top		EOL-ARGS . (title))
    (printindex		EOL-ARGS . (type))
    (paragraphindent    EOL-ARGS . (indent))

    ;; EOL text commands
    (*ENVIRON-ARGS*     EOL-TEXT)
    (itemx              EOL-TEXT)
    (set                EOL-TEXT)
    (center             EOL-TEXT)
    (title              EOL-TEXT)
    (subtitle           EOL-TEXT)
    (author             EOL-TEXT)
    (chapter            EOL-TEXT)
    (section            EOL-TEXT)
    (appendix           EOL-TEXT)
    (appendixsec        EOL-TEXT)
    (unnumbered         EOL-TEXT)
    (unnumberedsec      EOL-TEXT)
    (subsection         EOL-TEXT)
    (subsubsection      EOL-TEXT)
    (appendixsubsec     EOL-TEXT)
    (appendixsubsubsec  EOL-TEXT)
    (unnumberedsubsec   EOL-TEXT)
    (unnumberedsubsubsec EOL-TEXT)
    (chapheading        EOL-TEXT)
    (majorheading       EOL-TEXT)
    (heading            EOL-TEXT)
    (subheading         EOL-TEXT)
    (subsubheading      EOL-TEXT)

    (deftpx             EOL-TEXT-ARGS . (category name . attributes))
    (defcvx             EOL-TEXT-ARGS . (category class name))
    (defivarx           EOL-TEXT-ARGS . (class name))
    (deftypeivarx       EOL-TEXT-ARGS . (class data-type name))
    (defopx             EOL-TEXT-ARGS . (category class name . arguments))
    (deftypeopx         EOL-TEXT-ARGS . (category class data-type name . arguments))
    (defmethodx         EOL-TEXT-ARGS . (class name . arguments))
    (deftypemethodx     EOL-TEXT-ARGS . (class data-type name . arguments))
    (defoptx            EOL-TEXT-ARGS . (name))
    (defvrx             EOL-TEXT-ARGS . (category name))
    (defvarx            EOL-TEXT-ARGS . (name))
    (deftypevrx         EOL-TEXT-ARGS . (category data-type name))
    (deftypevarx        EOL-TEXT-ARGS . (data-type name))
    (deffnx             EOL-TEXT-ARGS . (category name . arguments))
    (deftypefnx         EOL-TEXT-ARGS . (category data-type name . arguments))
    (defspecx           EOL-TEXT-ARGS . (name . arguments))
    (defmacx            EOL-TEXT-ARGS . (name . arguments))
    (defunx             EOL-TEXT-ARGS . (name . arguments))
    (deftypefunx        EOL-TEXT-ARGS . (data-type name . arguments))

    ;; Indexing commands
    (cindex             INDEX . entry)
    (findex             INDEX . entry)
    (vindex             INDEX . entry)
    (kindex             INDEX . entry)
    (pindex             INDEX . entry)
    (tindex             INDEX . entry)

    ;; Environment commands (those that need @end)
    (texinfo            ENVIRON . title)
    (ignore             ENVIRON . ())
    (ifinfo             ENVIRON . ())
    (iftex              ENVIRON . ())
    (ifhtml             ENVIRON . ())
    (ifxml              ENVIRON . ())
    (ifplaintext        ENVIRON . ())
    (ifnotinfo          ENVIRON . ())
    (ifnottex           ENVIRON . ())
    (ifnothtml          ENVIRON . ())
    (ifnotxml           ENVIRON . ())
    (ifnotplaintext     ENVIRON . ())
    (titlepage          ENVIRON . ())
    (menu               ENVIRON . ())
    (direntry           ENVIRON . ())
    (copying            ENVIRON . ())
    (example            ENVIRON . ())
    (smallexample       ENVIRON . ())
    (display            ENVIRON . ())
    (smalldisplay       ENVIRON . ())
    (verbatim           ENVIRON . ())
    (format             ENVIRON . ())
    (smallformat        ENVIRON . ())
    (lisp               ENVIRON . ())
    (smalllisp          ENVIRON . ())
    (cartouche          ENVIRON . ())
    (quotation          ENVIRON . ())

    (deftp              ENVIRON . (category name . attributes))
    (defcv              ENVIRON . (category class name))
    (defivar            ENVIRON . (class name))
    (deftypeivar        ENVIRON . (class data-type name))
    (defop              ENVIRON . (category class name . arguments))
    (deftypeop          ENVIRON . (category class data-type name . arguments))
    (defmethod          ENVIRON . (class name . arguments))
    (deftypemethod      ENVIRON . (class data-type name . arguments))
    (defopt             ENVIRON . (name))
    (defvr              ENVIRON . (category name))
    (defvar             ENVIRON . (name))
    (deftypevr          ENVIRON . (category data-type name))
    (deftypevar         ENVIRON . (data-type name))
    (deffn              ENVIRON . (category name . arguments))
    (deftypefn          ENVIRON . (category data-type name . arguments))
    (defspec            ENVIRON . (name . arguments))
    (defmac             ENVIRON . (name . arguments))
    (defun              ENVIRON . (name . arguments))
    (deftypefun         ENVIRON . (data-type name . arguments))

    (table              TABLE-ENVIRON . (formatter))
    (itemize            TABLE-ENVIRON . (formatter))
    (enumerate          TABLE-ENVIRON . (start))
    (ftable             TABLE-ENVIRON . (formatter))
    (vtable             TABLE-ENVIRON . (formatter))))

(define command-depths
  '((chapter . 1) (section . 2) (subsection . 3) (subsubsection . 4)
    (top . 0) (unnumbered . 1) (unnumberedsec . 2)
    (unnumberedsubsec . 3) (unnumberedsubsubsec . 4)
    (appendix . 1) (appendixsec . 2) (appendixsection . 2)
    (appendixsubsec . 3) (appendixsubsubsec . 4)))
(define (texi-command-depth command max-depth)
  "Given the texinfo command @var{command}, return its nesting level, or
@code{#f} if it nests too deep for @var{max-depth}.

Examples:
@example
 (texi-command-depth 'chapter 4)        @result{} 1
 (texi-command-depth 'top 4)            @result{} 0
 (texi-command-depth 'subsection 4)     @result{} 3
 (texi-command-depth 'appendixsubsec 4) @result{} 3
 (texi-command-depth 'subsection 2)     @result{} #f
@end example"
  (let ((depth (and=> (assq command command-depths) cdr)))
    (and depth (<= depth max-depth) depth)))

;; The % is for arguments
(define (space-significant? command)
  (memq command
        '(example smallexample verbatim lisp smalllisp menu %)))

;; Like a DTD for texinfo
(define (command-spec command)
  (let ((spec (assq command texi-command-specs)))
    (cond
     ((not spec)
      (parser-error #f "Unknown command" command))
     ((eq? (cadr spec) 'ALIAS)
      (command-spec (cddr spec)))
     (else
      spec))))

(define (inline-content? content)
  (case content
    ((INLINE-TEXT INLINE-ARGS INLINE-TEXT-ARGS) #t)
    (else #f)))


;;========================================================================
;;		Lower-level parsers and scanners
;;
;; They deal with primitive lexical units (Names, whitespaces, tags) and
;; with pieces of more generic productions. Most of these parsers must
;; be called in appropriate context. For example, complete-start-command
;; must be called only when the @-command start has been detected and
;; its name token has been read.

;; Test if a string is made of only whitespace
;; An empty string is considered made of whitespace as well
(define (string-whitespace? str)
  (or (string-null? str)
      (string-every char-whitespace? str)))

;; Like read-text-line, but allows EOF.
(define read-eof-breaks '(*eof* #\return #\newline))
(define (read-eof-line port)
  (if (eof-object? (peek-char port))
      (peek-char port)
      (let* ((line (next-token '() read-eof-breaks
                               "reading a line" port))
             (c (read-char port)))	; must be either \n or \r or EOF
        (if (and (eq? c #\return) (eq? (peek-char port) #\newline))
            (read-char port))		; skip \n that follows \r
        line)))

(define (skip-whitespace port)
  (skip-while '(#\space #\tab #\return #\newline) port))

(define (skip-horizontal-whitespace port)
  (skip-while '(#\space #\tab) port))

;; command ::= Letter+

;; procedure:   read-command PORT
;;
;; Read a command starting from the current position in the PORT and
;; return it as a symbol.
(define (read-command port)
  (let ((first-char (peek-char port)))
    (or (char-alphabetic? first-char)
        (parser-error port "Nonalphabetic @-command char: '" first-char "'")))
  (string->symbol
    (next-token-of
      (lambda (c)
        (cond
          ((eof-object? c) #f)
          ((char-alphabetic? c) c)
          (else #f)))
      port)))

;; A token is a primitive lexical unit. It is a record with two fields,
;; token-head and token-kind.
;;
;; Token types:
;;      END     The end of a texinfo command. If the command is ended by },
;;              token-head will be #f. Otherwise if the command is ended by
;;              @end COMMAND, token-head will be COMMAND. As a special case,
;;              @bye is the end of a special @texinfo command.
;;      START   The start of a texinfo command. The token-head will be a
;;              symbol of the @-command name.
;;      INCLUDE An @include directive. The token-head will be empty -- the
;;              caller is responsible for reading the include file name.
;;      ITEM    @item commands have an irregular syntax. They end at the
;;              next @item, or at the end of the environment. For that
;;              read-command-token treats them specially.

(define (make-token kind head) (cons kind head))
(define token? pair?)
(define token-kind car)
(define token-head cdr)

;; procedure:	read-command-token PORT
;;
;; This procedure starts parsing of a command token. The current
;; position in the stream must be #\@. This procedure scans enough of
;; the input stream to figure out what kind of a command token it is
;; seeing. The procedure returns a token structure describing the token.

(define (read-command-token port)
  (assert-curr-char '(#\@) "start of the command" port)
  (let ((peeked (peek-char port)))
    (cond
     ((memq peeked '(#\! #\: #\. #\? #\@ #\\ #\{ #\}))
      ;; @-commands that escape characters
      (make-token 'STRING (string (read-char port))))
     (else
      (let ((name (read-command port)))
        (case name
          ((end)
           ;; got an ending tag
           (let ((command (string-trim-both
                           (read-eof-line port))))
             (or (and (not (string-null? command))
                      (string-every char-alphabetic? command))
                 (parser-error port "malformed @end" command))
             (make-token 'END (string->symbol command))))
          ((bye)
           ;; the end of the top
           (make-token 'END 'texinfo))
          ((item)
           (make-token 'ITEM 'item))
          ((include)
           (make-token 'INCLUDE #f))
          (else
           (make-token 'START name))))))))

;; procedure+: 	read-verbatim-body PORT STR-HANDLER SEED
;;
;; This procedure must be called after we have read a string
;; "@verbatim\n" that begins a verbatim section. The current position
;; must be the first position of the verbatim body. This function reads
;; _lines_ of the verbatim body and passes them to a STR-HANDLER, a
;; character data consumer.
;;
;; The str-handler is a STR-HANDLER, a procedure STRING1 STRING2 SEED.
;; The first STRING1 argument to STR-HANDLER never contains a newline.
;; The second STRING2 argument often will. On the first invocation of the
;; STR-HANDLER, the seed is the one passed to read-verbatim-body
;; as the third argument. The result of this first invocation will be
;; passed as the seed argument to the second invocation of the line
;; consumer, and so on. The result of the last invocation of the
;; STR-HANDLER is returned by the read-verbatim-body. Note a
;; similarity to the fundamental 'fold' iterator.
;;
;; Within a verbatim section all characters are taken at their face
;; value. It ends with "\n@end verbatim(\r)?\n".

;; Must be called right after the newline after @verbatim.
(define (read-verbatim-body port str-handler seed)
  (let loop ((seed seed))
    (let ((fragment (next-token '() '(#\newline)
                                "reading verbatim" port)))
      ;; We're reading the char after the 'fragment', which is
      ;; #\newline.
      (read-char port)
      (if (string=? fragment "@end verbatim")
          seed
          (loop (str-handler fragment "\n" seed))))))

;; procedure+:	read-arguments PORT
;;
;; This procedure reads and parses a production ArgumentList.
;; ArgumentList ::= S* Argument (S* , S* Argument)* S*
;; Argument ::= ([^@{},])*
;;
;; Arguments are the things in braces, i.e @ref{my node} has one
;; argument, "my node". Most commands taking braces actually don't have
;; arguments, they process text. For example, in
;; @emph{@strong{emphasized}}, the emph takes text, because the parse
;; continues into the braces.
;;
;; Any whitespace within Argument is replaced with a single space.
;; Whitespace around an Argument is trimmed.
;;
;; The procedure returns a list of arguments. Afterwards the current
;; character will be after the final #\}.

(define (read-arguments port stop-char)
  (define (split str)
    (read-char port) ;; eat the delimiter
    (let ((ret (map (lambda (x) (if (string-null? x) #f x))
                    (map string-trim-both (string-split str #\,)))))
      (if (and (pair? ret) (eq? (car ret) #f) (null? (cdr ret)))
          '()
          ret)))
  (split (next-token '() (list stop-char)
                     "arguments of @-command" port)))

;; procedure+:	complete-start-command COMMAND PORT
;;
;; This procedure is to complete parsing of an @-command. The procedure
;; must be called after the command token has been read. COMMAND is a
;; TAG-NAME.
;;
;; This procedure returns several values:
;;  COMMAND: a symbol.
;;  ARGUMENTS: command's arguments, as an alist.
;;  CONTENT-MODEL: the content model of the command.
;;
;; On exit, the current position in PORT will depend on the CONTENT-MODEL.
;;
;; Content model     Port position
;; =============     =============
;; INLINE-TEXT       One character after the #\{.
;; INLINE-TEXT-ARGS  One character after the #\{.
;; INLINE-ARGS       The first character after the #\}.
;; EOL-TEXT          The first non-whitespace character after the command.
;; ENVIRON, TABLE-ENVIRON, EOL-ARGS, EOL-TEXT
;;                   The first character on the next line.
;; PARAGRAPH, ITEM, EMPTY-COMMAND
;;                   The first character after the command.

(define (arguments->attlist port args arg-names)
  (let loop ((in args) (names arg-names) (opt? #f) (out '()))
    (cond
     ((symbol? names) ;; a rest arg
      (reverse (if (null? in) out (acons names in out))))
     ((and (not (null? names)) (eq? (car names) #:opt))
      (loop in (cdr names) #t out))
     ((null? in)
      (if (or (null? names) opt?)
          (reverse out)
          (parser-error port "@-command expected more arguments:" 
                        args arg-names names)))
     ((null? names)
      (parser-error port "@-command didn't expect more arguments:" in))
     ((not (car in))
      (or (and opt? (loop (cdr in) (cdr names) opt? out))
          (parser-error "@-command missing required argument"
                        (car names))))
     (else
      (loop (cdr in) (cdr names) opt?
            (acons (car names)
                   (if (list? (car in)) (car in) (list (car in)))
                   out))))))

(define (parse-table-args command port)
  (let* ((line (string-trim-both (read-text-line port)))
         (length (string-length line)))
    (define (get-formatter)
      (or (and (not (zero? length))
               (eq? (string-ref line 0) #\@)
               (let ((f (string->symbol (substring line 1))))
                 (or (inline-content? (cadr (command-spec f)))
                     (parser-error
                      port "@item formatter must be INLINE" f))
                 f))
          (parser-error port "Invalid @item formatter" line)))
    (case command
      ((enumerate)
       (if (zero? length)
           '()
           `((start
              ,(if (or (and (eq? length 1)
                            (char-alphabetic? (string-ref line 0)))
                       (string-every char-numeric? line))
                   line
                   (parser-error
                    port "Invalid enumerate start" line))))))
      ((itemize)
       `((bullet
          ,(or (and (eq? length 1) line)
               (and (string-null? line) '(bullet))
               (list (get-formatter))))))
      (else ;; tables of various varieties
       `((formatter (,(get-formatter))))))))

(define (complete-start-command command port)
  (define (get-arguments type arg-names stop-char)
    (arguments->attlist port (read-arguments port stop-char) arg-names))

  (let* ((spec (command-spec command))
         (command (car spec))
         (type (cadr spec))
         (arg-names (cddr spec)))
    (case type
      ((INLINE-TEXT)
       (assert-curr-char '(#\{) "Inline element lacks {" port)
       (values command '() type))
      ((INLINE-ARGS)
       (assert-curr-char '(#\{) "Inline element lacks {" port)
       (values command (get-arguments type arg-names #\}) type))
      ((INLINE-TEXT-ARGS)
       (assert-curr-char '(#\{) "Inline element lacks {" port)
       (values command '() type))
      ((EOL-ARGS)
       (values command (get-arguments type arg-names #\newline) type))
      ((ENVIRON ENTRY INDEX)
       (skip-horizontal-whitespace port)
       (values command (parse-environment-args command port) type))
      ((TABLE-ENVIRON)
       (skip-horizontal-whitespace port)
       (values command (parse-table-args command port) type))
      ((EOL-TEXT)
       (skip-horizontal-whitespace port)
       (values command '() type))
      ((EOL-TEXT-ARGS)
       (skip-horizontal-whitespace port)
       (values command (parse-eol-text-args command port) type))
      ((PARAGRAPH EMPTY-COMMAND ITEM FRAGMENT)
       (values command '() type))
      (else ;; INCLUDE shouldn't get here
       (parser-error port "can't happen")))))

;;-----------------------------------------------------------------------------
;;			Higher-level parsers and scanners
;;
;; They parse productions corresponding entire @-commands.

;; Only reads @settitle, leaves it to the command parser to finish
;; reading the title.
(define (take-until-settitle port)
  (or (find-string-from-port? "\n@settitle " port)
      (parser-error port "No \\n@settitle  found"))
  (skip-horizontal-whitespace port)
  (and (eq? (peek-char port) #\newline)
       (parser-error port "You have a @settitle, but no title")))

;; procedure+:	read-char-data PORT EXPECT-EOF? STR-HANDLER SEED
;;
;; This procedure is to read the CharData of a texinfo document.
;;
;; text ::= (CharData | Command)*
;;
;; The procedure reads CharData and stops at @-commands (or
;; environments). It also stops at an open or close brace.
;;
;; port
;;	a PORT to read
;; expect-eof?
;;	a boolean indicating if EOF is normal, i.e., the character
;;	data may be terminated by the EOF. EOF is normal
;;	while processing the main document.
;; preserve-ws?
;;	a boolean indicating if we are within a whitespace-preserving
;;      environment. If #t, suppress paragraph detection.
;; str-handler
;;	a STR-HANDLER, see read-verbatim-body
;; seed
;;	an argument passed to the first invocation of STR-HANDLER.
;;
;; The procedure returns two results: SEED and TOKEN. The SEED is the
;; result of the last invocation of STR-HANDLER, or the original seed if
;; STR-HANDLER was never called.
;;
;; TOKEN can be either an eof-object (this can happen only if expect-eof?
;; was #t), or a texinfo token denoting the start or end of a tag.

;; read-char-data port expect-eof? preserve-ws? str-handler seed
(define read-char-data
  (let* ((end-chars-eof '(*eof* #\{ #\} #\@ #\newline)))
    (define (handle str-handler str1 str2 seed)
      (if (and (string-null? str1) (string-null? str2))
          seed
          (str-handler str1 str2 seed)))

    (lambda (port expect-eof? preserve-ws? str-handler seed)
      (let ((end-chars ((if expect-eof? identity cdr) end-chars-eof)))
        (let loop ((seed seed))
          (let* ((fragment (next-token '() end-chars "reading char data" port))
                 (term-char (peek-char port))) ; one of end-chars
            (cond
             ((eof-object? term-char) ; only if expect-eof?
              (values (handle str-handler fragment "" seed) term-char))
             ((memq term-char '(#\@ #\{ #\}))
              (values (handle str-handler fragment "" seed)
                      (case term-char
                        ((#\@) (read-command-token port))
                        ((#\{) (make-token 'START '*braces*))
                        ((#\}) (read-char port) (make-token 'END #f)))))
             ((eq? term-char #\newline)
              ;; Always significant, unless directly before an end token.
              (let ((c (peek-next-char port)))
                (cond
                 ((eof-object? c)
                  (or expect-eof?
                      (parser-error port "EOF while reading char data"))
                  (values (handle str-handler fragment "" seed) c))
                 ((eq? c #\@)
                  (let* ((token (read-command-token port))
                         (end? (eq? (token-kind token) 'END)))
                    (values
                     (handle str-handler fragment
                             (if end? "" (if preserve-ws? "\n" " "))
                             seed)
                     token)))
                 ((and (not preserve-ws?) (eq? c #\newline))
                  ;; paragraph-separator ::= #\newline #\newline+
                  (skip-while '(#\newline) port)
                  (skip-horizontal-whitespace port)
                  (values (handle str-handler fragment "" seed)
                          (make-token 'PARA 'para)))
                 (else
                  (loop (handle str-handler fragment
                                (if preserve-ws? "\n" " ") seed)))))))))))))

; procedure+:	assert-token TOKEN KIND NAME
; Make sure that TOKEN is of anticipated KIND and has anticipated NAME
(define (assert-token token kind name)
  (or (and (token? token)
           (eq? kind (token-kind token))
           (equal? name (token-head token)))
      (parser-error #f "Expecting @end for " name ", got " token)))

;;========================================================================
;;		Highest-level parsers: Texinfo to SXML

;; These parsers are a set of syntactic forms to instantiate a SSAX
;; parser. The user tells what to do with the parsed character and
;; element data. These latter handlers determine if the parsing follows a
;; SAX or a DOM model.

;; syntax: make-command-parser fdown fup str-handler

;; Create a parser to parse and process one element, including its
;; character content or children elements. The parser is typically
;; applied to the root element of a document.

;; fdown
;;	procedure COMMAND ARGUMENTS EXPECTED-CONTENT SEED
;;
;;	This procedure is to generate the seed to be passed to handlers
;;	that process the content of the element. This is the function
;;	identified as 'fdown' in the denotational semantics of the XML
;;	parser given in the title comments to (sxml ssax).
;;
;; fup
;;	procedure COMMAND ARGUMENTS PARENT-SEED SEED
;;
;;	This procedure is called when parsing of COMMAND is finished.
;;	The SEED is the result from the last content parser (or from
;;	fdown if the element has the empty content). PARENT-SEED is the
;;	same seed as was passed to fdown. The procedure is to generate a
;;	seed that will be the result of the element parser. This is the
;;	function identified as 'fup' in the denotational semantics of
;;	the XML parser given in the title comments to (sxml ssax).
;;
;; str-handler
;;	A STR-HANDLER, see read-verbatim-body
;;

;; The generated parser is a
;;	procedure COMMAND PORT SEED
;;
;; The procedure must be called *after* the command token has been read.

(define (read-include-file-name port)
  (let ((x (string-trim-both (read-eof-line port))))
    (if (string-null? x)
        (error "no file listed")
        x))) ;; fixme: should expand @value{} references

(define (sxml->node-name sxml)
  "Turn some sxml string into a valid node name."
  (let loop ((in (string->list (sxml->string sxml))) (out '()))
    (if (null? in)
        (apply string (reverse out))
        (if (memq (car in) '(#\{ #\} #\@ #\,))
            (loop (cdr in) out)
            (loop (cdr in) (cons (car in) out))))))

(define (index command arguments fdown fup parent-seed)
  (case command
    ((deftp defcv defivar deftypeivar defop deftypeop defmethod
      deftypemethod defopt defvr defvar deftypevr deftypevar deffn
      deftypefn defspec defmac defun deftypefun)
     (let ((args `((name ,(string-append (symbol->string command) "-"
                                         (cadr (assq 'name arguments)))))))
       (fup 'anchor args parent-seed
            (fdown 'anchor args 'INLINE-ARGS '()))))
    ((cindex findex vindex kindex pindex tindex)
     (let ((args `((name ,(string-append (symbol->string command) "-"
                                         (sxml->node-name
                                          (assq 'entry arguments)))))))
       (fup 'anchor args parent-seed
            (fdown 'anchor args 'INLINE-ARGS '()))))
    (else parent-seed)))

(define (make-command-parser fdown fup str-handler)
  (lambda (command port seed)
    (let visit ((command command) (port port) (sig-ws? #f) (parent-seed seed))
      (let*-values (((command arguments expected-content)
                     (complete-start-command command port)))
        (let* ((parent-seed (index command arguments fdown fup parent-seed))
               (seed (fdown command arguments expected-content parent-seed))
               (eof-closes? (or (memq command '(texinfo para *fragment*))
                                (eq? expected-content 'EOL-TEXT)))
               (sig-ws? (or sig-ws? (space-significant? command)))
               (up (lambda (s) (fup command arguments parent-seed s)))
               (new-para (lambda (s) (fdown 'para '() 'PARAGRAPH s)))
               (make-end-para (lambda (p) (lambda (s) (fup 'para '() p s)))))
          
          (define (port-for-content)
            (if (eq? expected-content 'EOL-TEXT)
                (call-with-input-string (read-text-line port) identity)
                port))

          (cond
           ((memq expected-content '(EMPTY-COMMAND INLINE-ARGS EOL-ARGS INDEX
                                     EOL-TEXT-ARGS))
            ;; empty or finished by complete-start-command
            (up seed))
           ((eq? command 'verbatim)
            (up (read-verbatim-body port str-handler seed)))
           (else
            (let loop ((port (port-for-content))
                       (expect-eof? eof-closes?)
                       (end-para identity)
                       (need-break? (and (not sig-ws?)
                                         (memq expected-content
                                               '(ENVIRON TABLE-ENVIRON
                                                 ENTRY ITEM FRAGMENT))))
                       (seed seed))
              (cond
               ((and need-break? (or sig-ws? (skip-whitespace port))
                     (not (memq (peek-char port) '(#\@ #\})))
                     (not (eof-object? (peek-char port))))
                ;; Even if we have an @, it might be inline -- check
                ;; that later
                (let ((seed (end-para seed)))
                  (loop port expect-eof? (make-end-para seed) #f
                        (new-para seed))))
               (else
                (let*-values (((seed token)
                               (read-char-data
                                port expect-eof? sig-ws? str-handler seed)))
                  (cond
                   ((eof-object? token)
                    (case expect-eof? 
                      ((include #f) (end-para seed))
                      (else (up (end-para seed)))))
                   (else
                    (case (token-kind token)
                      ((STRING)
                       ;; this is only @-commands that escape
                       ;; characters: @}, @@, @{ -- new para if need-break
                       (let ((seed ((if need-break? end-para identity) seed)))
                         (loop port expect-eof?
                               (if need-break? (make-end-para seed) end-para) #f
                               (str-handler (token-head token) ""
                                            ((if need-break? new-para identity)
                                             seed)))))
                      ((END)
                       ;; The end will only have a name if it's for an
                       ;; environment
                       (cond
                        ((memq command '(item entry))
                         (let ((spec (command-spec (token-head token))))
                           (or (eq? (cadr spec) 'TABLE-ENVIRON)
                               (parser-error
                                port "@item not ended by @end table/enumerate/itemize"
                                token))))
                        ((eq? expected-content 'ENVIRON)
                         (assert-token token 'END command)))
                       (up (end-para seed)))
                      ((ITEM)
                       (cond
                        ((memq command '(enumerate itemize))
                         (up (visit 'item port sig-ws? (end-para seed))))
                        ((eq? expected-content 'TABLE-ENVIRON)
                         (up (visit 'entry port sig-ws? (end-para seed))))
                        ((memq command '(item entry))
                         (visit command port sig-ws? (up (end-para seed))))
                        (else
                         (parser-error
                          port "@item must be within a table environment"
                          command))))
                      ((PARA)
                       ;; examine valid paragraphs?
                       (loop port expect-eof? end-para (not sig-ws?) seed))
                      ((INCLUDE)
                       ;; Recurse for include files
                       (let ((seed (call-with-file-and-dir
                                    (read-include-file-name port)
                                    (lambda (port)
                                      (loop port 'include end-para
                                            need-break? seed)))))
                         (loop port expect-eof? end-para need-break? seed)))
                      ((START)          ; Start of an @-command
                       (let* ((head (token-head token))
                              (spec (command-spec head))
                              (head (car spec))
                              (type (cadr spec))
                              (inline? (inline-content? type))
                              (seed ((if (and inline? (not need-break?))
                                         identity end-para) seed))
                              (end-para (if inline?
                                            (if need-break? (make-end-para seed)
                                                end-para)
                                            identity))
                              (new-para (if (and inline? need-break?)
                                            new-para identity)))
                         (loop port expect-eof? end-para (not inline?)
                               (visit head port sig-ws? (new-para seed)))))
                      (else
                       (parser-error port "Unknown token type" token))))))))))))))))

;; procedure: reverse-collect-str-drop-ws fragments
;;
;; Given the list of fragments (some of which are text strings), reverse
;; the list and concatenate adjacent text strings. We also drop
;; "unsignificant" whitespace, that is, whitespace in front, behind and
;; between elements. The whitespace that is included in character data
;; is not affected.
(define (reverse-collect-str-drop-ws fragments)
  (cond 
   ((null? fragments)                   ; a shortcut
    '())
   ((and (string? (car fragments))	; another shortcut
         (null? (cdr fragments))	; remove single ws-only string
         (string-whitespace? (car fragments)))
    '())
   (else
    (let loop ((fragments fragments) (result '()) (strs '())
               (all-whitespace? #t))
      (cond
       ((null? fragments)
        (if all-whitespace?
            result                      ; remove leading ws
            (cons (apply string-append strs) result)))
       ((string? (car fragments))
        (loop (cdr fragments) result (cons (car fragments) strs)
              (and all-whitespace?
                   (string-whitespace? (car fragments)))))
       (else
        (loop (cdr fragments)
              (cons
               (car fragments)
               (cond
                ((null? strs) result)
                (all-whitespace?
                 (if (null? result)
                     result             ; remove trailing whitespace
                     (cons " " result))); replace interstitial ws with
					; one space
                (else
                 (cons (apply string-append strs) result))))
              '() #t)))))))

(define (parse-inline-text-args port spec text)
  (let lp ((in text) (cur '()) (out '()))
    (cond
     ((null? in)
      (if (and (pair? cur)
               (string? (car cur))
               (string-whitespace? (car cur)))
          (lp in (cdr cur) out)
          (let ((args (reverse (if (null? cur)
                                   out
                                   (cons (reverse cur) out)))))
            (arguments->attlist port args (cddr spec)))))
     ((pair? (car in))
      (lp (cdr in) (cons (car in) cur) out))
     ((string-index (car in) #\,)
      (let* ((parts (string-split (car in) #\,))
             (head (string-trim-right (car parts)))
             (rev-tail (reverse (cdr parts)))
             (last (string-trim (car rev-tail))))
        (lp (cdr in)
            (if (string-null? last) cur (cons last cur))
            (append (cdr rev-tail)
                    (cons (reverse (if (string-null? head) cur (cons head cur)))
                          out)))))
     (else
      (lp (cdr in)
          (cons (if (null? cur) (string-trim (car in)) (car in)) cur)
          out)))))

(define (make-dom-parser)
  (make-command-parser
   (lambda (command args content seed)      ; fdown
     '())
   (lambda (command args parent-seed seed)  ; fup
     (let* ((seed (reverse-collect-str-drop-ws seed))
            (spec (command-spec command))
            (command (car spec)))
       (if (eq? (cadr spec) 'INLINE-TEXT-ARGS)
           (cons (list command (cons '% (parse-inline-text-args #f spec seed)))
                 parent-seed)
           (acons command
                  (if (null? args) seed (acons '% args seed))
                  parent-seed))))
   (lambda (string1 string2 seed)           ; str-handler
     (if (string-null? string2)
         (cons string1 seed)
         (cons* string2 string1 seed)))))

(define parse-environment-args
  (let ((parser (make-dom-parser)))
    ;; duplicate arguments->attlist to avoid unnecessary splitting
    (lambda (command port)
      (let* ((args (cdar (parser '*ENVIRON-ARGS* port '())))
             (spec (command-spec command))
             (command (car spec))
             (arg-names (cddr spec)))
        (cond
         ((not arg-names)
          (if (null? args) '()
              (parser-error port "@-command doesn't take args" command)))
         ((eq? arg-names #t)
          (list (cons 'arguments args)))
         (else
          (let loop ((args args) (arg-names arg-names) (out '()))
            (cond
             ((null? arg-names)
              (if (null? args) (reverse! out)
                  (parser-error port "@-command didn't expect more args"
                                command args)))
             ((symbol? arg-names)
              (reverse! (acons arg-names args out)))
             ((null? args)
              (parser-error port "@-command expects more args"
                            command arg-names))
             ((and (string? (car args)) (string-index (car args) #\space))
              => (lambda (i)
                   (let ((rest (substring/shared (car args) (1+ i))))
                     (if (zero? i)
                         (loop (cons rest (cdr args)) arg-names out)
                         (loop (cons rest (cdr args)) (cdr arg-names)
                               (cons (list (car arg-names)
                                           (substring (car args) 0 i))
                                     out))))))
             (else
              (loop (cdr args) (cdr arg-names)
                    (if (and (pair? (car args)) (eq? (caar args) '*braces*))
                        (acons (car arg-names) (cdar args) out)
                        (cons (list (car arg-names) (car args)) out))))))))))))
   
(define (parse-eol-text-args command port)
  ;; perhaps parse-environment-args should be named more
  ;; generically.
  (parse-environment-args command port))

;; procedure: texi-fragment->stexi STRING
;;
;; A DOM parser for a texinfo fragment STRING.
;;
;; The procedure returns an SXML tree headed by the special tag,
;; *fragment*.

(define (texi-fragment->stexi string-or-port)
  "Parse the texinfo commands in @var{string-or-port}, and return the
resultant stexi tree. The head of the tree will be the special command,
@code{*fragment*}."
  (define (parse port)
    (postprocess (car ((make-dom-parser) '*fragment* port '()))))
  (if (input-port? string-or-port)
      (parse string-or-port)
      (call-with-input-string string-or-port parse)))

;; procedure: texi->stexi PORT
;;
;; This is an instance of a SSAX parser above that returns an SXML
;; representation of the texinfo document ready to be read at PORT.
;;
;; The procedure returns an SXML tree. The port points to the
;; first character after the @bye, or to the end of the file.

(define (texi->stexi port)
  "Read a full texinfo document from @var{port} and return the parsed
stexi tree. The parsing will start at the @code{@@settitle} and end at
@code{@@bye} or EOF."
  (let ((parser (make-dom-parser)))
    (take-until-settitle port)
    (postprocess (car (parser 'texinfo port '())))))

(define (car-eq? x y) (and (pair? x) (eq? (car x) y)))
(define (make-contents tree)
  (define (lp in out depth)
    (cond
     ((null? in) (values in (cons 'enumerate (reverse! out))))
     ((and (pair? (cdr in)) (texi-command-depth (caadr in) 4))
      => (lambda (new-depth)
           (let ((node-name (and (car-eq? (car in) 'node)
                                 (cadr (assq 'name (cdadar in))))))
             (cond
              ((< new-depth depth)
               (values in (cons 'enumerate (reverse! out))))
              ((> new-depth depth)
               (let ((out-cdr (if (null? out) '() (cdr out)))
                     (out-car (if (null? out) (list 'item) (car out))))
                 (let*-values (((new-in new-out) (lp in '() (1+ depth))))
                   (lp new-in
                       (cons (append out-car (list new-out)) out-cdr)
                       depth))))
              (else ;; same depth
               (lp (cddr in)
                   (cons
                    `(item (para
                            ,@(if node-name
                                  `((ref (% (node ,node-name))))
                                  (cdadr in))))
                    out)
                   depth))))))
     (else (lp (cdr in) out depth))))
  (let*-values (((_ contents) (lp tree '() 1)))
    `((chapheading "Table of Contents") ,contents)))

(define (trim-whitespace str trim-left? trim-right?)
  (let* ((left-space? (and (not trim-left?)
                           (string-prefix? " " str)))
         (right-space? (and (not trim-right?)
                            (string-suffix? " " str)))
         (tail (append! (string-tokenize str)
                        (if right-space? '("") '()))))
    (string-join (if left-space? (cons "" tail) tail))))

(define (postprocess tree)
  (define (loop in out state first? sig-ws?)
    (cond
     ((null? in)
      (values (reverse! out) state))
     ((string? (car in))
      (loop (cdr in)
            (cons (if sig-ws? (car in)
                      (trim-whitespace (car in) first? (null? (cdr in))))
                  out)
            state #f sig-ws?))
     ((pair? (car in))
      (case (caar in)
        ((set)
         (if (null? (cdar in)) (error "@set missing arguments" in))
         (if (string? (cadar in))
             (let ((i (string-index (cadar in) #\space)))
               (if i 
                   (loop (cdr in) out
                         (acons (substring (cadar in) 0 i)
                                (cons (substring (cadar in) (1+ i)) (cddar in))
                                state)
                         #f sig-ws?)
                   (loop (cdr in) out (acons (cadar in) (cddar in) state)
                         #f sig-ws?)))
             (error "expected a constant to define for @set" in)))
        ((value)
         (loop (fold-right cons (cdr in)
                           (or (and=>
                                (assoc (cadr (assq 'key (cdadar in))) state) cdr)
                               (error "unknown value" (cdadar in) state)))
               out
               state #f sig-ws?))
        ((copying)
         (loop (cdr in) out (cons (car in) state) #f sig-ws?))
        ((insertcopying)
         (loop (fold-right cons (cdr in)
                           (or (cdr (assoc 'copying state))
                               (error "copying isn't set yet")))
               out
               state #f sig-ws?))
        ((contents)
         (loop (cdr in) (fold cons out (make-contents tree)) state #f sig-ws?))
        (else
         (let*-values (((kid-out state)
                        (loop (car in) '() state #t
                              (or sig-ws? (space-significant? (caar in))))))
           (loop (cdr in) (cons kid-out out) state #f sig-ws?)))))
     (else ; a symbol
      (loop (cdr in) (cons (car in) out) state #t sig-ws?))))

  (call-with-values
      (lambda () (loop tree '() '() #t #f))
    (lambda (out state) out)))

;; Replace % with texinfo-arguments.
(define (stexi->sxml tree)
  "Transform the stexi tree @var{tree} into sxml. This involves
replacing the @code{%} element that keeps the texinfo arguments with an
element for each argument.

FIXME: right now it just changes % to @code{texinfo-arguments} -- that
doesn't hang with the idea of making a dtd at some point"
  (pre-post-order
   tree
   `((% . ,(lambda (x . t) (cons 'texinfo-arguments t)))
     (*text* . ,(lambda (x t) t))
     (*default* . ,(lambda (x . t) (cons x t))))))

;;; arch-tag: 73890afa-597c-4264-ae70-46fe7756ffb5
;;; texinfo.scm ends here
