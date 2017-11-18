;;;; (sxml ssax) -- the SSAX parser
;;;;
;;;; 	Copyright (C) 2009, 2010,2012,2013  Free Software Foundation, Inc.
;;;;    Modified 2004 by Andy Wingo <wingo at pobox dot com>.
;;;;    Written 2001,2002,2003,2004 by Oleg Kiselyov <oleg at pobox dot com> as SSAX.scm.
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
;@subheading Functional XML parsing framework
;@subsubheading SAX/DOM and SXML parsers with support for XML Namespaces and validation
;
; This is a package of low-to-high level lexing and parsing procedures
; that can be combined to yield a SAX, a DOM, a validating parser, or
; a parser intended for a particular document type. The procedures in
; the package can be used separately to tokenize or parse various
; pieces of XML documents. The package supports XML Namespaces,
; internal and external parsed entities, user-controlled handling of
; whitespace, and validation. This module therefore is intended to be
; a framework, a set of "Lego blocks" you can use to build a parser
; following any discipline and performing validation to any degree. As
; an example of the parser construction, this file includes a
; semi-validating SXML parser.

; The present XML framework has a "sequential" feel of SAX yet a
; "functional style" of DOM. Like a SAX parser, the framework scans the
; document only once and permits incremental processing. An application
; that handles document elements in order can run as efficiently as
; possible. @emph{Unlike} a SAX parser, the framework does not require
; an application register stateful callbacks and surrender control to
; the parser. Rather, it is the application that can drive the framework
; -- calling its functions to get the current lexical or syntax element.
; These functions do not maintain or mutate any state save the input
; port. Therefore, the framework permits parsing of XML in a pure
; functional style, with the input port being a monad (or a linear,
; read-once parameter).

; Besides the @var{port}, there is another monad -- @var{seed}. Most of
; the middle- and high-level parsers are single-threaded through the
; @var{seed}. The functions of this framework do not process or affect
; the @var{seed} in any way: they simply pass it around as an instance
; of an opaque datatype. User functions, on the other hand, can use the
; seed to maintain user's state, to accumulate parsing results, etc. A
; user can freely mix his own functions with those of the framework. On
; the other hand, the user may wish to instantiate a high-level parser:
; @code{SSAX:make-elem-parser} or @code{SSAX:make-parser}. In the latter
; case, the user must provide functions of specific signatures, which
; are called at predictable moments during the parsing: to handle
; character data, element data, or processing instructions (PI). The
; functions are always given the @var{seed}, among other parameters, and
; must return the new @var{seed}.

; From a functional point of view, XML parsing is a combined
; pre-post-order traversal of a "tree" that is the XML document
; itself. This down-and-up traversal tells the user about an element
; when its start tag is encountered. The user is notified about the
; element once more, after all element's children have been
; handled. The process of XML parsing therefore is a fold over the
; raw XML document. Unlike a fold over trees defined in [1], the
; parser is necessarily single-threaded -- obviously as elements
; in a text XML document are laid down sequentially. The parser
; therefore is a tree fold that has been transformed to accept an
; accumulating parameter [1,2].

; Formally, the denotational semantics of the parser can be expressed
; as
;@smallexample
; parser:: (Start-tag -> Seed -> Seed) ->
;	   (Start-tag -> Seed -> Seed -> Seed) ->
;	   (Char-Data -> Seed -> Seed) ->
;	   XML-text-fragment -> Seed -> Seed
; parser fdown fup fchar "<elem attrs> content </elem>" seed
;  = fup "<elem attrs>" seed
;	(parser fdown fup fchar "content" (fdown "<elem attrs>" seed))
;
; parser fdown fup fchar "char-data content" seed
;  = parser fdown fup fchar "content" (fchar "char-data" seed)
;
; parser fdown fup fchar "elem-content content" seed
;  = parser fdown fup fchar "content" (
;	parser fdown fup fchar "elem-content" seed)
;@end smallexample

; Compare the last two equations with the left fold
;@smallexample
; fold-left kons elem:list seed = fold-left kons list (kons elem seed)
;@end smallexample

; The real parser created by @code{SSAX:make-parser} is slightly more
; complicated, to account for processing instructions, entity
; references, namespaces, processing of document type declaration, etc.


; The XML standard document referred to in this module is
;	@uref{http://www.w3.org/TR/1998/REC-xml-19980210.html}
;
; The present file also defines a procedure that parses the text of an
; XML document or of a separate element into SXML, an S-expression-based
; model of an XML Information Set. SXML is also an Abstract Syntax Tree
; of an XML document. SXML is similar but not identical to DOM; SXML is
; particularly suitable for Scheme-based XML/HTML authoring, SXPath
; queries, and tree transformations. See SXML.html for more details.
; SXML is a term implementation of evaluation of the XML document [3].
; The other implementation is context-passing.

; The present frameworks fully supports the XML Namespaces Recommendation:
;	@uref{http://www.w3.org/TR/REC-xml-names/}
; Other links:
;@table @asis
;@item [1]
; Jeremy Gibbons, Geraint Jones, "The Under-appreciated Unfold,"
; Proc. ICFP'98, 1998, pp. 273-279.
;@item [2]
; Richard S. Bird, The promotion and accumulation strategies in
; transformational programming, ACM Trans. Progr. Lang. Systems,
; 6(4):487-504, October 1984.
;@item [3]
; Ralf Hinze, "Deriving Backtracking Monad Transformers,"
; Functional Pearl. Proc ICFP'00, pp. 186-197.
;@end table
;;
;;; Code:

(define-module (sxml ssax)
  #:use-module (sxml ssax input-parse)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)

  #:export     (current-ssax-error-port
                with-ssax-error-to-port
                xml-token? xml-token-kind xml-token-head
                make-empty-attlist attlist-add
                attlist-null?
                attlist-remove-top
                attlist->alist attlist-fold
                define-parsed-entity!
                reset-parsed-entity-definitions!
                ssax:uri-string->symbol
                ssax:skip-internal-dtd
                ssax:read-pi-body-as-string
                ssax:reverse-collect-str-drop-ws
                ssax:read-markup-token
                ssax:read-cdata-body
                ssax:read-char-ref
                ssax:read-attributes
                ssax:complete-start-tag
                ssax:read-external-id
                ssax:read-char-data
                ssax:xml->sxml
                ssax:make-parser
                ssax:make-pi-parser
                ssax:make-elem-parser))

(define (parser-error port message . rest)
  (apply throw 'parser-error port message rest))
(define ascii->char integer->char)
(define char->ascii char->integer)

(define current-ssax-error-port
  (make-parameter (current-error-port)))

(define *current-ssax-error-port*
  (parameter-fluid current-ssax-error-port))

(define (with-ssax-error-to-port port thunk)
  (parameterize ((current-ssax-error-port port))
    (thunk)))

(define (ssax:warn port . args)
  (with-output-to-port (current-ssax-error-port)
    (lambda ()
      (display ";;; SSAX warning: ")
      (for-each display args)
      (newline))))

(define (ucscode->string codepoint)
  (string (integer->char codepoint)))

(define char-newline #\newline)
(define char-return #\return)
(define char-tab #\tab)
(define nl "\n")

;; This isn't a great API, but a more proper fix will involve hacking
;; SSAX.
(define (reset-parsed-entity-definitions!)
  "Restore the set of parsed entity definitions to its initial state."
  (set! ssax:predefined-parsed-entities
        '((amp . "&")
          (lt . "<")
          (gt . ">")
          (apos . "'")
          (quot . "\""))))

(define (define-parsed-entity! entity str)
  "Define a new parsed entity. @var{entity} should be a symbol.

Instances of &@var{entity}; in XML text will be replaced with the
string @var{str}, which will then be parsed."
  (set! ssax:predefined-parsed-entities
        (acons entity str ssax:predefined-parsed-entities)))

;; Execute a sequence of forms and return the result of the _first_ one.
;; Like PROG1 in Lisp. Typically used to evaluate one or more forms with
;; side effects and return a value that must be computed before some or
;; all of the side effects happen.
(define-syntax begin0
  (syntax-rules ()
    ((begin0 form form1 ... ) 
      (let ((val form)) form1 ... val))))

; Like let* but allowing for multiple-value bindings
(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () . bodies) (begin . bodies))
    ((let*-values (((var) initializer) . rest) . bodies)
      (let ((var initializer))		; a single var optimization
	(let*-values rest . bodies)))
    ((let*-values ((vars initializer) . rest) . bodies)
      (call-with-values (lambda () initializer) ; the most generic case
	(lambda vars (let*-values rest . bodies))))))

;; needed for some dumb reason
(define inc 1+)
(define dec 1-)

(define-syntax include-from-path/filtered
  (lambda (x)
    (define (read-filtered accept-list file)
      (with-input-from-file (%search-load-path file)
        (lambda ()
          (let loop ((sexp (read)) (out '()))
            (cond
             ((eof-object? sexp) (reverse out))
             ((and (pair? sexp) (memq (car sexp) accept-list))
              (loop (read) (cons sexp out)))
             (else
              (loop (read) out)))))))
    (syntax-case x ()
      ((_ accept-list file)
       (with-syntax (((exp ...) (datum->syntax
                                 x 
                                 (read-filtered
                                  (syntax->datum #'accept-list)
                                  (syntax->datum #'file)))))
         #'(begin exp ...))))))

(include-from-path "sxml/upstream/assert.scm")
(include-from-path/filtered
 (define define-syntax ssax:define-labeled-arg-macro)
 "sxml/upstream/SSAX.scm")
