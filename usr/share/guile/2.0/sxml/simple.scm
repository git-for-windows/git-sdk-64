;;;; (sxml simple) -- a simple interface to the SSAX parser
;;;;
;;;; 	Copyright (C) 2009, 2010, 2013  Free Software Foundation, Inc.
;;;;    Modified 2004 by Andy Wingo <wingo at pobox dot com>.
;;;;    Originally written by Oleg Kiselyov <oleg at pobox dot com> as SXML-to-HTML.scm.
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
;;A simple interface to XML parsing and serialization.
;;
;;; Code:

(define-module (sxml simple)
  #:use-module (sxml ssax input-parse)
  #:use-module (sxml ssax)
  #:use-module (sxml transform)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-13)
  #:export (xml->sxml sxml->xml sxml->string))

;; Helpers from upstream/SSAX.scm.
;;

;     ssax:reverse-collect-str LIST-OF-FRAGS -> LIST-OF-FRAGS
; given the list of fragments (some of which are text strings)
; reverse the list and concatenate adjacent text strings.
; We can prove from the general case below that if LIST-OF-FRAGS
; has zero or one element, the result of the procedure is equal?
; to its argument. This fact justifies the shortcut evaluation below.
(define (ssax:reverse-collect-str fragments)
  (cond
    ((null? fragments) '())	; a shortcut
    ((null? (cdr fragments)) fragments) ; see the comment above
    (else
      (let loop ((fragments fragments) (result '()) (strs '()))
	(cond
	  ((null? fragments)
	    (if (null? strs) result
	      (cons (string-concatenate/shared strs) result)))
	  ((string? (car fragments))
	    (loop (cdr fragments) result (cons (car fragments) strs)))
	  (else
	    (loop (cdr fragments)
	      (cons
		(car fragments)
		(if (null? strs) result
		  (cons (string-concatenate/shared strs) result)))
	      '())))))))

(define (read-internal-doctype-as-string port)
  (string-concatenate/shared
    (let loop ()
      (let ((fragment
	     (next-token '() '(#\]) "reading internal DOCTYPE" port)))
	(if (eqv? #\> (peek-next-char port))
	    (begin
	      (read-char port)
	      (cons fragment '()))
	    (cons* fragment "]" (loop)))))))

;; Ideas for the future for this interface:
;;
;;  * Allow doctypes to provide parsed entities
;;
;;  * Allow validation (the ELEMENTS value from the DOCTYPE handler
;;    below)
;;
;;  * Parse internal DTDs
;;
;;  * Parse external DTDs
;;
(define* (xml->sxml #:optional (string-or-port (current-input-port)) #:key
                    (namespaces '())
                    (declare-namespaces? #t)
                    (trim-whitespace? #f)
                    (entities '())
                    (default-entity-handler #f)
                    (doctype-handler #f))
  "Use SSAX to parse an XML document into SXML. Takes one optional
argument, @var{string-or-port}, which defaults to the current input
port."
  ;; NAMESPACES: alist of PREFIX -> URI.  Specifies the symbol prefix
  ;; that the user wants on elements of a given namespace in the
  ;; resulting SXML, regardless of the abbreviated namespaces defined in
  ;; the document by xmlns attributes.  If DECLARE-NAMESPACES? is true,
  ;; these namespaces are treated as if they were declared in the DTD.

  ;; ENTITIES: alist of SYMBOL -> STRING.

  ;; NAMESPACES: list of (DOC-PREFIX . (USER-PREFIX . URI)).
  ;; A DOC-PREFIX of #f indicates that it comes from the user.
  ;; Otherwise, prefixes are symbols.
  (define (munge-namespaces namespaces)
    (map (lambda (el)
           (match el
             ((prefix . uri-string)
              (cons* (and declare-namespaces? prefix)
                     prefix
                     (ssax:uri-string->symbol uri-string)))))
         namespaces))

  (define (user-namespaces)
    (munge-namespaces namespaces))

  (define (user-entities)
    (if (and default-entity-handler
             (not (assq '*DEFAULT* entities)))
        (acons '*DEFAULT* default-entity-handler entities)
        entities))

  (define (name->sxml name)
    (match name
      ((prefix . local-part)
       (symbol-append prefix (string->symbol ":") local-part))
      (_ name)))

  (define (doctype-continuation seed)
    (lambda* (#:key (entities '()) (namespaces '()))
      (values #f
              (append entities (user-entities))
              (append (munge-namespaces namespaces) (user-namespaces))
              seed)))

  ;; The SEED in this parser is the SXML: initialized to '() at each new
  ;; level by the fdown handlers; built in reverse by the fhere parsers;
  ;; and reverse-collected by the fup handlers.
  (define parser
    (ssax:make-parser
     NEW-LEVEL-SEED ; fdown
     (lambda (elem-gi attributes namespaces expected-content seed)
       '())
   
     FINISH-ELEMENT ; fup
     (lambda (elem-gi attributes namespaces parent-seed seed)
       (let ((seed (if trim-whitespace?
                       (ssax:reverse-collect-str-drop-ws seed)
                       (ssax:reverse-collect-str seed)))
             (attrs (attlist-fold
                     (lambda (attr accum)
                       (cons (list (name->sxml (car attr)) (cdr attr))
                             accum))
                     '() attributes)))
         (acons (name->sxml elem-gi)
                (if (null? attrs)
                    seed
                    (cons (cons '@ attrs) seed))
                parent-seed)))

     CHAR-DATA-HANDLER ; fhere
     (lambda (string1 string2 seed)
       (if (string-null? string2)
           (cons string1 seed)
           (cons* string2 string1 seed)))

     DOCTYPE
     ;; -> ELEMS ENTITIES NAMESPACES SEED
     ;;
     ;; ELEMS is for validation and currently unused.
     ;;
     ;; ENTITIES is an alist of parsed entities (symbol -> string).
     ;;
     ;; NAMESPACES is as above.
     ;;
     ;; SEED builds up the content.
     (lambda (port docname systemid internal-subset? seed)
       (call-with-values
           (lambda ()
             (cond
              (doctype-handler
               (doctype-handler docname systemid
                                (and internal-subset?
                                     (read-internal-doctype-as-string port))))
              (else
               (when internal-subset?
                 (ssax:skip-internal-dtd port))
               (values))))
         (doctype-continuation seed)))

     UNDECL-ROOT
     ;; This is like the DOCTYPE handler, but for documents that do not
     ;; have a <!DOCTYPE!> entry.
     (lambda (elem-gi seed)
       (call-with-values
           (lambda ()
             (if doctype-handler
                 (doctype-handler #f #f #f)
                 (values)))
        (doctype-continuation seed)))

     PI
     ((*DEFAULT*
       . (lambda (port pi-tag seed)
           (cons
            (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
            seed))))))

  (let* ((port (if (string? string-or-port)
                   (open-input-string string-or-port)
                   string-or-port))
         (elements (reverse (parser port '()))))
    `(*TOP* ,@elements)))

(define check-name
  (let ((*good-cache* (make-hash-table)))
    (lambda (name)
      (if (not (hashq-ref *good-cache* name))
          (let* ((str (symbol->string name))
                 (i (string-index str #\:))
                 (head (or (and i (substring str 0 i)) str))
                 (tail (and i (substring str (1+ i)))))
            (and i (string-index (substring str (1+ i)) #\:)
                 (error "Invalid QName: more than one colon" name))
            (for-each
             (lambda (s)
               (and s
                    (or (char-alphabetic? (string-ref s 0))
                        (eq? (string-ref s 0) #\_)
                        (error "Invalid name starting character" s name))
                    (string-for-each
                     (lambda (c)
                       (or (char-alphabetic? c) (string-index "0123456789.-_" c)
                           (error "Invalid name character" c s name)))
                     s)))
             (list head tail))
            (hashq-set! *good-cache* name #t))))))

;; The following two functions serialize tags and attributes. They are
;; being used in the node handlers for the post-order function, see
;; below.

(define (attribute-value->xml value port)
  (cond
   ((pair? value)
    (attribute-value->xml (car value) port)
    (attribute-value->xml (cdr value) port))
   ((null? value)
    *unspecified*)
   ((string? value)
    (string->escaped-xml value port))
   ((procedure? value)
    (with-output-to-port port value))
   (else
    (string->escaped-xml
     (call-with-output-string (lambda (port) (display value port)))
     port))))

(define (attribute->xml attr value port)
  (check-name attr)
  (display attr port)
  (display "=\"" port)
  (attribute-value->xml value port)
  (display #\" port))

(define (element->xml tag attrs body port)
  (check-name tag)
  (display #\< port)
  (display tag port)
  (if attrs
      (let lp ((attrs attrs))
        (if (pair? attrs)
            (let ((attr (car attrs)))
              (display #\space port)
              (if (pair? attr)
                  (attribute->xml (car attr) (cdr attr) port)
                  (error "bad attribute" tag attr))
              (lp (cdr attrs)))
            (if (not (null? attrs))
                (error "bad attributes" tag attrs)))))
  (if (pair? body)
      (begin
        (display #\> port)
        (let lp ((body body))
          (cond
           ((pair? body)
            (sxml->xml (car body) port)
            (lp (cdr body)))
           ((null? body)
            (display "</" port)
            (display tag port)
            (display ">" port))
           (else
            (error "bad element body" tag body)))))
      (display " />" port)))

;; FIXME: ensure name is valid
(define (entity->xml name port)
  (display #\& port)
  (display name port)
  (display #\; port))

;; FIXME: ensure tag and str are valid
(define (pi->xml tag str port)
  (display "<?" port)
  (display tag port)
  (display #\space port)
  (display str port)
  (display "?>" port))

(define* (sxml->xml tree #:optional (port (current-output-port)))
  "Serialize the sxml tree @var{tree} as XML. The output will be written
to the current output port, unless the optional argument @var{port} is
present."
  (cond
   ((pair? tree)
    (if (symbol? (car tree))
        ;; An element.
        (let ((tag (car tree)))
          (case tag
            ((*TOP*)
             (sxml->xml (cdr tree) port))
            ((*ENTITY*)
             (if (and (list? (cdr tree)) (= (length (cdr tree)) 1))
                 (entity->xml (cadr tree) port)
                 (error "bad *ENTITY* args" (cdr tree))))
            ((*PI*)
             (if (and (list? (cdr tree)) (= (length (cdr tree)) 2))
                 (pi->xml (cadr tree) (caddr tree) port)
                 (error "bad *PI* args" (cdr tree))))
            (else
             (let* ((elems (cdr tree))
                    (attrs (and (pair? elems) (pair? (car elems))
                                (eq? '@ (caar elems))
                                (cdar elems))))
               (element->xml tag attrs (if attrs (cdr elems) elems) port)))))
        ;; A nodelist.
        (for-each (lambda (x) (sxml->xml x port)) tree)))
   ((string? tree)
    (string->escaped-xml tree port))
   ((null? tree) *unspecified*)
   ((not tree) *unspecified*)
   ((eqv? tree #t) *unspecified*)
   ((procedure? tree)
    (with-output-to-port port tree))
   (else
    (string->escaped-xml
     (call-with-output-string (lambda (port) (display tree port)))
     port))))

(define (sxml->string sxml)
  "Detag an sxml tree @var{sxml} into a string. Does not perform any
formatting."
  (string-concatenate-reverse
   (foldts
    (lambda (seed tree)                 ; fdown
      '())
    (lambda (seed kid-seed tree)        ; fup
      (append! kid-seed seed))
    (lambda (seed tree)                 ; fhere
      (if (string? tree) (cons tree seed) seed))
    '()
    sxml)))

(define (make-char-quotator char-encoding)
  (let ((bad-chars (list->char-set (map car char-encoding))))
 
    ;; Check to see if str contains one of the characters in charset,
    ;; from the position i onward. If so, return that character's index.
    ;; otherwise, return #f
    (define (index-cset str i charset)
      (string-index str charset i))
    
    ;; The body of the function
    (lambda (str port)
      (let ((bad-pos (index-cset str 0 bad-chars)))
        (if (not bad-pos)
            (display str port)          ; str had all good chars
            (let loop ((from 0) (to bad-pos))
              (cond
               ((>= from (string-length str)) *unspecified*)
               ((not to)
                (display (substring str from (string-length str)) port))
               (else
                (let ((quoted-char
                       (cdr (assv (string-ref str to) char-encoding)))
                      (new-to
                       (index-cset str (+ 1 to) bad-chars)))
                  (if (< from to)
                      (display (substring str from to) port))
                  (display quoted-char port)
                  (loop (1+ to) new-to))))))))))

;; Given a string, check to make sure it does not contain characters
;; such as '<' or '&' that require encoding. Return either the original
;; string, or a list of string fragments with special characters
;; replaced by appropriate character entities.

(define string->escaped-xml
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))

;;; arch-tag: 9c853b25-d82f-42ef-a959-ae26fdc7d1ac
;;; simple.scm ends here

