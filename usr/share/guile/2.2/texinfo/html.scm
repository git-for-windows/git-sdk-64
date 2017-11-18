;;;; (texinfo html) -- translating stexinfo into shtml
;;;;
;;;; 	Copyright (C) 2009, 2010, 2011  Free Software Foundation, Inc.
;;;;    Copyright (C) 2003,2004,2009 Andy Wingo <wingo at pobox dot com>
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
;;This module implements transformation from @code{stexi} to HTML. Note
;;that the output of @code{stexi->shtml} is actually SXML with the HTML
;;vocabulary. This means that the output can be further processed, and
;;that it must eventually be serialized by
;;@ref{sxml simple sxml->xml,sxml->xml}.
;;        
;;References (i.e., the @code{@@ref} family of commands) are resolved by
;;a @dfn{ref-resolver}.
;;@xref{texinfo html add-ref-resolver!,add-ref-resolver!}, for more
;;information.
;;
;;; Code:

;; TODO: nice ref resolving API, default CSS stylesheet (esp. to remove
;; margin-top on dd > p)

(define-module (texinfo html)
  #:use-module (texinfo)
  #:use-module (sxml transform)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-13)
  #:export (stexi->shtml add-ref-resolver! urlify))

;; The caller is responsible for carring the returned list.
(define (arg-ref key %-args)
  (and=> (assq key (cdr %-args)) (lambda (x) (stexi->shtml (cdr x)))))
(define (arg-req key %-args)
  (or (arg-ref key %-args)
      (error "Missing argument:" key %-args)))
(define (car* x) (and x (car x)))

(define (urlify str)
  (string-downcase
   (string-map
    (lambda (c)
      (case c
        ((#\space #\/ #\:) #\-)
        (else c)))
    str)))

(define ref-resolvers 
  (list
   (lambda (node-name manual-name) ;; the default
     (urlify (string-append (or manual-name "") "#" node-name)))))

(define (add-ref-resolver! proc)
  "Add @var{proc} to the head of the list of ref-resolvers. @var{proc}
will be expected to take the name of a node and the name of a manual and
return the URL of the referent, or @code{#f} to pass control to the next
ref-resolver in the list.

The default ref-resolver will return the concatenation of the manual
name, @code{#}, and the node name."
  (set! ref-resolvers (cons proc ref-resolvers)))

(define (resolve-ref node manual)
  (or (or-map (lambda (x) (x node manual)) ref-resolvers)
      (error "Could not resolve reference" node manual)))

(define (ref tag args)
  (let* ((node (car (arg-req 'node args)))
         (section (or (car* (arg-ref 'section args)) node))
         (manual (car* (arg-ref 'manual args)))
         (target (resolve-ref node manual)))
    `(span ,(and=> (assq tag '((xref "See ") (pxref "see "))) cdr)
           (a (@ (href ,target)) ,section))))

(define (uref tag args)
  (let ((url (car (arg-req 'url args))))
    `(a (@ (href ,url)) ,(or (car* (arg-ref 'title args)) url))))

;; @!*&%( Mozilla gets confused at an empty ("<a .. />") a tag. Put an
;; empty string here to placate the reptile.
(define (node tag args)
  `(a (@ (name ,(urlify (car (arg-req 'name args))))) ""))

(define (def tag args . body)
  (define (code x) (and x (cons 'code x)))
  (define (var x) (and x (cons 'var x)))
  (define (b x) (and x (cons 'b x)))
  (define (list/spaces . elts)
    (let lp ((in elts) (out '()))
      (cond ((null? in) (reverse! out))
            ((null? (car in)) (lp (cdr in) out))
            (else (lp (cdr in)
                      (cons (car in)
                            (if (null? out) out (cons " " out))))))))
  (define (left-td-contents)
    (list/spaces (code (arg-ref 'data-type args))
                 (b (list (code (arg-ref 'class args)))) ;; is this right?
                 (b (list (code (arg-ref 'name args))))
                 (if (memq tag '(deftypeop deftypefn deftypefun))
                     (code (arg-ref 'arguments args))
                     (var (list (code (arg-ref 'arguments args)))))))

  (let* ((category (case tag
                     ((defun) "Function")
                     ((defspec) "Special Form")
                     ((defvar) "Variable")
                     (else (car (arg-req 'category args))))))
    `(div
      (table
       (@ (cellpadding "0") (cellspacing "0") (width "100%") (class "def"))
       (tr (td ,@(left-td-contents))
           (td (div (@ (class "right")) "[" ,category "]"))))
      (div (@ (class "description")) ,@body))))

(define (enumerate tag . elts)
  (define (tonumber start)
    (let ((c (string-ref start 0)))
      (cond ((number? c) (string->number start))
            (else (1+ (- (char->integer c)
                         (char->integer (if (char-upper-case? c) #\A #\a))))))))
  `(ol ,@(if (and (pair? elts) (pair? (car elts)) (eq? (caar elts) '%))
             (cons `(@ (start ,@(tonumber (arg-req 'start (car elts)))))
                       ;; (type ,(type (arg-ref 'start (car elts)))))
                   (cdr elts))
             elts)))

(define (itemize tag . elts)
  `(ul ,@(match elts
           ;; Strip `bullet' attribute.
           ((('% . attrs) . elts) elts)
           (elts elts))))

(define (acronym tag . elts)
  (match elts
    ;; FIXME: Need attribute matcher that doesn't depend on attribute
    ;; order.
    ((('% ('acronym text) . _)) `(acronym ,text))))

(define (table tag args . body)
  (let ((formatter (caar (arg-req 'formatter args))))
    (cons 'dl
          (map (lambda (x)
                 (cond ((and (pair? x) (eq? (car x) 'dt))
                        (list (car x) (cons formatter (cdr x))))
                       (else x)))
               (apply append body)))))

(define (entry tag args . body)
  (let lp ((out `((dt ,@(arg-req 'heading args))))
           (body body))
    (if (and (pair? body) (pair? (car body)) (eq? (caar body) 'itemx))
        (lp (append out `(dt ,@(map stexi->shtml (cdar body))))
            (cdr body))
        (append out `((dd ,@(map stexi->shtml body)))))))

(define tag-replacements
  '((titlepage    div (@ (class "titlepage")))
    (title        h2  (@ (class "title")))
    (subtitle     h3  (@ (class "subtitle")))
    (author       h3  (@ (class "author")))
    (example      pre)
    (lisp         pre)
    (smallexample pre (@ (class "smaller")))
    (smalllisp    pre (@ (class "smaller")))
    (cartouche    div (@ (class "cartouche")))
    (verbatim     pre (@ (class "verbatim")))
    (chapter      h2)
    (section      h3)
    (subsection   h4)
    (subsubsection       h5)
    (appendix     h2)
    (appendixsec  h3)
    (appendixsubsec      h4)
    (appendixsubsubsec   h5)
    (unnumbered   h2)
    (unnumberedsec       h3)
    (unnumberedsubsec    h4)
    (unnumberedsubsubsec h5)
    (majorheading h2)
    (chapheading  h2)
    (heading      h3)
    (subheading   h4)
    (subsubheading       h5)
    (quotation    blockquote)
    (item         li) ;; itemx ?
    (para         p)
    (*fragment*   div) ;; should be ok

    (asis         span)
    (bold         b)
    (sample       samp)
    (samp         samp)
    (code         code)
    (kbd          kbd)
    (key          code (@ (class "key")))
    (var          var)
    (env          code (@ (class "env")))
    (file         code (@ (class "file")))
    (command      code (@ (class "command")))
    (option       code (@ (class "option")))
    (url          code (@ (class "url")))
    (dfn          dfn)
    (cite         cite)
    (acro         acronym)
    (email        code (@ (class "email")))
    (emph         em)
    (strong       strong)
    (sc           span (@ (class "small-caps")))))

(define ignore-list
  '(page setfilename setchapternewpage iftex ifinfo ifplaintext ifxml sp vskip
    menu ignore syncodeindex comment c dircategory direntry top shortcontents
    cindex printindex))

(define rules
  `((% *preorder* . ,(lambda args args)) ;; Keep these around...
    (texinfo   . ,(lambda (tag args . body)
                    (pre-post-order
                     `(html
                       (@ (xmlns "http://www.w3.org/1999/xhtml"))
                       (head (title ,(car (arg-req 'title args))))
                       (body ,@body))
                     `((% *preorder* . ,(lambda args #f)) ;; ... filter out.
                       (*text*       . ,(lambda (tag x) x))
                       (*default*    . ,(lambda (tag . body)
                                          (cons tag body)))))))
    (copyright . ,(lambda args '(*ENTITY* "copy")))
    (result    . ,(lambda args '(*ENTITY* "rArr")))
    (xref . ,ref) (ref . ,ref) (pxref . ,ref)
    (uref . ,uref)
    (node . ,node) (anchor . ,node)
    (table . ,table)
    (enumerate . ,enumerate)
    (itemize . ,itemize)
    (acronym . ,acronym)
    (entry *preorder* . ,entry)

    (deftp . ,def) (defcv . ,def) (defivar . ,def) (deftypeivar . ,def)
    (defop . ,def) (deftypeop . ,def) (defmethod . ,def)
    (deftypemethod . ,def) (defopt . ,def) (defvr . ,def) (defvar . ,def)
    (deftypevr . ,def) (deftypevar . ,def) (deffn . ,def) 
    (deftypefn . ,def) (defmac . ,def) (defspec . ,def) (defun . ,def)
    (deftypefun . ,def)
    (ifnottex . ,(lambda (tag . body) body))
    (*text*    . ,(lambda (tag x) x))
    (*default* . ,(lambda (tag . body)
                    (let ((subst (assq tag tag-replacements)))
                      (cond
                       (subst (append (cdr subst) body))
                       ((memq tag ignore-list) #f)
                       (else 
                        (warn "Don't know how to convert" tag "to HTML")
                        body)))))))

(define (stexi->shtml tree)
  "Transform the stexi @var{tree} into shtml, resolving references via
ref-resolvers. See the module commentary for more details."
  (pre-post-order tree rules))

;;; arch-tag: ab05f3fe-9981-4a78-b64c-48efcd9983a6
