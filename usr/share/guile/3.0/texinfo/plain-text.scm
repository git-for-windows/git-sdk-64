;;;; (texinfo plain-text) -- rendering stexinfo as plain text
;;;;
;;;; 	Copyright (C) 2009, 2010, 2011, 2013, 2020  Free Software Foundation, Inc.
;;;;    Copyright (C) 2003,2004,2009  Andy Wingo <wingo at pobox dot com>
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
;;Transformation from stexi to plain-text. Strives to re-create the
;;output from @code{info}; comes pretty damn close.
;;        
;;; Code:

(define-module (texinfo plain-text)
  #:use-module (texinfo)
  #:use-module (texinfo string-utils)
  #:use-module (sxml transform)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 match)
  #:export (stexi->plain-text
            *line-width*))

;; The return value is a string.
(define (arg-ref key %-args)
  (and=> (and=> (assq key (cdr %-args)) cdr)
         stexi->plain-text))
(define (arg-req key %-args)
  (or (arg-ref key %-args)
      (error "Missing argument:" key %-args)))

(define (make-ticker str)
  (lambda () str))
(define (make-enumerator n)
  (lambda ()
    (let ((last n))
      (set! n (1+ n))
      (format #f "~A. " last))))

(define *indent* (make-fluid ""))
(define *itemizer* (make-fluid (make-ticker "* ")))
(define *line-width* (make-fluid 72))

(define-macro (with-indent n . body)
  `(with-fluids ((*indent* (string-append (fluid-ref *indent*)
                                          (make-string ,n #\space))))
     ,@body))

(define (make-indenter n proc)
  (lambda args (with-indent n (apply proc args))))

(define (string-indent str)
  (string-append (fluid-ref *indent*) str "\n"))

(define-macro (with-itemizer itemizer . body)
  `(with-fluids ((*itemizer* ,itemizer))
     ,@body))

(define (wrap* . strings)
  (let ((indent (fluid-ref *indent*))
        (width  (fluid-ref *line-width*)))
    (fill-string (string-concatenate strings)
                 #:line-width width #:initial-indent indent
                 #:subsequent-indent indent)))
(define (wrap . strings)
  (string-append (apply wrap* strings) "\n\n"))
(define (wrap-heading . strings)
  (string-append (apply wrap* strings) "\n"))

(define (ref tag args)
  (let* ((node (arg-req 'node args))
         (name (or (arg-ref 'name args) node))
         (manual (arg-ref 'manual args)))
    (string-concatenate
     (cons*
      (or (and=> (assq tag '((xref "See ") (pxref "see "))) cadr) "")
      name
      (if manual `(" in manual " ,manual) '())))))

(define (uref tag args)
  (let ((url (arg-req 'url args))
        (title (arg-ref 'title args)))
    (if title
        (string-append title " (" url ")")
        (string-append "`" url "'"))))

(define (def tag args . body)
  (define (first-line)
    (string-join
     (filter identity
             (map (lambda (x) (arg-ref x args))
                  '(data-type class name arguments)))
     " "))

  (let* ((category (case tag
                     ((defun) "Function")
                     ((defspec) "Special Form")
                     ((defvar) "Variable")
                     (else (arg-req 'category args)))))
    (string-append
     (wrap-heading (string-append " - " category ": " (first-line)))
     (with-indent 5 (stexi->plain-text body)))))

(define (enumerate tag . elts)
  (define (tonumber start)
    (let ((c (string-ref start 0)))
      (cond ((number? c) (string->number start))
            (else (1+ (- (char->integer c)
                         (char->integer (if (char-upper-case? c) #\A #\a))))))))
  (let* ((args? (and (pair? elts) (pair? (car elts))
                     (eq? (caar elts) '%)))
         (start (and args? (arg-ref 'start (car elts)))))
    (with-itemizer (make-enumerator (if start (tonumber start) 1))
      (with-indent 5
        (stexi->plain-text (if start (cdr elts) elts))))))

(define (itemize tag args . elts)
  (with-itemizer (make-ticker "* ")
    (with-indent 5
      (stexi->plain-text elts))))

(define (item tag . elts)
  (let* ((ret (stexi->plain-text elts))
         (tick ((fluid-ref *itemizer*)))
         (tick-pos (- (string-length (fluid-ref *indent*))
                      (string-length tick))))
    (if (and (not (string-null? ret)) (not (negative? tick-pos)))
        (string-copy! ret tick-pos tick))
    ret))

(define (table tag args . body)
  (stexi->plain-text body))

(define (entry tag args . body)
  (let ((heading (wrap-heading
                  (stexi->plain-text (arg-req 'heading args)))))
    (string-append heading
                   (with-indent 5 (stexi->plain-text body)))))

(define (make-underliner char)
  (lambda (tag . body)
    (let ((str (stexi->plain-text body)))
      (string-append
       "\n"
       (string-indent str)
       (string-indent (make-string (string-length str) char))
       "\n"))))

(define chapter (make-underliner #\*))
(define section (make-underliner #\=))
(define subsection (make-underliner #\-))
(define subsubsection (make-underliner #\.))

(define (example tag . body)
  (let ((ret (stexi->plain-text body)))
    (string-append
     (string-concatenate
      (with-indent 5 (map string-indent (string-split ret #\newline))))
     "\n")))

(define (verbatim tag . body)
  (let ((ret (stexi->plain-text body)))
    (string-append
     (string-concatenate
      (map string-indent (string-split ret #\newline)))
     "\n")))

(define (fragment tag . body)
  (string-concatenate (map-in-order stexi->plain-text body)))

(define (para tag . body)
  (wrap (stexi->plain-text body)))

(define (make-surrounder str)
  (lambda (tag . body)
    (string-append str (stexi->plain-text body) str)))

(define (code tag . body)
  (string-append "`" (stexi->plain-text body) "'"))

(define (key tag . body)
  (string-append "<" (stexi->plain-text body) ">"))

(define (var tag . body)
  (string-upcase (stexi->plain-text body)))

(define (acronym tag . elts)
  (match elts
    ((('% ('acronym text)))
     (stexi->plain-text text))
    ((('% ('acronym text) ('meaning . body)))
     (string-append (stexi->plain-text text)
                    " ("
                    (string-concatenate (map stexi->plain-text body))
                    ")"))))

(define (passthrough tag . body)
  (stexi->plain-text body))

(define (texinfo tag args . body)
  (let ((title (chapter 'foo (arg-req 'title args))))
    (string-append title (stexi->plain-text body))))

(define ignore-list
  '(page setfilename setchapternewpage iftex ifinfo ifplaintext ifxml sp vskip
    menu ignore syncodeindex comment c % node anchor))
(define (ignored? tag)
  (memq tag ignore-list))

(define tag-handlers
  `((title        ,chapter)
    (chapter      ,chapter)
    (section      ,section)
    (subsection   ,subsection)
    (subsubsection ,subsubsection)
    (appendix     ,chapter)
    (appendixsec  ,section)
    (appendixsubsec ,subsection)
    (appendixsubsubsec ,subsubsection)
    (unnumbered   ,chapter)
    (unnumberedsec ,section)
    (unnumberedsubsec ,subsection)
    (unnumberedsubsubsec ,subsubsection)
    (majorheading ,chapter)
    (chapheading  ,chapter)
    (heading      ,section)
    (subheading   ,subsection)
    (subsubheading ,subsubsection)

    (strong       ,(make-surrounder "*"))
    (sample       ,code)
    (samp         ,code)
    (code         ,code)
    (math         ,passthrough)
    (kbd          ,code)
    (key          ,key)
    (var          ,var)
    (env          ,code)
    (file         ,code)
    (command      ,code)
    (option       ,code)
    (url          ,code)
    (dfn          ,(make-surrounder "\""))
    (cite         ,(make-surrounder "\""))
    (acro         ,acronym)                       ;XXX: useless?
    (acronym      ,acronym)
    (email        ,key)
    (emph         ,(make-surrounder "_"))
    (sc           ,var)
    (copyright    ,(lambda args "(C)"))
    (result       ,(lambda args "==>"))
    (dots         ,(lambda args "..."))
    (xref         ,ref)
    (ref          ,ref)
    (pxref        ,ref)
    (uref         ,uref)

    (texinfo      ,texinfo)
    (quotation    ,(make-indenter 5 para))
    (itemize      ,itemize)
    (enumerate    ,enumerate)
    (item         ,item)
    (table        ,table)
    (entry        ,entry)
    (example      ,example)
    (lisp         ,example)
    (smallexample ,example)
    (smalllisp    ,example)
    (verbatim     ,verbatim)
    (*fragment*   ,fragment)

    (deftp        ,def)
    (defcv        ,def)
    (defivar      ,def)
    (deftypeivar  ,def)
    (defop        ,def)
    (deftypeop    ,def)
    (defmethod    ,def)
    (deftypemethod  ,def)
    (defopt       ,def)
    (defvr        ,def)
    (defvar       ,def)
    (deftypevr    ,def)
    (deftypevar   ,def)
    (deffn        ,def) 
    (deftypefn    ,def)
    (defmac       ,def)
    (defspec      ,def)
    (defun        ,def)
    (deftypefun   ,def)))

(define (stexi->plain-text tree)
  "Transform @var{tree} into plain text. Returns a string."
  (match tree
    (() "")
    ((? string?) tree)
    (((? symbol? tag) body ...)
     (let ((handler (and (not (ignored? tag))
                         (or (and=> (assq tag tag-handlers) cadr)
                             para))))
       (if handler
           (apply handler tree)
           "")))
    ((tree ...)
     (string-concatenate (map-in-order stexi->plain-text tree)))
    (_ "")))

;;; arch-tag: f966c3f6-3b46-4790-bbf9-3ad27e4917c2
