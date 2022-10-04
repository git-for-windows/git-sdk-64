;;;; (texinfo serialize) -- rendering stexinfo as texinfo
;;;;
;;;; 	Copyright (C) 2009, 2012, 2013  Free Software Foundation, Inc.
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
;;Serialization of @code{stexi} to plain texinfo.
;;
;;; Code:

(define-module (texinfo serialize)
  #:use-module (texinfo)
  #:use-module (texinfo string-utils)
  #:use-module (sxml transform)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (stexi->texi))

(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
      (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
        (if (null? l) (reverse dest)
            (loop (cdr l) (cons (car l) (cons elem dest)))))))

;; converts improper lists to proper lists.
(define (filter* pred l)
  (let lp ((in l) (out '()))
    (cond ((null? in)
           (reverse! out))
          ((pair? in)
           (lp (cdr in) (if (pred (car in)) (cons (car in) out) out)))
          (else
           (lp '() (if (pred in) (cons in out) out))))))

;; (list* 'a '(b c) 'd '(e f g)) => '(a b c d e f g)
(define (list* . args)
  (let* ((args (reverse args))
         (tail (car args)))
    (let lp ((in (cdr args)) (out tail))
      (cond ((null? in) out)
            ((pair? (car in)) (lp (cdr in) (append (car in) out)))
            ((null? (car in)) (lp (cdr in) out))
            (else (lp (cdr in) (cons (car in) out)))))))

;; Why? Well, because syntax-case defines `include', and carps about its
;; wrong usage below...
(eval-when (expand load eval)
  (define (include exp lp command type formals rest? args accum)
    (list* "\n"
           (list-intersperse
            args
            " ")
           " " command "@" accum)))

(define (empty-command exp lp command type formals rest? args accum)
  (list* " " command "@" accum))

(define (inline-text exp lp command type formals rest? args accum)
  (if (not (string=? command "*braces*")) ;; fixme :(
      (list* "}"
             (append-map (lambda (x) (lp x '())) (reverse (cdr exp)))
             "{" command "@" accum)
      (list* "@}"
             (append-map (lambda (x) (lp x '())) (reverse (cdr exp)))
             "@{" accum)))

(define (inline-args exp lp command type formals rest? args accum)
  (list* "}"
         (if (not args) ""
             (list-intersperse
              (map
               (lambda (x)
                 (cond ((not x) "")
                       ((pair? x)
                        (if (pair? (cdr x))
                            (warn "Strange inline-args!" args))
                        (car x))
                       (else (error "Invalid inline-args" args))))
               (drop-while not
                           (map (lambda (x) (assq-ref args x))
                                (reverse formals))))
              ","))
         "{" command "@" accum))

(define (inline-text-args exp lp command type formals rest? args accum)
  (list* "}"
         (if (not args) ""
             (apply
              append
              (list-intersperse
               (map
                (lambda (x) (append-map (lambda (x) (lp x '())) (reverse x)))
                (drop-while not
                            (map (lambda (x) (assq-ref args x))
                                 (reverse formals))))
               '(","))))
         "{" command "@" accum))

(define (embrace x)
  (define (needs-embrace? x)
    (define (has-space? x)
      (and (string? x)
           (string-index x char-set:whitespace)))
    (or (null? x) (or-map has-space? x)))
  (if (needs-embrace? x)
      (append '("}") x '("{"))
      x))

(define (serialize-text-args lp formals rest? args)
  (define (serialize-arg formal rest?)
    (let ((val (assq-ref args formal)))
      (if val
          (let ((out (append-map (lambda (x) (lp x '()))
                                 (reverse val))))
            (if rest?
                out
                (embrace out)))
          #f)))
  (define (serialize-args rformals rest?)
    (match rformals
      (() '())
      ((formal . rformals)
       (cons (serialize-arg formal rest?)
             (serialize-args rformals #f)))))
  (apply append
         (list-intersperse
          (filter identity (serialize-args (reverse formals) rest?))
          '(" "))))

(define (eol-text-args exp lp command type formals rest? args accum)
  (list* "\n"
         (serialize-text-args lp formals rest? args)
         " " command "@" accum))

(define (eol-text exp lp command type formals rest? args accum)
  (list* "\n"
         (append-map (lambda (x) (lp x '()))
                     (reverse (if args (cddr exp) (cdr exp))))
         " " command "@" accum))

(define (eol-args exp lp command type formals rest? args accum)
  (list* "\n"
         (list-intersperse
          (apply append
                 (drop-while not
                             (map (lambda (x) (assq-ref args x))
                                  (reverse formals))))
          ", ")
         " " command "@" accum))

(define (environ exp lp command type formals rest? args accum)
  (case (car exp)
    ((texinfo)
     (list* "@bye\n"
            (append-map (lambda (x) (lp x '())) (reverse (cddr exp)))
            "\n@c %**end of header\n\n"
            (reverse (assq-ref args 'title)) "@settitle "
            (or (and=> (assq-ref args 'filename)
                       (lambda (filename)
                         (cons "\n" (reverse (cons "@setfilename " filename)))))
                "")
            "\\input texinfo   @c -*-texinfo-*-\n@c %**start of header\n"
            accum))
    (else
     (list* "\n\n" command "@end "
            (let ((body (append-map (lambda (x) (lp x '()))
                                    (reverse (if args (cddr exp) (cdr exp))))))
              (if (or (null? body)
                      (eqv? (string-ref (car body)
                                        (1- (string-length (car body))))
                            #\newline))
                  body
                  (cons "\n" body)))
            "\n"
            (serialize-text-args lp formals rest? args)
            " " command "@" accum))))

(define (table-environ exp lp command type formals rest? args accum)
  (list* "\n\n" command "@end "
         (append-map (lambda (x) (lp x '()))
                     (reverse (if args (cddr exp) (cdr exp))))
         "\n"
         (let* ((arg (if args (cadar args) ""))) ;; zero or one args
           (if (pair? arg)
               (list (symbol->string (car arg)) "@")
               arg))
         " " command "@" accum))

(define (wrap strings)
  (fill-string (string-concatenate strings)
               #:line-width 72
               #:break-long-words? #f))

(define (paragraph exp lp command type formals rest? args accum)
  (list* "\n\n"
         (wrap
          (reverse
           (append-map (lambda (x) (lp x '())) (reverse (cdr exp)))))
         accum))

(define (item exp lp command type formals rest? args accum)
  (list* (append-map (lambda (x) (lp x '())) (reverse (cdr exp)))
         "@item\n"
         accum))

(define (entry exp lp command type formals rest? args accum)
  (list* (append-map (lambda (x) (lp x '())) (reverse (cddr exp)))
         "\n"
         (append-map (lambda (x) (lp x '())) (reverse (cdar args)))
         "@item "
         accum))

(define (fragment exp lp command type formals rest? args accum)
  (list* "\n@c %end of fragment\n"
         (append-map (lambda (x) (lp x '())) (reverse (cdr exp)))
         "\n@c %start of fragment\n\n"
         accum))

(define serializers
  `((EMPTY-COMMAND . ,empty-command)
    (INLINE-TEXT . ,inline-text)
    (INLINE-ARGS . ,inline-args)
    (INLINE-TEXT-ARGS . ,inline-text-args)
    (EOL-TEXT . ,eol-text)
    (EOL-TEXT-ARGS . ,eol-text-args)
    (INDEX . ,eol-text-args)
    (EOL-ARGS . ,eol-args)
    (ENVIRON . ,environ)
    (TABLE-ENVIRON . ,table-environ)
    (ENTRY . ,entry)
    (ITEM . ,item)
    (PARAGRAPH . ,paragraph)
    (FRAGMENT . ,fragment)
    (#f . ,include))) ; support writing include statements

(define (serialize exp lp command type formals rest? args accum)
  ((or (assq-ref serializers type)
       (error "Unknown command type" exp type))
   exp lp command type formals rest? args accum))

(define escaped-chars '(#\} #\{ #\@))
(define (escape str)
  "Escapes any illegal texinfo characters (currently @{, @}, and @@)."
  (let loop ((in (string->list str)) (out '()))
    (if (null? in)
        (apply string (reverse out))
        (if (memq (car in) escaped-chars)
            (loop (cdr in) (cons* (car in) #\@ out))
            (loop (cdr in) (cons (car in) out))))))

(define (stexi->texi tree)
  "Serialize the stexi @var{tree} into plain texinfo."
  (string-concatenate-reverse
   (let lp ((in tree) (out '()))
     (cond
      ((or (not in) (null? in)) out)
      ((string? in) (cons (escape in) out))
      ((pair? in)
       (let ((command-spec (assq (car in) texi-command-specs)))
         (if (not command-spec)
             (begin
               (warn "Unknown stexi command, not rendering" in)
               out)
             (serialize in
                        lp
                        (symbol->string (car in))
                        (cadr command-spec)
                        (filter* symbol? (cddr command-spec))
                        (not (list? (cddr command-spec)))
                        (cond
                         ((and (pair? (cdr in)) (pair? (cadr in))
                               (eq? (caadr in) '%))
                          (cdadr in))
                         ((not (cadr command-spec))
                          ;; include
                          (cdr in))
                         (else
                          #f))
                        out))))
      (else
       (error "Invalid stexi" in))))))

;;; arch-tag: d3fa16ea-0bf7-4ec5-ab9f-3f08490f77f5
