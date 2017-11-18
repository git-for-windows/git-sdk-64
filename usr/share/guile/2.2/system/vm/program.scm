;;; Guile VM program functions

;;; Copyright (C) 2001, 2009, 2010, 2013, 2014 Free Software Foundation, Inc.
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

(define-module (system vm program)
  #:use-module (ice-9 match)
  #:use-module (system vm debug)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (source:addr source:line source:column source:file
            source:line-for-user
            program-sources program-sources-pre-retire program-source

            program-address-range

            program-arities program-arity arity:start arity:end

            arity:nreq arity:nopt arity:rest? arity:kw arity:allow-other-keys?

            program-arguments-alist program-arguments-alists
            program-lambda-list

            program? program-code
            program-free-variables
            program-num-free-variables
            program-free-variable-ref program-free-variable-set!

            print-program

            primitive-code?))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_programs")

;; These procedures are called by programs.c.
(define (program-name program)
  (and=> (find-program-debug-info (program-code program))
         program-debug-info-name))
(define (program-documentation program)
  (find-program-docstring (program-code program)))
(define (program-minimum-arity program)
  (find-program-minimum-arity (program-code program)))
(define (program-properties program)
  (find-program-properties (program-code program)))

(define (source:addr source)
  (car source))
(define (source:file source)
  (cadr source))
(define (source:line source)
  (caddr source))
(define (source:column source)
  (cdddr source))

;; Lines are zero-indexed inside Guile, but users expect them to be
;; one-indexed. Columns, on the other hand, are zero-indexed to both. Go
;; figure.
(define (source:line-for-user source)
  (1+ (source:line source)))

(define (source-for-addr addr)
  (and=> (find-source-for-addr addr)
         (lambda (source)
           ;; FIXME: absolute or relative address?
           (cons* 0
                  (source-file source)
                  (source-line source)
                  (source-column source)))))

(define (program-sources proc)
  (map (lambda (source)
         (cons* (- (source-post-pc source) (program-code proc))
                (source-file source)
                (source-line source)
                (source-column source)))
       (find-program-sources (program-code proc))))

(define* (program-source proc ip #:optional (sources (program-sources proc)))
  (let lp ((source #f) (sources sources))
    (match sources
      (() source)
      (((and s (pc . _)) . sources)
       (if (<= pc ip)
           (lp s sources)
           source)))))

(define (program-address-range program)
  "Return the start and end addresses of @var{program}'s code, as a pair
of integers."
  (let ((pdi (find-program-debug-info (program-code program))))
    (and pdi
         (cons (program-debug-info-addr pdi)
               (+ (program-debug-info-addr pdi)
                  (program-debug-info-size pdi))))))

;; Source information could in theory be correlated with the ip of the
;; instruction, or the ip just after the instruction is retired. Guile
;; does the latter, to make backtraces easy -- an error produced while
;; running an opcode always happens after it has retired its arguments.
;;
;; But for breakpoints and such, we need the ip before the instruction
;; is retired -- before it has had a chance to do anything. So here we
;; change from the post-retire addresses given by program-sources to
;; pre-retire addresses.
;;
(define (program-sources-pre-retire proc)
  (map (lambda (source)
         (cons* (- (source-pre-pc source) (program-code proc))
                (source-file source)
                (source-line source)
                (source-column source)))
       (find-program-sources (program-code proc))))

(define (arity:start a)
  (match a ((start end . _) start) (_ (error "bad arity" a))))
(define (arity:end a)
  (match a ((start end . _) end) (_ (error "bad arity" a))))
(define (arity:nreq a)
  (match a ((_ _ nreq . _) nreq) (_ 0)))
(define (arity:nopt a)
  (match a ((_ _ nreq nopt . _) nopt) (_ 0)))
(define (arity:rest? a)
  (match a ((_ _ nreq nopt rest? . _) rest?) (_ #f)))
(define (arity:kw a)
  (match a ((_ _ nreq nopt rest? (_ . kw)) kw) (_ '())))
(define (arity:allow-other-keys? a)
  (match a ((_ _ nreq nopt rest? (aok . kw)) aok) (_ #f)))

(define (program-arity prog ip)
  (let ((arities (program-arities prog)))
    (and arities
         (let lp ((arities arities))
           (cond ((null? arities) #f)
                 ((not ip) (car arities)) ; take the first one
                 ((and (< (arity:start (car arities)) ip)
                       (<= ip (arity:end (car arities))))
                  (car arities))
                 (else (lp (cdr arities))))))))

(define (arglist->arguments-alist arglist)
  (match arglist
    ((req opt keyword allow-other-keys? rest . extents)
     `((required . ,req)
       (optional . ,opt)
       (keyword . ,keyword)
       (allow-other-keys? . ,allow-other-keys?)
       (rest . ,rest)
       (extents . ,extents)))
    (_ #f)))

(define* (arity->arguments-alist prog arity
                                 #:optional
                                 (make-placeholder
                                  (lambda (i) (string->symbol "_"))))
  (let lp ((nreq (arity:nreq arity)) (req '())
           (nopt (arity:nopt arity)) (opt '())
           (rest? (arity:rest? arity)) (rest #f)
           (n 0))
    (cond
     ((< 0 nreq)
      (lp (1- nreq) (cons (make-placeholder n) req)
          nopt opt rest? rest (1+ n)))
     ((< 0 nopt)
      (lp nreq req
          (1- nopt) (cons (make-placeholder n) opt)
          rest? rest (1+ n)))
     (rest?
      (lp nreq req nopt opt
          #f (make-placeholder (+ n (length (arity:kw arity))))
          (1+ n)))
     (else
      `((required . ,(reverse req))
        (optional . ,(reverse opt))
        (keyword . ,(arity:kw arity))
        (allow-other-keys? . ,(arity:allow-other-keys? arity))
        (rest . ,rest))))))

;; the name "program-arguments" is taken by features.c...
(define* (program-arguments-alist prog #:optional ip)
  "Returns the signature of the given procedure in the form of an association list."
  (let ((code (program-code prog)))
    (cond
     ((primitive-code? code)
      (match (procedure-minimum-arity prog)
        (#f #f)
        ((nreq nopt rest?)
         (let ((start (primitive-call-ip prog)))
           ;; Assume that there is only one IP for the call.
           (and (or (not ip) (= start ip))
                (arity->arguments-alist
                 prog
                 (list 0 0 nreq nopt rest? '(#f . ()))))))))
     (else
      (or-map (lambda (arity)
                (and (or (not ip)
                         (and (<= (arity-low-pc arity) ip)
                              (< ip (arity-high-pc arity))))
                     (arity-arguments-alist arity)))
              (or (find-program-arities code) '()))))))

(define* (program-lambda-list prog #:optional ip)
  "Returns the signature of the given procedure in the form of an argument list."
  (and=> (program-arguments-alist prog ip) arguments-alist->lambda-list))

(define (arguments-alist->lambda-list arguments-alist)
  (let ((req (or (assq-ref arguments-alist 'required) '()))
        (opt (or (assq-ref arguments-alist 'optional) '()))
        (key (map keyword->symbol
                  (map car (or (assq-ref arguments-alist 'keyword) '()))))
        (rest (or (assq-ref arguments-alist 'rest) '())))
    `(,@req
      ,@(if (pair? opt) (cons #:optional opt) '())
      ,@(if (pair? key) (cons #:key key) '())
      . ,rest)))

(define (program-free-variables prog)
  "Return the list of free variables of PROG."
  (let ((count (program-num-free-variables prog)))
    (unfold (lambda (i) (>= i count))
            (cut program-free-variable-ref prog <>)
            1+
            0)))

(define (program-arguments-alists prog)
  "Returns all arities of the given procedure, as a list of association
lists."
  (define (fallback)
    (match (procedure-minimum-arity prog)
      (#f '())
      ((nreq nopt rest?)
       (list
        (arity->arguments-alist
         prog
         (list 0 0 nreq nopt rest? '(#f . ())))))))
  (let* ((code (program-code prog))
         (arities (and (not (primitive-code? code))
                       (find-program-arities code))))
    (if arities
        (map arity-arguments-alist arities)
        (fallback))))

(define* (print-program #:optional program (port (current-output-port))
                        #:key (addr (program-code program))
                        (always-print-addr? #f) (never-print-addr? #f)
                        (always-print-source? #f) (never-print-source? #f)
                        (name-only? #f) (print-formals? #t))
  (let* ((pdi (find-program-debug-info addr))
         ;; It could be the procedure had its name property set via the
         ;; procedure property interface.
         (name (or (and program (procedure-name program))
                   (and pdi (program-debug-info-name pdi))))
         (source (match (find-program-sources addr)
                   (() #f)
                   ((source . _) source)))
         (formals (if program
                      (program-arguments-alists program)
                      (let ((arities (find-program-arities addr)))
                        (if arities
                            (map arity-arguments-alist arities)
                            '())))))
    (define (hex n)
      (number->string n 16))

    (cond
     ((and name-only? name)
      (format port "~a" name))
     (else
      (format port "#<procedure")
      (format port " ~a"
              (or name
                  (and program (hex (object-address program)))
                  (if never-print-addr?
                      ""
                      (string-append "@" (hex addr)))))
      (when (and always-print-addr? (not never-print-addr?))
        (unless (and (not name) (not program))
          (format port " @~a" (hex addr))))
      (when (and source (not never-print-source?)
                 (or always-print-source? (not name)))
        (format port " at ~a:~a:~a"
                (or (source-file source) "<unknown port>")
                (source-line-for-user source)
                (source-column source)))
      (unless (or (null? formals) (not print-formals?))
        (format port "~a"
                (string-append
                 " " (string-join (map (lambda (a)
                                         (object->string
                                          (arguments-alist->lambda-list a)))
                                       formals)
                                  " | "))))
      (format port ">")))))

(define (write-program prog port)
  (print-program prog port))
