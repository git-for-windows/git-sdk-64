;;; Guile VM program functions

;;; Copyright (C) 2001, 2009, 2010, 2013 Free Software Foundation, Inc.
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
  #:use-module (system base pmatch)
  #:use-module (system vm instruction)
  #:use-module (system vm objcode)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (make-program

            make-binding binding:name binding:boxed? binding:index
            binding:start binding:end

            source:addr source:line source:column source:file
            source:line-for-user
            program-sources program-sources-pre-retire program-source

            program-bindings program-bindings-by-index program-bindings-for-ip
            program-arities program-arity arity:start arity:end

            arity:nreq arity:nopt arity:rest? arity:kw arity:allow-other-keys?

            program-arguments-alist program-lambda-list

            program-meta
            program-objcode program? program-objects
            program-module program-base
            program-free-variables
            program-num-free-variables
            program-free-variable-ref program-free-variable-set!))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_programs")

(define (make-binding name boxed? index start end)
  (list name boxed? index start end))
(define (binding:name b) (list-ref b 0))
(define (binding:boxed? b) (list-ref b 1))
(define (binding:index b) (list-ref b 2))
(define (binding:start b) (list-ref b 3))
(define (binding:end b) (list-ref b 4))

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

;; FIXME: pull this definition from elsewhere.
(define *bytecode-header-len* 8)

;; We could decompile the program to get this, but that seems like a
;; waste.
(define (bytecode-instruction-length bytecode ip)
  (let* ((idx (+ ip *bytecode-header-len*))
         (inst (opcode->instruction (bytevector-u8-ref bytecode idx))))
    ;; 1+ for the instruction itself.
    (1+ (cond
         ((eq? inst 'load-program)
          (+ (bytevector-u32-native-ref bytecode (+ idx 1))
             (bytevector-u32-native-ref bytecode (+ idx 5))))
         ((< (instruction-length inst) 0)
          ;; variable length instruction -- the length is encoded in the
          ;; instruction stream.
          (+ (ash (bytevector-u8-ref bytecode (+ idx 1)) 16)
             (ash (bytevector-u8-ref bytecode (+ idx 2)) 8)
             (bytevector-u8-ref bytecode (+ idx 3))))
         (else
          ;; fixed length
          (instruction-length inst))))))

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
  (let ((bv (objcode->bytecode (program-objcode proc))))
    (let lp ((in (program-sources proc))
             (out '())
             (ip 0))
      (cond
       ((null? in)
        (reverse out))
       (else
        (pmatch (car in)
          ((,post-ip . ,source)
           (let lp2 ((ip ip)
                     (next ip))
             (if (< next post-ip)
                 (lp2 next (+ next (bytecode-instruction-length bv next)))
                 (lp (cdr in)
                     (acons ip source out)
                     next))))
          (else
           (error "unexpected"))))))))

(define (collapse-locals locs)
  (let lp ((ret '()) (locs locs))
    (if (null? locs)
        (map cdr (sort! ret 
                        (lambda (x y) (< (car x) (car y)))))
        (let ((b (car locs)))
          (cond
           ((assv-ref ret (binding:index b))
            => (lambda (bindings)
                 (append! bindings (list b))
                 (lp ret (cdr locs))))
           (else
            (lp (acons (binding:index b) (list b) ret)
                (cdr locs))))))))

;; returns list of list of bindings
;; (list-ref ret N) == bindings bound to the Nth local slot
(define (program-bindings-by-index prog)
  (cond ((program-bindings prog) => collapse-locals)
        (else '())))

(define (program-bindings-for-ip prog ip)
  (let lp ((in (program-bindings-by-index prog)) (out '()))
    (if (null? in)
        (reverse out)
        (lp (cdr in)
            (let inner ((binds (car in)))
              (cond ((null? binds) out)
                    ((<= (binding:start (car binds))
                         ip
                         (binding:end (car binds)))
                     (cons (car binds) out))
                    (else (inner (cdr binds)))))))))

(define (arity:start a)
  (pmatch a ((,start ,end . _) start) (else (error "bad arity" a))))
(define (arity:end a)
  (pmatch a ((,start ,end . _) end) (else (error "bad arity" a))))
(define (arity:nreq a)
  (pmatch a ((_ _ ,nreq . _) nreq) (else 0)))
(define (arity:nopt a)
  (pmatch a ((_ _ ,nreq ,nopt . _) nopt) (else 0)))
(define (arity:rest? a)
  (pmatch a ((_ _ ,nreq ,nopt ,rest? . _) rest?) (else #f)))
(define (arity:kw a)
  (pmatch a ((_ _ ,nreq ,nopt ,rest? (_ . ,kw)) kw) (else '())))
(define (arity:allow-other-keys? a)
  (pmatch a ((_ _ ,nreq ,nopt ,rest? (,aok . ,kw)) aok) (else #f)))

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
  (pmatch arglist
    ((,req ,opt ,keyword ,allow-other-keys? ,rest . ,extents)
     `((required . ,req)
       (optional . ,opt)
       (keyword . ,keyword)
       (allow-other-keys? . ,allow-other-keys?)
       (rest . ,rest)
       (extents . ,extents)))
    (else #f)))

(define* (arity->arguments-alist prog arity
                                 #:optional
                                 (make-placeholder
                                  (lambda (i) (string->symbol "_"))))
  (define var-by-index
    (let ((rbinds (map (lambda (x)
                         (cons (binding:index x) (binding:name x)))
                       (program-bindings-for-ip prog
                                                (arity:start arity)))))
      (lambda (i)
        (or (assv-ref rbinds i)
            ;; if we don't know the name, return a placeholder
            (make-placeholder i)))))

  (let lp ((nreq (arity:nreq arity)) (req '())
           (nopt (arity:nopt arity)) (opt '())
           (rest? (arity:rest? arity)) (rest #f)
           (n 0))
    (cond
     ((< 0 nreq)
      (lp (1- nreq) (cons (var-by-index n) req)
          nopt opt rest? rest (1+ n)))
     ((< 0 nopt)
      (lp nreq req
          (1- nopt) (cons (var-by-index n) opt)
          rest? rest (1+ n)))
     (rest?
      (lp nreq req nopt opt
          #f (var-by-index (+ n (length (arity:kw arity))))
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
  (let ((arity (program-arity prog ip)))
    (and arity
         (arity->arguments-alist prog arity))))

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

(define (write-program prog port)
  (format port "#<procedure ~a~a>"
          (or (procedure-name prog)
              (and=> (program-source prog 0)
                     (lambda (s)
                       (format #f "~a at ~a:~a:~a"
                               (number->string (object-address prog) 16)
                               (or (source:file s)
                                   (if s "<current input>" "<unknown port>"))
                               (source:line-for-user s) (source:column s))))
              (number->string (object-address prog) 16))
          (let ((arities (program-arities prog)))
            (if (or (not arities) (null? arities))
                ""
                (string-append
                 " " (string-join (map (lambda (a)
                                         (object->string
                                          (arguments-alist->lambda-list
                                           (arity->arguments-alist prog a))))
                                       arities)
                                  " | "))))))

