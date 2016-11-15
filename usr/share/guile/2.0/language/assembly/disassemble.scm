;;; Guile VM code converters

;; Copyright (C) 2001, 2009, 2010, 2012, 2013 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language assembly disassemble)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (system vm instruction)
  #:use-module (system vm program)
  #:use-module (system base pmatch)
  #:use-module (language assembly)
  #:use-module (system base compile)
  #:export (disassemble))

(define (disassemble x)
  (format #t "Disassembly of ~A:\n\n" x)
  (call-with-values
      (lambda () (decompile x #:from 'value #:to 'assembly))
    disassemble-load-program))

(define (disassemble-load-program asm env)
  (pmatch asm
    ((load-program ,labels ,len ,meta . ,code)
     (let ((objs  (and env (assq-ref env 'objects)))
           (free-vars (and env (assq-ref env 'free-vars)))
           (meta  (and env (assq-ref env 'meta)))
           (blocs (and env (assq-ref env 'blocs)))
           (srcs  (and env (assq-ref env 'sources))))
       (let lp ((pos 0) (code code) (programs '()))
         (cond
          ((null? code)
           (newline)
           (for-each
            (lambda (sym+asm)
              (format #t "Embedded program ~A:\n\n" (car sym+asm))
              (disassemble-load-program (cdr sym+asm) '()))
            (reverse! programs)))
          (else
           (let* ((asm (car code))
                  (len (byte-length asm))
                  (end (+ pos len)))
             (pmatch asm
               ((load-program . _)
                (let ((sym (gensym "")))
                  (print-info pos `(load-program ,sym) #f #f)
                  (lp (+ pos (byte-length asm)) (cdr code)
                      (acons sym asm programs))))
               ((nop)
                (lp (+ pos (byte-length asm)) (cdr code) programs))
               (else
                (print-info pos asm
                            ;; FIXME: code-annotation for whether it's
                            ;; an arg or not, currently passing nargs=-1
                            (code-annotation end asm objs -1 blocs
                                             labels)
                            (and=> (and srcs (assq end srcs)) source->string))
                (lp (+ pos (byte-length asm)) (cdr code) programs)))))))
                 
       (if (pair? free-vars)
           (disassemble-free-vars free-vars))
       (if meta
           (disassemble-meta meta))

       ;; Disassemble other bytecode in it
       ;; FIXME: something about the module.
       (if objs
           (for-each
            (lambda (x)
              (if (program? x)
                  (begin (display "----------------------------------------\n")
                         (disassemble x))))
            (cdr (vector->list objs))))))
    (else
     (error "bad load-program form" asm))))

(define (disassemble-free-vars free-vars)
  (display "Free variables:\n\n")
  (fold (lambda (free-var i)
          (print-info i free-var #f #f)
          (+ 1 i))
        0
        free-vars))

(define-macro (unless test . body)
  `(if (not ,test) (begin ,@body)))

(define *uninteresting-props* '(name))

(define (disassemble-meta meta)
  (let ((props (filter (lambda (x)
                         (not (memq (car x) *uninteresting-props*)))
                       (cdddr meta))))
    (unless (null? props)
      (display "Properties:\n\n")
      (for-each (lambda (x) (print-info #f x #f #f)) props)
      (newline))))

(define (source->string src)
  (format #f "~a:~a:~a" (or (source:file src) "(unknown file)")
          (source:line-for-user src) (source:column src)))

(define (make-int16 byte1 byte2)
  (+ (* byte1 256) byte2))

(define (code-annotation end-addr code objs nargs blocs labels)
  (let* ((code (assembly-unpack code))
         (inst (car code))
         (args (cdr code)))
    (case inst
      ((list vector) 
       (list "~a element~:p" (apply make-int16 args)))
      ((br br-if br-if-eq br-if-not br-if-not-eq br-if-not-null br-if-null)
       (list "-> ~A" (assq-ref labels (car args))))
      ((br-if-nargs-ne br-if-nargs-lt br-if-nargs-gt)
       (list "-> ~A" (assq-ref labels (caddr args))))
      ((bind-optionals/shuffle-or-br)
       (list "-> ~A" (assq-ref labels (car (last-pair args)))))
      ((object-ref)
       (and objs (list "~s" (vector-ref objs (car args)))))
      ((local-ref local-boxed-ref local-set local-boxed-set)
       (and blocs
            (let lp ((bindings (list-ref blocs (car args))))
              (and (pair? bindings)
                   (let ((b (car bindings)))
                     (if (and (< (binding:start (car bindings)) end-addr)
                              (>= (binding:end (car bindings)) end-addr))
                         (list "`~a'~@[ (arg)~]"
                               (binding:name b) (< (binding:index b) nargs))
                         (lp (cdr bindings))))))))
      ((assert-nargs-ee/locals assert-nargs-ge/locals)
       (list "~a arg~:p, ~a local~:p"
             (logand (car args) #x7) (ash (car args) -3)))
      ((free-ref free-boxed-ref free-boxed-set)
       ;; FIXME: we can do better than this
       (list "(closure variable)"))
      ((toplevel-ref toplevel-set)
       (and objs
            (let ((v (vector-ref objs (car args))))
              (if (and (variable? v) (variable-bound? v))
                  (list "~s" (variable-ref v))
                  (list "`~s'" v)))))
      ((mv-call)
       (list "MV -> ~A" (assq-ref labels (cadr args))))
      ((prompt)
       ;; the H is for handler
       (list "H -> ~A" (assq-ref labels (cadr args))))
      (else
       (and=> (assembly->object code)
              (lambda (obj) (list "~s" obj)))))))

;; i am format's daddy.
(define (print-info addr info extra src)
  (format #t "~4@S    ~32S~@[;; ~1{~@?~}~]~@[~61t at ~a~]\n" addr info extra src))
