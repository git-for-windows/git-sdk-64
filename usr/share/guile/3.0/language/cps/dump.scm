;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2021,2023 Free Software Foundation, Inc.

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
;;;
;;; Helper facilities for working with CPS.
;;;
;;; Code:

(define-module (language cps dump)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (language cps)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:use-module (language cps graphs)
  #:use-module (language cps utils)
  #:export (dump))

;; ideas: unused vars print as _
;;        print all labels
;;        call bb headers with values
;;        annotate blocks with available bindings?  live bindings?
;;        how to print calls...
;;        dot graph

(define (cont-successors cont)
  (match cont
    (($ $kargs _ _ term)
     (match term
       (($ $continue k) (list k))
       (($ $branch kf kt) (list kf kt))
       (($ $switch kf kt*) (cons kf kt*))
       (($ $prompt k kh) (list k kh))
       (($ $throw) '())))
    (($ $kclause _ kbody kalternate)
     (if kalternate
         (list kbody kalternate)
         (list kbody)))
    (($ $kfun src meta self ktail kentry)
     (list ktail kentry))
    (($ $kreceive arity kargs) (list kargs))
    (($ $ktail) '())))

(define (compute-block-entries cps kfun body all-labels?)
  (if all-labels?
      body
      (let ((preds (compute-predecessors cps kfun #:labels body)))
        ;; Conts whose predecessor count is not 1 start blocks.
        (define (add-entry label blocks)
          (match (intmap-ref preds label)
            ((_) blocks)
            (_ (intset-add! blocks label))))
        ;; Continuations of branches start blocks.
        (define (add-exits label blocks)
          (fold1 (lambda (succ blocks)
                   (intset-add! blocks succ))
                 (match (cont-successors (intmap-ref cps label))
                   ((_) '())
                   (succs succs))
                 blocks))
        (persistent-intset
         (intset-fold
          (lambda (label blocks)
            (add-exits label (add-entry label blocks)))
          body
          empty-intset)))))

(define (collect-blocks cps entries)
  (define (collect-block entry)
    (let ((cont (intmap-ref cps entry)))
      (acons entry cont
             (match (cont-successors (intmap-ref cps entry))
               ((succ)
                (if (intset-ref entries succ)
                    '()
                    (collect-block succ)))
               (_ '())))))
  (persistent-intmap
   (intset-fold
    (lambda (start blocks)
      (intmap-add! blocks start (collect-block start)))
    entries
    empty-intmap)))

(define (compute-block-succs blocks)
  (intmap-map (lambda (entry conts)
                (match conts
                  (((_ . _) ... (exit . cont))
                   (fold1 (lambda (succ succs)
                            (intset-add succs succ))
                          (cont-successors cont)
                          empty-intset))))
              blocks))

(define (dump-block cps port labelled-conts)
  (define (format-label label) (format #f "L~a" label))
  (define (format-name name) (if name (symbol->string name) "_"))
  (define (format-var var) (format #f "v~a" var))
  (define (format-loc src)
    (match src
      (#f #f)
      (#(filename line column)
       (format #f "~a:~a:~a"
               (or filename "<unknown>")
               (1+ line)
               column))))
  (define (arg-list strs) (string-join strs ", "))
  (define (false-if-empty str) (if (string-null? str) #f str))
  (define (format-arity arity)
    (match arity
      (($ $arity req opt rest kw aok?)
       (arg-list
        `(,@(map format-name req)
          ,@(map (lambda (name)
                   (format #f "[~a]" (format-name name)))
                 opt)
          ,@(map (match-lambda
                   ((kw name var)
                    (format #f "~a" kw)))
                 kw)
          ,@(if aok? '("[#:allow-other-keys]") '())
          ,@(if rest
                (list (string-append (format-name rest) "..."))
                '()))))))
  (define (format-primcall op param args)
    (format #f "~a~@[[~s]~](~a)" op param (arg-list (map format-var args))))
  (define (format-exp exp)
    (match exp
      (($ $const val)
       (format #f "const ~s" val))
      (($ $prim name)
       (format #f "prim ~s" name))
      (($ $fun body)
       (format #f "fun ~a" (format-label body)))
      (($ $rec names syms funs)
       (format #f "rec(~a)" (arg-list (map format-exp funs))))
      (($ $const-fun label)
       (format #f "const-fun ~a" (format-label label)))
      (($ $code label)
       (format #f "code ~a" (format-label label)))
      (($ $call proc args)
       (format #f "call ~a(~a)"
               (format-var proc) (arg-list (map format-var args))))
      (($ $callk k proc args)
       (format #f "callk ~a(~a)" (format-label k)
               (arg-list
                (cons (if proc (format-var proc) "_")
                      (map format-var args)))))
      (($ $calli args callee)
       (format #f "calli ~a(~a)"
               (format-var callee) (arg-list (map format-var args))))
      (($ $primcall name param args)
       (format-primcall name param args))
      (($ $values args)
       (arg-list (map format-var args)))))
  (define (dump-annotation ann src)
    (when (or ann src)
      (format port "~45t ; ~@[~a ~]" ann)
      (when src
        (let* ((src (format-loc src))
               (col (- 80 4 (string-length src))))
          (format port "~vt at ~a" col src))))
    (newline port))
  (define (dump-definition src names vars fmt . args)
    (define (take formatter val)
      (cond
       ((not val) #f)
       ((string? val) (false-if-empty val))
       ((null? val) #f)
       (else (arg-list (map formatter val)))))
    (let ((names (take format-name names))
          (vars (take format-var vars)))
      (format port "  ~@[~a := ~]~?" vars fmt args)
      (dump-annotation names src)))
  (define (dump-statement src ann fmt . args)
    (format port "  ~?" fmt args)
    (dump-annotation (and ann (false-if-empty ann)) src))
  (define (dump-block-header label cont)
    (match cont
      (($ $kargs names vars)
       (format port "~a(~a):"
               (format-label label)
               (arg-list (map format-var vars)))
       (dump-annotation (false-if-empty (arg-list (map format-name names)))
                        #f))
      (($ $ktail)
       (values))
      (($ $kfun src meta self ktail kentry)
       (let ((name (assq-ref meta 'name)))
         (format port "~a:" (format-label label))
         (dump-annotation name src)))
      ((or ($ $kreceive) ($ $kclause))
       (format port "~a:\n" (format-label label)))))
  (define (dump-block-body label cont)
    (match cont
      (($ $kargs _ _ ($ $continue k src exp))
       (match (intmap-ref cps k)
         (($ $kargs names vars)
          (dump-definition src names vars "~a" (format-exp exp)))
         (_
          (dump-definition src #f #f "~a" (format-exp exp)))))
      (($ $kreceive arity kargs)
       (match (intmap-ref cps kargs)
         (($ $kargs names vars)
          (dump-definition #f names vars
                           "receive(~a)" (format-arity arity)))))
      (($ $ktail)
       (values))
      (($ $kclause arity kbody #f)
       (match (intmap-ref cps kbody)
         (($ $kargs names vars)
          (dump-definition #f names vars
                           "receive(~a)" (format-arity arity)))))))
  (define (dump-block-exit label cont)
    (match cont
      (($ $kargs _ _ term)
       (match term
         (($ $continue k src exp)
          (match (intmap-ref cps k)
            (($ $ktail)
             (match exp
               (($ $values vals)
                (dump-statement src #f
                                "return ~a" (arg-list (map format-var vals))))
               (_
                (dump-statement src #f
                                "tail ~a" (format-exp exp)))))
            (_
             (dump-statement src #f
                             "~a(~a)" (format-label k) (format-exp exp)))))
         (($ $branch kf kt src op param args)
          (dump-statement src #f
                          "~a ? ~a() : ~a()"
                          (format-primcall op param args)
                          (format-label kt)
                          (format-label kf)))
         (($ $switch kf kt* src arg)
          (dump-statement src #f
                          "[~a]~a() or ~a()"
                          (arg-list (map format-label kt*))
                          (format-var arg)
                          (format-label kf)))
         (($ $prompt k kh src escape? tag)
          (dump-statement src #f
                          "~a(prompt(kh:~a,~a tag:~a)"
                          (format-label k)
                          (format-label kh)
                          (if escape? ", escape-only" "")
                          (format-var tag)))
         (($ $throw src op param args)
          (dump-statement src #f
                          "throw ~a" (format-primcall op param args)))))
      (($ $kreceive arity kargs)
       (dump-statement #f #f
                       "~a(receive(~a))"
                       (format-label kargs)
                       (format-arity arity)))
      (($ $kfun src meta self ktail kentry)
       (for-each (match-lambda
                   ((k . v)
                    (unless (eq? k 'name)
                      (format port "  meta: ~a: ~s\n" k v))))
                 meta)
       ;; (format port "  tail: ~a:\n" (format-label ktail))
       (when self
         (format port "  ~a := self\n" (format-var self)))
       (format port "  ~a(...)\n" (format-label kentry)))
      (($ $kclause arity kbody kalt)
       (dump-statement #f #f
                       "~a(receive(~a))~@[or ~a()~]\n"
                       (format-label kbody)
                       (format-arity arity)
                       (and=> kalt format-label)))
      (($ $ktail)
       (values))))
  (match labelled-conts
    (((label . cont) . _)
     (dump-block-header label cont)))
  (let lp ((labelled-conts labelled-conts))
    (match labelled-conts
      (((label . cont))
       (dump-block-exit label cont))
      (((label . cont) . labelled-conts)
       (dump-block-body label cont)
       (lp labelled-conts)))))

(define (dump-function cps port kfun body all-labels?)
  (define entries (compute-block-entries cps kfun body all-labels?))
  (define blocks (collect-blocks cps entries))
  (define block-succs (compute-block-succs blocks))
  (define block-order (compute-reverse-post-order block-succs kfun))
  (for-each (lambda (entry)
              (dump-block cps port (intmap-ref blocks entry)))
            block-order)
  (values))

(define* (dump cps #:key
               (port (current-output-port))
               (entry (intmap-next cps))
               (all-labels? #f))
  (let ((functions (compute-reachable-functions cps entry)))
    (intmap-fold (lambda (kfun body)
                   (unless (eqv? kfun entry) (newline port))
                   (dump-function cps port kfun body all-labels?))
                 functions)))
