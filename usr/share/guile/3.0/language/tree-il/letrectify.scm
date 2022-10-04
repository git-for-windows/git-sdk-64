;;; transformation of top-level bindings into letrec*

;; Copyright (C) 2019-2021 Free Software Foundation, Inc.

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

(define-module (language tree-il letrectify)
  #:use-module ((srfi srfi-1) #:select (fold-right))
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:use-module (language tree-il effects)
  #:export (letrectify))

;; Take a sequence of top-level definitions and turn the defintions into
;; letrec*.  From this:
;;
;;    (begin
;;      (define a 10)
;;      (define b (lambda () a))
;;      (foo a)
;;      (define c (lambda () (set! c b) (c))))
;;
;; To this:
;;
;;    (letrec* ((a-var (module-make-local-var! (current-module) 'a))
;;              (a 10)
;;              (_ (begin (variable-set! a-var a)))
;;              (b-var (module-make-local-var! (current-module) 'b))
;;              (b (lambda () a))
;;              (_ (begin (variable-set! b-var b)))
;;              (_ (begin (foo a) #t))
;;              (c-var (module-make-local-var! (current-module) 'c)))
;;              (c (lambda () (variable-set! c-var b) ((variable-ref c-var))))
;;              (_ (begin (variable-set! c-var c))))
;;      (void))
;;
;; Inside the compilation unit, references to "declarative" top-level
;; definitions are accessed directly as lexicals.  A declarative
;; definition is a variable for which the expander knows the module,
;; which is defined in the compilation unit exactly one time, and which
;; is not assigned in the compilation unit.
;;
;; The assumption is that it's safe for the compiler to reason about the
;; *values* of declarative bindings, because they are immutable in
;; practice.  Of course someone can come later from another compilation
;; unit or another module and use the private module API to mutate
;; definitions from this compilation unit; in that case, updates from
;; that third party may not be visible to users of declarative
;; definitions.  That kind of use is not common, though.  The letrectify
;; transformation is so important for performance that most users are
;; willing to accept the restrictions of this transformation.
;;
;; Incidentally, the later fix-letrec and peval passes should optimize
;; the above example to:
;;
;;    (begin
;;      (variable-set! (module-make-local-var! (current-module) 'a) 10)
;;      (variable-set! (module-make-local-var! (current-module) 'b)
;;                     (lambda () 10))
;;      (foo 10)
;;      (let ((c-var (module-make-local-var! (current-module) 'c)))
;;        (variable-set! c-var
;;                       (lambda ()
;;                         (variable-set! c-var (lambda () 10))
;;                         ((variable-ref c-var))))
;;        (void)))
;;
;; As you can see, letrectification allowed for inlining of the uses of
;; both A and B.
;;

(define for-each-fold (make-tree-il-folder))
(define (tree-il-for-each f x)
  (for-each-fold x (lambda (x) (f x) (values)) (lambda (x) (values))))

(define (compute-declarative-toplevels x)
  (define dynamic (make-hash-table))
  (define defined (make-hash-table))
  (define assigned (make-hash-table))
  (tree-il-for-each
   (lambda (x)
     (match x
       (($ <toplevel-set> src mod name)
        (if mod
            (hash-set! assigned (cons mod name) #t)
            (hashq-set! dynamic name #t)))
       (($ <toplevel-define> src mod name expr)
        (if mod
            (hash-set! (if (hash-ref defined (cons mod name))
                           assigned
                           defined)
                       (cons mod name) expr)
            (hashq-set! dynamic name #t)))
       (_ (values))))
   x)
  (let ((declarative (make-hash-table)))
    (define (declarative-module? mod)
      (let ((m (resolve-module mod #f #:ensure #f)))
        (and m (module-declarative? m))))
    (hash-for-each (lambda (k expr)
                     (match k
                       ((mod . name)
                        (unless (or (hash-ref assigned k)
                                    (hashq-ref dynamic name)
                                    (not (declarative-module? mod)))
                          (hash-set! declarative k expr)))))
                   defined)
    declarative))

(define (compute-private-toplevels declarative)
  ;; Set of variables exported by the modules of declarative bindings in
  ;; this compilation unit.
  (define exports (make-hash-table))
  ;; If a module exports a macro, that macro could implicitly export any
  ;; top-level binding in a module; we have to avoid sealing private
  ;; bindings in that case.
  (define exports-macro? (make-hash-table))
  (hash-for-each
   (lambda (k _)
     (match k
       ((mod . name)
        (unless (hash-get-handle exports-macro? mod)
          (hash-set! exports-macro? mod #f)
          (let ((i (module-public-interface (resolve-module mod))))
            (when i
              (module-for-each
               (lambda (k v)
                 (hashq-set! exports v k)
                 (when (and (variable-bound? v) (macro? (variable-ref v)))
                   (hash-set! exports-macro? mod #t)))
               i)))))))
   declarative)
  (let ((private (make-hash-table)))
    (hash-for-each
     (lambda (k _)
       (match k
         ((mod . name)
          (unless (or (hash-ref exports-macro? mod)
                      (hashq-ref exports
                                 (module-local-variable (resolve-module mod) name)))
            (hash-set! private k #t)))))
     declarative)
    private))

(define* (letrectify expr #:key (seal-private-bindings? #f))
  (define declarative (compute-declarative-toplevels expr))
  (define private
    (if seal-private-bindings?
        (compute-private-toplevels declarative)
        (make-hash-table)))
  (define declarative-box+value
    (let ((tab (make-hash-table)))
      (hash-for-each (lambda (key val)
                       (let ((box (and (not (hash-ref private key))
                                       (gensym)))
                             (val (gensym)))
                         (hash-set! tab key (cons box val))))
                     declarative)
      (lambda (mod name)
        (hash-ref tab (cons mod name)))))

  (define compute-effects
    ;; Assume all lexicals are assigned, for the purposes of this
    ;; transformation.  (It doesn't matter.)
    (let ((assigned? (lambda (sym) #t)))
      (make-effects-analyzer assigned?)))

  (define (can-elide-statement? stmt)
    (let ((effects (compute-effects stmt)))
      (effect-free?
       (exclude-effects effects (logior &allocation &zero-values)))))

  (define (add-binding name var val tail)
    (match tail
      (($ <letrec> src #t names vars vals tail)
       (make-letrec src #t
                    (cons name names) (cons var vars) (cons val vals)
                    tail))
      (_
       (make-letrec (tree-il-src tail) #t
                    (list name) (list var) (list val)
                    tail))))

  (define (add-statement src stmt tail)
    (if (can-elide-statement? stmt)
        tail
        (add-binding '_ (gensym "_") (make-seq src stmt (make-void src))
                     tail)))

  (define (visit-expr expr)
    (post-order
     (lambda (expr)
       (match expr
         (($ <toplevel-ref> src mod name)
          (match (declarative-box+value mod name)
            (#f expr)
            ((box . value)
             (make-lexical-ref src name value))))
         (_ expr)))
     expr))

  (define (visit-top-level expr mod-vars)
    (match expr
      (($ <toplevel-define> src mod name exp)
       (match (declarative-box+value mod name)
         (#f (values (visit-expr expr) mod-vars))
         ((#f . value)
          (values (add-binding name value (visit-expr exp) (make-void src))
                  mod-vars))
         ((box . value)
          (match (assoc-ref mod-vars mod)
            (#f
             (let* ((mod-var (gensym "mod"))
                    (mod-vars (acons mod mod-var mod-vars)))
               (call-with-values (lambda () (visit-top-level expr mod-vars))
                 (lambda (tail mod-vars)
                   (values
                    (add-binding 'mod
                                 mod-var
                                 (make-primcall src 'current-module '())
                                 tail)
                    mod-vars)))))
            (mod-var
             (let* ((loc
                     (make-primcall src 'module-ensure-local-variable!
                                    (list (make-lexical-ref src 'mod mod-var)
                                          (make-const src name))))
                    (exp (visit-expr exp))
                    (ref (make-lexical-ref src name value))
                    (init
                     (make-primcall src '%variable-set!
                                    (list (make-lexical-ref src name box)
                                          ref))))
               (values
                (add-binding
                 name box loc
                 (add-binding
                  name value exp
                  (add-statement src init (make-void src))))
                mod-vars)))))))

      (($ <let> src names vars vals body)
       (let lp ((names names) (vars vars) (vals vals) (mod-vars mod-vars))
         (match (vector names vars vals)
           (#(() () ())
            (values (visit-expr body) mod-vars))
           (#((name . names) (var . vars) (val . vals))
            (let* ((val (visit-expr val))
                   (mod-vars
                    (match val
                      (($ <call> _
                          ($ <module-ref> _ '(guile) 'define-module* #f)
                          (($ <const> _ mod) . args))
                       (acons mod var mod-vars))
                      (_ mod-vars))))
              (let-values (((exp mod-vars) (lp names vars vals mod-vars)))
                (values (add-binding name var val exp)
                        mod-vars)))))))

      (($ <seq> src head tail)
       (let*-values (((head mod-vars) (visit-top-level head mod-vars))
                     ((tail mod-vars) (visit-top-level tail mod-vars)))
         
         (values (match head
                   (($ <letrec> src2 #t names vars vals head)
                    (fold-right add-binding (add-statement src head tail)
                                names vars vals))
                   (else
                    (add-statement src head tail)))
                 mod-vars)))

      ;; What would the advantages/disadvantages be if we flattened all
      ;; bindings here, even those from nested let/letrec?
      (_ (values (visit-expr expr) mod-vars))))

  (values (visit-top-level expr '())))
