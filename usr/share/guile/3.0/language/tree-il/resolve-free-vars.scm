;;; Resolving free top-level references to modules
;;; Copyright (C) 2021-2022
;;;   Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.



(define-module (language tree-il resolve-free-vars)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:use-module ((srfi srfi-1) #:select (filter-map))
  #:export (resolve-free-vars))

(define (compute-assigned-lexicals exp)
  (define assigned-lexicals '())
  (define (add-assigned-lexical! var)
    (set! assigned-lexicals (cons var assigned-lexicals)))
  ((make-tree-il-folder)
   exp
   (lambda (exp)
     (match exp
       (($ <lexical-set> _ _ var _)
        (add-assigned-lexical! var)
        (values))
       (_ (values))))
   (lambda (exp)
     (values)))
  assigned-lexicals)

(define (make-resolver mod local-definitions)
  ;; Given that module A imports B and C, and X is free in A,
  ;; unfortunately there are a few things preventing us from knowing
  ;; whether the binding proceeds from B or C, just based on the text:
  ;;
  ;;  - Renamers are evaluated at run-time.
  ;;  - Just using B doesn't let us know what definitions are in B.
  ;;
  ;; So instead of using the source program to determine where a binding
  ;; comes from, we use the first-class module interface.
  (define (imported-resolver iface)
    (let ((by-var (make-hash-table)))
      ;; When resolving a free variable, Guile visits all used modules
      ;; to see if there is a binding.  If one of those imports is an
      ;; autoload, it's possible that the autoload interface fails to
      ;; load.  In that case Guile will issue a warning and consider the
      ;; binding not found in that module.  Here we try to produce the
      ;; same behavior at optimization time that we do at expand time
      ;; that we would do at run time.
      (false-if-exception
       (let ((public-iface (resolve-interface (module-name iface))))
         (module-for-each (lambda (name var)
                            (hashq-set! by-var var name))
                          public-iface))
       #:warning "Failed to determine exported bindings from module ~a:\n"
       (module-name iface))
      (lambda (name)
        (let ((var (module-variable iface name)))
          (and var
               (cons (module-name iface)
                     (hashq-ref by-var var)))))))

  (define the-module (resolve-module mod))
  (define resolvers
    (map imported-resolver (module-uses the-module)))

  (lambda (name)
    (cond
     ((or (module-local-variable the-module name)
          (memq name local-definitions))
      'local)
     (else
      (match (filter-map (lambda (resolve) (resolve name)) resolvers)
        (() 'unknown)
        (((mod . #f)) 'unknown)
        (((mod . public-name)) (cons mod public-name))
        ((_ _ . _) 'duplicate))))))

;;; Record all bindings in a module, to know whether a toplevel-ref is
;;; an import or not.  If toplevel-ref to imported variable, transform
;;; to module-ref or primitive-ref.  New pass before peval.

(define (compute-free-var-resolver exp)
  (define assigned-lexicals (compute-assigned-lexicals exp))
  (define module-definitions '())
  (define module-lexicals '())
  (define bindings '())
  (define (add-module-definition! mod args)
    (set! module-definitions (acons mod args module-definitions)))
  (define (add-module-lexical! var mod)
    (unless (memq var assigned-lexicals)
      (set! module-lexicals (acons var mod module-lexicals))))
  (define (add-binding! mod name)
    (set! bindings (acons mod name bindings)))

  (define (record-bindings! mod vars vals)
    (for-each
     (lambda (var val)
       (match val
         (($ <call> _ ($ <module-ref> _ '(guile) 'define-module* #f)
             (($ <const> _ mod) . args))
          (add-module-definition! mod args)
          (add-module-lexical! var mod))
         (($ <primcall> _ 'current-module ())
          (when mod
            (add-module-lexical! var mod)))
         (_ #f)))
     vars vals))

  ;; Thread a conservative idea of what the current module is through
  ;; the visit.  Visiting an expression returns the name of the current
  ;; module when the expression completes, or #f if unknown.  Record the
  ;; define-module* forms, if any, and note any toplevel definitions.
  (define (visit exp) (visit/mod exp #f))
  (define (visit* exps)
    (unless (null? exps)
      (visit (car exps))
      (visit* (cdr exps))))
  (define (visit+ exps mod)
    (match exps
      (() mod)
      ((exp . exps)
       (let lp ((mod' (visit/mod exp mod)) (exps exps))
         (match exps
           (() mod')
           ((exp . exps)
            (lp (and (equal? mod' (visit/mod exp mod)) mod')
                exps)))))))
  (define (visit/mod exp mod)
    (match exp
      ((or ($ <void>) ($ <const>) ($ <primitive-ref>) ($ <lexical-ref>)
           ($ <module-ref>) ($ <toplevel-ref>))
       mod)

      (($ <call> _ ($ <module-ref> _ '(guile) 'set-current-module #f)
          (($ <lexical-ref> _ _ var)))
       (assq-ref module-lexicals var))

      (($ <call> _ proc args)
       (visit proc)
       (visit* args)
       #f)

      (($ <primcall> _ _ args)
       ;; There is no primcall that sets the current module.
       (visit+ args mod))

      (($ <conditional> src test consequent alternate)
       (visit+ (list consequent alternate) (visit/mod test mod)))

      (($ <lexical-set> src name gensym exp)
       (visit/mod exp mod))

      (($ <toplevel-set> src mod name exp)
       (visit/mod exp mod))

      (($ <module-set> src mod name public? exp)
       (visit/mod exp mod))

      (($ <toplevel-define> src mod name exp)
       (add-binding! mod name)
       (visit/mod exp mod))

      (($ <lambda> src meta body)
       (when body (visit body))
       mod)

      (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
       (visit* inits)
       (let* ((bodies (cons body inits))
              (bodies (if alternate (cons alternate bodies) bodies)))
         (visit+ bodies mod)))

      (($ <seq> src head tail)
       (visit/mod tail (visit/mod head mod)))

      (($ <let> src names gensyms vals body)
       (record-bindings! mod gensyms vals)
       (visit/mod body (visit+ vals mod)))

      (($ <letrec> src in-order? names gensyms vals body)
       (record-bindings! mod gensyms vals)
       (visit/mod body (visit+ vals mod)))

      (($ <fix> src names gensyms vals body)
       (record-bindings! mod gensyms vals)
       (visit/mod body (visit+ vals mod)))

      (($ <let-values> src exp body)
       (visit/mod body (visit/mod exp mod)))

      (($ <prompt> src escape-only? tag body handler)
       (visit+ (list body handler) (visit/mod tag mod)))

      (($ <abort> src tag args tail)
       (visit tag)
       (visit* args)
       (visit tail)
       #f)))

  (visit exp)

  (define (kwarg-ref args kw kt kf)
    (let lp ((args args))
      (match args
        (() (kf))
        ((($ <const> _ (? keyword? kw')) val . args)
         (if (eq? kw' kw)
             (kt val)
             (lp args)))
        ((_ _ . args)
         (lp args)))))
  (define (kwarg-ref/const args kw kt kf)
    (kwarg-ref args kw
               (lambda (exp)
                 (match exp
                   (($ <const> _ val') (kt val'))
                   (_ (kf))))
               kf))
  (define (has-constant-initarg? args kw val)
    (kwarg-ref/const args kw
                     (lambda (val')
                       (equal? val val'))
                     (lambda () #f)))

  ;; Collect declarative modules defined once in this compilation unit.
  (define declarative-modules
    (let lp ((defs module-definitions) (not-declarative '()) (declarative '()))
      (match defs
        (() declarative)
        (((mod . args) . defs)
         (cond ((member mod not-declarative)
                (lp defs not-declarative declarative))
               ((or (assoc mod defs) ;; doubly defined?
                    (not (has-constant-initarg? args #:declarative? #t)))
                (lp defs (cons mod not-declarative) declarative))
               (else
                (lp defs not-declarative (cons mod declarative))))))))

  (define resolvers
    (map (lambda (mod)
           (define resolve
             (make-resolver mod
                            (filter-map (match-lambda
                                          ((mod' . name)
                                           (and (equal? mod mod') name)))
                                        bindings)))
           (cons mod resolve))
         declarative-modules))

  (lambda (mod name)
    (cond
     ((assoc-ref resolvers mod)
      => (lambda (resolve) (resolve name)))
     (else 'unknown))))

(define (resolve-free-vars exp)
  "Traverse @var{exp}, extracting module-level definitions."
  (define resolve
    (compute-free-var-resolver exp))

  (post-order
   (lambda (exp)
     (match exp
       (($ <toplevel-ref> src mod name)
        (match (resolve mod name)
          ((or 'unknown 'duplicate 'local) exp)
          ((mod . name)
           (make-module-ref src mod name #t))))
       (($ <toplevel-set> src mod name val)
        (match (resolve mod name)
          ((or 'unknown 'duplicate 'local) exp)
          ((mod . name)
           (make-module-set src mod name #t val))))
       (exp exp)))
   exp))
