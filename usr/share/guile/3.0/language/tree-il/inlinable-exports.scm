;;; Attaching inlinable definitions of exported bindings to modules
;;; Copyright (C) 2021, 2022, 2024
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



(define-module (language tree-il inlinable-exports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (language tree-il)
  #:use-module (language tree-il primitives)
  #:use-module (language tree-il fix-letrec)
  #:use-module (language scheme compile-tree-il)
  #:use-module ((srfi srfi-1) #:select (filter-map))
  #:use-module (srfi srfi-9)
  #:use-module (system syntax)
  #:use-module (rnrs bytevectors)
  #:export (inlinable-exports))

;;;
;;; Inlining, as implemented by peval, is the mother of all
;;; optimizations.  It opens up space for other optimizations to work,
;;; such as constant folding, conditional branch folding, and so on.
;;;
;;; Inlining works naturally for lexical bindings.  Inlining of
;;; top-level binding is facilitated by letrectification, which turns
;;; top-level definition sequences to letrec*.  Here we facilitate
;;; inlining across module boundaries, so that module boundaries aren't
;;; necessarily optimization boundaries.
;;;
;;; The high-level idea is to attach a procedure to the module being
;;; compiled, which when called with a name of an export of that module
;;; will return a Tree-IL expression that can be copied into the use
;;; site.  There are two parts: first we determine the set of inlinable
;;; bindings, and then we compile that mapping to a procedure and attach
;;; it to the program being compiled.
;;;
;;; Because we don't want inter-module inlining to inhibit intra-module
;;; inlining, this pass is designed to run late in the Tree-IL
;;; optimization pipeline -- after letrectification, after peval, and so
;;; on.  Unfortunately this does mean that we have to sometimes
;;; pattern-match to determine higher-level constructs from lower-level
;;; residual code, for example to map back from
;;; module-ensure-local-variable! + %variable-set! to toplevel-define,
;;; as reduced by letrectification.  Ah well.
;;;
;;; Ultimately we want to leave the decision to peval as to what to
;;; inline or not to inline, based on its size and effort counters.  But
;;; still we do need to impose some limits -- there's no sense in
;;; copying a large constant from one module to another, for example.
;;; Similarly there's no sense in copying a very large procedure.
;;; Inspired by peval, we bound size growth via a counter that will
;;; abort an inlinable attempt if the term is too large.
;;;
;;; Note that there are some semantic limitations -- you wouldn't want
;;; to copy a mutable value, nor would you want to copy a closure with
;;; free variables.
;;;
;;; Once the set of inlinables is determined, we copy them and rename
;;; their lexicals.  Any reference to an exported binding by lexical
;;; variable is rewritten in terms of a reference to the exported
;;; binding.
;;;
;;; The result is then compiled to a procedure, which internally has a
;;; small interpreter for a bytecode, along with a set of constants.
;;; The assumption is that most of the constants will be written to the
;;; object file anyway, so we aren't taking up more space there.  Any
;;; non-immediate is built on demand, so we limit the impact of
;;; including inlinable definitions on load-time relocations,
;;; allocations, and heap space.
;;;

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

(define (compute-assigned-toplevels exp)
  (define assigned-toplevels '())
  (define (add-assigned-toplevel! mod name)
    (set! assigned-toplevels (acons mod name assigned-toplevels)))
  ((make-tree-il-folder)
   exp
   (lambda (exp)
     (match exp
       (($ <toplevel-set> _ mod name _)
        (add-assigned-toplevel! mod name)
        (values))
       (($ <module-set> src mod name public? exp)
        (unless public?
          (add-assigned-toplevel! mod name))
        (values))
       (_ (values))))
   (lambda (exp)
     (values)))
  assigned-toplevels)

;;; FIXME: Record all bindings in a module, to know whether a
;;; toplevel-ref is an import or not.  If toplevel-ref to imported
;;; variable, transform to module-ref or primitive-ref.  New pass before
;;; peval.

(define (compute-module-bindings exp)
  (define assigned-lexicals (compute-assigned-lexicals exp))
  (define assigned-toplevels (compute-assigned-toplevels exp))
  (define module-definitions '())
  (define lexicals (make-hash-table))
  (define module-lexicals '())
  (define variable-lexicals '())
  (define binding-lexicals '())
  (define binding-values '())
  (define (add-module-definition! mod args)
    (set! module-definitions (acons mod args module-definitions)))
  (define (add-lexical! var val)
    (unless (memq var assigned-lexicals)
      (hashq-set! lexicals var val)))
  (define (add-module-lexical! var mod)
    (unless (memq var assigned-lexicals)
      (set! module-lexicals (acons var mod module-lexicals))))
  (define (add-variable-lexical! var mod name)
    (unless (memq var assigned-lexicals)
      (set! variable-lexicals (acons var (cons mod name) variable-lexicals))))
  (define (add-binding-lexical! var mod name)
    (unless (memq var assigned-lexicals)
      (set! binding-lexicals (acons var (cons mod name) binding-lexicals))))
  (define (add-binding-value! mod name val)
    (set! binding-values (acons (cons mod name) val binding-values)))

  (define (record-bindings! mod gensyms vals)
    (for-each
     (lambda (var val)
       (add-lexical! var val)
       (match val
         (($ <call> _ ($ <module-ref> _ '(guile) 'define-module* #f)
             (($ <const> _ mod) . args))
          (add-module-definition! mod args)
          (add-module-lexical! var mod))
         (($ <primcall> _ 'current-module ())
          (when mod
            (add-module-lexical! var mod)))
         (($ <primcall> _ 'module-ensure-local-variable!
             (($ <lexical-ref> _ _ mod-var) ($ <const> _ name)))
          (let ((mod (assq-ref module-lexicals mod-var)))
            (when mod
              (add-variable-lexical! var mod name))))
         (_ #f)))
     gensyms vals))

  ;; Thread a conservative idea of what the current module is through
  ;; the visit.  Visiting an expression returns the name of the current
  ;; module when the expression completes, or #f if unknown.  Record the
  ;; define-module* forms, if any, and note any assigned or
  ;; multiply-defined variables.  Record definitions by matching
  ;; toplevel-define forms, but also by matching separate
  ;; module-ensure-local-variable! + %variable-set, as residualized by
  ;; letrectification.
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

      (($ <primcall> src '%variable-set! (($ <lexical-ref> _ _ var)
                                          val))
       (match (assq-ref variable-lexicals var)
         ((mod . name)
          (add-binding-value! mod name val)
          ;; Also record lexical for eta-expanded bindings.
          (match val
            (($ <lambda> _ _
                ($ <lambda-case> _ req #f #f #f () (arg ...)
                   ($ <call> _
                      (and eta ($ <lexical-ref> _ _ var))
                      (($ <lexical-ref> _ _ arg) ...))
                   #f))
             (add-binding-lexical! var mod name))
            (($ <lambda> _ _
                ($ <lambda-case> _ req #f (not #f) #f () (arg ...)
                   ($ <primcall> _ 'apply
                      ((and eta ($ <lexical-ref> _ _ var))
                       ($ <lexical-ref> _ _ arg) ...))
                   #f))
             (add-binding-lexical! var mod name))
            (($ <lexical-ref> _ _ var)
             (add-binding-lexical! var mod name))
            (_ #f)))
         (_ #f))
       (visit/mod val mod))

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
       (add-binding-value! mod name exp)
       (visit/mod exp mod))

      (($ <lambda> src meta body)
       (when body (visit body))
       mod)

      (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
       (visit* inits)
       (visit body)
       (when alternate (visit alternate))
       (values))

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
       (visit/mod body (visit/mod exp mod))
       #f)

      (($ <prompt> src escape-only? tag body handler)
       (visit tag)
       (visit body)
       (visit handler)
       #f)

      (($ <abort> src tag args tail)
       (visit tag)
       (visit* args)
       (visit tail)
       #f)))

  (visit exp)
  (values module-definitions lexicals binding-lexicals binding-values))

;; - define inlinable? predicate:
;;     exported && declarative && only references public vars && not too big
;;
;; - public := exported from a module, at -O2 and less.
;;   at -O3 and higher public just means defined in any module.
(define (inlinable-exp mod exports lexicals binding-lexicals exp)
  (define fresh-var!
    (let ((counter 0))
      (lambda ()
        (let ((name (string-append "t" (number->string counter))))
          (set! counter (1+ counter))
          (string->symbol name)))))
  (define (fresh-vars vars)
    (match vars
      (() '())
      ((_ . vars) (cons (fresh-var!) (fresh-vars vars)))))
  (define (add-bound-vars old new bound)
    (match (vector old new)
      (#(() ()) bound)
      (#((old . old*) (new . new*))
       (add-bound-vars old* new* (acons old new bound)))))
  (let/ec return
    (define (abort!) (return #f))
    (define count!
      ;; Same as default operator size limit for peval.
      (let ((counter 40))
        (lambda ()
          (set! counter (1- counter))
          (when (zero? counter) (abort!)))))
    (define (residualize-module-private-ref src mod' name)
      ;; TODO: At -O3, we could residualize a private
      ;; reference.  But that could break peoples'
      ;; expectations.
      (abort!))
    (define (eta-reduce exp)
      ;; Undo the result of eta-expansion pass.
      (match exp
        (($ <lambda> _ _
            ($ <lambda-case> _ req #f #f #f () (sym ...)
               ($ <call> _
                  (and eta ($ <lexical-ref>)) (($ <lexical-ref> _ _ sym) ...))
               #f))
         eta)
        (($ <lambda> _ _
            ($ <lambda-case> _ req #f (not #f) #f () (sym ...)
               ($ <primcall> _ 'apply 
                  ((and eta ($ <lexical-ref>)) ($ <lexical-ref> _ _ sym) ...))
               #f))
         eta)
        (_ exp)))

    (let copy ((exp (eta-reduce exp)) (bound '()) (in-lambda? #f))
      (define (recur exp) (copy exp bound in-lambda?))
      (count!)
      (match exp
        ((or ($ <void>) ($ <primitive-ref>) ($ <module-ref>))
         exp)

        (($ <const> src val)
         (match val
           ;; Don't copy values that could be "too big".
           ((? string?) exp) ; Oddly, (array? "") => #t.
           ((or (? pair?) (? syntax?) (? array?))
            (abort!))
           (_ exp)))

        (($ <lexical-ref> src name var)
         (cond
          ;; Rename existing lexicals.
          ((assq-ref bound var)
           => (lambda (var)
                (make-lexical-ref src name var)))
          ;; A free variable reference to a lambda, outside a lambda.
          ;; Could be the lexical-ref residualized by letrectification.
          ;; Copy and rely on size limiter to catch runaways.
          ((and (not in-lambda?) (lambda? (hashq-ref lexicals var)))
           (recur (hashq-ref lexicals var)))
          ((not in-lambda?)
           ;; No advantage to "inline" a toplevel to another toplevel.
           (abort!))
          ;; Some letrectified toplevels will be bound to lexical
          ;; variables, but unless the module has sealed private
          ;; bindings, there may be an associated top-level variable
          ;; as well.
          ((assq-ref binding-lexicals var)
           => (match-lambda
                ((mod' . name)
                 (cond
                  ((and (equal? mod' mod) (assq-ref exports name))
                   => (lambda (public-name)
                        (make-module-ref src mod public-name #t)))
                  (else
                   (residualize-module-private-ref src mod' name))))))
          ;; A free variable reference.  If it's in the program at this
          ;; point, that means that peval didn't see fit to copy it, so
          ;; there's no point in trying to do so here.
          (else (abort!))))

        (($ <toplevel-ref> src mod' name)
         (cond
          ;; Rewrite private references to exported bindings into public
          ;; references.  Peval can decide whether to continue inlining
          ;; or not.
          ((and (equal? mod mod') (assq-ref exports name))
           => (lambda (public-name)
                (make-module-ref src mod public-name #t)))
          (else
           (residualize-module-private-ref src mod' name))))

        (($ <call> src proc args)
         (unless in-lambda? (abort!))
         (make-call src (recur proc) (map recur args)))

        (($ <primcall> src name args)
         (unless in-lambda? (abort!))
         (make-primcall src name (map recur args)))

        (($ <conditional> src test consequent alternate)
         (unless in-lambda? (abort!))
         (make-conditional src (recur test)
                           (recur consequent) (recur alternate)))

        (($ <lexical-set> src name var exp)
         (unless in-lambda? (abort!))
         (cond
          ((assq-ref bound var)
           => (lambda (var)
                (make-lexical-set src name var (recur exp))))
          (else
           (abort!))))

        ((or ($ <toplevel-set>)
             ($ <module-set>)
             ($ <toplevel-define>))
         (abort!))

        (($ <lambda> src meta body)
         ;; Remove any lengthy docstring.
         (let ((meta (filter-map (match-lambda
                                   (('documentation . _) #f)
                                   (pair pair))
                                 meta)))
           (make-lambda src meta (and body (copy body bound #t)))))

        (($ <lambda-case> src req opt rest kw inits vars body alternate)
         (unless in-lambda? (abort!))
         (let* ((vars* (fresh-vars vars))
                (bound (add-bound-vars vars vars* bound)))
           (define (recur* exp) (copy exp bound #t))
           (make-lambda-case src req opt rest
                             (match kw
                               (#f #f)
                               ((aok? . kws)
                                (cons aok?
                                      (map
                                       (match-lambda
                                         ((kw name var)
                                          (list kw name (assq-ref bound var))))
                                       kws))))
                             (map recur* inits)
                             vars*
                             (recur* body)
                             (and alternate (recur alternate)))))

        (($ <seq> src head tail)
         (unless in-lambda? (abort!))
         (make-seq src (recur head) (recur tail)))
        
        (($ <let> src names vars vals body)
         (unless in-lambda? (abort!))
         (let* ((vars* (fresh-vars vars))
                (bound (add-bound-vars vars vars* bound)))
           (define (recur* exp) (copy exp bound #t))
           (make-let src names vars* (map recur vals) (recur* body))))

        (($ <letrec> src in-order? names vars vals body)
         (unless in-lambda? (abort!))
         (let* ((vars* (fresh-vars vars))
                (bound (add-bound-vars vars vars* bound)))
           (define (recur* exp) (copy exp bound #t))
           (make-letrec src in-order? names vars* (map recur* vals)
                        (recur* body))))

        (($ <fix> src names vars vals body)
         (unless in-lambda? (abort!))
         (let* ((vars* (fresh-vars vars))
                (bound (add-bound-vars vars vars* bound)))
           (define (recur* exp) (copy exp bound #t))
           (make-fix src names vars* (map recur* vals)
                     (recur* body))))

        (($ <let-values> src exp body)
         (unless in-lambda? (abort!))
         (make-let-values src (recur exp) (recur body)))

        (($ <prompt> src escape-only? tag body handler)
         (unless in-lambda? (abort!))
         (make-prompt src escape-only?
                      (recur tag) (recur body) (recur handler)))

        (($ <abort> src tag args tail)
         (unless in-lambda? (abort!))
         (make-abort src (recur tag) (map recur args) (recur tail)))))))

(define (compute-inlinable-bindings exp)
  "Traverse @var{exp}, extracting module-level definitions."

  (define-values (modules lexicals binding-lexicals bindings)
    (compute-module-bindings exp))

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
  (define modules-with-inlinable-exports
    (let lp ((defs modules) (not-inlinable '()) (inlinable '()))
      (match defs
        (() inlinable)
        (((mod . args) . defs)
         (cond ((member mod not-inlinable)
                (lp defs not-inlinable inlinable))
               ((or (assoc mod defs) ;; doubly defined?
                    (not (has-constant-initarg? args #:declarative? #t)))
                (lp defs (cons mod not-inlinable) inlinable))
               (else
                (lp defs not-inlinable (cons mod inlinable))))))))

  ;; Omit multiply-defined bindings, and definitions not in declarative
  ;; modules.
  (define non-declarative-definitions
    (let lp ((bindings bindings) (non-declarative '()))
      (match bindings
        (() non-declarative)
        ((((and mod+name (mod . name)) . val) . bindings)
         (cond
          ((member mod+name non-declarative)
           (lp bindings non-declarative))
          ((or (assoc mod+name bindings)
               (not (member mod modules-with-inlinable-exports)))
           (lp bindings (cons mod+name non-declarative)))
          (else
           (lp bindings non-declarative)))))))

  (define exports
    (map (lambda (module)
           (define args (assoc-ref modules module))
           ;; Return list of (PRIVATE-NAME . PUBLIC-NAME) pairs.
           (define (extract-exports kw)
             (kwarg-ref/const args kw
                              (lambda (val)
                                (map (match-lambda
                                       ((and pair (private . public)) pair)
                                       (name (cons name name)))
                                     val))
                              (lambda () '())))
           (cons module
                 (append (extract-exports #:exports)
                         (extract-exports #:replacements))))
         modules-with-inlinable-exports))

  ;; Compute ((PRIVATE-NAME . PUBLIC-NAME) . VALUE) pairs for each
  ;; module with inlinable bindings, for exported bindings only.
  (define inlinable-candidates
    (map
     (lambda (module)
       (define name-pairs (assoc-ref exports module))
       (define (name-pair private-name)
         (assq private-name name-pairs))
       (cons module
             (filter-map
              (match-lambda
                (((and mod+name (mod . name)) . val)
                 (and (equal? module mod)
                      (not (member mod+name non-declarative-definitions))
                      (and=> (name-pair name)
                             (lambda (pair) (cons pair val))))))
              bindings)))
     modules-with-inlinable-exports))

  (define inlinables
    (filter-map
     (match-lambda
       ((mod . exports)
        (let ((name-pairs (map car exports)))
          (match (filter-map
                  (match-lambda
                    (((private . public) . val)
                     (match (inlinable-exp mod name-pairs lexicals
                                           binding-lexicals val)
                       (#f #f)
                       (val (cons public val)))))
                  exports)
            (() #f)
            (exports (cons mod exports))))))
     inlinable-candidates))

  inlinables)

(define (put-uleb port val)
  (let lp ((val val))
    (let ((next (ash val -7)))
      (if (zero? next)
          (put-u8 port val)
          (begin
            (put-u8 port (logior #x80 (logand val #x7f)))
            (lp next))))))

(define (known-vtable vtable)
  (define-syntax-rule (tree-il-case vt ...)
    (cond
     ((eq? vtable vt) (values '(language tree-il) 'vt))
     ...
     (else (values #f #f))))
  (tree-il-case <void>
                <const>
                <primitive-ref>
                <lexical-ref>
                <lexical-set>
                <module-ref>
                <module-set>
                <toplevel-ref>
                <toplevel-set>
                <toplevel-define>
                <conditional>
                <call>
                <primcall>
                <seq>
                <lambda>
                <lambda-case>
                <let>
                <letrec>
                <fix>
                <let-values>
                <prompt>
                <abort>))

(define-record-type <encoding>
  (%make-encoding constants vtables pair-code vector-code symbol-code next-code)
  encoding?
  (constants constants)
  (vtables vtables)
  (pair-code pair-code set-pair-code!)
  (vector-code vector-code set-vector-code!)
  (symbol-code symbol-code set-symbol-code!)
  (next-code next-code set-next-code!))

(define (make-encoding)
  (%make-encoding (make-hash-table) (make-hash-table) #f #f #f 0))

(define (vtable-nfields vtable)
  (define vtable-index-size 5) ; FIXME: pull from struct.h
  (struct-ref/unboxed vtable vtable-index-size))

(define (build-encoding! term encoding)
  (define (next-code!)
    (let ((code (next-code encoding)))
      (set-next-code! encoding (1+ code))
      code))

  (define (intern-constant! x)
    (unless (hash-ref (constants encoding) x)
      (hash-set! (constants encoding) x (next-code!))))
  (define (intern-vtable! x)
    (unless (hashq-ref (vtables encoding) x)
      (hashq-set! (vtables encoding) x (next-code!))))
  (define (ensure-pair-code!)
    (unless (pair-code encoding)
      (set-pair-code! encoding (next-code!))))
  (define (ensure-vector-code!)
    (unless (vector-code encoding)
      (set-vector-code! encoding (next-code!))))
  (define (ensure-symbol-code!)
    (unless (symbol-code encoding)
      (set-symbol-code! encoding (next-code!))))

  (let visit ((term term))
    (cond
     ((pair? term)
      (ensure-pair-code!)
      (visit (car term))
      (visit (cdr term)))
     ((vector? term)
      (ensure-vector-code!)
      (visit (vector-length term))
      (let lp ((i 0))
        (when (< i (vector-length term))
          (visit (vector-ref term i))
          (lp (1+ i)))))
     ((symbol? term)
      (ensure-symbol-code!)
      (visit (symbol->string term)))
     ((struct? term)
      (let ((vtable (struct-vtable term)))
        (unless (known-vtable vtable)
          (error "struct of unknown type" term))
        (intern-vtable! vtable)
        (let ((nfields (vtable-nfields vtable)))
          (let lp ((i 0))
            (when (< i nfields)
              (visit (struct-ref term i))
              (lp (1+ i)))))))
     (else
      (intern-constant! term)))))

(define (compute-decoder encoding)
  (define (pair-clause code)
    `((eq? code ,code)
      (let* ((car (lp))
             (cdr (lp)))
        (cons car cdr))))
  (define (vector-clause code)
    `((eq? code ,code)
      (let* ((len (lp))
             (v (make-vector len)))
        (let init ((i 0))
          (when (< i len)
            (vector-set! v i (lp))
            (init (1+ i))))
        v)))
  (define (symbol-clause code)
    `((eq? code ,code)
      (string->symbol (lp))))
  (define (vtable-clause vtable code)
    (call-with-values (lambda () (known-vtable vtable))
      (lambda (mod name)
        (let ((fields (map (lambda (i) (string->symbol (format #f "f~a" i)))
                           (iota (vtable-nfields vtable)))))
          `((eq? code ,code)
            (let* (,@(map (lambda (field) `(,field (lp))) fields))
              (make-struct/simple (@ ,mod ,name) ,@fields)))))))
  (define (constant-clause constant code)
    `((eq? code ,code) ',constant))
  (define (map-encodings f table)
    (map (match-lambda
          ((value . code) (f value code)))
         (sort (hash-map->list cons table)
               (match-lambda*
                (((_ . code1) (_ . code2)) (< code1 code2))))))

  `(lambda (bv)
     (define pos 0)
     (define (next-u8!)
       (let ((u8 (bytevector-u8-ref bv pos)))
         (set! pos (1+ pos))
         u8))
     (define (next-uleb!)
       ,(if (< (next-code encoding) #x80)
            ;; No need for uleb decoding in this case.
            '(next-u8!)
            ;; FIXME: We have a maximum code length and probably we
            ;; should just inline the corresponding decoder instead of
            ;; looping.
            '(let lp ((n 0) (shift 0))
               (let ((b (next-u8!)))
                 (if (zero? (logand b #x80))
                     (logior (ash b shift) n)
                     (lp (logior (ash (logxor #x80 b) shift) n)
                         (+ shift 7)))))))
     (let lp ()
       (let ((code (next-uleb!)))
         (cond
          ,@(if (pair-code encoding)
                (list (pair-clause (pair-code encoding)))
                '())
          ,@(if (vector-code encoding)
                (list (vector-clause (vector-code encoding)))
                '())
          ,@(if (symbol-code encoding)
                (list (symbol-clause (symbol-code encoding)))
                '())
          ,@(map-encodings vtable-clause (vtables encoding))
          ,@(map-encodings constant-clause (constants encoding))
          (else (error "bad code" code)))))))

(define (encode term encoding)
  (call-with-output-bytevector
   (lambda (port)
     (define (put x) (put-uleb port x))
     (let visit ((term term))
       (cond
        ((pair? term)
         (put (pair-code encoding))
         (visit (car term))
         (visit (cdr term)))
        ((vector? term)
         (put (vector-code encoding))
         (visit (vector-length term))
         (let lp ((i 0))
           (when (< i (vector-length term))
             (visit (vector-ref term i))
             (lp (1+ i)))))
        ((symbol? term)
         (put (symbol-code encoding))
         (visit (symbol->string term)))
        ((struct? term)
         (let* ((vtable (struct-vtable term))
                (nfields (vtable-nfields vtable)))
           (put (hashq-ref (vtables encoding) vtable))
           (let lp ((i 0))
             (when (< i nfields)
               (visit (struct-ref term i))
               (lp (1+ i))))))
        (else
         (put (hash-ref (constants encoding) term))))))))

(define (compute-encoding bindings)
  (let ((encoding (make-encoding)))
    (for-each (match-lambda
                ((name . expr) (build-encoding! expr encoding)))
              bindings)
    (let ((encoded (map (match-lambda
                          ((name . expr) (cons name (encode expr encoding))))
                        bindings)))
      `(lambda (name)
         (define decode ,(compute-decoder encoding))
         (cond
          ,@(map (match-lambda
                   ((name . bv)
                    `((eq? name ',name) (decode ,bv))))
                 encoded)
          (else #f))))))

(define encoding-module (current-module))
(define (compile-inlinable-exports bindings)
  (let ((exp (compute-encoding bindings)))
    (fix-letrec
     (expand-primitives
      (resolve-primitives
       (compile-tree-il exp encoding-module '())
       encoding-module)))))

(define (attach-inlinables exp inlinables)
  (post-order
   (lambda (exp)
     (match exp
       (($ <call> src (and proc ($ <module-ref> _ '(guile) 'define-module* #f))
           ((and m ($ <const> _ mod)) . args))
        (cond
         ((assoc-ref inlinables mod)
          => (lambda (bindings)
               (let ((inlinables (compile-inlinable-exports bindings)))
                 (make-call src proc
                            (cons* m
                                   (make-const #f #:inlinable-exports)
                                   inlinables
                                   args)))))
         (else exp)))
       (exp exp)))
   exp))

(define (inlinable-exports exp)
  (attach-inlinables exp (compute-inlinable-bindings exp)))
