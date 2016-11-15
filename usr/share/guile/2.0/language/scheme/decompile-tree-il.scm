;;; Guile VM code converters

;; Copyright (C) 2001, 2009, 2012, 2013 Free Software Foundation, Inc.

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

(define-module (language scheme decompile-tree-il)
  #:use-module (language tree-il)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (system base syntax)
  #:export (decompile-tree-il))

(define (decompile-tree-il e env opts)
  (apply do-decompile e env opts))

(define* (do-decompile e env
                       #:key
                       (use-derived-syntax? #t)
                       (avoid-lambda? #t)
                       (use-case? #t)
                       (strip-numeric-suffixes? #f)
                       #:allow-other-keys)

  (receive (output-name-table occurrence-count-table)
      (choose-output-names e use-derived-syntax? strip-numeric-suffixes?)

    (define (output-name s)      (hashq-ref output-name-table s))
    (define (occurrence-count s) (hashq-ref occurrence-count-table s))

    (define (const x) (lambda (_) x))
    (define (atom? x) (not (or (pair? x) (vector? x))))

    (define (build-void) '(if #f #f))

    (define (build-begin es)
      (match es
        (() (build-void))
        ((e) e)
        (_ `(begin ,@es))))

    (define (build-lambda-body e)
      (match e
        (('let () body ...) body)
        (('begin es ...) es)
        (_ (list e))))

    (define (build-begin-body e)
      (match e
        (('begin es ...) es)
        (_ (list e))))

    (define (build-define name e)
      (match e
        ((? (const avoid-lambda?)
            ('lambda formals body ...))
         `(define (,name ,@formals) ,@body))
        ((? (const avoid-lambda?)
            ('lambda* formals body ...))
         `(define* (,name ,@formals) ,@body))
        (_ `(define ,name ,e))))

    (define (build-let names vals body)
      (match `(let ,(map list names vals)
                ,@(build-lambda-body body))
        ((_ () e) e)
        ((_ (b) ('let* (bs ...) body ...))
         `(let* (,b ,@bs) ,@body))
        ((? (const use-derived-syntax?)
            (_ (b1) ('let (b2) body ...)))
         `(let* (,b1 ,b2) ,@body))
        (e e)))

    (define (build-letrec in-order? names vals body)
      (match `(,(if in-order? 'letrec* 'letrec)
               ,(map list names vals)
               ,@(build-lambda-body body))
        ((_ () e) e)
        ((_ () body ...) `(let () ,@body))
        ((_ ((name ('lambda (formals ...) body ...)))
            (name args ...))
         (=> failure)
         (if (= (length formals) (length args))
             `(let ,name ,(map list formals args) ,@body)
             (failure)))
        ((? (const avoid-lambda?)
            ('letrec* _ body ...))
         `(let ()
            ,@(map build-define names vals)
            ,@body))
        (e e)))

    (define (build-if test consequent alternate)
      (match alternate
        (('if #f _) `(if ,test ,consequent))
        (_ `(if ,test ,consequent ,alternate))))

    (define (build-and xs)
      (match xs
        (() #t)
        ((x) x)
        (_ `(and ,@xs))))

    (define (build-or xs)
      (match xs
        (() #f)
        ((x) x)
        (_ `(or ,@xs))))

    (define (case-test-var test)
      (match test
        (('memv (? atom? v) ('quote (datums ...)))
         v)
        (('eqv? (? atom? v) ('quote datum))
         v)
        (_ #f)))

    (define (test->datums v test)
      (match (cons v test)
        ((v 'memv v ('quote (xs ...)))
         xs)
        ((v 'eqv? v ('quote x))
         (list x))
        (_ #f)))

    (define (build-else-tail e)
      (match e
        (('if #f _) '())
        (('and xs ... x) `((,(build-and xs) ,@(build-begin-body x))
                           (else #f)))
        (_ `((else ,@(build-begin-body e))))))

    (define (build-cond-else-tail e)
      (match e
        (('cond clauses ...) clauses)
        (_ (build-else-tail e))))

    (define (build-case-else-tail v e)
      (match (cons v e)
        ((v 'case v clauses ...)
         clauses)
        ((v 'if ('memv v ('quote (xs ...))) consequent . alternate*)
         `((,xs ,@(build-begin-body consequent))
           ,@(build-case-else-tail v (build-begin alternate*))))
        ((v 'if ('eqv? v ('quote x)) consequent . alternate*)
         `(((,x) ,@(build-begin-body consequent))
           ,@(build-case-else-tail v (build-begin alternate*))))
        (_ (build-else-tail e))))

    (define (clauses+tail clauses)
      (match clauses
        ((cs ... (and c ('else . _))) (values cs (list c)))
        (_ (values clauses '()))))

    (define (build-cond tests consequents alternate)
      (case (length tests)
        ((0) alternate)
        ((1) (build-if (car tests) (car consequents) alternate))
        (else `(cond ,@(map (lambda (test consequent)
                              `(,test ,@(build-begin-body consequent)))
                            tests consequents)
                     ,@(build-cond-else-tail alternate)))))

    (define (build-cond-or-case tests consequents alternate)
      (if (not use-case?)
          (build-cond tests consequents alternate)
          (let* ((v (and (not (null? tests))
                         (case-test-var (car tests))))
                 (datum-lists (take-while identity
                                          (map (cut test->datums v <>)
                                               tests)))
                 (n (length datum-lists))
                 (tail (build-case-else-tail v (build-cond
                                                (drop tests n)
                                                (drop consequents n)
                                                alternate))))
            (receive (clauses tail) (clauses+tail tail)
              (let ((n (+ n (length clauses)))
                    (datum-lists (append datum-lists
                                         (map car clauses)))
                    (consequents (append consequents
                                         (map build-begin
                                              (map cdr clauses)))))
                (if (< n 2)
                    (build-cond tests consequents alternate)
                    `(case ,v
                       ,@(map cons datum-lists (map build-begin-body
                                                    (take consequents n)))
                       ,@tail)))))))

    (define (recurse e)

      (define (recurse-body e)
        (build-lambda-body (recurse e)))

      (record-case e
        ((<void>)
         (build-void))

        ((<const> exp)
         (if (and (self-evaluating? exp) (not (vector? exp)))
             exp
             `(quote ,exp)))

        ((<sequence> exps)
         (build-begin (map recurse exps)))

        ((<application> proc args)
         (match `(,(recurse proc) ,@(map recurse args))
           ((('lambda (formals ...) body ...) args ...)
            (=> failure)
            (if (= (length formals) (length args))
                (build-let formals args (build-begin body))
                (failure)))
           (e e)))

        ((<primitive-ref> name)
         name)

        ((<lexical-ref> gensym)
         (output-name gensym))

        ((<lexical-set> gensym exp)
         `(set! ,(output-name gensym) ,(recurse exp)))

        ((<module-ref> mod name public?)
         `(,(if public? '@ '@@) ,mod ,name))

        ((<module-set> mod name public? exp)
         `(set! (,(if public? '@ '@@) ,mod ,name) ,(recurse exp)))

        ((<toplevel-ref> name)
         name)

        ((<toplevel-set> name exp)
         `(set! ,name ,(recurse exp)))

        ((<toplevel-define> name exp)
         (build-define name (recurse exp)))

        ((<lambda> meta body)
         (if body
             (let ((body (recurse body))
                   (doc (assq-ref meta 'documentation)))
               (if (not doc)
                   body
                   (match body
                     (('lambda formals body ...)
                      `(lambda ,formals ,doc ,@body))
                     (('lambda* formals body ...)
                      `(lambda* ,formals ,doc ,@body))
                     (('case-lambda (formals body ...) clauses ...)
                      `(case-lambda (,formals ,doc ,@body) ,@clauses))
                     (('case-lambda* (formals body ...) clauses ...)
                      `(case-lambda* (,formals ,doc ,@body) ,@clauses))
                     (e e))))
             '(case-lambda)))

        ((<lambda-case> req opt rest kw inits gensyms body alternate)
         (let ((names (map output-name gensyms)))
           (cond
            ((and (not opt) (not kw) (not alternate))
             `(lambda ,(if rest (apply cons* names) names)
                ,@(recurse-body body)))
            ((and (not opt) (not kw))
             (let ((alt-expansion (recurse alternate))
                   (formals (if rest (apply cons* names) names)))
               (case (car alt-expansion)
                 ((lambda)
                  `(case-lambda (,formals ,@(recurse-body body))
                                ,(cdr alt-expansion)))
                 ((lambda*)
                  `(case-lambda* (,formals ,@(recurse-body body))
                                 ,(cdr alt-expansion)))
                 ((case-lambda)
                  `(case-lambda (,formals ,@(recurse-body body))
                                ,@(cdr alt-expansion)))
                 ((case-lambda*)
                  `(case-lambda* (,formals ,@(recurse-body body))
                                 ,@(cdr alt-expansion))))))
            (else
             (let* ((alt-expansion (and alternate (recurse alternate)))
                    (nreq (length req))
                    (nopt (if opt (length opt) 0))
                    (restargs (if rest (list-ref names (+ nreq nopt)) '()))
                    (reqargs (list-head names nreq))
                    (optargs (if opt
                                 `(#:optional
                                   ,@(map list
                                          (list-head (list-tail names nreq) nopt)
                                          (map recurse
                                               (list-head inits nopt))))
                                 '()))
                    (kwargs (if kw
                                `(#:key
                                  ,@(map list
                                         (map output-name (map caddr (cdr kw)))
                                         (map recurse
                                              (list-tail inits nopt))
                                         (map car (cdr kw)))
                                  ,@(if (car kw)
                                        '(#:allow-other-keys)
                                        '()))
                                '()))
                    (formals `(,@reqargs ,@optargs ,@kwargs . ,restargs)))
               (if (not alt-expansion)
                   `(lambda* ,formals ,@(recurse-body body))
                   (case (car alt-expansion)
                     ((lambda lambda*)
                      `(case-lambda* (,formals ,@(recurse-body body))
                                     ,(cdr alt-expansion)))
                     ((case-lambda case-lambda*)
                      `(case-lambda* (,formals ,@(recurse-body body))
                                     ,@(cdr alt-expansion))))))))))

        ((<conditional> test consequent alternate)
         (define (simplify-test e)
           (match e
             (('if ('eqv? (? atom? v) ('quote a)) #t ('eqv? v ('quote b)))
              `(memv ,v '(,a ,b)))
             (('if ('eqv? (? atom? v) ('quote a)) #t ('memv v ('quote (bs ...))))
              `(memv ,v '(,a ,@bs)))
             (('case (? atom? v)
                ((datum) #t) ...
                ('else ('eqv? v ('quote last-datum))))
              `(memv ,v '(,@datum ,last-datum)))
             (_ e)))
         (match `(if ,(simplify-test (recurse test))
                     ,(recurse consequent)
                     ,@(if (void? alternate) '()
                           (list (recurse alternate))))
           (('if test ('if ('and xs ...) consequent))
            (build-if (build-and (cons test xs))
                      consequent
                      (build-void)))
           ((? (const use-derived-syntax?)
               ('if test1 ('if test2 consequent)))
            (build-if (build-and (list test1 test2))
                      consequent
                      (build-void)))
           (('if (? atom? x) x ('or ys ...))
            (build-or (cons x ys)))
           ((? (const use-derived-syntax?)
               ('if (? atom? x) x y))
            (build-or (list x y)))
           (('if test consequent)
            `(if ,test ,consequent))
           (('if test ('and xs ...) #f)
            (build-and (cons test xs)))
           ((? (const use-derived-syntax?)
               ('if test consequent #f))
            (build-and (list test consequent)))
           ((? (const use-derived-syntax?)
               ('if test1 consequent1
                    ('if test2 consequent2 . alternate*)))
            (build-cond-or-case (list test1 test2)
                                (list consequent1 consequent2)
                                (build-begin alternate*)))
           (('if test consequent ('cond clauses ...))
            `(cond (,test ,@(build-begin-body consequent))
                   ,@clauses))
           (('if ('memv (? atom? v) ('quote (xs ...))) consequent
                 ('case v clauses ...))
            `(case ,v (,xs ,@(build-begin-body consequent))
                   ,@clauses))
           (('if ('eqv? (? atom? v) ('quote x)) consequent
                 ('case v clauses ...))
            `(case ,v ((,x) ,@(build-begin-body consequent))
                   ,@clauses))
           (e e)))

        ((<let> gensyms vals body)
         (match (build-let (map output-name gensyms)
                           (map recurse vals)
                           (recurse body))
           (('let ((v e)) ('or v xs ...))
            (=> failure)
            (if (and (not (null? gensyms))
                     (= 3 (occurrence-count (car gensyms))))
                `(or ,e ,@xs)
                (failure)))
           (('let ((v e)) ('case v clauses ...))
            (=> failure)
            (if (and (not (null? gensyms))
                     ;; FIXME: This fails if any of the 'memv's were
                     ;; optimized into multiple 'eqv?'s, because the
                     ;; occurrence count will be higher than we expect.
                     (= (occurrence-count (car gensyms))
                        (1+ (length (clauses+tail clauses)))))
                `(case ,e ,@clauses)
                (failure)))
           (e e)))

        ((<letrec> in-order? gensyms vals body)
         (build-letrec in-order?
                       (map output-name gensyms)
                       (map recurse vals)
                       (recurse body)))

        ((<fix> gensyms vals body)
         ;; not a typo, we really do translate back to letrec. use letrec* since it
         ;; doesn't matter, and the naive letrec* transformation does not require an
         ;; inner let.
         (build-letrec #t
                       (map output-name gensyms)
                       (map recurse vals)
                       (recurse body)))

        ((<let-values> exp body)
         `(call-with-values (lambda () ,@(recurse-body exp))
            ,(recurse (make-lambda #f '() body))))

        ((<dynwind> body winder unwinder)
         `(dynamic-wind ,(recurse winder)
                        (lambda () ,@(recurse-body body))
                        ,(recurse unwinder)))

        ((<dynlet> fluids vals body)
         `(with-fluids ,(map list
                             (map recurse fluids)
                             (map recurse vals))
            ,@(recurse-body body)))

        ((<dynref> fluid)
         `(fluid-ref ,(recurse fluid)))

        ((<dynset> fluid exp)
         `(fluid-set! ,(recurse fluid) ,(recurse exp)))

        ((<prompt> tag body handler)
         `(call-with-prompt
           ,(recurse tag)
           (lambda () ,@(recurse-body body))
           ,(recurse handler)))


        ((<abort> tag args tail)
         `(apply abort ,(recurse tag) ,@(map recurse args)
                 ,(recurse tail)))))
    (values (recurse e) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Algorithm for choosing better variable names
;; ============================================
;;
;; First we perform an analysis pass, collecting the following
;; information:
;;
;; * For each gensym: how many occurrences will occur in the output?
;;
;; * For each gensym A: which gensyms does A conflict with?  Gensym A
;;   and gensym B conflict if they have the same base name (usually the
;;   same as the source name, but see below), and if giving them the
;;   same name would cause a bad variable reference due to unintentional
;;   variable capture.
;;
;; The occurrence counter is indexed by gensym and is global (within each
;; invocation of the algorithm), implemented using a hash table.  We also
;; keep a global mapping from gensym to source name as provided by the
;; binding construct (we prefer not to trust the source names in the
;; lexical ref or set).
;;
;; As we recurse down into lexical binding forms, we keep track of a
;; mapping from base name to an ordered list of bindings, innermost
;; first.  When we encounter a variable occurrence, we increment the
;; counter, look up the base name (preferring not to trust the 'name' in
;; the lexical ref or set), and then look up the bindings currently in
;; effect for that base name.  Hopefully our gensym will be the first
;; (innermost) binding.  If not, we register a conflict between the
;; referenced gensym and the other bound gensyms with the same base name
;; that shadow the binding we want.  These are simply the gensyms on the
;; binding list that come before our gensym.
;;
;; Top-level bindings are treated specially.  Whenever top-level
;; references are found, they conflict with every lexical binding
;; currently in effect with the same base name.  They are guaranteed to
;; be assigned to their source names.  For purposes of recording
;; conflicts (which are normally keyed on gensyms) top-level identifiers
;; are assigned a pseudo-gensym that is an interned pair of the form
;; (top-level . <name>).  This allows them to be compared using 'eq?'
;; like other gensyms.
;;
;; The base name is normally just the source name.  However, if the
;; source name has a suffix of the form "-N" (where N is a positive
;; integer without leading zeroes), then we strip that suffix (multiple
;; times if necessary) to form the base name.  We must do this because
;; we add suffixes of that form in order to resolve conflicts, and we
;; must ensure that only identifiers with the same base name can
;; possibly conflict with each other.
;;
;; XXX FIXME: Currently, primitives are treated exactly like top-level
;; bindings.  This handles conflicting lexical bindings properly, but
;; does _not_ handle the case where top-level bindings conflict with the
;; needed primitives.
;;
;; Also note that this requires that 'choose-output-names' be kept in
;; sync with 'tree-il->scheme'.  Primitives that are introduced by
;; 'tree-il->scheme' must be anticipated by 'choose-output-name'.
;;
;; We also ensure that lexically-bound identifiers found in operator
;; position will never be assigned one of the standard primitive names.
;; This is needed because 'tree-il->scheme' recognizes primitive names
;; in operator position and assumes that they have the standard
;; bindings.
;;
;;
;; How we assign an output name to each gensym
;; ===========================================
;;
;; We process the gensyms in order of decreasing occurrence count, with
;; each gensym choosing the best output name possible, as long as it
;; isn't the same name as any of the previously-chosen output names of
;; conflicting gensyms.
;;


;;
;; 'choose-output-names' analyzes the top-level form e, chooses good
;; variable names that are as close as possible to the source names,
;; and returns two values:
;;
;;  * a hash table mapping gensym to output name
;;  * a hash table mapping gensym to number of occurrences
;;
(define choose-output-names
  (let ()
    (define primitive?
      ;; This is a list of primitives that 'tree-il->scheme' assumes
      ;; will have the standard bindings when found in operator
      ;; position.
      (let* ((primitives '(if quote @ @@ set! define define*
                              begin let let* letrec letrec*
                              and or cond case
                              lambda lambda* case-lambda case-lambda*
                              apply call-with-values dynamic-wind
                              with-fluids fluid-ref fluid-set!
                              call-with-prompt abort memv eqv?))
             (table (make-hash-table (length primitives))))
        (for-each (cut hashq-set! table <> #t) primitives)
        (lambda (name) (hashq-ref table name))))

    ;; Repeatedly strip suffix of the form "-N", where N is a string
    ;; that could be produced by number->string given a positive
    ;; integer.  In other words, the first digit of N may not be 0.
    (define compute-base-name
      (let ((digits (string->char-set "0123456789")))
        (define (base-name-string str)
          (let ((i (string-skip-right str digits)))
            (if (and i (< (1+ i) (string-length str))
                     (eq? #\- (string-ref str i))
                     (not (eq? #\0 (string-ref str (1+ i)))))
                (base-name-string (substring str 0 i))
                str)))
        (lambda (sym)
          (string->symbol (base-name-string (symbol->string sym))))))

    ;; choose-output-names
    (lambda (e use-derived-syntax? strip-numeric-suffixes?)

      (define lexical-gensyms '())

      (define top-level-intern!
        (let ((table (make-hash-table)))
          (lambda (name)
            (let ((h (hashq-create-handle! table name #f)))
              (or (cdr h) (begin (set-cdr! h (cons 'top-level name))
                                 (cdr h)))))))
      (define (top-level? s) (pair? s))
      (define (top-level-name s) (cdr s))

      (define occurrence-count-table (make-hash-table))
      (define (occurrence-count s) (or (hashq-ref occurrence-count-table s) 0))
      (define (increment-occurrence-count! s)
        (let ((h (hashq-create-handle! occurrence-count-table s 0)))
          (if (zero? (cdr h))
              (set! lexical-gensyms (cons s lexical-gensyms)))
          (set-cdr! h (1+ (cdr h)))))

      (define base-name
        (let ((table (make-hash-table)))
          (lambda (name)
            (let ((h (hashq-create-handle! table name #f)))
              (or (cdr h) (begin (set-cdr! h (compute-base-name name))
                                 (cdr h)))))))

      (define source-name-table (make-hash-table))
      (define (set-source-name! s name)
        (if (not (top-level? s))
            (let ((name (if strip-numeric-suffixes?
                            (base-name name)
                            name)))
              (hashq-set! source-name-table s name))))
      (define (source-name s)
        (if (top-level? s)
            (top-level-name s)
            (hashq-ref source-name-table s)))

      (define conflict-table (make-hash-table))
      (define (conflicts s) (or (hashq-ref conflict-table s) '()))
      (define (add-conflict! a b)
        (define (add! a b)
          (if (not (top-level? a))
              (let ((h (hashq-create-handle! conflict-table a '())))
                (if (not (memq b (cdr h)))
                    (set-cdr! h (cons b (cdr h)))))))
        (add! a b)
        (add! b a))

      (let recurse-with-bindings ((e e) (bindings vlist-null))
        (let recurse ((e e))

          ;; We call this whenever we encounter a top-level ref or set
          (define (top-level name)
            (let ((bname (base-name name)))
              (let ((s (top-level-intern! name))
                    (conflicts (vhash-foldq* cons '() bname bindings)))
                (for-each (cut add-conflict! s <>) conflicts))))

          ;; We call this whenever we encounter a primitive reference.
          ;; We must also call it for every primitive that might be
          ;; inserted by 'tree-il->scheme'.  It is okay to call this
          ;; even when 'tree-il->scheme' will not insert the named
          ;; primitive; the worst that will happen is for a lexical
          ;; variable of the same name to be renamed unnecessarily.
          (define (primitive name) (top-level name))

          ;; We call this whenever we encounter a lexical ref or set.
          (define (lexical s)
            (increment-occurrence-count! s)
            (let ((conflicts
                   (take-while
                    (lambda (s*) (not (eq? s s*)))
                    (reverse! (vhash-foldq* cons
                                            '()
                                            (base-name (source-name s))
                                            bindings)))))
              (for-each (cut add-conflict! s <>) conflicts)))

          (record-case e
            ((<void>)  (primitive 'if)) ; (if #f #f)
            ((<const>) (primitive 'quote))

            ((<application> proc args)
             (if (lexical-ref? proc)
                 (let* ((gensym (lexical-ref-gensym proc))
                        (name (source-name gensym)))
                   ;; If the operator position contains a bare variable
                   ;; reference with the same source name as a standard
                   ;; primitive, we must ensure that it will be given a
                   ;; different name, so that 'tree-il->scheme' will not
                   ;; misinterpret the resulting expression.
                   (if (primitive? name)
                       (add-conflict! gensym (top-level-intern! name)))))
             (recurse proc)
             (for-each recurse args))

            ((<primitive-ref> name) (primitive name))

            ((<lexical-ref> gensym) (lexical gensym))
            ((<lexical-set> gensym exp)
             (primitive 'set!) (lexical gensym) (recurse exp))

            ((<module-ref> public?) (primitive (if public? '@ '@@)))
            ((<module-set> public? exp)
             (primitive 'set!) (primitive (if public? '@ '@@)) (recurse exp))

            ((<toplevel-ref> name) (top-level name))
            ((<toplevel-set> name exp)
             (primitive 'set!) (top-level name) (recurse exp))
            ((<toplevel-define> name exp) (top-level name) (recurse exp))

            ((<conditional> test consequent alternate)
             (cond (use-derived-syntax?
                    (primitive 'and) (primitive 'or)
                    (primitive 'cond) (primitive 'case)
                    (primitive 'else) (primitive '=>)))
             (primitive 'if)
             (recurse test) (recurse consequent) (recurse alternate))

            ((<sequence> exps) (primitive 'begin) (for-each recurse exps))
            ((<lambda> body)
             (if body (recurse body) (primitive 'case-lambda)))

            ((<lambda-case> req opt rest kw inits gensyms body alternate)
             (primitive 'lambda)
             (cond ((or opt kw alternate)
                    (primitive 'lambda*)
                    (primitive 'case-lambda)
                    (primitive 'case-lambda*)))
             (primitive 'let)
             (if use-derived-syntax? (primitive 'let*))
             (let* ((names (append req (or opt '()) (if rest (list rest) '())
                                   (map cadr (if kw (cdr kw) '()))))
                    (base-names (map base-name names))
                    (body-bindings
                     (fold vhash-consq bindings base-names gensyms)))
               (for-each increment-occurrence-count! gensyms)
               (for-each set-source-name! gensyms names)
               (for-each recurse inits)
               (recurse-with-bindings body body-bindings)
               (if alternate (recurse alternate))))

            ((<let> names gensyms vals body)
             (primitive 'let)
             (cond (use-derived-syntax? (primitive 'let*) (primitive 'or)))
             (for-each increment-occurrence-count! gensyms)
             (for-each set-source-name! gensyms names)
             (for-each recurse vals)
             (recurse-with-bindings
              body (fold vhash-consq bindings (map base-name names) gensyms)))

            ((<letrec> in-order? names gensyms vals body)
             (primitive 'let)
             (cond (use-derived-syntax? (primitive 'let*) (primitive 'or)))
             (primitive (if in-order? 'letrec* 'letrec))
             (for-each increment-occurrence-count! gensyms)
             (for-each set-source-name! gensyms names)
             (let* ((base-names (map base-name names))
                    (bindings (fold vhash-consq bindings base-names gensyms)))
               (for-each (cut recurse-with-bindings <> bindings) vals)
               (recurse-with-bindings body bindings)))

            ((<fix> names gensyms vals body)
             (primitive 'let)
             (primitive 'letrec*)
             (cond (use-derived-syntax? (primitive 'let*) (primitive 'or)))
             (for-each increment-occurrence-count! gensyms)
             (for-each set-source-name! gensyms names)
             (let* ((base-names (map base-name names))
                    (bindings (fold vhash-consq bindings base-names gensyms)))
               (for-each (cut recurse-with-bindings <> bindings) vals)
               (recurse-with-bindings body bindings)))

            ((<let-values> exp body)
             (primitive 'call-with-values)
             (recurse exp) (recurse body))

            ((<dynwind> winder body unwinder)
             (primitive 'dynamic-wind)
             (recurse winder) (recurse body) (recurse unwinder))

            ((<dynlet> fluids vals body)
             (primitive 'with-fluids)
             (for-each recurse fluids)
             (for-each recurse vals)
             (recurse body))

            ((<dynref> fluid) (primitive 'fluid-ref) (recurse fluid))
            ((<dynset> fluid exp)
             (primitive 'fluid-set!) (recurse fluid) (recurse exp))

            ((<prompt> tag body handler)
             (primitive 'call-with-prompt)
             (primitive 'lambda)
             (recurse tag) (recurse body) (recurse handler))

            ((<abort> tag args tail)
             (primitive 'apply)
             (primitive 'abort)
             (recurse tag) (for-each recurse args) (recurse tail)))))

      (let ()
        (define output-name-table (make-hash-table))
        (define (set-output-name! s name)
          (hashq-set! output-name-table s name))
        (define (output-name s)
          (if (top-level? s)
              (top-level-name s)
              (hashq-ref output-name-table s)))

        (define sorted-lexical-gensyms
          (sort-list lexical-gensyms
                     (lambda (a b) (> (occurrence-count a)
                                      (occurrence-count b)))))

        (for-each (lambda (s)
                    (set-output-name!
                     s
                     (let ((the-conflicts (conflicts s))
                           (the-source-name (source-name s)))
                       (define (not-yet-taken? name)
                         (not (any (lambda (s*)
                                     (and=> (output-name s*)
                                            (cut eq? name <>)))
                                   the-conflicts)))
                       (if (not-yet-taken? the-source-name)
                           the-source-name
                           (let ((prefix (string-append
                                          (symbol->string the-source-name)
                                          "-")))
                             (let loop ((i 1) (name the-source-name))
                               (if (not-yet-taken? name)
                                   name
                                   (loop (+ i 1)
                                         (string->symbol
                                          (string-append
                                           prefix
                                           (number->string i)))))))))))
                  sorted-lexical-gensyms)
        (values output-name-table occurrence-count-table)))))
