;;; TREE-IL -> GLIL compiler

;; Copyright (C) 2001, 2008, 2009, 2010, 2011, 2012,
;;   2014 Free Software Foundation, Inc.

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

(define-module (language tree-il analyze)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (system base syntax)
  #:use-module (system base message)
  #:use-module (system vm program)
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  #:export (analyze-lexicals
            analyze-tree
            unused-variable-analysis
            unused-toplevel-analysis
            unbound-variable-analysis
            arity-analysis
            format-analysis))

;; Allocation is the process of assigning storage locations for lexical
;; variables. A lexical variable has a distinct "address", or storage
;; location, for each procedure in which it is referenced.
;;
;; A variable is "local", i.e., allocated on the stack, if it is
;; referenced from within the procedure that defined it. Otherwise it is
;; a "closure" variable. For example:
;;
;;    (lambda (a) a) ; a will be local
;; `a' is local to the procedure.
;;
;;    (lambda (a) (lambda () a))
;; `a' is local to the outer procedure, but a closure variable with
;; respect to the inner procedure.
;;
;; If a variable is ever assigned, it needs to be heap-allocated
;; ("boxed"). This is so that closures and continuations capture the
;; variable's identity, not just one of the values it may have over the
;; course of program execution. If the variable is never assigned, there
;; is no distinction between value and identity, so closing over its
;; identity (whether through closures or continuations) can make a copy
;; of its value instead.
;;
;; Local variables are stored on the stack within a procedure's call
;; frame. Their index into the stack is determined from their linear
;; postion within a procedure's binding path:
;; (let (0 1)
;;   (let (2 3) ...)
;;   (let (2) ...))
;;   (let (2 3 4) ...))
;; etc.
;;
;; This algorithm has the problem that variables are only allocated
;; indices at the end of the binding path. If variables bound early in
;; the path are not used in later portions of the path, their indices
;; will not be recycled. This problem is particularly egregious in the
;; expansion of `or':
;;
;;  (or x y z)
;;    -> (let ((a x)) (if a a (let ((b y)) (if b b z))))
;;
;; As you can see, the `a' binding is only used in the ephemeral
;; `consequent' clause of the first `if', but its index would be
;; reserved for the whole of the `or' expansion. So we have a hack for
;; this specific case. A proper solution would be some sort of liveness
;; analysis, and not our linear allocation algorithm.
;;
;; Closure variables are captured when a closure is created, and stored in a
;; vector inline to the closure object itself. Each closure variable has a
;; unique index into that vector.
;;
;; There is one more complication. Procedures bound by <fix> may, in
;; some cases, be rendered inline to their parent procedure. That is to
;; say,
;;
;;  (letrec ((lp (lambda () (lp)))) (lp))
;;    => (fix ((lp (lambda () (lp)))) (lp))
;;      => goto FIX-BODY; LP: goto LP; FIX-BODY: goto LP;
;;         ^ jump over the loop  ^ the fixpoint lp ^ starting off the loop
;;
;; The upshot is that we don't have to allocate any space for the `lp'
;; closure at all, as it can be rendered inline as a loop. So there is
;; another kind of allocation, "label allocation", in which the
;; procedure is simply a label, placed at the start of the lambda body.
;; The label is the gensym under which the lambda expression is bound.
;;
;; The analyzer checks to see that the label is called with the correct
;; number of arguments. Calls to labels compile to rename + goto.
;; Lambda, the ultimate goto!
;;
;;
;; The return value of `analyze-lexicals' is a hash table, the
;; "allocation".
;;
;; The allocation maps gensyms -- recall that each lexically bound
;; variable has a unique gensym -- to storage locations ("addresses").
;; Since one gensym may have many storage locations, if it is referenced
;; in many procedures, it is a two-level map.
;;
;; The allocation also stored information on how many local variables
;; need to be allocated for each procedure, lexicals that have been
;; translated into labels, and information on what free variables to
;; capture from its lexical parent procedure.
;;
;; In addition, we have a conflation: while we're traversing the code,
;; recording information to pass to the compiler, we take the
;; opportunity to generate labels for each lambda-case clause, so that
;; generated code can skip argument checks at runtime if they match at
;; compile-time.
;;
;; Also, while we're a-traversing and an-allocating, we check prompt
;; handlers to see if the "continuation" argument is used. If not, we
;; mark the prompt as being "escape-only". This allows us to implement
;; `catch' and `throw' using `prompt' and `control', but without causing
;; a continuation to be reified. Heh heh.
;;
;; That is:
;;
;;  sym -> {lambda -> address}
;;  lambda -> (labels . free-locs)
;;  lambda-case -> (gensym . nlocs)
;;  prompt -> escape-only?
;;
;; address ::= (local? boxed? . index)
;; labels ::= ((sym . lambda) ...)
;; free-locs ::= ((sym0 . address0) (sym1 . address1) ...)
;; free variable addresses are relative to parent proc.

(define (make-hashq k v)
  (let ((res (make-hash-table)))
    (hashq-set! res k v)
    res))

(define (analyze-lexicals x)
  ;; bound-vars: lambda -> (sym ...)
  ;;  all identifiers bound within a lambda
  (define bound-vars (make-hash-table))
  ;; free-vars: lambda -> (sym ...)
  ;;  all identifiers referenced in a lambda, but not bound
  ;;  NB, this includes identifiers referenced by contained lambdas
  (define free-vars (make-hash-table))
  ;; assigned: sym -> #t
  ;;  variables that are assigned
  (define assigned (make-hash-table))
  ;; refcounts: sym -> count
  ;;  allows us to detect the or-expansion in O(1) time
  (define refcounts (make-hash-table))
  ;; labels: sym -> lambda
  ;;  for determining if fixed-point procedures can be rendered as
  ;;  labels.
  (define labels (make-hash-table))

  ;; returns variables referenced in expr
  (define (analyze! x proc labels-in-proc tail? tail-call-args)
    (define (step y) (analyze! y proc '() #f #f))
    (define (step-tail y) (analyze! y proc labels-in-proc tail? #f))
    (define (step-tail-call y args) (analyze! y proc labels-in-proc #f
                                              (and tail? args)))
    (define (recur/labels x new-proc labels)
      (analyze! x new-proc (append labels labels-in-proc) #t #f))
    (define (recur x new-proc) (analyze! x new-proc '() tail? #f))
    (record-case x
      ((<application> proc args)
       (apply lset-union eq? (step-tail-call proc args)
              (map step args)))

      ((<conditional> test consequent alternate)
       (lset-union eq? (step test) (step-tail consequent) (step-tail alternate)))

      ((<lexical-ref> gensym)
       (hashq-set! refcounts gensym (1+ (hashq-ref refcounts gensym 0)))
       (if (not (and tail-call-args
                     (memq gensym labels-in-proc)
                     (let ((p (hashq-ref labels gensym)))
                       (and p
                            (let lp ((c (lambda-body p)))
                              (and c (lambda-case? c)
                                   (or 
                                    ;; for now prohibit optional &
                                    ;; keyword arguments; can relax this
                                    ;; restriction later
                                    (and (= (length (lambda-case-req c))
                                            (length tail-call-args))
                                         (not (lambda-case-opt c))
                                         (not (lambda-case-kw c))
                                         (not (lambda-case-rest c)))
                                    (lp (lambda-case-alternate c)))))))))
           (hashq-set! labels gensym #f))
       (list gensym))
      
      ((<lexical-set> gensym exp)
       (hashq-set! assigned gensym #t)
       (hashq-set! labels gensym #f)
       (lset-adjoin eq? (step exp) gensym))
      
      ((<module-set> exp)
       (step exp))
      
      ((<toplevel-set> exp)
       (step exp))
      
      ((<toplevel-define> exp)
       (step exp))
      
      ((<sequence> exps)
       (let lp ((exps exps) (ret '()))
         (cond ((null? exps) '())
               ((null? (cdr exps))
                (lset-union eq? ret (step-tail (car exps))))
               (else
                (lp (cdr exps) (lset-union eq? ret (step (car exps))))))))
      
      ((<lambda> body)
       ;; order is important here
       (hashq-set! bound-vars x '())
       (let ((free (recur body x)))
         (hashq-set! bound-vars x (reverse! (hashq-ref bound-vars x)))
         (hashq-set! free-vars x free)
         free))
      
      ((<lambda-case> opt kw inits gensyms body alternate)
       (hashq-set! bound-vars proc
                   (append (reverse gensyms) (hashq-ref bound-vars proc)))
       (lset-union
        eq?
        (lset-difference eq?
                         (lset-union eq?
                                     (apply lset-union eq? (map step inits))
                                     (step-tail body))
                         gensyms)
        (if alternate (step-tail alternate) '())))
      
      ((<let> gensyms vals body)
       (hashq-set! bound-vars proc
                   (append (reverse gensyms) (hashq-ref bound-vars proc)))
       (lset-difference eq?
                        (apply lset-union eq? (step-tail body) (map step vals))
                        gensyms))
      
      ((<letrec> gensyms vals body)
       (hashq-set! bound-vars proc
                   (append (reverse gensyms) (hashq-ref bound-vars proc)))
       (for-each (lambda (sym) (hashq-set! assigned sym #t)) gensyms)
       (lset-difference eq?
                        (apply lset-union eq? (step-tail body) (map step vals))
                        gensyms))
      
      ((<fix> gensyms vals body)
       ;; Try to allocate these procedures as labels.
       (for-each (lambda (sym val) (hashq-set! labels sym val))
                 gensyms vals)
       (hashq-set! bound-vars proc
                   (append (reverse gensyms) (hashq-ref bound-vars proc)))
       ;; Step into subexpressions.
       (let* ((var-refs
               (map
                ;; Since we're trying to label-allocate the lambda,
                ;; pretend it's not a closure, and just recurse into its
                ;; body directly. (Otherwise, recursing on a closure
                ;; that references one of the fix's bound vars would
                ;; prevent label allocation.)
                (lambda (x)
                  (record-case x
                    ((<lambda> body)
                     ;; just like the closure case, except here we use
                     ;; recur/labels instead of recur
                     (hashq-set! bound-vars x '())
                     (let ((free (recur/labels body x gensyms)))
                       (hashq-set! bound-vars x (reverse! (hashq-ref bound-vars x)))
                       (hashq-set! free-vars x free)
                       free))))
                vals))
              (vars-with-refs (map cons gensyms var-refs))
              (body-refs (recur/labels body proc gensyms)))
         (define (delabel-dependents! sym)
           (let ((refs (assq-ref vars-with-refs sym)))
             (if refs
                 (for-each (lambda (sym)
                             (if (hashq-ref labels sym)
                                 (begin
                                   (hashq-set! labels sym #f)
                                   (delabel-dependents! sym))))
                           refs))))
         ;; Stepping into the lambdas and the body might have made some
         ;; procedures not label-allocatable -- which might have
         ;; knock-on effects. For example:
         ;;   (fix ((a (lambda () (b)))
         ;;         (b (lambda () a)))
         ;;     (a))
         ;; As far as `a' is concerned, both `a' and `b' are
         ;; label-allocatable. But `b' references `a' not in a proc-tail
         ;; position, which makes `a' not label-allocatable. The
         ;; knock-on effect is that, when back-propagating this
         ;; information to `a', `b' will also become not
         ;; label-allocatable, as it is referenced within `a', which is
         ;; allocated as a closure. This is a transitive relationship.
         (for-each (lambda (sym)
                     (if (not (hashq-ref labels sym))
                         (delabel-dependents! sym)))
                   gensyms)
         ;; Now lift bound variables with label-allocated lambdas to the
         ;; parent procedure.
         (for-each
          (lambda (sym val)
            (if (hashq-ref labels sym)
                ;; Remove traces of the label-bound lambda. The free
                ;; vars will propagate up via the return val.
                (begin
                  (hashq-set! bound-vars proc
                              (append (hashq-ref bound-vars val)
                                      (hashq-ref bound-vars proc)))
                  (hashq-remove! bound-vars val)
                  (hashq-remove! free-vars val))))
          gensyms vals)
         (lset-difference eq?
                          (apply lset-union eq? body-refs var-refs)
                          gensyms)))
      
      ((<let-values> exp body)
       (lset-union eq? (step exp) (step body)))
      
      ((<dynwind> body winder unwinder)
       (lset-union eq? (step body) (step winder) (step unwinder)))
      
      ((<dynlet> fluids vals body)
       (apply lset-union eq? (step body) (map step (append fluids vals))))
      
      ((<dynref> fluid)
       (step fluid))
      
      ((<dynset> fluid exp)
       (lset-union eq? (step fluid) (step exp)))
      
      ((<prompt> tag body handler)
       (lset-union eq? (step tag) (step body) (step-tail handler)))
      
      ((<abort> tag args tail)
       (apply lset-union eq? (step tag) (step tail) (map step args)))
      
      (else '())))
  
  ;; allocation: sym -> {lambda -> address}
  ;;             lambda -> (labels . free-locs)
  ;;             lambda-case -> (gensym . nlocs)
  (define allocation (make-hash-table))
  
  (define (allocate! x proc n)
    (define (recur y) (allocate! y proc n))
    (record-case x
      ((<application> proc args)
       (apply max (recur proc) (map recur args)))

      ((<conditional> test consequent alternate)
       (max (recur test) (recur consequent) (recur alternate)))

      ((<lexical-set> exp)
       (recur exp))
      
      ((<module-set> exp)
       (recur exp))
      
      ((<toplevel-set> exp)
       (recur exp))
      
      ((<toplevel-define> exp)
       (recur exp))
      
      ((<sequence> exps)
       (apply max (map recur exps)))
      
      ((<lambda> body)
       ;; allocate closure vars in order
       (let lp ((c (hashq-ref free-vars x)) (n 0))
         (if (pair? c)
             (begin
               (hashq-set! (hashq-ref allocation (car c))
                           x
                           `(#f ,(hashq-ref assigned (car c)) . ,n))
               (lp (cdr c) (1+ n)))))
      
       (let ((nlocs (allocate! body x 0))
             (free-addresses
              (map (lambda (v)
                     (hashq-ref (hashq-ref allocation v) proc))
                   (hashq-ref free-vars x)))
             (labels (filter cdr
                             (map (lambda (sym)
                                    (cons sym (hashq-ref labels sym)))
                                  (hashq-ref bound-vars x)))))
         ;; set procedure allocations
         (hashq-set! allocation x (cons labels free-addresses)))
       n)

      ((<lambda-case> opt kw inits gensyms body alternate)
       (max
        (let lp ((gensyms gensyms) (n n))
          (if (null? gensyms)
              (let ((nlocs (apply
                            max
                            (allocate! body proc n)
                            ;; inits not logically at the end, but they
                            ;; are the list...
                            (map (lambda (x) (allocate! x proc n)) inits))))
                ;; label and nlocs for the case
                (hashq-set! allocation x (cons (gensym ":LCASE") nlocs))
                nlocs)
              (begin
                (hashq-set! allocation (car gensyms)
                            (make-hashq
                             proc `(#t ,(hashq-ref assigned (car gensyms)) . ,n)))
                (lp (cdr gensyms) (1+ n)))))
        (if alternate (allocate! alternate proc n) n)))
      
      ((<let> gensyms vals body)
       (let ((nmax (apply max (map recur vals))))
         (cond
          ;; the `or' hack
          ((and (conditional? body)
                (= (length gensyms) 1)
                (let ((v (car gensyms)))
                  (and (not (hashq-ref assigned v))
                       (= (hashq-ref refcounts v 0) 2)
                       (lexical-ref? (conditional-test body))
                       (eq? (lexical-ref-gensym (conditional-test body)) v)
                       (lexical-ref? (conditional-consequent body))
                       (eq? (lexical-ref-gensym (conditional-consequent body)) v))))
           (hashq-set! allocation (car gensyms)
                       (make-hashq proc `(#t #f . ,n)))
           ;; the 1+ for this var
           (max nmax (1+ n) (allocate! (conditional-alternate body) proc n)))
          (else
           (let lp ((gensyms gensyms) (n n))
             (if (null? gensyms)
                 (max nmax (allocate! body proc n))
                 (let ((v (car gensyms)))
                   (hashq-set!
                    allocation v
                    (make-hashq proc
                                `(#t ,(hashq-ref assigned v) . ,n)))
                   (lp (cdr gensyms) (1+ n)))))))))
      
      ((<letrec> gensyms vals body)
       (let lp ((gensyms gensyms) (n n))
         (if (null? gensyms)
             (let ((nmax (apply max
                                (map (lambda (x)
                                       (allocate! x proc n))
                                     vals))))
               (max nmax (allocate! body proc n)))
             (let ((v (car gensyms)))
               (hashq-set!
                allocation v
                (make-hashq proc
                            `(#t ,(hashq-ref assigned v) . ,n)))
               (lp (cdr gensyms) (1+ n))))))

      ((<fix> gensyms vals body)
       (let lp ((in gensyms) (n n))
         (if (null? in)
             (let lp ((gensyms gensyms) (vals vals) (nmax n))
               (cond
                ((null? gensyms)
                 (max nmax (allocate! body proc n)))
                ((hashq-ref labels (car gensyms))                 
                 ;; allocate lambda body inline to proc
                 (lp (cdr gensyms)
                     (cdr vals)
                     (record-case (car vals)
                       ((<lambda> body)
                        (max nmax (allocate! body proc n))))))
                (else
                 ;; allocate closure
                 (lp (cdr gensyms)
                     (cdr vals)
                     (max nmax (allocate! (car vals) proc n))))))
             
             (let ((v (car in)))
               (cond
                ((hashq-ref assigned v)
                 (error "fixpoint procedures may not be assigned" x))
                ((hashq-ref labels v)
                 ;; no binding, it's a label
                 (lp (cdr in) n))
                (else
                 ;; allocate closure binding
                 (hashq-set! allocation v (make-hashq proc `(#t #f . ,n)))
                 (lp (cdr in) (1+ n))))))))

      ((<let-values> exp body)
       (max (recur exp) (recur body)))
      
      ((<dynwind> body winder unwinder)
       (max (recur body) (recur winder) (recur unwinder)))
      
      ((<dynlet> fluids vals body)
       (apply max (recur body) (map recur (append fluids vals))))
      
      ((<dynref> fluid)
       (recur fluid))
      
      ((<dynset> fluid exp)
       (max (recur fluid) (recur exp)))
      
      ((<prompt> tag body handler)
       (let ((cont-var (and (lambda-case? handler)
                            (pair? (lambda-case-gensyms handler))
                            (car (lambda-case-gensyms handler)))))
         (hashq-set! allocation x
                     (and cont-var (zero? (hashq-ref refcounts cont-var 0))))
         (max (recur tag) (recur body) (recur handler))))
      
      ((<abort> tag args tail)
       (apply max (recur tag) (recur tail) (map recur args)))
      
      (else n)))

  (analyze! x #f '() #t #f)
  (allocate! x #f 0)

  allocation)


;;;
;;; Tree analyses for warnings.
;;;

(define-record-type <tree-analysis>
  (make-tree-analysis leaf down up post init)
  tree-analysis?
  (leaf tree-analysis-leaf)  ;; (lambda (x result env locs) ...)
  (down tree-analysis-down)  ;; (lambda (x result env locs) ...)
  (up   tree-analysis-up)    ;; (lambda (x result env locs) ...)
  (post tree-analysis-post)  ;; (lambda (result env) ...)
  (init tree-analysis-init)) ;; arbitrary value

(define (analyze-tree analyses tree env)
  "Run all tree analyses listed in ANALYSES on TREE for ENV, using
`tree-il-fold'.  Return TREE.  The leaf/down/up procedures of each analysis are
passed a ``location stack', which is the stack of `tree-il-src' values for each
parent tree (a list); it can be used to approximate source location when
accurate information is missing from a given `tree-il' element."

  (define (traverse proc update-locs)
    ;; Return a tree traversing procedure that returns a list of analysis
    ;; results prepended by the location stack.
    (lambda (x results)
      (let ((locs (update-locs x (car results))))
        (cons locs ;; the location stack
              (map (lambda (analysis result)
                     ((proc analysis) x result env locs))
                   analyses
                   (cdr results))))))

  ;; Keeping/extending/shrinking the location stack.
  (define (keep-locs x locs)   locs)
  (define (extend-locs x locs) (cons (tree-il-src x) locs))
  (define (shrink-locs x locs) (cdr locs))

  (let ((results
         (tree-il-fold (traverse tree-analysis-leaf keep-locs)
                       (traverse tree-analysis-down extend-locs)
                       (traverse tree-analysis-up   shrink-locs)
                       (cons '() ;; empty location stack
                             (map tree-analysis-init analyses))
                       tree)))

    (for-each (lambda (analysis result)
                ((tree-analysis-post analysis) result env))
              analyses
              (cdr results)))

  tree)


;;;
;;; Unused variable analysis.
;;;

;; <binding-info> records are used during tree traversals in
;; `unused-variable-analysis'.  They contain a list of the local vars
;; currently in scope, and a list of locals vars that have been referenced.
(define-record-type <binding-info>
  (make-binding-info vars refs)
  binding-info?
  (vars binding-info-vars)  ;; ((GENSYM NAME LOCATION) ...)
  (refs binding-info-refs)) ;; (GENSYM ...)

(define (gensym? sym)
  ;; Return #t if SYM is (likely) a generated symbol.
  (string-any #\space (symbol->string sym)))

(define unused-variable-analysis
  ;; Report unused variables in the given tree.
  (make-tree-analysis
   (lambda (x info env locs)
     ;; X is a leaf: extend INFO's refs accordingly.
     (let ((refs (binding-info-refs info))
           (vars (binding-info-vars info)))
       (record-case x
         ((<lexical-ref> gensym)
          (make-binding-info vars (vhash-consq gensym #t refs)))
         (else info))))

   (lambda (x info env locs)
     ;; Going down into X: extend INFO's variable list
     ;; accordingly.
     (let ((refs (binding-info-refs info))
           (vars (binding-info-vars info))
           (src  (tree-il-src x)))
       (define (extend inner-vars inner-names)
         (fold (lambda (var name vars)
                 (vhash-consq var (list name src) vars))
               vars
               inner-vars
               inner-names))

       (record-case x
         ((<lexical-set> gensym)
          (make-binding-info vars (vhash-consq gensym #t refs)))
         ((<lambda-case> req opt inits rest kw gensyms)
          (let ((names `(,@req
                         ,@(or opt '())
                         ,@(if rest (list rest) '())
                         ,@(if kw (map cadr (cdr kw)) '()))))
            (make-binding-info (extend gensyms names) refs)))
         ((<let> gensyms names)
          (make-binding-info (extend gensyms names) refs))
         ((<letrec> gensyms names)
          (make-binding-info (extend gensyms names) refs))
         ((<fix> gensyms names)
          (make-binding-info (extend gensyms names) refs))
         (else info))))

   (lambda (x info env locs)
     ;; Leaving X's scope: shrink INFO's variable list
     ;; accordingly and reported unused nested variables.
     (let ((refs (binding-info-refs info))
           (vars (binding-info-vars info)))
       (define (shrink inner-vars refs)
         (vlist-for-each
          (lambda (var)
            (let ((gensym (car var)))
              ;; Don't report lambda parameters as unused.
              (if (and (memq gensym inner-vars)
                       (not (vhash-assq gensym refs))
                       (not (lambda-case? x)))
                  (let ((name (cadr var))
                        ;; We can get approximate source location by going up
                        ;; the LOCS location stack.
                        (loc  (or (caddr var)
                                  (find pair? locs))))
                    (if (and (not (gensym? name))
                             (not (eq? name '_)))
                        (warning 'unused-variable loc name))))))
          vars)
         (vlist-drop vars (length inner-vars)))

       ;; For simplicity, we leave REFS untouched, i.e., with
       ;; names of variables that are now going out of scope.
       ;; It doesn't hurt as these are unique names, it just
       ;; makes REFS unnecessarily fat.
       (record-case x
         ((<lambda-case> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         ((<let> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         ((<letrec> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         ((<fix> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         (else info))))

   (lambda (result env) #t)
   (make-binding-info vlist-null vlist-null)))


;;;
;;; Unused top-level variable analysis.
;;;

;; <reference-graph> record top-level definitions that are made, references to
;; top-level definitions and their context (the top-level definition in which
;; the reference appears), as well as the current context (the top-level
;; definition we're currently in).  The second part (`refs' below) is
;; effectively a graph from which we can determine unused top-level definitions.
(define-record-type <reference-graph>
  (make-reference-graph refs defs toplevel-context)
  reference-graph?
  (defs             reference-graph-defs) ;; ((NAME . LOC) ...)
  (refs             reference-graph-refs) ;; ((REF-CONTEXT REF ...) ...)
  (toplevel-context reference-graph-toplevel-context)) ;; NAME | #f

(define (graph-reachable-nodes root refs reachable)
  ;; Add to REACHABLE the nodes reachable from ROOT in graph REFS.  REFS is a
  ;; vhash mapping nodes to the list of their children: for instance,
  ;; ((A -> (B C)) (B -> (A)) (C -> ())) corresponds to
  ;;
  ;;  ,-------.
  ;;  v       |
  ;;  A ----> B
  ;;  |
  ;;  v
  ;;  C
  ;;
  ;; REACHABLE is a vhash of nodes known to be otherwise reachable.

  (let loop ((root   root)
             (path   vlist-null)
             (result reachable))
    (if (or (vhash-assq root path)
            (vhash-assq root result))
        result
        (let* ((children (or (and=> (vhash-assq root refs) cdr) '()))
               (path     (vhash-consq root #t path))
               (result   (fold (lambda (kid result)
                                 (loop kid path result))
                               result
                               children)))
          (fold (lambda (kid result)
                  (vhash-consq kid #t result))
                result
                children)))))

(define (graph-reachable-nodes* roots refs)
  ;; Return the list of nodes in REFS reachable from the nodes listed in ROOTS.
  (vlist-fold (lambda (root+true result)
                (let* ((root      (car root+true))
                       (reachable (graph-reachable-nodes root refs result)))
                  (vhash-consq root #t reachable)))
              vlist-null
              roots))

(define (partition* pred vhash)
  ;; Partition VHASH according to PRED.  Return the two resulting vhashes.
  (let ((result
         (vlist-fold (lambda (k+v result)
                       (let ((k  (car k+v))
                             (v  (cdr k+v))
                             (r1 (car result))
                             (r2 (cdr result)))
                         (if (pred k)
                             (cons (vhash-consq k v r1) r2)
                             (cons r1 (vhash-consq k v r2)))))
                     (cons vlist-null vlist-null)
                     vhash)))
    (values (car result) (cdr result))))

(define unused-toplevel-analysis
  ;; Report unused top-level definitions that are not exported.
  (let ((add-ref-from-context
         (lambda (graph name)
           ;; Add an edge CTX -> NAME in GRAPH.
           (let* ((refs     (reference-graph-refs graph))
                  (defs     (reference-graph-defs graph))
                  (ctx      (reference-graph-toplevel-context graph))
                  (ctx-refs (or (and=> (vhash-assq ctx refs) cdr) '())))
             (make-reference-graph (vhash-consq ctx (cons name ctx-refs) refs)
                                   defs ctx)))))
    (define (macro-variable? name env)
      (and (module? env)
           (let ((var (module-variable env name)))
             (and var (variable-bound? var)
                  (macro? (variable-ref var))))))

    (make-tree-analysis
     (lambda (x graph env locs)
       ;; X is a leaf.
       (let ((ctx (reference-graph-toplevel-context graph)))
         (record-case x
           ((<toplevel-ref> name src)
            (add-ref-from-context graph name))
           (else graph))))

     (lambda (x graph env locs)
       ;; Going down into X.
       (let ((ctx  (reference-graph-toplevel-context graph))
             (refs (reference-graph-refs graph))
             (defs (reference-graph-defs graph)))
         (record-case x
           ((<toplevel-define> name src)
            (let ((refs refs)
                  (defs (vhash-consq name (or src (find pair? locs))
                                     defs)))
              (make-reference-graph refs defs name)))
           ((<toplevel-set> name src)
            (add-ref-from-context graph name))
           (else graph))))

     (lambda (x graph env locs)
       ;; Leaving X's scope.
       (record-case x
         ((<toplevel-define>)
          (let ((refs (reference-graph-refs graph))
                (defs (reference-graph-defs graph)))
            (make-reference-graph refs defs #f)))
         (else graph)))

     (lambda (graph env)
       ;; Process the resulting reference graph: determine all private definitions
       ;; not reachable from any public definition.  Macros
       ;; (syntax-transformers), which are globally bound, never considered
       ;; unused since we can't tell whether a macro is actually used; in
       ;; addition, macros are considered roots of the graph since they may use
       ;; private bindings.  FIXME: The `make-syntax-transformer' calls don't
       ;; contain any literal `toplevel-ref' of the global bindings they use so
       ;; this strategy fails.
       (define (exported? name)
         (if (module? env)
             (module-variable (module-public-interface env) name)
             #t))

       (let-values (((public-defs private-defs)
                     (partition* (lambda (name)
                                   (or (exported? name)
                                       (macro-variable? name env)))
                                 (reference-graph-defs graph))))
         (let* ((roots     (vhash-consq #f #t public-defs))
                (refs      (reference-graph-refs graph))
                (reachable (graph-reachable-nodes* roots refs))
                (unused    (vlist-filter (lambda (name+src)
                                           (not (vhash-assq (car name+src)
                                                            reachable)))
                                         private-defs)))
           (vlist-for-each (lambda (name+loc)
                             (let ((name (car name+loc))
                                   (loc  (cdr name+loc)))
                               (if (not (gensym? name))
                                   (warning 'unused-toplevel loc name))))
                           unused))))

     (make-reference-graph vlist-null vlist-null #f))))


;;;
;;; Unbound variable analysis.
;;;

;; <toplevel-info> records are used during tree traversal in search of
;; possibly unbound variable.  They contain a list of references to
;; potentially unbound top-level variables, and a list of the top-level
;; defines that have been encountered.
(define-record-type <toplevel-info>
  (make-toplevel-info refs defs)
  toplevel-info?
  (refs  toplevel-info-refs)  ;; ((VARIABLE-NAME . LOCATION) ...)
  (defs  toplevel-info-defs)) ;; (VARIABLE-NAME ...)

(define (goops-toplevel-definition proc args env)
  ;; If application of PROC to ARGS is a GOOPS top-level definition, return
  ;; the name of the variable being defined; otherwise return #f.  This
  ;; assumes knowledge of the current implementation of `define-class' et al.
  (define (toplevel-define-arg args)
    (match args
      ((($ <const> _ (and (? symbol?) exp)) _)
       exp)
      (_ #f)))

  (match proc
    (($ <module-ref> _ '(oop goops) 'toplevel-define! #f)
     (toplevel-define-arg args))
    (($ <toplevel-ref> _ 'toplevel-define!)
     ;; This may be the result of expanding one of the GOOPS macros within
     ;; `oop/goops.scm'.
     (and (eq? env (resolve-module '(oop goops)))
          (toplevel-define-arg args)))
    (_ #f)))

(define unbound-variable-analysis
  ;; Report possibly unbound variables in the given tree.
  (make-tree-analysis
   (lambda (x info env locs)
     ;; X is a leaf: extend INFO's refs accordingly.
     (let ((refs (toplevel-info-refs info))
           (defs (toplevel-info-defs info)))
       (define (bound? name)
         (or (and (module? env)
                  (module-variable env name))
             (vhash-assq name defs)))

       (record-case x
         ((<toplevel-ref> name src)
          (if (bound? name)
              info
              (let ((src (or src (find pair? locs))))
                (make-toplevel-info (vhash-consq name src refs)
                                    defs))))
         (else info))))

   (lambda (x info env locs)
     ;; Going down into X.
     (let* ((refs (toplevel-info-refs info))
            (defs (toplevel-info-defs info))
            (src  (tree-il-src x)))
       (define (bound? name)
         (or (and (module? env)
                  (module-variable env name))
             (vhash-assq name defs)))

       (record-case x
         ((<toplevel-set> name src)
          (if (bound? name)
              (make-toplevel-info refs defs)
              (let ((src (find pair? locs)))
                (make-toplevel-info (vhash-consq name src refs)
                                    defs))))
         ((<toplevel-define> name)
          (make-toplevel-info (vhash-delq name refs)
                              (vhash-consq name #t defs)))

         ((<application> proc args)
          ;; Check for a dynamic top-level definition, as is
          ;; done by code expanded from GOOPS macros.
          (let ((name (goops-toplevel-definition proc args
                                                 env)))
            (if (symbol? name)
                (make-toplevel-info (vhash-delq name refs)
                                    (vhash-consq name #t defs))
                (make-toplevel-info refs defs))))
         (else
          (make-toplevel-info refs defs)))))

   (lambda (x info env locs)
     ;; Leaving X's scope.
     info)

   (lambda (toplevel env)
     ;; Post-process the result.
     (vlist-for-each (lambda (name+loc)
                       (let ((name (car name+loc))
                             (loc  (cdr name+loc)))
                         (warning 'unbound-variable loc name)))
                     (vlist-reverse (toplevel-info-refs toplevel))))

   (make-toplevel-info vlist-null vlist-null)))


;;;
;;; Arity analysis.
;;;

;; <arity-info> records contain information about lexical definitions of
;; procedures currently in scope, top-level procedure definitions that have
;; been encountered, and calls to top-level procedures that have been
;; encountered.
(define-record-type <arity-info>
  (make-arity-info toplevel-calls lexical-lambdas toplevel-lambdas)
  arity-info?
  (toplevel-calls   toplevel-procedure-calls) ;; ((NAME . APPLICATION) ...)
  (lexical-lambdas  lexical-lambdas)          ;; ((GENSYM . DEFINITION) ...)
  (toplevel-lambdas toplevel-lambdas))        ;; ((NAME . DEFINITION) ...)

(define (validate-arity proc application lexical?)
  ;; Validate the argument count of APPLICATION, a tree-il application of
  ;; PROC, emitting a warning in case of argument count mismatch.

  (define (filter-keyword-args keywords allow-other-keys? args)
    ;; Filter keyword arguments from ARGS and return the resulting list.
    ;; KEYWORDS is the list of allowed keywords, and ALLOW-OTHER-KEYS?
    ;; specified whethere keywords not listed in KEYWORDS are allowed.
    (let loop ((args   args)
               (result '()))
      (if (null? args)
          (reverse result)
          (let ((arg (car args)))
            (if (and (const? arg)
                     (or (memq (const-exp arg) keywords)
                         (and allow-other-keys?
                              (keyword? (const-exp arg)))))
                (loop (if (pair? (cdr args))
                          (cddr args)
                          '())
                      result)
                (loop (cdr args)
                      (cons arg result)))))))

  (define (arities proc)
    ;; Return the arities of PROC, which can be either a tree-il or a
    ;; procedure.
    (define (len x)
      (or (and (or (null? x) (pair? x))
               (length x))
          0))
    (cond ((program? proc)
           (values (procedure-name proc)
                   (map (lambda (a)
                          (list (arity:nreq a) (arity:nopt a) (arity:rest? a)
                                (map car (arity:kw a))
                                (arity:allow-other-keys? a)))
                        (program-arities proc))))
          ((procedure? proc)
           (if (struct? proc)
               ;; An applicable struct.
               (arities (struct-ref proc 0))
               ;; An applicable smob.
               (let ((arity (procedure-minimum-arity proc)))
                 (values (procedure-name proc)
                         (list (list (car arity) (cadr arity) (caddr arity)
                                     #f #f))))))
          (else
           (let loop ((name    #f)
                      (proc    proc)
                      (arities '()))
             (if (not proc)
                 (values name (reverse arities))
                 (record-case proc
                   ((<lambda-case> req opt rest kw alternate)
                    (loop name alternate
                          (cons (list (len req) (len opt) rest
                                      (and (pair? kw) (map car (cdr kw)))
                                      (and (pair? kw) (car kw)))
                                arities)))
                   ((<lambda> meta body)
                    (loop (assoc-ref meta 'name) body arities))
                   (else
                    (values #f #f))))))))

  (let ((args (application-args application))
        (src  (tree-il-src application)))
    (call-with-values (lambda () (arities proc))
      (lambda (name arities)
        (define matches?
          (find (lambda (arity)
                  (pmatch arity
                    ((,req ,opt ,rest? ,kw ,aok?)
                     (let ((args (if (pair? kw)
                                     (filter-keyword-args kw aok? args)
                                     args)))
                       (if (and req opt)
                           (let ((count (length args)))
                             (and (>= count req)
                                  (or rest?
                                      (<= count (+ req opt)))))
                           #t)))
                    (else #t)))
                arities))

        (if (not matches?)
            (warning 'arity-mismatch src
                     (or name (with-output-to-string (lambda () (write proc))))
                     lexical?)))))
  #t)

(define arity-analysis
  ;; Report arity mismatches in the given tree.
  (make-tree-analysis
   (lambda (x info env locs)
     ;; X is a leaf.
     info)
   (lambda (x info env locs)
     ;; Down into X.
     (define (extend lexical-name val info)
       ;; If VAL is a lambda, add NAME to the lexical-lambdas of INFO.
       (let ((toplevel-calls   (toplevel-procedure-calls info))
             (lexical-lambdas  (lexical-lambdas info))
             (toplevel-lambdas (toplevel-lambdas info)))
         (record-case val
           ((<lambda> body)
            (make-arity-info toplevel-calls
                             (vhash-consq lexical-name val
                                          lexical-lambdas)
                             toplevel-lambdas))
           ((<lexical-ref> gensym)
            ;; lexical alias
            (let ((val* (vhash-assq gensym lexical-lambdas)))
              (if (pair? val*)
                  (extend lexical-name (cdr val*) info)
                  info)))
           ((<toplevel-ref> name)
            ;; top-level alias
            (make-arity-info toplevel-calls
                             (vhash-consq lexical-name val
                                          lexical-lambdas)
                             toplevel-lambdas))
           (else info))))

     (let ((toplevel-calls   (toplevel-procedure-calls info))
           (lexical-lambdas  (lexical-lambdas info))
           (toplevel-lambdas (toplevel-lambdas info)))

       (record-case x
         ((<toplevel-define> name exp)
          (record-case exp
            ((<lambda> body)
             (make-arity-info toplevel-calls
                              lexical-lambdas
                              (vhash-consq name exp toplevel-lambdas)))
            ((<toplevel-ref> name)
             ;; alias for another toplevel
             (let ((proc (vhash-assq name toplevel-lambdas)))
               (make-arity-info toplevel-calls
                                lexical-lambdas
                                (vhash-consq (toplevel-define-name x)
                                             (if (pair? proc)
                                                 (cdr proc)
                                                 exp)
                                             toplevel-lambdas))))
            (else info)))
         ((<let> gensyms vals)
          (fold extend info gensyms vals))
         ((<letrec> gensyms vals)
          (fold extend info gensyms vals))
         ((<fix> gensyms vals)
          (fold extend info gensyms vals))

         ((<application> proc args src)
          (record-case proc
            ((<lambda> body)
             (validate-arity proc x #t)
             info)
            ((<toplevel-ref> name)
             (make-arity-info (vhash-consq name x toplevel-calls)
                              lexical-lambdas
                              toplevel-lambdas))
            ((<lexical-ref> gensym)
             (let ((proc (vhash-assq gensym lexical-lambdas)))
               (if (pair? proc)
                   (record-case (cdr proc)
                     ((<toplevel-ref> name)
                      ;; alias to toplevel
                      (make-arity-info (vhash-consq name x toplevel-calls)
                                       lexical-lambdas
                                       toplevel-lambdas))
                     (else
                      (validate-arity (cdr proc) x #t)
                      info))

                   ;; If GENSYM wasn't found, it may be because it's an
                   ;; argument of the procedure being compiled.
                   info)))
            (else info)))
         (else info))))

   (lambda (x info env locs)
     ;; Up from X.
     (define (shrink name val info)
       ;; Remove NAME from the lexical-lambdas of INFO.
       (let ((toplevel-calls   (toplevel-procedure-calls info))
             (lexical-lambdas  (lexical-lambdas info))
             (toplevel-lambdas (toplevel-lambdas info)))
         (make-arity-info toplevel-calls
                          (if (vhash-assq name lexical-lambdas)
                              (vlist-tail lexical-lambdas)
                              lexical-lambdas)
                          toplevel-lambdas)))

     (let ((toplevel-calls   (toplevel-procedure-calls info))
           (lexical-lambdas  (lexical-lambdas info))
           (toplevel-lambdas (toplevel-lambdas info)))
       (record-case x
         ((<let> gensyms vals)
          (fold shrink info gensyms vals))
         ((<letrec> gensyms vals)
          (fold shrink info gensyms vals))
         ((<fix> gensyms vals)
          (fold shrink info gensyms vals))

         (else info))))

   (lambda (result env)
     ;; Post-processing: check all top-level procedure calls that have been
     ;; encountered.
     (let ((toplevel-calls   (toplevel-procedure-calls result))
           (toplevel-lambdas (toplevel-lambdas result)))
       (vlist-for-each
        (lambda (name+application)
          (let* ((name        (car name+application))
                 (application (cdr name+application))
                 (proc
                  (or (and=> (vhash-assq name toplevel-lambdas) cdr)
                      (and (module? env)
                           (false-if-exception
                            (module-ref env name)))))
                 (proc*
                  ;; handle toplevel aliases
                  (if (toplevel-ref? proc)
                      (let ((name (toplevel-ref-name proc)))
                        (and (module? env)
                             (false-if-exception
                              (module-ref env name))))
                      proc)))
            (cond ((lambda? proc*)
                   (validate-arity proc* application #t))
                  ((procedure? proc*)
                   (validate-arity proc* application #f)))))
        toplevel-calls)))

   (make-arity-info vlist-null vlist-null vlist-null)))


;;;
;;; `format' argument analysis.
;;;

(define &syntax-error
  ;; The `throw' key for syntax errors.
  (gensym "format-string-syntax-error"))

(define (format-string-argument-count fmt)
  ;; Return the minimum and maxium number of arguments that should
  ;; follow format string FMT (or, ahem, a good estimate thereof) or
  ;; `any' if the format string can be followed by any number of
  ;; arguments.

  (define (drop-group chars end)
    ;; Drop characters from CHARS until "~END" is encountered.
    (let loop ((chars  chars)
               (tilde? #f))
      (if (null? chars)
          (throw &syntax-error 'unterminated-iteration)
          (if tilde?
              (if (eq? (car chars) end)
                  (cdr chars)
                  (loop (cdr chars) #f))
              (if (eq? (car chars) #\~)
                  (loop (cdr chars) #t)
                  (loop (cdr chars) #f))))))

  (define (digit? char)
    ;; Return true if CHAR is a digit, #f otherwise.
    (memq char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

  (define (previous-number chars)
    ;; Return the previous series of digits found in CHARS.
    (let ((numbers (take-while digit? chars)))
      (and (not (null? numbers))
           (string->number (list->string (reverse numbers))))))

  (let loop ((chars       (string->list fmt))
             (state       'literal)
             (params      '())
             (conditions  '())
             (end-group   #f)
             (min-count 0)
             (max-count 0))
    (if (null? chars)
        (if end-group
            (throw &syntax-error 'unterminated-conditional)
            (values min-count max-count))
        (case state
          ((tilde)
           (case (car chars)
             ((#\~ #\% #\& #\t #\T #\_ #\newline #\( #\) #\! #\| #\/ #\q #\Q)
                        (loop (cdr chars) 'literal '()
                              conditions end-group
                              min-count max-count))
             ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\, #\: #\@ #\+ #\- #\#)
                        (loop (cdr chars)
                              'tilde (cons (car chars) params)
                              conditions end-group
                              min-count max-count))
             ((#\v #\V) (loop (cdr chars)
                              'tilde (cons (car chars) params)
                              conditions end-group
                              (+ 1 min-count)
                              (+ 1 max-count)))
             ((#\p #\P) (let* ((colon?    (memq #\: params))
                               (min-count (if colon?
                                              (max 1 min-count)
                                              (+ 1 min-count))))
                          (loop (cdr chars) 'literal '()
                                conditions end-group
                                min-count
                                (if colon?
                                    (max max-count min-count)
                                    (+ 1 max-count)))))
             ((#\[)
              (loop chars 'literal '() '()
                    (let ((selector (previous-number params))
                          (at?      (memq #\@ params)))
                      (lambda (chars conds)
                        ;; end of group
                        (let ((mins (map car conds))
                              (maxs (map cdr conds))
                              (sel? (and selector
                                         (< selector (length conds)))))
                          (if (and (every number? mins)
                                   (every number? maxs))
                              (loop chars 'literal '() conditions end-group
                                    (+ min-count
                                       (if sel?
                                           (car (list-ref conds selector))
                                           (+ (if at? 0 1)
                                              (if (null? mins)
                                                  0
                                                  (apply min mins)))))
                                    (+ max-count
                                       (if sel?
                                           (cdr (list-ref conds selector))
                                           (+ (if at? 0 1)
                                              (if (null? maxs)
                                                  0
                                                  (apply max maxs))))))
                              (values 'any 'any))))) ;; XXX: approximation
                    0 0))
             ((#\;)
              (if end-group
                  (loop (cdr chars) 'literal '()
                        (cons (cons min-count max-count) conditions)
                        end-group
                        0 0)
                  (throw &syntax-error 'unexpected-semicolon)))
             ((#\])
              (if end-group
                  (end-group (cdr chars)
                             (reverse (cons (cons min-count max-count)
                                            conditions)))
                  (throw &syntax-error 'unexpected-conditional-termination)))
             ((#\{)     (if (memq #\@ params)
                            (values min-count 'any)
                            (loop (drop-group (cdr chars) #\})
                                  'literal '()
                                  conditions end-group
                                  (+ 1 min-count) (+ 1 max-count))))
             ((#\*)     (if (memq #\@ params)
                            (values 'any 'any) ;; it's unclear what to do here
                            (loop (cdr chars)
                                  'literal '()
                                  conditions end-group
                                  (+ (or (previous-number params) 1)
                                     min-count)
                                  (+ (or (previous-number params) 1)
                                     max-count))))
             ((#\? #\k #\K)
              ;; We don't have enough info to determine the exact number
              ;; of args, but we could determine a lower bound (TODO).
              (values 'any 'any))
             ((#\^)
              (values min-count 'any))
             ((#\h #\H)
                        (let ((argc (if (memq #\: params) 2 1)))
                          (loop (cdr chars) 'literal '()
                                conditions end-group
                                (+ argc min-count)
                                (+ argc max-count))))
             ((#\')
              (if (null? (cdr chars))
                  (throw &syntax-error 'unexpected-termination)
                  (loop (cddr chars) 'tilde (cons (cadr chars) params)
                        conditions end-group min-count max-count)))
             (else      (loop (cdr chars) 'literal '()
                              conditions end-group
                              (+ 1 min-count) (+ 1 max-count)))))
          ((literal)
           (case (car chars)
             ((#\~)     (loop (cdr chars) 'tilde '()
                              conditions end-group
                              min-count max-count))
             (else      (loop (cdr chars) 'literal '()
                              conditions end-group
                              min-count max-count))))
          (else (error "computer bought the farm" state))))))

(define (proc-ref? exp proc special-name env)
  "Return #t when EXP designates procedure PROC in ENV.  As a last
resort, return #t when EXP refers to the global variable SPECIAL-NAME."

  (define special?
    (cut eq? <> special-name))

  (match exp
    (($ <toplevel-ref> _ (? special?))
     ;; Allow top-levels like: (define _ (cut gettext <> "my-domain")).
     #t)
    (($ <toplevel-ref> _ name)
     (let ((var (module-variable env name)))
       (and var (variable-bound? var)
            (eq? (variable-ref var) proc))))
    (($ <module-ref> _ _ (? special?))
     #t)
    (($ <module-ref> _ module name public?)
     (let* ((mod (if public?
                     (false-if-exception (resolve-interface module))
                     (resolve-module module #:ensure #f)))
            (var (and mod (module-variable mod name))))
       (and var (variable-bound? var) (eq? (variable-ref var) proc))))
    (($ <lexical-ref> _ (? special?))
     #t)
    (_ #f)))

(define gettext? (cut proc-ref? <> gettext '_ <>))
(define ngettext? (cut proc-ref? <> ngettext 'N_ <>))

(define (const-fmt x env)
  ;; Return the literal format string for X, or #f.
  (match x
    (($ <const> _ (? string? exp))
     exp)
    (($ <application> _ (? (cut gettext? <> env))
        (($ <const> _ (? string? fmt))))
     ;; Gettexted literals, like `(_ "foo")'.
     fmt)
    (($ <application> _ (? (cut ngettext? <> env))
        (($ <const> _ (? string? fmt)) ($ <const> _ (? string?)) _ ..1))
     ;; Plural gettextized literals, like `(N_ "singular" "plural" n)'.

     ;; TODO: Check whether the singular and plural strings have the
     ;; same format escapes.
     fmt)
    (_ #f)))

(define format-analysis
  ;; Report arity mismatches in the given tree.
  (make-tree-analysis
   (lambda (x _ env locs)
     ;; X is a leaf.
     #t)

   (lambda (x _ env locs)
     ;; Down into X.
     (define (check-format-args args loc)
       (pmatch args
         ((,port ,fmt . ,rest)
          (guard (const-fmt fmt env))
          (if (and (const? port)
                   (not (boolean? (const-exp port))))
              (warning 'format loc 'wrong-port (const-exp port)))
          (let ((fmt   (const-fmt fmt env))
                (count (length rest)))
            (catch &syntax-error
              (lambda ()
                (let-values (((min max)
                              (format-string-argument-count fmt)))
                  (and min max
                       (or (and (or (eq? min 'any) (>= count min))
                                (or (eq? max 'any) (<= count max)))
                           (warning 'format loc 'wrong-format-arg-count
                                    fmt min max count)))))
              (lambda (_ key)
                (warning 'format loc 'syntax-error key fmt)))))
         ((,port ,fmt . ,rest)
          (if (and (const? port)
                   (not (boolean? (const-exp port))))
              (warning 'format loc 'wrong-port (const-exp port)))

          (match fmt
            (($ <const> loc* (? (negate string?) fmt))
             (warning 'format (or loc* loc) 'wrong-format-string fmt))

            ;; Warn on non-literal format strings, unless they refer to
            ;; a lexical variable named "fmt".
            (($ <lexical-ref> _ fmt)
             #t)
            ((? (negate const?))
             (warning 'format loc 'non-literal-format-string))))
         (else
          (warning 'format loc 'wrong-num-args (length args)))))

     (define (check-simple-format-args args loc)
       ;; Check the arguments to the `simple-format' procedure, which is
       ;; less capable than that of (ice-9 format).

       (define allowed-chars
         '(#\A #\S #\a #\s #\~ #\%))

       (define (format-chars fmt)
         (let loop ((chars  (string->list fmt))
                    (result '()))
           (match chars
             (()
              (reverse result))
             ((#\~ opt rest ...)
              (loop rest (cons opt result)))
             ((_ rest ...)
              (loop rest result)))))

       (match args
         ((port ($ <const> _ (? string? fmt)) _ ...)
          (let ((opts (format-chars fmt)))
            (or (every (cut memq <> allowed-chars) opts)
                (begin
                  (warning 'format loc 'simple-format fmt
                           (find (negate (cut memq <> allowed-chars)) opts))
                  #f))))
         ((port (= (cut const-fmt <> env) (? string? fmt)) args ...)
          (check-simple-format-args `(,port ,(make-const loc fmt) ,args) loc))
         (_ #t)))

     (define (resolve-toplevel name)
       (and (module? env)
            (false-if-exception (module-ref env name))))

     (match x
       (($ <application> src ($ <toplevel-ref> _ name) args)
        (let ((proc (resolve-toplevel name)))
          (if (or (and (eq? proc (@ (guile) simple-format))
                       (check-simple-format-args args
                                                 (or src (find pair? locs))))
                  (eq? proc (@ (ice-9 format) format)))
              (check-format-args args (or src (find pair? locs))))))
       (($ <application> src ($ <module-ref> _ '(ice-9 format) 'format) args)
        (check-format-args args (or src (find pair? locs))))
       (($ <application> src ($ <module-ref> _ '(guile)
                                (or 'format 'simple-format))
           args)
        (and (check-simple-format-args args
                                       (or src (find pair? locs)))
             (check-format-args args (or src (find pair? locs)))))
       (_ #t))
     #t)

   (lambda (x _ env locs)
     ;; Up from X.
     #t)

   (lambda (_ env)
     ;; Post-processing.
     #t)

   #t))
