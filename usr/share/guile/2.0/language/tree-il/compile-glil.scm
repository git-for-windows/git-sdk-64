;;; TREE-IL -> GLIL compiler

;; Copyright (C) 2001,2008,2009,2010,2011,2012,2013,2014 Free Software Foundation, Inc.

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

(define-module (language tree-il compile-glil)
  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:use-module (system base message)
  #:use-module (ice-9 receive)
  #:use-module (language glil)
  #:use-module (system vm instruction)
  #:use-module (language tree-il)
  #:use-module (language tree-il optimize)
  #:use-module (language tree-il canonicalize)
  #:use-module (language tree-il analyze)
  #:use-module ((srfi srfi-1) #:select (filter-map))
  #:export (compile-glil))

;; allocation:
;;  sym -> {lambda -> address}
;;  lambda -> (labels . free-locs)
;;  lambda-case -> (gensym . nlocs)
;;
;; address ::= (local? boxed? . index)
;; labels ::= ((sym . lambda) ...)
;; free-locs ::= ((sym0 . address0) (sym1 . address1) ...)
;; free variable addresses are relative to parent proc.

(define *comp-module* (make-fluid))

(define %warning-passes
  `((unused-variable     . ,unused-variable-analysis)
    (unused-toplevel     . ,unused-toplevel-analysis)
    (unbound-variable    . ,unbound-variable-analysis)
    (arity-mismatch      . ,arity-analysis)
    (format              . ,format-analysis)))

(define (compile-glil x e opts)
  (define warnings
    (or (and=> (memq #:warnings opts) cadr)
        '()))

  ;; Go through the warning passes.
  (let ((analyses (filter-map (lambda (kind)
                                (assoc-ref %warning-passes kind))
                              warnings)))
    (analyze-tree analyses x e))

  (let* ((x (make-lambda (tree-il-src x) '()
                         (make-lambda-case #f '() #f #f #f '() '() x #f)))
         (x (optimize! x e opts))
         (x (canonicalize! x))
         (allocation (analyze-lexicals x)))

    (with-fluids ((*comp-module* e))
      (values (flatten-lambda x #f allocation)
              e
              e))))



(define *primcall-ops* (make-hash-table))
(for-each
 (lambda (x) (hash-set! *primcall-ops* (car x) (cdr x)))
 '(((eq? . 2) . eq?)
   ((eqv? . 2) . eqv?)
   ((equal? . 2) . equal?)
   ((= . 2) . ee?)
   ((< . 2) . lt?)
   ((> . 2) . gt?)
   ((<= . 2) . le?)
   ((>= . 2) . ge?)
   ((+ . 2) . add)
   ((- . 2) . sub)
   ((1+ . 1) . add1)
   ((1- . 1) . sub1)
   ((* . 2) . mul)
   ((/ . 2) . div)
   ((quotient . 2) . quo)
   ((remainder . 2) . rem)
   ((modulo . 2) . mod)
   ((ash . 2) . ash)
   ((logand . 2) . logand)
   ((logior . 2) . logior)
   ((logxor . 2) . logxor)
   ((not . 1) . not)
   ((pair? . 1) . pair?)
   ((cons . 2) . cons)
   ((car . 1) . car)
   ((cdr . 1) . cdr)
   ((set-car! . 2) . set-car!)
   ((set-cdr! . 2) . set-cdr!)
   ((null? . 1) . null?)
   ((list? . 1) . list?)
   ((symbol? . 1) . symbol?)
   ((vector? . 1) . vector?)
   (list . list)
   (vector . vector)
   ((class-of . 1) . class-of)
   ((vector-ref . 2) . vector-ref)
   ((vector-set! . 3) . vector-set)
   ((variable-ref . 1) . variable-ref)
   ;; nb, *not* variable-set! -- the args are switched
   ((variable-bound? . 1) . variable-bound?)
   ((struct? . 1) . struct?)
   ((struct-vtable . 1) . struct-vtable)
   ((struct-ref . 2) . struct-ref)
   ((struct-set! . 3) . struct-set)
   (make-struct/no-tail . make-struct)

   ;; hack for javascript
   ((return . 1) . return)
   ;; hack for lua
   (return/values . return/values)

   ((bytevector-u8-ref . 2) . bv-u8-ref)
   ((bytevector-u8-set! . 3) . bv-u8-set)
   ((bytevector-s8-ref . 2) . bv-s8-ref)
   ((bytevector-s8-set! . 3) . bv-s8-set)

   ((bytevector-u16-ref . 3) . bv-u16-ref)
   ((bytevector-u16-set! . 4) . bv-u16-set)
   ((bytevector-u16-native-ref . 2) . bv-u16-native-ref)
   ((bytevector-u16-native-set! . 3) . bv-u16-native-set)
   ((bytevector-s16-ref . 3) . bv-s16-ref)
   ((bytevector-s16-set! . 4) . bv-s16-set)
   ((bytevector-s16-native-ref . 2) . bv-s16-native-ref)
   ((bytevector-s16-native-set! . 3) . bv-s16-native-set)
    
   ((bytevector-u32-ref . 3) . bv-u32-ref)
   ((bytevector-u32-set! . 4) . bv-u32-set)
   ((bytevector-u32-native-ref . 2) . bv-u32-native-ref)
   ((bytevector-u32-native-set! . 3) . bv-u32-native-set)
   ((bytevector-s32-ref . 3) . bv-s32-ref)
   ((bytevector-s32-set! . 4) . bv-s32-set)
   ((bytevector-s32-native-ref . 2) . bv-s32-native-ref)
   ((bytevector-s32-native-set! . 3) . bv-s32-native-set)
    
   ((bytevector-u64-ref . 3) . bv-u64-ref)
   ((bytevector-u64-set! . 4) . bv-u64-set)
   ((bytevector-u64-native-ref . 2) . bv-u64-native-ref)
   ((bytevector-u64-native-set! . 3) . bv-u64-native-set)
   ((bytevector-s64-ref . 3) . bv-s64-ref)
   ((bytevector-s64-set! . 4) . bv-s64-set)
   ((bytevector-s64-native-ref . 2) . bv-s64-native-ref)
   ((bytevector-s64-native-set! . 3) . bv-s64-native-set)
    
   ((bytevector-ieee-single-ref . 3) . bv-f32-ref)
   ((bytevector-ieee-single-set! . 4) . bv-f32-set)
   ((bytevector-ieee-single-native-ref . 2) . bv-f32-native-ref)
   ((bytevector-ieee-single-native-set! . 3) . bv-f32-native-set)
   ((bytevector-ieee-double-ref . 3) . bv-f64-ref)
   ((bytevector-ieee-double-set! . 4) . bv-f64-set)
   ((bytevector-ieee-double-native-ref . 2) . bv-f64-native-ref)
   ((bytevector-ieee-double-native-set! . 3) . bv-f64-native-set)))




(define (make-label) (gensym ":L"))

(define (vars->bind-list ids vars allocation proc)
  (map (lambda (id v)
         (pmatch (hashq-ref (hashq-ref allocation v) proc)
           ((#t ,boxed? . ,n)
            (list id boxed? n))
           (,x (error "bad var list element" id v x))))
       ids
       vars))

(define (emit-bindings src ids vars allocation proc emit-code)
  (emit-code src (make-glil-bind
                  (vars->bind-list ids vars allocation proc))))

(define (with-output-to-code proc)
  (let ((out '()))
    (define (emit-code src x)
      (set! out (cons x out))
      (if src
          (set! out (cons (make-glil-source src) out))))
    (proc emit-code)
    (reverse out)))

(define (flatten-lambda x self-label allocation)
  (record-case x
    ((<lambda> src meta body)
     (make-glil-program
      meta
      (with-output-to-code
       (lambda (emit-code)
         ;; write source info for proc
         (if src (emit-code #f (make-glil-source src)))
         ;; compile the body, yo
         (flatten-lambda-case body allocation x self-label
                              (car (hashq-ref allocation x))
                              emit-code)))))))

(define (flatten-lambda-case lcase allocation self self-label fix-labels
                             emit-code)
  (define (emit-label label)
    (emit-code #f (make-glil-label label)))
  (define (emit-branch src inst label)
    (emit-code src (make-glil-branch inst label)))

  ;; RA: "return address"; #f unless we're in a non-tail fix with labels
  ;; MVRA: "multiple-values return address"; #f unless we're in a let-values
  (let comp ((x lcase) (context 'tail) (RA #f) (MVRA #f))
    (define (comp-tail tree) (comp tree context RA MVRA))
    (define (comp-push tree) (comp tree 'push #f #f))
    (define (comp-drop tree) (comp tree 'drop #f #f))
    (define (comp-vals tree MVRA) (comp tree 'vals #f MVRA))
    (define (comp-fix tree RA) (comp tree context RA MVRA))

    ;; A couple of helpers. Note that if we are in tail context, we
    ;; won't have an RA.
    (define (maybe-emit-return)
      (if RA
          (emit-branch #f 'br RA)
          (if (eq? context 'tail)
              (emit-code #f (make-glil-call 'return 1)))))
    
    ;; After lexical binding forms in non-tail context, call this
    ;; function to clear stack slots, allowing their previous values to
    ;; be collected.
    (define (clear-stack-slots context syms)
      (case context
        ((push drop)
         (for-each (lambda (v)
                     (and=>
                      ;; Can be #f if the var is labels-allocated.
                      (hashq-ref allocation v)
                      (lambda (h)
                        (pmatch (hashq-ref h self)
                          ((#t _ . ,n)
                           (emit-code #f (make-glil-void))
                           (emit-code #f (make-glil-lexical #t #f 'set n)))
                          (,loc (error "bad let var allocation" x loc))))))
                   syms))))

    (record-case x
      ((<void>)
       (case context
         ((push vals tail)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))

      ((<const> src exp)
       (case context
         ((push vals tail)
          (emit-code src (make-glil-const exp))))
       (maybe-emit-return))

      ;; FIXME: should represent sequence as exps tail
      ((<sequence> exps)
       (let lp ((exps exps))
         (if (null? (cdr exps))
             (comp-tail (car exps))
             (begin
               (comp-drop (car exps))
               (lp (cdr exps))))))

      ((<application> src proc args)
       ;; FIXME: need a better pattern-matcher here
       (cond
        ((and (primitive-ref? proc)
              (eq? (primitive-ref-name proc) '@apply)
              (>= (length args) 1))
         (let ((proc (car args))
               (args (cdr args)))
           (cond
            ((and (primitive-ref? proc) (eq? (primitive-ref-name proc) 'values)
                  (not (eq? context 'push)) (not (eq? context 'vals)))
             ;; tail: (lambda () (apply values '(1 2)))
             ;; drop: (lambda () (apply values '(1 2)) 3)
             ;; push: (lambda () (list (apply values '(10 12)) 1))
             (case context
               ((drop) (for-each comp-drop args) (maybe-emit-return))
               ((tail)
                (for-each comp-push args)
                (emit-code src (make-glil-call 'return/values* (length args))))))

            (else
             (case context
               ((tail)
                (comp-push proc)
                (for-each comp-push args)
                (emit-code src (make-glil-call 'tail-apply (1+ (length args)))))
               ((push)
                (emit-code src (make-glil-call 'new-frame 0))
                (comp-push proc)
                (for-each comp-push args)
                (emit-code src (make-glil-call 'apply (1+ (length args))))
                (maybe-emit-return))
               ((vals)
                (comp-vals
                 (make-application src (make-primitive-ref #f 'apply)
                                   (cons proc args))
                 MVRA)
                (maybe-emit-return))
               ((drop)
                ;; Well, shit. The proc might return any number of
                ;; values (including 0), since it's in a drop context,
                ;; yet apply does not create a MV continuation. So we
                ;; mv-call out to our trampoline instead.
                (comp-drop
                 (make-application src (make-primitive-ref #f 'apply)
                                   (cons proc args)))
                (maybe-emit-return)))))))
        
        ((and (primitive-ref? proc) (eq? (primitive-ref-name proc) 'values))
         ;; tail: (lambda () (values '(1 2)))
         ;; drop: (lambda () (values '(1 2)) 3)
         ;; push: (lambda () (list (values '(10 12)) 1))
         ;; vals: (let-values (((a b ...) (values 1 2 ...))) ...)
         (case context
           ((drop) (for-each comp-drop args) (maybe-emit-return))
           ((push)
            (case (length args)
              ((0)
               ;; FIXME: This is surely an error.  We need to add a
               ;; values-mismatch warning pass.
               (emit-code src (make-glil-call 'new-frame 0))
               (comp-push proc)
               (emit-code src (make-glil-call 'call 0))
               (maybe-emit-return))
              (else
               ;; Taking advantage of unspecified order of evaluation of
               ;; arguments.
               (for-each comp-drop (cdr args))
               (comp-push (car args))
               (maybe-emit-return))))
           ((vals)
            (for-each comp-push args)
            (emit-code #f (make-glil-const (length args)))
            (emit-branch src 'br MVRA))
           ((tail)
            (for-each comp-push args)
            (emit-code src (let ((len (length args)))
                             (if (= len 1)
                                 (make-glil-call 'return 1)
                                 (make-glil-call 'return/values len)))))))
        
        ((and (primitive-ref? proc)
              (eq? (primitive-ref-name proc) '@call-with-values)
              (= (length args) 2))
	 ;; CONSUMER
         ;; PRODUCER
         ;; (mv-call MV)
         ;; ([tail]-call 1)
         ;; goto POST
         ;; MV: [tail-]call/nargs
         ;; POST: (maybe-drop)
         (case context
           ((vals)
            ;; Fall back.
            (comp-vals
             (make-application src (make-primitive-ref #f 'call-with-values)
                               args)
             MVRA)
            (maybe-emit-return))
           (else
            (let ((MV (make-label)) (POST (make-label))
                  (producer (car args)) (consumer (cadr args)))
              (if (not (eq? context 'tail))
                  (emit-code src (make-glil-call 'new-frame 0)))
              (comp-push consumer)
              (emit-code src (make-glil-call 'new-frame 0))
              (comp-push producer)
              (emit-code src (make-glil-mv-call 0 MV))
              (case context
                ((tail) (emit-code src (make-glil-call 'tail-call 1)))
                (else   (emit-code src (make-glil-call 'call 1))
                        (emit-branch #f 'br POST)))
              (emit-label MV)
              (case context
                ((tail) (emit-code src (make-glil-call 'tail-call/nargs 0)))
                (else   (emit-code src (make-glil-call 'call/nargs 0))
                        (emit-label POST)
                        (if (eq? context 'drop)
                            (emit-code #f (make-glil-call 'drop 1)))
                        (maybe-emit-return)))))))

        ((and (primitive-ref? proc)
              (eq? (primitive-ref-name proc) '@call-with-current-continuation)
              (= (length args) 1))
         (case context
           ((tail)
            (comp-push (car args))
            (emit-code src (make-glil-call 'tail-call/cc 1)))
           ((vals)
            (comp-vals
             (make-application
              src (make-primitive-ref #f 'call-with-current-continuation)
              args)
             MVRA)
            (maybe-emit-return))
           ((push)
            (comp-push (car args))
            (emit-code src (make-glil-call 'call/cc 1))
            (maybe-emit-return))
           ((drop)
            ;; Crap. Just like `apply' in drop context.
            (comp-drop
             (make-application
              src (make-primitive-ref #f 'call-with-current-continuation)
              args))
            (maybe-emit-return))))

        ;; A hack for variable-set, the opcode for which takes its args
        ;; reversed, relative to the variable-set! function
        ((and (primitive-ref? proc)
              (eq? (primitive-ref-name proc) 'variable-set!)
              (= (length args) 2))
         (comp-push (cadr args))
         (comp-push (car args))
         (emit-code src (make-glil-call 'variable-set 2))
         (case context
           ((tail push vals) (emit-code #f (make-glil-void))))
         (maybe-emit-return))
        
        ((and (primitive-ref? proc)
              (or (hash-ref *primcall-ops*
                            (cons (primitive-ref-name proc) (length args)))
                  (hash-ref *primcall-ops* (primitive-ref-name proc))))
         => (lambda (op)
              (for-each comp-push args)
              (emit-code src (make-glil-call op (length args)))
              (case (instruction-pushes op)
                ((0)
                 (case context
                   ((tail push vals) (emit-code #f (make-glil-void))))
                 (maybe-emit-return))
                ((1)
                 (case context
                   ((drop) (emit-code #f (make-glil-call 'drop 1))))
                 (maybe-emit-return))
                ((-1)
                 ;; A control instruction, like return/values.  Here we
                 ;; just have to hope that the author of the tree-il
                 ;; knew what they were doing.
                 *unspecified*)
                (else
                 (error "bad primitive op: too many pushes"
                        op (instruction-pushes op))))))
        
        ;; call to the same lambda-case in tail position
        ((and (lexical-ref? proc)
              self-label (eq? (lexical-ref-gensym proc) self-label)
              (eq? context 'tail)
              (not (lambda-case-kw lcase))
              (not (lambda-case-rest lcase))
              (= (length args)
                 (+ (length (lambda-case-req lcase))
                    (or (and=> (lambda-case-opt lcase) length) 0))))
         (for-each comp-push args)
         (for-each (lambda (sym)
                     (pmatch (hashq-ref (hashq-ref allocation sym) self)
                       ((#t #f . ,index) ; unboxed
                        (emit-code #f (make-glil-lexical #t #f 'set index)))
                       ((#t #t . ,index) ; boxed
                        ;; new box
                        (emit-code #f (make-glil-lexical #t #t 'box index)))
                       (,x (error "bad lambda-case arg allocation" x))))
                   (reverse (lambda-case-gensyms lcase)))
         (emit-branch src 'br (car (hashq-ref allocation lcase))))
        
        ;; lambda, the ultimate goto
        ((and (lexical-ref? proc)
              (assq (lexical-ref-gensym proc) fix-labels))
         ;; like the self-tail-call case, though we can handle "drop"
         ;; contexts too. first, evaluate new values, pushing them on
         ;; the stack
         (for-each comp-push args)
         ;; find the specific case, rename args, and goto the case label
         (let lp ((lcase (lambda-body
                          (assq-ref fix-labels (lexical-ref-gensym proc)))))
           (cond
            ((and (lambda-case? lcase)
                  (not (lambda-case-kw lcase))
                  (not (lambda-case-opt lcase))
                  (not (lambda-case-rest lcase))
                  (= (length args) (length (lambda-case-req lcase))))
             ;; we have a case that matches the args; rename variables
             ;; and goto the case label
             (for-each (lambda (sym)
                         (pmatch (hashq-ref (hashq-ref allocation sym) self)
                           ((#t #f . ,index) ; unboxed
                            (emit-code #f (make-glil-lexical #t #f 'set index)))
                           ((#t #t . ,index) ; boxed
                            (emit-code #f (make-glil-lexical #t #t 'box index)))
                           (,x (error "bad lambda-case arg allocation" x))))
                       (reverse (lambda-case-gensyms lcase)))
             (emit-branch src 'br (car (hashq-ref allocation lcase))))
            ((lambda-case? lcase)
             ;; no match, try next case
             (lp (lambda-case-alternate lcase)))
            (else
             ;; no cases left. we can't really handle this currently.
             ;; ideally we would push on a new frame, then do a "local
             ;; call" -- which doesn't require consing up a program
             ;; object. but for now error, as this sort of case should
             ;; preclude label allocation.
             (error "couldn't find matching case for label call" x)))))
        
        (else
         (if (not (eq? context 'tail))
             (emit-code src (make-glil-call 'new-frame 0)))
         (comp-push proc)
         (for-each comp-push args)
         (let ((len (length args)))
           (case context
             ((tail) (if (<= len #xff)
                         (emit-code src (make-glil-call 'tail-call len))
                         (begin
                           (comp-push (make-const #f len))
                           (emit-code src (make-glil-call 'tail-call/nargs 0)))))
             ((push) (if (<= len #xff)
                         (emit-code src (make-glil-call 'call len))
                         (begin
                           (comp-push (make-const #f len))
                           (emit-code src (make-glil-call 'call/nargs 0))))
                     (maybe-emit-return))
             ;; FIXME: mv-call doesn't have a /nargs variant, so it is
             ;; limited to 255 args.  Can work around it with a
             ;; trampoline and tail-call/nargs, but it's not so nice.
             ((vals) (emit-code src (make-glil-mv-call len MVRA))
                     (maybe-emit-return))
             ((drop) (let ((MV (make-label)) (POST (make-label)))
                       (emit-code src (make-glil-mv-call len MV))
                       (emit-code #f (make-glil-call 'drop 1))
                       (emit-branch #f 'br (or RA POST))
                       (emit-label MV)
                       (emit-code #f (make-glil-mv-bind 0 #f))
                       (if RA
                           (emit-branch #f 'br RA)
                           (emit-label POST)))))))))

      ((<conditional> src test consequent alternate)
       ;;     TEST
       ;;     (br-if-not L1)
       ;;     consequent
       ;;     (br L2)
       ;; L1: alternate
       ;; L2:
       (let ((L1 (make-label)) (L2 (make-label)))
         ;; need a pattern matcher
         (record-case test
           ((<application> proc args)
            (record-case proc
              ((<primitive-ref> name)
               (let ((len (length args)))
                 (cond

                  ((and (eq? name 'eq?) (= len 2))
                   (comp-push (car args))
                   (comp-push (cadr args))
                   (emit-branch src 'br-if-not-eq L1))

                  ((and (eq? name 'null?) (= len 1))
                   (comp-push (car args))
                   (emit-branch src 'br-if-not-null L1))

                  ((and (eq? name 'not) (= len 1))
                   (let ((app (car args)))
                     (record-case app
                       ((<application> proc args)
                        (let ((len (length args)))
                          (record-case proc
                            ((<primitive-ref> name)
                             (cond

                              ((and (eq? name 'eq?) (= len 2))
                               (comp-push (car args))
                               (comp-push (cadr args))
                               (emit-branch src 'br-if-eq L1))
                            
                              ((and (eq? name 'null?) (= len 1))
                               (comp-push (car args))
                               (emit-branch src 'br-if-null L1))

                              (else
                               (comp-push app)
                               (emit-branch src 'br-if L1))))
                            (else
                             (comp-push app)
                             (emit-branch src 'br-if L1)))))
                       (else
                        (comp-push app)
                        (emit-branch src 'br-if L1)))))
                  
                  (else
                   (comp-push test)
                   (emit-branch src 'br-if-not L1)))))
              (else
               (comp-push test)
               (emit-branch src 'br-if-not L1))))
           (else
            (comp-push test)
            (emit-branch src 'br-if-not L1)))

         (comp-tail consequent)
         ;; if there is an RA, comp-tail will cause a jump to it -- just
         ;; have to clean up here if there is no RA.
         (if (and (not RA) (not (eq? context 'tail)))
             (emit-branch #f 'br L2))
         (emit-label L1)
         (comp-tail alternate)
         (if (and (not RA) (not (eq? context 'tail)))
             (emit-label L2))))
      
      ((<primitive-ref> src name)
       (cond
        ((eq? (module-variable (fluid-ref *comp-module*) name)
              (module-variable the-root-module name))
         (case context
           ((tail push vals)
            (emit-code src (make-glil-toplevel 'ref name))))
         (maybe-emit-return))
        ((module-variable the-root-module name)
         (case context
           ((tail push vals)
            (emit-code src (make-glil-module 'ref '(guile) name #f))))
         (maybe-emit-return))
        (else
         (case context
           ((tail push vals)
            (emit-code src (make-glil-module
                            'ref (module-name (fluid-ref *comp-module*)) name #f))))
         (maybe-emit-return))))

      ((<lexical-ref> src gensym)
       (case context
         ((push vals tail)
          (pmatch (hashq-ref (hashq-ref allocation gensym) self)
            ((,local? ,boxed? . ,index)
             (emit-code src (make-glil-lexical local? boxed? 'ref index)))
            (,loc
             (error "bad lexical allocation" x loc)))))
       (maybe-emit-return))
      
      ((<lexical-set> src gensym exp)
       (comp-push exp)
       (pmatch (hashq-ref (hashq-ref allocation gensym) self)
         ((,local? ,boxed? . ,index)
          (emit-code src (make-glil-lexical local? boxed? 'set index)))
         (,loc
          (error "bad lexical allocation" x loc)))
       (case context
         ((tail push vals)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))
      
      ((<module-ref> src mod name public?)
       (emit-code src (make-glil-module 'ref mod name public?))
       (case context
         ((drop) (emit-code #f (make-glil-call 'drop 1))))
       (maybe-emit-return))
      
      ((<module-set> src mod name public? exp)
       (comp-push exp)
       (emit-code src (make-glil-module 'set mod name public?))
       (case context
         ((tail push vals)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))

      ((<toplevel-ref> src name)
       (emit-code src (make-glil-toplevel 'ref name))
       (case context
         ((drop) (emit-code #f (make-glil-call 'drop 1))))
       (maybe-emit-return))
      
      ((<toplevel-set> src name exp)
       (comp-push exp)
       (emit-code src (make-glil-toplevel 'set name))
       (case context
         ((tail push vals)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))
      
      ((<toplevel-define> src name exp)
       (comp-push exp)
       (emit-code src (make-glil-toplevel 'define name))
       (case context
         ((tail push vals)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))

      ((<lambda>)
       (let ((free-locs (cdr (hashq-ref allocation x))))
         (case context
           ((push vals tail)
            (emit-code #f (flatten-lambda x #f allocation))
            (if (not (null? free-locs))
                (begin
                  (for-each
                   (lambda (loc)
                     (pmatch loc
                       ((,local? ,boxed? . ,n)
                        (emit-code #f (make-glil-lexical local? #f 'ref n)))
                       (else (error "bad lambda free var allocation" x loc))))
                   free-locs)
                  (emit-code #f (make-glil-call 'make-closure
                                                (length free-locs))))))))
       (maybe-emit-return))
      
      ((<lambda-case> src req opt rest kw inits gensyms alternate body)
       ;; o/~ feature on top of feature o/~
       ;; req := (name ...)
       ;; opt := (name ...) | #f
       ;; rest := name | #f
       ;; kw: (allow-other-keys? (keyword name var) ...) | #f
       ;; gensyms: (sym ...)
       ;; init: tree-il in context of gensyms
       ;; gensyms map to named arguments in the following order:
       ;;  required, optional (positional), rest, keyword.
       (let* ((nreq (length req))
              (nopt (if opt (length opt) 0))
              (rest-idx (and rest (+ nreq nopt)))
              (opt-names (or opt '()))
              (allow-other-keys? (if kw (car kw) #f))
              (kw-indices (map (lambda (x)
                                 (pmatch x
                                   ((,key ,name ,var)
                                    (cons key (list-index gensyms var)))
                                   (else (error "bad kwarg" x))))
                               (if kw (cdr kw) '())))
              (nargs (apply max (+ nreq nopt (if rest 1 0))
                            (map 1+ (map cdr kw-indices))))
              (nlocs (cdr (hashq-ref allocation x)))
              (alternate-label (and alternate (make-label))))
         (or (= nargs
                (length gensyms)
                (+ nreq (length inits) (if rest 1 0)))
             (error "lambda-case gensyms don't correspond to args"
                    req opt rest kw inits gensyms nreq nopt kw-indices nargs))
         ;; the prelude, to check args & reset the stack pointer,
         ;; allowing room for locals
         (emit-code
          src
          (cond
           (kw
            (make-glil-kw-prelude nreq nopt rest-idx kw-indices
                                  allow-other-keys? nlocs alternate-label))
           ((or rest opt)
            (make-glil-opt-prelude nreq nopt rest-idx nlocs alternate-label))
           (#t
            (make-glil-std-prelude nreq nlocs alternate-label))))
         ;; box args if necessary
         (for-each
          (lambda (v)
            (pmatch (hashq-ref (hashq-ref allocation v) self)
              ((#t #t . ,n)
               (emit-code #f (make-glil-lexical #t #f 'ref n))
               (emit-code #f (make-glil-lexical #t #t 'box n)))))
          gensyms)
         ;; write bindings info
         (if (not (null? gensyms))
             (emit-bindings
              #f
              (let lp ((kw (if kw (cdr kw) '()))
                       (names (append (reverse opt-names) (reverse req)))
                       (gensyms (list-tail gensyms (+ nreq nopt
                                                (if rest 1 0)))))
                (pmatch kw
                  (()
                   ;; fixme: check that gensyms is empty
                   (reverse (if rest (cons rest names) names)))
                  (((,key ,name ,var) . ,kw)
                   (if (memq var gensyms)
                       (lp kw (cons name names) (delq var gensyms))
                       (lp kw names gensyms)))
                  (,kw (error "bad keywords, yo" kw))))
              gensyms allocation self emit-code))
         ;; init optional/kw args
         (let lp ((inits inits) (n nreq) (gensyms (list-tail gensyms nreq)))
           (cond
            ((null? inits))             ; done
            ((and rest-idx (= n rest-idx))
             (lp inits (1+ n) (cdr gensyms)))
            (#t
             (pmatch (hashq-ref (hashq-ref allocation (car gensyms)) self)
               ((#t ,boxed? . ,n*) (guard (= n* n))
                (let ((L (make-label)))
                  (emit-code #f (make-glil-lexical #t boxed? 'bound? n))
                  (emit-code #f (make-glil-branch 'br-if L))
                  (comp-push (car inits))
                  (emit-code #f (make-glil-lexical #t boxed? 'set n))
                  (emit-label L)
                  (lp (cdr inits) (1+ n) (cdr gensyms))))
               (#t (error "bad arg allocation" (car gensyms) inits))))))
         ;; post-prelude case label for label calls
         (emit-label (car (hashq-ref allocation x)))
         (comp-tail body)
         (if (not (null? gensyms))
             (emit-code #f (make-glil-unbind)))
         (if alternate-label
             (begin
               (emit-label alternate-label)
               (flatten-lambda-case alternate allocation self self-label
                                    fix-labels emit-code)))))
      
      ((<let> src names gensyms vals body)
       (for-each comp-push vals)
       (emit-bindings src names gensyms allocation self emit-code)
       (for-each (lambda (v)
                   (pmatch (hashq-ref (hashq-ref allocation v) self)
                     ((#t #f . ,n)
                      (emit-code src (make-glil-lexical #t #f 'set n)))
                     ((#t #t . ,n)
                      (emit-code src (make-glil-lexical #t #t 'box n)))
                     (,loc (error "bad let var allocation" x loc))))
                 (reverse gensyms))
       (comp-tail body)
       (clear-stack-slots context gensyms)
       (emit-code #f (make-glil-unbind)))

      ((<letrec> src in-order? names gensyms vals body)
       ;; First prepare heap storage slots.
       (for-each (lambda (v)
                   (pmatch (hashq-ref (hashq-ref allocation v) self)
                     ((#t #t . ,n)
                      (emit-code src (make-glil-lexical #t #t 'empty-box n)))
                     (,loc (error "bad letrec var allocation" x loc))))
                 gensyms)
       ;; Even though the slots are empty, the bindings are valid.
       (emit-bindings src names gensyms allocation self emit-code)
       (cond
        (in-order?
         ;; For letrec*, bind values in order.
         (for-each (lambda (name v val)
                     (pmatch (hashq-ref (hashq-ref allocation v) self)
                       ((#t #t . ,n)
                        (comp-push val)
                        (emit-code src (make-glil-lexical #t #t 'set n)))
                       (,loc (error "bad letrec var allocation" x loc))))
                   names gensyms vals))
        (else
         ;; But for letrec, eval all values, then bind.
         (for-each comp-push vals)
         (for-each (lambda (v)
                     (pmatch (hashq-ref (hashq-ref allocation v) self)
                       ((#t #t . ,n)
                        (emit-code src (make-glil-lexical #t #t 'set n)))
                       (,loc (error "bad letrec var allocation" x loc))))
                   (reverse gensyms))))
       (comp-tail body)
       (clear-stack-slots context gensyms)
       (emit-code #f (make-glil-unbind)))

      ((<fix> src names gensyms vals body)
       ;; The ideal here is to just render the lambda bodies inline, and
       ;; wire the code together with gotos. We can do that if
       ;; analyze-lexicals has determined that a given var has "label"
       ;; allocation -- which is the case if it is in `fix-labels'.
       ;;
       ;; But even for closures that we can't inline, we can do some
       ;; tricks to avoid heap-allocation for the binding itself. Since
       ;; we know the vals are lambdas, we can set them to their local
       ;; var slots first, then capture their bindings, mutating them in
       ;; place.
       (let ((new-RA (if (or (eq? context 'tail) RA) #f (make-label))))
         (for-each
          (lambda (x v)
            (cond
             ((hashq-ref allocation x)
              ;; allocating a closure
              (emit-code #f (flatten-lambda x v allocation))
              (let ((free-locs (cdr (hashq-ref allocation x))))
                (if (not (null? free-locs))
                    ;; Need to make-closure first, so we have a fresh closure on
                    ;; the heap, but with a temporary free values.
                    (begin
                      (for-each (lambda (loc)
                                  (emit-code #f (make-glil-const #f)))
                                free-locs)
                      (emit-code #f (make-glil-call 'make-closure
                                                    (length free-locs))))))
              (pmatch (hashq-ref (hashq-ref allocation v) self)
                ((#t #f . ,n)
                 (emit-code src (make-glil-lexical #t #f 'set n)))
                (,loc (error "bad fix var allocation" x loc))))
             (else
              ;; labels allocation: emit label & body, but jump over it
              (let ((POST (make-label)))
                (emit-branch #f 'br POST)
                (let lp ((lcase (lambda-body x)))
                  (if lcase
                      (record-case lcase
                        ((<lambda-case> src req gensyms body alternate)
                         (emit-label (car (hashq-ref allocation lcase)))
                         ;; FIXME: opt & kw args in the bindings
                         (emit-bindings #f req gensyms allocation self emit-code)
                         (if src
                             (emit-code #f (make-glil-source src)))
                         (comp-fix body (or RA new-RA))
                         (emit-code #f (make-glil-unbind))
                         (lp alternate)))
                      (emit-label POST)))))))
          vals
          gensyms)
         ;; Emit bindings metadata for closures
         (let ((binds (let lp ((out '()) (gensyms gensyms) (names names))
                        (cond ((null? gensyms) (reverse! out))
                              ((assq (car gensyms) fix-labels)
                               (lp out (cdr gensyms) (cdr names)))
                              (else
                               (lp (acons (car gensyms) (car names) out)
                                   (cdr gensyms) (cdr names)))))))
           (emit-bindings src (map cdr binds) (map car binds)
                          allocation self emit-code))
         ;; Now go back and fix up the bindings for closures.
         (for-each
          (lambda (x v)
            (let ((free-locs (if (hashq-ref allocation x)
                                 (cdr (hashq-ref allocation x))
                                 ;; can hit this latter case for labels allocation
                                 '())))
              (if (not (null? free-locs))
                  (begin
                    (for-each
                     (lambda (loc)
                       (pmatch loc
                         ((,local? ,boxed? . ,n)
                          (emit-code #f (make-glil-lexical local? #f 'ref n)))
                         (else (error "bad free var allocation" x loc))))
                     free-locs)
                    (pmatch (hashq-ref (hashq-ref allocation v) self)
                      ((#t #f . ,n)
                       (emit-code #f (make-glil-lexical #t #f 'fix n)))
                      (,loc (error "bad fix var allocation" x loc)))))))
          vals
          gensyms)
         (comp-tail body)
         (if new-RA
             (emit-label new-RA))
         (clear-stack-slots context gensyms)
         (emit-code #f (make-glil-unbind))))

      ((<let-values> src exp body)
       (record-case body
         ((<lambda-case> req opt kw rest gensyms body alternate)
          (if (or opt kw alternate)
              (error "unexpected lambda-case in let-values" x))
          (let ((MV (make-label)))
            (comp-vals exp MV)
            (emit-code #f (make-glil-const 1))
            (emit-label MV)
            (emit-code src (make-glil-mv-bind
                            (vars->bind-list
                             (append req (if rest (list rest) '()))
                             gensyms allocation self)
                            (and rest #t)))
            (for-each (lambda (v)
                        (pmatch (hashq-ref (hashq-ref allocation v) self)
                          ((#t #f . ,n)
                           (emit-code src (make-glil-lexical #t #f 'set n)))
                          ((#t #t . ,n)
                           (emit-code src (make-glil-lexical #t #t 'box n)))
                          (,loc (error "bad let-values var allocation" x loc))))
                      (reverse gensyms))
            (comp-tail body)
            (clear-stack-slots context gensyms)
            (emit-code #f (make-glil-unbind))))))

      ;; much trickier than i thought this would be, at first, due to the need
      ;; to have body's return value(s) on the stack while the unwinder runs,
      ;; then proceed with returning or dropping or what-have-you, interacting
      ;; with RA and MVRA. What have you, I say.
      ((<dynwind> src body winder unwinder)
       (comp-push winder)
       (comp-push unwinder)
       (comp-drop (make-application src winder '()))
       (emit-code #f (make-glil-call 'wind 2))

       (case context
         ((tail)
          (let ((MV (make-label)))
            (comp-vals body MV)
            ;; one value: unwind...
            (emit-code #f (make-glil-call 'unwind 0))
            (comp-drop (make-application src unwinder '()))
            ;; ...and return the val
            (emit-code #f (make-glil-call 'return 1))
            
            (emit-label MV)
            ;; multiple values: unwind...
            (emit-code #f (make-glil-call 'unwind 0))
            (comp-drop (make-application src unwinder '()))
            ;; and return the values.
            (emit-code #f (make-glil-call 'return/nvalues 1))))
         
         ((push)
          ;; we only want one value. so ask for one value
          (comp-push body)
          ;; and unwind, leaving the val on the stack
          (emit-code #f (make-glil-call 'unwind 0))
          (comp-drop (make-application src unwinder '())))
         
         ((vals)
          (let ((MV (make-label)))
            (comp-vals body MV)
            ;; one value: push 1 and fall through to MV case
            (emit-code #f (make-glil-const 1))
            
            (emit-label MV)
            ;; multiple values: unwind...
            (emit-code #f (make-glil-call 'unwind 0))
            (comp-drop (make-application src unwinder '()))
            ;; and goto the MVRA.
            (emit-branch #f 'br MVRA)))
         
         ((drop)
          ;; compile body, discarding values. then unwind...
          (comp-drop body)
          (emit-code #f (make-glil-call 'unwind 0))
          (comp-drop (make-application src unwinder '()))
          ;; and fall through, or goto RA if there is one.
          (if RA
              (emit-branch #f 'br RA)))))

      ((<dynlet> src fluids vals body)
       (for-each comp-push fluids)
       (for-each comp-push vals)
       (emit-code #f (make-glil-call 'wind-fluids (length fluids)))

       (case context
         ((tail)
          (let ((MV (make-label)))
            ;; NB: in tail case, it is possible to preserve asymptotic tail
            ;; recursion, via merging unwind-fluids structures -- but we'd need
            ;; to compile in the body twice (once in tail context, assuming the
            ;; caller unwinds, and once with this trampoline thing, unwinding
            ;; ourselves).
            (comp-vals body MV)
            ;; one value: unwind and return
            (emit-code #f (make-glil-call 'unwind-fluids 0))
            (emit-code #f (make-glil-call 'return 1))
            
            (emit-label MV)
            ;; multiple values: unwind and return values
            (emit-code #f (make-glil-call 'unwind-fluids 0))
            (emit-code #f (make-glil-call 'return/nvalues 1))))
         
         ((push)
          (comp-push body)
          (emit-code #f (make-glil-call 'unwind-fluids 0)))
         
         ((vals)
          (let ((MV (make-label)))
            (comp-vals body MV)
            ;; one value: push 1 and fall through to MV case
            (emit-code #f (make-glil-const 1))
            
            (emit-label MV)
            ;; multiple values: unwind and goto MVRA
            (emit-code #f (make-glil-call 'unwind-fluids 0))
            (emit-branch #f 'br MVRA)))
         
         ((drop)
          ;; compile body, discarding values. then unwind...
          (comp-drop body)
          (emit-code #f (make-glil-call 'unwind-fluids 0))
          ;; and fall through, or goto RA if there is one.
          (if RA
              (emit-branch #f 'br RA)))))

      ((<dynref> src fluid)
       (case context
         ((drop)
          (comp-drop fluid))
         ((push vals tail)
          (comp-push fluid)
          (emit-code #f (make-glil-call 'fluid-ref 1))))
       (maybe-emit-return))
      
      ((<dynset> src fluid exp)
       (comp-push fluid)
       (comp-push exp)
       (emit-code #f (make-glil-call 'fluid-set 2))
       (case context
         ((push vals tail)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))
      
      ;; What's the deal here? The deal is that we are compiling the start of a
      ;; delimited continuation. We try to avoid heap allocation in the normal
      ;; case; so the body is an expression, not a thunk, and we try to render
      ;; the handler inline. Also we did some analysis, in analyze.scm, so that
      ;; if the continuation isn't referenced, we don't reify it. This makes it
      ;; possible to implement catch and throw with delimited continuations,
      ;; without any overhead.
      ((<prompt> src tag body handler)
       (let ((H (make-label))
             (POST (make-label))
             (escape-only? (hashq-ref allocation x)))
         ;; First, set up the prompt.
         (comp-push tag)
         (emit-code src (make-glil-prompt H escape-only?))

         ;; Then we compile the body, with its normal return path, unwinding
         ;; before proceeding.
         (case context
           ((tail)
            (let ((MV (make-label)))
              (comp-vals body MV)
              ;; one value: unwind and return
              (emit-code #f (make-glil-call 'unwind 0))
              (emit-code #f (make-glil-call 'return 1))
              ;; multiple values: unwind and return
              (emit-label MV)
              (emit-code #f (make-glil-call 'unwind 0))
              (emit-code #f (make-glil-call 'return/nvalues 1))))
         
           ((push)
            ;; we only want one value. so ask for one value, unwind, and jump to
            ;; post
            (comp-push body)
            (emit-code #f (make-glil-call 'unwind 0))
            (emit-branch #f 'br (or RA POST)))
           
           ((vals)
            (let ((MV (make-label)))
              (comp-vals body MV)
              ;; one value: push 1 and fall through to MV case
              (emit-code #f (make-glil-const 1))
              ;; multiple values: unwind and goto MVRA
              (emit-label MV)
              (emit-code #f (make-glil-call 'unwind 0))
              (emit-branch #f 'br MVRA)))
         
           ((drop)
            ;; compile body, discarding values, then unwind & fall through.
            (comp-drop body)
            (emit-code #f (make-glil-call 'unwind 0))
            (emit-branch #f 'br (or RA POST))))
         
         (emit-label H)
         ;; Now the handler. The stack is now made up of the continuation, and
         ;; then the args to the continuation (pushed separately), and then the
         ;; number of args, including the continuation.
         (record-case handler
           ((<lambda-case> req opt kw rest gensyms body alternate)
            (if (or opt kw alternate)
                (error "unexpected lambda-case in prompt" x))
            (emit-code src (make-glil-mv-bind
                            (vars->bind-list
                             (append req (if rest (list rest) '()))
                             gensyms allocation self)
                            (and rest #t)))
            (for-each (lambda (v)
                        (pmatch (hashq-ref (hashq-ref allocation v) self)
                          ((#t #f . ,n)
                           (emit-code src (make-glil-lexical #t #f 'set n)))
                          ((#t #t . ,n)
                           (emit-code src (make-glil-lexical #t #t 'box n)))
                          (,loc
                           (error "bad prompt handler arg allocation" x loc))))
                      (reverse gensyms))
            (comp-tail body)
            (emit-code #f (make-glil-unbind))))

         (if (and (not RA)
                  (or (eq? context 'push) (eq? context 'drop)))
             (emit-label POST))))

      ((<abort> src tag args tail)
       (comp-push tag)
       (for-each comp-push args)
       (comp-push tail)
       (emit-code src (make-glil-call 'abort (length args)))
       ;; so, the abort can actually return. if it does, the values will be on
       ;; the stack, then the MV marker, just as in an MV context.
       (case context
         ((tail)
          ;; Return values.
          (emit-code #f (make-glil-call 'return/nvalues 1)))
         ((drop)
          ;; Drop all values and goto RA, or otherwise fall through.
          (emit-code #f (make-glil-mv-bind 0 #f))
          (if RA (emit-branch #f 'br RA)))
         ((push)
          ;; Truncate to one value.
          (emit-code #f (make-glil-mv-bind 1 #f)))
         ((vals)
          ;; Go to MVRA.
          (emit-branch #f 'br MVRA)))))))
