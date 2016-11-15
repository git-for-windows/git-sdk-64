;;; Guile VM assembler

;; Copyright (C) 2001, 2009, 2010, 2011, 2013 Free Software Foundation, Inc.

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

(define-module (language glil compile-assembly)
  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:use-module (language glil)
  #:use-module (language assembly)
  #:use-module (system vm instruction)
  #:use-module ((system vm program) #:select (make-binding))
  #:use-module (ice-9 receive)
  #:use-module (ice-9 vlist)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (rnrs bytevectors)
  #:export (compile-assembly))

;; Traversal helpers
;;
(define (vhash-fold-right2 proc vhash s0 s1)
  (let lp ((i (vlist-length vhash)) (s0 s0) (s1 s1))
    (if (zero? i)
        (values s0 s1)
        (receive (s0 s1) (let ((pair (vlist-ref vhash (1- i))))
                           (proc (car pair) (cdr pair) s0 s1))
          (lp (1- i) s0 s1)))))

(define (fold2 proc ls s0 s1)
  (let lp ((ls ls) (s0 s0) (s1 s1))
    (if (null? ls)
        (values s0 s1)
        (receive (s0 s1) (proc (car ls) s0 s1)
          (lp (cdr ls) s0 s1)))))

(define (vector-fold2 proc vect s0 s1)
  (let ((len (vector-length vect)))
    (let lp ((i 0) (s0 s0) (s1 s1))
      (if (< i len)
          (receive (s0 s1) (proc (vector-ref vect i) s0 s1)
            (lp (1+ i) s0 s1))
          (values s0 s1)))))

;; Variable cache cells go in the object table, and serialize as their
;; keys. The reason we wrap the keys in these records is so they don't
;; compare as `equal?' to other objects in the object table.
;;
;; `key' is either a symbol or the list (MODNAME SYM PUBLIC?)

(define-record <variable-cache-cell> key)

(define (limn-sources sources)
  (let lp ((in sources) (out '()) (filename #f))
    (if (null? in)
        (reverse! out)
        (let ((addr (caar in))
              (new-filename (assq-ref (cdar in ) 'filename))
              (line (assq-ref (cdar in) 'line))
              (column (assq-ref (cdar in) 'column)))
          (cond
           ((not (equal? new-filename filename))
            (lp (cdr in)
                `((,addr . (,line . ,column))
                  (filename . ,new-filename)
                  . ,out)
                new-filename))
           ((or (null? out) (not (equal? (cdar out) `(,line . ,column))))
            (lp (cdr in)
                `((,addr . (,line . ,column))
                  . ,out)
                filename))
           (else
            (lp (cdr in) out filename)))))))


;; Avoid going through the compiler so as to avoid adding to the
;; constant store.
(define (make-meta bindings sources arities tail)
  (let ((body `(,@(dump-object `(,bindings ,sources ,arities ,@tail) 0)
                (return))))
    `(load-program ()
                   ,(addr+ 0 body)
                   #f
                   ,@body)))

;; If this is true, the object doesn't need to go in a constant table.
;;
(define (immediate? x)
  (object->assembly x))

;; This tests for a proper scheme list whose last cdr is '(), not #nil.
;;
(define (scheme-list? x)
  (and (list? x)
       (or (eq? x '())
           (let ((p (last-pair x)))
             (and (pair? p)
                  (eq? (cdr p) '()))))))

;; Note: in all of these procedures that build up constant tables, the
;; first (zeroth) index is reserved.  At runtime it is replaced with the
;; procedure's module.  Hence all of this 1+ length business.

;; Build up a vhash of constant -> index, allowing us to build up a
;; constant table for a whole compilation unit.
;;
(define (build-constant-store x)
  (define (add-to-store store x)
    (define (add-to-end store x)
      (vhash-cons x (1+ (vlist-length store)) store))
    (cond
     ((vhash-assoc x store)
      ;; Already in the store.
      store)
     ((immediate? x)
      ;; Immediates don't need to go in the constant table.
      store)
     ((or (number? x)
          (string? x)
          (symbol? x)
          (keyword? x))
      ;; Atoms.
      (add-to-end store x))
     ((variable-cache-cell? x)
      ;; Variable cache cells (see below).
      (add-to-end (add-to-store store (variable-cache-cell-key x))
                  x))
     ((list? x)
      ;; Add the elements to the store, then the list itself.  We could
      ;; try hashing the cdrs as well, but that seems a bit overkill, and
      ;; this way we do compress the bytecode a bit by allowing the use of
      ;; the `list' opcode.
      (let ((store (fold (lambda (x store)
                           (add-to-store store x))
                         store
                         x)))
        (add-to-end store x)))
     ((pair? x)
      ;; Non-lists get caching on both fields.
      (let ((store (add-to-store (add-to-store store (car x))
                                 (cdr x))))
        (add-to-end store x)))
     ((and (vector? x)
           (equal? (array-shape x) (list (list 0 (1- (vector-length x))))))
      ;; Likewise, add the elements to the store, then the vector itself.
      ;; Important for the vectors produced by the psyntax expansion
      ;; process.
      (let ((store (fold (lambda (x store)
                           (add-to-store store x))
                         store
                         (vector->list x))))
        (add-to-end store x)))
     ((array? x)
      ;; Naive assumption that if folks are using arrays, that perhaps
      ;; there's not much more duplication.
      (add-to-end store x))
     (else
      (error "build-constant-store: unrecognized object" x))))

  (let walk ((x x) (store vlist-null))
    (record-case x
      ((<glil-program> meta body)
       (fold walk store body))
      ((<glil-const> obj)
       (add-to-store store obj))
      ((<glil-kw-prelude> kw)
       (add-to-store store kw))
      ((<glil-toplevel> op name)
       ;; We don't add toplevel variable cache cells to the global
       ;; constant table, because they are sensitive to changes in
       ;; modules as the toplevel expressions are evaluated.  So we just
       ;; add the name.
       (add-to-store store name))
      ((<glil-module> op mod name public?)
       ;; However, it is fine add module variable cache cells to the
       ;; global table, as their bindings are not dependent on the
       ;; current module.
       (add-to-store store
                     (make-variable-cache-cell (list mod name public?))))
      (else store))))

;; Analyze one <glil-program> to determine its object table.  Produces a
;; vhash of constant to index.
;;
(define (build-object-table x)
  (define (add store x)
    (if (vhash-assoc x store)
        store
        (vhash-cons x (1+ (vlist-length store)) store)))
  (record-case x
    ((<glil-program> meta body)
     (fold (lambda (x table)
             (record-case x
               ((<glil-program> meta body)
                ;; Add the GLIL itself to the table.
                (add table x))
               ((<glil-const> obj)
                (if (immediate? obj)
                    table
                    (add table obj)))
               ((<glil-kw-prelude> kw)
                (add table kw))
               ((<glil-toplevel> op name)
                (add table (make-variable-cache-cell name)))
               ((<glil-module> op mod name public?)
                (add table (make-variable-cache-cell (list mod name public?))))
               (else table)))
           vlist-null
           body))))

;; A functional stack of names of live variables.
(define (make-open-binding name boxed? index)
  (list name boxed? index))
(define (make-closed-binding open-binding start end)
  (make-binding (car open-binding) (cadr open-binding)
                (caddr open-binding) start end))
(define (open-binding bindings vars start)
  (cons
   (acons start
          (map
           (lambda (v)
             (pmatch v
               ((,name ,boxed? ,i)
                (make-open-binding name boxed? i))
               (else (error "unknown binding type" v))))
           vars)
          (car bindings))
   (cdr bindings)))
(define (close-binding bindings end)
  (pmatch bindings
    ((((,start . ,closing) . ,open) . ,closed)
     (cons open
           (fold (lambda (o tail)
                   ;; the cons is for dsu sort
                   (acons start (make-closed-binding o start end)
                          tail))
                 closed
                 closing)))
    (else (error "broken bindings" bindings))))
(define (close-all-bindings bindings end)
  (if (null? (car bindings))
      (map cdr
           (stable-sort (reverse (cdr bindings))
                        (lambda (x y) (< (car x) (car y)))))
      (close-all-bindings (close-binding bindings end) end)))


;; A functional arities thingamajiggy.
;; arities := ((ip nreq [[nopt] [[rest] [kw]]]]) ...)
(define (open-arity addr nreq nopt rest kw arities)
  (cons
   (cond
    (kw (list addr nreq nopt rest kw))
    (rest (list addr nreq nopt rest))
    (nopt (list addr nreq nopt))
    (nreq (list addr nreq))
    (else (list addr)))
   arities))
(define (close-arity addr arities)
  (pmatch arities
    (() '())
    (((,start . ,tail) . ,rest)
     `((,start ,addr . ,tail) . ,rest))
    (else (error "bad arities" arities))))
(define (begin-arity end start nreq nopt rest kw arities)
  (open-arity start nreq nopt rest kw (close-arity end arities)))

(define (compile-assembly glil)
  (let* ((all-constants (build-constant-store glil))
         (prog (compile-program glil all-constants))
         (len (byte-length prog)))
    ;; The top objcode thunk.  We're going to wrap this thunk in
    ;; a thunk -- yo dawgs -- with the goal being to lift all
    ;; constants up to the top level.  The store forms a DAG, so
    ;; we can actually build up later elements in terms of
    ;; earlier ones.
    ;;
    (cond
     ((vlist-null? all-constants)
      ;; No constants: just emit the inner thunk.
      prog)
     (else
      ;; We have an object store, so write it out, attach it
      ;; to the inner thunk, and tail call.
      (receive (tablecode addr) (dump-constants all-constants)
        (let ((prog (align-program prog addr)))
          ;; Outer thunk.
          `(load-program ()
                         ,(+ (addr+ addr prog)
                             2          ; for (tail-call 0)
                             )
                         #f
                         ;; Load the table, build the inner
                         ;; thunk, then tail call.
                         ,@tablecode
                         ,@prog
                         (tail-call 0))))))))

(define (compile-program glil constants)
  (record-case glil
    ((<glil-program> meta body)
     (let lp ((body body) (code '()) (bindings '(())) (source-alist '())
              (label-alist '()) (arities '()) (addr 0))
       (cond
        ((null? body)
         (let ((code (fold append '() code))
               (bindings (close-all-bindings bindings addr))
               (sources (limn-sources (reverse! source-alist)))
               (labels (reverse label-alist))
               (arities (reverse (close-arity addr arities)))
               (len addr))
           (let* ((meta (make-meta bindings sources arities meta))
                  (meta-pad (if meta (modulo (- 8 (modulo len 8)) 8) 0)))
             `(load-program ,labels
                            ,(+ len meta-pad)
                            ,meta
                            ,@code
                            ,@(if meta
                                  (make-list meta-pad '(nop))
                                  '())))))
        (else
         (receive (subcode bindings source-alist label-alist arities)
             (glil->assembly (car body) bindings
                             source-alist label-alist
                             constants arities addr)
           (lp (cdr body) (cons subcode code)
               bindings source-alist label-alist arities
               (addr+ addr subcode)))))))))

(define (compile-objtable constants table addr)
  (define (load-constant idx)
    (if (< idx 256)
        (values `((object-ref ,idx))
                2)
        (values `((long-object-ref
                   ,(quotient idx 256) ,(modulo idx 256)))
                3)))
  (cond
   ((vlist-null? table)
    ;; Empty table; just return #f.
    (values '((make-false))
            (1+ addr)))
   (else
    (call-with-values
        (lambda ()
          (vhash-fold-right2
           (lambda (obj idx codes addr)
             (cond
              ((vhash-assoc obj constants)
               => (lambda (pair)
                    (receive (load len) (load-constant (cdr pair))
                      (values (cons load codes)
                              (+ addr len)))))
              ((variable-cache-cell? obj)
               (cond
                ((vhash-assoc (variable-cache-cell-key obj) constants)
                 => (lambda (pair)
                      (receive (load len) (load-constant (cdr pair))
                        (values (cons load codes)
                                (+ addr len)))))
                (else (error "vcache cell key not in table" obj))))
              ((glil-program? obj)
               ;; Programs are not cached in the global constants
               ;; table because when a program is loaded, its module
               ;; is bound, and we want to do that only after any
               ;; preceding effectful statements.
               (let* ((table (build-object-table obj))
                      (prog (compile-program obj table)))
                 (receive (tablecode addr)
                     (compile-objtable constants table addr)
                   (let ((prog (align-program prog addr)))
                     (values (cons `(,@tablecode ,@prog)
                                   codes)
                             (addr+ addr prog))))))
              (else
               (error "unrecognized constant" obj))))
           table
           '(((make-false))) (1+ addr)))
      (lambda (elts addr)
        (let ((len (1+ (vlist-length table))))
          (values
           (fold append
                 `((vector ,(quotient len 256) ,(modulo len 256)))
                 elts)
           (+ addr 3))))))))

(define (glil->assembly glil bindings source-alist label-alist
                        constants arities addr)
  (define (emit-code x)
    (values x bindings source-alist label-alist arities))
  (define (emit-object-ref i)
    (values (if (< i 256)
                `((object-ref ,i))
                `((long-object-ref ,(quotient i 256) ,(modulo i 256))))
            bindings source-alist label-alist arities))
  (define (emit-code/arity x nreq nopt rest kw)
    (values x bindings source-alist label-alist
            (begin-arity addr (addr+ addr x) nreq nopt rest kw arities)))
  
  (record-case glil
    ((<glil-program> meta body)
     (cond
      ((vhash-assoc glil constants)
       ;; We are cached in someone's objtable; just emit a load.
       => (lambda (pair)
            (emit-object-ref (cdr pair))))
      (else
       ;; Otherwise, build an objtable for the program, compile it, and
       ;; emit a load-program.
       (let* ((table (build-object-table glil))
              (prog (compile-program glil table)))
         (receive (tablecode addr) (compile-objtable constants table addr)
           (emit-code `(,@tablecode ,@(align-program prog addr))))))))
    
    ((<glil-std-prelude> nreq nlocs else-label)
     (emit-code/arity
      (if (and (< nreq 8) (< nlocs (+ nreq 32)) (not else-label))
          `((assert-nargs-ee/locals ,(logior nreq (ash (- nlocs nreq) 3))))
          `(,(if else-label
                 `(br-if-nargs-ne ,(quotient nreq 256)
                                  ,(modulo nreq 256)
                                  ,else-label)
                 `(assert-nargs-ee ,(quotient nreq 256)
                                   ,(modulo nreq 256)))
            (reserve-locals ,(quotient nlocs 256)
                            ,(modulo nlocs 256))))
      nreq #f #f #f))

    ((<glil-opt-prelude> nreq nopt rest nlocs else-label)
     (let ((bind-required
            (if else-label
                `((br-if-nargs-lt ,(quotient nreq 256)
                                  ,(modulo nreq 256)
                                  ,else-label))
                `((assert-nargs-ge ,(quotient nreq 256)
                                   ,(modulo nreq 256)))))
           (bind-optionals
            (if (zero? nopt)
                '()
                `((bind-optionals ,(quotient (+ nopt nreq) 256)
                                  ,(modulo (+ nreq nopt) 256)))))
           (bind-rest
            (cond
             (rest
              `((push-rest ,(quotient (+ nreq nopt) 256)
                           ,(modulo (+ nreq nopt) 256))))
             (else
              (if else-label
                  `((br-if-nargs-gt ,(quotient (+ nreq nopt) 256)
                                    ,(modulo (+ nreq nopt) 256)
                                    ,else-label))
                  `((assert-nargs-ee ,(quotient (+ nreq nopt) 256)
                                     ,(modulo (+ nreq nopt) 256))))))))
       (emit-code/arity
        `(,@bind-required
          ,@bind-optionals
          ,@bind-rest
          (reserve-locals ,(quotient nlocs 256)
                          ,(modulo nlocs 256)))
        nreq nopt rest #f)))
    
    ((<glil-kw-prelude> nreq nopt rest kw allow-other-keys? nlocs else-label)
     (let* ((kw-idx (or (and=> (vhash-assoc kw constants) cdr)
                        (error "kw not in objtable")))
            (bind-required
             (if else-label
                 `((br-if-nargs-lt ,(quotient nreq 256)
                                   ,(modulo nreq 256)
                                   ,else-label))
                 `((assert-nargs-ge ,(quotient nreq 256)
                                    ,(modulo nreq 256)))))
            (ntotal (apply max (+ nreq nopt) (map 1+ (map cdr kw))))
            (bind-optionals-and-shuffle
             `((,(if (and else-label (not rest))
                     'bind-optionals/shuffle-or-br
                     'bind-optionals/shuffle)
                ,(quotient nreq 256)
                ,(modulo nreq 256)
                ,(quotient (+ nreq nopt) 256)
                ,(modulo (+ nreq nopt) 256)
                ,(quotient ntotal 256)
                ,(modulo ntotal 256)
                ,@(if (and else-label (not rest))
                      `(,else-label)
                      '()))))
            (bind-kw
             ;; when this code gets called, all optionals are filled
             ;; in, space has been made for kwargs, and the kwargs
             ;; themselves have been shuffled above the slots for all
             ;; req/opt/kwargs locals.
             `((bind-kwargs
                ,(quotient kw-idx 256)
                ,(modulo kw-idx 256)
                ,(quotient ntotal 256)
                ,(modulo ntotal 256)
                ,(logior (if rest 2 0)
                         (if allow-other-keys? 1 0)))))
            (bind-rest
             (if rest
                 `((bind-rest ,(quotient ntotal 256)
                              ,(modulo ntotal 256)
                              ,(quotient rest 256)
                              ,(modulo rest 256)))
                 '())))
         
       (let ((code `(,@bind-required
                     ,@bind-optionals-and-shuffle
                     ,@bind-kw
                     ,@bind-rest
                     (reserve-locals ,(quotient nlocs 256)
                                     ,(modulo nlocs 256)))))
         (values code bindings source-alist label-alist
                 (begin-arity addr (addr+ addr code) nreq nopt rest
                              (and kw (cons allow-other-keys? kw))
                              arities)))))
    
    ((<glil-bind> vars)
     (values '()
             (open-binding bindings vars addr)
             source-alist
             label-alist
             arities))

    ((<glil-mv-bind> vars rest)
     (if (integer? vars)
         (values `((truncate-values ,vars ,(if rest 1 0)))
                 bindings
                 source-alist
                 label-alist
                 arities)
         (values `((truncate-values ,(length vars) ,(if rest 1 0)))
                 (open-binding bindings vars addr)
                 source-alist
                 label-alist
                 arities)))
    
    ((<glil-unbind>)
     (values '()
             (close-binding bindings addr)
             source-alist
             label-alist
             arities))
             
    ((<glil-source> props)
     (values '()
             bindings
             (acons addr props source-alist)
             label-alist
             arities))

    ((<glil-void>)
     (emit-code '((void))))

    ((<glil-const> obj)
     (cond
      ((object->assembly obj)
       => (lambda (code)
            (emit-code (list code))))
      ((vhash-assoc obj constants)
       => (lambda (pair)
            (emit-object-ref (cdr pair))))
      (else (error "const not in table" obj))))

    ((<glil-lexical> local? boxed? op index)
     (emit-code
      (if local?
          (if (< index 256)
              (case op
                ((ref) (if boxed?
                           `((local-boxed-ref ,index))
                           `((local-ref ,index))))
                ((set) (if boxed?
                           `((local-boxed-set ,index))
                           `((local-set ,index))))
                ((box) `((box ,index)))
                ((empty-box) `((empty-box ,index)))
                ((fix) `((fix-closure 0 ,index)))
                ((bound?) (if boxed?
                              `((local-ref ,index)
                                (variable-bound?))
                              `((local-bound? ,index))))
                (else (error "what" op)))
              (let ((a (quotient index 256))
                    (b (modulo index 256)))
                (case op
                  ((ref)
                   (if boxed?
                       `((long-local-ref ,a ,b)
                         (variable-ref))
                       `((long-local-ref ,a ,b))))
                  ((set)
                   (if boxed?
                       `((long-local-ref ,a ,b)
                         (variable-set))
                       `((long-local-set ,a ,b))))
                  ((box)
                   `((make-variable)
                     (variable-set)
                     (long-local-set ,a ,b)))
                  ((empty-box)
                   `((make-variable)
                     (long-local-set ,a ,b)))
                  ((fix)
                   `((fix-closure ,a ,b)))
                  ((bound?)
                   (if boxed?
                       `((long-local-ref ,a ,b)
                         (variable-bound?))
                       `((long-local-bound? ,a ,b))))
                  (else (error "what" op)))))
          `((,(case op
                ((ref) (if boxed? 'free-boxed-ref 'free-ref))
                ((set) (if boxed? 'free-boxed-set (error "what." glil)))
                (else (error "what" op)))
             ,index)))))
    
    ((<glil-toplevel> op name)
     (case op
       ((ref set)
        (cond
         ((and=> (vhash-assoc (make-variable-cache-cell name) constants)
                 cdr)
          => (lambda (i)
               (emit-code (if (< i 256)
                              `((,(case op
                                    ((ref) 'toplevel-ref)
                                    ((set) 'toplevel-set))
                                 ,i))
                              `((,(case op
                                    ((ref) 'long-toplevel-ref)
                                    ((set) 'long-toplevel-set))
                                 ,(quotient i 256)
                                 ,(modulo i 256)))))))
         (else
          (let ((i (or (and=> (vhash-assoc name constants) cdr)
                       (error "toplevel name not in objtable" name))))
            (emit-code `(,(if (< i 256)
                              `(object-ref ,i)
                              `(long-object-ref ,(quotient i 256)
                                                ,(modulo i 256)))
                         (link-now)
                         ,(case op
                            ((ref) '(variable-ref))
                            ((set) '(variable-set)))))))))
       ((define)
        (let ((i (or (and=> (vhash-assoc name constants) cdr)
                     (error "toplevel name not in objtable" name))))
          (emit-code `(,(if (< i 256)
                            `(object-ref ,i)
                            `(long-object-ref ,(quotient i 256)
                                              ,(modulo i 256)))
                       (define)))))
       (else
        (error "unknown toplevel var kind" op name))))

    ((<glil-module> op mod name public?)
     (let ((key (list mod name public?)))
       (case op
         ((ref set)
          (let ((i (or (and=> (vhash-assoc (make-variable-cache-cell key)
                                           constants) cdr)
                       (error "module vcache not in objtable" key))))
            (emit-code (if (< i 256)
                           `((,(case op
                                 ((ref) 'toplevel-ref)
                                 ((set) 'toplevel-set))
                              ,i))
                           `((,(case op
                                 ((ref) 'long-toplevel-ref)
                                 ((set) 'long-toplevel-set))
                              ,(quotient i 256)
                              ,(modulo i 256)))))))
         (else
          (error "unknown module var kind" op key)))))

    ((<glil-label> label)
     (let ((code (align-block addr)))
       (values code
               bindings
               source-alist
               (acons label (addr+ addr code) label-alist)
               arities)))

    ((<glil-branch> inst label)
     (emit-code `((,inst ,label))))

    ;; nargs is number of stack args to insn. probably should rename.
    ((<glil-call> inst nargs)
     (if (not (instruction? inst))
         (error "Unknown instruction:" inst))
     (let ((pops (instruction-pops inst)))
       (cond ((< pops 0)
              (case (instruction-length inst)
                ((1) (emit-code `((,inst ,nargs))))
                ((2) (emit-code `((,inst ,(quotient nargs 256)
                                         ,(modulo nargs 256)))))
                (else (error "Unknown length for variable-arg instruction:"
                             inst (instruction-length inst)))))
             ((= pops nargs)
              (emit-code `((,inst))))
             (else
              (error "Wrong number of stack arguments to instruction:" inst nargs)))))

    ((<glil-mv-call> nargs ra)
     (emit-code `((mv-call ,nargs ,ra))))

    ((<glil-prompt> label escape-only?)
     (emit-code `((prompt ,(if escape-only? 1 0) ,label))))))

(define (dump-object x addr)
  (define (too-long x)
    (error (string-append x " too long")))

  (cond
   ((object->assembly x) => list)
   ((variable-cache-cell? x) (dump-object (variable-cache-cell-key x) addr))
   ((number? x)
    `((load-number ,(number->string x))))
   ((string? x)
    (case (string-bytes-per-char x)
      ((1) `((load-string ,x)))
      ((4) (align-code `(load-wide-string ,x) addr 4 4))
      (else (error "bad string bytes per char" x))))
   ((symbol? x)
    (let ((str (symbol->string x)))
      (case (string-bytes-per-char str)
        ((1) `((load-symbol ,str)))
        ((4) `(,@(dump-object str addr)
               (make-symbol)))
        (else (error "bad string bytes per char" str)))))
   ((keyword? x)
    `(,@(dump-object (keyword->symbol x) addr)
      (make-keyword)))
   ((scheme-list? x)
    (let ((tail (let ((len (length x)))
                  (if (>= len 65536) (too-long "list"))
                  `((list ,(quotient len 256) ,(modulo len 256))))))
      (let dump-objects ((objects x) (codes '()) (addr addr))
        (if (null? objects)
            (fold append tail codes)
            (let ((code (dump-object (car objects) addr)))
              (dump-objects (cdr objects) (cons code codes)
                            (addr+ addr code)))))))
   ((pair? x)
    (let ((kar (dump-object (car x) addr)))
      `(,@kar
        ,@(dump-object (cdr x) (addr+ addr kar))
        (cons))))
   ((and (vector? x)
         (equal? (array-shape x) (list (list 0 (1- (vector-length x))))))
    (let* ((len (vector-length x))
           (tail (if (>= len 65536)
                     (too-long "vector")
                     `((vector ,(quotient len 256) ,(modulo len 256))))))
      (let dump-objects ((i 0) (codes '()) (addr addr))
        (if (>= i len)
            (fold append tail codes)
            (let ((code (dump-object (vector-ref x i) addr)))
              (dump-objects (1+ i) (cons code codes)
                            (addr+ addr code)))))))
   ((and (array? x) (symbol? (array-type x)))
    (let* ((type (dump-object (array-type x) addr))
           (shape (dump-object (array-shape x) (addr+ addr type))))
      `(,@type
        ,@shape
        ,@(align-code
           `(load-array ,(uniform-array->bytevector x))
           (addr+ (addr+ addr type) shape)
           8
           4))))
   ((array? x)
    ;; an array of generic scheme values
    (let* ((contents (array-contents x))
           (len (vector-length contents)))
      (let dump-objects ((i 0) (codes '()) (addr addr))
        (if (< i len)
            (let ((code (dump-object (vector-ref contents i) addr)))
              (dump-objects (1+ i) (cons code codes)
                            (addr+ addr code)))
            (fold append
                  `(,@(dump-object (array-shape x) addr)
                    (make-array ,(quotient (ash len -16) 256)
                                ,(logand #xff (ash len -8))
                                ,(logand #xff len)))
                  codes)))))
   (else
    (error "dump-object: unrecognized object" x))))

(define (dump-constants constants)
  (define (ref-or-dump x i addr)
    (let ((pair (vhash-assoc x constants)))
      (if (and pair (< (cdr pair) i))
          (let ((idx (cdr pair)))
            (if (< idx 256)
                (values `((object-ref ,idx))
                        (+ addr 2))
                (values `((long-object-ref ,(quotient idx 256)
                                           ,(modulo idx 256)))
                        (+ addr 3))))
          (dump1 x i addr))))
  (define (dump1 x i addr)
    (cond
     ((object->assembly x)
      => (lambda (code)
           (values (list code)
                   (+ (byte-length code) addr))))
     ((or (number? x)
          (string? x)
          (symbol? x)
          (keyword? x))
      ;; Atoms.
      (let ((code (dump-object x addr)))
        (values code (addr+ addr code))))
     ((variable-cache-cell? x)
      (dump1 (variable-cache-cell-key x) i addr))
     ((scheme-list? x)
      (receive (codes addr)
          (fold2 (lambda (x codes addr)
                   (receive (subcode addr) (ref-or-dump x i addr)
                     (values (cons subcode codes) addr)))
                 x '() addr)
        (values (fold append
                      (let ((len (length x)))
                        `((list ,(quotient len 256) ,(modulo len 256))))
                      codes)
                (+ addr 3))))
     ((pair? x)
      (receive (car-code addr) (ref-or-dump (car x) i addr)
        (receive (cdr-code addr) (ref-or-dump (cdr x) i addr)
          (values `(,@car-code ,@cdr-code (cons))
                  (1+ addr)))))
     ((and (vector? x)
           (<= (vector-length x) #xffff)
           (equal? (array-shape x) (list (list 0 (1- (vector-length x))))))
      (receive (codes addr)
          (vector-fold2 (lambda (x codes addr)
                          (receive (subcode addr) (ref-or-dump x i addr)
                            (values (cons subcode codes) addr)))
                        x '() addr)
        (values (fold append
                      (let ((len (vector-length x)))
                        `((vector ,(quotient len 256) ,(modulo len 256))))
                      codes)
                (+ addr 3))))
     ((and (array? x) (symbol? (array-type x)))
      (receive (type addr) (ref-or-dump (array-type x) i addr)
        (receive (shape addr) (ref-or-dump (array-shape x) i addr)
          (let ((bv (align-code `(load-array ,(uniform-array->bytevector x))
                                addr 8 4)))
            (values `(,@type ,@shape ,@bv)
                    (addr+ addr bv))))))
     ((array? x)
      (let ((contents (array-contents x)))
        (receive (codes addr)
            (vector-fold2 (lambda (x codes addr)
                            (receive (subcode addr) (ref-or-dump x i addr)
                              (values (cons subcode codes) addr)))
                          contents '() addr)
          (receive (shape addr) (ref-or-dump (array-shape x) i addr)
            (values (fold append
                          (let ((len (vector-length contents)))
                            `(,@shape
                              (make-array ,(quotient (ash len -16) 256)
                                          ,(logand #xff (ash len -8))
                                          ,(logand #xff len))))
                          codes)
                    (+ addr 4))))))
     (else
      (error "write-table: unrecognized object" x))))

  (receive (codes addr)
      (vhash-fold-right2 (lambda (obj idx code addr)
                           ;; The vector is on the stack.  Dup it, push
                           ;; the index, push the val, then vector-set.
                           (let ((pre `((dup)
                                        ,(object->assembly idx))))
                             (receive (valcode addr) (dump1 obj idx
                                                            (addr+ addr pre))
                               (values (cons* '((vector-set))
                                              valcode
                                              pre
                                              code)
                                       (1+ addr)))))
                         constants
                         '(((assert-nargs-ee/locals 1)
                            ;; Push the vector.
                            (local-ref 0)))
                         4)
    (let* ((len (1+ (vlist-length constants)))
           (pre-prog-addr (+ 2          ; reserve-locals
                             len 3      ; empty vector
                             2          ; local-set
                             1          ; new-frame
                             2          ; local-ref
                             ))
           (prog (align-program
                  `(load-program ()
                                 ,(+ addr 1)
                                 #f
                                 ;; The `return' will be at the tail of the
                                 ;; program.  The vector is already pushed
                                 ;; on the stack.
                                 . ,(fold append '((return)) codes))
                  pre-prog-addr)))
      (values `(;; Reserve storage for the vector.
                (assert-nargs-ee/locals ,(logior 0 (ash 1 3)))
                ;; Push the vector, and store it in slot 0.
                ,@(make-list len '(make-false))
                (vector ,(quotient len 256) ,(modulo len 256))
                (local-set 0)
                ;; Now we open the call frame.
                ;;
                (new-frame)
                ;; Now build a thunk to init the constants.  It will
                ;; have the unfinished constant table both as its
                ;; argument and as its objtable.  The former allows it
                ;; to update the objtable, with vector-set!, and the
                ;; latter allows init code to refer to previously set
                ;; values.
                ;;
                ;; Grab the vector, to be the objtable.
                (local-ref 0)
                ;; Now the load-program, properly aligned.  Pops the vector.
                ,@prog
                ;; Grab the vector, as an argument this time.
                (local-ref 0)
                ;; Call the init thunk with the vector as an arg.
                (call 1)
                ;; The thunk also returns the vector.  Leave it on the
                ;; stack for compile-assembly to use.
                )
              ;; The byte length of the init code, which we can
              ;; determine without folding over the code again.
              (+ (addr+ pre-prog-addr prog) ; aligned program
                 2 ; local-ref
                 2 ; call
                 )))))
