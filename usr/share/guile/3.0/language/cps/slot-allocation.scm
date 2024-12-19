;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2021,2023 Free Software Foundation, Inc.

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
;;; A module to assign stack slots to variables in a CPS term.
;;;
;;; Code:

(define-module (language cps slot-allocation)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:use-module (language cps graphs)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:export (allocate-slots
            lookup-slot
            lookup-maybe-slot
            lookup-representation
            lookup-nlocals
            lookup-call-proc-slot
            lookup-send-parallel-moves
            lookup-receive-parallel-moves
            lookup-slot-map))

(define-record-type $allocation
  (make-allocation slots representations call-allocs shuffles frame-size)
  allocation?

  ;; A map of VAR to slot allocation.  A slot allocation is an integer,
  ;; if the variable has been assigned a slot.
  ;;
  (slots allocation-slots)

  ;; A map of VAR to representation.  A representation is 'scm, 'f64,
  ;; 'u64, 's64, 'ptr, 'bv-contents, or 'code.
  ;;
  (representations allocation-representations)

  ;; A map of LABEL to /call allocs/, for non-tail $call/$callk, and for
  ;; $prompt.
  ;;
  ;; A call alloc contains two pieces of information: the call's /proc
  ;; slot/ and a /dead slot map/.  The proc slot indicates the slot of a
  ;; procedure in a procedure call, or where the procedure would be in a
  ;; multiple-value return.
  ;;
  ;; The dead slot map indicates, what slots should be ignored by GC
  ;; when marking the frame.  A dead slot map is a bitfield, as an
  ;; integer.
  ;;
  (call-allocs allocation-call-allocs)

  ;; A map of LABEL to /parallel moves/.  Parallel moves shuffle locals
  ;; into position for a $call, $callk, $calli, or $values, or shuffle
  ;; returned values back into place for a $kreceive.
  ;;
  ;; A set of moves is expressed as an ordered list of (SRC . DST)
  ;; moves, where SRC and DST are slots.  This may involve a temporary
  ;; variable.
  ;;
  (shuffles allocation-shuffles)

  ;; The number of local slots needed for this function.  Because we can
  ;; contify common clause tails, we use one frame size for all clauses
  ;; to avoid having to adjust the frame size when continuing to labels
  ;; from other clauses.
  ;;
  (frame-size allocation-frame-size))

(define-record-type $call-alloc
  (make-call-alloc proc-slot slot-map)
  call-alloc?
  (proc-slot call-alloc-proc-slot)
  (slot-map call-alloc-slot-map))

(define (lookup-maybe-slot var allocation)
  (intmap-ref (allocation-slots allocation) var (lambda (_) #f)))

(define (lookup-slot var allocation)
  (intmap-ref (allocation-slots allocation) var))

(define (lookup-representation var allocation)
  (intmap-ref (allocation-representations allocation) var))

(define *absent* (list 'absent))

(define (lookup-call-alloc k allocation)
  (intmap-ref (allocation-call-allocs allocation) k))

(define (lookup-call-proc-slot k allocation)
  (or (call-alloc-proc-slot (lookup-call-alloc k allocation))
      (error "Call has no proc slot" k)))

(define (lookup-send-parallel-moves k allocation)
  (match (intmap-ref (allocation-shuffles allocation) k)
    ((send . receive) send)))

(define (lookup-receive-parallel-moves k allocation)
  (match (intmap-ref (allocation-shuffles allocation) k)
    ((send . receive) receive)))

(define (lookup-slot-map k allocation)
  (or (call-alloc-slot-map (lookup-call-alloc k allocation))
      (error "Call has no slot map" k)))

(define (lookup-nlocals allocation)
  (allocation-frame-size allocation))

(define* (add-prompt-control-flow-edges conts succs #:key complete?)
  "For all prompts in DFG in the range [MIN-LABEL, MIN-LABEL +
LABEL-COUNT), invoke F with arguments PROMPT, HANDLER, and BODY for each
body continuation in the prompt."
  (define (intset-filter pred set)
    (intset-fold (lambda (i set)
                   (if (pred i) set (intset-remove set i)))
                 set
                 set))
  (define (intset-any pred set)
    (intset-fold (lambda (i res)
                   (if (or res (pred i)) #t res))
                 set
                 #f))
  (define (compute-prompt-body label)
    (persistent-intset
     (let visit-cont ((label label) (level 1) (labels empty-intset))
       (cond
        ((zero? level) labels)
        ((intset-ref labels label) labels)
        (else
         (let ((labels (intset-add! labels label)))
           (match (intmap-ref conts label)
             (($ $kreceive arity k) (visit-cont k level labels))
             (($ $kargs names syms ($ $continue k src ($ $primcall 'wind)))
              (visit-cont k (1+ level) labels))
             (($ $kargs names syms ($ $continue k src ($ $primcall 'unwind)))
              (visit-cont k (1- level) labels))
             (($ $kargs names syms ($ $continue k src exp))
              (visit-cont k level labels))
             (($ $kargs names syms ($ $branch kf kt))
              (visit-cont kf level (visit-cont kt level labels)))
             (($ $kargs names syms ($ $switch kf kt*))
              (fold1 (lambda (label labels)
                       (visit-cont label level labels))
                     (cons kf kt*) labels))
             (($ $kargs names syms ($ $prompt k kh src escape? tag))
              (visit-cont kh level (visit-cont k (1+ level) labels)))
             (($ $kargs names syms ($ $throw)) labels))))))))
  (define (visit-prompt label handler succs)
    (let ((body (compute-prompt-body label)))
      (define (out-or-back-edge? label)
        ;; Most uses of visit-prompt-control-flow don't need every body
        ;; continuation, and would be happy getting called only for
        ;; continuations that postdominate the rest of the body.  Unless
        ;; you pass #:complete? #t, we only invoke F on continuations
        ;; that can leave the body, or on back-edges in loops.
        (not (intset-any (lambda (succ)
                           (and (intset-ref body succ) (< label succ)))
                         (intmap-ref succs label))))
      (intset-fold (lambda (pred succs)
                     (intmap-replace succs pred handler intset-add))
                   (if complete? body (intset-filter out-or-back-edge? body))
                   succs)))
  (intmap-fold
   (lambda (label cont succs)
     (match cont
       (($ $kargs _ _ ($ $prompt k kh))
        (visit-prompt k kh succs))
       (_ succs)))
   conts
   succs))

(define (compute-needs-slot cps defs uses)
  (define (get-defs k) (intmap-ref defs k))
  (define (get-uses label) (intmap-ref uses label))
  (intmap-fold
   (lambda (label cont needs-slot)
     (intset-union
      needs-slot
      (match cont
        (($ $kargs)
         (intset-union (get-defs label) (get-uses label)))
        (($ $kreceive arity k)
         ;; Only allocate results of function calls to slots if they are
         ;; used.
         empty-intset)
        (($ $kclause arity body alternate)
         (get-defs label))
        (($ $kfun src meta self)
         (get-defs label))
        (($ $ktail)
         empty-intset))))
   cps
   empty-intset))

(define (compute-lazy-vars cps live-in live-out defs needs-slot)
  "Compute and return a set of vars whose allocation can be delayed
until their use is seen.  These are \"lazy\" vars.  A var is lazy if its
uses are calls, it is always dead after the calls, and if the uses flow
to the definition.  A flow continues across a node iff the node kills no
values that need slots, and defines only lazy vars.  Calls also kill
flows; there's no sense in trying to juggle a pending frame while there
is an active call."
  (define (list->intset list)
    (persistent-intset
     (fold (lambda (i set) (intset-add! set i)) empty-intset list)))

  (let* ((succs (compute-successors cps))
         (gens (intmap-map
                (lambda (label cont)
                  (match cont
                    (($ $kargs _ _ ($ $continue _ _ ($ $call proc args)))
                     (intset-subtract (intset-add (list->intset args) proc)
                                      (intmap-ref live-out label)))
                    (($ $kargs _ _ ($ $continue _ _ ($ $callk _ proc args)))
                     (let ((args (list->intset args)))
                       (intset-subtract (if proc (intset-add args proc) args)
                                        (intmap-ref live-out label))))
                    (($ $kargs _ _ ($ $continue _ _ ($ $calli args callee)))
                     (intset-subtract (list->intset (cons callee args))
                                      (intmap-ref live-out label)))
                    (($ $kargs _ _ ($ $continue k _($ $values args)))
                     (match (intmap-ref cps k)
                       (($ $ktail) (list->intset args))
                       (_ #f)))
                    (_ #f)))
                cps))
         (kills (intmap-map
                 (lambda (label in)
                   (let* ((out (intmap-ref live-out label))
                          (killed (intset-subtract in out))
                          (killed-slots (intset-intersect killed needs-slot)))
                     (and (eq? killed-slots empty-intset)
                          ;; Kill output variables that need slots.
                          (intset-intersect (intmap-ref defs label)
                                            needs-slot))))
                 live-in))
         (preds (invert-graph succs))
         (old->new (compute-reverse-control-flow-order preds)))
    (define (subtract lazy kill)
      (cond
       ((eq? lazy empty-intset)
        lazy)
       ((not kill)
        empty-intset)
       ((and lazy (eq? empty-intset (intset-subtract kill lazy)))
        (intset-subtract lazy kill))
       (else
        empty-intset)))
    (define (add live gen) (or gen live))
    (define (meet in out)
      ;; Initial in is #f.
      (if in (intset-intersect in out) out))
    (call-with-values
        (lambda ()
          (let ((succs (rename-graph preds old->new))
                (init (persistent-intmap
                       (intmap-fold
                        (lambda (old new in)
                          (intmap-add! in new #f))
                        old->new empty-intmap)))
                (kills (rename-keys kills old->new))
                (gens (rename-keys gens old->new)))
            (solve-flow-equations succs init init kills gens
                                  subtract add meet)))
      (lambda (in out)
        ;; A variable is lazy if its uses reach its definition.
        (intmap-fold (lambda (label out lazy)
                       (match (intmap-ref cps label)
                         (($ $kargs names vars)
                          (let ((defs (list->intset vars)))
                            (intset-union lazy (intset-intersect out defs))))
                         (_ lazy)))
                     (rename-keys out (invert-bijection old->new))
                     empty-intset)))))

(define (find-first-zero n)
  ;; Naive implementation.
  (let lp ((slot 0))
    (if (logbit? slot n)
        (lp (1+ slot))
        slot)))

(define (find-first-trailing-zero n)
  (let lp ((slot (let lp ((count 2))
                   (if (< n (ash 1 (1- count)))
                       count
                       ;; Grow upper bound slower than factor 2 to avoid
                       ;; needless bignum allocation on 32-bit systems
                       ;; when there are more than 16 locals.
                       (lp (+ count (ash count -1)))))))
    (if (or (zero? slot) (logbit? (1- slot) n))
        slot
        (lp (1- slot)))))

(define (integers from count)
  (if (zero? count)
      '()
      (cons from (integers (1+ from) (1- count)))))

(define (solve-parallel-move src dst tmp)
  "Solve the parallel move problem between src and dst slot lists, which
are comparable with eqv?.  A tmp slot may be used."

  ;; This algorithm is taken from: "Tilting at windmills with Coq:
  ;; formal verification of a compilation algorithm for parallel moves"
  ;; by Laurence Rideau, Bernard Paul Serpette, and Xavier Leroy
  ;; <http://gallium.inria.fr/~xleroy/publi/parallel-move.pdf>

  (define (split-move moves reg)
    (let loop ((revhead '()) (tail moves))
      (match tail
        (((and s+d (s . d)) . rest)
         (if (eqv? s reg)
             (cons d (append-reverse revhead rest))
             (loop (cons s+d revhead) rest)))
        (_ #f))))

  (define (replace-last-source reg moves)
    (match moves
      ((moves ... (s . d))
       (append moves (list (cons reg d))))))

  (let loop ((to-move (map cons src dst))
             (being-moved '())
             (moved '())
             (last-source #f))
    ;; 'last-source' should always be equivalent to:
    ;; (and (pair? being-moved) (car (last being-moved)))
    (match being-moved
      (() (match to-move
            (() (reverse moved))
            (((and s+d (s . d)) . t1)
             (if (or (eqv? s d) ; idempotent
                     (not s))   ; src is a constant and can be loaded directly
                 (loop t1 '() moved #f)
                 (loop t1 (list s+d) moved s)))))
      (((and s+d (s . d)) . b)
       (match (split-move to-move d)
         ((r . t1) (loop t1 (acons d r being-moved) moved last-source))
         (#f (match b
               (() (loop to-move '() (cons s+d moved) #f))
               (_ (if (eqv? d last-source)
                      (loop to-move
                            (replace-last-source tmp b)
                            (cons s+d (acons d tmp moved))
                            tmp)
                      (loop to-move b (cons s+d moved) last-source))))))))))

(define-inlinable (add-live-slot slot live-slots)
  (logior live-slots (ash 1 slot)))

(define-inlinable (kill-dead-slot slot live-slots)
  (logand live-slots (lognot (ash 1 slot))))

(define-inlinable (compute-slot live-slots hint)
  (if (and hint (not (logbit? hint live-slots)))
      hint
      (find-first-zero live-slots)))

(define (compute-shuffles cps slots call-allocs live-in)
  (define (get-cont label)
    (intmap-ref cps label))

  (define (get-slot var)
    (intmap-ref slots var (lambda (_) #f)))

  (define (get-slots vars)
    (let lp ((vars vars))
      (match vars
        ((var . vars) (cons (get-slot var) (lp vars)))
        (_ '()))))

  (define (get-proc-slot label)
    (call-alloc-proc-slot (intmap-ref call-allocs label)))

  (define (compute-live-slots label)
    (intset-fold (lambda (var live)
                   (match (get-slot var)
                     (#f live)
                     (slot (add-live-slot slot live))))
                 (intmap-ref live-in label)
                 0))

  ;; Although some parallel moves may proceed without a temporary slot,
  ;; in general one is needed.  That temporary slot must not be part of
  ;; the source or destination sets, and that slot should not correspond
  ;; to a live variable.  Usually the source and destination sets are a
  ;; subset of the union of the live sets before and after the move.
  ;; However for stack slots that don't have names -- those slots that
  ;; correspond to function arguments or to function return values -- it
  ;; could be that they are out of the computed live set.  In that case
  ;; they need to be adjoined to the live set, used when choosing a
  ;; temporary slot.
  (define (compute-tmp-slot live stack-slots)
    (find-first-zero (fold add-live-slot live stack-slots)))

  (define (parallel-move src-slots dst-slots tmp-slot)
    (solve-parallel-move src-slots dst-slots tmp-slot))

  ;; A term can have two sets of shuffles: one set to shuffle operands
  ;; to the term (the "send moves"), and one set to shuffle results (the
  ;; "receive moves").  An example of send moves would be a call getting
  ;; its arguments into position, or a $values performing a parallel
  ;; move.  Receive moves come when binding call results to values, for
  ;; local returns (call returns) or non-local returns (prompt
  ;; handlers).
  (define (add-shuffles shuffles label send-moves receive-moves)
    (intmap-add! shuffles label (cons send-moves receive-moves)))

  (define (compute-receive-shuffles k proc-slot)
    (match (get-cont k)
      (($ $kreceive arity kargs)
       (compute-receive-shuffles kargs proc-slot))
      (($ $kargs names results)
       (let* ((value-slots (integers proc-slot (length results)))
              (result-slots (get-slots results))
              ;; Filter out unused results.
              (value-slots (filter-map (lambda (val result) (and result val))
                                       value-slots result-slots))
              (result-slots (filter (lambda (x) x) result-slots))
              (live (compute-live-slots k)))
         (parallel-move value-slots
                        result-slots
                        (compute-tmp-slot live value-slots))))))
    
  (define (add-call-shuffles label k args shuffles)
    (match (get-cont k)
      (($ $ktail)
       (let* ((live (compute-live-slots label))
              (tail-slots (integers 0 (length args)))
              (send-moves (parallel-move (get-slots args)
                                         tail-slots
                                         (compute-tmp-slot live tail-slots))))
         (add-shuffles shuffles label send-moves '())))
      ((or ($ $kargs) ($ $kreceive))
       (let* ((live (compute-live-slots label))
              (proc-slot (get-proc-slot label))
              (call-slots (integers proc-slot (length args)))
              (send-moves (parallel-move (get-slots args)
                                         call-slots
                                         (compute-tmp-slot live call-slots)))
              (receive-moves (compute-receive-shuffles k proc-slot)))
         (add-shuffles shuffles label send-moves receive-moves)))))
    
  (define (add-values-shuffles label k args shuffles)
    (match (get-cont k)
      (($ $ktail)
       (let* ((live (compute-live-slots label))
              (src-slots (get-slots args))
              (dst-slots (integers 0 (length args)))
              (send-moves (parallel-move src-slots dst-slots
                                         (compute-tmp-slot live dst-slots))))
         (add-shuffles shuffles label send-moves '())))
      (($ $kargs _ dst-vars)
       (let* ((live (logior (compute-live-slots label)
                            (compute-live-slots k)))
              (src-slots (get-slots args))
              (dst-slots (get-slots dst-vars))
              (send-moves (parallel-move src-slots dst-slots
                                         (compute-tmp-slot live '()))))
         (add-shuffles shuffles label send-moves '())))))

  (define (add-prompt-shuffles label k handler shuffles)
    (define receive-moves
      (compute-receive-shuffles handler (get-proc-slot label)))
    (add-shuffles shuffles label '() receive-moves))

  (define (compute-shuffles label cont shuffles)
    (match cont
      (($ $kargs names vars ($ $continue k src exp))
       (match exp
         (($ $call proc args)
          (add-call-shuffles label k (cons proc args) shuffles))
         (($ $callk _ proc args)
          (add-call-shuffles label k (if proc (cons proc args) args) shuffles))
         (($ $calli args callee)
          (add-call-shuffles label k (append args (list callee)) shuffles))
         (($ $values args)
          (add-values-shuffles label k args shuffles))
         (_ shuffles)))
      (($ $kargs names vars ($ $prompt k kh src escape? tag))
       (add-prompt-shuffles label k kh shuffles))
      (_ shuffles)))

  (persistent-intmap
   (intmap-fold compute-shuffles cps empty-intmap)))

(define (compute-frame-size cps slots call-allocs shuffles)
  ;; Minimum frame has one slot: the closure.
  (define minimum-frame-size 1)
  (define (get-shuffles label)
    (intmap-ref shuffles label))
  (define (get-proc-slot label)
    (match (intmap-ref call-allocs label (lambda (_) #f))
      (#f 0) ;; Tail call.
      (($ $call-alloc proc-slot) proc-slot)))
  (define (max-size var size)
    (match (intmap-ref slots var (lambda (_) #f))
      (#f size)
      (slot (max size (1+ slot)))))
  (define (max-size* vars size)
    (fold max-size size vars))
  (define (shuffle-size* moves size)
    (match moves
      (() size)
      (((src . dst) . moves)
       (shuffle-size* moves (max size (1+ src) (1+ dst))))))
  (define (shuffle-size send+receive size)
    (match send+receive
      ((send . receive) (shuffle-size* send (shuffle-size* receive size)))))
  (define (call-size label nargs size)
    (shuffle-size (get-shuffles label)
                  (max (+ (get-proc-slot label) nargs) size)))
  (define (measure-cont label cont size)
    (match cont
      (($ $kargs names vars term)
       (let ((size (max-size* vars size)))
         (match term
           (($ $continue _ _ ($ $call proc args))
            (call-size label (1+ (length args)) size))
           (($ $continue _ _ ($ $callk _ proc args))
            (let ((nclosure (if proc 1 0)))
              (call-size label (+ nclosure (length args)) size)))
           (($ $continue _ _ ($ $calli args callee))
            (call-size label (1+ (length args)) size))
           (($ $continue _ _ ($ $values args))
            (shuffle-size (get-shuffles label) size))
           (($ $prompt)
            (shuffle-size (get-shuffles label) size))
           (_ size))))
      (_ size)))

  (intmap-fold measure-cont cps minimum-frame-size))

(define (allocate-args cps)
  (define (add-clause entry first-slot slots)
    (match (intmap-ref cps entry)
      (($ $kclause arity body alt)
       (let ((slots (add-clause body first-slot slots)))
         (if alt
             (add-clause alt first-slot slots)
             slots)))
      (($ $kargs names vars)
       (let lp ((vars vars) (n first-slot) (slots slots))
         (match vars
           (() slots)
           ((var . vars)
            (lp vars
                (1+ n)
                (intmap-add slots var n))))))))
  (match (intmap-ref cps (intmap-next cps))
    (($ $kfun src meta self tail entry)
     (add-clause
      entry
      (if self 1 0)
      (if self
          (intmap-add empty-intmap self 0)
          empty-intmap)))))

(define (allocate-lazy-vars cps slots call-allocs live-in lazy)
  (define (compute-live-slots slots label)
    (intset-fold (lambda (var live)
                   (match (intmap-ref slots var (lambda (_) #f))
                     (#f live)
                     (slot (add-live-slot slot live))))
                 (intmap-ref live-in label)
                 0))

  (define (allocate var hint slots live)
    (match (and hint (intmap-ref slots var (lambda (_) #f)))
      (#f (if (intset-ref lazy var)
              (let ((slot (compute-slot live hint)))
                (values (intmap-add! slots var slot)
                        (add-live-slot slot live)))
              (values slots live)))
      (slot (values slots (add-live-slot slot live)))))

  (define (allocate* vars hints slots live)
    (match (vector vars hints)
      (#(() ()) slots)
      (#((var . vars) (hint . hints))
       (let-values (((slots live) (allocate var hint slots live)))
         (allocate* vars hints slots live)))))

  (define (get-proc-slot label)
    (match (intmap-ref call-allocs label (lambda (_) #f))
      (#f 0)
      (call (call-alloc-proc-slot call))))

  (define (allocate-call label args slots)
    (allocate* args (integers (get-proc-slot label) (length args))
               slots (compute-live-slots slots label)))

  (define (allocate-values label k args slots)
    (match (intmap-ref cps k)
      (($ $ktail)
       (allocate* args (integers 0 (length args))
                  slots (compute-live-slots slots label)))
      (($ $kargs names vars)
       (allocate* args
                  (map (cut intmap-ref slots <> (lambda (_) #f)) vars)
                  slots (compute-live-slots slots label)))))

  (define (allocate-lazy label cont slots)
    (match cont
      (($ $kargs names vars ($ $continue k src exp))
       (match exp
         (($ $call proc args)
          (allocate-call label (cons proc args) slots))
         (($ $callk _ proc args)
          (allocate-call label (if proc (cons proc args) args) slots))
         (($ $calli args callee)
          (allocate-call label (append args (list callee)) slots))
         (($ $values args)
          (allocate-values label k args slots))
         (_ slots)))
      (_
       slots)))

  ;; Sweep right to left to visit uses before definitions.
  (persistent-intmap
   (intmap-fold-right allocate-lazy cps slots)))

(define* (allocate-slots cps #:key (precolor-calls? #t))
  (let*-values (((defs uses) (compute-defs-and-uses cps))
                ((representations) (compute-var-representations cps))
                ((live-in live-out)
                 (let* ((succs (compute-successors cps))
                        (succs+ (add-prompt-control-flow-edges cps succs))
                        (preds (invert-graph succs+)))
                   (compute-live-variables preds defs uses)))
                ((needs-slot) (compute-needs-slot cps defs uses))
                ((lazy) (if precolor-calls?
                            (compute-lazy-vars cps live-in live-out defs
                                               needs-slot)
                            empty-intset)))

    (define frame-size 3)

    (define (empty-live-slots)
      #b0)

    (define (compute-call-proc-slot live-slots)
      (+ frame-size (find-first-trailing-zero live-slots)))

    (define (compute-prompt-handler-proc-slot live-slots)
      (find-first-trailing-zero live-slots))

    (define (get-cont label)
      (intmap-ref cps label))

    (define (get-slot slots var)
      (intmap-ref slots var (lambda (_) #f)))

    (define (get-slots slots vars)
      (let lp ((vars vars))
        (match vars
          ((var . vars) (cons (get-slot slots var) (lp vars)))
          (_ '()))))

    (define (compute-live-slots* slots label live-vars)
      (intset-fold (lambda (var live)
                     (match (get-slot slots var)
                       (#f live)
                       (slot (add-live-slot slot live))))
                   (intmap-ref live-vars label)
                   0))

    (define (compute-live-in-slots slots label)
      (compute-live-slots* slots label live-in))

    (define (compute-live-out-slots slots label)
      (compute-live-slots* slots label live-out))

    (define slot-desc-dead 0)
    (define slot-desc-live-raw 1)
    (define slot-desc-live-scm 2)
    (define slot-desc-unused 3)

    (define (compute-slot-map slots live-vars nslots)
      (intset-fold
       (lambda (var slot-map)
         (match (get-slot slots var)
           (#f slot-map)
           (slot
            (let ((desc (match (intmap-ref representations var)
                          ((or 'u64 'f64 's64 'ptr 'bv-contents 'code)
                           slot-desc-live-raw)
                          ('scm
                           slot-desc-live-scm))))
              (logior slot-map (ash desc (* 2 slot)))))))
       live-vars 0))

    (define (allocate var hint slots live)
      (cond
       ((not (intset-ref needs-slot var))
        (values slots live))
       ((get-slot slots var)
        => (lambda (slot)
             (values slots (add-live-slot slot live))))
       ((and (not hint) (intset-ref lazy var))
        (values slots live))
       (else
        (let ((slot (compute-slot live hint)))
          (values (intmap-add! slots var slot)
                  (add-live-slot slot live))))))

    (define (allocate* vars hints slots live)
      (match (vector vars hints)
        (#(() ()) (values slots live))
        (#((var . vars) (hint . hints))
         (call-with-values (lambda () (allocate var hint slots live))
           (lambda (slots live)
             (allocate* vars hints slots live))))))

    (define (allocate-defs label vars slots)
      (let ((live (compute-live-in-slots slots label))
            (live-vars (intmap-ref live-in label)))
        (let lp ((vars vars) (slots slots) (live live))
          (match vars
            (() (values slots live))
            ((var . vars)
             (call-with-values (lambda () (allocate var #f slots live))
               (lambda (slots live)
                 (lp vars slots
                     (let ((slot (get-slot slots var)))
                       (if (and slot (not (intset-ref live-vars var)))
                           (kill-dead-slot slot live)
                           live))))))))))

    ;; PRE-LIVE are the live slots coming into the term.  POST-LIVE
    ;; is the subset of PRE-LIVE that is still live after the term
    ;; uses its inputs.
    (define (allocate-call label k args slots call-allocs pre-live)
      (match (get-cont k)
        (($ $ktail)
         (let ((tail-slots (integers 0 (length args))))
           (values (allocate* args tail-slots slots pre-live)
                   call-allocs)))
        (($ $kreceive arity kargs)
         (allocate-call label kargs args slots call-allocs pre-live))
        (($ $kargs names results)
         (let*-values
             (((post-live) (compute-live-out-slots slots label))
              ((proc-slot) (compute-call-proc-slot post-live))
              ((call-slots) (integers proc-slot (length args)))
              ((slots pre-live) (allocate* args call-slots slots pre-live))
              ;; Allow the first result to be hinted by its use, but
              ;; hint the remaining results to stay in place.  This
              ;; strikes a balance between avoiding shuffling,
              ;; especially for unused extra values, and avoiding frame
              ;; size growth due to sparse locals.
              ((slots result-live)
               (match results
                 (()
                  (values slots post-live))
                 ((_ . results*)
                  (let ((result-slots (integers (+ proc-slot 1)
                                                (length results*))))
                    (allocate* results* result-slots slots post-live)))))
              ((slot-map) (compute-slot-map slots (intmap-ref live-out label)
                                            (- proc-slot frame-size)))
              ((call) (make-call-alloc proc-slot slot-map)))
           (values slots
                   (intmap-add! call-allocs label call))))))
    
    (define (allocate-values label k args slots call-allocs)
      (match (get-cont k)
        (($ $ktail)
         (values slots call-allocs))
        (($ $kargs (_) (dst))
         ;; When there is only one value in play, we allow the dst to be
         ;; hinted (see compute-lazy-vars).  If the src doesn't have a
         ;; slot, then the actual slot for the dst would end up being
         ;; decided by the call that args it.  Because we don't know the
         ;; slot, we can't really compute the parallel moves in that
         ;; case, so just bail and rely on the bytecode emitter to
         ;; handle the one-value case specially.
         (match args
           ((src)
            (let ((post-live (compute-live-out-slots slots label)))
              (values (allocate dst (get-slot slots src) slots post-live)
                      call-allocs)))))
        (($ $kargs _ dst-vars)
         (let ((src-slots (get-slots slots args))
               (post-live (compute-live-out-slots slots label)))
           (values (allocate* dst-vars src-slots slots post-live)
                   call-allocs)))))

    (define (allocate-prompt label k handler slots call-allocs)
      (match (get-cont handler)
        (($ $kreceive arity kargs)
         (let*-values
             (((handler-live) (compute-live-in-slots slots handler))
              ((proc-slot) (compute-prompt-handler-proc-slot handler-live))
              ((slot-map)  (compute-slot-map slots (intmap-ref live-in handler)
                                             (- proc-slot frame-size)))
              ((result-vars) (match (get-cont kargs)
                               (($ $kargs names vars) vars)))
              ((value-slots) (integers proc-slot (length result-vars)))
              ((slots result-live) (allocate* result-vars value-slots
                                              slots handler-live)))
           (values slots
                   (intmap-add! call-allocs label
                                (make-call-alloc proc-slot slot-map)))))))

    (define (allocate-cont label cont slots call-allocs)
      (match cont
        (($ $kargs names vars term)
         (let-values (((slots live) (allocate-defs label vars slots)))
           (match term
             (($ $continue k src ($ $call proc args))
              (allocate-call label k (cons proc args) slots call-allocs live))
             (($ $continue k src ($ $callk _ proc args))
              (allocate-call label k (if proc (cons proc args) args)
                             slots call-allocs live))
             (($ $continue k src ($ $calli args callee))
              (allocate-call label k (append args (list callee))
                             slots call-allocs live))
             (($ $continue k src ($ $values args))
              (allocate-values label k args slots call-allocs))
             (($ $prompt k kh src escape? tag)
              (allocate-prompt label k kh slots call-allocs))
             (_
              (values slots call-allocs)))))
        (_
         (values slots call-allocs))))

    (call-with-values (lambda ()
                        (let ((slots (allocate-args cps)))
                          (intmap-fold allocate-cont cps slots empty-intmap)))
      (lambda (slots calls)
        (let* ((slots (allocate-lazy-vars cps slots calls live-in lazy))
               (shuffles (compute-shuffles cps slots calls live-in))
               (frame-size (compute-frame-size cps slots calls shuffles)))
          (make-allocation slots representations calls shuffles frame-size))))))
