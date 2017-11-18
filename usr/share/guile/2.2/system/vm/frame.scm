;;; Guile VM frame functions

;;; Copyright (C) 2001, 2005, 2009-2016 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (system vm frame)
  #:use-module (system base pmatch)
  #:use-module (system foreign)
  #:use-module (system vm program)
  #:use-module (system vm debug)
  #:use-module (system vm disassembler)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:export (binding-index
            binding-name
            binding-slot
            binding-representation

            frame-bindings
            frame-lookup-binding
            binding-ref binding-set!

            frame-instruction-pointer-or-primitive-procedure-name
            frame-call-representation
            frame-environment
            frame-object-binding frame-object-name))

(eval-when (expand compile load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_frames_builtins"))

(define-record-type <binding>
  (make-binding frame idx name slot representation)
  binding?
  (frame binding-frame)
  (idx binding-index)
  (name binding-name)
  (slot binding-slot)
  (representation binding-representation))

(define (parse-code code)
  (let ((len (bytevector-length code)))
    (let lp ((pos 0) (out '()))
      (cond
       ((< pos len)
        (let* ((inst-len (instruction-length code pos))
               (pos (+ pos inst-len)))
          (unless (<= pos len)
            (error "Failed to parse codestream"))
          (lp pos (cons inst-len out))))
       (else
        (list->vector (reverse out)))))))

(define (compute-predecessors code parsed)
  (let ((preds (make-vector (vector-length parsed) '())))
    (define (add-pred! from target)
      (let lp ((to from) (target target))
        (cond
         ((negative? target)
          (lp (1- to) (+ target (vector-ref parsed (1- to)))))
         ((positive? target)
          (lp (1+ to) (- target (vector-ref parsed to))))
         ((= to (vector-length preds))
          ;; This can happen when an arity fails to match.  Just ignore
          ;; this case.
          #t)
         (else
          (vector-set! preds to (cons from (vector-ref preds to)))))))
    (let lp ((n 0) (pos 0))
      (when (< n (vector-length preds))
        (when (instruction-has-fallthrough? code pos)
          (add-pred! n (vector-ref parsed n)))
        (for-each (lambda (target)
                    (add-pred! n target))
                  (instruction-relative-jump-targets code pos))
        (lp (1+ n) (+ pos (vector-ref parsed n)))))
    preds))

(define (compute-frame-sizes code parsed initial-size)
  (let ((in-sizes (make-vector (vector-length parsed) #f))
        (out-sizes (make-vector (vector-length parsed) #f)))
    ;; This only computes all possible valid stack sizes if the bytecode
    ;; is sorted topologically.  Guiles' compiler does this currently,
    ;; but if that changes we should do a proper pre-order visit.  Of
    ;; course the bytecode has to be valid too.
    (define (find-idx n diff)
      (let lp ((n n) (diff diff))
        (cond
         ((= n (vector-length parsed))
          ;; Possible for jumps to alternate arities.
          #f)
         ((negative? diff)
          (lp (1- n) (+ diff (vector-ref parsed (1- n)))))
         ((positive? diff)
          (lp (1+ n) (- diff (vector-ref parsed n))))
         (else n))))
    (vector-set! in-sizes 0 initial-size)
    (let lp ((n 0) (pos 0))
      (define (offset->idx target)
        (call-with-values (lambda ()
                            (if (>= target pos)
                                (values n pos)
                                (values 0 0)))
          (lambda (n pos)
            (let lp ((n n) (pos pos))
              (cond
               ((= pos target) n)
               ((< pos target) (lp (1+ n) (+ pos (vector-ref parsed n))))
               (else (error "bad target" target)))))))
      (when (< n (vector-length parsed))
        (let* ((in (vector-ref in-sizes n))
               (out (instruction-stack-size-after code pos in)))
          (vector-set! out-sizes n out)
          (when out
            (when (instruction-has-fallthrough? code pos)
              (vector-set! in-sizes (1+ n) out))
            (for-each (lambda (target)
                        (let ((idx (find-idx n target)))
                          (when idx
                            (vector-set! in-sizes idx out))))
                      (instruction-relative-jump-targets code pos))))
        (lp (1+ n) (+ pos (vector-ref parsed n)))))
    (values in-sizes out-sizes)))

(define (compute-genv parsed defs)
  (let ((genv (make-vector (vector-length parsed) '())))
    (define (add-def! pos var)
      (vector-set! genv pos (cons var (vector-ref genv pos))))
    (let lp ((var 0) (pos 0) (pc-offset 0))
      (when (< var (vector-length defs))
        (match (vector-ref defs var)
          (#(name offset slot representation)
           (when (< offset pc-offset)
             (error "mismatch between def offsets and parsed code"))
           (cond
            ((< pc-offset offset)
             (lp var (1+ pos) (+ pc-offset (vector-ref parsed pos))))
            (else
             (add-def! pos var)
             (lp (1+ var) pos pc-offset)))))))
    genv))

(define (compute-defs-by-slot defs)
  (let* ((nslots (match defs
                   (#(#(_ _ slot _) ...) (1+ (apply max slot)))))
         (by-slot (make-vector nslots #f)))
    (let lp ((n 0))
      (when (< n nslots)
        (vector-set! by-slot n (make-bitvector (vector-length defs) #f))
        (lp (1+ n))))
    (let lp ((n 0))
      (when (< n (vector-length defs))
        (match (vector-ref defs n)
          (#(_ _ slot _)
           (bitvector-set! (vector-ref by-slot slot) n #t)
           (lp (1+ n))))))
    by-slot))

(define (compute-killv code parsed defs)
  (let*-values (((defs-by-slot) (compute-defs-by-slot defs))
                ((initial-frame-size) (vector-length defs-by-slot))
                ((in-sizes out-sizes)
                 (compute-frame-sizes code parsed initial-frame-size))
                ((killv) (make-vector (vector-length parsed) #f)))
    (define (kill-slot! n slot)
      (bit-set*! (vector-ref killv n) (vector-ref defs-by-slot slot) #t))
    (let lp ((n 0))
      (when (< n (vector-length killv))
        (vector-set! killv n (make-bitvector (vector-length defs) #f))
        (lp (1+ n))))
    ;; Some defs get into place without explicit instructions -- this is
    ;; the case if no shuffling need occur, for example.  In any case,
    ;; mark them as killing any previous definitions at that slot.
    (let lp ((var 0) (pos 0) (pc-offset 0))
      (when (< var (vector-length defs))
        (match (vector-ref defs var)
          (#(name offset slot representation)
           (when (< offset pc-offset)
             (error "mismatch between def offsets and parsed code"))
           (cond
            ((< pc-offset offset)
             (lp var (1+ pos) (+ pc-offset (vector-ref parsed pos))))
            (else
             (kill-slot! pos slot)
             (lp (1+ var) pos pc-offset)))))))
    (let lp ((n 0) (pos 0))
      (when (< n (vector-length parsed))
        (for-each (lambda (slot)
                    (when (< slot (vector-length defs-by-slot))
                      (kill-slot! n slot)))
                  (instruction-slot-clobbers code pos
                                             (vector-ref in-sizes n)
                                             (vector-ref out-sizes n)))
        (lp (1+ n) (+ pos (vector-ref parsed n)))))
    killv))

(define (available-bindings frame arity ip top-frame?)
  (let* ((defs (list->vector (arity-definitions arity)))
         (code (arity-code arity))
         (parsed (parse-code code))
         (len (vector-length parsed))
         (preds (compute-predecessors code parsed))
         (genv (compute-genv parsed defs))
         (killv (compute-killv code parsed defs))
         (inv (make-vector len #f))
         (outv (make-vector len #f))
         (tmp (make-bitvector (vector-length defs) #f)))
    (define (bitvector-copy! dst src)
      (bitvector-fill! dst #f)
      (bit-set*! dst src #t))
    (define (bitvector-meet! accum src)
      (bitvector-copy! tmp src)
      (bit-invert! tmp)
      (bit-set*! accum tmp #f))

    (let lp ((n 0))
      (when (< n len)
        (vector-set! inv n (make-bitvector (vector-length defs) #f))
        (vector-set! outv n (make-bitvector (vector-length defs) #f))
        (lp (1+ n))))

    (let lp ((n 0) (first? #t) (changed? #f))
      (cond
       ((< n len)
        (let ((in (vector-ref inv n))
              (out (vector-ref outv n))
              (kill (vector-ref killv n))
              (gen (vector-ref genv n)))
          (let ((out-count (or changed? (bit-count #t out))))
            (bitvector-fill! in (not (zero? n)))
            (let lp ((preds (vector-ref preds n)))
              (match preds
                (() #t)
                ((pred . preds)
                 (unless (and first? (<= n pred))
                   (bitvector-meet! in (vector-ref outv pred)))
                 (lp preds))))
            (bitvector-copy! out in)
            (bit-set*! out kill #f)
            (for-each (lambda (def)
                        (bitvector-set! out def #t))
                      gen)
            (lp (1+ n) first?
                (or changed? (not (eqv? out-count (bit-count #t out))))))))
       ((or changed? first?)
        (lp 0 #f #f))))

    (let lp ((n 0) (offset (- ip (arity-low-pc arity))))
      (when (< offset 0)
        (error "ip did not correspond to an instruction boundary?"))
      (if (zero? offset)
          ;; It shouldn't be the case that both OFFSET and N are zero
          ;; but TOP-FRAME? is false.  Still, it could happen, as is
          ;; currently the case in frame-arguments.
          (let ((live (if (or top-frame? (zero? n))
                          (vector-ref inv n)
                          ;; If we're not at a top frame, the IP points
                          ;; to the continuation -- but we haven't
                          ;; returned and defined its values yet.  The
                          ;; set of live variables is the set that was
                          ;; live going into the call, minus the set
                          ;; killed by the call, but not including
                          ;; values defined by the call.
                          (begin
                            (bitvector-copy! tmp (vector-ref inv (1- n)))
                            (bit-set*! tmp (vector-ref killv (1- n)) #f)
                            tmp))))
            (let lp ((n 0))
              (let ((n (bit-position #t live n)))
                (if n
                    (match (vector-ref defs n)
                      (#(name def-offset slot representation)
                       (cons (make-binding frame n name slot representation)
                             (lp (1+ n)))))
                    '()))))
          (lp (1+ n) (- offset (vector-ref parsed n)))))))

(define* (frame-bindings frame #:optional top-frame?)
  (let ((ip (frame-instruction-pointer frame)))
    (cond
     ((find-program-arity ip)
      => (lambda (arity)
           (available-bindings frame arity ip top-frame?)))
     (else '()))))

(define (frame-lookup-binding frame var)
  (let lp ((bindings (frame-bindings frame)))
    (cond ((null? bindings)
           #f)
          ((eq? (binding-name (car bindings)) var)
           (car bindings))
          (else
           (lp (cdr bindings))))))

(define (binding-ref binding)
  (frame-local-ref (or (binding-frame binding)
                       (error "binding has no frame" binding))
                   (binding-slot binding)
                   (binding-representation binding)))

(define (binding-set! binding val)
  (frame-local-set! (or (binding-frame binding)
                        (error "binding has no frame" binding))
                    (binding-slot binding)
                    val
                    (binding-representation binding)))

(define* (frame-procedure-name frame #:key
                               (info (find-program-debug-info
                                      (frame-instruction-pointer frame))))
  (cond
   (info => program-debug-info-name)
   ;; We can only try to get the name from the closure if we know that
   ;; slot 0 corresponds to the frame's procedure.  This isn't possible
   ;; to know in general.  If the frame has already begun executing and
   ;; the closure binding is dead, it could have been replaced with any
   ;; other random value, or an unboxed value.  Even if we're catching
   ;; the frame at its application, before it has started running, if
   ;; the callee is well-known and has only one free variable, closure
   ;; optimization could have chosen to represent its closure as that
   ;; free variable, and that free variable might be some other program,
   ;; or even an unboxed value.  It would be an error to try to get the
   ;; procedure name of some procedure that doesn't correspond to the
   ;; one being applied.  (Free variables are currently always boxed but
   ;; that could change in the future.)
   ((primitive-code? (frame-instruction-pointer frame))
    (procedure-name (frame-local-ref frame 0 'scm)))
   (else #f)))

;; This function is always called to get some sort of representation of the
;; frame to present to the user, so let's do the logical thing and dispatch to
;; frame-call-representation.
(define (frame-arguments frame)
  (cdr (frame-call-representation frame)))

;; Usually the IP is sufficient to identify the procedure being called.
;; However all primitive applications of the same arity share the same
;; code.  Perhaps we should change that in the future, but for now we
;; export this function to avoid having to export frame-local-ref.
;;
(define (frame-instruction-pointer-or-primitive-procedure-name frame)
  (let ((ip (frame-instruction-pointer frame)))
    (if (primitive-code? ip)
        (procedure-name (frame-local-ref frame 0 'scm))
        ip)))


;;;
;;; Pretty printing
;;;

;; Basically there are two cases to deal with here:
;;
;;   1. We've already parsed the arguments, and bound them to local
;;      variables. In a standard (lambda (a b c) ...) call, this doesn't
;;      involve any argument shuffling; but with rest, optional, or
;;      keyword arguments, the arguments as given to the procedure may
;;      not correspond to what's on the stack. We reconstruct the
;;      arguments using e.g. for the case above: `(,a ,b ,c). This works
;;      for rest arguments too: (a b . c) => `(,a ,b . ,c)
;;
;;   2. We have failed to parse the arguments. Perhaps it's the wrong
;;      number of arguments, or perhaps we're doing a typed dispatch and
;;      the types don't match. In that case the arguments are all on the
;;      stack, and nothing else is on the stack.

(define* (frame-call-representation frame #:key top-frame?)
  (let* ((ip (frame-instruction-pointer frame))
         (info (find-program-debug-info ip))
         (nlocals (frame-num-locals frame)))
    (define (find-slot i bindings)
      (match bindings
        (() #f)
        (((and binding ($ <binding> frame idx name slot)) . bindings)
         (if (< idx i)
             (find-slot i bindings)
             (and (= idx i) binding)))))
    (define (local-ref i bindings)
      (cond
       ((not bindings)
        ;; This case is only hit for primitives and application
        ;; arguments.
        (frame-local-ref frame i 'scm))
       ((find-slot i bindings)
        => (lambda (binding)
             (let ((val (frame-local-ref frame (binding-slot binding)
                                         (binding-representation binding))))
               ;; It could be that there's a value that isn't clobbered
               ;; by a call but that isn't live after a call either.  In
               ;; that case, if GC runs during the call, the value will
               ;; be collected, and on the stack it will be replaced
               ;; with the unspecified value.  Assume that clobbering
               ;; values is more likely than passing the unspecified
               ;; value as an argument, and replace unspecified with _,
               ;; as if the binding were not available.
               (if (unspecified? val) '_ val))))
       (else
        '_)))
    (define (application-arguments)
      ;; Case 1.
      (map (lambda (local) (local-ref local #f))
           ;; Cdr past the 0th local, which is the procedure.
           (cdr (iota nlocals))))
    (define (reconstruct-arguments bindings nreq nopt kw has-rest? local)
      ;; Case 2.
      (cond
       ((positive? nreq)
        (cons (local-ref local bindings)
              (reconstruct-arguments bindings
                                     (1- nreq) nopt kw has-rest? (1+ local))))
       ((positive? nopt)
        (cons (local-ref local bindings)
              (reconstruct-arguments bindings
                                     nreq (1- nopt) kw has-rest? (1+ local))))
       ((pair? kw)
        (cons* (caar kw) (local-ref (cdar kw) bindings)
               (reconstruct-arguments bindings
                                      nreq nopt (cdr kw) has-rest? (1+ local))))
       (has-rest?
        (local-ref local bindings))
       (else
        '())))
    (cons
     (or (frame-procedure-name frame #:info info) '_)
     (cond
      ((find-program-arity ip)
       => (lambda (arity)
            (if (and top-frame? (eqv? ip (arity-low-pc arity)))
                (application-arguments)
                (reconstruct-arguments
                 (available-bindings frame arity ip top-frame?)
                 (arity-nreq arity)
                 (arity-nopt arity)
                 (arity-keyword-args arity)
                 (arity-has-rest? arity)
                 1))))
      ((and (primitive-code? ip)
            (program-arguments-alist (frame-local-ref frame 0 'scm) ip))
       => (lambda (args)
            (match args
              ((('required . req)
                ('optional . opt)
                ('keyword . kw)
                ('allow-other-keys? . _)
                ('rest . rest))
               (reconstruct-arguments #f
                                      (length req) (length opt) kw rest 1)))))
      (else
       (application-arguments))))))



;;; Misc
;;;

(define (frame-environment frame)
  (map (lambda (binding)
	 (cons (binding-name binding) (binding-ref binding)))
       (frame-bindings frame)))

(define (frame-object-binding frame obj)
  (do ((bs (frame-bindings frame) (cdr bs)))
      ((or (null? bs) (eq? obj (binding-ref (car bs))))
       (and (pair? bs) (car bs)))))

(define (frame-object-name frame obj)
  (cond ((frame-object-binding frame obj) => binding-name)
	(else #f)))
