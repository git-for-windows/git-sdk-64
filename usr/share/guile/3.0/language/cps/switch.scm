;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2020, 2023 Free Software Foundation, Inc.

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
;;; A pass to optimize chains of "eq-constant?" branches.
;;;
;;; For chains that are more than a few comparisons long, we partition
;;; values by type, then dispatch in type-specific ways.  For fixnums
;;; and chars, we use a combination of binary search over sparse sets,
;;; table dispatch over dense sets, and comparison chains when sets are
;;; small enough.  For "special" values like #f and the eof-object, we
;;; just emit comparison chains.  For symbols, we do a hash dispatch
;;; using the hash code from the symbol, or a comparison chain if the
;;; set is very small.
;;;
;;; Code:

(define-module (language cps switch)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (system base target)
  #:export (optimize-branch-chains))

(define (fold-branch-chains cps kfun body f seed)
  "For each chain of one or more eq-constant? branches, where each
branch tests the same variable, branches to the next if the match fails,
and each non-head branch has only a single predecessor, fold F over SEED
by calling as (F VAR EXIT TESTS SEED), where VAR is the value being
tested, EXIT is the last failure continuation, and TESTS is an ordered
list of branch labels."
  (define single
    (compute-singly-referenced-labels (intmap-select cps body)))

  (define (start-chain var exit test)
    (traverse-chain var exit (list test)))
  (define (finish-chain var exit tests)
    (values var exit (reverse tests)))

  (define (traverse-chain var exit tests)
    (match (intmap-ref cps exit)
      (($ $kargs () ()
          ($ $branch kf kt src 'eq-constant? const (arg)))
       (if (and (eq? arg var)
                (intset-ref single exit))
           (traverse-chain var kf (cons exit tests))
           (finish-chain var exit tests)))
      (_ (finish-chain var exit tests))))

  (let fold-chains ((worklist (list kfun))
                    (visited empty-intset)
                    (seed seed))
    (match worklist
      (() seed)
      ((label . worklist)
       (if (intset-ref visited label)
           (fold-chains worklist visited seed)
           (let ((visited (intset-add! visited label)))
             (define (%continue worklist)
               (fold-chains worklist visited seed))
             (define (continue0)       (%continue worklist))
             (define (continue1 k)     (%continue (cons k worklist)))
             (define (continue2 k1 k2) (%continue (cons* k1 k2 worklist)))
             (define (continue* k*)    (%continue (append k* worklist)))
             (match (intmap-ref cps label)
               (($ $kfun src meta self ktail #f)    (continue0))
               (($ $kfun src meta self ktail kclause) (continue1 kclause))
               (($ $kclause arity kbody #f)         (continue1 kbody))
               (($ $kclause arity kbody kalt)       (continue2 kbody kalt))
               (($ $kargs names vars term)
                (match term
                  (($ $branch kf kt src 'eq-constant? const (arg))
                   (call-with-values (lambda () (start-chain arg kf label))
                     (lambda (var exit tests)
                       (fold-chains (cons exit worklist)
                                    (fold1 (lambda (k visited)
                                             (intset-add! visited k))
                                           tests visited)
                                    (f var exit tests seed)))))
                  (($ $continue k)                  (continue1 k))
                  (($ $branch kf kt)                (continue2 kf kt))
                  (($ $switch kf kt*)               (continue* (cons kf kt*)))
                  (($ $prompt k kh)                 (continue2 k kh))
                  (($ $throw)                       (continue0))))
               (($ $ktail)                          (continue0))
               (($ $kreceive arity kbody)           (continue1 kbody)))))))))

(define (length>? ls n)
  (match ls
    (() #f)
    ((_ . ls)
     (or (zero? n)
         (length>? ls (1- n))))))

(define (partition-targets targets)
  "Partition the list of (CONST . KT) values into five unordered
sub-lists, ignoring duplicates, according to CONST type: fixnums, chars,
\"special\" values, symbols, and other values.  A special value is one
of the immediates #f, (), #t, #nil, the EOF object, or the unspecified
object."
  (define (hash-table->alist table)
    (hash-map->list cons table))
  (define (hash-table->sorted-alist table less?)
    (sort (hash-table->alist table) (lambda (a b) (less? (car a) (car b)))))
  (let ((fixnums (make-hash-table))
        (chars (make-hash-table))
        (specials (make-hash-table))
        (symbols (make-hash-table))
        (others (make-hash-table)))
    (for-each (match-lambda
               ((const . k)
                (let ((table (cond
                              ((target-fixnum? const) fixnums)
                              ((char? const) chars)
                              ((eq? const #f) specials)
                              ((eq? const '()) specials)
                              ((eq? const #t) specials)
                              ((eq? const #nil) specials)
                              ((eof-object? const) specials)
                              ((unspecified? const) specials)
                              ((symbol? const) symbols)
                              (else others))))
                  (unless (hashq-ref table const)
                    (hashq-set! table const k)))))
              targets)
    (values (hash-table->sorted-alist fixnums <)
            (hash-table->sorted-alist chars char<?)
            (hash-table->alist specials)
            (hash-table->sorted-alist symbols
                                      (lambda (s1 s2)
                                        (string< (symbol->string s1)
                                                 (symbol->string s2))))
            (hash-table->alist others))))

;; Leave any chain this long or less as is.
(define *unoptimized-chain-length* 4)

;; If we are optimizing a subset of targets, any subset this long or
;; less will be reified as a chain of comparisons.
(define *leaf-chain-max-length* 3)

;; If we end up dispatching via type check with an eye to maybe doing
;; binary/table lookup but the set of targets for the type is this long
;; or less, just reify a chain instead of untagging.
(define *tagged-chain-max-length* 2)

;; When deciding whether to dispatch via binary search or via a switch
;; on constants in a range, do a switch if at least this fraction of
;; constants in the range have continuations.
(define *table-switch-minimum-density* 0.5)

;; When deciding whether to dispatch via hash value on a set of symbol
;; targets, reify a branch chain unless there are more than this many
;; targets.  Otherwise the cost outweighs the savings.
(define *symbol-hash-dispatch-min-length* 4)

(define (optimize-branch-chain var exit tests cps)
  (define (should-optimize? targets)
    (define (has-duplicates? targets)
      (let ((consts (make-hash-table)))
        (or-map (match-lambda
                 ((const . k)
                  (or (hash-ref consts const)
                      (begin
                        (hash-set! consts const #t)
                        #f))))
                targets)))
    ;; We optimize if there are "enough" targets, or if there are any
    ;; duplicate targets.
    (or (length>? targets *unoptimized-chain-length*)
        (has-duplicates? targets)))
  (define (reify-chain cps var targets op k)
    (match targets
      (() (with-cps cps k))
      (((const . kt) . targets)
       (with-cps cps
         (let$ ktail (reify-chain var targets op k))
         (letk khead ($kargs () ()
                       ($branch ktail kt #f op const (var))))
         khead))))
  (define (reify-switch cps var targets min max exit)
    (cond
     ((zero? min)
      (let ((kt* (make-vector (1+ max) exit)))
        (for-each (match-lambda
                   ((target . k) (vector-set! kt* target k)))
                  targets)
        (with-cps cps
          (letv u64)
          (letk kswitch ($kargs ('u64) (u64)
                          ($switch exit (vector->list kt*) #f u64)))
          (letk kcvt
                ($kargs () ()
                  ($continue kswitch #f ($primcall 's64->u64 #f (var)))))
          kcvt)))
     (else
      (let ((targets (map (match-lambda
                           ((target . k) (cons (- target min) k)))
                          targets))
            (op (if (positive? min) 'ssub/immediate 'sadd/immediate)))
        (with-cps cps
          (letv idx)
          (let$ kcvt (reify-switch idx targets 0 (- max min) exit))
          (letk kzero ($kargs ('idx) (idx)
                        ($continue kcvt #f ($values ()))))
          (letk ksub
                ($kargs () ()
                  ($continue kzero #f ($primcall op (abs min) (var)))))
          ksub)))))
  (define (dispatch-numerics cps var targets start end exit)
    ;; Precondition: VAR is an s64, START < END, and TARGETS hold the
    ;; untagged values.
    (define (value-at idx)
      (match (vector-ref targets idx)
        ((const . k) const)))
    (define (target-list)
      (let lp ((i start))
        (if (< i end)
            (cons (vector-ref targets i) (lp (1+ i)))
            '())))
    (let* ((min (value-at start))
           (max (value-at (1- end)))
           (range (1+ (- max min)))
           (len (- end start))
           (density (/ len 1.0 range)))
      (cond
       ((<= len *leaf-chain-max-length*)
        (reify-chain cps var (target-list) 's64-imm-= exit))
       ((<= *table-switch-minimum-density* density)
        (reify-switch cps var (target-list) min max exit))
       (else
        ;; binary search
        (let* ((split (ash (+ start end) -1))
               (mid (value-at split)))
          (with-cps cps
            (let$ klo (dispatch-numerics var targets start split exit))
            (let$ khi (dispatch-numerics var targets split end exit))
            (letk ktest
                  ($kargs () ()
                    ($branch khi klo #f 's64-imm-< mid (var))))
            ktest))))))
  (define (reify-known-numerics cps var targets untag-var untag-val exit)
    (cond
     ((length>? targets *tagged-chain-max-length*)
      (let ((targets (list->vector
                      (map (match-lambda
                            ((const . k) (cons (untag-val const) k)))
                           targets))))
        (with-cps cps
          (letv raw)
          (let$ kdispatch
                (dispatch-numerics raw targets 0 (vector-length targets) exit))
          (letk kraw ($kargs ('raw) (raw)
                       ($continue kdispatch #f ($values ()))))
          (let$ untag (untag-var var kraw))
          (letk kuntag ($kargs () () ,untag))
          kuntag)))
     (else
      (reify-chain cps var targets 'eq-constant? exit))))
  (define (reify-numeric cps var targets pred untag-var untag-val next exit)
    (cond
     ((null? targets) (with-cps cps next))
     (else
      (with-cps cps
        (let$ ktype (reify-known-numerics var targets untag-var untag-val exit))
        (letk test  ($kargs () () ($branch next ktype #f pred #f (var))))
        test))))
  (define (reify-fixnums cps var targets next exit)
    (reify-numeric cps var targets 'fixnum?
                   (lambda (cps var k)
                     (with-cps cps
                       (build-term
                         ($continue k #f
                           ($primcall 'untag-fixnum #f (var))))))
                   identity next exit))
  (define (reify-chars cps var targets next exit)
    (reify-numeric cps var targets 'char?
                   (lambda (cps var k)
                     (with-cps cps
                       (letv u64)
                       (letk kcvt
                             ($kargs ('u64) (u64)
                               ($continue k #f
                                 ($primcall 'u64->s64 #f (u64)))))
                       (build-term
                         ($continue kcvt #f
                           ($primcall 'untag-char #f (var))))))
                   char->integer next exit))
  (define (reify-specials cps var targets next exit)
    ;; Specials are a branch chain.
    (cond
     ((null? targets) (with-cps cps next))
     (else
      (with-cps cps
        (let$ kimm (reify-chain var targets 'eq-constant? exit))
        (letk test ($kargs () () ($branch kimm next #f 'heap-object? #f (var))))
        test))))
  (define (reify-symbols cps var targets next exit)
    (cond
     ((null? targets)
      (with-cps cps next))
     ((length>? targets *symbol-hash-dispatch-min-length*)
      ;; Hash dispatch.  The value has symbol-hash-bits significant
      ;; bits.  We dispatch on the bottom N bits of the significant
      ;; bits, where N <= symbol-hash-bits, for the smallest N for which
      ;; len(targets) <= 2^N.
      (let* ((backend (resolve-interface `(language cps ,(target-runtime))))
             (symbol-hash (module-ref backend 'target-symbol-hash))
             (symbol-hash-bits (module-ref backend 'target-symbol-hash-bits))
             (nbits (let ((ntargets (length targets)))
                      (let lp ((nbits 2))
                        (cond
                         ((= nbits symbol-hash-bits) nbits)
                         ((<= ntargets (ash 1 nbits)) nbits)
                         (else (lp (1+ nbits)))))))
             (nbuckets (ash 1 nbits))
             (buckets (make-vector nbuckets '()))
             (kt* (make-vector nbuckets exit)))
        (define (symbol->bucket sym)
          (logand (1- nbuckets) (symbol-hash (symbol->string sym))))
        (define (vector-push! v i x)
          (vector-set! v i (cons x (vector-ref v i))))
        (for-each (match-lambda
                    ((and pair (sym . target))
                     (vector-push! buckets (symbol->bucket sym) pair)))
                  targets)
        (let lp ((cps cps) (i 0))
          (cond
           ((< i nbuckets)
            (call-with-values (lambda ()
                                (reify-chain cps var (vector-ref buckets i)
                                             'eq-constant? exit))
              (lambda (cps k)
                (vector-set! kt* i k)
                (lp cps (1+ i)))))
           (else
            (with-cps cps
              (letv hash idx)
              (letk kswitch
                    ($kargs ('idx) (idx)
                      ($switch exit (vector->list kt*) #f idx)))
              (letk kidx
                    ($kargs ('hash) (hash)
                      ($continue kswitch #f
                        ($primcall 'ulogand/immediate (1- nbuckets) (hash)))))
              (letk khash
                    ($kargs () ()
                      ($continue kidx #f
                        ($primcall 'symbol-hash #f (var)))))
              (letk ksym
                    ($kargs () ()
                      ($branch next khash #f 'symbol? #f (var))))
              (letk kheap
                    ($kargs () ()
                      ($branch next ksym #f 'heap-object? #f (var))))
              kheap))))))
     (else
      (reify-chain cps var targets 'eq-constant? next))))
  (define (reify-others cps var targets exit)
    ;; Not an immediate, not a symbol -- an object without identity.
    ;; Perhaps it's reasonable to assume all these don't match.
    (reify-chain cps var targets 'eq-constant? exit))
  (define (apply-optimizations var exit tests targets)
    (call-with-values (lambda () (partition-targets targets))
      (lambda (fixnums chars specials symbols others)
        (match (intmap-ref cps (car tests))
          (($ $kargs names vars _)
           (with-cps cps
             ;; Reify an optimized version of the chain, and bind k to
             ;; its label.
             (let$ k (reify-others var others exit))
             (let$ k (reify-symbols var symbols k exit))
             (let$ k (reify-specials var specials k exit))
             (let$ k (reify-chars var chars k exit))
             (let$ k (reify-fixnums var fixnums k exit))
             (setk (car tests)
                   ;; Here we introduce a useless forwarding node in
                   ;; order to treat each node as being a nullary
                   ;; $kargs.  Simplification will remove it later.
                   ($kargs names vars
                     ($continue k #f ($values ()))))))))))
  (let ((targets (map (lambda (test)
                        (match (intmap-ref cps test)
                          (($ $kargs _ _ ($ $branch kf kt src op const (_)))
                           (cons const kt))))
                      tests)))
    (if (should-optimize? targets)
        (apply-optimizations var exit tests targets)
        cps)))

(define (optimize-branch-chains cps)
  (with-fresh-name-state cps
    (persistent-intmap
     (intmap-fold
      (lambda (kfun body cps)
        (fold-branch-chains cps kfun body
                            optimize-branch-chain cps))
      (compute-reachable-functions cps)
      cps))))
