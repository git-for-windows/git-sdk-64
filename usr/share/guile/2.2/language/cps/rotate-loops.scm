;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.

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
;;; Rotate loops so that they end with conditional jumps, if possible.
;;; The result goes from:
;;;
;;;   loop:
;;;     if x < 5 goto done;
;;;     x = x + 1;
;;;     goto loop;
;;;   done:
;;;
;;;     if x < 5 goto done;
;;;   loop:
;;;     x = x + 1;
;;;     if x < 5 goto done;
;;;   done:
;;;
;;; It's more code but there are fewer instructions in the body.  Note
;;; that this transformation isn't guaranteed to produce a loop that
;;; ends in a conditional jump, because usually your loop has some state
;;; that it's shuffling around and for now that shuffle is reified with
;;; the test, not the loop header.  Alack.
;;;
;;; Implementation-wise, things are complicated by values flowing out of
;;; the loop.  We actually perform this transformation only on loops
;;; that have a single exit continuation, so that we define values
;;; flowing out in one place.  We rename the loop variables in two
;;; places internally: one for the peeled comparison, and another for
;;; the body.  The loop variables' original names are then bound in a
;;; join continuation for use by successor code.
;;;
;;; Code:

(define-module (language cps rotate-loops)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:export (rotate-loops))

(define (loop-successors scc succs)
  (intset-subtract (intset-fold (lambda (label exits)
                                  (intset-union exits (intmap-ref succs label)))
                                scc empty-intset)
                   scc))

(define (find-exits scc succs)
  (intset-fold (lambda (label exits)
                 (if (eq? empty-intset
                          (intset-subtract (intmap-ref succs label) scc))
                     exits
                     (intset-add exits label)))
               scc
               empty-intset))

(define (find-entry scc preds)
  (trivial-intset (find-exits scc preds)))

(define (rotate-loop cps entry-label body-labels succs preds back-edges)
  (match (intmap-ref cps entry-label)
    ((and entry-cont
          ($ $kargs entry-names entry-vars
             ($ $continue entry-kf entry-src ($ $branch entry-kt entry-exp))))
     (let* ((exit-if-true? (intset-ref body-labels entry-kf))
            (loop-exits (find-exits body-labels succs))
            (exit (if exit-if-true? entry-kt entry-kf))
            (new-entry-label (if exit-if-true? entry-kf entry-kt))
            (join-label (fresh-label))
            (join-cont (build-cont
                         ($kargs entry-names entry-vars
                           ($continue exit entry-src ($values ())))))
            (cps (intmap-add! cps join-label join-cont)))
       (define (make-fresh-vars)
         (map (lambda (_) (fresh-var)) entry-vars))
       (define (make-trampoline k src values)
         (build-cont ($kargs () () ($continue k src ($values values)))))
       (define (replace-exit k trampoline)
         (if (eqv? k exit) trampoline k))
       (define (rename-exp exp vars)
         (define (rename-var var)
           (match (list-index entry-vars var)
             (#f var)
             (idx (list-ref vars idx))))
         (rewrite-exp exp
           ((or ($ $const) ($ $prim) ($ $closure)) ,exp)
           (($ $values args)
            ($values ,(map rename-var args)))
           (($ $call proc args)
            ($call (rename-var proc) ,(map rename-var args)))
           (($ $callk k proc args)
            ($callk k (rename-var proc) ,(map rename-var args)))
           (($ $branch kt ($ $values (arg)))
            ($branch kt ($values ((rename-var arg)))))
           (($ $branch kt ($ $primcall name args))
            ($branch kt ($primcall name ,(map rename-var args))))
           (($ $primcall name args)
            ($primcall name ,(map rename-var args)))
           (($ $prompt escape? tag handler)
            ($prompt escape? (rename-var tag) handler))))
       (define (attach-trampoline label src names vars args)
         (let* ((trampoline-out-label (fresh-label))
                (trampoline-out-cont
                 (make-trampoline join-label src args))
                (trampoline-in-label (fresh-label))
                (trampoline-in-cont
                 (make-trampoline new-entry-label src args))
                (kf (if exit-if-true? trampoline-in-label trampoline-out-label))
                (kt (if exit-if-true? trampoline-out-label trampoline-in-label))
                (cont (build-cont
                        ($kargs names vars
                          ($continue kf entry-src
                            ($branch kt ,(rename-exp entry-exp args))))))
                (cps (intmap-replace! cps label cont))
                (cps (intmap-add! cps trampoline-in-label trampoline-in-cont)))
           (intmap-add! cps trampoline-out-label trampoline-out-cont)))
       ;; Rewrite the targets of the entry branch to go to
       ;; trampolines.  One will pass values out of the loop, and
       ;; one will pass values into the loop.
       (let* ((pre-header-vars (make-fresh-vars))
              (body-vars (make-fresh-vars))
              (cps (attach-trampoline entry-label entry-src
                                      entry-names pre-header-vars
                                      pre-header-vars))
              (new-entry-cont (build-cont
                                ($kargs entry-names body-vars
                                  ,(match (intmap-ref cps new-entry-label)
                                     (($ $kargs () () term) term)))))
              (cps (intmap-replace! cps new-entry-label new-entry-cont)))
         (intset-fold
          (lambda (label cps)
            (cond
             ((intset-ref back-edges label)
              (match (intmap-ref cps label)
                (($ $kargs names vars ($ $continue _ src exp))
                 (match (rename-exp exp body-vars)
                   (($ $values args)
                    (attach-trampoline label src names vars args))
                   (exp
                    (let* ((args (make-fresh-vars))
                           (bind-label (fresh-label))
                           (edge* (build-cont
                                    ($kargs names vars
                                      ($continue bind-label src ,exp))))
                           (cps (intmap-replace! cps label edge*))
                           ;; attach-trampoline uses intmap-replace!.
                           (cps (intmap-add! cps bind-label #f)))
                      (attach-trampoline bind-label src
                                         entry-names args args)))))))
             ((intset-ref loop-exits label)
              (match (intmap-ref cps label)
                (($ $kargs names vars
                    ($ $continue kf src ($ $branch kt exp)))
                 (let* ((trampoline-out-label (fresh-label))
                        (trampoline-out-cont
                         (make-trampoline join-label src body-vars))
                        (kf (if (eqv? kf exit) trampoline-out-label kf))
                        (kt (if (eqv? kt exit) trampoline-out-label kt))
                        (cont (build-cont
                                ($kargs names vars
                                  ($continue kf src
                                    ($branch kt ,(rename-exp exp body-vars))))))
                        (cps (intmap-replace! cps label cont)))
                   (intmap-add! cps trampoline-out-label trampoline-out-cont)))))
             (else
              (match (intmap-ref cps label)
                (($ $kargs names vars ($ $continue k src exp))
                 (let ((cont (build-cont
                               ($kargs names vars
                                 ($continue k src
                                   ,(rename-exp exp body-vars))))))
                   (intmap-replace! cps label cont)))
                (($ $kreceive) cps)))))
          (intset-remove body-labels entry-label)
          cps))))))

(define (rotate-loops-in-function kfun body cps)
  (define (can-rotate? edges)
    (intset-fold (lambda (label rotate?)
                   (match (intmap-ref cps label)
                     (($ $kreceive) #f)
                     (($ $kargs _ _ ($ $continue _ _ exp))
                      (match exp
                        (($ $branch) #f)
                        (_ rotate?)))))
                 edges #t))
  (let* ((succs (compute-successors cps kfun))
         (preds (invert-graph succs)))
    (intmap-fold
     (lambda (id scc cps)
       (cond
        ((trivial-intset scc) cps)
        ((find-entry scc preds)
         => (lambda (entry)
              (let ((back-edges (intset-intersect scc
                                                  (intmap-ref preds entry))))
                (if (and (can-rotate? back-edges)
                         (trivial-intset
                          (intset-subtract (intmap-ref succs entry) scc))
                         (trivial-intset (loop-successors scc succs))
                         (match (intmap-ref cps entry)
                           ;; Can't rotate $prompt out of loop header.
                           (($ $kargs _ _ ($ $continue _ _ ($ $prompt))) #f)
                           (_ #t)))
                    ;; Loop header is an exit, and there is only one
                    ;; exit continuation.  Loop header isn't a prompt,
                    ;; so it must be a conditional branch and only one
                    ;; successor is an exit.  The values flowing out of
                    ;; the loop are the loop variables.
                    (rotate-loop cps entry scc succs preds back-edges)
                    cps))))
        (else cps)))
     (compute-strongly-connected-components succs kfun)
     cps)))

(define (rotate-loops cps)
  (persistent-intmap
   (with-fresh-name-state cps
     (intmap-fold rotate-loops-in-function
                  (compute-reachable-functions cps)
                  cps))))
