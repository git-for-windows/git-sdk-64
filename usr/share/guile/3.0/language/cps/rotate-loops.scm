;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

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
  #:use-module (language cps with-cps)
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
             ($ $branch entry-kf entry-kt entry-src
                entry-op entry-param entry-args)))
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
       (define (rename-var var replacements)
         "If VAR refers to a member of ENTRY-VARS, replace with a
corresponding var from REPLACEMENTS; otherwise return VAR."
         (match (list-index entry-vars var)
           (#f var)
           (idx (list-ref replacements idx))))
       (define (rename-vars vars replacements)
         (map (lambda (var) (rename-var var replacements)) vars))
       (define (rename-term term replacements)
         (define (rename arg) (rename-var arg replacements))
         (define (rename* arg) (rename-vars arg replacements))
         (rewrite-term term
           (($ $continue k src exp)
            ($continue k src
              ,(rewrite-exp exp
                 ((or ($ $const) ($ $prim) ($ $const-fun) ($ $code)) ,exp)
                 (($ $values args)
                  ($values ,(rename* args)))
                 (($ $call proc args)
                  ($call (rename proc) ,(rename* args)))
                 (($ $callk k proc args)
                  ($callk k (and proc (rename proc)) ,(rename* args)))
                 (($ $primcall name param args)
                  ($primcall name param ,(rename* args))))))
           (($ $branch kf kt src op param args)
            ($branch kf kt src op param ,(rename* args)))
           (($ $switch kf kt* src arg)
            ($switch kf kt* src (rename arg)))
           (($ $prompt k kh src escape? tag)
            ($prompt k kh src escape? (rename tag)))
           (($ $throw src op param args)
            ($throw src op param ,(rename* args)))))
       (define (attach-trampoline cps label src names vars args)
         (with-cps cps
           (letk ktramp-out ,(make-trampoline join-label src args))
           (letk ktramp-in ,(make-trampoline new-entry-label src args))
           (setk label
                 ($kargs names vars
                   ($branch (if exit-if-true? ktramp-in ktramp-out)
                            (if exit-if-true? ktramp-out ktramp-in)
                            entry-src
                     entry-op entry-param ,(rename-vars entry-args args))))))
       ;; Rewrite the targets of the entry branch to go to
       ;; trampolines.  One will pass values out of the loop, and
       ;; one will pass values into the loop.
       (let* ((pre-header-vars (make-fresh-vars))
              (body-vars (make-fresh-vars))
              (cps (attach-trampoline cps entry-label entry-src
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
                (($ $kargs names vars term)
                 (match (rename-term term body-vars)
                   (($ $continue _ src ($ $values args))
                    (attach-trampoline cps label src names vars args))
                   (($ $continue _ src exp)
                    (let* ((args (make-fresh-vars))
                           (bind-label (fresh-label))
                           (edge* (build-cont
                                    ($kargs names vars
                                      ($continue bind-label src ,exp))))
                           (cps (intmap-replace! cps label edge*))
                           ;; attach-trampoline uses setk.
                           (cps (intmap-add! cps bind-label #f)))
                      (attach-trampoline cps bind-label src
                                         entry-names args args)))))))
             ((intset-ref loop-exits label)
              (match (intmap-ref cps label)
                (($ $kargs names vars ($ $branch kf kt src op param args))
                 (with-cps cps
                   (letk ktramp-out ,(make-trampoline join-label src body-vars))
                   (setk label
                         ($kargs names vars
                           ($branch (if (eqv? kf exit) ktramp-out kf)
                                    (if (eqv? kt exit) ktramp-out kt)
                                    src
                             op param ,(rename-vars args body-vars))))))))
             (else
              (match (intmap-ref cps label)
                (($ $kargs names vars term)
                 (with-cps cps
                   (setk label ($kargs names vars
                                 ,(rename-term term body-vars)))))
                (($ $kreceive) cps)))))
          (intset-remove body-labels entry-label)
          cps))))))

(define (rotate-loops-in-function kfun body cps)
  (define (can-rotate? edges)
    (intset-fold (lambda (label rotate?)
                   (match (intmap-ref cps label)
                     (($ $kreceive) #f)
                     (($ $kargs _ _ (or ($ $branch) ($ $switch))) #f)
                     (($ $kargs _ _ ($ $continue)) rotate?)))
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
                           (($ $kargs _ _ ($ $prompt)) #f)
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
