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
;;; Loop invariant code motion (LICM) hoists terms that don't affect a
;;; loop out of the loop, so that the loop goes faster.
;;;
;;; Code:

(define-module (language cps licm)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps effects-analysis)
  #:use-module (language cps type-checks)
  #:export (hoist-loop-invariant-code))

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

(define (list->intset l)
  (persistent-intset
   (fold1 (lambda (i set) (intset-add! set i)) l empty-intset)))

(define (loop-invariant? label exp loop-vars loop-effects always-reached?)
  (let ((fx (intmap-ref loop-effects label)))
    (and
     (not (causes-effect? fx &allocation))
     (or always-reached?
         (not (causes-effect? fx &type-check)))
     (or (not (causes-effect? fx &write))
         (intmap-fold (lambda (label fx* invariant?)
                        (and invariant?
                             (not (effect-clobbers? fx fx*))))
                      loop-effects #t))
     (or (not (causes-effect? fx &read))
         (intmap-fold (lambda (label fx* invariant?)
                        (and invariant?
                             (not (effect-clobbers? fx* fx))))
                      loop-effects #t))
     (match exp
       ((or ($ $const) ($ $prim) ($ $closure)) #t)
       (($ $prompt) #f) ;; ?
       (($ $branch) #f)
       (($ $primcall 'values) #f)
       (($ $primcall name args)
        (and-map (lambda (arg) (not (intset-ref loop-vars arg)))
                 args))
       (($ $values args)
        (and-map (lambda (arg) (not (intset-ref loop-vars arg)))
                 args))))))

(define (hoist-one cps label cont preds
                   loop-vars loop-effects pre-header-label always-reached?)
  (define (filter-loop-vars names vars)
    (match (vector names vars)
      (#((name . names) (var . vars))
       (if (intset-ref loop-vars var)
           (let-values (((names vars) (filter-loop-vars names vars)))
             (values (cons name names) (cons var vars)))
           (filter-loop-vars names vars)))
      (_ (values '() '()))))
  (define (adjoin-loop-vars loop-vars vars)
    (fold1 (lambda (var loop-vars) (intset-add loop-vars var))
           vars loop-vars))
  (define (hoist-exp src exp def-names def-vars pre-header-label)
    (let* ((hoisted-label pre-header-label)
           (pre-header-label (fresh-label))
           (hoisted-cont
            (rewrite-cont (intmap-ref cps hoisted-label)
              (($ $kargs names vars)
               ($kargs names vars
                 ($continue pre-header-label src ,exp)))))
           (pre-header-cont
            (rewrite-cont (intmap-ref cps hoisted-label)
              (($ $kargs _ _ term)
               ($kargs def-names def-vars ,term)))))
      (values (intmap-add! (intmap-replace! cps hoisted-label hoisted-cont)
                           pre-header-label pre-header-cont)
              pre-header-label)))
  (define (hoist-call src exp req rest def-names def-vars pre-header-label)
    (let* ((hoisted-label pre-header-label)
           (receive-label (fresh-label))
           (pre-header-label (fresh-label))
           (hoisted-cont
            (rewrite-cont (intmap-ref cps hoisted-label)
              (($ $kargs names vars)
               ($kargs names vars
                 ($continue receive-label src ,exp)))))
           (receive-cont
            (build-cont
              ($kreceive req rest pre-header-label)))
           (pre-header-cont
            (rewrite-cont (intmap-ref cps hoisted-label)
              (($ $kargs _ _ term)
               ($kargs def-names def-vars ,term)))))
      (values (intmap-add!
               (intmap-add! (intmap-replace! cps hoisted-label hoisted-cont)
                            receive-label receive-cont)
               pre-header-label pre-header-cont)
              pre-header-label)))
  (match cont
    (($ $kargs names vars ($ $continue k src exp))
     ;; If k is a loop exit, it will be nullary.
     (let-values (((names vars) (filter-loop-vars names vars)))
       (match (intmap-ref cps k)
         (($ $kargs def-names def-vars)
          (cond
           ((not (loop-invariant? label exp loop-vars loop-effects
                                  always-reached?))
            (let* ((loop-vars (adjoin-loop-vars loop-vars def-vars))
                   (loop-vars (match exp
                                (($ $prompt escape? tag handler)
                                 (match (intmap-ref cps handler)
                                   (($ $kreceive arity kargs)
                                    (match (intmap-ref cps kargs)
                                      (($ $kargs names vars)
                                       (adjoin-loop-vars loop-vars vars))))))
                                (_ loop-vars)))
                   (cont (build-cont
                           ($kargs names vars
                             ($continue k src ,exp))))
                   (always-reached?
                    (and always-reached?
                         (match exp
                           (($ $branch) #f)
                           (_ (not (causes-effect? (intmap-ref loop-effects label)
                                                   &type-check)))))))
              (values cps cont loop-vars loop-effects
                      pre-header-label always-reached?)))
           ((trivial-intset (intmap-ref preds k))
            (let-values
                (((cps pre-header-label)
                  (hoist-exp src exp def-names def-vars pre-header-label))
                 ((cont) (build-cont
                           ($kargs names vars
                             ($continue k src ($values ()))))))
              (values cps cont loop-vars (intmap-remove loop-effects label)
                      pre-header-label always-reached?)))
           (else
            (let*-values
                (((def-names def-vars)
                  (match (intmap-ref cps k)
                    (($ $kargs names vars) (values names vars))))
                 ((loop-vars) (adjoin-loop-vars loop-vars def-vars))
                 ((fresh-vars) (map (lambda (_) (fresh-var)) def-vars))
                 ((cps pre-header-label)
                  (hoist-exp src exp def-names fresh-vars pre-header-label))
                 ((cont) (build-cont
                           ($kargs names vars
                             ($continue k src ($values fresh-vars))))))
              (values cps cont loop-vars (intmap-remove loop-effects label)
                      pre-header-label always-reached?)))))
         (($ $kreceive ($ $arity req () rest) kargs)
          (match (intmap-ref cps kargs)
            (($ $kargs def-names def-vars)
             (cond
              ((not (loop-invariant? label exp loop-vars loop-effects
                                     always-reached?))
               (let* ((loop-vars (adjoin-loop-vars loop-vars def-vars))
                      (cont (build-cont
                              ($kargs names vars
                                ($continue k src ,exp)))))
                 (values cps cont loop-vars loop-effects pre-header-label #f)))
              ((trivial-intset (intmap-ref preds k))
               (let ((loop-effects
                      (intmap-remove (intmap-remove loop-effects label) k)))
                 (let-values
                     (((cps pre-header-label)
                       (hoist-call src exp req rest def-names def-vars
                                   pre-header-label))
                      ((cont) (build-cont
                                ($kargs names vars
                                  ($continue kargs src ($values ()))))))
                   (values cps cont loop-vars loop-effects
                           pre-header-label always-reached?))))
              (else
               (let*-values
                   (((loop-vars) (adjoin-loop-vars loop-vars def-vars))
                    ((fresh-vars) (map (lambda (_) (fresh-var)) def-vars))
                    ((cps pre-header-label)
                     (hoist-call src exp req rest def-names fresh-vars
                                 pre-header-label))
                    ((cont) (build-cont
                              ($kargs names vars
                                ($continue kargs src
                                  ($values fresh-vars))))))
                 (values cps cont loop-vars loop-effects
                         pre-header-label always-reached?))))))))))
    (($ $kreceive ($ $arity req () rest) kargs)
     (values cps cont loop-vars loop-effects pre-header-label
             always-reached?))))

(define (hoist-in-loop cps entry body-labels succs preds effects)
  (let* ((interior-succs (intmap-map (lambda (label succs)
                                       (intset-intersect succs body-labels))
                                     succs))
         (sorted-labels (compute-reverse-post-order interior-succs entry))
         (header-label (fresh-label))
         (header-cont (intmap-ref cps entry))
         (loop-vars (match header-cont
                      (($ $kargs names vars) (list->intset vars))))
         (loop-effects (persistent-intmap
                        (intset-fold
                         (lambda (label loop-effects)
                           (let ((label*
                                  (if (eqv? label entry) header-label label))
                                 (fx (intmap-ref effects label)))
                             (intmap-add! loop-effects label* fx)))
                         body-labels empty-intmap)))
         (pre-header-label entry)
         (pre-header-cont (match header-cont
                            (($ $kargs names vars term)
                             (let ((vars* (map (lambda (_) (fresh-var)) vars)))
                               (build-cont
                                 ($kargs names vars*
                                   ($continue header-label #f
                                     ($values vars*))))))))
         (cps (intmap-add! cps header-label header-cont))
         (cps (intmap-replace! cps pre-header-label pre-header-cont))
         (to-visit (match sorted-labels
                     ((head . tail)
                      (unless (eqv? head entry) (error "what?"))
                      (cons header-label tail)))))
    (define (rename-back-edges cont)
      (define (rename label) (if (eqv? label entry) header-label label))
      (rewrite-cont cont
        (($ $kargs names vars ($ $continue kf src ($ $branch kt exp)))
         ($kargs names vars
           ($continue (rename kf) src ($branch (rename kt) ,exp))))
        (($ $kargs names vars ($ $continue k src exp))
         ($kargs names vars
           ($continue (rename k) src ,exp)))
        (($ $kreceive ($ $arity req () rest) k)
         ($kreceive req rest (rename k)))))
    (let lp ((cps cps) (to-visit to-visit)
             (loop-vars loop-vars) (loop-effects loop-effects)
             (pre-header-label pre-header-label) (always-reached? #t))
      (match to-visit
        (() cps)
        ((label . to-visit)
         (call-with-values
             (lambda ()
               (hoist-one cps label (intmap-ref cps label) preds
                          loop-vars loop-effects
                          pre-header-label always-reached?))
           (lambda (cps cont
                        loop-vars loop-effects pre-header-label always-reached?)
             (lp (intmap-replace! cps label (rename-back-edges cont)) to-visit
                 loop-vars loop-effects pre-header-label always-reached?))))))))

(define (hoist-in-function kfun body cps)
  (let* ((succs (compute-successors cps kfun))
         (preds (invert-graph succs))
         (loops (intmap-fold
                 (lambda (id scc loops)
                   (cond
                    ((trivial-intset scc) loops)
                    ((find-entry scc preds)
                     => (lambda (entry) (intmap-add! loops entry scc)))
                    (else loops)))
                 (compute-strongly-connected-components succs kfun)
                 empty-intmap)))
    (if (eq? empty-intset loops)
        cps
        (let ((effects (compute-effects/elide-type-checks
                        (intset-fold (lambda (label body-conts)
                                       (intmap-add! body-conts label
                                                    (intmap-ref cps label)))
                                     body empty-intmap))))
          (persistent-intmap
           (intmap-fold (lambda (entry scc cps)
                          (hoist-in-loop cps entry scc succs preds effects))
                        loops cps))))))

(define (hoist-loop-invariant-code cps)
  (with-fresh-name-state cps
    (intmap-fold hoist-in-function
                 (compute-reachable-functions cps)
                 cps)))
