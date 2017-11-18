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
;;; Split functions bound in $rec expressions into strongly-connected
;;; components.  The result will be that each $rec binds a
;;; strongly-connected component of mutually recursive functions.
;;;
;;; Code:

(define-module (language cps split-rec)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:export (split-rec))

(define (compute-free-vars conts kfun)
  "Compute a FUN-LABEL->FREE-VAR... map describing all free variable
references."
  (define (add-def var defs) (intset-add! defs var))
  (define (add-defs vars defs)
    (match vars
      (() defs)
      ((var . vars) (add-defs vars (add-def var defs)))))
  (define (add-use var uses) (intset-add! uses var))
  (define (add-uses vars uses)
    (match vars
      (() uses)
      ((var . vars) (add-uses vars (add-use var uses)))))
  (define (visit-nested-funs body)
    (intset-fold
     (lambda (label out)
       (match (intmap-ref conts label)
         (($ $kargs _ _ ($ $continue _ _
                           ($ $fun kfun)))
          (intmap-union out (visit-fun kfun)))
         (($ $kargs _ _ ($ $continue _ _
                           ($ $rec _ _ (($ $fun kfun) ...))))
          (fold (lambda (kfun out)
                  (intmap-union out (visit-fun kfun)))
                out kfun))
         (_ out)))
     body
     empty-intmap))
  (define (visit-fun kfun)
    (let* ((body (compute-function-body conts kfun))
           (free (visit-nested-funs body)))
      (call-with-values
          (lambda ()
            (intset-fold
             (lambda (label defs uses)
               (match (intmap-ref conts label)
                 (($ $kargs names vars ($ $continue k src exp))
                  (values
                   (add-defs vars defs)
                   (match exp
                     ((or ($ $const) ($ $prim)) uses)
                     (($ $fun kfun)
                      (intset-union (persistent-intset uses)
                                    (intmap-ref free kfun)))
                     (($ $rec names vars (($ $fun kfun) ...))
                      (fold (lambda (kfun uses)
                              (intset-union (persistent-intset uses)
                                            (intmap-ref free kfun)))
                            uses kfun))
                     (($ $values args)
                      (add-uses args uses))
                     (($ $call proc args)
                      (add-use proc (add-uses args uses)))
                     (($ $branch kt ($ $values (arg)))
                      (add-use arg uses))
                     (($ $branch kt ($ $primcall name args))
                      (add-uses args uses))
                     (($ $primcall name args)
                      (add-uses args uses))
                     (($ $prompt escape? tag handler)
                      (add-use tag uses)))))
                 (($ $kfun src meta self)
                  (values (add-def self defs) uses))
                 (_ (values defs uses))))
             body empty-intset empty-intset))
        (lambda (defs uses)
          (intmap-add free kfun (intset-subtract
                                 (persistent-intset uses)
                                 (persistent-intset defs)))))))
  (visit-fun kfun))

(define (compute-split fns free-vars)
  (define (get-free kfun)
    ;; It's possible for a fun to have been skipped by
    ;; compute-free-vars, if the fun isn't reachable.  Fall back to
    ;; empty-intset for the fun's free vars, in that case.
    (intmap-ref free-vars kfun (lambda (_) empty-intset)))
  (let* ((vars (intmap-keys fns))
         (edges (intmap-map
                 (lambda (var kfun)
                   (intset-intersect (get-free kfun) vars))
                 fns)))
    (compute-sorted-strongly-connected-components edges)))

(define (intmap-acons k v map)
  (intmap-add map k v))

(define (split-rec conts)
  (let ((free (compute-free-vars conts 0)))
    (with-fresh-name-state conts
      (persistent-intmap
       (intmap-fold
        (lambda (label cont out)
          (match cont
            (($ $kargs cont-names cont-vars
                ($ $continue k src ($ $rec names vars (($ $fun kfuns) ...))))
             (let ((fns (fold intmap-acons empty-intmap vars kfuns))
                   (fn-names (fold intmap-acons empty-intmap vars names)))
               (match (compute-split fns free)
                 (()
                  ;; Remove trivial $rec.
                  (with-cps out
                    (setk label ($kargs cont-names cont-vars
                                  ($continue k src ($values ()))))))
                 ((_)
                  ;; Bound functions already form a strongly-connected
                  ;; component.
                  out)
                 (components
                  ;; Multiple components.  Split them into separate $rec
                  ;; expressions.
                  (define (build-body out components)
                    (match components
                      (()
                       (match (intmap-ref out k)
                         (($ $kargs names vars term)
                          (with-cps (intmap-remove out k)
                            term))))
                      ((vars . components)
                       (match (intset-fold
                               (lambda (var out)
                                 (let ((name (intmap-ref fn-names var))
                                       (fun (build-exp
                                              ($fun (intmap-ref fns var)))))
                                   (cons (list name var fun) out)))
                               vars '())
                         (((name var fun) ...)
                          (with-cps out
                            (let$ body (build-body components))
                            (letk kbody ($kargs name var ,body))
                            (build-term
                              ($continue kbody src ($rec name var fun)))))))))
                  (with-cps out
                    (let$ body (build-body components))
                    (setk label ($kargs cont-names cont-vars ,body)))))))
             (_ out)))
          conts
          conts)))))
