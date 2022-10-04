;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2021 Free Software Foundation, Inc.

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
;;; Calls to well-known functions might be able to elide the values
;;; count check if the callee has a known return arity.
;;;
;;; Code:

(define-module (language cps return-types)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:export (optimize-known-return-types))

;; analysis := intmap of function label -> return-type
;; return-type := 'none | (value-type ...) | 'unknown
;; value-type := '_
;; tail-callers := intmap of callee label -> intset of caller label
;;
;; fixpoint on analysis

(define (adjoin-unknown-return-type fn analysis)
  (intmap-replace analysis fn 'unknown))

(define (adjoin-return-type fn type analysis)
  (match (intmap-ref analysis fn)
    ((? (lambda (type*) (equal? type type*)))
     analysis)
    ('none
     (intmap-replace analysis fn type))
    (_
     (adjoin-unknown-return-type fn analysis))))

(define (analyze1 fn body conts tail-callers analysis)
  (define preds (compute-predecessors conts fn #:labels body))
  (define (adjoin-tail-caller caller callee tail-callers)
    (intmap-add tail-callers callee (intset caller) intset-union))
  (define (visit-tail-cont cont tail-callers analysis)
    ;; Predecessors of tail are only calls and $values.
    (match cont
      (($ $kfun) (values tail-callers analysis))
      (($ $kargs _ _ ($ $continue _ _ exp))
       (match exp
         (($ $call proc args)
          (values tail-callers
                  (adjoin-unknown-return-type fn analysis)))
         (($ $callk k proc args)
          (values (adjoin-tail-caller fn k tail-callers)
                  analysis))
         (($ $values vals)
          (let ((type (map (lambda (_) '_) vals)))
            (values tail-callers
                    (adjoin-return-type fn type analysis))))))))
  (match (intmap-ref conts fn)
    (($ $kfun src meta self tail entry)
     (fold2
      (lambda (pred tail-callers analysis)
        (visit-tail-cont (intmap-ref conts pred) tail-callers analysis))
      (intmap-ref preds tail)
      tail-callers
      analysis))))

(define (analyze/local functions conts)
  (let ((tail-callers (intmap-map (lambda (k v) empty-intset) functions))
        (analysis (intmap-map (lambda (k v) 'none) functions)))
    (intmap-fold (lambda (fn body tail-callers analysis)
                   (analyze1 fn body conts tail-callers analysis))
                 functions tail-callers analysis)))

(define (propagate fn tail-callers worklist analysis)
  (let ((preds (intmap-ref tail-callers fn))
        (type (intmap-ref analysis fn)))
    (intset-fold (lambda (pred worklist analysis)
                   (let ((analysis* (adjoin-return-type pred type analysis)))
                     (values (if (eq? analysis analysis*)
                                 worklist
                                 (intset-add worklist pred))
                             analysis*)))
                 preds worklist analysis)))

(define (analyze/global tail-callers analysis)
  (worklist-fold
   (lambda (worklist analysis)
     (intset-fold (lambda (fn worklist analysis)
                    (propagate fn tail-callers worklist analysis))
                  worklist empty-intset analysis))
   (intmap-keys tail-callers)
   analysis))

(define (compute-return-types functions conts)
  (call-with-values (lambda () (analyze/local functions conts))
    (lambda (tail-callers analysis)
      (analyze/global tail-callers analysis))))

(define (optimize-return-continuation conts k req rest kargs type)
  (let ((nvalues (length type)))
    (cond
     ((= nvalues (length req))
      (if rest
          (let ((vars (map (lambda (_) (fresh-var)) req)))
            (with-cps conts
              (letv nil)
              (letk kvals ($kargs ('nil) (nil)
                            ($continue kargs #f
                              ($values ,(append vars (list nil))))))
              (letk knil ($kargs req vars
                           ($continue kvals #f ($const '()))))
              knil))
          (values conts kargs)))
     (else
      (values conts k)))))

(define (optimize-known-return-types conts)
  (define functions (compute-reachable-functions conts))
  (define return-types (compute-return-types functions conts))
  (define (fold-live-conts f functions seed)
    (intmap-fold
     (lambda (fn body seed)
       (intset-fold (lambda (label seed)
                      (f label (intmap-ref conts label) seed))
                    body seed))
     functions seed))
  (with-fresh-name-state conts
    (fold-live-conts
     (lambda (label cont conts)
       (match cont
         (($ $kargs names vars
             ($ $continue k src ($ $callk fn proc args)))
          ;; If the callee has known return type, we
          ;; might be able to avoid the number-of-values check.
          (match (intmap-ref return-types fn)
            ('none
             ;; Function does not return.  Do nothing for now.
             conts)
            ('unknown
             ;; Unknown return type.  Leave as is.
             conts)
            (type
             ;; Known return type.  Check if compatible with
             ;; continuation, and if so, elide the number-of-values
             ;; check.
             (match (intmap-ref conts k)
               (($ $kreceive ($ $arity req () rest () #f) kargs)
                (with-cps conts
                  (let$ k* (optimize-return-continuation k req rest kargs type))
                  (setk label ($kargs names vars
                                ($continue k* src ($callk fn proc args))))))
               (_ conts)))))
         (_ conts)))
     functions conts)))
