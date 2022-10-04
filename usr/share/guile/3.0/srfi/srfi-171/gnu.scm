;; 	Copyright (C) 2020 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (srfi srfi-171 gnu)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-171 meta)
  #:export (tbatch tfold))


(define tbatch
  (case-lambda
    ((reducer)
     (tbatch identity reducer))
    ((t r)
     (lambda (reducer)
       (let ((cur-reducer (t r))
             (cur-state (r)))
         (case-lambda
           (() (reducer))
           ((result)
            (if (equal? cur-state (cur-reducer))
                (reducer result)
                (let ((new-res (reducer result (cur-reducer cur-state))))
                  (if (reduced? new-res)
                      (reducer (unreduce new-res))
                      (reducer new-res)))))
           ((result value)
            (let ((val (cur-reducer cur-state value)))
              (cond
               ;; cur-reducer is done. Push value downstream
               ;; re-instantiate the state and the cur-reducer
               ((reduced? val)
                (let ((unreduced-val (unreduce val)))
                  (set! cur-reducer (t r))
                  (set! cur-state (cur-reducer))
                  (reducer result (cur-reducer unreduced-val))))
               (else
                (set! cur-state val)
                result))))))))))


(define* (tfold reducer #:optional (seed (reducer)))
  (lambda (r)
    (let ((state seed))
      (case-lambda
        (() (r))
        ((result) (r result))
        ((result value)
         (set! state (reducer state value))
         (if (reduced? state)
             (reduced (reducer (unreduce state)))
             (r result state)))))))
