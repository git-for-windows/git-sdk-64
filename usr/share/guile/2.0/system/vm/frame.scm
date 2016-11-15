;;; Guile VM frame functions

;;; Copyright (C) 2001, 2005, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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
  #:use-module (system vm program)
  #:use-module (system vm instruction)
  #:use-module (system vm objcode)
  #:export (frame-bindings
            frame-lookup-binding
            frame-binding-ref frame-binding-set!
            frame-next-source frame-call-representation
            frame-environment
            frame-object-binding frame-object-name
            frame-return-values))

(define (frame-bindings frame)
  (let ((p (frame-procedure frame)))
    (if (program? p)
        (program-bindings-for-ip p (frame-instruction-pointer frame))
        '())))

(define (frame-lookup-binding frame var)
  (let lp ((bindings (frame-bindings frame)))
    (cond ((null? bindings)
           #f)
          ((eq? (binding:name (car bindings)) var)
           (car bindings))
          (else
           (lp (cdr bindings))))))

(define (frame-binding-set! frame var val)
  (frame-local-set! frame
                    (binding:index
                     (or (frame-lookup-binding frame var)
                         (error "variable not bound in frame" var frame)))
                    val))

(define (frame-binding-ref frame var)
  (frame-local-ref frame
                   (binding:index
                    (or (frame-lookup-binding frame var)
                        (error "variable not bound in frame" var frame)))))


;; This function is always called to get some sort of representation of the
;; frame to present to the user, so let's do the logical thing and dispatch to
;; frame-call-representation.
(define (frame-arguments frame)
  (cdr (frame-call-representation frame)))



;;;
;;; Pretty printing
;;;

(define (frame-next-source frame)
  (let ((proc (frame-procedure frame)))
    (if (program? proc)
        (program-source proc
                        (frame-instruction-pointer frame)
                        (program-sources-pre-retire proc))
        '())))


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

(define (frame-call-representation frame)
  (let ((p (frame-procedure frame)))
    (cons
     (or (false-if-exception (procedure-name p)) p)
     (cond
      ((and (program? p)
            (program-arguments-alist p (frame-instruction-pointer frame)))
       ;; case 1
       => (lambda (arguments)
            (define (binding-ref sym i)
              (cond
               ((frame-lookup-binding frame sym)
                => (lambda (b) (frame-local-ref frame (binding:index b))))
               ((< i (frame-num-locals frame))
                (frame-local-ref frame i))
               (else
                ;; let's not error here, as we are called during backtraces...
                '???)))
            (let lp ((req (or (assq-ref arguments 'required) '()))
                     (opt (or (assq-ref arguments 'optional) '()))
                     (key (or (assq-ref arguments 'keyword) '()))
                     (rest (or (assq-ref arguments 'rest) #f))
                     (i 0))
              (cond
               ((pair? req)
                (cons (binding-ref (car req) i)
                      (lp (cdr req) opt key rest (1+ i))))
               ((pair? opt)
                (cons (binding-ref (car opt) i)
                      (lp req (cdr opt) key rest (1+ i))))
               ((pair? key)
                (cons* (caar key)
                       (frame-local-ref frame (cdar key))
                       (lp req opt (cdr key) rest (1+ i))))
               (rest
                (binding-ref rest i))
               (else
                '())))))
      (else
       ;; case 2
       (map (lambda (i)
              (frame-local-ref frame i))
            (iota (frame-num-locals frame))))))))



;;; Misc
;;;

(define (frame-environment frame)
  (map (lambda (binding)
	 (cons (binding:name binding) (frame-binding-ref frame binding)))
       (frame-bindings frame)))

(define (frame-object-binding frame obj)
  (do ((bs (frame-bindings frame) (cdr bs)))
      ((or (null? bs) (eq? obj (frame-binding-ref frame (car bs))))
       (and (pair? bs) (car bs)))))

(define (frame-object-name frame obj)
  (cond ((frame-object-binding frame obj) => binding:name)
	(else #f)))

;; Nota bene, only if frame is in a return context (i.e. in a
;; pop-continuation hook dispatch).
(define (frame-return-values frame)
  (let* ((len (frame-num-locals frame))
         (nvalues (frame-local-ref frame (1- len))))
    (map (lambda (i)
           (frame-local-ref frame (+ (- len nvalues 1) i)))
         (iota nvalues))))
