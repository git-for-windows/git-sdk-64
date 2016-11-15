;;; ECMAScript for Guile

;; Copyright (C) 2009, 2013 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language ecmascript base)
  #:use-module (oop goops)
  #:export (*undefined* *this*
            <js-object> *object-prototype*
            js-prototype js-props js-prop-attrs js-value js-constructor js-class
            pget prop-keys prop-attrs prop-has-attr? pput has-property? pdel

            object->string object->number object->value/string
            object->value/number object->value

            ->primitive ->boolean ->number ->integer ->int32 ->uint32
            ->uint16 ->string ->object

            call/this* call/this lambda/this define-js-method

            new-object new))

(define *undefined* ((@@ (oop goops) make-unbound)))
(define *this* (make-fluid))

(define-class <js-object> ()
  (prototype #:getter js-prototype #:init-keyword #:prototype
             #:init-thunk (lambda () *object-prototype*))
  (props #:getter js-props #:init-form (make-hash-table 7))
  (prop-attrs #:getter js-prop-attrs #:init-value #f)
  (value #:getter js-value #:init-value #f #:init-keyword #:value)
  (constructor #:getter js-constructor #:init-value #f #:init-keyword #:constructor)
  (class #:getter js-class #:init-value "Object" #:init-keyword #:class))

(define-method (prop-keys (o <js-object>))
  (hash-map->list (lambda (k v) k) (js-props o)))

(define-method (pget (o <js-object>) (p <string>))
  (pget o (string->symbol p)))

(define-method (pget (o <js-object>) p)
  (let ((h (hashq-get-handle (js-props o) p)))
    (if h
        (cdr h)
        (let ((proto (js-prototype o)))
          (if proto
              (pget proto p)
              *undefined*)))))

(define-method (prop-attrs (o <js-object>) p)
  (or (let ((attrs (js-prop-attrs o)))
        (and attrs (hashq-ref (js-prop-attrs o) p)))
      (let ((proto (js-prototype o)))
        (if proto
            (prop-attrs proto p)
            '()))))

(define-method (prop-has-attr? (o <js-object>) p attr)
  (memq attr (prop-attrs o p)))

(define-method (pput (o <js-object>) p v)
  (if (prop-has-attr? o p 'ReadOnly)
      (throw 'ReferenceError o p)
      (hashq-set! (js-props o) p v)))

(define-method (pput (o <js-object>) (p <string>) v)
  (pput o (string->symbol p) v))

(define-method (pdel (o <js-object>) p)
  (if (prop-has-attr? o p 'DontDelete)
      #f
      (begin
        (pput o p *undefined*)
        #t)))

(define-method (pdel (o <js-object>) (p <string>) v)
  (pdel o (string->symbol p)))

(define-method (has-property? (o <js-object>) p)
  (if (hashq-get-handle (js-props o) p)
      #t
      (let ((proto (js-prototype o)))
        (if proto
            (has-property? proto p)
            #f))))

(define (call/this* this f)
  (with-fluid* *this* this f))

(define-macro (call/this this f . args)
  `(with-fluid* *this* ,this (lambda () (,f . ,args))))
(define-macro (lambda/this formals . body)
  `(lambda ,formals (let ((this (fluid-ref *this*))) . ,body)))
(define-macro (define-js-method object name-and-args . body)
  `(pput ,object ',(car name-and-args) (lambda/this ,(cdr name-and-args) . ,body)))

(define *object-prototype* #f)
(set! *object-prototype* (make <js-object>))

(define-js-method *object-prototype* (toString)
  (format #f "[object ~A]" (js-class this)))
(define-js-method *object-prototype* (toLocaleString . args)
  ((pget *object-prototype* 'toString)))
(define-js-method *object-prototype* (valueOf)
  this)
(define-js-method *object-prototype* (hasOwnProperty p)
  (and (hashq-get-handle (js-props this) p) #t))
(define-js-method *object-prototype* (isPrototypeOf v)
  (eq? this (js-prototype v)))
(define-js-method *object-prototype* (propertyIsEnumerable p)
  (and (hashq-get-handle (js-props this) p)
       (not (prop-has-attr? this p 'DontEnum))))

(define (object->string o error?)
  (let ((toString (pget o 'toString)))
    (if (procedure? toString)
        (let ((x (call/this o toString)))
          (if (and error? (is-a? x <js-object>))
              (throw 'TypeError o 'default-value)
              x))
        (if error?
            (throw 'TypeError o 'default-value)
            o))))
              
(define (object->number o error?)
  (let ((valueOf (pget o 'valueOf)))
    (if (procedure? valueOf)
        (let ((x (call/this o valueOf)))
          (if (and error? (is-a? x <js-object>))
              (throw 'TypeError o 'default-value)
              x))
        (if error?
            (throw 'TypeError o 'default-value)
            o))))
              
(define (object->value/string o)
  (if (is-a? o <js-object>)
      (object->number o #t)
      o))

(define (object->value/number o)
  (if (is-a? o <js-object>)
      (object->string o #t)
      o))

(define (object->value o)
  ;; FIXME: if it's a date, we should try numbers first
  (object->value/string o))
              
(define (->primitive x)
  (if (is-a? x <js-object>)
      (object->value x)
      x))

(define (->boolean x)
  (not (or (not x) (null? x) (eq? x *undefined*)
           (and (number? x) (or (zero? x) (nan? x)))
           (and (string? x) (= (string-length x) 0)))))

(define (->number x)
  (cond ((number? x) x)
        ((boolean? x) (if x 1 0))
        ((null? x) 0)
        ((eq? x *undefined*) +nan.0)
        ((is-a? x <js-object>) (object->number x #t))
        ((string? x) (string->number x))
        (else (throw 'TypeError x '->number))))

(define (->integer x)
  (let ((n (->number x)))
    (cond ((nan? n) 0)
          ((zero? n) n)
          ((inf? n) n)
          (else (inexact->exact (round n))))))

(define (->int32 x)
  (let ((n (->number x)))
    (if (or (nan? n) (zero? n) (inf? n))
        0
        (let ((m (logand (1- (ash 1 32)) (inexact->exact (round n)))))
          (if (negative? n)
              (- m (ash 1 32))
              m)))))

(define (->uint32 x)
  (let ((n (->number x)))
    (if (or (nan? n) (zero? n) (inf? n))
        0
        (logand (1- (ash 1 32)) (inexact->exact (round n))))))

(define (->uint16 x)
  (let ((n (->number x)))
    (if (or (nan? n) (zero? n) (inf? n))
        0
        (logand (1- (ash 1 16)) (inexact->exact (round n))))))

(define (->string x)
  (cond ((eq? x *undefined*) "undefined")
        ((null? x) "null")
        ((boolean? x) (if x "true" "false"))
        ((string? x) x)
        ((number? x)
         (cond ((nan? x) "NaN")
               ((zero? x) "0")
               ((inf? x) "Infinity")
               (else (number->string x))))
        (else (->string (object->value/string x)))))

(define (->object x)
  (cond ((eq? x *undefined*) (throw 'TypeError x '->object))
        ((null? x) (throw 'TypeError x '->object))
        ((boolean? x) (make <js-object> #:prototype Boolean #:value x))
        ((number? x) (make <js-object> #:prototype String #:value x))
        ((string? x) (make <js-object> #:prototype Number #:value x))
        (else x)))

(define (new-object . pairs)
  (let ((o (make <js-object>)))
    (map (lambda (pair)
           (pput o (car pair) (cdr pair)))
         pairs)
    o))
(slot-set! *object-prototype* 'constructor new-object)

(define-method (new o . initargs)
  (let ((ctor (js-constructor o)))
    (if (not ctor)
        (throw 'TypeError 'new o)
        (let ((o (make <js-object>
                   #:prototype (or (js-prototype o) *object-prototype*))))
          (let ((new-o (call/this o apply ctor initargs)))
            (if (is-a? new-o <js-object>)
                new-o
                o))))))
