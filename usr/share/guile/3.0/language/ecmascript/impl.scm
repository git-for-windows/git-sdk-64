;;; ECMAScript for Guile

;; Copyright (C) 2009 Free Software Foundation, Inc.

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

(define-module (language ecmascript impl)
  #:use-module (oop goops)
  #:use-module (language ecmascript base)
  #:use-module (language ecmascript function)
  #:use-module (language ecmascript array)
  #:re-export (*undefined* *this* call/this*
               pget pput pdel has-property?
               ->boolean ->number
               new-object new new-array)
  #:export (js-init get-this
            typeof
            bitwise-not logical-not
            shift
            mod
            band bxor bior
            make-enumerator))


(define-class <js-module-object> (<js-object>)
  (module #:init-form (current-module) #:init-keyword #:module
          #:getter js-module))
(define-method (pget (o <js-module-object>) (p <string>))
  (pget o (string->symbol p)))
(define-method (pget (o <js-module-object>) (p <symbol>))
  (let ((v (module-variable (js-module o) p)))
    (if v
        (variable-ref v)
        (next-method))))
(define-method (pput (o <js-module-object>) (p <string>) v)
  (pput o (string->symbol p) v))
(define-method (pput (o <js-module-object>) (p <symbol>) v)
  (module-define! (js-module o) p v))
(define-method (prop-attrs (o <js-module-object>) (p <symbol>))
  (cond ((module-local-variable (js-module o) p) '())
        ((module-variable (js-module o) p) '(DontDelete ReadOnly))
        (else (next-method))))
(define-method (prop-attrs (o <js-module-object>) (p <string>))
  (prop-attrs o (string->symbol p)))
(define-method (prop-keys (o <js-module-object>))
  (append (hash-map->list (lambda (k v) k) (module-obarray (js-module o)))
          (next-method)))

;; we could make a renamer, but having obj['foo-bar'] should be enough
(define (js-require modstr)
  (make <js-module-object> #:module
        (resolve-interface (map string->symbol (string-split modstr #\.)))))
      
(define-class <js-global-object> (<js-module-object>))
(define-method (js-module (o <js-global-object>))
  (current-module))

(define (init-js-bindings! mod)
  (module-define! mod 'NaN +nan.0)
  (module-define! mod 'Infinity +inf.0)
  (module-define! mod 'undefined *undefined*)
  (module-define! mod 'require js-require)
  ;; isNAN, isFinite, parseFloat, parseInt, eval
  ;; decodeURI, decodeURIComponent, encodeURI, encodeURIComponent
  ;; Object Function Array String Boolean Number Date RegExp Error EvalError
  ;; RangeError ReferenceError SyntaxError TypeError URIError
  (module-define! mod 'Object *object-prototype*)
  (module-define! mod 'Array *array-prototype*))

(define (js-init)
  (cond ((get-this))
        (else
         (fluid-set! *this* (make <js-global-object>))
         (init-js-bindings! (current-module)))))

(define (get-this)
  (fluid-ref *this*))

(define (typeof x)
  (cond ((eq? x *undefined*) "undefined")
        ((null? x) "object")
        ((boolean? x) "boolean")
        ((number? x) "number")
        ((string? x) "string")
        ((procedure? x) "function")
        ((is-a? x <js-object>) "object")
        (else "scm")))

(define bitwise-not lognot)
(define (logical-not x)
  (not (->boolean (->primitive x))))

(define shift ash)

(define band logand)
(define bxor logxor)
(define bior logior)

(define mod modulo)

(define-method (+ (a <string>) (b <string>))
  (string-append a b))

(define-method (+ (a <string>) b)
  (string-append a (->string b)))

(define-method (+ a (b <string>))
  (string-append (->string a) b))

(define-method (+ a b)
  (+ (->number a) (->number b)))

(define-method (- a b)
  (- (->number a) (->number b)))

(define-method (* a b)
  (* (->number a) (->number b)))

(define-method (/ a b)
  (/ (->number a) (->number b)))

(define-method (< a b)
  (< (->number a) (->number b)))
(define-method (< (a <string>) (b <string>))
  (string< a b))

(define-method (<= a b)
  (<= (->number a) (->number b)))
(define-method (<= (a <string>) (b <string>))
  (string<= a b))

(define-method (>= a b)
  (>= (->number a) (->number b)))
(define-method (>= (a <string>) (b <string>))
  (string>= a b))

(define-method (> a b)
  (> (->number a) (->number b)))
(define-method (> (a <string>) (b <string>))
  (string> a b))

(define (obj-and-prototypes o)
  (if o
      (cons o (obj-and-prototypes (js-prototype o)))
      '()))
              
(define (make-enumerator obj)
  (let ((props (make-hash-table 23)))
    (for-each (lambda (o)
                (for-each (lambda (k) (hashq-set! props k #t))
                          (prop-keys o)))
              (obj-and-prototypes obj))
    (apply new-array (filter (lambda (p)
                               (not (prop-has-attr? obj p 'DontEnum)))
                             (hash-map->list (lambda (k v) k) props)))))
