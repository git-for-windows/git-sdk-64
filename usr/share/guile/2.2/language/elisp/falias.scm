(define-module (language elisp falias)
  #:export (falias?
            make-falias
            falias-function
            falias-object))

(define <falias-vtable>
  (make-struct/no-tail
   <applicable-struct-vtable>
   (make-struct-layout "pwpw")
   (lambda (object port)
     (format port "#<falias ~S>" (falias-object object)))))

(set-struct-vtable-name! <falias-vtable> 'falias)

(define (falias? object)
  (and (struct? object)
       (eq? (struct-vtable object) <falias-vtable>)))

(define (make-falias f object)
  (make-struct/no-tail <falias-vtable> f object))

(define (falias-function object)
  (struct-ref object 0))

(define (falias-object object)
  (struct-ref object 1))
