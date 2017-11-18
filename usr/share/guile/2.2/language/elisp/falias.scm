(define-module (language elisp falias)
  #:export (falias?
            make-falias
            falias-function
            falias-object))

(define <falias-vtable>
  (make-struct <applicable-struct-vtable>
               0
               (make-struct-layout "pwpw")
               (lambda (object port)
                 (format port "#<falias ~S>" (falias-object object)))))

(set-struct-vtable-name! <falias-vtable> 'falias)

(define (falias? object)
  (and (struct? object)
       (eq? (struct-vtable object) <falias-vtable>)))

(define (make-falias f object)
  (make-struct <falias-vtable> 0 f object))

(define (falias-function object)
  (struct-ref object 0))

(define (falias-object object)
  (struct-ref object 1))
