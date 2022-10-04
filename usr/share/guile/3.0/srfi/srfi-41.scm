;;; srfi-41.scm -- SRFI 41 streams

;; Copyright (c) 2007 Philip L. Bewig
;; Copyright (c) 2011, 2012, 2013 Free Software Foundation, Inc.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF, OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-module (srfi srfi-41)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (stream-null stream-cons stream? stream-null? stream-pair?
            stream-car stream-cdr stream-lambda define-stream
            list->stream port->stream stream stream->list stream-append
            stream-concat stream-constant stream-drop stream-drop-while
            stream-filter stream-fold stream-for-each stream-from
            stream-iterate stream-length stream-let stream-map
            stream-match stream-of stream-range stream-ref stream-reverse
            stream-scan stream-take stream-take-while stream-unfold
            stream-unfolds stream-zip))

(cond-expand-provide (current-module) '(srfi-41))

;;; Private supporting functions and macros.

(define-syntax-rule (must pred obj func msg args ...)
  (let ((item obj))
    (unless (pred item)
      (throw 'wrong-type-arg func msg (list args ...) (list item)))))

(define-syntax-rule (must-not pred obj func msg args ...)
  (let ((item obj))
    (when (pred item)
      (throw 'wrong-type-arg func msg (list args ...) (list item)))))

(define-syntax-rule (must-every pred objs func msg args ...)
  (let ((flunk (remove pred objs)))
    (unless (null? flunk)
      (throw 'wrong-type-arg func msg (list args ...) flunk))))

(define-syntax-rule (first-value expr)
  (receive (first . _) expr
    first))

(define-syntax-rule (second-value expr)
  (receive (first second . _) expr
    second))

(define-syntax-rule (third-value expr)
  (receive (first second third . _) expr
    third))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (name . args) body ...)
     (define-syntax name (lambda* args body ...)))
    ((_ name syntax)
     (define-syntax name syntax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Here we include a copy of the code of srfi-45.scm (but with renamed
;; identifiers), in order to create a new promise type that's disjoint
;; from the promises created by srfi-45.  Ideally this should be done
;; using a 'make-promise-type' macro that instantiates a copy of this
;; code, but a psyntax bug in Guile 2.0 prevents this from working
;; properly: <http://bugs.gnu.org/13995>.  So for now, we duplicate the
;; code.

;; Copyright (C) 2010, 2011 Free Software Foundation, Inc.
;; Copyright (C) 2003 André van Tonder. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-record-type stream-promise (make-stream-promise val) stream-promise?
  (val stream-promise-val stream-promise-val-set!))

(define-record-type stream-value (make-stream-value tag proc) stream-value?
  (tag stream-value-tag stream-value-tag-set!)
  (proc stream-value-proc stream-value-proc-set!))

(define-syntax-rule (stream-lazy exp)
  (make-stream-promise (make-stream-value 'lazy (lambda () exp))))

(define (stream-eager x)
  (make-stream-promise (make-stream-value 'eager x)))

(define-syntax-rule (stream-delay exp)
  (stream-lazy (stream-eager exp)))

(define (stream-force promise)
  (let ((content (stream-promise-val promise)))
    (case (stream-value-tag content)
      ((eager) (stream-value-proc content))
      ((lazy)  (let* ((promise* ((stream-value-proc content)))
                      (content  (stream-promise-val promise)))
                 (if (not (eqv? (stream-value-tag content) 'eager))
                     (begin (stream-value-tag-set! content
                                                   (stream-value-tag (stream-promise-val promise*)))
                            (stream-value-proc-set! content
                                                    (stream-value-proc (stream-promise-val promise*)))
                            (stream-promise-val-set! promise* content)))
                 (stream-force promise))))))

;;
;; End of the copy of the code from srfi-45.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Primitive stream functions and macros: (streams primitive)

(define stream? stream-promise?)

(define %stream-null (cons 'stream 'null))
(define stream-null (stream-eager %stream-null))

(define (stream-null? obj)
  (and (stream-promise? obj)
       (eqv? (stream-force obj) %stream-null)))

(define-record-type stream-pare (make-stream-pare kar kdr) stream-pare?
  (kar stream-kar)
  (kdr stream-kdr))

(define (stream-pair? obj)
  (and (stream-promise? obj) (stream-pare? (stream-force obj))))

(define-syntax-rule (stream-cons obj strm)
  (stream-eager (make-stream-pare (stream-delay obj) (stream-lazy strm))))

(define (stream-car strm)
  (must stream? strm 'stream-car "non-stream")
  (let ((pare (stream-force strm)))
    (must stream-pare? pare 'stream-car "null stream")
    (stream-force (stream-kar pare))))

(define (stream-cdr strm)
  (must stream? strm 'stream-cdr "non-stream")
  (let ((pare (stream-force strm)))
    (must stream-pare? pare 'stream-cdr "null stream")
    (stream-kdr pare)))

(define-syntax-rule (stream-lambda formals body0 body1 ...)
  (lambda formals (stream-lazy (begin body0 body1 ...))))

(define* (stream-promise-visit promise #:key on-eager on-lazy)
  (define content (stream-promise-val promise))
  (case (stream-value-tag content)
    ((eager) (on-eager (stream-value-proc content)))
    ((lazy)  (on-lazy (stream-value-proc content)))))

(set-record-type-printer! stream-promise
  (lambda (strm port)
    (display "#<stream" port)
    (let loop ((strm strm))
      (stream-promise-visit strm
        #:on-eager (lambda (pare)
                     (cond ((eq? pare %stream-null)
                            (write-char #\> port))
                           (else
                            (write-char #\space port)
                            (stream-promise-visit (stream-kar pare)
                              #:on-eager (cut write <> port)
                              #:on-lazy  (lambda (_) (write-char #\? port)))
                            (loop (stream-kdr pare)))))
        #:on-lazy (lambda (_) (display " ...>" port))))))

;;; Derived stream functions and macros: (streams derived)

(define-syntax-rule (define-stream (name . formal) body0 body1 ...)
  (define name (stream-lambda formal body0 body1 ...)))

(define-syntax-rule (stream-let tag ((name val) ...) body1 body2 ...)
  ((letrec ((tag (stream-lambda (name ...) body1 body2 ...))) tag) val ...))

(define (list->stream objs)
  (define (list? x)
    (or (proper-list? x) (circular-list? x)))
  (must list? objs 'list->stream "non-list argument")
  (stream-let recur ((objs objs))
    (if (null? objs) stream-null
        (stream-cons (car objs) (recur (cdr objs))))))

(define* (port->stream #:optional (port (current-input-port)))
  (must input-port? port 'port->stream "non-input-port argument")
  (stream-let recur ()
    (let ((c (read-char port)))
      (if (eof-object? c) stream-null
          (stream-cons c (recur))))))

(define-syntax stream
  (syntax-rules ()
    ((_) stream-null)
    ((_ x y ...) (stream-cons x (stream y ...)))))

;; Common helper for the various eager-folding functions, such as
;; stream-fold, stream-drop, stream->list, stream-length, etc.
(define-inlinable (stream-fold-aux proc base strm limit)
  (do ((val base (and proc (proc val (stream-car strm))))
       (strm strm (stream-cdr strm))
       (limit limit (and limit (1- limit))))
      ((or (and limit (zero? limit)) (stream-null? strm))
       (values val strm limit))))

(define stream->list
  (case-lambda
   ((strm) (stream->list #f strm))
   ((n strm)
    (must stream? strm 'stream->list "non-stream argument")
    (when n
      (must integer? n 'stream->list "non-integer count")
      (must exact? n 'stream->list "inexact count")
      (must-not negative? n 'stream->list "negative count"))
    (reverse! (first-value (stream-fold-aux xcons '() strm n))))))

(define (stream-append . strms)
  (must-every stream? strms 'stream-append "non-stream argument")
  (stream-let recur ((strms strms))
    (if (null? strms) stream-null
        (let ((strm (car strms)))
          (if (stream-null? strm) (recur (cdr strms))
              (stream-cons (stream-car strm)
                           (recur (cons (stream-cdr strm) (cdr strms)))))))))

(define (stream-concat strms)
  (must stream? strms 'stream-concat "non-stream argument")
  (stream-let recur ((strms strms))
    (if (stream-null? strms) stream-null
        (let ((strm (stream-car strms)))
          (must stream? strm 'stream-concat "non-stream object in input stream")
          (if (stream-null? strm) (recur (stream-cdr strms))
              (stream-cons (stream-car strm)
                           (recur (stream-cons (stream-cdr strm)
                                               (stream-cdr strms)))))))))

(define stream-constant
  (case-lambda
   (() stream-null)
   (objs (list->stream (apply circular-list objs)))))

(define-syntax* (stream-do x)
  (define (end x)
    (syntax-case x ()
      (() #'(if #f #f))
      ((result) #'result)
      ((result ...) #'(begin result ...))))
  (define (var-step v s)
    (syntax-case s ()
      (() v)
      ((e) #'e)
      (_ (syntax-violation 'stream-do "bad step expression" x s))))

  (syntax-case x ()
    ((_ ((var init . step) ...)
        (test result ...)
        expr ...)
     (with-syntax ((result (end #'(result ...)))
                   ((step ...) (map var-step #'(var ...) #'(step ...))))
       #'(stream-let loop ((var init) ...)
           (if test result
               (begin
                 expr ...
                 (loop step ...))))))))

(define (stream-drop n strm)
  (must integer? n 'stream-drop "non-integer argument")
  (must exact? n 'stream-drop "inexact argument")
  (must-not negative? n 'stream-drop "negative argument")
  (must stream? strm 'stream-drop "non-stream argument")
  (second-value (stream-fold-aux #f #f strm n)))

(define (stream-drop-while pred? strm)
  (must procedure? pred? 'stream-drop-while "non-procedural argument")
  (must stream? strm 'stream-drop-while "non-stream argument")
  (stream-do ((strm strm (stream-cdr strm)))
             ((or (stream-null? strm) (not (pred? (stream-car strm)))) strm)))

(define (stream-filter pred? strm)
  (must procedure? pred? 'stream-filter "non-procedural argument")
  (must stream? strm 'stream-filter "non-stream argument")
  (stream-let recur ((strm strm))
    (cond ((stream-null? strm) stream-null)
          ((pred? (stream-car strm))
           (stream-cons (stream-car strm) (recur (stream-cdr strm))))
          (else (recur (stream-cdr strm))))))

(define (stream-fold proc base strm)
  (must procedure? proc 'stream-fold "non-procedural argument")
  (must stream? strm 'stream-fold "non-stream argument")
  (first-value (stream-fold-aux proc base strm #f)))

(define stream-for-each
  (case-lambda
   ((proc strm)
    (must procedure? proc 'stream-for-each "non-procedural argument")
    (must stream? strm 'stream-for-each "non-stream argument")
    (do ((strm strm (stream-cdr strm)))
        ((stream-null? strm))
      (proc (stream-car strm))))
   ((proc strm . rest)
    (let ((strms (cons strm rest)))
      (must procedure? proc 'stream-for-each "non-procedural argument")
      (must-every stream? strms 'stream-for-each "non-stream argument")
      (do ((strms strms (map stream-cdr strms)))
          ((any stream-null? strms))
        (apply proc (map stream-car strms)))))))

(define* (stream-from first #:optional (step 1))
  (must number? first 'stream-from "non-numeric starting number")
  (must number? step 'stream-from "non-numeric step size")
  (stream-let recur ((first first))
    (stream-cons first (recur (+ first step)))))

(define (stream-iterate proc base)
  (must procedure? proc 'stream-iterate "non-procedural argument")
  (stream-let recur ((base base))
    (stream-cons base (recur (proc base)))))

(define (stream-length strm)
  (must stream? strm 'stream-length "non-stream argument")
  (- -1 (third-value (stream-fold-aux #f #f strm -1))))

(define stream-map
  (case-lambda
   ((proc strm)
    (must procedure? proc 'stream-map "non-procedural argument")
    (must stream? strm 'stream-map "non-stream argument")
    (stream-let recur ((strm strm))
      (if (stream-null? strm) stream-null
          (stream-cons (proc (stream-car strm))
                       (recur (stream-cdr strm))))))
   ((proc strm . rest)
    (let ((strms (cons strm rest)))
      (must procedure? proc 'stream-map "non-procedural argument")
      (must-every stream? strms 'stream-map "non-stream argument")
      (stream-let recur ((strms strms))
        (if (any stream-null? strms) stream-null
            (stream-cons (apply proc (map stream-car strms))
                         (recur (map stream-cdr strms)))))))))

(define-syntax* (stream-match x)
  (define (make-matcher x)
    (syntax-case x ()
      (() #'(? stream-null?))
      (rest (identifier? #'rest) #'rest)
      ((var . rest) (identifier? #'var)
       (with-syntax ((next (make-matcher #'rest)))
         #'(? (negate stream-null?)
              (= stream-car var)
              (= stream-cdr next))))))
  (define (make-guarded x fail)
    (syntax-case (list x fail) ()
      (((expr) _) #'expr)
      (((guard expr) fail) #'(if guard expr (fail)))))

  (syntax-case x ()
    ((_ strm-expr (pat . expr) ...)
     (with-syntax (((fail ...) (generate-temporaries #'(pat ...))))
       (with-syntax (((matcher ...) (map make-matcher #'(pat ...)))
                     ((expr ...) (map make-guarded #'(expr ...) #'(fail ...))))
         #'(let ((strm strm-expr))
             (must stream? strm 'stream-match "non-stream argument")
             (match strm (matcher (=> fail) expr) ...)))))))

(define-syntax-rule (stream-of expr rest ...)
  (stream-of-aux expr stream-null rest ...))

(define-syntax stream-of-aux
  (syntax-rules (in is)
    ((_ expr base)
     (stream-cons expr base))
    ((_ expr base (var in stream) rest ...)
     (stream-let recur ((strm stream))
       (if (stream-null? strm) base
           (let ((var (stream-car strm)))
             (stream-of-aux expr (recur (stream-cdr strm)) rest ...)))))
    ((_ expr base (var is exp) rest ...)
     (let ((var exp)) (stream-of-aux expr base rest ...)))
    ((_ expr base pred? rest ...)
     (if pred? (stream-of-aux expr base rest ...) base))))

(define* (stream-range first past #:optional step)
  (must number? first 'stream-range "non-numeric starting number")
  (must number? past 'stream-range "non-numeric ending number")
  (when step
    (must number? step 'stream-range "non-numeric step size"))
  (let* ((step (or step (if (< first past) 1 -1)))
         (lt? (if (< 0 step) < >)))
    (stream-let recur ((first first))
      (if (lt? first past)
          (stream-cons first (recur (+ first step)))
          stream-null))))

(define (stream-ref strm n)
  (must stream? strm 'stream-ref "non-stream argument")
  (must integer? n 'stream-ref "non-integer argument")
  (must exact? n 'stream-ref "inexact argument")
  (must-not negative? n 'stream-ref "negative argument")
  (let ((res (stream-drop n strm)))
    (must-not stream-null? res 'stream-ref "beyond end of stream")
    (stream-car res)))

(define (stream-reverse strm)
  (must stream? strm 'stream-reverse "non-stream argument")
  (stream-do ((strm strm (stream-cdr strm))
              (rev stream-null (stream-cons (stream-car strm) rev)))
             ((stream-null? strm) rev)))

(define (stream-scan proc base strm)
  (must procedure? proc 'stream-scan "non-procedural argument")
  (must stream? strm 'stream-scan "non-stream argument")
  (stream-let recur ((base base) (strm strm))
    (if (stream-null? strm) (stream base)
        (stream-cons base (recur (proc base (stream-car strm))
                                 (stream-cdr strm))))))

(define (stream-take n strm)
  (must stream? strm 'stream-take "non-stream argument")
  (must integer? n 'stream-take "non-integer argument")
  (must exact? n 'stream-take "inexact argument")
  (must-not negative? n 'stream-take "negative argument")
  (stream-let recur ((n n) (strm strm))
    (if (or (zero? n) (stream-null? strm)) stream-null
        (stream-cons (stream-car strm) (recur (1- n) (stream-cdr strm))))))

(define (stream-take-while pred? strm)
  (must procedure? pred? 'stream-take-while "non-procedural argument")
  (must stream? strm 'stream-take-while "non-stream argument")
  (stream-let recur ((strm strm))
    (cond ((stream-null? strm) stream-null)
          ((pred? (stream-car strm))
           (stream-cons (stream-car strm) (recur (stream-cdr strm))))
          (else stream-null))))

(define (stream-unfold mapper pred? generator base)
  (must procedure? mapper 'stream-unfold "non-procedural mapper")
  (must procedure? pred? 'stream-unfold "non-procedural pred?")
  (must procedure? generator 'stream-unfold "non-procedural generator")
  (stream-let recur ((base base))
    (if (pred? base)
        (stream-cons (mapper base) (recur (generator base)))
        stream-null)))

(define (stream-unfolds gen seed)
  (define-stream (generator-stream seed)
    (receive (next . items) (gen seed)
      (stream-cons (list->vector items) (generator-stream next))))
  (define-stream (make-result-stream genstrm index)
    (define head (vector-ref (stream-car genstrm) index))
    (define-stream (tail) (make-result-stream (stream-cdr genstrm) index))
    (match head
      (() stream-null)
      (#f (tail))
      ((item) (stream-cons item (tail)))
      ((? list? items) (stream-append (list->stream items) (tail)))))

  (must procedure? gen 'stream-unfolds "non-procedural argument")
  (let ((genstrm (generator-stream seed)))
    (apply values (list-tabulate (vector-length (stream-car genstrm))
                                 (cut make-result-stream genstrm <>)))))

(define (stream-zip strm . rest)
  (let ((strms (cons strm rest)))
    (must-every stream? strms 'stream-zip "non-stream argument")
    (stream-let recur ((strms strms))
      (if (any stream-null? strms) stream-null
          (stream-cons (map stream-car strms) (recur (map stream-cdr strms)))))))
