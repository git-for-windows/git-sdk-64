;; Copyright (C) 2010 Free Software Foundation, Inc.
;; Copyright (C) Ray Dillinger 2003. All Rights Reserved.
;;
;; Contains code based upon Alex Shinn's public-domain implementation of
;; `read-with-shared-structure' found in Chicken's SRFI 38 egg.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

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

(define-module (srfi srfi-38)
  #:export (write-with-shared-structure
            read-with-shared-structure)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-69)
  #:use-module (system vm trap-state))

(cond-expand-provide (current-module) '(srfi-38))

;; A printer that shows all sharing of substructures.  Uses the Common
;; Lisp print-circle notation: #n# refers to a previous substructure
;; labeled with #n=.   Takes O(n^2) time.

;; Code attributed to Al Petrofsky, modified by Ray Dillinger.

;; Modified in 2010 by Andreas Rottmann to use SRFI 69 hashtables,
;; making the time O(n), and adding some of Guile's data types to the
;; `interesting' objects.

(define* (write-with-shared-structure obj
                                      #:optional
                                      (outport (current-output-port))
                                      (optarg #f))

  ;; We only track duplicates of pairs, vectors, strings, bytevectors,
  ;; structs (which subsume R6RS and SRFI-9 records), ports and (native)
  ;; hash-tables.  We ignore zero-length vectors and strings because
  ;; r5rs doesn't guarantee that eq? treats them sanely (and they aren't
  ;; very interesting anyway).

  (define (interesting? obj)
    (or (pair? obj)
        (and (vector? obj) (not (zero? (vector-length obj))))
        (and (string? obj) (not (zero? (string-length obj))))
        (bytevector? obj)
        (struct? obj)
        (port? obj)
        (hash-table? obj)))
  
  ;; (write-obj OBJ STATE):
  ;;
  ;; STATE is a hashtable which has an entry for each interesting part
  ;; of OBJ.  The associated value will be:
  ;;
  ;;  -- a number if the part has been given one,
  ;;  -- #t if the part will need to be assigned a number but has not been yet,
  ;;  -- #f if the part will not need a number.
  ;; The entry `counter' in STATE should be the most recently
  ;; assigned number.
  ;;
  ;; Mutates STATE for any parts that had numbers assigned.
  (define (write-obj obj state)
    (define (write-interesting)
      (cond ((pair? obj)
             (display "(" outport)
             (write-obj (car obj) state)
             (let write-cdr ((obj (cdr obj)))
               (cond ((and (pair? obj) (not (hash-table-ref state obj)))
                      (display " " outport)
                      (write-obj (car obj) state)
                      (write-cdr (cdr obj)))
                     ((null? obj)
                      (display ")" outport))
                     (else
                      (display " . " outport)
                      (write-obj obj state)
                      (display ")" outport)))))
            ((vector? obj)
             (display "#(" outport)
             (let ((len (vector-length obj)))
               (write-obj (vector-ref obj 0) state)
               (let write-vec ((i 1))
                 (cond ((= i len) (display ")" outport))
                       (else (display " " outport)
                             (write-obj (vector-ref obj i) state)
                             (write-vec (+ i 1)))))))
            ;; else it's a string
            (else (write obj outport))))
    (cond ((interesting? obj)
           (let ((val (hash-table-ref state obj)))
             (cond ((not val) (write-interesting))
                   ((number? val) 
                    (begin (display "#" outport)
                           (write val outport)
                           (display "#" outport)))
                   (else
                    (let ((n (+ 1 (hash-table-ref state 'counter))))
                      (display "#" outport)
                      (write n outport)
                      (display "=" outport)
                      (hash-table-set! state 'counter n)
                      (hash-table-set! state obj n)
                      (write-interesting))))))
          (else
           (write obj outport))))

  ;; Scan computes the initial value of the hash table, which maps each
  ;; interesting part of the object to #t if it occurs multiple times,
  ;; #f if only once.
  (define (scan obj state)
    (cond ((not (interesting? obj)))
          ((hash-table-exists? state obj)
           (hash-table-set! state obj #t))
          (else
           (hash-table-set! state obj #f)
           (cond ((pair? obj)
                  (scan (car obj) state)
                  (scan (cdr obj) state))
                 ((vector? obj)
                  (let ((len (vector-length obj)))
                    (do ((i 0 (+ 1 i)))
                        ((= i len))
                      (scan (vector-ref obj i) state))))))))

  (let ((state (make-hash-table eq?)))
    (scan obj state)
    (hash-table-set! state 'counter 0)
    (write-obj obj state)))

;; A reader that understands the output of the above writer.  This has
;; been written by Andreas Rottmann to re-use Guile's built-in reader,
;; with inspiration from Alex Shinn's public-domain implementation of
;; `read-with-shared-structure' found in Chicken's SRFI 38 egg.

(define* (read-with-shared-structure #:optional (port (current-input-port)))
  (let ((parts-table (make-hash-table eqv?)))
    
    ;; reads chars that match PRED and returns them as a string.
    (define (read-some-chars pred initial)
      (let iter ((chars initial))
        (let ((c (peek-char port)))
          (if (or (eof-object? c) (not (pred c)))
              (list->string (reverse chars))
              (iter (cons (read-char port) chars))))))

    (define (read-hash c port)
      (let* ((n (string->number (read-some-chars char-numeric? (list c))))
             (c (read-char port))
             (thunk (hash-table-ref/default parts-table n #f)))
        (case c
          ((#\=)
           (if thunk
               (error "Double declaration of part " n))
           (let* ((cell (list #f))
                  (thunk (lambda () (car cell))))
             (hash-table-set! parts-table n thunk)
             (let ((obj (read port)))
               (set-car! cell obj)
               obj)))
          ((#\#)
           (or thunk
               (error "Use of undeclared part " n)))
          (else
           (error "Malformed shared part specifier")))))

    (with-fluid* %read-hash-procedures (fluid-ref %read-hash-procedures)
      (lambda ()
        (for-each (lambda (digit)
                    (read-hash-extend digit read-hash))
                  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
        (let ((result (read port)))
          (if (< 0 (hash-table-size parts-table))
              (patch! result))
          result)))))

(define (hole? x) (procedure? x))
(define (fill-hole x) (if (hole? x) (fill-hole (x)) x))

(define (patch! x)
  (cond
   ((pair? x)
    (if (hole? (car x)) (set-car! x (fill-hole (car x))) (patch! (car x)))
    (if (hole? (cdr x)) (set-cdr! x (fill-hole (cdr x))) (patch! (cdr x))))
   ((vector? x)
    (do ((i (- (vector-length x) 1) (- i 1)))
        ((< i 0))
      (let ((elt (vector-ref x i)))
        (if (hole? elt)
            (vector-set! x i (fill-hole elt))
            (patch! elt)))))))
