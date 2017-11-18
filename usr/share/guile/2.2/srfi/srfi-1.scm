;;; srfi-1.scm --- List Library

;; 	Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2009, 2010, 2011, 2014 Free Software Foundation, Inc.
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

;;; Some parts from the reference implementation, which is
;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use.

;;; Author: Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
;;; Date: 2001-06-06

;;; Commentary:

;; This is an implementation of SRFI-1 (List Library).
;;
;; All procedures defined in SRFI-1, which are not already defined in
;; the Guile core library, are exported.  The procedures in this
;; implementation work, but they have not been tuned for speed or
;; memory usage.
;;
;; This module is fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-1)
  :export (
;;; Constructors
 ;; cons				<= in the core
 ;; list				<= in the core
 xcons
 ;; cons*				<= in the core
 ;; make-list				<= in the core
 list-tabulate
 list-copy
 circular-list
 ;; iota				; Extended.

;;; Predicates
 proper-list?
 circular-list?
 dotted-list?
 ;; pair?				<= in the core
 ;; null?				<= in the core
 null-list?
 not-pair?
 list=

;;; Selectors
 ;; car					<= in the core
 ;; cdr					<= in the core
 ;; caar				<= in the core
 ;; cadr				<= in the core
 ;; cdar				<= in the core
 ;; cddr				<= in the core
 ;; caaar				<= in the core
 ;; caadr				<= in the core
 ;; cadar				<= in the core
 ;; caddr				<= in the core
 ;; cdaar				<= in the core
 ;; cdadr				<= in the core
 ;; cddar				<= in the core
 ;; cdddr				<= in the core
 ;; caaaar				<= in the core
 ;; caaadr				<= in the core
 ;; caadar				<= in the core
 ;; caaddr				<= in the core
 ;; cadaar				<= in the core
 ;; cadadr				<= in the core
 ;; caddar				<= in the core
 ;; cadddr				<= in the core
 ;; cdaaar				<= in the core
 ;; cdaadr				<= in the core
 ;; cdadar				<= in the core
 ;; cdaddr				<= in the core
 ;; cddaar				<= in the core
 ;; cddadr				<= in the core
 ;; cdddar				<= in the core
 ;; cddddr				<= in the core
 ;; list-ref				<= in the core
 first
 second
 third
 fourth
 fifth
 sixth
 seventh
 eighth
 ninth
 tenth
 car+cdr
 take
 drop
 take-right
 drop-right
 take!
 drop-right!
 split-at
 split-at!
 last
 ;; last-pair				<= in the core

;;; Miscelleneous: length, append, concatenate, reverse, zip & count
 ;; length				<= in the core
 length+
 ;; append				<= in the core
 ;; append!				<= in the core
 concatenate
 concatenate!
 ;; reverse				<= in the core
 ;; reverse!				<= in the core
 append-reverse
 append-reverse!
 zip
 unzip1
 unzip2
 unzip3
 unzip4
 unzip5
 count

;;; Fold, unfold & map
 fold
 fold-right
 pair-fold
 pair-fold-right
 reduce
 reduce-right
 unfold
 unfold-right
 ;; map					; Extended.
 ;; for-each				; Extended.
 append-map
 append-map!
 map!
 ;; map-in-order			; Extended.
 pair-for-each
 filter-map

;;; Filtering & partitioning
 ;; filter				<= in the core
 partition
 remove
 ;; filter!				<= in the core
 partition!
 remove!

;;; Searching
 find
 find-tail
 take-while
 take-while!
 drop-while
 span
 span!
 break
 break!
 any
 every
 ;; list-index				; Extended.
 ;; member				; Extended.
 ;; memq				<= in the core
 ;; memv				<= in the core

;;; Deletion
 ;; delete				; Extended.
 ;; delete!				; Extended.
 delete-duplicates
 delete-duplicates!

;;; Association lists
 ;; assoc				; Extended.
 ;; assq				<= in the core
 ;; assv				<= in the core
 alist-cons
 alist-copy
 alist-delete
 alist-delete!

;;; Set operations on lists
 lset<=
 lset=
 lset-adjoin
 lset-union
 lset-intersection
 lset-difference
 lset-xor
 lset-diff+intersection
 lset-union!
 lset-intersection!
 lset-difference!
 lset-xor!
 lset-diff+intersection!

;;; Primitive side-effects
 ;; set-car!				<= in the core
 ;; set-cdr!				<= in the core
 )
  :re-export (cons list cons* make-list pair? null?
	      car cdr caar cadr cdar cddr
	      caaar caadr cadar caddr cdaar cdadr cddar cdddr
	      caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	      cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	      list-ref last-pair length append append! reverse reverse!
	      filter filter! memq memv assq assv set-car! set-cdr!)
  :replace (iota map for-each map-in-order list-copy list-index member
	    delete delete! assoc)
  )

(cond-expand-provide (current-module) '(srfi-1))

;; Load the compiled primitives from the shared library.
;;
(load-extension (string-append "libguile-" (effective-version))
                "scm_init_srfi_1")


;;; Constructors

(define (xcons d a)
  "Like `cons', but with interchanged arguments.  Useful mostly when passed to
higher-order procedures."
  (cons a d))

(define (wrong-type-arg caller arg)
  (scm-error 'wrong-type-arg (symbol->string caller)
             "Wrong type argument: ~S" (list arg) '()))

(define-syntax-rule (check-arg pred arg caller)
  (if (not (pred arg))
      (wrong-type-arg 'caller arg)))

(define (out-of-range proc arg)
  (scm-error 'out-of-range proc
             "Value out of range: ~A" (list arg) (list arg)))

;; the srfi spec doesn't seem to forbid inexact integers.
(define (non-negative-integer? x) (and (integer? x) (>= x 0)))

(define (list-tabulate n init-proc)
  "Return an N-element list, where each list element is produced by applying the
procedure INIT-PROC to the corresponding list index.  The order in which
INIT-PROC is applied to the indices is not specified."
  (check-arg non-negative-integer? n list-tabulate)
  (let lp ((n n) (acc '()))
    (if (<= n 0)
        acc
        (lp (- n 1) (cons (init-proc (- n 1)) acc)))))

(define (circular-list elt1 . elts)
  (set! elts (cons elt1 elts))
  (set-cdr! (last-pair elts) elts)
  elts)

(define* (iota count #:optional (start 0) (step 1))
  (check-arg non-negative-integer? count iota)
  (let lp ((n 0) (acc '()))
    (if (= n count)
	(reverse! acc)
	(lp (+ n 1) (cons (+ start (* n step)) acc)))))

;;; Predicates

(define (proper-list? x)
  (list? x))

(define (circular-list? x)
  (if (not-pair? x)
    #f
    (let lp ((hare (cdr x)) (tortoise x))
      (if (not-pair? hare)
	#f
	(let ((hare (cdr hare)))
	  (if (not-pair? hare)
	    #f
	    (if (eq? hare tortoise)
	      #t
	      (lp (cdr hare) (cdr tortoise)))))))))

(define (dotted-list? x)
  (cond
    ((null? x) #f)
    ((not-pair? x) #t)
    (else
     (let lp ((hare (cdr x)) (tortoise x))
       (cond
	 ((null? hare) #f)
	 ((not-pair? hare) #t)
	 (else
	  (let ((hare (cdr hare)))
	    (cond
	      ((null? hare) #f)
	      ((not-pair? hare) #t)
	      ((eq? hare tortoise) #f)
	      (else
	       (lp (cdr hare) (cdr tortoise)))))))))))

(define (null-list? x)
  (cond
    ((proper-list? x)
     (null? x))
    ((circular-list? x)
     #f)
    (else
     (error "not a proper list in null-list?"))))

(define (not-pair? x)
  "Return #t if X is not a pair, #f otherwise.

This is shorthand notation `(not (pair? X))' and is supposed to be used for
end-of-list checking in contexts where dotted lists are allowed."
  (not (pair? x)))

(define (list= elt= . rest)
  (define (lists-equal a b)
    (let lp ((a a) (b b))
      (cond ((null? a)
	     (null? b))
	    ((null? b)
	     #f)
	    (else
	     (and (elt= (car a) (car b))
		  (lp (cdr a) (cdr b)))))))

  (check-arg procedure? elt= list=)
  (or (null? rest)
      (let lp ((lists rest))
	(or (null? (cdr lists))
	    (and (lists-equal (car lists) (cadr lists))
		 (lp (cdr lists)))))))

;;; Selectors

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define (fifth x) (car (cddddr x)))
(define (sixth x) (cadr (cddddr x)))
(define (seventh x) (caddr (cddddr x)))
(define (eighth x) (cadddr (cddddr x)))
(define (ninth x) (car (cddddr (cddddr x))))
(define (tenth x) (cadr (cddddr (cddddr x))))

(define (car+cdr x)
  "Return two values, the `car' and the `cdr' of PAIR."
  (values (car x) (cdr x)))

(define take list-head)
(define drop list-tail)

;;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list, 
;;; off by K, then chasing down the list until the lead pointer falls off
;;; the end.  Note that they diverge for circular lists.

(define (take-right lis k)
  (let lp ((lag lis)  (lead (drop lis k)))
    (if (pair? lead)
	(lp (cdr lag) (cdr lead))
	lag)))

(define (drop-right lis k)
  (let recur ((lag lis) (lead (drop lis k)))
    (if (pair? lead)
	(cons (car lag) (recur (cdr lag) (cdr lead)))
	'())))

(define (take! lst i)
  "Linear-update variant of `take'."
  (if (= i 0)
      '()
      (let ((tail (drop lst (- i 1))))
        (set-cdr! tail '())
        lst)))

(define (drop-right! lst i)
  "Linear-update variant of `drop-right'."
  (let ((tail (drop lst i)))
    (if (null? tail)
        '()
        (let loop ((prev lst)
                   (tail (cdr tail)))
          (if (null? tail)
              (if (pair? prev)
                  (begin
                    (set-cdr! prev '())
                    lst)
                  lst)
              (loop (cdr prev)
                    (cdr tail)))))))

(define (split-at lst i)
  "Return two values, a list of the elements before index I in LST, and
a list of those after."
  (if (< i 0)
      (out-of-range 'split-at i)
      (let lp ((l lst) (n i) (acc '()))
        (if (<= n 0)
            (values (reverse! acc) l)
            (lp (cdr l) (- n 1) (cons (car l) acc))))))

(define (split-at! lst i)
  "Linear-update variant of `split-at'."
  (cond ((< i 0)
         (out-of-range 'split-at! i))
        ((= i 0)
         (values '() lst))
        (else
         (let lp ((l lst) (n (- i 1)))
           (if (<= n 0)
               (let ((tmp (cdr l)))
                 (set-cdr! l '())
                 (values lst tmp))
               (lp (cdr l) (- n 1)))))))

(define (last pair)
  "Return the last element of the non-empty, finite list PAIR."
  (car (last-pair pair)))

;;; Miscelleneous: length, append, concatenate, reverse, zip & count

(define (zip clist1 . rest)
  (let lp ((l (cons clist1 rest)) (acc '()))
    (if (any null? l)
      (reverse! acc)
      (lp (map cdr l) (cons (map car l) acc)))))


(define (unzip1 l)
  (map first l))
(define (unzip2 l)
  (values (map first l) (map second l)))
(define (unzip3 l)
  (values (map first l) (map second l) (map third l)))
(define (unzip4 l)
  (values (map first l) (map second l) (map third l) (map fourth l)))
(define (unzip5 l)
  (values (map first l) (map second l) (map third l) (map fourth l)
	  (map fifth l)))

;;; Fold, unfold & map

(define fold
  (case-lambda
    "Apply PROC to the elements of LIST1 ... LISTN to build a result, and return
that result.  See the manual for details."
    ((kons knil list1)
     (check-arg procedure? kons fold)
     (check-arg list? list1 fold)
     (let fold1 ((knil knil) (list1 list1))
       (if (pair? list1)
           (fold1 (kons (car list1) knil) (cdr list1))
           knil)))
    ((kons knil list1 list2)
     (check-arg procedure? kons fold)
     (let* ((len1 (length+ list1))
            (len2 (length+ list2))
            (len (if (and len1 len2)
                     (min len1 len2)
                     (or len1 len2))))
       (unless len
         (scm-error 'wrong-type-arg "fold"
                    "Args do not contain a proper (finite) list: ~S"
                    (list (list list1 list2)) #f))
       (let fold2 ((knil knil) (list1 list1) (list2 list2) (len len))
         (if (zero? len)
             knil
             (fold2 (kons (car list1) (car list2) knil)
                    (cdr list1) (cdr list2) (1- len))))))
    ((kons knil list1 . rest)
     (check-arg procedure? kons fold)
     (let foldn ((knil knil) (lists (cons list1 rest)))
       (if (any null? lists)
           knil
           (let ((cars (map car lists))
                 (cdrs (map cdr lists)))
             (foldn (apply kons (append! cars (list knil))) cdrs)))))))

(define (fold-right kons knil clist1 . rest)
  (check-arg procedure? kons fold-right)
  (if (null? rest)
      (let loop ((lst    (reverse clist1))
                 (result knil))
        (if (null? lst)
            result
            (loop (cdr lst)
                  (kons (car lst) result))))
      (let loop ((lists  (map reverse (cons clist1 rest)))
                 (result knil))
        (if (any1 null? lists)
            result
            (loop (map cdr lists)
                  (apply kons (append! (map car lists) (list result))))))))

(define (pair-fold kons knil clist1 . rest)
  (check-arg procedure? kons pair-fold)
  (if (null? rest)
      (let f ((knil knil) (list1 clist1))
	(if (null? list1)
	    knil
	    (let ((tail (cdr list1)))
	    (f (kons list1 knil) tail))))
      (let f ((knil knil) (lists (cons clist1 rest)))
	(if (any null? lists)
	    knil
	    (let ((tails (map cdr lists)))
	      (f (apply kons (append! lists (list knil))) tails))))))


(define (pair-fold-right kons knil clist1 . rest)
  (check-arg procedure? kons pair-fold-right)
  (if (null? rest)
    (let f ((list1 clist1))
      (if (null? list1)
	knil
	(kons list1 (f (cdr list1)))))
    (let f ((lists (cons clist1 rest)))
      (if (any null? lists)
	knil
	(apply kons (append! lists (list (f (map cdr lists)))))))))

(define* (unfold p f g seed #:optional (tail-gen (lambda (x) '())))
  (define (reverse+tail lst seed)
    (let loop ((lst    lst)
               (result (tail-gen seed)))
      (if (null? lst)
          result
          (loop (cdr lst)
                (cons (car lst) result)))))

  (check-arg procedure? p unfold)
  (check-arg procedure? f unfold)
  (check-arg procedure? g unfold)
  (check-arg procedure? tail-gen unfold)
  (let loop ((seed   seed)
             (result '()))
    (if (p seed)
        (reverse+tail result seed)
        (loop (g seed)
              (cons (f seed) result)))))

(define* (unfold-right p f g seed #:optional (tail '()))
  (check-arg procedure? p unfold-right)
  (check-arg procedure? f unfold-right)
  (check-arg procedure? g unfold-right)
  (let uf ((seed seed) (lis tail))
    (if (p seed)
        lis
        (uf (g seed) (cons (f seed) lis)))))

(define (reduce f ridentity lst)
  "`reduce' is a variant of `fold', where the first call to F is on two
elements from LST, rather than one element and a given initial value.
If LST is empty, RIDENTITY is returned.  If LST has just one element
then that's the return value."
  (check-arg procedure? f reduce)
  (if (null? lst)
      ridentity
      (fold f (car lst) (cdr lst))))

(define (reduce-right f ridentity lst)
  "`reduce-right' is a variant of `fold-right', where the first call to
F is on two elements from LST, rather than one element and a given
initial value.  If LST is empty, RIDENTITY is returned.  If LST
has just one element then that's the return value."
  (check-arg procedure? f reduce)
  (if (null? lst)
      ridentity
      (fold-right f (last lst) (drop-right lst 1))))

(define map
  (case-lambda
    ((f l)
     (check-arg procedure? f map)
     (check-arg list? l map)
     (let map1 ((l l))
       (if (pair? l)
           (cons (f (car l)) (map1 (cdr l)))
           '())))
    
    ((f l1 l2)
     (check-arg procedure? f map)
     (let* ((len1 (length+ l1))
            (len2 (length+ l2))
            (len (if (and len1 len2)
                     (min len1 len2)
                     (or len1 len2))))
       (unless len
         (scm-error 'wrong-type-arg "map"
                    "Args do not contain a proper (finite) list: ~S"
                    (list (list l1 l2)) #f))
       (let map2 ((l1 l1) (l2 l2) (len len))
         (if (zero? len)
             '()
             (cons (f (car l1) (car l2))
                   (map2 (cdr l1) (cdr l2) (1- len)))))))

    ((f l1 . rest)
     (check-arg procedure? f map)
     (let ((len (fold (lambda (ls len)
                        (let ((ls-len (length+ ls)))
                          (if len
                              (if ls-len (min ls-len len) len)
                              ls-len)))
                      (length+ l1)
                      rest)))
       (if (not len)
           (scm-error 'wrong-type-arg "map"
                      "Args do not contain a proper (finite) list: ~S"
                      (list (cons l1 rest)) #f))
       (let mapn ((l1 l1) (rest rest) (len len))
         (if (zero? len)
             '()
             (cons (apply f (car l1) (map car rest))
                   (mapn (cdr l1) (map cdr rest) (1- len)))))))))

(define map-in-order map)

(define for-each
  (case-lambda
    ((f l)
     (check-arg procedure? f for-each)
     (check-arg list? l for-each)
     (let for-each1 ((l l))
       (unless (null? l)
         (f (car l))
         (for-each1 (cdr l)))))

    ((f l1 l2)
     (check-arg procedure? f for-each)
     (let* ((len1 (length+ l1))
            (len2 (length+ l2))
            (len (if (and len1 len2)
                     (min len1 len2)
                     (or len1 len2))))
       (unless len
         (scm-error 'wrong-type-arg "for-each"
                    "Args do not contain a proper (finite) list: ~S"
                    (list (list l1 l2)) #f))
       (let for-each2 ((l1 l1) (l2 l2) (len len))
         (unless (zero? len)
           (f (car l1) (car l2))
           (for-each2 (cdr l1) (cdr l2) (1- len))))))

    ((f l1 . rest)
     (check-arg procedure? f for-each)
     (let ((len (fold (lambda (ls len)
                        (let ((ls-len (length+ ls)))
                          (if len
                              (if ls-len (min ls-len len) len)
                              ls-len)))
                      (length+ l1)
                      rest)))
       (if (not len)
           (scm-error 'wrong-type-arg "for-each"
                      "Args do not contain a proper (finite) list: ~S"
                      (list (cons l1 rest)) #f))
       (let for-eachn ((l1 l1) (rest rest) (len len))
         (if (> len 0)
             (begin
               (apply f (car l1) (map car rest))
               (for-eachn (cdr l1) (map cdr rest) (1- len)))))))))

(define (append-map f clist1 . rest)
  (concatenate (apply map f clist1 rest)))
  
(define (append-map! f clist1 . rest)
  (concatenate! (apply map f clist1 rest)))

;; OPTIMIZE-ME: Re-use cons cells of list1
(define map! map)

(define (filter-map proc list1 . rest)
  "Apply PROC to the elements of LIST1... and return a list of the
results as per SRFI-1 `map', except that any #f results are omitted from
the list returned."
  (check-arg procedure? proc filter-map)
  (if (null? rest)
      (let lp ((l list1)
               (rl '()))
        (if (null? l)
            (reverse! rl)
            (let ((res (proc (car l))))
              (if res
                  (lp (cdr l) (cons res rl))
                  (lp (cdr l) rl)))))
      (let lp ((l (cons list1 rest))
               (rl '()))
        (if (any1 null? l)
            (reverse! rl)
            (let ((res (apply proc (map car l))))
              (if res
                  (lp (map cdr l) (cons res rl))
                  (lp (map cdr l) rl)))))))

(define (pair-for-each f clist1 . rest)
  (check-arg procedure? f pair-for-each)
  (if (null? rest)
    (let lp ((l clist1))
      (if (null? l)
	(if #f #f)
	(begin
	  (f l)
	  (lp (cdr l)))))
    (let lp ((l (cons clist1 rest)))
      (if (any1 null? l)
	(if #f #f)
	(begin
	  (apply f l)
	  (lp (map cdr l)))))))


;;; Searching

(define (take-while pred ls)
  "Return a new list which is the longest initial prefix of LS whose
elements all satisfy the predicate PRED."
  (check-arg procedure? pred take-while)
  (cond ((null? ls) '())
        ((not (pred (car ls))) '())
        (else
         (let ((result (list (car ls))))
           (let lp ((ls (cdr ls)) (p result))
             (cond ((null? ls) result)
                   ((not (pred (car ls))) result)
                   (else
                    (set-cdr! p (list (car ls)))
                    (lp (cdr ls) (cdr p)))))))))

(define (take-while! pred lst)
  "Linear-update variant of `take-while'."
  (check-arg procedure? pred take-while!)
  (let loop ((prev #f)
             (rest lst))
    (cond ((null? rest)
           lst)
          ((pred (car rest))
           (loop rest (cdr rest)))
          (else
           (if (pair? prev)
               (begin
                 (set-cdr! prev '())
                 lst)
               '())))))

(define (drop-while pred lst)
  "Drop the longest initial prefix of LST whose elements all satisfy the
predicate PRED."
  (check-arg procedure? pred drop-while)
  (let loop ((lst lst))
    (cond ((null? lst)
           '())
          ((pred (car lst))
           (loop (cdr lst)))
          (else lst))))

(define (span pred lst)
  "Return two values, the longest initial prefix of LST whose elements
all satisfy the predicate PRED, and the remainder of LST."
  (check-arg procedure? pred span)
  (let lp ((lst lst) (rl '()))
    (if (and (not (null? lst))
             (pred (car lst)))
        (lp (cdr lst) (cons (car lst) rl))
        (values (reverse! rl) lst))))

(define (span! pred list)
  "Linear-update variant of `span'."
  (check-arg procedure? pred span!)
  (let loop ((prev #f)
             (rest list))
    (cond ((null? rest)
           (values list '()))
          ((pred (car rest))
           (loop rest (cdr rest)))
          (else
           (if (pair? prev)
               (begin
                 (set-cdr! prev '())
                 (values list rest))
               (values '() list))))))

(define (break pred clist)
  "Return two values, the longest initial prefix of LST whose elements
all fail the predicate PRED, and the remainder of LST."
  (check-arg procedure? pred break)
  (let lp ((clist clist) (rl '()))
    (if (or (null? clist)
	    (pred (car clist)))
	(values (reverse! rl) clist)
	(lp (cdr clist) (cons (car clist) rl)))))

(define (break! pred list)
  "Linear-update variant of `break'."
  (check-arg procedure? pred break!)
  (let loop ((l    list)
             (prev #f))
    (cond ((null? l)
           (values list '()))
          ((pred (car l))
           (if (pair? prev)
               (begin
                 (set-cdr! prev '())
                 (values list l))
               (values '() list)))
          (else
           (loop (cdr l) l)))))

(define (any pred ls . lists)
  (check-arg procedure? pred any)
  (if (null? lists)
      (any1 pred ls)
      (let lp ((lists (cons ls lists)))
	(cond ((any1 null? lists)
	       #f)
	      ((any1 null? (map cdr lists))
	       (apply pred (map car lists)))
	      (else
	       (or (apply pred (map car lists)) (lp (map cdr lists))))))))

(define (any1 pred ls)
  (let lp ((ls ls))
    (cond ((null? ls)
	   #f)
	  ((null? (cdr ls))
	   (pred (car ls)))
	  (else
	   (or (pred (car ls)) (lp (cdr ls)))))))

(define (every pred ls . lists)
  (check-arg procedure? pred every)
  (if (null? lists)
      (every1 pred ls)
      (let lp ((lists (cons ls lists)))
	(cond ((any1 null? lists)
	       #t)
	      ((any1 null? (map cdr lists))
	       (apply pred (map car lists)))
	      (else
	       (and (apply pred (map car lists)) (lp (map cdr lists))))))))

(define (every1 pred ls)
  (let lp ((ls ls))
    (cond ((null? ls)
	   #t)
	  ((null? (cdr ls))
	   (pred (car ls)))
	  (else
	   (and (pred (car ls)) (lp (cdr ls)))))))

(define (list-index pred clist1 . rest)
  "Return the index of the first set of elements, one from each of
CLIST1 ... CLISTN, that satisfies PRED."
  (check-arg procedure? pred list-index)
  (if (null? rest)
    (let lp ((l clist1) (i 0))
      (if (null? l)
	#f
	(if (pred (car l))
	  i
	  (lp (cdr l) (+ i 1)))))
    (let lp ((lists (cons clist1 rest)) (i 0))
      (cond ((any1 null? lists)
	     #f)
	    ((apply pred (map car lists)) i)
	    (else
	     (lp (map cdr lists) (+ i 1)))))))

;;; Association lists

(define alist-cons acons)

(define (alist-copy alist)
  "Return a copy of ALIST, copying both the pairs comprising the list
and those making the associations."
  (let lp ((a  alist)
           (rl '()))
    (if (null? a)
        (reverse! rl)
        (lp (cdr a) (alist-cons (caar a) (cdar a) rl)))))

(define* (alist-delete key alist #:optional (k= equal?))
  (check-arg procedure? k= alist-delete)
  (let lp ((a alist) (rl '()))
    (if (null? a)
	(reverse! rl)
	(if (k= key (caar a))
            (lp (cdr a) rl)
            (lp (cdr a) (cons (car a) rl))))))

(define* (alist-delete! key alist #:optional (k= equal?))
  (alist-delete key alist k=))	; XXX:optimize

;;; Delete / assoc / member

(define* (member x ls #:optional (= equal?))
  (cond
   ;; This might be performance-sensitive, so punt on the check here,
   ;; relying on memq/memv to check that = is a procedure.
   ((eq? = eq?) (memq x ls))
   ((eq? = eqv?) (memv x ls))
   (else 
    (check-arg procedure? = member)
    (find-tail (lambda (y) (= x y)) ls))))

;;; Set operations on lists

(define (lset<= = . rest)
  (check-arg procedure? = lset<=)
  (if (null? rest)
      #t
      (let lp ((f (car rest)) (r (cdr rest)))
        (or (null? r)
            (and (every (lambda (el) (member el (car r) =)) f)
                 (lp (car r) (cdr r)))))))

(define (lset= = . rest)
  (check-arg procedure? = lset<=)
  (if (null? rest)
    #t
    (let lp ((f (car rest)) (r (cdr rest)))
      (or (null? r)
	  (and (every (lambda (el) (member el (car r) =)) f)
	       (every (lambda (el) (member el f (lambda (x y) (= y x)))) (car r))
	       (lp (car r) (cdr r)))))))

;; It's not quite clear if duplicates among the `rest' elements are meant to
;; be cast out.  The spec says `=' is called as (= lstelem restelem),
;; suggesting perhaps not, but the reference implementation shows the "list"
;; at each stage as including those elements already added.  The latter
;; corresponds to what's described for lset-union, so that's what's done.
;;
(define (lset-adjoin = list . rest)
  "Add to LIST any of the elements of REST not already in the list.
These elements are `cons'ed onto the start of LIST (so the return shares
a common tail with LIST), but the order they're added is unspecified.

The given `=' procedure is used for comparing elements, called
as `(@var{=} listelem elem)', i.e., the second argument is one of the
given REST parameters."
  ;; If `=' is `eq?' or `eqv?', users won't be able to tell which arg is
  ;; first, so we can pass the raw procedure through to `member',
  ;; allowing `memq' / `memv' to be selected.
  (define pred
    (if (or (eq? = eq?) (eq? = eqv?))
        =
        (begin
          (check-arg procedure? = lset-adjoin)
          (lambda (x y) (= y x)))))
  
  (let lp ((ans list) (rest rest))
    (if (null? rest)
        ans
        (lp (if (member (car rest) ans pred)
                ans
                (cons (car rest) ans))
            (cdr rest)))))

(define (lset-union = . rest)
  ;; Likewise, allow memq / memv to be used if possible.
  (define pred
    (if (or (eq? = eq?) (eq? = eqv?))
        =
        (begin
          (check-arg procedure? = lset-union)
          (lambda (x y) (= y x)))))
  
  (fold (lambda (lis ans)		; Compute ANS + LIS.
          (cond ((null? lis) ans)	; Don't copy any lists
                ((null? ans) lis) 	; if we don't have to.
                ((eq? lis ans) ans)
                (else
                 (fold (lambda (elt ans)
                         (if (member elt ans pred)
                             ans
                             (cons elt ans)))
                       ans lis))))
        '()
        rest))

(define (lset-intersection = list1 . rest)
  (check-arg procedure? = lset-intersection)
  (let lp ((l list1) (acc '()))
    (if (null? l)
      (reverse! acc)
      (if (every (lambda (ll) (member (car l) ll =)) rest)
	(lp (cdr l) (cons (car l) acc))
	(lp (cdr l) acc)))))

(define (lset-difference = list1 . rest)
  (check-arg procedure? = lset-difference)
  (if (null? rest)
    list1
    (let lp ((l list1) (acc '()))
      (if (null? l)
	(reverse! acc)
	(if (any (lambda (ll) (member (car l) ll =)) rest)
	  (lp (cdr l) acc)
	  (lp (cdr l) (cons (car l) acc)))))))

;(define (fold kons knil list1 . rest)

(define (lset-xor = . rest)
  (check-arg procedure? = lset-xor)
  (fold (lambda (lst res)
	  (let lp ((l lst) (acc '()))
	    (if (null? l)
	      (let lp0 ((r res) (acc acc))
		(if (null? r)
		  (reverse! acc)
		  (if (member (car r) lst =)
		    (lp0 (cdr r) acc)
		    (lp0 (cdr r) (cons (car r) acc)))))
	      (if (member (car l) res =)
		(lp (cdr l) acc)
		(lp (cdr l) (cons (car l) acc))))))
	'()
	rest))

(define (lset-diff+intersection = list1 . rest)
  (check-arg procedure? = lset-diff+intersection)
  (let lp ((l list1) (accd '()) (acci '()))
    (if (null? l)
      (values (reverse! accd) (reverse! acci))
      (let ((appears (every (lambda (ll) (member (car l) ll =)) rest)))
	(if appears
	  (lp (cdr l) accd (cons (car l) acci))
	  (lp (cdr l) (cons (car l) accd) acci))))))


(define (lset-union! = . rest)
  (check-arg procedure? = lset-union!)
  (apply lset-union = rest))		; XXX:optimize

(define (lset-intersection! = list1 . rest)
  (check-arg procedure? = lset-intersection!)
  (apply lset-intersection = list1 rest)) ; XXX:optimize

(define (lset-xor! = . rest)
  (check-arg procedure? = lset-xor!)
  (apply lset-xor = rest))		; XXX:optimize

(define (lset-diff+intersection! = list1 . rest)
  (check-arg procedure? = lset-diff+intersection!)
  (apply lset-diff+intersection = list1 rest)) ; XXX:optimize

;;; srfi-1.scm ends here
