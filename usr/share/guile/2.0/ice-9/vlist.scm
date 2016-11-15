;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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

(define-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)

  #:export (vlist? vlist-cons vlist-head vlist-tail vlist-null?
            vlist-null list->vlist vlist-ref vlist-drop vlist-take
            vlist-length vlist-fold vlist-fold-right vlist-map
            vlist-unfold vlist-unfold-right vlist-append
            vlist-reverse vlist-filter vlist-delete vlist->list
            vlist-for-each
            block-growth-factor

            vhash? vhash-cons vhash-consq vhash-consv
            vhash-assoc vhash-assq vhash-assv
            vhash-delete vhash-delq vhash-delv
            vhash-fold vhash-fold-right
            vhash-fold* vhash-foldq* vhash-foldv*
            alist->vhash))

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;;
;;; Commentary:
;;;
;;; This module provides an implementations of vlists, a functional list-like
;;; data structure described by Phil Bagwell in "Fast Functional Lists,
;;; Hash-Lists, Dequeues and Variable-Length Arrays", EPFL Technical Report,
;;; 2002.
;;;
;;; The idea is to store vlist elements in increasingly large contiguous blocks
;;; (implemented as vectors here).  These blocks are linked to one another using
;;; a pointer to the next block (called `block-base' here) and an offset within
;;; that block (`block-offset' here).  The size of these blocks form a geometric
;;; series with ratio `block-growth-factor'.
;;;
;;; In the best case (e.g., using a vlist returned by `list->vlist'),
;;; elements from the first half of an N-element vlist are accessed in O(1)
;;; (assuming `block-growth-factor' is 2), and `vlist-length' takes only
;;; O(ln(N)).  Furthermore, the data structure improves data locality since
;;; vlist elements are adjacent, which plays well with caches.
;;;
;;; Code:


;;;
;;; VList Blocks and Block Descriptors.
;;;

(define block-growth-factor
  (make-fluid 2))

(define-inlinable (make-block base offset size hash-tab?)
  ;; Return a block (and block descriptor) of SIZE elements pointing to
  ;; BASE at OFFSET.  If HASH-TAB? is true, we also reserve space for a
  ;; "hash table".  Note: We use `next-free' instead of `last-used' as
  ;; suggested by Bagwell.
  (if hash-tab?
      (vector (make-vector (* size 3) #f)
              base offset size 0)
      (vector (make-vector size)
              base offset size 0)))

(define-syntax-rule (define-block-accessor name index)
  (define-inlinable (name block)
    (vector-ref block index)))

(define-block-accessor block-content 0)
(define-block-accessor block-base 1)
(define-block-accessor block-offset 2)
(define-block-accessor block-size 3)
(define-block-accessor block-next-free 4)

(define-inlinable (block-hash-table? block)
  (< (block-size block) (vector-length (block-content block))))

(define-inlinable (set-block-next-free! block next-free)
  (vector-set! block 4 next-free))

(define-inlinable (block-append! block value offset)
  ;; This is not thread-safe.  To fix it, see Section 2.8 of the paper.
  (and (< offset (block-size block))
       (= offset (block-next-free block))
       (begin
         (set-block-next-free! block (1+ offset))
         (vector-set! (block-content block) offset value)
         #t)))

;; Return the item at slot OFFSET.
(define-inlinable (block-ref content offset)
  (vector-ref content offset))

;; Return the offset of the next item in the hash bucket, after the one
;; at OFFSET.
(define-inlinable (block-hash-table-next-offset content size offset)
  (vector-ref content (+ size size offset)))

;; Save the offset of the next item in the hash bucket, after the one
;; at OFFSET.
(define-inlinable (block-hash-table-set-next-offset! content size offset
                                                     next-offset)
  (vector-set! content (+ size size offset) next-offset))

;; Returns the index of the last entry stored in CONTENT with
;; SIZE-modulo hash value KHASH.
(define-inlinable (block-hash-table-ref content size khash)
  (vector-ref content (+ size khash)))

(define-inlinable (block-hash-table-set! content size khash offset)
  (vector-set! content (+ size khash) offset))

;; Add hash table information for the item recently added at OFFSET,
;; with SIZE-modulo hash KHASH.
(define-inlinable (block-hash-table-add! content size khash offset)
  (block-hash-table-set-next-offset! content size offset
                                     (block-hash-table-ref content size khash))
  (block-hash-table-set! content size khash offset))

(define block-null
  ;; The null block.
  (make-block #f 0 0 #f))


;;;
;;; VLists.
;;;

(define-record-type <vlist>
  ;; A vlist is just a base+offset pair pointing to a block.

  ;; XXX: Allocating a <vlist> record in addition to the block at each
  ;; `vlist-cons' call is inefficient.  However, Bagwell's hack to avoid it
  ;; (Section 2.2) would require GC_ALL_INTERIOR_POINTERS, which would be a
  ;; performance hit for everyone.
  (make-vlist base offset)
  vlist?
  (base    vlist-base)
  (offset  vlist-offset))

(set-record-type-printer! <vlist>
                          (lambda (vl port)
                            (cond ((vlist-null? vl)
                                   (format port "#<vlist ()>"))
                                  ((vhash? vl)
                                   (format port "#<vhash ~x ~a pairs>"
                                           (object-address vl)
                                           (vlist-length vl)))
                                  (else
                                   (format port "#<vlist ~a>"
                                           (vlist->list vl))))))


(define vlist-null
  ;; The empty vlist.
  (make-vlist block-null 0))

;; Asserting that something is a vlist is actually a win if your next
;; step is to call record accessors, because that causes CSE to
;; eliminate the type checks in those accessors.
;;
(define-inlinable (assert-vlist val)
  (unless (vlist? val)
    (throw 'wrong-type-arg
           #f
           "Not a vlist: ~S"
           (list val)
           (list val))))

(define-inlinable (block-cons item vlist hash-tab?)
  (let ((base (vlist-base vlist))
        (offset (1+ (vlist-offset vlist))))
    (cond
     ((block-append! base item offset)
      ;; Fast path: We added the item directly to the block.
      (make-vlist base offset))
     (else
      ;; Slow path: Allocate a new block.
      (let* ((size (block-size base))
             (base (make-block
                    base
                    (1- offset)
                    (cond
                     ((zero? size) 1)
                     ((< offset size) 1) ;; new vlist head
                     (else (* (fluid-ref block-growth-factor) size)))
                    hash-tab?)))
        (set-block-next-free! base 1)
        (vector-set! (block-content base) 0 item)
        (make-vlist base 0))))))

(define (vlist-cons item vlist)
  "Return a new vlist with ITEM as its head and VLIST as its
tail."
  ;; Note: Although the result of `vlist-cons' on a vhash is a valid
  ;; vlist, it is not a valid vhash.  The new item does not get a hash
  ;; table entry.  If we allocate a new block, the new block will not
  ;; have a hash table.  Perhaps we can do something more sensible here,
  ;; but this is a hot function, so there are performance impacts.
  (assert-vlist vlist)
  (block-cons item vlist #f))

(define (vlist-head vlist)
  "Return the head of VLIST."
  (assert-vlist vlist)
  (let ((base   (vlist-base vlist))
        (offset (vlist-offset vlist)))
    (block-ref (block-content base) offset)))

(define (vlist-tail vlist)
  "Return the tail of VLIST."
  (assert-vlist vlist)
  (let ((base   (vlist-base vlist))
        (offset (vlist-offset vlist)))
    (if (> offset 0)
        (make-vlist base (- offset 1))
        (make-vlist (block-base base)
                    (block-offset base)))))

(define (vlist-null? vlist)
  "Return true if VLIST is empty."
  (assert-vlist vlist)
  (let ((base (vlist-base vlist)))
    (and (not (block-base base))
         (= 0 (block-size base)))))


;;;
;;; VList Utilities.
;;;

(define (list->vlist lst)
  "Return a new vlist whose contents correspond to LST."
  (vlist-reverse (fold vlist-cons vlist-null lst)))

(define (vlist-fold proc init vlist)
  "Fold over VLIST, calling PROC for each element."
  ;; FIXME: Handle multiple lists.
  (assert-vlist vlist)
  (let loop ((base   (vlist-base vlist))
             (offset (vlist-offset vlist))
             (result init))
    (if (eq? base block-null)
        result
        (let* ((next  (- offset 1))
               (done? (< next 0)))
          (loop (if done? (block-base base) base)
                (if done? (block-offset base) next)
                (proc (block-ref (block-content base) offset) result))))))

(define (vlist-fold-right proc init vlist)
  "Fold over VLIST, calling PROC for each element, starting from
the last element."
  (assert-vlist vlist)
  (let loop ((index  (1- (vlist-length vlist)))
             (result init))
    (if (< index 0)
        result
        (loop (1- index)
          (proc (vlist-ref vlist index) result)))))

(define (vlist-reverse vlist)
  "Return a new VLIST whose content are those of VLIST in reverse
order."
  (vlist-fold vlist-cons vlist-null vlist))

(define (vlist-map proc vlist)
  "Map PROC over the elements of VLIST and return a new vlist."
  (vlist-fold (lambda (item result)
                (vlist-cons (proc item) result))
              vlist-null
              (vlist-reverse vlist)))

(define (vlist->list vlist)
  "Return a new list whose contents match those of VLIST."
  (vlist-fold-right cons '() vlist))

(define (vlist-ref vlist index)
  "Return the element at index INDEX in VLIST."
  (assert-vlist vlist)
  (let loop ((index   index)
             (base    (vlist-base vlist))
             (offset  (vlist-offset vlist)))
    (if (<= index offset)
        (block-ref (block-content base) (- offset index))
        (loop (- index offset 1)
              (block-base base)
              (block-offset base)))))

(define (vlist-drop vlist count)
  "Return a new vlist that does not contain the COUNT first elements of
VLIST."
  (assert-vlist vlist)
  (let loop ((count  count)
             (base   (vlist-base vlist))
             (offset (vlist-offset vlist)))
    (if (<= count offset)
        (make-vlist base (- offset count))
        (loop (- count offset 1)
              (block-base base)
              (block-offset base)))))

(define (vlist-take vlist count)
  "Return a new vlist that contains only the COUNT first elements of
VLIST."
  (let loop ((count  count)
             (vlist  vlist)
             (result vlist-null))
    (if (= 0 count)
        (vlist-reverse result)
        (loop (- count 1)
              (vlist-tail vlist)
              (vlist-cons (vlist-head vlist) result)))))

(define (vlist-filter pred vlist)
  "Return a new vlist containing all the elements from VLIST that
satisfy PRED."
  (vlist-fold-right (lambda (e v)
                      (if (pred e)
                          (vlist-cons e v)
                          v))
                    vlist-null
                    vlist))

(define* (vlist-delete x vlist #:optional (equal? equal?))
  "Return a new vlist corresponding to VLIST without the elements
EQUAL? to X."
  (vlist-filter (lambda (e)
                  (not (equal? e x)))
                vlist))

(define (vlist-length vlist)
  "Return the length of VLIST."
  (assert-vlist vlist)
  (let loop ((base (vlist-base vlist))
             (len  (vlist-offset vlist)))
    (if (eq? base block-null)
        len
        (loop (block-base base)
              (+ len 1 (block-offset base))))))

(define* (vlist-unfold p f g seed
                       #:optional (tail-gen (lambda (x) vlist-null)))
  "Return a new vlist.  See the description of SRFI-1 `unfold' for details."
  (let uf ((seed seed))
    (if (p seed)
        (tail-gen seed)
        (vlist-cons (f seed)
                    (uf (g seed))))))

(define* (vlist-unfold-right p f g seed #:optional (tail vlist-null))
  "Return a new vlist.  See the description of SRFI-1 `unfold-right' for
details."
  (let uf ((seed seed) (lis tail))
    (if (p seed)
        lis
        (uf (g seed) (vlist-cons (f seed) lis)))))

(define (vlist-append . vlists)
  "Append the given lists."
  (if (null? vlists)
      vlist-null
      (fold-right (lambda (vlist result)
                    (vlist-fold-right (lambda (e v)
                                        (vlist-cons e v))
                                      result
                                      vlist))
                  vlist-null
                  vlists)))

(define (vlist-for-each proc vlist)
  "Call PROC on each element of VLIST.  The result is unspecified."
  (vlist-fold (lambda (item x)
                (proc item))
              (if #f #f)
              vlist))


;;;
;;; Hash Lists, aka. `VHash'.
;;;

;; Assume keys K1 and K2, H = hash(K1) = hash(K2), and two values V1 and V2
;; associated with K1 and K2, respectively.  The resulting layout is a
;; follows:
;;
;;             ,--------------------.
;;            0| ,-> (K1 . V1)      | Vlist array
;;            1| |                  |
;;            2| |   (K2 . V2)      |
;;            3| |                  |
;;        size +-|------------------+
;;            0| |                  | Hash table
;;            1| |                  |
;;            2| +-- O <------------- H
;;            3| |                  |
;;    size * 2 +-|------------------+
;;            0| `-> 2              | Chain links
;;            1|                    |
;;            2|    #f              |
;;            3|                    |
;;    size * 3 `--------------------'
;;
;; The backing store for the vhash is partitioned into three areas: the
;; vlist part, the hash table part, and the chain links part.  In this
;; example we have a hash H which, when indexed into the hash table
;; part, indicates that a value with this hash can be found at offset 0
;; in the vlist part.  The corresponding index (in this case, 0) of the
;; chain links array holds the index of the next element in this block
;; with this hash value, or #f if we reached the end of the chain.
;;
;; This API potentially requires users to repeat which hash function and
;; which equality predicate to use.  This can lead to unpredictable
;; results if they are used in consistenly, e.g., between `vhash-cons'
;; and `vhash-assoc', which is undesirable, as argued in
;; http://savannah.gnu.org/bugs/?22159 .  OTOH, two arguments can be
;; made in favor of this API:
;;
;;  - It's consistent with how alists are handled in SRFI-1.
;;
;;  - In practice, users will probably consistenly use either the `q',
;;    the `v', or the plain variant (`vlist-cons' and `vlist-assoc'
;;    without any optional argument), i.e., they will rarely explicitly
;;    pass a hash function or equality predicate.

(define (vhash? obj)
  "Return true if OBJ is a hash list."
  (and (vlist? obj)
       (block-hash-table? (vlist-base obj))))

(define* (vhash-cons key value vhash #:optional (hash hash))
  "Return a new hash list based on VHASH where KEY is associated
with VALUE.  Use HASH to compute KEY's hash."
  (assert-vlist vhash)
  ;; We should also assert that it is a hash table.  Need to check the
  ;; performance impacts of that.  Also, vlist-null is a valid hash
  ;; table, which does not pass vhash?.  A bug, perhaps.
  (let* ((vhash     (block-cons (cons key value) vhash #t))
         (base      (vlist-base vhash))
         (offset    (vlist-offset vhash))
         (size      (block-size base))
         (khash     (hash key size))
         (content   (block-content base)))
    (block-hash-table-add! content size khash offset)
    vhash))

(define vhash-consq (cut vhash-cons <> <> <> hashq))
(define vhash-consv (cut vhash-cons <> <> <> hashv))

(define-inlinable (%vhash-fold* proc init key vhash equal? hash)
  ;; Fold over all the values associated with KEY in VHASH.
  (define (visit-block base max-offset result)
    (let* ((size (block-size base))
           (content (block-content base))
           (khash (hash key size)))
      (let loop ((offset (block-hash-table-ref content size khash))
                 (result result))
        (if offset
            (loop (block-hash-table-next-offset content size offset)
                  (if (and (<= offset max-offset)
                           (equal? key (car (block-ref content offset))))
                      (proc (cdr (block-ref content offset)) result)
                      result))
            (let ((next-block (block-base base)))
              (if (> (block-size next-block) 0)
                  (visit-block next-block (block-offset base) result)
                  result))))))

  (assert-vlist vhash)
  (if (> (block-size (vlist-base vhash)) 0)
      (visit-block (vlist-base vhash)
                   (vlist-offset vhash)
                   init)
      init))

(define* (vhash-fold* proc init key vhash
                      #:optional (equal? equal?) (hash hash))
  "Fold over all the values associated with KEY in VHASH, with each
call to PROC having the form ‘(proc value result)’, where
RESULT is the result of the previous call to PROC and INIT the
value of RESULT for the first call to PROC."
  (%vhash-fold* proc init key vhash equal? hash))

(define (vhash-foldq* proc init key vhash)
  "Same as ‘vhash-fold*’, but using ‘hashq’ and ‘eq?’."
  (%vhash-fold* proc init key vhash eq? hashq))

(define (vhash-foldv* proc init key vhash)
  "Same as ‘vhash-fold*’, but using ‘hashv’ and ‘eqv?’."
  (%vhash-fold* proc init key vhash eqv? hashv))

(define-inlinable (%vhash-assoc key vhash equal? hash)
  ;; A specialization of `vhash-fold*' that stops when the first value
  ;; associated with KEY is found or when the end-of-list is reached.  Inline to
  ;; make sure `vhash-assq' gets to use the `eq?' instruction instead of calling
  ;; the `eq?'  subr.
  (define (visit-block base max-offset)
    (let* ((size (block-size base))
           (content (block-content base))
           (khash (hash key size)))
      (let loop ((offset (block-hash-table-ref content size khash)))
        (if offset
            (if (and (<= offset max-offset)
                     (equal? key (car (block-ref content offset))))
                (block-ref content offset)
                (loop (block-hash-table-next-offset content size offset)))
            (let ((next-block (block-base base)))
              (and (> (block-size next-block) 0)
                   (visit-block next-block (block-offset base))))))))

  (assert-vlist vhash)
  (and (> (block-size (vlist-base vhash)) 0)
       (visit-block (vlist-base vhash)
                    (vlist-offset vhash))))

(define* (vhash-assoc key vhash #:optional (equal? equal?) (hash hash))
  "Return the first key/value pair from VHASH whose key is equal to
KEY according to the EQUAL? equality predicate."
  (%vhash-assoc key vhash equal? hash))

(define (vhash-assq key vhash)
  "Return the first key/value pair from VHASH whose key is ‘eq?’ to
KEY."
  (%vhash-assoc key vhash eq? hashq))

(define (vhash-assv key vhash)
  "Return the first key/value pair from VHASH whose key is ‘eqv?’ to
KEY."
  (%vhash-assoc key vhash eqv? hashv))

(define* (vhash-delete key vhash #:optional (equal? equal?) (hash hash))
  "Remove all associations from VHASH with KEY, comparing keys
with EQUAL?."
  (if (vhash-assoc key vhash equal? hash)
      (vlist-fold (lambda (k+v result)
                    (let ((k (car k+v))
                          (v (cdr k+v)))
                      (if (equal? k key)
                          result
                          (vhash-cons k v result hash))))
                  vlist-null
                  vhash)
      vhash))

(define vhash-delq (cut vhash-delete <> <> eq? hashq))
(define vhash-delv (cut vhash-delete <> <> eqv? hashv))

(define (vhash-fold proc init vhash)
  "Fold over the key/pair elements of VHASH from left to right, with
each call to PROC having the form ‘(PROC key value result)’,
where RESULT is the result of the previous call to PROC and
INIT the value of RESULT for the first call to PROC."
  (vlist-fold (lambda (key+value result)
                (proc (car key+value) (cdr key+value)
                      result))
              init
              vhash))

(define (vhash-fold-right proc init vhash)
  "Fold over the key/pair elements of VHASH from right to left, with
each call to PROC having the form ‘(PROC key value result)’,
where RESULT is the result of the previous call to PROC and
INIT the value of RESULT for the first call to PROC."
  (vlist-fold-right (lambda (key+value result)
                      (proc (car key+value) (cdr key+value)
                            result))
                    init
                    vhash))

(define* (alist->vhash alist #:optional (hash hash))
  "Return the vhash corresponding to ALIST, an association list."
  (fold-right (lambda (pair result)
                (vhash-cons (car pair) (cdr pair) result hash))
              vlist-null
              alist))

;;; vlist.scm ends here
