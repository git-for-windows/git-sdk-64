;;;; hash-table.scm --- Additional hash table procedures
;;;; Copyright (C) 2013 Free Software Foundation, Inc.
;;;;
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
;;;;

(define-module (ice-9 hash-table)
  #:export (alist->hash-table
            alist->hashq-table
            alist->hashv-table
            alist->hashx-table))

(define-syntax-rule (define-alist-converter name hash-set-proc)
  (define (name alist)
    "Convert ALIST into a hash table."
    (let ((table (make-hash-table)))
      (for-each (lambda (pair)
                  (hash-set-proc table (car pair) (cdr pair)))
                (reverse alist))
      table)))

(define-alist-converter alist->hash-table hash-set!)
(define-alist-converter alist->hashq-table hashq-set!)
(define-alist-converter alist->hashv-table hashv-set!)

(define (alist->hashx-table hash assoc alist)
  "Convert ALIST into a hash table with custom HASH and ASSOC
procedures."
  (let ((table (make-hash-table)))
    (for-each (lambda (pair)
                (hashx-set! hash assoc table (car pair) (cdr pair)))
              (reverse alist))
    table))
