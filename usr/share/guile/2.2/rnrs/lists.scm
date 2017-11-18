;;; lists.scm --- The R6RS list utilities library

;;      Copyright (C) 2010 Free Software Foundation, Inc.
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


(library (rnrs lists (6))
  (export find for-all exists filter partition fold-left fold-right remp remove 
	  remv remq memp member memv memq assp assoc assv assq cons*)
  (import (rnrs base (6))
          (only (guile) filter member memv memq assoc assv assq cons*)
	  (rename (only (srfi srfi-1) any 
				      every 
				      remove 
				      member 
				      assoc 
				      find 
				      partition
				      fold-right 
				      filter-map)
		  (any exists) 
		  (every for-all)
		  (remove remp)
		  
		  (member memp-internal)
		  (assoc assp-internal)))

  (define (fold-left combine nil list . lists)
    (define (fold nil lists)
      (if (exists null? lists)
          nil
          (fold (apply combine nil (map car lists))
                (map cdr lists))))
    (fold nil (cons list lists)))

  (define (remove obj list) (remp (lambda (elt) (equal? obj elt)) list))
  (define (remv obj list) (remp (lambda (elt) (eqv? obj elt)) list))
  (define (remq obj list) (remp (lambda (elt) (eq? obj elt)) list))

  (define (memp pred list) (memp-internal #f list (lambda (x y) (pred y))))
  (define (assp pred list) (assp-internal #f list (lambda (x y) (pred y))))
)
