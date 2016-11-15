;;; srfi-6.scm --- Basic String Ports

;; 	Copyright (C) 2001, 2002, 2003, 2006, 2012 Free Software Foundation, Inc.
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

;;; Commentary:

;; This module is fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-6)
  #:replace (open-input-string open-output-string)
  #:re-export (get-output-string))

;; SRFI-6 says nothing about encodings, and assumes that any character
;; or string can be written to a string port.  Thus, make all SRFI-6
;; string ports Unicode capable.  See <http://bugs.gnu.org/11197>.

(define (open-input-string s)
  (with-fluids ((%default-port-encoding "UTF-8"))
    ((@ (guile) open-input-string) s)))

(define (open-output-string)
  (with-fluids ((%default-port-encoding "UTF-8"))
    ((@ (guile) open-output-string))))

(cond-expand-provide (current-module) '(srfi-6))

;;; srfi-6.scm ends here
