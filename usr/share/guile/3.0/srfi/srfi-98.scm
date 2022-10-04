;;; srfi-98.scm --- An interface to access environment variables

;; Copyright (C) 2009 Free Software Foundation, Inc.
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

;;; Author: Julian Graham <julian.graham@aya.yale.edu>
;;; Date: 2009-05-26

;;; Commentary:

;; This is an implementation of SRFI-98 (An interface to access environment 
;; variables).
;;
;; This module is fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-98)
  :use-module (srfi srfi-1)
  :export (get-environment-variable
	   get-environment-variables))

(cond-expand-provide (current-module) '(srfi-98))

(define get-environment-variable getenv)
(define (get-environment-variables)
  (define (string->alist-entry str)
    (let ((pvt (string-index str #\=))
	  (len (string-length str)))
      (and pvt (cons (substring str 0 pvt) (substring str (+ pvt 1) len)))))
  (filter-map string->alist-entry (environ)))
