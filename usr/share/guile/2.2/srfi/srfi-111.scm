;;; srfi-111.scm -- SRFI 111 Boxes

;;      Copyright (C) 2014 Free Software Foundation, Inc.
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

(define-module (srfi srfi-111)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (box box? unbox set-box!))

(cond-expand-provide (current-module) '(srfi-111))

(define-record-type <box>
  (box value)
  box?
  (value unbox set-box!))

(set-record-type-printer! <box>
  (lambda (box port)
    (display "#<box " port)
    (display (number->string (object-address box) 16) port)
    (display " value: ")
    (write (unbox box) port)
    (display ">" port)))
