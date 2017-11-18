;;; r5rs.scm --- The R6RS / R5RS compatibility library

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


(library (rnrs r5rs (6))
  (export exact->inexact inexact->exact 

	  quotient remainder modulo 

	  delay force 

	  null-environment scheme-report-environment)
  (import (only (guile) exact->inexact inexact->exact
			
			quotient remainder modulo
			
			delay force)
          (only (ice-9 r5rs) scheme-report-environment)
          (only (ice-9 safe-r5rs) null-environment)))
