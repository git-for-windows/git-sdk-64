;;; exceptions.scm --- The R6RS exceptions library

;;      Copyright (C) 2010, 2011, 2013, 2020 Free Software Foundation, Inc.
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


(library (rnrs exceptions (6))
  (export guard with-exception-handler raise raise-continuable)
  (import (rnrs base (6))
          (rnrs control (6))
          (rnrs conditions (6))
	  (rename (ice-9 exceptions)
                  (raise-exception raise))))
