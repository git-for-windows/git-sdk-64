;;; installed-scm-file

;;;; 	Copyright (C) 2024 Free Software Foundation, Inc.
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


;;; This module replaces the standard method and define-method syntax
;;; with versions with advanced argument handling. This doesn't incur
;;; overhead for methods without keyword formals except marginally for
;;; compilation.
;;;
(define-module (oop goops keyword-formals)
  #:use-module ((oop goops)
                #:select ((method* . method)
                          (define-method* . define-method)))
  #:re-export-and-replace (method define-method))

