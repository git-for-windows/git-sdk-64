;;; srfi-34.scm --- Exception handling for programs

;; Copyright (C) 2003,2006,2008-2010,2019-2020
;;   Free Software Foundation, Inc.
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

;;; Author: Neil Jerram <neil@ossau.uklinux.net>

;;; Commentary:

;; This is an implementation of SRFI-34: Exception Handling for
;; Programs.  For documentation please see the SRFI-34 document; this
;; module is not yet documented at all in the Guile manual.

;;; Code:

(define-module (srfi srfi-34)
  #:use-module ((ice-9 exceptions) #:select (guard))
  #:re-export (with-exception-handler
               (raise-exception . raise)
               guard)
  #:re-export-and-replace ((raise-exception . raise)))

(cond-expand-provide (current-module) '(srfi-34))
