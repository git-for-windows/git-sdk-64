;;; Guile Emacs Lisp

;; Copyright (C) 2001, 2009, 2010, 2018 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language elisp spec)
  #:use-module (language elisp compile-tree-il)
  #:use-module (language elisp parser)
  #:use-module (system base language)
  #:use-module (system base compile)
  #:use-module (system base target)
  #:export (elisp))

(define-language elisp
  #:title     "Emacs Lisp"
  #:reader    (lambda (port env) (read-elisp port))
  #:printer   write
  #:compilers `((tree-il . ,compile-tree-il)))

;; Compile and load the Elisp boot code for the native host
;; architecture.  We must specifically ask for native compilation here,
;; because this module might be loaded in a dynamic environment where
;; cross-compilation has been requested using 'with-target'.  For
;; example, this happens when cross-compiling Guile itself.
(with-native-target
  (lambda ()
    (compile-and-load (%search-load-path "language/elisp/boot.el")
                      #:from 'elisp)))
