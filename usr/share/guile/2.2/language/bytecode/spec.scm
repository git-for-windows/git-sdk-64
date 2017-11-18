;;; Bytecode

;; Copyright (C) 2013 Free Software Foundation, Inc.

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

(define-module (language bytecode spec)
  #:use-module (system base language)
  #:use-module (system vm loader)
  #:use-module (ice-9 binary-ports)
  #:export (bytecode))

(define (bytecode->value x e opts)
  (let ((thunk (load-thunk-from-memory x)))
    (if (eq? e (current-module))
        ;; save a cons in this case
        (values (thunk) e e)
        (save-module-excursion
         (lambda ()
           (set-current-module e)
           (values (thunk) e e))))))

(define-language bytecode
  #:title	"Bytecode"
  #:compilers   `((value . ,bytecode->value))
  #:printer	(lambda (bytecode port) (put-bytevector port bytecode))
  #:reader      get-bytevector-all
  #:for-humans? #f)
