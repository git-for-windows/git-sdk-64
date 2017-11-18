;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2015 Free Software Foundation, Inc.

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

(define-module (language cps spec)
  #:use-module (ice-9 match)
  #:use-module (system base language)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps compile-bytecode)
  #:export (cps))

(define (read-cps port env)
  (let lp ((out empty-intmap))
    (match (read port)
      ((k exp) (lp (intmap-add! out k (parse-cps exp))))
      ((? eof-object?)
       (if (eq? out empty-intmap)
           the-eof-object
           (persistent-intmap out))))))

(define* (write-cps exp #:optional (port (current-output-port)))
  (intmap-fold (lambda (k cps port)
                 (write (list k (unparse-cps cps)) port)
                 (newline port)
                 port)
               exp port))

(define-language cps
  #:title	"CPS Intermediate Language"
  #:reader	read-cps
  #:printer	write-cps
  #:compilers   `((bytecode . ,compile-bytecode))
  #:for-humans? #f
  )
