;;; R7RS compatibility libraries
;;; Copyright (C) 2019 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (scheme eval)
  #:use-module (ice-9 match)
  #:export (environment)
  #:re-export (eval))

(define (environment . import-specs)
  (let ((module (make-module)))
    (beautify-user-module! module)
    (purify-module! module)
    (module-use! module (resolve-interface '(guile) #:select '(import)))
    (for-each (lambda (import-spec)
                (eval (list 'import import-spec) module))
	      import-specs)
    ;; Remove the "import" import.  FIXME: this is pretty hacky stuff :(
    (set-module-uses! module (cdr (module-uses module)))
    (hash-clear! (module-import-obarray module))
    (module-modified module)
    module))
