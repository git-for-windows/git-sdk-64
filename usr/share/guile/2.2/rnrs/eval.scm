;;; eval.scm --- The R6RS `eval' library

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


(library (rnrs eval (6))
  (export eval environment)
  (import (only (guile) eval 
		        make-module 
			module-uses
			beautify-user-module! 
			set-module-uses!)
	  (rnrs base (6))
	  (rnrs io simple (6))
	  (rnrs lists (6)))

  (define (environment . import-specs)
    (let ((module (make-module))
	  (needs-purify? (not (member '(guile) import-specs))))
      (beautify-user-module! module)
      (for-each (lambda (import-spec) (eval (list 'import import-spec) module))
		import-specs)
      (if needs-purify? (set-module-uses! module (cdr (module-uses module))))
      module))
)
