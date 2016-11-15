;;;; 	Copyright (C) 2000, 2001, 2004, 2006, 2010 Free Software Foundation, Inc.
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

;;;; A simple value history support

(define-module (ice-9 history)
  #:export (value-history-enabled? enable-value-history! disable-value-history!
            clear-value-history!))

(define-module* '(value-history))

(define *value-history-enabled?* #f)
(define (value-history-enabled?)
  *value-history-enabled?*)

(define (use-value-history x)
  (module-use! (current-module)
	       (resolve-interface '(value-history))))

(define save-value-history
  (let ((count 0)
	(history (resolve-module '(value-history))))
    (lambda (v)
      (if (not (unspecified? v))
	  (let* ((c (1+ count))
		 (s (string->symbol (simple-format #f "$~A" c))))
	    (simple-format #t "~A = " s)
	    (module-define! history s v)
	    (module-export! history (list s))
	    (set! count c))))))

(define (enable-value-history!)
  (if (not (value-history-enabled?))
      (begin
        (add-hook! before-eval-hook use-value-history)
        (add-hook! before-print-hook save-value-history)
        (set! *value-history-enabled?* #t))))

(define (disable-value-history!)
  (if (value-history-enabled?)
      (begin
        (remove-hook! before-eval-hook use-value-history)
        (remove-hook! before-print-hook save-value-history)
        (set! *value-history-enabled?* #f))))

(define (clear-value-history!)
  (let ((history (resolve-module '(value-history))))
    (hash-clear! (module-obarray history))
    (hash-clear! (module-obarray (module-public-interface history)))))

(enable-value-history!)
