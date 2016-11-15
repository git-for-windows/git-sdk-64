;;; files.scm --- The R6RS file system library

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


(library (rnrs files (6))
  (export file-exists? 
	  delete-file

	  &i/o make-i/o-error i/o-error?
	  &i/o-read make-i/o-read-error i/o-read-error?
	  &i/o-write make-i/o-write-error i/o-write-error?

	  &i/o-invalid-position 
	  make-i/o-invalid-position-error 
	  i/o-invalid-position-error? 
	  i/o-error-position
	  
	  &i/o-filename
	  make-i/o-filename-error
	  i/o-filename-error?
	  i/o-error-filename
	  
	  &i/o-file-protection 
	  make-i/o-file-protection-error
	  i/o-file-protection-error?

	  &i/o-file-is-read-only
	  make-i/o-file-is-read-only-error
	  i/o-file-is-read-only-error?

	  &i/o-file-already-exists
	  make-i/o-file-already-exists-error
	  i/o-file-already-exists-error?

	  &i/o-file-does-not-exist
	  make-i/o-file-does-not-exist-error
	  i/o-file-does-not-exist-error?

	  &i/o-port
	  make-i/o-port-error
	  i/o-port-error?
	  i/o-error-port)

  (import (rename (only (guile) file-exists? delete-file catch @@) 
		  (delete-file delete-file-internal))
	  (rnrs base (6))
	  (rnrs conditions (6))
	  (rnrs exceptions (6)))

  (define (delete-file filename)
    (catch #t 
	   (lambda () (delete-file-internal filename))
	   (lambda (key . args) (raise (make-i/o-filename-error filename)))))

  ;; Condition types that are used by (rnrs files), (rnrs io ports), and
  ;; (rnrs io simple).  These are defined here so as to be easily shareable by
  ;; these three libraries.
  
  (define-condition-type &i/o &error make-i/o-error i/o-error?)
  (define-condition-type &i/o-read &i/o make-i/o-read-error i/o-read-error?)
  (define-condition-type &i/o-write &i/o make-i/o-write-error i/o-write-error?)
  (define-condition-type &i/o-invalid-position
    &i/o make-i/o-invalid-position-error i/o-invalid-position-error?
    (position i/o-error-position))
  (define-condition-type &i/o-filename 
    &i/o make-i/o-filename-error i/o-filename-error?
    (filename i/o-error-filename))
  (define-condition-type &i/o-file-protection
    &i/o-filename make-i/o-file-protection-error i/o-file-protection-error?)
  (define-condition-type &i/o-file-is-read-only
    &i/o-file-protection make-i/o-file-is-read-only-error 
    i/o-file-is-read-only-error?)
  (define-condition-type &i/o-file-already-exists
    &i/o-filename make-i/o-file-already-exists-error 
    i/o-file-already-exists-error?)
  (define-condition-type &i/o-file-does-not-exist
    &i/o-filename make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error?)
  (define-condition-type &i/o-port &i/o make-i/o-port-error i/o-port-error?
    (port i/o-error-port))
)
