;;; installed-scm-file

;;;; Copyright (C) 2003, 2006, 2011, 2014 Free Software Foundation, Inc.
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


(define-module (ice-9 weak-vector)
  #:export (make-weak-vector
            list->weak-vector
            weak-vector
            weak-vector?
            weak-vector-ref
            weak-vector-set!))

(eval-when (load eval compile)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_weak_vector_builtins"))
