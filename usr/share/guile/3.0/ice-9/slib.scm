;;;; slib.scm --- definitions needed to get SLIB to work with Guile
;;;;
;;;; Copyright (C) 1997, 1998, 2000, 2001, 2002, 2003, 2004, 2006, 2007, 2013 Free Software Foundation, Inc.
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

;;; Look for slib.init in the $datadir, in /usr/share, and finally in
;;; the load path.  It's not usually in the load path on common distros,
;;; but it could be if the user put it there.  The init file takes care
;;; of defining the module.

(let ((try-load (lambda (dir)
                  (let ((init (string-append dir "/slib/guile.init")))
                    (and (file-exists? init)
                         (begin
                           (load init)
                           #t))))))
  (or (try-load (assq-ref %guile-build-info 'datadir))
      (try-load "/usr/share")
      (load-from-path "slib/guile.init")))
