;;; Guile VM core

;;; Copyright (C) 2001, 2009, 2010, 2013, 2014 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (system vm vm)
  #:export (call-with-vm
            call-with-stack-overflow-handler
            vm-trace-level set-vm-trace-level!
            vm-engine set-vm-engine! set-default-vm-engine!
            vm-push-continuation-hook vm-pop-continuation-hook
            vm-apply-hook
            vm-next-hook
            vm-abort-continuation-hook))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm")
