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

;;; Based on code from https://gitlab.com/akku/akku-scm, written
;;; 2018-2019 by Göran Weinholt <goran@weinholt.se>, as well as
;;; https://github.com/okuoku/yuni, written 2014-2018 by OKUMURA Yuki
;;; <mjt@cltn.org>.  This code was originally released under the
;;; following terms:
;;;
;;;     To the extent possible under law, the author(s) have dedicated
;;;     all copyright and related and neighboring rights to this
;;;     software to the public domain worldwide. This software is
;;;     distributed without any warranty.
;;;
;;;     See <http://creativecommons.org/publicdomain/zero/1.0/>, for a
;;;     copy of the CC0 Public Domain Dedication.

(define-module (scheme process-context)
  #:use-module (srfi srfi-98)
  #:re-export (command-line
               get-environment-variable
               get-environment-variables)
  #:export (emergency-exit
            (r7:exit . exit)))

(define (translate-status status)
  (case status
    ((#t) 0)
    ((#f) 1)
    (else status)))

(define r7:exit
  (case-lambda
    (()
     (exit))
    ((status)
     (exit (translate-status status)))))

(define emergency-exit
  (case-lambda
    (()
     (primitive-_exit))
    ((status)
     (primitive-_exit (translate-status status)))))
