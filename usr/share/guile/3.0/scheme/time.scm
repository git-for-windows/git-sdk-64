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

(define-module (scheme time)
  #:use-module (srfi srfi-19)
  #:export (current-jiffy current-second jiffies-per-second))

(define (jiffies-per-second)
  internal-time-units-per-second)

(define (current-jiffy)
  (get-internal-real-time))

(define (current-second)
  (let ((t (current-time time-tai)))
    (+ (time-second t)
       (* 1e-9 (time-nanosecond t)))))
