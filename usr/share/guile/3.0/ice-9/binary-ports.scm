;;; binary-ports.scm --- Binary IO on ports
;;; Copyright (C) 2009-2011,2013,2016,2019,2021 Free Software Foundation, Inc.
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

;;; Author: Ludovic Courtès <ludo@gnu.org>

;;; Commentary:
;;;
;;; The I/O port API of the R6RS is provided by this module.  In many areas
;;; it complements or refines Guile's own historical port API.  For instance,
;;; it allows for binary I/O with bytevectors.
;;;
;;; Code:

(define-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (eof-object
            open-bytevector-input-port
            make-custom-binary-input-port
            get-u8
            lookahead-u8
            get-bytevector-n
            get-bytevector-n!
            get-bytevector-some
            get-bytevector-some!  ; Guile extension, not in R6RS
            get-bytevector-all
            get-string-n!
            put-u8
            put-bytevector
            unget-bytevector
            open-bytevector-output-port
            make-custom-binary-output-port
            make-custom-binary-input/output-port
            call-with-input-bytevector
            call-with-output-bytevector))

;; Note that this extension also defines %make-transcoded-port, which is
;; not exported but is used by (rnrs io ports).

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_r6rs_ports")

(define (call-with-input-bytevector bv proc)
  "Call the one-argument procedure @var{proc} with a newly created
binary input port from which the bytevector @var{bv}'s contents may be
read.  All values yielded by @var{proc} are returned."
  (proc (open-bytevector-input-port bv)))

(define (call-with-output-bytevector proc)
  "Call the one-argument procedure @var{proc} with a newly created
binary output port.  When the function returns, port is closed and the
bytevector composed of the bytes written into the port is returned."
  (call-with-values
      (lambda ()
        (open-bytevector-output-port))
    (lambda (port get-bytevector)
      (proc port)
      (let ((bv (get-bytevector)))
        (close-port port)
        bv))))
