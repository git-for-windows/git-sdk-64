;;; Disassemble --- Disassemble .go files into something human-readable

;; Copyright 2005, 2008, 2009, 2011, 2014 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this software; see the file COPYING.LESSER.  If
;; not, write to the Free Software Foundation, Inc., 51 Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;; Author: Andy Wingo <wingo@pobox.com>

;;; Commentary:

;; Usage: disassemble [ARGS]

;;; Code:

(define-module (scripts disassemble)
  #:use-module (system vm objcode)
  #:use-module ((language assembly disassemble) #:prefix asm:)
  #:export (disassemble))

(define %summary "Disassemble a compiled .go file.")

(define (disassemble . files)
  (for-each (lambda (file)
              (asm:disassemble (load-objcode file)))
            files))

(define main disassemble)
