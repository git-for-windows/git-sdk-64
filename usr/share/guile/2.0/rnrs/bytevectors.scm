;;;; bytevectors.scm --- R6RS bytevector API           -*- coding: utf-8 -*-

;;;;	Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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

;;; Author: Ludovic Courtès <ludo@gnu.org>

;;; Commentary:
;;;
;;; A "bytevector" is a raw bit string.  This module provides procedures to
;;; manipulate bytevectors and interpret their contents in a number of ways:
;;; bytevector contents can be accessed as signed or unsigned integer of
;;; various sizes and endianness, as IEEE-754 floating point numbers, or as
;;; strings.  It is a useful tool to decode binary data.
;;;
;;; Code:

(define-module (rnrs bytevectors)
  #:version (6)
  #:export-syntax (endianness)
  #:export (native-endianness bytevector?
           make-bytevector bytevector-length bytevector=? bytevector-fill!
           bytevector-copy! bytevector-copy
           uniform-array->bytevector
           bytevector-u8-ref bytevector-s8-ref
           bytevector-u8-set! bytevector-s8-set! bytevector->u8-list
           u8-list->bytevector
           bytevector-uint-ref bytevector-uint-set!
           bytevector-sint-ref bytevector-sint-set!
           bytevector->sint-list bytevector->uint-list
           uint-list->bytevector sint-list->bytevector

           bytevector-u16-ref bytevector-s16-ref
           bytevector-u16-set! bytevector-s16-set!
           bytevector-u16-native-ref bytevector-s16-native-ref
           bytevector-u16-native-set! bytevector-s16-native-set!

           bytevector-u32-ref bytevector-s32-ref
           bytevector-u32-set! bytevector-s32-set!
           bytevector-u32-native-ref bytevector-s32-native-ref
           bytevector-u32-native-set! bytevector-s32-native-set!

           bytevector-u64-ref bytevector-s64-ref
           bytevector-u64-set! bytevector-s64-set!
           bytevector-u64-native-ref bytevector-s64-native-ref
           bytevector-u64-native-set! bytevector-s64-native-set!

           bytevector-ieee-single-ref
           bytevector-ieee-single-set!
           bytevector-ieee-single-native-ref
           bytevector-ieee-single-native-set!

           bytevector-ieee-double-ref
           bytevector-ieee-double-set!
           bytevector-ieee-double-native-ref
           bytevector-ieee-double-native-set!

           string->utf8 string->utf16 string->utf32
           utf8->string utf16->string utf32->string))


(load-extension (string-append "libguile-" (effective-version))
                "scm_init_bytevectors")

(define-macro (endianness sym)
  (if (memq sym '(big little))
      `(quote ,sym)
      (error "unsupported endianness" sym)))

;;; bytevector.scm ends here
