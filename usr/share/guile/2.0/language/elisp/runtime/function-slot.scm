;;; Guile Emacs Lisp

;;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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

(define-module (language elisp runtime function-slot)
  #:use-module (language elisp runtime subrs)
  #:use-module ((language elisp runtime macros)
                #:select
                ((macro-lambda . lambda)
                 (macro-prog1 . prog1)
                 (macro-prog2 . prog2)
                 (macro-when . when)
                 (macro-unless . unless)
                 (macro-cond . cond)
                 (macro-and . and)
                 (macro-or . or)
                 (macro-dotimes . dotimes)
                 (macro-dolist . dolist)
                 (macro-catch . catch)
                 (macro-unwind-protect . unwind-protect)
                 (macro-pop . pop)
                 (macro-push . push)))
  #:use-module ((language elisp compile-tree-il)
                #:select
                ((compile-progn . progn)
                 (compile-if . if)
                 (compile-defconst . defconst)
                 (compile-defvar . defvar)
                 (compile-setq . setq)
                 (compile-let . let)
                 (compile-lexical-let . lexical-let)
                 (compile-flet . flet)
                 (compile-let* . let*)
                 (compile-lexical-let* . lexical-let*)
                 (compile-flet* . flet*)
                 (compile-with-always-lexical . with-always-lexical)
                 (compile-guile-ref . guile-ref)
                 (compile-guile-primitive . guile-primitive)
                 (compile-while . while)
                 (compile-function . function)
                 (compile-defun . defun)
                 (compile-defmacro . defmacro)
                 (#{compile-`}# . #{`}#)
                 (compile-quote . quote)))
  #:duplicates (last)
  ;; special operators
  #:re-export (progn
               if
               defconst
               defvar
               setq
               let
               lexical-let
               flet
               let*
               lexical-let*
               flet*
               with-always-lexical
               guile-ref
               guile-primitive
               while
               function
               defun
               defmacro
               #{`}#
               quote)
  ;; macros
  #:re-export (lambda
               prog1
               prog2
               when
               unless
               cond
               and
               or
               dotimes
               dolist
               catch
               unwind-protect
               pop
               push)
  ;; functions
  #:re-export (eq
               equal
               floatp
               integerp
               numberp
               wholenump
               zerop
               =
               /=
               <
               <=
               >
               >=
               max
               min
               abs
               float
               1+
               1-
               +
               -
               *
               %
               ffloor
               fceiling
               ftruncate
               fround
               consp
               atomp
               listp
               nlistp
               null
               car
               cdr
               car-safe
               cdr-safe
               nth
               nthcdr
               length
               cons
               list
               make-list
               append
               reverse
               copy-tree
               number-sequence
               setcar
               setcdr
               symbol-value
               symbol-function
               set
               fset
               makunbound
               fmakunbound
               boundp
               fboundp
               apply
               funcall
               throw
               not
               eval
               load))
