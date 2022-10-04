;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; Guile's CPS language is a label->cont mapping, which seems simple
;;; enough.  However it's often cumbersome to thread around the output
;;; CPS program when doing non-trivial transformations, or when building
;;; a CPS program from scratch.  For example, when visiting an
;;; expression during CPS conversion, we usually already know the label
;;; and the $kargs wrapper for the cont, and just need to know the body
;;; of that cont.  However when building the body of that possibly
;;; nested Tree-IL expression we will also need to add conts to the
;;; result, so really it's a process that takes an incoming program,
;;; adds conts to that program, and returns the result program and the
;;; result term.
;;;
;;; It's a bit treacherous to do in a functional style as once you start
;;; adding to a program, you shouldn't add to previous versions of that
;;; program.  Getting that right in the context of this program seed
;;; that is threaded through the conversion requires the use of a
;;; pattern, with-cps.
;;;
;;; with-cps goes like this:
;;;
;;;   (with-cps cps clause ... tail-clause)
;;;
;;; Valid clause kinds are:
;;;
;;;   (letk LABEL CONT)
;;;   (setk LABEL CONT)
;;;   (letv VAR ...)
;;;   (let$ X (PROC ARG ...))
;;;
;;; letk and letv create fresh CPS labels and variable names,
;;; respectively.  Labels and vars bound by letk and letv are in scope
;;; from their point of definition onward.  letv just creates fresh
;;; variable names for use in other parts of with-cps, while letk binds
;;; fresh labels to values and adds them to the resulting program.  The
;;; right-hand-side of letk, CONT, is passed to build-cont, so it should
;;; be a valid production of that language.  setk is like letk but it
;;; doesn't create a fresh label name.
;;;
;;; let$ delegates processing to a sub-computation.  The form (PROC ARG
;;; ...) is syntactically altered to be (PROC CPS ARG ...), where CPS is
;;; the value of the program being built, at that point in the
;;; left-to-right with-cps execution.  That form is is expected to
;;; evaluate to two values: the new CPS term, and the value to bind to
;;; X.  X is in scope for the following with-cps clauses.  The name was
;;; chosen because the $ is reminiscent of the $ in CPS data types.
;;;
;;; The result of the with-cps form is determined by the tail clause,
;;; which may be of these kinds:
;;;
;;;   ($ (PROC ARG ...))
;;;   (setk LABEL CONT)
;;;   EXP
;;;
;;; $ is like let$, but in tail position.  If the tail clause is setk,
;;; then only one value is returned, the resulting CPS program.
;;; Otherwise EXP is any kind of expression, which should not add to the
;;; resulting program.  Ending the with-cps with EXP is equivalant to
;;; returning (values CPS EXP).
;;;
;;; It's a bit of a monad, innit?  Don't tell anyone though!
;;;
;;; Sometimes you need to just bind some constants to CPS values.
;;; with-cps-constants is there for you.  For example:
;;;
;;;   (with-cps-constants cps ((foo 34))
;;;     (build-term ($values (foo))))
;;;
;;; The body of with-cps-constants is a with-cps clause, or a sequence
;;; of such clauses.  But usually you will want with-cps-constants
;;; inside a with-cps, so it usually looks like this:
;;;
;;;   (with-cps cps
;;;     ...
;;;     ($ (with-cps-constants ((foo 34))
;;;          (build-term ($values (foo))))))
;;;
;;; which is to say that the $ or the let$ adds the CPS argument for us.
;;;
;;; Code:

(define-module (language cps with-cps)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:export (with-cps with-cps-constants))

(define-syntax with-cps
  (syntax-rules (letk setk letv let$ $)
    ((_ (exp ...) clause ...)
     (let ((cps (exp ...)))
       (with-cps cps clause ...)))
    ((_ cps (letk label cont) clause ...)
     (let-fresh (label) ()
       (with-cps (intmap-add! cps label (build-cont cont))
         clause ...)))
    ((_ cps (setk label cont))
     (intmap-add! cps label (build-cont cont)
                  (lambda (old new) new)))
    ((_ cps (setk label cont) clause ...)
     (with-cps (with-cps cps (setk label cont))
       clause ...))
    ((_ cps (letv v ...) clause ...)
     (let-fresh () (v ...)
       (with-cps cps clause ...)))
    ((_ cps (let$ var (proc arg ...)) clause ...)
     (call-with-values (lambda () (proc cps arg ...))
       (lambda (cps var)
         (with-cps cps clause ...))))
    ((_ cps ($ (proc arg ...)))
     (proc cps arg ...))
    ((_ cps exp)
     (values cps exp))))

(define-syntax with-cps-constants
  (syntax-rules ()
    ((_ cps () clause ...)
     (with-cps cps clause ...))
    ((_ cps ((var val) (var* val*) ...) clause ...)
     (let ((x val))
       (with-cps cps
         (letv var)
         (let$ body (with-cps-constants ((var* val*) ...)
                      clause ...))
         (letk label ($kargs ('var) (var) ,body))
         (build-term ($continue label #f ($const x))))))))
