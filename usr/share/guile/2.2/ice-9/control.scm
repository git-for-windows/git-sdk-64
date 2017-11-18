;;; Beyond call/cc

;; Copyright (C) 2010, 2011, 2013 Free Software Foundation, Inc.

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

;;; Code:

(define-module (ice-9 control)
  #:re-export (call-with-prompt abort-to-prompt
               default-prompt-tag make-prompt-tag)
  #:export (% abort shift reset shift* reset*
            call-with-escape-continuation call/ec
            let-escape-continuation let/ec
            suspendable-continuation?))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_ice_9_control")

(define (abort . args)
  (apply abort-to-prompt (default-prompt-tag) args))

(define-syntax %
  (syntax-rules ()
    ((_ expr)
     (call-with-prompt (default-prompt-tag)
                       (lambda () expr)
                       default-prompt-handler))
    ((_ expr handler)
     (call-with-prompt (default-prompt-tag)
                       (lambda () expr)
                       handler))
    ((_ tag expr handler)
     (call-with-prompt tag
                       (lambda () expr)
                       handler))))

;; Each prompt tag has a type -- an expected set of arguments, and an unwritten
;; contract of what its handler will do on an abort. In the case of the default
;; prompt tag, we could choose to return values, exit nonlocally, or punt to the
;; user.
;;
;; We choose the latter, by requiring that the user return one value, a
;; procedure, to an abort to the prompt tag. That argument is then invoked with
;; the continuation as an argument, within a reinstated default prompt. In this
;; way the return value(s) from a default prompt are under the user's control.
(define (default-prompt-handler k proc)
  (% (default-prompt-tag)
     (proc k)
     default-prompt-handler))

;; Kindly provided by Wolfgang J Moeller <wjm@heenes.com>, modelled
;; after the ones by Oleg Kiselyov in
;; http://okmij.org/ftp/Scheme/delim-control-n.scm, which are in the
;; public domain, as noted at the top of http://okmij.org/ftp/.
;; 
(define-syntax-rule (reset . body)
  (call-with-prompt (default-prompt-tag)
                    (lambda () . body)
                    (lambda (cont f) (f cont))))

(define-syntax-rule (shift var . body)
  (abort-to-prompt (default-prompt-tag)
                   (lambda (cont)
                     ((lambda (var) (reset . body))
                      (lambda vals (reset (apply cont vals)))))))

(define (reset* thunk)
  (reset (thunk)))

(define (shift* fc)
  (shift c (fc c)))

(define (call-with-escape-continuation proc)
  "Call PROC with an escape continuation."
  (let ((tag (list 'call/ec)))
    (call-with-prompt tag
                      (lambda ()
                        (proc (lambda args
                                (apply abort-to-prompt tag args))))
                      (lambda (_ . args)
                        (apply values args)))))

(define call/ec call-with-escape-continuation)

(define-syntax-rule (let-escape-continuation k body ...)
  "Bind K to an escape continuation within the lexical extent of BODY."
  (let ((tag (list 'let/ec)))
    (call-with-prompt tag
                      (lambda ()
                        (let ((k (lambda args
                                   (apply abort-to-prompt tag args))))
                          body ...))
                      (lambda (_ . results)
                        (apply values results)))))

(define-syntax-rule (let/ec k body ...)
  (let-escape-continuation k body ...))
