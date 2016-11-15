;;; srfi-26.scm --- specializing parameters without currying.

;; Copyright (C) 2002, 2006, 2010 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (srfi srfi-26)
  :export (cut cute))

(cond-expand-provide (current-module) '(srfi-26))

(define-syntax cut
  (lambda (stx)
    (syntax-case stx ()
      ((cut slot0 slot1+ ...)
       (let loop ((slots	#'(slot0 slot1+ ...))
                  (params	'())
                  (args	'()))
         (if (null? slots)
             #`(lambda #,(reverse params) #,(reverse args))
             (let ((s	  (car slots))
                   (rest (cdr slots)))
               (with-syntax (((var) (generate-temporaries '(var))))
                 (syntax-case s (<> <...>)
                   (<>
                    (loop rest (cons #'var params) (cons #'var args)))
                   (<...>
                    (if (pair? rest)
                        (error "<...> not on the end of cut expression"))
                    #`(lambda #,(append (reverse params) #'var)
                        (apply #,@(reverse (cons #'var args)))))
                   (else
                    (loop rest params (cons s args))))))))))))

(define-syntax cute
  (lambda (stx)
    (syntax-case stx ()
      ((cute slots ...)
       (let loop ((slots #'(slots ...))
                  (bindings '())
                  (arguments '()))
         (define (process-hole)
           (loop (cdr slots) bindings (cons (car slots) arguments)))
         (if (null? slots)
             #`(let #,bindings
                 (cut #,@(reverse arguments)))
             (syntax-case (car slots) (<> <...>)
               (<> (process-hole))
               (<...> (process-hole))
               (expr
                (with-syntax (((t) (generate-temporaries '(t))))
                  (loop (cdr slots)
                        (cons #'(t expr) bindings)
                        (cons #'t arguments)))))))))))
