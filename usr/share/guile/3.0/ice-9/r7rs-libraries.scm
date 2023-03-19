;; R7RS library support
;;      Copyright (C) 2020, 2021 Free Software Foundation, Inc.
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


;; This file is included from boot-9.scm and assumes the existence of (and
;; expands into) procedures and syntactic forms defined therein.

(define-syntax include-library-declarations
  (lambda (x)
    (syntax-violation
     'include-library-declarations
     "use of 'include-library-declarations' outside define-library" x x)))

;; FIXME: Implement properly!
(define-syntax-rule (include-ci filename)
  (include filename))

(define-syntax define-library
  (lambda (stx)
    (define (handle-includes filenames)
      (syntax-case filenames ()
        (() #'())
        ((filename . filenames)
         (append (call-with-include-port
                  #'filename
                  (lambda (p)
                    (let lp ()
                      (let ((x (read p)))
                        (if (eof-object? x)
                            #'()
                            (cons (datum->syntax #'filename x) (lp)))))))
                 (handle-includes #'filenames)))))

    (define (handle-cond-expand clauses)
      (define (has-req? req)
        (syntax-case req (and or not library)
          ((and req ...)
           (and-map has-req? #'(req ...)))
          ((or req ...)
           (or-map has-req? #'(req ...)))
          ((not req)
           (not (has-req? #'req)))
          ((library lib-name)
           (->bool
            (false-if-exception
             (resolve-r6rs-interface
              (syntax->datum #'lib-name)))))
          (id
           (identifier? #'id)
           ;; FIXME: R7RS (features) isn't quite the same as
           ;; %cond-expand-features; see scheme/base.scm.
           (memq (syntax->datum #'id) %cond-expand-features))))
      (syntax-case clauses ()
        (() #'())  ; R7RS says this is not specified :-/
        (((test decl ...) . clauses)
         (if (has-req? #'test)
             #'(decl ...)
             (handle-cond-expand #'clauses)))))

    (define (partition-decls decls exports imports code)
      (syntax-case decls (export import begin include include-ci
                                 include-library-declarations cond-expand)
        (() (values exports imports (reverse code)))
        (((export clause ...) . decls)
         (partition-decls #'decls (append exports #'(clause ...)) imports code))
        (((import clause ...) . decls)
         (partition-decls #'decls exports (append imports #'(clause ...)) code))
        (((begin expr ...) . decls)
         (partition-decls #'decls exports imports
                          (cons #'(begin expr ...) code)))
        (((include filename ...) . decls)
         (partition-decls #'decls exports imports
                          (cons #'(begin (include filename) ...) code)))
        (((include-ci filename ...) . decls)
         (partition-decls #'decls exports imports
                          (cons #'(begin (include-ci filename) ...) code)))
        (((include-library-declarations filename ...) . decls)
         (syntax-case (handle-includes #'(filename ...)) ()
           ((decl ...)
            (partition-decls #'(decl ... . decls) exports imports code))))
        (((cond-expand clause ...) . decls)
         (syntax-case (handle-cond-expand #'(clause ...)) ()
           ((decl ...)
            (partition-decls #'(decl ... . decls) exports imports code))))))

    (syntax-case stx ()
      ((_ name decl ...)
       (call-with-values (lambda ()
                           (partition-decls #'(decl ...) '() '() '()))
         (lambda (exports imports code)
           #`(library name
               (export . #,exports)
               (import . #,imports)
               . #,code)))))))
