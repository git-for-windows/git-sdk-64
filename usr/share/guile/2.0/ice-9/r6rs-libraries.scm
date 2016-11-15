;;; r6rs-libraries.scm --- Support for the R6RS `library' and `import' forms

;;      Copyright (C) 2010 Free Software Foundation, Inc.
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

(define (resolve-r6rs-interface import-spec)
  (define (make-custom-interface mod)
    (let ((iface (make-module)))
      (set-module-kind! iface 'custom-interface)
      (set-module-name! iface (module-name mod))
      iface))
  (define (sym? x) (symbol? (syntax->datum x)))

  (syntax-case import-spec (library only except prefix rename srfi)
    ;; (srfi :n ...) -> (srfi srfi-n ...)
    ((library (srfi colon-n rest ... (version ...)))
     (and (and-map sym? #'(srfi rest ...))
          (symbol? (syntax->datum #'colon-n))
          (eqv? (string-ref (symbol->string (syntax->datum #'colon-n)) 0) #\:))
     (let ((srfi-n (string->symbol
                    (string-append
                     "srfi-"
                     (substring (symbol->string (syntax->datum #'colon-n))
                                1)))))
       (resolve-r6rs-interface
        (syntax-case #'(rest ...) ()
          (()
           #`(library (srfi #,srfi-n (version ...))))
          ((name rest ...)
           ;; SRFI 97 says that the first identifier after the colon-n
           ;; is used for the libraries name, so it must be ignored.
           #`(library (srfi #,srfi-n rest ... (version ...))))))))
    
    ((library (name name* ... (version ...)))
     (and-map sym? #'(name name* ...))
     (resolve-interface (syntax->datum #'(name name* ...))
                        #:version (syntax->datum #'(version ...))))

    ((library (name name* ...))
     (and-map sym? #'(name name* ...))
     (resolve-r6rs-interface #'(library (name name* ... ()))))
    
    ((only import-set identifier ...)
     (and-map sym? #'(identifier ...))
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (iface (make-custom-interface mod)))
       (for-each (lambda (sym)
                   (module-add! iface sym
                                (or (module-local-variable mod sym)
                                    (error "no binding `~A' in module ~A"
                                           sym mod))))
                 (syntax->datum #'(identifier ...)))
       iface))
    
    ((except import-set identifier ...)
     (and-map sym? #'(identifier ...))
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (iface (make-custom-interface mod)))
       (module-for-each (lambda (sym var) (module-add! iface sym var)) mod)
       (for-each (lambda (sym)
                   (if (module-local-variable iface sym)
                       (module-remove! iface sym)
                       (error "no binding `~A' in module ~A" sym mod)))
                 (syntax->datum #'(identifier ...)))
       iface))

    ((prefix import-set identifier)
     (sym? #'identifier)
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (iface (make-custom-interface mod))
            (pre (syntax->datum #'identifier)))
       (module-for-each (lambda (sym var)
                          (module-add! iface (symbol-append pre sym) var))
                        mod)
       iface))

    ((rename import-set (from to) ...)
     (and (and-map sym? #'(from ...)) (and-map sym? #'(to ...)))
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (iface (make-custom-interface mod)))
       (module-for-each (lambda (sym var) (module-add! iface sym var)) mod)
       (let lp ((in (syntax->datum #'((from . to) ...))) (out '()))
         (cond
          ((null? in)
           (for-each
            (lambda (pair)
              (if (module-local-variable iface (car pair))
                  (error "duplicate binding for `~A' in module ~A"
                         (car pair) mod)
                  (module-add! iface (car pair) (cdr pair))))
            out)
           iface)
          (else
           (let ((var (or (module-local-variable mod (caar in))
                          (error "no binding `~A' in module ~A"
                                 (caar in) mod))))
             (module-remove! iface (caar in))
             (lp (cdr in) (acons (cdar in) var out))))))))
    
    ((name name* ... (version ...))
     (and-map sym? #'(name name* ...))
     (resolve-r6rs-interface #'(library (name name* ... (version ...)))))

    ((name name* ...) 
     (and-map sym? #'(name name* ...))
     (resolve-r6rs-interface #'(library (name name* ... ()))))))

(define-syntax library
  (lambda (stx)
    (define (compute-exports ifaces specs)
      (define (re-export? sym)
        (or-map (lambda (iface) (module-local-variable iface sym)) ifaces))
      (define (replace? sym)
        (module-local-variable the-scm-module sym))
      
      (let lp ((specs specs) (e '()) (r '()) (x '()))
        (syntax-case specs (rename)
          (() (values e r x))
          (((rename (from to) ...) . rest)
           (and (and-map identifier? #'(from ...))
                (and-map identifier? #'(to ...)))
           (let lp2 ((in #'((from . to) ...)) (e e) (r r) (x x))
             (syntax-case in ()
               (() (lp #'rest e r x))
               (((from . to) . in)
                (cond
                 ((re-export? (syntax->datum #'from))
                  (lp2 #'in e (cons #'(from . to) r) x))
                 ((replace? (syntax->datum #'from))
                  (lp2 #'in e r (cons #'(from . to) x)))
                 (else
                  (lp2 #'in (cons #'(from . to) e) r x)))))))
          ((id . rest)
           (identifier? #'id)
           (let ((sym (syntax->datum #'id)))
             (cond
              ((re-export? sym)
               (lp #'rest e (cons #'id r) x))
              ((replace? sym)
               (lp #'rest e r (cons #'id x)))
              (else
               (lp #'rest (cons #'id e) r x))))))))

    (syntax-case stx (export import)
      ((_ (name name* ...)
          (export espec ...)
          (import ispec ...)
          body ...)
       (and-map identifier? #'(name name* ...))
       ;; Add () as the version.
       #'(library (name name* ... ())
           (export espec ...)
           (import ispec ...)
           body ...))

      ((_ (name name* ... (version ...))
          (export espec ...)
          (import ispec ...)
	  body ...)
       (and-map identifier? #'(name name* ...))
       (call-with-values
           (lambda ()
             (compute-exports 
              (map (lambda (im)
                     (syntax-case im (for)
                       ((for import-set import-level ...)
                        (resolve-r6rs-interface #'import-set))
                       (import-set (resolve-r6rs-interface #'import-set))))
                   #'(ispec ...))
              #'(espec ...)))
         (lambda (exports re-exports replacements)
           (with-syntax (((e ...) exports)
                         ((r ...) re-exports)
                         ((x ...) replacements))
             ;; It would be nice to push the module that was current before the
             ;; definition, and pop it after the library definition, but I
             ;; actually can't see a way to do that. Helper procedures perhaps,
             ;; around a fluid that is rebound in save-module-excursion? Patches
             ;; welcome!
             #'(begin
                 (define-module (name name* ...)
                   #:pure
                   #:version (version ...))
                 (import ispec)
                 ...
                 (export e ...)
                 (re-export r ...)
                 (export! x ...)
                 (@@ @@ (name name* ...) body)
                 ...))))))))
    
(define-syntax import
  (lambda (stx)
    (define (strip-for import-set)
      (syntax-case import-set (for)
        ((for import-set import-level ...)
         #'import-set)
        (import-set
         #'import-set)))
    (syntax-case stx ()
      ((_ import-set ...)
       (with-syntax (((library-reference ...) (map strip-for #'(import-set ...))))
         #'(eval-when (expand load eval)
             (let ((iface (resolve-r6rs-interface 'library-reference)))
               (call-with-deferred-observers
                 (lambda ()
                   (module-use-interfaces! (current-module) (list iface)))))
             ...
             (if #f #f)))))))
