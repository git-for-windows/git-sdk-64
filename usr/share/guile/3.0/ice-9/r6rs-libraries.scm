;;; r6rs-libraries.scm --- Support for the R6RS `library' and `import' forms

;;      Copyright (C) 2010, 2019 Free Software Foundation, Inc.
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

;; Note that we can't use top-level define for helpers here as it will
;; pollute the (guile) module.

(define (resolve-r6rs-interface import-spec)
  (define (sym? stx)
    (symbol? (syntax->datum stx)))

  (define (n? stx)
    (let ((n (syntax->datum stx)))
      (and (exact-integer? n)
           (not (negative? n)))))

  (define (colon-n? x)
    (let ((sym (syntax->datum x)))
      (and (symbol? sym)
           (let ((str (symbol->string sym)))
             (and (string-prefix? ":" str)
                  (let ((num (string->number (substring str 1))))
                    (and (exact-integer? num)
                         (not (negative? num)))))))))

  (define (srfi-name? stx)
    (syntax-case stx (srfi)
      ((srfi n rest ...)
       (and (and-map sym? #'(rest ...))
            (or (n? #'n)
                (colon-n? #'n))))
      (_ #f)))

  (define (module-name? stx)
    (or (srfi-name? stx)
        (syntax-case stx ()
          ((name name* ...)
           (and-map sym? #'(name name* ...)))
          (_ #f))))

  (define (make-srfi-n context n)
    (datum->syntax
     context
     (string->symbol
      (string-append
       "srfi-"
       (let ((n (syntax->datum n)))
         (if (symbol? n)
             (substring (symbol->string n) 1)
             (number->string n)))))))

  (define (make-custom-interface mod)
    (let ((iface (make-module)))
      (set-module-kind! iface 'custom-interface)
      (set-module-name! iface (module-name mod))
      iface))
  (define (module-for-each/nonlocal f mod)
    (define (module-and-uses mod)
      (let lp ((in (list mod)) (out '()))
        (cond
         ((null? in) (reverse out))
         ((memq (car in) out) (lp (cdr in) out))
         (else (lp (append (module-uses (car in)) (cdr in))
                   (cons (car in) out))))))
    (for-each (lambda (mod)
                (module-for-each f mod))
              (module-and-uses mod)))

  (syntax-case import-spec (library only except prefix rename srfi)
    ;; (srfi :n ...) -> (srfi srfi-n ...)
    ;; (srfi n ...) -> (srfi srfi-n ...)
    ((library (srfi n rest ... (version ...)))
     (srfi-name? #'(srfi n rest ...))
     (let ((srfi-n (make-srfi-n #'srfi #'n)))
       (resolve-r6rs-interface
        (syntax-case #'(rest ...) ()
          (()
           #`(library (srfi #,srfi-n (version ...))))
          ((name rest ...)
           ;; SRFI 97 says that the first identifier after the `n'
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
                                (or (module-variable mod sym)
                                    (error (format
                                             #f
                                             "no binding `~A' in module ~A"
                                             sym mod))))
                   (when (hashq-ref (module-replacements mod) sym)
                     (hashq-set! (module-replacements iface) sym #t)))
                 (syntax->datum #'(identifier ...)))
       iface))
    
    ((except import-set identifier ...)
     (and-map sym? #'(identifier ...))
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (iface (make-custom-interface mod)))
       (module-for-each/nonlocal (lambda (sym var)
                                   (module-add! iface sym var))
                                 mod)
       (for-each (lambda (sym)
                   (unless (module-local-variable iface sym)
                     (error (format #f "no binding `~A' in module ~A" sym mod)))
                   (module-remove! iface sym))
                 (syntax->datum #'(identifier ...)))
       iface))

    ((prefix import-set identifier)
     (sym? #'identifier)
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (iface (make-custom-interface mod))
            (pre (syntax->datum #'identifier)))
       (module-for-each/nonlocal
        (lambda (sym var)
          (let ((sym* (symbol-append pre sym)))
            (module-add! iface sym* var)
            (when (hashq-ref (module-replacements mod) sym)
              (hashq-set! (module-replacements iface) sym* #t))))
        mod)
       iface))

    ((rename import-set (from to) ...)
     (and (and-map sym? #'(from ...)) (and-map sym? #'(to ...)))
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (replacements (module-replacements mod))
            (iface (make-custom-interface mod)))
       (module-for-each/nonlocal
        (lambda (sym var) (module-add! iface sym var))
        mod)
       (let lp ((in (syntax->datum #'((from . to) ...))) (out '()))
         (cond
          ((null? in)
           (for-each
            (lambda (v)
              (let ((to (vector-ref v 0))
                    (replace? (vector-ref v 1))
                    (var (vector-ref v 2)))
                (when (module-local-variable iface to)
                  (error (format
                           #f
                           "duplicate binding for `~A' in module ~A"
                           to
                           mod)))
                (module-add! iface to var)
                (when replace?
                  (hashq-set! replacements to #t))))
            out)
           iface)
          (else
           (let* ((from (caar in))
                  (to (cdar in))
                  (var (module-variable mod from))
                  (replace? (hashq-ref replacements from)))
             (unless var (error
                           (format #f "no binding `~A' in module ~A" from mod)))
             (module-remove! iface from)
             (hashq-remove! replacements from)
             (lp (cdr in) (cons (vector to replace? var) out))))))))
    
    ((name name* ... (version ...))
     (module-name? #'(name name* ...))
     (resolve-r6rs-interface #'(library (name name* ... (version ...)))))

    ((name name* ...)
     (module-name? #'(name name* ...))
     (resolve-r6rs-interface #'(library (name name* ... ()))))))

(define-syntax library
  (lambda (stx)
    (define (sym? stx)
      (symbol? (syntax->datum stx)))

    (define (n? stx)
      (let ((n (syntax->datum stx)))
        (and (exact-integer? n)
             (not (negative? n)))))

    (define (colon-n? x)
      (let ((sym (syntax->datum x)))
        (and (symbol? sym)
             (let ((str (symbol->string sym)))
               (and (string-prefix? ":" str)
                    (let ((num (string->number (substring str 1))))
                      (and (exact-integer? num)
                           (not (negative? num)))))))))

    (define (srfi-name? stx)
      (syntax-case stx (srfi)
        ((srfi n rest ...)
         (and (and-map sym? #'(rest ...))
              (or (n? #'n)
                  (colon-n? #'n))))
        (_ #f)))

    (define (module-name? stx)
      (or (srfi-name? stx)
          (syntax-case stx ()
            ((name name* ...)
             (and-map sym? #'(name name* ...)))
            (_ #f))))

    (define (make-srfi-n context n)
      (datum->syntax
       context
       (string->symbol
        (string-append
         "srfi-"
         (let ((n (syntax->datum n)))
           (if (symbol? n)
               (substring (symbol->string n) 1)
               (number->string n)))))))

    (define (compute-exports ifaces specs)
      (define (re-export? sym)
        (or-map (lambda (iface) (module-variable iface sym)) ifaces))
      (define (replace? sym)
        (module-variable the-scm-module sym))
      
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

    (syntax-case stx (export import srfi)
      ((_ (name name* ...)
          (export espec ...)
          (import ispec ...)
          body ...)
       (module-name? #'(name name* ...))
       ;; Add () as the version.
       #'(library (name name* ... ())
           (export espec ...)
           (import ispec ...)
           body ...))

      ((_ (srfi n rest ... (version ...))
          (export espec ...)
          (import ispec ...)
          body ...)
       (srfi-name? #'(srfi n rest ...))
       (let ((srfi-n (make-srfi-n #'srfi #'n)))
         #`(library (srfi #,srfi-n rest ... (version ...))
             (export espec ...)
             (import ispec ...)
             body ...)))

      ((_ (name name* ... (version ...))
          (export espec ...)
          (import ispec ...)
	  body ...)
       (module-name? #'(name name* ...))
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
