;;;; 	Copyright (C) 2009, 2010, 2013 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;


(define-module (system xref)
  #:use-module (system base compile)
  #:use-module (system vm program)
  #:use-module (system vm disassembler)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (*xref-ignored-modules*
            procedure-callees
            procedure-callers
            source-closures
            source-procedures))

;;;
;;; The cross-reference database: who calls whom.
;;;

(define (nested-procedures prog)
  (define (cons-uniq x y)
    (if (memq x y) y (cons x y)))
  (if (program? prog)
      (reverse
       (fold-program-code (lambda (elt out)
                            (match elt
                              (('static-ref dst proc)
                               (if (program? proc)
                                   (fold cons-uniq
                                         (cons proc out)
                                         (nested-procedures prog))
                                   out))
                              (_ out)))
                          (list prog)
                          prog))
      (list prog)))

(define (program-callee-rev-vars prog)
  (define (cons-uniq x y)
    (if (memq x y) y (cons x y)))
  (fold (lambda (prog out)
          (fold-program-code
           (lambda (elt out)
             (match elt
               (('toplevel-box dst var mod sym bound?)
                (let ((var (or var (and mod (module-variable mod sym)))))
                  (if var
                      (cons-uniq var out)
                      out)))
               (('module-box dst var public? mod-name sym bound?)
                (let ((var (or var
                               (module-variable (if public?
                                                    (resolve-interface mod-name)
                                                    (resolve-module mod-name))
                                                sym))))
                  (if var
                      (cons-uniq var out)
                      out)))
               (_ out)))
           out
           prog))
        '()
        (nested-procedures prog)))

(define (procedure-callee-rev-vars proc)
  (cond
   ((program? proc) (program-callee-rev-vars proc))
   (else '())))

(define (procedure-callees prog)
  "Evaluates to a list of the given program callees."
  (let lp ((in (procedure-callee-rev-vars prog)) (out '()))
    (cond ((null? in) out)
          ((variable-bound? (car in))
           (lp (cdr in) (cons (variable-ref (car in)) out)))
          (else (lp (cdr in) out)))))

;; var -> ((module-name caller ...) ...)
(define *callers-db* #f)
;; module-name -> (callee ...)
(define *module-callees-db* (make-hash-table))
;; (module-name ...)
(define *tainted-modules* '())

(define *xref-ignored-modules* '((value-history)))
(define (on-module-modified m)
  (let ((name (module-name m)))
    (if (and (not (member name *xref-ignored-modules*))
             (not (member name *tainted-modules*))
             (pair? name))
        (set! *tainted-modules* (cons name *tainted-modules*)))))

(define (add-caller callee caller mod-name)
  (let ((all-callers (hashq-ref *callers-db* callee)))
    (if (not all-callers)
        (hashq-set! *callers-db* callee `((,mod-name ,caller)))
        (let ((callers (assoc mod-name all-callers)))
          (if callers
              (if (not (member caller callers))
                  (set-cdr! callers (cons caller (cdr callers))))
              (hashq-set! *callers-db* callee
                          (cons `(,mod-name ,caller) all-callers)))))))

(define (forget-callers callee mod-name)
  (hashq-set! *callers-db* callee
             (assoc-remove! (hashq-ref *callers-db* callee '()) mod-name)))

(define (add-callees callees mod-name)
  (hash-set! *module-callees-db* mod-name
             (append callees (hash-ref *module-callees-db* mod-name '()))))

(define (untaint-modules)
  (define (untaint m)
    (for-each (lambda (callee) (forget-callers callee m))
              (hash-ref *module-callees-db* m '()))
    (ensure-callers-db m))
  (ensure-callers-db #f)
  (for-each untaint *tainted-modules*)
  (set! *tainted-modules* '()))

(define (ensure-callers-db mod-name)
  (let ((mod (and mod-name (resolve-module mod-name)))
        (visited #f))
    (define (visit-variable var mod-name)
      (if (variable-bound? var)
          (let ((x (variable-ref var)))
            (cond
             ((and visited (hashq-ref visited x)))
             ((procedure? x)
              (if visited (hashq-set! visited x #t))
              (let ((callees (filter variable-bound?
                                     (procedure-callee-rev-vars x))))
                (for-each (lambda (callee)
                            (add-caller callee x mod-name))
                          callees)
                (add-callees callees mod-name)))))))

    (define (visit-module mod)
      (if visited (hashq-set! visited mod #t))
      (if (not (memq on-module-modified (module-observers mod)))
          (module-observe mod on-module-modified))
      (let ((name (module-name mod)))
        (module-for-each (lambda (sym var)
                           (visit-variable var name))
                         mod)))

    (define (visit-submodules mod)
      (hash-for-each
       (lambda (name sub)
         (if (not (and visited (hashq-ref visited sub)))
             (begin
               (visit-module sub)
               (visit-submodules sub))))
       (module-submodules mod)))

    (cond ((and (not mod-name) (not *callers-db*))
           (set! *callers-db* (make-hash-table 1000))
           (set! visited (make-hash-table 1000))
           (visit-submodules (resolve-module '() #f)))
          (mod-name (visit-module mod)))))

(define (procedure-callers var)
  "Returns an association list, keyed by module name, of known callers
of the given procedure. The latter can specified directly as a
variable, a symbol (which gets resolved in the current module) or a
pair of the form (module-name . variable-name), "
  (let ((v (cond ((variable? var) var)
                 ((symbol? var) (module-variable (current-module) var))
                 (else
                  (match var
                    ((modname . sym)
                     (module-variable (resolve-module modname) sym))
                    (_
                     (error "expected a variable, symbol, or (modname . sym)" var)))))))
    (untaint-modules)
    (hashq-ref *callers-db* v '())))



;;;
;;; The source database: procedures defined at a given source location.
;;;

;; FIXME: refactor to share code with the xref database.

;; ((ip file line . col) ...)
(define (procedure-sources proc)
  (cond
   ((program? proc) (program-sources proc))
   (else '())))

;; file -> line -> (proc ...)
(define *closure-sources-db* #f)
;; file -> line -> (proc ...)
(define *sources-db* #f)
;; module-name -> proc -> sources
(define *module-sources-db* (make-hash-table))
;; (module-name ...)
(define *tainted-sources* '())

(define (on-source-modified m)
  (let ((name (module-name m)))
    (if (and (not (member name *xref-ignored-modules*))
             (not (member name *tainted-sources*))
             (pair? name))
        (set! *tainted-sources* (cons name *tainted-sources*)))))

(define (add-source proc file line db)
  (let ((file-table (or (hash-ref db file)
                        (let ((table (make-hash-table)))
                          (hash-set! db file table)
                          table))))
    (hashv-set! file-table
                line
                (cons proc (hashv-ref file-table line '())))))

(define (forget-source proc file line db)
  (let ((file-table (hash-ref db file)))
    (if file-table
        (let ((procs (delq proc (hashv-ref file-table line '()))))
          (if (pair? procs)
              (hashv-set! file-table line procs)
              (hashv-remove! file-table line))))))

(define (add-sources proc mod-name db)
  (let ((sources (procedure-sources proc)))
    (if (pair? sources)
        (begin
          ;; Add proc to *module-sources-db*, for book-keeping.
          (hashq-set! (or (hash-ref *module-sources-db* mod-name)
                          (let ((table (make-hash-table)))
                            (hash-set! *module-sources-db* mod-name table)
                            table))
                      proc
                      sources)
          ;; Actually add the source entries.
          (for-each (lambda (source)
                      (match source
                        ((ip file line . col)
                         (add-source proc file line db))
                        (_ (error "unexpected source format" source))))
                    sources)))
    ;; Add source entries for nested procedures.
    (for-each (lambda (obj)
                (add-sources obj mod-name *closure-sources-db*))
              (cdr (nested-procedures proc)))))

(define (forget-sources proc mod-name db)
  (let ((mod-table (hash-ref *module-sources-db* mod-name)))
    (when mod-table
      ;; Forget source entries.
      (for-each (lambda (source)
                  (match source
                    ((ip file line . col)
                     (forget-source proc file line db))
                    (_ (error "unexpected source format" source))))
                (hashq-ref mod-table proc '()))
      ;; Forget the proc.
      (hashq-remove! mod-table proc)
      ;; Forget source entries for nested procedures.
      (for-each (lambda (obj)
                  (forget-sources obj mod-name *closure-sources-db*))
                (cdr (nested-procedures proc))))))

(define (untaint-sources)
  (define (untaint m)
    (for-each (lambda (proc) (forget-sources proc m *sources-db*))
              (cond
               ((hash-ref *module-sources-db* m)
                => (lambda (table)
                     (hash-for-each (lambda (proc sources) proc) table)))
               (else '())))
    (ensure-sources-db m))
  (ensure-sources-db #f)
  (for-each untaint *tainted-sources*)
  (set! *tainted-sources* '()))

(define (ensure-sources-db mod-name)
  (define (visit-module mod)
    (if (not (memq on-source-modified (module-observers mod)))
        (module-observe mod on-source-modified))
    (let ((name (module-name mod)))
      (module-for-each
       (lambda (sym var)
         (if (variable-bound? var)
             (let ((x (variable-ref var)))
               (if (procedure? x)
                   (add-sources x name *sources-db*)))))
       mod)))

  (define visit-submodules
    (let ((visited #f))
      (lambda (mod)
        (if (not visited)
            (set! visited (make-hash-table)))
        (hash-for-each
         (lambda (name sub)
           (if (not (hashq-ref visited sub))
               (begin
                 (hashq-set! visited sub #t)
                 (visit-module sub)
                 (visit-submodules sub))))
         (module-submodules mod)))))

  (cond ((and (not mod-name) (not *sources-db*) (not *closure-sources-db*))
         (set! *closure-sources-db* (make-hash-table 1000))
         (set! *sources-db* (make-hash-table 1000))
         (visit-submodules (resolve-module '() #f)))
        (mod-name (visit-module (resolve-module mod-name)))))

(define (lines->ranges file-table)
  (let ((ranges (make-hash-table)))
    (hash-for-each
     (lambda (line procs)
       (for-each
        (lambda (proc)
          (cond
           ((hashq-ref ranges proc)
            => (lambda (pair)
                 (if (< line (car pair))
                     (set-car! pair line))
                 (if (> line (cdr pair))
                     (set-cdr! pair line))))
           (else
            (hashq-set! ranges proc (cons line line)))))
        procs))
     file-table)
    (sort! (hash-map->list cons ranges)
           (lambda (x y) (< (cadr x) (cadr y))))))

(define* (lookup-source-procedures canon-file line db)
  (let ((file-table (hash-ref db canon-file)))
    (let lp ((ranges (if file-table (lines->ranges file-table) '()))
             (procs '()))
      (cond
       ((null? ranges) (reverse procs))
       ((<= (cadar ranges) line (cddar ranges))
        (lp (cdr ranges) (cons (caar ranges) procs)))
       (else
        (lp (cdr ranges) procs))))))

(define* (source-closures file line #:key (canonicalization 'relative))
  (ensure-sources-db #f)
  (let* ((port (with-fluids ((%file-port-name-canonicalization canonicalization))
                 (false-if-exception (open-input-file file))))
         (file (if port (port-filename port) file)))
    (lookup-source-procedures file line *closure-sources-db*)))

(define* (source-procedures file line #:key (canonicalization 'relative))
  (ensure-sources-db #f)
  (let* ((port (with-fluids ((%file-port-name-canonicalization canonicalization))
                 (false-if-exception (open-input-file file))))
         (file (if port (port-filename port) file)))
    (lookup-source-procedures file line *sources-db*)))
