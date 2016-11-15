;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2010 Free Software Foundation, Inc.
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

(define-module (system vm coverage)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (with-code-coverage
            coverage-data?
            instrumented-source-files
            instrumented/executed-lines
            line-execution-counts
            procedure-execution-count
            coverage-data->lcov))

;;; Author: Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides support to gather code coverage data by instrumenting
;;; the VM.
;;;
;;; Code:


;;;
;;; Gathering coverage data.
;;;

(define (hashq-proc proc n)
  ;; Return the hash of PROC's objcode.
  (hashq (program-objcode proc) n))

(define (assq-proc proc alist)
  ;; Instead of really looking for PROC in ALIST, look for the objcode of PROC.
  ;; IOW the alist is indexed by procedures, not objcodes, but those procedures
  ;; are taken as an arbitrary representative of all the procedures (closures)
  ;; sharing that objcode.  This can significantly reduce memory consumption.
  (let ((code (program-objcode proc)))
    (find (lambda (pair)
            (eq? code (program-objcode (car pair))))
          alist)))

(define (with-code-coverage vm thunk)
  "Run THUNK, a zero-argument procedure, using VM; instrument VM to collect code
coverage data.  Return code coverage data and the values returned by THUNK."

  (define procedure->ip-counts
    ;; Mapping from procedures to hash tables; said hash tables map instruction
    ;; pointers to the number of times they were executed.
    (make-hash-table 500))

  (define (collect! frame)
    ;; Update PROCEDURE->IP-COUNTS with info from FRAME.
    (let* ((proc       (frame-procedure frame))
           (ip         (frame-instruction-pointer frame))
           (proc-entry (hashx-create-handle! hashq-proc assq-proc
                                             procedure->ip-counts proc #f)))
      (let loop ()
        (define ip-counts (cdr proc-entry))
        (if ip-counts
            (let ((ip-entry (hashv-create-handle! ip-counts ip 0)))
              (set-cdr! ip-entry (+ (cdr ip-entry) 1)))
            (begin
              (set-cdr! proc-entry (make-hash-table))
              (loop))))))

  ;; FIXME: It's unclear what the dynamic-wind is for, given that if the
  ;; VM is different from the current one, continuations will not be
  ;; resumable.
  (call-with-values (lambda ()
                      (let ((level   (vm-trace-level vm))
                            (hook    (vm-next-hook vm)))
                        (dynamic-wind
                          (lambda ()
                            (set-vm-trace-level! vm (+ level 1))
                            (add-hook! hook collect!))
                          (lambda ()
                            (call-with-vm vm thunk))
                          (lambda ()
                            (set-vm-trace-level! vm level)
                            (remove-hook! hook collect!)))))
    (lambda args
      (apply values (make-coverage-data procedure->ip-counts) args))))


;;;
;;; Coverage data summary.
;;;

(define-record-type <coverage-data>
  (%make-coverage-data procedure->ip-counts
                       procedure->sources
                       file->procedures
                       file->line-counts)
  coverage-data?

  ;; Mapping from procedures to hash tables; said hash tables map instruction
  ;; pointers to the number of times they were executed.
  (procedure->ip-counts data-procedure->ip-counts)

  ;; Mapping from procedures to the result of `program-sources'.
  (procedure->sources   data-procedure->sources)

  ;; Mapping from source file names to lists of procedures defined in the file.
  (file->procedures     data-file->procedures)

  ;; Mapping from file names to hash tables, which in turn map from line numbers
  ;; to execution counts.
  (file->line-counts    data-file->line-counts))


(define (make-coverage-data procedure->ip-counts)
  ;; Return a `coverage-data' object based on the coverage data available in
  ;; PROCEDURE->IP-COUNTS.  Precompute the other hash tables that make up
  ;; `coverage-data' objects.
  (let* ((procedure->sources (make-hash-table 500))
         (file->procedures   (make-hash-table 100))
         (file->line-counts  (make-hash-table 100))
         (data               (%make-coverage-data procedure->ip-counts
                                                  procedure->sources
                                                  file->procedures
                                                  file->line-counts)))
    (define (increment-execution-count! file line count)
      ;; Make the execution count of FILE:LINE the maximum of its current value
      ;; and COUNT.  This is so that LINE's execution count is correct when
      ;; several instruction pointers map to LINE.
      (let ((file-entry (hash-create-handle! file->line-counts file #f)))
        (if (not (cdr file-entry))
            (set-cdr! file-entry (make-hash-table 500)))
        (let ((line-entry (hashv-create-handle! (cdr file-entry) line 0)))
          (set-cdr! line-entry (max (cdr line-entry) count)))))

    ;; Update execution counts for procs that were executed.
    (hash-for-each (lambda (proc ip-counts)
                     (let* ((sources (program-sources* data proc))
                            (file    (and (pair? sources)
                                          (source:file (car sources)))))
                       (and file
                            (begin
                              ;; Add a zero count for all IPs in SOURCES and in
                              ;; the sources of procedures closed over by PROC.
                              (for-each
                               (lambda (source)
                                 (let ((file (source:file source))
                                       (line (source:line source)))
                                   (increment-execution-count! file line 0)))
                               (append-map (cut program-sources* data <>)
                                           (closed-over-procedures proc)))

                              ;; Add the actual execution count collected.
                              (hash-for-each
                               (lambda (ip count)
                                 (let ((line (closest-source-line sources ip)))
                                   (increment-execution-count! file line count)))
                               ip-counts)))))
                   procedure->ip-counts)

    ;; Set the execution count to zero for procedures loaded and not executed.
    ;; FIXME: Traversing thousands of procedures here is inefficient.
    (for-each (lambda (proc)
                (and (not (hashq-ref procedure->sources proc))
                     (for-each (lambda (proc)
                                 (let* ((sources (program-sources* data proc))
                                        (file    (and (pair? sources)
                                                      (source:file (car sources)))))
                                   (and file
                                        (for-each
                                         (lambda (ip)
                                           (let ((line (closest-source-line sources ip)))
                                             (increment-execution-count! file line 0)))
                                         (map source:addr sources)))))
                               (closed-over-procedures proc))))
              (append-map module-procedures (loaded-modules)))

    data))

(define (procedure-execution-count data proc)
  "Return the number of times PROC's code was executed, according to DATA, or #f
if PROC was not executed.  When PROC is a closure, the number of times its code
was executed is returned, not the number of times this code associated with this
particular closure was executed."
  (let ((sources (program-sources* data proc)))
    (and (pair? sources)
         (and=> (hashx-ref hashq-proc assq-proc
                           (data-procedure->ip-counts data) proc)
                (lambda (ip-counts)
                  ;; FIXME: broken with lambda*
                  (let ((entry-ip (source:addr (car sources))))
                    (hashv-ref ip-counts entry-ip 0)))))))

(define (program-sources* data proc)
  ;; A memoizing version of `program-sources'.
  (or (hashq-ref (data-procedure->sources data) proc)
      (and (program? proc)
           (let ((sources (program-sources proc))
                 (p->s    (data-procedure->sources data))
                 (f->p    (data-file->procedures data)))
             (if (pair? sources)
                 (let* ((file  (source:file (car sources)))
                        (entry (hash-create-handle! f->p file '())))
                   (hashq-set! p->s proc sources)
                   (set-cdr! entry (cons proc (cdr entry)))
                   sources)
                 sources)))))

(define (file-procedures data file)
  ;; Return the list of globally bound procedures defined in FILE.
  (hash-ref (data-file->procedures data) file '()))

(define (instrumented/executed-lines data file)
  "Return the number of instrumented and the number of executed source lines in
FILE according to DATA."
  (define instr+exec
    (and=> (hash-ref (data-file->line-counts data) file)
           (lambda (line-counts)
             (hash-fold (lambda (line count instr+exec)
                          (let ((instr (car instr+exec))
                                (exec  (cdr instr+exec)))
                            (cons (+ 1 instr)
                                  (if (> count 0)
                                      (+ 1 exec)
                                      exec))))
                        '(0 . 0)
                        line-counts))))

  (values (car instr+exec) (cdr instr+exec)))

(define (line-execution-counts data file)
  "Return a list of line number/execution count pairs for FILE, or #f if FILE
is not among the files covered by DATA."
  (and=> (hash-ref (data-file->line-counts data) file)
         (lambda (line-counts)
           (hash-fold alist-cons '() line-counts))))

(define (instrumented-source-files data)
  "Return the list of `instrumented' source files, i.e., source files whose code
was loaded at the time DATA was collected."
  (hash-fold (lambda (file counts files)
               (cons file files))
             '()
             (data-file->line-counts data)))


;;;
;;; Helpers.
;;;

(define (loaded-modules)
  ;; Return the list of all the modules currently loaded.
  (define seen (make-hash-table))

  (let loop ((modules (module-submodules (resolve-module '() #f)))
             (result  '()))
    (hash-fold (lambda (name module result)
                 (if (hashq-ref seen module)
                     result
                     (begin
                       (hashq-set! seen module #t)
                       (loop (module-submodules module)
                             (cons module result)))))
               result
               modules)))

(define (module-procedures module)
  ;; Return the list of procedures bound globally in MODULE.
  (hash-fold (lambda (binding var result)
               (if (variable-bound? var)
                   (let ((value (variable-ref var)))
                     (if (procedure? value)
                         (cons value result)
                         result))
                   result))
             '()
             (module-obarray module)))

(define (closest-source-line sources ip)
  ;; Given SOURCES, as returned by `program-sources' for a given procedure,
  ;; return the source line of code that is the closest to IP.  This is similar
  ;; to what `program-source' does.
  (let loop ((sources sources)
             (line    (and (pair? sources) (source:line (car sources)))))
    (if (null? sources)
        line
        (let ((source (car sources)))
          (if (> (source:addr source) ip)
              line
              (loop (cdr sources) (source:line source)))))))

(define (closed-over-procedures proc)
  ;; Return the list of procedures PROC closes over, PROC included.
  (let loop ((proc   proc)
             (result '()))
    (if (and (program? proc) (not (memq proc result)))
        (fold loop (cons proc result)
              (append (vector->list (or (program-objects proc) #()))
                      (program-free-variables proc)))
        result)))


;;;
;;; LCOV output.
;;;

(define* (coverage-data->lcov data port)
  "Traverse code coverage information DATA, as obtained with
`with-code-coverage', and write coverage information in the LCOV format to PORT.
The report will include all the modules loaded at the time coverage data was
gathered, even if their code was not executed."

  (define (dump-function proc)
    ;; Dump source location and basic coverage data for PROC.
    (and (program? proc)
         (let ((sources (program-sources* data proc)))
           (and (pair? sources)
                (let* ((line (source:line-for-user (car sources)))
                       (name (or (procedure-name proc)
                                 (format #f "anonymous-l~a" line))))
                  (format port "FN:~A,~A~%" line name)
                  (and=> (procedure-execution-count data proc)
                         (lambda (count)
                           (format port "FNDA:~A,~A~%" count name))))))))

  ;; Output per-file coverage data.
  (format port "TN:~%")
  (for-each (lambda (file)
              (let ((procs (file-procedures data file))
                    (path  (search-path %load-path file)))
                (if (string? path)
                    (begin
                      (format port "SF:~A~%" path)
                      (for-each dump-function procs)
                      (for-each (lambda (line+count)
                                  (let ((line  (car line+count))
                                        (count (cdr line+count)))
                                    (format port "DA:~A,~A~%"
                                            (+ 1 line) count)))
                                (line-execution-counts data file))
                      (let-values (((instr exec)
                                    (instrumented/executed-lines data file)))
                        (format port "LH: ~A~%" exec)
                        (format port "LF: ~A~%" instr))
                      (format port "end_of_record~%"))
                    (begin
                      (format (current-error-port)
                              "skipping unknown source file: ~a~%"
                              file)))))
            (instrumented-source-files data)))
