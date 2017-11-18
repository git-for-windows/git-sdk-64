;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2010, 2013 Free Software Foundation, Inc.
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
  #:use-module (system vm debug)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
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

(define (with-code-coverage thunk)
  "Run THUNK, a zero-argument procedure, while instrumenting Guile's VM to
collect code coverage data.  Return code coverage data and the values returned
by THUNK."

  (define ip-counts
    ;; A table mapping instruction pointers to the number of times they were
    ;; executed.
    (make-hash-table 5000))

  (define (collect! frame)
    ;; Update IP-COUNTS with info from FRAME.
    (let* ((ip (frame-instruction-pointer frame))
           (ip-entry (hashv-create-handle! ip-counts ip 0)))
      (set-cdr! ip-entry (+ (cdr ip-entry) 1))))

  ;; FIXME: It's unclear what the dynamic-wind is for, given that if the
  ;; VM is different from the current one, continuations will not be
  ;; resumable.
  (call-with-values (lambda ()
                      (let ((level   (vm-trace-level))
                            (hook    (vm-next-hook)))
                        (dynamic-wind
                          (lambda ()
                            (set-vm-trace-level! (+ level 1))
                            (add-hook! hook collect!))
                          (lambda ()
                            (call-with-vm thunk))
                          (lambda ()
                            (set-vm-trace-level! level)
                            (remove-hook! hook collect!)))))
    (lambda args
      (apply values (make-coverage-data ip-counts) args))))




;;;
;;; Source chunks.
;;;

(define-record-type <source-chunk>
  (make-source-chunk base length sources)
  source-chunk?
  (base source-chunk-base)
  (length source-chunk-length)
  (sources source-chunk-sources))

(set-record-type-printer!
 <source-chunk>
 (lambda (obj port)
   (format port "<source-chunk #x~x-#x~x>"
           (source-chunk-base obj)
           (+ (source-chunk-base obj) (source-chunk-length obj)))))

(define (compute-source-chunk ctx)
  "Build a sorted vector of source information for a given debugging
context (ELF image).  The return value is a @code{<source-chunk>}, which also
records the address range to which the source information applies."
  (make-source-chunk
   (debug-context-base ctx)
   (debug-context-length ctx)
   ;; The source locations are sorted already, but collected in reverse order.
   (list->vector (reverse! (fold-source-locations cons '() ctx)))))

(define (all-source-information)
  "Build and return a vector of source information corresponding to all
loaded code.  The vector will be sorted by ascending address order."
  (sort! (list->vector (fold-all-debug-contexts
                        (lambda (ctx seed)
                          (cons (compute-source-chunk ctx) seed))
                        '()))
         (lambda (x y)
           (< (source-chunk-base x) (source-chunk-base y)))))


;;;
;;; Coverage data summary.
;;;

(define-record-type <coverage-data>
  (%make-coverage-data ip-counts
                       sources
                       file->procedures
                       file->line-counts)
  coverage-data?

  ;; Mapping from instruction pointers to the number of times they were
  ;; executed, as a sorted vector of IP-count pairs.
  (ip-counts data-ip-counts)

  ;; Complete source census at the time the coverage analysis was run, as a
  ;; sorted vector of <source-chunk> values.
  (sources data-sources)

  ;; Mapping from source file names to lists of procedures defined in the file.
  ;; FIXME.
  (file->procedures     data-file->procedures)

  ;; Mapping from file names to hash tables, which in turn map from line numbers
  ;; to execution counts.
  (file->line-counts    data-file->line-counts))

(set-record-type-printer!
 <coverage-data>
 (lambda (obj port)
   (format port "<coverage-data ~x>" (object-address obj))))

(define (make-coverage-data ip-counts)
  ;; Return a `coverage-data' object based on the coverage data available in
  ;; IP-COUNTS.  Precompute the other hash tables that make up `coverage-data'
  ;; objects.
  (let* ((all-sources (all-source-information))
         (all-counts (sort! (list->vector (hash-fold acons '() ip-counts))
                            (lambda (x y)
                              (< (car x) (car y)))))
         (file->procedures   (make-hash-table 100))
         (file->line-counts  (make-hash-table 100))
         (data               (%make-coverage-data all-counts
                                                  all-sources
                                                  file->procedures
                                                  file->line-counts)))

    (define (observe-execution-count! file line count)
      ;; Make the execution count of FILE:LINE the maximum of its current value
      ;; and COUNT.  This is so that LINE's execution count is correct when
      ;; several instruction pointers map to LINE.
      (when file
        (let ((file-entry (hash-create-handle! file->line-counts file #f)))
          (if (not (cdr file-entry))
              (set-cdr! file-entry (make-hash-table 500)))
          (let ((line-entry (hashv-create-handle! (cdr file-entry) line 0)))
            (set-cdr! line-entry (max (cdr line-entry) count))))))

    ;; First, visit every known source location and mark it as instrumented but
    ;; unvisited.
    ;;
    ;; FIXME: This is not always necessary.  It's important to have the ability
    ;; to know when a source location is not reached, but sometimes all we need
    ;; to know is that a particular site *was* reached.  In that case we
    ;; wouldn't need to load up all the DWARF sections.  As it is, though, we
    ;; use the complete source census as part of the later phase.
    (let visit-chunk ((chunk-idx 0))
      (when (< chunk-idx (vector-length all-sources))
        (match (vector-ref all-sources chunk-idx)
          (($ <source-chunk> base chunk-length chunk-sources)
           (let visit-source ((source-idx 0))
             (when (< source-idx (vector-length chunk-sources))
               (let ((s (vector-ref chunk-sources source-idx)))
                 (observe-execution-count! (source-file s) (source-line s) 0)
                 (visit-source (1+ source-idx)))))))
        (visit-chunk (1+ chunk-idx))))

    ;; Then, visit the measured execution counts, walking the complete source
    ;; census at the same time.  This allows us to map observed addresses to
    ;; source locations.  Record observed execution counts.
    (let visit-chunk ((chunk-idx 0) (count-idx 0))
      (when (< chunk-idx (vector-length all-sources))
        (match (vector-ref all-sources chunk-idx)
          (($ <source-chunk> base chunk-length chunk-sources)
           (let visit-count ((count-idx count-idx) (source-idx 0) (source #f))
             (when (< count-idx (vector-length all-counts))
               (match (vector-ref all-counts count-idx)
                 ((ip . count)
                  (cond
                   ((< ip base)
                    ;; Address before chunk base; no corresponding source.
                    (visit-count (1+ count-idx) source-idx source))
                   ((< ip (+ base chunk-length))
                    ;; Address in chunk; count it.
                    (let visit-source ((source-idx source-idx) (source source))
                      (define (finish)
                        (when source
                          (observe-execution-count! (source-file source)
                                                    (source-line source)
                                                    count))
                        (visit-count (1+ count-idx) source-idx source))
                      (cond
                       ((< source-idx (vector-length chunk-sources))
                        (let ((source* (vector-ref chunk-sources source-idx)))
                          (if (<= (source-pre-pc source*) ip)
                              (visit-source (1+ source-idx) source*)
                              (finish))))
                       (else
                        (finish)))))
                   (else
                    ;; Address past chunk; fetch the next chunk.
                    (visit-chunk (1+ chunk-idx) count-idx)))))))))))

    data))

(define (procedure-execution-count data proc)
  "Return the number of times PROC's code was executed, according to DATA.  When
PROC is a closure, the number of times its code was executed is returned, not
the number of times this code associated with this particular closure was
executed."
  (define (binary-search v key val)
    (let lp ((start 0) (end (vector-length v)))
      (and (not (eqv? start end))
           (let* ((idx (floor/ (+ start end) 2))
                  (elt (vector-ref v idx))
                  (val* (key elt)))
             (cond
              ((< val val*)
               (lp start idx))
              ((< val* val)
               (lp (1+ idx) end))
              (else elt))))))
  (and (program? proc)
       (match (binary-search (data-ip-counts data) car (program-code proc))
         (#f 0)
         ((ip . code) code))))

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
;;; LCOV output.
;;;

(define* (coverage-data->lcov data port)
  "Traverse code coverage information DATA, as obtained with
`with-code-coverage', and write coverage information in the LCOV format to PORT.
The report will include all the modules loaded at the time coverage data was
gathered, even if their code was not executed."

  ;; FIXME: Re-enable this code, but using for-each-elf-symbol on each source
  ;; chunk.  Use that to build a map of file -> proc-addr + line + name.  Then
  ;; use something like procedure-execution-count to get the execution count.
  #;
  (define (dump-function proc)
    ;; Dump source location and basic coverage data for PROC.
    (and (or (program? proc))
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
              (let ((path (search-path %load-path file)))
                (if (string? path)
                    (begin
                      (format port "SF:~A~%" path)
                      #;
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
