;;;; module/srfi/srfi-64/automake.scm --- automake test driver compatible runner
;;;; Copyright (C) 2025 Free Software Foundation, Inc.
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.LESSER.
;;;; If not, write to the Free Software Foundation, Inc., 51 Franklin
;;;; Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; This provides an automake parallel test harness compatible SRFI-64
;;; test runner, and a check-guile compatible command line
;;; interface. For now, this is only intended to support check-guile and
;;; no public APIs have been settled, so there are no exports.
;;;
;;; Code:

(define-module (srfi srfi-64 automake)
  #:declarative? #f
  #:use-module ((ice-9 getopt-long) #:select (getopt-long option-ref))
  #:use-module ((srfi srfi-64)
                #:select (test-on-bad-count-simple
                          test-on-bad-end-name-simple
                          test-on-group-begin-simple
                          test-on-group-end-simple
                          test-on-test-end-simple
                          test-result-kind
                          test-runner-current
                          test-runner-fail-count
                          test-runner-group-path
                          test-runner-group-stack
                          test-runner-test-name
                          test-runner-null
                          test-runner-on-bad-count!
                          test-runner-on-bad-end-name!
                          test-runner-on-final!
                          test-runner-on-group-begin!
                          test-runner-on-group-end!
                          test-runner-on-test-begin!
                          test-runner-on-test-end!
                          test-runner-pass-count
                          test-runner-reset
                          test-runner-skip-count
                          test-runner-xfail-count
                          test-runner-xpass-count)))

(define (show port . args)
  (for-each (lambda (x) (display x port)) args))

(define (write->str x)
  (call-with-output-string (lambda (port) (write x port))))

(define (on-bad-count-callback log)
  (define (automake-srfi-64-on-bad-count runner actual-count expected-count)
    (with-output-to-port log
      (λ () (test-on-bad-count-simple runner actual-count expected-count))))
  automake-srfi-64-on-bad-count)

(define (on-bad-end-name-callback log)
  (define (automake-srfi-64-on-bad-end-name runner begin-name end-name)
    (with-output-to-port log
      (λ () (test-on-bad-end-name-simple runner begin-name end-name))))
  automake-srfi-64-on-bad-end-name)

(define (on-final-callback summary log trs)
  (define (automake-srfi-64-on-final runner)
    ;; Match guile-test output (currently omits fields srfi-64 doesn't
    ;; support).
    (let* ((pass (test-runner-pass-count runner))
           (fail (test-runner-fail-count runner))
           (xpass (test-runner-xpass-count runner))
           (xfail (test-runner-xfail-count runner))
           (skip (test-runner-skip-count runner))
           (report (list
                    "\n"
                    "Totals for this test run:\n"
                    "passes:                 " pass "\n"
                    "failures:               " fail "\n"
                    "unexpected passes:      " xpass "\n"
                    "expected failures:      " xfail "\n"
                    "untested test cases:    " skip "\n"
                    "\n")))
      (when trs
        (when (and (zero? pass) (zero? fail) (zero? xpass) (zero? xfail))
          (display ":recheck: no\n" trs))
        (display ":test-global-result: " trs)
        (for-each (λ (name count) (show trs " " name "=" count))
                  ;; Match test-suite-lib names
                  '("PASS" "FAIL" "UPASS" "XFAIL" "UNTESTED")
                  (list pass fail xpass xfail skip))
        (newline trs))
      (apply show log report)
      (unless trs
        (apply show summary report))))
  automake-srfi-64-on-final)

(define (on-group-begin-callback log)
  (define (automake-srfi-64-on-group-begin runner suite-name count)
    (with-output-to-port log
      (λ () (test-on-group-begin-simple runner suite-name count))))
  automake-srfi-64-on-group-begin)

(define (on-group-end-callback log)
  (define (automake-srfi-64-on-group-end runner)
    (with-output-to-port log (λ () (test-on-group-end-simple runner))))
  automake-srfi-64-on-group-end)

(define (automake-srfi-64-on-test-begin runner) #f)

(define (on-test-end-callback log trs)
  (define (automake-srfi-64-on-test-end runner)
    (with-output-to-port log (λ () (test-on-test-end-simple runner)))
    (let ((name (write->str (reverse
                             (cons (test-runner-test-name runner)
                                   (test-runner-group-stack runner))))))
      (case (test-result-kind runner)
        ((pass) (when trs (show trs ":test-result: PASS " name "\n")))
        ((fail)
         (show (current-output-port) "FAIL: " name "\n")
         (when trs (show trs ":test-result: FAIL " name "\n")))
        ((xfail)
         (show (current-output-port) "XFAIL: " name "\n")
         (when trs (show trs ":test-result: XFAIL " name "\n")))
        ((xpass)
         (show (current-output-port) "UPASS: " name "\n")
         (when trs (show trs ":test-result: XPASS " name "\n")))
        ((skip) (when trs (show trs ":test-result: SKIP " name "\n")))
        (else => (λ (k) (error "Unexpected SRFI-64 test result kind:" k))))))
  automake-srfi-64-on-test-end)

(define* (automake-srfi-64-runner #:key
                                  (log (current-error-port))
                                  (trs #f)
                                  (final? #t))
  "Return a SRFI-64 test runner that essentially matches the behavior of
guile-test (i.e. (test-suite lib) based tests).  Write the same summary
output to (current-output-port) unless there's a TRS port. Send
SRFI-64's default test runner output to the LOG, followed by the
summary, and write the automake test driver output to the TRS port. When
FINAL? is #f, ignore the final event (cf. test-runner-on-final),
alllowing the aggregation of results across multiple top-level groups.
"
  (let ((r (test-runner-null)))
    (test-runner-reset r)
    (test-runner-on-bad-count! r (on-bad-count-callback log))
    (test-runner-on-bad-end-name! r (on-bad-end-name-callback log))
    (test-runner-on-final! r
                           (if final?
                               (on-final-callback (current-output-port) log trs)
                               (λ args #f)))
    (test-runner-on-group-begin! r (on-group-begin-callback log))
    (test-runner-on-group-end! r (on-group-end-callback log))
    (test-runner-on-test-begin! r automake-srfi-64-on-test-begin)
    (test-runner-on-test-end! r (on-test-end-callback log trs))
    r))

(define (dir-content dir)
  (let ((d (opendir dir)))
    (let lp ((p (readdir d)) (result '()))
      (if (eof-object? p)
          (begin
            (closedir d)
            result)
          (lp (readdir d)
              (cons p result))))))

(define (run-tests tests log trs)
  (let ((runner (automake-srfi-64-runner #:log log #:trs trs #:final? #f)))
    (test-runner-current runner)
    (for-each (λ (test)
                (format (current-output-port) "Running ~a\n" (basename test))
                (load test))
              tests)
    ((on-final-callback (current-output-port) log trs) runner)
    (if (or (positive? (test-runner-fail-count runner))
            (positive? (test-runner-xpass-count runner))
            (positive? (test-runner-xfail-count runner)))
        (exit 2)
        (exit 0))))

(define-syntax-rule (with-final action body)
  (let ((entered? #f))
    (dynamic-wind
        (λ ()
          (when entered? (error "Not reentrant"))
          (set! entered? #t))
        (λ () body)
        (λ () action))))

(define (main args)
  "Provide a check-guile compatible command line interface, which means
matching guile-test's behavior where it's relevant, including support
for --log-file, --trs-file, and --test-suite."
  (let* ((opts (getopt-long args '((log-file (value #t))
                                   (test-suite (value #t) (required? #t))
                                   (trs-file (value #t)))))
         (test-suite (option-ref opts 'test-suite #f))
         (add-suite (λ (p) (string-append test-suite "/" p)))
         (tests (option-ref opts '() #f))
         (tests (if tests
                    (map add-suite tests)
                    (map add-suite
                         (filter (λ (s) (string-suffix? ".sr64" s))
                                 (dir-content test-suite)))))
         (log-file (option-ref opts 'log-file #f))
         (trs-file (option-ref opts 'trs-file #f))
         (log (if log-file (open-output-file log-file) (current-error-port))))
    (with-final
     (when log-file (close-port log))
     (let ((trs (and trs-file (open-output-file trs-file))))
       (with-final
        (when trs-file (close-port trs))
        (run-tests tests log trs))))))
