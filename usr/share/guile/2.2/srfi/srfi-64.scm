;;; srfi-64.scm -- SRFI 64 - A Scheme API for test suites.

;;      Copyright (C) 2014 Free Software Foundation, Inc.
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

(define-module (srfi srfi-64)
  #:export
  (test-begin
   test-end test-assert test-eqv test-eq test-equal
   test-approximate test-assert test-error test-apply test-with-runner
   test-match-nth test-match-all test-match-any test-match-name
   test-skip test-expect-fail test-read-eval-string
   test-runner-group-path test-group test-group-with-cleanup
   test-result-ref test-result-set! test-result-clear test-result-remove
   test-result-kind test-passed?
   test-log-to-file
   test-runner? test-runner-reset test-runner-null
   test-runner-simple test-runner-current test-runner-factory test-runner-get
   test-runner-create test-runner-test-name
   test-runner-pass-count test-runner-pass-count!
   test-runner-fail-count test-runner-fail-count!
   test-runner-xpass-count test-runner-xpass-count!
   test-runner-xfail-count test-runner-xfail-count!
   test-runner-skip-count test-runner-skip-count!
   test-runner-group-stack test-runner-group-stack!
   test-runner-on-test-begin test-runner-on-test-begin!
   test-runner-on-test-end test-runner-on-test-end!
   test-runner-on-group-begin test-runner-on-group-begin!
   test-runner-on-group-end test-runner-on-group-end!
   test-runner-on-final test-runner-on-final!
   test-runner-on-bad-count test-runner-on-bad-count!
   test-runner-on-bad-end-name test-runner-on-bad-end-name!
   test-result-alist test-result-alist!
   test-runner-aux-value test-runner-aux-value!
   test-on-group-begin-simple test-on-group-end-simple
   test-on-bad-count-simple test-on-bad-end-name-simple
   test-on-final-simple test-on-test-end-simple
   test-on-final-simple))

(cond-expand-provide (current-module) '(srfi-64))

(include-from-path "srfi/srfi-64/testing.scm")
