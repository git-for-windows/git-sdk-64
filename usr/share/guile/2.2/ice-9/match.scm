;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2010, 2011, 2012 Free Software Foundation, Inc.
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

(define-module (ice-9 match)
  #:export (match
            match-lambda
            match-lambda*
            match-let
            match-let*
            match-letrec))

(define (error _ . args)
  ;; Error procedure for run-time "no matching pattern" errors.
  (apply throw 'match-error "match" args))

;; Support for record matching.

(define-syntax slot-ref
  (syntax-rules ()
    ((_ rtd rec n)
     (struct-ref rec n))))

(define-syntax slot-set!
  (syntax-rules ()
    ((_ rtd rec n value)
     (struct-set! rec n value))))

(define-syntax is-a?
  (syntax-rules ()
    ((_ rec rtd)
     (and (struct? rec)
          (eq? (struct-vtable rec) rtd)))))

;; Compared to Andrew K. Wright's `match', this one lacks `match-define',
;; `match:error-control', `match:set-error-control', `match:error',
;; `match:set-error', and all structure-related procedures.  Also,
;; `match' doesn't support clauses of the form `(pat => exp)'.

;; Unmodified public domain code by Alex Shinn retrieved from
;; the Chibi-Scheme repository, commit 1206:acd808700e91.
;;
;; Note: Make sure to update `match.test.upstream' when updating this
;; file.
(include-from-path "ice-9/match.upstream.scm")
