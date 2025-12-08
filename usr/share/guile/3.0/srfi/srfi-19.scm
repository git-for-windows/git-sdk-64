;;; srfi-19.scm --- Time/Date Library

;; Copyright (C) 2001-2003, 2005-2011, 2014, 2016-2018
;;   Free Software Foundation, Inc.
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

;;; Author: Rob Browning <rlb@cs.utexas.edu>
;;;         Originally from SRFI reference implementation by Will Fitzgerald.

;;; Commentary:

;; This module is fully documented in the Guile Reference Manual.

;;; Code:

;; FIXME: I haven't checked a decent amount of this code for potential
;; performance improvements, but I suspect that there may be some
;; substantial ones to be realized, esp. in the later "parsing" half
;; of the file, by rewriting the code with use of more Guile native
;; functions that do more work in a "chunk".
;;
;; FIXME: mkoeppe: Time zones are treated a little simplistic in
;; SRFI-19; they are only a numeric offset.  Thus, printing time zones
;; (LOCALE-PRINT-TIME-ZONE) can't be implemented sensibly.  The
;; functions taking an optional TZ-OFFSET should be extended to take a
;; symbolic time-zone (like "CET"); this string should be stored in
;; the DATE structure.

(define-module (srfi srfi-19)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-6)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-9)
  #:autoload   (ice-9 rdelim) (read-line)
  #:use-module (ice-9 i18n)
  #:replace (current-time)
  #:export (;; Constants
           time-duration
           time-monotonic
           time-process
           time-tai
           time-thread
           time-utc
           ;; Current time and clock resolution
           current-date
           current-julian-day
           current-modified-julian-day
           time-resolution
           ;; Time object and accessors
           make-time
           time?
           time-type
           time-nanosecond
           time-second
           set-time-type!
           set-time-nanosecond!
           set-time-second!
           copy-time
           ;; Time comparison procedures
           time<=?
           time<?
           time=?
           time>=?
           time>?
           ;; Time arithmetic procedures
           time-difference
           time-difference!
           add-duration
           add-duration!
           subtract-duration
           subtract-duration!
           ;; Date object and accessors
           make-date
           date?
           date-nanosecond
           date-second
           date-minute
           date-hour
           date-day
           date-month
           date-year
           date-zone-offset
           date-year-day
           date-week-day
           date-week-number
           ;; Time/Date/Julian Day/Modified Julian Day converters
           date->julian-day
           date->modified-julian-day
           date->time-monotonic
           date->time-tai
           date->time-utc
           julian-day->date
           julian-day->time-monotonic
           julian-day->time-tai
           julian-day->time-utc
           modified-julian-day->date
           modified-julian-day->time-monotonic
           modified-julian-day->time-tai
           modified-julian-day->time-utc
           time-monotonic->date
           time-monotonic->julian-day
           time-monotonic->modified-julian-day
           time-monotonic->time-tai
           time-monotonic->time-tai!
           time-monotonic->time-utc
           time-monotonic->time-utc!
           time-tai->date
           time-tai->julian-day
           time-tai->modified-julian-day
           time-tai->time-monotonic
           time-tai->time-monotonic!
           time-tai->time-utc
           time-tai->time-utc!
           time-utc->date
           time-utc->julian-day
           time-utc->modified-julian-day
           time-utc->time-monotonic
           time-utc->time-monotonic!
           time-utc->time-tai
           time-utc->time-tai!
           ;; Date to string/string to date converters.
           date->string
           string->date))

(cond-expand-provide (current-module) '(srfi-19))

(define time-tai 'time-tai)
(define time-utc 'time-utc)
(define time-monotonic 'time-monotonic)
(define time-thread 'time-thread)
(define time-process 'time-process)
(define time-duration 'time-duration)

;; FIXME: do we want to add gc time?
;; (define time-gc 'time-gc)

;;-- LOCALE dependent constants

;; See date->string
(define locale-date-time-format "~a ~b ~d ~H:~M:~S~z ~Y")
(define locale-short-date-format "~m/~d/~y")
(define locale-time-format "~H:~M:~S")
(define iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")

;;-- Miscellaneous Constants.
;;-- only the utc-epoch-in-jd might need changing if
;;   a different epoch is used.

(define nano 1000000000)           ; nanoseconds in a second
(define sid  86400)                ; seconds in a day
(define sihd 43200)                ; seconds in a half day
(define utc-epoch-in-jd 4881175/2) ; julian day number for 'the epoch'

;; FIXME: should this be something other than misc-error?
(define (time-error caller type value)
  (if value
      (throw 'misc-error caller "TIME-ERROR type ~A: ~S" (list type value) #f)
      (throw 'misc-error caller "TIME-ERROR type ~A" (list type) #f)))

;; A table of leap seconds
;; See ftp://maia.usno.navy.mil/ser7/tai-utc.dat
;; and update as necessary.
;; this procedures reads the file in the above
;; format and creates the leap second table
;; it also calls the almost standard, but not R5 procedures read-line
;; & open-input-string
;; ie (set! leap-second-table (read-tai-utc-date "tai-utc.dat"))

(define (read-tai-utc-data filename)
  (define (convert-jd jd)
    (* (- (inexact->exact jd) utc-epoch-in-jd) sid))
  (define (convert-sec sec)
    (inexact->exact sec))
  (let ((port (open-input-file filename))
        (table '()))
    (let loop ((line (read-line port)))
      (if (not (eof-object? line))
          (begin
            (let* ((data (read (open-input-string
                                (string-append "(" line ")"))))
                   (year (car data))
                   (jd   (cadddr (cdr data)))
                   (secs (cadddr (cdddr data))))
              (if (>= year 1972)
                  (set! table (cons
                               (cons (convert-jd jd) (convert-sec secs))
                               table)))
              (loop (read-line port))))))
    table))

;; each entry is (tai seconds since epoch . # seconds to subtract for utc)
;; note they go higher to lower, and end in 1972.
(define leap-second-table
  '((1483228800 . 37)
    (1435708800 . 36)
    (1341100800 . 35)
    (1230768000 . 34)
    (1136073600 . 33)
    (915148800 . 32)
    (867715200 . 31)
    (820454400 . 30)
    (773020800 . 29)
    (741484800 . 28)
    (709948800 . 27)
    (662688000 . 26)
    (631152000 . 25)
    (567993600 . 24)
    (489024000 . 23)
    (425865600 . 22)
    (394329600 . 21)
    (362793600 . 20)
    (315532800 . 19)
    (283996800 . 18)
    (252460800 . 17)
    (220924800 . 16)
    (189302400 . 15)
    (157766400 . 14)
    (126230400 . 13)
    (94694400  . 12)
    (78796800  . 11)
    (63072000  . 10)))

(define (read-leap-second-table filename)
  (set! leap-second-table (read-tai-utc-data filename)))


(define (utc->tai utc-seconds)
  (let loop ((table leap-second-table))
    (cond ((null? table)
           utc-seconds)
          ((>= utc-seconds (caar table))
           (+ utc-seconds (cdar table)))
          (else
           (loop (cdr table))))))

(define (tai->utc tai-seconds)
  (let loop ((table leap-second-table))
    (cond ((null? table)
           tai-seconds)
          ((>= tai-seconds (+ (caar table) (cdar table)))
           (- tai-seconds (cdar table)))
          (else
           (loop (cdr table))))))


;;; the TIME structure; creates the accessors, too.

(define-record-type time
  (make-time-unnormalized type nanosecond second)
  time?
  (type time-type set-time-type!)
  (nanosecond time-nanosecond set-time-nanosecond!)
  (second time-second set-time-second!))

(define (copy-time time)
  (make-time (time-type time) (time-nanosecond time) (time-second time)))

(define (split-real r)
  (if (integer? r)
      (values (inexact->exact r) 0)
      (let ((l (truncate r)))
        (values (inexact->exact l) (- r l)))))

(define (time-normalize! t)
  (let ((s  (time-second t))
        (ns (time-nanosecond t)))
    (when (>= (abs (time-nanosecond t))
              nano)
      (let ((s*  (+ s (inexact->exact
                       (truncate-quotient ns nano))))
            (ns* (truncate-remainder ns nano)))
        (set-time-second!     t s*)
        (set-time-nanosecond! t ns*)))
    (cond ((and (positive? s) (negative? ns))
           (set-time-second!     t (- s 1))
           (set-time-nanosecond! t (+ ns nano)))
          ((and (negative? s) (positive? ns))
           (set-time-second!     t (+ s 1))
           (set-time-nanosecond! t (- ns nano))))
    t))

(define (make-time type nanosecond second)
  (time-normalize! (make-time-unnormalized type nanosecond second)))

;;; current-time

;;; specific time getters.

(define (current-time-utc)
  ;; Resolution is microseconds.
  (let ((tod (gettimeofday)))
    (make-time time-utc (* (cdr tod) 1000) (car tod))))

(define (current-time-tai)
  ;; Resolution is microseconds.
  (let* ((tod (gettimeofday))
         (sec (car tod))
         (usec (cdr tod)))
    (make-time time-tai
               (* usec 1000)
               (utc->tai sec))))

;;(define (current-time-ms-time time-type proc)
;;  (let ((current-ms (proc)))
;;    (make-time time-type
;;               (quotient current-ms 10000)
;;       (* (remainder current-ms 1000) 10000))))

;; -- we define it to be the same as TAI.
;;    A different implementation of current-time-monotonic
;;    will require rewriting all of the time-monotonic converters,
;;    of course.

(define (current-time-monotonic)
  ;; Guile monotonic and TAI times are the same.
  (let ((tai (current-time-tai)))
    (make-time time-monotonic
               (time-nanosecond tai)
               (time-second tai))))

(define (current-time-thread)
  (time-error 'current-time-thread 'unsupported-clock-type 'time-thread))

(define ns-per-guile-tick (/ 1000000000 internal-time-units-per-second))

(define (current-time-process)
  (let ((run-time (get-internal-run-time)))
    (make-time
     time-process
     (* (remainder run-time internal-time-units-per-second)
        ns-per-guile-tick)
     (quotient run-time internal-time-units-per-second))))

;;(define (current-time-gc)
;;  (current-time-ms-time time-gc current-gc-milliseconds))

(define (current-time . clock-type)
  (let ((clock-type (if (null? clock-type) time-utc (car clock-type))))
    (cond
     ((eq? clock-type time-tai) (current-time-tai))
     ((eq? clock-type time-utc) (current-time-utc))
     ((eq? clock-type time-monotonic) (current-time-monotonic))
     ((eq? clock-type time-thread) (current-time-thread))
     ((eq? clock-type time-process) (current-time-process))
     ;;     ((eq? clock-type time-gc) (current-time-gc))
     (else (time-error 'current-time 'invalid-clock-type clock-type)))))

;; -- Time Resolution
;; This is the resolution of the clock in nanoseconds.
;; This will be implementation specific.

(define (time-resolution . clock-type)
  (let ((clock-type (if (null? clock-type) time-utc (car clock-type))))
    (case clock-type
      ((time-tai) 1000)
      ((time-utc) 1000)
      ((time-monotonic) 1000)
      ((time-process) ns-per-guile-tick)
      ;;     ((eq? clock-type time-thread) 1000)
      ;;     ((eq? clock-type time-gc) 10000)
      (else (time-error 'time-resolution 'invalid-clock-type clock-type)))))

;; -- Time comparisons
 
(define (time-compare-check t1 t2 caller)
  (unless (and (time? t1) (time? t2)
               (eq? (time-type t1) (time-type t2)))
    (time-error caller 'incompatible-time-types (cons t1 t2))))

(define (time=? t1 t2)
  ;; Arrange tests for speed and presume that t1 and t2 are actually times.
  ;; also presume it will be rare to check two times of different types.
  (time-compare-check t1 t2 'time=?)
  (and (= (time-second t1) (time-second t2))
       (= (time-nanosecond t1) (time-nanosecond t2))))

(define (time>? t1 t2)
  (time-compare-check t1 t2 'time>?)
  (or (> (time-second t1) (time-second t2))
      (and (= (time-second t1) (time-second t2))
           (> (time-nanosecond t1) (time-nanosecond t2)))))

(define (time<? t1 t2)
  (time-compare-check t1 t2 'time<?)
  (or (< (time-second t1) (time-second t2))
      (and (= (time-second t1) (time-second t2))
           (< (time-nanosecond t1) (time-nanosecond t2)))))

(define (time>=? t1 t2)
  (time-compare-check t1 t2 'time>=?)
  (or (> (time-second t1) (time-second t2))
      (and (= (time-second t1) (time-second t2))
           (>= (time-nanosecond t1) (time-nanosecond t2)))))

(define (time<=? t1 t2)
  (time-compare-check t1 t2 'time<=?)
  (or (< (time-second t1) (time-second t2))
      (and (= (time-second t1) (time-second t2))
           (<= (time-nanosecond t1) (time-nanosecond t2)))))

;; -- Time arithmetic

;; XXX In the following comparison procedures, the SRFI-19 reference
;; implementation raises an error in case of unequal time types.

(define (time-difference! time1 time2)
  (time-compare-check time1 time2 'time-difference!)
  (let ((sec-diff (- (time-second time1) (time-second time2)))
        (nsec-diff (- (time-nanosecond time1) (time-nanosecond time2))))
    (set-time-type! time1 time-duration)
    (set-time-second! time1 sec-diff)
    (set-time-nanosecond! time1 nsec-diff)
    (time-normalize! time1)))

(define (time-difference time1 time2)
  (let ((result (copy-time time1)))
    (time-difference! result time2)))

(define (add-duration! t duration)
  (if (not (eq? (time-type duration) time-duration))
      (time-error 'add-duration! 'not-duration duration)
      (let ((sec-plus (+ (time-second t) (time-second duration)))
            (nsec-plus (+ (time-nanosecond t) (time-nanosecond duration))))
        (set-time-second! t sec-plus)
        (set-time-nanosecond! t nsec-plus)
        (time-normalize! t))))

(define (add-duration t duration)
  (let ((result (copy-time t)))
    (add-duration! result duration)))

(define (subtract-duration! t duration)
  (if (not (eq? (time-type duration) time-duration))
      (time-error 'subtract-duration! 'not-duration duration)
      (let ((sec-minus  (- (time-second t) (time-second duration)))
            (nsec-minus (- (time-nanosecond t) (time-nanosecond duration))))
        (set-time-second! t sec-minus)
        (set-time-nanosecond! t nsec-minus)
        (time-normalize! t))))

(define (subtract-duration time1 duration)
  (let ((result (copy-time time1)))
    (subtract-duration! result duration)))

;; -- Converters between types.

(define (priv:time-tai->time-utc! time-in time-out caller)
  (if (not (eq? (time-type time-in) time-tai))
      (time-error caller 'incompatible-time-types time-in))
  (set-time-type! time-out time-utc)
  (set-time-nanosecond! time-out (time-nanosecond time-in))
  (set-time-second!     time-out (tai->utc (time-second time-in)))
  time-out)

(define (time-tai->time-utc time-in)
  (priv:time-tai->time-utc! time-in (make-time-unnormalized #f #f #f) 'time-tai->time-utc))


(define (time-tai->time-utc! time-in)
  (priv:time-tai->time-utc! time-in time-in 'time-tai->time-utc!))

(define (priv:time-utc->time-tai! time-in time-out caller)
  (if (not (eq? (time-type time-in) time-utc))
      (time-error caller 'incompatible-time-types time-in))
  (set-time-type! time-out time-tai)
  (set-time-nanosecond! time-out (time-nanosecond time-in))
  (set-time-second!     time-out (utc->tai (time-second time-in)))
  time-out)

(define (time-utc->time-tai time-in)
  (priv:time-utc->time-tai! time-in (make-time-unnormalized #f #f #f) 'time-utc->time-tai))

(define (time-utc->time-tai! time-in)
  (priv:time-utc->time-tai! time-in time-in 'time-utc->time-tai!))

;; -- these depend on time-monotonic having the same definition as time-tai!
(define (time-monotonic->time-utc time-in)
  (if (not (eq? (time-type time-in) time-monotonic))
      (time-error 'time-monotonic->time-utc
                  'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (set-time-type! ntime time-tai)
    (priv:time-tai->time-utc! ntime ntime 'time-monotonic->time-utc)))

(define (time-monotonic->time-utc! time-in)
  (if (not (eq? (time-type time-in) time-monotonic))
      (time-error 'time-monotonic->time-utc!
                  'incompatible-time-types time-in))
  (set-time-type! time-in time-tai)
  (priv:time-tai->time-utc! time-in time-in 'time-monotonic->time-utc))

(define (time-monotonic->time-tai time-in)
  (if (not (eq? (time-type time-in) time-monotonic))
      (time-error 'time-monotonic->time-tai
                  'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (set-time-type! ntime time-tai)
    ntime))

(define (time-monotonic->time-tai! time-in)
  (if (not (eq? (time-type time-in) time-monotonic))
      (time-error 'time-monotonic->time-tai!
                  'incompatible-time-types time-in))
  (set-time-type! time-in time-tai)
  time-in)

(define (time-utc->time-monotonic time-in)
  (if (not (eq? (time-type time-in) time-utc))
      (time-error 'time-utc->time-monotonic
                  'incompatible-time-types time-in))
  (let ((ntime (priv:time-utc->time-tai! time-in (make-time-unnormalized #f #f #f)
                                         'time-utc->time-monotonic)))
    (set-time-type! ntime time-monotonic)
    ntime))

(define (time-utc->time-monotonic! time-in)
  (if (not (eq? (time-type time-in) time-utc))
      (time-error 'time-utc->time-monotonic!
                  'incompatible-time-types time-in))
  (let ((ntime (priv:time-utc->time-tai! time-in time-in
                                         'time-utc->time-monotonic!)))
    (set-time-type! ntime time-monotonic)
    ntime))

(define (time-tai->time-monotonic time-in)
  (if (not (eq? (time-type time-in) time-tai))
      (time-error 'time-tai->time-monotonic
                  'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (set-time-type! ntime time-monotonic)
    ntime))

(define (time-tai->time-monotonic! time-in)
  (if (not (eq? (time-type time-in) time-tai))
      (time-error 'time-tai->time-monotonic!
                  'incompatible-time-types time-in))
  (set-time-type! time-in time-monotonic)
  time-in)

;; -- Date Structures

;; FIXME: to be really safe, perhaps we should normalize the
;; seconds/nanoseconds/minutes coming in to make-date...

(define-record-type date
  (make-date nanosecond second minute
             hour day month
             year
             zone-offset)
  date?
  (nanosecond date-nanosecond set-date-nanosecond!)
  (second date-second set-date-second!)
  (minute date-minute set-date-minute!)
  (hour date-hour set-date-hour!)
  (day date-day set-date-day!)
  (month date-month set-date-month!)
  (year date-year set-date-year!)
  (zone-offset date-zone-offset set-date-zone-offset!))

;; gives the julian day which starts at noon.
(define (encode-julian-day-number day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- (+ year 4800) a (if (negative? year) -1  0)))
         (m (- (+ month (* 12 a)) 3)))
    (+ day
       (quotient (+ (* 153 m) 2) 5)
       (* 365 y)
       (floor-quotient y 4)
       (- (floor-quotient y 100))
       (floor-quotient y 400)
       -32045)))

;; gives the seconds/date/month/year
(define (decode-julian-day-number jdn)
  (let* ((days (inexact->exact (floor jdn)))
         (a (+ days 32044))
         (b (floor-quotient (+ (* 4 a) 3) 146097))
         (c (- a (floor-quotient (* 146097 b) 4)))
         (d (floor-quotient (+ (* 4 c) 3) 1461))
         (e (- c (floor-quotient (* 1461 d) 4)))
         (m (floor-quotient (+ (* 5 e) 2) 153))
         (y (+ (* 100 b) d -4800 (quotient m 10))))
    (values ; seconds date month year
     (* (- jdn days) sid)
     (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
     (+ m 3 (* -12 (quotient m 10)))
     (if (>= 0 y) (- y 1) y))))

;; relies on the fact that we named our time zone accessor
;; differently from MzScheme's....
;; This should be written to be OS specific.

(define (local-tz-offset utc-time)
  ;; SRFI 19 uses seconds East, but 'tm:gmtoff' returns seconds West.
  (- (tm:gmtoff (localtime (time-second utc-time)))))

;; special thing -- ignores nanos
(define (time->julian-day-number seconds tz-offset)
  (+ (/ (+ seconds tz-offset sihd)
        sid)
     utc-epoch-in-jd))

(define (tai-before-leap-second? second)
  (any (lambda (x)
         (= second (+ (car x) (cdr x) -1)))
       leap-second-table))

(define* (time-utc->date time #:optional (tz-offset
                                          (local-tz-offset time)))
  (if (not (eq? (time-type time) time-utc))
      (time-error 'time-utc->date 'incompatible-time-types  time))
  (let* ((nanoseconds (+ (time-nanosecond time)
                         (* nano (time-second time))))
         (jdn (time->julian-day-number (floor-quotient nanoseconds nano)
                                       tz-offset)))
    (call-with-values (lambda () (decode-julian-day-number jdn))
      (lambda (secs date month year)
	;; secs is a real because jdn is a real in Guile;
	;; but it is conceptionally an integer.
        (let* ((int-secs (inexact->exact (round secs)))
               (hours    (quotient int-secs (* 60 60)))
               (rem      (remainder int-secs (* 60 60)))
               (minutes  (quotient rem 60))
               (seconds  (remainder rem 60)))
          (make-date (floor-remainder nanoseconds nano)
                     seconds
                     minutes
                     hours
                     date
                     month
                     year
                     tz-offset))))))

(define (time-tai->date time  . tz-offset)
  (if (not (eq? (time-type time) time-tai))
      (time-error 'time-tai->date 'incompatible-time-types  time))
  (if (tai-before-leap-second? (time-second time))
      ;; If it's *right* before the leap, we must handle this case to
      ;; avoid the information lost when converting to UTC.  We subtract
      ;; a second before conversion, and then effectively add it back
      ;; after conversion by setting the second field to 60.
      (let ((d (apply time-utc->date
                      (subtract-duration! (time-tai->time-utc time)
                                          (make-time time-duration 0 1))
                      tz-offset)))
        (set-date-second! d 60)
        d)
      (apply time-utc->date (time-tai->time-utc time) tz-offset)))

(define (time-monotonic->date time . tz-offset)
  (if (not (eq? (time-type time) time-monotonic))
      (time-error 'time-monotonic->date 'incompatible-time-types  time))
  (apply time-tai->date (time-monotonic->time-tai time) tz-offset))

(define (date->time-utc date)
  (let* ((jdays (- (encode-julian-day-number (date-day date)
                                             (date-month date)
                                             (date-year date))
		   utc-epoch-in-jd))
	 ;; jdays is an integer plus 1/2,
	 (jdays-1/2 (inexact->exact (- jdays 1/2))))
    (make-time
     time-utc
     (date-nanosecond date)
     (+ (* jdays-1/2 24 60 60)
        (* (date-hour date) 60 60)
        (* (date-minute date) 60)
        (date-second date)
	(- (date-zone-offset date))))))

(define (date->time-tai d)
  (if (= (date-second d) 60)
      (subtract-duration! (time-utc->time-tai! (date->time-utc d))
                          (make-time time-duration 0 1))
      (time-utc->time-tai! (date->time-utc d))))

(define (date->time-monotonic d)
  (if (= (date-second d) 60)
      (subtract-duration! (time-utc->time-monotonic! (date->time-utc d))
                          (make-time time-duration 0 1))
      (time-utc->time-monotonic! (date->time-utc d))))

(define (leap-year? year)
  (let ((y (if (negative? year) (+ year 1) year)))
    (and (zero? (modulo y 4))
         (or (not (zero? (modulo y 100)))
             (zero? (modulo y 400))))))

;; Map 1-based month number M to number of days in the year before the
;; start of month M (in a non-leap year).
(define month-assoc '((1 . 0)   (2 . 31)   (3 . 59)   (4 . 90)
		      (5 . 120) (6 . 151)  (7 . 181)  (8 . 212)
		      (9 . 243) (10 . 273) (11 . 304) (12 . 334)))

(define (year-day day month year)
  (let ((days-pr (assoc month month-assoc)))
    (if (not days-pr)
        (time-error 'date-year-day 'invalid-month-specification month))
    (if (and (leap-year? year) (> month 2))
        (+ day (cdr days-pr) 1)
        (+ day (cdr days-pr)))))

(define (date-year-day date)
  (year-day (date-day date) (date-month date) (date-year date)))

;; from calendar faq
(define (week-day day month year)
  (let* ((yy (if (negative? year) (+ year 1) year))
         (a (quotient (- 14 month) 12))
         (y (- yy a))
         (m (+ month (* 12 a) -2)))
    (modulo (+ day
               y
               (floor-quotient y 4)
               (- (floor-quotient y 100))
               (floor-quotient y 400)
               (floor-quotient (* 31 m) 12))
            7)))

(define (date-week-day date)
  (week-day (date-day date) (date-month date) (date-year date)))

(define (days-before-first-week date day-of-week-starting-week)
  (let* ((first-day (make-date 0 0 0 0
                               1
                               1
                               (date-year date)
                               #f))
         (fdweek-day (date-week-day first-day)))
    (modulo (- day-of-week-starting-week fdweek-day)
            7)))

;; The "-1" here is a fix for the reference implementation, to make a new
;; week start on the given day-of-week-starting-week.  date-year-day returns
;; a day starting from 1 for 1st Jan.
;;
(define (date-week-number date day-of-week-starting-week)
  (floor-quotient (- (date-year-day date)
                     1
                     (days-before-first-week  date day-of-week-starting-week))
                  7))

;;; Adapted from the reference implementation.
(define (date-week-number-iso date)
  "Return a ISO-8601 week number for the @var{date}."
  ;; The week with the year's first Thursday is week 01.
  (let* ((first-day-of-the-week (week-day 1 1 (date-year date)))
         (offset (if (> first-day-of-the-week 4) 0 1))
         ;; -2: decrement one day to compensate 1-origin of date-year-day,
         ;; and decrement one more day for Sunday belongs to the previous week.
         (w (+ (floor-quotient (+ (date-year-day date) first-day-of-the-week -2)
                               7)
               offset)))
    (cond ((zero? w)
           ;; date belongs to the last week of the previous year
           (date-week-number-iso (make-date 0 0 0 0 31 12
                                            (- (date-year date) 1) 0)))
          ((and (= w 53)
                (<= (week-day 1 1 (+ (date-year date) 1)) 4))
           ;; date belongs to the first week of the next year
           1)
          (else w))))

(define (current-date . tz-offset)
  (let ((time (current-time time-utc)))
    (time-utc->date
     time
     (if (null? tz-offset)
	 (local-tz-offset time)
	 (car tz-offset)))))

;; given a 'two digit' number, find the year within 50 years +/-
(define (natural-year n)
  (let* ((current-year (date-year (current-date)))
         (current-century (* (quotient current-year 100) 100)))
    (cond
     ((>= n 100) n)
     ((<  n 0) n)
     ((<=  (- (+ current-century n) current-year) 50) (+ current-century n))
     (else (+ (- current-century 100) n)))))

(define (date->julian-day date)
  (let ((nanosecond (date-nanosecond date))
        (second (date-second date))
        (minute (date-minute date))
        (hour (date-hour date))
        (day (date-day date))
        (month (date-month date))
        (year (date-year date))
        (offset (date-zone-offset date)))
    (+ (encode-julian-day-number day month year)
       (- 1/2)
       (+ (/ (+ (- offset)
                (* hour 60 60)
                (* minute 60)
                second
                (/ nanosecond nano))
             sid)))))

(define (date->modified-julian-day date)
  (- (date->julian-day date)
     4800001/2))

(define (time-utc->julian-day time)
  (if (not (eq? (time-type time) time-utc))
      (time-error 'time-utc->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (time-second time) (/ (time-nanosecond time) nano))
        sid)
     utc-epoch-in-jd))

(define (time-utc->modified-julian-day time)
  (- (time-utc->julian-day time)
     4800001/2))

(define (time-tai->julian-day time)
  (if (not (eq? (time-type time) time-tai))
      (time-error 'time-tai->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (tai->utc (time-second time))
           (/ (time-nanosecond time) nano))
        sid)
     utc-epoch-in-jd))

(define (time-tai->modified-julian-day time)
  (- (time-tai->julian-day time)
     4800001/2))

;; this is the same as time-tai->julian-day
(define (time-monotonic->julian-day time)
  (if (not (eq? (time-type time) time-monotonic))
      (time-error 'time-monotonic->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (tai->utc (time-second time))
           (/ (time-nanosecond time) nano))
        sid)
     utc-epoch-in-jd))

(define (time-monotonic->modified-julian-day time)
  (- (time-monotonic->julian-day time)
     4800001/2))

(define (julian-day->time-utc jdn)
  (let ((secs (* sid (- jdn utc-epoch-in-jd))))
    (receive (seconds parts)
	(split-real secs)
      (make-time time-utc
		 (* parts nano)
		 seconds))))

(define (julian-day->time-tai jdn)
  (time-utc->time-tai! (julian-day->time-utc jdn)))

(define (julian-day->time-monotonic jdn)
  (time-utc->time-monotonic! (julian-day->time-utc jdn)))

(define (julian-day->date jdn . tz-offset)
  (let* ((time (julian-day->time-utc jdn))
	 (offset (if (null? tz-offset)
		     (local-tz-offset time)
		     (car tz-offset))))
    (time-utc->date time offset)))

(define (modified-julian-day->date jdn . tz-offset)
  (apply julian-day->date (+ jdn 4800001/2)
	 tz-offset))

(define (modified-julian-day->time-utc jdn)
  (julian-day->time-utc (+ jdn 4800001/2)))

(define (modified-julian-day->time-tai jdn)
  (julian-day->time-tai (+ jdn 4800001/2)))

(define (modified-julian-day->time-monotonic jdn)
  (julian-day->time-monotonic (+ jdn 4800001/2)))

(define (current-julian-day)
  (time-utc->julian-day (current-time time-utc)))

(define (current-modified-julian-day)
  (time-utc->modified-julian-day (current-time time-utc)))

;; returns a string rep. of number N, of minimum LENGTH, padded with
;; character PAD-WITH. If PAD-WITH is #f, no padding is done, and it's
;; as if number->string was used.  if string is longer than or equal
;; in length to LENGTH, it's as if number->string was used.

(define (padding n pad-with length)
  (let* ((str (number->string n))
         (str-len (string-length str)))
    (if (or (>= str-len length)
            (not pad-with))
        str
        (string-append (make-string (- length str-len) pad-with) str))))

(define (last-n-digits i n)
  (abs (remainder i (expt 10 n))))

(define (locale-abbr-weekday n) (locale-day-short (+ 1 n)))
(define (locale-long-weekday n) (locale-day (+ 1 n)))
(define locale-abbr-month       locale-month-short)
(define locale-long-month       locale-month)

(define (date-reverse-lookup needle haystack-ref haystack-len
                                  same?)
  ;; Lookup NEEDLE (a string) using HAYSTACK-REF (a one argument procedure
  ;; that returns a string corresponding to the given index) by passing it
  ;; indices lower than HAYSTACK-LEN.
  (let loop ((index 1))
    (cond ((> index haystack-len) #f)
          ((same? needle (haystack-ref index))
           index)
          (else (loop (+ index 1))))))

(define (locale-abbr-weekday->index string)
  (date-reverse-lookup string locale-day-short 7 string=?))

(define (locale-long-weekday->index string)
  (date-reverse-lookup string locale-day 7 string=?))

(define (locale-abbr-month->index string)
  (date-reverse-lookup string locale-abbr-month  12 string=?))

(define (locale-long-month->index string)
  (date-reverse-lookup string locale-long-month  12 string=?))


;; FIXME: mkoeppe: Put a symbolic time zone in the date structs.
;; Print it here instead of the numerical offset if available.
(define (locale-print-time-zone date port)
  (tz-printer (date-zone-offset date) port))

(define (locale-am-string/pm hr)
  (if (> hr 11) (locale-pm-string) (locale-am-string)))

(define (tz-printer offset port)
  (cond
   ((= offset 0) (display "Z" port))
   ((negative? offset) (display "-" port))
   (else (display "+" port)))
  (if (not (= offset 0))
      (let ((hours   (abs (quotient offset (* 60 60))))
            (minutes (abs (quotient (remainder offset (* 60 60)) 60))))
        (display (padding hours #\0 2) port)
        (display (padding minutes #\0 2) port))))

;; A table of output formatting directives.
;; the first time is the format char.
;; the second is a procedure that takes the date, a padding character
;; (which might be #f), and the output port.
;;
(define directives
  (list
   (cons #\~ (lambda (date pad-with port)
               (display #\~ port)))
   (cons #\a (lambda (date pad-with port)
               (display (locale-abbr-weekday (date-week-day date))
                        port)))
   (cons #\A (lambda (date pad-with port)
               (display (locale-long-weekday (date-week-day date))
                        port)))
   (cons #\b (lambda (date pad-with port)
               (display (locale-abbr-month (date-month date))
                        port)))
   (cons #\B (lambda (date pad-with port)
               (display (locale-long-month (date-month date))
                        port)))
   (cons #\c (lambda (date pad-with port)
               (display (date->string date locale-date-time-format) port)))
   (cons #\d (lambda (date pad-with port)
               (display (padding (date-day date)
                                 #\0 2)
                        port)))
   (cons #\D (lambda (date pad-with port)
               (display (date->string date "~m/~d/~y") port)))
   (cons #\e (lambda (date pad-with port)
               (display (padding (date-day date)
                                 #\Space 2)
                        port)))
   (cons #\f (lambda (date pad-with port)
               (receive (s ns) (floor/ (+ (* (date-second date) nano)
                                          (date-nanosecond date))
                                       nano)
                 (display (number->string s) port)
                 (display (locale-decimal-point) port)
                 (let ((str (padding ns #\0 9)))
                   (display (substring str 0 1) port)
                   (display (string-trim-right str #\0 1) port)))))
   (cons #\h (lambda (date pad-with port)
               (display (date->string date "~b") port)))
   (cons #\H (lambda (date pad-with port)
               (display (padding (date-hour date)
                                 pad-with 2)
                        port)))
   (cons #\I (lambda (date pad-with port)
               (let ((hr (date-hour date)))
                 (if (> hr 12)
                     (display (padding (- hr 12)
                                       pad-with 2)
                              port)
                     (display (padding hr
                                       pad-with 2)
                              port)))))
   (cons #\j (lambda (date pad-with port)
               (display (padding (date-year-day date)
                                 pad-with 3)
                        port)))
   (cons #\k (lambda (date pad-with port)
               (display (padding (date-hour date)
                                 #\Space 2)
                        port)))
   (cons #\l (lambda (date pad-with port)
               (let ((hr (if (> (date-hour date) 12)
                             (- (date-hour date) 12) (date-hour date))))
                 (display (padding hr  #\Space 2)
                          port))))
   (cons #\m (lambda (date pad-with port)
               (display (padding (date-month date)
                                 pad-with 2)
                        port)))
   (cons #\M (lambda (date pad-with port)
               (display (padding (date-minute date)
                                 pad-with 2)
                        port)))
   (cons #\n (lambda (date pad-with port)
               (newline port)))
   (cons #\N (lambda (date pad-with port)
               (display (padding (date-nanosecond date)
                                 pad-with 9)
                        port)))
   (cons #\p (lambda (date pad-with port)
               (display (locale-am-string/pm (date-hour date)) port)))
   (cons #\r (lambda (date pad-with port)
               (display (date->string date "~I:~M:~S ~p") port)))
   (cons #\s (lambda (date pad-with port)
               (display (time-second (date->time-utc date)) port)))
   (cons #\S (lambda (date pad-with port)
               (if (> (date-nanosecond date)
                      nano)
                   (display (padding (+ (date-second date) 1)
                                     pad-with 2)
                            port)
                   (display (padding (date-second date)
                                     pad-with 2)
                            port))))
   (cons #\t (lambda (date pad-with port)
               (display #\Tab port)))
   (cons #\T (lambda (date pad-with port)
               (display (date->string date "~H:~M:~S") port)))
   (cons #\U (lambda (date pad-with port)
               (if (> (days-before-first-week date 0) 0)
                   (display (padding (+ (date-week-number date 0) 1)
                                     #\0 2) port)
                   (display (padding (date-week-number date 0)
                                     #\0 2) port))))
   (cons #\V (lambda (date pad-with port)
               (display (padding (date-week-number-iso date)
                                 #\0 2) port)))
   (cons #\w (lambda (date pad-with port)
               (display (date-week-day date) port)))
   (cons #\x (lambda (date pad-with port)
               (display (date->string date locale-short-date-format) port)))
   (cons #\X (lambda (date pad-with port)
               (display (date->string date locale-time-format) port)))
   (cons #\W (lambda (date pad-with port)
               (if (> (days-before-first-week date 1) 0)
                   (display (padding (+ (date-week-number date 1) 1)
                                     #\0 2) port)
                   (display (padding (date-week-number date 1)
                                     #\0 2) port))))
   (cons #\y (lambda (date pad-with port)
               (display (padding (last-n-digits
                                  (date-year date) 2)
                                 pad-with
                                 2)
                        port)))
   (cons #\Y (lambda (date pad-with port)
               (let* ((yy (date-year date))
                      (y (if (negative? yy) (+ yy 1) yy)))
                 (unless (<= 0 y 9999)
                   (display (if (negative? y) #\- #\+) port))
                 (display (padding (abs y) pad-with 4) port))))
   (cons #\z (lambda (date pad-with port)
               (tz-printer (date-zone-offset date) port)))
   (cons #\Z (lambda (date pad-with port)
               (locale-print-time-zone date port)))
   (cons #\1 (lambda (date pad-with port)
               (display (date->string date "~Y-~m-~d") port)))
   (cons #\2 (lambda (date pad-with port)
               (display (date->string date "~H:~M:~S~z") port)))
   (cons #\3 (lambda (date pad-with port)
               (display (date->string date "~H:~M:~S") port)))
   (cons #\4 (lambda (date pad-with port)
               (display (date->string date "~Y-~m-~dT~H:~M:~S~z") port)))
   (cons #\5 (lambda (date pad-with port)
               (display (date->string date "~Y-~m-~dT~H:~M:~S") port)))))


(define (get-formatter char)
  (let ((associated (assoc char directives)))
    (if associated (cdr associated) #f)))

(define (date-printer date index format-string str-len port)
  (if (< index str-len)
      (let ((current-char (string-ref format-string index)))
        (if (not (char=? current-char #\~))
            (begin
              (display current-char port)
              (date-printer date (+ index 1) format-string str-len port))
            (if (= (+ index 1) str-len) ; bad format string.
                (time-error 'date-printer 'bad-date-format-string
                            format-string)
                (let ((pad-char? (string-ref format-string (+ index 1))))
                  (cond
                   ((char=? pad-char? #\-)
                    (if (= (+ index 2) str-len) ; bad format string.
                        (time-error 'date-printer
                                    'bad-date-format-string
                                    format-string)
                        (let ((formatter (get-formatter
                                          (string-ref format-string
                                                      (+ index 2)))))
                          (if (not formatter)
                              (time-error 'date-printer
                                          'bad-date-format-string
                                          format-string)
                              (begin
                                (formatter date #f port)
                                (date-printer date
                                              (+ index 3)
                                              format-string
                                              str-len
                                              port))))))

                   ((char=? pad-char? #\_)
                    (if (= (+ index 2) str-len) ; bad format string.
                        (time-error 'date-printer
                                    'bad-date-format-string
                                    format-string)
                        (let ((formatter (get-formatter
                                          (string-ref format-string
                                                      (+ index 2)))))
                          (if (not formatter)
                              (time-error 'date-printer
                                          'bad-date-format-string
                                          format-string)
                              (begin
                                (formatter date #\Space port)
                                (date-printer date
                                              (+ index 3)
                                              format-string
                                              str-len
                                              port))))))
                   (else
                    (let ((formatter (get-formatter
                                      (string-ref format-string
                                                  (+ index 1)))))
                      (if (not formatter)
                          (time-error 'date-printer
                                      'bad-date-format-string
                                      format-string)
                          (begin
                            (formatter date #\0 port)
                            (date-printer date
                                          (+ index 2)
                                          format-string
                                          str-len
                                          port))))))))))))


(define (date->string date .  format-string)
  (let ((str-port (open-output-string))
        (fmt-str (if (null? format-string) "~c" (car format-string))))
    (date-printer date 0 fmt-str (string-length fmt-str) str-port)
    (get-output-string str-port)))

(define (char->int ch)
  (case ch
   ((#\0) 0)
   ((#\1) 1)
   ((#\2) 2)
   ((#\3) 3)
   ((#\4) 4)
   ((#\5) 5)
   ((#\6) 6)
   ((#\7) 7)
   ((#\8) 8)
   ((#\9) 9)
   (else (time-error 'char->int 'bad-date-template-string
                     (list "Non-integer character" ch)))))

;; read an integer upto n characters long on port; upto -> #f is any length
(define (integer-reader upto port)
  (let loop ((accum 0) (nchars 0))
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto)))
          accum
          (loop (+ (* accum 10) (char->int (read-char port)))
                (+ nchars 1))))))

(define (make-integer-reader upto)
  (lambda (port)
    (integer-reader upto port)))

;; read an fractional integer upto n characters long on port; upto -> #f if any length
;;
;; The return value is normalized to upto decimal places. For example, if upto is 9 and
;; the string read is "123", the return value is 123000000.
(define (fractional-integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto)))
          (* accum (expt 10 (- upto nchars)))
          (accum-int port (+ (* accum 10) (char->int (read-char port))) (+ nchars 1)))))
  (accum-int port 0 0))

(define (make-fractional-integer-reader upto)
  (lambda (port)
    (fractional-integer-reader upto port)))

;; read *exactly* n characters and convert to integer; could be padded
(define (integer-reader-exact n port)
  (let ((padding-ok #t))
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
	(cond
	 ((>= nchars n) accum)
	 ((eof-object? ch)
	  (time-error 'string->date 'bad-date-template-string
                      "Premature ending to integer read."))
	 ((char-numeric? ch)
	  (set! padding-ok #f)
	  (accum-int port
                     (+ (* accum 10) (char->int (read-char port)))
		     (+ nchars 1)))
	 (padding-ok
	  (read-char port) ; consume padding
	  (accum-int port accum (+ nchars 1)))
	 (else ; padding where it shouldn't be
	  (time-error 'string->date 'bad-date-template-string
                      "Non-numeric characters in integer read.")))))
    (accum-int port 0 0)))


(define (make-integer-exact-reader n)
  (lambda (port)
    (integer-reader-exact n port)))

(define (zone-reader port)
  (let ((offset 0)
        (positive? #f))
    (let ((ch (read-char port)))
      (if (eof-object? ch)
          (time-error 'string->date 'bad-date-template-string
                      (list "Invalid time zone +/-" ch)))
      (if (or (char=? ch #\Z) (char=? ch #\z))
          0
          (begin
            (cond
             ((char=? ch #\+) (set! positive? #t))
             ((char=? ch #\-) (set! positive? #f))
             (else
              (time-error 'string->date 'bad-date-template-string
                          (list "Invalid time zone +/-" ch))))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (time-error 'string->date 'bad-date-template-string
                              (list "Invalid time zone number" ch)))
              (set! offset (* (char->int ch)
                              10 60 60)))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (time-error 'string->date 'bad-date-template-string
                              (list "Invalid time zone number" ch)))
              (set! offset (+ offset (* (char->int ch)
                                        60 60))))
            (let ((ch (read-char port)))
              (if (eqv? ch #\:)
                  (set! ch (read-char port)))
              (if (eof-object? ch)
                  (time-error 'string->date 'bad-date-template-string
                              (list "Invalid time zone number" ch)))
              (set! offset (+ offset (* (char->int ch)
                                        10 60))))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (time-error 'string->date 'bad-date-template-string
                              (list "Invalid time zone number" ch)))
              (set! offset (+ offset (* (char->int ch)
                                        60))))
            (if positive? offset (- offset)))))))

;; looking at a char, read the char string, run thru indexer, return index
(define (locale-reader port indexer)

  (define (read-char-string result)
    (let ((ch (peek-char port)))
      (if (char-alphabetic? ch)
          (read-char-string (cons (read-char port) result))
          (list->string (reverse! result)))))

  (let* ((str (read-char-string '()))
         (index (indexer str)))
    (if index index (time-error 'string->date
                                'bad-date-template-string
                                (list "Invalid string for " indexer)))))

(define (make-locale-reader indexer)
  (lambda (port)
    (locale-reader port indexer)))

(define (make-char-id-reader char)
  (lambda (port)
    (if (char=? char (read-char port))
        char
        (time-error 'string->date
                    'bad-date-template-string
                    "Invalid character match."))))

;; A List of formatted read directives.
;; Each entry is a list.
;; 1. the character directive;
;; a procedure, which takes a character as input & returns
;; 2. #t as soon as a character on the input port is acceptable
;; for input,
;; 3. a port reader procedure that knows how to read the current port
;; for a value. Its one parameter is the port.
;; 4. an optional action procedure, that takes the value (from 3.) and
;; some object (here, always the date) and (probably) side-effects it.
;; If no action is required, as with ~A, this element may be #f.

(define read-directives
  (let ((ireader4 (make-integer-reader 4))
        (ireader2 (make-integer-reader 2))
        (fireader9 (make-fractional-integer-reader 9))
        (eireader2 (make-integer-exact-reader 2))
        (locale-reader-abbr-weekday (make-locale-reader
                                     locale-abbr-weekday->index))
        (locale-reader-long-weekday (make-locale-reader
                                     locale-long-weekday->index))
        (locale-reader-abbr-month   (make-locale-reader
                                     locale-abbr-month->index))
        (locale-reader-long-month   (make-locale-reader
                                     locale-long-month->index))
        (char-fail (lambda (ch) #t)))

    (list
     (list #\~ char-fail (make-char-id-reader #\~) #f)
     (list #\a char-alphabetic? locale-reader-abbr-weekday #f)
     (list #\A char-alphabetic? locale-reader-long-weekday #f)
     (list #\b char-alphabetic? locale-reader-abbr-month
           (lambda (val object)
             (set-date-month! object val)))
     (list #\B char-alphabetic? locale-reader-long-month
           (lambda (val object)
             (set-date-month! object val)))
     (list #\d char-numeric? ireader2 (lambda (val object)
                                        (set-date-day!
                                         object val)))
     (list #\e char-fail eireader2 (lambda (val object)
                                     (set-date-day! object val)))
     (list #\h char-alphabetic? locale-reader-abbr-month
           (lambda (val object)
             (set-date-month! object val)))
     (list #\H char-numeric? ireader2 (lambda (val object)
                                        (set-date-hour! object val)))
     (list #\k char-fail eireader2 (lambda (val object)
                                     (set-date-hour! object val)))
     (list #\m char-numeric? ireader2 (lambda (val object)
                                        (set-date-month! object val)))
     (list #\M char-numeric? ireader2 (lambda (val object)
                                        (set-date-minute!
                                         object val)))
     (list #\N char-numeric? fireader9 (lambda (val object)
					 (set-date-nanosecond!
                                          object val)))
     (list #\S char-numeric? ireader2 (lambda (val object)
                                        (set-date-second! object val)))
     (list #\y char-fail eireader2
           (lambda (val object)
             (set-date-year! object (natural-year val))))

     ;; XXX FIXME: Support the extended year format used by
     ;; 'date->string' when the year is not in the range 0-9999.
     (list #\Y char-numeric? ireader4 (lambda (val object)
                                        (set-date-year! object val)))

     (list #\z (lambda (c)
                 (or (char=? c #\Z)
                     (char=? c #\z)
                     (char=? c #\+)
                     (char=? c #\-)))
           zone-reader (lambda (val object)
                         (set-date-zone-offset! object val))))))

(define (priv:string->date date index format-string str-len port template-string)
  (define (skip-until port skipper)
    (let ((ch (peek-char port)))
      (if (eof-object? ch)
          (time-error 'string->date 'bad-date-format-string template-string)
          (if (not (skipper ch))
              (begin (read-char port) (skip-until port skipper))))))
  (if (< index str-len)
      (let ((current-char (string-ref format-string index)))
        (if (not (char=? current-char #\~))
            (let ((port-char (read-char port)))
              (if (or (eof-object? port-char)
                      (not (char=? current-char port-char)))
                  (time-error 'string->date
                              'bad-date-format-string template-string))
              (priv:string->date date
                                 (+ index 1)
                                 format-string
                                 str-len
                                 port
                                 template-string))
            ;; otherwise, it's an escape, we hope
            (if (> (+ index 1) str-len)
                (time-error 'string->date
                            'bad-date-format-string template-string)
                (let* ((format-char (string-ref format-string (+ index 1)))
                       (format-info (assoc format-char read-directives)))
                  (if (not format-info)
                      (time-error 'string->date
                                  'bad-date-format-string template-string)
                      (begin
                        (let ((skipper (cadr format-info))
                              (reader  (caddr format-info))
                              (actor   (cadddr format-info)))
                          (skip-until port skipper)
                          (let ((val (reader port)))
                            (if (eof-object? val)
                                (time-error 'string->date
                                            'bad-date-format-string
                                            template-string)
                                (if actor (actor val date))))
                          (priv:string->date date
                                             (+ index 2)
                                             format-string
                                             str-len
                                             port
                                             template-string))))))))))

(define (string->date input-string template-string)
  (define (date-ok? date)
    (and (date-nanosecond date)
         (date-second date)
         (date-minute date)
         (date-hour date)
         (date-day date)
         (date-month date)
         (date-year date)
         (date-zone-offset date)))
  (let ((newdate (make-date 0 0 0 0 #f #f #f #f)))
    (priv:string->date newdate
                       0
                       template-string
                       (string-length template-string)
                       (open-input-string input-string)
                       template-string)
    (if (not (date-zone-offset newdate))
	(begin
	  ;; this is necessary to get DST right -- as far as we can
	  ;; get it right (think of the double/missing hour in the
	  ;; night when we are switching between normal time and DST).
	  (set-date-zone-offset! newdate
				 (local-tz-offset
				  (make-time time-utc 0 0)))
	  (set-date-zone-offset! newdate
				 (local-tz-offset
				  (date->time-utc newdate)))))
    (if (date-ok? newdate)
        newdate
        (time-error
         'string->date
         'bad-date-format-string
         (list "Incomplete date read. " newdate template-string)))))

;;; srfi-19.scm ends here
