;;; Ports, implemented in Scheme
;;; Copyright (C) 2016, 2019 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; We would like to be able to implement green threads using delimited
;;; continuations.  When a green thread would block on I/O, it should
;;; suspend and arrange to be resumed when it can make progress.
;;;
;;; The problem is that the ports code is written in C.  A delimited
;;; continuation that captures a C activation can't be resumed, because
;;; Guile doesn't know about the internal structure of the C activation
;;; (stack frame) and so can't compose it with the current continuation.
;;; For that reason, to implement this desired future, we have to
;;; implement ports in Scheme.
;;;
;;; If Scheme were fast enough, we would just implement ports in Scheme
;;; early in Guile's boot, and that would be that.  However currently
;;; that's not the case: character-by-character I/O is about three or
;;; four times slower in Scheme than in C.  This is mostly bytecode
;;; overhead, though there are some ways that compiler improvements
;;; could help us too.
;;;
;;; Note that the difference between Scheme and C is much less for
;;; batched operations, like read-bytes or read-line.
;;;
;;; So the upshot is that we need to keep the C I/O routines around for
;;; performance reasons.  We can still have our Scheme routines
;;; available as a module, though, for use by people working with green
;;; threads.  That's this module.  People that want green threads can
;;; even replace the core bindings, which enables green threading over
;;; other generic routines like the HTTP server.
;;;
;;; Code:


(define-module (ice-9 suspendable-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 ports internal)
  #:use-module (ice-9 match)
  #:export (current-read-waiter
            current-write-waiter

            install-suspendable-ports!
            uninstall-suspendable-ports!))

(define (default-read-waiter port) (port-poll port "r"))
(define (default-write-waiter port) (port-poll port "w"))

(define current-read-waiter  (make-parameter default-read-waiter))
(define current-write-waiter (make-parameter default-write-waiter))

(define (wait-for-readable port) ((current-read-waiter) port))
(define (wait-for-writable port) ((current-write-waiter) port))

(define (read-bytes port dst start count)
  (cond
   (((port-read port) port dst start count)
    => (lambda (read)
         (unless (<= 0 read count)
           (error "bad return from port read function" read))
         read))
   (else
    (wait-for-readable port)
    (read-bytes port dst start count))))

(define (write-bytes port src start count)
  (cond
   (((port-write port) port src start count)
    => (lambda (written)
         (unless (<= 0 written count)
           (error "bad return from port write function" written))
         (when (< written count)
           (write-bytes port src (+ start written) (- count written)))))
   (else
    (wait-for-writable port)
    (write-bytes port src start count))))

(define (flush-input port)
  (let* ((buf (port-read-buffer port))
         (cur (port-buffer-cur buf))
         (end (port-buffer-end buf)))
    (when (< cur end)
      (set-port-buffer-cur! buf 0)
      (set-port-buffer-end! buf 0)
      (seek port (- cur end) SEEK_CUR))))

(define (flush-output port)
  (let* ((buf (port-write-buffer port))
         (cur (port-buffer-cur buf))
         (end (port-buffer-end buf)))
    (when (< cur end)
      ;; Update cursors before attempting to write, assuming that I/O
      ;; errors are sticky.  That way if the write throws an error,
      ;; causing the computation to abort, and possibly causing the port
      ;; to be collected by GC when it's open, any subsequent close-port
      ;; or force-output won't signal *another* error.
      (set-port-buffer-cur! buf 0)
      (set-port-buffer-end! buf 0)
      (write-bytes port (port-buffer-bytevector buf) cur (- end cur)))))

(define utf8-bom #vu8(#xEF #xBB #xBF))
(define utf16be-bom #vu8(#xFE #xFF))
(define utf16le-bom #vu8(#xFF #xFE))
(define utf32be-bom #vu8(#x00 #x00 #xFE #xFF))
(define utf32le-bom #vu8(#xFF #xFE #x00 #x00))

(define (clear-stream-start-for-bom-read port io-mode)
  (define (maybe-consume-bom bom)
    (and (eq? (peek-byte port) (bytevector-u8-ref bom 0))
         (call-with-values (lambda ()
                             (fill-input port (bytevector-length bom)))
           (lambda (buf cur buffered)
             (and (<= (bytevector-length bom) buffered)
                  (let ((bv (port-buffer-bytevector buf)))
                    (let lp ((i 1))
                      (if (= i (bytevector-length bom))
                          (begin
                            (set-port-buffer-cur! buf (+ cur i))
                            #t)
                          (and (eq? (bytevector-u8-ref bv (+ cur i))
                                    (bytevector-u8-ref bom i))
                               (lp (1+ i)))))))))))
  (when (and (port-clear-stream-start-for-bom-read port)
             (eq? io-mode 'text))
    (case (%port-encoding port)
      ((UTF-8)
       (maybe-consume-bom utf8-bom))
      ((UTF-16)
       (cond
        ((maybe-consume-bom utf16le-bom)
         (specialize-port-encoding! port 'UTF-16LE))
        (else
         (maybe-consume-bom utf16be-bom)
         (specialize-port-encoding! port 'UTF-16BE))))
      ((UTF-32)
       (cond
        ((maybe-consume-bom utf32le-bom)
         (specialize-port-encoding! port 'UTF-32LE))
        (else
         (maybe-consume-bom utf32be-bom)
         (specialize-port-encoding! port 'UTF-32BE)))))))

(define* (fill-input port #:optional (minimum-buffering 1) (io-mode 'text))
  (clear-stream-start-for-bom-read port io-mode)
  (let* ((buf (port-read-buffer port))
         (cur (port-buffer-cur buf))
         (buffered (max (- (port-buffer-end buf) cur) 0)))
    (cond
     ((or (<= minimum-buffering buffered) (port-buffer-has-eof? buf))
      (values buf cur buffered))
     (else
      (unless (input-port? port)
        (error "not an input port" port))
      (when (port-random-access? port)
        (flush-output port))
      (let ((bv (port-buffer-bytevector buf)))
        (cond
         ((< (bytevector-length bv) minimum-buffering)
          (expand-port-read-buffer! port minimum-buffering)
          (fill-input port minimum-buffering))
         (else
          (when (< 0 cur)
            (bytevector-copy! bv cur bv 0 buffered)
            (set-port-buffer-cur! buf 0)
            (set-port-buffer-end! buf buffered))
          (let ((buffering (max (port-read-buffering port) minimum-buffering)))
            (let lp ((buffered buffered))
              (let* ((count (- buffering buffered))
                     (read (read-bytes port bv buffered count)))
                (cond
                 ((zero? read)
                  (set-port-buffer-has-eof?! buf #t)
                  (values buf 0 buffered))
                 (else
                  (let ((buffered (+ buffered read)))
                    (set-port-buffer-end! buf buffered)
                    (if (< buffered minimum-buffering)
                        (lp buffered)
                        (values buf 0 buffered)))))))))))))))

(define* (force-output #:optional (port (current-output-port)))
  (unless (and (output-port? port) (not (port-closed? port)))
    (error "not an open output port" port))
  (flush-output port))

(define close-port
  (let ((%close-port (@ (guile) close-port)))
    (lambda (port)
      (cond
       ((port-closed? port) #f)
       (else
        (when (output-port? port) (flush-output port))
        (%close-port port))))))

(define-inlinable (peek-bytes port count kfast kslow)
  (let* ((buf (port-read-buffer port))
         (cur (port-buffer-cur buf))
         (buffered (- (port-buffer-end buf) cur)))
    (if (<= count buffered)
        (kfast buf (port-buffer-bytevector buf) cur buffered)
        (call-with-values (lambda () (fill-input port count))
          (lambda (buf cur buffered)
            (kslow buf (port-buffer-bytevector buf) cur buffered))))))

(define (peek-byte port)
  (peek-bytes port 1
              (lambda (buf bv cur buffered)
                (bytevector-u8-ref bv cur))
              (lambda (buf bv cur buffered)
                (and (> buffered 0)
                     (bytevector-u8-ref bv cur)))))

(define* (lookahead-u8 port)
  (define (fast-path buf bv cur buffered)
    (bytevector-u8-ref bv cur))
  (define (slow-path buf bv cur buffered)
    (if (zero? buffered)
        the-eof-object
        (fast-path buf bv cur buffered)))
  (peek-bytes port 1 fast-path slow-path))

(define* (get-u8 port)
  (define (fast-path buf bv cur buffered)
    (set-port-buffer-cur! buf (1+ cur))
    (bytevector-u8-ref bv cur))
  (define (slow-path buf bv cur buffered)
    (if (zero? buffered)
        (begin
          (set-port-buffer-has-eof?! buf #f)
          the-eof-object)
        (fast-path buf bv cur buffered)))
  (peek-bytes port 1 fast-path slow-path))

(define (get-bytevector-n! port bv start count)
  (define (port-buffer-take! pos buf cur to-copy)
    (bytevector-copy! (port-buffer-bytevector buf) cur
                      bv pos to-copy)
    (set-port-buffer-cur! buf (+ cur to-copy))
    (+ pos to-copy))
  (define (take-already-buffered)
    (let* ((buf (port-read-buffer port))
           (cur (port-buffer-cur buf))
           (buffered (max (- (port-buffer-end buf) cur) 0)))
      (port-buffer-take! start buf cur (min count buffered))))
  (define (buffer-and-fill pos)
    (call-with-values (lambda () (fill-input port 1 'binary))
      (lambda (buf cur buffered)
        (if (zero? buffered)
            ;; We found EOF, which is marked in the port read buffer.
            ;; If we haven't read any bytes yet, clear the EOF from the
            ;; buffer and return it.  Otherwise return the number of
            ;; bytes that we have read.
            (if (= pos start)
                (begin
                  (set-port-buffer-has-eof?! buf #f)
                  the-eof-object)
                (- pos start))
            (let ((pos (port-buffer-take! pos buf cur
                                          (min (- (+ start count) pos)
                                               buffered))))
              (if (= pos (+ start count))
                  count
                  (buffer-and-fill pos)))))))
  (define (fill-directly pos)
    (when (port-random-access? port)
      (flush-output port))
    (port-clear-stream-start-for-bom-read port)
    (let lp ((pos pos))
      (let ((read (read-bytes port bv pos (- (+ start count) pos))))
        (cond
         ((= (+ pos read) (+ start count))
          count)
         ((zero? read)
          ;; We found EOF.  If we haven't read any bytes yet, return
          ;; EOF.  Otherwise save the EOF in the port read buffer.
          (if (= pos start)
              the-eof-object
              (begin
                (set-port-buffer-has-eof?! (port-read-buffer port) #t)
                (- pos start))))
         (else (lp (+ pos read)))))))
  (let ((pos (take-already-buffered)))
    (cond
     ((= pos (+ start count))
      count)
     ((< (- (+ start count) pos) (port-read-buffering port))
      (buffer-and-fill pos))
     (else (fill-directly pos)))))

(define (get-bytevector-n port count)
  (let* ((bv (make-bytevector count))
         (result (get-bytevector-n! port bv 0 count)))
    (cond ((eof-object? result)
           result)
          ((= result count)
           bv)
          (else
           (let ((bv* (make-bytevector result)))
             (bytevector-copy! bv 0 bv* 0 result)
             bv*)))))

(define (get-bytevector-some port)
  (call-with-values (lambda () (fill-input port 1 'binary))
    (lambda (buf cur buffered)
      (if (zero? buffered)
          (begin
            (set-port-buffer-has-eof?! buf #f)
            the-eof-object)
          (let ((result (make-bytevector buffered)))
            (bytevector-copy! (port-buffer-bytevector buf) cur
                              result 0 buffered)
            (set-port-buffer-cur! buf (+ cur buffered))
            result)))))

(define (get-bytevector-some! port bv start count)
  (if (zero? count)
      0
      (call-with-values (lambda () (fill-input port 1 'binary))
        (lambda (buf cur buffered)
          (if (zero? buffered)
              (begin
                (set-port-buffer-has-eof?! buf #f)
                the-eof-object)
              (let ((transfer-size (min count buffered)))
                (bytevector-copy! (port-buffer-bytevector buf) cur
                                  transfer-size start buffered)
                (set-port-buffer-cur! buf (+ cur transfer-size))
                transfer-size))))))

(define (put-u8 port byte)
  (let* ((buf (port-write-buffer port))
         (bv (port-buffer-bytevector buf))
         (end (port-buffer-end buf)))
    (unless (<= 0 end (bytevector-length bv))
      (error "not an output port" port))
    (when (and (eq? (port-buffer-cur buf) end) (port-random-access? port))
      (flush-input port))
    (cond
     ((= end (bytevector-length bv))
      ;; Multiple threads racing; race to flush, then retry.
      (flush-output port)
      (put-u8 port byte))
     (else
      (bytevector-u8-set! bv end byte)
      (set-port-buffer-end! buf (1+ end))
      (when (= (1+ end) (bytevector-length bv)) (flush-output port))))))

(define* (put-bytevector port src #:optional (start 0)
                         (count (- (bytevector-length src) start)))
  (unless (<= 0 start (+ start count) (bytevector-length src))
    (error "invalid start/count" start count))
  (let* ((buf (port-write-buffer port))
         (bv (port-buffer-bytevector buf))
         (size (bytevector-length bv))
         (cur (port-buffer-cur buf))
         (end (port-buffer-end buf))
         (buffered (max (- end cur) 0)))
    (when (and (eq? cur end) (port-random-access? port))
      (flush-input port))
    (cond
     ((<= size count)
      ;; The write won't fit in the buffer at all; write directly.
      ;; Write directly.  Flush write buffer first if needed.
      (when (< cur end) (flush-output port))
      (write-bytes port src start count))
     ((< (- size buffered) count)
      ;; The write won't fit into the buffer along with what's already
      ;; buffered.  Flush and fill.
      (flush-output port)
      (set-port-buffer-end! buf count)
      (bytevector-copy! src start bv 0 count))
     (else
      ;; The write will fit in the buffer, but we need to shuffle the
      ;; already-buffered bytes (if any) down.
      (set-port-buffer-cur! buf 0)
      (set-port-buffer-end! buf (+ buffered count))
      (bytevector-copy! bv cur bv 0 buffered)
      (bytevector-copy! src start bv buffered count)
      ;; If the buffer completely fills, we flush.
      (when (= (+ buffered count) size)
        (flush-output port))))))

(define (decoding-error subr port)
  ;; GNU definition; fixme?
  (define EILSEQ 84)
  (throw 'decoding-error subr "input decoding error" EILSEQ port))

(define-inlinable (decode-utf8 bv start avail u8_0 kt kf)
  (cond
   ((< u8_0 #x80)
    (kt (integer->char u8_0) 1))
   ((and (<= #xc2 u8_0 #xdf) (<= 2 avail))
    (let ((u8_1 (bytevector-u8-ref bv (1+ start))))
      (if (= (logand u8_1 #xc0) #x80)
          (kt (integer->char
               (logior (ash (logand u8_0 #x1f) 6)
                       (logand u8_1 #x3f)))
              2)
          (kf))))
   ((and (= (logand u8_0 #xf0) #xe0) (<= 3 avail))
    (let ((u8_1 (bytevector-u8-ref bv (+ start 1)))
          (u8_2 (bytevector-u8-ref bv (+ start 2))))
      (if (and (= (logand u8_1 #xc0) #x80)
               (= (logand u8_2 #xc0) #x80)
               (case u8_0
                 ((#xe0) (>= u8_1 #xa0))
                 ((#xed) (>= u8_1 #x9f))
                 (else #t)))
          (kt (integer->char
               (logior (ash (logand u8_0 #x0f) 12)
                       (ash (logand u8_1 #x3f) 6)
                       (logand u8_2 #x3f)))
              3)
          (kf))))
   ((and (<= #xf0 u8_0 #xf4) (<= 4 avail))
    (let ((u8_1 (bytevector-u8-ref bv (+ start 1)))
          (u8_2 (bytevector-u8-ref bv (+ start 2)))
          (u8_3 (bytevector-u8-ref bv (+ start 3))))
      (if (and (= (logand u8_1 #xc0) #x80)
               (= (logand u8_2 #xc0) #x80)
               (= (logand u8_3 #xc0) #x80)
               (case u8_0
                 ((#xf0) (>= u8_1 #x90))
                 ((#xf4) (>= u8_1 #x8f))
                 (else #t)))
          (kt (integer->char
               (logior (ash (logand u8_0 #x07) 18)
                       (ash (logand u8_1 #x3f) 12)
                       (ash (logand u8_2 #x3f) 6)
                       (logand u8_3 #x3f)))
              4)
          (kf))))
   (else (kf))))

(define (bad-utf8-len bv cur buffering first-byte)
  (define (ref n)
    (bytevector-u8-ref bv (+ cur n)))
  (cond
   ((< first-byte #x80) 0)
   ((<= #xc2 first-byte #xdf)
    (cond
     ((< buffering 2) 1)
     ((not (= (logand (ref 1) #xc0) #x80)) 1)
     (else 0)))
   ((= (logand first-byte #xf0) #xe0)
    (cond
     ((< buffering 2) 1)
     ((not (= (logand (ref 1) #xc0) #x80)) 1)
     ((and (eq? first-byte #xe0) (< (ref 1) #xa0)) 1)
     ((and (eq? first-byte #xed) (< (ref 1) #x9f)) 1)
     ((< buffering 3) 2)
     ((not (= (logand (ref 2) #xc0) #x80)) 2)
     (else 0)))
   ((<= #xf0 first-byte #xf4)
    (cond
     ((< buffering 2) 1)
     ((not (= (logand (ref 1) #xc0) #x80)) 1)
     ((and (eq? first-byte #xf0) (< (ref 1) #x90)) 1)
     ((and (eq? first-byte #xf4) (< (ref 1) #x8f)) 1)
     ((< buffering 3) 2)
     ((not (= (logand (ref 2) #xc0) #x80)) 2)
     ((< buffering 4) 3)
     ((not (= (logand (ref 3) #xc0) #x80)) 3)
     (else 0)))
   (else 1)))

(define (peek-char-and-next-cur/utf8 port buf cur first-byte)
  (if (< first-byte #x80)
      (values (integer->char first-byte) buf (+ cur 1))
      (call-with-values (lambda ()
                          (fill-input port
                                      (cond
                                       ((<= #xc2 first-byte #xdf) 2)
                                       ((= (logand first-byte #xf0) #xe0) 3)
                                       (else 4))))
        (lambda (buf cur buffering)
          (let ((bv (port-buffer-bytevector buf)))
            (define (bad-utf8)
              (let ((len (bad-utf8-len bv cur buffering first-byte)))
                (when (zero? len) (error "internal error"))
                (if (eq? (port-conversion-strategy port) 'substitute)
                    (values #\xFFFD buf (+ cur len))
                    (decoding-error "peek-char" port))))
            (decode-utf8 bv cur buffering first-byte
                         (lambda (char len)
                           (values char buf (+ cur len)))
                         bad-utf8))))))

(define (peek-char-and-next-cur/iso-8859-1 port buf cur first-byte)
  (values (integer->char first-byte) buf (+ cur 1)))

(define (peek-char-and-next-cur/iconv port)
  (let lp ((prev-input-size 0))
    (let ((input-size (1+ prev-input-size)))
      (call-with-values (lambda () (fill-input port input-size))
        (lambda (buf cur buffered)
          (cond
           ((< buffered input-size)
            ;; Buffer failed to fill; EOF, possibly premature.
            (cond
             ((zero? prev-input-size)
              (values the-eof-object buf cur))
             ((eq? (port-conversion-strategy port) 'substitute)
              (values #\xFFFD buf (+ cur prev-input-size)))
             (else
              (decoding-error "peek-char" port))))
           ((port-decode-char port (port-buffer-bytevector buf)
                              cur input-size)
            => (lambda (char)
                 (values char buf (+ cur input-size))))
           (else
            (lp input-size))))))))

(define (peek-char-and-next-cur port)
  (define (have-byte buf bv cur buffered)
    (let ((first-byte (bytevector-u8-ref bv cur)))
      (case (%port-encoding port)
        ((UTF-8)
         (peek-char-and-next-cur/utf8 port buf cur first-byte))
        ((ISO-8859-1)
         (peek-char-and-next-cur/iso-8859-1 port buf cur first-byte))
        (else
         (peek-char-and-next-cur/iconv port)))))
  (peek-bytes port 1 have-byte
              (lambda (buf bv cur buffered)
                (if (< 0 buffered)
                    (have-byte buf bv cur buffered)
                    (values the-eof-object buf cur)))))

(define* (peek-char #:optional (port (current-input-port)))
  (define (slow-path)
    (call-with-values (lambda () (peek-char-and-next-cur port))
      (lambda (char buf cur)
        char)))
  (define (fast-path buf bv cur buffered)
    (let ((u8 (bytevector-u8-ref bv cur))
          (enc (%port-encoding port)))
      (case enc
        ((UTF-8) (decode-utf8 bv cur buffered u8 (lambda (char len) char)
                              slow-path))
        ((ISO-8859-1) (integer->char u8))
        (else (slow-path)))))
  (peek-bytes port 1 fast-path
              (lambda (buf bv cur buffered) (slow-path))))

(define-inlinable (advance-port-position! pos char)
  ;; FIXME: this cond is a speed hack; really we should just compile
  ;; `case' better.
  (cond
   ;; FIXME: char>? et al should compile well.
   ((<= (char->integer #\space) (char->integer char))
    (set-port-position-column! pos (1+ (port-position-column pos))))
   (else
    (case char
      ((#\alarm) #t)                    ; No change.
      ((#\backspace)
       (let ((col (port-position-column pos)))
         (when (> col 0)
           (set-port-position-column! pos (1- col)))))
      ((#\newline)
       (set-port-position-line! pos (1+ (port-position-line pos)))
       (set-port-position-column! pos 0))
      ((#\return)
       (set-port-position-column! pos 0))
      ((#\tab)
       (let ((col (port-position-column pos)))
         (set-port-position-column! pos (- (+ col 8) (remainder col 8)))))
      (else
       (set-port-position-column! pos (1+ (port-position-column pos))))))))

(define* (read-char #:optional (port (current-input-port)))
  (define (finish buf char)
    (advance-port-position! (port-buffer-position buf) char)
    char)
  (define (slow-path)
    (call-with-values (lambda () (peek-char-and-next-cur port))
      (lambda (char buf cur)
        (set-port-buffer-cur! buf cur)
        (if (eq? char the-eof-object)
            (begin
              (set-port-buffer-has-eof?! buf #f)
              char)
            (finish buf char)))))
  (define (fast-path buf bv cur buffered)
    (let ((u8 (bytevector-u8-ref bv cur))
          (enc (%port-encoding port)))
      (case enc
        ((UTF-8)
         (decode-utf8 bv cur buffered u8
                      (lambda (char len)
                        (set-port-buffer-cur! buf (+ cur len))
                        (finish buf char))
                      slow-path))
        ((ISO-8859-1)
         (set-port-buffer-cur! buf (+ cur 1))
         (finish buf (integer->char u8)))
        (else (slow-path)))))
  (peek-bytes port 1 fast-path
              (lambda (buf bv cur buffered) (slow-path))))

(define-inlinable (port-fold-chars/iso-8859-1 port proc seed)
  (let* ((buf (port-read-buffer port))
         (cur (port-buffer-cur buf)))
    (let fold-buffer ((buf buf) (cur cur) (seed seed))
      (let ((bv (port-buffer-bytevector buf))
            (end (port-buffer-end buf)))
        (let fold-chars ((cur cur) (seed seed))
          (cond
           ((= end cur)
            (call-with-values (lambda () (fill-input port))
              (lambda (buf cur buffered)
                (if (zero? buffered)
                    (call-with-values (lambda () (proc the-eof-object seed))
                      (lambda (seed done?)
                        (if done? seed (fold-buffer buf cur seed))))
                    (fold-buffer buf cur seed)))))
           (else
            (let ((ch (integer->char (bytevector-u8-ref bv cur)))
                  (cur (1+ cur)))
              (set-port-buffer-cur! buf cur)
              (advance-port-position! (port-buffer-position buf) ch)
              (call-with-values (lambda () (proc ch seed))
                (lambda (seed done?)
                  (if done? seed (fold-chars cur seed))))))))))))

(define-inlinable (port-fold-chars port proc seed)
  (case (%port-encoding port)
    ((ISO-8859-1) (port-fold-chars/iso-8859-1 port proc seed))
    (else
     (let lp ((seed seed))
       (let ((ch (read-char port)))
         (call-with-values (lambda () (proc ch seed))
           (lambda (seed done?)
             (if done? seed (lp seed)))))))))

(define* (read-delimited delims #:optional (port (current-input-port))
                         (handle-delim 'trim))
  ;; Currently this function conses characters into a list, then uses
  ;; reverse-list->string.  It wastes 2 words per character but it still
  ;; seems to be the fastest thing at the moment.
  (define (finish delim chars)
    (define (->string chars)
      (if (and (null? chars) (not (char? delim)))
          the-eof-object
          (reverse-list->string chars)))
    (case handle-delim
      ((trim) (->string chars))
      ((split) (cons (->string chars) delim))
      ((concat)
       (->string (if (char? delim) (cons delim chars) chars)))
      ((peek)
       (when (char? delim) (unread-char delim port))
       (->string chars))
      (else
       (error "unexpected handle-delim value: " handle-delim))))
  (define-syntax-rule (make-folder delimiter?)
    (lambda (char chars)
      (if (or (not (char? char)) (delimiter? char))
          (values (finish char chars) #t)
          (values (cons char chars) #f))))
  (define-syntax-rule (specialized-fold delimiter?)
    (port-fold-chars port (make-folder delimiter?) '()))
  (case (string-length delims)
    ((0) (specialized-fold (lambda (char) #f)))
    ((1) (let ((delim (string-ref delims 0)))
           (specialized-fold (lambda (char) (eqv? char delim)))))
    (else => (lambda (ndelims)
               (specialized-fold
                (lambda (char)
                  (let lp ((i 0))
                    (and (< i ndelims)
                         (or (eqv? char (string-ref delims i))
                             (lp (1+ i)))))))))))

(define* (read-line #:optional (port (current-input-port))
                    (handle-delim 'trim))
  (read-delimited "\n" port handle-delim))

(define* (%read-line port)
  (read-line port 'split))

(define* (put-string port str #:optional (start 0)
                     (count (- (string-length str) start)))
  (let* ((aux (port-auxiliary-write-buffer port))
         (pos (port-buffer-position aux))
         (line (port-position-line pos)))
    (set-port-buffer-cur! aux 0)
    (port-clear-stream-start-for-bom-write port aux)
    (let lp ((encoded 0))
      (when (< encoded count)
        (let ((encoded (+ encoded
                          (port-encode-chars port aux str
                                             (+ start encoded)
                                             (- count encoded)))))
          (let ((end (port-buffer-end aux)))
            (set-port-buffer-end! aux 0)
            (put-bytevector port (port-buffer-bytevector aux) 0 end)
            (lp encoded)))))
    (when (and (not (eqv? line (port-position-line pos)))
               (port-line-buffered? port))
      (flush-output port))))

(define* (put-char port char)
  (let ((aux (port-auxiliary-write-buffer port)))
    (set-port-buffer-cur! aux 0)
    (port-clear-stream-start-for-bom-write port aux)
    (port-encode-char port aux char)
    (let ((end (port-buffer-end aux)))
      (set-port-buffer-end! aux 0)
      (put-bytevector port (port-buffer-bytevector aux) 0 end))
    (when (and (eqv? char #\newline) (port-line-buffered? port))
      (flush-output port))))

(define accept
  (let ((%accept (@ (guile) accept)))
    (lambda* (port #:optional (flags 0))
      (let lp ()
        (or (%accept port flags)
            (begin
              (wait-for-readable port)
              (lp)))))))

(define connect
  (let ((%connect (@ (guile) connect)))
    (lambda (port sockaddr . args)
      (unless (apply %connect port sockaddr args)
        ;; Clownshoes semantics; see connect(2).
        (wait-for-writable port)
        (let ((err (getsockopt port SOL_SOCKET SO_ERROR)))
          (unless (zero? err)
            (scm-error 'system-error "connect" "~A"
                       (list (strerror err)) #f)))))))

(define saved-port-bindings #f)
(define port-bindings
  '(((guile)
     read-char peek-char force-output close-port
     accept connect)
    ((ice-9 binary-ports)
     get-u8 lookahead-u8 get-bytevector-n get-bytevector-n!
     get-bytevector-some get-bytevector-some!
     put-u8 put-bytevector)
    ((ice-9 textual-ports)
     put-char put-string)
    ((ice-9 rdelim) %read-line read-line read-delimited)))
(define (install-suspendable-ports!)
  (unless saved-port-bindings
    (set! saved-port-bindings (make-hash-table))
    (let ((suspendable-ports (resolve-module '(ice-9 suspendable-ports))))
      (for-each
       (match-lambda
         ((mod . syms)
          (let ((mod (resolve-module mod)))
            (for-each (lambda (sym)
                        (hashq-set! saved-port-bindings sym
                                    (module-ref mod sym))
                        (module-set! mod sym
                                     (module-ref suspendable-ports sym)))
                      syms))))
       port-bindings))))

(define (uninstall-suspendable-ports!)
  (when saved-port-bindings
    (for-each
     (match-lambda
       ((mod . syms)
        (let ((mod (resolve-module mod)))
          (for-each (lambda (sym)
                      (let ((saved (hashq-ref saved-port-bindings sym)))
                        (module-set! mod sym saved)))
                    syms))))
     port-bindings)
    (set! saved-port-bindings #f)))
