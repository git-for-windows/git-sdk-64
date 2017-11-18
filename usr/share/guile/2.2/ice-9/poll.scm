;; poll

;;;; Copyright (C) 2010, 2011, 2012 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
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

(define-module (ice-9 poll)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs bytevectors)
  #:export (make-empty-poll-set
            poll-set?
            poll-set-nfds
            poll-set-find-port
            poll-set-port
            poll-set-events
            set-poll-set-events!
            poll-set-revents
            set-poll-set-revents!
            poll-set-add!
            poll-set-remove!
            poll))

(eval-when (expand load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_poll"))

(if (not (= %sizeof-struct-pollfd 8))
    (error "Unexpected struct pollfd size" %sizeof-struct-pollfd))

(if (defined? 'POLLIN)
    (export POLLIN))

(if (defined? 'POLLPRI)
    (export POLLPRI))

(if (defined? 'POLLOUT)
    (export POLLOUT))

(if (defined? 'POLLRDHUP)
    (export POLLRDHUP))

(if (defined? 'POLLERR)
    (export POLLERR))

(if (defined? 'POLLHUP)
    (export POLLHUP))

(if (defined? 'POLLNVAL)
    (export POLLNVAL))


(define-record-type <poll-set>
  (make-poll-set pollfds nfds ports)
  poll-set?
  (pollfds pset-pollfds set-pset-pollfds!)
  (nfds poll-set-nfds set-pset-nfds!)
  (ports pset-ports set-pset-ports!)
  )

(define-syntax-rule (pollfd-offset n)
  (* n 8))

(define* (make-empty-poll-set #:optional (pre-allocated 4))
  (make-poll-set (make-bytevector (pollfd-offset pre-allocated) 0)
                 0
                 (make-vector pre-allocated #f)))

(define (pset-size set)
  (vector-length (pset-ports set)))

(define (ensure-pset-size! set size)
  (let ((prev (pset-size set)))
    (if (< prev size)
        (let lp ((new prev))
          (if (< new size)
              (lp (* new 2))
              (let ((old-pollfds (pset-pollfds set))
                    (nfds (poll-set-nfds set))
                    (old-ports (pset-ports set))
                    (new-pollfds (make-bytevector (pollfd-offset new) 0))
                    (new-ports (make-vector new #f)))
                (bytevector-copy! old-pollfds 0 new-pollfds 0
                                  (pollfd-offset nfds))
                (vector-move-left! old-ports 0 nfds new-ports 0)
                (set-pset-pollfds! set new-pollfds)
                (set-pset-ports! set new-ports)))))))

(define (poll-set-find-port set port)
  (let lp ((i 0))
    (if (< i (poll-set-nfds set))
        (if (equal? (vector-ref (pset-ports set) i) port)
            i
            (lp (1+ i)))
        #f)))

(define (poll-set-port set idx)
  (if (< idx (poll-set-nfds set))
      (vector-ref (pset-ports set) idx)
      (error "poll set index out of bounds" set idx)))

(define (poll-set-events set idx)
  (if (< idx (poll-set-nfds set))
      (bytevector-u16-native-ref (pset-pollfds set) (+ (pollfd-offset idx) 4))
      (error "poll set index out of bounds" set idx)))

(define (set-poll-set-events! set idx events)
  (if (< idx (poll-set-nfds set))
      (bytevector-u16-native-set! (pset-pollfds set) (+ (pollfd-offset idx) 4)
                                  events)
      (error "poll set index out of bounds" set idx)))

(define (poll-set-revents set idx)
  (if (< idx (poll-set-nfds set))
      (bytevector-u16-native-ref (pset-pollfds set) (+ (pollfd-offset idx) 6))
      (error "poll set index out of bounds" set idx)))

(define (set-poll-set-revents! set idx revents)
  (if (< idx (poll-set-nfds set))
      (bytevector-u16-native-set! (pset-pollfds set) (+ (pollfd-offset idx) 6)
                                  revents)
      (error "poll set index out of bounds" set idx)))

(define (poll-set-add! set fd-or-port events)
  (let* ((idx (poll-set-nfds set))
         (off (pollfd-offset idx))
         (fd (if (integer? fd-or-port)
                 fd-or-port
                 (fileno fd-or-port))))

    (ensure-pset-size! set (1+ idx))
    (bytevector-s32-native-set! (pset-pollfds set) off fd)
    (bytevector-u16-native-set! (pset-pollfds set) (+ off 4) events)
    (bytevector-u16-native-set! (pset-pollfds set) (+ off 6) 0) ; revents
    (vector-set! (pset-ports set) idx fd-or-port)
    (set-pset-nfds! set (1+ idx))))

(define (poll-set-remove! set idx)
  (if (not (< idx (poll-set-nfds set)))
      (error "poll set index out of bounds" set idx))
  (let ((nfds (poll-set-nfds set))
        (off (pollfd-offset idx))
        (port (vector-ref (pset-ports set) idx)))
    (vector-move-left! (pset-ports set) (1+ idx) nfds
                       (pset-ports set) idx)
    (vector-set! (pset-ports set) (1- nfds) #f)
    (bytevector-copy! (pset-pollfds set) (pollfd-offset (1+ idx))
                      (pset-pollfds set) off
                      (- (pollfd-offset nfds) (pollfd-offset (1+ idx))))
    ;; zero the struct pollfd all at once
    (bytevector-u64-native-set! (pset-pollfds set) (pollfd-offset (1- nfds)) 0)
    (set-pset-nfds! set (1- nfds))
    port))

(define* (poll poll-set #:optional (timeout -1))
  (primitive-poll (pset-pollfds poll-set)
                  (poll-set-nfds poll-set)
                  (pset-ports poll-set)
                  timeout))
