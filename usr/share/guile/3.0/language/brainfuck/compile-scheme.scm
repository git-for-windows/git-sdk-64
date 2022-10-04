;;; Brainfuck for GNU Guile

;; Copyright (C) 2009, 2013 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language brainfuck compile-scheme)
  #:export (compile-scheme))

;; Compilation of Brainfuck to Scheme is pretty straight-forward.  For all of
;; brainfuck's instructions, there are basic representations in Scheme we
;; only have to generate.
;;
;; Brainfuck's pointer and data-tape are stored in the variables pointer and
;; tape, where tape is a vector of integer values initially set to zero.  Pointer
;; starts out at position 0.
;; Our tape is thus of finite length, with an address range of 0..n for
;; some defined upper bound n depending on the length of our tape.


;; Define the length to use for the tape.

(define tape-size 30000)


;; This compiles a whole brainfuck program.  This constructs a Scheme code like:
;; (let ((pointer 0)
;;       (tape (make-vector tape-size 0)))
;;   (begin
;;     <body>
;;     (write-char #\newline)))
;;
;; So first the pointer and tape variables are set up correctly, then the
;; program's body is executed in this context, and finally we output an
;; additional newline character in case the program does not output one.
;;
;; TODO: Find out and explain the details about env, the three return values and
;; how to use the options.  Implement options to set the tape-size, maybe.

(define (compile-scheme exp env opts)
  (values
    `(let ((pointer 0)
           (tape (make-vector ,tape-size 0)))
       ,@(compile-body (cdr exp))
       (write-char #\newline))
    env
    env))


;; Compile a list of instructions to get a list of Scheme codes.  As we always
;; strip off the car of the instructions-list and cons the result onto the
;; result-list, it will get out in reversed order first; so we have to (reverse)
;; it on return.

(define (compile-body instructions)
  (let iterate ((cur instructions)
                (result '()))
    (if (null? cur)
      (reverse result)
      (let ((compiled (compile-instruction (car cur))))
        (iterate (cdr cur) (cons compiled result))))))


;; Compile a single instruction to Scheme, using the direct representations
;; all of Brainfuck's instructions have.

(define (compile-instruction ins)
  (case (car ins)

    ;; Pointer moval >< is done simply by something like:
    ;; (set! pointer (+ pointer +-1))
    ((<bf-move>)
     (let ((dir (cadr ins)))
       `(set! pointer (+ pointer ,dir))))

    ;; Cell increment +- is done as:
    ;; (vector-set! tape pointer (+ (vector-ref tape pointer) +-1))
    ((<bf-increment>)
     (let ((inc (cadr ins)))
       `(vector-set! tape pointer (+ (vector-ref tape pointer) ,inc))))

    ;; Output . is done by converting the cell's integer value to a character
    ;; first and then printing out this character:
    ;; (write-char (integer->char (vector-ref tape pointer)))
    ((<bf-print>)
     '(write-char (integer->char (vector-ref tape pointer))))

    ;; Input , is done similarly, read in a character, get its ASCII code and
    ;; store it into the current cell:
    ;; (vector-set! tape pointer (char->integer (read-char)))
    ((<bf-read>)
     '(vector-set! tape pointer (char->integer (read-char))))

    ;; For loops [...] we use a named let construction to execute the body until
    ;; the current cell gets zero.  The body is compiled via a recursive call
    ;; back to (compile-body).
    ;; (let iterate ()
    ;;   (if (not (= (vector-ref! tape pointer) 0))
    ;;     (begin
    ;;       <body>
    ;;       (iterate))))
    ((<bf-loop>)
     `(let iterate ()
        (if (not (= (vector-ref tape pointer) 0))
          (begin
            ,@(compile-body (cdr ins))
            (iterate)))))

    (else (error "unknown brainfuck instruction " (car ins)))))
