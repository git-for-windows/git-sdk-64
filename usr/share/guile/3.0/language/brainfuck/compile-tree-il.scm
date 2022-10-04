;;; Brainfuck for GNU Guile

;; Copyright (C) 2009, 2011 Free Software Foundation, Inc.

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; Brainfuck is a simple language that mostly mimics the operations of a
;; Turing machine. This file implements a compiler from Brainfuck to
;; Guile's Tree-IL.

;;; Code:

(define-module (language brainfuck compile-tree-il)
  #:use-module (system base pmatch)
  #:use-module (language tree-il)
  #:export (compile-tree-il))

;; Compilation of Brainfuck is pretty straight-forward. For all of
;; brainfuck's instructions, there are basic representations in Tree-IL
;; we only have to generate.
;;
;; Brainfuck's pointer and data-tape are stored in the variables pointer and
;; tape, where tape is a vector of integer values initially set to zero.  Pointer
;; starts out at position 0.
;; Our tape is thus of finite length, with an address range of 0..n for
;; some defined upper bound n depending on the length of our tape.


;; Define the length to use for the tape.

(define tape-size 30000)


;; This compiles a whole brainfuck program. This constructs a Tree-IL
;; code equivalent to Scheme code like this:
;;
;;    (let ((pointer 0)
;;          (tape (make-vector tape-size 0)))
;;      (begin
;;       <body>
;;       (write-char #\newline)))
;;
;; So first the pointer and tape variables are set up correctly, then the
;; program's body is executed in this context, and finally we output an
;; additional newline character in case the program does not output one.
;;
;; The fact that we are compiling to Guile primitives gives this
;; implementation a number of interesting characteristics. First, the
;; values of the tape cells do not underflow or overflow. We could make
;; them do otherwise via compiling calls to "modulo" at certain points.
;;
;; In addition, tape overruns or underruns will be detected, and will
;; throw an error, whereas a number of Brainfuck compilers do not detect
;; this.
;;
;; Note that we're generating the S-expression representation of
;; Tree-IL, then using parse-tree-il to turn it into the actual Tree-IL
;; data structures. This makes the compiler more pleasant to look at,
;; but we do lose is the ability to propagate source information. Since
;; Brainfuck is so obtuse anyway, this shouldn't matter ;-)
;;
;; `compile-tree-il' takes as its input the read expression, the
;; environment, and some compile options. It returns the compiled
;; expression, the environment appropriate for the next pass of the
;; compiler -- in our case, just the environment unchanged -- and the
;; continuation environment.
;;
;; The normal use of a continuation environment is if compiling one
;; expression changes the environment, and that changed environment
;; should be passed to the next compiled expression -- for example,
;; changing the current module. But Brainfuck is incapable of that, so
;; for us, the continuation environment is just the same environment we
;; got in.
;;
;; FIXME: perhaps use options or the env to set the tape-size?

(define (compile-tree-il exp env opts)
  (values
   (parse-tree-il
    `(let (pointer tape) (pointer tape)
          ((const 0)
           (call (primitive make-vector) (const ,tape-size) (const 0)))
          ,(compile-body exp)))
   env
   env))


;; Compile a list of instructions to a Tree-IL expression.

(define (compile-body instructions)
  (let lp ((in instructions) (out '()))
    (define (emit x)
      (lp (cdr in) (cons x out)))
    (cond
     ((null? in)
      ;; No more input, build our output.
      (cond
       ((null? out) '(void))             ; no output
       ((null? (cdr out)) (car out))     ; single expression
       (else `(begin ,@(reverse out))))  ; sequence
      )
     (else
      (pmatch (car in)

        ;; Pointer moves >< are done simply by something like:
        ;;   (set! pointer (+ pointer +-1))
        ((<bf-move> ,dir)
         (emit `(set! (lexical pointer)
                      (call (primitive +) (lexical pointer) (const ,dir)))))

        ;; Cell increment +- is done as:
        ;;   (vector-set! tape pointer (+ (vector-ref tape pointer) +-1))
        ((<bf-increment> ,inc) 
         (emit `(call (primitive vector-set!) (lexical tape) (lexical pointer)
                      (call (primitive +)
                            (call (primitive vector-ref)
                                  (lexical tape) (lexical pointer))
                            (const ,inc)))))

        ;; Output . is done by converting the cell's integer value to a
        ;; character first and then printing out this character:
        ;;   (write-char (integer->char (vector-ref tape pointer)))
        ((<bf-print>) 
         (emit `(call (primitive write-char)
                      (call (primitive integer->char)
                            (call (primitive vector-ref)
                                  (lexical tape) (lexical pointer))))))

        ;; Input , is done similarly, read in a character, get its ASCII
        ;; code and store it into the current cell:
        ;;   (vector-set! tape pointer (char->integer (read-char)))
        ((<bf-read>) 
         (emit `(call (primitive vector-set!)
                      (lexical tape) (lexical pointer)
                      (call (primitive char->integer)
                            (call (primitive read-char))))))

        ;; For loops [...] we use a letrec construction to execute the body until
        ;; the current cell gets zero.  The body is compiled via a recursive call
        ;; back to (compile-body).
        ;;   (let iterate ()
        ;;     (if (not (= (vector-ref! tape pointer) 0))
        ;;         (begin
        ;;          <body>
        ;;          (iterate))))
        ;;
        ;; Indeed, letrec is the only way we have to loop in Tree-IL.
        ;; Note that this does not mean that the closure must actually
        ;; be created; later passes can compile tail-recursive letrec
        ;; calls into inline code with gotos. Admittedly, that part of
        ;; the compiler is not yet in place, but it will be, and in the
        ;; meantime the code is still reasonably efficient.
        ((<bf-loop> . ,body)
         (let ((iterate (gensym)))
           (emit `(letrec (iterate) (,iterate)
                          ((lambda ()
                             (lambda-case
                              ((() #f #f #f () ())
                               (if (call (primitive =)
                                         (call (primitive vector-ref)
                                               (lexical tape) (lexical pointer))
                                         (const 0))
                                   (void)
                                   (begin ,(compile-body body)
                                          (call (lexical ,iterate)))))
                              #f)))
                          (call (lexical ,iterate))))))

        (else (error "unknown brainfuck instruction" (car in))))))))
