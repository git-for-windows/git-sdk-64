;;; Guile bytecode assembler

;;; Copyright (C) 2001, 2009, 2010, 2012, 2013, 2014, 2015 Free Software Foundation, Inc.
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

;;; Commentary:
;;;
;;; This module implements an assembler that creates an ELF image from
;;; bytecode assembly and macro-assembly.  The input can be given in
;;; s-expression form, like ((OP ARG ...) ...).  Internally there is a
;;; procedural interface, the emit-OP procedures, but that is not
;;; currently exported.
;;;
;;; "Primitive instructions" correspond to VM operations.  Assemblers
;;; for primitive instructions are generated programmatically from
;;; (instruction-list), which itself is derived from the VM sources.
;;; There are also "macro-instructions" like "label" or "load-constant"
;;; that expand to 0 or more primitive instructions.
;;;
;;; The assembler also handles some higher-level tasks, like creating
;;; the symbol table, other metadata sections, creating a constant table
;;; for the whole compilation unit, and writing the dynamic section of
;;; the ELF file along with the appropriate initialization routines.
;;;
;;; Most compilers will want to use the trio of make-assembler,
;;; emit-text, and link-assembly.  That will result in the creation of
;;; an ELF image as a bytevector, which can then be loaded using
;;; load-thunk-from-memory, or written to disk as a .go file.
;;;
;;; Code:

(define-module (system vm assembler)
  #:use-module (system base target)
  #:use-module (system vm dwarf)
  #:use-module (system vm elf)
  #:use-module (system vm linker)
  #:use-module (system syntax internal)
  #:use-module (language bytecode)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:export (make-assembler

            (emit-receive* . emit-receive)
            (emit-mov* . emit-mov)
            (emit-fmov* . emit-fmov)

            emit-call
            emit-call-label
            emit-tail-call
            emit-tail-call-label
            emit-receive-values
            emit-return
            emit-return-values
            emit-call/cc
            emit-abort
            emit-builtin-ref
            emit-br-if-nargs-ne
            emit-br-if-nargs-lt
            emit-br-if-nargs-gt
            emit-assert-nargs-ee
            emit-assert-nargs-ge
            emit-assert-nargs-le
            emit-alloc-frame
            emit-reset-frame
            emit-assert-nargs-ee/locals
            emit-br-if-npos-gt
            emit-bind-kwargs
            emit-bind-rest
            emit-br
            emit-br-if-true
            emit-br-if-null
            emit-br-if-nil
            emit-br-if-pair
            emit-br-if-struct
            emit-br-if-char
            emit-br-if-tc7
            emit-br-if-eq
            emit-br-if-eqv
            emit-br-if-=
            emit-br-if-<
            emit-br-if-<=
            emit-br-if-logtest
            emit-br-if-u64-=
            emit-br-if-u64-<
            emit-br-if-u64-<=
            emit-br-if-u64-<-scm
            emit-br-if-u64-<=-scm
            emit-br-if-u64-=-scm
            emit-br-if-u64->=-scm
            emit-br-if-u64->-scm
            emit-br-if-f64-=
            emit-br-if-f64-<
            emit-br-if-f64-<=
            emit-br-if-f64->
            emit-br-if-f64->=
            emit-box
            emit-box-ref
            emit-box-set!
            emit-make-closure
            emit-free-ref
            emit-free-set!
            emit-current-module
            emit-resolve
            emit-define!
            emit-toplevel-box
            emit-module-box
            emit-prompt
            emit-wind
            emit-unwind
            emit-push-fluid
            emit-pop-fluid
            emit-push-dynamic-state
            emit-pop-dynamic-state
            emit-current-thread
            emit-fluid-ref
            emit-fluid-set!
            emit-string-length
            emit-string-ref
            emit-string-set!
            emit-string->number
            emit-string->symbol
            emit-symbol->keyword
            emit-cons
            emit-car
            emit-cdr
            emit-set-car!
            emit-set-cdr!
            emit-add
            emit-add/immediate
            emit-sub
            emit-sub/immediate
            emit-mul
            emit-div
            emit-quo
            emit-rem
            emit-mod
            emit-ash
            emit-fadd
            emit-fsub
            emit-fmul
            emit-fdiv
            emit-uadd
            emit-usub
            emit-umul
            emit-uadd/immediate
            emit-usub/immediate
            emit-umul/immediate
            emit-logand
            emit-logior
            emit-logxor
            emit-logsub
            emit-ulogand
            emit-ulogior
            emit-ulogxor
            emit-ulogsub
            emit-ursh
            emit-ulsh
            emit-ursh/immediate
            emit-ulsh/immediate
            emit-char->integer
            emit-integer->char
            emit-make-vector
            emit-make-vector/immediate
            emit-vector-length
            emit-vector-ref
            emit-vector-ref/immediate
            emit-vector-set!
            emit-vector-set!/immediate
            emit-struct-vtable
            emit-allocate-struct/immediate
            emit-struct-ref/immediate
            emit-struct-set!/immediate
            emit-allocate-struct
            emit-struct-ref
            emit-struct-set!
            emit-class-of
            emit-make-array
            emit-scm->f64
            emit-load-f64
            emit-f64->scm
            emit-scm->u64
            emit-scm->u64/truncate
            emit-load-u64
            emit-u64->scm
            emit-scm->s64
            emit-load-s64
            emit-s64->scm
            emit-bv-length
            emit-bv-u8-ref
            emit-bv-s8-ref
            emit-bv-u16-ref
            emit-bv-s16-ref
            emit-bv-u32-ref
            emit-bv-s32-ref
            emit-bv-u64-ref
            emit-bv-s64-ref
            emit-bv-f32-ref
            emit-bv-f64-ref
            emit-bv-u8-set!
            emit-bv-s8-set!
            emit-bv-u16-set!
            emit-bv-s16-set!
            emit-bv-u32-set!
            emit-bv-s32-set!
            emit-bv-u64-set!
            emit-bv-s64-set!
            emit-bv-f32-set!
            emit-bv-f64-set!
            emit-make-atomic-box
            emit-atomic-box-ref
            emit-atomic-box-set!
            emit-atomic-box-swap!
            emit-atomic-box-compare-and-swap!
            emit-handle-interrupts

            emit-text
            link-assembly))




;; Like define-inlinable, but only for first-order uses of the defined
;; routine.  Should residualize less code.
(eval-when (expand)
  (define-syntax define-inline
    (lambda (x)
      (syntax-case x ()
        ((_ (name arg ...) body ...)
         (with-syntax (((temp ...) (generate-temporaries #'(arg ...))))
           #`(eval-when (expand)
               (define-syntax-rule (name temp ...)
                 (let ((arg temp) ...)
                   body ...)))))))))

;;; Bytecode consists of 32-bit units, often subdivided in some way.
;;; These helpers create one 32-bit unit from multiple components.

(define-inline (check-urange x mask)
  (let ((x* (logand x mask)))
    (unless (= x x*)
      (error "out of range" x))
    x*))

(define-inline (check-srange x mask)
  (let ((x* (logand x mask)))
    (unless (if (negative? x)
                (= (+ x mask 1) x*)
                (= x x*))
      (error "out of range" x))
    x*))

(define-inline (pack-u8-u24 x y)
  (let ((x (check-urange x #xff))
        (y (check-urange y #xffffff)))
    (logior x (ash y 8))))

(define-inline (pack-u8-s24 x y)
  (let ((x (check-urange x #xff))
        (y (check-srange y #xffffff)))
    (logior x (ash y 8))))

(define-inline (pack-u1-u7-u24 x y z)
  (let ((x (check-urange x #x1))
        (y (check-urange y #x7f))
        (z (check-urange z #xffffff)))
    (logior x (ash y 1) (ash z 8))))

(define-inline (pack-u8-u12-u12 x y z)
  (let ((x (check-urange x #xff))
        (y (check-urange y #xfff))
        (z (check-urange z #xfff)))
    (logior x (ash y 8) (ash z 20))))

(define-inline (pack-u8-u8-u16 x y z)
  (let ((x (check-urange x #xff))
        (y (check-urange y #xff))
        (z (check-urange z #xffff)))
    (logior x (ash y 8) (ash z 16))))

(define-inline (pack-u8-u8-u8-u8 x y z w)
  (let ((x (check-urange x #xff))
        (y (check-urange y #xff))
        (z (check-urange z #xff))
        (w (check-urange w #xff)))
    (logior x (ash y 8) (ash z 16) (ash w 24))))

(eval-when (expand)
  (define-syntax pack-flags
    (syntax-rules ()
      ;; Add clauses as needed.
      ((pack-flags f1 f2) (logior (if f1 (ash 1 0) 0)
                                  (if f2 (ash 1 1) 0))))))


(define-syntax-rule (define-byte-order-swapper name size ref set)
  (define* (name buf #:optional (start 0) (end (bytevector-length buf)))
    "Patch up the text buffer @var{buf}, swapping the endianness of each
N-byte unit."
    (unless (zero? (modulo (- end start) size))
      (error "unexpected length"))
    (let lp ((pos start))
      (when (< pos end)
        (set buf pos (ref buf pos (endianness big)) (endianness little))
        (lp (+ pos size))))))

(define-byte-order-swapper byte-swap/2!
  2 bytevector-u16-ref bytevector-u16-set!)
(define-byte-order-swapper byte-swap/4!
  4 bytevector-u32-ref bytevector-u32-set!)
(define-byte-order-swapper byte-swap/8!
  8 bytevector-u64-ref bytevector-u64-set!)



;;; A <meta> entry collects metadata for one procedure.  Procedures are
;;; written as contiguous ranges of bytecode.
;;;
(eval-when (expand)
  (define-syntax-rule (assert-match arg pattern kind)
    (let ((x arg))
      (unless (match x (pattern #t) (_ #f))
        (error (string-append "expected " kind) x)))))

(define-record-type <meta>
  (%make-meta label properties low-pc high-pc arities)
  meta?
  (label meta-label)
  (properties meta-properties set-meta-properties!)
  (low-pc meta-low-pc)
  (high-pc meta-high-pc set-meta-high-pc!)
  (arities meta-arities set-meta-arities!))

(define (make-meta label properties low-pc)
  (assert-match label (or (? exact-integer?) (? symbol?)) "symbol")
  (assert-match properties (((? symbol?) . _) ...) "alist with symbolic keys")
  (%make-meta label properties low-pc #f '()))

(define (meta-name meta)
  (assq-ref (meta-properties meta) 'name))

;; Metadata for one <lambda-case>.
(define-record-type <arity>
  (make-arity req opt rest kw-indices allow-other-keys?
              low-pc high-pc definitions)
  arity?
  (req arity-req)
  (opt arity-opt)
  (rest arity-rest)
  (kw-indices arity-kw-indices)
  (allow-other-keys? arity-allow-other-keys?)
  (low-pc arity-low-pc)
  (high-pc arity-high-pc set-arity-high-pc!)
  (definitions arity-definitions set-arity-definitions!))

;;; An assembler collects all of the words emitted during assembly, and
;;; also maintains ancillary information such as the constant table, a
;;; relocation list, and so on.
;;;
;;; Bytecode consists of 32-bit units.  We emit bytecode using native
;;; endianness.  If we're targeting a foreign endianness, we byte-swap
;;; the bytevector as a whole instead of conditionalizing each access.
;;;
(define-record-type <asm>
  (make-asm buf pos start
            labels relocs
            word-size endianness
            constants inits
            shstrtab next-section-number
            meta sources
            slot-maps)
  asm?

  ;; We write bytecode into a bytevector, growing the bytevector as
  ;; needed.  asm-cur is that bytevector, and asm-pos is the byte offset
  ;; into the vector at which the next word should be written.
  ;;
  (buf asm-buf set-asm-buf!)
  (pos asm-pos set-asm-pos!)

  ;; asm-start is an absolute position, indicating the byte offset of
  ;; the beginning of an instruction.  It is updated after writing all
  ;; the words for one primitive instruction.  It models the position of
  ;; the instruction pointer during execution, given that the VM updates
  ;; the IP only at the end of executing the instruction, and is thus
  ;; useful for computing offsets between two points in a program.
  ;;
  (start asm-start set-asm-start!)

  ;; An alist of symbol -> position pairs, indicating the labels defined
  ;; in this compilation unit.
  ;;
  (labels asm-labels set-asm-labels!)

  ;; A list of relocations needed by the program text.  We use an
  ;; internal representation for relocations, and handle textualn
  ;; relative relocations in the assembler.  Other kinds of relocations
  ;; are later reified as linker relocations and resolved by the linker.
  ;;
  (relocs asm-relocs set-asm-relocs!)

  ;; Target information.
  ;;
  (word-size asm-word-size)
  (endianness asm-endianness)

  ;; The constant table, as a vhash of object -> label.  All constants
  ;; get de-duplicated and written into separate sections -- either the
  ;; .rodata section, for read-only data, or .data, for constants that
  ;; need initialization at load-time (like symbols).  Constants can
  ;; depend on other constants (e.g. a symbol depending on a stringbuf),
  ;; so order in this table is important.
  ;;
  (constants asm-constants set-asm-constants!)

  ;; A list of instructions needed to initialize the constants.  Will
  ;; run in a thunk with 2 local variables.
  ;;
  (inits asm-inits set-asm-inits!)

  ;; The shstrtab, for section names.
  ;;
  (shstrtab asm-shstrtab set-asm-shstrtab!)

  ;; The section number for the next section to be written.
  ;;
  (next-section-number asm-next-section-number set-asm-next-section-number!)

  ;; A list of <meta>, corresponding to procedure metadata.
  ;;
  (meta asm-meta set-asm-meta!)

  ;; A list of (pos . source) pairs, indicating source information.  POS
  ;; is relative to the beginning of the text section, and SOURCE is in
  ;; the same format that source-properties returns.
  ;;
  (sources asm-sources set-asm-sources!)

  ;; A list of (pos . slot-map) pairs, indicating slot maps.  POS is
  ;; relative to the beginning of the text section.  SLOT-MAP is a
  ;; bitfield describing the stack at call sites, as an integer.
  ;;
  (slot-maps asm-slot-maps set-asm-slot-maps!))

(define* (make-assembler #:key (word-size (target-word-size))
                         (endianness (target-endianness)))
  "Create an assembler for a given target @var{word-size} and
@var{endianness}, falling back to appropriate values for the configured
target."
  (make-asm (make-u32vector 1000) 0 0
            (make-hash-table) '()
            word-size endianness
            vlist-null '()
            (make-string-table) 1
            '() '() '()))

(define (intern-section-name! asm string)
  "Add a string to the section name table (shstrtab)."
  (string-table-intern! (asm-shstrtab asm) string))

(define (grow-buffer! asm)
  "Grow the code buffer of the asm."
  (let* ((buf (asm-buf asm))
         (len (bytevector-length buf))
         (new (make-u32vector (ash len -1) 0)))
    (bytevector-copy! buf 0 new 0 len)
    (set-asm-buf! asm new)
    #f))

(define-inline (emit asm u32)
  "Emit one 32-bit word into the instruction stream.  Assumes that there
is space for the word."
  (bytevector-u32-native-set! (asm-buf asm) (asm-pos asm) u32)
  (set-asm-pos! asm (+ (asm-pos asm) 4)))

(define-inline (make-reloc type label base word)
  "Make an internal relocation of type @var{type} referencing symbol
@var{label}, @var{word} words after position @var{start}.  @var{type}
may be x8-s24, indicating a 24-bit relative label reference that can be
fixed up by the assembler, or s32, indicating a 32-bit relative
reference that needs to be fixed up by the linker."
  (list type label base word))

(define-inline (reset-asm-start! asm)
  "Reset the asm-start after writing the words for one instruction."
  (set-asm-start! asm (asm-pos asm)))

(define (record-label-reference asm label)
  "Record an x8-s24 local label reference.  This value will get patched
up later by the assembler."
  (let* ((start (asm-start asm))
         (pos (asm-pos asm))
         (reloc (make-reloc 'x8-s24 label start (- pos start))))
    (set-asm-relocs! asm (cons reloc (asm-relocs asm)))))

(define* (record-far-label-reference asm label #:optional (offset 0))
  "Record an s32 far label reference.  This value will get patched up
later by the linker."
  (let* ((start (- (asm-start asm) offset))
         (pos (asm-pos asm))
         (reloc (make-reloc 's32 label start (- pos start))))
    (set-asm-relocs! asm (cons reloc (asm-relocs asm)))))




;;;
;;; Primitive assemblers are defined by expanding `assembler' for each
;;; opcode in `(instruction-list)'.
;;;

(eval-when (expand)
  (define (id-append ctx a b)
    (datum->syntax ctx (symbol-append (syntax->datum a) (syntax->datum b))))

  (define-syntax encoder
    (lambda (x)
      (define-syntax op-case
        (lambda (x)
          (syntax-case x ()
            ((_ asm name ((type arg ...) code ...) clause ...)
             #`(if (eq? name 'type)
                   (with-syntax (((arg ...) (generate-temporaries #'(arg ...))))
                     #'((arg ...)
                        code ...))
                   (op-case asm name clause ...)))
            ((_ asm name)
             #'(error "unmatched name" name)))))

      (define (pack-first-word asm opcode type)
        (with-syntax ((opcode opcode))
          (op-case
           asm type
           ((X32)
            (emit asm opcode))
           ((X8_S24 arg)
            (emit asm (pack-u8-u24 opcode arg)))
           ((X8_F24 arg)
            (emit asm (pack-u8-u24 opcode arg)))
           ((X8_C24 arg)
            (emit asm (pack-u8-u24 opcode arg)))
           ((X8_L24 label)
            (record-label-reference asm label)
            (emit asm opcode))
           ((X8_S8_I16 a imm)
            (emit asm (pack-u8-u8-u16 opcode a (immediate-bits asm imm))))
           ((X8_S12_S12 a b)
            (emit asm (pack-u8-u12-u12 opcode a b)))
           ((X8_S12_C12 a b)
            (emit asm (pack-u8-u12-u12 opcode a b)))
           ((X8_C12_C12 a b)
            (emit asm (pack-u8-u12-u12 opcode a b)))
           ((X8_F12_F12 a b)
            (emit asm (pack-u8-u12-u12 opcode a b)))
           ((X8_S8_S8_S8 a b c)
            (emit asm (pack-u8-u8-u8-u8 opcode a b c)))
           ((X8_S8_S8_C8 a b c)
            (emit asm (pack-u8-u8-u8-u8 opcode a b c)))
           ((X8_S8_C8_S8 a b c)
            (emit asm (pack-u8-u8-u8-u8 opcode a b c))))))

      (define (pack-tail-word asm type)
        (op-case
         asm type
         ((C32 a)
          (emit asm a))
         ((I32 imm)
          (let ((val (immediate-bits asm imm)))
            (emit asm val)))
         ((A32 imm)
          (unless (= (asm-word-size asm) 8)
            (error "make-long-immediate unavailable for this target"))
          (let ((bits (immediate-bits asm imm)))
            (emit asm (ash bits -32))
            (emit asm (logand bits (1- (ash 1 32))))))
         ((AF32 f64)
          (let ((u64 (u64vector-ref (f64vector f64) 0)))
            (emit asm (ash u64 -32))
            (emit asm (logand u64 (1- (ash 1 32))))))
         ((AU32 u64)
          (emit asm (ash u64 -32))
          (emit asm (logand u64 (1- (ash 1 32)))))
         ((AS32 s64)
          (let ((u64 (u64vector-ref (s64vector s64) 0)))
            (emit asm (ash u64 -32))
            (emit asm (logand u64 (1- (ash 1 32))))))
         ((B32))
         ((BU32))
         ((BS32))
         ((BF32))
         ((N32 label)
          (record-far-label-reference asm label)
          (emit asm 0))
         ((R32 label)
          (record-far-label-reference asm label)
          (emit asm 0))
         ((L32 label)
          (record-far-label-reference asm label)
          (emit asm 0))
         ((LO32 label offset)
          (record-far-label-reference asm label
                                      (* offset (asm-word-size asm)))
          (emit asm 0))
         ((C8_C24 a b)
          (emit asm (pack-u8-u24 a b)))
         ((B1_X7_L24 a label)
          (record-label-reference asm label)
          (emit asm (pack-u1-u7-u24 (if a 1 0) 0 0)))
         ((B1_C7_L24 a b label)
          (record-label-reference asm label)
          (emit asm (pack-u1-u7-u24 (if a 1 0) b 0)))
         ((B1_X31 a)
          (emit asm (pack-u1-u7-u24 (if a 1 0) 0 0)))
         ((B1_X7_S24 a b)
          (emit asm (pack-u1-u7-u24 (if a 1 0) 0 b)))
         ((B1_X7_F24 a b)
          (emit asm (pack-u1-u7-u24 (if a 1 0) 0 b)))
         ((B1_X7_C24 a b)
          (emit asm (pack-u1-u7-u24 (if a 1 0) 0 b)))
         ((X8_S24 a)
          (emit asm (pack-u8-u24 0 a)))
         ((X8_F24 a)
          (emit asm (pack-u8-u24 0 a)))
         ((X8_C24 a)
          (emit asm (pack-u8-u24 0 a)))
         ((X8_L24 label)
          (record-label-reference asm label)
          (emit asm 0))))

      (syntax-case x ()
        ((_ word0 word* ...)
         (with-syntax ((((formal0 ...)
                         code0 ...)
                        (pack-first-word #'asm #'opcode
                                         (syntax->datum #'word0)))
                       ((((formal* ...)
                          code* ...) ...)
                        (map (lambda (word) (pack-tail-word #'asm word))
                             (syntax->datum #'(word* ...)))))
           ;; The opcode is the last argument, so that assemblers don't
           ;; have to shuffle their arguments before tail-calling an
           ;; encoder.
           #'(lambda (asm formal0 ... formal* ... ... opcode)
               (let lp ()
                 (let ((words (length '(word0 word* ...))))
                   (unless (<= (+ (asm-pos asm) (* 4 words))
                               (bytevector-length (asm-buf asm)))
                     (grow-buffer! asm)
                     (lp))))
               code0 ...
               code* ... ...
               (reset-asm-start! asm)))))))

  (define (encoder-name operands)
    (let lp ((operands operands) (out #'encode))
      (syntax-case operands ()
        (() out)
        ((operand . operands)
         (lp #'operands
             (id-append #'operand (id-append out out #'-) #'operand))))))

  (define-syntax define-encoder
    (lambda (x)
      (syntax-case x ()
        ((_ operand ...)
         (with-syntax ((encode (encoder-name #'(operand ...))))
           #'(define encode (encoder operand ...)))))))

  (define-syntax visit-instruction-kinds
    (lambda (x)
      (syntax-case x ()
        ((visit-instruction-kinds macro arg ...)
         (with-syntax (((operands ...)
                        (delete-duplicates
                         (map (match-lambda
                                ((name opcode kind . operands)
                                 (datum->syntax #'macro operands)))
                              (instruction-list)))))
           #'(begin
               (macro arg ... . operands)
               ...)))))))

(visit-instruction-kinds define-encoder)

;; In Guile's VM, locals are usually addressed via the stack pointer
;; (SP).  There can be up to 2^24 slots for local variables in a
;; frame.  Some instructions encode their operands using a restricted
;; subset of the full 24-bit local address space, in order to make the
;; bytecode more dense in the usual case that a function needs few
;; local slots.  To allow these instructions to be used when there are
;; many local slots, we can temporarily push the values on the stack,
;; operate on them there, and then store back any result as we pop the
;; SP to its original position.
;;
;; We implement this shuffling via wrapper encoders that have the same
;; arity as the encoder they wrap, e.g. encode-X8_S12_S12/shuffle that
;; wraps encode-X8_S12_S12.  We make the emit-cons public interface
;; use the shuffling encoder.  That way we solve the problem fully and
;; in just one place.

(define (encode-X8_S12_S12!/shuffle asm a b opcode)
  (cond
   ((< (logior a b) (ash 1 12))
    (encode-X8_S12_S12 asm a b opcode))
   (else
    (emit-push asm a)
    (emit-push asm (1+ b))
    (encode-X8_S12_S12 asm 1 0 opcode)
    (emit-drop asm 2))))
(define (encode-X8_S12_S12<-/shuffle asm dst a opcode)
  (cond
   ((< (logior dst a) (ash 1 12))
    (encode-X8_S12_S12 asm dst a opcode))
   (else
    (emit-push asm a)
    (encode-X8_S12_S12 asm 0 0 opcode)
    (emit-pop asm dst))))
(define (encode-X8_S12_S12-X8_C24!/shuffle asm a b c opcode)
  (cond
   ((< (logior a b) (ash 1 12))
    (encode-X8_S12_S12-X8_C24 asm a b c opcode))
   (else
    (emit-push asm a)
    (emit-push asm (1+ b))
    (encode-X8_S12_S12-X8_C24 asm 1 0 c opcode)
    (emit-drop asm 2))))
(define (encode-X8_S12_S12-X8_C24<-/shuffle asm dst a const opcode)
  (cond
   ((< (logior dst a) (ash 1 12))
    (encode-X8_S12_S12-X8_C24 asm dst a const opcode))
   (else
    (emit-push asm a)
    (encode-X8_S12_S12-X8_C24 asm 0 0 const opcode)
    (emit-pop asm dst))))
(define (encode-X8_S12_C12<-/shuffle asm dst const opcode)
  (cond
   ((< dst (ash 1 12))
    (encode-X8_S12_C12 asm dst const opcode))
   (else
    ;; Push garbage value to make space for dst.
    (emit-push asm dst)
    (encode-X8_S12_C12 asm 0 const opcode)
    (emit-pop asm dst))))
(define (encode-X8_S8_I16<-/shuffle asm dst imm opcode)
  (cond
   ((< dst (ash 1 8))
    (encode-X8_S8_I16 asm dst imm opcode))
   (else
    ;; Push garbage value to make space for dst.
    (emit-push asm dst)
    (encode-X8_S8_I16 asm 0 imm opcode)
    (emit-pop asm dst))))
(define (encode-X8_S8_S8_S8!/shuffle asm a b c opcode)
  (cond
   ((< (logior a b c) (ash 1 8))
    (encode-X8_S8_S8_S8 asm a b c opcode))
   (else
    (emit-push asm a)
    (emit-push asm (+ b 1))
    (emit-push asm (+ c 2))
    (encode-X8_S8_S8_S8 asm 2 1 0 opcode)
    (emit-drop asm 3))))
(define (encode-X8_S8_S8_S8<-/shuffle asm dst a b opcode)
  (cond
   ((< (logior dst a b) (ash 1 8))
    (encode-X8_S8_S8_S8 asm dst a b opcode))
   (else
    (emit-push asm a)
    (emit-push asm (1+ b))
    (encode-X8_S8_S8_S8 asm 1 1 0 opcode)
    (emit-drop asm 1)
    (emit-pop asm dst))))
(define (encode-X8_S8_S8_C8<-/shuffle asm dst a const opcode)
  (cond
   ((< (logior dst a) (ash 1 8))
    (encode-X8_S8_S8_C8 asm dst a const opcode))
   (else
    (emit-push asm a)
    (encode-X8_S8_S8_C8 asm 0 0 const opcode)
    (emit-pop asm dst))))
(define (encode-X8_S8_C8_S8!/shuffle asm a const b opcode)
  (cond
   ((< (logior a b) (ash 1 8))
    (encode-X8_S8_C8_S8 asm a const b opcode))
   (else
    (emit-push asm a)
    (emit-push asm (1+ b))
    (encode-X8_S8_C8_S8 asm 1 const 0 opcode)
    (emit-drop asm 2))))
(define (encode-X8_S8_C8_S8<-/shuffle asm dst const a opcode)
  (cond
   ((< (logior dst a) (ash 1 8))
    (encode-X8_S8_C8_S8 asm dst const a opcode))
   (else
    (emit-push asm a)
    (encode-X8_S8_C8_S8 asm 0 const 0 opcode)
    (emit-pop asm dst))))

(eval-when (expand)
  (define (id-append ctx a b)
    (datum->syntax ctx (symbol-append (syntax->datum a) (syntax->datum b))))

  (define (shuffling-encoder-name kind operands)
    (match (cons (syntax->datum kind) (syntax->datum operands))
      (('! 'X8_S12_S12)          #'encode-X8_S12_S12!/shuffle)
      (('<- 'X8_S12_S12)         #'encode-X8_S12_S12<-/shuffle)
      (('! 'X8_S12_S12 'X8_C24)  #'encode-X8_S12_S12-X8_C24!/shuffle)
      (('<- 'X8_S12_S12 'X8_C24) #'encode-X8_S12_S12-X8_C24<-/shuffle)
      (('<- 'X8_S12_C12)         #'encode-X8_S12_C12<-/shuffle)
      (('<- 'X8_S8_I16)          #'encode-X8_S8_I16<-/shuffle)
      (('! 'X8_S8_S8_S8)         #'encode-X8_S8_S8_S8!/shuffle)
      (('<- 'X8_S8_S8_S8)        #'encode-X8_S8_S8_S8<-/shuffle)
      (('<- 'X8_S8_S8_C8)        #'encode-X8_S8_S8_C8<-/shuffle)
      (('! 'X8_S8_C8_S8)         #'encode-X8_S8_C8_S8!/shuffle)
      (('<- 'X8_S8_C8_S8)        #'encode-X8_S8_C8_S8<-/shuffle)
      (else (encoder-name operands))))

  (define-syntax assembler
    (lambda (x)
      (define (word-args word)
        (match word
          ('C32 #'(a))
          ('I32 #'(imm))
          ('A32 #'(imm))
          ('AF32 #'(f64))
          ('AU32 #'(u64))
          ('AS32 #'(s64))
          ('B32 #'())
          ('BU32 #'())
          ('BS32 #'())
          ('BF32 #'())
          ('N32 #'(label))
          ('R32 #'(label))
          ('L32 #'(label))
          ('LO32 #'(label offset))
          ('C8_C24 #'(a b))
          ('B1_X7_L24 #'(a label))
          ('B1_C7_L24 #'(a b label))
          ('B1_X31 #'(a))
          ('B1_X7_S24 #'(a b))
          ('B1_X7_F24 #'(a b))
          ('B1_X7_C24 #'(a b))
          ('X8_S24 #'(arg))
          ('X8_F24 #'(arg))
          ('X8_C24 #'(arg))
          ('X8_L24 #'(label))
          ('X8_S8_I16 #'(a imm))
          ('X8_S12_S12 #'(a b))
          ('X8_S12_C12 #'(a b))
          ('X8_C12_C12 #'(a b))
          ('X8_F12_F12 #'(a b))
          ('X8_S8_S8_S8 #'(a b c))
          ('X8_S8_S8_C8 #'(a b c))
          ('X8_S8_C8_S8 #'(a b c))
          ('X32 #'())))

      (syntax-case x ()
        ((_ name opcode kind word ...)
         (with-syntax (((formal ...)
                        (generate-temporaries
                         (append-map word-args (syntax->datum #'(word ...)))))
                       (encode (shuffling-encoder-name #'kind #'(word ...))))
           #'(lambda (asm formal ...)
               (encode asm formal ... opcode))))))))

(define assemblers (make-hash-table))

(eval-when (expand)
  (define-syntax define-assembler
    (lambda (x)
      (syntax-case x ()
        ((_ name opcode kind arg ...)
         (with-syntax ((emit (id-append #'name #'emit- #'name)))
           #'(define emit
               (let ((emit (assembler name opcode kind arg ...)))
                 (hashq-set! assemblers 'name emit)
                 emit)))))))

  (define-syntax visit-opcodes
    (lambda (x)
      (syntax-case x ()
        ((visit-opcodes macro arg ...)
         (with-syntax (((inst ...)
                        (map (lambda (x) (datum->syntax #'macro x))
                             (instruction-list))))
           #'(begin
               (macro arg ... . inst)
               ...)))))))

(visit-opcodes define-assembler)

;; Shuffling is a general mechanism to get around address space
;; limitations for SP-relative variable references.  FP-relative
;; variables need special support.  Also, some instructions like `mov'
;; have multiple variations with different addressing limits.

(define (emit-mov* asm dst src)
  (if (and (< dst (ash 1 12)) (< src (ash 1 12)))
      (emit-mov asm dst src)
      (emit-long-mov asm dst src)))

(define (emit-fmov* asm dst src)
  (emit-long-fmov asm dst src))

(define (emit-receive* asm dst proc nlocals)
  (if (and (< dst (ash 1 12)) (< proc (ash 1 12)))
      (emit-receive asm dst proc nlocals)
      (begin
        (emit-receive-values asm proc #t 1)
        (emit-fmov* asm dst (1+ proc))
        (emit-reset-frame asm nlocals))))

(define (emit-text asm instructions)
  "Assemble @var{instructions} using the assembler @var{asm}.
@var{instructions} is a sequence of instructions, expressed as a list of
lists.  This procedure can be called many times before calling
@code{link-assembly}."
  (for-each (lambda (inst)
              (apply (or (hashq-ref assemblers (car inst))
                         (error 'bad-instruction inst))
                     asm
                     (cdr inst)))
            instructions))



;;;
;;; The constant table records a topologically sorted set of literal
;;; constants used by a program.  For example, a pair uses its car and
;;; cdr, a string uses its stringbuf, etc.
;;;
;;; Some things we want to add to the constant table are not actually
;;; Scheme objects: for example, stringbufs, cache cells for toplevel
;;; references, or cache cells for non-closure procedures.  For these we
;;; define special record types and add instances of those record types
;;; to the table.
;;;

(define (immediate-bits asm x)
  "Return the bit pattern to write into the buffer if @var{x} is
immediate, and @code{#f} otherwise."
  (define tc2-int 2)
  (if (exact-integer? x)
      ;; Object is an immediate if it is a fixnum on the target.
      (call-with-values (lambda ()
                          (case (asm-word-size asm)
                            ((4) (values    (- #x20000000)
                                            #x1fffffff))
                            ((8) (values    (- #x2000000000000000)
                                            #x1fffffffFFFFFFFF))
                            (else (error "unexpected word size"))))
        (lambda (fixnum-min fixnum-max)
          (and (<= fixnum-min x fixnum-max)
               (let ((fixnum-bits (if (negative? x)
                                      (+ fixnum-max 1 (logand x fixnum-max))
                                      x)))
                 (logior (ash fixnum-bits 2) tc2-int)))))
      ;; Otherwise, the object will be immediate on the target if and
      ;; only if it is immediate on the host.  Except for integers,
      ;; which we handle specially above, any immediate value is an
      ;; immediate on both 32-bit and 64-bit targets.
      (let ((bits (object-address x)))
        (and (not (zero? (logand bits 6)))
             bits))))

(define-record-type <stringbuf>
  (make-stringbuf string)
  stringbuf?
  (string stringbuf-string))

(define-record-type <static-procedure>
  (make-static-procedure code)
  static-procedure?
  (code static-procedure-code))

(define-record-type <uniform-vector-backing-store>
  (make-uniform-vector-backing-store bytes element-size)
  uniform-vector-backing-store?
  (bytes uniform-vector-backing-store-bytes)
  (element-size uniform-vector-backing-store-element-size))

(define-record-type <cache-cell>
  (make-cache-cell scope key)
  cache-cell?
  (scope cache-cell-scope)
  (key cache-cell-key))

(define (simple-vector? obj)
  (and (vector? obj)
       (equal? (array-shape obj) (list (list 0 (1- (vector-length obj)))))))

(define (simple-uniform-vector? obj)
  (and (array? obj)
       (symbol? (array-type obj))
       (match (array-shape obj)
         (((0 n)) #t)
         (else #f))))

(define (statically-allocatable? x)
  "Return @code{#t} if a non-immediate constant can be allocated
statically, and @code{#f} if it would need some kind of runtime
allocation."
  (or (pair? x) (string? x) (stringbuf? x) (static-procedure? x)
      (array? x) (syntax? x)))

(define (intern-constant asm obj)
  "Add an object to the constant table, and return a label that can be
used to reference it.  If the object is already present in the constant
table, its existing label is used directly."
  (define (recur obj)
    (intern-constant asm obj))
  (define (field dst n obj)
    (let ((src (recur obj)))
      (if src
          (if (statically-allocatable? obj)
              `((static-patch! ,dst ,n ,src))
              `((static-ref 1 ,src)
                (static-set! 1 ,dst ,n)))
          '())))
  (define (intern obj label)
    (cond
     ((pair? obj)
      (append (field label 0 (car obj))
              (field label 1 (cdr obj))))
     ((simple-vector? obj)
      (let lp ((i 0) (inits '()))
        (if (< i (vector-length obj))
            (lp (1+ i)
                (append-reverse (field label (1+ i) (vector-ref obj i))
                                inits))
            (reverse inits))))
     ((syntax? obj)
      (append (field label 1 (syntax-expression obj))
              (field label 2 (syntax-wrap obj))
              (field label 3 (syntax-module obj))))
     ((stringbuf? obj) '())
     ((static-procedure? obj)
      `((static-patch! ,label 1 ,(static-procedure-code obj))))
     ((cache-cell? obj) '())
     ((symbol? obj)
      (unless (symbol-interned? obj)
        (error "uninterned symbol cannot be saved to object file" obj))
      `((make-non-immediate 1 ,(recur (symbol->string obj)))
        (string->symbol 1 1)
        (static-set! 1 ,label 0)))
     ((string? obj)
      `((static-patch! ,label 1 ,(recur (make-stringbuf obj)))))
     ((keyword? obj)
      `((static-ref 1 ,(recur (keyword->symbol obj)))
        (symbol->keyword 1 1)
        (static-set! 1 ,label 0)))
     ((number? obj)
      `((make-non-immediate 1 ,(recur (number->string obj)))
        (string->number 1 1)
        (static-set! 1 ,label 0)))
     ((uniform-vector-backing-store? obj) '())
     ((simple-uniform-vector? obj)
      (let ((width (case (array-type obj)
                     ((vu8 u8 s8) 1)
                     ((u16 s16) 2)
                     ;; Bitvectors are addressed in 32-bit units.
                     ;; Although a complex number is 8 or 16 bytes wide,
                     ;; it should be byteswapped in 4 or 8 byte units.
                     ((u32 s32 f32 c32 b) 4)
                     ((u64 s64 f64 c64) 8)
                     (else
                      (error "unhandled array type" obj)))))
        `((static-patch! ,label 2
                         ,(recur (make-uniform-vector-backing-store
                                  (uniform-array->bytevector obj)
                                  width))))))
     ((array? obj)
      `((static-patch! ,label 1 ,(recur (shared-array-root obj)))))
     (else
      (error "don't know how to intern" obj))))
  (cond
   ((immediate-bits asm obj) #f)
   ((vhash-assoc obj (asm-constants asm)) => cdr)
   (else
    ;; Note that calling intern may mutate asm-constants and asm-inits.
    (let* ((label (gensym "constant"))
           (inits (intern obj label)))
      (set-asm-constants! asm (vhash-cons obj label (asm-constants asm)))
      (set-asm-inits! asm (append-reverse inits (asm-inits asm)))
      label))))

(define (intern-non-immediate asm obj)
  "Intern a non-immediate into the constant table, and return its
label."
  (when (immediate-bits asm obj)
    (error "expected a non-immediate" obj))
  (intern-constant asm obj))

(define (intern-cache-cell asm scope key)
  "Intern a cache cell into the constant table, and return its label.
If there is already a cache cell with the given scope and key, it is
returned instead."
  (intern-constant asm (make-cache-cell scope key)))

;; Return the label of the cell that holds the module for a scope.
(define (intern-module-cache-cell asm scope)
  "Intern a cache cell for a module, and return its label."
  (intern-cache-cell asm scope #t))




;;;
;;; Macro assemblers bridge the gap between primitive instructions and
;;; some higher-level operations.
;;;

(eval-when (expand)
  (define-syntax define-macro-assembler
    (lambda (x)
      (syntax-case x ()
        ((_ (name arg ...) body body* ...)
         (with-syntax ((emit (id-append #'name #'emit- #'name)))
           #'(begin
               (define emit
                 (let ((emit (lambda (arg ...) body body* ...)))
                   (hashq-set! assemblers 'name emit)
                   emit))
               (export emit))))))))

(define-macro-assembler (load-constant asm dst obj)
  (cond
   ((immediate-bits asm obj)
    => (lambda (bits)
         (cond
          ((and (< dst 256) (zero? (ash bits -16)))
           (emit-make-short-immediate asm dst obj))
          ((zero? (ash bits -32))
           (emit-make-long-immediate asm dst obj))
          (else
           (emit-make-long-long-immediate asm dst obj)))))
   ((statically-allocatable? obj)
    (emit-make-non-immediate asm dst (intern-non-immediate asm obj)))
   (else
    (emit-static-ref asm dst (intern-non-immediate asm obj)))))

(define-macro-assembler (load-static-procedure asm dst label)
  (let ((loc (intern-constant asm (make-static-procedure label))))
    (emit-make-non-immediate asm dst loc)))

(define-syntax-rule (define-tc7-macro-assembler name tc7)
  (define-macro-assembler (name asm slot invert? label)
    (emit-br-if-tc7 asm slot invert? tc7 label)))

;; Keep in sync with tags.h.  Part of Guile's ABI.  Currently unused
;; macro assemblers are commented out.  See also
;; *branching-primcall-arities* in (language cps primitives), the set of
;; macro-instructions in assembly.scm, and
;; disassembler.scm:code-annotation.
;;
;; FIXME: Define all tc7 values in Scheme in one place, derived from
;; tags.h.
(define-tc7-macro-assembler br-if-symbol #x05)
(define-tc7-macro-assembler br-if-variable #x07)
(define-tc7-macro-assembler br-if-vector #x0d)
;(define-tc7-macro-assembler br-if-weak-vector 13)
(define-tc7-macro-assembler br-if-string #x15)
;(define-tc7-macro-assembler br-if-heap-number 23)
;(define-tc7-macro-assembler br-if-stringbuf 39)
(define-tc7-macro-assembler br-if-bytevector #x4d)
;(define-tc7-macro-assembler br-if-pointer 31)
;(define-tc7-macro-assembler br-if-hashtable 29)
;(define-tc7-macro-assembler br-if-fluid 37)
;(define-tc7-macro-assembler br-if-dynamic-state 45)
;(define-tc7-macro-assembler br-if-frame 47)
(define-tc7-macro-assembler br-if-keyword #x35)
;(define-tc7-macro-assembler br-if-syntax #x3d)
;(define-tc7-macro-assembler br-if-vm 55)
;(define-tc7-macro-assembler br-if-vm-cont 71)
;(define-tc7-macro-assembler br-if-rtl-program 69)
;(define-tc7-macro-assembler br-if-weak-set 85)
;(define-tc7-macro-assembler br-if-weak-table 87)
;(define-tc7-macro-assembler br-if-array 93)
(define-tc7-macro-assembler br-if-bitvector #x5f)
;(define-tc7-macro-assembler br-if-port 125)
;(define-tc7-macro-assembler br-if-smob 127)

(define-macro-assembler (begin-program asm label properties)
  (emit-label asm label)
  (let ((meta (make-meta label properties (asm-start asm))))
    (set-asm-meta! asm (cons meta (asm-meta asm)))))

(define-macro-assembler (end-program asm)
  (let ((meta (car (asm-meta asm))))
    (set-meta-high-pc! meta (asm-start asm))
    (set-meta-arities! meta (reverse (meta-arities meta)))))

(define-macro-assembler (begin-standard-arity asm req nlocals alternate)
  (emit-begin-opt-arity asm req '() #f nlocals alternate))

(define-macro-assembler (begin-opt-arity asm req opt rest nlocals alternate)
  (emit-begin-kw-arity asm req opt rest '() #f nlocals alternate))

(define-macro-assembler (begin-kw-arity asm req opt rest kw-indices
                                        allow-other-keys? nlocals alternate)
  (assert-match req ((? symbol?) ...) "list of symbols")
  (assert-match opt ((? symbol?) ...) "list of symbols")
  (assert-match rest (or #f (? symbol?)) "#f or symbol")
  (assert-match kw-indices (((? keyword?) . (? integer?)) ...)
                "alist of keyword -> integer")
  (assert-match allow-other-keys? (? boolean?) "boolean")
  (assert-match nlocals (? integer?) "integer")
  (assert-match alternate (or #f (? exact-integer?) (? symbol?)) "#f or symbol")
  (let* ((meta (car (asm-meta asm)))
         (arity (make-arity req opt rest kw-indices allow-other-keys?
                            (asm-start asm) #f '()))
         ;; The procedure itself is in slot 0, in the standard calling
         ;; convention.  For procedure prologues, nreq includes the
         ;; procedure, so here we add 1.
         (nreq (1+ (length req)))
         (nopt (length opt))
         (rest? (->bool rest)))
    (set-meta-arities! meta (cons arity (meta-arities meta)))
    (cond
     ((or allow-other-keys? (pair? kw-indices))
      (emit-kw-prelude asm nreq nopt rest? kw-indices allow-other-keys?
                       nlocals alternate))
     ((or rest? (pair? opt))
      (emit-opt-prelude asm nreq nopt rest? nlocals alternate))
     (else
      (emit-standard-prelude asm nreq nlocals alternate)))))

(define-macro-assembler (end-arity asm)
  (let ((arity (car (meta-arities (car (asm-meta asm))))))
    (set-arity-definitions! arity (reverse (arity-definitions arity)))
    (set-arity-high-pc! arity (asm-start asm))))

(define-macro-assembler (standard-prelude asm nreq nlocals alternate)
  (cond
   (alternate
    (emit-br-if-nargs-ne asm nreq alternate)
    (emit-alloc-frame asm nlocals))
   ((and (< nreq (ash 1 12)) (< (- nlocals nreq) (ash 1 12)))
    (emit-assert-nargs-ee/locals asm nreq (- nlocals nreq)))
   (else
    (emit-assert-nargs-ee asm nreq)
    (emit-alloc-frame asm nlocals))))

(define-macro-assembler (opt-prelude asm nreq nopt rest? nlocals alternate)
  (if alternate
      (emit-br-if-nargs-lt asm nreq alternate)
      (emit-assert-nargs-ge asm nreq))
  (cond
   (rest?
    (emit-bind-rest asm (+ nreq nopt)))
   (alternate
    (emit-br-if-nargs-gt asm (+ nreq nopt) alternate))
   (else
    (emit-assert-nargs-le asm (+ nreq nopt))))
  (emit-alloc-frame asm nlocals))

(define-macro-assembler (kw-prelude asm nreq nopt rest? kw-indices
                                    allow-other-keys? nlocals alternate)
  (if alternate
      (begin
        (emit-br-if-nargs-lt asm nreq alternate)
        (unless rest?
          (emit-br-if-npos-gt asm nreq (+ nreq nopt) alternate)))
      (emit-assert-nargs-ge asm nreq))
  (let ((ntotal (fold (lambda (kw ntotal)
                        (match kw
                          (((? keyword?) . idx)
                           (max (1+ idx) ntotal))))
                      (+ nreq nopt) kw-indices)))
    ;; FIXME: port 581f410f
    (emit-bind-kwargs asm nreq
                      (pack-flags allow-other-keys? rest?)
                      (+ nreq nopt)
                      ntotal
                      (intern-constant asm kw-indices))
    (emit-alloc-frame asm nlocals)))

(define-macro-assembler (label asm sym)
  (hashq-set! (asm-labels asm) sym (asm-start asm)))

(define-macro-assembler (source asm source)
  (set-asm-sources! asm (acons (asm-start asm) source (asm-sources asm))))

(define-macro-assembler (definition asm name slot representation)
  (let* ((arity (car (meta-arities (car (asm-meta asm)))))
         (def (vector name slot representation
                      (- (asm-start asm) (arity-low-pc arity)))))
    (set-arity-definitions! arity (cons def (arity-definitions arity)))))

(define-macro-assembler (cache-current-module! asm module scope)
  (let ((mod-label (intern-module-cache-cell asm scope)))
    (emit-static-set! asm module mod-label 0)))

(define-macro-assembler (cached-toplevel-box asm dst scope sym bound?)
  (let ((sym-label (intern-non-immediate asm sym))
        (mod-label (intern-module-cache-cell asm scope))
        (cell-label (intern-cache-cell asm scope sym)))
    (emit-toplevel-box asm dst cell-label mod-label sym-label bound?)))

(define-macro-assembler (cached-module-box asm dst module-name sym public? bound?)
  (let* ((sym-label (intern-non-immediate asm sym))
         (key (cons public? module-name))
         (mod-name-label (intern-constant asm key))
         (cell-label (intern-cache-cell asm key sym)))
    (emit-module-box asm dst cell-label mod-name-label sym-label bound?)))

(define-macro-assembler (slot-map asm proc-slot slot-map)
  (unless (zero? slot-map)
    (set-asm-slot-maps! asm (cons
                             (cons* (asm-start asm) proc-slot slot-map)
                             (asm-slot-maps asm)))))



;;;
;;; Helper for linking objects.
;;;

(define (make-object asm name bv relocs labels . kwargs)
  "Make a linker object.  This helper handles interning the name in the
shstrtab, assigning the size, allocating a fresh index, and defining a
corresponding linker symbol for the start of the section."
  (let ((name-idx (intern-section-name! asm (symbol->string name)))
        (index (asm-next-section-number asm)))
    (set-asm-next-section-number! asm (1+ index))
    (make-linker-object (apply make-elf-section
                               #:index index
                               #:name name-idx
                               #:size (bytevector-length bv)
                               kwargs)
                        bv relocs
                        (cons (make-linker-symbol name 0) labels))))




;;;
;;; Linking the constant table.  This code is somewhat intertwingled
;;; with the intern-constant code above, as that procedure also
;;; residualizes instructions to initialize constants at load time.
;;;

(define (write-immediate asm buf pos bits)
  (let ((endianness (asm-endianness asm)))
    (case (asm-word-size asm)
      ((4) (bytevector-u32-set! buf pos bits endianness))
      ((8) (bytevector-u64-set! buf pos bits endianness))
      (else (error "bad word size" asm)))))

(define (write-placeholder asm buf pos)
  (write-immediate asm buf pos (immediate-bits asm #f)))

(define (emit-init-constants asm)
  "If there is writable data that needs initialization at runtime, emit
a procedure to do that and return its label.  Otherwise return
@code{#f}."
  (let ((inits (asm-inits asm)))
    (and (not (null? inits))
         (let ((label (gensym "init-constants")))
           (emit-text asm
                      `((begin-program ,label ())
                        (assert-nargs-ee/locals 1 1)
                        ,@(reverse inits)
                        (load-constant 0 ,*unspecified*)
                        (return-values 2)
                        (end-program)))
           label))))

(define (link-data asm data name)
  "Link the static data for a program into the @var{name} section (which
should be .data or .rodata), and return the resulting linker object.
@var{data} should be a vhash mapping objects to labels."
  (define (align address alignment)
    (+ address
       (modulo (- alignment (modulo address alignment)) alignment)))

  (define tc7-vector #x0d)
  (define vector-immutable-flag #x80)

  (define tc7-string #x15)
  (define string-read-only-flag #x200)

  (define tc7-stringbuf #x27)
  (define stringbuf-wide-flag #x400)

  (define tc7-syntax #x3d)

  (define tc7-program #x45)

  (define tc7-bytevector #x4d)
  ;; This flag is intended to be left-shifted by 7 bits.
  (define bytevector-immutable-flag #x200)

  (define tc7-array #x5d)

  (define tc7-bitvector #x5f)
  (define bitvector-immutable-flag #x80)

  (let ((word-size (asm-word-size asm))
        (endianness (asm-endianness asm)))
    (define (byte-length x)
      (cond
       ((stringbuf? x)
        (let ((x (stringbuf-string x)))
          (+ (* 2 word-size)
             (case (string-bytes-per-char x)
               ((1) (1+ (string-length x)))
               ((4) (* (1+ (string-length x)) 4))
               (else (error "bad string bytes per char" x))))))
       ((static-procedure? x)
        (* 2 word-size))
       ((string? x)
        (* 4 word-size))
       ((pair? x)
        (* 2 word-size))
       ((simple-vector? x)
        (* (1+ (vector-length x)) word-size))
       ((syntax? x)
        (* 4 word-size))
       ((simple-uniform-vector? x)
        (* 4 word-size))
       ((uniform-vector-backing-store? x)
        (bytevector-length (uniform-vector-backing-store-bytes x)))
       ((array? x)
        (* word-size (+ 3 (* 3 (array-rank x)))))
       (else
        word-size)))

    (define (write-constant-reference buf pos x)
      (let ((bits (immediate-bits asm x)))
        (if bits
            (write-immediate asm buf pos bits)
            ;; The asm-inits will fix up any reference to a
            ;; non-immediate.
            (write-placeholder asm buf pos))))

    (define (write buf pos obj)
      (cond
       ((stringbuf? obj)
        (let* ((x (stringbuf-string obj))
               (len (string-length x))
               (tag (logior tc7-stringbuf
                            (if (= (string-bytes-per-char x) 1)
                                0
                                stringbuf-wide-flag))))
          (case word-size
            ((4)
             (bytevector-u32-set! buf pos tag endianness)
             (bytevector-u32-set! buf (+ pos 4) len endianness))
            ((8)
             (bytevector-u64-set! buf pos tag endianness)
             (bytevector-u64-set! buf (+ pos 8) len endianness))
            (else
             (error "bad word size" asm)))
          (let ((pos (+ pos (* word-size 2))))
            (case (string-bytes-per-char x)
              ((1)
               (let lp ((i 0))
                 (if (< i len)
                     (let ((u8 (char->integer (string-ref x i))))
                       (bytevector-u8-set! buf (+ pos i) u8)
                       (lp (1+ i)))
                     (bytevector-u8-set! buf (+ pos i) 0))))
              ((4)
               (let lp ((i 0))
                 (if (< i len)
                     (let ((u32 (char->integer (string-ref x i))))
                       (bytevector-u32-set! buf (+ pos (* i 4)) u32 endianness)
                       (lp (1+ i)))
                     (bytevector-u32-set! buf (+ pos (* i 4)) 0 endianness))))
              (else (error "bad string bytes per char" x))))))

       ((static-procedure? obj)
        (case word-size
          ((4)
           (bytevector-u32-set! buf pos tc7-program endianness)
           (bytevector-u32-set! buf (+ pos 4) 0 endianness))
          ((8)
           (bytevector-u64-set! buf pos tc7-program endianness)
           (bytevector-u64-set! buf (+ pos 8) 0 endianness))
          (else (error "bad word size"))))

       ((cache-cell? obj)
        (write-placeholder asm buf pos))

       ((string? obj)
        (let ((tag (logior tc7-string string-read-only-flag)))
          (case word-size
            ((4)
             (bytevector-u32-set! buf pos tag endianness)
             (write-placeholder asm buf (+ pos 4)) ; stringbuf
             (bytevector-u32-set! buf (+ pos 8) 0 endianness)
             (bytevector-u32-set! buf (+ pos 12) (string-length obj) endianness))
            ((8)
             (bytevector-u64-set! buf pos tag endianness)
             (write-placeholder asm buf (+ pos 8)) ; stringbuf
             (bytevector-u64-set! buf (+ pos 16) 0 endianness)
             (bytevector-u64-set! buf (+ pos 24) (string-length obj) endianness))
            (else (error "bad word size")))))

       ((pair? obj)
        (write-constant-reference buf pos (car obj))
        (write-constant-reference buf (+ pos word-size) (cdr obj)))

       ((simple-vector? obj)
        (let* ((len (vector-length obj))
               (tag (logior tc7-vector vector-immutable-flag (ash len 8))))
          (case word-size
            ((4) (bytevector-u32-set! buf pos tag endianness))
            ((8) (bytevector-u64-set! buf pos tag endianness))
            (else (error "bad word size")))
          (let lp ((i 0))
            (when (< i (vector-length obj))
              (let ((pos (+ pos word-size (* i word-size)))
                    (elt (vector-ref obj i)))
                (write-constant-reference buf pos elt)
                (lp (1+ i)))))))

       ((symbol? obj)
        (write-placeholder asm buf pos))

       ((keyword? obj)
        (write-placeholder asm buf pos))

       ((syntax? obj)
        (case word-size
          ((4) (bytevector-u32-set! buf pos tc7-syntax endianness))
          ((8) (bytevector-u64-set! buf pos tc7-syntax endianness))
          (else (error "bad word size")))
        (write-constant-reference buf (+ pos (* 1 word-size))
                                  (syntax-expression obj))
        (write-constant-reference buf (+ pos (* 2 word-size))
                                  (syntax-wrap obj))
        (write-constant-reference buf (+ pos (* 3 word-size))
                                  (syntax-module obj)))

       ((number? obj)
        (write-placeholder asm buf pos))

       ((simple-uniform-vector? obj)
        (let ((tag (if (bitvector? obj)
                       (logior tc7-bitvector
                               bitvector-immutable-flag)
                       (logior tc7-bytevector
                               ;; Bytevector immutable flag also shifted
                               ;; left.
                               (ash (logior bytevector-immutable-flag
                                            (array-type-code obj))
                                    7)))))
          (case word-size
            ((4)
             (bytevector-u32-set! buf pos tag endianness)
             (bytevector-u32-set! buf (+ pos 4)
                                  (if (bitvector? obj)
                                      (bitvector-length obj)
                                      (bytevector-length obj))
                                  endianness)                 ; length
             (bytevector-u32-set! buf (+ pos 8) 0 endianness) ; pointer
             (write-placeholder asm buf (+ pos 12)))          ; owner
            ((8)
             (bytevector-u64-set! buf pos tag endianness)
             (bytevector-u64-set! buf (+ pos 8)
                                  (if (bitvector? obj)
                                      (bitvector-length obj)
                                      (bytevector-length obj))
                                  endianness)                  ; length
             (bytevector-u64-set! buf (+ pos 16) 0 endianness) ; pointer
             (write-placeholder asm buf (+ pos 24)))           ; owner
            (else (error "bad word size")))))

       ((uniform-vector-backing-store? obj)
        (let ((bv (uniform-vector-backing-store-bytes obj)))
          (bytevector-copy! bv 0 buf pos (bytevector-length bv))
          (unless (eq? endianness (native-endianness))
            (case (uniform-vector-backing-store-element-size obj)
              ((1) #f) ;; Nothing to do.
              ((2) (byte-swap/2! buf pos (+ pos (bytevector-length bv))))
              ((4) (byte-swap/4! buf pos (+ pos (bytevector-length bv))))
              ((8) (byte-swap/8! buf pos (+ pos (bytevector-length bv))))
              (else (error "FIXME: Implement byte order swap"))))))

       ((array? obj)
        (let-values
            ;; array tag + rank + contp flag: see libguile/arrays.h .
            (((tag) (logior tc7-array (ash (array-rank obj) 17) (ash 1 16)))
             ((bv-set! bvs-set!)
              (case word-size
                ((4) (values bytevector-u32-set! bytevector-s32-set!))
                ((8) (values bytevector-u64-set! bytevector-s64-set!))
                (else (error "bad word size")))))
          (bv-set! buf pos tag endianness)
          (write-placeholder asm buf (+ pos word-size))      ; root vector (fixed later)
          (bv-set! buf (+ pos (* word-size 2)) 0 endianness) ; base
          (let lp ((pos (+ pos (* word-size 3)))
                   (bounds (array-shape obj))
                   (incs (shared-array-increments obj)))
            (when (pair? bounds)
              (bvs-set! buf pos (first (first bounds)) endianness)
              (bvs-set! buf (+ pos word-size) (second (first bounds)) endianness)
              (bvs-set! buf (+ pos (* word-size 2)) (first incs) endianness)
              (lp (+ pos (* 3 word-size)) (cdr bounds) (cdr incs))))))

       (else
        (error "unrecognized object" obj))))

    (cond
     ((vlist-null? data) #f)
     (else
      (let* ((byte-len (vhash-fold (lambda (k v len)
                                     (+ (byte-length k) (align len 8)))
                                   0 data))
             (buf (make-bytevector byte-len 0)))
        (let lp ((i 0) (pos 0) (symbols '()))
          (if (< i (vlist-length data))
              (let* ((pair (vlist-ref data i))
                     (obj (car pair))
                     (obj-label (cdr pair)))
                (write buf pos obj)
                (lp (1+ i)
                    (align (+ (byte-length obj) pos) 8)
                    (cons (make-linker-symbol obj-label pos) symbols)))
              (make-object asm name buf '() symbols
                           #:flags (match name
                                     ('.data (logior SHF_ALLOC SHF_WRITE))
                                     ('.rodata SHF_ALLOC))))))))))

(define (link-constants asm)
  "Link sections to hold constants needed by the program text emitted
using @var{asm}.

Returns three values: an object for the .rodata section, an object for
the .data section, and a label for an initialization procedure.  Any of
these may be @code{#f}."
  (define (shareable? x)
    (cond
     ((stringbuf? x) #t)
     ((pair? x)
      (and (immediate-bits asm (car x)) (immediate-bits asm (cdr x))))
     ((simple-vector? x)
      (let lp ((i 0))
        (or (= i (vector-length x))
            (and (immediate-bits asm (vector-ref x i))
                 (lp (1+ i))))))
     ((uniform-vector-backing-store? x) #t)
     (else #f)))
  (let* ((constants (asm-constants asm))
         (len (vlist-length constants)))
    (let lp ((i 0)
             (ro vlist-null)
             (rw vlist-null))
      (if (= i len)
          (values (link-data asm ro '.rodata)
                  (link-data asm rw '.data)
                  (emit-init-constants asm))
          (let ((pair (vlist-ref constants i)))
            (if (shareable? (car pair))
                (lp (1+ i) (vhash-consq (car pair) (cdr pair) ro) rw)
                (lp (1+ i) ro (vhash-consq (car pair) (cdr pair) rw))))))))



;;;
;;; Linking program text.
;;;

(define (process-relocs buf relocs labels)
  "Patch up internal x8-s24 relocations, and any s32 relocations that
reference symbols in the text section.  Return a list of linker
relocations for references to symbols defined outside the text section."
  (fold
   (lambda (reloc tail)
     (match reloc
       ((type label base offset)
        (let ((abs (hashq-ref labels label))
              (dst (+ base offset)))
          (case type
            ((s32)
             (if abs
                 (let ((rel (- abs base)))
                   (unless (zero? (logand rel #x3))
                     (error "reloc not in 32-bit units!"))
                   (bytevector-s32-native-set! buf dst (ash rel -2))
                   tail)
                 (cons (make-linker-reloc 'rel32/4 dst offset label)
                       tail)))
            ((x8-s24)
             (unless abs
               (error "unbound near relocation" reloc))
             (let ((rel (- abs base))
                   (u32 (bytevector-u32-native-ref buf dst)))
               (unless (zero? (logand rel #x3))
                 (error "reloc not in 32-bit units!"))
               (bytevector-u32-native-set! buf dst
                                           (pack-u8-s24 (logand u32 #xff)
                                                        (ash rel -2)))
               tail))
            (else (error "bad relocation kind" reloc)))))))
   '()
   relocs))

(define (process-labels labels)
  "Define linker symbols for the label-offset map in @var{labels}.
The offsets are expected to be expressed in words."
  (hash-map->list (lambda (label loc)
                    (make-linker-symbol label loc))
                  labels))

(define (link-text-object asm)
  "Link the .rtl-text section, swapping the endianness of the bytes if
needed."
  (let ((buf (make-bytevector (asm-pos asm))))
    (bytevector-copy! (asm-buf asm) 0 buf 0 (bytevector-length buf))
    (unless (eq? (asm-endianness asm) (native-endianness))
      (byte-swap/4! buf))
    (make-object asm '.rtl-text
                 buf
                 (process-relocs buf (asm-relocs asm)
                                 (asm-labels asm))
                 (process-labels (asm-labels asm)))))




;;;
;;; Create the frame maps.  These maps are used by GC to identify dead
;;; slots in pending call frames, to avoid marking them.  We only do
;;; this when frame makes a non-tail call, as that is the common case.
;;; Only the topmost frame will see a GC at any other point, but we mark
;;; top frames conservatively as serializing live slot maps at every
;;; instruction would take up too much space in the object file.
;;;

;; The .guile.frame-maps section starts with two packed u32 values: one
;; indicating the offset of the first byte of the .rtl-text section, and
;; another indicating the relative offset in bytes of the slots data.
(define frame-maps-prefix-len 8)

;; Each header is 8 bytes: 4 for the offset from .rtl_text, and 4 for
;; the offset of the slot map from the beginning of the
;; .guile.frame-maps section.  The length of a frame map depends on the
;; frame size at the call site, and is not encoded into this section as
;; it is available at run-time.
(define frame-map-header-len 8)

(define (link-frame-maps asm)
  (define (map-byte-length proc-slot)
    (ceiling-quotient (* 2 (- proc-slot 2)) 8))
  (define (make-frame-maps maps count map-len)
    (let* ((endianness (asm-endianness asm))
           (header-pos frame-maps-prefix-len)
           (map-pos (+ header-pos (* count frame-map-header-len)))
           (bv (make-bytevector (+ map-pos map-len) 0)))
      (bytevector-u32-set! bv 4 map-pos endianness)
      (let lp ((maps maps) (header-pos header-pos) (map-pos map-pos))
        (match maps
          (()
           (make-object asm '.guile.frame-maps bv
                        (list (make-linker-reloc 'abs32/1 0 0 '.rtl-text))
                        '() #:type SHT_PROGBITS #:flags SHF_ALLOC))
          (((pos proc-slot . map) . maps)
           (bytevector-u32-set! bv header-pos pos endianness)
           (bytevector-u32-set! bv (+ header-pos 4) map-pos endianness)
           (let write-bytes ((map-pos map-pos)
                             (map map)
                             (byte-length (map-byte-length proc-slot)))
             (if (zero? byte-length)
                 (lp maps (+ header-pos frame-map-header-len) map-pos)
                 (begin
                   (bytevector-u8-set! bv map-pos (logand map #xff))
                   (write-bytes (1+ map-pos) (ash map -8)
                                (1- byte-length))))))))))
  (match (asm-slot-maps asm)
    (() #f)
    (in
     (let lp ((in in) (out '()) (count 0) (map-len 0))
       (match in
         (() (make-frame-maps out count map-len))
         (((and head (pos proc-slot . map)) . in)
          (lp in (cons head out)
              (1+ count)
              (+ (map-byte-length proc-slot) map-len))))))))



;;;
;;; Linking other sections of the ELF file, like the dynamic segment,
;;; the symbol table, etc.
;;;

;; FIXME: Define these somewhere central, shared with C.
(define *bytecode-major-version* #x0202)
(define *bytecode-minor-version* (char->integer #\A))

(define (link-dynamic-section asm text rw rw-init frame-maps)
  "Link the dynamic section for an ELF image with bytecode @var{text},
given the writable data section @var{rw} needing fixup from the
procedure with label @var{rw-init}.  @var{rw-init} may be false.  If
@var{rw} is true, it will be added to the GC roots at runtime."
  (define-syntax-rule (emit-dynamic-section word-size %set-uword! reloc-type)
    (let* ((endianness (asm-endianness asm))
           (words 6)
           (words (if rw (+ words 4) words))
           (words (if rw-init (+ words 2) words))
           (words (if frame-maps (+ words 2) words))
           (bv (make-bytevector (* word-size words) 0))
           (set-uword!
            (lambda (i uword)
              (%set-uword! bv (* i word-size) uword endianness)))
           (relocs '())
           (set-label!
            (lambda (i label)
              (set! relocs (cons (make-linker-reloc 'reloc-type
                                                    (* i word-size) 0 label)
                                 relocs))
              (%set-uword! bv (* i word-size) 0 endianness))))
      (set-uword! 0 DT_GUILE_VM_VERSION)
      (set-uword! 1 (logior (ash *bytecode-major-version* 16)
                            *bytecode-minor-version*))
      (set-uword! 2 DT_GUILE_ENTRY)
      (set-label! 3 '.rtl-text)
      (when rw
        ;; Add roots to GC.
        (set-uword! 4 DT_GUILE_GC_ROOT)
        (set-label! 5 '.data)
        (set-uword! 6 DT_GUILE_GC_ROOT_SZ)
        (set-uword! 7 (bytevector-length (linker-object-bv rw)))
        (when rw-init
          (set-uword! 8 DT_INIT)        ; constants
          (set-label! 9 rw-init)))
      (when frame-maps
        (set-uword! (- words 4) DT_GUILE_FRAME_MAPS)
        (set-label! (- words 3) '.guile.frame-maps))
      (set-uword! (- words 2) DT_NULL)
      (set-uword! (- words 1) 0)
      (make-object asm '.dynamic bv relocs '()
                   #:type SHT_DYNAMIC #:flags SHF_ALLOC)))
  (case (asm-word-size asm)
    ((4) (emit-dynamic-section 4 bytevector-u32-set! abs32/1))
    ((8) (emit-dynamic-section 8 bytevector-u64-set! abs64/1))
    (else (error "bad word size" asm))))

(define (link-shstrtab asm)
  "Link the string table for the section headers."
  (intern-section-name! asm ".shstrtab")
  (make-object asm '.shstrtab
               (link-string-table! (asm-shstrtab asm))
               '() '()
               #:type SHT_STRTAB #:flags 0))

(define (link-symtab text-section asm)
  (let* ((endianness (asm-endianness asm))
         (word-size (asm-word-size asm))
         (size (elf-symbol-len word-size))
         (meta (reverse (asm-meta asm)))
         (n (length meta))
         (strtab (make-string-table))
         (bv (make-bytevector (* n size) 0)))
    (define (intern-string! name)
      (string-table-intern! strtab (if name (symbol->string name) "")))
    (for-each
     (lambda (meta n)
       (let ((name (intern-string! (meta-name meta))))
         (write-elf-symbol bv (* n size) endianness word-size
                           (make-elf-symbol
                            #:name name
                            ;; Symbol value and size are measured in
                            ;; bytes, not u32s.
                            #:value (meta-low-pc meta)
                            #:size (- (meta-high-pc meta)
                                      (meta-low-pc meta))
                            #:type STT_FUNC
                            #:visibility STV_HIDDEN
                            #:shndx (elf-section-index text-section)))))
     meta (iota n))
    (let ((strtab (make-object asm '.strtab
                               (link-string-table! strtab)
                               '() '()
                               #:type SHT_STRTAB #:flags 0)))
      (values (make-object asm '.symtab
                           bv
                           '() '()
                           #:type SHT_SYMTAB #:flags 0 #:entsize size
                           #:link (elf-section-index
                                   (linker-object-section strtab)))
              strtab))))

;;; The .guile.arities section describes the arities that a function can
;;; have.  It is in two parts: a sorted array of headers describing
;;; basic arities, and an array of links out to a string table (and in
;;; the case of keyword arguments, to the data section) for argument
;;; names.  The whole thing is prefixed by a uint32 indicating the
;;; offset of the end of the headers array.
;;;
;;; The arity headers array is a packed array of structures of the form:
;;;
;;;   struct arity_header {
;;;     uint32_t low_pc;
;;;     uint32_t high_pc;
;;;     uint32_t offset;
;;;     uint32_t flags;
;;;     uint32_t nreq;
;;;     uint32_t nopt;
;;;     uint32_t nlocals;
;;;   }
;;;
;;; All of the offsets and addresses are 32 bits.  We can expand in the
;;; future to use 64-bit offsets if appropriate, but there are other
;;; aspects of bytecode that constrain us to a total image that fits in
;;; 32 bits, so for the moment we'll simplify the problem space.
;;;
;;; The following flags values are defined:
;;;
;;;    #x1: has-rest?
;;;    #x2: allow-other-keys?
;;;    #x4: has-keyword-args?
;;;    #x8: is-case-lambda?
;;;    #x10: is-in-case-lambda?
;;;
;;; Functions with a single arity specify their number of required and
;;; optional arguments in nreq and nopt, and do not have the
;;; is-case-lambda? flag set.  Their "offset" member links to an array
;;; of pointers into the associated .guile.arities.strtab string table,
;;; identifying the argument names.  This offset is relative to the
;;; start of the .guile.arities section.
;;;
;;; If the arity has keyword arguments -- if has-keyword-args? is set in
;;; the flags -- the first uint32 pointed to by offset encodes a link to
;;; the "keyword indices" literal, in the data section.  Then follow the
;;; names for all locals, in order, as uleb128 values.  The required
;;; arguments will be the first locals, followed by the optionals,
;;; followed by the rest argument if if has-rest? is set.  The names
;;; point into the associated string table section.
;;;
;;; Functions with no arities have no arities information present in the
;;; .guile.arities section.
;;;
;;; Functions with multiple arities are preceded by a header with
;;; is-case-lambda? set.  All other fields are 0, except low-pc and
;;; high-pc which should be the bounds of the whole function.  Headers
;;; for the individual arities follow, with the is-in-case-lambda? flag
;;; set.  In this way the whole headers array is sorted in increasing
;;; low-pc order, and case-lambda clauses are contained within the
;;; [low-pc, high-pc] of the case-lambda header.

;; Length of the prefix to the arities section, in bytes.
(define arities-prefix-len 4)

;; Length of an arity header, in bytes.
(define arity-header-len (* 7 4))

;; Some helpers.
(define (put-uleb128 port val)
  (let lp ((val val))
    (let ((next (ash val -7)))
      (if (zero? next)
          (put-u8 port val)
          (begin
            (put-u8 port (logior #x80 (logand val #x7f)))
            (lp next))))))

(define (put-sleb128 port val)
  (let lp ((val val))
    (if (<= 0 (+ val 64) 127)
        (put-u8 port (logand val #x7f))
        (begin
          (put-u8 port (logior #x80 (logand val #x7f)))
          (lp (ash val -7))))))

(define (port-position port)
  (seek port 0 SEEK_CUR))

(define-inline (pack-arity-flags has-rest? allow-other-keys?
                                 has-keyword-args? is-case-lambda?
                                 is-in-case-lambda?)
  (logior (if has-rest? (ash 1 0) 0)
          (if allow-other-keys? (ash 1 1) 0)
          (if has-keyword-args? (ash 1 2) 0)
          (if is-case-lambda? (ash 1 3) 0)
          (if is-in-case-lambda? (ash 1 4) 0)))

(define (write-arities asm metas headers names-port strtab)
  (define (write-header pos low-pc high-pc offset flags nreq nopt nlocals)
    (unless (<= (+ nreq nopt) nlocals)
      (error "forgot to emit definition instructions?"))
    (bytevector-u32-set! headers pos low-pc (asm-endianness asm))
    (bytevector-u32-set! headers (+ pos 4) high-pc (asm-endianness asm))
    (bytevector-u32-set! headers (+ pos 8) offset (asm-endianness asm))
    (bytevector-u32-set! headers (+ pos 12) flags (asm-endianness asm))
    (bytevector-u32-set! headers (+ pos 16) nreq (asm-endianness asm))
    (bytevector-u32-set! headers (+ pos 20) nopt (asm-endianness asm))
    (bytevector-u32-set! headers (+ pos 24) nlocals (asm-endianness asm)))
  (define (write-kw-indices kw-indices relocs)
    ;; FIXME: Assert that kw-indices is already interned.
    (if (pair? kw-indices)
        (let ((pos (+ (bytevector-length headers)
                      (port-position names-port)))
              (label (intern-constant asm kw-indices)))
          (put-bytevector names-port #vu8(0 0 0 0))
          (cons (make-linker-reloc 'abs32/1 pos 0 label) relocs))
        relocs))
  (define (write-arity pos arity in-case-lambda? relocs)
    (write-header pos (arity-low-pc arity)
                  (arity-high-pc arity)
                  ;; FIXME: Seems silly to add on bytevector-length of
                  ;; headers, given the arities-prefix.
                  (+ (bytevector-length headers) (port-position names-port))
                  (pack-arity-flags (arity-rest arity)
                                    (arity-allow-other-keys? arity)
                                    (pair? (arity-kw-indices arity))
                                    #f
                                    in-case-lambda?)
                  (length (arity-req arity))
                  (length (arity-opt arity))
                  (length (arity-definitions arity)))
    (let ((relocs (write-kw-indices (arity-kw-indices arity) relocs)))
      ;; Write local names.
      (let lp ((definitions (arity-definitions arity)))
        (match definitions
          (() relocs)
          ((#(name slot representation def) . definitions)
           (let ((sym (if (symbol? name)
                          (string-table-intern! strtab (symbol->string name))
                          0)))
             (put-uleb128 names-port sym)
             (lp definitions)))))
      ;; Now write their definitions.
      (let lp ((definitions (arity-definitions arity)))
        (match definitions
          (() relocs)
          ((#(name slot representation def) . definitions)
           (put-uleb128 names-port def)
           (let ((tag (case representation
                        ((scm) 0)
                        ((f64) 1)
                        ((u64) 2)
                        ((s64) 3)
                        (else (error "what!" representation)))))
             (put-uleb128 names-port (logior (ash slot 2) tag)))
           (lp definitions))))))
  (let lp ((metas metas) (pos arities-prefix-len) (relocs '()))
    (match metas
      (()
       (unless (= pos (bytevector-length headers))
         (error "expected to fully fill the bytevector"
                pos (bytevector-length headers)))
       relocs)
      ((meta . metas)
       (match (meta-arities meta)
         (() (lp metas pos relocs))
         ((arity)
          (lp metas
              (+ pos arity-header-len)
              (write-arity pos arity #f relocs)))
         (arities
          ;; Write a case-lambda header, then individual arities.
          ;; The case-lambda header's offset link is 0.
          (write-header pos (meta-low-pc meta) (meta-high-pc meta) 0
                        (pack-arity-flags #f #f #f #t #f) 0 0 0)
          (let lp* ((arities arities) (pos (+ pos arity-header-len))
                    (relocs relocs))
            (match arities
              (() (lp metas pos relocs))
              ((arity . arities)
               (lp* arities
                    (+ pos arity-header-len)
                    (write-arity pos arity #t relocs)))))))))))

(define (link-arities asm)
  (define (meta-arities-header-size meta)
    (define (lambda-size arity)
      arity-header-len)
    (define (case-lambda-size arities)
      (fold +
            arity-header-len            ;; case-lambda header
            (map lambda-size arities))) ;; the cases
    (match (meta-arities meta)
      (() 0)
      ((arity) (lambda-size arity))
      (arities (case-lambda-size arities))))

  (define (bytevector-append a b)
    (let ((out (make-bytevector (+ (bytevector-length a)
                                   (bytevector-length b)))))
      (bytevector-copy! a 0 out 0 (bytevector-length a))
      (bytevector-copy! b 0 out (bytevector-length a) (bytevector-length b))
      out))

  (let* ((endianness (asm-endianness asm))
         (metas (reverse (asm-meta asm)))
         (header-size (fold (lambda (meta size)
                              (+ size (meta-arities-header-size meta)))
                            arities-prefix-len
                            metas))
         (strtab (make-string-table))
         (headers (make-bytevector header-size 0)))
    (bytevector-u32-set! headers 0 (bytevector-length headers) endianness)
    (let-values (((names-port get-name-bv) (open-bytevector-output-port)))
      (let* ((relocs (write-arities asm metas headers names-port strtab))
             (strtab (make-object asm '.guile.arities.strtab
                                  (link-string-table! strtab)
                                  '() '()
                                  #:type SHT_STRTAB #:flags 0)))
        (values (make-object asm '.guile.arities
                             (bytevector-append headers (get-name-bv))
                             relocs '()
                             #:type SHT_PROGBITS #:flags 0
                             #:link (elf-section-index
                                     (linker-object-section strtab)))
                strtab)))))

;;;
;;; The .guile.docstrs section is a packed, sorted array of (pc, str)
;;; values.  Pc and str are both 32 bits wide.  (Either could change to
;;; 64 bits if appropriate in the future.)  Pc is the address of the
;;; entry to a program, relative to the start of the text section, in
;;; bytes, and str is an index into the associated .guile.docstrs.strtab
;;; string table section.
;;;

;; The size of a docstrs entry, in bytes.
(define docstr-size 8)

(define (link-docstrs asm)
  (define (find-docstrings)
    (filter-map (lambda (meta)
                  (define (is-documentation? pair)
                    (eq? (car pair) 'documentation))
                  (let* ((props (meta-properties meta))
                         (tail (find-tail is-documentation? props)))
                    (and tail
                         (not (find-tail is-documentation? (cdr tail)))
                         (string? (cdar tail))
                         (cons (meta-low-pc meta) (cdar tail)))))
                (reverse (asm-meta asm))))
  (let* ((endianness (asm-endianness asm))
         (docstrings (find-docstrings))
         (strtab (make-string-table))
         (bv (make-bytevector (* (length docstrings) docstr-size) 0)))
    (fold (lambda (pair pos)
            (match pair
              ((pc . string)
               (bytevector-u32-set! bv pos pc endianness)
               (bytevector-u32-set! bv (+ pos 4)
                                    (string-table-intern! strtab string)
                                    endianness)
               (+ pos docstr-size))))
          0
          docstrings)
    (let ((strtab (make-object asm '.guile.docstrs.strtab
                               (link-string-table! strtab)
                               '() '()
                               #:type SHT_STRTAB #:flags 0)))
      (values (make-object asm '.guile.docstrs
                           bv
                           '() '()
                           #:type SHT_PROGBITS #:flags 0
                           #:link (elf-section-index
                                   (linker-object-section strtab)))
              strtab))))

;;;
;;; The .guile.procprops section is a packed, sorted array of (pc, addr)
;;; values.  Pc and addr are both 32 bits wide.  (Either could change to
;;; 64 bits if appropriate in the future.)  Pc is the address of the
;;; entry to a program, relative to the start of the text section, and
;;; addr is the address of the associated properties alist, relative to
;;; the start of the ELF image.
;;;
;;; Since procedure properties are stored in the data sections, we need
;;; to link the procedures property section first.  (Note that this
;;; constraint does not apply to the arities section, which may
;;; reference the data sections via the kw-indices literal, because
;;; assembling the text section already makes sure that the kw-indices
;;; are interned.)
;;;

;; The size of a procprops entry, in bytes.
(define procprops-size 8)

(define (link-procprops asm)
  (define (assoc-remove-one alist key value-pred)
    (match alist
      (() '())
      ((((? (lambda (x) (eq? x key))) . value) . alist)
       (if (value-pred value)
           alist
           (acons key value alist)))
      (((k . v) . alist)
       (acons k v (assoc-remove-one alist key value-pred)))))
  (define (props-without-name-or-docstring meta)
    (assoc-remove-one
     (assoc-remove-one (meta-properties meta) 'name (lambda (x) #t))
     'documentation
     string?))
  (define (find-procprops)
    (filter-map (lambda (meta)
                  (let ((props (props-without-name-or-docstring meta)))
                    (and (pair? props)
                         (cons (meta-low-pc meta) props))))
                (reverse (asm-meta asm))))
  (let* ((endianness (asm-endianness asm))
         (procprops (find-procprops))
         (bv (make-bytevector (* (length procprops) procprops-size) 0)))
    (let lp ((procprops procprops) (pos 0) (relocs '()))
      (match procprops
        (()
         (make-object asm '.guile.procprops
                      bv
                      relocs '()
                      #:type SHT_PROGBITS #:flags 0))
        (((pc . props) . procprops)
         (bytevector-u32-set! bv pos pc endianness)
         (lp procprops
             (+ pos procprops-size)
             (cons (make-linker-reloc 'abs32/1 (+ pos 4) 0
                                      (intern-constant asm props))
                   relocs)))))))

;;;
;;; The DWARF .debug_info, .debug_abbrev, .debug_str, and .debug_loc
;;; sections provide line number and local variable liveness
;;; information.  Their format is defined by the DWARF
;;; specifications.
;;;

(define (asm-language asm)
  ;; FIXME: Plumb language through to the assembler.
  'scheme)

;; -> 5 values: .debug_info, .debug_abbrev, .debug_str, .debug_loc, .debug_lines
(define (link-debug asm)
  (define (put-s8 port val)
    (let ((bv (make-bytevector 1)))
      (bytevector-s8-set! bv 0 val)
      (put-bytevector port bv)))

  (define (put-u16 port val)
    (let ((bv (make-bytevector 2)))
      (bytevector-u16-set! bv 0 val (asm-endianness asm))
      (put-bytevector port bv)))

  (define (put-u32 port val)
    (let ((bv (make-bytevector 4)))
      (bytevector-u32-set! bv 0 val (asm-endianness asm))
      (put-bytevector port bv)))

  (define (put-u64 port val)
    (let ((bv (make-bytevector 8)))
      (bytevector-u64-set! bv 0 val (asm-endianness asm))
      (put-bytevector port bv)))

  (define (meta->subprogram-die meta)
    `(subprogram
      (@ ,@(cond
            ((meta-name meta)
             => (lambda (name) `((name ,(symbol->string name)))))
            (else
             '()))
         (low-pc ,(meta-label meta))
         (high-pc ,(- (meta-high-pc meta) (meta-low-pc meta))))))

  (define (make-compile-unit-die asm)
    `(compile-unit
      (@ (producer ,(string-append "Guile " (version)))
         (language ,(asm-language asm))
         (low-pc .rtl-text)
         (high-pc ,(asm-pos asm))
         (stmt-list 0))
      ,@(map meta->subprogram-die (reverse (asm-meta asm)))))

  (let-values (((die-port get-die-bv) (open-bytevector-output-port))
               ((die-relocs) '())
               ((abbrev-port get-abbrev-bv) (open-bytevector-output-port))
               ;; (tag has-kids? attrs forms) -> code
               ((abbrevs) vlist-null)
               ((strtab) (make-string-table))
               ((line-port get-line-bv) (open-bytevector-output-port))
               ((line-relocs) '())
               ;; file -> code
               ((files) vlist-null))

    (define (write-abbrev code tag has-children? attrs forms)
      (put-uleb128 abbrev-port code)
      (put-uleb128 abbrev-port (tag-name->code tag))
      (put-u8 abbrev-port (children-name->code (if has-children? 'yes 'no)))
      (for-each (lambda (attr form)
                  (put-uleb128 abbrev-port (attribute-name->code attr))
                  (put-uleb128 abbrev-port (form-name->code form)))
                attrs forms)
      (put-uleb128 abbrev-port 0)
      (put-uleb128 abbrev-port 0))

    (define (intern-abbrev tag has-children? attrs forms)
      (let ((key (list tag has-children? attrs forms)))
        (match (vhash-assoc key abbrevs)
          ((_ . code) code)
          (#f (let ((code (1+ (vlist-length abbrevs))))
                (set! abbrevs (vhash-cons key code abbrevs))
                (write-abbrev code tag has-children? attrs forms)
                code)))))

    (define (intern-file file)
      (match (vhash-assoc file files)
        ((_ . code) code)
        (#f (let ((code (1+ (vlist-length files))))
              (set! files (vhash-cons file code files))
              code))))

    (define (write-sources)
      ;; Choose line base and line range values that will allow for an
      ;; address advance range of 16 words.  The special opcode range is
      ;; from 10 to 255, so 246 values.
      (define base -4)
      (define range 15)
      (define min-inc 4) ; Minimum PC increment.

      (let lp ((sources (asm-sources asm)) (out '()))
        (match sources
          (((pc . s) . sources)
           (let ((file (assq-ref s 'filename))
                 (line (assq-ref s 'line))
                 (col (assq-ref s 'column)))
             (lp sources
                 ;; Guile line and column numbers are 0-indexed, but
                 ;; they are 1-indexed for DWARF.
                 (if (and line col)
                     (cons (list pc
                                 (if (string? file) (intern-file file) 0)
                                 (1+ line)
                                 (1+ col))
                           out)
                     out))))
          (()
           ;; Compilation unit header for .debug_line.  We write in
           ;; DWARF 2 format because more tools understand it than DWARF
           ;; 4, which incompatibly adds another field to this header.

           (put-u32 line-port 0) ; Length; will patch later.
           (put-u16 line-port 2) ; DWARF 2 format.
           (put-u32 line-port 0) ; Prologue length; will patch later.
           (put-u8 line-port min-inc) ; Minimum instruction length: 4 bytes.
           (put-u8 line-port 1) ; Default is-stmt: true.

           (put-s8 line-port base) ; Line base.  See the DWARF standard.
           (put-u8 line-port range) ; Line range.  See the DWARF standard.
           (put-u8 line-port 10) ; Opcode base: the first "special" opcode.

           ;; A table of the number of uleb128 arguments taken by each
           ;; of the standard opcodes.
           (put-u8 line-port 0) ; 1: copy
           (put-u8 line-port 1) ; 2: advance-pc
           (put-u8 line-port 1) ; 3: advance-line
           (put-u8 line-port 1) ; 4: set-file
           (put-u8 line-port 1) ; 5: set-column
           (put-u8 line-port 0) ; 6: negate-stmt
           (put-u8 line-port 0) ; 7: set-basic-block
           (put-u8 line-port 0) ; 8: const-add-pc
           (put-u8 line-port 1) ; 9: fixed-advance-pc

           ;; Include directories, as a zero-terminated sequence of
           ;; nul-terminated strings.  Nothing, for the moment.
           (put-u8 line-port 0)

           ;; File table.  For each file that contributes to this
           ;; compilation unit, a nul-terminated file name string, and a
           ;; uleb128 for each of directory the file was found in, the
           ;; modification time, and the file's size in bytes.  We pass
           ;; zero for the latter three fields.
           (vlist-fold-right
            (lambda (pair seed)
              (match pair
                ((file . code)
                 (put-bytevector line-port (string->utf8 file))
                 (put-u8 line-port 0)
                 (put-uleb128 line-port 0) ; directory
                 (put-uleb128 line-port 0) ; mtime
                 (put-uleb128 line-port 0))) ; size
              seed)
            #f
            files)
           (put-u8 line-port 0) ; 0 byte terminating file list.

           ;; Patch prologue length.
           (let ((offset (port-position line-port)))
             (seek line-port 6 SEEK_SET)
             (put-u32 line-port (- offset 10))
             (seek line-port offset SEEK_SET))

           ;; Now write the statement program.
           (let ()
             (define (extended-op opcode payload-len)
               (put-u8 line-port 0)                     ; extended op
               (put-uleb128 line-port (1+ payload-len)) ; payload-len + opcode
               (put-uleb128 line-port opcode))
             (define (set-address sym)
               (define (add-reloc! kind)
                 (set! line-relocs
                       (cons (make-linker-reloc kind
                                                (port-position line-port)
                                                0
                                                sym)
                             line-relocs)))
               (match (asm-word-size asm)
                 (4
                  (extended-op 2 4)
                  (add-reloc! 'abs32/1)
                  (put-u32 line-port 0))
                 (8
                  (extended-op 2 8)
                  (add-reloc! 'abs64/1)
                  (put-u64 line-port 0))))
             (define (end-sequence pc)
               (let ((pc-inc (/ (- (asm-pos asm) pc) min-inc)))
                 (put-u8 line-port 2)   ; advance-pc
                 (put-uleb128 line-port pc-inc))
               (extended-op 1 0))
             (define (advance-pc pc-inc line-inc)
               (let ((spec (+ (- line-inc base)
                              (* (/ pc-inc min-inc) range)
                              10)))
                 (cond
                  ((or (< line-inc base) (>= line-inc (+ base range)))
                   (advance-line line-inc)
                   (advance-pc pc-inc 0))
                  ((<= spec 255)
                   (put-u8 line-port spec))
                  ((< spec 500)
                   (put-u8 line-port 8) ; const-advance-pc
                   (advance-pc (- pc-inc (* (floor/ (- 255 10) range) min-inc))
                               line-inc))
                  (else
                   (put-u8 line-port 2) ; advance-pc
                   (put-uleb128 line-port (/ pc-inc min-inc))
                   (advance-pc 0 line-inc)))))
             (define (advance-line inc)
               (put-u8 line-port 3)
               (put-sleb128 line-port inc))
             (define (set-file file)
               (put-u8 line-port 4)
               (put-uleb128 line-port file))
             (define (set-column col)
               (put-u8 line-port 5)
               (put-uleb128 line-port col))

             (set-address '.rtl-text)

             (let lp ((in out) (pc 0) (file 1) (line 1) (col 0))
               (match in
                 (()
                  (when (null? out)
                    ;; There was no source info in the first place.  Set
                    ;; file register to 0 before adding final row.
                    (set-file 0))
                  (end-sequence pc))
                 (((pc* file* line* col*) . in*)
                  (cond
                   ((and (eqv? file file*) (eqv? line line*) (eqv? col col*))
                    (lp in* pc file line col))
                   (else
                    (unless (eqv? col col*)
                      (set-column col*))
                    (unless (eqv? file file*)
                      (set-file file*))
                    (advance-pc (- pc* pc) (- line* line))
                    (lp in* pc* file* line* col*)))))))))))

    (define (compute-code attr val)
      (match attr
        ('name (string-table-intern! strtab val))
        ('low-pc val)
        ('high-pc val)
        ('producer (string-table-intern! strtab val))
        ('language (language-name->code val))
        ('stmt-list val)))

    (define (choose-form attr val code)
      (cond
       ((string? val) 'strp)
       ((eq? attr 'stmt-list) 'sec-offset)
       ((eq? attr 'low-pc) 'addr)
       ((exact-integer? code)
        (cond
         ((< code 0) 'sleb128)
         ((<= code #xff) 'data1)
         ((<= code #xffff) 'data2)
         ((<= code #xffffffff) 'data4)
         ((<= code #xffffffffffffffff) 'data8)
         (else 'uleb128)))
       (else (error "unhandled case" attr val code))))

    (define (add-die-relocation! kind sym)
      (set! die-relocs
            (cons (make-linker-reloc kind (port-position die-port) 0 sym)
                  die-relocs)))

    (define (write-value code form)
      (match form
        ('data1 (put-u8 die-port code))
        ('data2 (put-u16 die-port code))
        ('data4 (put-u32 die-port code))
        ('data8 (put-u64 die-port code))
        ('uleb128 (put-uleb128 die-port code))
        ('sleb128 (put-sleb128 die-port code))
        ('addr
         (match (asm-word-size asm)
           (4
            (add-die-relocation! 'abs32/1 code)
            (put-u32 die-port 0))
           (8
            (add-die-relocation! 'abs64/1 code)
            (put-u64 die-port 0))))
        ('sec-offset (put-u32 die-port code))
        ('strp (put-u32 die-port code))))

    (define (write-die die)
      (match die
        ((tag ('@ (attrs vals) ...) children ...)
         (let* ((codes (map compute-code attrs vals))
                (forms (map choose-form attrs vals codes))
                (has-children? (not (null? children)))
                (abbrev-code (intern-abbrev tag has-children? attrs forms)))
           (put-uleb128 die-port abbrev-code)
           (for-each write-value codes forms)
           (when has-children?
             (for-each write-die children)
             (put-uleb128 die-port 0))))))

    ;; Compilation unit header.
    (put-u32 die-port 0) ; Length; will patch later.
    (put-u16 die-port 4) ; DWARF 4.
    (put-u32 die-port 0) ; Abbrevs offset.
    (put-u8 die-port (asm-word-size asm)) ; Address size.

    (write-die (make-compile-unit-die asm))

    ;; Terminate the abbrevs list.
    (put-uleb128 abbrev-port 0)

    (write-sources)

    (values (let ((bv (get-die-bv)))
              ;; Patch DWARF32 length.
              (bytevector-u32-set! bv 0 (- (bytevector-length bv) 4)
                                   (asm-endianness asm))
              (make-object asm '.debug_info bv die-relocs '()
                           #:type SHT_PROGBITS #:flags 0))
            (make-object asm '.debug_abbrev (get-abbrev-bv) '() '()
                         #:type SHT_PROGBITS #:flags 0)
            (make-object asm '.debug_str (link-string-table! strtab) '() '()
                         #:type SHT_PROGBITS #:flags 0)
            (make-object asm '.debug_loc #vu8() '() '()
                         #:type SHT_PROGBITS #:flags 0)
            (let ((bv (get-line-bv)))
              ;; Patch DWARF32 length.
              (bytevector-u32-set! bv 0 (- (bytevector-length bv) 4)
                                   (asm-endianness asm))
              (make-object asm '.debug_line bv line-relocs '()
                           #:type SHT_PROGBITS #:flags 0)))))

(define (link-objects asm)
  (let*-values (;; Link procprops before constants, because it probably
                ;; interns more constants.
                ((procprops) (link-procprops asm))
                ((ro rw rw-init) (link-constants asm))
                ;; Link text object after constants, so that the
                ;; constants initializer gets included.
                ((text) (link-text-object asm))
                ((frame-maps) (link-frame-maps asm))
                ((dt) (link-dynamic-section asm text rw rw-init frame-maps))
                ((symtab strtab) (link-symtab (linker-object-section text) asm))
                ((arities arities-strtab) (link-arities asm))
                ((docstrs docstrs-strtab) (link-docstrs asm))
                ((dinfo dabbrev dstrtab dloc dline) (link-debug asm))
                ;; This needs to be linked last, because linking other
                ;; sections adds entries to the string table.
                ((shstrtab) (link-shstrtab asm)))
    (filter identity
            (list text ro frame-maps rw dt symtab strtab
                  arities arities-strtab
                  docstrs docstrs-strtab procprops
                  dinfo dabbrev dstrtab dloc dline
                  shstrtab))))




;;;
;;; High-level public interfaces.
;;;

(define* (link-assembly asm #:key (page-aligned? #t))
  "Produce an ELF image from the code and data emitted into @var{asm}.
The result is a bytevector, by default linked so that read-only and
writable data are on separate pages.  Pass @code{#:page-aligned? #f} to
disable this behavior."
  (link-elf (link-objects asm) #:page-aligned? page-aligned?))
