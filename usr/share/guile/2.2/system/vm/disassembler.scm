;;; Guile bytecode disassembler

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

;;; Code:

(define-module (system vm disassembler)
  #:use-module (language bytecode)
  #:use-module (system vm elf)
  #:use-module (system vm debug)
  #:use-module (system vm program)
  #:use-module (system vm loader)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:export (disassemble-program
            fold-program-code
            disassemble-image
            disassemble-file

            instruction-length
            instruction-has-fallthrough?
            instruction-relative-jump-targets
            instruction-stack-size-after
            instruction-slot-clobbers))

(define-syntax-rule (u32-ref buf n)
  (bytevector-u32-native-ref buf (* n 4)))

(define-syntax-rule (s32-ref buf n)
  (bytevector-s32-native-ref buf (* n 4)))

(define-syntax visit-opcodes
  (lambda (x)
    (syntax-case x ()
      ((visit-opcodes macro arg ...)
       (with-syntax (((inst ...)
                      (map (lambda (x) (datum->syntax #'macro x))
                           (instruction-list))))
         #'(begin
             (macro arg ... . inst)
             ...))))))

(eval-when (expand compile load eval)
  (define (id-append ctx a b)
    (datum->syntax ctx (symbol-append (syntax->datum a) (syntax->datum b)))))

(define (unpack-scm n)
  (pointer->scm (make-pointer n)))

(define (unpack-s24 s)
  (if (zero? (logand s (ash 1 23)))
      s
      (- s (ash 1 24))))

(define (unpack-s32 s)
  (if (zero? (logand s (ash 1 31)))
      s
      (- s (ash 1 32))))

(define-syntax disassembler
  (lambda (x)
    (define (parse-first-word word type)
      (with-syntax ((word word))
        (case type
          ((X32)
           #'())
          ((X8_S24 X8_F24 X8_C24)
           #'((ash word -8)))
          ((X8_L24)
           #'((unpack-s24 (ash word -8))))
          ((X8_S8_I16)
           #'((logand (ash word -8) #xff)
              (ash word -16)))
          ((X8_S12_S12
            X8_S12_C12
            X8_C12_C12
            X8_F12_F12)
           #'((logand (ash word -8) #xfff)
              (ash word -20)))
          ((X8_S8_S8_S8
            X8_S8_S8_C8
            X8_S8_C8_S8)
           #'((logand (ash word -8) #xff)
              (logand (ash word -16) #xff)
              (ash word -24)))
          (else
           (error "bad head kind" type)))))

    (define (parse-tail-word word type)
      (with-syntax ((word word))
        (case type
          ((C32 I32 A32 B32 AU32 BU32 AS32 BS32 AF32 BF32)
           #'(word))
          ((N32 R32 L32 LO32)
           #'((unpack-s32 word)))
          ((C8_C24)
           #'((logand word #xff)
              (ash word -8)))
          ((B1_C7_L24)
           #'((not (zero? (logand word #x1)))
              (logand (ash word -1) #x7f)
              (unpack-s24 (ash word -8))))
          ((B1_X7_S24 B1_X7_F24 B1_X7_C24)
           #'((not (zero? (logand word #x1)))
              (ash word -8)))
          ((B1_X7_L24)
           #'((not (zero? (logand word #x1)))
              (unpack-s24 (ash word -8))))
          ((B1_X31)
           #'((not (zero? (logand word #x1)))))
          ((X8_S24 X8_F24 X8_C24)
           #'((ash word -8)))
          ((X8_L24)
           #'((unpack-s24 (ash word -8))))
          (else
           (error "bad tail kind" type)))))

    (syntax-case x ()
      ((_ name opcode word0 word* ...)
       (let ((vars (generate-temporaries #'(word* ...))))
         (with-syntax (((word* ...) vars)
                       ((n ...) (map 1+ (iota (length #'(word* ...)))))
                       ((asm ...)
                        (parse-first-word #'first (syntax->datum #'word0)))
                       (((asm* ...) ...)
                        (map (lambda (word type)
                               (parse-tail-word word type))
                             vars
                             (syntax->datum #'(word* ...)))))
           #'(lambda (buf offset first)
               (let ((word* (u32-ref buf (+ offset n)))
                     ...)
                 (values (+ 1 (length '(word* ...)))
                         (list 'name asm ... asm* ... ...))))))))))

(define (disasm-invalid buf offset first)
  (error "bad instruction" (logand first #xff) first buf offset))

(define disassemblers (make-vector 256 disasm-invalid))

(define-syntax define-disassembler
  (lambda (x)
    (syntax-case x ()
      ((_ name opcode kind arg ...)
       (with-syntax ((parse (id-append #'name #'parse- #'name)))
         #'(let ((parse (disassembler name opcode arg ...)))
             (vector-set! disassemblers opcode parse)))))))

(visit-opcodes define-disassembler)

;; -> len list
(define (disassemble-one buf offset)
  (let ((first (u32-ref buf offset)))
    ((vector-ref disassemblers (logand first #xff)) buf offset first)))

(define (u32-offset->addr offset context)
  "Given an offset into an image in 32-bit units, return the absolute
address of that offset."
  (+ (debug-context-base context) (* offset 4)))

(define (code-annotation code len offset start labels context push-addr!)
  ;; FIXME: Print names for register loads and stores that correspond to
  ;; access to named locals.
  (define (reference-scm target)
    (unpack-scm (u32-offset->addr (+ offset target) context)))

  (define (dereference-scm target)
    (let ((addr (u32-offset->addr (+ offset target)
                                  context)))
      (pointer->scm
       (dereference-pointer (make-pointer addr)))))

  (match code
    (((or 'br
          'br-if-nargs-ne 'br-if-nargs-lt 'br-if-nargs-gt
          'br-if-true 'br-if-null 'br-if-nil 'br-if-pair 'br-if-struct
          'br-if-char 'br-if-eq 'br-if-eqv
          'br-if-= 'br-if-< 'br-if-<= 'br-if-> 'br-if->=
          'br-if-u64-= 'br-if-u64-< 'br-if-u64-<=
          'br-if-u64-<-scm 'br-if-u64-<=-scm 'br-if-u64-=-scm
          'br-if-u64->-scm 'br-if-u64->=-scm
          'br-if-f64-= 'br-if-f64-< 'br-if-f64-<=
          'br-if-f64-> 'br-if-f64->=
          'br-if-logtest) _ ... target)
     (list "-> ~A" (vector-ref labels (- (+ offset target) start))))
    (('br-if-tc7 slot invert? tc7 target)
     (list "~A -> ~A"
           (let ((tag (case tc7
                        ((5) "symbol?")
                        ((7) "variable?")
                        ((13) "vector?")
                        ((15) "string?")
                        ((53) "keyword?")
                        ((#x3d) "syntax?")
                        ((77) "bytevector?")
                        ((95) "bitvector?")
                        (else (number->string tc7)))))
             (if invert? (string-append "not " tag) tag))
           (vector-ref labels (- (+ offset target) start))))
    (('prompt tag escape-only? proc-slot handler)
     ;; The H is for handler.
     (list "H -> ~A" (vector-ref labels (- (+ offset handler) start))))
    (((or 'make-short-immediate 'make-long-immediate) _ imm)
     (list "~S" (unpack-scm imm)))
    (('make-long-long-immediate _ high low)
     (list "~S" (unpack-scm (logior (ash high 32) low))))
    (('assert-nargs-ee/locals nargs locals)
     ;; The nargs includes the procedure.
     (list "~a slot~:p (~a arg~:p)" (+ locals nargs) (1- nargs)))
    (('alloc-frame nlocals)
     (list "~a slot~:p" nlocals))
    (('reset-frame nlocals)
     (list "~a slot~:p" nlocals))
    (('return-values nlocals)
     (if (zero? nlocals)
         (list "all values")
         (list "~a value~:p" (1- nlocals))))
    (('bind-rest dst)
     (list "~a slot~:p" (1+ dst)))
    (('tail-call nargs proc)
     (list "~a arg~:p" nargs))
    (('make-closure dst target nfree)
     (let* ((addr (u32-offset->addr (+ offset target) context))
            (pdi (find-program-debug-info addr context))
            (name (or (and pdi (program-debug-info-name pdi))
                      "anonymous procedure")))
       (push-addr! addr name)
       (list "~A at #x~X (~A free var~:p)" name addr nfree)))
    (('call-label closure nlocals target)
     (let* ((addr (u32-offset->addr (+ offset target) context))
            (pdi (find-program-debug-info addr context))
            (name (or (and pdi (program-debug-info-name pdi))
                      "anonymous procedure")))
       (push-addr! addr name)
       (list "~A at #x~X" name addr)))
    (('tail-call-label nlocals target)
     (let* ((addr (u32-offset->addr (+ offset target) context))
            (pdi (find-program-debug-info addr context))
            (name (or (and pdi (program-debug-info-name pdi))
                      "anonymous procedure")))
       (push-addr! addr name)
       (list "~A at #x~X" name addr)))
    (('make-non-immediate dst target)
     (let ((val (reference-scm target)))
       (when (program? val)
         (push-addr! (program-code val) val))
       (list "~@Y" val)))
    (('builtin-ref dst idx)
     (list "~A" (builtin-index->name idx)))
    (((or 'static-ref 'static-set!) _ target)
     (list "~@Y" (dereference-scm target)))
    (((or 'free-ref 'free-set!) _ _ index)
     (list "free var ~a" index))
    (('resolve-module dst name public)
     (list "~a" (if (zero? public) "private" "public")))
    (('toplevel-box _ var-offset mod-offset sym-offset bound?)
     (list "`~A'~A" (dereference-scm sym-offset)
           (if bound? "" " (maybe unbound)")))
    (('module-box _ var-offset mod-name-offset sym-offset bound?)
     (let ((mod-name (reference-scm mod-name-offset)))
       (list "`(~A ~A ~A)'~A" (if (car mod-name) '@ '@@) (cdr mod-name)
             (dereference-scm sym-offset)
             (if bound? "" " (maybe unbound)"))))
    (('load-typed-array dst type shape target len)
     (let ((addr (u32-offset->addr (+ offset target) context)))
       (list "~a bytes from #x~X" len addr)))
    (_ #f)))

(define (compute-labels bv start end)
  (let ((labels (make-vector (- end start) #f)))
    (define (add-label! pos header)
      (unless (vector-ref labels (- pos start))
        (vector-set! labels (- pos start) header)))

    (let lp ((offset start))
      (when (< offset end)
        (call-with-values (lambda () (disassemble-one bv offset))
          (lambda (len elt)
            (match elt
              ((inst arg ...)
               (case inst
                 ((br
                   br-if-nargs-ne br-if-nargs-lt br-if-nargs-gt
                   br-if-true br-if-null br-if-nil br-if-pair br-if-struct
                   br-if-char br-if-tc7 br-if-eq br-if-eqv
                   br-if-= br-if-< br-if-<= br-if-> br-if->= br-if-logtest
                   br-if-u64-= br-if-u64-< br-if-u64-<=
                   br-if-u64-<-scm br-if-u64-<=-scm br-if-u64-=-scm
                   br-if-u64->-scm br-if-u64->=-scm)
                  (match arg
                    ((_ ... target)
                     (add-label! (+ offset target) "L"))))
                 ((prompt)
                  (match arg
                    ((_ ... target)
                     (add-label! (+ offset target) "H")))))))
            (lp (+ offset len))))))
    (let lp ((offset start) (n 1))
      (when (< offset end)
        (let* ((pos (- offset start))
               (label (vector-ref labels pos)))
          (if label
              (begin
                (vector-set! labels
                             pos
                             (string->symbol
                              (string-append label (number->string n))))
                (lp (1+ offset) (1+ n)))
              (lp (1+ offset) n)))))
    labels))

(define (print-info port addr label info extra src)
  (when label
    (format port "~A:\n" label))
  (format port "~4@S    ~32S~@[;; ~1{~@?~}~]~@[~61t at ~a~]\n"
          addr info extra src))

(define (disassemble-buffer port bv start end context push-addr!)
  (let ((labels (compute-labels bv start end))
        (sources (find-program-sources (u32-offset->addr start context)
                                       context)))
    (define (lookup-source addr)
      (let lp ((sources sources))
        (match sources
          (() #f)
          ((source . sources)
           (let ((pc (source-pre-pc source)))
             (cond
              ((< pc addr) (lp sources))
              ((= pc addr)
               (format #f "~a:~a:~a"
                       (or (source-file source) "(unknown file)")
                       (source-line-for-user source)
                       (source-column source)))
              (else #f)))))))
    (let lp ((offset start))
      (when (< offset end)
        (call-with-values (lambda () (disassemble-one bv offset))
          (lambda (len elt)
            (let ((pos (- offset start))
                  (addr (u32-offset->addr offset context))
                  (annotation (code-annotation elt len offset start labels
                                               context push-addr!)))
              (print-info port pos (vector-ref labels pos) elt annotation
                          (lookup-source addr))
              (lp (+ offset len)))))))))

(define* (disassemble-addr addr label port #:optional (seen (make-hash-table)))
  (format port "Disassembly of ~A at #x~X:\n\n" label addr)
  (cond
   ((find-program-debug-info addr)
    => (lambda (pdi)
         (let ((worklist '()))
           (define (push-addr! addr label)
             (unless (hashv-ref seen addr)
               (hashv-set! seen addr #t)
               (set! worklist (acons addr label worklist))))
           (disassemble-buffer port
                               (program-debug-info-image pdi)
                               (program-debug-info-u32-offset pdi)
                               (program-debug-info-u32-offset-end pdi)
                               (program-debug-info-context pdi)
                               push-addr!)
           (for-each (match-lambda
                      ((addr . label)
                       (display "\n----------------------------------------\n"
                                port)
                       (disassemble-addr addr label port seen)))
                     worklist))))
   (else
    (format port "Debugging information unavailable.~%")))
  (values))

(define* (disassemble-program program #:optional (port (current-output-port)))
  (disassemble-addr (program-code program) program port))

(define (fold-code-range proc seed bv start end context raw?)
  (define (cook code offset)
    (define (reference-scm target)
      (unpack-scm (u32-offset->addr (+ offset target) context)))

    (define (dereference-scm target)
      (let ((addr (u32-offset->addr (+ offset target)
                                    context)))
        (pointer->scm
         (dereference-pointer (make-pointer addr)))))
    (match code
      (((or 'make-short-immediate 'make-long-immediate) dst imm)
       `(,(car code) ,dst ,(unpack-scm imm)))
      (('make-long-long-immediate dst high low)
       `(make-long-long-immediate ,dst
                                  ,(unpack-scm (logior (ash high 32) low))))
      (('make-closure dst target nfree)
       `(make-closure ,dst
                      ,(u32-offset->addr (+ offset target) context)
                      ,nfree))
      (('make-non-immediate dst target)
       `(make-non-immediate ,dst ,(reference-scm target)))
      (('builtin-ref dst idx)
       `(builtin-ref ,dst ,(builtin-index->name idx)))
      (((or 'static-ref 'static-set!) dst target)
       `(,(car code) ,dst ,(dereference-scm target)))
      (('toplevel-box dst var-offset mod-offset sym-offset bound?)
       `(toplevel-box ,dst
                      ,(dereference-scm var-offset)
                      ,(dereference-scm mod-offset)
                      ,(dereference-scm sym-offset)
                      ,bound?))
      (('module-box dst var-offset mod-name-offset sym-offset bound?)
       (let ((mod-name (reference-scm mod-name-offset)))
         `(module-box ,dst
                      ,(dereference-scm var-offset)
                      ,(car mod-name)
                      ,(cdr mod-name)
                      ,(dereference-scm sym-offset)
                      ,bound?)))
      (_ code)))
  (let lp ((offset start) (seed seed))
    (cond
     ((< offset end)
      (call-with-values (lambda () (disassemble-one bv offset))
        (lambda (len elt)
          (lp (+ offset len)
              (proc (if raw? elt (cook elt offset))
                    seed)))))
     (else seed))))

(define* (fold-program-code proc seed program-or-addr #:key raw?)
  (cond
   ((find-program-debug-info (if (program? program-or-addr)
                                 (program-code program-or-addr)
                                 program-or-addr))
    => (lambda (pdi)
         (fold-code-range proc seed
                          (program-debug-info-image pdi)
                          (program-debug-info-u32-offset pdi)
                          (program-debug-info-u32-offset-end pdi)
                          (program-debug-info-context pdi)
                          raw?)))
   (else seed)))

(define* (disassemble-image bv #:optional (port (current-output-port)))
  (let* ((ctx (debug-context-from-image bv))
         (base (debug-context-text-base ctx)))
    (for-each-elf-symbol
     ctx
     (lambda (sym)
       (let ((name (elf-symbol-name sym))
             (value (elf-symbol-value sym))
             (size (elf-symbol-size sym)))
         (format port "Disassembly of ~A at #x~X:\n\n"
                 (if (and (string? name) (not (string-null? name)))
                     name
                     "<unnamed function>")
                 (+ base value))
         (disassemble-buffer port
                             bv
                             (/ (+ base value) 4)
                             (/ (+ base value size) 4)
                             ctx
                             (lambda (addr name) #t))
         (display "\n\n" port)))))
  (values))

(define (disassemble-file file)
  (let* ((thunk (load-thunk-from-file file))
         (elf (find-mapped-elf-image (program-code thunk))))
    (disassemble-image elf)))

(define-syntax instruction-lengths-vector
  (lambda (x)
    (syntax-case x ()
      ((_)
       (let ((lengths (make-vector 256 #f)))
         (for-each (match-lambda
                    ((name opcode kind words ...)
                     (vector-set! lengths opcode (* 4 (length words)))))
                   (instruction-list))
         (datum->syntax x lengths))))))

(define (instruction-length code pos)
  (let ((opcode (logand (bytevector-u32-native-ref code pos) #xff)))
    (or (vector-ref (instruction-lengths-vector) opcode)
        (error "Unknown opcode" opcode))))

(define-syntax static-opcode-set
  (lambda (x)
    (define (instruction-opcode inst)
      (cond
       ((assq inst (instruction-list))
        => (match-lambda ((name opcode . _) opcode)))
       (else
        (error "unknown instruction" inst))))

    (syntax-case x ()
      ((static-opcode-set inst ...)
       (let ((bv (make-bitvector 256 #f)))
         (for-each (lambda (inst)
                     (bitvector-set! bv (instruction-opcode inst) #t))
                   (syntax->datum #'(inst ...)))
         (datum->syntax #'static-opcode-set bv))))))

(define (instruction-has-fallthrough? code pos)
  (define non-fallthrough-set
    (static-opcode-set halt
                       tail-call tail-call-label tail-call/shuffle
                       return-values
                       subr-call foreign-call continuation-call
                       tail-apply
                       br))
  (let ((opcode (logand (bytevector-u32-native-ref code pos) #xff)))
    (not (bitvector-ref non-fallthrough-set opcode))))

(define-syntax define-jump-parser
  (lambda (x)
    (syntax-case x ()
      ((_ name opcode kind word0 word* ...)
       (let ((symname (syntax->datum #'name)))
         (if (or (memq symname '(br prompt))
                 (string-prefix? "br-" (symbol->string symname)))
             (let ((offset (* 4 (length #'(word* ...)))))
               #`(vector-set!
                  jump-parsers
                  opcode
                  (lambda (code pos)
                    (let ((target
                           (bytevector-s32-native-ref code (+ pos #,offset))))
                      ;; Assume that the target is in the last word, as
                      ;; an L24 in the high bits.
                      (list (* 4 (ash target -8)))))))
             #'(begin)))))))

(define jump-parsers (make-vector 256 (lambda (code pos) '())))
(visit-opcodes define-jump-parser)

(define (instruction-relative-jump-targets code pos)
  (let ((opcode (logand (bytevector-u32-native-ref code pos) #xff)))
    ((vector-ref jump-parsers opcode) code pos)))

(define-syntax define-stack-effect-parser
  (lambda (x)
    (define (stack-effect-parser name)
      (case name
        ((push)
         #'(lambda (code pos size) (+ size 1)))
        ((pop)
         #'(lambda (code pos size) (- size 1)))
        ((drop)
         #'(lambda (code pos size)
             (let ((count (ash (bytevector-u32-native-ref code pos) -8)))
               (- size count))))
        ((alloc-frame reset-frame)
         #'(lambda (code pos size)
             (let ((nlocals (ash (bytevector-u32-native-ref code pos) -8)))
               nlocals)))
        ((receive)
         #'(lambda (code pos size)
             (let ((nlocals (ash (bytevector-u32-native-ref code (+ pos 4))
                                 -8)))
               nlocals)))
        ((bind-kwargs)
         #'(lambda (code pos size)
             (let ((ntotal (ash (bytevector-u32-native-ref code (+ pos 8)) -8)))
               ntotal)))
        ((bind-rest)
         #'(lambda (code pos size)
             (let ((dst (ash (bytevector-u32-native-ref code pos) -8)))
               (+ dst 1))))
        ((assert-nargs-ee/locals)
         #'(lambda (code pos size)
             (let ((nargs (logand (ash (bytevector-u32-native-ref code pos) -8)
                                  #xfff))
                   (nlocals (ash (bytevector-u32-native-ref code pos) -20)))
               (+ nargs nlocals))))
        ((call call-label)
         #'(lambda (code pos size) #f))
        ((tail-call tail-call-label tail-call/shuffle tail-apply)
         #'(lambda (code pos size) #f))
        (else
         #f)))
    (syntax-case x ()
      ((_ name opcode kind word0 word* ...)
       (let ((parser (stack-effect-parser (syntax->datum #'name))))
         (if parser
             #`(vector-set! stack-effect-parsers opcode #,parser)
             #'(begin)))))))

(define stack-effect-parsers (make-vector 256 (lambda (code pos size) size)))
(visit-opcodes define-stack-effect-parser)

(define (instruction-stack-size-after code pos size)
  (let ((opcode (logand (bytevector-u32-native-ref code pos) #xff)))
    ((vector-ref stack-effect-parsers opcode) code pos size)))

(define-syntax define-clobber-parser
  (lambda (x)
    (syntax-case x ()
      ((_ name opcode kind arg0 arg* ...)
       (case (syntax->datum #'kind)
         ((!)
          (case (syntax->datum #'name)
            ((call call-label)
             #'(let ((parse (lambda (code pos nslots-in nslots-out)
                              (call-with-values
                                  (lambda ()
                                    (disassemble-one code (/ pos 4)))
                                (lambda (len elt)
                                  (match elt
                                    ((_ proc . _)
                                     (let lp ((slot (- proc 2)))
                                       (if (< slot nslots-in)
                                           (cons slot (lp (1+ slot)))
                                           '())))))))))
                 (vector-set! clobber-parsers opcode parse)))
            (else
             #'(begin))))
         ((<-)
          #`(let ((parse (lambda (code pos nslots-in nslots-out)
                           (call-with-values
                               (lambda ()
                                 (disassemble-one code (/ pos 4)))
                             (lambda (len elt)
                               (match elt
                                 ((_ dst . _)
                                  #,(case (syntax->datum #'arg0)
                                      ((X8_F24 X8_F12_F12)
                                       #'(list dst))
                                      (else
                                       #'(list (- nslots-out 1 dst)))))))))))
              (vector-set! clobber-parsers opcode parse)))
         (else (error "unexpected instruction kind" #'kind)))))))

(define clobber-parsers
  (make-vector 256 (lambda (code pos nslots-in nslots-out) '())))
(visit-opcodes define-clobber-parser)

(define (instruction-slot-clobbers code pos nslots-in nslots-out)
  (let ((opcode (logand (bytevector-u32-native-ref code pos) #xff)))
    ((vector-ref clobber-parsers opcode) code pos nslots-in nslots-out)))
