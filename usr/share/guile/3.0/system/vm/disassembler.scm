;;; Guile bytecode disassembler

;;; Copyright (C) 2001, 2009-2010, 2012-2015, 2017-2020, 2022, 2023 Free Software Foundation, Inc.
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
  #:use-module (system base types internal)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (disassemble-program
            fold-program-code
            disassemble-image
            disassemble-file

            instruction-length
            instruction-has-fallthrough?
            instruction-relative-jump-targets
            instruction-stack-size-after
            instruction-slot-clobbers))

(define (unpack-scm n)
  (pointer->scm (make-pointer n)))

(define (unpack-s24 s)
  (if (zero? (logand s (ash 1 23)))
      s
      (- s (ash 1 24))))

(define (unpack-s12 s)
  (if (zero? (logand s (ash 1 11)))
      s
      (- s (ash 1 12))))

(define (unpack-s32 s)
  (if (zero? (logand s (ash 1 31)))
      s
      (- s (ash 1 32))))

(eval-when (expand)
  (define-syntax-rule (u32-ref buf n)
    (bytevector-u32-native-ref buf (* n 4)))

  (define-syntax-rule (s32-ref buf n)
    (bytevector-s32-native-ref buf (* n 4)))

  (define-syntax-rule (define-op-handlers handlers make-handler)
    (define handlers
      (let ((handlers (make-vector 256 #f)))
        (define-syntax init-handlers
          (lambda (stx)
            #`(begin
                #,@(filter-map
                    (match-lambda
                     ((name opcode kind . word-types)
                      (match (make-handler name kind word-types)
                        (#f #f)
                        (init #`(vector-set! handlers #,opcode #,init)))))
                    (instruction-list)))))
        (init-handlers)
        handlers))))

(define-op-handlers disassemblers
  (lambda (name kind word-types)
    (define (parse-first-word word type)
      (with-syntax ((word word))
        (case type
          ((X32)
           #'())
          ((X8_S24 X8_F24 X8_C24)
           #'((ash word -8)))
          ((X8_L24)
           #'((unpack-s24 (ash word -8))))
          ((X8_S8_I16 X8_S8_ZI16)
           #'((logand (ash word -8) #xff)
              (ash word -16)))
          ((X8_S12_S12
            X8_S12_C12
            X8_C12_C12
            X8_F12_F12)
           #'((logand (ash word -8) #xfff)
              (ash word -20)))
          ((X8_S12_Z12)
           #'((logand (ash word -8) #xfff)
              (unpack-s12 (ash word -20))))
          ((X8_S8_S8_S8
            X8_S8_S8_C8
            X8_S8_C8_S8)
           #'((logand (ash word -8) #xff)
              (logand (ash word -16) #xff)
              (ash word -24)))
          (else
           (error "bad head kind" type)))))

    (define (parse-tail-word word type n)
      (with-syntax ((word word) (n n))
        (case type
          ((C32 I32 A32 B32 AU32 BU32 AS32 BS32 AF32 BF32)
           #'(1 word))
          ((N32 R32 L32 LO32)
           #'(1 (unpack-s32 word)))
          ((C8_C24 C8_S24)
           #'(1
              (logand word #xff)
              (ash word -8)))
          ((C16_C16)
           #'(1
              (logand word #xffff)
              (ash word -16)))
          ((B1_C7_L24)
           #'(1
              (not (zero? (logand word #x1)))
              (logand (ash word -1) #x7f)
              (unpack-s24 (ash word -8))))
          ((B1_X7_S24 B1_X7_F24 B1_X7_C24)
           #'(1
              (not (zero? (logand word #x1)))
              (ash word -8)))
          ((B1_X7_L24)
           #'(1
              (not (zero? (logand word #x1)))
              (unpack-s24 (ash word -8))))
          ((B1_X31)
           #'(1 (not (zero? (logand word #x1)))))
          ((X8_S24 X8_F24 X8_C24)
           #'(1 (ash word -8)))
          ((X8_L24)
           #'(1 (unpack-s24 (ash word -8))))
          ((V32_X8_L24)
           #'((+ 1 word)
              (let ((v (make-vector word))
                    (base (+ offset n 1)))
                (let lp ((i 0))
                  (when (< i word)
                    (vector-set! v i
                                 (unpack-s24 (ash (u32-ref buf (+ base i)) -8)))
                    (lp (1+ i))))
                v)))
          (else
           (error "bad tail kind" type)))))

    (match word-types
      ((first-word . tail-words)
       (let ((vars (generate-temporaries tail-words))
             (word-offsets (map 1+ (iota (length tail-words)))))
         (with-syntax ((name (datum->syntax #'nowhere name))
                       ((word* ...) vars)
                       ((n ...) word-offsets)
                       ((asm ...)
                        (parse-first-word #'first first-word))
                       (((len asm* ...) ...)
                        (map parse-tail-word vars tail-words word-offsets)))
           #'(lambda (buf offset first)
               (let ((word* (u32-ref buf (+ offset n)))
                     ...)
                 (values (+ 1 len ...)
                         (list 'name asm ... asm* ... ...))))))))))

;; -> len list
(define (disassemble-one buf offset)
  (let ((first (u32-ref buf offset)))
    (match (vector-ref disassemblers (logand first #xff))
      (#f (error "bad instruction" (logand first #xff) first buf offset))
      (disassemble (disassemble buf offset first)))))

(define (u32-offset->addr offset context)
  "Given an offset into an image in 32-bit units, return the absolute
address of that offset."
  (+ (debug-context-base context) (* offset 4)))

(define immediate-tag-annotations '())
(define-syntax-rule (define-immediate-tag-annotation name pred mask tag)
  (set! immediate-tag-annotations
        (cons `((,mask ,tag)
                ,(cond
                  ('pred => symbol->string)
                  (else (string-append "eq-" (symbol->string 'name) "?"))))
              immediate-tag-annotations)))
(visit-immediate-tags define-immediate-tag-annotation)

(define heap-tag-annotations '())
(define-syntax-rule (define-heap-tag-annotation name pred mask tag)
  (set! heap-tag-annotations
        (cons `((,mask ,tag) ,(symbol->string 'pred)) heap-tag-annotations)))

(visit-heap-tags define-heap-tag-annotation)

(define (sign-extended-immediate uimm n)
  (unpack-scm
   (if (>= uimm (ash 1 (- n 1)))
       (let ((word-bits (* (sizeof '*) 8))) ; FIXME
         (logand (1- (ash 1 word-bits))
                 (- uimm (ash 1 n))))
       uimm)))

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

  (define (intrinsic-name index)
    (and=> (intrinsic-index->name index)
           (compose list symbol->string)))

  (match code
    (((or 'j 'je 'jl 'jge 'jne 'jnl 'jnge) target)
     (list "-> ~A" (vector-ref labels (- (+ offset target) start))))
    (('immediate-tag=? _ mask tag)
     (assoc-ref immediate-tag-annotations (list mask tag)))
    (('heap-tag=? _ mask tag)
     (assoc-ref heap-tag-annotations (list mask tag)))
    (('prompt tag escape-only? proc-slot handler)
     ;; The H is for handler.
     (list "H -> ~A" (vector-ref labels (- (+ offset handler) start))))
    (((or 'make-immediate 'eq-immediate?) _ imm)
     (list "~S" (sign-extended-immediate imm 16)))
    (((or 'make-short-immediate 'make-long-immediate) _ imm)
     (list "~S" (unpack-scm imm)))
    (('make-long-long-immediate _ high low)
     (list "~S" (unpack-scm (logior (ash high 32) low))))
    (('assert-nargs-ee/locals nargs locals)
     ;; The nargs includes the procedure.
     (list "~a slot~:p (~a arg~:p)" (+ locals nargs) (1- nargs)))
    (('bind-optionals nargs)
     (list "~a args~:p" (1- nargs)))
    (('alloc-frame nlocals)
     (list "~a slot~:p" nlocals))
    (('reset-frame nlocals)
     (list "~a slot~:p" nlocals))
    (('bind-rest dst)
     (list "~a slot~:p" (1+ dst)))
    (('make-closure dst target nfree)
     (let* ((addr (u32-offset->addr (+ offset target) context))
            (pdi (find-program-debug-info addr context))
            (name (or (and pdi (program-debug-info-name pdi))
                      "anonymous procedure")))
       (push-addr! addr name)
       (list "~A at #x~X (~A free var~:p)" name addr nfree)))
    (('load-label dst src)
     (let* ((addr (u32-offset->addr (+ offset src) context))
            (pdi (find-program-debug-info addr context))
            (name (or (and pdi (program-debug-info-name pdi))
                      "anonymous procedure")))
       (push-addr! addr name)
       (list "~A at #x~X" name addr)))
    (('call-label closure nlocals target)
     (let* ((addr (u32-offset->addr (+ offset target) context))
            (pdi (find-program-debug-info addr context))
            (name (or (and pdi (program-debug-info-name pdi))
                      "anonymous procedure")))
       (push-addr! addr name)
       (list "~A at #x~X" name addr)))
    (('tail-call-label target)
     (let* ((addr (u32-offset->addr (+ offset target) context))
            (pdi (find-program-debug-info addr context))
            (name (or (and pdi (program-debug-info-name pdi))
                      "anonymous procedure")))
       (push-addr! addr name)
       (list "~A at #x~X" name addr)))

    ;; intrinsics
    (('call-thread index)
     (intrinsic-name index))
    (('call-thread-scm _ index)
     (intrinsic-name index))
    (('call-thread-scm-scm _ _ index)
     (intrinsic-name index))
    (('call-scm-sz-u32 _ _ index)
     (intrinsic-name index))
    (('call-scm<-thread _ index)
     (intrinsic-name index))
    (('call-scm<-u64 _ _ index)
     (intrinsic-name index))
    (('call-scm<-s64 _ _ index)
     (intrinsic-name index))
    (('call-scm<-scm _ _ index)
     (intrinsic-name index))
    (('call-u64<-scm _ _ index)
     (intrinsic-name index))
    (('call-s64<-scm _ _ index)
     (intrinsic-name index))
    (('call-f64<-scm _ _ index)
     (intrinsic-name index))
    (('call-scm<-scm-scm _ _ _ index)
     (intrinsic-name index))
    (('call-scm<-scm-uim _ _ _ index)
     (intrinsic-name index))
    (('call-scm<-scm-u64 _ _ _ index)
     (intrinsic-name index))
    (('call-scm-scm _ _ index)
     (intrinsic-name index))
    (('call-scm-scm-scm _ _ _ index)
     (intrinsic-name index))
    (('call-scm-uimm-scm _ _ _ index)
     (intrinsic-name index))
    (('call-scm<-scm-uimm _ _ _ index)
     (intrinsic-name index))
    (('call-scm<-scmn-scmn _ _ _ index)
     (intrinsic-name index))

    (('make-non-immediate dst target)
     (let ((val (reference-scm target)))
       (when (program? val)
         (push-addr! (program-code val) val))
       (list "~@Y" val)))
    (((or 'throw/value 'throw/value+data) dst target)
     (list "~@Y" (reference-scm target)))
    (('builtin-ref dst idx)
     (list "~A" (builtin-index->name idx)))
    (((or 'static-ref 'static-set!) _ target)
     (list "~@Y" (dereference-scm target)))
    (('resolve-module dst name public)
     (list "~a" (if (zero? public) "private" "public")))
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
                 ((j je jl jge jne jnl jnge)
                  (match arg
                    ((_ ... target)
                     (add-label! (+ offset target) "L"))))
                 ((prompt)
                  (match arg
                    ((_ ... target)
                     (add-label! (+ offset target) "H"))))
                 ((jtable)
                  (match arg
                    ((_ ... targets)
                     (let ((len (vector-length targets)))
                       (let lp ((i 0))
                         (when (< i len)
                           (add-label! (+ offset (vector-ref targets i)) "L")
                           (lp (1+ i)))))))))))
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
      (('load-label dst src)
       `(load-label ,dst ,(u32-offset->addr (+ offset src) context)))
      (('make-non-immediate dst target)
       `(make-non-immediate ,dst ,(reference-scm target)))
      (('builtin-ref dst idx)
       `(builtin-ref ,dst ,(builtin-index->name idx)))
      (((or 'static-ref 'static-set!) dst target)
       `(,(car code) ,dst ,(dereference-scm target)))
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

(define* (disassemble-file file #:optional (port (current-output-port)))
  (let* ((thunk (load-thunk-from-file file))
         (elf (find-mapped-elf-image (program-code thunk))))
    (disassemble-image elf port)))

(define-syntax instruction-lengths-vector
  (lambda (x)
    (syntax-case x ()
      ((_)
       (let ((lengths (make-vector 256 #f)))
         (for-each (match-lambda
                    ((name opcode kind word ... 'V32_X8_L24)
                     ;; Indicate variable-length instruction by setting
                     ;; statically known length to 0.
                     (vector-set! lengths opcode 0))
                    ((name opcode kind words ...)
                     (vector-set! lengths opcode (* 4 (length words)))))
                   (instruction-list))
         (datum->syntax x lengths))))))

(define (instruction-length code pos)
  (unless (zero? (modulo pos 4))
    (error "invalid pos"))
  (let ((opcode (logand (bytevector-u32-native-ref code pos) #xff)))
    (match (vector-ref (instruction-lengths-vector) opcode)
      (#f (error "Unknown opcode" opcode))
      (0 (call-with-values (lambda ()
                             (let ((offset (/ pos 4)))
                               (disassemble-one code offset)))
           (lambda (u32-len disasm)
             (* u32-len 4))))
      (len len))))

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
                     (bitvector-set-bit! bv (instruction-opcode inst)))
                   (syntax->datum #'(inst ...)))
         (datum->syntax #'static-opcode-set bv))))))

(define (instruction-has-fallthrough? code pos)
  (define non-fallthrough-set
    (static-opcode-set halt
                       throw throw/value throw/value+data unreachable
                       tail-call tail-call-label
                       return-values
                       subr-call foreign-call continuation-call
                       j jtable))
  (let ((opcode (logand (bytevector-u32-native-ref code pos) #xff)))
    (bitvector-bit-clear? non-fallthrough-set opcode)))

(define (word-offset->byte-offset n)
  (* n 4))

(define-op-handlers jump-parsers
  (lambda (op kind word-types)
    (case op
      ((prompt j je jl jge jne jnl jnge)
       #'(lambda (code pos)
           (call-with-values (lambda () (disassemble-one code (/ pos 4)))
             (lambda (len disasm)
               (match disasm
                 ;; Assume that the target is in the last word, as a
                 ;; word offset.
                 ((_ ___ target) (list (word-offset->byte-offset target))))))))
      ((jtable)
       #'(lambda (code pos)
           (call-with-values (lambda () (disassemble-one code (/ pos 4)))
             (lambda (len disasm)
               (match disasm
                 ;; Assume that the target is in the last word, as a
                 ;; vector of word offsets.
                 ((_ ___ targets)
                  (map word-offset->byte-offset (vector->list targets))))))))
      (else #f))))

(define (instruction-relative-jump-targets code pos)
  (let ((opcode (logand (bytevector-u32-native-ref code pos) #xff)))
    (match (vector-ref jump-parsers opcode)
      (#f '())
      (proc (proc code pos)))))

(define-op-handlers stack-effect-parsers
  (lambda (name kind word-types)
    (case name
      ((push)
       #'(lambda (code pos size) (and size (+ size 1))))
      ((pop)
       #'(lambda (code pos size) (and size (- size 1))))
      ((drop)
       #'(lambda (code pos size)
           (let ((count (ash (bytevector-u32-native-ref code pos) -8)))
             (and size (- size count)))))
      ((alloc-frame reset-frame bind-optionals)
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
      ((call call-label tail-call tail-call-label expand-apply-argument)
       #'(lambda (code pos size) #f))
      ((shuffle-down)
       #'(lambda (code pos size)
           (let ((from (logand (ash (bytevector-u32-native-ref code pos) -8)
                               #xfff))
                 (to (ash (bytevector-u32-native-ref code pos) -20)))
             (and size (- size (- from to))))))
      (else
       #f))))

(define (instruction-stack-size-after code pos size)
  (let ((opcode (logand (bytevector-u32-native-ref code pos) #xff)))
    (match (vector-ref stack-effect-parsers opcode)
      (#f size)
      (proc (proc code pos size)))))

(define-op-handlers clobber-parsers
  (lambda (name kind word-types)
    (match kind
      ('!
       (case name
         ((call call-label)
          #'(lambda (code pos nslots-in nslots-out)
              (call-with-values
                  (lambda ()
                    (disassemble-one code (/ pos 4)))
                (lambda (len elt)
                  (define frame-size 3)
                  (match elt
                    ((_ proc . _)
                     (let lp ((slot (- proc frame-size)))
                       (if (and nslots-in (< slot nslots-in))
                           (cons slot (lp (1+ slot)))
                           '()))))))))
         (else #f)))
      ('<-
       #`(lambda (code pos nslots-in nslots-out)
           (call-with-values (lambda ()
                               (disassemble-one code (/ pos 4)))
             (lambda (len elt)
               (match elt
                 ((_ dst . _)
                  #,(match word-types
                      (((or 'X8_F24 'X8_F12_F12) . _)
                       #'(list dst))
                      (else
                       #'(if nslots-out
                             (list (- nslots-out 1 dst))
                             '()))))))))))))

(define (instruction-slot-clobbers code pos nslots-in nslots-out)
  (let ((opcode (logand (bytevector-u32-native-ref code pos) #xff)))
    (match (vector-ref clobber-parsers opcode)
      (#f '())
      (proc (proc code pos nslots-in nslots-out)))))
