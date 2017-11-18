;;; Guile runtime debug information

;;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.
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
;;; Guile's bytecode compiler and linker serialize debugging information
;;; into separate sections of the ELF image.  This module reads those
;;; sections.
;;;
;;; Code:

(define-module (system vm debug)
  #:use-module (system vm elf)
  #:use-module (system vm dwarf)
  #:use-module (system vm loader)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold split-at))
  #:use-module (srfi srfi-9)
  #:export (debug-context-image
            debug-context-base
            debug-context-length
            debug-context-text-base

            program-debug-info-name
            program-debug-info-context
            program-debug-info-image
            program-debug-info-offset
            program-debug-info-size
            program-debug-info-addr
            program-debug-info-u32-offset
            program-debug-info-u32-offset-end

            arity?
            arity-low-pc
            arity-high-pc
            arity-nreq
            arity-nopt
            arity-nlocals
            arity-has-rest?
            arity-allow-other-keys?
            arity-has-keyword-args?
            arity-keyword-args
            arity-is-case-lambda?
            arity-definitions
            arity-code

            debug-context-from-image
            fold-all-debug-contexts
            for-each-elf-symbol
            find-debug-context
            find-program-debug-info
            arity-arguments-alist
            find-program-arities
            find-program-arity
            find-program-minimum-arity

            find-program-docstring

            find-program-properties

            source?
            source-pre-pc
            source-post-pc
            source-file
            source-line
            source-line-for-user
            source-column
            find-source-for-addr
            find-program-sources
            fold-source-locations))

;;; A compiled procedure comes from a specific loaded ELF image.  A
;;; debug context identifies that image.
;;;
(define-record-type <debug-context>
  (make-debug-context elf base text-base)
  debug-context?
  (elf debug-context-elf)
  ;; Address at which this image is loaded in memory, in bytes.
  (base debug-context-base)
  ;; Offset of the text section relative to the image start, in bytes.
  (text-base debug-context-text-base))

(define (debug-context-image context)
  "Return the bytevector aliasing the mapped ELF image corresponding to
@var{context}."
  (elf-bytes (debug-context-elf context)))

(define (debug-context-length context)
  "Return the size of the mapped ELF image corresponding to
@var{context}, in bytes."
  (bytevector-length (debug-context-image context)))

(define (for-each-elf-symbol context proc)
  "Call @var{proc} on each symbol in the symbol table of @var{context}."
  (let ((elf (debug-context-elf context)))
    (cond
     ((elf-section-by-name elf ".symtab")
      => (lambda (symtab)
           (let ((len (elf-symbol-table-len symtab))
                 (strtab (elf-section elf (elf-section-link symtab))))
             (let lp ((n 0))
               (when (< n len)
                 (proc (elf-symbol-table-ref elf symtab n strtab))
                 (lp (1+ n))))))))))

;;; A program debug info (PDI) is a handle on debugging meta-data for a
;;; particular program.
;;;
(define-record-type <program-debug-info>
  (make-program-debug-info context name offset size)
  program-debug-info?
  (context program-debug-info-context)
  (name program-debug-info-name)
  ;; Offset of the procedure in the text section, in bytes.
  (offset program-debug-info-offset)
  (size program-debug-info-size))

(define (program-debug-info-addr pdi)
  "Return the address in memory of the entry of the program represented
by the debugging info @var{pdi}."
  (+ (program-debug-info-offset pdi)
     (debug-context-text-base (program-debug-info-context pdi))
     (debug-context-base (program-debug-info-context pdi))))

(define (program-debug-info-image pdi)
  "Return the ELF image containing @var{pdi}, as a bytevector."
  (debug-context-image (program-debug-info-context pdi)))

(define (program-debug-info-u32-offset pdi)
  "Return the start address of the program represented by @var{pdi}, as
an offset from the beginning of the ELF image in 32-bit units."
  (/ (+ (program-debug-info-offset pdi)
        (debug-context-text-base (program-debug-info-context pdi)))
     4))

(define (program-debug-info-u32-offset-end pdi)
  "Return the end address of the program represented by @var{pdi}, as an
offset from the beginning of the ELF image in 32-bit units."
  (/ (+ (program-debug-info-size pdi)
        (program-debug-info-offset pdi)
        (debug-context-text-base (program-debug-info-context pdi)))
     4))

(define (debug-context-from-image bv)
  "Build a debugging context corresponding to a given ELF image."
  (let* ((elf (parse-elf bv))
         (base (pointer-address (bytevector->pointer (elf-bytes elf))))
         (text-base (elf-section-offset
                     (or (elf-section-by-name elf ".rtl-text")
                         (error "ELF object has no text section")))))
    (make-debug-context elf base text-base)))

(define (fold-all-debug-contexts proc seed)
  "Fold @var{proc} over debug contexts corresponding to all images that
are mapped at the time this procedure is called.  Any images mapped
during the fold are omitted."
  (fold (lambda (image seed)
          (proc (debug-context-from-image image) seed))
        seed
        (all-mapped-elf-images)))

(define (find-debug-context addr)
  "Find and return the debugging context corresponding to the ELF image
containing the address @var{addr}.  @var{addr} is an integer.  If no ELF
image is found, return @code{#f}.  It's possible for an bytecode program
not to have an ELF image if the program was defined in as a stub in C."
  (and=> (find-mapped-elf-image addr)
         debug-context-from-image))

(define-inlinable (binary-search start end inc try failure)
  (let lp ((start start) (end end))
    (if (eqv? start end)
        (failure)
        (let ((mid (+ start (* inc (floor/ (- end start) (* 2 inc))))))
          (try mid
               (lambda ()
                 (lp start mid))
               (lambda ()
                 (lp (+ mid inc) end)))))))

(define (find-elf-symbol elf text-offset)
  "Search the symbol table of @var{elf} for the ELF symbol containing
@var{text-offset}.  @var{text-offset} is a byte offset in the text
section of the ELF image.  Returns an ELF symbol, or @code{#f}."
  (and=>
   (elf-section-by-name elf ".symtab")
   (lambda (symtab)
     (let ((strtab (elf-section elf (elf-section-link symtab))))
       (binary-search
        0 (elf-symbol-table-len symtab) 1
        (lambda (n continue-before continue-after)
          (let* ((sym (elf-symbol-table-ref elf symtab n strtab))
                 (val (elf-symbol-value sym))
                 (size (elf-symbol-size sym)))
            (cond
             ((< text-offset val) (continue-before))
             ((<= (+ val size) text-offset) (continue-after))
             (else sym))))
        (lambda ()
          #f))))))

(define* (find-program-debug-info addr #:optional
                                  (context (find-debug-context addr)))
  "Find and return the @code{<program-debug-info>} containing
@var{addr}, or @code{#f}."
  (cond
   ((and context
         (find-elf-symbol (debug-context-elf context)
                          (- addr
                             (debug-context-base context)
                             (debug-context-text-base context))))
    => (lambda (sym)
         (make-program-debug-info context
                                  (and=> (elf-symbol-name sym)
                                         ;; The name might be #f if
                                         ;; the string table was
                                         ;; stripped somehow.
                                         (lambda (x)
                                           (and (string? x)
                                                (not (string-null? x))
                                                (string->symbol x))))
                                  (elf-symbol-value sym)
                                  (elf-symbol-size sym))))
   (else #f)))

(define-record-type <arity>
  (make-arity context base header-offset)
  arity?
  (context arity-context)
  (base arity-base)
  (header-offset arity-header-offset))

(define arities-prefix-len 4)
(define arity-header-len (* 7 4))

;;;   struct arity_header {
;;;     uint32_t low_pc;
;;;     uint32_t high_pc;
;;;     uint32_t offset;
;;;     uint32_t flags;
;;;     uint32_t nreq;
;;;     uint32_t nopt;
;;;     uint32_t nlocals;
;;;   }

(define (arity-low-pc* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 0 4))))
(define (arity-high-pc* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 1 4))))
(define (arity-offset* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 2 4))))
(define (arity-flags* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 3 4))))
(define (arity-nreq* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 4 4))))
(define (arity-nopt* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 5 4))))
(define (arity-nlocals* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 6 4))))

;;;    #x1: has-rest?
;;;    #x2: allow-other-keys?
;;;    #x4: has-keyword-args?
;;;    #x8: is-case-lambda?
;;;   #x10: is-in-case-lambda?

(define (has-rest? flags)         (not (zero? (logand flags (ash 1 0)))))
(define (allow-other-keys? flags) (not (zero? (logand flags (ash 1 1)))))
(define (has-keyword-args? flags) (not (zero? (logand flags (ash 1 2)))))
(define (is-case-lambda? flags)   (not (zero? (logand flags (ash 1 3)))))
(define (is-in-case-lambda? flags) (not (zero? (logand flags (ash 1 4)))))

(define (arity-low-pc arity)
  (let ((ctx (arity-context arity)))
    (+ (debug-context-base ctx)
       (debug-context-text-base ctx)
       (arity-low-pc* (elf-bytes (debug-context-elf ctx))
                      (arity-header-offset arity)))))

(define (arity-high-pc arity)
  (let ((ctx (arity-context arity)))
    (+ (debug-context-base ctx)
       (debug-context-text-base ctx)
       (arity-high-pc* (elf-bytes (debug-context-elf ctx))
                       (arity-header-offset arity)))))

(define (arity-nreq arity)
  (arity-nreq* (elf-bytes (debug-context-elf (arity-context arity)))
               (arity-header-offset arity)))

(define (arity-nopt arity)
  (arity-nopt* (elf-bytes (debug-context-elf (arity-context arity)))
               (arity-header-offset arity)))

(define (arity-nlocals arity)
  (arity-nlocals* (elf-bytes (debug-context-elf (arity-context arity)))
                  (arity-header-offset arity)))

(define (arity-flags arity)
  (arity-flags* (elf-bytes (debug-context-elf (arity-context arity)))
                (arity-header-offset arity)))

(define (arity-has-rest? arity) (has-rest? (arity-flags arity)))
(define (arity-allow-other-keys? arity) (allow-other-keys? (arity-flags arity)))
(define (arity-has-keyword-args? arity) (has-keyword-args? (arity-flags arity)))
(define (arity-is-case-lambda? arity) (is-case-lambda? (arity-flags arity)))
(define (arity-is-in-case-lambda? arity) (is-in-case-lambda? (arity-flags arity)))

(define (arity-keyword-args arity)
  (define (unpack-scm n)
    (pointer->scm (make-pointer n)))
  (if (arity-has-keyword-args? arity)
      (let* ((bv (elf-bytes (debug-context-elf (arity-context arity))))
             (header (arity-header-offset arity))
             (link-offset (arity-offset* bv header))
             (link (+ (arity-base arity) link-offset))
             (offset (bytevector-u32-native-ref bv link)))
        (unpack-scm (+ (debug-context-base (arity-context arity)) offset)))
      '()))

(define (arity-load-symbol arity)
  (let ((elf (debug-context-elf (arity-context arity))))
    (cond
     ((elf-section-by-name elf ".guile.arities")
      =>
      (lambda (sec)
        (let* ((strtab (elf-section elf (elf-section-link sec)))
               (bv (elf-bytes elf))
               (strtab-offset (elf-section-offset strtab)))
          (lambda (n)
            (string->symbol (string-table-ref bv (+ strtab-offset n)))))))
     (else (error "couldn't find arities section")))))

(define* (arity-definitions arity)
  (let* ((bv (elf-bytes (debug-context-elf (arity-context arity))))
         (load-symbol (arity-load-symbol arity))
         (header (arity-header-offset arity))
         (nlocals (arity-nlocals* bv header))
         (flags (arity-flags* bv header))
         (link-offset (arity-offset* bv header))
         (link (+ (arity-base arity)
                  link-offset
                  (if (has-keyword-args? flags) 4 0))))
    (define (read-uleb128 bv pos)
      ;; Unrolled by one.
      (let ((b (bytevector-u8-ref bv pos)))
        (if (zero? (logand b #x80))
            (values b
                    (1+ pos))
            (let lp ((n (logxor #x80 b)) (pos (1+ pos)) (shift 7))
              (let ((b (bytevector-u8-ref bv pos)))
                (if (zero? (logand b #x80))
                    (values (logior (ash b shift) n)
                            (1+ pos))
                    (lp (logior (ash (logxor #x80 b) shift) n)
                        (1+ pos)
                        (+ shift 7))))))))
    (define (load-definitions pos names)
      (let lp ((pos pos) (names names))
        (match names
          (() '())
          ((name . names)
           (call-with-values (lambda () (read-uleb128 bv pos))
             (lambda (def-offset pos)
               (call-with-values (lambda () (read-uleb128 bv pos))
                 (lambda (slot+representation pos)
                   (let ((slot (ash slot+representation -2))
                         (representation (case (logand slot+representation #x3)
                                           ((0) 'scm)
                                           ((1) 'f64)
                                           ((2) 'u64)
                                           ((3) 's64)
                                           (else 'unknown))))
                     (cons (vector name def-offset slot representation)
                           (lp pos names)))))))))))
    (define (load-symbols pos)
      (let lp ((pos pos) (n nlocals) (out '()))
        (if (zero? n)
            (load-definitions pos (reverse out))
            (call-with-values (lambda () (read-uleb128 bv pos))
              (lambda (strtab-offset pos)
                strtab-offset
                (lp pos
                    (1- n)
                    (cons (if (zero? strtab-offset)
                              #f
                              (load-symbol strtab-offset))
                          out)))))))
    (when (is-case-lambda? flags)
      (error "invalid request for definitions of case-lambda wrapper arity"))
    (load-symbols link)))

(define (arity-code arity)
  (let* ((ctx (arity-context arity))
         (bv (elf-bytes (debug-context-elf ctx)))
         (header (arity-header-offset arity))
         (base-addr (+ (debug-context-base ctx) (debug-context-text-base ctx)))
         (low-pc (+ base-addr (arity-low-pc* bv header)))
         (high-pc (+ base-addr (arity-high-pc* bv header))))
    ;; FIXME: We should be able to use a sub-bytevector operation here;
    ;; it would be safer.
    (pointer->bytevector (make-pointer low-pc) (- high-pc low-pc))))

(define* (arity-locals arity #:optional nlocals)
  (let* ((bv (elf-bytes (debug-context-elf (arity-context arity))))
         (load-symbol (arity-load-symbol arity))
         (header (arity-header-offset arity))
         (nlocals (if nlocals
                      (if (<= 0 nlocals (arity-nlocals* bv header))
                          nlocals
                          (error "request for too many locals"))
                      (arity-nlocals* bv header)))
         (flags (arity-flags* bv header))
         (link-offset (arity-offset* bv header))
         (link (+ (arity-base arity)
                  link-offset
                  (if (has-keyword-args? flags) 4 0))))
    (define (read-uleb128 bv pos)
      ;; Unrolled by one.
      (let ((b (bytevector-u8-ref bv pos)))
        (if (zero? (logand b #x80))
            (values b
                    (1+ pos))
            (let lp ((n (logxor #x80 b)) (pos (1+ pos)) (shift 7))
              (let ((b (bytevector-u8-ref bv pos)))
                (if (zero? (logand b #x80))
                    (values (logior (ash b shift) n)
                            (1+ pos))
                    (lp (logior (ash (logxor #x80 b) shift) n)
                        (1+ pos)
                        (+ shift 7))))))))
    (define (load-symbols pos n)
      (let lp ((pos pos) (n n) (out '()))
        (if (zero? n)
            (reverse out)
            (call-with-values (lambda () (read-uleb128 bv pos))
              (lambda (strtab-offset pos)
                strtab-offset
                (lp pos
                    (1- n)
                    (cons (if (zero? strtab-offset)
                              #f
                              (load-symbol strtab-offset))
                          out)))))))
    (when (is-case-lambda? flags)
      (error "invalid request for locals of case-lambda wrapper arity"))
    (load-symbols link nlocals)))

(define (arity-arguments-alist arity)
  (let* ((bv (elf-bytes (debug-context-elf (arity-context arity))))
         (header (arity-header-offset arity))
         (flags (arity-flags* bv header))
         (nreq (arity-nreq* bv header))
         (nopt (arity-nopt* bv header))
         (nargs (+ nreq nopt (if (has-rest? flags) 1 0)))
         (nargs+closure (1+ nargs)))
    (when (is-case-lambda? flags)
      (error "invalid request for locals of case-lambda wrapper arity"))
    (match (arity-locals arity nargs+closure)
      ((closure . args)
       (call-with-values (lambda () (split-at args nreq))
         (lambda (req args)
           (call-with-values (lambda () (split-at args nopt))
             (lambda (opt args)
               `((required . ,req)
                 (optional . ,opt)
                 (keyword . ,(arity-keyword-args arity))
                 (allow-other-keys? . ,(allow-other-keys? flags))
                 (rest . ,(and (has-rest? flags) (car args))))))))))))

(define (find-first-arity context base addr)
  (let* ((bv (elf-bytes (debug-context-elf context)))
         (text-offset (- addr
                         (debug-context-text-base context)
                         (debug-context-base context))))
    (binary-search
     (+ base arities-prefix-len)
     (+ base (bytevector-u32-native-ref bv base))
     arity-header-len
     (lambda (pos continue-before continue-after)
       (let lp ((pos pos))
         (cond
          ((is-in-case-lambda? (arity-flags* bv pos))
           (lp (- pos arity-header-len)))
          ((< text-offset (arity-low-pc* bv pos))
           (continue-before))
          ((<= (arity-high-pc* bv pos) text-offset)
           (continue-after))
          (else
           (make-arity context base pos)))))
     (lambda ()
       #f))))

(define (read-sub-arities context base outer-header-offset)
  (let* ((bv (elf-bytes (debug-context-elf context)))
         (headers-end (+ base (bytevector-u32-native-ref bv base)))
         (low-pc (arity-low-pc* bv outer-header-offset))
         (high-pc (arity-high-pc* bv outer-header-offset)))
    (let lp ((pos (+ outer-header-offset arity-header-len)) (out '()))
      (if (and (< pos headers-end) (<= (arity-high-pc* bv pos) high-pc))
          (lp (+ pos arity-header-len)
              (cons (make-arity context base pos) out))
          (reverse out)))))

(define* (find-program-arities addr #:optional
                               (context (find-debug-context addr)))
  (and=>
   (and context
        (elf-section-by-name (debug-context-elf context) ".guile.arities"))
   (lambda (sec)
     (let* ((base (elf-section-offset sec))
            (first (find-first-arity context base addr)))
       (cond
        ((not first) '())
        ((arity-is-case-lambda? first)
         (read-sub-arities context base (arity-header-offset first)))
        (else (list first)))))))

(define* (find-program-arity addr #:optional
                             (context (find-debug-context addr)))
  (let lp ((arities (or (find-program-arities addr context) '())))
    (match arities
      (() #f)
      ((arity . arities)
       (if (and (<= (arity-low-pc arity) addr)
                (< addr (arity-high-pc arity)))
           arity
           (lp arities))))))

(define* (find-program-minimum-arity addr #:optional
                                     (context (find-debug-context addr)))
  (and=>
   (and context
        (elf-section-by-name (debug-context-elf context) ".guile.arities"))
   (lambda (sec)
     (let* ((base (elf-section-offset sec))
            (first (find-first-arity context base addr)))
       (if (arity-is-case-lambda? first)
           (let ((arities (read-sub-arities context base
                                            (arity-header-offset first))))
             (and (pair? arities)
                  (list (apply min (map arity-nreq arities))
                        0
                        (or-map (lambda (arity)
                                  (or (positive? (arity-nopt arity))
                                      (arity-has-rest? arity)
                                      (arity-has-keyword-args? arity)
                                      (arity-allow-other-keys? arity)))
                                arities))))
           (list (arity-nreq first)
                 (arity-nopt first)
                 (arity-has-rest? first)))))))

(define* (find-program-docstring addr #:optional
                                 (context (find-debug-context addr)))
  (and=>
   (and context
        (elf-section-by-name (debug-context-elf context) ".guile.docstrs"))
   (lambda (sec)
     ;; struct docstr {
     ;;   uint32_t pc;
     ;;   uint32_t str;
     ;; }
     (let ((start (elf-section-offset sec))
           (bv (elf-bytes (debug-context-elf context)))
           (text-offset (- addr
                           (debug-context-text-base context)
                           (debug-context-base context))))
       (binary-search
        start
        (+ start (elf-section-size sec))
        8
        (lambda (pos continue-before continue-after)
          (let ((pc (bytevector-u32-native-ref bv pos)))
            (cond
             ((< text-offset pc) (continue-before))
             ((< pc text-offset) (continue-after))
             (else
              (let ((strtab (elf-section (debug-context-elf context)
                                         (elf-section-link sec)))
                    (idx (bytevector-u32-native-ref bv (+ pos 4))))
                (string-table-ref bv (+ (elf-section-offset strtab) idx)))))))
        (lambda ()
          #f))))))

(define* (find-program-properties addr #:optional
                                  (context (find-debug-context addr)))
  (define (add-name-and-docstring props)
    (define (maybe-acons k v tail)
      (if v (acons k v tail) tail))
    (let ((name (and=> (find-program-debug-info addr context)
                       program-debug-info-name))
          (docstring (find-program-docstring addr context)))
      (maybe-acons 'name name
                   (maybe-acons 'documentation docstring props))))
  (add-name-and-docstring
   (cond
    ((and context
          (elf-section-by-name (debug-context-elf context) ".guile.procprops"))
     => (lambda (sec)
          ;; struct procprop {
          ;;   uint32_t pc;
          ;;   uint32_t offset;
          ;; }
          (define procprop-len 8)
          (let* ((start (elf-section-offset sec))
                 (bv (elf-bytes (debug-context-elf context)))
                 (text-offset (- addr
                                 (debug-context-text-base context)
                                 (debug-context-base context))))
            (define (unpack-scm addr)
              (pointer->scm (make-pointer addr)))
            (define (load-non-immediate offset)
              (unpack-scm (+ (debug-context-base context) offset)))
            (binary-search
             start (+ start (elf-section-size sec)) 8
             (lambda (pos continue-before continue-after)
               (let ((pc (bytevector-u32-native-ref bv pos)))
                 (cond
                  ((< text-offset pc) (continue-before))
                  ((< pc text-offset) (continue-after))
                  (else
                   (load-non-immediate
                    (bytevector-u32-native-ref bv (+ pos 4)))))))
             (lambda ()
               '())))))
    (else '()))))

(define-record-type <source>
  (make-source pre-pc file line column)
  source?
  (pre-pc source-pre-pc)
  (file source-file)
  (line source-line)
  (column source-column))

(define (make-source/dwarf pc file line column)
  (make-source pc file
               ;; Convert DWARF-numbered (1-based) lines and
               ;; columns to Guile conventions (0-based).
               (and line (1- line)) (and column (1- column))))

;; FIXME
(define (source-post-pc source)
  (source-pre-pc source))

;; Lines are zero-indexed inside Guile, but users expect them to be
;; one-indexed. Columns, on the other hand, are zero-indexed to both. Go
;; figure.
(define (source-line-for-user source)
  (and (source-line source) (1+ (source-line source))))

(define* (find-source-for-addr addr #:optional
                               (context (find-debug-context addr))
                               #:key exact?)
  (and=>
   (and context
        (false-if-exception
         (elf->dwarf-context (debug-context-elf context))))
   (lambda (dwarf-ctx)
     (let* ((base (debug-context-base context))
            (pc (- addr base)))
       (or-map (lambda (die)
                 (and=>
                  (die-line-prog die)
                  (lambda (prog)
                    (call-with-values
                        (lambda () (line-prog-scan-to-pc prog pc))
                      (lambda (pc* file line col)
                        (and pc* (or (= pc pc*) (not exact?))
                             (make-source/dwarf (+ pc* base)
                                                file line col)))))))
               (read-die-roots dwarf-ctx))))))

(define* (find-program-die addr #:optional
                           (context (find-debug-context addr)))
  (and=> (and context
              (false-if-exception
               (elf->dwarf-context (debug-context-elf context))))
         (lambda (dwarf-ctx)
           (find-die-by-pc (read-die-roots dwarf-ctx)
                           (- addr (debug-context-base context))))))

(define* (find-program-sources addr #:optional
                               (context (find-debug-context addr)))
  (cond
   ((find-program-die addr context)
    => (lambda (die)
         (let* ((base (debug-context-base context))
                (low-pc (die-ref die 'low-pc))
                (high-pc (die-high-pc die))
                (prog (let line-prog ((die die))
                        (and die
                             (or (die-line-prog die)
                                 (line-prog (ctx-die (die-ctx die))))))))
           (cond
            ((and low-pc high-pc prog)
             (let lp ((sources '()))
               (call-with-values (lambda ()
                                   (if (null? sources)
                                       (line-prog-scan-to-pc prog low-pc)
                                       (line-prog-advance prog)))
                 (lambda (pc file line col)
                   (if (and pc (< pc high-pc))
                       ;; For the first source, it's probable that the
                       ;; address of the line program is before the
                       ;; low-pc, since the line program is for the
                       ;; entire compilation unit, and there are no
                       ;; redundant "rows" in the line program.
                       ;; Therefore in that case use the addr of low-pc
                       ;; instead of the one we got back.
                       (let ((addr (+ (if (null? sources) low-pc pc) base)))
                         (lp (cons (make-source/dwarf addr file line col)
                                   sources)))
                       (reverse sources))))))
            (else '())))))
   (else '())))

(define* (fold-source-locations proc seed context)
  "Fold @var{proc} over all source locations in @var{context}.
@var{proc} will be called with two arguments: the source object and the
seed."
  (cond
   ((and context
         (false-if-exception
          (elf->dwarf-context (debug-context-elf context))))
    =>
    (lambda (dwarf-ctx)
      (let ((base (debug-context-base context)))
        (fold
         (lambda (die seed)
           (cond
            ((die-line-prog die)
             =>
             (lambda (prog)
               (let lp ((seed seed))
                 (call-with-values
                     (lambda () (line-prog-advance prog))
                   (lambda (pc* file line col)
                     (if pc*
                         (lp
                          (proc (make-source/dwarf (+ pc* base) file line col)
                                seed))
                         seed))))))
            (else seed)))
         seed
         (read-die-roots dwarf-ctx)))))
   (else seed)))
