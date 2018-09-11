;;; Guile ELF linker

;; Copyright (C)  2011, 2012, 2013, 2014, 2018 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; A linker combines several linker objects into an executable or a
;;; loadable library.
;;;
;;; There are several common formats for libraries out there.  Since
;;; Guile includes its own linker and loader, we are free to choose any
;;; format, or make up our own.
;;;
;;; There are essentially two requirements for a linker format:
;;; libraries should be able to be loaded with the minimal amount of
;;; work; and they should support introspection in some way, in order to
;;; enable good debugging.
;;;
;;; These requirements are somewhat at odds, as loading should not have
;;; to stumble over features related to introspection.  It so happens
;;; that a lot of smart people have thought about this situation, and
;;; the ELF format embodies the outcome of their thinking.  Guile uses
;;; ELF as its format, regardless of the platform's native library
;;; format.  It's not inconceivable that Guile could interoperate with
;;; the native dynamic loader at some point, but it's not a near-term
;;; goal.
;;;
;;; Guile's linker takes a list of objects, sorts them according to
;;; similarity from the perspective of the loader, then writes them out
;;; into one big bytevector in ELF format.
;;;
;;; It is often the case that different parts of a library need to refer
;;; to each other.  For example, program text may need to refer to a
;;; constant from writable memory.  When the linker places sections
;;; (linker objects) into specific locations in the linked bytevector,
;;; it needs to fix up those references.  This process is called
;;; /relocation/.  References needing relocations are recorded in
;;; "linker-reloc" objects, and collected in a list in each
;;; "linker-object".  The actual definitions of the references are
;;; stored in "linker-symbol" objects, also collected in a list in each
;;; "linker-object".
;;;
;;; By default, the ELF files created by the linker include some padding
;;; so that different parts of the file can be loaded in with different
;;; permissions.  For example, some parts of the file are read-only and
;;; thus can be shared between processes.  Some parts of the file don't
;;; need to be loaded at all.  However this padding can be too much for
;;; interactive compilation, when the code is never written out to disk;
;;; in that case, pass #:page-aligned? #f to `link-elf'.
;;;
;;; Code:

(define-module (system vm linker)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system base target)
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (system vm elf)
  #:export (make-linker-reloc
            make-linker-symbol

            make-linker-object
            linker-object?
            linker-object-section
            linker-object-bv
            linker-object-relocs
            (linker-object-symbols* . linker-object-symbols)

            make-string-table
            string-table-intern!
            link-string-table!

            link-elf))

(define-syntax fold-values
  (lambda (x)
    (syntax-case x ()
      ((_ proc list seed ...)
       (with-syntax (((s ...) (generate-temporaries #'(seed ...))))
         #'(let ((p proc))
             (let lp ((l list) (s seed) ...)
               (match l
                 (() (values s ...))
                 ((elt . l)
                  (call-with-values (lambda () (p elt s ...))
                    (lambda (s ...) (lp l s ...))))))))))))

;; A relocation records a reference to a symbol.  When the symbol is
;; resolved to an address, the reloc location will be updated to point
;; to the address.
;;
;; Two types.  Abs32/1 and Abs64/1 are absolute offsets in bytes.
;; Rel32/1 and Rel32/1 are relative signed offsets, in 8-bit or 32-bit
;; units, respectively.  Either can have an arbitrary addend as well.
;;
(define-record-type <linker-reloc>
  (make-linker-reloc type loc addend symbol)
  linker-reloc?
  (type linker-reloc-type) ;; rel32/1, rel32/4, abs32/1, abs64/1
  (loc linker-reloc-loc)
  (addend linker-reloc-addend)
  (symbol linker-reloc-symbol))

;; A symbol is an association between a name and an address.  The
;; address is always in regard to some particular address space.  When
;; objects come into the linker, their symbols live in the object
;; address space.  When the objects are allocated into ELF segments, the
;; symbols will be relocated into memory address space, corresponding to
;; the position the ELF will be loaded at.
;;
(define-record-type <linker-symbol>
  (make-linker-symbol name address)
  linker-symbol?
  (name linker-symbol-name)
  (address linker-symbol-address))

(define-record-type <linker-object>
  (%make-linker-object section bv relocs symbols)
  linker-object?
  (section linker-object-section)
  (bv linker-object-bv)
  (relocs linker-object-relocs)
  (symbols linker-object-symbols))

(define (make-linker-object section bv relocs symbols)
  "Create a linker object with the @code{<elf-section>} header
@var{section}, bytevector contents @var{bv}, list of linker relocations
@var{relocs}, and list of linker symbols @var{symbols}."
  (%make-linker-object section bv relocs
                       ;; Hide a symbol to the beginning of the section
                       ;; in the symbols.
                       (cons (make-linker-symbol (gensym "*section*") 0)
                             symbols)))
(define (linker-object-section-symbol object)
  "Return the linker symbol corresponding to the start of this section."
  (car (linker-object-symbols object)))
(define (linker-object-symbols* object)
  "Return the linker symbols defined by the user for this this section."
  (cdr (linker-object-symbols object)))

(define-record-type <string-table>
  (%make-string-table strings linked?)
  string-table?
  (strings string-table-strings set-string-table-strings!)
  (linked? string-table-linked? set-string-table-linked?!))

(define (make-string-table)
  "Return a string table with one entry: the empty string."
  (%make-string-table '(("" 0 #vu8())) #f))

(define (string-table-length strings)
  "Return the number of bytes needed for the @var{strings}."
  (match strings
    (((str pos bytes) . _)
     ;; The + 1 is for the trailing NUL byte.
     (+ pos (bytevector-length bytes) 1))))

(define (string-table-intern! table str)
  "Ensure that @var{str} is present in the string table @var{table}.
Returns the byte index of the string in that table."
  (match table
    (($ <string-table> strings linked?)
     (match (assoc str strings)
       ((_ pos _) pos)
       (#f
        (let ((next (string-table-length strings)))
          (when linked?
            (error "string table already linked, can't intern" table str))
          (set-string-table-strings! table
                                     (cons (list str next (string->utf8 str))
                                           strings))
          next))))))

(define (link-string-table! table)
  "Link the functional string table @var{table} into a sequence of
bytes, suitable for use as the contents of an ELF string table section."
  (match table
    (($ <string-table> strings #f)
     (let ((out (make-bytevector (string-table-length strings) 0)))
       (for-each
        (match-lambda
         ((_ pos bytes)
          (bytevector-copy! bytes 0 out pos (bytevector-length bytes))))
        strings)
       (set-string-table-linked?! table #t)
       out))))

(define (segment-kind section)
  "Return the type of segment needed to store @var{section}, as a pair.
The car is the @code{PT_} segment type, or @code{#f} if the section
doesn't need to be present in a loadable segment.  The cdr is a bitfield
of associated @code{PF_} permissions."
  (let ((flags (elf-section-flags section)))
    ;; Sections without SHF_ALLOC don't go in segments.
    (cons (if (zero? flags) #f PT_LOAD)
          (logior (if (logtest SHF_ALLOC flags) PF_R 0)
                  (if (logtest SHF_EXECINSTR flags) PF_X 0)
                  (if (logtest SHF_WRITE flags) PF_W 0)))))

(define (count-segments objects)
  "Return the total number of segments needed to represent the linker
objects in @var{objects}, including the segment needed for the ELF
header and segment table."
  (define (adjoin x xs)
    (if (member x xs) xs (cons x xs)))
  (length
   (fold-values (lambda (object kinds)
                  (let ((kind (segment-kind (linker-object-section object))))
                    (if (= (elf-section-type (linker-object-section object))
                           SHT_DYNAMIC)
                        ;; The dynamic section is part of a loadable
                        ;; segment, and also gets the additional
                        ;; PT_DYNAMIC segment header.
                        (cons (cons PT_DYNAMIC (cdr kind))
                              (adjoin kind kinds))
                        (if (car kind) (adjoin kind kinds) kinds))))
                objects
                ;; We know there will be at least one segment,
                ;; containing at least the header and segment table.
                (list (cons PT_LOAD PF_R)))))

(define (group-by-cars ls)
  (let lp ((ls ls) (k #f) (group #f) (out '()))
    (match ls
      (()
       (reverse!
        (if group
            (cons (cons k (reverse! group)) out)
            out)))
      (((k* . v) . ls)
       (if (and group (equal? k k*))
           (lp ls k (cons v group) out)
           (lp ls k* (list v)
               (if group
                   (cons (cons k (reverse! group)) out)
                   out)))))))

(define (collate-objects-into-segments objects)
  "Given the list of linker objects @var{objects}, group them into
contiguous ELF segments of the same type and flags.  The result is an
alist that maps segment types to lists of linker objects.  See
@code{segment-type} for a description of segment types.  Within a
segment, the order of the linker objects is preserved."
  (group-by-cars
   (stable-sort!
    (map (lambda (o)
           (cons (segment-kind (linker-object-section o)) o))
         objects)
    (lambda (x y)
      (let* ((x-kind (car x)) (y-kind (car y))
             (x-type (car x-kind)) (y-type (car y-kind))
             (x-flags (cdr x-kind)) (y-flags (cdr y-kind))
             (x-section (linker-object-section (cdr x)))
             (y-section (linker-object-section (cdr y))))
        (cond
         ((not (equal? x-kind y-kind))
          (cond
           ((and x-type y-type)
            (cond
             ((not (equal? x-flags y-flags))
              (< x-flags y-flags))
             (else
              (< x-type y-type))))
           (else
            (not y-type))))
         ((not (equal? (elf-section-type x-section)
                       (elf-section-type y-section)))
          (cond
           ((equal? (elf-section-type x-section) SHT_NOBITS) #t)
           ((equal? (elf-section-type y-section) SHT_NOBITS) #f)
           (else (< (elf-section-type x-section)
                    (elf-section-type y-section)))))
         (else
          ;; Leave them in the initial order.  This allows us to ensure
          ;; that the ELF header is written first.
          #f)))))))

(define (align address alignment)
  (if (zero? alignment)
      address
      (+ address
         (modulo (- alignment (modulo address alignment)) alignment))))

(define (relocate-section-header sec offset)
  "Return a new section header, just like @var{sec} but with its
@code{offset} (and @code{addr} if it is loadable) set to @var{offset}."
  (make-elf-section #:index (elf-section-index sec)
                    #:name (elf-section-name sec)
                    #:type (elf-section-type sec)
                    #:flags (elf-section-flags sec)
                    #:addr (if (zero? (logand SHF_ALLOC
                                              (elf-section-flags sec)))
                               0
                               offset)
                    #:offset offset
                    #:size (elf-section-size sec)
                    #:link (elf-section-link sec)
                    #:info (elf-section-info sec)
                    #:addralign (elf-section-addralign sec)
                    #:entsize (elf-section-entsize sec)))


;; We assume that 64K is a multiple of the page size.  A
;; least-common-multiple, if you will.
;;
;; It would be possible to choose smaller, target-specific page sizes.
;; This is still a little tricky; on amd64 for example, systems commonly
;; have 4KB pages, but they are allowed by the ABI to have any
;; multiple-of-2 page size up to 64 KB.  On Cygwin, pages are 4kB but
;; they can only be allocated 16 at a time.  MIPS and ARM64 can use 64K
;; pages too and that's not uncommon.
;;
;; At the current time, in Guile we would like to reduce the number of
;; binaries we ship to the existing 32-or-64-bit and
;; big-or-little-endian variants, if possible.  It would seem that with
;; the least-common-multiple of 64 KB pages, we can do that.
;;
;; See https://github.com/golang/go/issues/10180 for a discussion of
;; this issue in the Go context.
;;
;; Using 64KB instead of the more usual 4KB will increase the size of
;; our .go files, but not the prebuilt/ part of the tarball as that part
;; of the file will be zeroes and compress well.  Additionally on a
;; system with 4KB pages, the extra padding will never be paged in, nor
;; read from disk (though it causes more seeking etc so on spinning
;; metal it's a bit of a lose).
;;
;; By way of comparison, on many 64-bit platforms, binutils currently
;; defaults to aligning segments on 2MB boundaries.  It does so by
;; making the file and the memory images not the same: the pages are all
;; together on disk, but then when loading, the loader will mmap a
;; region "memsz" large which might be greater than the file size, then
;; map segments into that region.  We can avoid this complication for
;; now.  We can consider adding it in the future in a compatible way in
;; 2.2 if it is important.
;;
(define *lcm-page-size* (ash 1 16))

(define (add-symbols symbols offset symtab)
  "Add @var{symbols} to the symbol table @var{symtab}, relocating them
from object address space to memory address space.  Returns a new symbol
table."
  (fold-values
   (lambda (symbol symtab)
     (let ((name (linker-symbol-name symbol))
           (addr (linker-symbol-address symbol)))
       (when (vhash-assq name symtab)
         (error "duplicate symbol" name))
       (vhash-consq name (make-linker-symbol name (+ addr offset)) symtab)))
   symbols
   symtab))

(define (allocate-segment write-segment-header!
                          phidx type flags objects addr symtab alignment)
  "Given a list of linker objects that should go in a segment, the type
and flags that the segment should have, and the address at which the
segment should start, compute the positions that each object should have
in the segment.

Returns three values: the address of the next byte after the segment, a
list of relocated objects, and the symbol table.  The symbol table is
the same as @var{symtab}, augmented with the symbols defined in
@var{objects}, relocated to their positions in the image.

In what is something of a quirky interface, this routine also patches up
the segment table using @code{write-segment-header!}."
  (let* ((alignment (fold-values (lambda (o alignment)
                                   (lcm (elf-section-addralign
                                         (linker-object-section o))
                                        alignment))
                                 objects
                                 alignment))
         (addr (align addr alignment)))
    (receive (objects endaddr symtab)
        (fold-values
         (lambda (o out addr symtab)
           (let* ((section (linker-object-section o))
                  (addr (align addr (elf-section-addralign section))))
             (values
              (cons (make-linker-object
                     (relocate-section-header section addr)
                     (linker-object-bv o)
                     (linker-object-relocs o)
                     (linker-object-symbols o))
                    out)
              (+ addr (elf-section-size section))
              (add-symbols (linker-object-symbols o) addr symtab))))
         objects
         '() addr symtab)
      (when type
        (write-segment-header!
         (make-elf-segment #:index phidx #:type type
                           #:offset addr #:vaddr addr #:paddr addr
                           #:filesz (- endaddr addr) #:memsz (- endaddr addr)
                           #:flags flags #:align alignment)))
      (values endaddr
              (reverse objects)
              symtab))))

(define (process-reloc reloc bv section-offset symtab endianness)
  "Process a relocation.  Given that a section containing @var{reloc}
was just written into the image @var{bv} at offset @var{section-offset},
fix it up so that its reference points to the correct position of its
symbol, as present in @var{symtab}."
  (match (vhash-assq (linker-reloc-symbol reloc) symtab)
    (#f
     (error "Undefined symbol" (linker-reloc-symbol reloc)))
    ((name . symbol)
     ;; The reloc was written at LOC bytes after SECTION-OFFSET.
     (let* ((offset (+ (linker-reloc-loc reloc) section-offset))
            (target (linker-symbol-address symbol)))
       (case (linker-reloc-type reloc)
         ((rel32/4)
          (let ((diff (+ (- target offset) (linker-reloc-addend reloc))))
            (unless (zero? (modulo diff 4))
              (error "Bad offset" reloc symbol offset))
            (bytevector-s32-set! bv offset (/ diff 4) endianness)))
         ((rel32/1)
          (let ((diff (- target offset)))
            (bytevector-s32-set! bv offset
                                 (+ diff (linker-reloc-addend reloc))
                                 endianness)))
         ((abs32/1)
          (bytevector-u32-set! bv offset target endianness))
         ((abs64/1)
          (bytevector-u64-set! bv offset target endianness))
         (else
          (error "bad reloc type" reloc)))))))

(define (write-linker-object bv o symtab endianness)
  "Write the bytevector for the section wrapped by the linker object
@var{o} into the image @var{bv}.  The section header in @var{o} should
already be relocated its final position in the image.  Any relocations
in the section will be processed to point to the correct symbol
locations, as given in @var{symtab}."
  (let* ((section (linker-object-section o))
         (offset (elf-section-offset section))
         (len (elf-section-size section))
         (bytes (linker-object-bv o))
         (relocs (linker-object-relocs o)))
    (if (zero? (logand SHF_ALLOC (elf-section-flags section)))
        (unless (zero? (elf-section-addr section))
          (error "non-loadable section has non-zero addr" section))
        (unless (= offset (elf-section-addr section))
          (error "loadable section has offset != addr" section)))
    (if (not (= (elf-section-type section) SHT_NOBITS))
        (begin
          (if (not (= len (bytevector-length bytes)))
              (error "unexpected length" section bytes))
          (bytevector-copy! bytes 0 bv offset len)
          (for-each (lambda (reloc)
                      (process-reloc reloc bv offset symtab endianness))
                    relocs)))))

(define (find-shstrndx objects)
  "Find the section name string table in @var{objects}, and return its
section index."
  (or-map (lambda (object)
            (let* ((section (linker-object-section object))
                   (bv (linker-object-bv object))
                   (name (elf-section-name section)))
              (and (= (elf-section-type section) SHT_STRTAB)
                   (< name (bytevector-length bv))
                   (string=? (string-table-ref bv name) ".shstrtab")
                   (elf-section-index section))))
          objects))

(define (add-elf-objects objects endianness word-size abi type machine-type)
  "Given the list of linker objects supplied by the user, add linker
objects corresponding to parts of the ELF file: the null object, the ELF
header, and the section table.

Both of these internal objects include relocs, allowing their
inter-object references to be patched up when the final image allocation
is known.  There is special support for patching up the segment table,
however.  Because the segment table needs to know the segment sizes,
which is the difference between two symbols in image space, and there is
no reloc kind that is the difference between two symbols, we make a hack
and return a closure that patches up segment table entries.  It seems to
work.

Returns two values: the procedure to patch the segment table, and the
list of objects, augmented with objects for the special ELF sections."
  (define phoff (elf-header-len word-size))
  (define phentsize (elf-program-header-len word-size))
  (define shentsize (elf-section-header-len word-size))
  (define shnum (+ (length objects) 3))
  (define reloc-kind
    (case word-size
      ((4) 'abs32/1)
      ((8) 'abs64/1)
      (else (error "bad word size" word-size))))

  ;; ELF requires that the first entry in the section table be of type
  ;; SHT_NULL.
  ;;
  (define (make-null-section)
    (make-linker-object (make-elf-section #:index 0 #:type SHT_NULL
                                          #:flags 0 #:addralign 0)
                        #vu8() '() '()))

  ;; The ELF header and the segment table.
  ;;
  (define (make-header phnum index shoff-label)
    (let* ((header (make-elf #:byte-order endianness #:word-size word-size
                             #:abi abi #:type type #:machine-type machine-type
                             #:phoff phoff #:phnum phnum #:phentsize phentsize
                             #:shoff 0 #:shnum shnum #:shentsize shentsize
                             #:shstrndx (or (find-shstrndx objects) SHN_UNDEF)))
           (shoff-reloc (make-linker-reloc reloc-kind
                                           (elf-header-shoff-offset word-size)
                                           0
                                           shoff-label))
           (size (+ phoff (* phnum phentsize)))
           (bv (make-bytevector size 0)))
      (write-elf-header bv header)
      ;; Leave the segment table uninitialized; it will be filled in
      ;; later by calls to the write-segment-header! closure.
      (make-linker-object (make-elf-section #:index index #:type SHT_PROGBITS
                                            #:flags SHF_ALLOC #:size size)
                          bv
                          (list shoff-reloc)
                          '())))

  ;; The section table.
  ;;
  (define (make-footer objects shoff-label)
    (let* ((size (* shentsize shnum))
           (bv (make-bytevector size 0))
           (section-table (make-elf-section #:index (length objects)
                                            #:type SHT_PROGBITS
                                            #:flags 0
                                            #:size size)))
      (define (write-and-reloc section-label section relocs)
        (let ((offset (* shentsize (elf-section-index section))))
          (write-elf-section-header bv offset endianness word-size section)
          (if (= (elf-section-type section) SHT_NULL)
              relocs
              (let ((relocs
                     (cons (make-linker-reloc
                            reloc-kind
                            (+ offset
                               (elf-section-header-offset-offset word-size))
                            0
                            section-label)
                           relocs)))
                (if (zero? (logand SHF_ALLOC (elf-section-flags section)))
                    relocs
                    (cons (make-linker-reloc
                            reloc-kind
                            (+ offset
                               (elf-section-header-addr-offset word-size))
                            0
                            section-label)
                          relocs))))))
      (let ((relocs (fold-values
                     (lambda (object relocs)
                       (write-and-reloc
                        (linker-symbol-name
                         (linker-object-section-symbol object))
                        (linker-object-section object)
                        relocs))
                     objects
                     (write-and-reloc shoff-label section-table '()))))
        (%make-linker-object section-table bv relocs
                             (list (make-linker-symbol shoff-label 0))))))

  (let* ((null-section (make-null-section))
         (objects (cons null-section objects))

         (shoff (gensym "*section-table*"))
         (header (make-header (count-segments objects) (length objects) shoff))
         (objects (cons header objects))

         (footer (make-footer objects shoff))
         (objects (cons footer objects)))

    ;; The header includes the segment table, which needs offsets and
    ;; sizes of the segments.  Normally we would use relocs to rewrite
    ;; these values, but there is no reloc type that would allow us to
    ;; compute size.  Such a reloc would need to take the difference
    ;; between two symbols, and it's probably a bad idea architecturally
    ;; to create one.
    ;;
    ;; So instead we return a closure to patch up the segment table.
    ;; Normally we'd shy away from such destructive interfaces, but it's
    ;; OK as we create the header section ourselves.
    ;;
    (define (write-segment-header! segment)
      (let ((bv (linker-object-bv header))
            (offset (+ phoff (* (elf-segment-index segment) phentsize))))
        (write-elf-program-header bv offset endianness word-size segment)))

    (values write-segment-header! objects)))

(define (record-special-segments write-segment-header! phidx all-objects)
  (let lp ((phidx phidx) (objects all-objects))
    (match objects
      (() #t)
      ((object . objects)
       (let ((section (linker-object-section object)))
         (cond
          ((eqv? (elf-section-type section) SHT_DYNAMIC)
           (let ((addr (elf-section-offset section))
                 (size (elf-section-size section))
                 (align (elf-section-addralign section))
                 (flags (cdr (segment-kind section))))
             (write-segment-header!
              (make-elf-segment #:index phidx #:type PT_DYNAMIC
                                #:offset addr #:vaddr addr #:paddr addr
                                #:filesz size #:memsz size
                                #:flags flags #:align align))
             (lp (1+ phidx) objects)))
          (else
           (lp phidx objects))))))))

(define (allocate-elf objects page-aligned? endianness word-size
                      abi type machine-type)
  "Lay out @var{objects} into an ELF image, computing the size of the
file, the positions of the objects, and the global symbol table.

If @var{page-aligned?} is true, read-only and writable data are
separated so that only those writable parts of the image need be mapped
with writable permissions.  This makes the resulting image larger.  It
is more suitable to situations where you would write a file out to disk
and read it in with mmap.  Otherwise if @var{page-aligned?} is false,
sections default to 8-byte alignment.

Returns three values: the total image size, a list of objects with
relocated headers, and the global symbol table."
  (receive (write-segment-header! objects)
      (add-elf-objects objects endianness word-size abi type machine-type)
    (let lp ((seglists (collate-objects-into-segments objects))
             (objects '())
             (phidx 0)
             (addr 0)
             (symtab vlist-null)
             (prev-flags 0))
      (match seglists
        ((((type . flags) objs-in ...) seglists ...)
         (receive (addr objs-out symtab)
             (allocate-segment
              write-segment-header!
              phidx type flags objs-in addr symtab
              (if (and page-aligned?
                       (not (= flags prev-flags))
                       ;; Allow sections that are not in
                       ;; loadable segments to share pages
                       ;; with PF_R segments.
                       (not (and (not type) (= PF_R prev-flags))))
                  *lcm-page-size*
                  8))
           (lp seglists
               (fold-values cons objs-out objects)
               (if type (1+ phidx) phidx)
               addr
               symtab
               flags)))
        (()
         (record-special-segments write-segment-header! phidx objects)
         (values addr
                 (reverse objects)
                 symtab))))))

(define (check-section-numbers objects)
  "Verify that taken as a whole, that all objects have distinct,
contiguous section numbers, starting from 1.  (Section 0 is the null
section.)"
  (let* ((nsections (1+ (length objects))) ; 1+ for initial NULL section.
         (sections (make-vector nsections #f)))
    (for-each (lambda (object)
                (let ((n (elf-section-index (linker-object-section object))))
                  (cond
                   ((< n 1)
                    (error "Invalid section number" object))
                   ((>= n nsections)
                    (error "Invalid section number" object))
                   ((vector-ref sections n)
                    (error "Duplicate section" (vector-ref sections n) object))
                   (else
                    (vector-set! sections n object)))))
              objects)))

;; Given a list of linker objects, collate the objects into segments,
;; allocate the segments, allocate the ELF bytevector, and write the
;; segments into the bytevector, relocating as we go.
;;
(define* (link-elf objects #:key
                   (page-aligned? #t)
                   (endianness (target-endianness))
                   (word-size (target-word-size))
                   (abi ELFOSABI_STANDALONE)
                   (type ET_DYN)
                   (machine-type EM_NONE))
  "Create an ELF image from the linker objects, @var{objects}.

If @var{page-aligned?} is true, read-only and writable data are
separated so that only those writable parts of the image need be mapped
with writable permissions.  This is suitable for situations where you
would write a file out to disk and read it in with @code{mmap}.
Otherwise if @var{page-aligned?} is false, sections default to 8-byte
alignment.

Returns a bytevector."
  (check-section-numbers objects)
  (receive (size objects symtab)
      (allocate-elf objects page-aligned? endianness word-size
                    abi type machine-type)
    (let ((bv (make-bytevector size 0)))
      (for-each
       (lambda (object)
         (write-linker-object bv object symtab endianness))
       objects)
      bv)))
