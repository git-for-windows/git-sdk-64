;;; Guile DWARF reader and writer

;; Copyright (C) 2012, 2013 Free Software Foundation, Inc.

;; Parts of this file were derived from sysdeps/generic/dwarf2.h, from
;; the GNU C Library.  That file is available under the LGPL version 2
;; or later, and is copyright:
;;
;; Copyright (C) 1992, 1993, 1995, 1996, 1997, 2000, 2011
;; 	Free Software Foundation, Inc.
;; Contributed by Gary Funck (gary@intrepid.com).  Derived from the
;; DWARF 1 implementation written by Ron Guilmette (rfg@monkeys.com).

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
;;
;; DWARF is a flexible format for describing compiled programs.  It is
;; used by Guile to record source positions, describe local variables,
;; function arities, and other function metadata.
;;
;; Structurally, DWARF describes a tree of data.  Each node in the tree
;; is a debugging information entry ("DIE").  Each DIE has a "tag",
;; possible a set of attributes, and possibly some child DIE nodes.
;; That's basically it!
;;
;; The DIE nodes are contained in the .debug_info section of an ELF
;; file.  Attributes within the DIE nodes link them to mapped ranges of
;; the ELF file (.rtl-text, .data, etc.).
;;
;; A .debug_info section logically contains a series of debugging
;; "contributions", one for each compilation unit.  Each contribution is
;; prefixed by a header and contains a single DIE element whose tag is
;; "compilation-unit".  That node usually contains child nodes, for
;; example of type "subprogram".
;;
;; Since usually one will end up producing many DIE nodes with the same
;; tag and attribute types, DIE nodes are defined by referencing a known
;; shape, and then filling in the values.  The shapes are defined in the
;; form of "abbrev" entries, which specify a specific combination of a
;; tag and an ordered set of attributes, with corresponding attribute
;; representations ("forms").  Abbrevs are written out to a separate
;; section, .debug_abbrev.  Abbrev nodes also specify whether the
;; corresponding DIE node has children or not.  When a DIE is written
;; into the .debug_info section, it references one of the abbrevs in
;; .debug_abbrev.  You need the abbrev in order to parse the DIE.
;;
;; For completeness, the other sections that DWARF uses are .debug_str,
;; .debug_loc, .debug_pubnames, .debug_aranges, .debug_frame, and
;; .debug_line.  These are described in section 6 of the DWARF 3.0
;; specification, at http://dwarfstd.org/.
;;
;; This DWARF module is currently capable of parsing all of DWARF 2.0
;; and parts of DWARF 3.0.  For Guile's purposes, we also use DWARF as
;; the format for our own debugging information.  The DWARF generator is
;; fairly minimal, and is not intended to be complete.
;;
;;; Code:

(define-module (system vm dwarf)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system base target)
  #:use-module (system vm elf)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:export (elf->dwarf-context
            read-die-roots
            fold-pubnames fold-aranges

            access-name->code
            address-name->code
            attribute-name->code
            call-frame-address-name->code
            children-name->code
            convention-name->code
            discriminant-name->code
            form-name->code
            inline-name->code
            language-name->code
            macro-name->code
            ordering-name->code
            sensitivity-name->code
            tag-name->code
            virtuality-name->code
            visibility-name->code

            abbrev? abbrev-code
            abbrev-tag abbrev-has-children? abbrev-attrs abbrev-forms

            die? die-ctx die-offset die-abbrev die-vals die-children
            die-tag die-attrs die-forms die-ref
            die-name die-specification die-qname die-low-pc die-high-pc

            ctx-parent ctx-die ctx-start ctx-end ctx-children ctx-language

            die-line-prog line-prog-advance line-prog-scan-to-pc

            find-die-context find-die-by-offset find-die find-die-by-pc
            read-die fold-die-list

            fold-die-children die->tree))

;;;
;;; First, define a number of constants.  The figures numbers refer to
;;; the DWARF 2.0 draft specification available on http://dwarfstd.org/.
;;; Extra codes not defined in that document are taken from the dwarf2
;;; header in glibc.
;;;

(define-syntax-rule (define-enumeration code->name name->code
                      (tag value) ...)
  (begin
    (define code->name
      (let ((table (make-hash-table)))
        (hashv-set! table value 'tag)
        ...
        (lambda (v)
          (hashv-ref table v v))))
    (define name->code
      (let ((table (make-hash-table)))
        (hashv-set! table 'tag value)
        ...
        (lambda (v)
          (hashv-ref table v v))))))

;; Figures 14 and 15: Tag names and codes.
;;
(define-enumeration tag-code->name tag-name->code
  (padding #x00)
  (array-type #x01)
  (class-type #x02)
  (entry-point #x03)
  (enumeration-type #x04)
  (formal-parameter #x05)
  (imported-declaration #x08)
  (label #x0a)
  (lexical-block #x0b)
  (member #x0d)
  (pointer-type #x0f)
  (reference-type #x10)
  (compile-unit #x11)
  (string-type #x12)
  (structure-type #x13)
  (subroutine-type #x15)
  (typedef #x16)
  (union-type #x17)
  (unspecified-parameters #x18)
  (variant #x19)
  (common-block #x1a)
  (common-inclusion #x1b)
  (inheritance #x1c)
  (inlined-subroutine #x1d)
  (module #x1e)
  (ptr-to-member-type #x1f)
  (set-type #x20)
  (subrange-type #x21)
  (with-stmt #x22)
  (access-declaration #x23)
  (base-type #x24)
  (catch-block #x25)
  (const-type #x26)
  (constant #x27)
  (enumerator #x28)
  (file-type #x29)
  (friend #x2a)
  (namelist #x2b)
  (namelist-item #x2c)
  (packed-type #x2d)
  (subprogram #x2e)
  (template-type-param #x2f)
  (template-value-param #x30)
  (thrown-type #x31)
  (try-block #x32)
  (variant-part #x33)
  (variable #x34)
  (volatile-type #x35)
  ;; DWARF 3.
  (dwarf-procedure #x36)
  (restrict-type #x37)
  (interface-type #x38)
  (namespace #x39)
  (imported-module #x3a)
  (unspecified-type #x3b)
  (partial-unit #x3c)
  (imported-unit #x3d)
  (condition #x3f)
  (shared-type #x40)
  ;; Extensions.
  (format-label #x4101)
  (function-template #x4102)
  (class-template #x4103)
  (GNU-BINCL #x4104)
  (GNU-EINCL #x4105)
  (lo-user #x4080)
  (hi-user #xffff))

;; Figure 16: Flag that tells whether entry has a child or not.
;;
(define-enumeration children-code->name children-name->code
  (no 0)
  (yes 1))

;; Figures 17 and 18: Attribute names and codes.
;;
(define-enumeration attribute-code->name attribute-name->code
  (sibling #x01)
  (location #x02)
  (name #x03)
  (ordering #x09)
  (subscr-data #x0a)
  (byte-size #x0b)
  (bit-offset #x0c)
  (bit-size #x0d)
  (element-list #x0f)
  (stmt-list #x10)
  (low-pc #x11)
  (high-pc #x12)
  (language #x13)
  (member #x14)
  (discr #x15)
  (discr-value #x16)
  (visibility #x17)
  (import #x18)
  (string-length #x19)
  (common-reference #x1a)
  (comp-dir #x1b)
  (const-value #x1c)
  (containing-type #x1d)
  (default-value #x1e)
  (inline #x20)
  (is-optional #x21)
  (lower-bound #x22)
  (producer #x25)
  (prototyped #x27)
  (return-addr #x2a)
  (start-scope #x2c)
  (stride-size #x2e)
  (upper-bound #x2f)
  (abstract-origin #x31)
  (accessibility #x32)
  (address-class #x33)
  (artificial #x34)
  (base-types #x35)
  (calling-convention #x36)
  (count #x37)
  (data-member-location #x38)
  (decl-column #x39)
  (decl-file #x3a)
  (decl-line #x3b)
  (declaration #x3c)
  (discr-list #x3d)
  (encoding #x3e)
  (external #x3f)
  (frame-base #x40)
  (friend #x41)
  (identifier-case #x42)
  (macro-info #x43)
  (namelist-items #x44)
  (priority #x45)
  (segment #x46)
  (specification #x47)
  (static-link #x48)
  (type #x49)
  (use-location #x4a)
  (variable-parameter #x4b)
  (virtuality #x4c)
  (vtable-elem-location #x4d)
  ;; DWARF 3.
  (associated #x4f)
  (data-location #x50)
  (byte-stride #x51)
  (entry-pc #x52)
  (use-UTF8 #x53)
  (extension #x54)
  (ranges #x55)
  (trampoline #x56)
  (call-column #x57)
  (call-file #x58)
  (call-line #x59)
  (description #x5a)
  (binary-scale #x5b)
  (decimal-scale #x5c)
  (small #x5d)
  (decimal-sign #x5e)
  (digit-count #x5f)
  (picture-string #x60)
  (mutable #x61)
  (threads-scaled #x62)
  (explicit #x63)
  (object-pointer #x64)
  (endianity #x65)
  (elemental #x66)
  (pure #x67)
  (recursive #x68)
  ;; Extensions.
  (linkage-name #x2007)
  (sf-names #x2101)
  (src-info #x2102)
  (mac-info #x2103)
  (src-coords #x2104)
  (body-begin #x2105)
  (body-end #x2106)
  (lo-user #x2000)
  (hi-user #x3fff))

;; Figure 19: Form names and codes.
;;
(define-enumeration form-code->name form-name->code
  (addr #x01)
  (block2 #x03)
  (block4 #x04)
  (data2 #x05)
  (data4 #x06)
  (data8 #x07)
  (string #x08)
  (block #x09)
  (block1 #x0a)
  (data1 #x0b)
  (flag #x0c)
  (sdata #x0d)
  (strp #x0e)
  (udata #x0f)
  (ref-addr #x10)
  (ref1 #x11)
  (ref2 #x12)
  (ref4 #x13)
  (ref8 #x14)
  (ref-udata #x15)
  (indirect #x16)
  (sec-offset #x17)
  (exprloc #x18)
  (flag-present #x19)
  (ref-sig8 #x20))

;; Figures 22 and 23: Location atom names and codes.
;;
(define-enumeration location-op->name location-name->op
  (addr #x03)
  (deref #x06)
  (const1u #x08)
  (const1s #x09)
  (const2u #x0a)
  (const2s #x0b)
  (const4u #x0c)
  (const4s #x0d)
  (const8u #x0e)
  (const8s #x0f)
  (constu #x10)
  (consts #x11)
  (dup #x12)
  (drop #x13)
  (over #x14)
  (pick #x15)
  (swap #x16)
  (rot #x17)
  (xderef #x18)
  (abs #x19)
  (and #x1a)
  (div #x1b)
  (minus #x1c)
  (mod #x1d)
  (mul #x1e)
  (neg #x1f)
  (not #x20)
  (or #x21)
  (plus #x22)
  (plus-uconst #x23)
  (shl #x24)
  (shr #x25)
  (shra #x26)
  (xor #x27)
  (bra #x28)
  (eq #x29)
  (ge #x2a)
  (gt #x2b)
  (le #x2c)
  (lt #x2d)
  (ne #x2e)
  (skip #x2f)
  (lit0 #x30)
  (lit1 #x31)
  (lit2 #x32)
  (lit3 #x33)
  (lit4 #x34)
  (lit5 #x35)
  (lit6 #x36)
  (lit7 #x37)
  (lit8 #x38)
  (lit9 #x39)
  (lit10 #x3a)
  (lit11 #x3b)
  (lit12 #x3c)
  (lit13 #x3d)
  (lit14 #x3e)
  (lit15 #x3f)
  (lit16 #x40)
  (lit17 #x41)
  (lit18 #x42)
  (lit19 #x43)
  (lit20 #x44)
  (lit21 #x45)
  (lit22 #x46)
  (lit23 #x47)
  (lit24 #x48)
  (lit25 #x49)
  (lit26 #x4a)
  (lit27 #x4b)
  (lit28 #x4c)
  (lit29 #x4d)
  (lit30 #x4e)
  (lit31 #x4f)
  (reg0 #x50)
  (reg1 #x51)
  (reg2 #x52)
  (reg3 #x53)
  (reg4 #x54)
  (reg5 #x55)
  (reg6 #x56)
  (reg7 #x57)
  (reg8 #x58)
  (reg9 #x59)
  (reg10 #x5a)
  (reg11 #x5b)
  (reg12 #x5c)
  (reg13 #x5d)
  (reg14 #x5e)
  (reg15 #x5f)
  (reg16 #x60)
  (reg17 #x61)
  (reg18 #x62)
  (reg19 #x63)
  (reg20 #x64)
  (reg21 #x65)
  (reg22 #x66)
  (reg23 #x67)
  (reg24 #x68)
  (reg25 #x69)
  (reg26 #x6a)
  (reg27 #x6b)
  (reg28 #x6c)
  (reg29 #x6d)
  (reg30 #x6e)
  (reg31 #x6f)
  (breg0 #x70)
  (breg1 #x71)
  (breg2 #x72)
  (breg3 #x73)
  (breg4 #x74)
  (breg5 #x75)
  (breg6 #x76)
  (breg7 #x77)
  (breg8 #x78)
  (breg9 #x79)
  (breg10 #x7a)
  (breg11 #x7b)
  (breg12 #x7c)
  (breg13 #x7d)
  (breg14 #x7e)
  (breg15 #x7f)
  (breg16 #x80)
  (breg17 #x81)
  (breg18 #x82)
  (breg19 #x83)
  (breg20 #x84)
  (breg21 #x85)
  (breg22 #x86)
  (breg23 #x87)
  (breg24 #x88)
  (breg25 #x89)
  (breg26 #x8a)
  (breg27 #x8b)
  (breg28 #x8c)
  (breg29 #x8d)
  (breg30 #x8e)
  (breg31 #x8f)
  (regx #x90)
  (fbreg #x91)
  (bregx #x92)
  (piece #x93)
  (deref-size #x94)
  (xderef-size #x95)
  (nop #x96)
  ;; DWARF 3.
  (push-object-address #x97)
  (call2 #x98)
  (call4 #x99)
  (call-ref #x9a)
  (form-tls-address #x9b)
  (call-frame-cfa #x9c)
  (bit-piece #x9d)
  (lo-user #x80)
  (hi-user #xff))

;; Figure 24: Type encodings.
;;
(define-enumeration type-encoding->name type-name->encoding
  (void #x0)
  (address #x1)
  (boolean #x2)
  (complex-float #x3)
  (float #x4)
  (signed #x5)
  (signed-char #x6)
  (unsigned #x7)
  (unsigned-char #x8)
  ;; DWARF 3.
  (imaginary-float #x09)
  (packed-decimal #x0a)
  (numeric-string #x0b)
  (edited #x0c)
  (signed-fixed #x0d)
  (unsigned-fixed #x0e)
  (decimal-float #x0f)
  (lo-user #x80)
  (hi-user #xff))

;; Figure 25: Access attribute.
;;
(define-enumeration access-code->name access-name->code
  (public 1)
  (protected 2)
  (private 3))

;; Figure 26: Visibility.
;;
(define-enumeration visibility-code->name visibility-name->code
  (local 1)
  (exported 2)
  (qualified 3))

;; Figure 27: Virtuality.
;;
(define-enumeration virtuality-code->name virtuality-name->code
  (none 0)
  (virtual 1)
  (pure-virtual 2))

;; Figure 28: Source language names and codes.
;;
(define-enumeration language-code->name language-name->code
  (c89 #x0001)
  (c #x0002)
  (ada83 #x0003)
  (c++ #x0004)
  (cobol74 #x0005)
  (cobol85 #x0006)
  (fortran77 #x0007)
  (fortran90 #x0008)
  (pascal83 #x0009)
  (modula2 #x000a)
  (java #x000b)
  (c99 #x000c)
  (ada95 #x000d)
  (fortran95 #x000e)
  (pli #x000f)
  (objc #x0010)
  (objc++ #x0011)
  (upc #x0012)
  (d #x0013)
  (python #x0014)
  (mips-assembler #x8001)

  (lo-user #x8000)

  ;; FIXME: Ask for proper codes for these.
  (scheme #xaf33)
  (emacs-lisp #xaf34)
  (ecmascript #xaf35)
  (lua #xaf36)
  (brainfuck #xaf37)

  (hi-user #xffff))

;; Figure 29: Case sensitivity.
;;
(define-enumeration case-sensitivity-code->name case-sensitivity-name->code
  (case-sensitive 0)
  (up-case 1)
  (down-case 2)
  (case-insensitive 3))

;; Figure 30: Calling convention.
;;
(define-enumeration calling-convention-code->name calling-convention-name->code
  (normal #x1)
  (program #x2)
  (nocall #x3)
  (lo-user #x40)
  (hi-user #xff))

;; Figure 31: Inline attribute.
;;
(define-enumeration inline-code->name inline-name->code
  (not-inlined 0)
  (inlined 1)
  (declared-not-inlined 2)
  (declared-inlined 3))

;; Figure 32: Array ordering names and codes.
(define-enumeration ordering-code->name ordering-name->code
  (row-major 0)
  (col-major 1))

;; Figure 33: Discriminant lists.
;;
(define-enumeration discriminant-code->name discriminant-name->code
  (label 0)
  (range 1))

;; Figure 34: "Standard" line number opcodes.
;;
(define-enumeration standard-line-opcode->name standard-line-name->opcode
  (extended-op 0)
  (copy 1)
  (advance-pc 2)
  (advance-line 3)
  (set-file 4)
  (set-column 5)
  (negate-stmt 6)
  (set-basic-block 7)
  (const-add-pc 8)
  (fixed-advance-pc 9)
  ;; DWARF 3.
  (set-prologue-end #x0a)
  (set-epilogue-begin #x0b)
  (set-isa #x0c))

;; Figure 35: "Extended" line number opcodes.
;;
(define-enumeration extended-line-opcode->name extended-line-name->opcode
  (end-sequence 1)
  (set-address 2)
  (define-file 3)
  ;; DWARF 3.
  (lo-user #x80)
  (hi-user #xff))

;; Figure 36: Names and codes for macro information.
;;
(define-enumeration macro-code->name macro-name->code
  (define 1)
  (undef 2)
  (start-file 3)
  (end-file 4)
  (vendor-ext 255))

;; Figure 37: Call frame information.
;;
(define-enumeration call-frame-address-code->name call-frame-address-code->name
  (advance-loc #x40)
  (offset #x80)
  (restore #xc0)
  (nop #x00)
  (set-loc #x01)
  (advance-loc1 #x02)
  (advance-loc2 #x03)
  (advance-loc4 #x04)
  (offset-extended #x05)
  (restore-extended #x06)
  (undefined #x07)
  (same-value #x08)
  (register #x09)
  (remember-state #x0a)
  (restore-state #x0b)
  (def-cfa #x0c)
  (def-cfa-register #x0d)
  (def-cfa-offset #x0e)
  ;; DWARF 3.
  (def-cfa-expression #x0f)
  (expression #x10)
  (offset-extended-sf #x11)
  (def-cfa-sf #x12)
  (def-cfa-offset-sf #x13)
  (val-offset #x14)
  (val-offset-sf #x15)
  (val-expression #x16)
  (GNU-window-save #x2d)
  (GNU-args-size #x2e)
  (GNU-negative-offset-extended #x2f)

  (extended 0)
  (low-user #x1c)
  (high-user #x3f))

;(define CIE-ID #xffffffff)
;(define CIE-VERSION 1)
;(define ADDR-none 0)


;;;
;;; A general configuration object.
;;;

(define-record-type <dwarf-meta>
  (make-dwarf-meta addr-size
                   vaddr memsz
                   path lib-path
                   info-start info-end
                   abbrevs-start abbrevs-end
                   strtab-start strtab-end
                   loc-start loc-end
                   line-start line-end
                   pubnames-start pubnames-end
                   aranges-start aranges-end)
  dwarf-meta?
  (addr-size meta-addr-size)
  (vaddr meta-vaddr)
  (memsz meta-memsz)
  (path meta-path)
  (lib-path meta-lib-path)
  (info-start meta-info-start)
  (info-end meta-info-end)
  (abbrevs-start meta-abbrevs-start)
  (abbrevs-end meta-abbrevs-end)
  (strtab-start meta-strtab-start)
  (strtab-end meta-strtab-end)
  (loc-start meta-loc-start)
  (loc-end meta-loc-end)
  (line-start meta-line-start)
  (line-end meta-line-end)
  (pubnames-start meta-pubnames-start)
  (pubnames-end meta-pubnames-end)
  (aranges-start meta-aranges-start)
  (aranges-end meta-aranges-end))

;; A context represents a namespace.  The root context is the
;; compilation unit.  DIE nodes of type class-type, structure-type, or
;; namespace may form child contexts.
;;
(define-record-type <dwarf-context>
  (make-dwarf-context bv offset-size endianness meta
                      abbrevs
                      parent die start end children)
  dwarf-context?
  (bv ctx-bv)
  (offset-size ctx-offset-size)
  (endianness ctx-endianness)
  (meta ctx-meta)
  (abbrevs ctx-abbrevs)
  (parent ctx-parent)
  (die ctx-die)
  (start ctx-start)
  (end ctx-end)
  (children ctx-children set-children!))


(set-record-type-printer! <dwarf-context>
                          (lambda (x port)
                            (format port "<dwarf-context ~a>"
                                    (number->string (object-address x) 16))))

(define-inlinable (ctx-addr-size ctx)
  (meta-addr-size (ctx-meta ctx)))

;;;
;;; Procedures for reading DWARF data.
;;;

(define (read-u8 ctx pos)
  (values (bytevector-u8-ref (ctx-bv ctx) pos)
          (1+ pos)))
(define (read-s8 ctx pos)
  (values (bytevector-s8-ref (ctx-bv ctx) pos)
          (1+ pos)))
(define (skip-8 ctx pos)
  (+ pos 1))

(define (read-u16 ctx pos)
  (values (bytevector-u16-ref (ctx-bv ctx) pos (ctx-endianness ctx))
          (+ pos 2)))
(define (skip-16 ctx pos)
  (+ pos 2))

(define (read-u32 ctx pos)
  (values (bytevector-u32-ref (ctx-bv ctx) pos (ctx-endianness ctx))
          (+ pos 4)))
(define (skip-32 ctx pos)
  (+ pos 4))

(define (read-u64 ctx pos)
  (values (bytevector-u64-ref (ctx-bv ctx) pos (ctx-endianness ctx))
          (+ pos 8)))
(define (skip-64 ctx pos)
  (+ pos 8))

(define (read-addr ctx pos)
  (case (ctx-addr-size ctx)
    ((4) (read-u32 ctx pos))
    ((8) (read-u64 ctx pos))
    (else (error "unsupported word size" ctx))))
(define (skip-addr ctx pos)
  (+ pos (ctx-addr-size ctx)))

(define (%read-uleb128 bv pos)
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

(define (%read-sleb128 bv pos)
  (let lp ((n 0) (pos pos) (shift 0))
    (let ((b (bytevector-u8-ref bv pos)))
      (if (zero? (logand b #x80))
          (values (logior (ash b shift) n
                          (if (zero? (logand #x40 b))
                              0
                              (- (ash 1 (+ shift 7)))))
                  (1+ pos))
          (lp (logior (ash (logxor #x80 b) shift) n)
              (1+ pos)
              (+ shift 7))))))

(define (read-uleb128 ctx pos)
  (%read-uleb128 (ctx-bv ctx) pos))

(define (read-sleb128 ctx pos)
  (%read-sleb128 (ctx-bv ctx) pos))

(define (skip-leb128 ctx pos)
  (let ((bv (ctx-bv ctx)))
    (let lp ((pos pos))
      (let ((b (bytevector-u8-ref bv pos)))
        (if (zero? (logand b #x80))
            (1+ pos)
            (lp (1+ pos)))))))

(define (read-initial-length ctx pos)
  (let ((len (bytevector-u32-ref (ctx-bv ctx) pos (ctx-endianness ctx))))
    (cond
     ((= len #xffffffff)
      (values (bytevector-u32-ref (ctx-bv ctx) (+ pos 4) (ctx-endianness ctx))
              (+ pos 12)
              8))
     ((>= len #xfffffff0)
      (error "bad initial length value" len))
     (else
      (values len
              (+ pos 4)
              4)))))

(define* (read-offset ctx pos #:optional (offset-size (ctx-offset-size ctx)))
  (case offset-size
    ((4) (values (read-u32 ctx pos) (+ pos 4)))
    ((8) (values (read-u64 ctx pos) (+ pos 8)))
    (else (error "bad word size" offset-size))))

(define* (skip-offset ctx pos #:optional (offset-size (ctx-offset-size ctx)))
  (+ pos offset-size))

(define (read-block ctx pos len)
  (let ((bv (make-bytevector len)))
    (bytevector-copy! (ctx-bv ctx) pos bv 0 len)
    (values bv
            (+ pos len))))

(define (read-string ctx pos)
  (let ((bv (ctx-bv ctx)))
    (let lp ((end pos))
      (if (zero? (bytevector-u8-ref bv end))
          (let ((out (make-bytevector (- end pos))))
            (bytevector-copy! bv pos out 0 (- end pos))
            (values (utf8->string out)
                    (1+ end)))
          (lp (1+ end))))))

(define (skip-string ctx pos)
  (let ((bv (ctx-bv ctx)))
    (let lp ((end pos))
      (if (zero? (bytevector-u8-ref bv end))
          (1+ end)
          (lp (1+ end))))))

(define (read-string-seq ctx pos)
  (let ((bv (ctx-bv ctx)))
    (let lp ((pos pos) (strs '()))
      (if (zero? (bytevector-u8-ref bv pos))
          (values (list->vector (reverse strs)) (1+ pos))
          (let-values (((str pos) (read-string ctx pos)))
            (lp pos (cons str strs)))))))

(define-record-type <abbrev>
  (make-abbrev code tag has-children? attrs forms)
  abbrev?
  (code abbrev-code)
  (tag abbrev-tag)
  (has-children? abbrev-has-children?)
  (attrs abbrev-attrs)
  (forms abbrev-forms))

(define (read-abbrev ctx pos)
  (let*-values (((code pos) (read-uleb128 ctx pos))
                ((tag pos) (read-uleb128 ctx pos))
                ((children pos) (read-u8 ctx pos)))
    (let lp ((attrs '()) (forms '()) (pos pos))
      (let*-values (((attr pos) (read-uleb128 ctx pos))
                    ((form pos) (read-uleb128 ctx pos)))
        (if (and (zero? attr) (zero? form))
            (values (make-abbrev code
                                 (tag-code->name tag)
                                 (eq? (children-code->name children) 'yes)
                                 (reverse attrs)
                                 (reverse forms))
                    pos)
            (lp (cons (attribute-code->name attr) attrs)
                (cons (form-code->name form) forms)
                pos))))))

(define* (read-abbrevs ctx pos
                       #:optional (start (meta-abbrevs-start
                                          (ctx-meta ctx)))
                       (end (meta-abbrevs-end
                             (ctx-meta ctx))))
  (let lp ((abbrevs '()) (pos (+ start pos)) (max-code -1))
    (if (zero? (read-u8 ctx pos))
        (if (< pos end)
            (let ((av (make-vector (1+ max-code) #f)))
              (for-each (lambda (a)
                          (vector-set! av (abbrev-code a) a))
                        abbrevs)
              av)
            (error "Unexpected length" abbrevs pos start end))
        (let-values (((abbrev pos) (read-abbrev ctx pos)))
          (lp (cons abbrev abbrevs)
              pos
              (max (abbrev-code abbrev) max-code))))))

(define (ctx-compile-unit-start ctx)
  (if (ctx-die ctx)
      (ctx-compile-unit-start (ctx-parent ctx))
      (ctx-start ctx)))

;; Values.
;;
(define *readers* (make-hash-table))
(define *scanners* (make-hash-table))
(define-syntax define-value-reader
  (syntax-rules ()
    ((_ form reader scanner)
     (begin
       (hashq-set! *readers* 'form reader)
       (hashq-set! *scanners* 'form scanner)))))

(define-value-reader addr read-addr skip-addr)

(define-value-reader block
  (lambda (ctx pos)
    (let-values (((len pos) (read-uleb128 ctx pos)))
      (read-block ctx pos len)))
  (lambda (ctx pos)
    (let-values (((len pos) (read-uleb128 ctx pos)))
      (+ pos len))))

(define-value-reader block1
  (lambda (ctx pos)
    (let-values (((len pos) (read-u8 ctx pos)))
      (read-block ctx pos len)))
  (lambda (ctx pos)
    (+ pos 1 (bytevector-u8-ref (ctx-bv ctx) pos))))

(define-value-reader block2
  (lambda (ctx pos)
    (let-values (((len pos) (read-u16 ctx pos)))
      (read-block ctx pos len)))
  (lambda (ctx pos)
    (+ pos 2 (bytevector-u16-ref (ctx-bv ctx) pos (ctx-endianness ctx)))))

(define-value-reader block4
  (lambda (ctx pos)
    (let-values (((len pos) (read-u32 ctx pos)))
      (read-block ctx pos len)))
  (lambda (ctx pos)
    (+ pos 4 (bytevector-u32-ref (ctx-bv ctx) pos (ctx-endianness ctx)))))

(define-value-reader data1 read-u8 skip-8)
(define-value-reader data2 read-u16 skip-16)
(define-value-reader data4 read-u32 skip-32)
(define-value-reader data8 read-u64 skip-64)
(define-value-reader udata read-uleb128 skip-leb128)
(define-value-reader sdata read-sleb128 skip-leb128)

(define-value-reader flag
  (lambda (ctx pos)
    (values (not (zero? (bytevector-u8-ref (ctx-bv ctx) pos)))
            (1+ pos)))
  skip-8)

(define-value-reader string
  read-string
  skip-string)

(define-value-reader strp
  (lambda (ctx pos)
    (let ((strtab (meta-strtab-start (ctx-meta ctx))))
      (unless strtab
        (error "expected a string table" ctx))
      (let-values (((offset pos) (read-offset ctx pos)))
        (values (read-string ctx (+ strtab offset))
                pos))))
  skip-32)

(define-value-reader ref-addr
  (lambda (ctx pos)
    (let-values (((addr pos) (read-addr ctx pos)))
      (values (+ addr (meta-info-start (ctx-meta ctx)))
              pos)))
  skip-addr)

(define-value-reader ref1
  (lambda (ctx pos)
    (let-values (((addr pos) (read-u8 ctx pos)))
      (values (+ addr (ctx-compile-unit-start ctx))
              pos)))
  skip-8)

(define-value-reader ref2
  (lambda (ctx pos)
    (let-values (((addr pos) (read-u16 ctx pos)))
      (values (+ addr (ctx-compile-unit-start ctx))
              pos)))
  skip-16)

(define-value-reader ref4
  (lambda (ctx pos)
    (let-values (((addr pos) (read-u32 ctx pos)))
      (values (+ addr (ctx-compile-unit-start ctx))
              pos)))
  skip-32)

(define-value-reader ref8
  (lambda (ctx pos)
    (let-values (((addr pos) (read-u64 ctx pos)))
      (values (+ addr (ctx-compile-unit-start ctx))
              pos)))
  skip-64)

(define-value-reader ref
  (lambda (udata ctx pos)
    (let-values (((addr pos) (read-uleb128 ctx pos)))
      (values (+ addr (ctx-compile-unit-start ctx))
              pos)))
  skip-leb128)

(define-value-reader indirect
  (lambda (ctx pos)
    (let*-values (((form pos) (read-uleb128 ctx pos))
                  ((val pos) (read-value ctx pos (form-code->name form))))
      (values (cons form val)
              pos)))
  (lambda (ctx pos)
    (let*-values (((form pos) (read-uleb128 ctx pos)))
      (skip-value ctx pos (form-code->name form)))))

(define-value-reader sec-offset
  read-offset
  skip-offset)

(define-value-reader exprloc
  (lambda (ctx pos)
    (let-values (((len pos) (read-uleb128 ctx pos)))
      (read-block ctx pos len)))
  (lambda (ctx pos)
    (let-values (((len pos) (read-uleb128 ctx pos)))
      (+ pos len))))

(define-value-reader flag-present
  (lambda (ctx pos)
    (values #t pos))
  (lambda (ctx pos)
    pos))

(define-value-reader ref-sig8
  read-u64
  skip-64)

(define (read-value ctx pos form)
  ((or (hashq-ref *readers* form)
       (error "unrecognized form" form))
   ctx pos))

(define (skip-value ctx pos form)
  ((or (hashq-ref *scanners* form)
       (error "unrecognized form" form))
   ctx pos))

;; Parsers for particular attributes.
;;
(define (parse-location-list ctx offset)
  (let lp ((pos (+ (meta-loc-start (ctx-meta ctx)) offset))
           (out '()))
    (let*-values (((start pos) (read-addr ctx pos))
                  ((end pos) (read-addr ctx pos)))
      (if (and (zero? start) (zero? end))
          (reverse out)
          (let*-values (((len pos) (read-u16 ctx pos))
                        ((block pos) (read-block ctx pos len)))
            (lp pos
                (cons (list start end (parse-location ctx block)) out)))))))

(define (parse-location ctx loc)
  (cond
   ((bytevector? loc)
    (let ((len (bytevector-length loc))
          (addr-size (ctx-addr-size ctx))
          (endianness (ctx-endianness ctx)))
      (define (u8-ref pos) (bytevector-u8-ref loc pos))
      (define (s8-ref pos) (bytevector-s8-ref loc pos))
      (define (u16-ref pos) (bytevector-u16-ref loc pos endianness))
      (define (s16-ref pos) (bytevector-s16-ref loc pos endianness))
      (define (u32-ref pos) (bytevector-u32-ref loc pos endianness))
      (define (s32-ref pos) (bytevector-s32-ref loc pos endianness))
      (define (u64-ref pos) (bytevector-u64-ref loc pos endianness))
      (define (s64-ref pos) (bytevector-s64-ref loc pos endianness))
      (let lp ((pos 0) (out '()))
        (if (= pos len)
            (reverse out)
            (let ((op (location-op->name (u8-ref pos))))
              (case op
                ((addr)
                 (case addr-size
                   ((4) (lp (+ pos 5) (cons (list op (u32-ref (1+ pos))) out)))
                   ((8) (lp (+ pos 9) (cons (list op (u64-ref (1+ pos))) out)))
                   (else (error "what!"))))
                ((call-ref)
                 (case addr-size
                   ((4) (lp (+ pos 5)
                            (cons (list op (+ (meta-info-start (ctx-meta ctx))
                                              (u32-ref (1+ pos))))
                                  out)))
                   ((8) (lp (+ pos 9)
                            (cons (list op (+ (meta-info-start (ctx-meta ctx))
                                              (u64-ref (1+ pos))))
                                  out)))
                   (else (error "what!"))))
                ((const1u pick deref-size xderef-size)
                 (lp (+ pos 2) (cons (list op (u8-ref (1+ pos))) out)))
                ((const1s)
                 (lp (+ pos 2) (cons (list op (s8-ref (1+ pos))) out)))
                ((const2u)
                 (lp (+ pos 3) (cons (list op (u16-ref (1+ pos))) out)))
                ((call2)
                 (lp (+ pos 3) (cons (list op (+ (ctx-compile-unit-start ctx)
                                                 (u16-ref (1+ pos))))
                                     out)))
                ((const2s skip bra)
                 (lp (+ pos 3) (cons (list op (s16-ref (1+ pos))) out)))
                ((const4u)
                 (lp (+ pos 5) (cons (list op (u32-ref (1+ pos))) out)))
                ((call4)
                 (lp (+ pos 5) (cons (list op (+ (ctx-compile-unit-start ctx)
                                                 (u32-ref (1+ pos))))
                                     out)))
                ((const4s)
                 (lp (+ pos 5) (cons (list op (s32-ref (1+ pos))) out)))
                ((const8u)
                 (lp (+ pos 9) (cons (list op (u64-ref (1+ pos))) out)))
                ((const8s)
                 (lp (+ pos 9) (cons (list op (s64-ref (1+ pos))) out)))
                ((plus-uconst regx piece)
                 (let-values (((val pos) (%read-uleb128 loc (1+ pos))))
                   (lp pos (cons (list op val) out))))
                ((bit-piece)
                 (let*-values (((bit-len pos) (%read-uleb128 loc (1+ pos)))
                               ((bit-offset pos) (%read-uleb128 loc pos)))
                   (lp pos (cons (list op bit-len bit-offset) out))))
                ((breg0 breg1 breg2 breg3 breg4 breg5 breg6 breg7 breg8 breg9
                        breg10 breg11 breg12 breg13 breg14 breg15 breg16 breg17
                        breg18 breg19 breg20 breg21 breg22 breg23 breg24 breg25
                        breg26 breg27 breg28 breg29 breg30 breg31 fbreg)
                 (let-values (((val pos) (%read-sleb128 loc (1+ pos))))
                   (lp pos (cons (list op val) out))))
                (else
                 (if (number? op)
                     ;; We failed to parse this opcode; we have to give
                     ;; up
                     loc
                     (lp (1+ pos) (cons (list op) out))))))))))
   (else
    (parse-location-list ctx loc))))

;; Statement programs.
(define-record-type <lregs>
  (make-lregs pos pc file line column)
  lregs?
  (pos lregs-pos set-lregs-pos!)
  (pc lregs-pc set-lregs-pc!)
  (file lregs-file set-lregs-file!)
  (line lregs-line set-lregs-line!)
  (column lregs-column set-lregs-column!))

(define-record-type <line-prog>
  (%make-line-prog ctx version
                   header-offset program-offset end
                   min-insn-length max-insn-ops default-stmt?
                   line-base line-range opcode-base
                   standard-opcode-lengths
                   include-directories file-names
                   regs)
  line-prog?
  (ctx line-prog-ctx)
  (version line-prog-version)
  (header-offset line-prog-header-offset)
  (program-offset line-prog-program-offset)
  (end line-prog-end)
  (min-insn-length line-prog-min-insn-length)
  (max-insn-ops line-prog-max-insn-ops)
  (default-stmt? line-prog-default-stmt?)
  (line-base line-prog-line-base)
  (line-range line-prog-line-range)
  (opcode-base line-prog-opcode-base)
  (standard-opcode-lengths line-prog-standard-opcode-lengths)
  (include-directories line-prog-include-directories)
  (file-names line-prog-file-names)
  (regs line-prog-regs))

(define (make-line-prog ctx header-pos end)
  (unless (> end (+ header-pos 12))
    (error "statement program header too short"))
  (let-values (((len pos offset-size) (read-initial-length ctx header-pos)))
    (unless (<= (+ pos len) end)
      (error (".debug_line too short")))
    (let*-values (((version pos) (read-u16 ctx pos))
                  ((prologue-len prologue-pos) (read-u32 ctx pos))
                  ((min-insn-len pos) (read-u8 ctx prologue-pos))
                  ;; The maximum_operations_per_instruction field is
                  ;; only present in DWARFv4.
                  ((max-insn-ops pos) (if (< version 4)
                                          (values 1 pos)
                                          (read-u8 ctx pos)))
                  ((default-stmt pos) (read-u8 ctx pos))
                  ((line-base pos) (read-s8 ctx pos))
                  ((line-range pos) (read-u8 ctx pos))
                  ((opcode-base pos) (read-u8 ctx pos))
                  ((opcode-lens pos) (read-block ctx pos (1- opcode-base)))
                  ((include-directories pos) (read-string-seq ctx pos))
                  ((file-names pos)
                   (let lp ((pos pos) (strs '()))
                     (if (zero? (bytevector-u8-ref (ctx-bv ctx) pos))
                         (values (reverse strs) (1+ pos))
                         (let-values (((str pos) (read-string ctx pos)))
                           (let* ((pos (skip-leb128 ctx pos)) ; skip dir
                                  (pos (skip-leb128 ctx pos)) ; skip mtime
                                  (pos (skip-leb128 ctx pos))) ; skip len
                             (lp pos (cons str strs))))))))
      (unless (= pos (+ prologue-pos prologue-len))
        (error "unexpected prologue length"))
      (%make-line-prog ctx version header-pos pos end
                       min-insn-len max-insn-ops (not (zero? default-stmt))
                       line-base line-range opcode-base opcode-lens
                       include-directories file-names
                       ;; Initial state: file=1, line=1, col=0
                       (make-lregs pos 0 1 1 0)))))

(define (line-prog-next-row prog pos pc file line col)
  (let ((ctx (line-prog-ctx prog))
        (end (line-prog-end prog))
        (min-insn-len (line-prog-min-insn-length prog))
        (line-base (line-prog-line-base prog))
        (line-range (line-prog-line-range prog))
        (opcode-base (line-prog-opcode-base prog))
        (opcode-lens (line-prog-standard-opcode-lengths prog)))

    (let lp ((pos pos) (pc pc) (file file) (line line) (col col))
      (cond
       ((>= pos end)
        (values #f #f #f #f #f))
       (else
        (let-values (((op pos) (read-u8 ctx pos)))
          (cond
           ((zero? op)                  ; extended opcodes
            (let*-values (((len pos*) (read-uleb128 ctx pos))
                          ((op pos) (read-u8 ctx pos*)))
              (case op
                ((1)                    ; end-sequence
                 (values pos pc file line col))
                ((2)                    ; set-address
                 (let-values (((addr pos) (read-addr ctx pos)))
                   (unless (>= addr pc)
                     (error "pc not advancing"))
                   (lp pos addr file line col)))
                ((3)                    ; define-file
                 (warn "define-file unimplemented")
                 (lp (+ pos* len) pc file line col))
                ((4)                    ; set-discriminator; ignore.
                 (lp (+ pos* len) pc file line col))
                (else
                 (warn "unknown extended op" op)
                 (lp (+ pos* len) pc file line col)))))

           ((< op opcode-base)          ; standard opcodes
            (case op
              ((1)                      ; copy
               (values pos pc file line col))
              ((2)                      ; advance-pc
               (let-values (((advance pos) (read-uleb128 ctx pos)))
                 (lp pos (+ pc (* advance min-insn-len)) file line col)))
              ((3)                      ; advance-line
               (let-values (((diff pos) (read-sleb128 ctx pos)))
                 (lp pos pc file (+ line diff) col)))
              ((4)                      ; set-file
               (let-values (((file pos) (read-uleb128 ctx pos)))
                 (lp pos pc file line col)))
              ((5)                      ; set-column
               (let-values (((col pos) (read-uleb128 ctx pos)))
                 (lp pos pc file line col)))
              ((6)                      ; negate-line
               (lp pos pc file line col))
              ((7)                      ; set-basic-block
               (lp pos pc file line col))
              ((8)                      ; const-add-pc
               (let ((advance (floor/ (- 255 opcode-base) line-range)))
                 (lp pos (+ pc (* advance min-insn-len)) file line col)))
              ((9)                      ; fixed-advance-pc
               (let-values (((advance pos) (read-u16 ctx pos)))
                 (lp pos (+ pc (* advance min-insn-len)) file line col)))
              (else
               ;; fixme: read args and move on
               (error "unknown extended op" op))))
           (else                        ; special opcodes
            (let-values (((quo rem) (floor/ (- op opcode-base) line-range)))
              (values pos (+ pc (* quo min-insn-len))
                      file (+ line (+ rem line-base)) col))))))))))

(define (line-prog-advance prog)
  (let ((regs (line-prog-regs prog)))
    (call-with-values (lambda ()
                        (line-prog-next-row prog
                                            (lregs-pos regs)
                                            (lregs-pc regs)
                                            (lregs-file regs)
                                            (lregs-line regs)
                                            (lregs-column regs)))
      (lambda (pos pc file line col)
        (cond
         ((not pos)
          (values #f #f #f #f))
         (else
          (set-lregs-pos! regs pos)
          (set-lregs-pc! regs pc)
          (set-lregs-file! regs file)
          (set-lregs-line! regs line)
          (set-lregs-column! regs col)
          ;; Return DWARF-numbered lines and columns (1-based).
          (values pc
                  (if (zero? file)
                      #f
                      (list-ref (line-prog-file-names prog) (1- file)))
                  (if (zero? line) #f line)
                  (if (zero? col) #f col))))))))

(define (line-prog-scan-to-pc prog target-pc)
  (let ((regs (line-prog-regs prog)))
    (define (finish pos pc file line col)
      (set-lregs-pos! regs pos)
      (set-lregs-pc! regs pc)
      (set-lregs-file! regs file)
      (set-lregs-line! regs line)
      (set-lregs-column! regs col)
      ;; Return DWARF-numbered lines and columns (1-based).
      (values pc
              (if (zero? file)
                  #f
                  (list-ref (line-prog-file-names prog) (1- file)))
              (if (zero? line) #f line)
              (if (zero? col) #f col)))
    (define (scan pos pc file line col)
      (call-with-values (lambda ()
                          (line-prog-next-row prog pos pc file line col))
        (lambda (pos* pc* file* line* col*)
          (cond
           ((not pos*)
            (values #f #f #f #f))
           ((< pc* target-pc)
            (scan pos* pc* file* line* col*))
           ((= pc* target-pc)
            (finish pos* pc* file* line* col*))
           ((zero? pc)
            ;; We scanned from the beginning didn't find any info.
            (values #f #f #f #f))
           (else
            (finish pos pc file line col))))))
    (let ((pos (lregs-pos regs))
          (pc (lregs-pc regs))
          (file (lregs-file regs))
          (line (lregs-line regs))
          (col (lregs-column regs)))
      (if (< pc target-pc)
          (scan pos pc file line col)
          (scan (line-prog-program-offset prog) 0 1 1 0)))))

(define-syntax-rule (define-attribute-parsers parse (name parser) ...)
  (define parse
    (let ((parsers (make-hash-table)))
      (hashq-set! parsers 'name parser)
      ...
      (lambda (ctx attr val)
        (cond
         ((hashq-ref parsers attr) => (lambda (p) (p ctx val)))
         (else val))))))

(define-attribute-parsers parse-attribute
  (encoding (lambda (ctx val) (type-encoding->name val)))
  (accessibility (lambda (ctx val) (access-code->name val)))
  (visibility (lambda (ctx val) (visibility-code->name val)))
  (virtuality (lambda (ctx val) (virtuality-code->name val)))
  (language (lambda (ctx val) (language-code->name val)))
  (location parse-location)
  (data-member-location parse-location)
  (case-sensitive (lambda (ctx val) (case-sensitivity-code->name val)))
  (calling-convention (lambda (ctx val) (calling-convention-code->name val)))
  (inline (lambda (ctx val) (inline-code->name val)))
  (ordering (lambda (ctx val) (ordering-code->name val)))
  (discr-value (lambda (ctx val) (discriminant-code->name val))))

;; "Debugging Information Entries": DIEs.
;;
(define-record-type <die>
  (make-die ctx offset abbrev vals)
  die?
  (ctx die-ctx)
  (offset die-offset)
  (abbrev die-abbrev)
  (vals %die-vals %set-die-vals!))

(define (die-tag die)
  (abbrev-tag (die-abbrev die)))

(define (die-attrs die)
  (abbrev-attrs (die-abbrev die)))

(define (die-forms die)
  (abbrev-forms (die-abbrev die)))

(define (die-vals die)
  (let ((vals (%die-vals die)))
    (or vals
        (begin
          (%set-die-vals! die (read-values (die-ctx die) (skip-leb128 (die-ctx die) (die-offset die)) (die-abbrev die)))
          (die-vals die)))))

(define* (die-next-offset die #:optional offset-vals)
  (let ((ctx (die-ctx die)))
    (skip-values ctx (or offset-vals (skip-leb128 ctx (die-offset die)))
                 (die-abbrev die))))

(define* (die-ref die attr #:optional default)
  (cond
   ((list-index (die-attrs die) attr)
    => (lambda (n) (list-ref (die-vals die) n)))
   (else default)))

(define (die-specification die)
  (and=> (die-ref die 'specification)
         (lambda (offset) (find-die-by-offset (die-ctx die) offset))))

(define (die-name die)
  (or (die-ref die 'name)
      (and=> (die-specification die) die-name)))

(define (die-qname die)
  (cond
   ((eq? (die-tag die) 'compile-unit) "")
   ((die-ref die 'name)
    => (lambda (name)
         (if (eq? (die-tag (ctx-die (die-ctx die))) 'compile-unit)
             name ; short cut
             (string-append (die-qname (ctx-die (die-ctx die))) "::" name))))
   ((die-specification die)
    => die-qname)
   (else #f)))

(define (die-line-prog die)
  (let ((stmt-list (die-ref die 'stmt-list)))
    (and stmt-list
         (let* ((ctx (die-ctx die))
                (meta (ctx-meta ctx)))
           (make-line-prog ctx
                           (+ (meta-line-start meta) stmt-list)
                           (meta-line-end meta))))))

(define (read-values ctx offset abbrev)
  (let lp ((attrs (abbrev-attrs abbrev))
           (forms (abbrev-forms abbrev))
           (vals '())
           (pos offset))
    (if (null? forms)
        (values (reverse vals) pos)
        (let-values (((val pos) (read-value ctx pos (car forms))))
          (lp (cdr attrs) (cdr forms)
              (cons (parse-attribute ctx (car attrs) val) vals)
              pos)))))

(define (skip-values ctx offset abbrev)
  (let lp ((forms (abbrev-forms abbrev))
           (pos offset))
    (if (null? forms)
        pos
        (lp (cdr forms) (skip-value ctx pos (car forms))))))

(define (read-die-abbrev ctx offset)
  (let*-values (((code pos) (read-uleb128 ctx offset)))
    (values (cond ((zero? code) #f)
                  ((vector-ref (ctx-abbrevs ctx) code))
                  (else (error "unknown abbrev" ctx code)))
            pos)))

(define (read-die ctx offset)
  (let*-values (((abbrev pos) (read-die-abbrev ctx offset)))
    (if abbrev
        (values (make-die ctx offset abbrev #f)
                (skip-values ctx pos abbrev))
        (values #f pos))))

(define* (die-sibling ctx abbrev offset #:optional offset-vals offset-end)
  (cond
   ((not (abbrev-has-children? abbrev))
    (or offset-end
        (skip-values ctx
                     (or offset-vals (skip-leb128 ctx offset))
                     abbrev)))
   ((memq 'sibling (abbrev-attrs abbrev))
    (let lp ((offset (or offset-vals (skip-leb128 ctx offset)))
             (attrs (abbrev-attrs abbrev))
             (forms (abbrev-forms abbrev)))
      (if (eq? (car attrs) 'sibling)
          (read-value ctx offset (car forms))
          (lp (skip-value ctx offset (car forms))
              (cdr attrs) (cdr forms)))))
   (else
    (call-with-values
        (lambda ()
          (fold-die-list ctx
                         (or offset-end
                             (skip-values ctx
                                          (or offset-vals
                                              (skip-leb128 ctx offset))
                                          abbrev))
                         (lambda (ctx offset abbrev) #t)
                         error
                         #f))
      (lambda (seed pos)
        pos)))))

(define (find-die-context ctx offset)
  (define (not-found)
    (error "failed to find DIE by context" offset))
  (define (in-context? ctx)
    (and (<= (ctx-start ctx) offset)
         (< offset (ctx-end ctx))))
  (define (find-root ctx)
    (if (in-context? ctx)
        ctx
        (find-root (or (ctx-parent ctx) (not-found)))))
  (define (find-leaf ctx)
    (let lp ((kids (ctx-children ctx)))
      (if (null? kids)
          ctx
          (if (in-context? (car kids))
              (find-leaf (car kids))
              (lp (cdr kids))))))
  (find-leaf (find-root ctx)))

(define (find-die-by-offset ctx offset)
  (or (read-die (find-die-context ctx offset) offset)
      (error "Failed to read DIE at offset" offset)))

(define-syntax-rule (let/ec k e e* ...)
  (let ((tag (make-prompt-tag)))
    (call-with-prompt
     tag
     (lambda ()
       (let ((k (lambda args (apply abort-to-prompt tag args))))
         e e* ...))
     (lambda (_ res) res))))

(define* (find-die roots pred #:key
                   (skip? (lambda (ctx offset abbrev) #f))
                   (recurse? (lambda (die) #t)))
  (let/ec k
    (define (visit-die die)
      (cond
       ((pred die)
        (k die))
       ((recurse? die)
        (fold-die-children die (lambda (die seed) (visit-die die)) #f
                           #:skip? skip?))
       (else #f)))
    (for-each visit-die roots)
    #f))

(define (die-low-pc die)
  (die-ref die 'low-pc))
(define (die-high-pc die)
  (let ((val (die-ref die 'high-pc)))
    (and val
         (let ((idx (list-index (die-attrs die) 'high-pc)))
           (case (list-ref (die-forms die) idx)
             ((addr) val)
             (else (+ val (die-low-pc die))))))))

(define (find-die-by-pc roots pc)
  ;; The result will be a subprogram.
  (define (skip? ctx offset abbrev)
    (case (abbrev-tag abbrev)
      ((subprogram compile-unit) #f)
      (else #t)))
  (define (recurse? die)
    (case (die-tag die)
      ((compile-unit)
       (not (or (and=> (die-low-pc die)
                       (lambda (low) (< pc low)))
                (and=> (die-high-pc die)
                       (lambda (high) (<= high pc))))))
      (else #f)))
  (find-die roots
            (lambda (die)
              (and (eq? (die-tag die) 'subprogram)
                   (equal? (die-low-pc die) pc)))
            #:skip? skip? #:recurse? recurse?))

(define (fold-die-list ctx offset skip? proc seed)
  (let ((ctx (find-die-context ctx offset)))
    (let lp ((offset offset) (seed seed))
      (let-values (((abbrev pos) (read-die-abbrev ctx offset)))
        (cond
         ((not abbrev) (values seed pos))
         ((skip? ctx offset abbrev)
          (lp (die-sibling ctx abbrev offset pos) seed))
         (else
          (let-values (((vals pos) (read-values ctx pos abbrev)))
            (let* ((die (make-die ctx offset abbrev vals))
                   (seed (proc die seed)))
              (lp (die-sibling ctx abbrev offset #f pos) seed)))))))))

(define* (fold-die-children die proc seed #:key
                            (skip? (lambda (ctx offset abbrev) #f)))
  (if (abbrev-has-children? (die-abbrev die))
      (values (fold-die-list (die-ctx die) (die-next-offset die)
                             skip? proc seed))
      seed))

(define (die-children die)
  (reverse (fold-die-children die cons '())))

(define (add-to-parent! ctx)
  (let ((parent (ctx-parent ctx)))
    (set-children! parent
                   (append (ctx-children parent) (list ctx)))
    ctx))

(define (make-compilation-unit-context ctx offset-size addr-size
                                       abbrevs start len)
  (unless (= addr-size (ctx-addr-size ctx))
    (error "ELF word size not equal to compilation unit addrsize"))
  (add-to-parent!
   (make-dwarf-context (ctx-bv ctx)
                       offset-size (ctx-endianness ctx)
                       (ctx-meta ctx)
                       abbrevs ctx #f start (+ start 4 len) '())))

(define (make-child-context die)
  (let ((ctx (die-ctx die)))
    (add-to-parent!
     (make-dwarf-context (ctx-bv ctx)
                         (ctx-offset-size ctx) (ctx-endianness ctx)
                         (ctx-meta ctx)
                         (ctx-abbrevs ctx)
                         ctx die
                         (die-next-offset die)
                         (die-sibling ctx (die-abbrev die) (die-offset die))
                         '()))))

(define (ctx-language ctx)
  (or (and=> (ctx-die ctx) (lambda (x) (die-ref x 'language)))
      (and=> (ctx-parent ctx) ctx-language)))

(define (populate-context-tree! die)
  (define (skip? ctx offset abbrev)
    (case (abbrev-tag abbrev)
      ((class-type structure-type namespace) #f)
      (else #t)))
  (case (die-tag die)
    ((compile-unit class-type structure-type namespace)
     (let ((ctx (make-child-context die)))
       ;; For C++, descend into classes and structures so that we
       ;; populate the context tree.  Note that for compile-unit, we
       ;; still need to call `make-child-context' for its side effect of
       ;; adding to the context tree.
       (when (eq? (ctx-language ctx) 'c++)
         (fold-die-children die
                            (lambda (die seed) (populate-context-tree! die))
                            #f
                            #:skip? skip?))))))

(define (read-compilation-unit ctx pos)
  (let*-values (((start) pos)
                ((len pos offset-size) (read-initial-length ctx pos))
                ((version pos) (read-u16 ctx pos))
                ((abbrevs-offset pos) (read-offset ctx pos offset-size))
                ((av) (read-abbrevs ctx abbrevs-offset))
                ((addrsize pos) (read-u8 ctx pos))
                ((ctx) (make-compilation-unit-context ctx offset-size addrsize
                                                      av start len))
                ((die pos) (read-die ctx pos)))
    (populate-context-tree! die)
    (values die (ctx-end ctx))))

(define (read-die-roots ctx)
  (let lp ((dies '()) (pos (meta-info-start (ctx-meta ctx))))
    (if (< pos (meta-info-end (ctx-meta ctx)))
        (let-values (((die pos) (read-compilation-unit ctx pos)))
          (if die
              (lp (cons die dies) pos)
              (reverse dies)))
        (reverse dies))))

(define (fold-pubname-set ctx pos folder seed)
  (let*-values (((len pos offset-size) (read-initial-length ctx pos))
                ((version pos) (read-u16 ctx pos))
                ((info-offset pos) (read-offset ctx pos offset-size))
                ((info-offset) (+ info-offset
                                  (meta-info-start (ctx-meta ctx))))
                ((info-len pos) (read-offset ctx pos offset-size)))
    (let lp ((pos pos) (seed seed))
      (let-values (((offset pos) (read-offset ctx pos offset-size)))
        (if (zero? offset)
            (values seed pos)
            (let-values (((str pos) (read-string ctx pos)))
              (lp pos
                  (folder str (+ offset info-offset) seed))))))))

(define (fold-pubnames ctx folder seed)
  (let ((end (meta-pubnames-end (ctx-meta ctx))))
    (if end
        (let lp ((pos (meta-pubnames-start (ctx-meta ctx))) (seed seed))
          (if (< pos end)
              (let-values (((seed pos) (fold-pubname-set ctx pos folder seed)))
                (lp pos seed))
              seed))
        seed)))

(define (align address alignment)
  (+ address
     (modulo (- alignment (modulo address alignment)) alignment)))

(define (fold-arange-set ctx pos folder seed)
  (let*-values (((len pos offset-size) (read-initial-length ctx pos))
                ((version pos) (read-u16 ctx pos))
                ((info-offset pos) (read-offset ctx pos offset-size))
                ((info-offset) (+ info-offset
                                  (meta-info-start (ctx-meta ctx))))
                ((addr-size pos) (read-u8 ctx pos))
                ((segment-size pos) (read-u8 ctx pos)))
    (let lp ((pos (align pos (* 2 (ctx-addr-size ctx)))) (seed seed))
      (let*-values (((addr pos) (read-addr ctx pos))
                    ((len pos) (read-addr ctx pos)))
        (if (and (zero? addr) (zero? len))
            (values seed pos)
            (lp pos
                (folder info-offset addr len seed)))))))

(define (fold-aranges ctx folder seed)
  (let ((end (meta-aranges-end (ctx-meta ctx))))
    (if end
        (let lp ((pos (meta-aranges-start (ctx-meta ctx))) (seed seed))
          (if (< pos end)
              (let-values (((seed pos) (fold-arange-set ctx pos folder seed)))
                (lp pos seed))
              seed))
        seed)))

(define* (elf->dwarf-context elf #:key (vaddr 0) (memsz 0)
                             (path #f) (lib-path path))
  (let* ((sections (elf-sections-by-name elf))
         (info (assoc-ref sections ".debug_info"))
         (abbrevs (assoc-ref sections ".debug_abbrev"))
         (strtab (assoc-ref sections ".debug_str"))
         (loc (assoc-ref sections ".debug_loc"))
         (line (assoc-ref sections ".debug_line"))
         (pubnames (assoc-ref sections ".debug_pubnames"))
         (aranges (assoc-ref sections ".debug_aranges")))
    (make-dwarf-context (elf-bytes elf)
                        4 ;; initial offset size
                        (elf-byte-order elf)
                        (make-dwarf-meta
                         (elf-word-size elf)
                         vaddr memsz
                         path lib-path
                         (elf-section-offset info)
                         (+ (elf-section-offset info)
                            (elf-section-size info))
                         (elf-section-offset abbrevs)
                         (+ (elf-section-offset abbrevs)
                            (elf-section-size abbrevs))
                         (elf-section-offset strtab)
                         (+ (elf-section-offset strtab)
                            (elf-section-size strtab))
                         (elf-section-offset loc)
                         (+ (elf-section-offset loc)
                            (elf-section-size loc))
                         (and line
                              (elf-section-offset line))
                         (and line
                              (+ (elf-section-offset line)
                                 (elf-section-size line)))
                         (and pubnames
                              (elf-section-offset pubnames))
                         (and pubnames
                              (+ (elf-section-offset pubnames)
                                 (elf-section-size pubnames)))
                         (and aranges
                              (elf-section-offset aranges))
                         (and aranges
                              (+ (elf-section-offset aranges)
                                 (elf-section-size aranges))))
                        #() #f #f
                        (elf-section-offset info)
                        (+ (elf-section-offset info)
                           (elf-section-size info))
                        '())))

(define (die->tree die)
  (cons* (die-tag die)
         (cons 'offset (die-offset die))
         (reverse! (fold-die-children
                    die
                    (lambda (die seed)
                      (cons (die->tree die) seed))
                    (fold acons '() (die-attrs die) (die-vals die))))))
