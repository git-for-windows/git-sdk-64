;;; 'SCM' type tag decoding.
;;; Copyright (C) 2014, 2015, 2017, 2018 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (system base types)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-60)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 iconv) #:prefix iconv:)
  #:use-module (ice-9 format)
  #:use-module (ice-9 vlist)
  #:use-module (system foreign)
  #:export (%word-size

            memory-backend
            memory-backend?
            %ffi-memory-backend
            dereference-word
            memory-port
            type-number->name

            inferior-object?
            inferior-object-kind
            inferior-object-sub-kind
            inferior-object-address

            inferior-struct?
            inferior-struct-name
            inferior-struct-fields

            scm->object))

;; This module can be loaded from GDB-linked-against-2.0, so use 2.2
;; features conditionally.
(cond-expand
  (guile-2.2 (use-modules (system syntax internal))) ;for 'make-syntax'
  (else #t))

;;; Commentary:
;;;
;;; 'SCM' type tag decoding, primarily to support Guile debugging in GDB.
;;;
;;; Code:


;;;
;;; Memory back-ends.
;;;

(define %word-size
  ;; The pointer size.
  (sizeof '*))

(define-record-type <memory-backend>
  (memory-backend peek open type-name)
  memory-backend?
  (peek      memory-backend-peek)
  (open      memory-backend-open)
  (type-name memory-backend-type-name))           ;for SMOBs

(define %ffi-memory-backend
  ;; The FFI back-end to access the current process's memory.  The main
  ;; purpose of this back-end is to allow testing.
  (let ()
    (define (dereference-word address)
      (let* ((ptr (make-pointer address))
             (bv  (pointer->bytevector ptr %word-size)))
        (bytevector-uint-ref bv 0 (native-endianness) %word-size)))

    (define (open address size)
      (define current-address address)

      (define (read-memory! bv index count)
        (let* ((ptr   (make-pointer current-address))
               (mem   (pointer->bytevector ptr count)))
          (bytevector-copy! mem 0 bv index count)
          (set! current-address (+ current-address count))
          count))

      (if size
          (let* ((ptr (make-pointer address))
                 (bv  (pointer->bytevector ptr size)))
            (open-bytevector-input-port bv))
          (let ((port (make-custom-binary-input-port "ffi-memory"
                                                     read-memory!
                                                     #f #f #f)))
            (setvbuf port 'none)
            port)))

    (memory-backend dereference-word open #f)))

(define-inlinable (dereference-word backend address)
  "Return the word at ADDRESS, using BACKEND."
  (let ((peek (memory-backend-peek backend)))
    (peek address)))

(define-syntax memory-port
  (syntax-rules ()
    "Return an input port to the SIZE bytes at ADDRESS, using BACKEND.  When
SIZE is omitted, return an unbounded port to the memory at ADDRESS."
    ((_ backend address)
     (let ((open (memory-backend-open backend)))
       (open address #f)))
    ((_ backend address size)
     (if (zero? size)
         ;; GDB's 'open-memory' raises an error when size
         ;; is zero, so we must handle that case specially.
         (open-bytevector-input-port '#vu8())
         (let ((open (memory-backend-open backend)))
           (open address size))))))

(define (get-word port)
  "Read a word from PORT and return it as an integer."
  (let ((bv (get-bytevector-n port %word-size)))
    (bytevector-uint-ref bv 0 (native-endianness) %word-size)))

(define (read-c-string backend address)
  "Read a NUL-terminated string from ADDRESS, decode it as UTF-8, and
return the corresponding string."
  (define port
    (memory-port backend address))

  (let loop ((bytes '()))
    (let ((byte (get-u8 port)))
      (if (zero? byte)
          (utf8->string (u8-list->bytevector (reverse bytes)))
          (loop (cons byte bytes))))))

(define-inlinable (type-number->name backend kind number)
  "Return the name of the type NUMBER of KIND, where KIND is one of
'smob or 'port, or #f if the information is unavailable."
  (let ((proc (memory-backend-type-name backend)))
    (and proc (proc kind number))))


;;;
;;; Matching bit patterns and cells.
;;;

(define-syntax match-cell-words
  (syntax-rules (bytevector)
    ((_ port ((bytevector name len) rest ...) body)
     (let ((name      (get-bytevector-n port len))
           (remainder (modulo len %word-size)))
       (unless (zero? remainder)
         (get-bytevector-n port (- %word-size remainder)))
       (match-cell-words port (rest ...) body)))
    ((_ port (name rest ...) body)
     (let ((name (get-word port)))
       (match-cell-words port (rest ...) body)))
    ((_ port () body)
     body)))

(define-syntax match-bit-pattern
  (syntax-rules (& || = _)
    ((match-bit-pattern bits ((a || b) & n = c) consequent alternate)
     (let ((tag (logand bits n)))
       (if (= tag c)
           (let ((b tag)
                 (a (logand bits (bitwise-not n))))
             consequent)
           alternate)))
    ((match-bit-pattern bits (x & n = c) consequent alternate)
     (let ((tag (logand bits n)))
       (if (= tag c)
           (let ((x bits))
             consequent)
           alternate)))
    ((match-bit-pattern bits (_ & n = c) consequent alternate)
     (let ((tag (logand bits n)))
       (if (= tag c)
           consequent
           alternate)))
    ((match-bit-pattern bits ((a << n) || c) consequent alternate)
     (let ((tag (bitwise-and bits (- (expt 2 n) 1))))
       (if (= tag c)
           (let ((a (arithmetic-shift bits (- n))))
             consequent)
           alternate)))))

(define-syntax match-cell-clauses
  (syntax-rules ()
    ((_ port tag (((tag-pattern thing ...) body) rest ...))
     (match-bit-pattern tag tag-pattern
                        (match-cell-words port (thing ...) body)
                        (match-cell-clauses port tag (rest ...))))
    ((_ port tag ())
     (inferior-object 'unmatched-tag tag))))

(define-syntax match-cell
  (syntax-rules ()
    "Match a cell---i.e., a non-immediate value other than a pair.  The
cell's contents are read from PORT."
    ((_ port (pattern body ...) ...)
     (let ((port* port)
           (tag   (get-word port)))
       (match-cell-clauses port* tag
                           ((pattern (begin body ...))
                            ...))))))

(define-syntax match-scm-clauses
  (syntax-rules ()
    ((_ bits
        (bit-pattern body ...)
        rest ...)
     (match-bit-pattern bits bit-pattern
                        (begin body ...)
                        (match-scm-clauses bits rest ...)))
    ((_ bits)
     'unmatched-scm)))

(define-syntax match-scm
  (syntax-rules ()
    "Match BITS, an integer representation of an 'SCM' value, against
CLAUSES.  Each clause must have the form:

  (PATTERN BODY ...)

PATTERN is a bit pattern that may specify bitwise operations on BITS to
determine if it matches.  TEMPLATE specify the name of the variable to bind
the matching bits, possibly with bitwise operations to extract it from BITS."
    ((_ bits clauses ...)
     (let ((bits* bits))
       (match-scm-clauses bits* clauses ...)))))


;;;
;;; Tags---keep in sync with libguile/tags.h!
;;;

;; Immediate values.
(define %tc2-int 2)
(define %tc3-imm24 4)

(define %tc3-cons 0)
(define %tc3-int1 %tc2-int)
(define %tc3-int2 (+ %tc2-int 4))

(define %tc8-char (+ 8 %tc3-imm24))
(define %tc8-flag (+ %tc3-imm24 0))

;; Cell types.
(define %tc3-struct #x01)
(define %tc7-symbol #x05)
(define %tc7-variable #x07)
(define %tc7-vector #x0d)
(define %tc7-wvect #x0f)
(define %tc7-string #x15)
(define %tc7-number #x17)
(define %tc7-hashtable #x1d)
(define %tc7-pointer #x1f)
(define %tc7-fluid #x25)
(define %tc7-stringbuf #x27)
(define %tc7-dynamic-state #x2d)
(define %tc7-frame #x2f)
(define %tc7-keyword #x35)
(define %tc7-syntax #x3d)
(define %tc7-program #x45)
(define %tc7-vm-continuation #x47)
(define %tc7-bytevector #x4d)
(define %tc7-weak-set #x55)
(define %tc7-weak-table #x57)
(define %tc7-array #x5d)
(define %tc7-bitvector #x5f)
(define %tc7-port #x7d)
(define %tc7-smob #x77)

(define %tc16-bignum (+ %tc7-number (* 1 256)))
(define %tc16-real (+ %tc7-number (* 2 256)))
(define %tc16-complex (+ %tc7-number (* 3 256)))
(define %tc16-fraction (+ %tc7-number (* 4 256)))


;; "Stringbufs".
(define-record-type <stringbuf>
  (stringbuf string)
  stringbuf?
  (string stringbuf-contents))

(set-record-type-printer! <stringbuf>
                          (lambda (stringbuf port)
                            (display "#<stringbuf " port)
                            (write (stringbuf-contents stringbuf) port)
                            (display "#>" port)))

;; Structs.
(define-record-type <inferior-struct>
  (inferior-struct name fields)
  inferior-struct?
  (name   inferior-struct-name)
  (fields inferior-struct-fields set-inferior-struct-fields!))

(define print-inferior-struct
  (let ((%printed-struct (make-parameter vlist-null)))
    (lambda (struct port)
      (if (vhash-assq struct (%printed-struct))
          (format port "#-1#")
          (begin
            (format port "#<struct ~a"
                    (inferior-struct-name struct))
            (parameterize ((%printed-struct
                            (vhash-consq struct #t (%printed-struct))))
              (for-each (lambda (field)
                          (if (eq? field struct)
                              (display " #0#" port)
                              (format port " ~s" field)))
                        (inferior-struct-fields struct)))
            (format port " ~x>" (object-address struct)))))))

(set-record-type-printer! <inferior-struct> print-inferior-struct)

;; Object type to represent complex objects from the inferior process that
;; cannot be really converted to usable Scheme objects in the current
;; process.
(define-record-type <inferior-object>
  (%inferior-object kind sub-kind address)
  inferior-object?
  (kind     inferior-object-kind)
  (sub-kind inferior-object-sub-kind)
  (address  inferior-object-address))

(define inferior-object
  (case-lambda
    "Return an object representing an inferior object at ADDRESS, of type
KIND/SUB-KIND."
    ((kind address)
     (%inferior-object kind #f address))
    ((kind sub-kind address)
     (%inferior-object kind sub-kind address))))

(set-record-type-printer! <inferior-object>
                          (lambda (io port)
                            (match io
                              (($ <inferior-object> kind sub-kind address)
                               (format port "#<~a ~:[~*~;~a ~]~x>"
                                       kind sub-kind sub-kind
                                       address)))))

(define (inferior-smob backend type-number address)
  "Return an object representing the SMOB at ADDRESS whose type is
TYPE-NUMBER."
  (inferior-object 'smob
                   (or (type-number->name backend 'smob type-number)
                       type-number)
                   address))

(define (inferior-port-type backend address)
  "Return an object representing the 'scm_t_port_type' structure at
ADDRESS."
  (inferior-object 'port-type
                   ;; The 'name' field lives at offset 0.
                   (let ((name (dereference-word backend address)))
                     (if (zero? name)
                         "(nameless)"
                         (read-c-string backend name)))
                   address))

(define (inferior-port backend type-number address)
  "Return an object representing the port at ADDRESS whose type is
TYPE-NUMBER."
  (inferior-object 'port
                   (let ((address (+ address (* 3 %word-size))))
                     (inferior-port-type backend
                                         (dereference-word backend address)))
                   address))

(define %visited-cells
  ;; Vhash of mapping addresses of already visited cells to the
  ;; corresponding inferior object.  This is used to detect and represent
  ;; cycles.
  (make-parameter vlist-null))

(define-syntax visited
  (syntax-rules (->)
    ((_ (address -> object) body ...)
     (parameterize ((%visited-cells (vhash-consv address object
                                                 (%visited-cells))))
       body ...))))

(define (address->inferior-struct address vtable-data-address backend)
  "Read the struct at ADDRESS using BACKEND.  Return an 'inferior-struct'
object representing it."
  (define %vtable-layout-index 0)
  (define %vtable-name-index 5)

  (let* ((layout-address (+ vtable-data-address
                            (* %vtable-layout-index %word-size)))
         (layout-bits    (dereference-word backend layout-address))
         (layout         (scm->object layout-bits backend))
         (name-address   (+ vtable-data-address
                            (* %vtable-name-index %word-size)))
         (name-bits      (dereference-word backend name-address))
         (name           (scm->object name-bits backend)))
    (if (symbol? layout)
        (let* ((layout (symbol->string layout))
               (len    (/ (string-length layout) 2))
               (slots  (dereference-word backend (+ address %word-size)))
               (port   (memory-port backend slots (* len %word-size)))
               (fields (get-bytevector-n port (* len %word-size)))
               (result (inferior-struct name #f)))

          ;; Keep track of RESULT so callees can refer to it if we are
          ;; decoding a circular struct.
          (visited (address -> result)
            (let ((values (map (cut scm->object <> backend)
                               (bytevector->uint-list fields
                                                      (native-endianness)
                                                      %word-size))))
              (set-inferior-struct-fields! result values)
              result)))
        (inferior-object 'invalid-struct address))))

(define* (cell->object address #:optional (backend %ffi-memory-backend))
  "Return an object representing the object at ADDRESS, reading from memory
using BACKEND."
  (or (and=> (vhash-assv address (%visited-cells)) cdr) ; circular object
      (let ((port (memory-port backend address)))
        (match-cell port
          (((vtable-data-address & 7 = %tc3-struct))
           (address->inferior-struct address
                                     (- vtable-data-address %tc3-struct)
                                     backend))
          (((_ & #x7f = %tc7-symbol) buf hash props)
           (match (cell->object buf backend)
             (($ <stringbuf> string)
              (string->symbol string))))
          (((_ & #x7f = %tc7-variable) obj)
           (inferior-object 'variable address))
          (((_ & #x7f = %tc7-string) buf start len)
           (match (cell->object buf backend)
             (($ <stringbuf> string)
              (substring string start (+ start len)))))
          (((_ & #x047f = %tc7-stringbuf) len (bytevector buf len))
           (stringbuf (iconv:bytevector->string buf "ISO-8859-1")))
          (((_ & #x047f = (bitwise-ior #x400 %tc7-stringbuf))
            len (bytevector buf (* 4 len)))
           (stringbuf (iconv:bytevector->string buf
                                                (match (native-endianness)
                                                  ('little "UTF-32LE")
                                                  ('big "UTF-32BE")))))
          (((_ & #x7f = %tc7-bytevector) len address)
           (let ((bv-port (memory-port backend address len)))
             (get-bytevector-n bv-port len)))
          ((((len << 8) || %tc7-vector))
           (let ((words  (get-bytevector-n port (* len %word-size)))
                 (vector (make-vector len)))
             (visited (address -> vector)
               (fold (lambda (element index)
                       (vector-set! vector index element)
                       (+ 1 index))
                     0
                     (map (cut scm->object <> backend)
                          (bytevector->uint-list words (native-endianness)
                                                 %word-size)))
               vector)))
          (((_ & #x7f = %tc7-wvect))
           (inferior-object 'weak-vector address))   ; TODO: show elements
          (((_ & #x7f = %tc7-fluid) init-value)
           (inferior-object 'fluid address))
          (((_ & #x7f = %tc7-dynamic-state))
           (inferior-object 'dynamic-state address))
          ((((flags << 8) || %tc7-port))
           (inferior-port backend (logand flags #xff) address))
          (((_ & #x7f = %tc7-program))
           (inferior-object 'program address))
          (((_ & #xffff = %tc16-bignum))
           (inferior-object 'bignum address))
          (((_ & #xffff = %tc16-real) pad)
           (let* ((address (+ address (* 2 %word-size)))
                  (port    (memory-port backend address (sizeof double)))
                  (words   (get-bytevector-n port (sizeof double))))
             (bytevector-ieee-double-ref words 0 (native-endianness))))
          (((_ & #x7f = %tc7-number) mpi)
           (inferior-object 'number address))
          (((_ & #x7f = %tc7-hashtable) buckets meta-data unused)
           (inferior-object 'hash-table address))
          (((_ & #x7f = %tc7-pointer) address)
           (make-pointer address))
          (((_ & #x7f = %tc7-keyword) symbol)
           (symbol->keyword (cell->object symbol backend)))
          (((_ & #x7f = %tc7-syntax) expression wrap module)
           (cond-expand
             (guile-2.2
              (make-syntax (cell->object expression backend)
                           (cell->object wrap backend)
                           (cell->object module backend)))
             (else
              (inferior-object 'syntax address))))
          (((_ & #x7f = %tc7-vm-continuation))
           (inferior-object 'vm-continuation address))
          (((_ & #x7f = %tc7-weak-set))
           (inferior-object 'weak-set address))
          (((_ & #x7f = %tc7-weak-table))
           (inferior-object 'weak-table address))
          (((_ & #x7f = %tc7-array))
           (inferior-object 'array address))
          (((_ & #x7f = %tc7-bitvector))
           (inferior-object 'bitvector address))
          ((((smob-type << 8) || %tc7-smob) word1)
           (inferior-smob backend smob-type address))))))


(define* (scm->object bits #:optional (backend %ffi-memory-backend))
  "Return the Scheme object corresponding to BITS, the bits of an 'SCM'
object."
  (match-scm bits
    (((integer << 2) || %tc2-int)
     integer)
    ((address & 6 = %tc3-cons)
     (let* ((type  (dereference-word backend address))
            (pair? (not (bit-set? 0 type))))
       (if pair?
           (or (and=> (vhash-assv address (%visited-cells)) cdr)
               (let ((car    type)
                     (cdrloc (+ address %word-size))
                     (pair   (cons *unspecified* *unspecified*)))
                 (visited (address -> pair)
                   (set-car! pair (scm->object car backend))
                   (set-cdr! pair
                             (scm->object (dereference-word backend cdrloc)
                                          backend))
                   pair)))
           (cell->object address backend))))
    (((char << 8) || %tc8-char)
     (integer->char char))
    (((flag << 8) || %tc8-flag)
     (case flag
       ((0)  #f)
       ((1)  #nil)
       ((3)  '())
       ((4)  #t)
       ((8)  (if #f #f))
       ((9)  (inferior-object 'undefined bits))
       ((10) (eof-object))
       ((11) (inferior-object 'unbound bits))))))

;;; Local Variables:
;;; eval: (put 'match-scm 'scheme-indent-function 1)
;;; eval: (put 'match-cell 'scheme-indent-function 1)
;;; eval: (put 'visited 'scheme-indent-function 1)
;;; End:

;;; types.scm ends here
