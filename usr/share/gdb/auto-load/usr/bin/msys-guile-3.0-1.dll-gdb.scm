;;; GDB debugging support for Guile.
;;;
;;; Copyright 2014, 2015, 2017, 2020 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guile-gdb)
  #:use-module (system base types)

  ;; Note: (system vm debug) is 2.2-specific, but GDB might be built
  ;; with Guile 2.0.
  #:autoload   (system vm debug) (debug-context-from-image
                                  debug-context-base
                                  find-program-debug-info)

  #:use-module ((gdb) #:hide (symbol? frame?))
  #:use-module ((gdb) #:select ((symbol? . gdb:symbol?) (frame? . gdb:frame?)))
  #:use-module (gdb printing)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-41)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:export (%gdb-memory-backend
            display-vm-frames))

;;; Commentary:
;;;
;;; This file defines GDB extensions to pretty-print 'SCM' objects, and
;;; to walk Guile's virtual machine stack.
;;;
;;; This file is installed under a name that follows the convention that
;;; allows GDB to auto-load it anytime the user is debugging libguile
;;; (info "(gdb) objfile-gdbdotext file").
;;;
;;; Code:

;; At run time, make sure we load (system base types) from the Guile
;; being debugged rather than from the Guile GDB is linked against.
(set! %load-path
  (cons "/usr/share/guile/3.0" %load-path))
(set! %load-compiled-path
  (cons "/usr/lib/guile/3.0/site-ccache" %load-compiled-path))
(reload-module (resolve-module '(system base types)))


(define (type-name-from-descriptor descriptor-array type-number)
  "Return the name of the type TYPE-NUMBER as seen in DESCRIPTOR-ARRAY, or #f
if the information is not available."
  (let ((descriptors (lookup-global-symbol descriptor-array)))
    (and descriptors
         (let ((code (type-code (symbol-type descriptors))))
           (or (= TYPE_CODE_ARRAY code)
               (= TYPE_CODE_PTR code)))
         (let* ((type-descr (value-subscript (symbol-value descriptors)
                                             type-number))
                (name       (value-field type-descr "name")))
           (value->string name)))))

(define %gdb-memory-backend
  ;; The GDB back-end to access the inferior's memory.

  ;; When run through 'rr replay', even the 'void' type is initially
  ;; unavailable.  Thus, delay lookup until it's actually needed.
  (let ((void* (delay (type-pointer (lookup-type "void")))))
    (define (dereference-word address)
      ;; Return the word at ADDRESS.
      (value->integer
       (value-dereference (value-cast (make-value address)
                                      (type-pointer (force void*))))))

    (define (open address size)
      ;; Return a port to the SIZE bytes starting at ADDRESS.
      (if size
          (open-memory #:start address #:size size)
          (open-memory #:start address)))

    (define (type-name kind number)
      ;; Return the type name of KIND type NUMBER.
      (type-name-from-descriptor (case kind
                                   ((smob) "scm_smobs")
                                   ((port) "scm_ptobs"))
                                 number))

    (memory-backend dereference-word open type-name)))


;;;
;;; GDB pretty-printer registration.
;;;

(define scm-value->string
  (lambda* (value #:optional (backend %gdb-memory-backend))
    "Return a representation of value VALUE as a string."
    (object->string (scm->object (value->integer value) backend))))

(define (make-scm-pretty-printer-worker obj)
  (define (list->iterator list)
    (make-iterator list list
                   (let ((n 0))
                     (lambda (iter)
                       (match (iterator-progress iter)
                         (() (end-of-iteration))
                         ((elt . list)
                          (set-iterator-progress! iter list)
                          (let ((name (format #f "[~a]" n)))
                            (set! n (1+ n))
                            (cons name (object->string elt)))))))))
  (cond
   ((string? obj)
    (make-pretty-printer-worker
     "string" ; display hint
     (lambda (printer) obj)
     #f))
   ((and (array? obj)
         (match (array-shape obj)
           (((0 _)) #t)
           (_ #f)))
    (make-pretty-printer-worker
     "array" ; display hint
     (lambda (printer)
       (let ((tag (array-type obj)))
         (case tag
           ((#t) "#<vector>")
           ((b) "#<bitvector>")
           (else (format #f "#<~avector>" tag)))))
     (lambda (printer)
       (list->iterator (array->list obj)))))
   ((inferior-struct? obj)
    (make-pretty-printer-worker
     "array" ; display hint
     (lambda (printer)
       (format #f "#<struct ~a>" (inferior-struct-name obj)))
     (lambda (printer)
       (list->iterator (inferior-struct-fields obj)))))
   (else
    (make-pretty-printer-worker
     #f                                 ; display hint
     (lambda (printer)
       (object->string obj))
     #f))))

(define %scm-pretty-printer
  (make-pretty-printer
   "SCM"
   (lambda (pp value)
     (let ((name (type-name (value-type value))))
       (and (and name (string=? name "SCM"))
            (make-scm-pretty-printer-worker
             (scm->object (value->integer value) %gdb-memory-backend)))))))

(define* (register-pretty-printer #:optional objfile)
  (prepend-pretty-printer! objfile %scm-pretty-printer))

(register-pretty-printer)


;;;
;;; VM stack walking.
;;;

(define-record-type <vm-frame>
  (make-vm-frame ip sp fp saved-ip saved-fp)
  vm-frame?
  (ip vm-frame-ip)
  (sp vm-frame-sp)
  (fp vm-frame-fp)
  (saved-ip vm-frame-saved-ip)
  (saved-fp vm-frame-saved-fp))

;; See libguile/frames.h.
(define* (vm-frame ip sp fp #:optional (backend %gdb-memory-backend))
  "Return the components of the stack frame at FP."
  (define ip-type (type-pointer (lookup-type "uint32_t")))
  (define uint-type (type-pointer (lookup-type "uintptr_t")))

  (make-vm-frame ip
                 sp
                 fp

                 ;; fp[0] is the return address.
                 (value-dereference (value-cast fp (type-pointer ip-type)))

                 ;; fp[1] is the offset to the previous frame pointer.
                 (value-add fp
                            (value->integer
                             (value-dereference
                              (value-cast (value-add fp 1)
                                          (type-pointer uint-type)))))))

(define (vm-engine-frame? frame)
  (let ((sym (frame-function frame)))
    (and sym
         (member (symbol-name sym)
                 '("vm_debug_engine" "vm_regular_engine")))))

(define (find-vp)
  "Find the scm_vm pointer for the current thread."
  (match (lookup-symbol "scm_i_current_thread")
    ((#f _)
     #f)
    ((symbol _)
     (let ((thread (symbol-value symbol)))
       (value-field thread "vm")))))

(define (newest-vm-frame)
  "Return the newest VM frame or #f."
  (let ((vp (find-vp)))
    (and vp
         (vm-frame (value-field vp "ip")
                   (value-field vp "sp")
                   (value-field vp "fp")))))

(define* (vm-frame-older frame #:optional (backend %gdb-memory-backend))
  (let ((ip (vm-frame-saved-ip frame))
        (sp (value-sub (vm-frame-fp frame) 3))
        (fp (vm-frame-saved-fp frame)))
    (and (not (zero? (value->integer ip)))
         (vm-frame ip sp fp backend))))

(define (vm-frames)
  "Return a SRFI-41 stream of the current VM frame stack."
  (stream-unfold identity
                 vm-frame?
                 vm-frame-older
                 (newest-vm-frame)))

(define (vm-frame-locals frame)
  (let ((fp (vm-frame-fp frame))
        (sp (vm-frame-sp frame)))
    (let lp ((slot 0) (ptr fp))
      (if (value<=? ptr sp)
          (acons (string-append "v" (number->string slot))
                 (value-dereference ptr)
                 (lp (1+ slot) (value-add ptr 1)))
          '()))))

(define (lookup-symbol-or-false name)
  (match (lookup-symbol name)
    (#f #f)
    ((sym _) sym)))

(define (find-mapped-elf-image addr)
  (let ((array (lookup-symbol-or-false "mapped_elf_images"))
        (count (lookup-symbol-or-false "mapped_elf_images_count")))
    (and array count
         (let ((array (symbol-value array))
               (count (value->integer (symbol-value count))))
           (let lp ((start 0) (end count))
             (if (< start end)
                 (let ((n (+ start (ash (- end start) -1))))
                   (if (value<? addr (value-field (value-add array n) "end"))
                       (lp start n)
                       (lp (1+ n) end)))
                 (let ((mei (value-add array start)))
                   (and (value<=? (value-field mei "start") addr)
                        mei))))))))

(define (vm-frame-program-debug-info frame)
  (let ((addr (vm-frame-ip frame)))
    (and=> (find-mapped-elf-image addr)
           (lambda (mei)
             (let* ((start (value->integer (value-field mei "start")))
                    (size (- (value->integer (value-field mei "end"))
                             start))
                    (mem-port (open-memory #:start start #:size size))
                    (bv (get-bytevector-all mem-port))
                    (ctx (debug-context-from-image bv)))
               ;; The image is in this process at "bv", but in the
               ;; inferior at mei.start.  Therefore we relocate addr
               ;; before we look for the PDI.
               (let ((addr (+ (value->integer addr)
                              (- (debug-context-base ctx) start))))
                 (find-program-debug-info addr ctx)))))))

(define (vm-frame-function-name frame)
  (define (default-name)
    "[unknown]")
  (cond
   ((false-if-exception (vm-frame-program-debug-info frame))
    => (lambda (pdi)
         (or (and=> (program-debug-info-name pdi) symbol->string)
             "[anonymous]")))
   (else
    (let ((ip (vm-frame-ip frame)))
      (define (ip-in-symbol? name)
        (let ((sym (lookup-symbol-or-false name)))
          (and sym
               (not (value-optimized-out? (symbol-value sym)))
               (let* ((val (symbol-value sym))
                      (size (type-sizeof (value-type val)))
                      (char* (type-pointer (arch-char-type (current-arch))))
                      (val-as-char* (value-cast val char*)))
                 (and (value<=? val-as-char* ip)
                      (value<? ip (value-add val-as-char* size)))))))
      (cond
       ((ip-in-symbol? "vm_boot_continuation_code") "[boot continuation]")
       ;; FIXME: For subrs, read the name from slot 0 in the frame.
       ((ip-in-symbol? "subr_stub_code") "[subr call]")
       ((ip-in-symbol? "vm_builtin_apply_code") "apply")
       ((ip-in-symbol? "vm_builtin_values_code") "values")
       ((ip-in-symbol? "vm_builtin_abort_to_prompt_code") "abort-to-prompt")
       ((ip-in-symbol? "vm_builtin_call_with_values_code") "call-with-values")
       ((ip-in-symbol? "vm_builtin_call_with_current_continuation_code")
        "call-with-current-continuation")
       ((ip-in-symbol? "continuation_stub_code") "[continuation]")
       ((ip-in-symbol? "compose_continuation_code") "[delimited continuation]")
       ((ip-in-symbol? "foreign_stub_code") "[ffi call]")
       (else (default-name)))))))

(define (vm-frame-source frame)
  (let* ((ip (value->integer (vm-frame-ip frame)))
         (pdi (vm-frame-program-debug-info frame)))
    (and pdi
         (find-source-for-addr (program-debug-info-addr pdi)
                               (program-debug-info-context pdi)))))

(define* (dump-vm-frame frame #:optional (port (current-output-port)))
  (format port "  name: ~a~%" (vm-frame-function-name frame))
  (format port "  ip: 0x~x~%" (value->integer (vm-frame-ip frame)))
  (format port "  fp: 0x~x~%" (value->integer (vm-frame-fp frame)))
  (for-each (match-lambda
             ((name . val)
              (let ((obj (scm->object (value->integer val) %gdb-memory-backend)))
                (format port "    ~a: ~a~%" name obj))))
            (vm-frame-locals frame)))

(define* (display-vm-frames #:optional (port (current-output-port)))
  "Display the VM frames on PORT."
  (stream-for-each (lambda (frame)
                     (dump-vm-frame frame port))
                   (vm-frames)))

(register-command!
 (make-command "guile-backtrace"
               #:command-class COMMAND_STACK
               #:doc "Display a backtrace of Guile's VM stack for the \
current thread"
               #:invoke (lambda (self args from-tty)
                          (display-vm-frames))))


;;;
;;; Frame filters.
;;;

(define-syntax compile-time-cond
  (lambda (x)
    (syntax-case x ()
      ((_ (test body ...) clause ...)
       (if (eval (syntax->datum #'test) (current-module))
           #'(begin body ...)
           #'(compile-time-cond clause ...)))
      ((_)
       #'(begin)))))

(compile-time-cond
 ;; What follows depends on (gdb frame-filters), which unfortunately has
 ;; not yet been merged in GDB:
 ;; <https://sourceware.org/ml/gdb-patches/2015-02/msg00362.html>.
 ((false-if-exception (resolve-interface '(gdb frame-filters)))
  (use-modules (gdb frame-filters))

  (define (snarfy-frame-decorator dec)
    (let* ((frame (decorated-frame-frame dec))
           (sym (frame-function frame)))
      (or
       (and sym
            (gdb:symbol? sym)
            (let ((c-name (symbol-name sym)))
              (match (lookup-symbol (string-append "s_" c-name))
                (#f #f)
                ((scheme-name-sym _)
                 (and (string-prefix?
                       "const char ["
                       (type-print-name (symbol-type scheme-name-sym)))
                      (let* ((scheme-name-value (symbol-value scheme-name-sym))
                             (scheme-name (value->string scheme-name-value))
                             (name (format #f "~a [~a]" scheme-name c-name)))
                        (redecorate-frame dec #:function-name name)))))))
       dec)))

  (define* (vm-frame-filter gdb-frames #:optional (vm-frames (vm-frames)))
    (define (synthesize-frame gdb-frame vm-frame)
      (let* ((ip (value->integer (vm-frame-ip vm-frame)))
             (source (vm-frame-source vm-frame)))
        (redecorate-frame gdb-frame
                          #:function-name (vm-frame-function-name vm-frame)
                          #:address ip
                          #:filename (and=> source source-file)
                          #:line (and=> source source-line-for-user)
                          #:arguments '()
                          #:locals (vm-frame-locals vm-frame)
                          #:children '())))
    (define (recur gdb-frame gdb-frames vm-frames)
      (stream-cons gdb-frame
                   (vm-frame-filter gdb-frames vm-frames)))
    (cond
     ((or (stream-null? gdb-frames)
          (not (lookup-symbol "vm_boot_continuation_code")))
      gdb-frames)
     (else
      (let ((gdb-frame (stream-car gdb-frames))
            (gdb-frames (stream-cdr gdb-frames)))
        (match (lookup-symbol "vm_boot_continuation_code")
          ((boot-sym _)
           (let ((boot-ptr (symbol-value boot-sym)))
             (cond
              ((vm-engine-frame? (decorated-frame-frame gdb-frame))
               (let lp ((children (reverse
                                   (decorated-frame-children gdb-frame)))
                        (vm-frames vm-frames))
                 (define (finish reversed-children vm-frames)
                   (let ((children (reverse reversed-children)))
                     (recur (redecorate-frame gdb-frame #:children children)
                            gdb-frames
                            vm-frames)))
                 (cond
                  ((stream-null? vm-frames)
                   (finish children vm-frames))
                  (else
                   (let* ((vm-frame (stream-car vm-frames))
                          (vm-frames (stream-cdr vm-frames)))
                     (if (value=? (vm-frame-ip vm-frame) boot-ptr)
                         ;; Drop the boot frame and finish.
                         (finish children vm-frames)
                         (lp (cons (synthesize-frame gdb-frame vm-frame)
                                   children)
                             vm-frames)))))))
              (else
               (recur gdb-frame gdb-frames vm-frames))))))))))

  (add-frame-filter!
   (make-decorating-frame-filter "guile-snarf-decorator"
                                 snarfy-frame-decorator
                                 #:objfile (current-objfile)))
  (add-frame-filter!
   (make-frame-filter "guile-vm-frame-filter"
                      vm-frame-filter
                      #:objfile (current-objfile))))
 (#t #f))

;;; libguile-2.2-gdb.scm ends here
