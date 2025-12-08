;;; Dynamically linking foreign libraries via dlopen and dlsym
;;; Copyright (C) 2021 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Implementation of dynamic-link.
;;;
;;; Code:


(define-module (system foreign-library)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module ((ice-9 ftw) #:select (scandir))
  #:use-module ((srfi srfi-1) #:select (find))
  #:export (guile-extensions-path
            ltdl-library-path
            guile-system-extensions-path

            lib->cyg lib->msys
            load-foreign-library
            foreign-library?
            foreign-library-pointer
            foreign-library-function))

(define-record-type <foreign-library>
  (make-foreign-library filename handle)
  foreign-library?
  (filename foreign-library-filename)
  (handle foreign-library-handle set-foreign-library-handle!))

(eval-when (expand load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_system_foreign_library"))

(define system-library-extensions
  (cond
   ((string-contains %host-type "-darwin")
    '(".bundle" ".so" ".dylib"))
   ((or (string-contains %host-type "cygwin")
        (string-contains %host-type "mingw"))
    '(".dll"))
   (else
    '(".so"))))

(define (has-extension? head exts)
  (match exts
    (() #f)
    ((ext . exts)
     (or (string-contains head ext)
         (has-extension? head exts)))))

(define (is-integer-string? str)
  (and
   (not (string-null? str))
   (let ((n (string-length str)))
     (let lp ((i 0))
       (let ((c (string-ref str i)))
         (if (or (char<? c #\0) (char>? c #\9))
             #f
             (if (= i (1- n))
                 #t
                 (lp (1+ i)))))))))

(define (dll-name-match? src tgt)
  (let ((srclen (string-length src))
        (tgtlen (string-length tgt)))
    (cond
     ((or (< srclen tgtlen)
          (< tgtlen 5)
          (string-ci<> src ".dll" (- srclen 4))
          (string-ci<> tgt ".dll" (- tgtlen 4))
          (string-ci<> src tgt 0 (- tgtlen 4) 0 (- tgtlen 4)))
      #f)
     (else
      (let ((mid (substring src (- tgtlen 4) (- srclen 4))))
        (cond
         ((or (string-null? mid)
              (and (char=? (string-ref mid 0) #\-)
                   (is-integer-string? (string-drop mid 1))))
          #t)
         (else
          #f)))))))

(define (find-best-dll-from-matches dllname lst)
  ;; A DLL name without a version suffix is preferred,
  ;; like libfoo.dll. But if we must have a version
  ;; suffix as in libfoo-5.dll, we want the largest one.
  (define (ver>? a b)
    (cond
     ((> (string-length a) (string-length b))
      #t)
     ((< (string-length a) (string-length b))
      #f)
     (else
      (string-ci>? a b))))

  (cond
   ((null? lst)
    #f)
   ((= (length lst) 1)
    (car lst))
   (else
    (or
     (find (lambda (entry) (string-ci= entry dllname)) lst)
     ;; The longest string that is alphabetically last
     ;; is numerically the highest.
     (car (sort lst ver>?))))))

(define (dll-exists-with-version head)
  ;; Searches for a DLL given a filepath, allowing
  ;; for DLLs with version suffixes.
  (let* ((fname (if (has-extension? head '(".dll"))
                    head
                    (string-append head ".dll")))
         (dir (dirname fname))
         (base (basename fname)))
    (let ((matches (scandir dir (lambda (f) (dll-name-match? f base)))))
      (if (or (not matches) (null? matches))
          #f
          (in-vicinity dir (find-best-dll-from-matches fname matches))))))

(define (file-exists-with-extension head extensions versioned-dlls?)
  (if (has-extension? head extensions)
      (and (file-exists? head) head)
      (let lp ((exts extensions))
        (match exts
          (() (if (and versioned-dlls? (member ".dll" extensions))
                  (dll-exists-with-version head)
                  #f))
          ((ext . exts)
           (let ((head (string-append head ext)))
             (if (file-exists? head)
                 head
                 (lp exts))))))))

(define (file-exists-in-path-with-extension basename path exts versioned-dlls?)
  (match path
    (() #f)
    ((dir . path)
     (or (file-exists-with-extension (in-vicinity dir basename) exts versioned-dlls?)
         (file-exists-in-path-with-extension basename path exts versioned-dlls?)))))

(define path-separator
  (case (system-file-name-convention)
    ((posix) #\:)
    ((windows) #\;)
    (else (error "unreachable"))))

(define (parse-path var)
  (match (getenv var)
    (#f #f)
    ;; Ignore e.g. "export GUILE_SYSTEM_EXTENSIONS_PATH=".
    ("" '())
    (val (string-split val path-separator))))

(define guile-extensions-path
  (make-parameter
   (or (parse-path "GUILE_EXTENSIONS_PATH") '())))

(define ltdl-library-path
  (make-parameter
   (or (parse-path "LTDL_LIBRARY_PATH") '())))

(define guile-system-extensions-path
  (make-parameter
   (or (parse-path "GUILE_SYSTEM_EXTENSIONS_PATH")
       (list (if (or (string-contains %host-type "cygwin")
                     (string-contains %host-type "mingw"))
                 (assq-ref %guile-build-info 'bindir)
                 (assq-ref %guile-build-info 'libdir))
             (assq-ref %guile-build-info 'extensiondir)))))

(define system-dll-path
  (make-parameter
   (or (parse-path "PATH") '())))

;; There are a few messy situations here related to libtool.
;;
;; Guile used to use libltdl, the dynamic library loader provided by
;; libtool.  This loader used LTDL_LIBRARY_PATH, and for backwards
;; compatibility we still support that path.
;;
;; However, libltdl would not only open ".so" (or ".dll", etc) files,
;; but also the ".la" files created by libtool.  In installed libraries
;; -- libraries that are in the target directories of "make install" --
;; .la files are never needed, to the extent that most GNU/Linux
;; distributions remove them entirely.  It is sufficient to just load
;; the ".so" (or ".dll", etc) files.
;;
;; But for uninstalled dynamic libraries, like those in a build tree, it
;; is a bit of a mess.  If you have a project that uses libtool to build
;; libraries -- which is the case for Guile, and for most projects using
;; autotools -- and you build foo.so in directory D, libtool will put
;; foo.la in D, but foo.so goes in D/.libs.
;;
;; The nice thing about ltdl was that it could load the .la file, even
;; from a build tree, preventing the existence of ".libs" from leaking
;; out to the user.
;;
;; We don't use libltdl now, essentially for flexibility and
;; error-reporting reasons.  But, it would be nice to keep this old
;; use-case working.  So as a stopgap solution, we add a ".libs" subdir
;; to the path for each entry in LTDL_LIBRARY_PATH, in case the .so is
;; there instead of alongside the .la file.
(define (augment-ltdl-library-path path)
  (match path
    (() '())
    ((dir . path)
     (cons* dir (in-vicinity dir ".libs")
            (augment-ltdl-library-path path)))))


(define (default-search-path search-ltdl-library-path?)
  (append
   (guile-extensions-path)
   (if search-ltdl-library-path?
       (augment-ltdl-library-path (ltdl-library-path))
       '())
   (guile-system-extensions-path)))

(define (lib->cyg name)
  "Convert a standard shared library name to a Cygwin shared library
name."
  (if (not name)
      #f
      (let ((start (1+ (or (string-index-right
                            name
                            (lambda (c) (or (char=? #\\ c) (char=? #\/ c))))
                           -1))))
        (cond
         ((>= (+ 3 start) (string-length name))
          name)
         ((string= name "lib" start (+ start 3))
          (string-append (substring name 0 start)
                         "cyg"
                         (substring name (+ start 3))))
         (else
          name)))))

(define (lib->msys name)
  "Convert a standard shared library name to a MSYS shared library
name."
  (if (not name)
      #f
      (let ((start (1+ (or (string-index-right
                            name
                            (lambda (c) (or (char=? #\\ c) (char=? #\/ c))))
                           -1))))
        (cond
         ((>= (+ 3 start) (string-length name))
          name)
         ((string= name "lib" start (+ start 3))
          (string-append (substring name 0 start)
                         "msys-"
                         (substring name (+ start 3))))
         (else
          name)))))

(define* (load-foreign-library #:optional filename #:key
                               (extensions system-library-extensions)
                               (search-ltdl-library-path? #t)
                               (search-path (default-search-path
                                              search-ltdl-library-path?))
                               (search-system-paths? #t)
                               (lazy? #t) (global? #f) (host-type-rename? #t)
                               (allow-dll-version-suffix? #t))
  (define (error-not-found)
    (scm-error 'misc-error "load-foreign-library"
               "file: ~S, message: ~S"
               (list filename "file not found")
               #f))
  (define flags
    (logior (if lazy? RTLD_LAZY RTLD_NOW)
            (if global? RTLD_GLOBAL RTLD_LOCAL)))
  (define (dlopen* name)
    (dlopen name flags))
  (define (file-exists-with-ext filename extensions)
    (file-exists-with-extension filename extensions allow-dll-version-suffix?))
  (define (file-exists-in-path-with-ext filename search-path extensions)
    (file-exists-in-path-with-extension
     filename search-path extensions allow-dll-version-suffix?))
  (when host-type-rename?
    (let ((sys (vector-ref (uname) 0)))
      (cond
       ((string-contains-ci sys "CYGWIN_NT")
        (set! filename (lib->cyg filename)))
       ((string-contains-ci sys "MSYS_NT")
        (set! filename (lib->msys filename))))))
  (make-foreign-library
   filename
   (cond
    ((not filename)
     ;; The self-open trick.
     (dlopen* #f))
    ((or (absolute-file-name? filename)
         (string-any file-name-separator? filename))
     (cond
      ((or (file-exists-with-ext filename extensions)
           (and search-ltdl-library-path?
                (file-exists-with-ext
                 (in-vicinity (in-vicinity (dirname filename) ".libs")
                              (basename filename))
                 extensions)))
       => dlopen*)
      (else
       (error-not-found))))
    ((file-exists-in-path-with-ext filename search-path extensions)
     => dlopen*)
    ((and search-system-paths?
          (or (string-contains %host-type "cygwin")
              (string-contains %host-type "mingw")))
     (let ((fullname (file-exists-in-path-with-ext filename (system-dll-path) '(".dll"))))
       (if fullname
           (dlopen* fullname)
           (error-not-found))))

    (search-system-paths?
     (if (or (null? extensions) (has-extension? filename extensions))
         (dlopen* filename)
         (let lp ((extensions extensions))
           (match extensions
             ((extension)
              ;; Open in tail position to propagate any exception.
              (dlopen* (string-append filename extension)))
             ((extension . extensions)
              ;; If there is more than one extension, unfortunately we
              ;; only report the error for the last extension.  This is
              ;; not great because maybe the library was found with the
              ;; first extension, failed to load and had an interesting
              ;; error, but then we swallowed that interesting error and
              ;; proceeded, eventually throwing a "file not found"
              ;; exception.  FIXME to use more structured exceptions and
              ;; stop if the error that we get is more specific than
              ;; just "file not found".
              (or (false-if-exception
                   (dlopen* (string-append filename extension)))
                  (lp extensions)))))))
    (else
     (error-not-found)))))

(define (->foreign-library lib)
  (if (foreign-library? lib)
      lib
      (load-foreign-library lib)))

(define* (foreign-library-pointer lib name)
  (let ((handle (foreign-library-handle (->foreign-library lib))))
    (dlsym handle name)))

(define* (foreign-library-function lib name
                                   #:key
                                   (return-type void)
                                   (arg-types '())
                                   (return-errno? #f))
  (let ((pointer (foreign-library-pointer lib name)))
    (pointer->procedure return-type pointer arg-types
                        #:return-errno? return-errno?)))
