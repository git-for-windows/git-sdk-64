;;;; ftw.scm --- file system tree walk

;;;; 	Copyright (C) 2002, 2003, 2006, 2011, 2012, 2014, 2016, 2018 Free Software Foundation, Inc.
;;;;
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

;;; Author: Thien-Thi Nguyen <ttn@gnu.org>

;;; Commentary:

;; Two procedures are provided: `ftw' and `nftw'.

;; NOTE: The following description was adapted from the GNU libc info page, w/
;; significant modifications for a more "Schemey" interface.  Most noticible
;; are the inlining of `struct FTW *' parameters `base' and `level' and the
;; omission of `descriptors' parameters.

;; * Types
;;
;;    The X/Open specification defines two procedures to process whole
;; hierarchies of directories and the contained files.  Both procedures
;; of this `ftw' family take as one of the arguments a callback procedure
;; which must be of these types.
;;
;;  - Data Type: __ftw_proc_t
;;           (lambda (filename statinfo flag) ...) => status
;;
;;      Type for callback procedures given to the `ftw' procedure.  The
;;      first parameter is a filename, the second parameter is the
;;      vector value as returned by calling `stat' on FILENAME.
;;
;;      The last parameter is a symbol giving more information about
;;      FILENAM.  It can have one of the following values:
;;
;;     `regular'
;;           The current item is a normal file or files which do not fit
;;           into one of the following categories.  This means
;;           especially special files, sockets etc.
;;
;;     `directory'
;;           The current item is a directory.
;;
;;     `invalid-stat'
;;           The `stat' call to fill the object pointed to by the second
;;           parameter failed and so the information is invalid.
;;
;;     `directory-not-readable'
;;           The item is a directory which cannot be read.
;;
;;     `symlink'
;;           The item is a symbolic link.  Since symbolic links are
;;           normally followed seeing this value in a `ftw' callback
;;           procedure means the referenced file does not exist.  The
;;           situation for `nftw' is different.
;;
;;  - Data Type: __nftw_proc_t
;;           (lambda (filename statinfo flag base level) ...) => status
;;
;;      The first three arguments have the same as for the
;;      `__ftw_proc_t' type.  A difference is that for the third
;;      argument some additional values are defined to allow finer
;;      differentiation:
;;
;;     `directory-processed'
;;           The current item is a directory and all subdirectories have
;;           already been visited and reported.  This flag is returned
;;           instead of `directory' if the `depth' flag is given to
;;           `nftw' (see below).
;;
;;     `stale-symlink'
;;           The current item is a stale symbolic link.  The file it
;;           points to does not exist.
;;
;;      The last two parameters are described below.  They contain
;;      information to help interpret FILENAME and give some information
;;      about current state of the traversal of the directory hierarchy.
;;
;;     `base'
;;           The value specifies which part of the filename argument
;;           given in the first parameter to the callback procedure is
;;           the name of the file.  The rest of the string is the path
;;           to locate the file.  This information is especially
;;           important if the `chdir' flag for `nftw' was set since then
;;           the current directory is the one the current item is found
;;           in.
;;
;;     `level'
;;           While processing the directory the procedures tracks how
;;           many directories have been examined to find the current
;;           item.  This nesting level is 0 for the item given starting
;;           item (file or directory) and is incremented by one for each
;;           entered directory.
;;
;; * Procedure: (ftw filename proc . options)
;;   Do a file system tree walk starting at FILENAME using PROC.
;;
;;   The `ftw' procedure calls the callback procedure given in the
;;   parameter PROC for every item which is found in the directory
;;   specified by FILENAME and all directories below.  The procedure
;;   follows symbolic links if necessary but does not process an item
;;   twice.  If FILENAME names no directory this item is the only
;;   object reported by calling the callback procedure.
;;
;;   The filename given to the callback procedure is constructed by
;;   taking the FILENAME parameter and appending the names of all
;;   passed directories and then the local file name.  So the
;;   callback procedure can use this parameter to access the file.
;;   Before the callback procedure is called `ftw' calls `stat' for
;;   this file and passes the information up to the callback
;;   procedure.  If this `stat' call was not successful the failure is
;;   indicated by setting the flag argument of the callback procedure
;;   to `invalid-stat'.  Otherwise the flag is set according to the
;;   description given in the description of `__ftw_proc_t' above.
;;
;;   The callback procedure is expected to return non-#f to indicate
;;   that no error occurred and the processing should be continued.
;;   If an error occurred in the callback procedure or the call to
;;   `ftw' shall return immediately the callback procedure can return
;;   #f.  This is the only correct way to stop the procedure.  The
;;   program must not use `throw' or similar techniques to continue
;;   the program in another place.  [Can we relax this? --ttn]
;;
;;   The return value of the `ftw' procedure is #t if all callback
;;   procedure calls returned #t and all actions performed by the
;;   `ftw' succeeded.  If some procedure call failed (other than
;;   calling `stat' on an item) the procedure returns #f.  If a
;;   callback procedure returns a value other than #t this value is
;;   returned as the return value of `ftw'.
;;
;; * Procedure: (nftw filename proc . control-flags)
;;   Do a new-style file system tree walk starting at FILENAME using PROC.
;;   Various optional CONTROL-FLAGS alter the default behavior.
;;
;;   The `nftw' procedures works like the `ftw' procedures.  It calls
;;   the callback procedure PROC for all items it finds in the
;;   directory FILENAME and below.
;;
;;   The differences are that for one the callback procedure is of a
;;   different type.  It takes also `base' and `level' parameters as
;;   described above.
;;
;;   The second difference is that `nftw' takes additional optional
;;   arguments which are zero or more of the following symbols:
;;
;;   physical'
;;        While traversing the directory symbolic links are not
;;        followed.  I.e., if this flag is given symbolic links are
;;        reported using the `symlink' value for the type parameter
;;        to the callback procedure.  Please note that if this flag is
;;        used the appearance of `symlink' in a callback procedure
;;        does not mean the referenced file does not exist.  To
;;        indicate this the extra value `stale-symlink' exists.
;;
;;   mount'
;;        The callback procedure is only called for items which are on
;;        the same mounted file system as the directory given as the
;;        FILENAME parameter to `nftw'.
;;
;;   chdir'
;;        If this flag is given the current working directory is
;;        changed to the directory containing the reported object
;;        before the callback procedure is called.
;;
;;   depth'
;;        If this option is given the procedure visits first all files
;;        and subdirectories before the callback procedure is called
;;        for the directory itself (depth-first processing).  This
;;        also means the type flag given to the callback procedure is
;;        `directory-processed' and not `directory'.
;;
;;   The return value is computed in the same way as for `ftw'.
;;   `nftw' returns #t if no failure occurred in `nftw' and all
;;   callback procedure call return values are also #t.  For internal
;;   errors such as memory problems the error `ftw-error' is thrown.
;;   If the return value of a callback invocation is not #t this
;;   very same value is returned.

;;; Code:

(define-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:autoload   (ice-9 i18n)   (string-locale<?)
  #:export (ftw nftw
            file-system-fold
            file-system-tree
            scandir))

(define-macro (getuid-or-false)
  (if (defined? 'getuid)
      '(getuid)
      #f))

(define-macro (getgid-or-false)
  (if (defined? 'getgid)
      '(getgid)
      #f))

(define (directory-files dir)
  (let ((dir-stream (opendir dir)))
    (let loop ((new (readdir dir-stream))
               (acc '()))
      (if (eof-object? new)
	  (begin
	    (closedir dir-stream)
	    acc)
          (loop (readdir dir-stream)
                (if (or (string=? "."  new)             ;;; ignore
                        (string=? ".." new))            ;;; ignore
                    acc
                    (cons new acc)))))))

(define (pathify . nodes)
  (let loop ((nodes nodes)
             (result ""))
    (if (null? nodes)
        (or (and (string=? "" result) "")
            (substring result 1 (string-length result)))
        (loop (cdr nodes) (string-append result "/" (car nodes))))))

;; `visited?-proc' returns a test procedure VISITED? which when called as
;; (VISITED? stat-obj) returns #f the first time a distinct file is seen,
;; then #t on any subsequent sighting of it.
;;
;; stat:dev and stat:ino together uniquely identify a file (see "Attribute
;; Meanings" in the glibc manual).  Often there'll be just one dev, and
;; usually there's just a handful mounted, so the strategy here is a small
;; hash table indexed by dev, containing hash tables indexed by ino.
;;
;; On some file systems, stat:ino is always zero.  In that case,
;; a string hash of the full file name is used.
;;
;; It'd be possible to make a pair (dev . ino) and use that as the key to a
;; single hash table.  It'd use an extra pair for every file visited, but
;; might be a little faster if it meant less scheme code.
;;
(define (visited?-proc size)
  (let ((dev-hash (make-hash-table 7)))
    (lambda (s name)
      (and s
	   (let* ((ino-hash (hashv-ref dev-hash (stat:dev s)))
                  (%ino     (stat:ino s))
                  (ino      (if (= 0 %ino)
                                (string-hash name)
                                %ino)))
	     (or ino-hash
		 (begin
		   (set! ino-hash (make-hash-table size))
		   (hashv-set! dev-hash (stat:dev s) ino-hash)))
	     (or (hashv-ref ino-hash ino)
		 (begin
		   (hashv-set! ino-hash ino #t)
		   #f)))))))

(define (stat-dir-readable?-proc uid gid)
  (lambda (s)
    (let* ((perms (stat:perms s))
           (perms-bit-set? (lambda (mask)
                             (logtest mask perms))))
      (or (equal? uid 0)
          (and (equal? uid (stat:uid s))
               (perms-bit-set? #o400))
          (and (equal? gid (stat:gid s))
               (perms-bit-set? #o040))
          (perms-bit-set? #o004)))))

(define (stat&flag-proc dir-readable? . control-flags)
  (let* ((directory-flag (if (memq 'depth control-flags)
                             'directory-processed
                             'directory))
         (stale-symlink-flag (if (memq 'nftw-style control-flags)
                                 'stale-symlink
                                 'symlink))
         (physical? (memq 'physical control-flags))
         (easy-flag (lambda (s)
                      (let ((type (stat:type s)))
                        (if (eq? 'directory type)
                            (if (dir-readable? s)
                                directory-flag
                                'directory-not-readable)
                            'regular)))))
    (lambda (name)
      (let ((s (false-if-exception (lstat name))))
        (cond ((not s)
               (values s 'invalid-stat))
              ((eq? 'symlink (stat:type s))
               (let ((s-follow (false-if-exception (stat name))))
                 (cond ((not s-follow)
                        (values s stale-symlink-flag))
                       ((and s-follow physical?)
                        (values s 'symlink))
                       ((and s-follow (not physical?))
                        (values s-follow (easy-flag s-follow))))))
              (else (values s (easy-flag s))))))))

(define (clean name)
  (let ((end (- (string-length name) 1)))
    (if (and (positive? end) (char=? #\/ (string-ref name end)))
      (substring name 0 end)
      name)))

(define (ftw filename proc . options)
  (let* ((visited? (visited?-proc (cond ((memq 'hash-size options) => cadr)
                                        (else 211))))
         (stat&flag (stat&flag-proc
                     (stat-dir-readable?-proc (getuid-or-false)
                                              (getgid-or-false)))))
    (letrec ((go (lambda (fullname)
                   (call-with-values (lambda () (stat&flag fullname))
                     (lambda (s flag)
                       (or (visited? s fullname)
                           (let ((ret (proc fullname s flag))) ; callback
                             (or (eq? #t ret)
                                 (throw 'ftw-early-exit ret))
                             (and (eq? 'directory flag)
                                  (for-each
                                   (lambda (child)
                                     (go (pathify fullname child)))
                                   (directory-files fullname)))
                             #t)))))))
      (catch 'ftw-early-exit
             (lambda () (go (clean filename)))
             (lambda (key val) val)))))

(define (nftw filename proc . control-flags)
  (let* ((od (getcwd))                  ; orig dir
         (odev (let ((s (false-if-exception (lstat filename))))
                 (if s (stat:dev s) -1)))
         (same-dev? (if (memq 'mount control-flags)
                        (lambda (s) (= (stat:dev s) odev))
                        (lambda (s) #t)))
         (base-sub (lambda (name base) (substring name 0 base)))
         (maybe-cd (if (memq 'chdir control-flags)
                       (if (absolute-file-name? filename)
                           (lambda (fullname base)
                             (or (= 0 base)
                                 (chdir (base-sub fullname base))))
                           (lambda (fullname base)
                             (chdir
                              (pathify od (base-sub fullname base)))))
                       (lambda (fullname base) #t)))
         (maybe-cd-back (if (memq 'chdir control-flags)
                            (lambda () (chdir od))
                            (lambda () #t)))
         (depth-first? (memq 'depth control-flags))
         (visited? (visited?-proc
                    (cond ((memq 'hash-size control-flags) => cadr)
                          (else 211))))
         (has-kids? (if depth-first?
                        (lambda (flag) (eq? flag 'directory-processed))
                        (lambda (flag) (eq? flag 'directory))))
         (stat&flag (apply stat&flag-proc
                           (stat-dir-readable?-proc (getuid-or-false)
                                                    (getgid-or-false))
                           (cons 'nftw-style control-flags))))
    (letrec ((go (lambda (fullname base level)
                   (call-with-values (lambda () (stat&flag fullname))
                     (lambda (s flag)
                       (letrec ((self (lambda ()
                                        (maybe-cd fullname base)
                                        ;; the callback
                                        (let ((ret (proc fullname s flag
                                                         base level)))
                                          (maybe-cd-back)
                                          (or (eq? #t ret)
                                              (throw 'nftw-early-exit ret)))))
                                (kids (lambda ()
                                        (and (has-kids? flag)
                                             (for-each
                                              (lambda (child)
                                                (go (pathify fullname child)
                                                    (1+ (string-length
                                                         fullname))
                                                    (1+ level)))
                                              (directory-files fullname))))))
                         (or (visited? s fullname)
                             (not (same-dev? s))
                             (if depth-first?
                                 (begin (kids) (self))
                                 (begin (self) (kids)))))))
                   #t)))
      (let ((ret (catch 'nftw-early-exit
                        (lambda () (go (clean filename) 0 0))
                        (lambda (key val) val))))
        (chdir od)
        ret))))


;;;
;;; `file-system-fold' & co.
;;;

(define-syntax-rule (errno-if-exception expr)
  (catch 'system-error
    (lambda ()
      expr)
    (lambda args
      (system-error-errno args))))

(define* (file-system-fold enter? leaf down up skip error init file-name
                           #:optional (stat lstat))
  "Traverse the directory at FILE-NAME, recursively.  Enter
sub-directories only when (ENTER? PATH STAT RESULT) returns true.  When
a sub-directory is entered, call (DOWN PATH STAT RESULT), where PATH is
the path of the sub-directory and STAT the result of (stat PATH); when
it is left, call (UP PATH STAT RESULT).  For each file in a directory,
call (LEAF PATH STAT RESULT).  When ENTER? returns false, call (SKIP
PATH STAT RESULT).  When an `opendir' or STAT call raises an exception,
call (ERROR PATH STAT ERRNO RESULT), with ERRNO being the operating
system error number that was raised.

Return the result of these successive applications.
When FILE-NAME names a flat file, (LEAF PATH STAT INIT) is returned.
The optional STAT parameter defaults to `lstat'."

  ;; Use drive and inode number as a hash key.  If the filesystem
  ;; doesn't use inodes, fall back to a string hash.
  (define (mark v s fname)
    (vhash-cons (cons (stat:dev s)
                      (if (= 0 (stat:ino s))
                          (string-hash fname)
                          (stat:ino s)))
                #t v))

  (define (visited? v s fname)
    (vhash-assoc (cons (stat:dev s)
                       (if (= 0 (stat:ino s))
                           (string-hash fname)
                           (stat:ino s)))
                 v))

  (let loop ((name     file-name)
             (path     "")
             (dir-stat (errno-if-exception (stat file-name)))
             (result   init)
             (visited  vlist-null))

    (define full-name
      (if (string=? path "")
          name
          (string-append path "/" name)))

    (cond
     ((integer? dir-stat)
      ;; FILE-NAME is not readable.
      (error full-name #f dir-stat result))
     ((visited? visited dir-stat full-name)
      (values result visited))
     ((eq? 'directory (stat:type dir-stat)) ; true except perhaps the 1st time
      (if (enter? full-name dir-stat result)
          (let ((dir     (errno-if-exception (opendir full-name)))
                (visited (mark visited dir-stat full-name)))
            (cond
             ((directory-stream? dir)
              (let liip ((entry   (readdir dir))
                         (result  (down full-name dir-stat result))
                         (subdirs '()))
                (cond ((eof-object? entry)
                       (begin
                         (closedir dir)
                         (let ((r+v
                                (fold (lambda (subdir result+visited)
                                        (call-with-values
                                            (lambda ()
                                              (loop (car subdir)
                                                    full-name
                                                    (cdr subdir)
                                                    (car result+visited)
                                                    (cdr result+visited)))
                                          cons))
                                      (cons result visited)
                                      subdirs)))
                           (values (up full-name dir-stat (car r+v))
                                   (cdr r+v)))))
                      ((or (string=? entry ".")
                           (string=? entry ".."))
                       (liip (readdir dir)
                             result
                             subdirs))
                      (else
                       (let* ((child (string-append full-name "/" entry))
                              (st    (errno-if-exception (stat child))))
                         (if (integer? st) ; CHILD is a dangling symlink?
                             (liip (readdir dir)
                                   (error child #f st result)
                                   subdirs)
                             (if (eq? (stat:type st) 'directory)
                                 (liip (readdir dir)
                                       result
                                       (alist-cons entry st subdirs))
                                 (liip (readdir dir)
                                       (leaf child st result)
                                       subdirs))))))))
             (else
              ;; Directory FULL-NAME not readable, but it is stat'able.
              (values (error full-name dir-stat dir result)
                      visited))))
          (values (skip full-name dir-stat result)
                  (mark visited dir-stat full-name))))
     (else
      ;; Caller passed a FILE-NAME that names a flat file, not a directory.
      (leaf full-name dir-stat result)))))

(define* (file-system-tree file-name
                           #:optional (enter? (lambda (n s) #t))
                                      (stat lstat))
  "Return a tree of the form (FILE-NAME STAT CHILDREN ...) where STAT is
the result of (STAT FILE-NAME) and CHILDREN are similar structures for
each file contained in FILE-NAME when it designates a directory.  The
optional ENTER? predicate is invoked as (ENTER? NAME STAT) and should
return true to allow recursion into directory NAME; the default value is
a procedure that always returns #t.  When a directory does not match
ENTER?, it nonetheless appears in the resulting tree, only with zero
children.  The optional STAT parameter defaults to `lstat'.  Return #f
when FILE-NAME is not readable."
  (define (enter?* name stat result)
    (enter? name stat))
  (define (leaf name stat result)
    (match result
      (((siblings ...) rest ...)
       (cons (alist-cons (basename name) (cons stat '()) siblings)
             rest))))
  (define (down name stat result)
    (cons '() result))
  (define (up name stat result)
    (match result
      (((children ...) (siblings ...) rest ...)
       (cons (alist-cons (basename name) (cons stat children)
                         siblings)
             rest))))
  (define skip                   ; keep an entry for skipped directories
    leaf)
  (define (error name stat errno result)
    (if (string=? name file-name)
        result
        (leaf name stat result)))

  (match (file-system-fold enter?* leaf down up skip error '(())
                           file-name stat)
    (((tree)) tree)
    ((())     #f)))                            ; FILE-NAME is unreadable

(define* (scandir name #:optional (select? (const #t))
                                  (entry<? string-locale<?))
  "Return the list of the names of files contained in directory NAME
that match predicate SELECT? (by default, all files.)  The returned list
of file names is sorted according to ENTRY<?, which defaults to
`string-locale<?'.  Return #f when NAME is unreadable or is not a
directory."

  ;; This procedure is implemented in terms of 'readdir' instead of
  ;; 'file-system-fold' to avoid the extra 'stat' call that the latter
  ;; makes for each entry.

  (define (opendir* directory)
    (catch 'system-error
      (lambda ()
        (opendir directory))
      (const #f)))

  (and=> (opendir* name)
         (lambda (stream)
           (let loop ((entry  (readdir stream))
                      (files   '()))
             (if (eof-object? entry)
                 (begin
                   (closedir stream)
                   (sort files entry<?))
                 (loop (readdir stream)
                       (if (select? entry)
                           (cons entry files)
                           files)))))))

;;; ftw.scm ends here
