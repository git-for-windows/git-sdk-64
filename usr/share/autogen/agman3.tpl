{+ AutoGen5 template  -*- nroff -*-

null

## agman3.tpl -- Template for command line man pages
##
## Author:          Bruce Korb <bkorb@gnu.org>
##
##  This file is part of AutoOpts, a companion to AutoGen.
##  AutoOpts is free software.
##  AutoOpts is Copyright (C) 1992-2018 by Bruce Korb - all rights reserved
##
##  AutoOpts is available under any one of two licenses.  The license
##  in use must be one of these two and the choice is under the control
##  of the user of the license.
##
##   The GNU Lesser General Public License, version 3 or later
##      See the files "COPYING.lgplv3" and "COPYING.gplv3"
##
##   The Modified Berkeley Software Distribution License
##      See the file "COPYING.mbsd"
##
##  These files have the following sha256 sums:
##
##  8584710e9b04216a394078dc156b781d0b47e1729104d666658aecef8ee32e95  COPYING.gplv3
##  4379e7444a0e2ce2b12dd6f5a52a27a4d02d39d247901d3285c88cf0d37f477b  COPYING.lgplv3
##  13aa749a5b0a454917a944ed8fffc530b784f5ead522b1aacaf4ec8aa55a6239  COPYING.mbsd

+}{+

(define see-also "")
(if (exist? "see-also")
    (set! see-also (string-append (get "see-also") " ")) )

+}{+

FOR export-func     +}{+
  (if (not (exist? "private"))
      (set! see-also (string-append see-also
            (get "name") "(3) " ))  )
  +}{+

ENDFOR export-func  +}{+


FOR export-func                +}{+
  IF (not (exist? "private"))  +}{+

    (out-push-new (string-append
         (get "name") ".3" ))

+}.TH {+name+} 3 {+ `
  if test -z "${MAN_PAGE_DATE}"
  then date +%Y-%m-%d
  else echo "${MAN_PAGE_DATE}"; fi
` +} "" "Programmer's Manual"
{+

;; The following "dne" argument is a string of 5 characters:
;; '.' '\\' '"' and two spaces.  It _is_ hard to read. "
;;
(dne ".\\\"  ")

+}
.SH NAME
{+name+} - {+what+}
.sp 1
.SH SYNOPSIS
{+IF (exist? "header") +}
#include <\fI{+header+}\fP>
.br{+
  ENDIF+}
cc [...] -o outfile infile.c -l\fB{+library+}\fP [...]
.sp 1
{+ ?% ret-type "%s" void
+} \fB{+name+}\fP({+
  IF (not (exist? "arg")) +}void{+
  ELSE  +}{+
    FOR arg ", " +}{+arg-type+} \fI{+arg-name+}\fP{+
    ENDFOR arg +}{+
  ENDIF +});
.sp 1
.SH DESCRIPTION
{+
  INCLUDE "agman.tlib"
+}{+
(get "doc")    +}{+
  IF (exist? "arg") +}{+
    FOR arg         +}
.TP
.IR {+ arg-name +}
{+ arg-desc  +}{+

    ENDFOR  arg     +}{+
  ENDIF  arg exists +}{+

  IF (exist? "ret-type") +}
.sp 1
.SH RETURN VALUE
{+ret-desc+}{+

  ENDIF +}{+

  IF (exist? "err") +}
.sp 1
.SH ERRORS
{+ err +}{+

  ENDIF +}{+

  IF (exist? "example") +}
.sp 1
.SH EXAMPLES
.nf
.in +5
{+ example +}
.in -5
.fi{+

  ENDIF +}{+

emit-man-text

+}
.SH SEE ALSO
The \fIinfo\fP documentation for the -l\fI{+library+}\fP library.
.br
{+
(define tmp-txt (get "see"))
(if (> (string-length see-also) 0)
    (set! tmp-txt (string-append see-also ", " tmp-txt))  )

(shellf "echo '%s' | \
sed 's@%s(3) @@;s/3) $/3)/;s/(3) /(3), /g;s/, *,/,/g;s/^, *//'"
    tmp-txt (get "name")) +}
{+

    (out-pop)    +}{+

  ENDIF private  +}{+

ENDFOR  export-func


+}
