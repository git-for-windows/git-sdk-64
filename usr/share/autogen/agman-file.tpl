[+: -*- Mode: nroff -*-

  AutoGen5 template man

## agman-file.tpl -- Template for file man pages
##
##  This file is part of AutoOpts, a companion to AutoGen.
##  AutoOpts is free software.
##  Copyright (C) 1992-2018 Bruce Korb - all rights reserved
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

# Produce a man page for section 5 - configuration file formats.
#
:+][+:
(out-push-new)  :+]
if test -z "${MAN_PAGE_DATE}"
then LC_ALL=C date '+%d %b %Y' | sed 's/  */ /g'
else echo "${MAN_PAGE_DATE}"
fi
[+:
(define mpage-date (shell (out-pop #t)))

(define head-line (lambda()
        (sprintf ".TH %s %s \"%s\" \"%s\" \"%s\"\n.\\\"\n"
                (get "prog-name") man-sect
                mpage-date package-text section-name) ))

(define man-page #t)

:+][+:

INCLUDE "cmd-doc.tlib"

:+]
.\"
.SH NAME
[+: prog-name :+] \- [+: prog-title :+] configuration file
[+:

(define command-doc #f)
(out-push-new)            :+][+:

INVOKE build-doc          :+][+:

  (shell (string-append
    "fn='" (find-file "mdoc2man") "'\n"
    "test -f ${fn} || die mdoc2man not found from $PWD\n"
    "${fn} <<\\_EndOfMdoc_ || die ${fn} failed in $PWD\n"
    (out-pop #t)
    "\n_EndOfMdoc_" ))

:+][+:

(out-move (string-append (get "prog-name") "."
          man-sect))      :+][+: #

.\" = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
.\"  S Y N O P S I S
.\" = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = :+][+:

DEFINE mk-synopsis                          :+][+:
  (out-push-new file-name)                 \:+]
.SH SYNOPSIS
.B [+: file-path :+]
.PP [+:

(if (exist? "explain")
    (string-append "\n.PP\n"
      (join "\n.PP\n" (stack "explain"))) ) :+][+:

(out-pop)                                   :+][+:

ENDDEF mk-synopsis

agman-file.tpl ends here   :+]
