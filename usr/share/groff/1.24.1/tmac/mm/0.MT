.ig

Copyright 1991-2025 Free Software Foundation, Inc.

mm for groff is written by Jörgen Hägg <jh@axis.com>

Modified by G. Branden Robinson <g.branden.robinson@gmail.com> to more
closely approximate DWB 3.3 troff output, and to make nroff output
flexible.

This file is part of mm for groff.

mm is distributed with groff, the GNU roff typesetting system.

groff is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or
(at your option) any later version.

groff is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Please submit bug reports using groff's 'BUG-REPORT' file to
http://savannah.gnu.org/bugs/?group=groff.
..
.
.\"------------
.\" cover sheet for memorandum types 0-3 and 6 ("string")
.\"------------
.if !r Au .nr Au 1
.\" TODO: We might compute these from the page dimensions (L, W, O).
.\" For historical reconstructions, these suffice because the AT&T mm
.\" product anticipated only U.S. letter paper.
.nr cov*top1 1.1i \" horizontal rule above firm name
.nr cov*top2 1.8i \" cover sheet field data
.ie t \{\
.	nr cov*column2-hpos   0.4i \" title, charging case, filing case
.	nr cov*column2-limit 4i
.	nr cov*column3-hpos   4.3i \" "date", "from"
.	nr cov*column4-hpos   4.65i \" date, author names, TM number
.	nr cov*user-width     0
.	if r W .nr cov*user-width \n[W]u
.	nr cov*column4-limit (6i>?\n[cov*user-width]u)
.\}
.el \{\
.	nr cov*column2-hpos   9n
.	nr cov*column2-limit 37n
.	nr cov*column3-hpos  39n
.	nr cov*column4-hpos  45n
.	nr cov*user-width     0
.	if r W .nr cov*user-width \n[W]u
.	nr cov*column4-limit (60n>?\n[cov*user-width]u)
.\}
.\"------------
.de cov@print-title
.\" "subject:" is indented flush left; the title and any charging or
.\" file numbers share another indentation.
.if !d cov*title .@error title not defined; call TL and AU before MT
.SP |\\n[cov*top2]u
.S 8
.nop \\$1:
.S
.ft \\*[@metadata-font]
.nr cov*saved-adjustment \\n[.j]
.ad l
.\" Cause break to force the indentation to "take", then back up.
.in \\n[cov*column2-hpos]u
.ll \\n[cov*column2-limit]u
.sp -1
.cov*title
.ft
.br
.if d cov*title-charge-case \{\
.	B
.	nop Work Project No.\& \\*[cov*title-charge-case]
.	R
.\}
.br
.if d cov*title-file-case \{\
.	B
.	nop File Case \\*[cov*title-file-case]
.	R
.\}
.ad \\n[cov*saved-adjustment]
.rr cov*saved-adjustment
.ll
.in
..
.\"------------
.\" mm changed what `AU` fields in printed at the top of memoranda over
.\" time, from PWB to DWB 3.3 at least.  groff mm defaults to formatting
.\" all that are present, but supports an `Aumt` string enabling their
.\" selective suppression by position.
.nr cov*i 0 1
.while \n+[cov*i]<=9 \{\
.	nr cov*fmt-au!\n[cov*i] 1
.\}
.rr cov*i
.
.de cov*suppress-au-fields
.while \\n[.$] \{\
.	ie \B'\\$1' .nr cov*fmt-au!\\$1 0
.	el .tm 0.MT:\\$0: non-numeric 'Aumt' argument '\\$1'
.	shift
.\}
..
.
.if d Aumt .cov*suppress-au-fields \*[Aumt]
.
.de cov@print-authors
.\" The following diagnostic might be unreachable.
.if !r cov*au .@error no authors defined; call AU before MT
.rt \\n[cov*authors-vpos]
.\" The authors always start 2 vees below the date and have 1 vee
.\" between them in DWB 3.3.
.SP
.\" The material formatted here consists of proper names that could be
.\" from any linguistic background, and character sequences that may
.\" already be abbreviations, so suspend automatic hyphenation.
.nr cov*saved-hyphenation-mode \n[.hy]
.nh
.nr cov*saved-indentation \\n[.i]
'in \\n[cov*column3-hpos]u
.S 8
\\$1:
.S
.\" Cause break to force the indentation to "take", then back up.
.in \\n[cov*column4-hpos]u
.ll \\n[cov*column4-limit]u
.sp -1
.ft \\*[@metadata-font]
.\" DWB 3.3 mm turned off filling, but that runs a big risk of
.\" oversetting the line in nroff mode.
.nr cov*saved-adjustment \\n[.j]
.ad l
.nr cov*i 0 1
.while \\n+[cov*i]<=\\n[cov*au] \{\
.	nr cov*au-line-counter 0
.	cov@print-au1 \\n[cov*i] 1
.	if \\n[Au] \{\
.		if \\n[cov*fmt-au!4] .cov@print-au1 \\n[cov*i] 4
.		ie \\n[cov*fmt-au!3]&\\n[cov*fmt-au!6] \{\
.			cov@print-au2 \\n[cov*i] 3 6
.		\}
.		el \{\
.			if \\n[cov*fmt-au!3] .cov@print-au1 \\n[cov*i] 3
.			if \\n[cov*fmt-au!6] .cov@print-au1 \\n[cov*i] 6
.		\}
.		if \\n[cov*fmt-au!5] .cov@print-au1 \\n[cov*i] 5
.		if \\n[cov*fmt-au!6] .cov@print-au1 \\n[cov*i] 7
.		if \\n[cov*fmt-au!7] .cov@print-au1 \\n[cov*i] 8
.		if \\n[cov*fmt-au!8] .cov@print-au1 \\n[cov*i] 9
.	\}
.	if (\\n[cov*i]<\\n[cov*au])&(\\n[cov*au-line-counter]>1) .SP 1
.\}
.rr cov*au-line-counter
.rr cov*i
.ft
.if r cov*mt-tm-max \{\
.	SP 1
.	nr cov*i 0 1
.	ft \\*[@metadata-font]
.	while \\n+[cov*i]<=\\n[cov*mt-tm-max] \\*[cov*mt-tm!\\n[cov*i]]
.	ft
.\}
.ad \\n[cov*saved-adjustment]
.rr cov*saved-adjustment
.in \\n[cov*saved-indentation]
.rr cov*saved-indentation
.ll
.hy \\n[cov*saved-hyphenation-mode]
.rr cov*saved-hyphenation-mode
..
.\"------------
.\" cov@print-au1 m n
.\" Format nth argument to `AU` call of mth author.
.de cov@print-au1
.if d cov*au!\\$1!\\$2 \{\
.	ds cov*prefix \" empty
.	if '\\$2'4' .as cov*prefix Org.\& \"
.	if '\\$2'5' .as cov*prefix x\"
.	nop \\*[cov*prefix]\\*[cov*au!\\$1!\\$2]
.	nr cov*au-line-counter +1
.	rm cov*prefix
.\}
.br
..
.\"------------
.\" cov@print-au2 m n q
.\" Format nth and qth arguments to `AU` call of mth author.
.de cov@print-au2
.if d cov*au!\\$1!\\$2 \\*[cov*au!\\$1!\\$2] \c
.if d cov*au!\\$1!\\$3 \\*[cov*au!\\$1!\\$3]
.nr cov*au-line-counter +1
.br
..
.\"------------
.de cov@print-date
.\" We must use `sp` instead of `SP` because this motion is negative.
.sp |\\n[cov*top2]u
.nr cov*saved-indentation \\n[.i]
'in \\n[cov*column3-hpos]u
.S 8
\\$1:
.S
.\" Cause break to force the indentation to "take", then back up.
.in \\n[cov*column4-hpos]u
.ll \\n[cov*column4-limit]u
.sp -1
.ft \\*[@metadata-font]
.nop \\*[cov*date]
.ft
.fi
.ll
.mk cov*authors-vpos
.in \\n[cov*saved-indentation]
.rr cov*saved-indentation
..
.\"------------
.de cov@print-firm
.if !d cov*firm .return
.\" We must use `sp` instead of `SP` because this motion is negative.
.sp |\\n[cov*top1]u
.nr cov*saved-adjustment \\n[.j]
.ad l
.\" Use the em dash to draw the rule in nroff mode, since terminal
.\" devices generally don't have a baseline rule glyph.  The "low line"
.\" descends below the baseline and terminal fonts generally don't have
.\" it span the character cell completely.  (A Unicode font might do so
.\" for the em dash; we make a best-effort attempt to look pleasant.)
.ie t .nop \l'\\n[.l]u'
.el   .nop \l'\\n[.l]u\[em]'
.\" DWB 3.3 mm set the firm name in bold on nroff devices, and
.\" Helvetica roman on troff devices.  Register `E` had no effect on it.
.ie t .if F HR .fam H
.el   .B
.nop \\*[cov*firm]
.ie t .if F HR .fam
.el   .R
.ad \\n[cov*saved-adjustment]
.rr cov*saved-adjustment
..
.\"------------
.de cov@print-abstract
.SP 3
.if d cov*abstract \{\
.	misc@ev-keep cov*ev
.	if \\n[cov*abstract-indent]>0 \{\
.		in +\\n[cov*abstract-indent]u
.		ll -\\n[cov*abstract-indent]u
.	\}
.	ce
.	I
.	nop \\$1
.	R
.	SP 1.5
.	fi
.	cov*abstract
.	br
.	ev
.\}
..
.\"-----------------
.ds cov*mt0-txt!1 TECHNICAL MEMORANDUM\"
.ds cov*mt0-txt!2 INTERNAL MEMORANDUM\"
.ds cov*mt0-txt!3 ADMINISTRATIVE MEMORANDUM\"
.if d cov*default-firm \
.	if !d cov*firm .ds cov*firm \\*[cov*default-firm]\"
.\"
.\" Note: The firm name (if any) is formatted _above_ the title and
.\" other data, but the `AF` macro call defining it might be stuffed
.\" inside the `TL` macro content, so its string won't be visible until
.\" we format the title.  That in turn means that we _must_ call
.\" `cov@print-title` before `cov@print-firm`.  This is weird but
.\" DWB-compatible.
.\"
.\" (Technical details: DWB mm stores the title in a diversion.  groff
.\" mm stores it in a macro, and only calling `AU` ends the macro
.\" definition.)
.if !d cov*mt-printed \{\
.	cov@print-title subject
.	ie d AFX \{\
.		if \n[D]>=5 .tm calling user-defined AFX macro
.		AFX
.	\}
.	el       .cov@print-firm
.	cov@print-date date
.	cov@print-authors from
.	cov@print-abstract \\*[Abstract]
.	SP 3
.	if (\*[cov*mt-type]>=1)&(\*[cov*mt-type]<=3) \{\
.		ce
.		I
.		nop \*[cov*mt0-txt!\*[cov*mt-type]]
.		R
.	\}
.	if \*[cov*mt-type]=6 \{\
.		ce
.		I
.		nop \*[cov*mt-type-text]
.		R
.	\}
.	SP 3
.	pg@enable-top-trap
.	pg@enable-trap
.	ds cov*mt-printed
.\}
.
.\" Local Variables:
.\" mode: nroff
.\" fill-column: 72
.\" End:
.\" vim: set filetype=groff textwidth=72:
