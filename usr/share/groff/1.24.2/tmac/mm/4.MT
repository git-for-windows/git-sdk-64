.ig

Copyright 1991-2025 Free Software Foundation, Inc.

mm for groff is written by Jörgen Hägg <jh@axis.com>

Modified by G. Branden Robinson <g.branden.robinson@gmail.com> to more
closely approximate DWB 3.3 troff output.

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
.\" Cover sheet. Memorandum type 4
.\"------------
.de cov@print-title
.if !d cov*title .@error title not defined; call TL and AU before MT
.\" TODO: We might compute this from the vertical page dimension (L).
.\" For historical reconstructions, this suffices because the AT&T mm
.\" product anticipated only U.S. letter paper.
.SP |(1.3333i-1v)
.S +2
.nr cov*saved-adjustment \\n[.j]
.ad c
.ft \\*[@metadata-font]
.cov*title
.br
.S
.ft
.ad \\n[cov*saved-adjustment]
.rr cov*saved-adjustment
..
.\"------------
.de cov@print-authors
.\" The following diagnostic might be unreachable.
.if !r cov*au .@error no authors defined; call AU before MT
.ie t .SP 0.5
.el   .SP
.I
.S +1
.nr cov*i 0 1
.while \\n+[cov*i]<=\\n[cov*au] \{\
.	ce
.	nop \\*[cov*au!\\n[cov*i]!1]
.\}
.S
.R
.ie t .SP 0.5
.el   .SP
..
.\"------------
.de cov@print-firm
.if d cov*firm \{\
.	SP 2
.	ce
.	nop \\*[cov*firm]
.\}
..
.\"------------
.de cov@print-abstract
.SP 2
.if d cov*abstract \{\
.	misc@ev-keep cov*ev
.	@reset
.	if \\n[cov*abstract-indent]>0 \{\
.		in +\\n[cov*abstract-indent]u
.		ll -\\n[cov*abstract-indent]u
.	\}
.	ce
.	I
.	nop \\*[Abstract]
.	R
.	SP 2
.	fi
.	cov*abstract
.	br
.	ev
.\}
..
.\"-----------------
.nr let*sg-suppress-all 1
.nr let*ns-suppress 1
.
.if d cov*default-firm .if !d cov*firm .ds cov*firm \\*[cov*default-firm]
.if !d cov*mt-printed \{\
.	cov@print-title
.	cov@print-authors
.	ie d AFX \{\
.		if \n[D]>=5 .tm calling user-defined AFX macro
.		AFX
.	\}
.	el       .cov@print-firm
.	if d cov*abstract \{\
.		if !\n[cov*abstract-placement] .cov@print-abstract
.	\}
.	SP 2
.	nr hd*cur-bline \n[nl]
.	ds cov*mt-printed
.	pg@enable-top-trap
.	pg@enable-trap
.\}
.de CS
.pg@disable-top-trap
.SK
.cov@print-title
.cov@print-authors
.cov@print-firm
.cov@print-abstract
..
.
.\" Local Variables:
.\" mode: nroff
.\" fill-column: 72
.\" End:
.\" vim: set filetype=groff textwidth=72:
