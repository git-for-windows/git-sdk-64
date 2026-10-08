.ig

Copyright 1991-2020 Free Software Foundation, Inc.

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
.\" Cover sheet. Memorandum type 5
.\"------------
.de cov@print-title
.if !d cov*title .@error title not defined; call TL and AU before MT
.nr cov*saved-adjustment \\n[.j]
.ad l
.ft \\*[@metadata-font]
.\" TODO: We might compute this from the horizontal page dimensions
.\" (W, O).  For historical reconstructions, this suffices because the
.\" AT&T mm product anticipated only U.S. letter paper.
.ie t .ll 3i
.el   .ll 26n
.cov*title
.br
.ll
.if d cov*title-charge-case \{\
.	nop Work Project No.\& \\*[cov*title-charge-case]
.\}
.br
.if d cov*title-file-case File Case \\*[cov*title-file-case]
.br
.ft
.ad \\n[cov*saved-adjustment]
.rr cov*saved-adjustment
..
.\"------------
.\" DWB 3.3 sets the date a little inboard of the right margin.  But it
.\" didn't have an `rj` request, and groff does, so we can flex a bit.
.de cov@print-date
.rj 1
.ft \\*[@metadata-font]
.nop \\*[cov*date]
.ft
..
.\"------------
.nr let*sg-suppress-annotation 1
.
.if !d cov*mt-printed \{\
.	SP |4
.	cov@print-title
.	ie \n(nlu<=13v .SP |14
.	el             .SP
.	cov@print-date
.	SP 2
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
