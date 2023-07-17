.ig

Copyright (C) 1991-2020 Free Software Foundation, Inc.
mm is written by Jörgen Hägg <jh@axis.com>

mm is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or
(at your option) any later version.

mm is distributed in the hope that it will be useful, but WITHOUT ANY
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
.B
.ll 9c
.fi
.cov*title
.R
.ll
.nf
.if d cov*title-charge-case \fBCharge Case \\*[cov*title-charge-case]\fP
.if d cov*title-file-case \fBFile Case \\*[cov*title-file-case]\fP
.fi
..
.\"------------
.de cov@print-date
.rj 1
\f[\\*[@sdf_font]]\\*[cov*new-date]\fP
.br
..
.\"------------
.if !d cov*mt-printed \{\
.	SP 1.9c
.	cov@print-title 
.	SP 1.2c
.	cov@print-date 
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
