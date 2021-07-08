#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:

# Interface to Encode.
# Copyright (C) 2002-2017 Guido Flohr <guido.flohr@cantanea.com>,
# all rights reserved.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

package Locale::RecodeData::_Encode;

use strict;
use integer;

use Encode;

require Locale::RecodeData;
use base qw (Locale::RecodeData);

sub _recode
{
	use bytes;

	my $retval;
	
	if ($_[0]->{_from} eq 'INTERNAL') {
		$_[1] = pack "N*", @{$_[1]};
		$retval = Encode::from_to ($_[1], 'UTF-32BE', $_[0]->{_to});
	} elsif ($_[0]->{_to} eq 'INTERNAL') {
		$retval = Encode::from_to ($_[1], $_[0]->{_from}, 'UTF-32BE');
		return unless defined $retval;
		$_[1] = [ unpack "N*", $_[1] ];
	} else {
		$retval = Encode::from_to ($_[1], $_[0]->{_from}, $_[0]->{_to});
	}
	
	return unless defined $retval;
	return 1;
}

1;

__END__

=head1 NAME

Locale::RecodeData::_Encode - Internal wrapper around Encode 

=head1 SYNOPSIS

use Locale::RecodeData::_Encode;

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module converts text with the help of Encode(3).  It is 
tried first for conversions if libintl-perl detects the presence
of Encode.

=head1 AUTHOR

Copyright (C) 2002-2017 L<Guido Flohr|http://www.guido-flohr.net/>
(L<mailto:guido.flohr@cantanea.com>), all rights reserved.  See the source
code for details!code for details!

=head1 SEE ALSO

Locale::Recode(3), Encode(3), perl(1)

=cut
Local Variables:
mode: perl
perl-indent-level: 4
perl-continued-statement-offset: 4
perl-continued-brace-offset: 0
perl-brace-offset: -4
perl-brace-imaginary-offset: 0
perl-label-offset: -4
cperl-indent-level: 4
cperl-continued-statement-offset: 2
tab-width: 4
End:
=cut
