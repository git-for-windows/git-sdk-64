# Copyright (C) 2001-2025 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

package Automake::Version;

use 5.006; use strict; use warnings;

use Automake::ChannelDefs;

=head1 NAME

Automake::Version - version comparison

=head1 SYNOPSIS

  use Automake::Version;

  print "Version $version is older than required version $required\n"
    if Automake::Version::check ($version, $required);

=head1 DESCRIPTION

This module provides support for comparing versions string
as they are used in Automake.

A version is a string that looks like
C<MAJOR.MINOR[.MICRO][ALPHA][-FORK]> where C<MAJOR>, C<MINOR>, and
C<MICRO> are digits, C<ALPHA> is either a character or another C<.NUM>,
and C<FORK> any alphanumeric word.

Usually, C<ALPHA> is used to label alpha releases or intermediate
snapshots, C<FORK> is used for git branches or patched releases, and
C<MICRO> is used for bug fix releases on the C<MAJOR.MINOR> branch.

For the purpose of ordering, C<1.4> is the same as C<1.4.0>, but
C<1.4g> is the same as C<1.4.99g>.  The C<FORK> identifier is ignored
in the ordering, except when it looks like C<-pMINOR[ALPHA]>: some
versions were labeled like C<1.4-p3a>, this is the same as an alpha
release labeled C<1.4.3a>.  Yes, it's horrible, but Automake did not
support two-dot versions in the past.

In 2024, Automake switched from using letter suffixes like C<1.4g> for
pretests to the C<.90> form (used by nearly all other GNU packages),
e.g., C<1.16.90> was a pretest leading up to C<1.17>. Thus we also have
to support four-part version numbers, since test releases leading up to
C<1.17.1> have to be C<1.17.0.92>, etc., to follow the pattern. In this
case, all four version parts have to be present and all-numeric; the
C<-FORK> is still optional (but entirely ignored).

Aggravatingly, version number syntax is also recognized in
lib/Automake/Options.pm, since bare version numbers are also valid
Automake options, as tested in C<version6[.sh]>.

=head2 FUNCTIONS

=over 4

=item C<split ($version)>

Split the string C<$version> into the corresponding C<(MAJOR, MINOR,
MICRO, ALPHA, FORK)> tuple.  For instance C<'1.4g'> would be split
into C<(1, 4, 99, 'g', '')> and C<'1.17.0.91'> into C<1, 17, 0, 91, ''>.
Return C<()> on error.

=cut

sub split ($)
{
  my ($ver) = @_;

  # Recognize MAJOR.MINOR, plus special case for versions like 1.4-p2a.
  if ($ver =~ /^(\d+)\.(\d+)(?:-p(\d+)([a-z]+)?)$/)
  {
    return ($1, $2, $3, $4 || '', '');
  }
  # Recognize MAJOR.MINOR and MAJOR.MINOR.MICRO, as well as
  # the pre-2024 case with possible letters in the alpha part.
  elsif ($ver =~ /^(\d+)\.(\d+)(?:\.(\d+))?([a-z])?(?:-([A-Za-z0-9]+))?$/)
  {
    return ($1, $2, $3 || (defined $4 ? 99 : 0), $4 || '', $5 || '');
  }
  # 2024ff. case with all numbers: MAJOR.MINOR.MICRO.ALPHA[-FORK].
  elsif ($ver =~ /^(\d+)\.(\d+).(\d+)\.(\d+)(?:-([A-Za-z0-9]+))?$/)
  {
    return ($1, $2, $3, $4, $5 || '');
  }
  return ();
}

=item C<compare (\@LVERSION, \@RVERSION)>

Compare two version tuples, as returned by C<split>.

Return 1, 0, or -1, if C<LVERSION> is found to be respectively
greater than, equal to, or less than C<RVERSION>.

=cut

sub compare (\@\@)
{
  my @l = @{$_[0]};
  my @r = @{$_[1]};

  for my $i (0, 1, 2)
  {
    return 1  if ($l[$i] > $r[$i]);
    return -1 if ($l[$i] < $r[$i]);
  }
  for my $i (3, 4)
  {
    return 1  if ($l[$i] gt $r[$i]);
    return -1 if ($l[$i] lt $r[$i]);
  }
  return 0;
}

=item C<check($VERSION, $REQUIRED)>

Handles the logic of requiring a version number in Automake.
C<$VERSION> should be Automake's version, while C<$REQUIRED>
is the version required by the user input.

Return 0 if the required version is satisfied, 1 otherwise.

=cut

sub check ($$)
{
  my ($version, $required) = @_;
  my @version = Automake::Version::split ($version);
  my @required = Automake::Version::split ($required);

  prog_error "version is incorrect: $version"
    if $#version == -1;

  # This should not happen, because process_option_list and split_version
  # use similar regexes.
  prog_error "required version is incorrect: $required"
    if $#required == -1;

  # If we require 3.4n-foo then we require something
  # >= 3.4n, with the 'foo' fork identifier.
  return 1
    if ($required[4] ne '' && $required[4] ne $version[4]);

  return 0 > compare (@version, @required);
}

1;
