# UnFilled.pm: handle unfilled line of text.
#
# Copyright 2010, 2011, 2017 Free Software Foundation, Inc.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License,
# or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

package Texinfo::Convert::UnFilled;

use 5.006;
use strict;

# initialize a paragraph object.
sub new($;$)
{
  my $class = shift;
  my $conf = shift;

  if (!$conf) {
    $conf = {};
  }

  $conf->{'max'} = 10000000;
  $conf->{'ignore_columns'} = 1;
  $conf->{'keep_end_lines'} = 1;

  $conf->{'frenchspacing'} = 1;
  $conf->{'unfilled'} = 1;
  $conf->{'no_final_newline'} = 1;

  my $paragraph = Texinfo::Convert::Paragraph->new($conf);

  return $paragraph;
}

1;
