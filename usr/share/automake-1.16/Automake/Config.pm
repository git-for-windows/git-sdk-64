#  -*- Perl -*-
# Copyright (C) 2003-2020 Free Software Foundation, Inc.
# Generated from Config.in; do not edit by hand.

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

package Automake::Config;
use strict;

use 5.006;
require Exporter;

our @ISA = qw (Exporter);
our @EXPORT = qw ($APIVERSION $PACKAGE $PACKAGE_BUGREPORT $VERSION
                  $RELEASE_YEAR $libdir $perl_threads);

# Parameters set by configure.  Not to be changed.  NOTE: assign
# VERSION as string so that e.g. version 0.30 will print correctly.
our $APIVERSION = '1.16';
our $PACKAGE = 'automake';
our $PACKAGE_BUGREPORT = 'bug-automake@gnu.org';
our $VERSION = '1.16.2';
our $RELEASE_YEAR = '2020';
our $libdir = $ENV{"AUTOMAKE_LIBDIR"} || '/usr/share/automake-1.16';

our $perl_threads = 0;
# We need at least this version for CLONE support.
if (eval { require 5.007_002; })
  {
    use Config;
    $perl_threads = $Config{useithreads};
  }

1;
