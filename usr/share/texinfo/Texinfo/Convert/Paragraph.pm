# Copyright 2014-2019 Free Software Foundation, Inc.
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

package Texinfo::Convert::Paragraph;

# same as texi2any.pl
use 5.00405;
use strict;
use warnings;

our $VERSION = '6.8';

use Texinfo::XSLoader;


# Import symbols into the module that is using this one.  We don't
# use the Exporter module because its symbols are lost when we override
# this module with Texinfo::XSLoader::init.
sub import {
  my @EXPORT = qw(
    add_text 
    add_next 
    set_space_protection 
    remove_end_sentence 
    allow_end_sentence 
    add_end_sentence 
    end_line 
    add_pending_word 
    get_pending 
  );

  my ($callpkg, $filename, $line) = caller(0);
  for my $sym (@EXPORT) {
    no strict 'refs';
    *{"${callpkg}::$sym"} = \&{"Texinfo::Convert::Paragraph::${sym}"};
  }
}

BEGIN {
  our $warning_message = undef;
  our $fatal_message = undef;

  # Save reference to subroutine before we do anything.
  my $import_fn = \&import;

  my $package = Texinfo::XSLoader::init (
    "Texinfo::Convert::Paragraph",
    "Texinfo::Convert::ParagraphNonXS",
    "XSParagraph",
    undef,
    0,
    $warning_message,
    $fatal_message
  );

  no strict 'refs';
  *{"${package}::import"} = $import_fn;
}


# NB Don't add more functions down here, because this can cause an error
# with some versions of Perl, connected with any typeglob assignments done
# above.  ("Can't call mro_method_changed_in() on anonymous symbol table").
#
# See http://perl5.git.perl.org/perl.git/commitdiff/03d9f026ae253e9e69212a3cf6f1944437e9f070?hp=ac73ea1ec401df889d312b067f78b618f7ffecc3
#
# (change to Perl interpreter on 22 Oct 2011)


1;
__END__
