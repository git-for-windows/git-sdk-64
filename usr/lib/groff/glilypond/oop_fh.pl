my $License = q*
########################################################################
# Legalese
########################################################################

Source file position: '<groff-source>/contrib/glilypond/oop_fh.pl'
Installed position: '<prefix>/lib/groff/glilypond/oop_fh.pl'

Copyright (C) 2013-2013 Free Software Foundation, Inc.
  Written by Bernd Warken <groff-bernd.warken-72@web.de>

This file is part of 'glilypond', which is part of 'GNU groff'.

glilypond - integrate 'lilypond' into 'groff' files

  'GNU groff' is free software: you can redistribute it and/or modify it
under the terms of the 'GNU General Public License' as published by the
'Free Software Foundation', either version 3 of the License, or (at your
option) any later version.

  'GNU groff' is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 'GNU
General Public License' for more details.

  You should have received a copy of the 'GNU General Public License'
along with 'groff', see the files 'COPYING' and 'LICENSE' in the top
directory of the 'groff' source package.  If not, see
<http://www.gnu.org/licenses/>.
*;

##### end legalese


# use strict;
# use warnings;
# use diagnostics;

use integer;

########################################################################
# OOP for writing file handles that are open by default, like STD*
########################################################################

# -------------------------- _FH_WRITE_OPENED --------------------------

{	# FH_OPENED: base class for all opened file handles, like $TD*

  package _FH_WRITE_OPENED;
  use strict;

  sub new {
    my ( $pkg, $std ) = @_;
    bless {
	   'fh' => $std,
	  }
  }

  sub open {
  }

  sub close {
  }

  sub print {
    my $self = shift;
    for ( @_ ) {
      print { $self->{'fh'} } $_;
    }
  }

}


# ------------------------------ FH_STDOUT ----------------------------

{			     # FH_STDOUT: print to noral output STDOUT

  package FH_STDOUT;
  use strict;
  @FH_STDOUT::ISA = qw( _FH_WRITE_OPENED );

  sub new {
    &_FH_WRITE_OPENED::new( '_FH_WRITE_OPENED', *STDOUT );
  }

}				# end FH_STDOUT


# ------------------------------ FH_STDERR -----------------------------

{				# FH_STDERR: print to STDERR

  package FH_STDERR;
  use strict;
  @FH_STDERR::ISA = qw( _FH_WRITE_OPENED );

  sub new {
    &_FH_WRITE_OPENED::new( 'FH_OPENED', *STDERR );
  }

}				# end FH_STDERR


########################################################################
# OOP for file handles that write into a file or string
########################################################################

# ------------------------------- FH_FILE ------------------------------

{	       # FH_FILE: base class for writing into a file or string

  package FH_FILE;
  use strict;

  sub new {
    my ( $pkg, $file ) = @_;
    bless {
	   'fh' => undef,
	   'file' => $file,
	   'opened' => main::FALSE,
	  }
  }

  sub DESTROY {
    my $self = shift;
    $self->close();
  }

  sub open {
    my $self = shift;
    my $file = $self->{'file'};
    if ( $file && -e $file ) {
      die "file $file is not writable" unless ( -w $file );
      die "$file is a directory" if ( -d $file );
    }
    open $self->{'fh'}, ">", $self->{'file'}
      or die "could not open file '$file' for writing: $!";
    $self->{'opened'} = main::TRUE;
  }

  sub close {
    my $self = shift;
    close $self->{'fh'} if ( $self->{'opened'} );
    $self->{'opened'} = main::FALSE;
  }

  sub print {
    my $self = shift;
    $self->open() unless ( $self->{'opened'} );
    for ( @_ ) {
      print { $self->{'fh'} } $_;
    }
  }

}				# end FH_FILE


# ------------------------------ FH_STRING -----------------------------

{				# FH_STRING: write into a string

  package FH_STRING;		# write to \string
  use strict;
  @FH_STRING::ISA = qw( FH_FILE );

  sub new {
    my $pkg = shift;		# string is a reference to scalar
    bless
      {
       'fh' => undef,
       'string' => '',
       'opened' => main::FALSE,
      }
    }

  sub open {
    my $self = shift;
    open $self->{'fh'}, ">", \ $self->{'string'}
      or die "could not open string for writing: $!";
    $self->{'opened'} = main::TRUE;
  }

  sub get { # get string, move to array ref, close, and return array ref
    my $self = shift;
    return '' unless ( $self->{'opened'} );
    my $a = &string2array( $self->{'string'} );
    $self->close();
    return $a;
  }

}				# end FH_STRING


# -------------------------------- FH_NULL -----------------------------

{				# FH_NULL: write to null device

  package FH_NULL;
  use strict;
  @FH_NULL::ISA = qw( FH_FILE FH_STRING );

  use File::Spec;

  my $devnull = File::Spec->devnull();
  $devnull = '' unless ( -e $devnull && -w $devnull );

  sub new {
    my $pkg = shift;
    if ( $devnull ) {
      &FH_FILE::new( $pkg, $devnull );
    } else {
      &FH_STRING::new( $pkg );
    }
  } # end new()

}				# end FH_NULL


########################################################################
# OOP for reading file handles
########################################################################

# ---------------------------- FH_READ_FILE ----------------------------

{ # FH_READ_FILE: read a file

  package FH_READ_FILE;
  use strict;

  sub new {
    my ( $pkg, $file ) = @_;
    die "File '$file' cannot be read." unless ( -f $file && -r $file );
    bless {
	   'fh' => undef,
	   'file' => $file,
	   'opened' => main::FALSE,
	  }
  }

  sub DESTROY {
    my $self = shift;
    $self->close();
  }

  sub open {
    my $self = shift;
    my $file = $self->{'file'};
    if ( $file && -e $file ) {
      die "file $file is not writable" unless ( -r $file );
      die "$file is a directory" if ( -d $file );
    }
    open $self->{'fh'}, "<", $self->{'file'}
      or die "could not read file '$file': $!";
    $self->{'opened'} = main::TRUE;
  }

  sub close {
    my $self = shift;
    close $self->{'fh'} if ( $self->{'opened'} );
    $self->{'opened'} = main::FALSE;
  }

  sub read_line {
    # Read 1 line of the file into a chomped string.
    # Do not close the read handle at the end.
    my $self = shift;
    $self->open() unless ( $self->{'opened'} );

    my $res;
    if ( defined($res = CORE::readline($self->{'fh'}) ) ) {
      chomp $res;
      return $res;
    } else {
      $self->close();
      return undef;
    }
  }

  sub read_all {
    # Read the complete file into an array reference.
    # Close the read handle at the end.
    # Return array reference.
    my $self = shift;
    $self->open() unless ( $self->{'opened'} );

    my $res = [];
    my $line;
    while ( defined ( $line = CORE::readline $self->{'fh'} ) ) {
      chomp $line;
      push @$res, $line;
    }
    $self->close();
    $self->{'opened'} = main::FALSE;
    return $res;
  }

}

# end of OOP definitions

package main;

1;
########################################################################
### Emacs settings
# Local Variables:
# mode: CPerl
# End:
