########################################################################
# Legalese
########################################################################

my $License = q*
groff_lilypond - integrate 'lilypond' into 'groff' files

Source file position: '<groff-source>/contrib/glilypond/args.pl'
Installed position: '<prefix>/lib/groff/glilypond'

Copyright (C) 2013-2018 Free Software Foundation, Inc.
  Written by Bernd Warken <groff-bernd.warken-72@web.de>

Last update: 10 Sep 2015

This file is part of 'GNU groff'.

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

our ( $Globals, $Args, $stderr, $v, $out );

# ----------
# subs for second run, for remaining long options after splitting and
# transfer
# ----------

my %opts_with_arg =
  (

   '--eps_dir' => sub {
     $Args->{'eps_dir'} = shift;
   },

   '--output' => sub {
     $Args->{'output'} = shift;
   },

   '--prefix' => sub {
     $Args->{'prefix'} = shift;
   },

   '--temp_dir' => sub {
     $Args->{'temp_dir'} = shift;
   },

  );				# end of %opts_with_arg


my %opts_noarg =
  (

   '--help' => sub {
     &usage;
     exit;
   },

   '--keep_all' => sub {
     $Args->{'keep_all'} = TRUE;
   },

   '--license' => sub {
     &license;
     exit;
   },

   '--ly2eps' => sub {
     $Args->{'eps_func'} = 'ly';
   },

   '--pdf2eps' => sub {
     $Args->{'eps_func'} = 'pdf';
   },

   '--verbose' => sub {
     $Args->{'verbose'} = TRUE;
   },

   '--version' => sub {
     &version;
     exit;
   },

  );				# end of %opts_noarg


# used variables in both runs

my @files = EMPTYARRAY;


#----------
# first run for command-line arguments
#----------

# global variables for first run

my @splitted_args;
my $double_minus = FALSE;
my $arg = EMPTYSTRING;
my $has_arg = FALSE;


# Split short option collections and transfer these to suitable long
# options from above.  Note that '-v' now means '--verbose' in version
# 'v1.1', earlier versions had '--version' for '-v'.

my %short_opts =
  (
   '?' => '--help',
   'e' => '--eps_dir',
   'h' => '--help',
   'l' => '--license',
   'k' => '--keep_all',
   'o' => '--output',
   'p' => '--prefix',
   't' => '--temp_dir',
   'v' => '--verbose',
   'V' => '--verbose',
  );


# transfer long option abbreviations to the long options from above

my @long_opts;

$long_opts[3] =
  {				# option abbreviations of 3 characters
   '--e' => '--eps_dir',
   '--f' => '--prefix',		# --f for --file_prefix
   '--h' => '--help',
   '--k' => '--keep_all',	# and --keep_files
   '--o' => '--output',
   '--p' => '--prefix',		# and --file_prefix
   '--t' => '--temp_dir',
   '--u' => '--help',		# '--usage' is mapped to '--help'
  };

$long_opts[4] =
  {				# option abbreviations of 4 characters
   '--li' => '--license',
   '--ly' => '--ly2eps',
   '--pd' => '--pdf2eps',
   '--pr' => '--prefix',
  };

$long_opts[6] =
  {				# option abbreviations of 6 characters
   '--verb' => '--verbose',
   '--vers' => '--version',
  };


# subs for short splitting and replacing long abbreviations

my $split_short = sub {

  my @chars = split //, $1;	# omit leading dash

     # if result is TRUE: run 'next SPLIT' afterwards

   CHARS: while ( @chars ) {
       my $c = shift @chars;

       unless ( exists $short_opts{$c} ) {
	 $stderr->print( "Unknown short option '-$c'." );
	 next CHARS;
       }

       # short option exists

       # map or transfer to special long option from above
       my $transopt = $short_opts{$c};

       if ( exists $opts_noarg{$transopt} ) {
	 push @splitted_args, $transopt;
	 $Args->{'verbose'}  = TRUE if ( $transopt eq '--verbose' );
	 next CHARS;
       }

       if ( exists $opts_with_arg{$transopt} ) {
	 push @splitted_args, $transopt;

	 if ( @chars ) {
	   # if @chars is not empty, option $transopt has argument
	   # in this arg, the rest of characters in @chars
	   push @splitted_args, join "", @chars;
	   @chars = EMPTYARRAY;
	   return TRUE;		# use 'next SPLIT' afterwards
	 }

	 # optarg is the next argument
	 $has_arg = $transopt;
	 return TRUE;		# use 'next SPLIT' afterwards
       }			# end of if %opts_with_arg
     }				# end of while CHARS
     return FALSE;		# do not do anything
};				# end of sub for short_opt_collection


my $split_long = sub {
  my $from_arg = shift;
  $from_arg =~ /^([^=]+)/;
  my $opt_part = lc($1);
  my $optarg = undef;
  if ( $from_arg =~ /=(.*)$/ ) {
    $optarg = $1;
  }

 N: for my $n ( qw/6 4 3/ ) {
    $opt_part =~ / # match $n characters
		   ^
		   (
		     .{$n}
		   )
		 /x;
    my $argn = $1;		# get the first $n characters

    # no match, so luck for fewer number of chars
    next N unless ( $argn );

    next N unless ( exists $long_opts[$n]->{$argn} );
    # not in $n hash, so go on to next loop for $n

    # now $n-hash has arg

    # map or transfer to special long opt from above
    my $transopt = $long_opts[$n]->{$argn};

    # test on option without arg
    if ( exists $opts_noarg{$transopt} ) { # opt has no arg
      $stderr->print( 'Option ' . $transopt . 'has no argument: ' .
		      $from_arg . '.' ) if ( defined($optarg) );
      push @splitted_args, $transopt;
      $Args->{'verbose'} = TRUE if ( $transopt eq '--verbose' );
      return TRUE;		# use 'next SPLIT' afterwards
    }				# end of if %opts_noarg

    # test on option with arg
    if ( exists $opts_with_arg{$transopt} ) { # opt has arg
      push @splitted_args, $transopt;

      # test on optarg in arg
      if ( defined($optarg) ) {
	push @splitted_args, $1;
	return TRUE; # use 'next SPLIT' afterwards
      } # end of if optarg in arg

      # has optarg in next arg
      $has_arg = $transopt;
      return TRUE; # use 'next SPLIT' afterwards
    } # end of if %opts_with_arg

    # not with and without option, so is not permitted
    $stderr->print( "'" . $transopt .
		    "' is unknown long option from '" . $from_arg . "'" );
    return TRUE; # use 'next SPLIT' afterwards
  } # end of for N
  return FALSE; # do nothing
}; # end of split_long()


#----------
# do split and transfer arguments
#----------
sub run_first {

 SPLIT: foreach (@ARGV) {
    # Transform long and short options into some given long options.
    # Split long opts with arg into 2 args (no '=').
    # Transform short option collections into given long options.
    chomp;

    if ( $has_arg ) {
      push @splitted_args, $_;
      $has_arg = EMPTYSTRING;
      next SPLIT;
    }

    if ( $double_minus ) {
      push @files, $_;
      next SPLIT;
    }

    if ( $_ eq '-' ) {		# file arg '-'
      push @files, $_;
      next SPLIT;
    }

    if ( $_ eq '--' ) {		# POSIX arg '--'
      push @splitted_args, $_;
      $double_minus = TRUE;
      next SPLIT;
    }

    if ( / # short option or collection of short options
	   ^
	   -
	   (
	     [^-]
	     .*
	   )
	   $
	 /x ) {
      $split_short->($1);
      next SPLIT;
    }				# end of short option

    if ( /^--/ ) {		# starts with 2 dashes, a long option
      $split_long->($_);
      next SPLIT;
    }				# end of long option

    # unknown option without leading dash is a file name
    push @files, $_;
    next SPLIT;
  }				# end of foreach SPLIT

				# all args are considered
  $stderr->print( "Option '$has_arg' needs an argument." )
    if ( $has_arg );


  push @files, '-' unless ( @files );
  @ARGV = @splitted_args;

};		    # end of first run, splitting with map or transfer


#----------
# open or ignore verbose output
#----------
sub install_verbose {
  if ( $Args->{'verbose'} ) { # '--verbose' was used
    # make verbose output into $v
    my $s = $v->get(); # get content of string so far as array ref, close

    $v = new FH_STDERR(); # make verbose output into STDERR
    if ( $s ) {
      for ( @$s ) {
	# print the file content into new verbose output
	$v->print($_);
      }
    }
    # verbose output is now active (into STDERR)
    $v->print( "Option '-v' means '--verbose'." );
    $v->print( "Version information is printed by option '--version'." );
    $v->print( "#" x 72 );

  } else { # '--verbose' was not used
    # do not be verbose, make verbose invisible

    $v->close(); # close and ignore the string content

    $v = new FH_NULL();
    # this is either into /dev/null or in an ignored string

  } # end if-else about verbose
  # '$v->print' works now in any case

  $v->print( "Verbose output was chosen." );

  my $s = $Globals->{'prog_is_installed'} ? '' : ' not';
  $v->print( $Globals->{'prog'} . " is" . $s .
	     " installed." );

  $v->print( 'The command-line options are:' );

  $s = "  options:";
  $s .= " '" . $_ . "'" for ( @ARGV );
  $v->print( $s );

  $s = "  file names:";
  $s .= " '" . $_ . "'\n" for ( @files );
  $v->print( $s );
} # end install_verbose()


#----------
# second run of command-line arguments
#----------
sub run_second {
    # Second run of args with new @ARGV from the former splitting.
    # Arguments are now splitted and transformed into special long options.

    my $double_minus = FALSE;
    my $has_arg = FALSE;

  ARGS: for my $arg ( @ARGV ) {

      # ignore '--', file names are handled later on
      last ARGS if ( $arg eq '--' );

      if ( $has_arg ) {
	unless ( exists $opts_with_arg{$has_arg} ) {
	  $stderr->print( "'\%opts_with_args' does not have key '" .
			    $has_arg . "'." );
	  next ARGS;
	}

	$opts_with_arg{$has_arg}->($arg);
	$has_arg = FALSE;
	next ARGS;
      } # end of $has_arg

      if ( exists $opts_with_arg{$arg} ) {
	$has_arg = $arg;
	next ARGS;
      }

      if ( exists $opts_noarg{$arg} ) {
	$opts_noarg{$arg}->();
	next ARGS;
      }

      # not a suitable option
      $stderr->print( "Wrong option '" . $arg . "'." );
      next ARGS;

    } # end of for ARGS:


    if ( $has_arg ) { # after last argument
      die "Option '$has_arg' needs an argument.";
    }

  }; # end of second run


sub handle_args {
  # handling the output of args

  if ( $Args->{'output'} ) { # '--output' was set in the arguments
    my $out_path = &path2abs($Args->{'output'});
    die "Output file name $Args->{'output'} cannot be used."
      unless ( $out_path );

    my ( $file, $dir );
    ( $file, $dir ) = File::Basename::fileparse($out_path)
      or die "Could not handle output file path '" . $out_path . "': " .
	"directory name '" . $dir . "' and file name '" . $file . "'.";

    die "Could not find output directory for '" . $Args->{'output'} . "'"
      unless ( $dir );
    die "Could not find output file: '" . $Args->{'output'} .
      "'" unless ( $file );

    if ( -d $dir ) {
      die "Could not write to output directory '" . $dir . "'."
	unless ( -w $dir );
    } else {
      $dir = &make_dir($dir);
      die "Could not create output directory in: '" . $out_path . "'."
	unless ( $dir );
    }

    # now $dir is a writable directory

    if ( -e $out_path ) {
      die "Could not write to output file '" . $out_path . "'."
	unless ( -w $out_path );
    }

    $out = new FH_FILE( $out_path );
    $v->print( "Output goes to file '" . $out_path . "'." );
  } else { # '--output' was not set
    $out = new FH_STDOUT();
  }
  # no $out is the right behavior for standard output

#  $Args->{'prefix'} .= '_' . $Args->{'eps_func'} . '2eps';

  @ARGV = @files;
}


1;
########################################################################
### Emacs settings
# Local Variables:
# mode: CPerl
# End:
