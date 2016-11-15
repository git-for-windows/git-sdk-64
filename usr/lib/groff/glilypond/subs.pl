my $License = q*
########################################################################
# Legalese
########################################################################

Subroutines for `glilypond'.

Source file position: `<groff-source>/contrib/glilypond/subs.pl'
Installed position: `<prefix>/lib/groff/glilypond/subs.pl'

Copyright (C) 2013-2014  Free Software Foundation, Inc.
  Written by Bernd Warken <groff-bernd.warken-72@web.de>

This file is part of `glilypond', which is part of `GNU groff'.

  `GNU groff' is free software: you can redistribute it and/or modify it
under the terms of the `GNU General Public License' as published by the
`Free Software Foundation', either version 3 of the License, or (at your
option) any later version.

  `GNU groff' is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the `GNU
General Public License' for more details.

  You should have received a copy of the 'GNU General Public License`
along with `groff', see the files `COPYING' and `LICENSE' in the top
directory of the `groff' source package.  If not, see
<http://www.gnu.org/licenses/>.
*;

##### end legalese


# use strict;
# use warnings;
# use diagnostics;

use integer;
use utf8;
use feature 'state';


########################################################################
# subs for using several times
########################################################################

sub create_ly2eps {		       # `--ly2eps' default
  our ( $out, $Read, $Temp );

  my $prefix = $Read->{'file_numbered'};   # with dir change to temp dir

  # `$ lilypond --ps -dbackend=eps -dgs-load-fonts \
  #      output=file_without_extension file.ly'
  # extensions are added automatically
  my $opts = '--ps -dbackend=eps -dinclude-eps-fonts -dgs-load-fonts ' .
    "--output=$prefix $prefix";
  &run_lilypond("$opts");

  Cwd::chdir $Temp->{'cwd'} or
      die "Could not change to former directory `" .
	$Temp->{'cwd'} . "': $!";

  my $eps_dir = $Temp->{'eps_dir'};
  my $dir = $Temp->{'temp_dir'};
  opendir( my $dh, $dir ) or
    die "could not open temporary directory `$dir': $!";

  my $re = qr<
	       ^
	       $prefix
	       -
	       .*
	       \.eps
	       $
	     >x;
  my $file;
  while ( readdir( $dh ) ) {
    chomp;
    $file = $_;
    if ( /$re/ ) {
      my $file_path = File::Spec->catfile($dir, $file);
      if ( $eps_dir ) {
	my $could_copy = FALSE;
	File::Copy::copy($file_path, $eps_dir)
	    and $could_copy = TRUE;
	if ( $could_copy ) {
	  unlink $file_path;
	  $file_path = File::Spec->catfile($eps_dir, $_);
	}
      }
      $out->print( '.PSPIC ' . $file_path );
    }
  }				# end while readdir
  closedir( $dh );
}				# end sub create_ly2eps()


sub create_pdf2eps {		       # `--pdf2eps'
  our ( $v, $stdout, $stderr, $out, $Read, $Temp );

  my $prefix = $Read->{'file_numbered'};   # with dir change to temp dir

  &run_lilypond("--pdf --output=$prefix $prefix");

  my $file_pdf = $prefix . '.pdf';
  my $file_ps = $prefix . '.ps';

  # pdf2ps in temp dir
  my $temp_file = &next_temp_file;
  $v->print( "\n##### run of `pdf2ps'" );
  # `$ pdf2ps file.pdf file.ps'
  my $output = `pdf2ps $file_pdf $file_ps 2> $temp_file`;
  die 'Program pdf2ps does not work.' if ( $? );
  &shell_handling($output, $temp_file);
  $v->print( "##### end run of `pdf2ps'\n" );

  # ps2eps in temp dir
  $temp_file = &next_temp_file;
  $v->print( "\n##### run of `ps2eps'" );
  # `$ ps2eps file.ps'
  $output = `ps2eps $file_ps 2> $temp_file`;
  die 'Program ps2eps does not work.' if ( $? );
  &shell_handling($output, $temp_file);
  $v->print( "##### end run of `ps2eps'\n" );

  # change back to former dir
  Cwd::chdir $Temp->{'cwd'} or
      die "Could not change to former directory `" .
	$Temp->{'cwd'} . "': $!";

  # handling of .eps file
  my $file_eps = $prefix . '.eps';
  my $eps_path = File::Spec->catfile($Temp->{'temp_dir'}, $file_eps);
  if ( $Temp->{'eps_dir'} ) {
    my $has_copied = FALSE;
    File::Copy::copy( $eps_path, $Temp->{'eps_dir'} )
	and $has_copied = TRUE;
    if ( $has_copied ) {
      unlink $eps_path;
      $eps_path = File::Spec->catfile( $Temp->{'eps_dir'}, $file_eps );
    } else {
      $stderr->print( "Could not use EPS-directory." );
    } # end Temp->{'eps_dir'}
  }
  # print into groff output
  $out->print( '.PSPIC ' . $eps_path );
}				# end sub create_pdf2eps()


sub is_subdir {			# arg1 is subdir of arg2 (is longer)
  my ( $dir1, $dir2 ) = @_;
  $dir1 = &path2abs( $dir1 );;
  $dir2 = &path2abs( $dir2 );;
  my @split1 = File::Spec->splitdir($dir1);
  my @split2 = File::Spec->splitdir($dir2);
  for ( @split2 ) {
    next if ( $_ eq shift @split1 );
    return FALSE;
  }
  return TRUE;
}


sub license {
  our ( $Legalese, $stdout );
  &version;
  $stdout->print( $Legalese->{'license'} );
} # end sub license()


sub make_dir {			# make directory or check if it exists
  our ( $v, $Args );

  my $dir_arg = shift;
  chomp $dir_arg;
  $dir_arg =~ s/^\s*(.*)\s*$/$1/;

  unless ( $dir_arg ) {
    $v->print( "make_dir(): empty argument" );
    return FALSE;
  }

  unless ( File::Spec->file_name_is_absolute($dir_arg) ) {
    my $res = Cwd::realpath($dir_arg);
    $res = File::Spec->canonpath($dir_arg) unless ( $res );
    $dir_arg = $res if ( $res );
  }

  return $dir_arg if ( -d $dir_arg && -w $dir_arg );


  # search thru the dir parts
  my @dir_parts = File::Spec->splitdir($dir_arg);
  my @dir_grow;
  my $dir_grow;
  my $can_create = FALSE;	# dir could be created if TRUE

 DIRPARTS: for ( @dir_parts ) {
    push @dir_grow, $_;
    next DIRPARTS unless ( $_ ); # empty string for root directory

    # from array to path dir string
    $dir_grow = File::Spec->catdir(@dir_grow);

    next DIRPARTS if ( -d $dir_grow );

    if ( -e $dir_grow ) {  # exists, but not a dir, so must be removed
      die "Couldn't create dir `$dir_arg', it is blocked by `$dir_grow'."
	unless ( -w $dir_grow );

      # now it's writable, but not a dir, so it can be removed
      unlink ( $dir_grow ) or
	die "Couldn't remove `$dir_grow', " .
	  "so I cannot create dir `$dir_arg': $!";
    }

    # $dir_grow does no longer exist, so the former dir must be writable
    # in order to create the directory
    pop @dir_grow;
    $dir_grow = File::Spec->catdir(@dir_grow);

    die "`$dir_grow' is not writable, " .
      "so directory `$dir_arg' can't be createdd."
	unless ( -w $dir_grow );

    # former directory is writable, so `$dir_arg' can be created

    File::Path::make_path( $dir_arg,
			   {
			    mask => oct('0700'),
			    verbose => $Args->{'verbose'},
			   }
			 )	#  `mkdir -P'
	or die "Could not create directory `$dir_arg': $!";

    last DIRPARTS;
  }

  die "`$dir_arg' is not a writable directory"
    unless ( -d $dir_arg && -w $dir_arg );

  return $dir_arg;

} # end sub make_dir()


my $number = 0;
sub next_temp_file {
  our ( $Temp, $v, $Args );
  ++$number;
  my $temp_basename = $Args->{'prefix'} . '_temp_' . $number;
  my $temp_file = File::Spec->catfile( $Temp->{'temp_dir'} ,
				       $temp_basename );
  $v->print( "next temporary file: `$temp_file'" );
  return $temp_file;
}				# end sub next_temp_file()


sub path2abs {
  our ( $Temp, $Args );

  my $path = shift;
  $path =~ s/
	      ^
	      \s*
	      (
		.*
	      )
	      \s*
	      $
	    /$1/x;

  die "path2abs(): argument is empty." unless ( $path );

  # Perl does not support shell `~' for home dir
  if ( $path =~ /
		  ^
		  ~
		/x ) {
    if ( $path eq '~' ) {	# only own home
      $path = File::HomeDir->my_home;
    } elsif ( $path =~ m<
			  ^
			  ~ /
			  (
			    .*
			  )
			  $
			>x ) {	# subdir of own home
      $path = File::Spec->catdir( $Temp->{'cwd'}, $1 );
    } elsif ( $path =~ m<
			  ^
			  ~
			  (
			    [^/]+
			  )
			  $
			>x ) {	# home of other user
      $path = File::HomeDir->users_home($1);
    } elsif ( $path =~ m<
			  ^
			  ~
			  (
			    [^/]+
			  )
			  /+
			  (
			    .*
			  )
			  $
			>x ) {	# subdir of other home
      $path = File::Spec->
	catdir( File::HomeDir->users_home($1), $2 );
    }
  }

  $path = File::Spec->rel2abs($path);

  # now $path is absolute
  return $path;
} # end sub path2abs()


sub run_lilypond {
  # arg is the options collection for `lilypond' to run
  # either from ly or pdf

  our ( $Temp, $v );

  my $opts = shift;
  chomp $opts;

  my $temp_file = &next_temp_file;
  my $output = EMPTYSTRING;

  # change to temp dir
  Cwd::chdir $Temp->{'temp_dir'} or
      die "Could not change to temporary directory `" .
	$Temp->{'temp_dir'} . "': $!";

  $v->print( "\n##### run of `lilypond " . $opts . "'" );
  $output = `lilypond $opts 2>$temp_file`;
  die "Program lilypond does not work, see `$temp_file': $?"
    if ( $? );
  chomp $output;
  &shell_handling($output, $temp_file);
  $v->print( "##### end run of `lilypond'\n" );

  # stay in temp dir
} # end sub run_lilypond()


sub shell_handling {
  # Handle ``-shell-command output in a string (arg1).
  # stderr goes to temporary file $TempFile.

  our ( $out, $v, $Args );

  my $out_string = shift;
  my $temp_file = shift;

  my $a = &string2array($out_string); # array ref
  for ( @$a ) {
    $out->print( $_ );
  }

  $temp_file && -f $temp_file && -r $temp_file ||
    die "shell_handling(): $temp_file is not a readable file.";
  my $temp = new FH_READ_FILE($temp_file);
  my $res = $temp->read_all();
  for ( @$res ) {
    chomp;
    $v->print($_);
  }

  unlink $temp_file unless ( $Args->{'keep_all'} );
} # end sub shell_handling()


sub string2array {
  my $s = shift;
  my @a = ();
  for ( split "\n", $s ) {
    chomp;
    push @a, $_;
  }
  return \@a;
} # end string2array()


sub usage {			# for `--help'
  our ( $Globals, $Args );

  my $p = $Globals->{'prog'};
  my $usage = EMPTYSTRING;
  $usage = '###### usage:' . "\n" if ( $Args->{'verbose'} );
  $usage .= qq*Options for $p:
Read a `roff' file or standard input and transform `lilypond' parts
(everything between `.lilypond start' and `.lilypond end') into
`EPS'-files that can be read by groff using `.PSPIC'.

There is also a command `.lilypond include <file_name>' that can
include a complete `lilypond' file into the `groff' document.


# Breaking options:
$p -?|-h|--help|--usage    # usage
$p --version               # version information
$p --license               # the license is GPL >= 3


# Normal options:
$p [options] [--] [filename ...]

There are 2 options for influencing the way how the `EPS' files for the
`roff' display are generated:
--ly2eps           `lilypond' generates `EPS' files directly (default)
--pdf2eps          `lilypond' generates a `PDF' file that is transformed

-k|--keep_all      do not delete any temporary files
-v|--verbose       print much information to STDERR

Options with an argument:
-e|--eps_dir=...   use a directory for the EPS files
-o|--output=...    sent output in the groff language into file ...
-p|--prefix=...    start for the names of temporary files
-t|--temp_dir=...  provide the directory for temporary files.

The directories set are created when they do not exist.
*;

  # old options:
  # --keep_files       -k: do not delete any temporary files
  # --file_prefix=...  -p: start for the names of temporary files

  $main::stdout->print( $usage );
} # end sub usage()


sub version { # for `--version'
  our ( $Globals, $Legalese, $stdout, $Args );
  my $end;
  if ( $Globals->{'groff_version'} ) {
    $end = " version $Globals->{'groff_version'}";
  } else {
    $end = '.';
  }

  my $output = EMPTYSTRING;
  $output = "###### version:\n" if ( $Args->{'verbose'} );
  $output .= "`" . $Globals->{'prog'} . "' version `" .
    $Legalese->{'version'} . "' is part of `GNU groff'" . $end;

  $stdout->print($output);
} # end sub version()


# end of subs

1;
########################################################################
### Emacs settings
# Local Variables:
# mode: CPerl
# End:
