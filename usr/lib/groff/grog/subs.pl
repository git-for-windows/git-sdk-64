#! /usr/bin/env perl
# grog - guess options for groff command
# Inspired by doctype script in Kernighan & Pike, Unix Programming
# Environment, pp 306-8.

# Source file position: <groff-source>/src/roff/grog/subs.pl
# Installed position: <prefix>/lib/grog/subs.pl

# Copyright (C) 1993-2018 Free Software Foundation, Inc.
# This file was split from grog.pl and put under GPL2 by
#               Bernd Warken <groff-bernd.warken-72@web.de>.
# The macros for identifying the devices were taken from Ralph
# Corderoy's 'grog.sh' of 2006.

# Last update: 10 Sep 2015

# This file is part of 'grog', which is part of 'groff'.

# 'groff' is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.

# 'groff' is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You can get the license text for the GNU General Public License
# version 2 in the internet at
# <http://www.gnu.org/licenses/gpl-2.0.html>.

########################################################################

require v5.6;

use warnings;
use strict;

use File::Spec;

# printing of hashes: my %hash = ...; print Dumper(\%hash);
use Data::Dumper;

# for running shell based programs within Perl; use `` instead of
# use IPC::System::Simple qw(capture capturex run runx system systemx);

$\ = "\n";

# my $Sp = "[\\s\\n]";
# my $Sp = qr([\s\n]);
# my $Sp = '' if $arg eq '-C';
my $Sp = '';

# from 'src/roff/groff/groff.cpp' near 'getopt_long'
my $groff_opts =
  'abcCd:D:eEf:F:gGhiI:jJkK:lL:m:M:n:No:pP:r:RsStT:UvVw:W:XzZ';

my @Command = ();		# stores the final output
my @Mparams = ();		# stores the options '-m*'
my @devices = ();		# stores -T

my $do_run = 0;			# run generated 'groff' command
my $pdf_with_ligatures = 0;	# '-P-y -PU' for 'pdf' device
my $with_warnings = 0;

my $Prog = $0;
{
  my ($v, $d, $f) = File::Spec->splitpath($Prog);
  $Prog = $f;
}


my %macros;
my %Groff =
  (
   # preprocessors
   'chem' => 0,
   'eqn' => 0,
   'gperl' => 0,
   'grap' => 0,
   'grn' => 0,
   'gideal' => 0,
   'gpinyin' => 0,
   'lilypond' => 0,

   'pic' => 0,
   'PS' => 0,		# opening for pic
   'PF' => 0,		# alternative opening for pic
   'PE' => 0,		# closing for pic

   'refer' => 0,
   'refer_open' => 0,
   'refer_close' => 0,
   'soelim' => 0,
   'tbl' => 0,

   # tmacs
#   'man' => 0,
#   'mandoc' => 0,
#   'mdoc' => 0,
#   'mdoc_old' => 0,
#   'me' => 0,
#   'mm' => 0,
#   'mom' => 0,
#   'ms' => 0,

   # requests
   'AB' => 0,		# ms
   'AE' => 0,		# ms
   'AI' => 0,		# ms
   'AU' => 0,		# ms
   'NH' => 0,		# ms
   'TH_later' => 0,	# TH not 1st command is ms
   'TL' => 0,		# ms
   'UL' => 0,		# ms
   'XP' => 0,		# ms

   'IP' => 0,		# man and ms
   'LP' => 0,		# man and ms
   'P' => 0,		# man and ms
   'PP' => 0,		# man and ms
   'SH' => 0,		# man and ms

   'OP' => 0,		# man
   'SS' => 0,		# man
   'SY' => 0,		# man
   'TH_first' => 0,	# TH as 1st command is man
   'TP' => 0,		# man
   'UR' => 0,		# man
   'YS' => 0,		# man

   # for mdoc and mdoc-old
   # .Oo and .Oc for modern mdoc, only .Oo for mdoc-old
   'Oo' => 0,		# mdoc and mdoc-old
   'Oc' => 0,		# mdoc
   'Dd' => 0,		# mdoc
  ); # end of %Groff


# for first line check
my %preprocs_tmacs =
  (
   'chem' => 0,
   'eqn' => 0,
   'gideal' => 0,
   'gpinyin' => 0,
   'grap' => 0,
   'grn' => 0,
   'pic' => 0,
   'refer' => 0,
   'soelim' => 0,
   'tbl' => 0,

   'geqn' => 0,
   'gpic' => 0,
   'neqn' => 0,

   'man' => 0,
   'mandoc' => 0,
   'mdoc' => 0,
   'mdoc-old' => 0,
   'me' => 0,
   'mm' => 0,
   'mom' => 0,
   'ms' => 0,
  );

my @filespec;

my $tmac_ext = '';


########################################################################
# err()
########################################################################

sub err {
  my $text = shift;
  print STDERR $text;
}


########################################################################
# handle_args()
########################################################################

sub handle_args {
  my $double_minus = 0;
  my $was_minus = 0;
  my $was_T = 0;
  my $optarg = 0;
  # globals: @filespec, @Command, @devices, @Mparams

  foreach my $arg (@ARGV) {

    if ( $optarg ) {
      push @Command, $arg;
      $optarg = 0;
      next;
    }

    if ( $double_minus ) {
      if (-f $arg && -r $arg) {
	push @filespec, $arg;
      } else {
	print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
	  "grog: $arg is not a readable file.";
      }
      next;
    }

    if ( $was_T ) {
      push @devices, $arg;
      $was_T = 0;
      next;
    }
####### handle_args()

    unless ( $arg =~ /^-/ ) { # file name, no opt, no optarg
      unless (-f $arg && -r $arg) {
	print 'unknown file name: ' . $arg;
      }
      push @filespec, $arg;
      next;
    }

    # now $arg starts with '-'

    if ($arg eq '-') {
      unless ($was_minus) {
	push @filespec, $arg;
	$was_minus = 1;
      }
      next;
    }

    if ($arg eq '--') {
      $double_minus = 1;
      push(@filespec, $arg);
      next;
    }

    &version() if $arg =~ /^--?v/;	# --version, with exit
    &help() if $arg  =~ /--?h/;		# --help, with exit

    if ( $arg =~ /^--r/ ) {		#  --run, no exit
      $do_run = 1;
      next;
    }

    if ( $arg =~ /^--wa/ ) {		#  --warnings, no exit
      $with_warnings = 1;
      next;
    }
####### handle_args()

    if ( $arg =~ /^--(wi|l)/ ) { # --ligatures, no exit
      # the old --with_ligatures is only kept for compatibility
      $pdf_with_ligatures = 1;
      next;
    }

    if ($arg =~ /^-m/) {
      push @Mparams, $arg;
      next;
    }

    if ($arg =~ /^-T$/) {
      $was_T = 1;
      next;
    }

    if ($arg =~ s/^-T(\w+)$/$1/) {
      push @devices, $1;
      next;
    }

    if ($arg =~ /^-(\w)(\w*)$/) {	# maybe a groff option
      my $opt_char = $1;
      my $opt_char_with_arg = $opt_char . ':';
      my $others = $2;
      if ( $groff_opts =~ /$opt_char_with_arg/ ) {	# groff optarg
	if ( $others ) {	# optarg is here
	  push @Command, '-' . $opt_char;
	  push @Command, '-' . $others;
	  next;
	}
	# next arg is optarg
	$optarg = 1;
	next;
####### handle_args()
      } elsif ( $groff_opts =~ /$opt_char/ ) {	# groff no optarg
	push @Command, '-' . $opt_char;
	if ( $others ) {	# $others is now an opt collection
	  $arg = '-' . $others;
	  redo;
	}
	# arg finished
	next;
      } else {		# not a groff opt
	print STDERR __FILE__ . ' '  . __LINE__ . ': ' .
	  'unknown argument ' . $arg;
	push(@Command, $arg);
	next;
      }
    }
  }
  @filespec = ('-') unless (@filespec);
} # handle_args()



########################################################################
# handle_file_ext()
########################################################################

sub handle_file_ext {
  # get tmac from file name extension
  # output number of found single tmac

  # globals: @filespec, $tmac_ext;

  foreach my $file ( @filespec ) {
    # test for each file name in the arguments
    unless ( open(FILE, $file eq "-" ? $file : "< $file") ) {
      print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
	"$Prog: can't open \'$file\': $!";
      next;
    }

    next unless ( $file =~ /\./ ); # file name has no dot '.'

##### handle_file_ext()
    # get extension
    my $ext = $file;
    $ext =~ s/^
	      .*
	      \.
	      ([^.]*)
	      $
	     /$1/x;
    next unless ( $ext );

##### handle_file_ext()
    # these extensions are correct, but not based on a tmac
    next if ( $ext =~ /^(
			 chem|
			 eqn|
			 g|
			 grap|
			 grn|
			 groff|
			 hdtbl|
			 pdfroff|
			 pic|
			 pinyin|
			 ref|
			 roff|
			 t|
			 tbl|
			 tr|
			 www
		       )$/x );

##### handle_file_ext()
    # extensions for man tmac
    if ( $ext =~ /^(
		      [1-9lno]|
		      man|
		      n|
		      1b
		    )$/x ) {
      # 'man|n' from 'groff' source
      # '1b' from 'heirloom'
      # '[1-9lno]' from man-pages
      if ( $tmac_ext && $tmac_ext ne 'man' ) {
	# found tmac is not 'man'
	print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
	  '2 different file name extensions found ' .
	    $tmac_ext . ' and ' . $ext;
	$tmac_ext = '';
	next;
      }

##### handle_file_ext()
      $tmac_ext = 'man';
      next;
    }

    if ( $ext =~ /^(
		    mandoc|
		    mdoc|
		    me|
		    mm|
		    mmse|
		    mom|
		    ms|
		    $)/x ) {
      if ( $tmac_ext && $tmac_ext ne $ext ) {
	# found tmac is not identical to former found tmac
##### handle_file_ext()
	print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
	  '2 different file name extensions found ' .
	    $tmac_ext . ' and ' . $ext;
	$tmac_ext = '';
	next;
      }

      $tmac_ext = $ext;
      next;
    }

    print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
      'Unknown file name extension '. $file . '.';
    next;
  } # end foreach file

  1;
} # handle_file_ext()


########################################################################
# handle_whole_files()
########################################################################

sub handle_whole_files {
  # globals: @filespec

  foreach my $file ( @filespec ) {
    unless ( open(FILE, $file eq "-" ? $file : "< $file") ) {
      print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
	"$Prog: can't open \'$file\': $!";
      next;
    }
    my $line = <FILE>; # get single line

    unless ( defined($line) ) {
      # empty file, go to next filearg
      close (FILE);
      next;
    }

    if ( $line ) {
      chomp $line;
      unless ( &do_first_line( $line, $file ) ) {
	# not an option line
	&do_line( $line, $file );
      }
    } else { # empty line
      next;
    }

    while (<FILE>) { # get lines by and by
      chomp;
      &do_line( $_, $file );
    }
    close(FILE);
  } # end foreach
} # handle_whole_files()


########################################################################
# do_first_line()
########################################################################

# As documented for the 'man' program, the first line can be
# used as a groff option line.  This is done by:
# - start the line with '\" (apostrophe, backslash, double quote)
# - add a space character
# - a word using the following characters can be appended: 'egGjJpRst'.
#     Each of these characters means an option for the generated
#     'groff' command line, e.g. '-t'.

sub do_first_line {
  my ( $line, $file ) = @_;

  # globals: %preprocs_tmacs

  # For a leading groff options line use only [egGjJpRst]
  if  ( $line =~ /^[.']\\"[\segGjJpRst]+&/ ) {
    # this is a groff options leading line
    if ( $line =~ /^\./ ) {
      # line is a groff options line with . instead of '
      print "First line in $file must start with an apostrophe \ " .
	"instead of a period . for groff options line!";
    }

    if ( $line =~ /j/ ) {
      $Groff{'chem'}++;
    }
    if ( $line =~ /e/ ) {
      $Groff{'eqn'}++;
    }
    if ( $line =~ /g/ ) {
      $Groff{'grn'}++;
    }
    if ( $line =~ /G/ ) {
      $Groff{'grap'}++;
    }
    if ( $line =~ /i/ ) {
      $Groff{'gideal'}++;
    }
    if ( $line =~ /p/ ) {
      $Groff{'pic'}++;
    }
    if ( $line =~ /R/ ) {
      $Groff{'refer'}++;
    }
    if ( $line =~ /s/ ) {
      $Groff{'soelim'}++;
    }
####### do_first_line()
    if ( $line =~ /t/ ) {
      $Groff{'tbl'}++;
    }
    return 1;	# a leading groff options line, 1 means yes, 0 means no
  }

  # not a leading short groff options line

  return 0 if ( $line !~ /^[.']\\"\s*(.*)$/ );	# ignore non-comments

  return 0 unless ( $1 );	# for empty comment

  # all following array members are either preprocs or 1 tmac
  my @words = split '\s+', $1;

  my @in = ();
  my $word;
  for $word ( @words ) {
    if ( $word eq 'ideal' ) {
      $word = 'gideal';
    } elsif ( $word eq 'gpic' ) {
      $word = 'pic';
    } elsif ( $word =~ /^(gn|)eqn$/ ) {
      $word = 'eqn';
    }
    if ( exists $preprocs_tmacs{$word} ) {
      push @in, $word;
    } else {
      # not word for preproc or tmac
      return 0;
    }
  }

  for $word ( @in ) {
    $Groff{$word}++;
  }
} # do_first_line()


########################################################################
# do_line()
########################################################################

my $before_first_command = 1; # for check of .TH

sub do_line {
  my ( $line, $file ) = @_;

  return if ( $line =~ /^[.']\s*\\"/ );	# comment

  return unless ( $line =~ /^[.']/ );	# ignore text lines

  $line =~ s/^['.]\s*/./;	# let only a dot as leading character,
				# remove spaces after the leading dot
  $line =~ s/\s+$//;		# remove final spaces

  return if ( $line =~ /^\.$/ );	# ignore .
  return if ( $line =~ /^\.\.$/ );	# ignore ..

  if ( $before_first_command ) { # so far without 1st command
    if ( $line =~ /^\.TH/ ) {
      # check if .TH is 1st command for man
      $Groff{'TH_first'} = 1 if ( $line =~ /^\.\s*TH/ );
    }
    if ( $line =~ /^\./ ) {
      $before_first_command = 0;
    }
  }

  # split command
  $line =~ /^(\.\w+)\s*(.*)$/;
  my $command = $1;
  $command = '' unless ( defined $command );
  my $args = $2;
  $args = '' unless ( defined $args );


  ######################################################################
  # soelim
  if ( $line =~ /^\.(do)?\s*(so|mso|PS\s*<|SO_START).*$/ ) {
    # '.so', '.mso', '.PS<...', '.SO_START'
    $Groff{'soelim'}++;
    return;
  }
  if ( $line =~ /^\.(do)?\s*(so|mso|PS\s*<|SO_START).*$/ ) {
    # '.do so', '.do mso', '.do PS<...', '.do SO_START'
    $Groff{'soelim'}++;
    return;
  }
####### do_line()

  ######################################################################
  # macros

  if ( $line =~ /^\.de1?\W?/ ) {
    # this line is a macro definition, add it to %macros
    my $macro = $line;
    $macro =~ s/^\.de1?\s+(\w+)\W*/.$1/;
    return if ( exists $macros{$macro} );
    $macros{$macro} = 1;
    return;
  }


  # if line command is a defined macro, just ignore this line
  return if ( exists $macros{$command} );


  ######################################################################
  # preprocessors

  if ( $command =~ /^(\.cstart)|(begin\s+chem)$/ ) {
    $Groff{'chem'}++;		# for chem
    return;
  }
  if ( $command =~ /^\.EQ$/ ) {
    $Groff{'eqn'}++;		# for eqn
    return;
  }
  if ( $command =~ /^\.G1$/ ) {
    $Groff{'grap'}++;		# for grap
    return;
  }
  if ( $command =~ /^\.Perl/ ) {
    $Groff{'gperl'}++;		# for gperl
    return;
  }
  if ( $command =~ /^\.pinyin/ ) {
    $Groff{'gpinyin'}++;		# for gperl
    return;
  }
  if ( $command =~ /^\.GS$/ ) {
    $Groff{'grn'}++;		# for grn
    return;
  }
  if ( $command =~ /^\.IS$/ ) {
    $Groff{'gideal'}++;		# preproc gideal for ideal
    return;
  }
  if ( $command =~ /^\.lilypond$/ ) {
    $Groff{'lilypond'}++;	# for glilypond
    return;
  }

####### do_line()

  # pic can be opened by .PS or .PF and closed by .PE
  if ( $command =~ /^\.PS$/ ) {
    $Groff{'pic'}++;		# normal opening for pic
    return;
  }
  if ( $command =~ /^\.PF$/ ) {
    $Groff{'PF'}++;		# alternate opening for pic
    return;
  }
  if ( $command =~ /^\.PE$/ ) {
    $Groff{'PE'}++;		# closing for pic
    return;
  }

  if ( $command =~ /^\.R1$/ ) {
    $Groff{'refer'}++;		# for refer
    return;
  }
  if ( $command =~ /^\.\[$/ ) {
    $Groff{'refer_open'}++;	# for refer open
    return;
  }
  if ( $command =~ /^\.\]$/ ) {
    $Groff{'refer_close'}++;	# for refer close
    return;
  }
  if ( $command =~ /^\.TS$/ ) {
    $Groff{'tbl'}++;		# for tbl
    return;
  }
  if ( $command =~ /^\.TH$/ ) {
    unless ( $Groff{'TH_first'} ) {
      $Groff{'TH_later'}++;		# for tbl
    }
    return;
  }


  ######################################################################
  # macro package (tmac)
  ######################################################################

  ##########
  # modern mdoc

  if ( $command =~ /^\.(Dd)$/ ) {
    $Groff{'Dd'}++;		# for modern mdoc
    return;
  }

####### do_line()
  # In the old version of -mdoc 'Oo' is a toggle, in the new it's
  # closed by 'Oc'.
  if ( $command =~ /^\.Oc$/ ) {
    $Groff{'Oc'}++;		# only for modern mdoc
    return;
  }


  ##########
  # old and modern mdoc

  if ( $command =~ /^\.Oo$/ ) {
    $Groff{'Oo'}++;		# for mdoc and mdoc-old
    return;
  }


  ##########
  # old mdoc
  if ( $command =~ /^\.(Tp|Dp|De|Cx|Cl)$/ ) {
    $Groff{'mdoc_old'}++;	# true for old mdoc
    return;
  }


  ##########
  # for ms

####### do_line()
  if ( $command =~ /^\.AB$/ ) {
    $Groff{'AB'}++;		# for ms
    return;
  }
  if ( $command =~ /^\.AE$/ ) {
    $Groff{'AE'}++;		# for ms
    return;
  }
  if ( $command =~ /^\.AI$/ ) {
    $Groff{'AI'}++;		# for ms
    return;
  }
  if ( $command =~ /^\.AU$/ ) {
    $Groff{'AU'}++;		# for ms
    return;
  }
  if ( $command =~ /^\.NH$/ ) {
    $Groff{'NH'}++;		# for ms
    return;
  }
  if ( $command =~ /^\.TL$/ ) {
    $Groff{'TL'}++;		# for ms
    return;
  }
  if ( $command =~ /^\.XP$/ ) {
    $Groff{'XP'}++;		# for ms
    return;
  }


  ##########
  # for man and ms

  if ( $command =~ /^\.IP$/ ) {
    $Groff{'IP'}++;		# for man and ms
    return;
  }
  if ( $command =~ /^\.LP$/ ) {
    $Groff{'LP'}++;		# for man and ms
    return;
  }
####### do_line()
  if ( $command =~ /^\.P$/ ) {
    $Groff{'P'}++;		# for man and ms
    return;
  }
  if ( $command =~ /^\.PP$/ ) {
    $Groff{'PP'}++;		# for man and ms
    return;
  }
  if ( $command =~ /^\.SH$/ ) {
    $Groff{'SH'}++;		# for man and ms
    return;
  }
  if ( $command =~ /^\.UL$/ ) {
    $Groff{'UL'}++;		# for man and ms
    return;
  }


  ##########
  # for man only

  if ( $command =~ /^\.OP$/ ) {	# for man
    $Groff{'OP'}++;
    return;
  }
  if ( $command =~ /^\.SS$/ ) {	# for man
    $Groff{'SS'}++;
    return;
  }
  if ( $command =~ /^\.SY$/ ) {	# for man
    $Groff{'SY'}++;
    return;
  }
  if ( $command =~ /^\.TP$/ ) {	# for man
    $Groff{'TP'}++;
    return;
  }
  if ( $command =~ /^\.UR$/ ) {
    $Groff{'UR'}++;		# for man
    return;
  }
  if ( $command =~ /^\.YS$/ ) {	# for man
   $Groff{'YS'}++;
    return;
  }
####### do_line()


  ##########
  # me

  if ( $command =~ /^\.(
		      [ilnp]p|
		      sh
		    )$/x ) {
    $Groff{'me'}++;		# for me
    return;
  }


  #############
  # mm and mmse

  if ( $command =~ /^\.(
		      H|
		      MULB|
		      LO|
		      LT|
		      NCOL|
		      P\$|
		      PH|
		      SA
		    )$/x ) {
    $Groff{'mm'}++;		# for mm and mmse
    if ( $command =~ /^\.LO$/ ) {
      if ( $args =~ /^(DNAMN|MDAT|BIL|KOMP|DBET|BET|SIDOR)/ ) {
	$Groff{'mmse'}++;	# for mmse
      }
    } elsif ( $command =~ /^\.LT$/ ) {
      if ( $args =~ /^(SVV|SVH)/ ) {
	$Groff{'mmse'}++;	# for mmse
      }
    }
    return;
  }
####### do_line()

  ##########
  # mom

  if ( $line =~ /^\.(
		   ALD|
		   DOCTYPE|
		   FAMILY|
		   FT|
		   FAM|
		   LL|
		   LS|
		   NEWPAGE|
		   PAGE|
		   PAPER|
		   PRINTSTYLE|
		   PT_SIZE|
		   T_MARGIN
		 )$/x ) {
    $Groff{'mom'}++;		# for mom
    return;
  }

} # do_line()


########################################################################
# sub make_groff_device
########################################################################

my @m = ();
my @preprograms = ();
my $correct_tmac = '';

sub make_groff_device {
  # globals: @devices

  # default device is 'ps' when without '-T'
  my $device;
  push @devices, 'ps' unless ( @devices );

###### make_groff_device()
  for my $d ( @devices ) {
    if ( $d =~ /^(		# suitable devices
		  dvi|
		  html|
		  xhtml|
		  lbp|
		  lj4|
		  ps|
		  pdf|
		  ascii|
		  cp1047|
		  latin1|
		  utf8
		)$/x ) {
###### make_groff_device()
      $device = $d;
    } else {
      next;
    }


    if ( $device ) {
      push @Command, '-T';
      push @Command, $device;
    }
  }

###### make_groff_device()
  if ( $device eq 'pdf' ) {
    if ( $pdf_with_ligatures ) {	# with --ligature argument
      push( @Command, '-P-y' );
      push( @Command, '-PU' );
    } else {	# no --ligature argument
      if ( $with_warnings ) {
	print STDERR <<EOF;
If you have trouble with ligatures like 'fi' in the 'groff' output, you
can proceed as one of
- add 'grog' option '--with_ligatures' or
- use the 'grog' option combination '-P-y -PU' or
- try to remove the font named similar to 'fonts-texgyre' from your system.
EOF
      }	# end of warning
    }	# end of ligature
  }	# end of pdf device
} # make_groff_device()


########################################################################
# make_groff_preproc()
########################################################################

sub make_groff_preproc {
  # globals: %Groff, @preprograms, @Command

  # preprocessors without 'groff' option
  if ( $Groff{'lilypond'} ) {
    push @preprograms, 'glilypond';
  }
  if ( $Groff{'gperl'} ) {
    push @preprograms, 'gperl';
  }
  if ( $Groff{'gpinyin'} ) {
    push @preprograms, 'gpinyin';
  }

  # preprocessors with 'groff' option
  if ( ( $Groff{'PS'} ||  $Groff{'PF'} ) &&  $Groff{'PE'} ) {
    $Groff{'pic'} = 1;
  }
  if ( $Groff{'gideal'} ) {
    $Groff{'pic'} = 1;
  }

###### make_groff_preproc()
  $Groff{'refer'} ||= $Groff{'refer_open'} && $Groff{'refer_close'};

  if ( $Groff{'chem'} || $Groff{'eqn'} ||  $Groff{'gideal'} ||
       $Groff{'grap'} || $Groff{'grn'} || $Groff{'pic'} ||
       $Groff{'refer'} || $Groff{'tbl'} ) {
    push(@Command, '-s') if $Groff{'soelim'};

    push(@Command, '-R') if $Groff{'refer'};

    push(@Command, '-t') if $Groff{'tbl'};	# tbl before eqn
    push(@Command, '-e') if $Groff{'eqn'};

    push(@Command, '-j') if $Groff{'chem'};	# chem produces pic code
    push(@Command, '-J') if $Groff{'gideal'};	# gideal produces pic
    push(@Command, '-G') if $Groff{'grap'};
    push(@Command, '-g') if $Groff{'grn'};	# gremlin files for -me
    push(@Command, '-p') if $Groff{'pic'};

  }
} # make_groff_preproc()


########################################################################
# make_groff_tmac_man_ms()
########################################################################

sub make_groff_tmac_man_ms {
  # globals: @filespec, $tmac_ext, %Groff

  # 'man' requests, not from 'ms'
  if ( $Groff{'SS'} || $Groff{'SY'} || $Groff{'OP'} ||
       $Groff{'TH_first'} || $Groff{'TP'} || $Groff{'UR'} ) {
    $Groff{'man'} = 1;
    push(@m, '-man');

    $tmac_ext = 'man' unless ( $tmac_ext );
    &err('man requests found, but file name extension ' .
	 'was: ' . $tmac_ext) unless ( $tmac_ext eq 'man' );
    $tmac_ext = 'man';
    return 1;	# true
  }

###### make_groff_tmac_man_ms()
  # 'ms' requests, not from 'man'
  if (
      $Groff{'1C'} || $Groff{'2C'} ||
      $Groff{'AB'} || $Groff{'AE'} || $Groff{'AI'} || $Groff{'AU'} ||
      $Groff{'BX'} || $Groff{'CD'} || $Groff{'DA'} || $Groff{'DE'} ||
      $Groff{'DS'} || $Groff{'ID'} || $Groff{'LD'} || $Groff{'NH'} ||
      $Groff{'TH_later'} ||
      $Groff{'TL'} || $Groff{'UL'} || $Groff{'XP'}
     ) {
    $Groff{'ms'} = 1;
    push(@m, '-ms');

    $tmac_ext = 'ms' unless ( $tmac_ext );
    &err('ms requests found, but file name extension ' .
	 'was: ' . $tmac_ext) unless ( $tmac_ext eq 'ms' );
    $tmac_ext = 'ms';
    return 1;	# true
  }

###### make_groff_tmac_man_ms()

  # both 'man' and 'ms' requests
  if ( $Groff{'P'} || $Groff{'IP'}  ||
       $Groff{'LP'} || $Groff{'PP'} || $Groff{'SH'} ) {
    if ( $tmac_ext eq 'man' ) {
      $Groff{'man'} = 1;
      push(@m, '-man');
      return 1;	# true
    } elsif ( $tmac_ext eq 'ms' ) {
      $Groff{'ms'} = 1;
      push(@m, '-ms');
      return 1;	# true
    }
    return 0;
  }
} # make_groff_tmac_man_ms()



########################################################################
# make_groff_tmac_others()
########################################################################

sub make_groff_tmac_others {
  # globals: @filespec, $tmac_ext, %Groff

  # mdoc
  if ( ( $Groff{'Oo'} && $Groff{'Oc'} ) || $Groff{'Dd'} ) {
    $Groff{'Oc'} = 0;
    $Groff{'Oo'} = 0;
    push(@m, '-mdoc');
    return 1;	# true
  }
  if ( $Groff{'mdoc_old'} || $Groff{'Oo'} ) {
    push(@m, '-mdoc_old');
    return 1;	# true
  }

  # me
  if ( $Groff{'me'} ) {
    push(@m, '-me');
    return 1;	# true
  }

##### make_groff_tmac_others()
  # mm and mmse
  if ( $Groff{'mm'} ) {
    push(@m, '-mm');
    return 1;	# true
  }
  if ( $Groff{'mmse'} ) {	# Swedish mm
    push(@m, '-mmse');
    return 1;	# true
  }

  # mom
  if ( $Groff{'mom'} ) {
    push(@m, '-mom');
    return 1;	# true
  }
} # make_groff_tmac_others()


########################################################################
# make_groff_line_rest()
########################################################################

sub make_groff_line_rest {
  my $file_args_included;	# file args now only at 1st preproc
  unshift @Command, 'groff';
  if ( @preprograms ) {
    my @progs;
    $progs[0] = shift @preprograms;
    push(@progs, @filespec);
    for ( @preprograms ) {
      push @progs, '|';
      push @progs, $_;
    }
    push @progs, '|';
    unshift @Command, @progs;
    $file_args_included = 1;
  } else {
    $file_args_included = 0;
  }

###### make_groff_line_rest()
  foreach (@Command) {
    next unless /\s/;
    # when one argument has several words, use accents
    $_ = "'" . $_ . "'";
  }


###### make_groff_line_rest()
  ##########
  # -m arguments
  my $nr_m_guessed = scalar @m;
  if ( $nr_m_guessed > 1 ) {
    print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
      'argument for -m found: ' . @m;
  }


  my $nr_m_args = scalar @Mparams;	# m-arguments for grog
  my $last_m_arg = '';	# last provided -m option
  if ( $nr_m_args > 1 ) {
    # take the last given -m argument of grog call,
    # ignore other -m arguments and the found ones
    $last_m_arg = $Mparams[-1];	# take the last -m argument
    print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
      $Prog . ": more than 1 '-m' argument: @Mparams";
    print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
      'We take the last one: ' . $last_m_arg;
  } elsif ( $nr_m_args == 1 ) {
    $last_m_arg = $Mparams[0];
  }

###### make_groff_line_rest()
  my $final_m = '';
  if ( $last_m_arg ) {
    my $is_equal = 0;
    for ( @m ) {
      if ( $_ eq $last_m_arg ) {
	$is_equal = 1;
	last;
      }
      next;
    }	# end for @m
    if ( $is_equal ) {
      $final_m = $last_m_arg;
    } else {
      print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
	'Provided -m argument ' . $last_m_arg .
	  ' differs from guessed -m args: ' . @m;
      print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
	'The argument is taken.';
      $final_m = $last_m_arg;
    }
###### make_groff_line_rest()
  } else {	# no -m arg provided
    if ( $nr_m_guessed > 1 ) {
      print STDERR __FILE__ . ' ' .  __LINE__ . ': ' .
	'More than 1 -m arguments were guessed: ' . @m;
      print STDERR __FILE__ . ' ' .  __LINE__ . ': ' . 'Guessing stopped.';
      exit 1;
    } elsif ( $nr_m_guessed == 1 ) {
      $final_m = $m[0];
    } else {
      # no -m provided or guessed
    }
  }
  push @Command, $final_m if ( $final_m );

  push(@Command, @filespec) unless ( $file_args_included );

  #########
  # execute the 'groff' command here with option '--run'
  if ( $do_run ) { # with --run
    print STDERR __FILE__ . ' ' .  __LINE__ . ': ' . "@Command";
    my $cmd = join ' ', @Command;
    system($cmd);
  } else {
    print "@Command";
  }

  exit 0;
} # make_groff_line_rest()


########################################################################
# sub help
########################################################################

sub help {
  print <<EOF;
usage: grog [option]... [--] [filespec]...

"filespec" is either the name of an existing, readable file or "-" for
standard input.  If no 'filespec' is specified, standard input is
assumed automatically.  All arguments after a '--' are regarded as file
names, even if they start with a '-' character.

'option' is either a 'groff' option or one of these:

-h|--help	print this uasge message and exit
-v|--version	print version information and exit

-C		compatibility mode
--ligatures	include options '-P-y -PU' for internal font, which
		preserves the ligatures like 'fi'
--run		run the checked-out groff command
--warnings	display more warnings to standard error

All other options should be 'groff' 1-character options.  These are then
appended to the generated 'groff' command line.  The '-m' options will
be checked by 'grog'.

EOF
  exit 0;
} # help()


########################################################################
# sub version
########################################################################

sub version {
  our %at_at;
  print "Perl version of GNU $Prog " .
    "in groff version " . $at_at{'GROFF_VERSION'};
  exit 0;
} # version()


1;
########################################################################
### Emacs settings
# Local Variables:
# mode: CPerl
# End:
