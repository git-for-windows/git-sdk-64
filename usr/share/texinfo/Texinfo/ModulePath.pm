# @configure_input@
#
# Add directories to @INC, Perl's module search path, to find modules,
# either in the source or build directories.

package Texinfo::ModulePath;

use vars qw($VERSION);

$VERSION = '6.3dev';

use File::Basename;
use File::Spec;

# Used as a flag to say whether to look for data files in uninstalled 
# locations.
our $texinfo_uninstalled = 0;

# Pathname of the tp/ build directory.  Used to find the locale
# data.
our $builddir = '';

# If $LIB_DIR and $LIBEXEC_DIR are given,
# (likely the installation directories)
# use them to add directories
# to @INC.
#
# LIB_DIR is for bundled libraries.
# LIBEXEC_DIR is for XS modules.
#
# otherwise use 'top_srcdir'
# and 'top_builddir' environment variables.
sub init {
  my $lib_dir = shift;
  my $libexec_dir = shift;
  my %named_args = @_;

  if (!$named_args{'installed'}) {
    $texinfo_uninstalled = 1;

    my $top_srcdir;
    if ($ENV{'top_srcdir'}) {
      $top_srcdir = $ENV{'top_srcdir'};
    } elsif (defined $named_args{'updirs'}) {
      my ($real_command_name, $command_directory, $command_suffix) 
              = fileparse($0, '.pl');
      my $updir = File::Spec->updir();

      # e.g. tp/t -> tp/t/../.. for 'updirs' = 2.
      my $count = $named_args{'updirs'};
      $top_srcdir = $command_directory;
      while ($count-- > 0) {
        $top_srcdir = File::Spec->catdir($top_srcdir, $updir);
      }
    }
     
    if (defined($top_srcdir)) {
      # For Texinfo::Parser and the rest.
      unshift @INC, File::Spec->catdir($top_srcdir, 'tp');

      $lib_dir = File::Spec->catdir($top_srcdir, 'tp', 'maintain');
    }

    # Find XS modules in the build directory
    my $top_builddir;
    if (defined($ENV{'top_builddir'})) {
      $top_builddir = $ENV{'top_builddir'};
    } else {
      $top_builddir = $top_srcdir;
    }
    if (defined($top_builddir)) {
      unshift @INC, File::Spec->catdir($top_builddir, 'tp',
        'Texinfo', 'XS');
      unshift @INC, File::Spec->catdir($top_builddir, 'tp',
        'Texinfo', 'XS', 'parsetexi');
    }

    $builddir = File::Spec->catdir($top_builddir, 'tp');
  }

  if (defined($lib_dir)) {
    unshift @INC, $lib_dir;

    # '@USE_EXTERNAL_LIBINTL @' and similar are substituted
    if ('no' ne 'yes') {
      unshift @INC, (File::Spec->catdir($lib_dir, 'lib', 'libintl-perl', 'lib'));
    }
    if ('no' ne 'yes') {
      unshift @INC, (File::Spec->catdir($lib_dir, 'lib', 'Unicode-EastAsianWidth', 'lib'));
    }
    if ('no' ne 'yes') {
      unshift @INC, (File::Spec->catdir($lib_dir, 'lib', 'Text-Unidecode', 'lib'));
    }
  }

  if (defined($libexec_dir)) {
    unshift @INC, $libexec_dir;
  }

  unshift @INC, sub { 
    my ($coderef, $filename) = @_;
    if ($filename eq 'Texinfo/Parser.pm') {
      my $replacement;
      if ($ENV{TEXINFO_XS_PARSER}) {
        $replacement = 'Texinfo/XS/parsetexi/Parsetexi.pm';
      } else {
        $replacement = 'Texinfo/ParserNonXS.pm';
      }
      foreach my $prefix (@INC) {
        if (ref($prefix)) {
          next;
        }
        my $realfilename = File::Spec->catdir($prefix, $replacement);
        if (-f $realfilename) {
          my $fh;
          open ($fh, '<', $realfilename);
          if ($fh) {
            $INC{$filename} = $realfilename;
            return $fh;
          }
        }
      }
    }
    return;
  };
}

sub import { 
  my $class = shift;
  goto &init;
}

1;
