# tp/Texinfo/ModulePath.pm.  Generated from ModulePath.pm.in by configure.
#
# Add directories to @INC, Perl's module search path, to find modules,
# either in the source or build directories.

package Texinfo::ModulePath;
use File::Spec;

BEGIN {

} # end BEGIN

# If $LIB_DIR and $LIBEXEC_DIR are given, use them to add directories
# to @INC (likely the installation directories), otherwise use 'top_srcdir'
# and 'top_builddir' environment variables.
sub init (;$$) {
  my $lib_dir = shift;
  my $libexec_dir = shift;

  if (defined($ENV{'top_builddir'}) and !defined($ENV{'top_srcdir'})) {
    $ENV{'top_srcdir'} = $ENV{'top_builddir'};
  }
   
  if (!$lib_dir) {
    if (defined($ENV{'top_srcdir'})) {
      # For Texinfo::Parser and the rest.
      unshift @INC, File::Spec->catdir($ENV{'top_srcdir'}, 'tp');

      $lib_dir = File::Spec->catdir($ENV{'top_srcdir'}, 'tp', 'maintain');
    }
  }

  # module using values from configure
  if (defined($lib_dir)) {
    #warn "lib dir is $lib_dir\n";

    unshift @INC, $lib_dir;

    # '@USE_EXTERNAL_LIBINTL @ and similar are substituted
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

  if (!$libexec_dir) {
    # XSParagraph.la is generated in the build directory.
    if (defined($ENV{'top_builddir'})) {
      $libexec_dir = File::Spec->catdir($ENV{'top_builddir'}, 'tp',
        'Texinfo', 'Convert', 'XSParagraph');
    }
  }

  if (defined($libexec_dir)) {
    unshift @INC, (File::Spec->catdir($libexec_dir));
  }
}

1;
