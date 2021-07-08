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

package Texinfo::XSLoader;

use DynaLoader;

use 5.00405;
use strict;
use warnings;

our $TEXINFO_XS;

our $VERSION = '6.8';

our $disable_XS;

# For verbose information about what's being done
sub _debug($) {
  if ($TEXINFO_XS eq 'debug') {
    my $msg = shift;
    warn $msg . "\n";
  }
}

# For messages to say that XS module couldn't be loaded
sub _fatal($) {
  if ($TEXINFO_XS eq 'debug'
      or $TEXINFO_XS eq 'required'
      or $TEXINFO_XS eq 'warn') {
    my $msg = shift;
    warn $msg . "\n";
  }
}

# We look for the .la and .so files in @INC because this allows us to override
# which modules are used using -I flags to "perl".
sub _find_file($) {
  my $file = shift;
  for my $dir (@INC) {
    next if ref($dir);
    _debug "checking $dir/$file";
    if (-f "$dir/$file") {
      _debug "found $dir/$file";
      return ($dir, "$dir/$file");
    }
  }
  return undef;
}
 
# Load either from XS implementation in $MODULE along with Perl file 
# $PERL_EXTRA_FILE, or non-XS implementation $FALLBACK_MODULE.
# $MODULE_NAME is the name of a Libtool file used for
# loading the XS subroutines.
# $INTERFACE_VERSION is a module interface number, to be changed when the XS 
# interface changes.  
sub init {
 my ($module,
     $fallback_module,
     $module_name,
     $perl_extra_file,
     $interface_version,
     $warning_message,
     $fatal_message
   ) = @_;
 
 # Possible values for TEXINFO_XS environment variable:
 #
 # TEXINFO_XS=omit         # don't try loading xs at all
 # TEXINFO_XS=default      # try xs, libtool, silent fallback
 # TEXINFO_XS=warn         # try xs, libtool warn on failure
 # TEXINFO_XS=required     # abort if not loadable, no fallback
 # TEXINFO_XS=debug        # voluminuous debugging
 #
 # Other values are treated at the moment as 'default'.
 
 $TEXINFO_XS = $ENV{'TEXINFO_XS'};
 if (!defined($TEXINFO_XS)) {
   $TEXINFO_XS = '';
 }
 
 if ($TEXINFO_XS eq 'omit') {
   # Don't try to use the XS module
   goto FALLBACK;
 }
 
 if ($disable_XS) {
   _fatal "use of XS modules was disabled when Texinfo was built";
   goto FALLBACK;
 }

 if ($warning_message) {
   _debug $warning_message;
 }

 if ($fatal_message) {
   _fatal $fatal_message;
   goto FALLBACK;
 }

 if (!$module) {
   goto FALLBACK;
 }
 
 my ($libtool_dir, $libtool_archive) = _find_file("$module_name.la");
 if (!$libtool_archive) {
   if ($TEXINFO_XS eq 'libtool') {
     _fatal "$module_name: couldn't find Libtool archive file";
     goto FALLBACK;
   }
   _debug "$module_name: couldn't find Libtool archive file";
   goto FALLBACK;
 }
 
 my $dlname = undef;
 my $dlpath = undef;
 
 my $fh;
 open $fh, $libtool_archive;
 if (!$fh) {
   _fatal "$module_name: couldn't open Libtool archive file";
   goto FALLBACK;
 }
 
 # Look for the line in XS*.la giving the name of the loadable object.
 while (my $line = <$fh>) {
   if ($line =~ /^\s*dlname\s*=\s*'([^']+)'\s$/) {
     $dlname = $1;
     last;
   }
 }
 if (!$dlname) {
   _fatal "$module_name: couldn't find name of shared object";
   goto FALLBACK;
 }
 
 # The *.so file is under .libs in the source directory.
 push @DynaLoader::dl_library_path, $libtool_dir;
 push @DynaLoader::dl_library_path, "$libtool_dir/.libs";
 
 $dlpath = DynaLoader::dl_findfile($dlname);
 if (!$dlpath) {
   _fatal "$module_name: couldn't find $dlname";
   goto FALLBACK;
 }
 
  #my $flags = dl_load_flags $module; # This is 0 in DynaLoader
  my $flags = 0;
  my $libref = DynaLoader::dl_load_file($dlpath, $flags);
  if (!$libref) {
    _fatal "$module_name: couldn't load file $dlpath";
    goto FALLBACK;
  }
  _debug "$dlpath loaded";
  my @undefined_symbols = DynaLoader::dl_undef_symbols();
  if ($#undefined_symbols+1 != 0) {
    _fatal "$module_name: still have undefined symbols after dl_load_file";
  }
  my $bootname = "boot_$module";
  $bootname =~ s/:/_/g;
  _debug "looking for $bootname";
  my $symref = DynaLoader::dl_find_symbol($libref, $bootname);
  if (!$symref) {
    _fatal "$module_name: couldn't find $bootname symbol";
    goto FALLBACK;
  }
  _debug "trying to call $bootname...";
  my $boot_fn = DynaLoader::dl_install_xsub("${module}::bootstrap",
                                                  $symref, $dlname);
  
  if (!$boot_fn) {
    _fatal "$module_name: couldn't bootstrap";
    goto FALLBACK;
  }
  _debug "  ...succeeded";
  
  push @DynaLoader::dl_shared_objects, $dlpath; # record files loaded
  
  # This is the module bootstrap function, which causes all the other
  # functions (XSUB's) provided by the module to become available to
  # be called from Perl code.
  &$boot_fn($module, $interface_version);
  
  # This makes it easier to refer to packages and symbols by name.
  no strict 'refs';
  
  if (defined &{"${module}::init"}
      and !&{"${module}::init"} ($Texinfo::ModulePath::texinfo_uninstalled,
                                 $Texinfo::ModulePath::builddir)) {
    _fatal "$module_name: error initializing";
    goto FALLBACK;
  }
  
  if ($perl_extra_file) {
    eval "require $perl_extra_file";
  }
  
  return $module;
  
FALLBACK:
  if ($TEXINFO_XS eq 'required') {
    die "unset the TEXINFO_XS environment variable to use the "
       ."pure Perl modules\n";
  } elsif ($TEXINFO_XS eq 'warn' or $TEXINFO_XS eq 'debug') {
    warn "falling back to pure Perl module $fallback_module\n";
  }
  if (!defined $fallback_module) {
    warn "no fallback module for $module\n";
    die "unset the TEXINFO_XS and TEXINFO_XS_PARSER environment variables "
       ."to use the pure Perl modules\n";
  }

  # Fall back to using the Perl code.
  # Use eval here to interpret :: properly in module name.
  eval "require $fallback_module";
  if ($@) {
    warn();
    die "Error loading $fallback_module\n";
  }

  return  $fallback_module;
} # end init

# Override subroutine $TARGET with $SOURCE.
sub override {
  my ($target, $source) = @_;

  _debug "attempting to override $target with $source...";

  no strict 'refs'; # access modules and symbols by name.
  no warnings 'redefine'; # do not warn about redefining a function.

  if (defined &{"${source}"}) {
    *{"${target}"} = \&{"${source}"};
    _debug "  ...succeeded";
  } else {
    _debug "  ...failed";
  }
}


1;
__END__
