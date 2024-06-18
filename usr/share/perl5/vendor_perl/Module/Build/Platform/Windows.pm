package Module::Build::Platform::Windows;

use strict;
use warnings;
our $VERSION = '0.4234';
$VERSION = eval $VERSION;

use Config;
use File::Basename;
use File::Spec;

use Module::Build::Base;

our @ISA = qw(Module::Build::Base);


sub manpage_separator {
    return '.';
}

sub have_forkpipe { 0 }

sub _detildefy {
  my ($self, $value) = @_;
  $value =~ s,^~(?= [/\\] | $ ),$ENV{HOME},x
    if $ENV{HOME};
  return $value;
}

sub ACTION_realclean {
  my ($self) = @_;

  $self->SUPER::ACTION_realclean();

  my $basename = basename($0);
  $basename =~ s/(?:\.bat)?$//i;

  if ( lc $basename eq lc $self->build_script ) {
    if ( $self->build_bat ) {
      $self->log_verbose("Deleting $basename.bat\n");
      my $full_progname = $0;
      $full_progname =~ s/(?:\.bat)?$/.bat/i;

      # Voodoo required to have a batch file delete itself without error;
      # Syntax differs between 9x & NT: the later requires a null arg (???)
      require Win32;
      my $null_arg = (Win32::IsWinNT()) ? '""' : '';
      my $cmd = qq(start $null_arg /min "\%comspec\%" /c del "$full_progname");

      open(my $fh, '>>', "$basename.bat")
        or die "Can't create $basename.bat: $!";
      print $fh $cmd;
      close $fh ;
    } else {
      $self->delete_filetree($self->build_script . '.bat');
    }
  }
}

sub make_executable {
  my $self = shift;

  $self->SUPER::make_executable(@_);

  foreach my $script (@_) {

    # Native batch script
    if ( $script =~ /\.(bat|cmd)$/ ) {
      $self->SUPER::make_executable($script);
      next;

    # Perl script that needs to be wrapped in a batch script
    } else {
      my %opts = ();
      if ( $script eq $self->build_script ) {
        $opts{ntargs}    = q(-x -S %0 --build_bat %*);
        $opts{otherargs} = q(-x -S "%0" --build_bat %1 %2 %3 %4 %5 %6 %7 %8 %9);
      }

      my $out = eval {$self->pl2bat(in => $script, update => 1, %opts)};
      if ( $@ ) {
        $self->log_warn("WARNING: Unable to convert file '$script' to an executable script:\n$@");
      } else {
        $self->SUPER::make_executable($out);
      }
    }
  }
}

sub pl2bat {
  my $self = shift;
  my %opts = @_;
  require ExtUtils::PL2Bat;
  return ExtUtils::PL2Bat::pl2bat(%opts);
}


sub _quote_args {
  # Returns a string that can become [part of] a command line with
  # proper quoting so that the subprocess sees this same list of args.
  my ($self, @args) = @_;

  my @quoted;

  for (@args) {
    if ( /^[^\s*?!\$<>;|'"\[\]\{\}]+$/ ) {
      # Looks pretty safe
      push @quoted, $_;
    } else {
      # XXX this will obviously have to improve - is there already a
      # core module lying around that does proper quoting?
      s/"/\\"/g;
      push @quoted, qq("$_");
    }
  }

  return join " ", @quoted;
}


sub split_like_shell {
  # As it turns out, Windows command-parsing is very different from
  # Unix command-parsing.  Double-quotes mean different things,
  # backslashes don't necessarily mean escapes, and so on.  So we
  # can't use Text::ParseWords::shellwords() to break a command string
  # into words.  The algorithm below was bashed out by Randy and Ken
  # (mostly Randy), and there are a lot of regression tests, so we
  # should feel free to adjust if desired.

  (my $self, local $_) = @_;

  return @$_ if defined() && ref() eq 'ARRAY';

  my @argv;
  return @argv unless defined() && length();

  my $length = length;
  m/\G\s*/gc;

  ARGS: until ( pos == $length ) {
    my $quote_mode;
    my $arg = '';
    CHARS: until ( pos == $length ) {
      if ( m/\G((?:\\\\)+)(?=\\?(")?)/gc ) {
          if (defined $2) {
              $arg .= '\\' x (length($1) / 2);
          }
          else {
              $arg .= $1;
          }
      }
      elsif ( m/\G\\"/gc ) {
        $arg .= '"';
      }
      elsif ( m/\G"/gc ) {
        if ( $quote_mode && m/\G"/gc ) {
            $arg .= '"';
        }
        $quote_mode = !$quote_mode;
      }
      elsif ( !$quote_mode && m/\G\s+/gc ) {
        last;
      }
      elsif ( m/\G(.)/sgc ) {
        $arg .= $1;
      }
    }
    push @argv, $arg;
  }

  return @argv;
}


# system(@cmd) does not like having double-quotes in it on Windows.
# So we quote them and run it as a single command.
sub do_system {
  my ($self, @cmd) = @_;

  my $cmd = $self->_quote_args(@cmd);
  my $status = system($cmd);
  if ($status and $! =~ /Argument list too long/i) {
    my $env_entries = '';
    foreach (sort keys %ENV) { $env_entries .= "$_=>".length($ENV{$_})."; " }
    warn "'Argument list' was 'too long', env lengths are $env_entries";
  }
  return !$status;
}

# Copied from ExtUtils::MM_Win32
sub _maybe_command {
    my($self,$file) = @_;
    my @e = exists($ENV{'PATHEXT'})
          ? split(/;/, $ENV{PATHEXT})
	  : qw(.com .exe .bat .cmd);
    my $e = '';
    for (@e) { $e .= "\Q$_\E|" }
    chop $e;
    # see if file ends in one of the known extensions
    if ($file =~ /($e)$/i) {
	return $file if -e $file;
    }
    else {
	for (@e) {
	    return "$file$_" if -e "$file$_";
	}
    }
    return;
}


1;

__END__

=head1 NAME

Module::Build::Platform::Windows - Builder class for Windows platforms

=head1 DESCRIPTION

The sole purpose of this module is to inherit from
C<Module::Build::Base> and override a few methods.  Please see
L<Module::Build> for the docs.

=head1 AUTHOR

Ken Williams <kwilliams@cpan.org>, Randy W. Sims <RandyS@ThePierianSpring.org>

=head1 SEE ALSO

perl(1), Module::Build(3)

=cut
