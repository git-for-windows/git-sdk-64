package YAML::Syck;

# See documentation after the __END__ mark.

use strict;

our ( $Headless, $SingleQuote, $ImplicitBinary, $ImplicitTyping, $ImplicitUnicode, $UseCode, $LoadCode, $DumpCode, $DeparseObject, $MaxDepth );

use 5.006;
use Exporter;
use XSLoader ();

our $VERSION   = '1.46';
our @EXPORT    = qw( Dump Load DumpFile LoadFile );
our @EXPORT_OK = qw( DumpInto LoadBytes LoadUTF8 DumpBytes DumpUTF8 );
our @ISA       = qw( Exporter );

our $SortKeys    = 1;
our $LoadBlessed = 0;

XSLoader::load( 'YAML::Syck', $VERSION );

use constant QR_MAP => {
    ''   => sub { qr{$_[0]} },
    x    => sub { qr{$_[0]}x },
    i    => sub { qr{$_[0]}i },
    s    => sub { qr{$_[0]}s },
    m    => sub { qr{$_[0]}m },
    ix   => sub { qr{$_[0]}ix },
    sx   => sub { qr{$_[0]}sx },
    mx   => sub { qr{$_[0]}mx },
    si   => sub { qr{$_[0]}si },
    mi   => sub { qr{$_[0]}mi },
    ms   => sub { qr{$_[0]}sm },
    six  => sub { qr{$_[0]}six },
    mix  => sub { qr{$_[0]}mix },
    msx  => sub { qr{$_[0]}msx },
    msi  => sub { qr{$_[0]}msi },
    msix => sub { qr{$_[0]}msix },
};

sub __qr_helper {
    if ( $_[0] =~ /\A  \(\?  \^?  ([ixsm]*)  (?:-  (?:[ixsm]*))?  : (.*) \)  \z/x ) {
        my $sub = QR_MAP()->{$1} || QR_MAP()->{''};
        &$sub($2);
    }
    else {
        qr/$_[0]/;
    }
}

sub Dump {
    $#_
      ? join( '', map { YAML::Syck::DumpYAML($_) } @_ )
      : YAML::Syck::DumpYAML( $_[0] );
}

sub Load {
    if (wantarray) {
        return unless defined $_[0] && length $_[0];
        my ($rv) = YAML::Syck::LoadYAML( $_[0] );
        return unless defined $rv && ref $rv eq 'ARRAY';
        @{$rv};
    }
    else {
        @_ = $_[0];
        goto &YAML::Syck::LoadYAML;
    }
}

sub _is_glob {
    my $h = shift;

    return 1 if ( ref($h) eq 'GLOB' );
    return 1 if ( ref( \$h ) eq 'GLOB' );
    return 1 if ( index( ref($h), 'IO::' ) == 0 );
    return 1 if ( ref($h) && UNIVERSAL::isa($h, 'IO::Handle') );

    return;
}

sub DumpFile {
    my $file = shift;
    if ( _is_glob($file) ) {
        if ( tied(*$file) ) {
            # Tied filehandles (IO::String, IO::Scalar, etc.) don't support
            # C-level PerlIO_write. Fall back to Perl-level print.
            for (@_) {
                print $file YAML::Syck::Dump($_)
                    or die "Error writing to filehandle $file: $!\n";
            }
        }
        else {
            for (@_) {
                my $err = YAML::Syck::DumpYAMLFile( $_, $file );
                if ($err) {
                    $! = 0 + $err;
                    die "Error writing to filehandle $file: $!\n";
                }
            }
        }
    }
    else {
        open( my $fh, '>', $file ) or die "Cannot write to $file: $!";
        for (@_) {
            my $err = YAML::Syck::DumpYAMLFile( $_, $fh );
            if ($err) {
                $! = 0 + $err;
                die "Error writing to file $file: $!\n";
            }
        }
        close $fh
          or die "Error writing to file $file: $!\n";
    }
    return 1;
}

sub LoadFile {
    my $file = shift;
    if ( _is_glob($file) ) {
        Load(
            do { local $/; <$file> }
        );
    }
    else {
        if ( !-e $file || -z $file ) {
            die("'$file' is empty or non-existent");
        }
        open( my $fh, '<', $file ) or die "Cannot read from $file: $!";
        Load(
            do { local $/; <$fh> }
        );
    }
}

sub LoadBytes {
    my ($str) = @_;
    utf8::downgrade($str);
    if (wantarray) {
        return unless defined $str && length $str;
        my ($rv) = YAML::Syck::LoadYAML($str);
        return unless defined $rv && ref $rv eq 'ARRAY';
        return @{$rv};
    }
    return YAML::Syck::LoadYAML($str);
}

sub LoadUTF8 {
    my ($str) = @_;
    local $YAML::Syck::ImplicitUnicode = 1;
    if (wantarray) {
        return unless defined $str && length $str;
        my ($rv) = YAML::Syck::LoadYAML($str);
        return unless defined $rv && ref $rv eq 'ARRAY';
        return @{$rv};
    }
    return YAML::Syck::LoadYAML($str);
}

sub DumpBytes {
    my $result = $#_
      ? join( '', map { YAML::Syck::DumpYAML($_) } @_ )
      : YAML::Syck::DumpYAML( $_[0] );
    utf8::encode($result) if utf8::is_utf8($result);
    return $result;
}

sub DumpUTF8 {
    local $YAML::Syck::ImplicitUnicode = 1;
    $#_
      ? join( '', map { YAML::Syck::DumpYAML($_) } @_ )
      : YAML::Syck::DumpYAML( $_[0] );
}

sub DumpInto {
    my $bufref = shift;
    ( ref $bufref ) or die "DumpInto not given reference to output buffer\n";
    YAML::Syck::DumpYAMLInto( $_, $bufref ) for @_;
    1;
}

1;

__END__
=pod

=head1 NAME 

YAML::Syck - Fast, lightweight YAML loader and dumper

=head1 SYNOPSIS

    use YAML::Syck;

    # Set this for interoperability with other YAML/Syck bindings:
    # e.g. Load('Yes') becomes 1 and Load('No') becomes ''.
    $YAML::Syck::ImplicitTyping = 1;

    $data = Load($yaml);
    $yaml = Dump($data);

    # $file can be an IO object, or a filename
    $data = LoadFile($file);
    DumpFile($file, $data);

    # A string with multiple YAML streams in it
    $yaml = Dump(@data);
    @data = Load($yaml);

    # Dumping into a pre-existing output buffer
    my $yaml;
    DumpInto(\$yaml, @data);

=head1 DESCRIPTION

This module provides a Perl interface to the B<libsyck> data serialization
library.  It exports the C<Dump> and C<Load> functions for converting
Perl data structures to YAML strings, and the other way around.

B<NOTE>: If you are working with other language's YAML/Syck bindings
(such as Ruby), please set C<$YAML::Syck::ImplicitTyping> to C<1> before
calling the C<Load>/C<Dump> functions.  The default setting is for
preserving backward-compatibility with C<YAML.pm>.

=head1 Differences Between YAML::Syck and YAML

=head2 Error handling

Some calls are designed to die rather than returning YAML. You should wrap
your calls in eval to assure you do not get unexpected results.

=head1 FLAGS

=head2 $YAML::Syck::MaxDepth

Maximum nesting depth for C<Dump>.  Defaults to 512.  If a data structure
is nested deeper than this limit, C<Dump> will C<croak> instead of
overflowing the C stack.  Increase this if you legitimately need to
serialize very deeply nested structures.

=head2 $YAML::Syck::Headless

Defaults to false.  Setting this to a true value will make C<Dump> omit the
leading C<---\n> marker.

=head2 $YAML::Syck::SortKeys

Defaults to true (1).  Setting this to a true value will make C<Dump> sort
hash keys.

=head2 $YAML::Syck::SingleQuote

Defaults to false.  Setting this to a true value will make C<Dump> always emit
single quotes instead of bare strings.

=head2 $YAML::Syck::ImplicitTyping

Defaults to false.  Setting this to a true value will make C<Load> recognize
various implicit types in YAML, such as unquoted C<true>, C<false>, as well as
integers and floating-point numbers.  Otherwise, only C<~> is recognized to
be C<undef>.

=head2 $YAML::Syck::ImplicitUnicode

Defaults to false.  For Perl 5.8.0 or later, setting this to a true value will
make C<Load> set Unicode flag on for every string that contains valid UTF8
sequences, and make C<Dump> return a unicode string.

Regardless of this flag, Unicode strings are dumped verbatim without escaping;
byte strings with high-bit set will be dumped with backslash escaping.

However, because YAML does not distinguish between these two kinds of strings,
so this flag will affect loading of both variants of strings.

If you want to use LoadFile or DumpFile with unicode, you are required to open
your own file in order to assure it's UTF8 encoded:

  open(my $fh, ">:encoding(UTF-8)", "out.yml");
  DumpFile($fh, $hashref);

=head2 $YAML::Syck::ImplicitBinary

Defaults to false.  For Perl 5.8.0 or later, setting this to a true value will
make C<Dump> generate Base64-encoded C<!!binary> data for all non-Unicode
scalars containing high-bit bytes.

=head2 $YAML::Syck::UseCode / $YAML::Syck::LoadCode / $YAML::Syck::DumpCode

These flags control whether or not to try and eval/deparse perl source code;
each of them defaults to false.

Setting C<$YAML::Syck::UseCode> to a true value is equivalent to setting
both C<$YAML::Syck::LoadCode> and C<$YAML::Syck::DumpCode> to true.

=head2 $YAML::Syck::LoadBlessed

Defaults to false. Setting to true will allow YAML::Syck to bless objects as it
imports objects. This default changed in 1.32.

You can create any kind of object with YAML. The creation itself is not the
critical part. If the class has a DESTROY method, it will be called once the
object is deleted. An example with File::Temp removing files can be found at
L<https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=862373|https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=862373>

=head1 ENCODING-EXPLICIT FUNCTIONS

The standard C<Load> and C<Dump> functions rely on Perl's internal string
representation, which can lead to surprising results when a string has been
C<utf8::upgrade>'d.  The following functions make the encoding explicit:

=head2 LoadBytes($yaml_string)

Treats the input as a byte (octet) string.  If the string has been internally
upgraded to UTF-8 by Perl, it is downgraded first so the parser sees the
original bytes.  Croaks if the string contains characters above 0xFF.

    use YAML::Syck qw(LoadBytes);
    my $data = LoadBytes($yaml_bytes);

=head2 LoadUTF8($yaml_string)

Treats the input as UTF-8.  The parsed values will have Perl's UTF-8 flag
set (equivalent to C<local $YAML::Syck::ImplicitUnicode = 1>).  Works
correctly regardless of whether the input string is upgraded or not.

    use YAML::Syck qw(LoadUTF8);
    my $data = LoadUTF8($yaml_utf8);

=head2 DumpBytes($data, ...)

Dumps data to a YAML byte string.  The return value will never have the
UTF-8 flag set; any UTF-8 content is encoded to raw bytes.

    use YAML::Syck qw(DumpBytes);
    my $yaml = DumpBytes($data);

=head2 DumpUTF8($data, ...)

Dumps data to a YAML string with the UTF-8 flag set (equivalent to
C<local $YAML::Syck::ImplicitUnicode = 1>).

    use YAML::Syck qw(DumpUTF8);
    my $yaml = DumpUTF8($data);

=head1 BUGS

Dumping Glob/IO values do not work yet.

Dumping into tied (or other magic variables) with C<DumpInto> might not work
properly in all cases.

=head1 CAVEATS

This module implements the YAML 1.0 spec.  To deal with data in YAML 1.1, 
please use the C<YAML::XS> module instead.

The current implementation bundles libsyck source code; if your system has a
site-wide shared libsyck, it will I<not> be used.

Tag names such as C<!!perl/hash:Foo> is blessed into the package C<Foo>, but
the C<!hs/foo> and C<!!hs/Foo> tags are blessed into C<hs::Foo>.  Note that
this holds true even if the tag contains non-word characters; for example,
C<!haskell.org/Foo> is blessed into C<haskell.org::Foo>.  Please use
L<Class::Rebless> to cast it into other user-defined packages. You can also
set the LoadBlessed flag false to disable all blessing.

This module has L<a lot of known
issues|https://rt.cpan.org/Public/Dist/Display.html?Name=YAML-Syck>
and has only been semi-actively maintained since 2007. If you
encounter an issue with it probably won't be fixed unless you L<offer
up a patch|http://github.com/toddr/YAML-Syck> in Git that's ready for
release.

There are still good reasons to use this module, such as better
interoperability with other syck wrappers (like Ruby's), or some edge
case of YAML's syntax that it handles better. It'll probably work
perfectly for you, but if it doesn't you may want to look at
L<YAML::XS>, or perhaps at looking another serialization format like
L<JSON>.

=head1 SEE ALSO

L<YAML>, L<JSON::Syck>

L<http://www.yaml.org/>

=head1 AUTHORS

Audrey Tang E<lt>cpan@audreyt.orgE<gt>

=head1 COPYRIGHT

Copyright 2005-2009 by Audrey Tang E<lt>cpan@audreyt.orgE<gt>.

This software is released under the MIT license cited below.

The F<libsyck> code bundled with this library is released by
"why the lucky stiff", under a BSD-style license.  See the F<COPYING>
file for details.

=head2 The "MIT" License

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.

=cut
