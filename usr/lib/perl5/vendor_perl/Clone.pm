package Clone;

use strict;

require Exporter;
use XSLoader ();

our @ISA       = qw(Exporter);
our @EXPORT;
our @EXPORT_OK = qw( clone );

our $VERSION = '0.51';
our $WARN    = 1;

XSLoader::load('Clone', $VERSION);

1;
__END__

=head1 NAME

Clone - recursively copy Perl datatypes

=for html
<a href="https://github.com/garu/Clone/actions/workflows/test.yml"><img src="https://github.com/garu/Clone/actions/workflows/test.yml/badge.svg" alt="Build Status"></a>
<a href="https://metacpan.org/pod/Clone"><img src="https://badge.fury.io/pl/Clone.svg" alt="CPAN version"></a>

=head1 SYNOPSIS

    use Clone 'clone';

    my $data = {
       set => [ 1 .. 50 ],
       foo => {
           answer => 42,
           object => SomeObject->new,
       },
    };

    my $cloned_data = clone($data);

    $cloned_data->{foo}{answer} = 1;
    print $cloned_data->{foo}{answer};  # '1'
    print $data->{foo}{answer};         # '42'

You can also add it to your class:

    package Foo;
    use parent 'Clone';
    sub new { bless {}, shift }

    package main;

    my $obj = Foo->new;
    my $copy = $obj->clone;

=head1 DESCRIPTION

This module provides a C<clone()> method which makes recursive
copies of nested hash, array, scalar and reference types,
including tied variables and objects.

C<clone()> takes a scalar argument and duplicates it. To duplicate lists,
arrays or hashes, pass them in by reference, e.g.

    my $copy = clone (\@array);

    # or

    my %copy = %{ clone (\%hash) };

=head1 EXAMPLES

=head2 Cloning Blessed Objects

    package Person;
    sub new {
        my ($class, $name) = @_;
        bless { name => $name, friends => [] }, $class;
    }

    package main;
    use Clone 'clone';

    my $person = Person->new('Alice');
    my $clone = clone($person);

    # $clone is a separate object with the same data
    push @{$person->{friends}}, 'Bob';
    print scalar @{$clone->{friends}};  # 0

=head2 Handling Circular References

Clone properly handles circular references, preventing infinite loops:

    my $a = { name => 'A' };
    my $b = { name => 'B', ref => $a };
    $a->{ref} = $b;  # circular reference

    my $clone = clone($a);
    # Circular structure is preserved in the clone

=head2 Cloning Weakened References

    use Scalar::Util 'weaken';

    my $obj = { data => 'important' };
    my $container = { strong => $obj, weak => $obj };
    weaken($container->{weak});

    my $clone = clone($container);
    # Both strong and weak references are preserved correctly

=head2 Cloning Tied Variables

    use Tie::Hash;
    tie my %hash, 'Tie::StdHash';
    %hash = (a => 1, b => 2);

    my $clone = clone(\%hash);
    # The tied behavior is preserved in the clone

=head1 LIMITATIONS

=over 4

=item * Maximum Recursion Depth

Clone uses a recursion depth counter to prevent stack overflow.
The default limit is 4000 rdepth units on Linux/macOS and 2000 on
Windows/Cygwin. Each nesting level consumes approximately 2 rdepth
units, so the effective limits are roughly 2000 nesting levels on
Linux/macOS and 1000 on Windows/Cygwin.

When the limit is exceeded, Clone switches to an iterative fallback
that preserves deep-copy semantics without stack overflow. This
covers arrays, hashes, and all reference types (including deeply
nested scalar references). The fallback drives nested containers
through a heap-allocated work queue, so its C stack usage does not
grow with nesting depth whatever the shape of the data.

Non-clonable types (globs, code references, formats, IO handles)
are always shared regardless of depth. Encountering one directly as
a container element past the depth limit also emits a warning (one
reached through a reference is shared silently, as at any depth).
To silence it:

    $Clone::WARN = 0;

You can override the depth limit by passing it as the second argument
to C<clone()>:

    my $copy = clone($data, 8000);  # allow deeper recursion

=item * Filehandles and IO Objects

Filehandles and IO objects are not deep-copied. The clone shares the
same underlying filehandle object as the original (reference count is
incremented). For DBI database handles, Clone skips opaque XS magic
to avoid dangling pointers, but the resulting clone should not be used
as a database handle.

=item * Code References

Code references (subroutines) are cloned by reference, not by value.
The cloned coderef points to the same subroutine as the original.

=item * Thread Safety

Clone is not explicitly thread-safe. Use appropriate synchronization
when cloning data structures across threads.

=back

=head1 PERFORMANCE

Clone is implemented in C using Perl's XS interface, making it very fast
for most use cases.

=over 4

=item * When to use Clone

Clone is optimized for speed and works best with:

=over 4

=item * Shallow to medium-depth structures (3 levels or fewer)

=item * Data structures that need fast cloning in hot code paths

=item * Structures containing blessed objects and tied variables

=back

=item * When to use Storable::dclone

L<Storable>'s C<dclone()> may be faster for:

=over 4

=item * Very deep structures (4+ levels)

=item * When you need serialization features

=back

=back

Benchmarking your specific use case is recommended for performance-critical
applications.

=head1 CAVEATS

=over 4

=item * Cloned objects are deep copies

Changes to the clone do not affect the original, and vice versa. This
includes nested references and objects.

=item * Object internals

While Clone handles most blessed objects correctly, objects with XS
components or complex internal state may not clone as expected. Test
thoroughly with your specific object types.

=item * Memory usage

Cloning large data structures creates a complete copy in memory. Ensure
you have sufficient memory available.

=back

=head1 SEE ALSO

L<Storable>'s C<dclone()> is a flexible solution for cloning variables,
albeit slower for average-sized data structures. Simple
and naive benchmarks show that Clone is faster for data structures
with 3 or fewer levels, while C<dclone()> can be faster for structures
4 or more levels deep.

Other modules that may be of interest:

L<Clone::PP> - Pure Perl implementation of Clone

L<Scalar::Util> - For C<weaken()> and other scalar utilities

L<Data::Dumper> - For debugging and inspecting data structures

=head1 SUPPORT

=over 4

=item * Bug Reports and Feature Requests

Please report bugs on GitHub: L<https://github.com/garu/Clone/issues>

=item * Source Code

The source code is available on GitHub: L<https://github.com/garu/Clone>

=back

=head1 COPYRIGHT

Copyright 2001-2026 Ray Finch. All Rights Reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 AUTHOR

Ray Finch C<< <rdf@cpan.org> >>

Breno G. de Oliveira C<< <garu@cpan.org> >>,
Nicolas Rochelemagne C<< <atoomic@cpan.org> >>
and
Florian Ragwitz C<< <rafl@debian.org> >> perform routine maintenance
releases since 2012.

=cut
