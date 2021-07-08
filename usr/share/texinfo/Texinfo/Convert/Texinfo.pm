# Texinfo.pm: output a Texinfo tree as Texinfo.
#
# Copyright 2010-2018 Free Software Foundation, Inc.
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
# 
# Original author: Patrice Dumas <pertusus@free.fr>
# Parts (also from Patrice Dumas) come from texi2html.pl or texi2html.init.

package Texinfo::Convert::Texinfo;

use 5.00405;
use strict;

# commands definitions
use Texinfo::Common;

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

%EXPORT_TAGS = ( 'all' => [ qw(
  convert
  node_extra_to_texi
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '6.8';

my %misc_commands            = %Texinfo::Common::misc_commands;
my %brace_commands           = %Texinfo::Common::brace_commands;    
my %block_commands           = %Texinfo::Common::block_commands;    
my %def_commands             = %Texinfo::Common::def_commands;    

my @ignored_types = ('spaces_inserted', 'bracketed_inserted',
'command_as_argument_inserted');
my %ignored_types;
for my $a (@ignored_types) {
  $ignored_types{$a} = 1;
}

# Following subroutines deal with transforming a texinfo tree into texinfo
# text.  Should give the text that was used parsed, except for a few cases.

# expand a tree to the corresponding texinfo.
sub convert
{
  my $root = shift;

  die "convert: root undef\n" if (!defined($root));
  die "convert: bad root type (".ref($root).") $root\n" 
     if (ref($root) ne 'HASH');
  my $result = '';

  return '' if ($root->{'type'} and $ignored_types{$root->{'type'}});

  if (defined($root->{'text'})) {
    $result .= $root->{'text'};
  } else {
    if ($root->{'cmdname'} 
       or ($root->{'type'} and ($root->{'type'} eq 'def_line'
                                or $root->{'type'} eq 'menu_entry'
                                or $root->{'type'} eq 'menu_comment'))) {
      $result .= _expand_cmd_args_to_texi($root);
    }
    if ($root->{'type'}
        and ($root->{'type'} eq 'bracketed'
             or $root->{'type'} eq 'bracketed_def_content')) {
      $result .= '{';
      if ($root->{'extra'}
          and $root->{'extra'}->{'spaces_before_argument'}) {
         $result .= $root->{'extra'}->{'spaces_before_argument'};
      }
    }
    if (defined($root->{'contents'})) {
      foreach my $child (@{$root->{'contents'}}) {
        $result .= convert($child);
      }
    }
    if ($root->{'extra'} and $root->{'extra'}->{'spaces_after_argument'}) {
      $result .= $root->{'extra'}->{'spaces_after_argument'};
    }
    if ($root->{'extra'} and $root->{'extra'}->{'comment_at_end'}) {
      $result .= convert($root->{'extra'}->{'comment_at_end'});
    }
    $result .= '}' if ($root->{'type'}
                       and ($root->{'type'} eq 'bracketed'
                            or $root->{'type'} eq 'bracketed_def_content'));
    if ($root->{'cmdname'} and defined($block_commands{$root->{'cmdname'}})
        and $block_commands{$root->{'cmdname'}} eq 'raw') {
      $result .= '@end '.$root->{'cmdname'};
      $result .= "\n" if ($block_commands{$root->{'cmdname'}} ne 'raw');
    } 
  }
  return $result;
}

# used to put a node name in error messages.
sub node_extra_to_texi($)
{
  my $node = shift;
  my $result = '';
  if ($node->{'manual_content'}) {
    $result = '('.Texinfo::Convert::Texinfo::convert({'contents'
                                     => $node->{'manual_content'}}) .')';
  }
  if ($node->{'node_content'}) {
    $result .= Texinfo::Convert::Texinfo::convert ({'contents'
                                          => $node->{'node_content'}});
  }
  return $result;
}


# expand a command argument as texinfo.
sub _expand_cmd_args_to_texi {
  my $cmd = shift;

  my $cmdname = $cmd->{'cmdname'};
  $cmdname = '' if (!$cmd->{'cmdname'}); 
  my $result = '';
  $result = '@'.$cmdname if ($cmdname);

  # this is done here otherwise for some constructs, there are
  # no 'args', and so the space is never readded.
  if ($cmd->{'extra'} and exists ($cmd->{'extra'}->{'spaces'})) {
    $result .= $cmd->{'extra'}->{'spaces'};
  }
  # must be before the next condition
  if ($block_commands{$cmdname}
         and ($def_commands{$cmdname}
              or $block_commands{$cmdname} eq 'multitable')
         and $cmd->{'args'}) {
     $result .= $cmd->{'extra'}->{'spaces_before_argument'}
       if $cmd->{'extra'} and $cmd->{'extra'}->{'spaces_before_argument'};
     foreach my $arg (@{$cmd->{'args'}}) {
        $result .= convert($arg);
    }
  # for misc_commands with type special
  } elsif (($cmd->{'extra'} or $cmdname eq 'macro' or $cmdname eq 'rmacro') 
           and defined($cmd->{'extra'}->{'arg_line'})) {
    $result .= $cmd->{'extra'}->{'spaces_before_argument'}
      if $cmd->{'extra'} and $cmd->{'extra'}->{'spaces_before_argument'};
    $result .= $cmd->{'extra'}->{'arg_line'};
  } elsif (($block_commands{$cmdname} or $cmdname eq 'node')
            and defined($cmd->{'args'})) {
    $result .= $cmd->{'extra'}->{'spaces_before_argument'}
      if $cmd->{'extra'} and $cmd->{'extra'}->{'spaces_before_argument'};
    foreach my $arg (@{$cmd->{'args'}}) {
      next if $arg->{'type'} and $ignored_types{$arg->{'type'}};
      if ($arg->{'extra'} and $arg->{'extra'}->{'spaces_before_argument'}) {
        $result .= $arg->{'extra'}->{'spaces_before_argument'};
      }
      $result .= convert($arg);
      $result .= ',';
    }
    $result =~ s/,$//;
  } elsif (defined($cmd->{'args'})) {
    my $braces;
    $braces = 1 if ($cmd->{'args'}->[0]->{'type'} 
                    and ($cmd->{'args'}->[0]->{'type'} eq 'brace_command_arg'
                         or $cmd->{'args'}->[0]->{'type'} eq 'brace_command_context'));
    $result .= '{' if ($braces);
    if ($cmdname eq 'verb') {
      $result .= $cmd->{'extra'}->{'delimiter'};
    }
    if ($cmd->{'extra'}
        and $cmd->{'extra'}->{'spaces_before_argument'}) {
      $result .= $cmd->{'extra'}->{'spaces_before_argument'};
    }
    my $arg_nr = 0;
    foreach my $arg (@{$cmd->{'args'}}) {
      if (exists($brace_commands{$cmdname}) or ($cmd->{'type'} 
                    and $cmd->{'type'} eq 'definfoenclose_command')) {
        $result .= ',' if ($arg_nr);
        $arg_nr++;
      }
      if ($arg->{'extra'} and $arg->{'extra'}->{'spaces_before_argument'}) {
        $result .= $arg->{'extra'}->{'spaces_before_argument'};
      }
      $result .= convert($arg);
    }
    if ($cmdname eq 'verb') {
      $result .= $cmd->{'extra'}->{'delimiter'};
    }
    #die "Shouldn't have args: $cmdname\n";
    $result .= '}' if ($braces);
  } else {
    $result .= $cmd->{'extra'}->{'spaces_before_argument'}
      if $cmd->{'extra'} and $cmd->{'extra'}->{'spaces_before_argument'};
  }
  $result .= '{'.$cmd->{'type'}.'}' if ($cmdname eq 'value');
  return $result;
}

1;
__END__

=head1 NAME

Texinfo::Convert::Texinfo - Convert a Texinfo tree to Texinfo code

=head1 SYNOPSIS

  use Texinfo::Convert::Texinfo qw(convert);
  
  my $texinfo_text = convert($tree);

=head1 DESCRIPTION

Texinfo::Convert::Texinfo converts a Texinfo tree (described in 
L<Texinfo::Parser>) to Texinfo code.  If the Texinfo tree results from 
parsing some Texinfo document, The converted Texinfo code should be
exactly the same as the initial document, except that user defined @-macros 
and C<@value> are expanded, and some invalid code is discarded.

=head1 METHODS

=over

=item $texinfo_text = convert($tree)

Converts the Texinfo tree I<$tree> to Texinfo code.

=back

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=cut
