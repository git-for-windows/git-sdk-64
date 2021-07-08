# Copyright 2014-2020 Free Software Foundation, Inc.
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

# File to be loaded in conjunction with Parsetexi.xs module

package Texinfo::Parser;

use 5.00405;
use strict;
use warnings;

require Exporter;

use Texinfo::Common;
use Texinfo::Encoding;
use Texinfo::Convert::NodeNameNormalization;
use Texinfo::Report;

our @ISA = qw(Exporter DynaLoader Texinfo::Report);
our %EXPORT_TAGS = ( 'all' => [ qw(
    parser
    parse_texi_text
    parse_texi_line
    parse_texi_file
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

sub get_conf($$)
{
  my $self = shift;
  my $var = shift;
  return $self->{$var};
}

my %parser_default_configuration =
  (%Texinfo::Common::default_parser_state_configuration,
   %Texinfo::Common::default_parser_customization_values);

use Storable qw(dclone); # standard in 5.007003

sub simple_parser {
  goto &parser;
}

# Stub for Texinfo::Parser::parser
sub parser (;$$)
{
  my $conf = shift;

  my $parser = dclone(\%parser_default_configuration);

  reset_parser ();
  if (defined($conf)) {
    foreach my $key (keys (%$conf)) {
      # Copy conf to parser object.  Not used in parser module itself
      # but some settings may be used elsewhere, especially in
      # Structuring.pm.
      if ($key ne 'values' and ref($conf->{$key})) {
        $parser->{$key} = dclone($conf->{$key});
      } else {
        $parser->{$key} = $conf->{$key};
      }

      if ($key eq 'include_directories') {
        foreach my $d (@{$conf->{'include_directories'}}) {
          add_include_directory ($d);
        }
      } elsif ($key eq 'values') {
	# This is used by Texinfo::Report::gdt for substituted values
	for my $v (keys %{$conf->{'values'}}) {
	  if (!ref($conf->{'values'}->{$v})) {
	    store_value ($v, $conf->{'values'}->{$v});
          } elsif (ref($conf->{'values'}->{$v}) eq 'HASH') {
            store_value ($v, "<<HASH VALUE>>");
	  } elsif (ref($conf->{'values'}->{$v}) eq 'ARRAY') {
	    store_value ($v, "<<ARRAY VALUE>>");
	  } else {
	    store_value ($v, "<<UNKNOWN VALUE>>");
	  }
	}
      } elsif ($key eq 'expanded_formats') {
        clear_expanded_formats ();

        for my $f (@{$conf->{$key}}) {
          add_expanded_format ($f);
        }
      } elsif ($key eq 'documentlanguage') {
        if (defined ($conf->{$key})) {
          set_documentlanguage ($conf->{$key});
        }
      } elsif ($key eq 'info') {
        if (defined($conf->{$key}->{'novalidate'})) { 
          set_novalidate($conf->{$key}->{'novalidate'});
        }
      } elsif ($key eq 'FORMAT_MENU') {
        if ($conf->{$key} eq 'menu') {
          conf_set_show_menu (1);
        } else {
          conf_set_show_menu (0);
        }
      } elsif ($key eq 'IGNORE_SPACE_AFTER_BRACED_COMMAND_NAME') {
        conf_set_IGNORE_SPACE_AFTER_BRACED_COMMAND_NAME ($conf->{$key});
      } elsif ($key eq 'CPP_LINE_DIRECTIVES') {
        conf_set_CPP_LINE_DIRECTIVES($conf->{$key});
      } elsif ($key eq 'DEBUG') {
        set_debug($conf->{$key}) if $conf->{'key'};
      } elsif ($key eq 'in_gdt'
               or $key eq 'ENABLE_ENCODING'
               or defined($Texinfo::Common::default_structure_customization_values{$key})) {
        # no action needed
      } else {
        warn "ignoring parser configuration value \"$key\"\n";
      }
    }
  }

  bless $parser;

  $parser->Texinfo::Report::new;

  return $parser;
}

# Record any @menu elements under $root in the 'menus' array of $node.
sub _find_menus_of_node {
  my $node = shift;
  my $root = shift;

  if ($root->{'contents'}) {
    my $contents = $root->{'contents'};
    foreach my $child (@{$contents}) {
      if ($child->{'cmdname'} and $child->{'cmdname'} eq 'menu') {
        push @{$node->{'menus'}}, $child;
      }
    }
  }
}

# Set 'menus' array for each node.  This accounts for malformed input where
# the number of sectioning commands between @node and @menu is not exactly 1.
sub _complete_node_menus {
  my $self = shift;
  my $root = shift;

  if (!defined $self->{'nodes'}) {
    $self->{'nodes'} = [];
  }
  my $node;
  foreach my $child (@{$root->{'contents'}}) {
    if ($child->{'cmdname'} and $child->{'cmdname'} eq 'node') {
      $node = $child;
    }
    _find_menus_of_node ($node, $child) unless !defined $node;
  }
}

sub get_parser_info {
  my $self = shift;

  my ($TARGETS, $INTL_XREFS, $FLOATS,
      $INDEX_NAMES, $ERRORS, $GLOBAL_INFO, $GLOBAL_INFO2);

  $TARGETS = build_label_list ();
  $INTL_XREFS = build_internal_xref_list ();
  $FLOATS = build_float_list ();

  $INDEX_NAMES = build_index_data ();
  $self->{'index_names'} = $INDEX_NAMES;

  $GLOBAL_INFO = build_global_info ();
  $GLOBAL_INFO2 = build_global_info2 ();

  $self->{'targets'} = $TARGETS;
  $self->{'labels'} = {};
  $self->{'internal_references'} = $INTL_XREFS;
  $self->{'floats'} = $FLOATS;
  $self->{'info'} = $GLOBAL_INFO;
  $self->{'extra'} = $GLOBAL_INFO2;

  # Propagate these settings from 'info' hash to 'values' hash.
  # The 'values' hash is not otherwise used.  Maybe we should use
  # the 'info' hash for this instead in the pure Perl code.
  for my $txi_flag ('txiindexatsignignore', 'txiindexbackslashignore',
                    'txiindexhyphenignore', 'txiindexlessthanignore') {
    if ($self->{'info'}->{$txi_flag}) {
      $self->{'values'}->{$txi_flag} = 1;
    }
  }

  _get_errors ($self);
  Texinfo::Common::complete_indices ($self);
}

use File::Basename; # for fileparse

# Handle 'IGNORE_BEFORE_SETFILENAME' conf value.
# Put everything before @setfilename in a special type.  This allows
# ignoring everything before @setfilename.
sub _maybe_ignore_before_setfilename {
  my ($self, $text_root) = @_;

  if ($self->{'IGNORE_BEFORE_SETFILENAME'} and $text_root
      and $self->{'extra'}->{'setfilename'}
      and $self->{'extra'}->{'setfilename'}->{'parent'} eq $text_root) {
    my $before_setfilename = {'type' => 'preamble_before_setfilename',
      'parent' => $text_root,
      'contents' => []};
    while (@{$text_root->{'contents'}}
        and (!$text_root->{'contents'}->[0]->{'cmdname'}
          or $text_root->{'contents'}->[0]->{'cmdname'} ne 'setfilename')) {
      my $content = shift @{$text_root->{'contents'}};
      $content->{'parent'} = $before_setfilename;
      push @{$before_setfilename->{'contents'}}, $content;
    }
    if (!@{$text_root->{'contents'}}) {
      # not found
      #splice @{$text_root->{'contents'}}, 0, 0, @$before_setfilename;
      $text_root->{'contents'} = $before_setfilename->{'contents'};
    } else {
      unshift (@{$text_root->{'contents'}}, $before_setfilename)
        if (@{$before_setfilename->{'contents'}});
    }
  }
}

# Replacement for Texinfo::Parser::parse_texi_file
sub parse_texi_file ($$)
{
  my $self = shift;
  my $file_name = shift;
  my $tree_stream;

  my $status = parse_file ($file_name);
  if ($status) {
    # TODO: internationalise this message?
    $self->document_error(sprintf("could not open %s: %s", $file_name, $!));
    return undef;
  }

  my $TREE = build_texinfo_tree ();
  get_parser_info ($self);
  _complete_node_menus ($self, $TREE);

  my $text_root;
  if ($TREE->{'type'} eq 'text_root') {
    $text_root = $TREE;
  } elsif ($TREE->{'contents'} and $TREE->{'contents'}->[0]
      and $TREE->{'contents'}->[0]->{'type'} eq 'text_root') {
    $text_root = $TREE->{'contents'}->[0];
  }

  _maybe_ignore_before_setfilename($self, $text_root);

  ############################################################

  my ($basename, $directories, $suffix) = fileparse($file_name);
  $self->{'info'}->{'input_file_name'} = $basename;
  $self->{'info'}->{'input_directory'} = $directories;

  return $TREE;
}

# Copy the errors into the error list in Texinfo::Report.
# TODO: Could we just access the error list directly instead of going
# through Texinfo::Report line_error?
sub _get_errors($)
{
  my $self = shift;
  my $ERRORS;
  my $tree_stream = dump_errors();
  eval $tree_stream;
  for my $error (@{$ERRORS}) {
    if ($error->{'type'} eq 'error') {
      $self->line_error ($error->{'message'}, $error->{'line_nr'});
    } else {
      $self->line_warn ($error->{'message'}, $error->{'line_nr'});
    }
  }
}

# Replacement for Texinfo::Parser::parse_texi_text
#
# Used in tests under tp/t.
sub parse_texi_text($$;$$$$)
{
    my $self = shift;
    my $text = shift;
    my $lines_nr = shift;
    my $file = shift;
    my $macro = shift;
    my $fixed_line_number = shift;

    return undef if (!defined($text));

    $self = parser() if (!defined($self));

    # make sure that internal byte buffer is in UTF-8 before we pass
    # it in to the XS code.
    utf8::upgrade($text);

    parse_text($text);
    my $tree = build_texinfo_tree ();
    my $INDEX_NAMES = build_index_data ();
    $self->{'index_names'} = $INDEX_NAMES;

    for my $index (keys %$INDEX_NAMES) {
      if ($INDEX_NAMES->{$index}->{'merged_in'}) {
        $self->{'merged_indices'}-> {$index}
          = $INDEX_NAMES->{$index}->{'merged_in'};
      }
    }

    get_parser_info($self);
    _complete_node_menus ($self, $tree);
    return $tree;
}

# Replacement for Texinfo::Parser::parse_texi_line
sub parse_texi_line($$;$$$$)
{
    my $self = shift;
    my $text = shift;
    my $lines_nr = shift;
    my $file = shift;
    my $macro = shift;
    my $fixed_line_number = shift;

    return undef if (!defined($text));

    $self = parser() if (!defined($self));
    utf8::upgrade($text);
    parse_string($text);
    my $tree = build_texinfo_tree ();
    return $tree;
}

# Public interfaces of Texinfo::Parser
sub indices_information($)
{
  my $self = shift;

  my $INDEX_NAMES;
  if (!$self->{'index_names'}) {
    $INDEX_NAMES = build_index_data ();
    $self->{'index_names'} = $INDEX_NAMES;
  }
  return $self->{'index_names'};
}

sub floats_information($)
{
  my $self = shift;
  return $self->{'floats'};
}

sub internal_references_information($)
{
  my $self = shift;
  return $self->{'internal_references'};
}

sub global_commands_information($)
{
  my $self = shift;
  return $self->{'extra'};
}

sub global_informations($)
{
  my $self = shift;
  return $self->{'info'};
}

# Setup labels and nodes info and return labels
sub labels_information($)
{
  goto &Texinfo::Common::labels_information;
}

1;
__END__
