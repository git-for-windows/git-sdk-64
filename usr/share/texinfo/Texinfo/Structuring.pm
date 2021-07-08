# Structuring.pm: extract informations about a document structure based on the 
#                 document tree.
#
# Copyright 2010-2019 Free Software Foundation, Inc.
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
# Parts (also from Patrice Dumas) come from texi2html.pl.

package Texinfo::Structuring;

use 5.00405;

# See comment at start of HTML.pm
use if $] >= 5.012, feature => 'unicode_strings';

use strict;

use Texinfo::Common;

# for debugging.  Also for index entries sorting.
use Texinfo::Convert::Text;
# for error messages 
use Texinfo::Convert::Texinfo;

*node_extra_to_texi = \&Texinfo::Convert::Texinfo::node_extra_to_texi;

use Carp qw(cluck);

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

%EXPORT_TAGS = ( 'all' => [ qw(
  associate_internal_references
  elements_directions
  elements_file_directions
  merge_indices
  nodes_tree
  number_floats
  sectioning_structure
  set_menus_node_directions
  sort_indices
  sort_indices_by_letter
  split_by_node
  split_by_section
  split_pages
  warn_non_empty_parts
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '6.8';


my %types_to_enter;
foreach my $type_to_enter ('brace_command_arg', 'line_arg',
    'paragraph') {
  $types_to_enter{$type_to_enter} = 1;
}


my %command_structuring_level = %Texinfo::Common::command_structuring_level;

my %appendix_commands;
my %unnumbered_commands;
foreach my $command (keys(%command_structuring_level)) {
  if ($command =~ /appendix/) {
    $appendix_commands{$command} = 1;
  } elsif ($command =~ /unnumbered/) {
    $unnumbered_commands{$command} = 1;
  }
}
$unnumbered_commands{'top'} = 1;
$unnumbered_commands{'centerchap'} = 1;
$unnumbered_commands{'part'} = 1;

my $min_level = $command_structuring_level{'chapter'};
my $max_level = $command_structuring_level{'subsubsection'};

# Return numbered level of an element
sub section_level($)
{
  my $section = shift;
  my $level = $command_structuring_level{$section->{'cmdname'}};
  # correct level according to raise/lowersections
  if ($section->{'extra'} and $section->{'extra'}->{'sections_level'}) {
    $level -= $section->{'extra'}->{'sections_level'};
    if ($level < $min_level) {
      if ($command_structuring_level{$section->{'cmdname'}} < $min_level) {
        $level = $command_structuring_level{$section->{'cmdname'}};
      } else {
        $level = $min_level;
      }
    } elsif ($level > $max_level) {
      $level = $max_level;
    }
  }
  return $level;
}


# Go through the sectioning commands (e.g. @chapter, not @node), and
# set:
# 'number'
# 'section_childs'
# 'section_up'
# 'section_prev'
# 'section_next'
# 'toplevel_next'
# 'toplevel_prev'
# 'toplevel_up'
sub sectioning_structure($$)
{
  my $self = shift;
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'}) {
    return undef;
  }

  my $sec_root = {};
  my $previous_section;
  my $previous_toplevel;

  my $in_appendix = 0;
  # lowest level with a number.  This is the lowest level above 0.
  my $number_top_level;

  my $section_top;
  my @sections_list;
  
  # holds the current number for all the levels.  It is not possible to use
  # something like the last child index, because of @unnumbered.
  my @command_numbers;
  # keep track of the unnumbered
  my @command_unnumbered;
  foreach my $content (@{$root->{'contents'}}) {
    if (!$content->{'cmdname'} or $content->{'cmdname'} eq 'node'
        or $content->{'cmdname'} eq 'bye') {
      next;
    }
    push @sections_list, $content;
    if ($content->{'cmdname'} eq 'top') {
      if (! $section_top) {
        $section_top = $content;
      }
    }
    my $level;
    $level = $content->{'level'} = section_level($content);
    if (!defined($level)) {
      warn "bug: level not defined for $content->{'cmdname'}\n";
      $level = $content->{'level'} = 0;
    }

    if ($previous_section) {
      # new command is below
      if ($previous_section->{'level'} < $level) {
        if ($level - $previous_section->{'level'} > 1) {
          $self->line_error(sprintf(
              __("raising the section level of \@%s which is too low"), 
              $content->{'cmdname'}), $content->{'line_nr'});
          $content->{'level'} = $previous_section->{'level'} + 1;
        }
        $previous_section->{'section_childs'} = [$content];
        $content->{'section_up'} = $previous_section;

        # if the up is unnumbered, the number information has to be kept,
        # to avoid reusing an already used number.
        if (!$unnumbered_commands{$previous_section->{'cmdname'}}) {
          $command_numbers[$content->{'level'}] = undef;
        } elsif (!$unnumbered_commands{$content->{'cmdname'}}) {
          $command_numbers[$content->{'level'}]++;
        }
        if ($unnumbered_commands{$content->{'cmdname'}}) {
          $command_unnumbered[$content->{'level'}] = 1;
        } else {
          $command_unnumbered[$content->{'level'}] = 0;
        }
      } else {
        my $up = $previous_section->{'section_up'};
        my $new_upper_part_element;
        if ($previous_section->{'level'} != $level) {
          # means it is above the previous command, the up is to be found
          while ($up->{'section_up'} and $up->{'level'} >= $level) {
            $up = $up->{'section_up'};
          }
          if ($level <= $up->{'level'}) {
            if ($content->{'cmdname'} eq 'part') {
              $new_upper_part_element = 1;
              if ($level < $up->{'level'}) {
                $self->line_warn(sprintf(__(
                      "no chapter-level command before \@%s"),
                    $content->{'cmdname'}), $content->{'line_nr'});
              }
            } else {
              $self->line_warn(sprintf(__(
                    "lowering the section level of \@%s appearing after a lower element"), 
                  $content->{'cmdname'}), $content->{'line_nr'});
              $content->{'level'} = $up->{'level'} + 1;
            }
          }
        }
        if ($appendix_commands{$content->{'cmdname'}} and !$in_appendix
            and $content->{'level'} <= $number_top_level 
            and $up->{'cmdname'} and $up->{'cmdname'} eq 'part') {
          $up = $up->{'section_up'};
        }
        if ($new_upper_part_element) {
          # In that case the root has to be updated because the first 
          # 'part' just appeared
          $content->{'section_up'} = $sec_root;
          $sec_root->{'level'} = $level - 1;
          push @{$sec_root->{'section_childs'}}, $content;
          $number_top_level = $level;
          $number_top_level++ if (!$number_top_level);
        } else {
          push @{$up->{'section_childs'}}, $content;
          $content->{'section_up'} = $up;
          $content->{'section_prev'} = $up->{'section_childs'}->[-2];
          $content->{'section_prev'}->{'section_next'} = $content;
        }
        if (!$unnumbered_commands{$content->{'cmdname'}}) {
          $command_numbers[$content->{'level'}]++;
          $command_unnumbered[$content->{'level'}] = 0;
        } else {
          $command_unnumbered[$content->{'level'}] = 1;
        }
      }
    } else { # first section determines the level of the root.  It is 
      # typically -1 when there is a @top.
      $content->{'section_up'} = $sec_root;
      $sec_root->{'level'} = $level - 1;
      $sec_root->{'section_childs'} = [$content];
      $number_top_level = $level;
      # if $level of top sectioning element is 0, which means that
      # it is a @top, $number_top_level is 1 as it is associated to
      # the level of chapter/unnumbered...
      $number_top_level++ if (!$number_top_level);
      if ($content->{'cmdname'} ne 'top') {
        if (!$unnumbered_commands{$content->{'cmdname'}}) {
          $command_unnumbered[$content->{'level'}] = 0;
        } else {
          $command_unnumbered[$content->{'level'}] = 1;
        }
      }
    }
    if (!defined($command_numbers[$content->{'level'}])) {
      if ($unnumbered_commands{$content->{'cmdname'}}) {
        $command_numbers[$content->{'level'}] = 0;
      } else {
        $command_numbers[$content->{'level'}] = 1;
      }
    }
    if ($appendix_commands{$content->{'cmdname'}} and !$in_appendix) {
      $in_appendix = 1;
      $command_numbers[$content->{'level'}] = 'A';
    }
    if (!$unnumbered_commands{$content->{'cmdname'}}) {
      # construct the number, if not below an unnumbered
      if (!$command_unnumbered[$number_top_level]) {
        $content->{'number'} = $command_numbers[$number_top_level];
        for (my $i = $number_top_level+1; $i <= $content->{'level'}; $i++) {
          $content->{'number'} .= ".$command_numbers[$i]";
          # If there is an unnumbered above, then no number is added.
          if ($command_unnumbered[$i]) {
            delete $content->{'number'};
            last;
          }
        }
      }
    }
    $previous_section = $content;
    if ($content->{'cmdname'} ne 'part' 
        and $content->{'level'} <= $number_top_level) {
      if ($previous_toplevel) {
        $previous_toplevel->{'toplevel_next'} = $content;
        $content->{'toplevel_prev'} = $previous_toplevel;
      }
      $previous_toplevel = $content;
      if ($section_top and $content ne $section_top) {
        $content->{'toplevel_up'} = $section_top;
      }
    } elsif ($content->{'cmdname'} eq 'part' 
        and !$content->{'extra'}->{'part_associated_section'}) {
      $self->line_warn(sprintf(__(
            "no sectioning command associated with \@%s"),
          $content->{'cmdname'}), $content->{'line_nr'});
    }
  }
  $self->{'structuring'}->{'sectioning_root'} = $sec_root;
  $self->{'structuring'}->{'sections_list'} = \@sections_list;
  return $sec_root;
}

sub _print_sectioning_tree($);
sub _print_sectioning_tree($)
{
  my $current = shift;
  my $result = ' ' x $current->{'level'} . _print_root_command_texi($current)."\n";
  foreach my $child (@{$current->{'section_childs'}}) {
    $result .= _print_sectioning_tree($child);
  }
  return $result;
}


sub warn_non_empty_parts($)
{
  my $self = shift;
  my $global_commands = $self->global_commands_information();
  if ($global_commands->{'part'}) {
    foreach my $part (@{$global_commands->{'part'}}) {
      if (!Texinfo::Common::is_content_empty($part)) {
        $self->line_warn(sprintf(__("\@%s not empty"),
                         $part->{'cmdname'}), $part->{'line_nr'});
      }
    }
  }
}

sub _check_node_same_texinfo_code($$)
{
  my $reference_node = shift;
  my $node_extra = shift;

  my $reference_node_texi;
  if ($reference_node->{'extra'}->{'node_content'}) {
    $reference_node_texi = Texinfo::Convert::Texinfo::convert (
        {'contents' => $reference_node->{'extra'}->{'node_content'}});
    $reference_node_texi =~ s/\s+/ /g;
  } else {
    $reference_node_texi = '';
  }

  my $node_texi;
  if ($node_extra and $node_extra->{'node_content'}) {
    my @contents_node = @{$node_extra->{'node_content'}};
    pop @contents_node if ($contents_node[-1]->{'type'} 
               and $contents_node[-1]->{'type'} eq 'space_at_end_menu_node');
    $node_texi = Texinfo::Convert::Texinfo::convert (
      {'contents' => \@contents_node});
    $node_texi =~ s/\s+/ /g;
  } else {
    $node_texi = '';
  }
  return ($reference_node_texi eq $node_texi);
}


my @node_directions = ('next', 'prev', 'up');
# No translation of those special Info keywords.
my %direction_texts = (
 'prev' => 'Prev',
 'next' => 'Next',
 'up' => 'Up'
);

sub _check_menu_entry($$$)
{
  my $self = shift;
  my $command = shift;
  my $menu_content = shift;

  my $normalized_menu_node
      = $menu_content->{'extra'}->{'menu_entry_node'}->{'normalized'};

  my $menu_node = $self->{'labels'}->{$normalized_menu_node};

  if (!$menu_node) {
    $self->line_error(sprintf(
     __("\@%s reference to nonexistent node `%s'"), $command,
        node_extra_to_texi($menu_content->{'extra'}->{'menu_entry_node'})), 
     $menu_content->{'line_nr'});
  } else {
    if (!_check_node_same_texinfo_code($menu_node, 
                           $menu_content->{'extra'}->{'menu_entry_node'})) {
      $self->line_warn(sprintf(
       __("\@%s entry node name `%s' different from %s name `%s'"), 
         $command,
         node_extra_to_texi($menu_content->{'extra'}->{'menu_entry_node'}),
         $menu_node->{'cmdname'},
         node_extra_to_texi($menu_node->{'extra'})),
                            $menu_content->{'line_nr'});
    }
  }
}

sub _check_referenced_nodes
{
  my ($self, $top_node) = @_;

  my %referenced_nodes = ($top_node => 1);
  foreach my $node (@{$self->{'nodes'}}) {
    # gather referenced nodes based on node pointers
    foreach my $direction (@node_directions) {
      if ($node->{'node_'.$direction}
          and not $node->{'node_'.$direction}->{'extra'}->{'manual_content'}) {
        $referenced_nodes{$node->{'node_'.$direction}} = 1;
      }
    }
    if ($node->{'menu_up_hash'}) {
      $referenced_nodes{$node} = 1;
    }
  }

  # consider nodes in @*ref commands to be referenced
  my $labels = $self->labels_information();
  my $refs = $self->internal_references_information();
  if (defined($refs)) {
    foreach my $ref (@$refs) {
      my $node_arg = $ref->{'extra'}{'node_argument'};
      if ($node_arg->{'node_content'}) {
        my $normalized =
           Texinfo::Convert::NodeNameNormalization::normalize_node(
              {'contents' => $node_arg->{'node_content'} });
        my $node_target = $labels->{$normalized};
        if ($node_target) {
          $referenced_nodes{$node_target} = 1;
        }
      }
    }
  }

  foreach my $node (@{$self->{'nodes'}}) {
    if (not exists($referenced_nodes{$node})) {
      $self->line_warn(sprintf(__("node `%s' unreferenced"),
          node_extra_to_texi($node->{'extra'})),
           $node->{'line_nr'});
    }
  }
}

# set menu directions
sub set_menus_node_directions($)
{
  my $self = shift;
  return undef unless ($self->{'nodes'} and @{$self->{'nodes'}});

  my $check_menu_entries = (!$self->{'info'}->{'novalidate'}
                      and $self->get_conf('FORMAT_MENU') eq 'menu');

  # First go through all the menus and set menu_up, menu_next and menu_prev,
  # and warn for unknown nodes.
  # Remark: since the @menu are only checked if they are in @node, 
  # menu entries before the first node, or @menu nested inside
  # another command such as @format, may be treated slightly
  # differently; at least, there are no error messages for them.
  #
  foreach my $node (@{$self->{'nodes'}}) {
    if ($node->{'menus'}) {
      if (@{$node->{'menus'}} > 1) {
        foreach my $menu (@{$node->{'menus'}}[1 .. $#{$node->{'menus'}}]) {
          $self->line_warn(sprintf(__("multiple \@%s"), 
                        $menu->{'cmdname'}), $menu->{'line_nr'});
        }
      }
      foreach my $menu (@{$node->{'menus'}}) {
        my $previous_node;
        foreach my $menu_content (@{$menu->{'contents'}}) {
          if ($menu_content->{'extra'}
             and $menu_content->{'extra'}->{'menu_entry_node'}) {
            my $menu_node;
            my $external_node;
            if (!$menu_content->{'extra'}->{'menu_entry_node'}->{'manual_content'}) {
              $menu_node = $self->{'labels'}->{
                      $menu_content->{'extra'}
                                   ->{'menu_entry_node'}->{'normalized'}};

              if ($check_menu_entries) {
                _check_menu_entry($self, 'menu', $menu_content);
              }
              # this may happen more than once for a given node if the node 
              # is in more than one menu.  Therefore all the menu up node 
              # are kept in $menu_node->{'menu_up_hash'}
              if ($menu_node) {
                $menu_node->{'menu_up'} = $node;
                $menu_node->{'menu_up_hash'}->{$node->{'extra'}->{'normalized'}} = 1;
              }
            } else {
              $external_node = 1;
              $menu_node = {'extra' => $menu_content->{'extra'}->{'menu_entry_node'}};
            }
            if ($menu_node) {
              if ($previous_node) {
                if (!$external_node) {
                  $menu_node->{'menu_prev'} = $previous_node;
                }
                if (!$previous_node->{'extra'}->{'manual_content'}) {
                  $previous_node->{'menu_next'} = $menu_node;
                }
              } else {
                $node->{'menu_child'} = $menu_node;
              }
              $previous_node = $menu_node;
            }
          }
        } # end menu
      }
    }
  }
  # Check @detailmenu
  if ($check_menu_entries) {
    my $global_commands = $self->global_commands_information();
    if ($global_commands->{'detailmenu'}) {
      foreach my $detailmenu (@{$global_commands->{'detailmenu'}}) {
        foreach my $menu_content (@{$detailmenu->{'contents'}}) {
          if ($menu_content->{'extra'}
             and $menu_content->{'extra'}->{'menu_entry_node'}) {
            if (!$menu_content->{'extra'}->{'menu_entry_node'}->{'manual_content'}) {
              _check_menu_entry($self, 'detailmenu', $menu_content);
            }
          }
        }
      }
    }
  }
}

# determine node found through section directions, usually
# from section_$direction.  It could also be from
# toplevel_$direction if going through parts, except for @top
# as prev or next.
sub _section_direction_associated_node($$)
{
  my $section = shift;
  my $direction = shift;

  foreach my $direction_base ('section', 'toplevel') {
    if ($section->{$direction_base.'_'.$direction}
       and $section->{$direction_base.'_'.$direction}->{'extra'}
       and ($direction_base ne 'toplevel'
            or $direction eq 'up'
            or $section->{$direction_base.'_'.$direction}->{'cmdname'} ne 'top')
       and $section->{$direction_base.'_'.$direction}->{'extra'}->{'associated_node'}) {
         return $section->{$direction_base.'_'.$direction}->{'extra'}->{'associated_node'};
    }
  }
  return undef;
}

# complete automatic directions with menus (and first node
# for Top node).
# Checks on structure related to menus.
sub complete_node_tree_with_menus($$)
{
  my $self = shift;
  my $top_node = shift;

  return undef unless ($self->{'nodes'} and @{$self->{'nodes'}});
  # Go through all the nodes
  foreach my $node (@{$self->{'nodes'}}) {
    my $automatic_directions =
      (scalar(@{$node->{'extra'}->{'nodes_manuals'}}) == 1);

    if ($automatic_directions) {
      if ($node->{'extra'}->{'normalized'} ne 'Top') {
        foreach my $direction (@node_directions) {
          # prev already defined for the node first Top node menu entry
          if ($direction eq 'prev' and $node->{'node_'.$direction}
              and $node->{'node_'.$direction}->{'extra'}
              and $node->{'node_'.$direction}->{'extra'}->{'normalized'}
              and $node->{'node_'.$direction}->{'extra'}->{'normalized'} eq 'Top') {
            next;
          }
          if ($node->{'extra'}->{'associated_section'}) {
            my $section = $node->{'extra'}->{'associated_section'};

            # Prefer the section associated with a @part for node directions.
            if ($section->{'extra'}->{'part_associated_section'}) {
              $section = $section->{'extra'}->{'part_associated_section'};
            }
            # Check consistency with section and menu structure
            my $direction_associated_node
              = _section_direction_associated_node($section, $direction);
            if ($direction_associated_node) {
              if ($self->get_conf('CHECK_NORMAL_MENU_STRUCTURE')) {
                if ($section->{'section_up'}{'extra'}
          and $section->{'section_up'}{'extra'}{'associated_node'}
          and $section->{'section_up'}{'extra'}{'associated_node'}{'menus'}
          and @{$section->{'section_up'}{'extra'}{'associated_node'}{'menus'}}
                    and !$node->{'menu_'.$direction}) {
                  $self->line_warn(sprintf(
               __("node %s for `%s' is `%s' in sectioning but not in menu"),
                  $direction,
                  node_extra_to_texi($node->{'extra'}),
                  node_extra_to_texi($direction_associated_node->{'extra'})),
                    $node->{'line_nr'});
                }
              }
            }
          }
          # no direction was found using sections, use menus.  This allows
          # using only automatic direction for manuals without sectioning
          # commands.
          if (!$node->{'node_'.$direction}
              and $node->{'menu_'.$direction}
              and !$node->{'menu_'.$direction}->{'extra'}->{'manual_content'}) {
            if ($self->get_conf('CHECK_NORMAL_MENU_STRUCTURE')
                  and $node->{'extra'}->{'associated_section'}) {
              $self->line_warn(sprintf(
                  __("node `%s' is %s for `%s' in menu but not in sectioning"),
                node_extra_to_texi($node->{'menu_'.$direction}->{'extra'}),
                                   $direction,
                node_extra_to_texi($node->{'extra'}),
                  ),
                $node->{'line_nr'});
            }
            $node->{'node_'.$direction} = $node->{'menu_'.$direction};
          }
        }
      } elsif (not $node->{'node_next'}) {
        # use first menu entry if available as next for Top
        if ($node->{'menu_child'}) {
          $node->{'node_next'} = $node->{'menu_child'};
          if (!$node->{'menu_child'}->{'extra'}->{'manual_content'}
              and !$node->{'menu_child'}->{'node_prev'}) {
            $node->{'menu_child'}->{'node_prev'} = $node;
          }
        } else {
          # use the first non top node as next for Top
          foreach my $first_non_top_node (@{$self->{'nodes'}}) {
            if ($first_non_top_node ne $node) {
              $node->{'node_next'} = $first_non_top_node;
              if (scalar(@{$first_non_top_node->{'extra'}->{'nodes_manuals'}}) == 1) {
                $first_non_top_node->{'node_prev'} = $node;
              }
              last;
            }
          }
        }
      }
    }
    # check consistency between node pointer and node entries menu order
    if ($node->{'extra'}->{'normalized'} ne 'Top') {
      foreach my $direction (@node_directions) {
        if ($self->get_conf('CHECK_NORMAL_MENU_STRUCTURE')
            and $node->{'node_'.$direction}
            and $node->{'menu_'.$direction}
            and $node->{'menu_'.$direction}
               ne $node->{'node_'.$direction}
            and not $node->{'menu_'.$direction}->{'extra'}->{'manual_content'}) {
          $self->line_warn(sprintf(
           __("node %s pointer for `%s' is `%s' but %s is `%s' in menu"),
                  $direction,
                  node_extra_to_texi($node->{'extra'}),
                  node_extra_to_texi($node->{'node_'.$direction}->{'extra'}),
                  $direction,
                  node_extra_to_texi($node->{'menu_'.$direction}->{'extra'})),
                 $node->{'line_nr'});
        }
      }
    }

    # check for node up / menu up mismatch
    if ($self->get_conf('CHECK_NORMAL_MENU_STRUCTURE')
        and $node->{'node_up'}
        # No check if node up is an external manual
        and (!$node->{'node_up'}->{'extra'}->{'manual_content'})
        and (!$node->{'menu_up_hash'}
          or !$node->{'menu_up_hash'}->{$node->{'node_up'}->{'extra'}->{'normalized'}})) {
      # check if up node has a menu
      if ($node->{'node_up'}->{'menus'} and @{$node->{'node_up'}->{'menus'}}) {
        $self->line_warn(sprintf(
           __("node `%s' lacks menu item for `%s' despite being its Up target"), 
           node_extra_to_texi($node->{'node_up'}->{'extra'}), 
           node_extra_to_texi($node->{'extra'})),
           $node->{'node_up'}->{'line_nr'});
      }
      # FIXME check that the menu_up_hash is not empty (except for Top)?
      # FIXME check that node_up is not an external node (except for Top)?
    }
  }
  _check_referenced_nodes($self, $top_node);
}


# set node directions based on sectioning and @node explicit directions
sub nodes_tree($)
{
  my $self = shift;
  return undef unless ($self->{'nodes'} and @{$self->{'nodes'}});

  my $top_node;
  # Go through all the nodes and set directions.
  foreach my $node (@{$self->{'nodes'}}) {
    if ($node->{'extra'}->{'normalized'} eq 'Top') {
      $top_node = $node;
    }
    my $automatic_directions = 
      (scalar(@{$node->{'extra'}->{'nodes_manuals'}}) == 1);

    if ($automatic_directions) {
      if ($node->{'extra'}->{'normalized'} ne 'Top') {
        foreach my $direction (@node_directions) {
          # prev already defined for the node first Top node menu entry
          if ($direction eq 'prev' and $node->{'node_'.$direction}
              and $node->{'node_'.$direction}->{'extra'}
              and $node->{'node_'.$direction}->{'extra'}->{'normalized'}
              and $node->{'node_'.$direction}->{'extra'}->{'normalized'} eq 'Top') {
            next;
          }
          if ($node->{'extra'}->{'associated_section'}) {
            my $section = $node->{'extra'}->{'associated_section'};

            # Prefer the section associated with a @part for node directions.
            if ($section->{'extra'}->{'part_associated_section'}) {
              $section = $section->{'extra'}->{'part_associated_section'};
            }

            my $direction_associated_node
              = _section_direction_associated_node($section, $direction);
            if ($direction_associated_node) {
              $node->{'node_'.$direction} = $direction_associated_node;
            }
          }
        }
      } else {
        # Special case for Top node, use first section
        if ($node->{'extra'}->{'associated_section'}
            and $node->{'extra'}->{'associated_section'}->{'section_childs'}
            and $node->{'extra'}->{'associated_section'}->{'section_childs'}->[0]
            and $node->{'extra'}->{'associated_section'}->{'section_childs'}->[0]->{'extra'}->{'associated_node'}) {
          my $top_node_section_child
            = $node->{'extra'}->{'associated_section'}->{'section_childs'}->[0]->{'extra'}->{'associated_node'};
          $node->{'node_next'} = $top_node_section_child;
          if (scalar(@{$top_node_section_child->{'extra'}->{'nodes_manuals'}}) == 1) {
            $top_node_section_child->{'node_prev'} = $node;
          }
        }
      }
    } else { # explicit directions
      my @directions = @{$node->{'extra'}->{'nodes_manuals'}};
      shift @directions;
      foreach my $direction (@node_directions) {
        my $node_direction = shift @directions;
        next if (!defined($node_direction));
        # external node
        if ($node_direction->{'manual_content'}) {
          $node->{'node_'.$direction} = { 'extra' => $node_direction };
        } else {
          if ($self->{'labels'}->{$node_direction->{'normalized'}}) {
            my $node_target 
               = $self->{'labels'}->{$node_direction->{'normalized'}};
            $node->{'node_'.$direction} = $node_target;

            if (!$self->{'info'}->{'novalidate'}
                and !_check_node_same_texinfo_code($node_target,
                                                   $node_direction)) {
              $self->line_warn(sprintf(
                __("%s pointer `%s' (for node `%s') different from %s name `%s'"),
                  $direction_texts{$direction},
                  node_extra_to_texi($node_direction),
                  node_extra_to_texi($node->{'extra'}),
                                     $node_target->{'cmdname'},
                  node_extra_to_texi($node_target->{'extra'})),
                                     $node->{'line_nr'});
            }
          } else {
            if ($self->{'info'}->{'novalidate'}) {
              $node->{'node_'.$direction} = { 'extra' => $node_direction };
            } else {
              $self->line_error(sprintf(
                                  __("%s reference to nonexistent `%s'"),
                    $direction_texts{$direction},
                    node_extra_to_texi($node_direction)), $node->{'line_nr'});
            }
          }
        }
      }
    }
  }
  $top_node = $self->{'nodes'}->[0] if (!$top_node);
  $self->{'structuring'}->{'top_node'} = $top_node;

  return $top_node;
}

# Return a list of elements to be converted into pages.  Each element starts
# with a @node as its first child (except possibly the first one).
sub split_by_node($)
{
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'} or !@{$root->{'contents'}}) {
    return undef;
  }
  my $elements;
  my $current = { 'type' => 'element', 'extra' => {'no_node' => 1}};
  push @$elements, $current; 
  my @pending_parts = ();
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'} and $content->{'cmdname'} eq 'part'
        and $content->{'extra'}->{'part_associated_section'}) {
      push @pending_parts, $content;
      next;
    }
    if ($content->{'cmdname'} and $content->{'cmdname'} eq 'node') {
      if ($current->{'extra'}->{'no_node'}) {
        delete $current->{'extra'}->{'no_node'};
        $current->{'extra'}->{'node'} = $content;
      } else {
        $current = { 'type' => 'element', 'extra' => {'node' => $content}};
        $current->{'element_prev'} = $elements->[-1];
        $elements->[-1]->{'element_next'} = $current;
        push @$elements, $current;
      }
      $elements->[-1]->{'extra'}->{'element_command'} = $content;
      if ($content->{'extra'}->{'associated_section'}) {
        $elements->[-1]->{'extra'}->{'section'} 
          = $content->{'extra'}->{'associated_section'};
      }
    }
    if (@pending_parts) {
      foreach my $part (@pending_parts) {
        push @{$current->{'contents'}}, $part;
        $part->{'parent'} = $current;
      }
      @pending_parts = ();
    }
    push @{$current->{'contents'}}, $content;
    $content->{'parent'} = $current;
  }
  return $elements;
}

# Return a list of elements to be converted into pages.  Each element starts
# with the @node associated with a sectioning command or with the sectioning
# command if there is no associated node
sub split_by_section($)
{
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'} or !@{$root->{'contents'}}) {
    return undef;
  }
  my $elements;
  my $current = { 'type' => 'element', 'extra' => {'no_section' => 1}};
  push @$elements, $current; 
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'}
        and (($content->{'cmdname'} eq 'node' 
              and $content->{'extra'}->{'associated_section'})
             or ($content->{'cmdname'} eq 'part'
                 and $content->{'extra'}->{'part_associated_section'}))) {
      my $new_section;
      if ($content->{'cmdname'} eq 'node') {
        $new_section = $content->{'extra'}->{'associated_section'};
      } else {
        $new_section = $content->{'extra'}->{'part_associated_section'};
      }
      if (! $current->{'extra'}->{'section'}
        or $new_section ne $current->{'extra'}->{'section'}) {
        if ($current->{'extra'}->{'no_section'}) {
          delete $current->{'extra'}->{'no_section'};
          $current->{'extra'}->{'section'}
            = $new_section;
        } else {
          $current = { 'type' => 'element', 
                       'extra' => {'section' => $new_section}};
          $current->{'element_prev'} = $elements->[-1];
          $elements->[-1]->{'element_next'} = $current;
          push @$elements, $current;
        }
        $elements->[-1]->{'extra'}->{'element_command'} 
          = $new_section;
      }
    } elsif ($content->{'cmdname'} and $content->{'cmdname'} ne 'node' 
                                   and $content->{'cmdname'} ne 'bye') {
      if ($current->{'extra'}->{'no_section'}) {
        delete $current->{'extra'}->{'no_section'};
        $current->{'extra'}->{'section'} = $content;
        $current->{'extra'}->{'element_command'} = $content;
      } elsif ($current->{'extra'}->{'section'} ne $content) {
        $current = { 'type' => 'element', 'extra' => {'section' => $content,
                                              'element_command' => $content}};
        $current->{'element_prev'} = $elements->[-1];
        $elements->[-1]->{'element_next'} = $current;
        push @$elements, $current;
      }
    }
    if ($content->{'cmdname'} and $content->{'cmdname'} eq 'node' 
        and $content->{'extra'}->{'associated_section'}) {
      $current->{'extra'}->{'node'} = $content;
    }
    push @{$current->{'contents'}}, $content;
    $content->{'parent'} = $current;
  }
  return $elements;
}

# Associate top-level elements with pages according to the splitting 
# specification.  Set 'first_in_page' on each top-level element to the element 
# that is the first in the output page.
sub split_pages ($$)
{
  my $elements = shift;
  my $split = shift;

  return undef if (!$elements or !@$elements);

  my $split_level;
  if (!$split) {
    foreach my $element (@$elements) {
      $element->{'extra'}->{'first_in_page'} = $elements->[0];
    }
    return;
  } elsif ($split eq 'chapter') {
    $split_level = 1;
  } elsif ($split eq 'section') {
    $split_level = 2;
  } elsif ($split ne 'node') {
    warn "Unknown split specification: $split\n";
  }

  my $current_first_in_page;
  foreach my $element (@$elements) {
    my $level;
    if ($element->{'extra'}->{'section'}) {
      $level = $element->{'extra'}->{'section'}->{'level'};
    } elsif ($element->{'extra'}->{'node'} 
             and $element->{'extra'}->{'node'}->{'associated_section'}) {
      $level = $element->{'extra'}->{'node'}->{'associated_section'}->{'level'};
    }
    #print STDERR "level($split_level) $level "._print_element_command_texi($element)."\n";
    if (!defined($split_level) or (defined($level) and $split_level >= $level)
        or !$current_first_in_page) {
      $current_first_in_page = $element;
    }
    $element->{'extra'}->{'first_in_page'} = $current_first_in_page;
  }
}

# undef in argument should be an error.  Thus only node existing should be
# passed to this function.  Even if not existing the value returned should
# be undef.
sub _node_element($)
{
  my $node = shift;
  if ($node->{'extra'} and $node->{'extra'}->{'manual_content'}) {
    my $external_node = { 'type' => 'external_node',
      'extra' => {'manual_content' => $node->{'extra'}->{'manual_content'}}};
  
    if ($node->{'extra'}->{'node_content'}) {
      $external_node->{'extra'}->{'node_content'} 
        = $node->{'extra'}->{'node_content'};
      $external_node->{'extra'}->{'normalized'} = 
        Texinfo::Convert::NodeNameNormalization::normalize_node(
          {'contents' => $node->{'extra'}->{'node_content'}}); 
    }
    return $external_node;
  } elsif ($node->{'cmdname'} and $node->{'cmdname'} eq 'node') {
    return $node->{'parent'};
  } else {
    # case of a @float or an @anchor
    return undef;
  }
}

# Do element directions (like in texi2html) and store them 
# in 'extra'->'directions'.
sub elements_directions($$)
{
  my $self = shift;
  my $elements = shift;
  return if (!$elements or !@$elements);

  my $node_top = $self->{'labels'}->{'Top'};
  foreach my $element (@$elements) {
    my $directions;
    $directions->{'This'} = $element;
    $directions->{'Forward'} = $element->{'element_next'}
      if ($element->{'element_next'}
          and (($element->{'extra'}->{'special_element'}
                and $element->{'element_next'}->{'extra'}->{'special_element'})
               or (!$element->{'extra'}->{'special_element'}
                and !$element->{'element_next'}->{'extra'}->{'special_element'})));
    $directions->{'Back'} = $element->{'element_prev'}
      if ($element->{'element_prev'}
          and (($element->{'extra'}->{'special_element'}
                and $element->{'element_prev'}->{'extra'}->{'special_element'})
               or (!$element->{'extra'}->{'special_element'}
                and !$element->{'element_prev'}->{'extra'}->{'special_element'})));
    if ($element->{'extra'}->{'node'}) {
      my $node = $element->{'extra'}->{'node'};
      foreach my $direction(['NodeUp', 'node_up'], ['NodeNext', 'node_next'],
                            ['NodePrev', 'node_prev']) {
        $directions->{$direction->[0]} = _node_element($node->{$direction->[1]})
            if ($node->{$direction->[1]});
      }
      # Now do NodeForward which is something like the following node.
      my $automatic_directions = 
        (scalar(@{$node->{'extra'}->{'nodes_manuals'}}) == 1);
      if ($node->{'menu_child'}) {
        $directions->{'NodeForward'} = _node_element($node->{'menu_child'});
      } elsif ($automatic_directions and $node->{'associated_section'}
               and $node->{'associated_section'}->{'section_childs'}
               and $node->{'associated_section'}->{'section_childs'}->[0]) {
        $directions->{'NodeForward'} 
          = $node->{'associated_section'}->{'section_childs'}->[0]->{'parent'};
      } elsif ($node->{'node_next'}) {
        $directions->{'NodeForward'} = _node_element($node->{'node_next'});
      } else {
        my $up = $node->{'node_up'};
        my @up_list = ($node);
        # the condition with the up_list avoids infinite loops
        # the last condition stops when the Top node is reached.
        while (defined($up) 
               and not (grep {$up eq $_} @up_list  
                        or ($node_top and $up eq $node_top))) {
          if (defined($up->{'node_next'})) {
            $directions->{'NodeForward'} = _node_element($up->{'node_next'});
            last;
          }
          push @up_list, $up;
          $up = $up->{'node_up'};
        }
      }
      
      $directions->{'NodeForward'}->{'extra'}->{'directions'}->{'NodeBack'} = $element
        if ($directions->{'NodeForward'}
            and $directions->{'NodeForward'}->{'type'} eq 'element'
            and !$directions->{'NodeForward'}->{'extra'}->{'directions'}->{'NodeBack'});
    }

    if (!$element->{'extra'}->{'section'}) {
      # If there is no associated section, find the previous element section.
      # Use the FastForward of this element.
      # Use it as FastBack if the section is top level, or use the FastBack.
      my $section_element;
      my $current = $element;
      while ($current->{'element_prev'}) {
        $current = $current->{'element_prev'};
        if ($current->{'extra'}->{'section'}) {
          $section_element = $current;
          last;
        }
      }
      if ($section_element) {
        if ($section_element->{'extra'}->{'directions'}->{'FastForward'}) {
          $directions->{'FastForward'} 
            = $section_element->{'extra'}->{'directions'}->{'FastForward'};
        }
        if ($section_element->{'extra'}->{'section'}->{'level'} <= 1) {
          $directions->{'FastBack'} = $section_element;
        } elsif ($section_element->{'extra'}->{'directions'}->{'Fastback'}) {
          $directions->{'FastBack'} 
            = $section_element->{'extra'}->{'directions'}->{'Fastback'};
        }
      }
    } else {
      my $section = $element->{'extra'}->{'section'};
      foreach my $direction(['Up', 'section_up'], ['Next', 'section_next'],
                            ['Prev', 'section_prev']) {
        # in most cases $section->{$direction->[1]}->{'parent'} is defined
        # but it may not be the case for the up of @top.
        # The section may be its own up in cases like
        #  @part part
        #  @chapter chapter
        # in that cas the direction is not set up
        $directions->{$direction->[0]} = $section->{$direction->[1]}->{'parent'}
          if ($section->{$direction->[1]} 
              and $section->{$direction->[1]}->{'parent'}
              and $section->{$direction->[1]}->{'parent'} ne $section->{'parent'});
      }

      my $up = $section;
      while ($up->{'level'} > 1 and $up->{'section_up'}) {
        $up = $up->{'section_up'};
      }

      # fastforward is the next element on same level than the upper parent
      # element.
      if ($up->{'level'} < 1 and $up->{'cmdname'} and $up->{'cmdname'} eq 'top'
          and $up->{'section_childs'} and @{$up->{'section_childs'}}) {
        $directions->{'FastForward'} = $up->{'section_childs'}->[0]->{'parent'};
      } elsif ($up->{'toplevel_next'}) {
        $directions->{'FastForward'} = $up->{'toplevel_next'}->{'parent'};
      } elsif ($up->{'section_next'}) {
        $directions->{'FastForward'} = $up->{'section_next'}->{'parent'};
      }
      # if the element isn't at the highest level, fastback is the 
      # highest parent element
      if ($up and $up ne $section) {
        $directions->{'FastBack'} = $up->{'parent'};
      } elsif ($section->{'level'} <= 1) {
        # the element is a top level element, we adjust the next
        # toplevel element fastback
        $directions->{'FastForward'}->{'extra'}->{'directions'}->{'FastBack'}  
          = $element if ($directions->{'FastForward'});
      }
    }
    # Use node up for Up if there is no section up.
    # Not done in the default case.
    if ($self->get_conf('USE_UP_NODE_FOR_ELEMENT_UP')
        and !$directions->{'Up'} and $element->{'extra'}->{'node'}
        and $element->{'extra'}->{'node'}->{'node_up'} 
        and (!$node_top or ($element->{'extra'}->{'node'} ne $node_top))) {
      #print STDERR "Using node for up "._print_element_command_texi($element)."\n";
      my $up_node_element = _node_element($element->{'extra'}->{'node'}->{'node_up'});
      $directions->{'Up'} = $up_node_element if ($up_node_element);
    }
    if ($element->{'extra'}->{'directions'}) {
      %{$element->{'extra'}->{'directions'}} = (%{$element->{'extra'}->{'directions'}}, 
                                                %$directions)
    } else {
      $element->{'extra'}->{'directions'} = $directions;
    }
  }
  if ($self->get_conf('DEBUG')) {
    foreach my $element (@$elements) {
      print STDERR "Directions($element): "
         .Texinfo::Structuring::_print_directions($element)."\n";
    }
  }
}

sub elements_file_directions($$)
{
  my $self = shift;
  my $elements = shift;
  return if (!$elements or !@$elements);

  my $current_filename;
  my $first_element_in_file;
  # need to gather the directions before the FirstInFile* directions
  # are added to the first element in the file.
  my @first_element_in_file_directions;
  foreach my $element (@$elements) {
    my $directions;
    my $filename;
    if (defined($element->{'filename'})) {
      $filename = $element->{'filename'};
      my $current_element = $element;
      if (not defined($current_filename)
          or $filename ne $current_filename) {
        $first_element_in_file = $element;
        @first_element_in_file_directions = keys %{$element->{'extra'}->{'directions'}};
        $current_filename = $filename;
      }
      while ($current_element->{'element_prev'}) {
        $current_element = $current_element->{'element_prev'};
        if (defined($current_element->{'filename'})) {
          if ($current_element->{'filename'} ne $filename) {
            $element->{'extra'}->{'directions'}->{'PrevFile'} = $current_element;
            last;
          }
        } else {
          last;
        }
      }
      $current_element = $element;
      while ($current_element->{'element_next'}) {
        $current_element = $current_element->{'element_next'};
        if (defined($current_element->{'filename'})) {
          if ($current_element->{'filename'} ne $filename) {
            $element->{'extra'}->{'directions'}->{'NextFile'} = $current_element;
            last;
          }
        } else {
          last;
        }
      }
    }
    # set the directions of the first elements in file, to
    # be used in footers for example
    if (defined($first_element_in_file)) {
      foreach my $first_in_file_direction
                (@first_element_in_file_directions) {
        $element->{'extra'}->{'directions'}->{'FirstInFile'.$first_in_file_direction}
          = $first_element_in_file->{'extra'}->{'directions'}->{$first_in_file_direction};
      }
    }
  }
}

my %sectioning_commands = %Texinfo::Common::sectioning_commands;
# for debugging
sub _print_root_command_texi($)
{
  my $command = shift;
  my $tree;
  if ($command->{'cmdname'}) {
    if ($command->{'cmdname'} eq 'node') {
      $tree = $command->{'extra'}->{'node_content'};
    } elsif ($sectioning_commands{$command->{'cmdname'}}) {
      $tree = $command->{'args'}->[0]->{'contents'};
    }
  } else {
    return "Not a root command";
  }
  return '@'.$command->{'cmdname'}. ' '
       .Texinfo::Convert::Texinfo::convert ({'contents' => $tree})
          if ($tree);
  return 'UNDEF @'.$command->{'cmdname'};
}

# for debugging
sub _print_element_command_texi($)
{
  my $element = shift;
  if (!$element) {
    return "UNDEF ELEMENT";
  }
  if (!$element->{'type'}) {
    return "element $element without type: ".
       Texinfo::Parser::_print_current_keys($element);
  }

  if ($element->{'type'} eq 'external_node') {
    my $command = {'contents' => [{'text' => '('}, 
                        @{$element->{'extra'}->{'manual_content'}},
                               {'text' => ')'}]};
    if ($element->{'extra'}->{'node_content'}) {
      unshift @{$command->{'contents'}}, @{$element->{'extra'}->{'node_content'}};
    }
    return Texinfo::Convert::Texinfo::convert($command);
  }
  
  my $command = $element->{'extra'}->{'element_command'};
  if (!defined($command)) {
    # happens when there are only nodes and sections are used as elements
    my $result = "No associated command ";
    $result .= "(type $element->{'type'})" if (defined($element->{'type'}));
    return $result;
  }
  return _print_root_command_texi($command);
}

# for debugging
sub _print_directions($)
{
  my $element = shift;
  my $result = 'element: '._print_element_command_texi($element)."\n";

  if ($element->{'extra'} and $element->{'extra'}->{'directions'}) {
    foreach my $direction (sort(keys(%{$element->{'extra'}->{'directions'}}))) {
      $result .= "  $direction: ".
       _print_element_command_texi($element->{'extra'}->{'directions'}->{$direction})."\n";
    }
  } else {
    $result .= "  NO DIRECTION";
  }
  return $result;
}

# this is used in the test suite, but not likely to be useful in real life.
sub _unsplit($)
{
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'}) {
    return $root;
  }
  foreach my $content (@{$root->{'contents'}}) {
    $content->{'parent'} = $root;
  }
  return $root;
}

# For each internal reference command, set the 'label' key in the 'extra' 
# hash of the reference tree element to the associated labeled tree element.
sub associate_internal_references($)
{
  my $self = shift;

  my $labels = $self->labels_information();
  my $refs = $self->internal_references_information();
  return if (!defined($refs));
  foreach my $ref (@$refs) {
    my $node_arg;
    $node_arg = $ref->{'extra'}{'menu_entry_node'};
    
    if (defined $node_arg) {
      if ($node_arg->{'node_content'}) {
        my $normalized =
             Texinfo::Convert::NodeNameNormalization::normalize_node(
                {'contents' => $node_arg->{'node_content'} });
        $node_arg->{'normalized'} = $normalized
          if (defined $normalized and $normalized ne '');
      }
      next;
    }
    
    $node_arg = $ref->{'extra'}{'node_argument'};
    if ($node_arg->{'node_content'}) {
      my $normalized =
           Texinfo::Convert::NodeNameNormalization::normalize_node(
              {'contents' => $node_arg->{'node_content'} });
      $node_arg->{'normalized'} = $normalized;
    }
    if (!defined($labels->{$node_arg->{'normalized'}})) {
      if (!$self->{'info'}->{'novalidate'}) {
        $self->line_error(sprintf(__("\@%s reference to nonexistent node `%s'"),
                $ref->{'cmdname'}, node_extra_to_texi($node_arg)),
                $ref->{'line_nr'});
      }
    } else {
      my $node_target = $labels->{$node_arg->{'normalized'}};
      $ref->{'extra'}->{'label'} = $node_target;
      if (!$self->{'info'}->{'novalidate'}
          and !_check_node_same_texinfo_code($node_target, $node_arg)) {
        $self->line_warn(sprintf(
           __("\@%s to `%s', different from %s name `%s'"), 
           $ref->{'cmdname'},
           node_extra_to_texi($node_arg),
           $node_target->{'cmdname'},
           node_extra_to_texi($node_target->{'extra'})), $ref->{'line_nr'});
      }
    }
  }
}

sub number_floats($)
{
  my $floats = shift;
  return if (!defined($floats));
  foreach my $style (keys(%$floats)) {
    my %nr_in_chapter;
    my $float_index = 0;
    foreach my $float (@{$floats->{$style}}) {
      next if (!$float->{'extra'} 
               or !defined($float->{'extra'}->{'node_content'}));
      $float_index++;
      my $number;
      if ($float->{'extra'}->{'float_section'}) {
        my $up = $float->{'extra'}->{'float_section'};
        while ($up->{'section_up'} 
               #and $command_structuring_level{$up->{'cmdname'}} 
               and defined($up->{'section_up'}->{'cmdname'})
               and $command_structuring_level{$up->{'section_up'}->{'cmdname'}}) {
          $up = $up->{'section_up'};
        }
        if (!$unnumbered_commands{$up->{'cmdname'}}) {
          $nr_in_chapter{$up->{'number'}}++;
          $number = $up->{'number'} . '.' . $nr_in_chapter{$up->{'number'}};
        }
      }
      $number = $float_index if (!defined($number));
      $float->{'number'} = $number;
    }
  }
}

sub _copy_contents($)
{
  my $contents = shift;
  if (ref($contents) ne 'ARRAY') {
    cluck "$contents not an array";
    return undef;
  }
  my $copy = Texinfo::Common::copy_tree({'contents' => $contents});
  return $copy->{'contents'};
}

sub get_node_node_childs
{
  my ($node) = @_;

  my @node_childs;

  if ($node->{'extra'}->{'associated_section'}->{'section_childs'}) {
    foreach my $child (@{$node->{'extra'}->{'associated_section'}->{'section_childs'}}) {
      if ($child->{'extra'} and $child->{'extra'}->{'associated_node'}) {
        push @node_childs, $child->{'extra'}->{'associated_node'};
      }
    }
  }
  # Special case for @top.  Gather all the children of the @part following
  # @top.
  if ($node->{'extra'}->{'associated_section'}->{'cmdname'} eq 'top') {
    my $current = $node->{'extra'}->{'associated_section'};
    while ($current->{'section_next'}) {
      $current = $current->{'section_next'};
      if ($current->{'cmdname'} and $current->{'cmdname'} eq 'part'
          and $current->{'section_childs'}) {
        foreach my $child (@{$current->{'section_childs'}}) {
          if ($child->{'extra'} and $child->{'extra'}->{'associated_node'}) {
            push @node_childs, $child->{'extra'}->{'associated_node'};
          }
        }
      } elsif ($current->{'extra'}->{'associated_node'}) {
        # for @appendix, and what follows, as it stops a @part, but is 
        # not below @top
        push @node_childs, $current->{'extra'}->{'associated_node'};
      }
    }
  }
  return @node_childs;
}

sub new_node_menu_entry
{
  my ($self, $node, $use_sections) = @_;

  my $node_contents = $node->{'extra'}->{'node_content'};
  
  my ($name_contents, $menu_entry_name);
  if ($use_sections) {
    if (defined $node->{'extra'}->{'associated_section'}) {
      $name_contents = $node->{'extra'}->{'associated_section'}->{'args'}->[0]->{'contents'};
    } else {
      $name_contents = $node_contents; # shouldn't happen
    }
  }

  my $entry = {'type' => 'menu_entry'};

  if ($use_sections) {
    $menu_entry_name = {'type' => 'menu_entry_name'};
    $menu_entry_name->{'contents'} = _copy_contents ($name_contents);
    foreach my $content (@{$menu_entry_name->{'contents'}}) {
      $content->{'parent'} = $menu_entry_name;
    }
  }

  my $menu_entry_node = {'type' => 'menu_entry_node'};
  $menu_entry_node->{'contents'}
    = _copy_contents ($node_contents);

  foreach my $content (@{$menu_entry_node->{'contents'}}) {
    $content->{'parent'} = $menu_entry_node;
  }
  Texinfo::Common::protect_colon_in_tree($menu_entry_node);

  my $description = {'type' => 'menu_entry_description'};
  $description->{'contents'}->[0] = {'type' => 'preformatted',
                                     'parent' => $description};
  $description->{'contents'}->[0]->{'contents'}->[0] = {'text' =>"\n",
         'parent' => $description->{'contents'}->[0]};

  if ($use_sections) {
    $entry->{'args'} 
     = [{'text' => '* ', 'type' => 'menu_entry_leading_text'},
       $menu_entry_name, 
       {'text' => ': ', 'type' => 'menu_entry_separator'},
       $menu_entry_node, 
       {'text' => '.', 'type' => 'menu_entry_separator'},
       $description];
  } else {
    $entry->{'args'} 
     = [{'text' => '* ', 'type' => 'menu_entry_leading_text'},
       $menu_entry_node, 
       {'text' => '::', 'type' => 'menu_entry_separator'},
       $description];
  }

  foreach my $arg(@{$entry->{'args'}}) {
    $arg->{'parent'} = $entry;
  }
  $entry->{'extra'}->{'menu_entry_name'} = $menu_entry_name;

  $entry->{'extra'}->{'menu_entry_node'} =
    Texinfo::Common::parse_node_manual($menu_entry_node);
  my $content = $entry->{'extra'}->{'menu_entry_node'}->{'node_content'};
  if ($content) {
    $entry->{'extra'}->{'menu_entry_node'}->{'normalized'}
     = Texinfo::Convert::NodeNameNormalization::normalize_node(
         {'contents' => $content } );
  }

  $entry->{'extra'}->{'menu_entry_description'} = $description;

  return $entry;
}

sub new_block_command($$$)
{
  my $block_contents = shift;
  my $parent = shift;
  my $command_name = shift;

  my $end = {'cmdname' => 'end', 'extra' => 
                 {'command_argument' => $command_name,
                  'text_arg' => $command_name}};
  push @{$end->{'args'}},
    {'type' => 'line_arg', 'parent' => $end};
  push @{$end->{'args'}->[0]->{'contents'}},
         ({'text' => $command_name, 'parent' => $end->{'args'}->[0]},
          {'type' => 'spaces_at_end', 'text' => "\n", 
           'parent' => $end->{'args'}->[0]});
  $end->{'args'}->[0]->{'extra'} = {'spaces_before_argument' => ' '};
  my $new_block = {'cmdname' => $command_name, 'parent' => $parent,
                  'extra'=>{'end_command' => $end}};
  $new_block->{'contents'} = [{'type' => 'empty_line_after_command',
                               'text' => "\n"},
                              @$block_contents, $end];
  foreach my $content (@{$new_block->{'contents'}}) {
    $content->{'parent'} = $new_block;
  }
  return $new_block;
}

sub new_complete_node_menu
{
  my ($self, $node, $use_sections) = @_;

  my @node_childs = get_node_node_childs($node);

  if (not scalar(@node_childs)) {
    return;
  }

  my @pending;
  for my $child (@node_childs) {
    my $entry = new_node_menu_entry($self, $child, $use_sections);
    push @pending, $entry;
  }

  my $section = $node->{'extra'}->{'associated_section'};
  my $current_menu = new_block_command (\@pending, $section, 'menu');

  return $current_menu;
}

sub _sort_string($$)
{
  my $a = shift;
  my $b = shift;
  return (($a =~ /^[[:alpha:]]/ and $b =~ /^[[:alpha:]]/)
              or ($a !~ /^[[:alpha:]]/ and $b !~ /^[[:alpha:]]/))
              ? ($a cmp $b)
                : (($a =~ /^[[:alpha:]]/ && 1) || -1);
}

sub _sort_index_entries($$)
{
  my $key1 = shift;
  my $key2 = shift;
  my $a = uc($key1->{'key'});
  my $b = uc($key2->{'key'});
  my $res = _sort_string($a, $b);
  if ($res == 0) {
    $res = ($key1->{'number'} <=> $key2->{'number'});
  }
  # This may happen if 2 indices are merged as the number is per 
  # index name.  The @-command should be different though, for 
  # index names to be different.
  if ($res == 0) {
    $res = ($key1->{'index_at_command'} cmp $key2->{'index_at_command'});
  }
  return $res;
}

sub _sort_index_entries_in_letter($$)
{
  my $key1 = shift;
  my $key2 = shift;
  my $a = uc($key1->{'key'});
  my $b = uc($key2->{'key'});
  my $res = ($a cmp $b);
  if ($res == 0) {
    $res = ($key1->{'number'} <=> $key2->{'number'});
  }
  if ($res == 0) {
    $res = ($key1->{'index_at_command'} cmp $key2->{'index_at_command'});
  }
  return $res;
}

# Go through all the index entries and set 'key', the sort key, on
# each one.
sub do_index_keys($$)
{
  my $self = shift;
  my $index_names = shift;
  my $ignore_chars = '';

  # '-' must come first to avoid e.g. [<-@] looking like a character range
  $ignore_chars .= '-'
    if defined $self->{'values'}->{'txiindexhyphenignore'};
  $ignore_chars .= '\\\\' # string with 2 \s, for escaping inside regex
    if defined $self->{'values'}->{'txiindexbackslashignore'};
  $ignore_chars .= '<'
    if defined $self->{'values'}->{'txiindexlessthanignore'};
  $ignore_chars .= '@'
    if defined $self->{'values'}->{'txiindexatsignignore'};

  my $options = {'sort_string' => 1};
  if ($self->get_conf('ENABLE_ENCODING') 
      and $self->{'info'}->{'input_encoding_name'}) {
    $options->{'enabled_encoding'} = $self->{'info'}->{'input_encoding_name'};
  }

  if ($self->get_conf('ENABLE_ENCODING')) {
    if ($self->get_conf('OUTPUT_ENCODING_NAME')) {
      $options->{'enabled_encoding'} = $self->get_conf('OUTPUT_ENCODING_NAME');
    }
  }
  $options->{'expanded_formats_hash'} = $self->{'expanded_formats_hash'};

  foreach my $index_name (keys(%$index_names)) {
    foreach my $entry (@{$index_names->{$index_name}->{'index_entries'}}) {
      $options->{'code'} = $entry->{'in_code'};
      if (defined $entry->{'sortas'}) {
        $entry->{'key'} = $entry->{'sortas'};
      } else {
        $entry->{'key'} = Texinfo::Convert::Text::convert(
                              {'contents' => $entry->{'content'}}, $options);
        if ($ignore_chars) {
          $entry->{'key'} =~ s/[$ignore_chars]//g;
        }
      }
      if ($entry->{'key'} !~ /\S/) {
        $self->line_warn(sprintf(__("empty index key in \@%s"), 
                                 $entry->{'index_at_command'}),
                        $entry->{'command'}->{'line_nr'});
      }
      # This avoids varying results depending on whether the string is
      # represented internally in UTF-8.  See "the Unicode bug" in the
      # "perlunicode" man page.
      utf8::upgrade($entry->{'key'});
    }
  }
}

sub sort_indices($$$)
{
  my $self = shift;
  my $index_entries = shift;
  my $index_names = shift;
  my $sorted_index_entries;
  do_index_keys($self, $index_names);
  foreach my $index_name (keys(%$index_entries)) {
    @{$sorted_index_entries->{$index_name}} = 
        sort _sort_index_entries 
            grep {$_->{'key'} =~ /\S/} @{$index_entries->{$index_name}};
  }
  return $sorted_index_entries;
}

sub sort_indices_by_letter($$$)
{
  my $self = shift;
  my $index_entries = shift;
  my $index_names = shift;
  my $indices_sorted_by_letters;
  do_index_keys($self, $index_names);
  foreach my $index_name (keys(%$index_entries)) {
    my $index_letter_hash;
    foreach my $index_entry (@{$index_entries->{$index_name}}) {
      next if ($index_entry->{'key'} !~ /\S/);
      my $letter = uc(substr($index_entry->{'key'}, 0, 1));
      push @{$index_letter_hash->{$letter}}, $index_entry;
    }
    foreach my $letter (sort _sort_string (keys %$index_letter_hash)) {
      my @sorted_letter_entries 
         = sort _sort_index_entries_in_letter @{$index_letter_hash->{$letter}};
      push @{$indices_sorted_by_letters->{$index_name}},
        { 'letter' => $letter, 'entries' => \@sorted_letter_entries }; 
    }
  }
  return $indices_sorted_by_letters;
}

sub merge_indices($)
{
  my $index_names = shift;

  my $merged_index_entries;
  foreach my $index_name (keys(%$index_names)) {
    my $index_info = $index_names->{$index_name};
    next if ($index_info->{'merged_in'});
    foreach my $contained_index (keys (%{$index_info->{'contained_indices'}})) {
      if ($index_names->{$contained_index}->{'index_entries'}) {
        push @{$merged_index_entries->{$index_name}},
          @{$index_names->{$contained_index}->{'index_entries'}};
      }
    }
  }
  return $merged_index_entries;
}


1;

__END__

#Last,
#C<output_internal_links> may be used to output element and
#index entries references, mostly for HTML output.

=head1 NAME

Texinfo::Structuring - information on Texinfo::Parser tree

=head1 SYNOPSIS

  use Texinfo::Structuring qw(sectioning_structure nodes_tree number_floats
    associate_internal_references split_by_node split_by_section split_pages
    merge_indices sort_indices_by_letter sort_indices elements_directions
    elements_file_directions);
  # $tree is a Texinfo document tree.  $parser is a Texinfo::Parser object.
  my $sections_root = sectioning_structure ($parser, $tree);
  set_menus_node_directions($parser);
  my $top_node = nodes_tree($parser);
  complete_node_tree_with_menus($parser, $top_node);
  number_floats($parser->floats_information());
  associate_internal_references($parser);
  my $elements;
  if ($split_at_nodes) {
    $elements = split_by_node($tree);
  } else {
    $elements = split_by_section($tree);
  }
  split_pages($elements, $split);
  elements_directions($parser, $elements);
  elements_file_directions($parser, $elements);

  my $index_names = $parser->indices_information();
  my $merged_index_entries
     = merge_indices($index_names);
  my $index_entries_sorted;
  if ($sort_by_letter) {
    $index_entries_sorted = sort_indices_by_letter($parser,
                                       $merged_index_entries, $index_names);
  } else {
    $index_entries_sorted = sort_indices($parser, $merged_index_entries,
                                         $index_names);
  }
  
  
=head1 DESCRIPTION

Texinfo::Structuring first allows to collect informations on a Texinfo tree.
In most case, it also requires a parser object to do that job.  Thanks to
C<sectioning_structure> the hierarchy of sectioning commands is determined.
The directions implied by menus are determined with
C<set_menus_node_directions>.  The node tree is analysed with C<nodes_tree>.
Nodes directions are completed with menu directions with
C<complete_node_tree_with_menus>.  Floats get their standard numbering with
C<number_floats> and internal references are matched up with nodes, floats or
anchors with C<associate_internal_references>.

It is also possible to group the top-level contents of the tree, which consist
in nodes and sectioning commands into elements that group together a node and
the next sectioning element.  With C<split_by_node> nodes are considered
to be the main sectioning elements, while with C<split_by_section> the 
sectioning command elements are the main elements.  The first mode is typical
of Info format, while the second correspond to a traditional book.
The elements may be further split in I<pages>, which are not pages as
in book pages, but more like web pages, and hold series of elements.

The elements may have directions to other elements prepared 
by C<elements_directions>.  C<elements_file_directions> should also
set direction related to files, provided files are associated with 
elements by the user.

C<merge_indices> may be used to merge indices, which may be sorted
with C<sort_indices> or C<sort_indices_by_letter> to sort by letters.

Other miscellaneous methods include C<set_menus_to_simple_menu> and
C<menu_to_simple_menu> to change the menu texinfo tree, as well
as C<insert_nodes_for_sectioning_commands> that adds nodes for 
sectioning commands without nodes and C<complete_tree_nodes_menus>
that completes the node menus based on the sectioning tree.



=head1 METHODS

No method is exported in the default case.

Most of those function references takes a Texinfo::Parser object
as argument, see L<Texinfo::Parser>.

=over

=item $sections_root = sectioning_structure ($parser, $tree)

This function goes through the tree and gather information on
the document structure for sectioning commands.  It returns the 
root of the sectioning commands tree.

For section elements, it sets:

=over

=item level

The level in the sectioning tree hierarchy.  0 is for C<@top> or 
C<@part>, 1 for C<@chapter>, C<@appendix>...  This level is corrected
by C<@raisesections> and C<@lowersections>.

=item number

The sectioning element number.

=item section_childs

An array holding sectioning elements children of the element.

=item section_up

=item section_prev

=item section_next

The up, previous and next sectioning elements.

=item toplevel_next

=item toplevel_prev

=item toplevel_up

The next and previous and up sectioning elements of toplevel sectioning
elements (like C<@top>, C<@chapter>, C<@appendix>), not taking into 
account C<@part> elements.

=back

=item set_menus_node_directions($parser)

Goes through menu and set directions.

=over

=item menu_child

The first child in the menu of the node.

=item menu_up

=item menu_next

=item menu_prev

Up, next and previous directions as set in menus.

=item node_up

=back

=item my $top_node = nodes_tree($parser)

Goes through nodes and set directions.  Returns the top
node.

This functions sets:

=over

=item node_up

=item node_prev

=item node_next

Up, next and previous directions for the node.

=back

=item complete_node_tree_with_menus($parser, $top_node)

Complete nodes directions with menu directions.  Check consistency
of menus, sectionning and nodes direction structures.  Check that
all the nodes are referenced (in menu, @*ref or node direction).

=item number_floats($float_information)

Number the floats as described in the Texinfo manual.  Sets
the I<number> key of the float tree elements.

=item associate_internal_references($parser)

Verify that internal references (C<@ref> and similar without
fourth of fifth argument) have an associated node, anchor or float.
Set the I<label> key in the I<extra> hash of the reference tree
element to the associated labeled tree element.

=item warn_non_empty_parts($parser)

Register a warning in C<$parser> for each C<@part> that is not empty.

=item $elements = split_by_node($tree)

Returns a reference array of elements where a node is associated to
the following sectioning commands.  Sectioning commands without nodes
are also with the previous node, while nodes without sectioning commands
are alone in their elements.

Elements are regular tree items with type I<element>, the
associated nodes and sectioning tree items are in the array associated
with the I<contents> key.  They have directions, namely I<element_next>
and I<element_prev> pointing to the previous and the next element.

In the I<extra> hash they have

=over

=item no_node

A special case, if there are no nodes in the document, the value is set.

=item node

=item element_command

The node command associated with the element.

=item section

The sectioning command associated with the element node.

=back

=item $elements = split_by_section($tree) 

Similarly with C<split_by_node>, returns an array of elements.  This time,
lone nodes are associated with the previous sections and lone sections
makes up an element.

The extra hash keys set are the same, except that I<element_command> is 
the sectioning command associated with the element, and I<no_node> is 
replaced by I<no_section>.

=item $pages = split_pages($elements, $split)

The elements from the array reference argument have an extra I<first_in_page>
value set to the first element on the unit, and based on the
value of I<$split>.  The possible values for I<$split> are

=over

=item chapter

The elements are split at chapter or other toplevel sectioning elements.

=item node

Each element has its own page.

=item section

The elements are split at sectioning commands below chapter.

=item value evaluating to false

No splitting, only one page is returned, holding all the elements.

=back

=item elements_directions($parser, $elements)

Directions are set up for the elements in the array reference given in 
argument.  The corresponding hash reference is in 
C<< {'extra'}->{'directions'} >>
and keys correspond to directions while values are elements.

The following directions are set up:

=over

=item This

The element itself.

=item Forward

Element next.

=item Back

Previous element.

=item NodeForward

Following node element in reading order.  It is the next node, or the 
first in menu or the next of the up node.

=item NodeBack

Preceding node element.

=item NodeUp

=item NodeNext

=item NodePrev

The up, next and previous node elements.

=item Up

=item Next

=item Prev

The up, next and previous section elements.

=item FastForward

The next top level section element.

=item FastBack

For top level elements, the previous top level element.  For other elements
the up top level element.  For example, for a chapter element it is the 
previous chapter, for a subsection element it is the chapter element 
that contains the subsection.

=item FastForward

The next top level element.

=back

=item elements_file_directions($parser, $elements)

In the directions reference described above for C<elements_directions>, sets
the I<PrevFile> and C<NextFile> directions to the elements in previous and
following files.  

It also sets C<FirstInFile*> directions for all the elements by using
the directions of the first element in file.  So, for example, 
C<FirstInFileNodeNext> is the next node of the first element in
the file of each element.

The API for association of pages/elements to files is not defined yet.

=item $merged_entries = merge_indices($index_names)

Using informations returned by L<Texinfo::Parser/indices_information>,
a structure holding all the index entries by index name is returned, 
with all the entries of merged indices merged with those of the indice 
merged into.

The I<$merged_entries> returned is a hash reference whose
keys are the index names and values arrays of index entry structures
described in details in L<Texinfo::Parser/index_entries>.

=item $index_entries_sorted = sort_indices_by_letter($parser, $merged_index_entries, $index_names)

=item $index_entries_sorted = sort_indices($parser, $merged_index_entries, $index_names)

These functions first sets a plain text key for each index entry, used for 
sorting.  In both cases, a hash reference with index names as keys is returned.

When sorting by letter, an array reference of letter hash references is 
associated with each index name.  Each letter hash reference has two 
keys, a I<letter> key with the letter, and an I<entries> key with an array
reference of sorted index entries beginning with the letter.

When simply sorting, the array of the sorted indes entries is associated
with the index name.

=back

=head1 SEE ALSO

L<Texinfo manual|http://www.gnu.org/s/texinfo/manual/texinfo/>, 
L<Texinfo::Parser>.

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=cut
