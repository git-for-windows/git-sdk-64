# Transformations.pm: some transformations of the document tree
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

package Texinfo::Transformations;

use 5.00405;

use strict;

use Texinfo::Common;
use Texinfo::Structuring;

use Carp qw(cluck);

# Add raise/lowersections to be back at the normal level
sub _correct_level($$;$)
{
  my $section = shift;
  my $parent = shift;
  my $modifier = shift;
  $modifier = 1 if (!defined($modifier));

  my @result;
  if ($section->{'extra'} and $section->{'extra'}->{'sections_level'}) {
    my $level_to_remove = $modifier * $section->{'extra'}->{'sections_level'};
    my $command;
    if ($level_to_remove < 0) {
      $command = 'raisesection';
    } else {
      $command = 'lowersection';
    }
    my $remaining_level = abs($level_to_remove);
    while ($remaining_level) {
      push @result, {'cmdname' => $command,
                     'parent' => $parent};
      push @result, {'type' => 'empty_line', 'text' => "\n",
                     'parent' => $parent};
      $remaining_level--;
    }
  }
  return @result;
}

sub fill_gaps_in_sectioning($)
{
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'}) {
    return undef;
  }
  my @sections_list;
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'} and $content->{'cmdname'} ne 'node'
        and $content->{'cmdname'} ne 'bye') {
      push @sections_list, $content;
    }
  }

  return (undef, undef) if (!scalar(@sections_list));

  my @added_sections;
  my @contents;
  my $previous_section;
  foreach my $content(@{$root->{'contents'}}) {
    push @contents, $content;
    if (!@sections_list or $sections_list[0] ne $content) {
      next;
    }
    my $current_section = shift @sections_list;
    my $current_section_level
       = Texinfo::Structuring::section_level($current_section);
    my $next_section = $sections_list[0];
    
    if (defined($next_section)) {
      my $next_section_level
                        = Texinfo::Structuring::section_level($next_section);

      if ($next_section_level - $current_section_level > 1) {
        my @correct_level_offset_commands = _correct_level($next_section,
                                                          $contents[-1]);
        if (@correct_level_offset_commands) {
          push @{$contents[-1]->{'contents'}}, @correct_level_offset_commands;
        }
        #print STDERR "* $current_section_level "._print_root_command_texi($current_section)."\n";
        #print STDERR "  $next_section_level "._print_root_command_texi($next_section)."\n";
        while ($next_section_level - $current_section_level > 1) {
          $current_section_level++;
          my $new_section = {'cmdname' =>
            $Texinfo::Common::level_to_structuring_command{'unnumbered'}->[$current_section_level],
            'parent' => $root,
          };
          $new_section->{'contents'} = [{'type' => 'empty_line', 
                                         'text' => "\n",
                                         'parent' => $new_section}];
          $new_section->{'args'} = [{'type' => 'line_arg',
                                     'parent' => $new_section}];
          $new_section->{'args'}->[0]->{'contents'} = [
             {'cmdname' => 'asis',
              'parent' => $new_section->{'args'}->[0]
             },
             {'type' => 'spaces_at_end',
              'text' => "\n",
              'parent' => $new_section->{'args'}->[0]
             }];
          $new_section->{'args'}->[0]->{'extra'}
            = {'spaces_before_argument' => ' '};
          $new_section->{'args'}->[0]->{'contents'}->[0]->{'args'}
             = [{'type' => 'brace_command_arg',
                 'contents' => [],
                 'parent' => $new_section->{'args'}->[0]->{'contents'}->[1]}];
          push @contents, $new_section;
          push @added_sections, $new_section;
          #print STDERR "  -> "._print_root_command_texi($new_section)."\n";
        }
        my @set_level_offset_commands = _correct_level($next_section,
                                                       $contents[-1], -1);
        if (@set_level_offset_commands) {
          push @{$contents[-1]->{'contents'}}, @set_level_offset_commands;
        }
      }
    }
  }
  return (\@contents, \@added_sections);
}

# This converts a reference @-command to simple text using one of the 
# arguments.  This is used to remove reference @-command from 
# constructed node names trees, as node names cannot contain
# reference @-command while there could be some in the tree used in 
# input for the node name tree.
sub _reference_to_arg($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  if ($current->{'cmdname'} and 
      $Texinfo::Common::ref_commands{$current->{'cmdname'}}
      and $current->{'args'}) {
    my @args_try_order;
    if ($current->{'cmdname'} eq 'inforef') {
      @args_try_order = (0, 1, 2);
    } else {
      @args_try_order = (0, 1, 2, 4, 3);
    }
    foreach my $index (@args_try_order) {
      if (defined($current->{'args'}->[$index])) {
        # This a double checking that there is some content.
        # Not sure that it is useful.
        my $text = Texinfo::Convert::Text::convert($current->{'args'}->[$index]);
        if (defined($text) and $text =~ /\S/) {
          my $result
            = {'contents' => $current->{'args'}->[$index]->{'contents'},
                        'parent' => $current->{'parent'}};
          return ($result);
        }
      }
    }
    return {'text' => '', 'parent' => $current->{'parent'}};
  } else {
    return ($current);
  }
}

sub reference_to_arg_in_tree($$)
{
  my $self = shift;
  my $tree = shift;
  return Texinfo::Common::modify_tree($self, $tree, \&_reference_to_arg);
}

# prepare a new node
sub _new_node($$)
{
  my $self = shift;
  my $node_tree = shift;

  $node_tree = Texinfo::Common::protect_comma_in_tree($node_tree);
  $node_tree->{'contents'} 
     = Texinfo::Common::protect_first_parenthesis($node_tree->{'contents'});
  $node_tree = reference_to_arg_in_tree($self, $node_tree);

  my $empty_node = 0;
  if (!$node_tree->{'contents'} 
      or !scalar(@{$node_tree->{'contents'}})) {
    $node_tree->{'contents'} = [{'text' => ''}];
    $empty_node = 1;
  }

  unless (($node_tree->{'contents'}->[-1]->{'cmdname'}
       and ($node_tree->{'contents'}->[-1]->{'cmdname'} eq 'c'
            or $node_tree->{'contents'}->[-1]->{'cmdname'} eq 'comment'))
      or (defined($node_tree->{'contents'}->[-1]->{'text'})
          and $node_tree->{'contents'}->[-1]->{'text'} =~ /\n/)) {
    push @{$node_tree->{'contents'}}, 
           {'type' => 'spaces_at_end', 'text' => "\n"};
  }

  my $appended_number = 0 +$empty_node;
  my ($node, $parsed_node);

  while (!defined($node) 
         or ($self->{'labels'} 
            and $self->{'labels'}->{$parsed_node->{'normalized'}})) {
    $node = {'cmdname' => 'node', 'args' => [{}]};
    my $node_arg = $node->{'args'}->[0];
    $node_arg->{'parent'} = $node;
    @{$node_arg->{'contents'}} = (
       {'extra' => {'command' => $node},
        'text' => ' ',
        'type' => 'empty_spaces_after_command'},
        @{$node_tree->{'contents'}});
    if ($appended_number) {
      splice (@{$node_arg->{'contents'}}, -1, 0,
                  {'text' => " $appended_number"});
    }
    foreach my $content (@{$node_arg->{'contents'}}) {
      $content->{'parent'} = $node_arg;
    }
    $parsed_node = Texinfo::Common::parse_node_manual($node_arg);
    if ($parsed_node and $parsed_node->{'node_content'}) {
      $parsed_node->{'normalized'} =
      Texinfo::Convert::NodeNameNormalization::normalize_node (
        { 'contents' => $parsed_node->{'node_content'} });
    }
    if (!defined($parsed_node) or !$parsed_node->{'node_content'}
        or $parsed_node->{'normalized'} !~ /[^-]/) {
      if ($appended_number) {
        return undef;
      } else {
        $node = undef;
      }
    }
    $appended_number++;
  }

  push @{$node->{'extra'}->{'nodes_manuals'}}, $parsed_node;
  if ($parsed_node->{'normalized'} ne '') {
    $self->{'labels'}->{$parsed_node->{'normalized'}} = $node;
    $node->{'extra'}->{'normalized'} = $parsed_node->{'normalized'};
  }
  push @{$self->{'targets'}}, $node;
  if ($parsed_node->{'node_content'}) {
    $node->{'extra'}->{'node_content'} = $parsed_node->{'node_content'};
  }
  push @{$self->{'nodes'}}, $node;
  return $node;
}

# reassociate a tree element to the new node, from previous node
sub _reassociate_to_node($$$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;
  my $nodes = shift;
  my ($new_node, $previous_node) = @{$nodes};

  if ($current->{'cmdname'} and $current->{'cmdname'} eq 'menu') {
    if ($previous_node) {
      if (!$previous_node->{'menus'} or !@{$previous_node->{'menus'}}
           or !grep {$current eq $_} @{$previous_node->{'menus'}}) {
        print STDERR "Bug: menu $current not in previous node $previous_node\n";
      } else {
        @{$previous_node->{'menus'}} = grep {$_ ne $current} @{$previous_node->{'menus'}};
        delete $previous_node->{'menus'} if !(@{$previous_node->{'menus'}});
      }
    }
    push @{$new_node->{'menus'}}, $current;
  } elsif ($current->{'extra'} and $current->{'extra'}->{'index_entry'}) {
    if ($previous_node 
        and (!$current->{'extra'}->{'index_entry'}->{'node'}
             or $current->{'extra'}->{'index_entry'}->{'node'} ne $previous_node)) {
      print STDERR "Bug: index entry $current (".
        Texinfo::Convert::Texinfo::convert ({'contents' => $current->{'extra'}->{'index_entry'}->{'content'}})
         .") not in previous node $previous_node\n";
      print STDERR "  previous node: "._print_root_command_texi($previous_node)."\n";
      if ($current->{'extra'}->{'index_entry'}->{'node'}) {
        print STDERR "  current node: ".
         _print_root_command_texi($current->{'extra'}->{'index_entry'}->{'node'})."\n";
      } else {
        print STDERR "  current node not set\n";
      }
    }
    $current->{'extra'}->{'index_entry'}->{'node'} = $new_node;
  }
  return ($current);
}

sub insert_nodes_for_sectioning_commands($$)
{
  my $self = shift;
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'}) {
    return (undef, undef);
  }
  my @added_nodes;
  my @contents;
  my $previous_node;
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'} and $content->{'cmdname'} ne 'node'
        and $content->{'cmdname'} ne 'bye'
        and $content->{'cmdname'} ne 'part'
        and not ($content->{'extra'} 
                 and $content->{'extra'}->{'associated_node'})) {
      my $new_node_tree;
      if ($content->{'cmdname'} eq 'top') {
        $new_node_tree = {'contents' => [{'text' => 'Top'}]};
      } else {
        $new_node_tree = Texinfo::Common::copy_tree({'contents' 
          => $content->{'args'}->[0]->{'contents'}});
      }
      my $new_node = _new_node($self, $new_node_tree);
      if (defined($new_node)) {
        push @contents, $new_node;
        push @added_nodes, $new_node;
        $new_node->{'extra'}->{'associated_section'} = $content;
        $content->{'extra'}->{'associated_node'} = $new_node;
        $new_node->{'parent'} = $content->{'parent'};
        # reassociate index entries and menus
        Texinfo::Common::modify_tree($self, $content, \&_reassociate_to_node,
                                     [$new_node, $previous_node]);
      }
    }
    # check normalized to avoid erroneous nodes, such as duplicates
    $previous_node = $content 
      if ($content->{'cmdname'} 
          and $content->{'cmdname'} eq 'node'
          and $content->{'extra'}->{'normalized'});
    push @contents, $content;
  }
  return (\@contents, \@added_nodes);
}

sub _prepend_new_menu_in_node_section($$$)
{
  my $node = shift;
  my $section = shift;
  my $current_menu = shift;

  if (not defined($current_menu)) {
    cluck "input menu undef";
  }
  push @{$section->{'contents'}}, $current_menu;
  push @{$section->{'contents'}}, {'type' => 'empty_line',
                                   'text' => "\n",
                                   'parent' => $section};
  push @{$node->{'menus'}}, $current_menu;
}

sub complete_node_menu($$;$)
{
  my $self = shift;
  my $node = shift;
  my $use_sections = shift;

  my @node_childs = Texinfo::Structuring::get_node_node_childs($node);

  if (scalar(@node_childs)) {
    my %existing_entries;
    if ($node->{'menus'} and @{$node->{'menus'}}) {
      foreach my $menu (@{$node->{'menus'}}) {
        foreach my $entry (@{$menu->{'contents'}}) {
          if ($entry->{'type'} and $entry->{'type'} eq 'menu_entry') {
            my $entry_node = $entry->{'extra'}->{'menu_entry_node'};
            if (! $entry_node->{'manual_content'}
                and defined($entry_node->{'normalized'})) {
              $existing_entries{$entry_node->{'normalized'}} 
                = [$menu, $entry];
            }
          }
        }
      }
    }
    #print STDERR join('|', keys(%existing_entries))."\n";
    my @pending;
    my $current_menu;
    foreach my $node_entry (@node_childs) {
      if ($existing_entries{$node_entry->{'extra'}->{'normalized'}}) {
        my $entry;
        ($current_menu, $entry) 
         = @{$existing_entries{$node_entry->{'extra'}->{'normalized'}}};
        if (@pending) {
          my $index;
          for ($index = 0; $index < scalar(@{$current_menu->{'contents'}}); $index++) {
            #print STDERR "$index, ".scalar(@{$current_menu->{'contents'}})."\n";
            last if ($current_menu->{'contents'}->[$index] eq $entry);
          }
          splice (@{$current_menu->{'contents'}}, $index, 0, @pending);
          foreach my $entry (@pending) {
            $entry->{'parent'} = $current_menu;
          }
          @pending = ();
        }
      } else {
        my $entry = Texinfo::Structuring::new_node_menu_entry($self, 
                              $node_entry, $use_sections);
        push @pending, $entry;
      }
    }
    if (scalar(@pending)) {
      if (!$current_menu) {
        my $section = $node->{'extra'}->{'associated_section'};
        $current_menu =
       Texinfo::Structuring::new_block_command (\@pending, $section, 'menu');
        _prepend_new_menu_in_node_section($node, $section, $current_menu);
      } else {
        foreach my $entry (@pending) {
          $entry->{'parent'} = $current_menu;
        }
        my $end;
        if ($current_menu->{'contents'}->[-1]->{'cmdname'}
            and $current_menu->{'contents'}->[-1]->{'cmdname'} eq 'end') {
          $end = pop @{$current_menu->{'contents'}};
        }
        push @{$current_menu->{'contents'}}, @pending;
        push @{$current_menu->{'contents'}}, $end if ($end);
      }
    }
  }
}

sub _get_non_automatic_nodes_with_sections($)
{
  my $root = shift;

  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'}) {
    return undef;
  }
  my @non_automatic_nodes;
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'} and $content->{'cmdname'} eq 'node'
        and (scalar(@{$content->{'extra'}->{'nodes_manuals'}}) == 1)
        and $content->{'extra'} 
        and $content->{'extra'}->{'associated_section'}) {
      push @non_automatic_nodes, $content;
    }
  }
  return [ @non_automatic_nodes ];
}

# This should be called after Texinfo::Structuring::sectioning_structure.
sub complete_tree_nodes_menus($$;$)
{
  my $self = shift;
  my $root = shift;
  my $use_sections = shift;

  my $non_automatic_nodes = _get_non_automatic_nodes_with_sections($root);
  if (not defined($non_automatic_nodes)) {
    return undef;
  }
  foreach my $node (@{$non_automatic_nodes}) {
    complete_node_menu($self, $node, $use_sections);
  }
}

# this only complete menus if there was no menu
sub complete_tree_nodes_missing_menu($$;$)
{
  my $self = shift;
  my $root = shift;
  my $use_sections = shift;

  my $non_automatic_nodes = _get_non_automatic_nodes_with_sections($root);
  if (not defined($non_automatic_nodes)) {
    return undef;
  }
  foreach my $node (@{$non_automatic_nodes}) {
    if (not $node->{'menus'} or not scalar(@{$node->{'menus'}})) {
      my $section = $node->{'extra'}->{'associated_section'};
      my $current_menu = Texinfo::Structuring::new_complete_node_menu($self, $node, $use_sections);
      if (defined($current_menu)) {
        _prepend_new_menu_in_node_section($node, $section, $current_menu);
      }
    }
  }
}

sub _copy_contents($)
{
  my $contents = shift;
  my $copy = Texinfo::Common::copy_tree({'contents' => $contents});
  return $copy->{'contents'};
}

sub _print_down_menus($$;$);
sub _print_down_menus($$;$)
{
  my $self = shift;
  my $node = shift;
  my $labels = shift;
  $labels = $self->labels_information() if (!defined($labels));

  my @master_menu_contents;

  if ($node->{'menus'} and scalar(@{$node->{'menus'}})) {
    my @node_children;
    foreach my $menu (@{$node->{'menus'}}) {
      foreach my $entry (@{$menu->{'contents'}}) {
        if ($entry->{'type'} and $entry->{'type'} eq 'menu_entry') {
          push @master_menu_contents, Texinfo::Common::copy_tree($entry);
          # gather node children to recusrsively print their menus
          my $entry_node = $entry->{'extra'}->{'menu_entry_node'};
          if (! $entry_node->{'manual_content'}
              and defined($entry_node->{'normalized'})) {
            my $node = $labels->{$entry_node->{'normalized'}};
            if (defined($node) and $node->{'extra'}) {
              push @node_children, $node;
            }
          }
        }
      }
    }
    if (scalar(@master_menu_contents)) {
      # Prepend node title
      my $node_title_contents;
      if ($node->{'extra'}->{'associated_section'}
          and $node->{'extra'}->{'associated_section'}->{'args'}
          and $node->{'extra'}->{'associated_section'}->{'args'}->[0]
          and $node->{'extra'}->{'associated_section'}->{'args'}->[0]->{'contents'}) {
        $node_title_contents
          = _copy_contents($node->{'extra'}->{'associated_section'}->{'args'}->[0]->{'contents'});
      } else {
        $node_title_contents = _copy_contents($node->{'extra'}->{'node_content'});
      }
      my $menu_comment = {'type' => 'menu_comment'};
      $menu_comment->{'contents'}->[0] = {'type' => 'preformatted',
                                          'parent' => $menu_comment};
    
      $menu_comment->{'contents'}->[0]->{'contents'}
        = [{'text' => "\n", 'type' => 'empty_line'}, @$node_title_contents,
           {'text' => "\n", 'type' => 'empty_line'},
           {'text' => "\n", 'type' => 'empty_line'}];
      foreach my $content (@{$menu_comment->{'contents'}->[0]->{'contents'}}) {
        $content->{'parent'} = $menu_comment->{'contents'}->[0];
      }
      unshift @master_menu_contents, $menu_comment;

      # now recurse in the children
      foreach my $child (@node_children) {
        push @master_menu_contents, _print_down_menus($self, $child, $labels);
      }
    }
  }
  return @master_menu_contents;
}

sub new_master_menu($;$)
{
  my $self = shift;
  my $labels = shift;
  $labels = $self->labels_information() if (!defined($labels));
  my $node = $labels->{'Top'};
  return undef if (!defined($node));

  my @master_menu_contents;
  if ($node->{'menus'} and scalar(@{$node->{'menus'}})) {
    foreach my $menu (@{$node->{'menus'}}) {
      foreach my $entry (@{$menu->{'contents'}}) {
        if ($entry->{'type'} and $entry->{'type'} eq 'menu_entry') {
          my $entry_node = $entry->{'extra'}->{'menu_entry_node'};
          if (! $entry_node->{'manual_content'}
              and defined($entry_node->{'normalized'})) {
            my $node = $labels->{$entry_node->{'normalized'}};
            if (defined($node) and $node->{'extra'}) {
              push @master_menu_contents, _print_down_menus($self, 
                                                            $node, $labels);
            }
          }
        }
      }
    }
  }
  if (scalar(@master_menu_contents)) {
    my $first_preformatted = $master_menu_contents[0]->{'contents'}->[0];
    my $master_menu_title = $self->gdt(' --- The Detailed Node Listing ---');
    my @master_menu_title_contents;
    foreach my $content (@{$master_menu_title->{'contents'}}, {'text' => "\n"}) {
      $content->{'parent'} = $first_preformatted;
      push @master_menu_title_contents, $content;
    }
    unshift @{$first_preformatted->{'contents'}}, @master_menu_title_contents;
    return Texinfo::Structuring::new_block_command(\@master_menu_contents, undef, 'detailmenu');
  } else {
    return undef;
  }
}

sub regenerate_master_menu($;$)
{
  my $self = shift;
  my $labels = shift;
  $labels = $self->labels_information() if (!defined($labels));
  my $top_node = $labels->{'Top'};
  return undef if (!defined($top_node));

  my $new_master_menu = new_master_menu($self, $labels);
  return undef if (!defined($new_master_menu) or !$top_node->{'menus'}
                   or !scalar(@{$top_node->{'menus'}}));

  foreach my $menu (@{$top_node->{'menus'}}) {
    my $detailmenu_index = 0;
    foreach my $entry (@{$menu->{'contents'}}) {
      if ($entry->{'cmdname'} and $entry->{'cmdname'} eq 'detailmenu') {
        # replace existing detailmenu by the master menu
        $new_master_menu->{'parent'} = $menu;
        splice (@{$menu->{'contents'}}, $detailmenu_index, 1, 
                $new_master_menu);
        return 1;
      }
      $detailmenu_index++;
    }
  }

  my $last_menu = $top_node->{'menus'}->[-1];
  my $index = scalar(@{$last_menu->{'contents'}});
  if ($index
      and $last_menu->{'contents'}->[$index-1]->{'cmdname'}
      and $last_menu->{'contents'}->[$index-1]->{'cmdname'} eq 'end') {
    $index --;
  }
  $new_master_menu->{'parent'} = $last_menu;
  if ($index
      and $last_menu->{'contents'}->[$index-1]->{'type'}
      and $last_menu->{'contents'}->[$index-1]->{'type'} eq 'menu_comment'
      and $last_menu->{'contents'}->[$index-1]->{'contents'}->[-1]->{'type'}
      and $last_menu->{'contents'}->[$index-1]->{'contents'}->[-1]->{'type'}
             eq 'preformatted') {
    my $empty_line = {'type' => 'empty_line', 'text' => "\n", 'parent' =>
               $last_menu->{'contents'}->[$index-1]->{'contents'}->[-1]};
    push @{$last_menu->{'contents'}->[$index-1]->{'contents'}}, $empty_line;
  } elsif ($index
           and $last_menu->{'contents'}->[$index-1]->{'type'}
           and $last_menu->{'contents'}->[$index-1]->{'type'} eq 'menu_entry') {
    my $menu_comment = {'type' => 'menu_comment', 'parent' => $last_menu};
    splice (@{$last_menu->{'contents'}}, $index, 0, $menu_comment);
    $index++;
    my $preformatted = {'type' => 'preformatted', 'parent' => $menu_comment};
    push @{$menu_comment->{'contents'}}, $preformatted;
    my $empty_line = {'type' => 'after_description_line', 'text' => "\n",
                      'parent' => $preformatted};
    push @{$preformatted->{'contents'}}, $empty_line;
  }
  splice (@{$last_menu->{'contents'}}, $index, 0, $new_master_menu);

  return 1;
}

# modify the menu tree to put description and menu comment content
# together directly in the menu.  Put the menu_entry in a preformatted.
# last merge preformatted.
sub menu_to_simple_menu($);

sub menu_to_simple_menu($)
{
  my $menu = shift;
  
  my @contents;
  foreach my $content (@{$menu->{'contents'}}) {
    if ($content->{'type'} and $content->{'type'} eq 'menu_comment') {
      push @contents, @{$content->{'contents'}};
    } elsif ($content->{'type'} and $content->{'type'} eq 'menu_entry') {
      my $preformatted = {'type' => 'preformatted', 'contents' => [$content]};
      push @contents, $preformatted;
      $content->{'parent'} = $preformatted;

      my $in_description;
      my @args = @{$content->{'args'}};
      @{$content->{'args'}} = ();
      while (@args) {
        if ($args[0]->{'type'} and $args[0]->{'type'} eq 'menu_entry_description') {
          my $description = shift @args;
          push @contents, @{$description->{'contents'}};
          push @contents, @args;
          last;
        } else {
          my $arg = shift @args;
          push @{$content->{'args'}}, $arg;
        }
      }
    } elsif ($content->{'cmdname'}
             and $Texinfo::Common::menu_commands{$content->{'cmdname'}}) {
      menu_to_simple_menu($content);
      push @contents, $content;
    } else {
      push @contents, $content;
    }
  }
  
  # reset parent, put in menu and merge preformatted.
  @{$menu->{'contents'}} = ();
  my $current_preformatted;
  foreach my $content (@contents) {
    $content->{'parent'} = $menu;
    if ($content->{'type'} and $content->{'type'} eq 'preformatted') {
      if (!defined($current_preformatted)) {
        $current_preformatted = $content;
        push @{$menu->{'contents'}}, $content;
      } else {
        foreach my $preformatted_content (@{$content->{'contents'}}) {
          push @{$current_preformatted->{'contents'}}, $preformatted_content;
          $preformatted_content->{'parent'} = $current_preformatted;
        }
      }
    } else {
      $current_preformatted = undef;
      push @{$menu->{'contents'}}, $content;
    }
  }
}

sub set_menus_to_simple_menu($)
{
  my $self = shift;

  if ($self->{'nodes'} and @{$self->{'nodes'}}) {
    foreach my $node (@{$self->{'nodes'}}) {
      if ($node->{'menus'}) {
        foreach my $menu (@{$node->{'menus'}}) {
          menu_to_simple_menu($menu);
        }
      }
    }
  }
}

1;

__END__

=head1 NAME

Texinfo::Transformations - transformations of Texinfo::Parser.pm tree

=head1 SYNOPSIS

  use Texinfo:Transformations;
  
  
=head1 DESCRIPTION

Includes miscellaneous methods C<set_menus_to_simple_menu> and
C<menu_to_simple_menu> to change the menu texinfo tree, as well
as C<insert_nodes_for_sectioning_commands> that adds nodes for 
sectioning commands without nodes and C<complete_tree_nodes_menus>
and C<complete_tree_nodes_missing_menu> that completes the 
node menus based on the sectioning tree.



=head1 METHODS

No method is exported in the default case.

Most of those function references takes a Texinfo::Parser object
as argument, see L<Texinfo::Parser>.

=over


=item ($root_content, $added_sections) = fill_gaps_in_sectioning ($root)

This function adds empty C<@unnumbered> and similar commands in a tree
to fill gaps in sectioning.  This may be used, for example, when converting 
from a format that can handle gaps in sectioning.  I<$root> is the tree
root.  An array reference is returned, containing the root contents
with added sectioning commands, as well as an array reference containing 
the added sectioning commands.

If the sectioning commands are lowered or raised (with C<@raisesections>,
C<@lowersection>) the tree may be modified with C<@raisesections> or
C<@lowersection> added to some tree elements.

=item menu_to_simple_menu ($menu)

=item set_menus_to_simple_menu ($parser)

C<menu_to_simple_menu> transforms the tree of a menu tree element.  
C<set_menus_to_simple_menu> calls C<menu_to_simple_menu> for all the
menus of the document.

A simple menu has no I<menu_comment>, I<menu_entry> or I<menu_entry_description>
container anymore, their content are merged directly in the menu in 
I<preformatted> container.

=item ($root_content, $added_nodes) = insert_nodes_for_sectioning_commands ($parser, $tree)

Insert nodes for sectioning commands without node in C<$tree>.
An array reference is returned, containing the root contents
with added nodes, as well as an array reference containing the 
added nodes.

=item complete_tree_nodes_menus ($parser, $tree, $add_section_names_in_entries)

Add menu entries or whole menus for nodes associated with sections,
based on the sectioning tree.  If the optional 
C<$add_section_names_in_entries> argument is set, a menu entry 
name is added using the section name.  This function should be
called after L<sectioning_structure>.

=item complete_tree_nodes_missing_menu ($parser, $tree, $use_section_names_in_entries)

Add whole menus for nodes associated with sections and without menu,
based on the sectioning tree.  If the optional 
C<$add_section_names_in_entries> argument is set, a menu entry 
name is added using the section name.  This function should be
called after L<sectioning_structure>.

=item $detailmenu = new_master_menu ($parser)

Returns a detailmenu tree element formatted as a master node.

=item regenerate_master_menu ($parser)

Regenerate the Top node master menu, replacing the first detailmenu
in Top node menus or appending at the end of the Top node menu.

=back

=head1 SEE ALSO

L<Texinfo manual|http://www.gnu.org/s/texinfo/manual/texinfo/>, 
L<Texinfo::Parser>.

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=cut
