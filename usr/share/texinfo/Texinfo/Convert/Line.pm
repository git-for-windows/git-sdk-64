# Line.pm: handle line of text.
#
# Copyright 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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

# this module has nothing Texinfo specific.  It is similar with 
# Texinfo::Convert::Paragraph, but simpler.
# The delay to output a word is here to be able to detect when an upper
# case letter is before an end of line

package Texinfo::Convert::Line;

use 5.006;
use strict;

use Unicode::EastAsianWidth;

# initialize a line object.
sub new($;$)
{
  my $class = shift;
  my $conf = shift;
  my $self = {'indent_length' => 0, 'counter' => 0,
              'space' => '', 'frenchspacing' => 0, 'line_beginning' => 1,
              'lines_counter' => 0, 'end_line_count' => 0};
  if (defined($conf)) {
    foreach my $key (keys(%$conf)) {
      if ($key eq 'text') {
        $self->{'counter'} = Texinfo::Convert::Unicode::string_width($conf->{$key});
        $self->{'line_beginning'} = 0 if ($self->{'counter'});
      } else {
        $self->{$key} = $conf->{$key};
      }
    }
  }
  bless $self, $class;
}

# for debug
sub dump($)
{
  my $self = shift;
  my $word = 'UNDEF';
  if (defined($self->{'word'})) {
    $word = $self->{'word'};
  }
  my $end_sentence = 'UNDEF';
  $end_sentence = $self->{'end_sentence'} if (defined($self->{'end_sentence'}));
  print STDERR "line ($self->{'line_beginning'},$self->{'counter'}) word: $word, space `$self->{'space'}' end_sentence: $end_sentence\n"; 
}

sub end_line($)
{
  my $line = shift;
  $line->{'end_line_count'} = 0;
  return $line->_end_line();
}

# end a line.
sub _end_line($)
{
  my $line = shift;
  my $result = $line->_add_pending_word();
  $line->{'line_beginning'} = 1;
  $line->{'space'} = '';
  $line->{'lines_counter'}++;
  $line->{'end_line_count'}++;
  $line->{'counter'} = 0;
  print STDERR "END_LINE.L\n" if ($line->{'DEBUG'});
  return "$result\n";
}

sub end_line_count($)
{
  my $line = shift;
  return $line->{'end_line_count'};
}

sub get_pending($)
{
  my $line = shift;
  my $result = '';
  if ($line->{'space'}) {
    $result .= $line->{'space'};
  }
  if (defined($line->{'word'})) {
    $result .= $line->{'word'};
  }
  return $result;
}

sub add_pending_word($;$)
{
  my $line = shift;
  my $add_spaces = shift;
  $line->{'end_line_count'} = 0;
  return $line->_add_pending_word($add_spaces);
}

# put a pending word and spaces in the result string.
sub _add_pending_word($)
{
  my $line = shift;
  my $add_spaces = shift;
  my $result = '';

  if (defined($line->{'word'}) or $add_spaces) {
    if ($line->{'line_beginning'}) {
      if ($line->{'indent_length'}) {
        $result .= ' ' x ($line->{'indent_length'} - $line->{'counter'});
        print STDERR "INDENT.L($line->{'counter'})\n" if ($line->{'DEBUG'});
      }
      $line->{'line_beginning'} = 0;
    } elsif ($line->{'space'}) {
      $result .= $line->{'space'};
      print STDERR "ADD_SPACES.L\n" if ($line->{'DEBUG'});
    }
    $line->{'space'} = '';
    if (defined($line->{'word'})) {
      $result .= $line->{'word'};
      print STDERR "ADD_WORD.L[$line->{'word'}]\n" if ($line->{'DEBUG'});
      $line->{'word'} = undef;
      $line->{'last_char'} = undef;
    }
  }
  return $result;
}

# end a line
sub end($)
{
  my $line = shift;
  $line->{'end_line_count'} = 0;
  my $result = $line->_add_pending_word();
  $result .= $line->{'space'};
  print STDERR "END_LINE.L\n" if ($line->{'DEBUG'});
  return $result;
}

sub add_next($;$$)
{
  my $line = shift;
  my $word = shift;
  my $transparent = shift;
  $line->{'end_line_count'} = 0;
  return $line->_add_next($word, $transparent);
}

my $end_sentence_character = quotemeta('.?!');
my $after_punctuation_characters = quotemeta('"\')]');

# add a word
sub _add_next($;$$)
{
  my $line = shift;
  my $word = shift;
  my $transparent = shift;
  my $result = '';

  if (defined($word)) {
    my $disinhibit; # full stop after capital letter ends sentence
    if ($word =~ s/\x08$//) {
      $disinhibit = 1;
    }
    if (!defined($line->{'word'})) {
      $line->{'word'} = '';
      $line->{'last_char'} = '';
      if ($line->{'end_sentence'}
          and $line->{'end_sentence'} > 0
          and !$line->{'frenchspacing'}
           and !$line->{'line_beginning'} and $line->{'space'}) {
        if ($word !~ /^\s/) {
          $line->{'space'} .= ' ' x (2 - length($line->{'space'}));
        }
        delete $line->{'end_sentence'};
      }
    }
    $line->{'word'} .= $word;

    if (!$transparent) {
      if ($disinhibit) {
        $line->{'last_char'} = 'a';
      } elsif ($word =~
           /([^$end_sentence_character$after_punctuation_characters])
            [$end_sentence_character$after_punctuation_characters]*$/x) {
        # Save the last character in $word before punctuation
        $line->{'last_char'} = $1;
      }
    }

    if ($line->{'DEBUG'}) {
      print STDERR "WORD+.L $word -> $line->{'word'}\n";
    }
  }

  return $result;
}

sub remove_end_sentence($)
{
  my $line = shift;
  $line->{'end_sentence'} = 0;
}

sub add_end_sentence($;$)
{
  my $line = shift;
  my $value = shift;
  $line->{'end_sentence'} = $value;
}

sub allow_end_sentence($)
{
  my $line = shift;
  $line->{'last_char'} = 'a'; # lower-case
}

sub set_space_protection($$;$$$)
{
  my $line = shift;
  my $space_protection = shift;
  my $ignore_columns = shift;
  my $keep_end_lines = shift;
  my $frenchspacing = shift;
  $line->{'protect_spaces'} = $space_protection 
    if defined($space_protection);
  $line->{'ignore_columns'} = $ignore_columns
    if defined($ignore_columns);
  # a no-op in fact
  $line->{'keep_end_lines'} = $keep_end_lines
    if defined($keep_end_lines);
  if (!$line->{'frenchspacing'} and $frenchspacing
    and $line->{'end_sentence'} and !$line->{'line_beginning'} 
    and $line->{'space'} and !defined($line->{'word'})) {
    $line->{'space'} .= ' ' x (2 - length($line->{'space'}));
    print STDERR "SWITCH.L frenchspacing end sentence space\n" if ($line->{'DEBUG'});
    delete $line->{'end_sentence'};
  }
  $line->{'frenchspacing'} = $frenchspacing
    if defined($frenchspacing);
  # begin a word, to have something even if empty
  if ($space_protection) {
    $line->_add_next('');
  }
  return '';
}

# wrap a text.
sub add_text($$)
{
  my $line = shift;
  my $text = shift;
  $line->{'end_line_count'} = 0;
  my $result = '';

  while ($text ne '') {
    if ($line->{'DEBUG'}) {
      my $word = 'UNDEF';
      $word = $line->{'word'} if (defined($line->{'word'}));
      print STDERR "s `$line->{'space'}', w `$word'\n";
    }
    # \x{202f}\x{00a0} are non breaking spaces
    if ($text =~ s/^([^\S\x{202f}\x{00a0}\n]+)//) {
      my $spaces = $1;
      print STDERR "SPACES.L\n" if ($line->{'DEBUG'});
      if ($line->{'protect_spaces'}) {
        $line->{'word'} .= $spaces;
        $line->{'last_char'} = substr($spaces, -1);
      } else {
        my $added_word = $line->{'word'};
        $result .= $line->_add_pending_word();

        if (!$line->{'line_beginning'}) {
          if (!$line->{'frenchspacing'}
               and $line->{'end_sentence'}
               and $line->{'end_sentence'} > 0) {
            if (length($line->{'space'}) >= 1 or length($spaces) > 1) {
              my $all_spaces = substr($line->{'space'} . $spaces, 0, 2);
              $all_spaces =~ s/[\n\r]/ /g;
              $all_spaces .= ' ' x (2 - length($all_spaces));
              $line->{'space'} = $all_spaces;
              delete $line->{'end_sentence'};
            } else {
              my $new_space = $spaces;
              $new_space =~ s/^[\n\r]/ /;
              $line->{'space'} = $new_space;
            }
          } else {
            my $new_space = substr($spaces, 0, 1);
            $new_space =~ s/^[\n\r]/ /;
            $line->{'space'} = $new_space;
          }
        }
      }
    } elsif ($text =~ s/^(([^\s\p{InFullwidth}]|[\x{202f}\x{00a0}])+)//) {
      my $added_word = $1;

      # Whether a sentence end is permitted in spite of a preceding
      # upper case letter.
      my $disinhibit = 0;

      # Reverse the insertion of the control character in Plaintext.pm.
      if ($added_word =~ s/\x08(?=[$end_sentence_character]
                                  [$after_punctuation_characters]*$)//x) {
        $disinhibit = 0;
      }
      $result .= _add_next($line, $added_word);

      # Check if it is considered as an end of sentence.  There are two things
      # to check: one, that we have a ., ! or ?; and second, that it is not
      # preceded by an upper-case letter (ignoring some punctuation)
      if (defined($line->{'end_sentence'})
          and $added_word =~ /^[$after_punctuation_characters]*$/) {
        # do nothing in the case of a continuation of 
        # after_punctuation_characters
      } elsif (($disinhibit
                or !$line->{'last_char'}
                or $line->{'last_char'} !~ /[[:upper:]]/)
              and $added_word =~ /[$end_sentence_character]
                                  [$after_punctuation_characters]*$/x) {
        if ($line->{'frenchspacing'}) {
          $line->{'end_sentence'} = -1;
        } else {
          $line->{'end_sentence'} = 1;
        }
        print STDERR "END_SENTENCE.L\n" if ($line->{'DEBUG'});
      } else {
        print STDERR "delete END_SENTENCE.L($line->{'end_sentence'}): text\n" 
          if (defined($line->{'end_sentence'}) and $line->{'DEBUG'});
        delete $line->{'end_sentence'};
      }
    } elsif ($text =~ s/^\n//) {
      $result .= $line->_end_line();
    } elsif ($text =~ s/^(\p{InFullwidth})//) {
      my $added = $1;
      print STDERR "EAST_ASIAN.L\n" if ($line->{'DEBUG'});
      if (!defined($line->{'word'})) {
        $line->{'word'} = '';
      }
      $line->{'word'} .= $added;
      $line->{'last_char'} = $added;
      $result .= $line->_add_pending_word();
      delete $line->{'end_sentence'};
      $line->{'space'} = '';
    } else {
      # Some characters are not handled by the cases above.
      # For example, it happened for strange caracters that seems to be
      # some special spaces.  It is a bit strange since the cases above 
      # include a possibility and the complement.  Maybe a character 
      # invalid in a given encoding?
      #die "Unknown caracter leading $text";
      last;
    }
  }
  return $result;
}

1;
