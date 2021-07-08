# ParagraphNonXS.pm: handle paragraph text.
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

# this module has nothing Texinfo specific.  In contrast with existing
# modules Text::Wrap, Text::Format, it keeps a state of the paragraph 
# and waits for text to be fed into it.

package Texinfo::Convert::Paragraph;

use 5.006;
use strict;

use Unicode::EastAsianWidth;
use Carp qw(cluck);

# initialize a paragraph object.
sub new($;$)
{
  my $class = shift;
  my $conf = shift;
  my $self = {'max' => 72, 'indent_length' => 0, 'counter' => 0, 
              'word_counter' => 0, 'space' => '', 'frenchspacing' => 0,
              'lines_counter' => 0, 'end_line_count' => 0,
              'unfilled' => 0 };
  if (defined($conf)) {
    foreach my $key (keys(%$conf)) {
      $self->{$key} = $conf->{$key};
    }
  }
  bless $self, $class;
}

# for debugging
sub dump($)
{
  my $self = shift;
  my $word = 'UNDEF';
  if (defined($self->{'word'})) {
    $word = $self->{'word'};
  }
  my $end_sentence = 'UNDEF';
  $end_sentence = $self->{'end_sentence'} if (defined($self->{'end_sentence'}));
  print STDERR "para ($self->{'counter'}+$self->{'word_counter'}) word: $word, space `$self->{'space'}' end_sentence: $end_sentence\n"; 
}

sub _cut_line($)
{
  my $paragraph = shift;
  return '' if ($paragraph->{'ignore_columns'});
  return _end_line($paragraph);
}

sub end_line_count($)
{
  my $paragraph = shift;
  return $paragraph->{'end_line_count'};
}

sub end_line($)
{
  my $paragraph = shift;
  $paragraph->{'end_line_count'} = 0;
  return _end_line($paragraph);
}

# end a line.
sub _end_line($)
{
  my $paragraph = shift;
  $paragraph->{'counter'} = 0;
  $paragraph->{'space'} = '';
  if (defined($paragraph->{'indent_length_next'})) {
    $paragraph->{'indent_length'} = $paragraph->{'indent_length_next'};
    delete $paragraph->{'indent_length_next'};        
  }
  $paragraph->{'lines_counter'}++;
  $paragraph->{'end_line_count'}++;
  print STDERR "END_LINE\n" if ($paragraph->{'DEBUG'});
  return "\n";
}

sub get_pending($)
{
  my $paragraph = shift;
  my $result = '';
  if ($paragraph->{'space'}) {
    $result .= $paragraph->{'space'};
  }
  if (defined($paragraph->{'word'})) {
    $result .= $paragraph->{'word'};
  }
  return $result;
}

sub add_pending_word($;$)
{
  my $paragraph = shift;
  my $add_spaces = shift;
  $paragraph->{'end_line_count'} = 0;
  return _add_pending_word($paragraph, $add_spaces);
}

# put a pending word and spaces in the result string.
sub _add_pending_word($;$)
{
  my $paragraph = shift;
  my $add_spaces = shift;
  my $result = '';

  if (defined($paragraph->{'word'}) or $add_spaces) {
    if ($paragraph->{'indent_length'} > $paragraph->{'counter'}) {
      $result .= ' ' x ($paragraph->{'indent_length'} - $paragraph->{'counter'});
      $paragraph->{'counter'} = $paragraph->{'indent_length'};
      print STDERR "INDENT($paragraph->{'counter'}+$paragraph->{'word_counter'})\n" 
                   if ($paragraph->{'DEBUG'});
      delete $paragraph->{'space'} unless $paragraph->{'unfilled'};
    }
    if ($paragraph->{'space'}) {
      $result .= $paragraph->{'space'};
      $paragraph->{'counter'} += length($paragraph->{'space'});
      print STDERR "ADD_SPACES($paragraph->{'counter'}+$paragraph->{'word_counter'})\n" 
         if ($paragraph->{'DEBUG'});
      
    }
    if (defined($paragraph->{'word'})) {
      $result .= $paragraph->{'word'};
      $paragraph->{'counter'} += $paragraph->{'word_counter'};
      print STDERR "ADD_WORD[$paragraph->{'word'}]+$paragraph->{'word_counter'} ($paragraph->{'counter'})\n"
        if ($paragraph->{'DEBUG'});
      $paragraph->{'word'} = undef;
      $paragraph->{'last_char'} = undef;
      $paragraph->{'word_counter'} = 0;
    }
    $paragraph->{'space'} = '';
  }
  return $result;
}

# end a paragraph
sub end($)
{
  my $paragraph = shift;
  $paragraph->{'end_line_count'} = 0;
  print STDERR "PARA END\n" if ($paragraph->{'DEBUG'});
  my $result = _add_pending_word($paragraph, $paragraph->{'add_final_space'});
  if (!$paragraph->{'no_final_newline'} and $paragraph->{'counter'} != 0) {
    $result .= "\n"; 
    $paragraph->{'lines_counter'}++;
    $paragraph->{'end_line_count'}++;
  }
  return $result;
}

my $end_sentence_character = quotemeta('.?!');
my $after_punctuation_characters = quotemeta('"\')]');

# Add $WORD to paragraph, returning the text to be added to the paragraph. 
# Any end of sentence punctuation in $WORD that should be allowed to end a 
# sentence but which would otherwise be preceded by an upper-case letter should 
# instead by preceded by a backspace character.
sub add_next($;$$)
{
  my $paragraph = shift;
  my $word = shift;
  my $transparent = shift;
  $paragraph->{'end_line_count'} = 0;
  return _add_next($paragraph, $word, $transparent);
}

# add a word (without wrapping).
sub _add_next($;$$$)
{
  my $paragraph = shift;
  my $word = shift;
  my $transparent = shift;
  my $newlines_impossible = shift;
  my $result = '';

  if (defined($word)) {
    my $disinhibit = 0;
    # Reverse the insertion of any control characters in Plaintext.pm.
    if ($word =~ /\x08$/) {
      $disinhibit = 1;
    }
    $word =~ s/\x08//g;

    $paragraph->{'word'} .= $word;

    if (!$transparent) {
      if ($disinhibit) {
        $paragraph->{'last_char'} = 'a';
      } elsif ($word =~
           /([^$end_sentence_character$after_punctuation_characters])
            [$end_sentence_character$after_punctuation_characters]*$/x) {
        # Save the last character in $word before punctuation
        $paragraph->{'last_char'} = $1;
      }
    }

    if (!$newlines_impossible and $word =~ /\n/) {
      $result .= _add_pending_word ($paragraph);
      _end_line($paragraph);
      $paragraph->{'word_counter'} = 0;
      $paragraph->{'word'} = undef;
      $paragraph->{'last_char'} = undef;
    } else {
      my $word2;
      $word2 = $word;
      $word2 =~ s/[\177]//g;
      $paragraph->{'word_counter'} += length($word2);
      # We don't count DEL bytes here for INFO_SPECIAL_CHARS_QUOTE.  We 
      # shouldn't count combining characters for accents either: see the
      # t/converters_tests.t (at_commands_in_refs_utf8) test.
    }
    if ($paragraph->{'DEBUG'}) {
      my $para_word = 'UNDEF';;
      if (defined($paragraph->{'word'})) {
        $para_word = $paragraph->{'word'};
      }
      print STDERR "WORD+ $word -> $para_word\n";
    }
    # The $paragraph->{'counter'} != 0 is here to avoid having an
    # additional line output when the text is longer than the max.
    if ($paragraph->{'counter'} != 0 and 
        $paragraph->{'counter'} + $paragraph->{'word_counter'} + 
           length($paragraph->{'space'}) > $paragraph->{'max'}) {
      $result .= _cut_line($paragraph);
    }
  }
  return $result;
}

sub remove_end_sentence($)
{
  my $paragraph = shift;
  $paragraph->{'end_sentence'} = 0;
}

sub add_end_sentence($;$) {
  my $paragraph = shift;
  my $value = shift;
  $paragraph->{'end_sentence'} = $value;
}

sub allow_end_sentence($)
{
  my $paragraph = shift;
  printf STDERR "ALLOW END SENTENCE\n" if $paragraph->{'DEBUG'};
  $paragraph->{'last_char'} = 'a'; # lower-case
}

sub set_space_protection($$;$$$$)
{
  my $paragraph = shift;
  my $space_protection = shift;
  my $ignore_columns = shift;
  my $keep_end_lines = shift;
  my $frenchspacing = shift;
  my $double_width_no_break = shift;
  $paragraph->{'protect_spaces'} = $space_protection 
    if defined($space_protection);
  $paragraph->{'ignore_columns'} = $ignore_columns
    if defined($ignore_columns);
  $paragraph->{'keep_end_lines'} = $keep_end_lines
    if defined($keep_end_lines);
  if (!$paragraph->{'frenchspacing'} and $frenchspacing
    and $paragraph->{'end_sentence'} and $paragraph->{'counter'} != 0 
    and $paragraph->{'space'} and !defined($paragraph->{'word'})) {
    $paragraph->{'space'} .= ' ' x (2 - length($paragraph->{'space'}));
    print STDERR "SWITCH frenchspacing end sentence space\n" 
       if ($paragraph->{'DEBUG'});
    delete $paragraph->{'end_sentence'};
  }
  $paragraph->{'frenchspacing'} = $frenchspacing
    if defined($frenchspacing);
  $paragraph->{'double_width_no_break'} = $double_width_no_break
    if defined($double_width_no_break);
  # begin a word, to have something even if empty
  if ($space_protection) {
    _add_next($paragraph, '');
  }
}

# Wrap $TEXT, returning the wrapped text, taking into account the current state 
# of $PARAGRAPH.  Any end of sentence punctuation in $TEXT that should be 
# allowed to end a sentence but which would otherwise be preceded by an 
# upper-case letter should instead by preceded by a backspace character.
sub add_text($$)
{
  my $paragraph = shift;
  my $text = shift;
  $paragraph->{'end_line_count'} = 0;
  my $result = '';

  my $protect_spaces_flag = $paragraph->{'protect_spaces'};

  my @segments = split
/([^\S\x{202f}\x{00a0}]+)|(\p{InFullwidth})|((?:[^\s\p{InFullwidth}]|[\x{202f}\x{00a0}])+)/,
    $text;

  # Check now if a newline exists anywhere in the string to
  # try to eliminate regex checks later.
  my $newline_possible_flag = ($text =~ /\n/);

  my $debug_flag = $paragraph->{'DEBUG'};
  while (@segments) {
    # $empty_segment should be an empty string; the other variables
    # here were recognized as field separators by splice.
    my ($empty_segment, $spaces, $fullwidth_segment, $added_word)
     = splice (@segments, 0, 4);

    if ($debug_flag) {
      my $word = 'UNDEF';
      $word = $paragraph->{'word'} if (defined($paragraph->{'word'}));
      print STDERR "p ($paragraph->{'counter'}+$paragraph->{'word_counter'}) s `"._print_escaped_spaces($paragraph->{'space'})."', w `$word'\n";
      #print STDERR "TEXT: "._print_escaped_spaces($text)."|\n"
    }
    # \x{202f}\x{00a0} are non breaking spaces
    if (defined $spaces) {
      print STDERR "SPACES($paragraph->{'counter'}) `"._print_escaped_spaces($spaces)."'\n" if $debug_flag;
      if ($protect_spaces_flag) {
        $paragraph->{'word'} .= $spaces;
        $paragraph->{'last_char'} = substr($spaces, -1);
        $paragraph->{'word_counter'} += length($spaces);
        $paragraph->{'word'} =~ s/\n/ /g;

        # The $paragraph->{'counter'} != 0 is here to avoid having an
        # additional line output when the text is longer than the max.
        if ($paragraph->{'counter'} != 0 and 
            $paragraph->{'counter'} + $paragraph->{'word_counter'} + 
               length($paragraph->{'space'}) > $paragraph->{'max'}) {
          $result .= _cut_line($paragraph);
        }
      } else {
        my $pending_word = $paragraph->{'word'};

        $result .= _add_pending_word($paragraph);
        if ($paragraph->{'counter'} != 0 or $paragraph->{'unfilled'}
            or (defined $pending_word)) {
          if ($paragraph->{'end_sentence'} 
              and $paragraph->{'end_sentence'} > 0
              and !$paragraph->{'frenchspacing'}
              and !$paragraph->{'unfilled'}) {
            $paragraph->{'space'} = '  ';
          } else {
            # Only save the first space
            if ($paragraph->{'unfilled'}
                or length($paragraph->{'space'}) < 1) {
              if ($spaces =~ /\n/) {
                if (!$paragraph->{'unfilled'}) {
                  $paragraph->{'space'} = ' ';
                } elsif ($spaces =~ /\n/) {
                  $result .= _add_pending_word ($paragraph);
                  $result .= _end_line ($paragraph);
                }
              } else {
                if (!$paragraph->{'unfilled'}) {
                  $spaces =~ s/\r/ /g;
                  $paragraph->{'space'} .= substr ($spaces, 0, 1);
                } else {
                  $paragraph->{'space'} .= $spaces;
                }
              }
            }
          }
        }
      }
      #print STDERR "delete END_SENTENCE($paragraph->{'end_sentence'}): spaces\n" 
      #  if (defined($paragraph->{'end_sentence'}) and $paragraph->{'DEBUG'});
      #delete $paragraph->{'end_sentence'};
      if ($paragraph->{'counter'} + length($paragraph->{'space'}) 
                      > $paragraph->{'max'}) {
        $result .= _cut_line($paragraph);
      }
      if ($newline_possible_flag and !$paragraph->{'unfilled'}
          and $paragraph->{'keep_end_lines'} and $spaces =~ /\n/) {
        $result .= _end_line($paragraph);
      }
    } elsif (defined $added_word) {
      my $tmp = $added_word;
      if (defined $paragraph->{'last_char'}) {
        # Use 'last_char' here because _add_next overwrites it.
        $tmp = $paragraph->{'last_char'} . $tmp;
      }

      $result .= _add_next($paragraph, $added_word, undef,
                           !$newline_possible_flag);

      # Check if it is considered as an end of sentence.  There are two things
      # to check: one, that we have a ., ! or ?; and second, that it is not
      # preceded by an upper-case letter (ignoring some punctuation)
      if (defined($paragraph->{'end_sentence'})
          and $added_word =~ /^[$after_punctuation_characters]*$/o) {
        # do nothing in the case of a continuation of after_punctuation_characters
      } elsif (!$paragraph->{'unfilled'}
          and $tmp =~
        /(^|[^[:upper:]$after_punctuation_characters$end_sentence_character])
         [$after_punctuation_characters]*[$end_sentence_character]
         [$end_sentence_character\x08$after_punctuation_characters]*$/x) {
        if ($paragraph->{'frenchspacing'}) {
          $paragraph->{'end_sentence'} = -1;
        } else {
          $paragraph->{'end_sentence'} = 1;
        }
        print STDERR "END_SENTENCE\n" if ($paragraph->{'DEBUG'});
      } else {
        delete $paragraph->{'end_sentence'};
        print STDERR "delete END_SENTENCE($paragraph->{'end_sentence'}): text\n" 
          if (defined($paragraph->{'end_sentence'}) and $paragraph->{'DEBUG'});
      }
    } elsif (defined $fullwidth_segment) {
      print STDERR "EAST_ASIAN\n" if ($paragraph->{'DEBUG'});
      if (!defined($paragraph->{'word'})) {
        $paragraph->{'word'} = '';
      }
      $paragraph->{'word'} .= $fullwidth_segment;
      $paragraph->{'last_char'} = $fullwidth_segment;
      $paragraph->{'word_counter'} += 2;
      if ($paragraph->{'counter'} != 0 and
          $paragraph->{'counter'} + $paragraph->{'word_counter'} 
                               > $paragraph->{'max'}) {
        $result .= _cut_line($paragraph);
      }
      if (!$paragraph->{'protect_spaces'}
          and !$paragraph->{'double_width_no_break'}) {
        $result .= _add_pending_word($paragraph);
        $paragraph->{'space'} = '';
      }
      delete $paragraph->{'end_sentence'};
    }
  }
  return $result;
}

# for debug
sub _print_escaped_spaces($)
{
  my $spaces = shift;
  my $result = '';
  foreach my $pos (0 .. length($spaces)-1) {
    my $char = substr($spaces, $pos, 1);
    if ($char eq ' ') {
      $result .= $char;
    } elsif ($char =~ /[\f\n]/) {
      $char =~ s/\f/\\f/;
      $char =~ s/\n/\\n/;
      $result .= $char;
    } elsif ($char =~ /\s/) {
      if (ord($char) <= hex(0xFFFF)) {
        $result .= '\x'.sprintf("%04x",ord($char));
      } else {
        $result .= '\x'.sprintf("%06x",ord($char));
      }
    } else {
      $result .= $char;
    }
  }
  return $result;
}

1;
