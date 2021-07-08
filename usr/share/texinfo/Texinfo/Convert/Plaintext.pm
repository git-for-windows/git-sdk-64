# Plaintext.pm: output tree as text with filling.
#
# Copyright 2010-2020 Free Software Foundation, Inc.
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

package Texinfo::Convert::Plaintext;

use 5.00405;

# See comment at start of HTML.pm
use if $] >= 5.012, feature => qw(unicode_strings);

use strict;

use Texinfo::Convert::Converter;
use Texinfo::Common;
use Texinfo::Convert::Texinfo;
use Texinfo::Convert::Paragraph;

use Texinfo::Convert::Text;

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Texinfo::Convert::Converter);

# Some extra initialization for the first time this module is loaded.
# This could be done in a UNITCHECK block, introduced in Perl 5.10.
our $module_loaded = 0;
sub import {
  if (!$module_loaded) {
    Texinfo::XSLoader::override(
      "Texinfo::Convert::Plaintext::_process_text_internal",
      "Texinfo::MiscXS::process_text");
    $module_loaded = 1;
  }
  # The usual import method
  goto &Exporter::import;
}

%EXPORT_TAGS = ( 'all' => [ qw(
  convert
  output
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '6.8';

# misc commands that are of use for formatting.
my %formatting_misc_commands = %Texinfo::Convert::Text::formatting_misc_commands;

my $NO_NUMBER_FOOTNOTE_SYMBOL = '*';

my @informative_global_commands = ('paragraphindent', 'firstparagraphindent',
'frenchspacing', 'documentencoding', 'footnotestyle', 'documentlanguage',
'contents', 'shortcontents', 'summarycontents', 'deftypefnnewline');

my %informative_commands;
foreach my $informative_command (@informative_global_commands) {
  $informative_commands{$informative_command} = 1;
}

my %no_brace_commands = %Texinfo::Common::no_brace_commands;
my %brace_no_arg_commands;
foreach my $command (keys (%Texinfo::Common::brace_commands)) {
  $brace_no_arg_commands{$command} = 1 
    if ($Texinfo::Common::brace_commands{$command} eq '0');
}
my %accent_commands = %Texinfo::Common::accent_commands;
my %misc_commands = %Texinfo::Common::misc_commands;
my %sectioning_commands = %Texinfo::Common::sectioning_commands;
my %def_commands = %Texinfo::Common::def_commands;
my %ref_commands = %Texinfo::Common::ref_commands;
my %block_commands = %Texinfo::Common::block_commands;
my %menu_commands = %Texinfo::Common::menu_commands;
my %root_commands = %Texinfo::Common::root_commands;
my %preformatted_commands = %Texinfo::Common::preformatted_commands;
my %math_commands = %Texinfo::Common::math_commands;
my %explained_commands = %Texinfo::Common::explained_commands;
my %inline_format_commands = %Texinfo::Common::inline_format_commands;
my %inline_commands = %Texinfo::Common::inline_commands;
my %item_container_commands = %Texinfo::Common::item_container_commands;
my %raw_commands = %Texinfo::Common::raw_commands;
my %format_raw_commands = %Texinfo::Common::format_raw_commands;
my %code_style_commands       = %Texinfo::Common::code_style_commands;
my %regular_font_style_commands = %Texinfo::Common::regular_font_style_commands;
my %preformatted_code_commands = %Texinfo::Common::preformatted_code_commands;
my %default_index_commands = %Texinfo::Common::default_index_commands;
my %letter_no_arg_commands = %Texinfo::Common::letter_no_arg_commands;

foreach my $kept_command(keys (%informative_commands),
  keys (%default_index_commands),
  'verbatiminclude', 'insertcopying', 
  'listoffloats', 'printindex', ) {
  $formatting_misc_commands{$kept_command} = 1;
}

foreach my $def_command (keys(%def_commands)) {
  $formatting_misc_commands{$def_command} = 1 if ($misc_commands{$def_command});
}

# There are 6 stacks that define the context.
# context:   relevant for alignement of text.  Set in math, footnote, 
#            listoffloats, flush_commands, preformatted_context_commands 
#            (preformatted + menu + verbatim), and raw commands if 
#            on top level.
# format_context: used for the count of paragraphs and for the indentation.
#            Set in footnote, for all commands relevant for indenting, like
#            @*table, @itemize, @enumerate, preformatted commands, 
#            @*quotation, @def*, and also menu commands, @flushleft, 
#            @flushright, @float, in multitable cell and raw commands if at
#            toplevel.
# text_element_context: for the max columns and the counter in the line
#            position (although the counter in the line is taken over by 
#            the formatter once a formatter is opened).
#            Set in footnote and in multitable cells.
# formatters: a formatter environment has stacks for formatting context.
#            Also holds a container, an objects that does the counting 
#            of columns, actual indentation.  In general, it is better not 
#            to have formatters in parallel, but it may happen.
# count_context: holds the bytes count, the lines count and the location
#            of the commands that have their byte count or lines count
#            recorded.  It is set for out of document formatting to avoid
#            counting some converted text, but it is also set when it has
#            to be modified afterwards, for aligned commands or multitable
#            cells for example.
# document_context: Used to keep track if we are in a multitable.

# formatters have their own stack
# in container
# 'upper_case'
# 'var'
# 'font_type_stack'
# 
# paragraph number incremented with paragraphs, center, listoffloats
# and block commands except: html and such, group, raggedright, menu*, float

my %default_preformatted_context_commands = (%preformatted_commands,
                                             %format_raw_commands);
foreach my $preformatted_command ('verbatim', keys(%menu_commands)) {
  $default_preformatted_context_commands{$preformatted_command} = 1;
}

my %block_math_commands;
foreach my $block_math_command (keys(%math_commands)) {
  if (exists($block_commands{$block_math_command})) {
    $block_math_commands{$block_math_command} = 1;
  }
}

my %ignored_misc_commands;
foreach my $misc_command (keys(%misc_commands)) {
  $ignored_misc_commands{$misc_command} = 1 
    unless ($formatting_misc_commands{$misc_command});
}

my %ignored_commands = %ignored_misc_commands;
foreach my $ignored_brace_commands ('caption', 'shortcaption', 
  'hyphenation', 'sortas') {
  $ignored_commands{$ignored_brace_commands} = 1;
}

my %item_indent_format_length = ('enumerate' => 2,
    'itemize' => 3,
    'table' => 0,
    'vtable' => 0,
    'ftable' => 0,
 );

my $indent_length = 5;

my %indented_commands;
foreach my $indented_command (keys(%item_indent_format_length), 
           keys(%preformatted_commands), 'quotation', 'smallquotation', 
           'indentedblock', 'smallindentedblock',
           keys(%def_commands)) {
  $indented_commands{$indented_command} = 1 
    if exists($block_commands{$indented_command});
}

my %default_format_context_commands = %indented_commands;

foreach my $non_indented('format', 'smallformat') {
  delete $indented_commands{$non_indented};
}

# FIXME should keys(%math_brace_commands) be added here?
# How can this be tested?
foreach my $format_context_command (keys(%menu_commands), 'verbatim',
 'flushleft', 'flushright', 'multitable', 'float') {
  $default_format_context_commands{$format_context_command} = 1;
}

my %flush_commands = (
  'flushleft'  => 1,
  'flushright' => 1
);

# commands that leads to advancing the paragraph number.  This is mostly
# used to determine the first line, in fact.
my %advance_paragraph_count_commands;
foreach my $command (keys(%block_commands)) {
  next if ($menu_commands{$command} 
            or $block_commands{$command} eq 'raw');
  $advance_paragraph_count_commands{$command} = 1;
}

# group and raggedright do more than not advancing para, they should also
# be transparent with respect to paragraph number counting.
foreach my $not_advancing_para ('group', 'raggedright',
  'titlepage', 'copying', 'documentdescription', 'float') {
  delete $advance_paragraph_count_commands{$not_advancing_para};
}

foreach my $advancing_para('center', 'verbatim', 'listoffloats') {
  $advance_paragraph_count_commands{$advancing_para} = 1;
}

foreach my $ignored_block_commands ('ignore', 'macro', 'rmacro', 'copying',
  'documentdescription', 'titlepage', 'direntry') {
  $ignored_commands{$ignored_block_commands} = 1;
}

my %punctuation_no_arg_commands;
foreach my $punctuation_command('enddots', 'exclamdown', 'questiondown') {
  $punctuation_no_arg_commands{$punctuation_command} = 1;
}

my %upper_case_commands = (
 'sc' => 1,
 'var' => 1
);

my %ignorable_space_types;
foreach my $type ('empty_line_after_command',
            'empty_spaces_after_command', 'spaces_at_end',
            'empty_spaces_before_argument', 'empty_spaces_before_paragraph',
            'empty_spaces_after_close_brace') {
  $ignorable_space_types{$type} = 1;
}

my %ignored_types;
foreach my $type ('preamble',
            'preamble_before_setfilename') {
  $ignored_types{$type} = 1;
}

my %ignorable_types = %ignorable_space_types;
foreach my $ignored_type(keys(%ignored_types)) {
  $ignorable_types{$ignored_type} = 1;
}

# All those commands run with the text.
my %style_map = (
  'strong' => '*',
  'dfn'    => '"',
  'emph'   => '_',
);

foreach my $command (keys(%style_map)) {
  $style_map{$command} = [$style_map{$command}, $style_map{$command}];
}

# math is special
my @asis_commands = ('asis', 'w', 'b', 'i', 'sc', 't', 'r',
  'slanted', 'sansserif', 'var', 'verb', 'clicksequence',
  'headitemfont', 'dmn');

foreach my $asis_command (@asis_commands) {
  $style_map{$asis_command} = ['', ''];
}

my @quoted_commands = ('cite', 'code', 'command', 'env', 'file', 'kbd',
  'option', 'samp', 'indicateurl');

# %non_quoted_commands_when_nested have no quote when in code command contexts
my %non_quoted_commands_when_nested;

# Quotes are reset in converter_initialize and unicode quotes are used 
# if @documentencoding utf-8 is used.
foreach my $quoted_command (@quoted_commands) {
  $style_map{$quoted_command} = ["'", "'"];
  if ($code_style_commands{$quoted_command}) {
    $non_quoted_commands_when_nested{$quoted_command} = 1;
  }
}
# always quoted even when nested
delete $non_quoted_commands_when_nested{'samp'};
delete $non_quoted_commands_when_nested{'indicateurl'};

$style_map{'key'} = ['<', '>'];
$style_map{'sub'} = ['_{', '}'];
$style_map{'sup'} = ['^{', '}'];

# Commands producing styles that are output in node names and index entries.
my %index_style_commands;
for my $index_style_command ('strong', 'emph', 'sub', 'sup', 'key') {
  $index_style_commands{$index_style_command} = 1;
}


# in those commands, there is no addition of double space after a dot.
# math is special
my %no_punctation_munging_commands;
foreach my $command ('var', 'cite', 'dmn', keys(%code_style_commands)) {
  $no_punctation_munging_commands{$command} = 1;
}

my %defaults = (
  'ENABLE_ENCODING'      => 1,
  'FORMAT_MENU'          => 'nomenu',
  #'EXTENSION'            => 'info',
  'EXTENSION'            => 'txt',
  #'USE_SETFILENAME_EXTENSION' => 1,
  'INFO_SPECIAL_CHARS_WARNING' => 1,

  #'OUTFILE'              => undef,
  'OUTFILE'              => '-',
  'SUBDIR'               => undef,
  'documentlanguage'     => undef,

  'output_format'        => '',
  'USE_NODES'            => 1,
);

sub push_top_formatter($$)
{
  my ($self, $top_context) = @_;

  push @{$self->{'context'}}, $top_context;
  push @{$self->{'format_context'}}, {
                                     'cmdname' => '_top_format',
                                     'indent_level' => 0,
                                     'paragraph_count' => 0,
                                   };
  push @{$self->{'text_element_context'}}, {
                                     'max' => $self->{'fillcolumn'}
                                   };
  push @{$self->{'document_context'}}, {
                                     'in_multitable' => 0
                                   };

  # This is not really meant to be used, as contents should open 
  # their own formatters, however it happens that there is some text
  # outside any content that needs to be formatted, as @sp for example.
  push @{$self->{'formatters'}}, $self->new_formatter('line');
  $self->{'formatters'}->[-1]->{'_top_formatter'} = 1;
}


my %contents_commands = (
 'contents' => 1,
 'shortcontents' => 1,
 'summarycontents' => 1,
);

sub converter_defaults($$)
{
  return %defaults;
}

sub converter_global_commands($)
{
  return @informative_global_commands;
}

sub converter_initialize($)
{
  my $self = shift;

  $self->{'context'} = [];
  $self->{'format_context'} = [];
  $self->{'empty_lines_count'} = undef;
  push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0,
                                     'locations' => []};

  %{$self->{'ignored_types'}} = %ignored_types;
  %{$self->{'ignorable_space_types'}} = %ignorable_space_types;
  %{$self->{'ignored_commands'}} = %ignored_commands;
  # this is dynamic because raw formats may either be full commands if
  # isolated, or simple text if in a paragraph
  %{$self->{'format_context_commands'}} = %default_format_context_commands;
  %{$self->{'preformatted_context_commands'}} 
     = %default_preformatted_context_commands;
  $self->{'footnote_index'} = 0;
  $self->{'pending_footnotes'} = [];

  foreach my $format (keys(%format_raw_commands)) {
    $self->{'ignored_commands'}->{$format} = 1 
       unless ($self->{'expanded_formats_hash'}->{$format});
  }

  %{$self->{'style_map'}} = %style_map;
  $self->{'convert_text_options'} 
      = {Texinfo::Common::_convert_text_options($self)};

  if ($self->get_conf('ENABLE_ENCODING') and $self->get_conf('OUTPUT_ENCODING_NAME')
      and $self->get_conf('OUTPUT_ENCODING_NAME') eq 'utf-8') {
    # cache this to avoid redoing calls to get_conf
    $self->{'to_utf8'} = 1;
    if (!$self->{'extra'}->{'documentencoding'}) {
      # Do not use curly quotes or some other unnecessary non-ASCII characters
      # if '@documentencoding UTF-8' is not given.
      $self->{'convert_text_options'}->{'no_extra_unicode'} = 1;
    } else {
      foreach my $quoted_command (@quoted_commands) {
        # Directed single quotes
        $self->{'style_map'}->{$quoted_command} = ["\x{2018}", "\x{2019}"];
      }
      # Directed double quotes
      $self->{'style_map'}->{'dfn'} = ["\x{201C}", "\x{201D}"];
    }
  }
  if (defined($self->get_conf('OPEN_QUOTE_SYMBOL'))) {
    foreach my $quoted_command (@quoted_commands) {
      $self->{'style_map'}->{$quoted_command}->[0] 
       = $self->get_conf('OPEN_QUOTE_SYMBOL');
    }
  }
  if (defined($self->get_conf('CLOSE_QUOTE_SYMBOL'))) {
    foreach my $quoted_command (@quoted_commands) {
      $self->{'style_map'}->{$quoted_command}->[1] 
       = $self->get_conf('CLOSE_QUOTE_SYMBOL');
    }
  }
  if ($self->get_conf('FILLCOLUMN')) {
    $self->{'fillcolumn'} = $self->get_conf('FILLCOLUMN');
    # else it's already set via the defaults
  }
  # This needs to be here to take into account $self->{'fillcolumn'}.
  $self->push_top_formatter('_Root_context');
  # some caching to avoid calling get_conf
  if ($self->get_conf('OUTPUT_PERL_ENCODING')) {
    $self->{'output_perl_encoding'} = $self->get_conf('OUTPUT_PERL_ENCODING');
  } else {
    $self->{'output_perl_encoding'} = '';
  }
  $self->{'enable_encoding'} = $self->get_conf('ENABLE_ENCODING');
  $self->{'output_encoding_name'} = $self->get_conf('OUTPUT_ENCODING_NAME');
  $self->{'debug'} = $self->get_conf('DEBUG');

  return $self;
}

sub _count_context_bug_message($$$)
{
  my ($self, $precision, $element) = @_;

  if (scalar(@{$self->{'count_context'}}) != 1) {
    my $element_text;
    if ($element) {
      $element_text 
         = Texinfo::Structuring::_print_element_command_texi($element);
    } else {
      $element_text = '';
    }
    $self->_bug_message("Too much count_context ${precision}(".
      scalar(@{$self->{'count_context'}}). "): ". $element_text, $element);
    die;
  }
}

sub _convert_element($$)
{
  my ($self, $element) = @_;

  my $result = '';
  $result .= $self->_convert($element);
  $self->_count_context_bug_message('', $element);
  $result .= $self->_footnotes($element);
  $self->_count_context_bug_message('footnotes ', $element);

  return $result;
}

sub convert($$)
{
  my ($self, $root) = @_;

  my $result = '';

  my $elements = Texinfo::Structuring::split_by_node($root);
  $self->{'empty_lines_count'} = 1;
  if (!defined($elements)) {
    $result = $self->_convert($root);
    $self->_count_context_bug_message('no element ');
    my $footnotes = $self->_footnotes();
    $self->_count_context_bug_message('no element footnotes ');
    $result .= $footnotes;
  } else {
    foreach my $node (@$elements) {
      my $node_text = _convert_element($self, $node);
      $result .= $node_text;
    }
  }

  return $result;
}

sub convert_tree($$)
{
  my ($self, $root) = @_;

  $self->{'empty_lines_count'} = 1;
  my $result;
  if ($root->{'type'} and $root->{'type'} eq 'element') {
    $result = _convert_element($self, $root);
  } else {
    $result = $self->_convert($root);
  }
  return $result;
}

my $end_sentence = quotemeta('.?!');
my $after_punctuation = quotemeta('"\')]');

sub _protect_sentence_ends ($) {
  my $text = shift;
  # Avoid suppressing end of sentence, by inserting a control character
  # in front of the full stop.  The choice of BS for this is arbitrary.
  $text =~ s/(?<=[^[:upper:]])
             (?=[$end_sentence][$after_punctuation]*(?:\s|$))
             /\x08/gx;

  # Also insert a control character at end of string, to protect a full stop 
  # that may follow later.

  #$text =~ s/(?<=[^[:upper:]][$after_punctuation]*)$/\x08/;
  # Perl doesn't support "variable length lookbehind"

  $text = reverse $text;
  $text =~ s/^(?=[$after_punctuation]*
                 (?:[^[:upper:]\s]|[\x{202f}\x{00a0}]))
            /\x08/x;
  $text = reverse $text;

  return $text;
}

sub _process_text_internal {
  my ($text) = @_;

  $text =~ s/---/\x{1F}/g;
  $text =~ s/--/-/g;
  $text =~ s/\x{1F}/--/g;
  $text =~ s/``/"/g;
  $text =~ s/\'\'/"/g;
  $text =~ s/`/'/g;

  return $text;
}

# Convert ``, '', `, ', ---, -- in $COMMAND->{'text'} to their output,
# possibly coverting to upper case as well.
sub _process_text($$$)
{
  my ($self, $command, $context) = @_;

  my $text = $command->{'text'};

  if ($context->{'upper_case'}
      or $self->{'formatters'}[-1]->{'var'}) {
    $text = _protect_sentence_ends($text);
    $text = uc($text);
  }

  if ($self->{'to_utf8'}
      and $self->{'extra'}->{'documentencoding'}) {
    return Texinfo::Convert::Unicode::unicode_text($text, 
            $context->{'font_type_stack'}->[-1]->{'monospace'});
  } elsif (!$context->{'font_type_stack'}->[-1]->{'monospace'}) {
    return _process_text_internal($text);
  }
  return $text;
}

sub new_formatter($$;$)
{
  my ($self, $type, $conf) = @_;

  my $first_indent_length = $conf->{'first_indent_length'};
  delete $conf->{'first_indent_length'};
  
  my $container;
  my $container_conf = {
         'max'               => $self->{'text_element_context'}->[-1]->{'max'},
         'indent_level'      => $self->{'format_context'}->[-1]->{'indent_level'}, 
  };
  $container_conf->{'frenchspacing'} = 1 
    if ($self->{'conf'}->{'frenchspacing'} eq 'on');
  $container_conf->{'counter'} 
    = $self->{'text_element_context'}->[-1]->{'counter'}
      if (defined($self->{'text_element_context'}->[-1]->{'counter'}));
  $container_conf->{'DEBUG'} = 1 if ($self->{'debug'});
  if ($conf) {
    foreach my $key (keys(%$conf)) {
      $container_conf->{$key} = $conf->{$key};
    }
  }
  my $indent = $container_conf->{'indent_length'};
  $indent = $indent_length*$container_conf->{'indent_level'}
    if (!defined($indent));

  if ($first_indent_length) {
    $container_conf->{'indent_length'} = $first_indent_length;
    $container_conf->{'indent_length_next'} = $indent;
  } else {
    $container_conf->{'indent_length'} = $indent;
  }
    
  if ($type eq 'line') {
    $container_conf->{'max'} = 10000001;
    $container_conf->{'keep_end_lines'} = 1;
    $container_conf->{'no_final_newline'} = 1;
    $container_conf->{'add_final_space'} = 1;

    $container = Texinfo::Convert::Paragraph->new($container_conf);
  } elsif ($type eq 'paragraph') {
    $container = Texinfo::Convert::Paragraph->new($container_conf);
  } elsif ($type eq 'unfilled') {
    $container_conf->{'max'} = 10000000;
    $container_conf->{'ignore_columns'} = 1;
    $container_conf->{'keep_end_lines'} = 1;
    $container_conf->{'frenchspacing'} = 1;
    $container_conf->{'unfilled'} = 1;
    $container_conf->{'no_final_newline'} = 1;

    $container = Texinfo::Convert::Paragraph->new($container_conf);
  } else {
    die "Unknown container type $type\n";
  }

  if ($flush_commands{$self->{'context'}->[-1]}) {
    set_space_protection($container, undef, 1, 1);
  }

  my $formatter = {'container' => $container, 'upper_case' => 0,
                   'font_type_stack' => [{}],
                   'w' => 0, 'type' => $type,
              'frenchspacing_stack' => [$self->{'conf'}->{'frenchspacing'}],
              'suppress_styles' => $conf->{'suppress_styles'}};

  if ($type eq 'unfilled') {
    foreach my $context (reverse(@{$self->{'context'}})) {
      if ($menu_commands{$context}) {
        last;
      } elsif ($preformatted_code_commands{$context}
               or $format_raw_commands{$context}
               or $math_commands{$context}) {
        $formatter->{'font_type_stack'}->[-1]->{'monospace'} = 1;
        $formatter->{'font_type_stack'}->[-1]->{'code_command'} = 1 
          if ($preformatted_code_commands{$context}
              or $math_commands{$context});
        last;
      }
    }
  }
  return $formatter;
}

sub convert_line($$;$)
{
  my ($self, $converted, $conf) = @_;
  my $formatter = $self->new_formatter('line', $conf);
  push @{$self->{'formatters'}}, $formatter;
  my $text = $self->_convert($converted);
  $text .= _count_added($self, $formatter->{'container'},
                Texinfo::Convert::Paragraph::end($formatter->{'container'}));
  pop @{$self->{'formatters'}};
  return $text;
}

sub convert_unfilled($$;$)
{
  my ($self, $converted, $conf) = @_;
  my $formatter = $self->new_formatter('unfilled', $conf);
  $formatter->{'font_type_stack'}->[-1]->{'monospace'} = 1;
  push @{$self->{'formatters'}}, $formatter;
  my $result = $self->_convert($converted);
  $result .= _count_added($self, $formatter->{'container'},
                Texinfo::Convert::Paragraph::end($formatter->{'container'}));
  pop @{$self->{'formatters'}};
  return $result;
}

sub count_bytes($$) 
{
  my ($self, $string) = @_;

  return Texinfo::Common::count_bytes($self, $string, 
                                      $self->{'output_perl_encoding'});
}

sub _add_text_count($$)
{
  my ($self, $text) = @_;
  if (!$self->{'count_context'}->[-1]->{'pending_text'}) {
    $self->{'count_context'}->[-1]->{'pending_text'} = '';
  }
  $self->{'count_context'}->[-1]->{'pending_text'} .= $text;
}

sub _add_lines_count($$)
{
  my ($self, $lines_count) = @_;
  $self->{'count_context'}->[-1]->{'lines'} += $lines_count;
}

# Update $SELF->{'count_context'}->[-1]->{'bytes'} by counting the text that
# hasn't been counted yet.  It is faster to count the text all together than
# piece by piece in _add_text_count.
sub _update_count_context($)
{
  my $self = shift;
  if ($self->{'count_context'}->[-1]->{'pending_text'}) {
    $self->{'count_context'}->[-1]->{'bytes'} +=
      Texinfo::Common::count_bytes($self,
        $self->{'count_context'}->[-1]->{'pending_text'},
        $self->{'output_perl_encoding'});
    $self->{'count_context'}->[-1]->{'pending_text'} = '';
  }
}

# Save the line and byte offset of $ROOT.
sub _add_location($$)
{
  my ($self, $root) = @_;
  my $location = { 'lines' => $self->{'count_context'}->[-1]->{'lines'} };
  push @{$self->{'count_context'}->[-1]->{'locations'}}, $location;
  if (!($root->{'extra'} and $root->{'extra'}->{'index_entry'})) {
    _update_count_context($self);
    $location->{'bytes'} = $self->{'count_context'}->[-1]->{'bytes'};
    $location->{'root'} = $root;
  } else {
    $location->{'index_entry'} = $root;
  }
  return $location;
}

sub _add_image($$$$;$)
{
  my ($self, $root, $lines_count, $image_width, $no_align) = @_;

  push @{$self->{'count_context'}->[-1]->{'images'}}, {
    'lines' => $self->{'count_context'}->[-1]->{'lines'},
    'lines_count' => $lines_count,
    'image_width'  => $image_width,
    'no_align' =>  $no_align,
    # may be used for debugging?
    #'_ref' => $root,
  };
}

sub _count_added($$$)
{
  my ($self, $container, $text) = @_;

  my $count_context = $self->{'count_context'}->[-1];
  $count_context->{'lines'}
    += Texinfo::Convert::Paragraph::end_line_count($container);

  if (!defined $count_context->{'pending_text'}) {
    $count_context->{'pending_text'} = '';
  }
  $count_context->{'pending_text'} .= $text;
  return $text;
}

sub _update_locations_counts($$)
{
  my ($self, $locations) = @_;

  _update_count_context($self);
  foreach my $location (@$locations) {
    $location->{'bytes'} += $self->{'count_context'}->[-1]->{'bytes'}
       if (defined($location->{'bytes'}));
    $location->{'lines'} += $self->{'count_context'}->[-1]->{'lines'}
      if (defined($location->{'lines'}));
  }
}

sub _add_newline_if_needed($) {
  my $self = shift;
  if (defined($self->{'empty_lines_count'}) 
       and $self->{'empty_lines_count'} == 0) {
    _add_text_count($self, "\n");
    _add_lines_count($self, 1);
    $self->{'empty_lines_count'} = 1;
    return "\n";
  }
  return '';
}

my $footnote_indent = 3;
sub _footnotes($;$)
{
  my ($self, $element) = @_;

  $element = undef if ($element and $element->{'extra'}->{'no_node'});

  my $result = '';
  if (scalar(@{$self->{'pending_footnotes'}})) {
    $result .= _add_newline_if_needed($self);
    if ($self->get_conf('footnotestyle') eq 'end' or !defined($element)) {
      my $footnotes_header = "   ---------- Footnotes ----------\n\n";
      $result .= $footnotes_header;
      _add_text_count($self, $footnotes_header);
      _add_lines_count($self, 2);
      $self->{'empty_lines_count'} = 1;
    } else {

      my $node_contents = [@{$element->{'extra'}->{'node'}->{'extra'}->{'node_content'}},
                                     {'text' => '-Footnotes'}];
      my $footnotes_node = {
        'cmdname' => 'node',
        'node_up' => $element->{'extra'}->{'node'},
        'extra' => {'node_content' => $node_contents }
      };
      $result .= $self->_node($footnotes_node);
      $self->{'node'} = $footnotes_node;
    }
    while (@{$self->{'pending_footnotes'}}) {
      my $footnote = shift (@{$self->{'pending_footnotes'}});

      # If nested within another footnote and footnotestyle is separate,
      # the element here will be the parent element and not the footnote
      # element, while the pxref will point to the name with the 
      # footnote node taken into account.  Not really problematic as
      # nested footnotes are not right.
      if ($element) {
        my $node_contents = [@{$element->{'extra'}->{'node'}->{'extra'}->{'node_content'}},
                    {'text' => "-Footnote-$footnote->{'number'}"}];
        $self->_add_location({'cmdname' => 'anchor',
                        'extra' => {'node_content' => $node_contents }
                       });
      }
      # this pushes on 'context', 'formatters', 'format_context',
      # 'text_element_context' and 'document_context'
      $self->push_top_formatter('footnote');
      my $formatted_footnote_number;
      if ($self->get_conf('NUMBER_FOOTNOTES')) {
        $formatted_footnote_number = $footnote->{'number'};
      } else {
        $formatted_footnote_number = $NO_NUMBER_FOOTNOTE_SYMBOL;
      }
      my $footnote_text = ' ' x $footnote_indent 
               . "($formatted_footnote_number) ";
      $result .= $footnote_text;
      $self->{'text_element_context'}->[-1]->{'counter'} += 
         Texinfo::Convert::Unicode::string_width($footnote_text);
      _add_text_count($self, $footnote_text);
      $self->{'empty_lines_count'} = 0;

      $result .= $self->_convert($footnote->{'root'}->{'args'}->[0]); 
      $result .= _add_newline_if_needed($self);
      
      my $old_context = pop @{$self->{'context'}};
      die if ($old_context ne 'footnote');
      pop @{$self->{'formatters'}};
      pop @{$self->{'format_context'}};
      pop @{$self->{'text_element_context'}};
      pop @{$self->{'document_context'}};
    }
  }
  $self->{'footnote_index'} = 0;

  return $result;
}

sub _compute_spaces_align_line($$$;$)
{
  my ($line_width, $max_column, $direction, $no_align) = @_;

  my $spaces_prepended;
  if ($line_width >= $max_column or $no_align) {
    $spaces_prepended = 0;
  } elsif ($direction eq 'center') {
    # if no int we may end up with floats...
    $spaces_prepended = int(($max_column -1 - $line_width) /2);
  } else {
    $spaces_prepended = ($max_column -1 - $line_width);
  }
  return $spaces_prepended;
}

sub _align_lines($$$$$$)
{
  my ($self, $text, $max_column, $direction, $locations, $images) = @_;

  my $result = '';

  my $updated_locations = {};
  if ($locations and @$locations) {
    foreach my $location (@$locations) {
      next unless (defined($location->{'bytes'}));
      push @{$updated_locations->{$location->{'lines'}}}, $location;
    }
  }
  my $images_marks = {};
  if ($images and @$images) {
    foreach my $image (@$images) {
      if ($image->{'lines_count'} > 1) {
        if (!$images_marks->{$image->{'lines'}}) {
          $images_marks->{$image->{'lines'}} = $image;
        }# else {
        # Happens in Info with the special construct as, in that 
        # case, there are no lines!  So no error...
        #  $self->_bug_message("more than one image with lines on $image->{'lines'}");
        # in that case, the $image->{'lines'} is not in sync with the
        # lines count.  So the second image will be treated as simple text.
        #}
      }
    }
  }

  my $bytes_count = 0;
  my $delta_bytes = 0;
  my $line_index = 0;
  my $image;
  my $image_lines_count;
  my $image_prepended_spaces;
  foreach my $line (split /^/, $text) {
    my $line_bytes_begin = 0;
    my $line_bytes_end = 0;
    my $removed_line_bytes_end = 0;
    my $removed_line_bytes_begin = 0;

    my ($new_image, $new_image_prepended_spaces);
    if ($images_marks->{$line_index}) {
      $new_image = $images_marks->{$line_index};
      $image_lines_count = 0;
      $new_image_prepended_spaces
       = _compute_spaces_align_line($new_image->{'image_width'}, $max_column, 
                                    $direction, $new_image->{'no_align'});
      if (!defined($image)) {
        $image = $new_image;
        $image_prepended_spaces = $new_image_prepended_spaces;
        $new_image = undef;
      }
    }

    my $orig_line;
    if (!$image) {
      my $chomped = chomp($line);
      # for debugging.
      $orig_line = $line;
      $removed_line_bytes_end -= count_bytes($self, $chomped);
      $line =~ s/^(\s*)//;
      $removed_line_bytes_begin -= count_bytes($self, $1);
      $line =~ s/(\s*)$//;
      $removed_line_bytes_end -= count_bytes($self, $1);
      my $line_width = Texinfo::Convert::Unicode::string_width($line);
      if ($line_width == 0) {
        $result .= "\n";
        $line_bytes_end += count_bytes($self, "\n");
        $bytes_count += count_bytes($self, "\n");
      } else {
        my $spaces_prepended 
         = _compute_spaces_align_line($line_width, $max_column, $direction);
        $result .= ' ' x$spaces_prepended . $line ."\n";
        $line_bytes_begin += count_bytes($self, ' ' x$spaces_prepended);
        $line_bytes_end += count_bytes($self, "\n");
        $bytes_count += $line_bytes_begin + $line_bytes_end 
                        + count_bytes($self, $line);
      }
    } else {
      $image_lines_count++;
      my $prepended_spaces = $image_prepended_spaces;
      # adjust if there is something else that the image on the first or
      # last line.  The adjustment is approximate.
      if (($image_lines_count == 1 or $image_lines_count == $image->{'lines_count'})
          and Texinfo::Convert::Unicode::string_width($line) > $image->{'image_width'}) {
        $prepended_spaces 
         -= Texinfo::Convert::Unicode::string_width($line) - $image->{'image_width'};
        $prepended_spaces = 0 if ($prepended_spaces < 0);
      }
      $result .= ' ' x$prepended_spaces . $line;
      $line_bytes_begin += count_bytes($self, ' ' x$prepended_spaces);
      $bytes_count += $line_bytes_begin + count_bytes($self, $line);
      if ($new_image) {
        $image = $new_image;
        $image_prepended_spaces = $new_image_prepended_spaces;
      } elsif ($image_lines_count == $image->{'lines_count'}) {
        $image = undef;
        $image_lines_count = undef;
        $image_prepended_spaces = undef;
      }
    }

    if ($updated_locations->{$line_index}) {
      foreach my $location (@{$updated_locations->{$line_index}}) {
        $location->{'bytes'} += $line_bytes_begin + $removed_line_bytes_begin 
                                + $delta_bytes;
      }
    }
    $delta_bytes += $line_bytes_begin + $line_bytes_end 
             + $removed_line_bytes_begin + $removed_line_bytes_end;
    $line_index++;
  }
  return ($result, $bytes_count);
}

sub _align_environment($$$$)
{
  my ($self, $result, $max, $align) = @_;

  _update_count_context($self);
  my $counts = pop @{$self->{'count_context'}};
  my $bytes_count;
  ($result, $bytes_count) = $self->_align_lines($result, $max,
                      $align, $counts->{'locations'}, $counts->{'images'});
  $self->_update_locations_counts($counts->{'locations'});
  $self->{'count_context'}->[-1]->{'bytes'} += $bytes_count;
  $self->{'count_context'}->[-1]->{'lines'} += $counts->{'lines'};
  push @{$self->{'count_context'}->[-1]->{'locations'}},
                       @{$counts->{'locations'}};
  return $result;
}

sub _contents($$$)
{
  my ($self, $section_root, $contents_or_shortcontents) = @_;

  my $contents = 1 if ($contents_or_shortcontents eq 'contents');

  # no sections
  return ('', 0) if (!$section_root or !$section_root->{'section_childs'});
  my $root_level = $section_root->{'section_childs'}->[0]->{'level'};
  foreach my $top_section(@{$section_root->{'section_childs'}}) {
    $root_level = $top_section->{'level'} 
      if ($top_section->{'level'} < $root_level);
  }

  my $result = '';
  my $lines_count = 0;
  # This is done like that because the tree may not be well formed if
  # there is a @part after a @chapter for example.
  foreach my $top_section (@{$section_root->{'section_childs'}}) {
    my $section = $top_section;
 SECTION:
    while ($section) {
      push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0};
      my $section_title_tree;
      if (defined($section->{'number'}) 
          and ($self->get_conf('NUMBER_SECTIONS')
               or !defined($self->get_conf('NUMBER_SECTIONS')))) {
        if ($section->{'cmdname'} eq 'appendix' and $section->{'level'} == 1) {
          $section_title_tree = $self->gdt('Appendix {number} {section_title}',
                           {'number' => {'text' => $section->{'number'}},
                            'section_title' 
                              => {'contents' => $section->{'args'}->[0]->{'contents'}}});
        } else {
          $section_title_tree = $self->gdt('{number} {section_title}',
                           {'number' => {'text' => $section->{'number'}},
                            'section_title' 
                              => {'contents' => $section->{'args'}->[0]->{'contents'}}});
        }
      } else {
        $section_title_tree = {'contents' => $section->{'args'}->[0]->{'contents'}};
      }
      my $section_title = $self->convert_line(
            {'contents' => [$section_title_tree],
             'type' => 'frenchspacing'});
      pop @{$self->{'count_context'}};
      my $text = $section_title;
      chomp ($text);
      $text .= "\n";
      my $repeat_count = 2 * ($section->{'level'} - ($root_level+1));
      ($result .= (' ' x $repeat_count)) if $repeat_count > 0;
      $result .= $text;
      $lines_count++;
      if ($section->{'section_childs'} 
          and ($contents or $section->{'level'} < $root_level+1)) {
        $section = $section->{'section_childs'}->[0];
      } elsif ($section->{'section_next'}) {
        last if ($section eq $top_section);
        $section = $section->{'section_next'};
      } else {
        last if ($section eq $top_section);
        while ($section->{'section_up'}) {
          $section = $section->{'section_up'};
          last SECTION if ($section eq $top_section);
          if ($section->{'section_next'}) {
            $section = $section->{'section_next'};
            last;
          }
        }
      }
    }
  }
  return ($result, $lines_count);
}

sub _menu($$)
{
  my ($self, $menu_command) = @_;

  if ($menu_command->{'cmdname'} eq 'menu') {
    my $result = "* Menu:\n\n";
    _add_text_count($self, $result);
    _add_lines_count($self, 2);
    if ($self->{'node'}) {
      $self->{'seenmenus'}->{$self->{'node'}} = 1;
    }
    return $result;
  } else {
    return '';
  }
}

sub _printindex($$)
{
  my ($self, $printindex) = @_;
  return $self->_printindex_formatted($printindex);
}

sub _normalize_top_node($)
{
  my $node = shift;
  return Texinfo::Common::normalize_top_node_name($node);
}

# convert and cache a node name.  $NODE is a node element.
sub _node_line($$)
{
  my ($self, $node) = @_;
  if (!$self->{'node_lines_text'}->{$node}) {
    my $node_text = {'type' => '_code',
              'contents' => $node->{'extra'}->{'node_content'}};
    push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0};
    $self->{'node_lines_text'}->{$node}->{'text'} 
       = _normalize_top_node($self->convert_line($node_text,
                                                 {'suppress_styles' => 1}));
    _update_count_context($self);
    my $end_context = pop @{$self->{'count_context'}};
    $self->{'node_lines_text'}->{$node}->{'count'} 
      = $end_context->{'bytes'};
  }
  return ($self->{'node_lines_text'}->{$node}->{'text'},
          $self->{'node_lines_text'}->{$node}->{'count'});
}

my $index_length_to_node = 41;

sub _printindex_formatted($$;$)
{
  my ($self, $printindex, $in_info) = @_;

  my $index_name;

  if ($printindex->{'extra'} and $printindex->{'extra'}->{'misc_args'}
      and defined($printindex->{'extra'}->{'misc_args'}->[0])) {
    $index_name = $printindex->{'extra'}->{'misc_args'}->[0];
  } else {
    return '';
  }

  # this is not redone for each index, only once
  if (!defined($self->{'index_entries'}) and $self->{'parser'}) {

    my $index_names = $self->{'parser'}->indices_information();
    my $merged_index_entries 
      = Texinfo::Structuring::merge_indices($index_names);
    $self->{'index_entries'} 
      = Texinfo::Structuring::sort_indices($self->{'parser'},
                                $merged_index_entries, $index_names);
    $self->{'index_names'} = $index_names;
  }
  if (!$self->{'index_entries'} or !$self->{'index_entries'}->{$index_name}
      or ! @{$self->{'index_entries'}->{$index_name}}) {
    return '';
  }

  my $result = '';
  $result .= _add_newline_if_needed($self);
  if ($in_info) {
    my $info_printindex_magic = "\x{00}\x{08}[index\x{00}\x{08}]\n";
    $result .= $info_printindex_magic;
    _add_text_count($self, $info_printindex_magic);
    _add_lines_count($self, 1);
  }
  my $heading = "* Menu:\n\n";

  $result .= $heading;
  _add_text_count($self, $heading);
  _add_lines_count($self, 2);

  # first determine the line numbers for the spacing of their formatting
  my %line_nrs;
  my %entry_nodes;
  my $max_index_line_nr_string_length = 0;
  my %ignored_entries;
  foreach my $entry (@{$self->{'index_entries'}->{$index_name}}) {
    my $line_nr;

    if (defined ($self->{'index_entries_line_location'}->{$entry->{'command'}})) {
      $line_nr = $self->{'index_entries_line_location'}->{$entry->{'command'}}->{'lines'};
      # ignore index entries in special regions that haven't been seen
    } elsif ($entry->{'region'}) {
      $ignored_entries{$entry} = 1;
      next;
    }

    my $node;
    # priority given to the location determined dynamically as the
    # index entry may be in footnote.
    if (defined($self->{'index_entries_line_location'}->{$entry->{'command'}}->{'node'})) {
      $node = $self->{'index_entries_line_location'}->{$entry->{'command'}}->{'node'};
    } elsif (defined($entry->{'node'})) {
      $node = $entry->{'node'};
    }
    $entry_nodes{$entry} = $node;
    if (!defined($node)) {
      $line_nr = 0;
    } elsif($in_info) {
      $line_nr = 3 if (defined($line_nr) and $line_nr < 3);
      $line_nr = 4 if (!defined($line_nr));
    } else {
      $line_nr = 0 if (!defined($line_nr));
    }
    my $index_line_nr_string_length = 
      Texinfo::Convert::Unicode::string_width($line_nr);
    $max_index_line_nr_string_length = $index_line_nr_string_length 
     if ($max_index_line_nr_string_length < $index_line_nr_string_length);
    $line_nrs{$entry} = $line_nr;
  }

  # this is used to count entries that are the same
  my %entry_counts = ();

  foreach my $entry (@{$self->{'index_entries'}->{$index_name}}) {
    next if ($ignored_entries{$entry});
    my $entry_tree = {'contents' => $entry->{'content'}};
    if ($entry->{'in_code'}) {
      $entry_tree->{'type'} = '_code';
    } else {
      $entry_tree->{'type'} = 'frenchspacing';
    }
    my $entry_text = '';

    my $formatter = $self->new_formatter('line',
                                   {'indent' => 0, 'suppress_styles' => 1});
    push @{$self->{'formatters'}}, $formatter;
    $entry_text = $self->_convert($entry_tree);
    $entry_text .= $self->convert_index_subentries($entry);
    $entry_text .= _count_added($self, $formatter->{'container'},
                  Texinfo::Convert::Paragraph::end($formatter->{'container'}));
    pop @{$self->{'formatters'}};

    next if ($entry_text !~ /\S/);

    # FIXME protect instead
    if ($entry_text =~ /:/ and $self->get_conf('INDEX_SPECIAL_CHARS_WARNING')) {
      $self->line_warn (sprintf(__("Index entry in \@%s with : produces invalid Info: %s"),
                                 $entry->{'index_at_command'},
          Texinfo::Convert::Texinfo::convert($entry_tree)), 
                        $entry->{'command'}->{'line_nr'});
    }

    my $entry_nr = '';
    if (!defined($entry_counts{$entry_text})) {
      $entry_counts{$entry_text} = 0;
    } else {
      $entry_counts{$entry_text}++;
      $entry_nr = ' <'.$entry_counts{$entry_text}.'>';
      _add_text_count($self, $entry_nr);
    }
    my $entry_line = "* $entry_text${entry_nr}: ";
    _add_text_count($self, "* ".": ");
    #_add_text_count($self, $entry_line);
    
    my $line_width = Texinfo::Convert::Unicode::string_width($entry_line);
    my $entry_line_addition = '';
    if ($line_width < $index_length_to_node) {
      my $spaces = ' ' x ($index_length_to_node - $line_width);
      $entry_line_addition .= $spaces;
      _add_text_count($self, $spaces);
    }
    my $node = $entry_nodes{$entry};

    if (!defined($node)) {
      # cache the transformation to text and byte counting, as
      # it is likeky that there is more than one such entry
      if (!$self->{'outside_of_any_node_text'}) {
        push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0};
        my $node_text = $self->gdt('(outside of any node)');
        $self->{'outside_of_any_node_text'}->{'text'} 
          = $self->convert_line($node_text);
        _update_count_context($self);
        my $end_context = pop @{$self->{'count_context'}};
        $self->{'outside_of_any_node_text'}->{'count'} 
          = $end_context->{'bytes'};
      }
      $entry_line_addition .= $self->{'outside_of_any_node_text'}->{'text'};
      $self->{'count_context'}->[-1]->{'bytes'} 
            += $self->{'outside_of_any_node_text'}->{'count'};
      # FIXME when outside of sectioning commands this message was already
      # done by the Parser.
      # Warn, only once.
      if (!$self->{'index_entries_no_node'}->{$entry}) {
        $self->line_warn(sprintf(__("entry for index `%s' outside of any node"),
                                 $index_name), $entry->{'command'}->{'line_nr'});
        $self->{'index_entries_no_node'}->{$entry} = 1;
      }
    } else {
      my ($node_line, $byte_count) = $self->_node_line($node);
      $entry_line_addition .= $node_line;
      $self->{'count_context'}->[-1]->{'bytes'} += $byte_count;
    }
    $entry_line_addition .= '.';
    _add_text_count($self, '.');

    $entry_line .= $entry_line_addition;
    $result .= $entry_line;

    my $line_nr = $line_nrs{$entry};
    my $line_nr_spaces = sprintf("%${max_index_line_nr_string_length}d", $line_nr);
    my $line_part = "(line ${line_nr_spaces})";
    $line_width += Texinfo::Convert::Unicode::string_width($entry_line_addition);
    my $line_part_width = Texinfo::Convert::Unicode::string_width($line_part);
    if ($line_width + $line_part_width +1 > $self->{'fillcolumn'}) {
      $line_part = "\n" . ' ' x ($self->{'fillcolumn'} - $line_part_width) 
           . "$line_part\n";
      _add_lines_count($self, 1);
    } else { 
      $line_part 
        = ' ' x ($self->{'fillcolumn'} - $line_part_width - $line_width)
           . "$line_part\n";
    }
    _add_lines_count($self, 1);
    _add_text_count($self, $line_part);
    $result .= $line_part;
  }

  $result .= "\n"; 
  _add_text_count($self, "\n");
  _add_lines_count($self, 1);
  
  return $result;
}


sub _node($$)
{
  my $self = shift;
  my $node = shift;

  return '';
}

# no error in plaintext
sub _error_outside_of_any_node($$)
{
  my $self = shift;
  my $root = shift;
}

sub _anchor($$)
{
  my $self = shift;
  my $anchor = shift;

  if (!($self->{'multiple_pass'} or $self->{'in_copying_header'})) {
    $self->_add_location($anchor); 
    $self->_error_outside_of_any_node($anchor);
  }
  return '';
}

my $listoffloat_entry_length = 41;
my $listoffloat_append = '...';

sub ensure_end_of_line($$)
{
  my ($self, $text) = @_;

  my $chomped = chomp ($text);
  if ($chomped) {
    $self->{'count_context'}->[-1]->{'bytes'} -= count_bytes($self, $chomped);
    $self->{'count_context'}->[-1]->{'lines'} -= 1;
  }
  $text .= "\n";
  $self->{'text_element_context'}->[-1]->{'counter'} = 0;
  _add_text_count($self, "\n");
  _add_lines_count($self, 1);
  return $text;
}

sub _image_text($$$)
{
  my ($self, $root, $basefile) = @_;

  my $txt_file = $self->Texinfo::Common::locate_include_file($basefile.'.txt');
  if (!defined($txt_file)) {
    return undef;
  } else {
    my $filehandle = do { local *FH };
    if (open ($filehandle, $txt_file)) {
      my $enc = $root->{'extra'}->{'input_perl_encoding'};
      binmode($filehandle, ":encoding($enc)")
        if ($enc);
      my $result = '';
      my $max_width = 0;
      while (<$filehandle>) {
        my $width = Texinfo::Convert::Unicode::string_width($_);
        if ($width > $max_width) {
          $max_width = $width;
        }
        $result .= $_;
      }
      # remove last end of line
      chomp ($result);
      if (!close ($filehandle)) {
        $self->document_warn(sprintf(__("error on closing image text file %s: %s"),
                                     $txt_file, $!));
      }
      return ($result, $max_width);
    } else {
      $self->line_warn(sprintf(__("\@image file `%s' unreadable: %s"), 
                               $txt_file, $!), $root->{'line_nr'});
    }
  }
  return undef;
}

sub _image_formatted_text($$$$)
{
  my ($self, $root, $basefile, $text) = @_;

  my $result;
  if (defined($text)) {
    $result = $text;
  } elsif (defined($root->{'args'}->[3])
      and @{$root->{'args'}->[3]->{'contents'}}) {
    $result = '[' .Texinfo::Convert::Text::convert(
      {'contents' => $root->{'args'}->[3]->{'contents'}},
      $self->{'convert_text_options'}) .']';
  } else {
    $self->line_warn(sprintf(__(
                    "could not find \@image file `%s.txt' nor alternate text"),
                             $basefile), $root->{'line_nr'});
    $result = '['.$basefile.']';
  }
  return $result;
}

sub _image($$)
{
  my ($self, $root) = @_;

  if (defined($root->{'args'}->[0])
        and @{$root->{'args'}->[0]->{'contents'}}) {
    my $basefile = Texinfo::Convert::Text::convert(
     {'contents' => $root->{'args'}->[0]->{'contents'}},
     {'code' => 1, %{$self->{'convert_text_options'}}});
    my ($text, $width) = $self->_image_text($root, $basefile);
    my $result = $self->_image_formatted_text($root, $basefile, $text);
    my $lines_count = ($result =~ tr/\n/\n/);
    if (!defined($width)) {
      $width = Texinfo::Convert::Unicode::string_width($result);
    }
    # the last line is part of the image but do not have a new line,
    # so 1 is added to $lines_count to have the number of lines of
    # the image
    $self->_add_image($root, $lines_count+1, $width);
    return ($result, $lines_count);
  }
  return ('', 0);
}

sub _get_form_feeds($)
{
  my $form_feeds = shift;
  $form_feeds =~ s/^[^\f]*//;
  $form_feeds =~ s/[^\f]$//;
  return $form_feeds;
}

sub _convert($$);

# Convert the Texinfo tree under $ROOT to plain text.
sub _convert($$)
{
  my ($self, $root) = @_;

  my $formatter = $self->{'formatters'}->[-1];

  if (($root->{'type'} and $self->{'ignored_types'}->{$root->{'type'}})
       or ($root->{'cmdname'} 
            and ($self->{'ignored_commands'}->{$root->{'cmdname'}}
                 or ($inline_commands{$root->{'cmdname'}}
                     and $root->{'cmdname'} ne 'inlinefmtifelse'
                     and (($inline_format_commands{$root->{'cmdname'}}
                          and (!$root->{'extra'}->{'format'}
                               or !$self->{'expanded_formats_hash'}->{$root->{'extra'}->{'format'}}))
                         or (!$inline_format_commands{$root->{'cmdname'}}
                             and !defined($root->{'extra'}->{'expand_index'}))))))) {
    return '';
  }
  my $result = '';

  my $type = $root->{'type'};
  my $command = $root->{'cmdname'};

  # in ignorable spaces, keep only form feeds.
  if ($type and $self->{'ignorable_space_types'}->{$type}
      and ($type ne 'empty_spaces_before_paragraph'
           or $self->get_conf('paragraphindent') ne 'asis')) {
    if ($root->{'text'} =~ /\f/) {
      $result = _get_form_feeds($root->{'text'});
    }
    _add_text_count($self, $result);
    return $result;
  }

  # First handle empty lines. This has to be done before the handling
  # of text below to be sure that an empty line is always processed
  # especially
  if ($type and ($type eq 'empty_line' 
                           or $type eq 'after_description_line')) {
    delete $self->{'text_element_context'}->[-1]->{'counter'};
    $self->{'empty_lines_count'}++;
    if ($self->{'empty_lines_count'} <= 1
        or $self->{'preformatted_context_commands'}->{$self->{'context'}->[-1]}) {
      $result = "";
      if ($root->{'text'} =~ /\f/) {
        $result .= _get_form_feeds($root->{'text'});
        _add_text_count($self, $result);
      }
      $result .= _count_added($self, $formatter->{'container'},
                add_text($formatter->{'container'}, "\n"));
      return $result;
    } else {
      return '';
    }
  }

  # process text
  if (defined($root->{'text'})) {
    if (!$type or $type ne 'untranslated') {
      if (!$formatter->{'_top_formatter'}) {
        if ($type and ($type eq 'raw' 
                                 or $type eq 'last_raw_newline')) {
          $result = _count_added($self, $formatter->{'container'},
                      add_next($formatter->{'container'}, $root->{'text'}));
        } else {
          my $text = _process_text($self, $root, $formatter);
          $result = _count_added($self, $formatter->{'container'},
                      add_text ($formatter->{'container'}, $text));
        }
        return $result;
      # the following is only possible if paragraphindent is set to asis
      } elsif ($type and $type eq 'empty_spaces_before_paragraph') {
        _add_text_count($self, $root->{'text'});
        return $root->{'text'};
      # ignore text outside of any format, but warn if ignored text not empty
      } elsif ($root->{'text'} =~ /\S/) {
        $self->_bug_message("ignored text not empty `$root->{'text'}'", $root);
        return '';
      } else {
        # miscellaneous top-level whitespace - possibly after an @image
        return _count_added($self, $formatter->{'container'},
                  add_text($formatter->{'container'}, $root->{'text'}));
      }
    } else {
      my $tree = $self->gdt($root->{'text'});
      my $converted = _convert($self, $tree);
      return $converted;
    }
  }

  if ($root->{'extra'}) {
    if ($root->{'extra'}->{'missing_argument'} 
             and (!$root->{'contents'} or !@{$root->{'contents'}})) {
      return '';
    }
  }

  if ($root->{'extra'} and $root->{'extra'}->{'index_entry'}
      and !$self->{'multiple_pass'} and !$self->{'in_copying_header'}) {
    my $location = $self->_add_location($root);
    # remove a 'lines' from $location if at the very end of a node
    # since it will lead to the next node otherwise.
    if ($command and $command =~ /index/) {
      my $following_not_empty;
      my @parents = @{$self->{'current_roots'}};
      my @parent_contents = @{$self->{'current_contents'}};
      while (@parents) {
        my $parent = pop @parents;
        my $parent_content = pop @parent_contents;
        if ($parent->{'type'} and $parent->{'type'} eq 'paragraph') {
          $following_not_empty = 1;
          last;
        }
        foreach my $following_content (@$parent_content) {
          unless (($following_content->{'type'} 
                and ($following_content->{'type'} eq 'empty_line'
                    or $ignorable_types{$following_content->{'type'}}))
              or ($following_content->{'cmdname'} 
                  and ($following_content->{'cmdname'} eq 'c'  
                       or $following_content->{'cmdname'} eq 'comment'))) {
            $following_not_empty = 1;
            last;
          }
        }
        last if $following_not_empty;
        if ($parent->{'cmdname'} and $root_commands{$parent->{'cmdname'}}) {
          last;
        }
      }
      if (! $following_not_empty) {
        $location->{'lines'}--;
      }
    }
    # this covers the special case for index entry not associated with a 
    # node but seen.  this will be an index entry in @copying, 
    # in @insertcopying.
    # This also covers the case of an index entry in a node added by a 
    # @footnote with footnotestyle separate.
    if ($self->{'node'}) {
      $location->{'node'} = $self->{'node'};
    }
    $self->{'index_entries_line_location'}->{$root} = $location;
  }

  my $cell;
  my $preformatted;
  if ($command) {
    my $unknown_command;
    if (defined($no_brace_commands{$command})) {
      if ($command eq ':') {
        remove_end_sentence($formatter->{'container'});
        return '';
      } elsif ($command eq '*') {
        $result = _count_added($self, $formatter->{'container'},
                              add_pending_word($formatter->{'container'}));
        $result .= _count_added($self, $formatter->{'container'},
                              end_line($formatter->{'container'}));
      } elsif ($command eq '.' or $command eq '?' or $command eq '!') {
        $result .= _count_added($self, $formatter->{'container'},
            add_next($formatter->{'container'}, $command));
        add_end_sentence($formatter->{'container'}, 1);
      } elsif ($command eq ' ' or $command eq "\n" or $command eq "\t") {
        $result .= _count_added($self, $formatter->{'container'}, 
            add_next($formatter->{'container'}, $no_brace_commands{$command}));
      } else {
        $result .= _count_added($self, $formatter->{'container'}, 
            add_text($formatter->{'container'}, $no_brace_commands{$command}));
      }
      return $result;
    } elsif ($command eq 'today') {
      my $today = $self->Texinfo::Common::expand_today();
      unshift @{$self->{'current_contents'}->[-1]}, $today;
    } elsif (exists($brace_no_arg_commands{$command})) {
      my $text;
      
      if ($command eq 'dots' or $command eq 'enddots') {
        # Don't use Unicode ellipsis character.
        $text = '...';
      } else {
        $text = Texinfo::Convert::Text::brace_no_arg_command($root, 
                                           $self->{'convert_text_options'});
      }

      # @AA{} should suppress an end sentence, @aa{} shouldn't.  This
      # is the case whether we are in @sc or not.
      if ($formatter->{'upper_case'}
          and $letter_no_arg_commands{$command}) {
        $text = _protect_sentence_ends($text);
        $text = uc($text);
      }

      if ($punctuation_no_arg_commands{$command}) {
        $result .= _count_added($self, $formatter->{'container'},
                    add_next($formatter->{'container'}, $text));
        add_end_sentence($formatter->{'container'}, 1);
      } elsif ($command eq 'tie') {
        $formatter->{'w'}++;
        set_space_protection($formatter->{'container'}, 1, undef)
          if ($formatter->{'w'} == 1);
        $result .= _count_added($self, $formatter->{'container'}, 
                       add_text($formatter->{'container'}, $text));
        $formatter->{'w'}--;
        set_space_protection($formatter->{'container'}, 0, undef)
          if ($formatter->{'w'} == 0);
      } else {
        $result .= _count_added($self, $formatter->{'container'}, 
                       add_text($formatter->{'container'}, $text));

        # This is to have @TeX{}, for example, not to prevent end sentences.
        if (!$letter_no_arg_commands{$command}) {
          allow_end_sentence($formatter->{'container'});
        }

        if ($command eq 'dots') {
          remove_end_sentence($formatter->{'container'});
        }
      }
      if ($formatter->{'var'} 
          or $formatter->{'font_type_stack'}->[-1]->{'monospace'}) {
        allow_end_sentence($formatter->{'container'});
      }
      return $result;
    # commands with braces
    } elsif ($accent_commands{$command}) {
      my $encoding;
      if ($self->{'enable_encoding'}) {
        $encoding = $self->{'output_encoding_name'};
      }
      my $sc;
      if ($formatter->{'upper_case'}) {
        $sc = 1;
      }
      my $accented_text 
         = Texinfo::Convert::Text::text_accents($root, $encoding, $sc);
      $result .= _count_added($self, $formatter->{'container'},
         add_text($formatter->{'container'}, $accented_text));

      my $accented_text_original;
      if ($formatter->{'upper_case'}) {
        $accented_text_original
         = Texinfo::Convert::Text::text_accents($root, $encoding);
      }

      if ($accented_text_original
            and $accented_text_original !~ /[[:upper:]]/
          or $formatter->{'var'} 
          or $formatter->{'font_type_stack'}->[-1]->{'monospace'}) {
        allow_end_sentence($formatter->{'container'});
      }

      # in case the text added ends with punctuation.  
      # If the text is empty (likely because of an error) previous 
      # punctuation will be cancelled, we don't want that.
      remove_end_sentence($formatter->{'container'})
        if ($accented_text ne '');
      return $result;
    } elsif ($self->{'style_map'}->{$command} 
         or ($root->{'type'} and $root->{'type'} eq 'definfoenclose_command')) {
      if ($code_style_commands{$command}) {
        if (!$formatter->{'font_type_stack'}->[-1]->{'monospace'}) {
          push @{$formatter->{'font_type_stack'}}, {'monospace' => 1};
        } else {
          $formatter->{'font_type_stack'}->[-1]->{'monospace'}++;
        }
      } elsif ($regular_font_style_commands{$command}) {
        if ($formatter->{'font_type_stack'}->[-1]->{'monospace'}) {
          push @{$formatter->{'font_type_stack'}}, {'monospace' => 0, 
                                                    'normal' => 1};
        } elsif ($formatter->{'font_type_stack'}->[-1]->{'normal'}) {
          $formatter->{'font_type_stack'}->[-1]->{'normal'}++;
        }
      }
      if ($no_punctation_munging_commands{$command}) {
        push @{$formatter->{'frenchspacing_stack'}}, 'on';
        set_space_protection($formatter->{'container'}, undef,
          undef,undef,1);
      }
      if ($upper_case_commands{$command}) {
        $formatter->{'upper_case'}++;
        $formatter->{'var'}++ if ($command eq 'var');
      }
      if ($command eq 'w') {
        $formatter->{'w'}++;
        set_space_protection($formatter->{'container'}, 1,undef)
          if ($formatter->{'w'} == 1);
      }
      my ($text_before, $text_after);
      if ($root->{'type'} and $root->{'type'} eq 'definfoenclose_command') {
        $text_before = $root->{'extra'}->{'begin'};
        $text_after = $root->{'extra'}->{'end'};
      } elsif ($non_quoted_commands_when_nested{$command} 
            and $formatter->{'font_type_stack'}->[-1]->{'code_command'}) {
        $text_before = '';
        $text_after = '';
      } elsif ($formatter->{'suppress_styles'}
               and !$index_style_commands{$command}) {
        $text_before = '';
        $text_after = '';
      } else {
        $text_before = $self->{'style_map'}->{$command}->[0];
        $text_after = $self->{'style_map'}->{$command}->[1];
      }
      # do this after determining $text_before/$text_after such that it
      # doesn't impact the current command, but only commands nested within
      if ($non_quoted_commands_when_nested{$command}) {
        $formatter->{'font_type_stack'}->[-1]->{'code_command'}++;
      }
      $result .= _count_added($self, $formatter->{'container'},
               add_next($formatter->{'container'}, $text_before, 1))
         if ($text_before ne '');
      if ($root->{'args'}) {
        $result .= _convert($self, $root->{'args'}->[0]);
        if ($command eq 'strong' 
             and scalar (@{$root->{'args'}->[0]->{'contents'}})
             and $root->{'args'}->[0]->{'contents'}->[0]->{'text'}
             and $root->{'args'}->[0]->{'contents'}->[0]->{'text'} =~ /^Note\s/i
             and $self->{'output_format'}
             and $self->{'output_format'} eq 'info') {
          $self->line_warn(__(
    "\@strong{Note...} produces a spurious cross-reference in Info; reword to avoid that"), 
                           $root->{'line_nr'});
        }
      }
      $result .= _count_added($self, $formatter->{'container'},
               add_next($formatter->{'container'}, $text_after, 1))
         if ($text_after ne '');
      if ($command eq 'w') {
        $formatter->{'w'}--;
        set_space_protection($formatter->{'container'},0,undef)
          if ($formatter->{'w'} == 0);
      }
      if ($code_style_commands{$command}) {
        $formatter->{'font_type_stack'}->[-1]->{'monospace'}--;
        allow_end_sentence($formatter->{'container'});
        pop @{$formatter->{'font_type_stack'}}
          if !$formatter->{'font_type_stack'}->[-1]->{'monospace'};
      } elsif ($regular_font_style_commands{$command}) {
        if ($formatter->{'font_type_stack'}->[-1]->{'normal'}) {
          $formatter->{'font_type_stack'}->[-1]->{'normal'}--;
          pop @{$formatter->{'font_type_stack'}}
            if !$formatter->{'font_type_stack'}->[-1]->{'normal'};
        }
      }
      if ($non_quoted_commands_when_nested{$command}) {
        #$formatter->{'code_command'}--;
        $formatter->{'font_type_stack'}->[-1]->{'code_command'}--;
      }
      if ($no_punctation_munging_commands{$command}) {
        pop @{$formatter->{'frenchspacing_stack'}};
        my $frenchspacing = 0;
        $frenchspacing = 1 if ($formatter->{'frenchspacing_stack'}->[-1] eq 'on');
        set_space_protection($formatter->{'container'}, undef,
          undef, undef, $frenchspacing);
      }
      if ($upper_case_commands{$command}) {
        $formatter->{'upper_case'}--;
        if ($command eq 'var') {
          $formatter->{'var'}--;
          # Allow a following full stop to terminate a sentence.
          allow_end_sentence($formatter->{'container'});
        }
      }
      return $result;
    } elsif ($command eq 'image') {
      $result = _count_added($self, $formatter->{'container'},
                   add_pending_word($formatter->{'container'}, 1));
      # add an empty word so that following spaces aren't lost
      add_next($formatter->{'container'},'');
      my ($image, $lines_count) = $self->_image($root);
      _add_lines_count($self, $lines_count);
      _add_text_count($self, $image);
      if ($image ne '' and $formatter->{'type'} ne 'paragraph') {
        $self->{'empty_lines_count'} = 0;
      }
      $result .= $image; 
      return $result;
    } elsif ($command eq 'email') {
      # nothing is output for email, instead the command is substituted.
      my @email_contents;
      if ($root->{'args'}) {
        my $name;
        my $email;
        if (scalar (@{$root->{'args'}}) == 2
            and defined($root->{'args'}->[1])
            and @{$root->{'args'}->[1]->{'contents'}}) {
          $name = $root->{'args'}->[1]->{'contents'};
        }
        if (defined($root->{'args'}->[0])
            and @{$root->{'args'}->[0]->{'contents'}}) {
          $email = $root->{'args'}->[0]->{'contents'};
        }
        my $prepended;
        if ($name and $email) {
          $prepended = $self->gdt('{name} @url{{email}}', 
                           {'name' => $name, 'email' => $email});
        } elsif ($email) {
          $prepended = $self->gdt('@url{{email}}', 
                           {'email' => $email});
        } elsif ($name) {
          $prepended = {'contents' => $name};
        } else {
          return '';
        }
        unshift @{$self->{'current_contents'}->[-1]}, $prepended;
      }
      return '';
    } elsif ($command eq 'uref' or $command eq 'url') {
      if ($root->{'args'}) {
        if (scalar(@{$root->{'args'}}) == 3
             and defined($root->{'args'}->[2])
             and @{$root->{'args'}->[2]->{'contents'}}) {
          unshift @{$self->{'current_contents'}->[-1]}, 
            {'contents' => $root->{'args'}->[2]->{'contents'}};
        } elsif (@{$root->{'args'}->[0]->{'contents'}}) {
          # no mangling of --- and similar in url.
          my $url = {'type' => '_code',
              'contents' => $root->{'args'}->[0]->{'contents'}};
          if (scalar(@{$root->{'args'}}) == 2
             and defined($root->{'args'}->[1])
             and @{$root->{'args'}->[1]->{'contents'}}) {
            my $prepended = $self->gdt('{text} ({url})', 
                 {'text' => $root->{'args'}->[1]->{'contents'},
                  'url' => $url });
            unshift @{$self->{'current_contents'}->[-1]}, $prepended;
          } else {
            my $prepended = $self->gdt('@t{<{url}>}', 
                                        {'url' => $url});
            unshift @{$self->{'current_contents'}->[-1]}, $prepended
          }
        } elsif (scalar(@{$root->{'args'}}) == 2
                 and defined($root->{'args'}->[1])
                 and @{$root->{'args'}->[1]->{'contents'}}) {
          unshift @{$self->{'current_contents'}->[-1]}, 
            {'contents' => $root->{'args'}->[1]->{'contents'}};
        }
      }
      return '';
    } elsif ($command eq 'footnote') {
      $self->{'footnote_index'}++ unless ($self->{'multiple_pass'});
      my $formatted_footnote_number;
      if ($self->get_conf('NUMBER_FOOTNOTES')) {
        $formatted_footnote_number = $self->{'footnote_index'};
      } else {
        $formatted_footnote_number = $NO_NUMBER_FOOTNOTE_SYMBOL;
      }
      push @{$self->{'pending_footnotes'}}, {'root' => $root, 
                                    'number' => $self->{'footnote_index'}}
          unless ($self->{'multiple_pass'});
      if (!$self->{'in_copying_header'}) {
        $self->_error_outside_of_any_node($root);
      }
      $result .= _count_added($self, $formatter->{'container'},
           add_next($formatter->{'container'},
                    "($formatted_footnote_number)", 1));
      if ($self->get_conf('footnotestyle') eq 'separate' and $self->{'node'}) {
        $result .= _convert($self, {'contents' => 
         [{'text' => ' ('},
          {'cmdname' => 'pxref',
           'args' => [
             {'type' => 'brace_command_arg',
              'contents' => [
                 @{$self->{'node'}->{'extra'}->{'node_content'}},
                 {'text' => "-Footnote-$self->{'footnote_index'}"}
              ]
             }
           ]
          },
          {'text' => ')'}],
          });
      }
      return $result;
    } elsif ($command eq 'anchor') {
      $result = _count_added($self, $formatter->{'container'},
                   add_pending_word($formatter->{'container'}));
      $result .= $self->_anchor($root);
      return $result;
    } elsif ($ref_commands{$command}) {
      if (scalar(@{$root->{'args'}})) {
        my @args;
        for my $a (@{$root->{'args'}}) {
          if (defined $a->{'contents'} and @{$a->{'contents'}}) {
            push @args, $a->{'contents'};
          } else {
            push @args, undef;
          }
        }
        $args[0] = [{'text' => ''}] if (!defined($args[0]));

        # normalize node name, to get a ref with the right formatting
        # NOTE as a consequence, the line numbers appearing in case of errors
        # correspond to the node lines numbers, and not the @ref.
        my $node_content;
        if ($root->{'extra'}
            and $root->{'extra'}->{'label'}) {
          $node_content = $root->{'extra'}->{'label'}->{'extra'}->{'node_content'};
        } else { 
          $node_content = $args[0];
        }

        # if it a reference to a float with a label, $arg[1] is
        # set to '$type $number' or '$number' if there is no type.
        if (! defined($args[1]) 
            and $root->{'extra'}
            and $root->{'extra'}->{'label'}
            and $root->{'extra'}->{'label'}->{'cmdname'}
            and $root->{'extra'}->{'label'}->{'cmdname'} eq 'float') {
          my $float = $root->{'extra'}->{'label'};

          my $name = $self->_float_type_number($float);
          $args[1] = $name->{'contents'};
        }
        if ($command eq 'inforef' and scalar(@args) == 3) {
          $args[3] = $args[2];
          $args[2] = undef;
        }

        # Treat cross-reference commands in a multitable cell as if they
        # were surrounded by @w{ ... }, so the output will not be split across
        # lines, leading text from other columns appearing to be part of the
        # cross-reference.
        my $in_multitable = 0;
        if ($self->{'document_context'}->[-1]->{'in_multitable'}) {
          $in_multitable = 1;
          $formatter->{'w'}++;
          set_space_protection($formatter->{'container'}, 1, undef)
            if ($formatter->{'w'} == 1);
        }
        # Disallow breaks in runs of Chinese text in node names, because a 
        # break would be normalized to a single space by the Info reader, and 
        # the node wouldn't be found.
        set_space_protection($formatter->{'container'},
                    undef, undef, undef, undef, 1);

        if ($command eq 'xref') {
          $result = _convert($self, {'contents' => [{'text' => '*Note '}]});
        } else {
          $result = _convert($self, {'contents' => [{'text' => '*note '}]});
        }
        my $name;
        if (defined($args[1])) {
          $name = $args[1];
        } elsif (defined($args[2])) {
          $name = $args[2];
        }
        my $file;
        if (defined($args[3])) {
          $file = [{'text' => '('},
                   {'type' => '_code',
                    'contents' => $args[3]},
                   {'text' => ')'},];
        } elsif (defined($args[4])) {
          # add a () such that the node is considered to be external, 
          # even though the manual name is not known.
          $file = [{'text' => '()'}];
        }
         
        if ($name) {
          my $name_text = _convert($self, {'contents' => $name});
          # needed, as last word is added only when : is added below
          my $name_text_checked = $name_text
             .get_pending($self->{'formatters'}->[-1]->{'container'});
          my $quoting_required = 0;
          if ($name_text_checked =~ /:/m) { 
              if ($self->get_conf('INFO_SPECIAL_CHARS_WARNING')) {
                $self->line_warn(sprintf(__(
                   "\@%s cross-reference name should not contain `:'"),
                                               $command), $root->{'line_nr'});
              }
              if ($self->get_conf('INFO_SPECIAL_CHARS_QUOTE')) {
                $quoting_required = 1;
              }
          }
          my $pre_quote = $quoting_required ? "\x{7f}" : '';
          my $post_quote = $pre_quote;
          $name_text .= _convert($self, {'contents' => [
                {'text' => "$post_quote: "}]});
          $name_text =~ s/^(\s*)/$1$pre_quote/ if $pre_quote;
          $result .= $name_text;
          _count_added($self,$self->{'formatters'}[-1]{'container'},
                       $pre_quote)
            if $pre_quote;

          if ($file) {
            $result .= _convert($self, {'contents' => $file});
          }
          # node name
          $self->{'formatters'}->[-1]->{'suppress_styles'} = 1;

          my $maybe_file
            = get_pending($self->{'formatters'}->[-1]->{'container'});
          my $node_text = _convert($self, {'type' => '_code',
                                           'contents' => $node_content});
          delete $self->{'formatters'}->[-1]->{'suppress_styles'};

          my $node_text_checked = $node_text 
             .get_pending($self->{'formatters'}->[-1]->{'container'});
          $maybe_file =~ s/^\s*//;
          $maybe_file = quotemeta $maybe_file;
          $node_text_checked =~ s/^\s*$maybe_file//;
          $quoting_required = 0;
          if ($node_text_checked =~ /([,\t\.])/m ) {
              if ($self->get_conf('INFO_SPECIAL_CHARS_WARNING')) {
                $self->line_warn(sprintf(__(
                   "\@%s node name should not contain `%s'"), $command, $1),
                                 $root->{'line_nr'});
              }
              if ($self->get_conf('INFO_SPECIAL_CHARS_QUOTE')) {
                $quoting_required = 1;
              }
          }
          $pre_quote = $quoting_required ? "\x{7f}" : '';
          $post_quote = $pre_quote;
          if ($pre_quote) {
            $node_text =~ s/^(\s*)/$1$pre_quote/;
            _count_added($self,$self->{'formatters'}[-1]{'container'},
                         $pre_quote);
          }
          $result .= $node_text;
          _count_added($self, $self->{'formatters'}[-1]{'container'},
            add_next($self->{'formatters'}->[-1]->{'container'}, $post_quote))
                 if $post_quote;
        } else { # Label same as node specification
          if ($file) {
            $result .= _convert($self, {'contents' => $file});
          }
          $self->{'formatters'}->[-1]->{'suppress_styles'} = 1;
          my $node_text = _convert($self, {'type' => '_code',
                                           'contents' => $node_content});
          delete $self->{'formatters'}->[-1]->{'suppress_styles'};

          my $node_text_checked = $node_text 
             .get_pending($self->{'formatters'}->[-1]->{'container'});
          my $quoting_required = 0;
          if ($node_text_checked =~ /:/m) {
            if ($self->get_conf('INFO_SPECIAL_CHARS_WARNING')) {
              $self->line_warn(sprintf(__(
                 "\@%s node name should not contain `:'"), $command),
                               $root->{'line_nr'});
            }
            if ($self->get_conf('INFO_SPECIAL_CHARS_QUOTE')) {
              $quoting_required = 1;
            }
          }
          my $pre_quote = $quoting_required ? "\x{7f}" : '';
          my $post_quote = $pre_quote;
          $node_text .= _convert($self, {'contents' => [
                {'text' => "${post_quote}::"}]});
          _count_added($self,$self->{'formatters'}[-1]{'container'},
                       $pre_quote)
            if $pre_quote;
          if ($pre_quote) {
            # This is needed to get a pending word.  We could use
            # add_pending_word, but that would not include following 
            # punctuation in the word.
            my $next = $self->{'current_contents'}->[-1]->[0];
            if ($next) {
              $node_text .= _convert($self, $next);
              shift @{$self->{'current_contents'}->[-1]};
            }

            $node_text =~ s/^(\s*)/$1$pre_quote/;
          }
          $result .= $node_text;
        }
        # we could use $formatter, but in case it was changed in _convert 
        # we play it safe.
        my $pending = $result 
             .get_pending($self->{'formatters'}->[-1]->{'container'});

        # If command is @xref, the punctuation must always follow the
        # command, for other commands it may be in the argument, hence the
        # use of $pending.
        # FIXME: is @xref really special here?
        if ($name and ($command eq 'xref'
            or ($pending !~ /[\.,]$/ and $pending !~ /::$/))) {
          my $next = $self->{'current_contents'}->[-1]->[0];
          if (!($next and $next->{'text'} and $next->{'text'} =~ /^[\.,]/)) {
            if ($command eq 'xref') {
              if ($next and defined($next->{'text'}) and $next->{'text'} =~ /\S/) {
                my $text = $next->{'text'};
                $text =~ s/^\s*//;
                my $char = substr($text, 0, 1);
                $self->line_warn(sprintf(__(
                            "`.' or `,' must follow \@xref, not %s"), 
                                         $char), $root->{'line_nr'});
              } else {
                $self->line_warn(__("`.' or `,' must follow \@xref"), 
                                 $root->{'line_nr'});
              }
            }
            my @added = ({'text' => '.'});
            # The added full stop does not end a sentence.  Info readers will
            # have a chance of guessing correctly whether the full stop was
            # added by whether it is followed by 2 spaces (although this
            # doesn't help at the end of a line).
            push @added, {'cmdname' => ':'};
            unshift @{$self->{'current_contents'}->[-1]}, @added;
          }
        }

        if ($in_multitable) {
          $formatter->{'w'}--;
          set_space_protection($formatter->{'container'}, 0, undef)
            if ($formatter->{'w'} == 0);
        }
        set_space_protection($formatter->{'container'},
          undef,undef,undef,undef,0); # double_width_no_break
        return $result;
      }
      return '';
    } elsif ($explained_commands{$command}) {
      if ($root->{'args'}
          and defined($root->{'args'}->[0])
          and @{$root->{'args'}->[0]->{'contents'}}) {
        # in abbr spaces never end a sentence.
        my $argument;
        if ($command eq 'abbr') {
          $argument = {'type' => 'frenchspacing',
                       'contents' => $root->{'args'}->[0]->{'contents'}};
        } else {
          $argument = { 'contents' => $root->{'args'}->[0]->{'contents'}};
        }
        if (scalar (@{$root->{'args'}}) == 2
            and defined($root->{'args'}->[-1])
            and @{$root->{'args'}->[-1]->{'contents'}}) {
          my $prepended = $self->gdt('{abbr_or_acronym} ({explanation})', 
                           {'abbr_or_acronym' => $argument, 
                            'explanation' => $root->{'args'}->[-1]->{'contents'}});
          unshift @{$self->{'current_contents'}->[-1]}, $prepended;
          return '';
        } else {
          $result = _convert($self, $argument);

          # We want to permit an end of sentence, but not force it as @. does.
          allow_end_sentence($formatter->{'container'});
          return $result;
        }
      }
      return '';
    } elsif ($inline_commands{$command}) {
      my $arg_index = 1;
      if ($command eq 'inlinefmtifelse'
          and (!$root->{'extra'}->{'format'} 
               or !$self->{'expanded_formats_hash'}->{$root->{'extra'}->{'format'}})) {
        $arg_index = 2;
      }
      if (scalar(@{$root->{'args'}}) > $arg_index
         and defined($root->{'args'}->[$arg_index])
         and @{$root->{'args'}->[$arg_index]->{'contents'}}) {
        my $argument;
        if ($command eq 'inlineraw') {
          $argument->{'type'} = '_code';
        }
        $argument->{'contents'} = $root->{'args'}->[$arg_index]->{'contents'};
        unshift @{$self->{'current_contents'}->[-1]}, ($argument);
      }
      return '';
      # condition should actually be that the $command is inline
    } elsif ($math_commands{$command} and not exists($block_commands{$command})) {
      push @{$self->{'context'}}, $command;
      if ($root->{'args'}) {
        $result .= _convert($self, {'type' => 'frenchspacing',
             'contents' => [{'type' => '_code',
                            'contents' => [$root->{'args'}->[0]]}]});
      }
      my $old_context = pop @{$self->{'context'}};
      die if ($old_context ne $command);
      return $result;
    } elsif ($command eq 'titlefont') {
      push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0};
      $result = $self->convert_line ({'type' => 'frenchspacing',
               'contents' => [$root->{'args'}->[0]]});
      pop @{$self->{'count_context'}};
      $result = Texinfo::Convert::Text::heading({'level' => 0, 
        'cmdname' => 'titlefont'}, $result, $self, 
        $self->get_conf('NUMBER_SECTIONS'),
        ($self->{'format_context'}->[-1]->{'indent_level'}) * $indent_length);
      $result =~ s/\n$//; # final newline has its own tree element
      $self->{'empty_lines_count'} = 0 unless ($result eq '');
      _add_text_count($self, $result);
      _add_lines_count($self, 1);
      return $result;

    } elsif ($command eq 'U') {
      my $arg;
      if ($root->{'args'}
          and $root->{'args'}->[0]
          and $root->{'args'}->[0]->{'contents'}
          and $root->{'args'}->[0]->{'contents'}->[0]
          and $root->{'args'}->[0]->{'contents'}->[0]->{'text'}) {
        $arg = $root->{'args'}->[0]->{'contents'}->[0]->{'text'};
      }
      if ($arg) {
        # Syntactic checks on the value were already done in Parser.pm,
        # but we have one more thing to test: since this is the one
        # place where we might output actual UTF-8 binary bytes, we have
        # to check that chr(hex($arg)) is valid.  Perl gives a warning
        # and will not output UTF-8 for Unicode non-characters such as
        # U+10FFFF.  In this case, silently fall back to plain text, on
        # the theory that the user wants something.
        my $res;
        if ($self->{'to_utf8'}) {
          my $error = 0;
          # The warning about non-characters is only given when the code
          # point is attempted to be output, not just manipulated.
          # http://stackoverflow.com/questions/5127725/how-could-i-catch-an-unicode-non-character-warning
          #
          # Therefore, we have to try to output it within an eval.
          # Since opening /dev/null or a temporary file means
          # more system-dependent checks, use a string as our
          # filehandle.
          eval {
            use warnings FATAL => qw(all);
            my ($fh, $string);
            open($fh, ">", \$string) || die "open(U string eval) failed: $!";
            binmode($fh, ":utf8") || die "binmode(U string eval) failed: $!";
            print $fh chr(hex("$arg"));
          };
          if ($@) {
            warn "\@U chr(hex($arg)) eval failed: $@\n" if ($self->{'DEBUG'});
            $error = 1;
          } elsif (hex($arg) > 0x10FFFF) {
            # The check above appears not to work in older versions of perl,
            # so check the argument is not greater the maximum Unicode code 
            # point.
            $error = 1;
          }
          if ($error) {
            $res = "U+$arg";
          } else {
            $res = chr(hex($arg)); # ok to call chr
          }
        } else {
          $res = "U+$arg";  # not outputting UTF-8
        }
        $result .= _count_added($self, $formatter->{'container'}, 
                   add_text($formatter->{'container'}, $res));
      } else {
        $result = '';  # arg was not defined
      }
      return $result;

    } elsif ($command eq 'value') {
      my $expansion = $self->gdt('@{No value for `{value}\'@}', 
                                    {'value' => $root->{'type'}});
      if ($formatter->{'_top_formatter'}) {
        $expansion = {'type' => 'paragraph',
                      'contents' => [$expansion]};
      }
      $result .= _convert($self, $expansion);
      #  unshift @{$self->{'current_contents'}->[-1]}, $expansion;
      #return '';
      return $result;
    } elsif ($root->{'args'} and $root->{'args'}->[0] 
             and $root->{'args'}->[0]->{'type'}
             and $root->{'args'}->[0]->{'type'} eq 'brace_command_arg') {
    # block commands
    } elsif (exists($block_commands{$command})) {
      # remark:
      # cartouche group and raggedright -> nothing on format stack

      if ($menu_commands{$command} and $self->get_conf('FORMAT_MENU') eq 'nomenu') {
        return '';
      }
      if ($self->{'preformatted_context_commands'}->{$command}
          or $command eq 'float') {
        if ($self->{'formatters'}->[-1]->{'type'} eq 'paragraph'
            and $format_raw_commands{$command}) {
          $result .= _count_added($self, $formatter->{'container'},
                              add_pending_word($formatter->{'container'}, 1));
          $result .= _count_added($self, $formatter->{'container'},
                              end_line($formatter->{'container'}));
        }
        push @{$self->{'context'}}, $command;
      } elsif ($flush_commands{$command}) {
        push @{$self->{'context'}}, $command;
      } elsif ($raw_commands{$command} or $block_math_commands{$command}) {
        if (!$self->{'formatters'}->[-1]->{'_top_formatter'}) {
          # reuse the current formatter if not in top level
          $result .= _count_added($self, $formatter->{'container'},
                              add_pending_word($formatter->{'container'}, 1));
          $result .= _count_added($self, $formatter->{'container'},
                              end_line($formatter->{'container'}));
        } else {
          # if in top level, the raw block command is turned into a 
          # simple preformatted command (alike @verbatim), to have a 
          # formatter container being created.
          push @{$self->{'context'}}, $command;
          $self->{'format_context_commands'}->{$command} = 1;
          $self->{'preformatted_context_commands'}->{$command} = 1;
        }
      }

      if ($self->{'format_context_commands'}->{$command}) {
        push @{$self->{'format_context'}}, 
             { 'cmdname' => $command,
               'paragraph_count' => 0,
               'indent_level' => 
                   $self->{'format_context'}->[-1]->{'indent_level'},
             };
        $self->{'format_context'}->[-1]->{'indent_level'}++
           if ($indented_commands{$command});
        # open a preformatted container, if the command opening the 
        # preformatted context is not a classical preformatted 
        # command (ie if it is menu or verbatim, and not example or  
        # similar)
        if ($self->{'preformatted_context_commands'}->{$command}
            and ! $preformatted_commands{$command}
            and ! $format_raw_commands{$command}) {
          $preformatted = $self->new_formatter('unfilled');
          push @{$self->{'formatters'}}, $preformatted;
        }
      }
      if ($command eq 'quotation'
          or $command eq 'smallquotation') {
        if ($root->{'args'} and $root->{'args'}->[0]
            and $root->{'args'}->[0]->{'contents'}
            and @{$root->{'args'}->[0]->{'contents'}}) {
          my $prepended = $self->gdt('@b{{quotation_arg}:} ', 
             {'quotation_arg' => $root->{'args'}->[0]->{'contents'}});
          $prepended->{'type'} = 'frenchspacing';
          $result .= $self->convert_line($prepended);
          $self->{'text_element_context'}->[-1]->{'counter'} += 
             Texinfo::Convert::Unicode::string_width($result);
          $self->{'empty_lines_count'} = 0 unless ($result eq '');
        }
      } elsif ($menu_commands{$command}) {
        $result .= $self->_menu($root);
      } elsif ($command eq 'multitable') {
        my $columnsize;
        if ($root->{'extra'}->{'columnfractions'}) {
          foreach my $fraction (@{$root->{'extra'}->{'columnfractions'}
                                       ->{'extra'}->{'misc_args'}}) {
            push @$columnsize, int($fraction * $self->{'text_element_context'}->[-1]->{'max'} +0.5);
          }
        } elsif ($root->{'extra'}->{'prototypes'}) {
          foreach my $prototype (@{$root->{'extra'}->{'prototypes'}}) {
            push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0};
            my ($formatted_prototype) = $self->convert_line($prototype, 
                                                        {'indent_length' => 0});
            pop @{$self->{'count_context'}};
            push @$columnsize, 
                 2+Texinfo::Convert::Unicode::string_width($formatted_prototype);
          }
        }
        $self->{'format_context'}->[-1]->{'columns_size'} = $columnsize;
        $self->{'format_context'}->[-1]->{'row_empty_lines_count'} 
          = $self->{'empty_lines_count'};
        $self->{'document_context'}->[-1]->{'in_multitable'}++;
      } elsif ($command eq 'float') {
        $result .= _add_newline_if_needed($self);
        if ($root->{'extra'} and $root->{'extra'}->{'node_content'}) {
          $result .= $self->_anchor($root);
        }
      }
    } elsif ($command eq 'node') {
      $self->{'node'} = $root;
      $result .= $self->_node($root);
      $self->{'format_context'}->[-1]->{'paragraph_count'} = 0;
    } elsif ($sectioning_commands{$command}) {
      # use settitle for empty @top
      # ignore @part
      my $contents;
      if ($root->{'args'}->[0]->{'contents'} 
          and @{$root->{'args'}->[0]->{'contents'}} 
          and $command ne 'part') {
        $contents = $root->{'args'}->[0]->{'contents'};
      } elsif ($command eq 'top'
          and $self->{'extra'}->{'settitle'} 
          and $self->{'extra'}->{'settitle'}->{'args'}
          and @{$self->{'extra'}->{'settitle'}->{'args'}}
          and $self->{'extra'}->{'settitle'}->{'args'}->[0]->{'contents'}
          and @{$self->{'extra'}->{'settitle'}->{'args'}->[0]->{'contents'}}) {
        $contents = $self->{'extra'}->{'settitle'}->{'args'}->[0]->{'contents'};
      }
             
      if ($contents) {
        push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0};
        my $heading = $self->convert_line({'type' => 'frenchspacing',
                         'contents' => $contents});
        pop @{$self->{'count_context'}};
        # @* leads to an end of line, underlying appears on the line below
        # over one line
        my $heading_underlined = 
             Texinfo::Convert::Text::heading($root, $heading, $self,
                                             $self->get_conf('NUMBER_SECTIONS'),
                           ($self->{'format_context'}->[-1]->{'indent_level'})
                                           * $indent_length);
        $result .= _add_newline_if_needed($self);
        $self->{'empty_lines_count'} = 0 unless ($heading_underlined eq '');
        _add_text_count($self, $heading_underlined);
        $result .= $heading_underlined;
        if ($heading_underlined ne '') {
          _add_lines_count($self, 2);
          $result .= _add_newline_if_needed($self);
        }
      }
      $self->{'format_context'}->[-1]->{'paragraph_count'} = 0;
    } elsif (($command eq 'item' or $command eq 'itemx')
            and $root->{'args'} and $root->{'args'}->[0] 
            and $root->{'args'}->[0]->{'type'}
            and $root->{'args'}->[0]->{'type'} eq 'line_arg') {
      if ($root->{'args'} and @{$root->{'args'}}
          and $root->{'args'}->[0]->{'contents'}) {

        my $converted_tree = $self->_table_item_content_tree($root,
                                         $root->{'args'}->[0]->{'contents'});

        $converted_tree->{'type'} = 'frenchspacing';
        $result = $self->convert_line($converted_tree,
                    {'indent_level'
                      => $self->{'format_context'}->[-1]->{'indent_level'} -1});
        if ($result ne '') {
          $result = $self->ensure_end_of_line($result);
          $self->{'empty_lines_count'} = 0;
        }
      }
    } elsif ($command eq 'item' and $root->{'parent'}->{'cmdname'}
             and $item_container_commands{$root->{'parent'}->{'cmdname'}}) {
      $self->{'format_context'}->[-1]->{'paragraph_count'} = 0;
      my $line = $self->new_formatter('line', 
          {'indent_length' => 
              ($self->{'format_context'}->[-1]->{'indent_level'} -1)
                * $indent_length
                 + $item_indent_format_length{$root->{'parent'}->{'cmdname'}}});
      push @{$self->{'formatters'}}, $line;
      if ($root->{'parent'}->{'cmdname'} eq 'enumerate') {
        $result = _count_added($self, $line->{'container'},
            add_next($line->{'container'},
               Texinfo::Common::enumerate_item_representation(
                 $root->{'parent'}->{'extra'}->{'enumerate_specification'},
                 $root->{'extra'}->{'item_number'}) . '. '));
      } elsif ($root->{'parent'}->{'args'}
          and $root->{'parent'}->{'args'}->[0]) {
        # this is the text prepended to items.
        
        $result = _convert($self, $root->{'parent'}->{'args'}->[0]);
        $result .= _convert($self, { 'text' => ' ' });
      }
      $result .= _count_added($self, $line->{'container'}, 
                      Texinfo::Convert::Paragraph::end($line->{'container'}));
      pop @{$self->{'formatters'}};
      $self->{'text_element_context'}->[-1]->{'counter'} += 
         Texinfo::Convert::Unicode::string_width($result);
      $self->{'empty_lines_count'} = 0 unless ($result eq '');
    # open a multitable cell
    } elsif ($command eq 'headitem' or $command eq 'item'
             or $command eq 'tab') {
      my $cell_width = $self->{'format_context'}->[-1]->{'columns_size'}->[$root->{'extra'}->{'cell_number'}-1];
      $self->{'format_context'}->[-1]->{'item_command'} = $command
        if ($command ne 'tab');
      die if (!defined($cell_width));
      $self->{'empty_lines_count'} 
         = $self->{'format_context'}->[-1]->{'row_empty_lines_count'};

      push @{$self->{'format_context'}},
           { 'cmdname' => $command,
             'paragraph_count' => 0,
             'indent_level' => 0 };
      push @{$self->{'text_element_context'}}, {'max' => $cell_width - 2 };
      push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0,
                                                   'locations' => []};
      $cell = 1;
    } elsif ($command eq 'center') {
      #my ($counts, $new_locations);
      push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0, 
                                                   'locations' => []};
      $result = $self->convert_line (
                       {'type' => 'frenchspacing',
                        'contents' => $root->{'args'}->[0]->{'contents'}},
                       {'indent_length' => 0});
      if ($result ne '') {
        $result = $self->ensure_end_of_line($result);

        $result = $self->_align_environment ($result, 
             $self->{'text_element_context'}->[-1]->{'max'}, 'center'); 
        $self->{'empty_lines_count'} = 0;
      } else {
        # it has to be done here, as it is done in _align_environment above
        pop @{$self->{'count_context'}};
      }
      $self->{'format_context'}->[-1]->{'paragraph_count'}++;
      return $result;
    } elsif ($command eq 'exdent') {
      if ($self->{'preformatted_context_commands'}->{$self->{'context'}->[-1]}) {
        $result = $self->convert_unfilled({'contents' => $root->{'args'}->[0]->{'contents'}},
         {'indent_level'
          => $self->{'format_context'}->[-1]->{'indent_level'} -1});
      } else {
        $result = $self->convert_line({'contents' => $root->{'args'}->[0]->{'contents'}},
         {'indent_level' 
          => $self->{'format_context'}->[-1]->{'indent_level'} -1});
      }
      if ($result ne '') {
        $result = $self->ensure_end_of_line($result);
        $self->{'empty_lines_count'} = 0;
      }
      return $result;
    } elsif ($command eq 'verbatiminclude') {
      my $expansion = $self->Texinfo::Common::expand_verbatiminclude($root);
      unshift @{$self->{'current_contents'}->[-1]}, $expansion
        if ($expansion);
      return '';
    } elsif ($command eq 'insertcopying') {
      if ($self->{'extra'} and $self->{'extra'}->{'copying'}) {
        unshift @{$self->{'current_contents'}->[-1]}, 
           {'contents' => $self->{'extra'}->{'copying'}->{'contents'}};
      }
      return '';
    } elsif ($command eq 'printindex') {
      $result = $self->_printindex($root);
      return $result;
    } elsif ($command eq 'listoffloats') {
      my $lines_count = 0;
      if ($root->{'extra'} and $root->{'extra'}->{'type'}
          and defined($root->{'extra'}->{'type'}->{'normalized'}) 
          and $self->{'floats'} 
          and $self->{'floats'}->{$root->{'extra'}->{'type'}->{'normalized'}}
          and @{$self->{'floats'}->{$root->{'extra'}->{'type'}->{'normalized'}}}) {
        push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0};
        if (!$self->{'empty_lines_count'}) {
          $result .= "\n";
          $lines_count++;
        }
        $result .= "* Menu:\n\n";
        $lines_count += 2;
        foreach my $float (@{$self->{'floats'}->{$root->{'extra'}->{'type'}->{'normalized'}}}) {
          next if !$float->{'args'} or !$float->{'args'}->[1]
                   or !@{$float->{'args'}->[1]->{'contents'}};
          my $float_label_text = $self->convert_line({'type' => '_code',
             'contents' => $float->{'args'}->[1]->{'contents'}});
          my $float_entry = $self->_float_type_number($float);
          my $float_entry_text = ':';
          if (defined($float_entry)) {
            $float_entry->{'type'} = 'frenchspacing';
            $float_entry_text = $self->convert_line($float_entry);
          }
          # no translation here, this is required Info format.
          my $float_line = "* $float_entry_text: $float_label_text.";
          my $line_width 
             = Texinfo::Convert::Unicode::string_width($float_line);
          if ($line_width > $listoffloat_entry_length) {
            $float_line .= "\n" . ' ' x $listoffloat_entry_length;
            $lines_count++;
          } else {
            $float_line .= ' ' x ($listoffloat_entry_length - $line_width);
          }
          $line_width = $listoffloat_entry_length;
          my $caption;
          if ($float->{'extra'}->{'shortcaption'}) {
            $caption = $float->{'extra'}->{'shortcaption'};
          } elsif ($float->{'extra'}->{'caption'}) {
            $caption = $float->{'extra'}->{'caption'};
          }
          if ($caption) {
            $self->{'multiple_pass'} = 1;
            push @{$self->{'context'}}, 'listoffloats';
            my $tree = {'contents' => $caption->{'args'}->[0]->{'contents'}};
            # the following does nothing since there are paragraphs within
            # the shortcaption.
            #if ($caption->{'cmdname'} eq 'shortcaption') {
            #  $tree->{'type'} = 'frenchspacing';
            #}
            my $caption_text = _convert($self, $tree);
            my $old_context = pop @{$self->{'context'}};
            delete $self->{'multiple_pass'};
            die if ($old_context ne 'listoffloats');
            while ($caption_text =~ s/^\s*(\p{Unicode::EastAsianWidth::InFullwidth}\s*|\S+\s*)//) {
              my $new_word = $1;
              $new_word =~ s/\n//g;
              if ((Texinfo::Convert::Unicode::string_width($new_word) +
                   $line_width) > 
                       ($self->{'text_element_context'}->[-1]->{'max'} - 3)) {
                $float_line .= $listoffloat_append;
                last;
              } else {
                $float_line .= $new_word;
                $line_width += 
                  Texinfo::Convert::Unicode::string_width($new_word);
              }
            }
          }
          $result .= $float_line. "\n";
          $lines_count++;
        }
        $result .= "\n";
        $lines_count++;
        $self->{'empty_lines_count'} = 1;
        pop @{$self->{'count_context'}};
      }
      $self->{'format_context'}->[-1]->{'paragraph_count'}++;
      _add_text_count($self, $result);
      _add_lines_count($self, $lines_count);
      return $result;
    } elsif ($command eq 'sp') {
      if ($root->{'extra'}->{'misc_args'}->[0]) {
        $result = _count_added($self, $formatter->{'container'},
                              add_pending_word($formatter->{'container'}));
        # this useless copy avoids perl changing the type to integer!
        my $sp_nr = $root->{'extra'}->{'misc_args'}->[0];
        for (my $i = 0; $i < $sp_nr; $i++) {
          $result .= _count_added($self, $formatter->{'container'},
                end_line($formatter->{'container'}));
        }
        
        $self->{'empty_lines_count'} += $sp_nr;
        delete $self->{'text_element_context'}->[-1]->{'counter'};
      }
      return $result;
    } elsif ($command eq 'contents') {
      if ($self->{'structuring'}
            and $self->{'structuring'}->{'sectioning_root'}) {
        my $lines_count;
        ($result, $lines_count) 
            = $self->_contents($self->{'structuring'}->{'sectioning_root'}, 
                              'contents');
        _add_lines_count($self, $lines_count);
        _add_text_count($self, $result);
      }
      return $result;
    } elsif ($command eq 'shortcontents' 
               or $command eq 'summarycontents') {
      if ($self->{'structuring'}
            and $self->{'structuring'}->{'sectioning_root'}) {
        my $lines_count;
        ($result, $lines_count) 
              = $self->_contents($self->{'structuring'}->{'sectioning_root'}, 
                              'shortcontents');
        _add_lines_count($self, $lines_count);
        _add_text_count($self, $result);
      }
      return $result;
    # all the @-commands that have an information for the formatting, like
    # @paragraphindent, @frenchspacing...
    } elsif ($informative_commands{$command}) {
      $self->_informative_command($root);
      return '';
    } else {
      $unknown_command = 1;
    }
    if ($unknown_command
        and !($root->{'extra'} 
                and ($root->{'extra'}->{'index_entry'}
                     or $root->{'extra'}->{'seeentry'}
                     or $root->{'extra'}->{'seealso'}))
        # commands like def*x are not processed above, since only the def_line
        # associated is processed. If they have no name and no category they 
        # are not considered as index entries either so they have a specific
        # condition
        and !($def_commands{$command} 
              and $command =~ /x$/)) {
      warn "Unhandled $command\n";
      $result .= "!!!!!!!!! Unhandled $command !!!!!!!!!\n";
    }
  }

  # open 'type' constructs.
  my $paragraph;
  if ($root->{'type'}) {
    if ($root->{'type'} eq 'paragraph') {
      $self->{'empty_lines_count'} = 0;
      my $conf;
      # indent. Not first paragraph.
      if ($self->{'format_context'}->[-1]->{'cmdname'} eq '_top_format'
          and $self->get_conf('paragraphindent') ne 'asis' 
          and $self->get_conf('paragraphindent')
          and (($root->{'extra'} and $root->{'extra'}->{'indent'})
             or (!($root->{'extra'} and $root->{'extra'}->{'noindent'})
                and ($self->{'format_context'}->[-1]->{'paragraph_count'} 
                  or $self->get_conf('firstparagraphindent') eq 'insert') 
               and !$self->{'text_element_context'}->[-1]->{'counter'}))) {
        $conf->{'first_indent_length'} = $self->get_conf('paragraphindent');
        $conf->{'first_indent_length'} = 0
          if ($conf->{'first_indent_length'} eq 'none');
      }
      $paragraph = $self->new_formatter('paragraph', $conf);
      push @{$self->{'formatters'}}, $paragraph;
      $self->{'format_context'}->[-1]->{'paragraph_count'}++;
      if ($self->{'context'}->[-1] eq 'flushright') {
        push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0,
                                                   'locations' => []};
      }
    } elsif ($root->{'type'} eq 'preformatted'
             or $root->{'type'} eq 'rawpreformatted') {
      # if in a description reuse the main menu unfilled, to keep things
      # simpler and avoid having to do a separate count.
      if ($root->{'type'} eq 'rawpreformatted'
          or !$root->{'parent'}->{'type'}
          or $root->{'parent'}->{'type'} ne 'menu_entry_description') {
        $preformatted = $self->new_formatter('unfilled');
        push @{$self->{'formatters'}}, $preformatted;
        if ($self->{'context'}->[-1] eq 'flushright') {
          push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0,
                                                     'locations' => []};
        }
      }
    } elsif ($root->{'type'} eq 'def_line') {
      if ($root->{'extra'} and $root->{'extra'}->{'def_parsed_hash'}
             and %{$root->{'extra'}->{'def_parsed_hash'}}) {
        my $arguments = Texinfo::Common::definition_arguments_content($root);
        my $tree;
        my $command;
        if ($Texinfo::Common::def_aliases{$root->{'extra'}->{'def_command'}}) {
          $command = $Texinfo::Common::def_aliases{$root->{'extra'}->{'def_command'}};
        } else {
          $command = $root->{'extra'}->{'def_command'};
        }
        my $name;
        if ($root->{'extra'}->{'def_parsed_hash'}->{'name'}) {
          $name = $root->{'extra'}->{'def_parsed_hash'}->{'name'};
        } else {
          $name = '';
        }
        
        if ($command eq 'deffn'
            or $command eq 'defvr'
            or $command eq 'deftp'
            or (($command eq 'deftypefn'
                 or $command eq 'deftypevr')
                and !$root->{'extra'}->{'def_parsed_hash'}->{'type'})) {
          if ($arguments) {
            $tree = $self->gdt("\@tie{}-- {category}: {name} {arguments}", {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'name' => $name,
                    'arguments' => $arguments});
          } else {
            $tree = $self->gdt("\@tie{}-- {category}: {name}", {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'name' => $name});
          }
        } elsif ($command eq 'deftypefn'
                 or $command eq 'deftypevr') {
          if ($arguments) {
            my $strings = {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'name' => $name,
                    'type' => $root->{'extra'}->{'def_parsed_hash'}->{'type'},
                    'arguments' => $arguments};
            if ($self->get_conf('deftypefnnewline') eq 'on') {
              $tree = $self->gdt("\@tie{}-- {category}:\@*{type}\@*{name} {arguments}",
                                 $strings);
            } else {
              $tree = $self->gdt("\@tie{}-- {category}: {type} {name} {arguments}",
                                 $strings);
            }
          } else {
            my $strings = {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'type' => $root->{'extra'}->{'def_parsed_hash'}->{'type'},
                    'name' => $name};
            if ($self->get_conf('deftypefnnewline') eq 'on') {
              $tree = $self->gdt("\@tie{}-- {category}:\@*{type}\@*{name}",
                                 $strings);
            } else {
              $tree = $self->gdt("\@tie{}-- {category}: {type} {name}",
                                 $strings);
            }
          }
        } elsif ($command eq 'defcv'
                 or ($command eq 'deftypecv'
                     and !$root->{'extra'}->{'def_parsed_hash'}->{'type'})) {
          if ($arguments) {
            $tree = $self->gdt("\@tie{}-- {category} of {class}: {name} {arguments}", {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'name' => $name,
                    'class' => $root->{'extra'}->{'def_parsed_hash'}->{'class'},
                    'arguments' => $arguments});
          } else {
            $tree = $self->gdt("\@tie{}-- {category} of {class}: {name}", {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'class' => $root->{'extra'}->{'def_parsed_hash'}->{'class'},
                    'name' => $name});
          }
        } elsif ($command eq 'defop'
                 or ($command eq 'deftypeop'
                     and !$root->{'extra'}->{'def_parsed_hash'}->{'type'})) {
          if ($arguments) {
            $tree = $self->gdt("\@tie{}-- {category} on {class}: {name} {arguments}", {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'name' => $name,
                    'class' => $root->{'extra'}->{'def_parsed_hash'}->{'class'},
                    'arguments' => $arguments});
          } else {
            $tree = $self->gdt("\@tie{}-- {category} on {class}: {name}", {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'class' => $root->{'extra'}->{'def_parsed_hash'}->{'class'},
                    'name' => $name});
          }
        } elsif ($command eq 'deftypeop') {
          if ($arguments) {
            my $strings = {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'name' => $name,
                    'class' => $root->{'extra'}->{'def_parsed_hash'}->{'class'},
                    'type' => $root->{'extra'}->{'def_parsed_hash'}->{'type'},
                    'arguments' => $arguments};
            if ($self->get_conf('deftypefnnewline') eq 'on') {
              $tree 
                = $self->gdt("\@tie{}-- {category} on {class}:\@*{type}\@*{name} {arguments}",
                             $strings);
            } else {
              $tree 
                = $self->gdt("\@tie{}-- {category} on {class}: {type} {name} {arguments}",
                             $strings);
            }
          } else {
            my $strings = {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'type' => $root->{'extra'}->{'def_parsed_hash'}->{'type'},
                    'class' => $root->{'extra'}->{'def_parsed_hash'}->{'class'},
                    'name' => $name};
            if ($self->get_conf('deftypefnnewline') eq 'on') {
              $tree 
                = $self->gdt("\@tie{}-- {category} on {class}:\@*{type}\@*{name}",
                             $strings);
            } else {
              $tree 
                = $self->gdt("\@tie{}-- {category} on {class}: {type} {name}",
                             $strings);
            }
          }
        } elsif ($command eq 'deftypecv') {
          if ($arguments) {
            my $strings = {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'name' => $name,
                    'class' => $root->{'extra'}->{'def_parsed_hash'}->{'class'},
                    'type' => $root->{'extra'}->{'def_parsed_hash'}->{'type'},
                    'arguments' => $arguments};
            if ($self->get_conf('deftypefnnewline') eq 'on') {
              $tree 
                = $self->gdt("\@tie{}-- {category} of {class}:\@*{type}\@*{name} {arguments}",
                             $strings);
            } else {
              $tree 
                = $self->gdt("\@tie{}-- {category} of {class}: {type} {name} {arguments}",
                             $strings);
            }
          } else {
            my $strings = {
                    'category' => $root->{'extra'}->{'def_parsed_hash'}->{'category'},
                    'type' => $root->{'extra'}->{'def_parsed_hash'}->{'type'},
                    'class' => $root->{'extra'}->{'def_parsed_hash'}->{'class'},
                    'name' => $name};
            if ($self->get_conf('deftypefnnewline') eq 'on') {
              $tree 
                = $self->gdt("\@tie{}-- {category} of {class}:\@*{type}\@*{name}",
                             $strings);
            } else {
              $tree 
                = $self->gdt("\@tie{}-- {category} of {class}: {type} {name}",
                             $strings);
            }
          }
        }

        my $def_paragraph = $self->new_formatter('paragraph', 
         { 'indent_length' => ($self->{'format_context'}->[-1]->{'indent_level'} -1) *$indent_length,
           'indent_length_next' => (1+$self->{'format_context'}->[-1]->{'indent_level'})*$indent_length,
           'suppress_styles' => 1
         });
        push @{$self->{'formatters'}}, $def_paragraph;

        $result .= _convert($self, {'type' => '_code', 'contents' => [$tree]});
        $result .= _count_added($self, $def_paragraph->{'container'},
              Texinfo::Convert::Paragraph::end($def_paragraph->{'container'}));

        pop @{$self->{'formatters'}};
        delete $self->{'text_element_context'}->[-1]->{'counter'};
        $self->{'empty_lines_count'} = 0;
      }
    } elsif ($root->{'type'} eq 'menu_entry') {
      my $entry_name_seen = 0;
      foreach my $arg (@{$root->{'args'}}) {
        my ($pre_quote, $post_quote);
        if ($arg->{'type'} eq 'menu_entry_node') {
          $self->{'formatters'}->[-1]->{'suppress_styles'} = 1;

          # Flush a leading space
          $result .= _count_added($self, $formatter->{'container'},
                           add_pending_word($formatter->{'container'}, 1));

          my $node_text = _convert($self, {'type' => '_code',
                                      'contents' => $arg->{'contents'}});

          $node_text .= _count_added($self, $formatter->{'container'},
                           add_pending_word($formatter->{'container'}, 1));
          delete $self->{'formatters'}->[-1]->{'suppress_styles'};
          $pre_quote = $post_quote = '';
          if ($entry_name_seen) {
            if ($node_text =~ /([,\t]|\.\s)/) {
              if ($self->get_conf('INFO_SPECIAL_CHARS_WARNING')) {
                $self->line_warn(sprintf(__(
                   "menu entry node name should not contain `%s'"), $1),
                               $root->{'line_nr'});
              }
            }
            if ($self->get_conf('INFO_SPECIAL_CHARS_QUOTE')) {
              $pre_quote = $post_quote = "\x{7f}";
            }
          } else {
            if ($node_text =~ /:/) {
              if ($self->get_conf('INFO_SPECIAL_CHARS_WARNING')) {
                $self->line_warn(__(
                 "menu entry node name should not contain `:'"),
                               $root->{'line_nr'});
              }
              if ($self->get_conf('INFO_SPECIAL_CHARS_QUOTE')) {
                $pre_quote = $post_quote = "\x{7f}";
              }
            }
          }
          $result .= $pre_quote . $node_text . $post_quote;
        } elsif ($arg->{'type'} eq 'menu_entry_name') {
          my $entry_name = _convert($self, $arg);
          my $formatter = $self->{'formatters'}->[-1];
          $entry_name .= _count_added($self,
                           $formatter->{'container'},
                           add_pending_word($formatter->{'container'}, 1));
          $entry_name_seen = 1;
          $pre_quote = $post_quote = '';
          if ($entry_name =~ /:/) {
            if ($self->get_conf('INFO_SPECIAL_CHARS_WARNING')) {
              $self->line_warn(__(
                 "menu entry name should not contain `:'"),
                               $root->{'line_nr'});
            }
            if ($self->get_conf('INFO_SPECIAL_CHARS_QUOTE')) {
              $pre_quote = $post_quote = "\x{7f}";
            }
          }
          $result .= $pre_quote . $entry_name . $post_quote;
        } else {
          $result .= _convert($self, $arg);
        }
      }

      # If we are nested inside an @example, a 'menu_entry_description' may not 
      # have been processed yet, and we need to output any pending spaces 
      # before 'end_line' throws them away.  The argument to 'add_pending_word' 
      # does this.
      if ($root->{'parent'}->{'type'} 
              and $root->{'parent'}->{'type'} eq 'preformatted') {
        $result .= _count_added($self,
                         $formatter->{'container'},
                         add_pending_word($formatter->{'container'}, 1));
      } else {
        $result .= _count_added($self,
                         $formatter->{'container'},
                         add_pending_word($formatter->{'container'}));
        end_line($formatter->{'container'});
        $result = $self->ensure_end_of_line($result) ;
      }

    } elsif ($root->{'type'} eq 'frenchspacing') {
      push @{$formatter->{'frenchspacing_stack'}}, 'on';
      set_space_protection($formatter->{'container'}, undef, undef, undef, 1);
    } elsif ($root->{'type'} eq '_code') {
      if (!$formatter->{'font_type_stack'}->[-1]->{'monospace'}) {
        push @{$formatter->{'font_type_stack'}}, {'monospace' => 1};
      } else {
        $formatter->{'font_type_stack'}->[-1]->{'monospace'}++;
      }
      push @{$formatter->{'frenchspacing_stack'}}, 'on';
      set_space_protection($formatter->{'container'}, undef, undef, undef, 1);
    } elsif ($root->{'type'} eq 'bracketed') {
      $result .= _count_added($self, $formatter->{'container'}, 
                   add_text($formatter->{'container'}, '{'));
    }
  }

  # The processing of contents is done here.
  if ($root->{'contents'}) {
    my @contents = @{$root->{'contents'}};
    push @{$self->{'current_contents'}}, \@contents;
    push @{$self->{'current_roots'}}, $root;
    while (@contents) {
      my $content = shift @contents;
      my $text = _convert($self, $content);
      $self->{'empty_lines_count'} = 0 
        if ($preformatted and $text =~ /\S/);
      $result .= $text;
    }
    pop @{$self->{'current_contents'}};
    pop @{$self->{'current_roots'}};
  }

  # now closing. First, close types.
  if ($root->{'type'}) {
    if ($root->{'type'} eq 'frenchspacing') {
      pop @{$formatter->{'frenchspacing_stack'}};
      my $frenchspacing = 0;
      $frenchspacing = 1 if ($formatter->{'frenchspacing_stack'}->[-1] eq 'on');
      set_space_protection($formatter->{'container'}, undef,
                           undef, undef, $frenchspacing);
    } elsif ($root->{'type'} eq '_code') {
      $formatter->{'font_type_stack'}->[-1]->{'monospace'}--;
      pop @{$formatter->{'font_type_stack'}}
        if !$formatter->{'font_type_stack'}->[-1]->{'monospace'};
      pop @{$formatter->{'frenchspacing_stack'}};
      my $frenchspacing = 0;
      $frenchspacing = 1 if ($formatter->{'frenchspacing_stack'}->[-1] eq 'on');
      set_space_protection($formatter->{'container'}, undef,
                           undef, undef, $frenchspacing);
    } elsif ($root->{'type'} eq 'bracketed') {
      $result .= _count_added($self, $formatter->{'container'}, 
                                     add_text($formatter->{'container'}, '}'));
    } elsif ($root->{'type'} eq 'row') {
      my @cell_beginnings;
      my @cell_lines;
      my $cell_beginning = 0;
      my $cell_idx = 0;
      my $max_lines = 0;
      my $indent_len = $indent_length 
             * $self->{'format_context'}->[-1]->{'indent_level'};
      foreach my $cell (@{$self->{'format_context'}->[-1]->{'row'}}) {
        $cell_beginnings[$cell_idx] = $cell_beginning;
        my $cell_width = $self->{'format_context'}->[-1]->{'columns_size'}->[$cell_idx];
        $cell_beginning += $cell_width +1;
        $cell_lines[$cell_idx] = [ split /^/, $cell ];
        $max_lines = scalar(@{$cell_lines[$cell_idx]}) 
          if (scalar(@{$cell_lines[$cell_idx]}) > $max_lines);
        $cell_idx++;
      }

      $cell_idx = 0;
      my $cell_updated_locations;
      my @row_locations;
      foreach my $cell_locations (@{$self->{'format_context'}->[-1]->{'row_counts'}}) {
        foreach my $location (@{$cell_locations->{'locations'}}) {
          next unless (defined($location->{'bytes'}) and defined($location->{'lines'}));
          push @{$cell_updated_locations->[$cell_idx]->{$location->{'lines'}}},
                 $location;
          $max_lines = $location->{'lines'}+1 
                            if ($location->{'lines'}+1 > $max_lines);
        }
        push @row_locations, @{$cell_locations->{'locations'}};
        $cell_idx++;
      }

      # this is used to keep track of the last cell with content.
      my $max_cell = scalar(@{$self->{'format_context'}->[-1]->{'row'}});
      my $bytes_count = 0;
      my $line;
      for (my $line_idx = 0; $line_idx < $max_lines; $line_idx++) {
        my $line_width = $indent_len;
        $line = '';
        # determine the last cell in the line, to fill spaces in 
        # cells preceding that cell on the line
        my $last_cell = 0;
        for (my $cell_idx = 0; $cell_idx < $max_cell; $cell_idx++) {
          $last_cell = $cell_idx+1 if (defined($cell_lines[$cell_idx]->[$line_idx])
                                       or defined($cell_updated_locations->[$cell_idx]->{$line_idx}));
        }

        for (my $cell_idx = 0; $cell_idx < $last_cell; $cell_idx++) {
          my $cell_text = $cell_lines[$cell_idx]->[$line_idx];
          if (defined($cell_text)) {
            chomp($cell_text);
            if ($line eq '' and $cell_text ne '') {
              $line = ' ' x $indent_len;
              $bytes_count += count_bytes($self, $line);
            }
            $line .= $cell_text;
            $bytes_count += count_bytes($self, $cell_text);
            $line_width += Texinfo::Convert::Unicode::string_width($cell_text);
          }
          if (defined($cell_updated_locations->[$cell_idx]->{$line_idx})) {
            foreach my $location (@{$cell_updated_locations->[$cell_idx]->{$line_idx}}) {
              $location->{'bytes'} = $bytes_count;
            }
          }
          if ($cell_idx+1 < $last_cell) {
            if ($line_width < $indent_len + $cell_beginnings[$cell_idx+1]) {
              if ($line eq '') {
                $line = ' ' x $indent_len;
                $bytes_count += count_bytes($self, $line);
              }
              my $spaces = ' ' x ($indent_len + $cell_beginnings[$cell_idx+1] - $line_width);
              $line_width += Texinfo::Convert::Unicode::string_width($spaces);
              $line .= $spaces;
              $bytes_count += count_bytes($self, $spaces);
            }
          }
        }
        $line .= "\n";
        $bytes_count += count_bytes($self, "\n");
        $result .= $line;
      }
      if ($self->{'format_context'}->[-1]->{'item_command'} eq 'headitem') {
        # at this point cell_beginning is at the beginning of
        # the cell following the end of the table -> full width
        my $line = ' ' x $indent_len . '-' x $cell_beginning . "\n";
        $bytes_count += count_bytes($self, $line);
        $result .= $line;
        $self->{'empty_lines_count'} = 0;
        $max_lines++;
      # there may be empty lines, in that case $line is undef, $max_lines == 0
      } elsif ($max_lines) {
        if ($line eq "\n") {
          $self->{'empty_lines_count'} = 1;
        } else {
          $self->{'empty_lines_count'} = 0;
        }
      }
      $self->_update_locations_counts(\@row_locations);
      push @{$self->{'count_context'}->[-1]->{'locations'}}, @row_locations;
      $self->{'count_context'}->[-1]->{'bytes'} += $bytes_count;
      $self->{'count_context'}->[-1]->{'lines'} += $max_lines;
      $self->{'format_context'}->[-1]->{'row'} = [];
      $self->{'format_context'}->[-1]->{'row_counts'} = [];
      $self->{'format_context'}->[-1]->{'row_empty_lines_count'} 
        = $self->{'empty_lines_count'};
    } elsif ($root->{'type'} eq 'text_root') {
      $self->{'text_before_first_node'} = $result;
    }
  }
  # close paragraphs and preformatted
  if ($paragraph) {
    $result .= _count_added($self, $paragraph->{'container'},
               Texinfo::Convert::Paragraph::end($paragraph->{'container'}));
    if ($self->{'context'}->[-1] eq 'flushright') {
      $result = $self->_align_environment($result, 
        $self->{'text_element_context'}->[-1]->{'max'}, 'right');
    }
    pop @{$self->{'formatters'}};
    delete $self->{'text_element_context'}->[-1]->{'counter'};
  } elsif ($preformatted) {
    $result .= _count_added($self, $preformatted->{'container'},
               Texinfo::Convert::Paragraph::end($preformatted->{'container'}));
    if ($result ne '') {
      $result = $self->ensure_end_of_line($result);
    }
    if ($self->{'context'}->[-1] eq 'flushright') {
      $result = $self->_align_environment ($result, 
        $self->{'text_element_context'}->[-1]->{'max'}, 'right');
    }
    pop @{$self->{'formatters'}};
    # We assume that, upon closing the preformatted we are at the 
    # beginning of a line.
    delete $self->{'text_element_context'}->[-1]->{'counter'};
  }

  # close commands
  if ($command) {
    if ($command eq 'float') {
      if ($root->{'extra'}
          and ($root->{'extra'}->{'type'}->{'normalized'} ne '' 
               or defined($root->{'number'})
               or $root->{'extra'}->{'caption'} or $root->{'extra'}->{'shortcaption'})) {
        
        $result .= _add_newline_if_needed($self);
        my ($caption, $prepended) = Texinfo::Common::float_name_caption($self,
                                                                        $root);
        if ($prepended) {
          $prepended->{'type'} = 'frenchspacing';
          my $float_number = $self->convert_line ($prepended);
          $result .= $float_number;
          $self->{'text_element_context'}->[-1]->{'counter'} += 
            Texinfo::Convert::Unicode::string_width($float_number);
          $self->{'empty_lines_count'} = 0;
        }
        if ($caption) {
          $self->{'format_context'}->[-1]->{'paragraph_count'} = 0;
          my $tree = $caption->{'args'}->[0];
          $result .= _convert($self, $tree);
        }
      }
    } elsif (($command eq 'quotation' 
               or $command eq 'smallquotation')
             and $root->{'extra'} and $root->{'extra'}->{'authors'}) {
      foreach my $author (@{$root->{'extra'}->{'authors'}}) {
        $result .= _convert($self, 
                 $self->gdt("\@center --- \@emph{{author}}\n",
                    {'author' => $author->{'args'}->[0]->{'contents'}}));
      }
    } elsif (($command eq 'multitable')) {
      $self->{'document_context'}->[-1]->{'in_multitable'}--;
    } elsif ($root_commands{$command}
        and $sectioning_commands{$command}
        and $command ne 'part') {
      # add menu if missing
      my $node = $self->{'node'};
      my $automatic_directions;
      if ($node and defined($node->{'extra'}->{'nodes_manuals'})) {
        $automatic_directions =
            (scalar(@{$node->{'extra'}->{'nodes_manuals'}}) == 1);
      }
      if ($node and $automatic_directions
            and !$self->{'seenmenus'}->{$node}) {
        $self->{'seenmenus'}->{$node} = 1;
        my $menu_node = Texinfo::Structuring::new_complete_node_menu(undef, $node);
        if ($menu_node) {
          my $menu_text = $self->_convert($menu_node);
          if ($menu_text) {
            $result .= $menu_text;
          }
        }
      }
    }
 
    # close the contexts and register the cells
    if ($self->{'preformatted_context_commands'}->{$command}
        or $command eq 'float') {
      my $old_context = pop @{$self->{'context'}};
      die "Not a preformatted context: $old_context"
        if (!$self->{'preformatted_context_commands'}->{$old_context}
            and $old_context ne 'float');
      if ($old_context ne 'float' and !$menu_commands{$old_context}) {
        $self->{'empty_lines_count'} = 0;
      }
      delete ($self->{'preformatted_context_commands'}->{$command})
       unless ($default_preformatted_context_commands{$command});
    } elsif ($flush_commands{$command}) {
      my $old_context = pop @{$self->{'context'}};
      die if (! $flush_commands{$old_context});
    }

    if ($self->{'format_context_commands'}->{$command}) {
      pop @{$self->{'format_context'}};
      delete ($self->{'format_context_commands'}->{$command})
       unless ($default_format_context_commands{$command});
    } elsif ($cell) {
      pop @{$self->{'format_context'}};
      pop @{$self->{'text_element_context'}};
      push @{$self->{'format_context'}->[-1]->{'row'}}, $result;
      _update_count_context($self);
      my $cell_counts = pop @{$self->{'count_context'}};
      push @{$self->{'format_context'}->[-1]->{'row_counts'}}, $cell_counts;
      $result = '';
    }
    if ($advance_paragraph_count_commands{$command}) {
      $self->{'format_context'}->[-1]->{'paragraph_count'}++;
    }
  }

  return $result;
}

1;

__END__
# $Id$
# Automatically generated from maintain/template.pod

=head1 NAME

Texinfo::Convert::Plaintext - Convert Texinfo tree to Plaintext

=head1 SYNOPSIS

  my $converter 
    = Texinfo::Convert::Plaintext->converter({'parser' => $parser});

  $converter->output($tree);
  $converter->convert($tree);
  $converter->convert_tree($tree);

=head1 DESCRIPTION

Texinfo::Convert::Plaintext converts a Texinfo tree to Plaintext.

=head1 METHODS

=over

=item $converter = Texinfo::Convert::Plaintext->converter($options)

Initialize converter from Texinfo to Plaintext.  

The I<$options> hash reference holds options for the converter.  In
this option hash reference a parser object may be associated with the 
I<parser> key.  The other options should be configuration options
described in the Texinfo manual.  Those options, when appropriate,
override the document content.

See L<Texinfo::Convert::Converter> for more informations.

=item $converter->output($tree)

Convert a Texinfo tree I<$tree> and output the result in files as
described in the Texinfo manual.

=item $result = $converter->convert($tree)

Convert a Texinfo tree I<$tree> or tree portion and return 
the resulting output.

=item $result = $converter->convert_tree($tree)

Convert a Texinfo tree portion I<$tree> and return the resulting 
output.  This function does not try to output a full document but only
portions.  For a full document use C<convert>.

=back

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=cut
