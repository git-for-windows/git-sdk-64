# Common.pm: definition of commands. Common code of other Texinfo modules.
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
# Parts (also from Patrice Dumas) come from texi2html.pl or texi2html.init.

package Texinfo::Common;

use strict;

# for unicode/layer support in binmode
use 5.006;

# to determine the null file
use Config;
use File::Spec;

use Texinfo::Documentlanguages;

# debugging
use Carp qw(cluck);

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

%EXPORT_TAGS = ( 'all' => [ qw(
debug_hash
debug_list
definition_category
expand_verbatiminclude
expand_today
float_name_caption
is_content_empty
move_index_entries_after_items_in_tree
normalize_top_node_name
numbered_heading
protect_comma_in_tree
protect_first_parenthesis
protect_hashchar_at_line_beginning
protect_colon_in_tree
protect_node_after_label_in_tree
trim_spaces_comment_from_content
valid_tree_transformation
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
__ __p print_tree
);

$VERSION = '6.8';

# i18n
sub N__($)
{
  return $_[0];
}

# determine the null devices
my $default_null_device = File::Spec->devnull();
our %null_device_file = (
  $default_null_device => 1
);
# special case, djgpp recognizes both null devices
if ($Config{osname} eq 'dos' and $Config{osvers} eq 'djgpp') {
  $null_device_file{'/dev/null'} = 1;
  $null_device_file{'NUL'} = 1;
}

use Locale::Messages;

my $messages_textdomain = 'texinfo';

sub __($) {
  my $msgid = shift;
  return Locale::Messages::dgettext($messages_textdomain, $msgid);
}

sub __p($$) {
  my $context = shift;
  my $msgid = shift;
  return Locale::Messages::dpgettext($messages_textdomain, $context, $msgid);
}

# these are the default values for the parser state that may be 
# initialized to values given by the user.
# They are defined here, because they are used below and we 
# don't want Texinfo::Common to use Texinfo::Parser.
our %default_parser_state_configuration = (
  'expanded_formats' => [],
  'include_directories' => [ '.' ],
  # these are the user-added indices.  May be an array reference on names
  # or an hash reference in the same format than %index_names below
  'indices' => [],
  # the following are dynamically modified during the document parsing.
  'aliases' => {},            # key is a command name value is the alias
  'documentlanguage' => undef,
                              # Current documentlanguage set by 
                              # @documentlanguage
  'explained_commands' => {}, # the key is a command name, either acronym
                              # or abbr, the value is a hash.  The key hash 
                              # is a normalized first argument of the 
                              # corresponding command, the value is the 
                              # contents array of the previous command with
                              # this first arg and a second arg.
  'labels'          => {},    # keys are normalized label names, as described
                              # in the `HTML Xref' node.  Value should be
                              # a node/anchor or float in the tree.
  'targets' => [],            # array of elements used to build 'labels'
  'macros' => {},             # the key is the user-defined macro name.  The 
                              # value is the reference on a macro element 
                              # as obtained by parsing the @macro
  'merged_indices' => {},     # the key is merged in the value
  'sections_level' => 0,      # modified by raise/lowersections
  'values' => {'txicommandconditionals' => 1},
                              # the key is the name, the value the @set name 
                              # argument.  A Texinfo tree may also be used.
  'info' => {
    'novalidate' => 0,        # same as setting @novalidate.
    'input_encoding_name' => 'utf-8',
    'input_perl_encoding' => 'utf-8'
  },
  'in_gdt' => 0 # whether we are being called by gdt
);


# Customization variables obeyed by the parser, and the default values.
our %default_parser_customization_values = (
  'DEBUG' => 0,     # if >= 10, tree is printed in texi2any.pl after parsing.
                    # If >= 100 tree is printed every line.
  'FORMAT_MENU' => 'menu',           # if not 'menu' no menu error related.
  'IGNORE_BEFORE_SETFILENAME' => 1,
  'IGNORE_SPACE_AFTER_BRACED_COMMAND_NAME' => 1,
  'CPP_LINE_DIRECTIVES' => 1, # handle cpp like synchronization lines
  'MAX_MACRO_CALL_NESTING' => 100000, # max number of nested macro calls
  # This is not used directly, but passed to Convert::Text through 
  # Texinfo::Common::_convert_text_options
  'ENABLE_ENCODING' => 1,     # output accented and special characters
                              # based on @documentencoding
);

# Customization variables set in the parser for other modules, and the
# default values.
our %default_structure_customization_values = (
  # following are used in Texinfo::Structuring
  'USE_UP_NODE_FOR_ELEMENT_UP' => 0, # Use node up for Up if there is no 
                                     # section up.
  'CHECK_NORMAL_MENU_STRUCTURE' => 0, # output warnings when node with
            # automatic direction does directions in menu are not consistent
            # with sectionning, and when node directions are not consistent
            # with menu directions
);


# customization options
our %document_settable_at_commands = (
  'allowcodebreaks' => 'true',
  'clickstyle' => '@arrow',
  'codequotebacktick' => 'off',
  'codequoteundirected' => 'off',
  'contents' => 0,
  'deftypefnnewline' => 'off',
  'documentencoding' => 'us-ascii',
  'documentlanguage' => 'en',
  # is N ems in TeX, 0.4 in.
  'exampleindent' => 5,
  'firstparagraphindent' => 'none',
  'frenchspacing' => 'off',
  'headings' => 'on',
  'kbdinputstyle' => 'distinct',
  'paragraphindent' => 3,
  'shortcontents' => 0,
  'urefbreakstyle' => 'after',
  'xrefautomaticsectiontitle' => 'off',
);

# those should be unique
our %document_settable_unique_at_commands = (
  # when passed through a configuration variable, documentdescription
  # should be already formatted for HTML
  'documentdescription' => undef,
  'evenfootingmarks' => undef,
  'evenheadingmarks' => undef,
  'everyfootingmarks' => 'bottom', 
  'everyheadingmarks' => 'bottom',
  'fonttextsize' => 11, 
  'footnotestyle' => 'end', 
  'novalidate' => 0,
  'oddfootingmarks' => undef,
  'oddheadingmarks' => undef,
  # FIXME not clear here.
  'pagesizes' => undef,
  'setchapternewpage' => 'on',
  'setfilename' => undef,
  'everyheading'      => undef,
  'everyfooting'      => undef,
  'evenheading'       => undef,
  'evenfooting'       => undef,
  'oddheading'        => undef,
  'oddfooting'        => undef,
);

my @command_line_settables = (
  'CASE_INSENSITIVE_FILENAMES', 'ENABLE_ENCODING', 'ERROR_LIMIT',
  'FILLCOLUMN', 'FORCE', 'HEADERS', 'INTERNAL_LINKS', 'MACRO_EXPAND',
  'NODE_FILES', 'NO_WARN', 'NUMBER_FOOTNOTES', 'NUMBER_SECTIONS',
  'OUTFILE', 'SPLIT', 'SPLIT_SIZE', 'SUBDIR', 'TRANSLITERATE_FILE_NAMES',
  'VERBOSE'
);

# documented in the Texinfo::Parser pod section
# all are lower cased in texi2any.pl
my @parser_options = map {uc($_)} (keys(%default_parser_state_configuration));

our @variable_string_settables = (
'AFTER_ABOUT',
'AFTER_BODY_OPEN',
'AFTER_OVERVIEW',
'AFTER_TOC_LINES',
'AVOID_MENU_REDUNDANCY',
'BASEFILENAME_LENGTH',
'BEFORE_OVERVIEW',
'BEFORE_TOC_LINES',
'BIG_RULE',
'BODYTEXT',
'COPIABLE_ANCHORS',
'CHAPTER_HEADER_LEVEL',
'CHECK_HTMLXREF',
'CHECK_NORMAL_MENU_STRUCTURE',
'CLOSE_QUOTE_SYMBOL',
'COMPLEX_FORMAT_IN_TABLE',
'CONTENTS_OUTPUT_LOCATION',
'CPP_LINE_DIRECTIVES',
'CSS_LINES',
'DATE_IN_HEADER',
'DEBUG',
'DEFAULT_RULE',
'DEF_TABLE',
'DO_ABOUT',
'DOCTYPE',
'DUMP_TEXI',
'DUMP_TREE',
'ENABLE_ENCODING_USE_ENTITY',
'EXTENSION',
'EXTERNAL_CROSSREF_SPLIT',
'EXTERNAL_DIR',
'EXTRA_HEAD',
'FOOTNOTE_END_HEADER_LEVEL',
'FOOTNOTE_SEPARATE_HEADER_LEVEL',
'FRAMES',
'FRAMESET_DOCTYPE',
'HEADER_IN_TABLE',
'HTML_MATH',
'HTMLXREF',
'ICONS',
'IGNORE_BEFORE_SETFILENAME',
'IGNORE_SPACE_AFTER_BRACED_COMMAND_NAME',
'IMAGE_LINK_PREFIX',
'INDEX_ENTRY_COLON',
'INDEX_SPECIAL_CHARS_WARNING',
'INFO_JS_DIR',
'INFO_SPECIAL_CHARS_QUOTE',
'INFO_SPECIAL_CHARS_WARNING',
'INLINE_CSS_STYLE',
'JS_WEBLABELS',
'JS_WEBLABELS_FILE',
'KEEP_TOP_EXTERNAL_REF',
'L2H',
'L2H_CLEAN',
'L2H_FILE',
'L2H_HTML_VERSION',
'L2H_L2H',
'L2H_SKIP',
'L2H_TMP',
'MATHJAX_SCRIPT',
'MATHJAX_SOURCE',
'MAX_HEADER_LEVEL',
'MAX_MACRO_CALL_NESTING',
'MENU_ENTRY_COLON',
'MENU_SYMBOL',
'MONOLITHIC',
'NO_CSS',
'NODE_FILE_EXTENSION',
'NODE_FILENAMES',
'NODE_NAME_IN_INDEX',
'NODE_NAME_IN_MENU',
'NO_USE_SETFILENAME',
'OPEN_QUOTE_SYMBOL',
'OUTPUT_ENCODING_NAME',
'OUTPUT_PERL_ENCODING',
'OVERVIEW_LINK_TO_TOC',
'PACKAGE',
'PACKAGE_AND_VERSION',
'PACKAGE_NAME',
'PACKAGE_URL',
'PACKAGE_VERSION',
'PRE_ABOUT',
'PRE_BODY_CLOSE',
'PREFIX',
'PROGRAM',
'PROGRAM_NAME_IN_FOOTER',
'SECTION_NAME_IN_TITLE',
'SHORTEXTN',
'FORMAT_MENU',
'SHOW_TITLE',
'SIMPLE_MENU',
'SORT_ELEMENT_COUNT',
'SORT_ELEMENT_COUNT_WORDS',
'TEST',
'TEXI2DVI',
'TEXI2HTML',
'TEXINFO_DTD_VERSION',
'TEXINFO_OUTPUT_FORMAT',
'TEXTCONTENT_COMMENT',
'TOC_LINKS',
'TOP_FILE',
'TOP_NODE_FILE_TARGET',
'TOP_NODE_UP',
'TOP_NODE_UP_URL',
'TREE_TRANSFORMATIONS',
'USE_ACCESSKEY',
'USE_ISO',
'USE_LINKS',
'USE_NODES',
'USE_NODE_DIRECTIONS',
'USE_NUMERIC_ENTITY',
'USE_REL_REV',
'USE_SETFILENAME_EXTENSION',
'USE_TITLEPAGE_FOR_TITLE',
'USE_UNIDECODE',
'USE_UP_NODE_FOR_ELEMENT_UP',
'VERTICAL_HEAD_NAVIGATION',
'WORDS_IN_PAGE',
'XREF_USE_FLOAT_LABEL',
'XREF_USE_NODE_NAME_ARG',
);

# Not strings. 
# FIXME To be documented somewhere, but where?
my @variable_other_settables = (
  'LINKS_BUTTONS', 'TOP_BUTTONS', 'SECTION_BUTTONS', 'BUTTONS_TEXT',
  'BUTTONS_ACCESSKEY', 'BUTTONS_REL', 'BUTTONS_GOTO',
  'CHAPTER_FOOTER_BUTTONS', 'SECTION_FOOTER_BUTTONS',
  'NODE_FOOTER_BUTTONS',
  'MISC_BUTTONS', 'CHAPTER_BUTTONS', 'BUTTONS_NAME',
  'BUTTONS_EXAMPLE', 'SPECIAL_ELEMENTS_NAME', 'SPECIAL_ELEMENTS_CLASS',
  'ACTIVE_ICONS', 'PASSIVE_ICONS',
  'CSS_FILES', 'CSS_REFS', 
  'GLOBAL_COMMANDS',
);

my %valid_options;
foreach my $var (keys(%document_settable_at_commands), 
         keys(%document_settable_unique_at_commands),
         @command_line_settables, @variable_string_settables, 
         @variable_other_settables, @parser_options) {
  $valid_options{$var} = 1;
}

sub valid_option($)
{
  my $option = shift;
  return $valid_options{$option};
}

sub add_valid_option($)
{
  my $option = shift;
  if ($option =~ /^[A-Z][A-Z_]{2,}$/) {
    $valid_options{$option} = 1;
    return 1;
  }
  return 0;
}

my %customization_variable_classes = (
  'document_settable_at_commands' => [ sort(keys(%document_settable_at_commands)) ],
  'document_settable_unique_at_commands' => [ sort(keys(%document_settable_unique_at_commands)) ],
  'command_line_settables' => \@command_line_settables,
  'variable_string_settables' => \@variable_string_settables,
  'variable_other_settables' => \@variable_other_settables,
  'parser_options' => \@parser_options,
);

my %valid_tree_transformations;
foreach my $valid_transformation ('simple_menus', 
    'fill_gaps_in_sectioning', 'move_index_entries_after_items',
    'insert_nodes_for_sectioning_commands',
    'complete_tree_nodes_menus', 'regenerate_master_menu',
    'indent_menu_descriptions') {
  $valid_tree_transformations{$valid_transformation} = 1;
}

sub valid_tree_transformation ($)
{
  my $transformation = shift;
  return 1 if (defined($transformation) 
               and $valid_tree_transformations{$transformation});
  return 0;
}

our %no_brace_commands;             # commands never taking braces
%no_brace_commands = (
           '*', "\n",
           ' ', ' ',
           "\t", ' ',
           "\n", ' ',
           '-', '',  # hyphenation hint
           '|', '',  # used in formatting commands @evenfooting and friends
           '/', '',
           ':', '',
           '!', '!',
           '?', '?',
           '.', '.',
           '@', '@',
           '}', '}',
           '{', '{',
           '&', '&',
           '\\', '\\',  # should only appear in math
);


# commands taking a line as argument or no argument.
# sectioning commands and def* commands are added below.
# index commands are added dynamically.
#
# The values signification is:
# special:     no value and macro expansion, all the line is used, and 
#              analysed during parsing (_parse_special_misc_command)
# lineraw:     no value and macro expansion, the line is kept as-is, not 
#              analysed
# skipline:    no argument, everything else on the line is skipped
# text:        the line is parsed as texinfo, and the argument is converted
#              to simple text (in _end_line)
# line:        the line is parsed as texinfo
# a number:    the line is parsed as texinfo and the result should be plain 
#              text maybe followed by a comment; the result is analysed
#              during parsing (_parse_line_command_args).  
#              The number is an indication of the number of arguments of 
#              the command.
#
# Beware that @item may be a 'line' command or an 'other' command
# depending on the context.
our %line_commands = (
  'node'              => 'line', # special arg
  'bye'               => 'skipline', # no arg
  'end'               => 'text',
  # set, clear
  'set'               => 'special', # special arg
  'clear'             => 'special', # special arg
  'unmacro'           => 'special', 
  # comments
  'comment'           => 'lineraw',
  'c'                 => 'lineraw',
  # special
  'definfoenclose'    => 3,
  'alias'             => 2, 
  # number of arguments is not known in advance.
  'columnfractions'   => 1, 
  # file names
  'setfilename'       => 'text',
  'verbatiminclude'   => 'text',
  'include'           => 'text',

  'raisesections'     => 'skipline',  # no arg
  'lowersections'     => 'skipline', # no arg
  'contents'          => 'skipline', # no arg
  'shortcontents'     => 'skipline', # no arg
  'summarycontents'   => 'skipline', # no arg
  'insertcopying'     => 'skipline', # no arg
  'clickstyle'        => 'special', # arg should be an @-command
  # more relevant in preamble
  'documentencoding'  => 'text', # or 1?
  'novalidate'        => 'skipline', # no arg
  'dircategory'       => 'line', # line. Position with regard 
                                 # with direntry is significant
  'pagesizes'         => 'line', # can have 2 args 
                           # or one? 200mm,150mm 11.5in
  'finalout'          => 'skipline', # no arg
  'paragraphindent'   => 1, # arg none asis 
                       # or a number and forbids anything else on the line
  'firstparagraphindent' => 1, # none insert
  'frenchspacing'     => 1, # on off
  'codequoteundirected'       => 1, # on off
  'codequotebacktick'         => 1, # on off
  'xrefautomaticsectiontitle' => 1, # on off
  'deftypefnnewline'  => 1, # on off
  'fonttextsize'      => 1, # 10 11
  'allowcodebreaks'   => 1, # false or true
  'exampleindent'     => 1, # asis or a number
  'footnotestyle'     => 1, # end and separate, nothing else on the line
  'urefbreakstyle'    => 1, # after|before|none
  'afourpaper'        => 'skipline', # no arg
  'afivepaper'        => 'skipline', # no arg
  'afourlatex'        => 'skipline', # no arg
  'afourwide'         => 'skipline', # no arg
  'bsixpaper'         => 'skipline', # no arg
  'headings'          => 1, #off on single double singleafter doubleafter
                            # interacts with setchapternewpage
  'setchapternewpage' => 1, # off on odd

  # only relevant in TeX, and special
  'everyheading'      => 'lineraw',  # @*heading @*footing use @|
  'everyfooting'      => 'lineraw',  # + @thispage @thissectionname
  'evenheading'       => 'lineraw',  # @thissectionnum @thissection
  'evenfooting'       => 'lineraw',  # @thischaptername @thischapternum
  'oddheading'        => 'lineraw',  # @thischapter @thistitle @thisfile
  'oddfooting'        => 'lineraw',

  'smallbook'         => 'skipline', # no arg
  'syncodeindex'      => 2,   # args are index identifiers
  'synindex'          => 2,
  'defindex'          => 1, # one identifier arg
  'defcodeindex'      => 1, # one identifier arg
  'documentlanguage'  => 'text',     # language code arg
  'kbdinputstyle'     => 1,          # code example distinct
  'everyheadingmarks' => 1, # top bottom
  'everyfootingmarks' => 1,
  'evenheadingmarks'  => 1,
  'oddheadingmarks'   => 1,
  'evenfootingmarks'  => 1,
  'oddfootingmarks'   => 1,

  # formatting
  'center'            => 'line',
  'printindex'        => 1,
  'listoffloats'      => 'line',
  # especially in titlepage
#  'shorttitle'        => 'line',
  'shorttitlepage'    => 'line',
  'settitle'          => 'line',
  'author'            => 'line',
  'subtitle'          => 'line',
  'title'             => 'line',
  'sp'                => 1, # numerical arg
  'page'              => 'skipline', # no arg (pagebreak)
  'need'              => 1, # one numerical/real arg
  # formatting
  'exdent'            => 'line',
  'item'              => 'line', # or skipspace, depending on the context
  'itemx'             => 'line',
  # not valid for info (should be in @iftex)
  'vskip'             => 'lineraw', # arg line in TeX
  'subentry'          => 'line', 
);

# commands that do not take the whole line as argument
#
# skipspace:   no argument, following spaces are skipped.
# noarg:       no argument
#
our %other_commands = (
  # formatting
  'noindent'          => 'skipspace',
  'indent'            => 'skipspace',
  'headitem'          => 'skipspace',
  'item'              => 'skipspace', # or line, depending on the context
  'tab'               => 'skipspace', 
  'refill'            => 'noarg',     # obsolete
);

# only valid in heading or footing
our %in_heading_commands;
foreach my $in_heading_command ('thischapter', 'thischaptername',
  'thischapternum', 'thisfile', 'thispage', 'thistitle') {
  $in_heading_commands{$in_heading_command} = 1;

  $other_commands{$in_heading_command} = 'noarg';
}


# only valid in index entries
our %in_index_commands;
foreach my $in_index_command ('sortas', 'seeentry', 'seealso', 'subentry') {
  $in_index_commands{$in_index_command} = 1;
}


our %index_names = (
 'cp' => {'in_code' => 0},
 'fn' => {'in_code' => 1},
 'vr' => {'in_code' => 1},
 'ky' => {'in_code' => 1},
 'pg' => {'in_code' => 1},
 'tp' => {'in_code' => 1},
);

foreach my $index(keys(%index_names)) {
  $index_names{$index}->{'name'} = $index;
}

our %default_index_commands;
# all the commands are readded dynamically in the Parser.
foreach my $index_name (keys (%index_names)) {
  if ($index_name =~ /^(.).$/) {
    my $index_prefix = $1;
    # only put the one letter versions in the hash.
    $line_commands{$index_prefix.'index'} = 'line';
    $default_index_commands{$index_prefix.'index'} = 1;
  }
}

# command with braces. Value is the max number of arguments.
our %brace_commands;    

our %letter_no_arg_commands;
foreach my $letter_no_arg_command ('aa','AA','ae','oe','AE','OE','o','O',
                                   'ss','l','L','DH','dh','TH','th') {
  $letter_no_arg_commands{$letter_no_arg_command} = 1;
  $brace_commands{$letter_no_arg_command} = 0;
}

foreach my $no_arg_command ('TeX','LaTeX','bullet','copyright',
  'registeredsymbol','dots','enddots','equiv','error','expansion','arrow',
  'minus','point','print','result','today',
  'exclamdown','questiondown','pounds','ordf','ordm',
  'atchar', 'lbracechar', 'rbracechar', 'backslashchar', 'hashchar', 'comma',
  'ampchar',
  'euro', 'geq','leq','tie','textdegree','click',
  'quotedblleft','quotedblright','quoteleft','quoteright','quotedblbase',
  'quotesinglbase','guillemetleft','guillemetright','guillemotleft',
  'guillemotright','guilsinglleft','guilsinglright') {
  $brace_commands{$no_arg_command} = 0;
}

# accent commands. They may be called with and without braces.
our %accent_commands;
foreach my $accent_command ('"','~','^','`',"'",',','=',
                           'ringaccent','H','dotaccent','u','ubaraccent',
                           'udotaccent','v','ogonek','tieaccent', 'dotless') {
  $accent_commands{$accent_command} = 1;
  $brace_commands{$accent_command} = 'accent';
}

our %style_commands;
foreach my $style_command ('asis','cite','clicksequence',
  'dfn', 'emph',
  'sc', 't', 'var',
  'headitemfont', 'code', 'command', 'env', 'file', 'kbd',
  'option', 'samp', 'strong', 'sub', 'sup') {
  $brace_commands{$style_command} = 'style';
  $style_commands{$style_command} = 1;
}

our %regular_font_style_commands;
foreach my $command ('r', 'i', 'b', 'sansserif', 'slanted') {
  $regular_font_style_commands{$command} = 1;
  $brace_commands{$command} = 'style';
  $style_commands{$command} = 1;
}

foreach my $one_arg_command ('U', 'dmn', 'key',
    'titlefont', 'anchor', 'errormsg', 'sortas', 'seeentry', 'seealso') {
  $brace_commands{$one_arg_command} = 1;
}

# FIXME: 'key', 'verb', 't'?
foreach my $other_arg_command ('w', 'hyphenation') {
  $brace_commands{$other_arg_command} = 'other';
}

our %code_style_commands;
foreach my $command ('code', 'command', 'env', 'file', 'kbd', 'key', 'option',
   'samp', 'verb', 't') {
  $code_style_commands{$command} = 1;
  $brace_commands{$command} = 'style';
}

# FIXME: a special case?
$code_style_commands{'indicateurl'} = 1;
$brace_commands{'indicateurl'} = 1;


# Commands that enclose full texts, that can contain multiple paragraphs.
our %context_brace_commands;
foreach my $context_brace_command ('footnote', 'caption',
    'shortcaption') {
  $context_brace_commands{$context_brace_command} = $context_brace_command;
  $brace_commands{$context_brace_command} = 'context';
}

our %math_commands;
# Commands that enclose full texts, that can contain multiple paragraphs
# and contain maths
foreach my $math_brace_command ('math') {
  $context_brace_commands{$math_brace_command} = $math_brace_command;
  $brace_commands{$math_brace_command} = 'context';
  $math_commands{$math_brace_command} = 1;
}

our %explained_commands;
foreach my $explained_command ('abbr', 'acronym') {
  $explained_commands{$explained_command} = 1;
  $brace_commands{$explained_command} = 2;
}


our %inline_format_commands;
our %inline_commands;
foreach my $inline_format_command ('inlineraw', 'inlinefmt', 
        'inlinefmtifelse') {
  $inline_format_commands{$inline_format_command} = 1;
  $brace_commands{$inline_format_command} = 2;
  $inline_commands{$inline_format_command} = 1;
}

$brace_commands{'inlinefmtifelse'} = 3;

our %inline_conditional_commands;
foreach my $inline_conditional_command ('inlineifclear', 'inlineifset') {
  $inline_conditional_commands{$inline_conditional_command} = 1;
  $brace_commands{$inline_conditional_command} = 2;
  $inline_commands{$inline_conditional_command} = 1;
}

foreach my $two_arg_command('email') {
  $brace_commands{$two_arg_command} = 2;
}

foreach my $three_arg_command('uref','url','inforef') {
  $brace_commands{$three_arg_command} = 3;
}

foreach my $five_arg_command('xref','ref','pxref','image') {
  $brace_commands{$five_arg_command} = 5;
}


# some classification to help converters
our %ref_commands;
foreach my $ref_command ('xref','ref','pxref','inforef') {
  $ref_commands{$ref_command} = 1;
}


# brace command that is not replaced with text.
my %unformatted_brace_commands;
foreach my $unformatted_brace_command ('anchor', 'shortcaption', 
    'caption', 'hyphenation', 'errormsg') {
  $unformatted_brace_commands{$unformatted_brace_command} = 1;
}


# commands delimiting blocks, with an @end.
# Value is either the number of arguments on the line separated by
# commas or the type of command, 'raw', 'def', 'conditional',
# or 'multitable'.
our %block_commands;

# commands that have a possible content before an item
our %block_item_commands;

sub gdt($)
{
  return $_[0];
}

our %def_map = (
    # basic commands. 
    # 'arg' and 'argtype' are for everything appearing after the other
    # arguments.
    'deffn',     [ 'category', 'name', 'arg' ],
    'defvr',     [ 'category', 'name' ],
    'deftypefn', [ 'category', 'type', 'name', 'argtype' ],
    'deftypeop', [ 'category', 'class' , 'type', 'name', 'argtype' ],
    'deftypevr', [ 'category', 'type', 'name' ],
    'defcv',     [ 'category', 'class' , 'name' ],
    'deftypecv', [ 'category', 'class' , 'type', 'name' ],
    'defop',     [ 'category', 'class' , 'name', 'arg' ],
    'deftp',     [ 'category', 'name', 'argtype' ],
    # shortcuts
    'defun',         {'deffn'     => gdt('Function')},
    'defmac',        {'deffn'     => gdt('Macro')},
    'defspec',       {'deffn'     => gdt('Special Form')},
    'defvar',        {'defvr'     => gdt('Variable')},
    'defopt',        {'defvr'     => gdt('User Option')},
    'deftypefun',    {'deftypefn' => gdt('Function')},
    'deftypevar',    {'deftypevr' => gdt('Variable')},
    'defivar',       {'defcv'     => gdt('Instance Variable')},
    'deftypeivar',   {'deftypecv' => gdt('Instance Variable')},
    'defmethod',     {'defop'     => gdt('Method')},
    'deftypemethod', {'deftypeop' => gdt('Method')},
);

# the type of index, fn: function, vr: variable, tp: type
my %index_type_def = (
 'fn' => ['deffn', 'deftypefn', 'deftypeop', 'defop'],
 'vr' => ['defvr', 'deftypevr', 'defcv', 'deftypecv' ],
 'tp' => ['deftp']
);

# Keys are commmands, values are names of indices.
our %command_index;

$command_index{'vtable'} = 'vr';
$command_index{'ftable'} = 'fn';

foreach my $index_type (keys %index_type_def) {
  foreach my $def (@{$index_type_def{$index_type}}) {
    $command_index{$def} = $index_type;
  }
}

our %def_commands;
our %def_aliases;
foreach my $def_command(keys %def_map) {
  if (ref($def_map{$def_command}) eq 'HASH') {
    my ($real_command) = keys (%{$def_map{$def_command}});
    $command_index{$def_command} = $command_index{$real_command};
    $def_aliases{$def_command} = $real_command;
  }
  $block_commands{$def_command} = 'def';
  $line_commands{$def_command.'x'} = 'line';
  $def_commands{$def_command} = 1;
  $def_commands{$def_command.'x'} = 1;
  $command_index{$def_command.'x'} = $command_index{$def_command};
}

#print STDERR "".Data::Dumper->Dump([\%def_aliases]);
#print STDERR "".Data::Dumper->Dump([\%def_prepended_content]);

$block_commands{'multitable'} = 'multitable';
$block_item_commands{'multitable'} = 1;

# block commands in which menu entry and menu comments appear
our %menu_commands;
foreach my $menu_command ('menu', 'detailmenu', 'direntry') {
  $menu_commands{$menu_command} = 1;
  $block_commands{$menu_command} = 0;
};

our %align_commands;
foreach my $align_command('raggedright', 'flushleft', 'flushright') {
  $block_commands{$align_command} = 0;
  $align_commands{$align_command} = 1;
}
$align_commands{'center'} = 1;

foreach my $block_command(
    'cartouche', 'group', 'indentedblock', 'smallindentedblock') {
  $block_commands{$block_command} = 0;
}

our %region_commands;
foreach my $block_command('titlepage', 'copying', 'documentdescription') {
  $block_commands{$block_command} = 0;
  $region_commands{$block_command} = 1;
}
  
our %preformatted_commands;
our %preformatted_code_commands;
foreach my $preformatted_command(
    'example', 'smallexample', 'lisp', 'smalllisp') {
  $block_commands{$preformatted_command} = 0;
  $preformatted_commands{$preformatted_command} = 1;
  $preformatted_code_commands{$preformatted_command} = 1;
}
$block_commands{'example'} = 'variadic'; # unlimited arguments

foreach my $preformatted_command(
    'display', 'smalldisplay', 'format', 'smallformat') {
  $block_commands{$preformatted_command} = 0;
  $preformatted_commands{$preformatted_command} = 1;
}

foreach my $block_math_command('displaymath') {
  $block_commands{$block_math_command} = 0;
  $math_commands{$block_math_command} = 1;
}

our %format_raw_commands;
foreach my $format_raw_command('html', 'tex', 'xml', 'docbook') {
  $block_commands{$format_raw_command} = 0;
  $format_raw_commands{$format_raw_command} = 1;
}

our %raw_commands;
# macro/rmacro are special
foreach my $raw_command ('verbatim',
                         'ignore', 'macro', 'rmacro') {
  $block_commands{$raw_command} = 'raw';
  $raw_commands{$raw_command} = 1;
}

our %texinfo_output_formats;
foreach my $command (keys(%format_raw_commands), 'info', 'plaintext') {
  $block_commands{'if' . $command} = 'conditional';
  $block_commands{'ifnot' . $command} = 'conditional';
  $texinfo_output_formats{$command} = $command;
}

$block_commands{'ifset'} = 'conditional';
$block_commands{'ifclear'} = 'conditional';

$block_commands{'ifcommanddefined'} = 'conditional';
$block_commands{'ifcommandnotdefined'} = 'conditional';

# 'macro' ?
foreach my $block_command_one_arg('table', 'ftable', 'vtable',
  'itemize', 'enumerate', 'quotation', 'smallquotation') {
  $block_commands{$block_command_one_arg} = 1;
  $block_item_commands{$block_command_one_arg} = 1 
    unless ($block_command_one_arg =~ /quotation/);
}

$block_commands{'float'} = 2;

# commands that forces closing an opened paragraph.
our %close_paragraph_commands;

foreach my $block_command (keys(%block_commands)) {
  $close_paragraph_commands{$block_command} = 1
     unless ($block_commands{$block_command} eq 'raw' or
             $block_commands{$block_command} eq 'conditional'
             or $format_raw_commands{$block_command});
}

$close_paragraph_commands{'verbatim'} = 1;

foreach my $close_paragraph_command ('titlefont', 'insertcopying', 'sp',
  'verbatiminclude', 'page', 'item', 'itemx', 'tab', 'headitem',
  'printindex', 'listoffloats', 'center', 'dircategory', 'contents',
  'shortcontents', 'summarycontents', 'caption', 'shortcaption',
  'setfilename', 'exdent') {
  $close_paragraph_commands{$close_paragraph_command} = 1;
}

foreach my $close_paragraph_command (keys(%def_commands)) {
  $close_paragraph_commands{$close_paragraph_command} = 1;
}

our %item_container_commands;
foreach my $item_container_command ('itemize', 'enumerate') {
  $item_container_commands{$item_container_command} = 1;
}
our %item_line_commands;
foreach my $item_line_command ('table', 'ftable', 'vtable') {
  $item_line_commands{$item_line_command} = 1;
}

our %deprecated_commands = (
  'definfoenclose' => '',
  'refill' => '',
  'inforef' => '',
  'centerchap' => '',
);

my %unformatted_block_commands;
foreach my $unformatted_block_command ('ignore', 'macro', 'rmacro') {
  $unformatted_block_commands{$unformatted_block_command} = 1;
}


# commands that should only appear at the root level and contain up to
# the next root command.  @node and sectioning commands.
our %root_commands;

our %command_structuring_level = (
              'top', 0,
              'chapter', 1,
              'unnumbered', 1,
              'chapheading', 1,
              'appendix', 1,
              'section', 2,
              'unnumberedsec', 2,
              'heading', 2,
              'appendixsec', 2,
              'subsection', 3,
              'unnumberedsubsec', 3,
              'subheading', 3,
              'appendixsubsec', 3,
              'subsubsection', 4,
              'unnumberedsubsubsec', 4,
              'subsubheading', 4,
              'appendixsubsubsec', 4,
         );

our %level_to_structuring_command;

{
  my $sections = [ ];
  my $appendices = [ ];
  my $unnumbered = [ ];
  my $headings = [ ];
  foreach my $command (keys (%command_structuring_level)) {
    if ($command =~ /^appendix/) {
      $level_to_structuring_command{$command} = $appendices;
    } elsif ($command =~ /^unnumbered/ or $command eq 'top') {
      $level_to_structuring_command{$command} = $unnumbered;
    } elsif ($command =~ /section$/ or $command eq 'chapter') {
      $level_to_structuring_command{$command} = $sections;
    } else {
      $level_to_structuring_command{$command} = $headings;
    }
    $level_to_structuring_command{$command}->[$command_structuring_level{$command}] 
      = $command;
  }
  $level_to_structuring_command{'appendixsection'} = $appendices;
  $level_to_structuring_command{'majorheading'} = $headings;
  $level_to_structuring_command{'centerchap'} = $unnumbered;
}


# out of the main hierarchy
$command_structuring_level{'part'} = 0;
# this are synonyms
$command_structuring_level{'appendixsection'} = 2;
# command_structuring_level{'majorheading'} is also 1 and not 0
$command_structuring_level{'majorheading'} = 1;
$command_structuring_level{'centerchap'} = 1;

our %sectioning_commands;

foreach my $sectioning_command (keys (%command_structuring_level)) {
  $line_commands{$sectioning_command} = 'line';
  if ($sectioning_command =~ /heading/) {
    $close_paragraph_commands{$sectioning_command} = 1;
  } else {
    $root_commands{$sectioning_command} = 1;
  }
  $sectioning_commands{$sectioning_command} = 1;
}


# misc commands that may be formatted as text.
# index commands may be too, but index command may be added with
# @def*index so they are not added here.
my %formatted_misc_commands;
foreach my $formatted_misc_command ('insertcopying', 'contents', 
   'shortcontents', 'summarycontents', 'center', 'printindex', 
   'listoffloats', 'shorttitlepage', 'settitle', 
   'author', 'subtitle', 'title', 'sp', 'exdent', 'headitem', 'item', 
   'itemx', 'tab', 'node', keys(%sectioning_commands)) {
  $formatted_misc_commands{$formatted_misc_command} = 1;
}


our %misc_commands = (%line_commands, %other_commands);

$root_commands{'node'} = 1;

our %all_commands;
foreach my $command (
  keys(%Texinfo::Common::block_commands),
  keys(%Texinfo::Common::brace_commands),
  keys(%Texinfo::Common::misc_commands),
  keys(%Texinfo::Common::no_brace_commands), 
  qw(value),
 ) {
  $all_commands{$command} = 1;
} 

our @MONTH_NAMES =
    (
     'January', 'February', 'March', 'April', 'May',
     'June', 'July', 'August', 'September', 'October',
     'November', 'December'
    );

# file:        file name to locate. It can be a file path.
# directories: a reference on a array containing a list of directories to
#              search the file in. 
# all_files:   if true collect all the files with that name, otherwise stop
#              at first match.
sub locate_init_file($$$)
{
  my $file = shift;
  my $directories = shift;
  my $all_files = shift;

  if (File::Spec->file_name_is_absolute($file)) {
    return $file if (-e $file and -r $file);
  } else {
    my @files;
    foreach my $dir (@$directories) {
      next unless (-d $dir);
      my $possible_file = File::Spec->catfile($dir, $file);
      if ($all_files) {
        push (@files, $possible_file) 
          if (-e $possible_file and -r $possible_file);
      } else {
        return $possible_file if (-e $possible_file and -r $possible_file);
      }
    }
    return @files if ($all_files);
  }
  return undef;
}

sub locate_include_file($$)
{
  my $self = shift;
  my $text = shift;
  my $file;

  my $ignore_include_directories = 0;

  my ($volume, $directories, $filename) = File::Spec->splitpath($text);
  my @directories = File::Spec->splitdir($directories);

  #print STDERR "$self $text @{$self->{'include_directories'}}\n";
  # If the path is absolute or begins with . or .., do not search in
  # include directories.
  if (File::Spec->file_name_is_absolute($text)) {
    $ignore_include_directories = 1;
  } else {
    foreach my $dir (@directories) {
      if ($dir eq File::Spec->updir() or $dir eq File::Spec->curdir()) {
        $ignore_include_directories = 1;
        last;
      } elsif ($dir ne '') {
        last;
      }
    }
  }

  #if ($text =~ m,^(/|\./|\.\./),) {
  if ($ignore_include_directories) {
    $file = $text if (-e $text and -r $text);
  } else {
    my @dirs;
    if ($self) {
      @dirs = @{$self->{'include_directories'}};
    } else {
      # no object with directory list and not an absolute path, never succeed
      return undef;
    }
    foreach my $include_dir (@{$self->{'include_directories'}}) {
      my ($include_volume, $include_directories, $include_filename) 
         = File::Spec->splitpath($include_dir, 1);
      
      my $possible_file = File::Spec->catpath($include_volume, 
        File::Spec->catdir(File::Spec->splitdir($include_directories), 
                           @directories), $filename);
      #$file = "$include_dir/$text" if (-e "$include_dir/$text" and -r "$include_dir/$text");
      $file = "$possible_file" if (-e "$possible_file" and -r "$possible_file");
      last if (defined($file));
    }
  }
  return $file;
}

sub open_out($$;$$)
{
  my $self = shift;
  my $file = shift;
  my $encoding = shift;
  my $use_binmode = shift;

  if (!defined($encoding) and $self 
      and defined($self->get_conf('OUTPUT_PERL_ENCODING'))) {
    $encoding = $self->get_conf('OUTPUT_PERL_ENCODING');
  }

  if ($file eq '-') {
    binmode(STDOUT) if $use_binmode;
    binmode(STDOUT, ":encoding($encoding)") if ($encoding);
    if ($self) {
      $self->{'unclosed_files'}->{$file} = \*STDOUT;
    }
    return \*STDOUT;
  }
  my $filehandle = do { local *FH };
  if (!open ($filehandle, '>', $file)) {
    return undef; 
  }
  # We run binmode to turn off outputting LF as CR LF under MS-Windows,
  # so that Info tag tables will have correct offsets.  This must be done
  # before setting the encoding filters with binmode.
  binmode($filehandle) if $use_binmode;
  if ($encoding) {
    if ($encoding eq 'utf8'
        or $encoding eq 'utf-8'
        or $encoding eq 'utf-8-strict') {
      binmode($filehandle, ':utf8');
    } else { # FIXME also right for shiftijs or similar encodings?
      binmode($filehandle, ':bytes');
    }
    binmode($filehandle, ":encoding($encoding)");
  }
  if ($self) {
    push @{$self->{'opened_files'}}, $file;
    $self->{'unclosed_files'}->{$file} = $filehandle;
  }
  return $filehandle;
}

sub warn_unknown_language($) {
  my $lang = shift;

  my @messages = ();
  my $lang_code = $lang;
  my $region_code;

  if ($lang =~ /^([a-z]+)_([A-Z]+)/) {
    $lang_code = $1;
    $region_code = $2;
  }

  if (! $Texinfo::Documentlanguages::language_codes{$lang_code}) {
    push @messages, sprintf(__("%s is not a valid language code"), 
                            $lang_code);
  }
  if (defined($region_code) 
       and ! $Texinfo::Documentlanguages::region_codes{$region_code}) {
    push @messages, sprintf(__("%s is not a valid region code"), 
                            $region_code);
  }
  return @messages;
}

my %possible_split = (
  'chapter' => 1,
  'section' => 1,
  'node' => 1,
);

sub warn_unknown_split($) {
  my $split = shift;

  my @messages = ();
  if ($split and !$possible_split{$split}) {
    push @messages, sprintf(__("%s is not a valid split possibility"), $split);
  }
  return @messages;
}

# This should do the job, or at least don't do wrong if $self
# is not defined, as could be the case if called from 
# Texinfo::Convert::Text.
sub expand_verbatiminclude($$)
{
  my $self = shift;
  my $current = shift;

  return unless ($current->{'extra'} and defined($current->{'extra'}->{'text_arg'}));
  my $text = $current->{'extra'}->{'text_arg'};
  my $file = locate_include_file($self, $text);

  my $verbatiminclude;

  if (defined($file)) {
    if (!open(VERBINCLUDE, $file)) {
      if ($self) {
        $self->line_error(sprintf(__("could not read %s: %s"), $file, $!), 
                            $current->{'line_nr'});
      }
    } else {
      if (defined $current->{'extra'}->{'input_perl_encoding'}) {
        binmode(VERBINCLUDE, ":encoding("
                             . $current->{'extra'}->{'input_perl_encoding'}
                             . ")");
      }
      $verbatiminclude = { 'cmdname' => 'verbatim',
                           'parent' => $current->{'parent'},
                           'extra' => 
                        {'text_arg' => $current->{'extra'}->{'text_arg'}} };
      while (<VERBINCLUDE>) {
        push @{$verbatiminclude->{'contents'}}, 
                  {'type' => 'raw', 'text' => $_ };
      }
      if (!close (VERBINCLUDE)) {
        if ($self) {
          $self->document_warn(sprintf(__(
                      "error on closing \@verbatiminclude file %s: %s"),
                             $file, $!));
        }
      }
    }
  } elsif ($self) {
    $self->line_error(sprintf(__("\@%s: could not find %s"), 
                    $current->{'cmdname'}, $text), $current->{'line_nr'});
  }
  return $verbatiminclude;
}

sub definition_category($$)
{
  my $self = shift;
  my $current = shift;

  return undef if (!$current->{'extra'}
      or !$current->{'extra'}->{'def_parsed_hash'});

  my $arg_category = $current->{'extra'}->{'def_parsed_hash'}->{'category'};
  my $arg_class = $current->{'extra'}->{'def_parsed_hash'}->{'class'};

  return $arg_category
    if (!defined($arg_class));
  
  my $style = $command_index{$current->{'extra'}->{'def_command'}};
  if ($style eq 'fn') {
    if ($self) {
      return $self->gdt('{category} on {class}', { 'category' => $arg_category,
                                          'class' => $arg_class });
    } else {
      return {'contents' => [$arg_category, {'text' => ' on '}, $arg_class]};
    }
  } elsif ($style eq 'vr') {
    if ($self) {
      return $self->gdt('{category} of {class}', { 'category' => $arg_category,
                                          'class' => $arg_class });
    } else {
      return {'contents' => [$arg_category, {'text' => ' of '}, $arg_class]};
    }
  }
}

sub expand_today($)
{
  my $self = shift;
  if ($self->get_conf('TEST')) {
    return {'text' => 'a sunny day'};
  }

  my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst)
    = ($ENV{SOURCE_DATE_EPOCH}
        ? gmtime($ENV{SOURCE_DATE_EPOCH})
        : localtime(time));
  # See https://reproducible-builds.org/specs/source-date-epoch/.

  $year += ($year < 70) ? 2000 : 1900;
  return $self->gdt('{month} {day}, {year}',
          { 'month' => $self->gdt($MONTH_NAMES[$mon]),
            'day' => $mday, 'year' => $year });
}

sub translated_command_tree($$)
{
  my $self = shift;
  my $cmdname = shift;
  if ($self->{'translated_commands'}->{$cmdname}) {
    return $self->gdt($self->{'translated_commands'}->{$cmdname});
  }
  return undef;
}

sub numbered_heading($$$;$)
{
  my $self = shift;
  my $current = shift;
  my $text = shift;
  my $numbered = shift;

  my $number;
  if (defined($current->{'number'}) and ($numbered or !defined($numbered))) {
    $number = $current->{'number'};
  }

  my $result;
  if ($self) {
    if (defined($number)) {
      if ($current->{'cmdname'} eq 'appendix' and $current->{'level'} == 1) {
        $result = $self->gdt('Appendix {number} {section_title}',
                   {'number' => $number, 'section_title' => $text}, 
                   'translated_text');
      } else {
        $result = $self->gdt('{number} {section_title}',
                   {'number' => $number, 'section_title' => $text},
                   'translated_text');
      }
    } else {
      $result = $text;
    }
  } else {
    $result = $text;
    $result = $number.' '.$result if (defined($number));
    if ($current->{'cmdname'} eq 'appendix' and $current->{'level'} == 1) {
      $result = 'Appendix '.$result;
    }
  }
  chomp ($result);
  return $result;
}

sub definition_arguments_content($)
{
  my $root = shift;
  my $result;

  return undef if (!defined($root->{'extra'}) 
                    or !defined($root->{'extra'}->{'def_parsed_hash'}));
  my @args = @{$root->{'args'}->[0]->{'contents'}};
  while (@args) {
    last if (defined($args[0]->{'extra'})
             and defined($args[0]->{'extra'}->{'def_role'})
             and $args[0]->{'extra'}->{'def_role'} ne 'spaces'
             and !$root->{'extra'}->{'def_parsed_hash'}
                       ->{$args[0]->{'extra'}->{'def_role'}});
    shift @args;
  }
  if (scalar(@args) > 0) {
    return \@args;
  } else {
    return undef;
  }
}

# find the accent commands stack and the innermost text contents
sub find_innermost_accent_contents($;$)
{
  my $current = shift;
  my $encoding = shift;
  my @accent_commands = ();
  my $debug = 0;
 ACCENT:
  while (1) {
    # the following can happen if called with a bad tree
    if (!$current->{'cmdname'} 
        or !$accent_commands{$current->{'cmdname'}}) {
      #print STDERR "BUG: Not an accent command in accent\n";
      cluck "BUG: Not an accent command in accent\n";
      #print STDERR Texinfo::Convert::Texinfo::convert($current)."\n";
      #print STDERR Data::Dumper->Dump([$current]);
      last;
    }
    push @accent_commands, $current;
    # A bogus accent, that may happen
    if (!$current->{'args'}) {
      return ([], \@accent_commands);
    }
    my $arg = $current->{'args'}->[0];
    if (!$arg->{'contents'}) {
      print STDERR "BUG: No content in accent command\n";
      #print STDERR Data::Dumper->Dump([$current]);
      #print STDERR Texinfo::Convert::Texinfo::convert($current)."\n";
      return ([], \@accent_commands);
    }
    # inside the argument of an accent
    my $text_contents = [];
    foreach my $content (@{$arg->{'contents'}}) {
      if (!($content->{'cmdname'} and ($content->{'cmdname'} eq 'c'
                                  or $content->{'cmdname'} eq 'comment'))) {
        if ($content->{'cmdname'} and $accent_commands{$content->{'cmdname'}}) {
          $current = $content;
          next ACCENT;
        } else {
          push @$text_contents, $content;
        }
      }
    }
    # we go here if there was no nested accent
    return ($text_contents, \@accent_commands);
  }
}

sub trim_spaces_comment_from_content($)
{
  my $contents = shift;
  shift @$contents 
    if ($contents->[0] and $contents->[0]->{'type'}
       and ($contents->[0]->{'type'} eq 'empty_line_after_command'
            or $contents->[0]->{'type'} eq 'empty_spaces_after_command'
            or $contents->[0]->{'type'} eq 'empty_spaces_before_argument'
            or $contents->[0]->{'type'} eq 'empty_spaces_after_close_brace'));

  while (@$contents 
         and (($contents->[-1]->{'cmdname'}
               and ($contents->[-1]->{'cmdname'} eq 'c' 
                    or $contents->[-1]->{'cmdname'} eq 'comment'))
              or ($contents->[-1]->{'type'}
                  and ($contents->[-1]->{'type'} eq 'spaces_at_end'
                       or $contents->[-1]->{'type'} eq 'space_at_end_block_command')))) {
    pop @$contents;
  }
}

sub _find_end_brace($$)
{
  my $text = shift;
  my $braces_count = shift;

  my $before = '';
  while ($braces_count > 0 and length($text)) {
    if ($text =~ s/([^()]*)([()])//) {
      $before .= $1.$2;
      my $brace = $2;
      if ($brace eq '(') {
        $braces_count++;
      } else {
        $braces_count--;
        if ($braces_count == 0) {
          return ($before, $text, 0);
        }
      }
    } else {
      $before .= $text;
      $text = '';
    }
  }
  return ($before, undef, $braces_count);
}

# This only counts opening braces, and returns 0 once all the parentheses
# are closed
sub _count_opened_tree_braces($$);
sub _count_opened_tree_braces($$)
{
  my $current = shift;
  my $braces_count = shift;
  if (defined($current->{'text'})) {
    my ($before, $after);
    ($before, $after, $braces_count) = _find_end_brace($current->{'text'},
                                                          $braces_count);
  }
  return $braces_count;
}

# $NODE->{'contents'} is the Texinfo for the specification of a node.
# Returned object is a hash with two fields:
#
#     manual_content - Texinfo tree for a manual name extracted from the
#                      node specification.
#     node_content - Texinfo tree for the node name on its own
#
# retrieve a leading manual name in parentheses, if there is one.
sub parse_node_manual($)
{
  my $node = shift;
  my @contents = @{$node->{'contents'}};

  my $manual;
  my $result;
  my ($end_paren, $spaces_after);

  if ($contents[0] and $contents[0]->{'text'} and $contents[0]->{'text'} =~ /^\(/) {
    my $braces_count = 1;
    if ($contents[0]->{'text'} !~ /^\($/) {
      my $brace = shift @contents;
      my $brace_text = $brace->{'text'};
      $brace_text =~ s/^\(//;
      unshift @contents, { 'text' => $brace_text, 'type' => $brace->{'type'},
                           'parent' => $brace->{'parent'} } if $brace_text ne '';
    } else {
      shift @contents;
    }
    while(@contents) {
      my $content = shift @contents;
      if (!defined($content->{'text'}) or $content->{'text'} !~ /\)/) {
        push @$manual, $content;
        $braces_count = _count_opened_tree_braces($content, $braces_count);
        # This is an error, braces were closed in a command
        if ($braces_count == 0) {
          last;
        }
      } else {
        my ($before, $after);
        ($before, $after, $braces_count) = _find_end_brace($content->{'text'},
                                                              $braces_count);
        if ($braces_count == 0) {
          $before =~ s/(\))$//;
          $end_paren = $1;
          push @$manual, { 'text' => $before, 'parent' => $content->{'parent'} }
            if ($before ne '');
          $after =~ s/^(\s*)//;
          $spaces_after = $1;
          unshift @contents,  { 'text' => $after, 'parent' => $content->{'parent'} }
            if ($after ne '');
          last;
        } else {
          push @$manual, $content;
        }
      }
    }
    if ($braces_count == 0) {
      $result->{'manual_content'} = $manual if (defined($manual));
    } else {
      @contents = ({ 'text' => '(', 'parent' => $node }, @$manual);
    }
  }
  if (@contents) {
    $result->{'node_content'} = \@contents;
  }

  # Overwrite the contents array so that all the elements in 'manual_content'
  # and 'node_content' are in the main tree.
  my $new_contents = [];
  if (defined($result) and defined($result->{'manual_content'})) {
    @$new_contents = ({ 'text' => '(', 'parent' => $node },
                      @$manual);
    push @$new_contents, {  'text' => ')', 'parent' => $node }
      if $end_paren;
    push @$new_contents, { 'text' => $spaces_after, 'parent' => $node }
      if $spaces_after;
  }
  if (@contents) {
    @$new_contents = (@$new_contents, @contents);
  }
  $node->{'contents'} = $new_contents;

  return $result;
}

sub float_name_caption($$)
{
  my $self = shift;
  my $root = shift;

  my $caption;
  if ($root->{'extra'}->{'caption'}) {
    $caption = $root->{'extra'}->{'caption'};
  } elsif ($root->{'extra'}->{'shortcaption'}) {
    $caption = $root->{'extra'}->{'shortcaption'};
  }
  #if ($self->get_conf('DEBUG')) {
  #  my $caption_texi = 
  #    Texinfo::Convert::Texinfo::convert({ 'contents' => $caption->{'contents'}});
  #  print STDERR "  CAPTION: $caption_texi\n";
  #}
  my $type;
  if ($root->{'extra'}->{'type'}->{'normalized'} ne '') {
    $type = {'contents' => $root->{'extra'}->{'type'}->{'content'}};
  }

  my $prepended;
  if ($type) {
    if ($caption) {
      if (defined($root->{'number'})) {
        $prepended = $self->gdt('{float_type} {float_number}: ',
            {'float_type' => $type,
             'float_number' => $root->{'number'}});
      } else {
        $prepended = $self->gdt('{float_type}: ',
          {'float_type' => $type});
      }
    } else {
      if (defined($root->{'number'})) {
        $prepended = $self->gdt("{float_type} {float_number}\n",
            {'float_type' => $type,
              'float_number' => $root->{'number'}});
      } else {
        $prepended = $self->gdt("{float_type}\n",
            {'float_type' => $type});
      }
    }
  } elsif (defined($root->{'number'})) {
    if ($caption) {
      $prepended = $self->gdt('{float_number}: ',
          {'float_number' => $root->{'number'}});
    } else {
      $prepended = $self->gdt("{float_number}\n",
           {'float_number' => $root->{'number'}});
    }
  }
  return ($caption, $prepended);
}

# decompose a decimal number on a given base.
sub _decompose_integer($$)
{
  my $number = shift;
  my $base = shift;
  my @result = ();

  while ($number >= 0) {
    my $factor = $number % $base;
    push (@result, $factor);
    $number = int(($number - $factor) / $base) - 1;
  }
  return @result;
}

sub enumerate_item_representation($$)
{
  my $specification = shift;
  my $number = shift;

  if ($specification =~ /^[0-9]+$/) {
    return $specification + $number -1;
  }

  my $result = '';
  my $base_letter = ord('a');
  $base_letter = ord('A') if (ucfirst($specification) eq $specification);
  my @letter_ords = _decompose_integer(ord($specification) - $base_letter + $number - 1, 26);
  foreach my $ord (@letter_ords) {
    $result = chr($base_letter + $ord) . $result;
  }
  return $result;
}

sub is_content_empty($;$);
sub is_content_empty($;$)
{
  my $tree = shift;
  my $do_not_ignore_index_entries = shift;
  if (!defined($tree) or !exists($tree->{'contents'})) {
    return 1;
  }
  foreach my $content (@{$tree->{'contents'}}) {
    #print STDERR _print_current($content);
    if ($content->{'cmdname'}) {
      if ($content->{'type'} and $content->{'type'} eq 'index_entry_command') {
        if ($do_not_ignore_index_entries) {
          return 0;
        } else {
          next;
        }
      }
      if (exists($misc_commands{$content->{'cmdname'}})) {
        my @truc = keys(%formatted_misc_commands);
        if ($formatted_misc_commands{$content->{'cmdname'}}) {
          return 0;
        } else {
          next;
        }
      } elsif ($unformatted_brace_commands{$content->{'cmdname'}} 
               or $unformatted_block_commands{$content->{'cmdname'}}) {
        next;
      } else {
        return 0;
      }
    }
    if ($content->{'type'}) {
      if ($content->{'type'} eq 'paragraph') {
        return 0;
      }
    }
    if ($content->{'text'} and $content->{'text'} =~ /\S/) {
      return 0;
    }
    if (not is_content_empty($content, $do_not_ignore_index_entries)) {
      return 0;
    }
  }
  return 1;
}
sub normalize_top_node_name($)
{
  my $node = shift;
  if ($node =~ /^top$/i) {
    return 'Top';
  }
  return $node;
}

# Argument is a converter object
sub _convert_text_options($)
{
  my $self = shift;
  my %options;
  if ($self->get_conf('ENABLE_ENCODING')) {
    if ($self->get_conf('OUTPUT_ENCODING_NAME')) {
      $options{'enabled_encoding'} = $self->get_conf('OUTPUT_ENCODING_NAME');
    }
  }
  $options{'TEST'} = 1 if ($self->get_conf('TEST'));
  $options{'NUMBER_SECTIONS'} = $self->get_conf('NUMBER_SECTIONS');
  $options{'converter'} = $self;
  $options{'expanded_formats_hash'} = $self->{'expanded_formats_hash'};
  return %options;
}

# Used in count_bytes
my $Encode_encoding_object;
my $last_encoding;

sub count_bytes($$;$) 
{
  my $self = shift;
  my $string = shift;
  my $encoding = shift;

  if (!defined($encoding) and $self and $self->get_conf('OUTPUT_PERL_ENCODING')) {
    $encoding = $self->get_conf('OUTPUT_PERL_ENCODING');
  }

  if ($encoding eq 'utf-8'
      or $encoding eq 'utf-8-strict') {
    if (Encode::is_utf8($string)) {
      # Get the number of bytes in the underlying storage.  This may
      # be slightly faster than calling Encode::encode_utf8.
      use bytes;
      return length($string);

      # Here's another way of doing it.
      #Encode::_utf8_off($string);
      #my $length = length($string);
      #Encode::_utf8_on($string);
      #return $length
    } else {
      return length(Encode::encode_utf8($string));
    }
  } elsif ($encoding and $encoding ne 'ascii') {
    if (!defined($last_encoding) or $last_encoding ne $encoding) {
      # Look up and save encoding object for next time.  This is
      # slightly faster than calling Encode::encode.
      $last_encoding = $encoding;
      $Encode_encoding_object = Encode::find_encoding($encoding);
      if (!defined($Encode_encoding_object)) {
        Carp::croak "Unknown encoding '$encoding'";
      }
    }
    return length($Encode_encoding_object->encode($string));
  } else {
    return length($string);
    #my $length = length($string);
    #$string =~ s/\n/\\n/g;
    #$string =~ s/\f/\\f/g;
    #print STDERR "Count($length): $string\n";
    #return $length;
  }
  # FIXME is the following required for correct count of end of lines?
  #if ($encoding) {
  #  return length(Encode::encode($encoding, $string));
  #} else {
  #  return length(Encode::encode('ascii', $string));
  #}
}

# TODO
# also recurse into
# extra->misc_args, extra->args_index
# extra->index_entry extra->type
#
# extra that should point to other elements: 
# command_as_argument end_command
# associated_section part_associated_section associated_node associated_part
# @prototypes @columnfractions titlepage quotation @author command
# menu_entry_description menu_entry_name
# 
# should point to other elements, or be copied.  And some should be recursed
# into too.
# extra->type->content
# extra->nodes_manuals->[]
# extra->node_content
# extra->node_argument
# extra->explanation_contents
# extra->menu_entry_node


sub _copy_tree($$$);
sub _copy_tree($$$)
{
  my $current = shift;
  my $parent = shift;
  my $reference_associations = shift;
  my $new = {};
  $reference_associations->{$current} = $new;
  $new->{'parent'} = $parent if ($parent);
  foreach my $key ('type', 'cmdname', 'text') {
    $new->{$key} = $current->{$key} if (exists($current->{$key}));
  }
  foreach my $key ('args', 'contents') {
    if ($current->{$key}) {
      if (ref($current->{$key}) ne 'ARRAY') {
        my $command_or_type = '';
        if ($new->{'cmdname'}) {
          $command_or_type = '@'.$new->{'cmdname'};
        } elsif ($new->{'type'}) {
          $command_or_type = $new->{'type'};
        }
        print STDERR "Not an array [$command_or_type] $key ".ref($current->{$key})."\n";
      }
      $new->{$key} = [];
      $reference_associations->{$current->{$key}} = $new->{$key};
      foreach my $child (@{$current->{$key}}) {
        push @{$new->{$key}}, _copy_tree($child, $new, $reference_associations);
      }
    }
  }
  if ($current->{'extra'}) {
    $new->{'extra'} = {};
    foreach my $key (keys %{$current->{'extra'}}) {
      if ($current->{'cmdname'} and $current->{'cmdname'} eq 'multitable'
          and $key eq 'prototypes') {
        $new->{'extra'}->{$key} = [];
        $reference_associations->{$current->{'extra'}->{$key}} = $new->{$key};
        foreach my $child (@{$current->{'extra'}->{$key}}) {
          push @{$new->{'extra'}->{$key}}, 
                  _copy_tree($child, $new, $reference_associations);
        }
      } elsif (!ref($current->{'extra'}->{$key})) {
        $new->{'extra'}->{$key} = $current->{'extra'}->{$key};
      }
    }
  }
  return $new;
}

# for user-defined code
sub collect_commands_in_tree($$)
{
  my $root = shift;
  my $commands_list = shift;

  my $commands_hash = {};
  foreach my $command_name (@$commands_list) {
    $commands_hash->{$command_name} = [];
  }
  _collect_commands_in_tree($root, $commands_hash);
  return $commands_hash;
}

sub _collect_commands_in_tree($$);
sub _collect_commands_in_tree($$)
{
  my $current = shift;
  my $commands_hash = shift;

  if (defined($current->{'cmdname'})
      and defined($commands_hash->{$current->{'cmdname'}})) {
    push @{$commands_hash->{$current->{'cmdname'}}}, $current;
  }
  foreach my $key ('args', 'contents') {
    if ($current->{$key}) {
      foreach my $child (@{$current->{$key}}) {
        _collect_commands_in_tree($child, $commands_hash);
      }
    }
  }
}

# Not used.
sub _collect_references($$);
sub _collect_references($$)
{
  my $current = shift;
  my $references = shift;
  foreach my $key ('args', 'contents') {
    if ($current->{$key}) {
      $references->{$current->{$key}} = $current->{$key};
      foreach my $child (@{$current->{$key}}) {
        $references->{$child} = $child;
        _collect_references($child, $references);
      }
    }
  }
}

sub _substitute_references_in_array($$$);
sub _substitute_references_in_array($$$)
{
  my $array = shift;
  my $reference_associations = shift;
  my $context = shift;

  my $result = [];
  my $index = 0;
  foreach my $item (@{$array}) {
    if (!ref($item)) {
      push @{$result}, $item;
    } elsif ($reference_associations->{$item}) {
      push @{$result}, $reference_associations->{$item};
    } elsif (ref($item) eq 'ARRAY') {
      push @$result, 
        _substitute_references_in_array($item, $reference_associations,
                                        "$context [$index]");
    } elsif (defined($item->{'text'})) {
      my $new_text = _copy_tree($item, undef, $reference_associations);
      substitute_references($item, $new_text, $reference_associations);
      push @{$result}, $new_text;
    } else {
      print STDERR "Trouble with $context [$index] (".ref($item).")\n";
      push @{$result}, undef;
    }
    $index++;
  }
  return $result;
}

sub substitute_references($$$);
sub substitute_references($$$)
{
  my $current = shift;
  my $new = shift;
  my $reference_associations = shift;
  
  foreach my $key ('args', 'contents') {
    if ($new->{$key}) {
      my $index = 0;
      foreach my $child (@{$new->{$key}}) {
        substitute_references($child, $current->{$key}->[$index],
                              $reference_associations);
        $index++;
      }
    }
  }
  if ($current->{'extra'}) {
    foreach my $key (keys %{$current->{'extra'}}) {
      if (ref($current->{'extra'}->{$key})) {
        my $command_or_type = '';
        if ($new->{'cmdname'}) {
          $command_or_type = '@'.$new->{'cmdname'};
        } elsif ($new->{'type'}) {
          $command_or_type = $new->{'type'};
        }
        
        if ($current->{'cmdname'} and $current->{'cmdname'} eq 'multitable'
            and $key eq 'prototypes') {
          my $index = 0;
          foreach my $child (@{$new->{'extra'}->{$key}}) {
            substitute_references($child, $current->{'extra'}->{$key}->[$index],
                                  $reference_associations);
            $index++;
          }
        } elsif ($reference_associations->{$current->{'extra'}->{$key}}) {
          $new->{'extra'}->{$key} 
            = $reference_associations->{$current->{'extra'}->{$key}};
          #print STDERR "Done [$command_or_type]: $key\n";
        } else {
          if (ref($current->{'extra'}->{$key}) eq 'ARRAY') {
            
            #print STDERR "Array $command_or_type -> $key\n";
            $new->{'extra'}->{$key} = _substitute_references_in_array(
              $current->{'extra'}->{$key}, $reference_associations,
              "[$command_or_type]{$key}");
          } else {
            if (($current->{'cmdname'} 
                 and ($current->{'cmdname'} eq 'listoffloats'
                     or $current->{'cmdname'} eq 'float') 
                 and $key eq 'type')
                 or ($key eq 'index_entry')
                 or ($current->{'type'} 
                     and $current->{'type'} eq 'menu_entry'
                     and $key eq 'menu_entry_node')) {
              foreach my $type_key (keys(%{$current->{'extra'}->{$key}})) {
                if (!ref($current->{'extra'}->{$key}->{$type_key})) {
                  $new->{'extra'}->{$key}->{$type_key} 
                    = $current->{'extra'}->{$key}->{$type_key};
                } elsif ($reference_associations->{$current->{'extra'}->{$key}->{$type_key}}) {
                  $new->{'extra'}->{$key}->{$type_key}
                    = $reference_associations->{$current->{'extra'}->{$key}->{$type_key}};
                } elsif (ref($current->{'extra'}->{$key}->{$type_key}) eq 'ARRAY') {
                  $new->{'extra'}->{$key}->{$type_key}
                    = _substitute_references_in_array(
                      $current->{'extra'}->{$key}->{$type_key}, 
                      $reference_associations,
                      "[$command_or_type]{$key}{$type_key}");
                } else {
                  print STDERR "Not substituting [$command_or_type]{$key}: $type_key\n";
                }
              }
            } else {
              print STDERR "Not substituting [$command_or_type]: $key ($current->{'extra'}->{$key})\n";
            }
          }
        }
      }
    }
  }
}

sub copy_tree($;$)
{
  my $current = shift;
  my $parent = shift;
  my $reference_associations = {};
  my $copy = _copy_tree($current, $parent, $reference_associations);
  substitute_references($current, $copy, $reference_associations);
  return $copy;
}

sub modify_tree($$$;$);
sub modify_tree($$$;$)
{
  my $self = shift;
  my $tree = shift;
  my $operation = shift;
  my $argument = shift;
  #print STDERR "modify_tree tree: $tree\n";

  if ($tree->{'args'}) {
    my @args = @{$tree->{'args'}};
    for (my $i = 0; $i <= $#args; $i++) {
      my @new_args = &$operation($self, 'arg', $args[$i], $argument);
      modify_tree($self, $args[$i], $operation, $argument);
      # this puts the new args at the place of the old arg using the 
      # offset from the end of the array
      splice (@{$tree->{'args'}}, $i - $#args -1, 1, @new_args);
      #foreach my $arg (@new_args) {
      #  modify_tree($self, $arg, $operation);
      #}
    }
  }
  if ($tree->{'contents'}) {
    my @contents = @{$tree->{'contents'}};
    for (my $i = 0; $i <= $#contents; $i++) {
      my @new_contents = &$operation($self, 'content', $contents[$i], $argument);
      modify_tree($self, $contents[$i], $operation, $argument);
      # this puts the new contents at the place of the old content using the 
      # offset from the end of the array
      splice (@{$tree->{'contents'}}, $i - $#contents -1, 1, @new_contents);
      #foreach my $content (@new_contents) {
      #  modify_tree($self, $content, $operation);
      #}
    }
  }
  return $tree;
}

sub _protect_comma($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  return _protect_text($current, quotemeta(','));
}

sub protect_comma_in_tree($)
{
  my $tree = shift;
  return modify_tree(undef, $tree, \&_protect_comma);
}

sub _new_asis_command_with_text($$;$)
{
  my $text = shift;
  my $parent = shift;
  my $text_type = shift;
  my $new_command = {'cmdname' => 'asis', 'parent' => $parent };
  push @{$new_command->{'args'}}, {'type' => 'brace_command_arg',
                                   'parent' => $new_command};
  push @{$new_command->{'args'}->[0]->{'contents'}}, {
    'text' => $text,
    'parent' => $new_command->{'args'}->[0]};
  if (defined($text_type)) {
    $new_command->{'args'}->[0]->{'contents'}->[0]->{'type'} = $text_type;
  }
  return $new_command;
}

sub _protect_text($$)
{
  my $current = shift;
  my $to_protect = shift;

  #print STDERR "$to_protect: $current "._print_current($current)."\n";
  if (defined($current->{'text'}) and $current->{'text'} =~ /$to_protect/
      and !(defined($current->{'type'}) and $current->{'type'} eq 'raw')) {
    my @result = ();
    my $remaining_text = $current->{'text'};
    while ($remaining_text) {
      if ($remaining_text =~ s/^(.*?)(($to_protect)+)//) {
        if ($1 ne '') {
          push @result, {'text' => $1, 'parent' => $current->{'parent'}};
          $result[-1]->{'type'} = $current->{'type'} 
            if defined($current->{'type'});
        }
        if ($to_protect eq quotemeta(',')) {
          for (my $i = 0; $i < length($2); $i++) {
            push @result, {'cmdname' => 'comma', 'parent' => $current->{'parent'},
                           'args' => [{'type' => 'brace_command_arg'}]};
          }
        } else {
          push @result, _new_asis_command_with_text($2, $current->{'parent'},
                                                    $current->{'type'});
        }
      } else {
        push @result, {'text' => $remaining_text, 'parent' => $current->{'parent'}};
        $result[-1]->{'type'} = $current->{'type'} 
          if defined($current->{'type'});
        last;
      }
    }
    #print STDERR "Result: @result\n";
    return @result;
  } else {
    #print STDERR "No change: $current\n";
    return ($current);
  }
}

sub _protect_colon($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  return _protect_text ($current, quotemeta(':'));
}

sub protect_colon_in_tree($)
{
  my $tree = shift;
  return modify_tree(undef, $tree, \&_protect_colon);
}

sub _protect_node_after_label($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  return _protect_text ($current, '['. quotemeta(".\t,") .']');
}

sub protect_node_after_label_in_tree($)
{
  my $tree = shift;
  return modify_tree(undef, $tree, \&_protect_node_after_label);
}

sub _is_cpp_line($)
{
  my $text = shift;
  return 1 if ($text =~ /^\s*#\s*(line)? (\d+)(( "([^"]+)")(\s+\d+)*)?\s*$/);
  return 0;
}

sub _protect_hashchar_at_line_beginning($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  #print STDERR "$type $current "._print_current($current)."\n";
  # if the next is a hash character at line beginning, mark it
  if (defined($current->{'text'}) and $current->{'text'} =~ /\n$/
      and $current->{'parent'} and $current->{'parent'}->{'contents'}) {
    my $parent = $current->{'parent'};
    #print STDERR "End of line in $current, parent $parent: (@{$parent->{'contents'}})\n";
    my $current_found = 0;
    foreach my $content (@{$parent->{'contents'}}) {
      if ($current_found) {
        #print STDERR "after $current: $content $content->{'text'}\n";
        if ($content->{'text'} and _is_cpp_line($content->{'text'})) {
          $content->{'extra'}->{'_protect_hashchar'} = 1;
        }
        last;
      } elsif ($content eq $current) {
        $current_found = 1;
      }
    }
  }

  my $protect_hash = 0;
  # if marked, or first and a cpp_line protect a leading hash character
  if ($current->{'extra'} and $current->{'extra'}->{'_protect_hashchar'}) {
    delete $current->{'extra'}->{'_protect_hashchar'};
    if (!scalar(keys(%{$current->{'extra'}}))) {
      delete $current->{'extra'};
    }
    $protect_hash = 1;
  } elsif ($current->{'parent'} and $current->{'parent'}->{'contents'}
           and $current->{'parent'}->{'contents'}->[0]
           and $current->{'parent'}->{'contents'}->[0] eq $current
           and $current->{'text'}
           and _is_cpp_line($current->{'text'})) {
    $protect_hash = 1;
  }
  if ($protect_hash) {
    my @result = ();
    if ($current->{'type'} and $current->{'type'} eq 'raw') {
      if ($self) {
        my $parent = $current->{'parent'};
        while ($parent) {
          if ($parent->{'cmdname'} and $parent->{'line_nr'}) {
            $self->line_warn(sprintf(__(
                  "could not protect hash character in \@%s"), 
                             $parent->{'cmdname'}), $parent->{'line_nr'});
            last;
          }
          $parent = $parent->{'parent'};
        }
      }
    } else {
      $current->{'text'} =~ s/^(\s*)#//;
      if ($1 ne '') {
        push @result, {'text' => $1, 'parent' => $current->{'parent'}};
      }
      push @result, {'cmdname' => 'hashchar', 'parent' => $current->{'parent'},
                     'args' => [{'type' => 'brace_command_arg'}]};
    }
    push @result, $current;
    return @result;
  } else {
    return ($current);
  }
}

sub protect_hashchar_at_line_beginning($$)
{
  my $self = shift;
  my $tree = shift;
  return modify_tree($self, $tree, \&_protect_hashchar_at_line_beginning);
}

sub protect_first_parenthesis($)
{
  my $contents = shift;
  return undef if (!defined ($contents));
  my @contents = @$contents;
  my $brace;
  if ($contents[0] and $contents->[0]{'text'} and $contents[0]->{'text'} =~ /^\(/) {
    if ($contents[0]->{'text'} !~ /^\($/) {
      $brace = shift @contents;
      my $brace_text = $brace->{'text'};
      $brace_text =~ s/^\(//;
      unshift @contents, { 'text' => $brace_text, 'type' => $brace->{'type'},
                           'parent' => $brace->{'parent'} } if $brace_text ne '';
    } else {
      $brace = shift @contents;
    }
    unshift @contents, _new_asis_command_with_text('(', $brace->{'parent'},
                                                    $brace->{'type'});
  }
  return \@contents;
}

sub find_parent_root_command($$)
{
  my $parser = shift;
  my $current = shift;

  my $root_command;
  while (1) {
    if ($current->{'cmdname'}) {
      if ($root_commands{$current->{'cmdname'}}) {
        return $current;
      } elsif ($region_commands{$current->{'cmdname'}}) {
        if ($current->{'cmdname'} eq 'copying' and $parser
            and $parser->{'extra'} and $parser->{'extra'}->{'insertcopying'}) {
          foreach my $insertcopying(@{$parser->{'extra'}->{'insertcopying'}}) {
            my $root_command
              = $parser->find_parent_root_command($insertcopying);
            return $root_command if (defined($root_command));
          }
        } else {
          return undef;
        }
      }
    }
    if ($current->{'parent'}) {
      $current = $current->{'parent'};
    } else {
      return undef;
    }
  }
  # Should never get there
  return undef;
}

# for debugging
sub _print_current($)
{
  my $current = shift;
  if (ref($current) ne 'HASH') {
    return  "_print_current: $current not a hash\n";
  }
  my $type = '';
  my $cmd = '';
  my $parent_string = '';
  my $text = '';
  $type = "($current->{'type'})" if (defined($current->{'type'}));
  $cmd = "\@$current->{'cmdname'}" if (defined($current->{'cmdname'}));
  $cmd .= "($current->{'level'})" if (defined($current->{'level'}));
  $text = "[text: $current->{'text'}]" if (defined($current->{'text'}));
  if ($current->{'parent'}) {
    my $parent = $current->{'parent'};
    my $parent_cmd = '';
    my $parent_type = '';
    $parent_cmd = "\@$parent->{'cmdname'}" if (defined($parent->{'cmdname'}));
    $parent_type = "($parent->{'type'})" if (defined($parent->{'type'}));
    $parent_string = " <- $parent_cmd$parent_type\n";
  }
  my $args = '';
  my $contents = '';
  $args = "args(".scalar(@{$current->{'args'}}).')' if $current->{'args'};
  $contents = "contents(".scalar(@{$current->{'contents'}}).')'
    if $current->{'contents'};
  if ("$cmd$type" ne '') {
    return "$cmd$type : $text $args $contents\n$parent_string";
  } else {
    return "$text $args $contents\n$parent_string";
  }
}

sub move_index_entries_after_items($) {
  # enumerate or itemize
  my $current = shift;

  return unless ($current->{'contents'});

  my $previous;
  foreach my $item (@{$current->{'contents'}}) {
    #print STDERR "Before proceeding: $previous $item->{'cmdname'} (@{$previous->{'contents'}})\n" if ($previous and $previous->{'contents'});
    if (defined($previous) and $item->{'cmdname'} 
        and $item->{'cmdname'} eq 'item' 
        and $previous->{'contents'} and scalar(@{$previous->{'contents'}})) {

      my $previous_ending_container;
      if ($previous->{'contents'}->[-1]->{'type'}
          and ($previous->{'contents'}->[-1]->{'type'} eq 'paragraph'
               or $previous->{'contents'}->[-1]->{'type'} eq 'preformatted')) {
        $previous_ending_container = $previous->{'contents'}->[-1];
      } else {
        $previous_ending_container = $previous;
      }

      my @gathered_index_entries;

      #print STDERR "Gathering for item $item in previous $previous ($previous_ending_container)\n";
      while ($previous_ending_container->{'contents'}->[-1]
             and (($previous_ending_container->{'contents'}->[-1]->{'type'}
                   and $previous_ending_container->{'contents'}->[-1]->{'type'} eq 'index_entry_command')
                  or ($previous_ending_container->{'contents'}->[-1]->{'cmdname'}
                      and ($previous_ending_container->{'contents'}->[-1]->{'cmdname'} eq 'c'
                           or $previous_ending_container->{'contents'}->[-1]->{'cmdname'} eq 'comment')))) {
        unshift @gathered_index_entries, pop @{$previous_ending_container->{'contents'}};
      }
      #print STDERR "Gathered: @gathered_index_entries\n";
      if (scalar(@gathered_index_entries)) {
        # put back leading comments
        while ($gathered_index_entries[0]
               and (!$gathered_index_entries[0]->{'type'}
                    or $gathered_index_entries[0]->{'type'} ne 'index_entry_command')) {
          #print STDERR "Putting back $gathered_index_entries[0] $gathered_index_entries[0]->{'cmdname'}\n";
          push @{$previous_ending_container->{'contents'}}, 
             shift @gathered_index_entries;
        }

        # We have the index entries of the previous @item or before item.
        # Now put them right after the current @item command.
        if (scalar(@gathered_index_entries)) {
          my $item_container;
          if ($item->{'contents'} and $item->{'contents'}->[0]
              and $item->{'contents'}->[0]->{'type'}
              and $item->{'contents'}->[0]->{'type'} eq 'preformatted') {
            $item_container = $item->{'contents'}->[0];
          } else {
            $item_container = $item;
          }
          foreach my $entry(@gathered_index_entries) {
            $entry->{'parent'} = $item_container;
          }
          if ($item->{'extra'} 
              and $item->{'extra'}->{'spaces_before_argument'}
       and $item->{'extra'}->{'spaces_before_argument'} !~ /\n$/) {
            $item->{'extra'}->{'spaces_before_argument'} .= "\n";
          # TODO: could we delete all these cases down here?
          } elsif ($item_container->{'contents'} 
              and $item_container->{'contents'}->[0]
              and $item_container->{'contents'}->[0]->{'type'}) {
            if ($item_container->{'contents'}->[0]->{'type'} eq 'empty_line_after_command') {
              unshift @gathered_index_entries, shift @{$item_container->{'contents'}};
            } elsif ($item_container->{'contents'}->[0]->{'type'} eq 'empty_spaces_after_command') {
               unshift @gathered_index_entries, shift @{$item_container->{'contents'}};
               $gathered_index_entries[0]->{'type'} = 'empty_line_after_command';
               $gathered_index_entries[0]->{'text'} .= "\n";
            }
          }
          unshift @{$item_container->{'contents'}}, @gathered_index_entries;
        }
      }
    }
    $previous = $item;
  }
}

sub _move_index_entries_after_items($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  if ($current->{'cmdname'} and ($current->{'cmdname'} eq 'enumerate'
                                 or $current->{'cmdname'} eq 'itemize')) {
    move_index_entries_after_items($current);
  }
  return ($current);
}

sub move_index_entries_after_items_in_tree($)
{
  my $tree = shift;
  return modify_tree(undef, $tree, \&_move_index_entries_after_items);
}

sub _relate_index_entry_to_table_entry($)
{
  my $current = shift; # table_entry

  my ($table_term, $table_item, $item);

  if ($current->{'contents'}
        and $current->{'contents'}->[0]
        and $current->{'contents'}->[0]->{'type'} eq 'table_term') {
    $table_term = $current->{'contents'}->[0];
  }

  if ($current->{'contents'}
        and $current->{'contents'}->[1]
        and $current->{'contents'}->[1]->{'type'} eq 'table_item') {
    $table_item = $current->{'contents'}->[1];
  }

  if ($table_term->{'contents'}
    and $table_term->{'contents'}->[0]
    and (!$table_term->{'contents'}->[0]->{'extra'}
          or !$table_term->{'contents'}->[0]->{'extra'}->{'index_entry'})) {
    $item = $table_term->{'contents'}->[0];
  }

  return if !$table_term or !$table_item or !$item;

  if ($table_item->{'contents'}
    and $table_item->{'contents'}->[0]
    and $table_item->{'contents'}->[0]->{'type'}
    and $table_item->{'contents'}->[0]->{'type'} eq 'index_entry_command') {
      my $index_command = shift @{$table_item->{'contents'}};
      delete $index_command->{'parent'};
      $item->{'extra'}->{'index_entry'}
        = $index_command->{'extra'}->{'index_entry'};
      $item->{'extra'}->{'index_entry'}->{'command'} = $item;
  }
}

sub _relate_index_entries_to_table_entries_in_tree($$$)
{
  my ($self, $type, $current) = @_;

  if ($current->{'type'} and ($current->{'type'} eq 'table_entry')) {
    _relate_index_entry_to_table_entry($current);
  }
  return ($current);
}

sub relate_index_entries_to_table_entries_in_tree($)
{
  my $tree = shift;  
  return modify_tree(undef, $tree,
                     \&_relate_index_entries_to_table_entries_in_tree);
}


sub debug_list
{
  my ($label) = shift;
  my (@list) = (ref $_[0] && $_[0] =~ /.*ARRAY.*/) ? @{$_[0]} : @_;

  my $str = "$label: [";
  my @items = ();
  for my $item (@list) {
    $item = "" if ! defined ($item);
    $item =~ s/\n/\\n/g;
    push (@items, $item);
  }
  $str .= join (" ", @items);
  $str .= "]";

  warn "$str\n";
}
#
sub debug_hash
{
  my ($label) = shift;
  my (%hash) = (ref $_[0] && $_[0] =~ /.*HASH.*/) ? %{$_[0]} : @_;

  my $str = "$label: {";
  my @items = ();
  for my $key (sort keys %hash) {
    my $val = $hash{$key} || ""; # no undef
    $key =~ s/\n/\\n/g;
    $val =~ s/\n/\\n/g;
    push (@items, "$key:$val");
  }
  $str .= join (",", @items);
  $str .= "}";

  warn "$str\n";
}

use Data::Dumper;

my @kept_keys = ('contents', 'cmdname', 'type', 'text', 'args',
  'extra', 'def_role', 'spaces_before_argument',
  'spaces_after_argument', 'comment_at_end', 'index_entry'
);
my %kept_keys;
foreach my $key (@kept_keys) {
  $kept_keys{$key} = 1;
}
sub _filter_print_keys { [grep {$kept_keys{$_}} ( sort keys %{$_[0]} )] };
sub print_tree($)
{
  my $tree = shift;
  local $Data::Dumper::Sortkeys = \&_filter_print_keys;
  local $Data::Dumper::Purity = 1;
  local $Data::Dumper::Indent = 1;

  return Data::Dumper->Dump([$tree]);
}

# common parser functions

sub _non_bracketed_contents {
  my $current = shift;

  if ($current->{'type'} and $current->{'type'} eq 'bracketed') {
    my $new = {};
    $new->{'contents'} = $current->{'contents'} if ($current->{'parent'});
    $new->{'parent'} = $current->{'parent'} if ($current->{'parent'});
    return $new;
  } else {
    return $current;
  }
}

# In a handful of cases, we delay storing the contents of the
# index entry until now to avoid needing Texinfo::Report::gdt
# in the main code of Parser.pm.  Also set 'in_code' value on
# index entries.

sub complete_indices {
  my $self = shift;

  my ($index_entry, $index_contents_normalized);
    
  my $save_lang = $self->get_conf('documentlanguage');

  foreach my $index_name (keys(%{$self->{'index_names'}})) {
    next if !defined $self->{'index_names'}->{$index_name}->{'index_entries'};
    foreach my $entry (@{$self->{'index_names'}->{$index_name}->{'index_entries'}}) {
      $entry->{'in_code'} = $self->{'index_names'}->{$index_name}->{'in_code'};
      
      if (!defined $entry->{'content'}) {
        my $def_command = $entry->{'command'}->{'extra'}->{'def_command'};

        my $def_parsed_hash = $entry->{'command'}->{'extra'}->{'def_parsed_hash'}; 
        if ($def_parsed_hash and $def_parsed_hash->{'class'}
            and $def_command) {
          # Use the document language that was current when the command was
          # used for getting the translation.
          $self->{'documentlanguage'} = $entry->{'command'}->{'extra'}->{'documentlanguage'};
          delete $entry->{'command'}->{'extra'}->{'documentlanguage'};
          if ($def_command eq 'defop'
              or $def_command eq 'deftypeop'
              or $def_command eq 'defmethod'
              or $def_command eq 'deftypemethod') {
            $index_entry = $self->gdt('{name} on {class}',
                                  {'name' => $def_parsed_hash->{'name'},
                                   'class' => $def_parsed_hash->{'class'}});
           $index_contents_normalized
             = [_non_bracketed_contents($def_parsed_hash->{'name'}),
                { 'text' => ' on '},
                _non_bracketed_contents($def_parsed_hash->{'class'})];
          } elsif ($def_command eq 'defivar'
                   or $def_command eq 'deftypeivar'
                   or $def_command eq 'deftypecv') {
            $index_entry = $self->gdt('{name} of {class}',
                                     {'name' => $def_parsed_hash->{'name'},
                                     'class' => $def_parsed_hash->{'class'}});
            $index_contents_normalized
              = [_non_bracketed_contents($def_parsed_hash->{'name'}),
                 { 'text' => ' of '},
                 _non_bracketed_contents($def_parsed_hash->{'class'})];
          }
        }
        # 'root_line' is the container returned by gdt.
        if ($index_entry->{'type'} and $index_entry->{'type'} eq 'root_line') {
          for my $child (@{$index_entry->{'contents'}}) {
            delete $child->{'parent'};
          }
        }
        if ($index_entry->{'contents'}) {
          $entry->{'content'} = [@{$index_entry->{'contents'}}];
          $entry->{'content_normalized'} = $index_contents_normalized;
        }
      }
    }
  }
  $self->{'documentlanguage'} = $save_lang;
}

# Called from Texinfo::Parser and Texinfo::XS::parsetexi::Parsetexi.
sub labels_information
{
  my $self = shift;
  if (defined $self->{'targets'}) {
    my %labels = ();
    for my $target (@{$self->{'targets'}}) {
      if ($target->{'cmdname'} eq 'node') {
        if ($target->{'extra'}->{'nodes_manuals'}) {
          for my $node_manual (@{$target->{'extra'}{'nodes_manuals'}}) {
            if (defined $node_manual
                  and defined $node_manual->{'node_content'}) {
              my $normalized = Texinfo::Convert::NodeNameNormalization::normalize_node({'contents' => $node_manual->{'node_content'}});
              $node_manual->{'normalized'} = $normalized;
            }
          }
        }
      }
      if (defined $target->{'extra'}
            and defined $target->{'extra'}->{'node_content'}) {
        my $normalized = Texinfo::Convert::NodeNameNormalization::normalize_node({'contents' => $target->{'extra'}->{'node_content'}});

        if ($normalized !~ /[^-]/) {
          $self->line_error (sprintf(__("empty node name after expansion `%s'"),
                Texinfo::Convert::Texinfo::convert({'contents' 
                               => $target->{'extra'}->{'node_content'}})), 
                $target->{'line_nr'});
          delete $target->{'extra'}->{'node_content'};
        } else {
          if (defined $labels{$normalized}) {
            $self->line_error(
              sprintf(__("\@%s `%s' previously defined"), 
                         $target->{'cmdname'}, 
                   Texinfo::Convert::Texinfo::convert({'contents' => 
                       $target->{'extra'}->{'node_content'}})), 
                           $target->{'line_nr'});
            $self->line_error(
              sprintf(__("here is the previous definition as \@%s"),
                               $labels{$normalized}->{'cmdname'}),
                       $labels{$normalized}->{'line_nr'});
            delete $target->{'extra'}->{'node_content'};
          } else {
            $labels{$normalized} = $target;
            $target->{'extra'}->{'normalized'} = $normalized;
            if ($target->{'cmdname'} eq 'node') {
              if ($target->{'extra'}
                  and $target->{'extra'}{'node_argument'}) {
                $target->{'extra'}{'node_argument'}{'normalized'}
                  = $normalized;
              }
              push @{$self->{'nodes'}}, $target;
            }
          }
        }
      } else {
        if ($target->{'cmdname'} eq 'node') {
          $self->line_error (sprintf(__("empty argument in \@%s"),
                  $target->{'cmdname'}), $target->{'line_nr'});
          delete $target->{'extra'}->{'node_content'};
        }
      }
    }
    $self->{'labels'} = \%labels;
    delete $self->{'targets'};
  }
  return $self->{'labels'};
}

1;

__END__

=head1 NAME

Texinfo::Common - Classification of commands and miscellaneous methods

=head1 SYNOPSIS

  use Texinfo::Common qw(expand_today expand_verbatiminclude);
  if ($Texinfo::Common::accent_commands{$a_command}) {
    print STDERR "$a_command is an accent command\n";
  }
  
  my $today_tree = expand_today($converter);
  my $verbatiminclude_tree 
     = expand_verbatiminclude(undef, $verbatiminclude);

=head1 DESCRIPTION

Texinfo::Common holds interesting hashes classifying Texinfo @-commands,
as well as miscellaneous methods that may be useful for any backend
converting texinfo trees.

It also defines, as our variable a hash for default indices,
named C<%index_names>.  The format of this hash is described in 
L<Texinfo::Parser/indices_information>.

=head1 COMMAND CLASSES

Hashes are defined as C<our> variables, and are therefore available
outside of the module.

The key of the hashes are @-command names without the @.  The 
following hashes are available:

=over

=item %all_commands

All the @-commands.

=item %no_brace_commands

Commands without brace with a single character as name, like C<*>
or C<:>.  The value is an ascii representation of the command.  It
may be an empty string.

=item %misc_commands

Command that do not take braces and are not block commands either, like
C<@node>, C<@chapter>, C<@cindex>, C<@deffnx>, C<@end>, C<@footnotestyle>, 
C<@set>, C<@settitle>, C<@indent>, C<@definfoenclose>, C<@comment> and many 
others.

=item %default_index_commands

Index entry commands corresponding to default indices. For example 
C<@cindex>.

=item %root_commands

Commands that are at the root of a Texinfo document, namely
C<@node> and sectioning commands, except heading commands.

=item %sectioning_commands

All the sectioning and heading commands.

=item %brace_commands

The commands that take braces.  The associated value is the maximum
number of arguments.

=item %letter_no_arg_commands

@-commands with braces but no argument corresponding to letters, 
like C<@AA{}> or C<@ss{}> or C<@o{}>.

=item %accent_commands

Accent @-commands taking an argument, like C<@'> or C<@ringaccent> 
including C<@dotless> and C<@tieaccent>.

=item %style_commands

Commands that mark a fragment of texinfo, like C<@strong>,
C<@cite>, C<@code> or C<@asis>.

=item %code_style_commands

I<style_commands> that have their argument in code style, like 
C<@code>.

=item %regular_font_style_commands

I<style_commands> that have their argument in regular font, like
C<@r> or C<@slanted>.

=item %context_brace_commands

@-commands with brace like C<@footnote>, C<@caption> and C<@math>
whose argument is outside of the main text flow in one way or another.

=item %ref_commands

Cross reference @-command referencing nodes, like C<@xref>.

=item %explained_commands

@-commands whose second argument explain first argument and further
@-command call without first argument, as C<@abbr> and C<@acronym>.

=item %block commands

Commands delimiting a block with a closing C<@end>.  The value
is I<conditional> for C<@if> commands, I<def> for definition
commands like C<@deffn>, I<raw> for @-commands that have no expansion
of @-commands in their bodies and I<multitable> for C<@multitable>.  
Otherwise it is set to the number of arguments separated by commas 
that may appear on the @-command line. That means 0 in most cases, 
1 for C<@quotation> and 2 for C<@float>.

=item %raw_commands

@-commands that have no expansion of @-commands in their bodies,
as C<@macro>, C<@verbatim> or C<@ignore>.

=item %format_raw_commands

@-commands associated with raw output format, like C<@html>, or
C<@docbook>.

=item %math_commands

@-commands which contains math, like C<@math> or C<@displaymath>.

=item %texinfo_output_formats

Cannonical output formats that have associated conditionals.  In
practice C<%format_raw_commands> plus C<info> and C<plaintext>.

=item %def_commands

=item %def_aliases

Definition commands.  C<%def_aliases> associates an aliased command
to the original command, for example C<defun> is associated to C<deffn>.

=item %menu_commands

@-commands with menu entries.

=item %align_commands

@-commands related with alignement of text.

=item %region_commands

Block @-commands that enclose full text regions, like C<@titlepage>.

=item %preformatted_commands

=item %preformatted_code_commands

I<%preformatted_commands> is for commands whose content should not 
be filled, like C<@example> or C<@display>.  If the command is meant 
for code, it is also in I<%preformatted_code_commands>, like C<@example>.

=item %item_container_commands

Commands holding C<@item> with C<@item> that contains blocks of text, 
like C<@itemize>.

=item %item_line_commands

Commands with C<@item> that have their arguments on their lines, like
C<@ftable>.

=back

=head1 METHODS

No method is exported in the default case.

Most methods takes a I<$converter> as argument, sometime optionally, 
to get some information and use methods for error reporting, 
see L<Texinfo::Convert::Converter> and L<Texinfo::Report>.

=over

=item $tree = expand_today($converter)

Expand today's date, as a texinfo tree with translations.

=item $tree = expand_verbatiminclude($converter, $verbatiminclude)

The I<$converter> argument may be undef.  I<$verbatiminclude> is a
C<@verbatiminclude> tree element.  This function returns a 
C<@verbatim> tree elements after finding the included file and
reading it.  If I<$converter> is not defined, the document encoding 
is not taken into account when reading the file.

=item $tree = definition_category($converter, $def_line)

The I<$converter> argument may be undef.  I<$def_line> is a 
C<def_line> texinfo tree container.  This function
returns a texinfo tree corresponding to the category of the
I<$def_line> taking the class into account, if there is one.
If I<$converter> is not defined, the resulting string won't be
translated.

=item $result = is_content_empty($tree, $do_not_ignore_index_entries)

Return true if the C<$tree> has content that could be formatted.
C<$do_not_ignore_index_entries> is optional.  If set, index entries
are considered to be formatted.

=item $result = numbered_heading ($converter, $heading_element, $heading_text, $do_number)

The I<$converter> argument may be undef.  I<$heading_element> is 
a heading command tree element.  I<$heading_text> is the already 
formatted heading text.  if the I<$do_number> optional argument is 
defined and false, no number is used and the text is returned as is.
This function returns the heading with a number and the appendix 
part if needed.  If I<$converter> is not defined, the resulting 
string won't be translated.

=item ($caption, $prepended) = float_name_caption ($converter, $float)

I<$float> is a texinfo tree C<@float> element.  This function 
returns the caption that should be used for the float formatting 
and the I<$prepended> texinfo tree combining the type and label
of the float.

=item $text = enumerate_item_representation($specification, $number)

This function returns the number or letter correponding to item
number I<$number> for an C<@enumerate> specification I<$specification>,
appearing on an C<@enumerate> line.  For example

  enumerate_item_representation('c', 3)

is C<e>.

=item trim_spaces_comment_from_content($contents)

Remove empty spaces after commands or braces at begin and
spaces and comments at end from a content array, modifying it.

=item $normalized_name = normalize_top_node_name ($node_string)

Normalize the node name string given in argument, by normalizing
Top node case.

=item protect_comma_in_tree($tree)

Protect comma characters, replacing C<,> with @comma{} in tree.

=item protect_colon_in_tree($tree)

=item protect_node_after_label_in_tree($tree)

Protect colon with C<protect_colon_in_tree> and characters that 
are special in node names after a label in menu entries (tab
dot and comma) with C<protect_node_after_label_in_tree>.  
The protection is achieved by putting protected characters 
in C<@asis{}>.

=item $contents_result = protect_first_parenthesis ($contents)

Return a contents array reference with first parenthesis in the 
contents array reference protected.

=item protect_hashchar_at_line_beginning($parser, $tree)

Protect hash character at beginning of line if the line is a cpp
line directive.  The I<$parser> argument maybe undef, if it is 
defined it is used for error reporting in case an hash character
could not be protected because it appeared in a raw environment.

=item move_index_entries_after_items_in_tree($tree)

In C<@enumerate> and C<@itemize> from the tree, move index entries 
appearing just before C<@item> after the C<@item>.  Comment lines 
between index entries are moved too.

=item $command = find_parent_root_command($parser, $tree_element)

Find the parent root command of a tree element (sectioning command or node).
The C<$parser> argument is optional, it is used to continue 
through C<@insertcopying> if in a C<@copying>.

=item valid_tree_transformation($name)

Return true if the I<$name> is a known tree transformation name
that may be passed with C<TREE_TRANSFORMATIONS> to modify a texinfo
tree.

=item collect_commands_in_tree($tree, $commands_list)

Returns a hash reference with keys @-commands names specified
in the I<$commands_list> array reference and values arrays of
tree elements corresponding to those @-command found in I<$tree>
by traversing the tree.

=back

=head1 SEE ALSO

L<Texinfo::Parser>, L<Texinfo::Convert::Converter> and L<Texinfo::Report>. 

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=cut
