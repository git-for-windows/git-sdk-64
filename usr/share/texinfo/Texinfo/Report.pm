# Report.pm: prepare error messages and translate strings.
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

require Exporter;
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

%EXPORT_TAGS = ( 'all' => [ qw(
  errors
  gdt
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

package Texinfo::Report;

use 5.00405;
use strict;

# for fileparse
use File::Basename;

use POSIX qw(setlocale LC_ALL);
use Locale::Messages;
# to be able to load a parser if none was given to gdt.
use Texinfo::Parser;

# return the errors and warnings
sub errors($)
{
  my $self = shift;
  return ($self->{'errors_warnings'}, $self->{'error_nrs'});
}

sub new($)
{
  my $self = shift;
  $self->{'errors_warnings'} = [];
  #print STDERR "REPORT NEW $self $self->{'errors_warnings'}\n";
  $self->{'errors_nrs'} = 0;
  return $self;
}

# format a line warning
sub line_warn($$$)
{
  my $self = shift;
  my $text = shift;
  chomp ($text);
  my $line_number = shift;
  return if (!defined($line_number) or $self->{'ignore_notice'});
  my $file = $line_number->{'file_name'};
  # otherwise out of source build fail since the file names are different
  my ($directories, $suffix);
  ($file, $directories, $suffix) = fileparse($file)
    if ($self->get_conf('TEST'));
  my $warn_line;
  if ($line_number->{'macro'} ne '') {
    $warn_line = sprintf(__p("Texinfo source file warning",
                             "%s:%d: warning: %s (possibly involving \@%s)\n"),
             $file, $line_number->{'line_nr'}, $text, $line_number->{'macro'});
  } else {
    $warn_line = sprintf(__p("Texinfo source file warning", 
                                    "%s:%d: warning: %s\n"),
                         $file, $line_number->{'line_nr'}, $text);
  }
  warn $warn_line if ($self->get_conf('DEBUG'));
  push @{$self->{'errors_warnings'}},
       { 'type' => 'warning', 'text' => $text, 'error_line' => $warn_line,
         %{$line_number} };
}

# format a line error
sub line_error($$$)
{
  my $self = shift;
  my $text = shift;
  chomp ($text);
  my $line_number = shift;
  return if ($self->{'ignore_notice'});
  if (defined($line_number)) {
    my $file = $line_number->{'file_name'};
    my ($directories, $suffix);
    ($file, $directories, $suffix) = fileparse($file)
       if ($self->get_conf('TEST'));
    my $macro_text = '';
    $macro_text = " (possibly involving \@$line_number->{'macro'})"
       if ($line_number->{'macro'} ne '');
    my $error_text = "$file:$line_number->{'line_nr'}: $text$macro_text\n";
    warn "$error_text" if ($self->get_conf('DEBUG'));
    push @{$self->{'errors_warnings'}},
         { 'type' => 'error', 'text' => $text, 'error_line' => $error_text,
           %{$line_number} };
  }
  $self->{'error_nrs'}++;
}

sub document_warn($$)
{
  my $self = shift;
  my $text = shift;
  return if ($self->{'ignore_notice'});
  chomp($text);

  my $warn_line;
  if (defined($self->get_conf('PROGRAM')) and $self->get_conf('PROGRAM') ne '') {
    $warn_line = sprintf(__p("whole document warning", "%s: warning: %s\n"), 
                         $self->get_conf('PROGRAM'), $text);
  } else {
    $warn_line = sprintf(__p("whole document warning", "warning: %s\n"), 
                         $text);
  }
  push @{$self->{'errors_warnings'}},
    { 'type' => 'warning', 'text' => $text, 'error_line' => $warn_line };
}

sub document_error($$)
{
  my $self = shift;
  my $text = shift;
  return if ($self->{'ignore_notice'});
  chomp($text);
  my $error_line;
  if (defined($self->get_conf('PROGRAM')) and $self->get_conf('PROGRAM') ne '') {
    $error_line = sprintf("%s: %s\n", $self->get_conf('PROGRAM'), $text);
  } else {
    $error_line = "$text\n";
  }
  push @{$self->{'errors_warnings'}},
    { 'type' => 'error', 'text' => $text, 'error_line' => $error_line };
  $self->{'error_nrs'}++;
}

sub file_line_warn($$$;$)
{
  my $self = shift;
  my $text = shift;
  return if ($self->{'ignore_notice'});
  chomp($text);
  my $file = shift;
  my $line_nr = shift;

  my $warn_line;
  if (!defined($file)) {
    $warn_line = sprintf(__p("file warning", "warning: %s\n"), $text);
  } elsif (!defined($line_nr)) {
    $warn_line = sprintf(__p("file warning", "%s: warning: %s\n"), 
                         $file, $text);
  } else {
    $warn_line = sprintf(__p("file warning", "%s:%d: warning: %s\n"), 
                         $file, $line_nr, $text);
  }
  #print STDERR "REPORT FILE_LINE_WARN $self $self->{'errors_warnings'}\n";
  push @{$self->{'errors_warnings'}},
    { 'type' => 'warning', 'text' => $text, 'error_line' => $warn_line};
}

sub file_line_error($$$;$)
{
  my $self = shift;
  my $text = shift;
  return if ($self->{'ignore_notice'});
  chomp($text);
  my $file = shift;
  my $line_nr = shift;

  my $error_line;
  if (!defined($file)) {
    $error_line = "$text\n";
  } elsif (!defined($line_nr)) {
    $error_line = "$file: $text\n";
  } else {
    $error_line = "$file:$line_nr: $text\n";
  }
  #print STDERR "REPORT FILE_LINE_WARN $self $self->{'errors_warnings'}\n";
  push @{$self->{'errors_warnings'}},
    { 'type' => 'error', 'text' => $text, 'error_line' => $error_line};
  $self->{'error_nrs'}++;
}


# i18n

my $DEFAULT_LANGUAGE = 'en';

# we want a reliable way to switch locale, so we don't use the system
# gettext.
Locale::Messages->select_package ('gettext_pp');

my $strings_textdomain = 'texinfo_document';
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



# libintl converts between encodings but doesn't decode them into the
# perl internal format.  This is only called if the encoding is a proper
# perl encoding.
sub _encode_i18n_string($$)
{
  my $string = shift;
  my $encoding = shift;
  return Encode::decode($encoding, $string);
}

# Get document translation - handle translations of in-document strings.
# Return a parsed Texinfo tree
sub gdt($$;$$)
{
  my ($self, $message, $context, $type) = @_;

  my $re = join '|', map { quotemeta $_ } keys %$context
      if (defined($context) and ref($context));

  my $saved_LC_ALL = POSIX::setlocale(LC_ALL);
  my $saved_LANGUAGE = $ENV{'LANGUAGE'};

  # The following is necessary when the locale is "C" (as is the case
  # when the tests are run), due to the fix for
  #   https://rt.cpan.org/Public/Bug/Display.html?id=81315
  # Translation is not done if LC_MESSAGES is "C" or "POSIX".
  # This may not work if a locale named here doesn't exist on the system.
  # Set LC_ALL rather than LC_MESSAGES as LC_MESSAGES may not be supported
  # on Perl for MS-Windows.
  for my $try ('en_US.UTF-8', 'en_US') {
    my $locale = POSIX::setlocale(LC_ALL, $try);
    last if $locale;
  }

  Locale::Messages::textdomain($strings_textdomain);

  # FIXME do this only once when encoding is seen (or at beginning)
  # instead of here, each time that gdt is called?
  my $encoding;
  if ($self->get_conf('OUTPUT_ENCODING_NAME')) {
    $encoding = $self->get_conf('OUTPUT_ENCODING_NAME');
  }
  Locale::Messages::bind_textdomain_codeset($strings_textdomain, $encoding)
    if ($encoding and $encoding ne 'us-ascii');
  if (!($encoding and $encoding eq 'us-ascii')) {
    my $perl_encoding;
    if ($self->get_conf('OUTPUT_PERL_ENCODING')) {
      $perl_encoding = $self->get_conf('OUTPUT_PERL_ENCODING');
    }
    if ($perl_encoding) {
      Locale::Messages::bind_textdomain_filter($strings_textdomain,
        \&_encode_i18n_string, $perl_encoding);
    }
  }

  # FIXME do this once when @documentlanguage changes (or at beginning)
  # instead of here, each time that gdt is called?
  my $lang = $self->get_conf('documentlanguage');
  $lang = $DEFAULT_LANGUAGE if (!defined($lang));
  my @langs = ($lang);
  if ($lang =~ /^([a-z]+)_([A-Z]+)/) {
    my $main_lang = $1;
    my $region_code = $2;
    push @langs, $main_lang;
  }

  my $locales = '';

  foreach my $language (@langs) {
    if ($encoding) {
      $locales .= "$language.$encoding:";
    } else {
      $locales .= "$language:";
    }
    # always try us-ascii, the charset should always be a subset of
    # all charset, and should resort to @-commands if needed for non
    # ascii characters
    if (!$encoding or ($encoding and $encoding ne 'us-ascii')) {
      $locales .= "$language.us-ascii:";
    }
  }
  $locales =~ s/:$//;
  # END FIXME

  Locale::Messages::nl_putenv("LANGUAGE=$locales");

  my $translation_result = Locale::Messages::gettext($message);

  Locale::Messages::textdomain($messages_textdomain);
  if (!defined($saved_LANGUAGE)) {
    delete ($ENV{'LANGUAGE'});
  } else {
    $ENV{'LANGUAGE'} = $saved_LANGUAGE;
  }
  if (defined($saved_LC_ALL)) {
    POSIX::setlocale(LC_ALL, $saved_LC_ALL);
  } else {
    POSIX::setlocale(LC_ALL, '');
  }

  if ($type and $type eq 'translated_text') {
    if (defined($re)) {
      # next line taken from libintl perl, copyright Guido. sub __expand
      $translation_result =~ s/\{($re)\}/defined $context->{$1} ? $context->{$1} : "{$1}"/ge;
    }
    return $translation_result;
  }

  my $parser_conf;
  # we change the substituted brace-enclosed strings to values, that
  # way they are substituted, including when they are Texinfo trees.
  # a _ is prepended to avoid clashing with other values, although since
  # the parser is a new one there should not be any problem anyway.
  if (defined($re)) {
    # next line taken from libintl perl, copyright Guido. sub __expand
    $translation_result =~ s/\{($re)\}/\@value\{_$1\}/g;
    foreach my $substitution(keys %$context) {
      # Only pass simple string @values to parser.
      if (!ref($context->{$substitution})) {
        $parser_conf->{'values'}->{'_'.$substitution}
        = $context->{$substitution};
      }
    }
  }

  # Don't reuse the current parser itself, as (tested) the parsing goes 
  # wrong, certainly because the parsed text can affect the parser state.
  my $current_parser;
  if (ref($self) eq 'Texinfo::Parser') {
    $current_parser = $self;
  } elsif ($self->{'parser'}) {
    $current_parser = $self->{'parser'};
  }

  if ($current_parser) {
    # not sure 'gettext' could in fact be useful in parser for
    # translated fragments.  'TEST' can be used fot @today{} expansion.
    foreach my $duplicated_conf ('clickstyle', 'kbdinputstyle', 'DEBUG',
                                 'TEST', 'gettext') {
      $parser_conf->{$duplicated_conf} = $current_parser->{$duplicated_conf}
        if (defined($current_parser->{$duplicated_conf}));
    }
  }
  $parser_conf->{'in_gdt'} = 1;
  #my $parser = Texinfo::Parser::parser($parser_conf);
  my $parser = Texinfo::Parser::simple_parser($parser_conf);
  if ($parser->{'DEBUG'}) {
    print STDERR "GDT $translation_result\n";
  }

  my $tree = $parser->parse_texi_line($translation_result);
  $tree = _substitute ($tree, $context);
  return $tree;
}

sub _substitute_element_array ($$);
sub _substitute_element_array ($$) {
  my $array = shift; my $context = shift;

  @{$array} = map {
    if ($_->{'cmdname'} and $_->{'cmdname'} eq 'value') {
      my $name = $_->{'type'};
      $name =~ s/^_//;
      if (ref($context->{$name}) eq 'HASH') {
        $context->{$name};
      } elsif (ref($context->{$name}) eq 'ARRAY') {
        @{$context->{$name}};
      } else {
        (); # undefined - shouldn't happen?
      }
    } else {
      _substitute($_, $context);
      ( $_ );
    }
  } @{$array};
}

# Recursively substitute @value elements in $TREE with their values given
# in $CONTEXT.
sub _substitute ($$);
sub _substitute ($$) {
  my $tree = shift; my $context = shift;

  if ($tree->{'contents'}) {
    _substitute_element_array ($tree->{'contents'}, $context);
  }

  if ($tree->{'args'}) {
    _substitute_element_array ($tree->{'args'}, $context);
  }

  return $tree;
}


1;

__END__

=head1 NAME

Texinfo::Report - Error storing and string translations for Texinfo modules

=head1 SYNOPSIS

  @ISA = qw(Texinfo::Report);

  $converter->Texinfo::Report::new();
  
  if ($warning_happened) {
    $converter->line_warn(sprintf($converter->__("\@%s is wrongly used"),
                       $current->{'cmdname'}), $current->{'line_nr'});
  }
  
  my ($errors, $errors_count) = $converter->errors();
  foreach my $error_message (@$errors) {
    warn $error_message->{'error_line'};
  }

  my $tree_translated = $converter->gdt('See {reference} in @cite{{book}}',
                       {'reference' => $tree_reference,
                        'book'  => {'text' => $book_name}});


=head1 DESCRIPTION

The Texinfo::Report module helps with string translations and error 
handling.  It is used by the Texinfo modules Texinfo::Parser and 
Texinfo::Convert::Converter.  To use this module, the usual way is
to inherit Texinfo::Report methods and initialize the Texinfo::Report
variables for a I<$converter> object. This is done by calling 
C<Texinfo::Report::new()> on the I<$converter> object.  This is done by 
Texinfo::Convert::Converter, for instance, so every module that inherits
Texinfo::Convert::Converter can automatically use the Texinfo::Report
methods in an object-oriented way.

Besides the C<new> method, C<gdt> is used for string translations, 
C<errors> for reporting errors, and the other methods to store errors
(and warnings).

=head1 METHODS

No method is exported in the default case.  

The C<new> method initializes Texinfo::Report related fields:

  $converter->Texinfo::Report::new()

The C<gdt> method is used to translate strings to be output in 
converted documents, and return a texinfo tree.

=over

=item $tree = $converter->gdt($string, $replaced_substrings, $mode)

The I<$string> is a string to be translated.  In the default case, 
the function returns a Texinfo tree, as the string is 
interpreted as Texinfo code after
translation.  I<$replaced_substrings> is an optional 
hash reference specifying some 
substitution to be done after the translation.  The key of 
the I<$replaced_substrings> hash reference identifies what is to 
be substituted, and the value is some string, texinfo tree or array content 
that is substituted in the resulting texinfo tree.
In the string to be translated word in brace matching keys of 
I<$replaced_substrings> are replaced.

I<$mode> is an optional string which may modify how the function
behaves.  The possible values are

=over 

=item translated_text

In that case the string is not considered to be Texinfo, a plain string
that is returned after translation and substitution.  The substitutions
may only be strings in that case.

=back

For example in the following call, the string 
I<See {reference} in @cite{{book}}> is translated, then
parsed as a Texinfo string, with I<{reference}> substituted by
I<$tree_reference> in the resulting tree, and I<{book}> 
replaced by the associated texinfo tree text element:

  $tree = $converter->gdt('See {reference} in @cite{{book}}',
                       {'reference' => $tree_reference,
                        'book'  => {'text' => $book_name}});

C<gdt> uses the information in the I<$converter> to know the
encoding and documentlanguage.  More precisely, 
C<< $converter->{'encoding_name'} >>, C<< $converter->{'perl_encoding'} >>
and C<< $converter->get_conf('documentlanguage') >> are used.

C<gdt> uses a gettext-like infrastructure to retrieve the 
translated strings, using the I<texinfo_document> domain.

=back

The errors collected are available through the C<errors> method, the other
methods allow registering errors and warnings.

=over

=item ($error_warnings_list, $error_count) = errors ($converter)

This function returns as I<$error_count> the count of errors since
calling C<new>.  The I<$error_warnings_list> is an array of hash references
one for each error, warning or error line continuation.  Each of these has 
the following keys:

=over

=item type

May be C<warning>, or C<error>.

=item text

The text of the error.

=item error_line

The text of the error formatted with the file name, line number and macro
name, as needed.

=item line_nr

The line number of the error or warning.

=item file_name

The file name where the error or warning occurs.

=item macro

The user macro name that is expanded at the location of 
the error or warning.

=back

=item $converter->line_warn($text, $line_nr)

=item $converter->line_error($text, $line_nr)

Register a warning or an error.  The I<$text> is the text of the
error or warning.  The optional I<$line_nr> holds the information
on the error or warning location.  It is associated with the I<line_nr> 
key of Texinfo tree elements as described in L<Texinfo::Parser/line_nr>
for the @-commands.  The I<$line_nr> structure is described in L<errors|($error_warnings_list, $error_count) = errors ($converter)>
above.

=item $converter->document_warn($text)

=item $converter->document_error($text)

Register a document-wide error or warning.  I<$text> is the error or
warning message.

=item $converter->file_line_warn($text, $file, $line_nr)

Register the warning message I<$text> for file I<$file>, with, optionally
the line I<$line_nr> in the file.

=item $converter->file_line_error($text, $file, $line_nr)

Register the error message I<$text> for file I<$file>, with, optionally
the line I<$line_nr> in the file.

=back

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=cut
