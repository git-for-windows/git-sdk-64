#! /usr/bin/env perl

# gpinyin - European-like Chinese writing 'pinyin' into 'groff'

# Source file position: <groff-source>/contrib/gpinyin/gpinyin.pl
# Installed position: <prefix>/bin/gpinyin

# Copyright (C) 2014-2018 Free Software Foundation, Inc.

# Written by Bernd Warken <groff-bernd.warken-72@web.de>.

# This file is part of 'gpinyin', which is part of 'groff'.

# 'groff' is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.

# 'groff' is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You can find a copy of the GNU General Public License in the internet
# at <http://www.gnu.org/licenses/gpl-2.0.html>.

########################################################################

########################################################################
# All Pinyin syllables from wikipedia
########################################################################

my %syllables =
  (
   'a' => 1, 'ai' => 1, 'an' => 1, 'ang' => 1, 'ao' => 1,
   'ba' => 1, 'bai' => 1, 'ban' => 1, 'bang' => 1, 'bao' => 1,
   'bei' => 1, 'ben' => 1, 'beng' => 1,
   'bi' => 1, 'bian' => 1, 'biao' => 1, 'bie' => 1, 'bin' => 1,
   'bing' => 1, 'bo' => 1, 'bu' => 1,
   'ca' => 1, 'cai' => 1, 'can' => 1, 'cang' => 1, 'cao' => 1,
   'ce' => 1, 'cen' => 1, 'ceng' => 1,
   'cha' => 1, 'chai' => 1, 'chan' => 1, 'chang' => 1, 'chao' => 1,
   'che' => 1, 'chen' => 1, 'cheng' => 1, 'chi' => 1,
   'chong' => 1, 'chou' => 1, 'chu' => 1,
   'chua' => 1, 'chuai' => 1, 'chuan' => 1, 'chuang' => 1,
   'chui' => 1, 'chun' => 1, 'chuo' => 1,
   'ci' => 1, 'cong' => 1, 'cou' => 1,
   'cu' => 1, 'cuan' => 1, 'cui' => 1, 'cun' => 1, 'cuo' => 1,
   'da' => 1, 'dai' => 1, 'dan' => 1, 'dang' => 1, 'dao' => 1,
   'de' => 1, 'dei' => 1, 'den' => 1, 'deng' => 1,
   'di' => 1, 'dian' => 1, 'diao' => 1, 'die' => 1,
   'ding' => 1, 'diu' => 1, 'dong' => 1, 'dou' => 1,
   'du' => 1, 'duan' => 1, 'dui' => 1, 'dun' => 1, 'duo' => 1,
   'e' => 1, 'ei' => 1, 'en' => 1,  'eng' => 1, 'er' => 1,
   'fa' => 1, 'fan' => 1, 'fang' => 1,
   'fei' => 1, 'fen' => 1, 'feng' => 1, 'fiao' => 1,
   'fo' => 1, 'fou' => 1, 'fu' => 1,
   'ga' => 1, 'gai' => 1, 'gan' => 1, 'gang' => 1, 'gao' => 1,
   'ge' => 1, 'gei' => 1, 'gen' => 1, 'geng' => 1,
   'gong' => 1, 'gou' => 1, 'gu' => 1,
   'gua' => 1, 'guai' => 1, 'guan' => 1, 'guang' => 1, 'gui' => 1,
   'gun' => 1, 'guo' => 1,
   'ha' => 1, 'hai' => 1, 'han' => 1, 'hang' => 1, 'hao' => 1,
   'he' => 1, 'hei' => 1, 'hen' => 1, 'heng' => 1,
   'hong' => 1, 'hou' => 1,
   'hu' => 1, 'hua' => 1, 'huai' => 1, 'huan' => 1, 'huang' => 1,
   'hui' => 1, 'hun' => 1, 'huo' => 1,
   'ji' => 1, 'jia' => 1, 'jian' => 1, 'jiang' => 1, 'jiao' => 1,
   'jie' => 1, 'jin' => 1, 'jing' => 1, 'jiong' => 1, 'jiu' => 1,
   'ju' => 1, 'juan' => 1, 'jue' => 1, 'jun' => 1,
   'ka' => 1, 'kai' => 1, 'kan' => 1, 'kang' => 1, 'kao' => 1,
   'ke' => 1, 'kei' => 1, 'ken' => 1, 'keng' => 1,
   'kong' => 1, 'kou' => 1, 'ku' => 1, 'kua' => 1, 'kuai' => 1,
   'kuan' => 1, 'kuang' => 1, 'kui' => 1, 'kun' => 1, 'kuo' => 1,
   'la' => 1, 'lai' => 1, 'lan' => 1, 'lang' => 1, 'lao' => 1,
   'le' => 1, 'lei' => 1, 'leng' => 1,
   'li' => 1, 'lia' => 1, 'lian' => 1, 'liang' => 1, 'liao' => 1,
   'lie' => 1, 'lin' => 1, 'ling' => 1, 'liu' => 1,
   'lo' => 1, 'long' => 1, 'lou' => 1,
   'lu' => 1, 'luan' => 1, 'lun' => 1, 'luo' => 1,
   'lü' => 1, 'lüe' => 1,
   'ma' => 1, 'mai' => 1, 'man' => 1, 'mang' => 1, 'mao' => 1,
   'me' => 1, 'mei' => 1, 'men' => 1, 'meng' => 1,
   'mi' => 1, 'mian' => 1, 'miao' => 1, 'mie' => 1,
   'min' => 1, 'ming' => 1, 'miu' => 1,
   'mo' => 1, 'mou' => 1, 'mu' => 1,
   'na' => 1, 'nai' => 1, 'nan' => 1, 'nang' => 1, 'nao' => 1,
   'ne' => 1, 'nei' => 1, 'nen' => 1, 'neng' => 1,
   'ni' => 1, 'nian' => 1, 'niang' => 1, 'niao' => 1, 'nie' => 1,
   'nin' => 1, 'ning' => 1, 'niu' => 1, 'nong' => 1, 'nou' => 1,
   'nu' => 1, 'nuan' => 1, 'nun' => 1, 'nuo' => 1,
   'nü' => 1, 'nüe' => 1,
   'o' => 1, 'ong' => 1, 'ou' => 1,
   'pa' => 1, 'pai' => 1, 'pan' => 1, 'pang' => 1, 'pao' => 1,
   'pei' => 1, 'pen' => 1, 'peng' => 1,
   'pi' => 1, 'pian' => 1, 'piao' => 1, 'pie' => 1, 'pin' => 1,
   'ping' => 1, 'po' => 1, 'pou' => 1, 'pu' => 1,
   'qi' => 1, 'qia' => 1, 'qian' => 1, 'qiang' => 1, 'qiao' => 1, 'qie' => 1,
   'qin' => 1, 'qing' => 1, 'qiong' => 1, 'qiu' => 1,
   'qu' => 1, 'quan' => 1, 'que' => 1, 'qun' => 1,
   'ran' => 1, 'rang' => 1, 'rao' => 1, 're' => 1, 'ren' => 1,
   'ri' => 1, 'rong' => 1, 'rou' => 1,
   'ru' => 1, 'ruan' => 1, 'rui' => 1, 'run' => 1, 'ruo' => 1,
   'sa' => 1, 'sai' => 1, 'san' => 1, 'sang' => 1, 'sao' => 1,
   'se' => 1, 'sen' => 1, 'seng' => 1,
   'sha' => 1, 'shai' => 1, 'shan' => 1, 'shang' => 1, 'shao' => 1,
   'she' => 1, 'shei' => 1, 'shen' => 1, 'sheng' => 1, 'shi' => 1,
   'shou' => 1, 'shu' => 1, 'shua' => 1, 'shuai' => 1, 'shuan' => 1,
   'shuang' => 1, 'shui' => 1, 'shun' => 1, 'shuo' => 1,
   'si' => 1, 'song' => 1, 'sou' => 1, 'su' => 1, 'suan' => 1, 'sui' => 1,
   'sun' => 1, 'suo' => 1,
   'ta' => 1, 'tai' => 1, 'tan' => 1, 'tang' => 1, 'tao' => 1,
   'te' => 1, 'teng' => 1,
   'ti' => 1, 'tian' => 1, 'tiao' => 1, 'tie' => 1, 'ting' => 1,
   'tong' => 1, 'tou' => 1,
   'tu' => 1, 'tuan' => 1, 'tui' => 1, 'tun' => 1, 'tuo' => 1,
   'wa' => 1, 'wai' => 1, 'wan' => 1, 'wang' => 1,
   'wei' => 1, 'wen' => 1, 'weng' => 1, 'wo' => 1, 'wu' => 1,
   'xi' => 1, 'xia' => 1, 'xian' => 1, 'xiang' => 1, 'xiao' => 1,
   'xie' => 1, 'xin' => 1, 'xing' => 1, 'xiong' => 1, 'xiu' => 1,
   'xu' => 1, 'xuan' => 1, 'xue' => 1, 'xun' => 1,
   'ya' => 1, 'yai' => 1, 'yan' => 1, 'yang' => 1, 'yao' => 1,
   'ye' => 1, 'yi' => 1, 'yin' => 1, 'ying' => 1,
   'yo' => 1, 'yong' => 1, 'you' => 1,
   'yu' => 1, 'yuan' => 1, 'yue' => 1, 'yun' => 1,
   'za' => 1, 'zai' => 1, 'zan' => 1, 'zang' => 1, 'zao' => 1,
   'ze' => 1, 'zei' => 1, 'zen' => 1, 'zeng' => 1,
   'zha' => 1, 'zhai' => 1, 'zhan' => 1, 'zhang' => 1, 'zhao' => 1,
   'zhe' => 1, 'zhei' => 1, 'zhen' => 1, 'zheng' => 1, 'zhi' => 1,
   'zhong' => 1, 'zhou' => 1, 'zhu' => 1, 'zhua' => 1, 'zhuai' => 1,
   'zhuan' => 1, 'zhuang' => 1, 'zhui' => 1, 'zhun' => 1, 'zhuo' => 1,
   'zi' => 1, 'zong' => 1, 'zou' => 1, 'zu' => 1, 'zuan' => 1,
   'zui' => 1, 'zun' => 1, 'zuo' => 1,
  );

########################################################################
# Unicode variables for utf8 tty (nroff)
########################################################################

my %tones1_Unicode =
  (
   'A' => q(\\[u0100]),
   'E' => q(\\[u0112]),
   'I' => q(\\[u012A]),
   'O' => q(\\[u014C]),
   'U' => q(\\[u016A]),
   'Ü' => q(\\[u016A]),
   'a' => q(\\[u0101]),
   'e' => q(\\[u0113]),
   'i' => q(\\[u012B]),
   'o' => q(\\[u014D]),
   'u' => q(\\[u016B]),
   'ü' => q(\\[u01D6]),
  );

my %tones2_Unicode =
  (
   'A' => q(\\[u00C1]),
   'E' => q(\\[u00C9]),
   'I' => q(\\[u00CD]),
   'O' => q(\\[u00D3]),
   'U' => q(\\[u00DA]),
   'Ü' => q(\\[u01D7]),
   'a' => q(\\[u00E1]),
   'e' => q(\\[u00E9]),
   'i' => q(\\[u00ED]),
   'o' => q(\\[u00F3]),
   'u' => q(\\[u00FA]),
   'ü' => q(\\[u01D8]),
  );

my %tones3_Unicode =
  (
   'A' => q(\\[u01CD]),
   'E' => q(\\[u011A]),
   'I' => q(\\[u01CF]),
   'O' => q(\\[u01D1]),
   'U' => q(\\[u01D3]),
   'Ü' => q(\\[u01D9]),
   'a' => q(\\[u01CE]),
   'e' => q(\\[u011B]),
   'i' => q(\\[u01D0]),
   'o' => q(\\[u01D2]),
   'u' => q(\\[u01D4]),
   'ü' => q(\\[u01DA]),
  );

my %tones4_Unicode =
  (
   'A' => q(\\[u00C0]),
   'E' => q(\\[u00C8]),
   'I' => q(\\[u00CC]),
   'O' => q(\\[u00D2]),
   'U' => q(\\[u00D9]),
   'Ü' => q(\\[u01DB]),
   'a' => q(\\[u00E0]),
   'e' => q(\\[u00E8]),
   'i' => q(\\[u00EC]),
   'o' => q(\\[u00F2]),
   'u' => q(\\[u00F9]),
   'ü' => q(\\[u01DC]),
  );


########################################################################
# glyph variables for troff
########################################################################

#my $tone1_macron = '\\[a-]';
#my $tone2_acute = '\\[aa]';
#my $tone3_caron = '\\[ah]';
#my $tone4_grave = '\\[ga]';
my @accents = ( '', '\\[a-]', '\\[aa]', '\\[ah]', '\\[ga]', );

my %tones2_glyphs =
  (
   'A' => q(\\['A]),
   'E' => q(\\['E]),
   'I' => q(\\['I]),
   'O' => q(\\['O]),
   'U' => q(\\['U]),
   'a' => q(\\['a]),
   'e' => q(\\['e]),
   'i' => q(\\['i]),
   'o' => q(\\['o]),
   'u' => q(\\['u]),
  );

my %tones4_glyphs =
  (
   'A' => q(\\[`A]),
   'E' => q(\\[`E]),
   'I' => q(\\[`I]),
   'O' => q(\\[`O]),
   'U' => q(\\[`U]),
   'a' => q(\\[`a]),
   'e' => q(\\[`e]),
   'i' => q(\\[`i]),
   'o' => q(\\[`o]),
   'u' => q(\\[`u]),
  );



########################################################################
# subs
########################################################################

# Pinyin consists of syllables with a final number to be translated
# into an accent.  Such numbered syllables are combined into words.
# Such words can have a final punctuation.  A line is a collection of
# such words.

my @roffs = ( 'n',
	      't',
	    );

########################################################################
sub err {
  my $s = shift;
  print STDERR $s;
  1;
} # err()


########################################################################
sub handle_line {
  my $starting_blanks = shift;
  my $line = shift;

#&err('handle_line start: ' . $line);

  my %outline = ( 'n' => $starting_blanks, 't' => $starting_blanks, );

  # transform to Ü only for inside of Perl
  $line =~ s/\\
	     \(:U
	    /Ü/gx;
  $line =~ s/\\
	     \[:U\]
	    /Ü/gx;

# handle_line()

  # transform to ü only for inside of Perl
  $line =~ s/\\
	     \(:u
	    /ü/gx;
  $line =~ s/\\
	     \[:u\]
	    /ü/gx;

  $line =~ s/U[eE]/Ü/g;
  $line =~ s/u[eE]/ü/g;

  $line =~ s/\\\(aq/'/g;	# \(aq is an apostrophe
  $line =~ s/\\\[aq\]/'/g;	# \[aq] is an apostrophe
  $line =~ s/^[']//;		# remove leading apostrophe
  $line =~ s/[']$//;		# remove final apostrophe
  $line =~ s/['][']+/'/g;	# combine apostrophe groups
  $line =~ s/([0-4])'/$1/;
  $line =~ s/([^0-4])'/${1}0/;

  my @words = split /\s+/, $line;


# handle_line()
  for my $word ( @words ) {
#&err('handle_line word: ' . $word);

    next unless ( $word );

    # this is a word, maybe composed of several syllables
    my $punctuation = $1 if ( $word =~ s/([,.?!:;]*)$// );
    # '$word' is now without punctuation

    my %outword = &handle_word($word);
    next unless ( %outword );

    for my $roff ( @roffs ) {
#&err('handle_line roff ' . $roff .  ': ' . $outword{$roff});

      # combine words to line
      next unless ( $outword{$roff} );

      # non-initial space
      $outline{$roff} .= ' ' if ( $outline{$roff} );

      $outline{$roff} .= $outword{$roff};
      $outline{$roff} .= $punctuation;
    }
  }
#for my $roff ( @roffs ) {
#&err('handle_line end ' . $roff .  ': ' . $outline{$roff});
#}
  return %outline;
} # handle_line()


########################################################################
sub handle_word {
  my $word = shift;
#&err('handle_word start: ' . $word);

  $word =~ s/5/0/g;		# transform 5 to 0
  $word =~ s/([^0-4])$/${1}0/;	# add lacking final no-tone

  # remove apostrophes with tone
  $word =~ s/
	      ([0-4])
	      [']
	    /$1/gx;
  # replace apostrophes without tone by 0
  $word =~ s/
	      ([^0-4])
	      [']
	    /${1}0/gx;

# handle_word()
  # detect wrong tone numbers
  if ( $word =~ s/[5-9]/0/g ) {
    &err('word ' . $word . ': wrong tone number ' . $1);
    return {};
  }

  $word =~ s/[']//g;		# remove apostrophes

  # remove starting apostrophe or number
  $word =~ s/^(['0-4])+//;

  # add 0 for final no-tone
  $word .= '0' if ( $word =~ /[^0-4]$/ );

  if ( $word =~ /^[0-9]/ ) {	# word starts with number
    print 'word: ' . $word . ' starts with tone number';
    $word =~ s/^[0-9]+//;
  }
#&err('handle_word 0: ' . $word);

# handle_word()

  my %outword = ( 'n' => '', 't' => '', );

  # split word into syllables
  while ( $word =~ /^[a-zA-ZüÜ']/ ) {
    $word =~ s/^([a-zA-ZüÜ']+)([0-4])//;
    my $syll = $1;
    my $tone = $2;
#err('handle_word split: ' . $syll . ' ' . $tone);
    my %outsyll = &handle_syll( $syll, $tone );
    next unless ( %outsyll );
    for my $roff ( @roffs ) {
      my $out = $outsyll{$roff};
      $out = '\\[aq]' . $out if ( $out && $out =~ /^[aeo]/ );
      $outword{$roff} .= $out;
#&err('handle_word ' . $roff . ': ' . $outword{$roff});
    }
  }
  return %outword;
} # handle_word()


########################################################################
sub handle_syll {
  my $syll = shift;
  my $tone = shift;
#&err( 'handle_syll start: ' . $syll . ' ' . $tone);

  my $lower_case = lc($syll);
  $lower_case =~ s/Ü/ü/g;
  unless ( exists($syllables{$lower_case}) ) {
    err('The syllable ' . $syll . ' is not a Chinese syllable.');
    return {};
  }

  my %outsyll = ( 'n' => '', 't' => '', );

  if ( $tone == 0 ) {	# no accent
    # use u umlaut without accent
    $syll =~ s/Ü/\\[:U]/g;
    $syll =~ s/ü/\\[:u]/g;

    for my $roff ( @roffs ) {
      $outsyll{$roff} = $syll;
#&err('handle_syll 0 outsyll ' . $roff . ': ' . $outsyll{$roff});
    }
    return %outsyll;
  }	# end of tone 0

# handle_syll()

  # split syllable
  $syll =~
    /^
     ([a-zA-Z]*)
     ([aeiouAEIOUüÜ]+)
     ([a-zA-Z]*)
     $/x;
  my $initial = $1;
  my $vowels = $2;
  my $final = $3;
  unless ( $vowels ) {
    &err( 'Syllable ' . $syll . ' does not have vowels' );
    return {};
  }

  # split vowels
  my $vowels_before = '';
  my $vowel = '';
  my $vowels_after = '';

# handle_syll()

  # find vowel for accent
  if ( $vowels =~ /^[aeiouAEIOU]$/ ) {		# only 1 vowel
#&err('handle_syll single vowel ' . $vowels);
    $vowel = $vowels;
  } elsif ( $vowels eq 'ü' ) {
    $vowel = $vowels;
  } elsif ( $vowels eq 'Ü' ) {
    $vowel = $vowels;
  } elsif ( $vowels =~ /^([^aeAE]*)([aeAE])(.*)$/ ) {	# a, A, e or E
    $vowels_before = $1;
    $vowel = $2;
    $vowels_after = $3;
  } elsif ( $vowels =~ /^([^oO]*)(oO)(.*)$/ ) {		# o or O
    $vowels_before = $1;
    $vowel = $2;
    $vowels_after = $3;
  } elsif ( $vowels =~ /^(\w)(\w)(.*)$/ ) {		# take 2nd vowel
    $vowels_before = $1;
    $vowel = $2;
    $vowels_after = $3;
  } else {
    &err( 'Unknown vowels: ' . $vowels . ' in syllable: ' . $syll );
    return {};
  }

#  unless ( $vowel =~ /^[aeiouAEIOU]$/ ) {
#    print STDERR q(The argument ') . $vowel . q(' is not a vowel!);
#    return {};
#  }

# handle_syll()

  $outsyll{'n'} = &vowel_n($vowel, $tone);
  $outsyll{'t'} = &vowel_t($vowel, $tone);

  for my $roff ( @roffs ) {
    $outsyll{$roff} = $initial .  $vowels_before .
      $outsyll{$roff} . $vowels_after .  $final;
#&err('handle_syll out ' . $roff . ': ' . $outsyll{$roff});
  }

  return %outsyll;
} # handle_syll()


########################################################################
sub vowel_n {	# Unicode for nroff
  my $vowel = shift;
  my $tone = shift;
#&err('vowel_n: ' . $vowel . ' ' . $tone);

  return '' unless ( $vowel );

  if ( $tone == 1 ) {		# macron
    $vowel = $tones1_Unicode{$vowel};
  } elsif ( $tone == 2 ) {	# acute
    $vowel = $tones2_Unicode{$vowel};
  } elsif ( $tone == 3 ) {	# caron
    $vowel = $tones3_Unicode{$vowel};
  } elsif ( $tone == 4 ) {	# grave
    $vowel = $tones4_Unicode{$vowel};
  }
  return $vowel;
} # vowel_nr()


########################################################################
sub vowel_t {	# named glyphs for troff
  my $vowel = shift;
  my $tone = shift;
#&err( 'vowel_t: ' . $vowel . ' ' . $tone);

  return '' unless ( $vowel );

  # \o'\s-2\[:u]\s0\[a-]'
  if ( $vowel =~ /[üÜ]/ ) {
    my $smaller = 2;
    $vowel = q(\\o'\\s-) . $smaller . q(\\[:u]\\s0) .
      $accents[$tone] . q(');
    return $vowel;
  }

  $vowel = q(\\[.i]) if ( $vowel eq 'i' );

  if ( $tone == 1 ) {		# macron
    $vowel = q(\\o') . $vowel . $accents[$tone] . q(');
  } elsif ( $tone == 2 ) {	# acute
    $vowel = $tones2_glyphs{$vowel};
  } elsif ( $tone == 3 ) {	# caron
    $vowel = q(\\o') . $vowel . $accents[$tone] . q(');
  } elsif ( $tone == 4 ) {	# grave
    $vowel = $tones4_glyphs{$vowel};
  }
  return $vowel;
} # vowel_t()


########################################################################
sub finish_pinyin_mode {
#&err( 'finish' );
  my $n = shift;
  my $t = shift;
  push @$n, '\\}';
  push @$t, '\\}';

  for ( @$n ) {	# Unicode for nroff
    print;
  }

  for ( @$t ) {	# glyphs for troff
    print;
  }

  1;
} # finish_pinyin_mode()


1;
########################################################################
### Emacs settings
# Local Variables:
# mode: CPerl
# End:
