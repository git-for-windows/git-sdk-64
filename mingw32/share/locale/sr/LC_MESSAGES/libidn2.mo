��    *      l  ;   �      �  �   �  �   �  �   >  c   �     A     Z  R  g  N   �  &   	  O   0     �  #   �  !   �  *   �  D   �  @   D	  %   �	  &   �	  &   �	  (   �	     "
     =
  6   I
     �
  (   �
  '   �
  4   �
  4     &   I  /   p  /   �  7   �  -     %   6  %   \  "   �     �  .   �  #   �  '        6    >  I  D  �   �  �   �  �   \  ?   �     /  B  L  �   �  G      �   h     �  7     <   M  J   �  o   �  j   E  E   �  >   �  =   5  V   s  8   �       d     "   �  X   �  H   �  _   G  _   �  7     I   ?  I   �  W   �  O   +  J   {  B   �  C   	  (   M  U   v  9   �  S        Z                	   )                                 #          
                            !         '                  (                                         $          "          *             &   %          --usestd3asciirules   Enable STD3 ASCII rules
      --no-alabelroundtrip  Disable A-label roundtrip for lookups
      --debug               Print debugging information
      --quiet               Silent operation
   -T, --tr46t               Enable TR46 transitional processing
  -N, --tr46nt              Enable TR46 non-transitional processing
      --no-tr46             Disable TR46 processing
   -d, --decode              Decode (punycode) domain name
  -l, --lookup              Lookup domain name (default)
  -r, --register            Register label
   -h, --help                Print help and exit
  -V, --version             Print version and exit
 A-label roundtrip failed Charset: %s
 Command line interface to the Libidn2 implementation of IDNA2008.

All strings are expected to be encoded in the locale charset.

To process a string that starts with `-', for example `-foo', use `--'
to signal the end of parameters, as in `idn2 --quiet -- -foo'.

Mandatory arguments to long options are mandatory for short options too.
 Internationalized Domain Name (IDNA2008) convert STRINGS, or standard input.

 Try `%s --help' for more information.
 Type each input string on a line by itself, terminated by a newline character.
 Unknown error Usage: %s [OPTION]... [STRINGS]...
 could not convert string to UTF-8 could not determine locale encoding format domain label has character forbidden in non-transitional mode (TR46) domain label has character forbidden in transitional mode (TR46) domain label has forbidden dot (TR46) domain label longer than 63 characters domain name longer than 255 characters input A-label and U-label does not match input A-label is not valid input error libiconv required for non-UTF-8 character encoding: %s out of memory punycode conversion resulted in overflow punycode encoded data will be too large string contains a context-j character with null rule string contains a context-o character with null rule string contains a disallowed character string contains a forbidden context-j character string contains a forbidden context-o character string contains a forbidden leading combining character string contains forbidden two hyphens pattern string contains invalid punycode data string contains unassigned code point string could not be NFC normalized string encoding error string has forbidden bi-directional properties string is not in Unicode NFC format string start/ends with forbidden hyphen success Project-Id-Version: libidn2-2.3.3
Report-Msgid-Bugs-To: bug-libidn2@gnu.org
PO-Revision-Date: 2022-09-08 05:47+0200
Last-Translator: Мирослав Николић <miroslavnikolic@rocketmail.com>
Language-Team: Serbian <(nothing)>
Language: sr
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
Plural-Forms: nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);
X-Bugs: Report translation errors to the Language-Team address.
       --usestd3asciirules   Укључује STD3 АСКРИ правила
      --no-alabelroundtrip  Искључује време одзива А-натписа за тражењем
      --debug               Исписује податке о пречишћавању
      --quiet               Нечујна радња
   -T, --tr46t               Укључује TR46 транзициону обраду
  -N, --tr46nt              Укључује TR46 не-транзициону обраду
      --no-tr46             Искључује TR46 обраду
   -d, --decode              Декодира (punycode) назив домена
  -l, --lookup              Тражи назив домена (основно)
  -r, --register            Записује натпис
   -h, --help                Исписује помоћ и излази
  -V, --version             Исписује издање и излази
 Време одзива А-натписа није успело Скуп знакова: %s
 Сучеље линије наредби за „Libidn2“ примену „IDNA2008“.

Очекује се да су све ниске кодиране у скупу знакова коришћеног језика.

Да обрадите ниску која почиње са „-“, на пример „-foo“, употребите „--“
да назначите крај параметара, као у „idn2 --quiet -- -foo“.

Обавезни аргументи за дуге опције су такође обавезни и за кратке опције.
 Интернационализовани назив домена (IDNA2008) претвара НИСКЕ, или стандардни улаз.

 Пробајте „%s --help“ за више информација.
 Куцајте сваку улазну ниску у засебном реду, завршавајући знаком за нови ред.
 Непозната грешка Употреба: %s [ОПЦИЈА]... [НИСКЕ]...
 не могу да претворим ниску у УТФ-8 не могу да одредим запис кодирања језика натпис домена има знак забрањен у не-транзиционом режиму (TR46) натпис домена има знак забрањен у транзиционом режиму (TR46) натпис домена има забрањену тачку (TR46) натпис домена је дужи од 63 знакова назив домена је дужи од 255 знакова уноси „A-натпис“ и „U-натпис“ се не подударају унос „А-натпис“ није исправан улазна грешка „libiconv“ је потребно за кодирање знака који није УТФ-8: %s нема више меморије „punycode“ претварање је резултирало препуњавањем „punycode“ кодирани подаци биће превелики ниска садржи садржајни-ј знак са ништавним правилом ниска садржи садржајни-о знак са ништавним правилом ниска садржи недопуштени знак ниска садржи забрањени садржајни-ј знак ниска садржи забрањени садржајни-о знак ниска садржи забрањени водећи комбинујући знак ниска садржи забрањена два шаблона заграде ниска садржи неисправне „punycode“ податке ниска садржи недодељену кодну тачку ниска не може бити НФЦ нормализована грешка ниске кодирања ниска има забрањена дво-усмеравајућа својства ниска није у запису Уникода НФЦ ниска почиње/завршава се забрањеном заградом успешно 