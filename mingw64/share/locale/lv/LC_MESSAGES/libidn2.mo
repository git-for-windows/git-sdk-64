��    *      l  ;   �      �  �   �  �   �  �   >  c   �     A     Z  R  g  N   �  &   	  O   0     �  #   �  !   �  *   �  D   �  @   D	  %   �	  &   �	  &   �	  (   �	     "
     =
  6   I
     �
  (   �
  '   �
  4   �
  4     &   I  /   p  /   �  7   �  -     %   6  %   \  "   �     �  .   �  #   �  '        6  �  >  �   �  �   �  �   P  W   �      I     j  q  �  Y   �  ;   M  h   �     �  (     $   0  /   U  N   �  K   �  ,      ,   M  +   z  0   �  !   �     �  A   	     K  5   ]  (   �  7   �  7   �  "   ,  -   O  -   }  7   �  .   �  &     &   9      `     �  .   �      �  (   �                     	   )                                 #          
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
 could not convert string to UTF-8 could not determine locale encoding format domain label has character forbidden in non-transitional mode (TR46) domain label has character forbidden in transitional mode (TR46) domain label has forbidden dot (TR46) domain label longer than 63 characters domain name longer than 255 characters input A-label and U-label does not match input A-label is not valid input error libiconv required for non-UTF-8 character encoding: %s out of memory punycode conversion resulted in overflow punycode encoded data will be too large string contains a context-j character with null rule string contains a context-o character with null rule string contains a disallowed character string contains a forbidden context-j character string contains a forbidden context-o character string contains a forbidden leading combining character string contains forbidden two hyphens pattern string contains invalid punycode data string contains unassigned code point string could not be NFC normalized string encoding error string has forbidden bi-directional properties string is not in Unicode NFC format string start/ends with forbidden hyphen success Project-Id-Version: libidn2 2.3.3
Report-Msgid-Bugs-To: bug-libidn2@gnu.org
PO-Revision-Date: 2024-12-22 09:42+0000
Last-Translator: Rihards Priedītis <rprieditis@gmail.com>
Language-Team: Latvian <translation-team-lv@lists.sourceforge.net>
Language: lv
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
X-Bugs: Report translation errors to the Language-Team address.
X-Generator: Poedit 3.5
       --usestd3asciirules Ieslēgt STD3 ASCII noteikumus
      --no-alabelroundtrip Izslēgt A-label atpakaļgaitas meklēšanai
      --debug Izdrukāt atkļūdošanas informāciju
      --quiet Klusā darbība
   -T, --tr46t Ieslēgt TR46 pārejas apstrādi
  -N, --tr46nt Ieslēgt TR46 ne-pārejas apstrādi
      --no-tr46 Izslēgt TR46 apstrādi
   -d, --decode Dekodēt (punycode) domēna vārdu
  -l, --lookup Pārlūkot domēna nosaukumu (noklusējuma iestatījums)
  -r, --register Reģistrēt etiķeti
   -h, --help Izdrukāt palīdzību un iziet
  -V, --versija Izdrukāt versiju un iziet
 A-label atpakaļgaita neizdevās Rakstzīmes kopas: %s
 Komandrindas saskarne ar Libidn2 implementāciju IDNA2008.

Paredzams, ka visas virknes ir kodētas ar vietējās valodas rakstzīmju kopu.

Lai apstrādātu virkni, kas sākas ar `-', piemēram, `-foo', izmantojiet `--',
lai signalizētu parametru beigas, piemēram, `idn2 --quiet -- -foo'.

Garajām opcijām obligātie argumenti ir obligāti arī īsajām opcijām.
 Internacionalizētā Domēna Vārda (IDNA2008) konvertēt VIRKNES vai standarta ievadi.

 Lai iegūtu vairāk informācijas, mēģiniet `%s --help'.
 Ievadiet katru ievades virkni atsevišķi vienā rindā, kas tiek pabeigta ar jaunas rindas rakstzīmi.
 Nepazīstama kļūda Lietošana: %s [OPCIJA]... [VIRKNES]...
 nevarēja konvertēt virkni uz UTF-8 nevarēja noteikt lokāles kodēšanas formātu domēna etiķetei ir rakstzīme, kas ir aizliegta ne-pārejas režīmā (TR46) domēna etiķetē ir rakstzīme, kas ir aizliegta pārejas režīmā (TR46) domēna etiķetei ir aizliegts punkts (TR46) domēna etiķete garāka par 63 rakstzīmēm domēna vārds garāks par 255 rakstzīmēm ievades A-marķējums un U-marķējums nesakrīt ievades A-marķējums nav derīgs ievades kļūda libiconv nepieciešama rakstzīmju kodēšanai, kas nav UTF-8: %s atmiņas trūkums punycode konversijas rezultātā radās pārplūšana punycode kodēti dati būs pārāk lieli virkne satur konteksta-j rakstzīmi ar nulles noteikumu virkne satur konteksta-o rakstzīmi ar nulles noteikumu virkne satur neatļautu rakstzīmi virkne satur aizliegtu konteksta-j rakstzīmi virkne satur aizliegtu konteksta-o rakstzīmi virkne satur aizliegtu ievada kombinēšanas rakstzīmi rindā sastopami divi aizliegti defisi paraugi virkne satur nederīgus punycode datus virkne satur nepiešķirtu koda punktu virkni nevarēja normalizēt NFC virknes kodēšanas kļūda virknei ir aizliegtas div-virzienu īpašības virkne nav Unicode NFC formātā virknes sākums/gals ar aizliegtu defisi veiksmīgs iznākums 