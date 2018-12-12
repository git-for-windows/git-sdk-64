[= AutoGen5 Template h c -*- Mode: Scheme -*-

# This file contains the templates used to generate
# keyword parsing code

##  This file is part of AutoGen.
##  AutoGen is free software.
##  AutoGen is Copyright (C) 1992-2018 by Bruce Korb - all rights reserved
##
## AutoGen is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## AutoGen is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
## See the GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this program.  If not, see <http://www.gnu.org/licenses/>.
##  This file has the following md5sum:
##
##  43b91e8ca915626ed3818ffb1b71248b COPYING.gplv3

=][=

(emit
  (dne " *  " "/*  ")
  "\n *\n"
  (if (exist? "copyright")
      (license-description
          (get "copyright.type" "unknown")
          (get "package" (shell "basename `pwd`"))
          " * "
          (get "copyright.owner" (get "copyright.author" "unknown")) )
      (license-description "mbsd" "str2enum" " * " "Bruce Korb")
  )
)

=][=

CASE (suffix)                   =][= #

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  H CODE
;;;
;;;=][=
== h                            =][=
INVOKE init-header              =][=
  IF (exist? "addtogroup")      =]
/** \file [= (out-name) =]
 * Header for string to enumeration values and back again.
 * @addtogroup [= addtogroup =]
 * @{
 */[=
  ENDIF addtogroup              =]
#include <sys/types.h>
#ifndef MISSING_INTTYPES_H
# include <inttypes.h>
#endif

typedef enum {[=
    (if (> enum-val-offset 0)
        (string-append "\n    " invalid-cmd " = 0,") ) =][=
    (shellf "mk_enum_list %d" enum-val-offset) =]
    [= (. cmd-count) =][=
    (if (= (get "invalid-val") "~0")
        (string-append ",\n    " invalid-cmd " = ~0") ) =]
} [= (. enum-name) =];
[=

  FOR add-on-text               =][=
    IF (= (get "ao-file") "enum-header") =]
[=    ao-text                   =]
[=  ENDIF                       =][=
  ENDFOR add-on-text            =][=

  IF (if (exist? "no-code")
         (if (exist? "dispatch")
             (error "dispatch does not work without code")
             #f)
         #t)
=]
extern [=
(define find-arg-list (string-append "char const * str" len-arg))
(string-append enum-name "\n"
find-func-name "(" find-arg-list ");\n")
=][=

IF (not (exist? "no-name"))

=]
extern char const *
[=(. base-type-name)=]_name([= (. enum-name) =] id);
[=

ENDIF no-name

=][=

    IF (define disp-text "")
       (exist? "dispatch")      =][=

      (out-push-new)
      (out-suspend "disp-text") =][=

      FOR dispatch   "\n"       =]
[=      INVOKE mk-dispatch      =][=
      ENDFOR dispatch           =][=

      (out-resume "disp-text")
      (set! disp-text (out-pop #t))
      =][=

    ENDIF dispatch exists

=]
[=ENDIF exist/not    "no-code"  =][=
(if (exist? "addtogroup") "\n/** @} */") \=]
#endif /* [=(. header-guard)    =] */[= #

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  C CODE
;;;
;;;=][=
== c

=]
 */
#include "[= ;;"
    (if (exist? "no-code") (out-delete))
    (if move-output-file
        (out-move (string-append base-file-name ".c")))
    header-file                 =]"[=#"=][=
  (. extra-c-incl)              =][=
  IF (exist? "addtogroup")      =]
/** \file [= (out-name) =]
 * Code for string to enumeration values and back again.
 * @addtogroup [= addtogroup =]
 * @{
 */
[=ENDIF addtogroup              =][=

  INVOKE run-gperf              =][=
  INVOKE mk-finder              =][=

  IF (not (exist? "no-name"))   =][=
    INVOKE mk-enum2name         =][=
  ENDIF dispatch                =][=

  (. disp-text)                 =][=

  FOR add-on-text               =][=
    IF (= (get "ao-file") "enum-code") =]
[= ao-text                      =][=
    ENDIF correct type          =][=
  ENDFOR add-on-text            =][=
(if (exist? "addtogroup") "\n/** @} */") \=][=

ESAC  suffix c/h

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;; Create the function that converts the name into an enum value.
;;;
;;;=][=

DEFINE init-header              =][=

(define computed-length #f)
(define return-length   #f)
(define tmp-str         "")
(define extra-c-incl    "")
(define len-arg         ", size_t len")
(define len-descrip
"\n * @param[in] len   the provided length of the keyword at \\a str.")
(define enum-type       "cmd")  =][=

INCLUDE "str2init.tlib"         =][=

  CASE length                   =][=
  !E                            =][=
    (if (or (exist? "no-case") (exist? "alias"))
      (set! extra-c-incl "\n#include <ctype.h>")) =][=

  = provided                    =][=
    (if (or (exist? "no-case") (exist? "alias"))
      (set! extra-c-incl "\n#include <ctype.h>")) =][=

  = returned                    =][=
    (set! extra-c-incl "\n#include <ctype.h>\n#include <string.h>")
    (set! len-arg         ", size_t * len")
    (define len-descrip
    "\n * @param[out] len   address to store the keyword length from \\a str.")
    (set! computed-length #t)
    (set! return-length   #t)   =][=

  *                             =][=
    (set! extra-c-incl "\n#include <ctype.h>\n#include <string.h>")
    (set! computed-length #t)
    (set! len-descrip "")
    (set! len-arg "")           =][=
  ESAC                          =][=

(define equate-from "")
(define equate-to   "")
(out-push-new)

=]
mk_char_list() {
    echo "$1" | sed 's/\(.\)/\1\
/g' | sort -u | tr -d '\n\t '
}

readonly max_cmd_width=[=(. max-cmd-width)=]
readonly max_enm_width=[=(+ max-cmd-width (string-length enum-prefix) 1)=]
readonly min_cmd_width=[=(. min-cmd-width)=]
[= IF (exist? "equate") =]
equate_from='[=(define equate-from (get "equate")) equate-from=]'
equate_to=`echo "$equate_from" | sed "s#.#[=(substring equate-from 0 1)=]#g"`
[= ENDIF =]
mk_enum_list() {
    tr '[a-z]' '[A-Z]' < ${tmp_dir}/commands | sort > ${tmp_dir}/commands-UC
    exec 3< ${tmp_dir}/commands-UC
    declare n c
    declare fmt="\n    [=(. enum-prefix)=]_%-${max_cmd_width}s = %s,"
    while read -u3 n c
    do
        printf "$fmt" $c `expr $n + $1`
    done
    exec 3<&-
}
[=

(shell (out-pop #t))
(if (exist? "equate")
    (set! equate-to (shell "echo \"${equate_to}\"")) )
(define cmd-chars (join "" (stack "cmd")))

(if (exist? "no-case")
    (set! cmd-chars (string-append
          (string-downcase cmd-chars) (string-upcase cmd-chars) )) )

(if (exist? "equate")
    (set! cmd-chars (string-append cmd-chars (get "equate")))
)

(set! cmd-chars (shell "mk_char_list '" cmd-chars "'" ))
(emit "\n *\n * Command/Keyword Dispatcher\n */\n")
(make-header-guard "str2enum")

=][=

ENDDEF init-header

;;; = = = = = = = = = = = = = = = = = = =
;;;
;;; Create the function that converts the name into an enum value.
;;;
;;;=][=

DEFINE mk-finder                =]

/**
 * Convert a command (keyword) to a [= (. enum-name) =] enumeration value.[=
   IF (. computed-length)       =]
 * The length of the command is computed by calling \a strspn()
 * on the input argument.[=
   ENDIF                        =][=
   IF (exist? "equate")         =]
 * Within the keyword, the following characters are considered equivalent:
 *   \a [=(. cmd-chars)     =][=
   ENDIF                        =]
 *
 * @param[in] str   a string that should start with a known key word.[=
   (. len-descrip)              =]
 * @returns the enumeration value.
 * If not found, that value is [=(. invalid-cmd)=].
 */
[= (string-append enum-name "\n" find-func-name)
=](char const * str[=(. len-arg)=])
{[=
  IF (exist? "alias")           =]
  switch (*str) {[=
    FOR alias                   =]
  case '[= (define cmd (get "alias"))
           (substring cmd 0 1)=]': return [=
           (set! cmd (shellf "echo %s" (substring cmd 1)))
           (string-append enum-prefix "_"
                   (string->c-name! (string-upcase! cmd))) =];[=
    ENDFOR alias                =]
  default:
    if (! isalpha((unsigned char)*str))
      return [=(. invalid-cmd)=];
    break;
  }

  {[=
  ENDIF   alias                 =]
    [= (. base-type-name) =]_map_t const * map;[=

  IF (define len-param-name (if computed-length "clen" "len"))
     (define check-length   (if return-length
             "\n    if (len != NULL)\n\t*len = clen;" "\n"))
     computed-length            =]
    static char const accept[] =
        [= (set! check-length (string-append
        check-length
        "\n    if (isalnum((unsigned char)str[clen]))"
        "\n        return " invalid-cmd ";" ))

        (kr-string cmd-chars)
        =];
    unsigned int clen = strspn(str, accept);[=
  ENDIF computed-length

=][=
  IF (or (exist? "no-case") (exist? "equate"))   =][=
    INVOKE cvt-chars            =][=
  ELSE  =][= (. check-length)   =][=
  ENDIF                        \=]

    map = find_[=(. base-type-name)=]_name(str, (unsigned int)[=
       (. len-param-name) =]);[=

  IF (not (exist? "partial"))   =]
    return (map == NULL) ? [=(. invalid-cmd)=] : map->[=(. enum-field)=];[=
  ELSE                          =][=
    INVOKE find-part-match      =][=
  ENDIF                         =][=
  (if (exist? "alias") "\n  }") =]
}
[=

ENDDEF mk-finder

;;; = = = = = = = = = = = = = = = = = = =
;;;
;;; Convert one character to canonical form.  If there are equated characters
;;; or case mappings to do, do it here.
;;;
;;;=][=

DEFINE cvt-chars                =][=

(define min-len (if (and (exist? "partial") (> min-cmd-width 2))
                2 min-cmd-width))

=]
    char name_buf[[=(+ 1 max-cmd-width)=]];
    unsigned int ix;

    /* too short, too long or followed immediately by a name char... */
    if (  ([=(sprintf "%s < %u" len-param-name min-len)=])
       || ([=(sprintf "%s > %u" len-param-name max-cmd-width)=])
       || isalnum((unsigned char)str[[=(. len-param-name)=]]) )
        return [=(. invalid-cmd)=];

    for (ix = 0; ix < [=(. len-param-name)=]; ix++) {
        int ch = (unsigned char)str[ix];[=
  IF (exist? "equate")          =]
        switch (ch) {
        [= (shell "echo '" (get "equate") "' | "
               "sed \"s/\\(.\\)/case '\\1': /g\"") =]
            name_buf[ix] = '[= (substring (get "equate") 0 1) =]';
            break;
        default:[=

    IF (exist? "no-case")       =]
            name_buf[ix] = tolower(ch);[=
    ELSE                        =]
            name_buf[ix] = ch;[=
    ENDIF                       =]
            break;
        }[=

  ELSE                          =]
        name_buf[ix] = tolower(ch);[=
  ENDIF no-case/equate          =]
    }
    str = name_buf;[=
  (if return-length (string-append
      "\n    if (len != NULL)\n\t*len = clen;"))
  =][=

ENDDEF cvt-chars

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;=][=

DEFINE mk-dispatch      =][=

(if (not (exist? "d-nam"))
    (error "dispatch needs callout procedure name format ('d-nam')"))

(define disp-type-name (shell
  "echo " (get "d-nam") " | sed 's/%s//;s/__*/_/g;s/^_//;s/_$//'" ))
(define handler-type   (string-append disp-type-name "_hdl_t"))

(define disp-proc-name
        (string-append disp-type-name "_" base-type-name "_disp" ))

(define hdlr-arg-list (string-append enum-name " id, char const * str"))
(define disp-arg-list find-arg-list)
(define extra-args    "")

(if (exist? "d-arg") (begin
    (set! extra-args (shell "echo '" (get "d-arg") "' | tr '\\n' ' '"))
    (set! disp-arg-list (string-append disp-arg-list ",\n\t" extra-args))
    (set! hdlr-arg-list (string-append hdlr-arg-list ",\n\t" extra-args))
)   )

(emit (string-append "extern " (get "d-ret") "\n"
disp-proc-name "(" disp-arg-list ");"
"\n\ntypedef " (get "d-ret") " (" handler-type ")(\n\t"
     hdlr-arg-list ");\n\n" handler-type "\n"))

(out-push-new)          =][=

IF (exist? "d-only")    =][=

(out-push-new (string-append tmp-dir "/disp-list"))
(emit (string-downcase (string->c-name! (join "\n" (stack "d-only"))))
      "\n"
)
(out-pop)               =][=

ELSE

=]
rm -f ${tmp_dir}/disp-list
awk '{ print $2 }' ${tmp_dir}/commands[=

  IF (exist? "d-omit") =] | \
   grep -E -v '^([=
  (string-downcase (string->c-name! (join " " (stack "d-only"))))
=])$' \
   sed 's/ /|/g'[=

  ENDIF d-omit =] > ${tmp_dir}/disp-list[=

ENDIF d-only

=]
{
    echo [=(. invalid-name)=]
    cat ${tmp_dir}/disp-list
} | $CLexe -f '[= d-nam =]' -I4 --spread=1 -S, --end ';'[=

(shell  (out-pop #t))           =][=
(out-resume "disp-text")        =][=
INVOKE mk-disp-code             =][=
(out-suspend "disp-text")       =][=

ENDDEF mk-dispatch

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;=][=

DEFINE mk-disp-code             =][=

(out-push-new)

=]mk_proc_dispatch() {
    declare fmt="\n        [[=(. enum-prefix)=]_%-${max_cmd_width}s] = [=
       d-nam =],"
    exec 3< ${tmp_dir}/disp-list
    while read -u3 c
    do
        C=$(echo $c | tr '[a-z]' '[A-Z]')
        printf "$fmt" $C $c
    done
    exec 3<&-

    printf "\n        [%-${max_enm_width}s] = [= d-nam =] };\n" \
        [=(. invalid-cmd)=] [=(. invalid-name)=]
}
mk_lengths() {
    exec 3< ${tmp_dir}/commands-UC
    declare n=`expr ${max_cmd_width} + 1`
    declare c
    declare fmt="\n        [[=(. enum-prefix)=]_%-${max_cmd_width}s] ="
    while read -u3 n c
    do
        printf "$fmt ${#c}," "$c"
    done | sed '$s/,$//'

    exec 3<&-
}
[=
(shell (out-pop #t))
=]

/**
 * Dispatch a [=(. base-type-name)=] function, based on the keyword.
 *
 * @param[in] str  a string that should start with a known key word.[=
   (. len-descrip) =]
 * @returns [= d-ret =], returned by the dispatched function.
 */
[= d-ret =]
[=(string-append disp-proc-name "(" disp-arg-list ")")=]
{
    static [=(. handler-type)=] * const dispatch[[=
        (+ 2 bit-count)=]] = {[=
        (shell "mk_proc_dispatch") =][=

IF (. return-length)

=]
    [= (define length-value "*len")
       enum-name =] id = [=(. find-func-name)=](str, len);[=

ELIF (> (string-length len-arg) 0)

=]
    [= (define length-value "len")
       enum-name =] id = [=(. find-func-name)=](str, len);[=

ELSE

=]
    static unsigned int keywd_len[] = {[=
        (shell "mk_lengths") =] };
    [= (define length-value "klen")
       enum-name =] id = [=(. find-func-name)=](str);
    unsigned int klen = [=
  IF (exist? "alias") =](! isalnum((unsigned char)*str)) ? 1 : [=
  ENDIF  =]keywd_len[id];[=

ENDIF

=]
    [=(. handler-type)=] * disp = dispatch[id];
    if (disp == NULL)
        disp = dispatch[[=(. invalid-cmd)=]];
    [=
    (if (= (get "d-ret") "void") "" "return ")
    =]disp(id, str + [=

    IF (emit length-value)
       (exist? "d-arg") =], [=

       (out-push-new) \=]
        set -- `echo '[=(. extra-args)=]' | tr '*' ' '`
        for f in $*
        do case "$f" in
           ( *, ) printf '%s ' "$f" ;;
           esac
        done
        eval echo "\${$#}"[=
    (shell (out-pop #t)) =][=

    ENDIF  d-arg  =]);
}
[=

ENDDEF mk-disp-code

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  Search through the alphabetized indexes for a unique partial match.
;;;
;;;=][=

DEFINE find-part-match          =]
    if (map != NULL)
        return map->[=(. enum-field)=];
    /* Check for a partial match */
    {
        /*
         * Indexes of valid [=(. base-type-name)
=]_table entries in sorted order:
         */
        static unsigned int const ix_map[] = {
[=

(out-push-new)

=]
for f in `sed 's/:.*//' ${tmp_dir}/table`
do
    echo `expr $f - 1`
done | $CLexe --spread=1 -S, -I12 --end ' };'[=

(shell (out-pop #t))

=][= (define cmp-call (string-append
        "strncmp(map->" name-field ", str, " len-param-name ")" )) =]
        [= (. enum-name) =] res = [=(. invalid-cmd)=];
        static int const HI = (sizeof(ix_map) / sizeof(ix_map[0])) - 1;
        int lo = 0;
        int hi = HI;
        int av;
        int cmp;

        for (;;) {
            av  = (hi + lo) / 2;
            map = [=(. base-type-name)=]_table + ix_map[av];
            cmp = [=(. cmp-call)=];
            if (cmp == 0) break;
            if (cmp > 0)
                 hi = av - 1;
            else lo = av + 1;
            if (lo > hi)
                return [=(. invalid-cmd)=];
        }
        res = map->[=(. enum-field)=];
        /*
         * If we have an exact match, accept it.
         */
        if (map->[= (. name-field) =][[= (. len-param-name) =]] == NUL)
            return res;
        /*
         * Check for a duplicate partial match (a partial match
         * with a higher or lower index than "av".
         */
        if (av < HI) {
            map = [=(. base-type-name)=]_table + ix_map[av + 1];
            if ([=(. cmp-call)=] == 0)
                return [=(. invalid-cmd)=];
        }
        if (av > 0) {
            map = [=(. base-type-name)=]_table + ix_map[av - 1];
            if ([=(. cmp-call)=] == 0)
                return [=(. invalid-cmd)=];
        }
        return res;
    }[=

ENDDEF find-part-match

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  Make an enum to name converter.  If the input id is valid, we *must*
;;;  find an associated name.  The table is compact and starts with 0.
;;;
;;;=][=

DEFINE mk-enum2name

=]
/**
 * Convert an [= (. enum-name) =] value into a string.
 *
 * @param[in] id  the enumeration value
 * @returns the associated string, or "[=(. undef-str)=]" if \a id
 * is out of range.
 */
char const *
[=(. base-type-name)=]_name([= (. enum-name) =] id)
{
    static char const undef[] = "[=
        (if insert-undef "* UNDEFINED *" undef-str) =]";
    static char const * const nm_table[] = {
[=

(out-push-new)

=]
exec 4< ${tmp_dir}/table
while IFS='' read -u4 line
do
    str=${line%\"*}
    line=${line%'}'*}
    printf "        [%-${max_enm_width}s] = \"%s\",\n" ${line##*,} ${str#*\"}
done | sed '$s/,$/ };/'[=

(shell (out-pop #t))

=]
    char const * res = undef;
    if (id < [=(. cmd-count)=]) {
        res = nm_table[id];
        if (res == NULL)
            res = undef;
    }
    return res;
}
[=

ENDDEF mk-enum2name

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;=][=
DEFINE run-gperf     =]
[=
(define table-fmt (string-append
                "%-" (number->string (+ max-cmd-width 1))
                "s " enum-prefix "_%s\n" ))
(out-push-new)

=][= #

;;; = = = = = = = = = = = = = = = = = = =
;;;
;;; GPERF OPTIONS
;;;
;;;\=]
%struct-type
%language=ANSI-C
%includes
%global-table
%omit-struct-type
%readonly-tables
%compare-strncmp

%define slot-name               [=
(define name-field (string-append pfx-str "_name")) name-field =]
%define hash-function-name      [=(. base-type-name)=]_hash
%define lookup-function-name    find_[=(. base-type-name)=]_name
%define word-array-name         [=(. base-type-name)=]_table
%define initializer-suffix      ,[=(. cmd-count)=]
[=

(define gperf-opts (out-pop #t))
(out-push-new (string-append tmp-dir "/" base-file-name ".gp"))

\=][= #

;;; = = = = = = = = = = = = = = = = = = =
;;;
;;; GPERF DEFINITIONS
;;;
;;;\=]
%{
# if 0 /* gperf build options: */
[= (prefix "// " gperf-opts) =]
# endif

#include "[=(. header-file)=]"
typedef struct {
    char const *    [=(. name-field)=];
    [= (. enum-name) =] [= (define enum-field (string-append pfx-str "_id"))
                       enum-field =];
} [=(. base-type-name)=]_map_t;
%}

[= (. gperf-opts) =]

[=(. base-type-name)=]_map_t;
%%
[=
FOR cmd =][=
 (define cmd (get "cmd"))
 (if (exist? "no-case")
     (set! cmd (string-downcase cmd)) )
 (if (exist? "equate")
     (set! cmd (string-tr! cmd equate-from equate-to)) )

 (define tmp-val (string-append
                 (if (exist? "no-case") (string-downcase cmd) cmd) "," ))

 (sprintf table-fmt tmp-val (string-upcase! (string->c-name! cmd))) =][=
ENDFOR \=]
%%
[=

(out-pop)
(out-push-new)

=][= #

;;; = = = = = = = = = = = = = = = = = = =  RUN GPERF
;;;
;;; After running gperf, delete all the "inline" lines, the soure line
;;; numbers, GNUC INLINE hints, attribute inlines, the ASCII text tests and
;;; all the conditional code.  These all check for stuff that is not relevant
;;; here.  Replace all the #define-s with their defined-to values and strip
;;; out the #define-s themselves.  Since we are appending to the output code,
;;; these #define-s interfere with what we need to do.  Finally, we also force
;;; the generated find procedure to be static.  We don't export it.
;;;
\=]
outdir=$PWD
cd ${tmp_dir}
use_args() {
  while IFS='' read line
  do
    case "$line" in
    *ARGSUSED* ) break ;;
    * ) echo "$line" ;;
    esac
  done

  while IFS='' read line
  do
    case "$line" in
    *' return '* )
        printf $'  (void)str;\n  (void)len;\n%s\n' "$line"
        cat
        return 0
        ;;
    * ) echo "$line"
        ;;
    esac
  done
}

gperf [= (. base-file-name) =].gp | \
    sed -e '/^_*inline$/d' \
        -e '/^#line /d' \
        -e '/GNUC_.*_INLINE/d' \
        -e '/__attribute__.*inline/d' \
        -e '/^#if.*== *32/,/^#endif/d' \
        -e '/^#ifdef/d' \
        -e '/^#else/d' \
        -e '/^#endif$/d' \
        -e '/key = [=(. base-type-name)=]_hash/s/key = /key = (int)/' \
        -e 's/^\(const [=(. base-type-name)=]_map_t\)/static inline \1/' \
    > baseline

sed -n -e '1,/_table\[\] =/d' \
       -e '/^ *{$/d' \
       -e '/^ *};/q' \
       -e 's/^ *//' \
       -e 's/}, {/},\
{/g' \
       -e p baseline | \
    grep -n -v '""'  | \
    sort -t: -k2     > table

sedcmd=`
    egrep '^#define ' baseline | \
        while read _ nm val _
        do
            echo "/^#define *${nm}/d;s@${nm}@${val}@g"
        done`

grep -q -F '/*ARGSUSED*/' baseline && useargs=true || useargs=false
if $useargs
then
  sed "${sedcmd}" baseline | use_args
else
  sed "${sedcmd}" baseline
fi
[=
(shell (out-pop #t)) =][=

ENDDEF run-gperf    =]
/* end of [= (out-name)  =] */[=

 #
 * Local Variables:
 * mode: text
 * indent-tabs-mode: nil
 * End:
 * end of str2mask.tpl =]
