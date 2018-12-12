[= AutoGen5 Template h c -*- Mode: Scheme -*-

# This file contains the templates used to generate
# bit map handling code

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

CASE (suffix)                   =][= #

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  H CODE
;;;
;;;=][=
== h                            =][=

INVOKE init-header              =][=

INVOKE sizes-n-formats          =][=

(out-push-new)

=]
hdr_file=[=(string-append tmp-dir "/" base-file-name)=].h
mask_name=[= (. mask-name) =]

sed '/^#define .*_GUARD/q' ${hdr_file}
cat <<- _EOF_
	#include <sys/types.h>
	#include <inttypes.h>

	/** integral type for holding [=(. base-type-name)=] masks */
	typedef $mask_type ${mask_name};
[=
FOR add-on-text                 =][=
  IF (= (get "ao-file") "mask-header") =]
[= ao-text                      =][=
  ENDIF correct type            =][=
ENDFOR add-on-text              =]
	_EOF_

echo "/** bits defined for ${mask_name} */"
ix=0

declare C
exec 4< ${tmp_dir}/commands
all_mask=0
while read -u4 n C
do
    C=$(echo $C | tr '[a-z]' '[A-Z]')
    v=$(( 1 << n ))
    desc=${desc_what[$n]}
    test -z "$desc" || {
        test ${#desc} -gt 72 && \
            desc=$(echo "$desc" | fmt | sed '2,$s/^/  * /')
        printf '/** %s */\n'  "$desc"
    }
    printf "$def_fmt" $C $v
    (( all_mask += v ))
done
exec 4<&-

emit_mask_def() {
    declare v=0
    declare mname=${1}
    shift
    declare which_bits='in'
    $INVERT && which_bits='omitted from'
    if test $# -eq 0
    then
        printf "\n/** There are no bits in ${mname}. */\n"
    else
        printf "\n/** bits $which_bits ${mname%_MASK} mask:\n"
        echo $* | tr ' ' '\n' | $CLexe --spread=1 -I' *  ' --end=' */'
    fi

    for f in $*
    do  eval f=\${val_$f}
        (( v |= f ))
    done
    $INVERT && (( v ^= all_mask ))
    printf "$def_fmt" ${mname} $v
    eval $(echo val_${mname}=$v | tr '[A-Z]' '[a-z]')
}
[=

FOR mask            =][=
   (set! tmp-str (string-append
       (string-upcase! (string->c-name! (get "m-name"))) "_MASK"))
   (string-append "INVERT=" (if (exist? "m-invert") "true" "false")
   " emit_mask_def " tmp-str " "
      (string->c-name! (join " " (stack "m-bit"))) "\n") =][=

ENDFOR mask

\=]
printf "\n/** all bits in ${mask_name} masks */\n"
printf "$def_fmt" MASK_ALL $all_mask[=
(define zero-mask-name (string-append "MASK_" )) =][=
IF (define zero-name (get "zero-name" "EMPTY"))
   (> (string-length zero-name) 0)  =]
printf "\n/** no bits in ${mask_name} */\n"
printf "$def_fmt" [=(string-upcase! (string->c-name! zero-name))=] 0[=
ENDIF have zero-name            =][=

IF (not (exist? "no-code"))     =]
cat <<- _EOF_

	/** buffer size needed to hold all bit names for ${mask_name} masks */
	#define MAX_[=(. BASE-TYPE)=]_NAME_SIZE [=
		(+ 1 (string-length (join " " (stack "cmd")))) =]

	extern ${mask_name}
	[=(. base-type-name) =]_str2mask(char const * str, ${mask_name} old);
[=

IF (not (exist? "no-name"))     =]
	extern size_t
	[=(. base-type-name)=]_mask2str([=(. mask-name)
	=] mask, char * buf, size_t len);
[=

ENDIF no name

=]
	_EOF_[=

ENDIF not exist no-code         =]
grep -E '^#endif .*_GUARD ' ${hdr_file}
[=
(emit (shell (out-pop #t)))
=][=

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  C CODE
;;;
;;;=][=
== c

=][=
(if (exist? "no-code") (out-delete))
(out-move (string-append base-file-name ".c"))
(out-push-new) \=]
exec 4< ${tmp_dir}/[=(. base-file-name)=].c
while IFS='' read -u4 line
do
    echo "$line"
    case "$line" in
    '#include '* ) break ;;
    esac
done

sed '1,/^#define.*_GUARD/d;s/^extern /static /;/^#endif.*_GUARD/,$d' \
    ${tmp_dir}/[=(. base-file-name)=].h

cat <<\_EOF_

#include <sys/types.h>
#include <string.h>
#ifndef NUL
#define NUL '\0'
#endif

_EOF_

sed 's/^[=(. enum-name)=]$/static [=(. enum-name)=]/
     s/^char const \*$/static char const */
     /end of .*\.c/d' <&4
exec 4<&-

[=
(shell (out-pop #t))
=]

/**
 * Convert a string to a [= (. mask-name) =] mask.
 * Bit names prefixed with a hyphen have the bit removed from the mask.
 * If the string starts with a '-', '+' or '|' character, then
 * the old value is used as a base, otherwise the result mask
 * is initialized to zero.  Separating bit names with '+' or '|'
 * characters is optional.  By default, the bits are "or"-ed into the
 * result.
 *
 * @param[in] str string with a list of bit names
 * @param[in] old previous value, used if \a str starts with a '+' or '-'.
 *
 * @returns an unsigned integer with the bits set.
 */
[= (string-append mask-name "\n" base-type-name)
=]_str2mask(char const * str, [=(. mask-name)=] old)
{
    static char const white[] = ", \t\f";
    static char const name_chars[] =
[= (shell
"name_chars=`echo '"
   (string->c-name! (join "" (stack "cmd")))
   "' | sed 's/\\(.\\)/\\1\\\\\n/g' | tr '[A-Z]' '[a-z]' | sort -u`

alpha=`echo \"$name_chars\" | grep -E  '^[a-z]$' | tr -d ' \\n'`
digit=`echo \"$name_chars\" | grep -Ev '^[a-z]$' | tr -d ' \\n'`
fmt='        \"%s\"\\n'
printf \"$fmt\" $alpha
printf \"$fmt\" `echo $alpha | tr '[a-z]' '[A-Z]'`
test -z \"${digit}\" || \
    printf \"$fmt\" ${digit}
") =];

    [=(. mask-name)=] res = 0;
    int have_data = 0;

    for (;;) {
        [=(. enum-name)=] val;
        unsigned int val_len;
        unsigned int invert = 0;

        str += strspn(str, white);
        switch (*str) {
        case NUL: return res;
        case '-': case '~':
            invert = 1;
            /* FALLTHROUGH */

        case '+': case '|':
            if (have_data == 0)
                res = old;

            str += 1 + strspn(str + 1, white);
            if (*str == NUL)
                return 0;
        }

        val_len = strspn(str, name_chars);
        if (val_len == 0)
            return 0;
        val = [=(. find-func-name)=](str, val_len);
        if (val == [=(. enum-count)=])
            return 0;
        if (invert)
            res &= ~(([=(. mask-name)=])1 << val);
        else
            res |= ([=(. mask-name)=])1 << val;
        have_data = 1;
        str += val_len;
    }
}[=

IF (not (exist? "no-name"))   =]

/**
 * Convert a [=(. mask-name)=] mask to a string.
 *
 * @param[in]  mask  the mask with the bits to be named
 * @param[out] buf   where to store the result.  This may be NULL.
 * @param[in]  len   size of the output buffer
 * @results    The full length of the space needed for the result,
 * including the terminating NUL byte.  The actual result will not
 * overwrite \a len bytes at \a buf.  This value will also never
 * exceed MAX_[=(. BASE-TYPE)=]_NAME_SIZE.
 */
size_t
[=(. base-type-name)=]_mask2str([=(. mask-name)=] mask, char * buf, size_t len)
{
    [=(. enum-name)=] val = ([=(. enum-name)=])0;
    size_t res = 0;
    if (buf == NULL) len = 0;

    for (; mask != 0; val++, mask >>= 1) {
        char const * p;
        size_t l;

        if (val >= [=(. enum-count)=])
            break;

        if ((mask & 1) == 0)
            continue;

        p = [=(. base-type-name)=]_name(val);
        if (*p == '*')
            continue; /* ignore invalid bits */

        l = strlen(p) + 1; /* includes NUL byte or spacer byte */
        if (l <= len) {
            if (res > 0)
                *(buf++) = ' ';
            memcpy(buf, p, l);
            buf += l - 1;
            len -= l;
        }
        res += l;
    }
    return (res == 0) ? 1 : res;
}[=
ENDIF dispatch=][=

  FOR add-on-text               =][=
    IF (= (get "ao-file") "mask-code") =]
[= ao-text                      =][=
    ENDIF correct type          =][=
  ENDFOR add-on-text            =][=

ESAC  suffix c/h

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;; Create the function that converts the name into an mask value.
;;;
;;;=]
/* end of [= (out-name) =] */
[=

DEFINE sizes-n-formats              =][=
(out-push-new)                      =][=

FOR mask                            =][=
  (set! tmp-str (string-append (get "m-name") "_MASK"))
  (set! idx     (string-length tmp-str))
  (if (> idx max-cmd-width) (set! max-cmd-width idx))
  =][=
ENDFOR mask

=]
sfx=U
bits=[=(. bit-count)=]
hex_width=$(( (bits + 3) / 4 ))
(( hex_width < 4 )) && hex_width=4
[=

IF (exist? "mask-type")

=]
mask_type=[= mask-type =]
(( bits > 32 )) && {
    (( bits > 64 )) && die "cannot handle a $bits bit mask"
    sfx=UL
}
[=

ELSE mask type not provided

=]
if (( bits <= 8 ))
then mask_type='uint8_t'
elif (( bits <= 16 ))
then mask_type='uint16_t'
elif (( bits <= 32 ))
then mask_type='uint32_t'
elif (( bits <= 64 ))
then mask_type='uint64_t'
     sfx=UL
else
    die "cannot handle a $bits bit mask"
fi
[=

ENDIF mask type provided/not

=]
hex_fmt=0x%0${hex_width}X${sfx}
def_fmt="#define [=(string-append PFX-STR ENUM-TYPE "_")
=]%-[=(. max-cmd-width)=]s ${hex_fmt}\\n"
[=

(shell (out-pop #t))                =][=

ENDDEF sizes-n-formats

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  Initialize for the header
;;;
;;;=][=

DEFINE init-header                  =][=

(if (exist? "dispatch")
    (error "bit masks do not have dispatching available"))

(if (exist? "alias")
    (error "bit name aliases are not allowed"))

(define enum-type (if (exist? "prefix") "" "bit"))

=][=

INCLUDE "str2init.tlib"

=][=

(out-move ".Str2Mask-Set-Aside")
(define bit-enum-type
    (string-append (if (== enum-type "bit") "" enum-type) "bnm"))
(define enum-count      (string-append PFX-STR "_COUNT_"
       (string-upcase bit-enum-type)))
(define assign-vals "")
(define find-func-name  (string-append
   "find_" base-type-name "_" bit-enum-type ))
(out-push-new (string-append tmp-dir "/" base-file-name ".def"))
=]
AutoGen Definitions str2enum;
prefix  = '[=(. pfx-str)=]';
type    = '[=(string-downcase bit-enum-type)=]';
invalid-name = '[=(. invalid-name)=]';
invalid-val  = '';
[=

FOR cmd     =][=
    (set! tmp-str (get "cmd"))
    (set! idx (for-index))
    (ag-fprintf 0 "cmd[%u] = '%s';\n" idx tmp-str)
    (set! tmp-str (string-downcase! (string->c-name! tmp-str)))
    (set! idx (ash 1 idx))
    (set! assign-vals (string-append assign-vals
          "val_" tmp-str "=" (number->string idx) "\n"))
            =][=
ENDFOR      =][=
(if (exist? "no-code") (emit "no-code;\n"))
(if (exist? "partial") (emit "partial;\n"))
(if (exist? "no-name") (emit "no-name;\n"))
(out-pop)
(shell assign-vals
"{ ${AGexe} -L" (dirname (tpl-file #t)) " ${tmp_dir}/" base-file-name ".def"
" || die 'Could not build enumeration\n'"
      "\"`cat ${tmp_dir}/" base-file-name ".def`\"\n"
"cp " base-file-name ".[ch] ${tmp_dir}/.\n"
"rm -f " base-file-name ".[ch]\n} 1>&2")
(out-move (string-append base-file-name ".h"))

=][=

ENDDEF init-header

 * Local Variables:
 * mode: text
 * indent-tabs-mode: nil
 * End:
 * end of str2mask.tpl \=]
