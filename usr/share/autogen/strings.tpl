[= AutoGen5 Template c -*- Mode: scheme -*-

## Author:          Bruce Korb <bkorb@gnu.org>
##
## Copyright (C) 2011-2018 Bruce Korb, all rights reserved.
## This is free software. It is licensed for use, modification and
## redistribution under the terms of the
## Modified (3 clause) Berkeley Software Distribution License
##   <http://www.xfree86.org/3.3.6/COPYRIGHT2.html>
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
## 3. Neither the name ``Bruce Korb'' nor the name of any other
##    contributor may be used to endorse or promote products derived
##    from this software without specific prior written permission.
##
## strings IS PROVIDED BY Bruce Korb ``AS IS'' AND ANY EXPRESS
## OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
## ARE DISCLAIMED.  IN NO EVENT SHALL Bruce Korb OR ANY OTHER CONTRIBUTORS
## BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
## CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
## SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
## BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
## OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
## ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=]
[= INCLUDE "tpl-config.tlib" =][=

(define copy-years (shell
   "sed -n '/Copyright (C)/ {
      s/.*(C) *//
      s/ *Bruce.*//
      p
      q
   }' " (tpl-file #t) ))  \=]
[= INVOKE leader guard = false  \=]
[= (out-push-new (string-append (base-name) ".h")) \=]
[= INVOKE leader guard = true    =]
[= (out-suspend  "header") ;; resume defines
   (define string-name (string->c-name! (string-append
                       (base-name) "-strtable")))
   (string-table-new string-name)
   (define max-name-len 0)
   (define tmp-str     "")
   (define tmp-len      0)      \=]
#include "[= (. header-file) =]"
[=

(define str-nm  "")
(define str-val "")
(define str-ct   0)
(define name-ln  0)
(define find-ln "")
(define def-fmt "#define %%-%us     (%%s)\n#define %%-%us %%d\n")
(out-push-new) ;; temp file for #defines
=][=
FOR string                      =][=
   (set! find-ln  (string-length (get "nm" "")))
   (if (> find-ln name-ln)
       (set! name-ln find-ln))  =][=
ENDFOR string                   =][=

(if (exist? "file-name-string") (begin
      (set! str-nm     (string-upcase (string-append string-name "_file")))
      (set! find-ln    (string-length str-nm))
      (if (> find-ln name-ln)
          (set! name-ln find-ln))
      (set! def-fmt (sprintf def-fmt (+ 1 name-ln) (+ 5 name-ln)))
      (set! str-ct     1)
      (set! str-val    (string-append (base-name) ".c"))
      (set! tmp-str    (string-table-add-ref string-name str-val))
      (ag-fprintf 0 def-fmt str-nm tmp-str
                       (string-append str-nm "_LEN") (string-length str-val))
    )

    (set! def-fmt (sprintf def-fmt (+ 1 name-ln) (+ 5 name-ln)))
)

(set! find-ln "")               =][=

FOR string                      =][=
   (set! str-nm  (get "nm"))
   (set! str-ct  (+ str-ct 1))
   (set! str-val (get "str" str-nm))
   (set! tmp-str (string-table-add-ref string-name str-val))
   (if (exist? "define-line-no")
       (set! find-ln (string-append find-ln str-nm " " tmp-str "\n")) )
   (sprintf def-fmt str-nm tmp-str
                    (string-append str-nm "_LEN") (string-length str-val)) =][=
ENDFOR string                   =][=

(out-suspend "defines")
(out-resume  "header")  ;; real header file
(ag-fprintf 0
    "/*\n * %d strings in %s string table\n */\n" str-ct string-name)
(out-resume  "defines")
(emit (shell (string-append "sort <<\\_EOF_\n"
             (out-pop #t) "_EOF_"))) ;; #defines now in real header file
(emit "\n")
(out-push-new)
(emit-string-table string-name)
(define str-table (out-pop #t))
(emit (shell (string-append
   "sed -n '/static char const/ {
        s/static char/extern char/
        s/ *=.*/;/
        p
        q
   }' <<\\_EOF_\n"
   str-table
   "\n_EOF_"
)))
(out-suspend "header") ;; resuming text output
(shell (string-append
   "sed 's/^static char const/char const/' <<\\_EOF_\n"
   str-table
   "\n_EOF_"
))                              =][=

IF (out-resume "header") ;; real header file
   (> (string-length find-ln) 0)=]

[= (out-push-new) =]
while read nm ln
do
    test -z "$nm" && break
    ln='/\* *'${ln#*+}' \*/'
    ln=`[=(. egrep-prog)=] -n "$ln" [= (base-name) =].c`
    nm=`echo $nm | tr '[a-z]' '[A-Z]'`_LINENO
    printf '#define %-31s %s\n' ${nm} ${ln%%:*}
done <<\_EOF_
[= (. find-ln) =]_EOF_[=

(shell (out-pop #t))            =][=
ENDIF find-ln not empty         =][=
(if (exist? "header-trailer")
    (emit (join "\n\n" "header-trailer")) ) \=]


#endif /* [= (. header-guard)   =] */
[= (out-pop)                    =][=

DEFINE leader \=][=
 (emit (dne " * " "/* "))
 (emit "\n *\n")
 (emit (license-full "mbsd" "strings" " *  " "Bruce Korb" copy-years))
 (emit "\n */\n")
 (if (= (get "guard") "true")
     (emit (string-append
        (make-header-guard "strings")
        (if (exist? "header-leader") (string-append "\n\n"
            (join "\n\n" "header-leader") ) "" ) ))
     (if (exist? "code-leader")
         (emit (string-append "\n\n" (join "\n\n" "code-leader"))) )
 )
=][=

ENDDEF leader =][=
(if (exist? "code-trailer")
    (emit (join "\n\n" "code-trailer")) ) =]

/* end of [= (out-name) =] */
