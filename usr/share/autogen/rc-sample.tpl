[= AutoGen5 Template rc

##  This file is part of AutoOpts, a companion to AutoGen.
##  AutoOpts is free software.
##  AutoOpts is Copyright (C) 1992-2018 by Bruce Korb - all rights reserved
##
##  AutoOpts is available under any one of two licenses.  The license
##  in use must be one of these two and the choice is under the control
##  of the user of the license.
##
##   The GNU Lesser General Public License, version 3 or later
##      See the files "COPYING.lgplv3" and "COPYING.gplv3"
##
##   The Modified Berkeley Software Distribution License
##      See the file "COPYING.mbsd"
##
##  These files have the following sha256 sums:
##
##  8584710e9b04216a394078dc156b781d0b47e1729104d666658aecef8ee32e95  COPYING.gplv3
##  4379e7444a0e2ce2b12dd6f5a52a27a4d02d39d247901d3285c88cf0d37f477b  COPYING.lgplv3
##  13aa749a5b0a454917a944ed8fffc530b784f5ead522b1aacaf4ec8aa55a6239  COPYING.mbsd

=]
# [= (define prog-name (get "prog-name"))
     prog-name =] sample configuration file
#[=

IF (if (not (exist? "homerc"))
       (error "RC file samples only work for rc-optioned programs")  )

   (out-move (string-append "sample-"
                (if (exist? "rcfile") (get "rcfile")
                    (string-append (get "prog-name") "rc")  )
   )         )

   (set-writable)

   (exist? "copyright")
\=]
# This source file is copyrighted and licensed under the following terms:
#
[=
  CASE copyright.type                   =][=
  == ""   =][=
   (sprintf "#  %s Copyright (C) %s %s - all rights reserved\n#  %s"
     prog-name (get "copyright.date") (get "copyright.owner")
     "licensing type not specified" )   =][=

  = note  =][= (prefix "#  "  (get "copyright.text")) =][=

  *       =][= (license-full (get "copyright.type") prog-name "#  "
                   (get "copyright.owner") (get "copyright.date")) =][=
  ESAC                                  =][=

ENDIF "copyright exists"                =][=

FOR flag                                =][=

  IF (not (or (exist? "documentation") (exist? "no-preset")))     =]

# [= name =] -- [= descrip =]
#
[= INVOKE emit-description =]
# Example:
#
#[= name =][=
    IF (exist? "arg-type")
  =]	[= (if (exist? "arg-default") (get "arg-default")
           (if (exist? "arg-name")    (get "arg-name")
               (get "arg-type")  ))     =][=
    ENDIF (exist? "arg-type")           =][=

  ENDIF (not (exist? "documentation"))  =][=

ENDFOR flag

= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =][=

DEFINE emit-description =][=
(out-push-new)          =][=

  IF (~* (get "arg-type") "key|set")
=]This configuration value takes a keyword as its argument[=

    IF (=* (get "arg-type") "set")

=] list.  Each entry turns on or off membership bits.  The bits are set by [=#
=]name or numeric value and cleared by preceding the name or number with an [=#
=]exclamation character ('!').  [=

    ELSE

=].  [=

    ENDIF

=]The available keywords are:  [=
            (join ", " (stack "keyword")) =].  [=

  ELIF (=* (get "arg-type") "num")
 =]This configuration value takes an integer number as its argument.  [=
    IF (exist? "scaled") =]That number may be scaled with a single letter [=#
=]suffix:  k/K/m/M/g/G/t/T  These will multiply the value by powers of [=#
=]1000 (lower case) or 1024 (upper case).  [=
    ENDIF   =][=

  ENDIF     =][=

  (define fill-txt (out-pop #t))
  (if (defined? 'fill-txt)
      (string-append

         (shell (string-append "while read line
         do echo ${line} | fold -s -w76 | sed 's/^/# /'
            echo '#'
         done <<'__EndOfText__'\n" fill-txt "\n__EndOfText__" ))

         "\n#\n"
  )   ) =][=

  (if (exist? "doc") (prefix "# " (get "doc"))) =][=

ENDDEF emit-description

=]
