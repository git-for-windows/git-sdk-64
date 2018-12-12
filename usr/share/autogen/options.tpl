[= Autogen5 Template -*- Mode: scheme -*-

h
c

=][=
# This file contains the templates used to generate the
# option descriptions for client programs, and it declares
# the macros used in the templates.

#  This file is part of AutoOpts, a companion to AutoGen.
#  AutoOpts is free software.
#  AutoOpts is Copyright (C) 1992-2018 by Bruce Korb - all rights reserved
#
#  AutoOpts is available under any one of two licenses.  The license
#  in use must be one of these two and the choice is under the control
#  of the user of the license.
#
#   The GNU Lesser General Public License, version 3 or later
#      See the files "COPYING.lgplv3" and "COPYING.gplv3"
#
#   The Modified Berkeley Software Distribution License
#      See the file "COPYING.mbsd"
#
#  These files have the following sha256 sums:
#
#  8584710e9b04216a394078dc156b781d0b47e1729104d666658aecef8ee32e95  COPYING.gplv3
#  4379e7444a0e2ce2b12dd6f5a52a27a4d02d39d247901d3285c88cf0d37f477b  COPYING.lgplv3
#  13aa749a5b0a454917a944ed8fffc530b784f5ead522b1aacaf4ec8aa55a6239  COPYING.mbsd
=][=

(shell "CLexe=`echo ${AGexe} | sed 's@/autogen@/columns@'`
       test -x \"${CLexe}\" || CLexe=`which columns`")


 (dne " *  " "/*  ")        =][=

CASE    (suffix)            =][=

== h                        =][=

  INCLUDE "optlib.tlib"     =][=
  INVOKE  init-and-validate =][=
  INVOKE  option-copyright  =][=
  INCLUDE "opthead.tlib"    =][=

== c                        =][=

  (if (exist? "library")
      (out-delete))         =][=

  INVOKE  option-copyright  =][=
  INCLUDE "optcode.tlib"    =][=

  (if (exist? "flag.extract-code")
      (shellf "test -f %1$s.c && rm -f %1$s.c.save" (base-name)))  =][=

ESAC =][=
IF (exist? "addtogroup")    =]
/** @} */[= ENDIF           =]
/* [= (out-name) =] ends here */[=

# options.tpl ends here     =]
