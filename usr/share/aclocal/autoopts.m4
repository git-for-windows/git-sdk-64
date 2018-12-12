dnl  -*- Mode: M4 -*-
dnl --------------------------------------------------------------------
dnl autoopts.m4 --- Configure paths for autoopts
dnl
dnl Author:            Gary V. Vaughan <gvaughan@localhost>
dnl  This file is part of AutoOpts, a companion to AutoGen.
dnl  AutoOpts is free software.
dnl  AutoOpts is Copyright (C) 1992-2018 by Bruce Korb - all rights reserved
dnl
dnl  AutoOpts is available under any one of two licenses.  The license
dnl  in use must be one of these two and the choice is under the control
dnl  of the user of the license.
dnl
dnl   The GNU Lesser General Public License, version 3 or later
dnl      See the files "COPYING.lgplv3" and "COPYING.gplv3"
dnl
dnl   The Modified Berkeley Software Distribution License
dnl      See the file "COPYING.mbsd"
dnl
dnl  These files have the following sha256 sums:
dnl
dnl  8584710e9b04216a394078dc156b781d0b47e1729104d666658aecef8ee32e95  COPYING.gplv3
dnl  4379e7444a0e2ce2b12dd6f5a52a27a4d02d39d247901d3285c88cf0d37f477b  COPYING.lgplv3
dnl  13aa749a5b0a454917a944ed8fffc530b784f5ead522b1aacaf4ec8aa55a6239  COPYING.mbsd
dnl --------------------------------------------------------------------
dnl Code:

# serial 1

dnl AG_PATH_AUTOOPTS([MIN-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for AUTOOPTS, and define AUTOGEN, AUTOOPTS_CFLAGS, AUTOGEN_LDFLAGS
dnl      and AUTOOPTS_LIBS.
dnl
AC_DEFUN([AG_PATH_AUTOOPTS],
[dnl Get the cflags and libraries from the autoopts-config script
AC_ARG_WITH(opts-prefix,
[  --with-opts-prefix=PFX  Prefix where autoopts is installed (optional)])

AC_ARG_WITH(opts-exec-prefix,
[  --with-opts-exec-prefix=PFX
                          Exec prefix where autoopts is installed (optional)])

AC_ARG_ENABLE(opts-test,
[  --disable-opts-test     Do not try to run a test AutoOpts program])

  if test x$with_opts_exec_prefix != x ; then
    aocfg_args="$aocfg_args --exec-prefix=$with_opts_exec_prefix"
    if test x${AUTOOPTS_CONFIG+set} != xset ; then
      AUTOOPTS_CONFIG=$with_opts_exec_prefix/bin/autoopts-config
    fi
  fi
  if test x$with_opts_prefix != x ; then
     aocfg_args="$aocfg_args --prefix=$with_opts_prefix"
    if test x${AUTOOPTS_CONFIG+set} != xset ; then
      AUTOOPTS_CONFIG=$with_opts_prefix/bin/autoopts-config
    fi
  fi
  if test -n "$AUTOOPTS_CONFIG"; then
    :
  else
    AC_PATH_PROG(AUTOOPTS_CONFIG, autoopts-config, no)
  fi
  AC_MSG_CHECKING(for compatible autoopts version)[
  no_autoopts=""
  if test "$AUTOOPTS_CONFIG" = "no" ; then
    no_autoopts=yes
  else
    AUTOGEN=`$AUTOOPTS_CONFIG $aocfg_args --autogen`
    AUTOOPTS_CFLAGS=`$AUTOOPTS_CONFIG $aocfg_args --cflags`
    AUTOGEN_LDFLAGS=`$AUTOOPTS_CONFIG $aocfg_args --pkgdatadir`
    AUTOOPTS_LIBS=`$AUTOOPTS_CONFIG $aocfg_args --libs`
    aocfg_version=`$AUTOOPTS_CONFIG $aocfg_args --version`
    save_IFS=$IFS
    IFS=' :'
    set -- $aocfg_version
    IFS=$save_IFS
    aocfg_current=$1
    aocfg_revision=$2
    aocfg_age=$3
    aocfg_currev=$1.$2
    if test "x$enable_opts_test" != "xno" ; then
      AC_LANG_SAVE
      AC_LANG_C
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LDFLAGS="$LDFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $AUTOOPTS_CFLAGS"
      LDFLAGS="$LDFLAGS $AUTOOPTS $CFLAGS"
      LIBS="$LIBS $AUTOOPTS_LIBS"]
      dnl
      dnl Now check if the installed AUTOOPTS is sufficiently new. (Also
      dnl sanity checks the results of autoopts-config to some extent.
      dnl
      rm -f confopts.def conf.optstest
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <autoopts/options.h>
#ifndef OPTIONS_VER_TO_NUM
#define  OPTIONS_VER_TO_NUM(_v, _r) (((_v) * 4096) + (_r))
#endif

static char const zBadVer[] = "\n\\
*** 'autoopts-config --version' returned $aocfg_version,\n\\
***                but autoopts returned %d:%d:0\n\\
***             and the header file says %s\n\\
*** These should all be consistent.\n\n\\
*** If autoopts-config was correct, then it is best to remove the old version\n\\
*** of autoopts. You may also be able to fix the error by modifying your\n\\
*** LD_LIBRARY_PATH enviroment variable, or by editing /etc/ld.so.conf.\n\\
*** Make sure you have run ldconfig if that is required on your system.\n\\
*** Otherwise, set the environment variable AUTOOPTS_CONFIG to point to\n\\
*** the correct copy of autoopts-config, and remove the file config.cache\n\\
*** before re-running configure.\n";

int
main (int argc, char ** argv)
{
    int current, revision, ct;
    char tmp_version[256];

    system ("touch conf.optstest");

    /*
     *  Test liked library against header file
     */
    strcpy(tmp_version, optionVersion());
    ct = sscanf(tmp_version, "%d.%d", &current, &revision);
    if (ct != 2) {
        printf("bad version string: -->>%s<<-- != -->>$aocfg_currev<<--\n",
               optionVersion());
        return 1;
    }

    if (OPTIONS_VER_TO_NUM(current, revision) != OPTIONS_STRUCT_VERSION) {
        printf(zBadVer, current, revision, OPTIONS_VERSION_STRING);
        return 1;
    }

    /*
     *  Test autoopts-config against header version
     */
    if (   OPTIONS_VER_TO_NUM($aocfg_current, $aocfg_revision)
        != OPTIONS_STRUCT_VERSION) {
        printf("*** autoopts header file version "OPTIONS_VERSION_STRING"\n"
               "*** does not match autoopts-config value $aocfg_version\n"
               "*** library version is %d:%d\n", current, revision);
        return 1;
    }

    return 0;
}
],, no_autoopts=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
      CFLAGS="$ac_save_CFLAGS"
      LDFLAGS="$ac_save_LDFLAGS"
      LIBS="$ac_save_LIBS"
      AC_LANG_RESTORE
    fi
  fi

  if test "x$no_autoopts" = x ; then
    AC_MSG_RESULT(yes)
    ifelse([$2], , :, [$2])
  else
    AC_MSG_RESULT(no)
    if test "$AUTOOPTS_CONFIG" = "no" ; then
      cat <<- _EOF_
	*** The autoopts-config script installed by AutoGen could not be found
	*** If AutoGen was installed in PREFIX, make sure PREFIX/bin is in
	*** your path, or set the AUTOOPTS_CONFIG environment variable to the
	*** full path to autoopts-config.
	_EOF_
     else
       if test -f conf.optstest ; then
         :
       else
         echo "*** Could not run autoopts test program, checking why..."
         CFLAGS="$CFLAGS $AUTOOPTS_CFLAGS"
         LIBS="$LIBS $AUTOOPTS_LIBS"
         AC_LANG_SAVE
         AC_LANG_C
         AC_TRY_LINK([
#include <autoopts/options.h>
#include <stdio.h>
], [return strcmp("$aocfg_current:$aocfg_revision:$aocfg_age", optionVersion());],
        [ cat << _EOF_
*** The test program compiled, but did not run. This usually means that
*** the run-time linker is not finding libopts or finding the wrong version
*** of libopts. If it is not finding libopts, you'll need to set your
*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point
*** to the installed location  Also, make sure you have run ldconfig if that
*** is required on your system
***
*** If you have an old version installed, it is best to remove it, although
*** you may also be able to get things to work by modifying LD_LIBRARY_PATH
_EOF_
], [cat << _EOF_
*** The test program failed to compile or link. See the file config.log for
*** the exact error that occured. This usually means AutoGen was incorrectly
*** installed or that you have moved libopts since it was installed. In the
*** latter case, you may want to edit the autoopts-config script:
*** $AUTOOPTS_CONFIG
_EOF_
])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
          AC_LANG_RESTORE
       fi
     fi
     AUTOGEN=:
     AUTOOPTS_CFLAGS=""
     AUTOOPTS_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(AUTOGEN)
  AC_SUBST(AUTOOPTS_CFLAGS)
  AC_SUBST(AUTOGEN_LDFLAGS)
  AC_SUBST(AUTOOPTS_LIBS)
  rm -f confopts.def conf.optstest
])
dnl
dnl autoopts.m4 ends here
