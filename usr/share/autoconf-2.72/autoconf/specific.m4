# This file is part of Autoconf.			-*- Autoconf -*-
# Macros that test for specific, unclassified, features.
#
# Copyright (C) 1992-1996, 1998-2017, 2020-2023 Free Software
# Foundation, Inc.

# This file is part of Autoconf.  This program is free
# software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the Autoconf Configure Script Exception,
# version 3.0, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License
# and a copy of the Autoconf Configure Script Exception along with
# this program; see the files COPYINGv3 and COPYING.EXCEPTION
# respectively.  If not, see <https://www.gnu.org/licenses/>.

# Written by David MacKenzie, with help from
# François Pinard, Karl Berry, Richard Pixley, Ian Lance Taylor,
# Roland McGrath, Noah Friedman, david d zuhn, and many others.


## ------------------------- ##
## Checks for declarations.  ##
## ------------------------- ##


# AC_DECL_SYS_SIGLIST
# -------------------
AN_IDENTIFIER([sys_siglist],     [AC_CHECK_DECLS([sys_siglist])])
AU_DEFUN([AC_DECL_SYS_SIGLIST],
[AC_CHECK_DECLS([sys_siglist],,,
[#include <signal.h>
/* NetBSD declares sys_siglist in unistd.h.  */
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
])
])# AC_DECL_SYS_SIGLIST




## -------------------------------------- ##
## Checks for operating system services.  ##
## -------------------------------------- ##


# AC_SYS_INTERPRETER
# ------------------
AC_DEFUN([AC_SYS_INTERPRETER],
[AC_CACHE_CHECK(whether @%:@! works in shell scripts, ac_cv_sys_interpreter,
[echo '#! /bin/cat
exit 69
' >conftest
chmod u+x conftest
(SHELL=/bin/sh; export SHELL; ./conftest >/dev/null 2>&1)
if test $? -ne 69; then
   ac_cv_sys_interpreter=yes
else
   ac_cv_sys_interpreter=no
fi
rm -f conftest])
interpval=$ac_cv_sys_interpreter
])


AU_DEFUN([AC_HAVE_POUNDBANG],
[AC_SYS_INTERPRETER],
[Remove this warning when you adjust your code to use
`AC_SYS_INTERPRETER'.])


AU_DEFUN([AC_ARG_ARRAY], [],
[$0 is no longer implemented: don't do unportable things
with arguments. Remove this warning when you adjust your code.])


# _AC_SYS_YEAR2038_TEST_CODE
# --------------------------
# C code used to probe for time_t that can represent time points more
# than 2**31 - 1 seconds after the epoch.  With the usual Unix epoch,
# these correspond to dates after 2038-01-18 22:14:07 +0000 (Gregorian),
# hence the name.
AC_DEFUN([_AC_SYS_YEAR2038_TEST_CODE],
[[
  #include <time.h>
  /* Check that time_t can represent 2**32 - 1 correctly.  */
  #define LARGE_TIME_T \\
    ((time_t) (((time_t) 1 << 30) - 1 + 3 * ((time_t) 1 << 30)))
  int verify_time_t_range[(LARGE_TIME_T / 65537 == 65535
                           && LARGE_TIME_T % 65537 == 0)
                          ? 1 : -1];
]])

# _AC_SYS_YEAR2038_OPTIONS
# ------------------------
# List of known ways to enable support for large time_t.  If you change
# this list you probably also need to change the AS_CASE at the end of
# _AC_SYS_YEAR2038_PROBE.
m4_define([_AC_SYS_YEAR2038_OPTIONS], m4_normalize(
    ["none needed"]                   dnl 64-bit and newer 32-bit Unix
    ["-D_TIME_BITS=64"]               dnl glibc 2.34 with some 32-bit ABIs
    ["-D__MINGW_USE_VC2005_COMPAT"]   dnl 32-bit MinGW
    ["-U_USE_32_BIT_TIME_T -D__MINGW_USE_VC2005_COMPAT"]
                                      dnl 32-bit MinGW (misconfiguration)
))

# _AC_SYS_YEAR2038_PROBE
# ----------------------
# Subroutine of AC_SYS_YEAR2038.  Probe for time_t that can represent
# time points more than 2**31 - 1 seconds after the epoch (dates after
# 2038-01-18, see above) and set the cache variable ac_cv_sys_year2038_opts
# to one of the values in the _AC_SYS_YEAR2038_OPTIONS list, or to
# "support not detected" if none of them worked.  Then, set compilation
# options and #defines as necessary to enable large time_t support.
#
# Note that we do not test whether mktime, localtime, etc. handle
# large values of time_t correctly, as that would require use of
# AC_TRY_RUN.  Note also that some systems only support large time_t
# together with large off_t.
#
# If you change this macro you may also need to change
# _AC_SYS_YEAR2038_OPTIONS.
AC_DEFUN([_AC_SYS_YEAR2038_PROBE],
[AC_CACHE_CHECK([for $CC option for timestamps after 2038],
  [ac_cv_sys_year2038_opts],
  [ac_save_CPPFLAGS="$CPPFLAGS"
  ac_opt_found=no
  for ac_opt in _AC_SYS_YEAR2038_OPTIONS; do
    AS_IF([test x"$ac_opt" != x"none needed"],
      [CPPFLAGS="$ac_save_CPPFLAGS $ac_opt"])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([_AC_SYS_YEAR2038_TEST_CODE])],
      [ac_cv_sys_year2038_opts="$ac_opt"
      ac_opt_found=yes])
    test $ac_opt_found = no || break
  done
  CPPFLAGS="$ac_save_CPPFLAGS"
  test $ac_opt_found = yes || ac_cv_sys_year2038_opts="support not detected"])

ac_have_year2038=yes
AS_CASE([$ac_cv_sys_year2038_opts],
  ["none needed"], [],
  ["support not detected"],
    [ac_have_year2038=no],

  ["-D_TIME_BITS=64"],
    [AC_DEFINE([_TIME_BITS], [64],
      [Number of bits in time_t, on hosts where this is settable.])],

  ["-D__MINGW_USE_VC2005_COMPAT"],
    [AC_DEFINE([__MINGW_USE_VC2005_COMPAT], [1],
      [Define to 1 on platforms where this makes time_t a 64-bit type.])],

  ["-U_USE_32_BIT_TIME_T"*],
    [AC_MSG_FAILURE(m4_text_wrap(
      [the 'time_t' type is currently forced to be 32-bit.
       It will stop working after mid-January 2038.
       Remove _USE_32BIT_TIME_T from the compiler flags.],
      [], [], [55]))],

  [AC_MSG_ERROR(
    [internal error: bad value for \$ac_cv_sys_year2038_opts])])
])

# _AC_SYS_YEAR2038_ENABLE
# -----------------------
# Depending on which of the YEAR2038 macros was used, add either an
# --enable-year2038 or a --disable-year2038 to
# the configure script.  This is expanded very late and
# therefore there cannot be any code in the AC_ARG_ENABLE.  The
# default value for 'enable_year2038' is emitted unconditionally
# because the generated code always looks at this variable.
m4_define([_AC_SYS_YEAR2038_ENABLE],
[m4_divert_text([DEFAULTS],
  m4_provide_if([AC_SYS_YEAR2038],
    [enable_year2038=yes],
    [enable_year2038=no]))]dnl
[AC_ARG_ENABLE([year2038],
  m4_provide_if([AC_SYS_YEAR2038],
    [AS_HELP_STRING([--disable-year2038],
      [don't support timestamps after 2038])],
    [AS_HELP_STRING([--enable-year2038],
      [support timestamps after 2038])]))])

# AC_SYS_YEAR2038
# ---------------
# Attempt to detect and activate support for large time_t.
# On systems where time_t is not always 64 bits, this probe can be
# skipped by passing the --disable-year2038 option to configure.
AC_DEFUN([AC_SYS_YEAR2038],
[AC_REQUIRE([AC_SYS_LARGEFILE])dnl
AS_IF([test "$enable_year2038,$ac_have_year2038,$cross_compiling" = yes,no,no],
 [# If we're not cross compiling and 'touch' works with a large
  # timestamp, then we can presume the system supports wider time_t
  # *somehow* and we just weren't able to detect it.  One common
  # case that we deliberately *don't* probe for is a system that
  # supports both 32- and 64-bit ABIs but only the 64-bit ABI offers
  # wide time_t.  (It would be inappropriate for us to override an
  # intentional use of -m32.)  Error out, demanding use of
  # --disable-year2038 if this is intentional.
  AS_IF([TZ=UTC0 touch -t 210602070628.15 conftest.time 2>/dev/null],
    [AS_CASE([`TZ=UTC0 LC_ALL=C ls -l conftest.time 2>/dev/null`],
       [*'Feb  7  2106'* | *'Feb  7 17:10'*],
       [AC_MSG_FAILURE(m4_text_wrap(
	  [this system appears to support timestamps after mid-January 2038,
	   but no mechanism for enabling wide 'time_t' was detected.
	   Did you mean to build a 64-bit binary? (E.g., 'CC="${CC} -m64"'.)
	   To proceed with 32-bit time_t, configure with '--disable-year2038'.],
	  [], [], [55]))])])])])

# AC_SYS_YEAR2038_RECOMMENDED
# ---------------------------
# Same as AC_SYS_YEAR2038, but recommend support for large time_t.
# If we cannot find any way to make time_t capable of representing
# values larger than 2**31 - 1, error out unless --disable-year2038 is given.
AC_DEFUN([AC_SYS_YEAR2038_RECOMMENDED],
[AC_REQUIRE([AC_SYS_YEAR2038])dnl
AS_IF([test "$enable_year2038,$ac_have_year2038" = yes,no],
   [AC_MSG_FAILURE(m4_text_wrap(
      [could not enable timestamps after mid-January 2038.
       This package recommends support for these later timestamps.
       However, to proceed with signed 32-bit time_t even though it
       will fail then, configure with '--disable-year2038'.],
      [], [], [55]))])])

# _AC_SYS_LARGEFILE_TEST_CODE
# ---------------------------
# C code used to probe for large file support.
m4_define([_AC_SYS_LARGEFILE_TEST_CODE],
[@%:@include <sys/types.h>
@%:@ifndef FTYPE
@%:@ define FTYPE off_t
@%:@endif
 /* Check that FTYPE can represent 2**63 - 1 correctly.
    We can't simply define LARGE_FTYPE to be 9223372036854775807,
    since some C++ compilers masquerading as C compilers
    incorrectly reject 9223372036854775807.  */
@%:@define LARGE_FTYPE (((FTYPE) 1 << 31 << 31) - 1 + ((FTYPE) 1 << 31 << 31))
  int FTYPE_is_large[[(LARGE_FTYPE % 2147483629 == 721
		       && LARGE_FTYPE % 2147483647 == 1)
		      ? 1 : -1]];[]dnl
])
# Defined by Autoconf 2.71 and circa 2022 Gnulib unwisely depended on it.
m4_define([_AC_SYS_LARGEFILE_TEST_INCLUDES], [_AC_SYS_LARGEFILE_TEST_CODE])

# _AC_SYS_LARGEFILE_OPTIONS
# -------------------------
# List of known ways to enable support for large files.  If you change
# this list you probably also need to change the AS_CASE at the end of
# _AC_SYS_LARGEFILE_PROBE.
m4_define([_AC_SYS_LARGEFILE_OPTIONS], m4_normalize(
    ["none needed"]                   dnl Most current systems
    ["-D_FILE_OFFSET_BITS=64"]        dnl X/Open LFS spec
    ["-D_LARGE_FILES=1"]              dnl 32-bit AIX 4.2.1+, 32-bit z/OS
    ["-n32"]                          dnl 32-bit IRIX 6, SGI cc (obsolete)
))

# _AC_SYS_LARGEFILE_PROBE
# -----------------------
# Subroutine of AC_SYS_LARGEFILE. Probe for large file support and set
# the cache variable ac_cv_sys_largefile_opts to one of the values in
# the _AC_SYS_LARGEFILE_OPTIONS list, or to "support not detected" if
# none of the options in that list worked.  Then, set compilation
# options and #defines as necessary to enable large file support.
#
# If large file support is not detected, the behavior depends on which of
# the top-level AC_SYS_LARGEFILE macros was used (see below).
#
# If you change this macro you may also need to change
# _AC_SYS_LARGEFILE_OPTIONS.
AC_DEFUN([_AC_SYS_LARGEFILE_PROBE],
[AC_CACHE_CHECK([for $CC option to enable large file support],
  [ac_cv_sys_largefile_opts],
  [ac_save_CC="$CC"
  ac_opt_found=no
  for ac_opt in _AC_SYS_LARGEFILE_OPTIONS; do
    AS_IF([test x"$ac_opt" != x"none needed"],
      [CC="$ac_save_CC $ac_opt"])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([_AC_SYS_LARGEFILE_TEST_CODE])],
     [AS_IF([test x"$ac_opt" = x"none needed"],
	[# GNU/Linux s390x and alpha need _FILE_OFFSET_BITS=64 for wide ino_t.
	 CC="$CC -DFTYPE=ino_t"
	 AC_COMPILE_IFELSE([], [],
	   [CC="$CC -D_FILE_OFFSET_BITS=64"
	    AC_COMPILE_IFELSE([], [ac_opt='-D_FILE_OFFSET_BITS=64'])])])
      ac_cv_sys_largefile_opts=$ac_opt
      ac_opt_found=yes])
    test $ac_opt_found = no || break
  done
  CC="$ac_save_CC"
  dnl Gnulib implements large file support for native Windows, based on the
  dnl variables WINDOWS_64_BIT_OFF_T, WINDOWS_64_BIT_ST_SIZE.
  m4_ifdef([gl_LARGEFILE], [
    AC_REQUIRE([AC_CANONICAL_HOST])
    if test $ac_opt_found != yes; then
      AS_CASE([$host_os],
        [mingw* | windows*],
          [ac_cv_sys_largefile_opts="supported through gnulib"
           ac_opt_found=yes]
      )
    fi
  ])
  test $ac_opt_found = yes || ac_cv_sys_largefile_opts="support not detected"])

ac_have_largefile=yes
AS_CASE([$ac_cv_sys_largefile_opts],
  ["none needed"], [],
  ["supported through gnulib"], [],
  ["support not detected"],
    [ac_have_largefile=no],

  ["-D_FILE_OFFSET_BITS=64"],
    [AC_DEFINE([_FILE_OFFSET_BITS], [64],
      [Number of bits in a file offset, on hosts where this is settable.])],

  ["-D_LARGE_FILES=1"],
    [AC_DEFINE([_LARGE_FILES], [1],
      [Define to 1 on platforms where this makes off_t a 64-bit type.])],

  ["-n32"],
    [CC="$CC -n32"],

  [AC_MSG_ERROR(
    [internal error: bad value for \$ac_cv_sys_largefile_opts])])

AS_IF([test "$enable_year2038" != no],
  [_AC_SYS_YEAR2038_PROBE])
AC_CONFIG_COMMANDS_PRE([_AC_SYS_YEAR2038_ENABLE])])

# AC_SYS_LARGEFILE
# ----------------
# By default, many hosts won't let programs access large files;
# one must use special compiler options to get large-file access to work.
# For more details about this brain damage please see:
# http://www.unix.org/version2/whatsnew/lfs20mar.html
# Additionally, on Linux file systems with 64-bit inodes a file that happens
# to have a 64-bit inode number cannot be accessed by 32-bit applications on
# Linux x86/x86_64.  This can occur with file systems such as XFS and NFS.
AC_DEFUN([AC_SYS_LARGEFILE],
[AC_ARG_ENABLE([largefile],
   [AS_HELP_STRING([--disable-largefile],
      [omit support for large files])])dnl
AS_IF([test "$enable_largefile,$enable_year2038" != no,no],
  [_AC_SYS_LARGEFILE_PROBE])])

# AC_SYS_LONG_FILE_NAMES
# ----------------------
# Security: use a temporary directory as the most portable way of
# creating files in /tmp securely.  Removing them leaves a race
# condition, set -C is not portably guaranteed to use O_EXCL, so still
# leaves a race, and not all systems have the 'mktemp' utility.  We
# still test for existence first in case of broken systems where the
# mkdir succeeds even when the directory exists.  Broken systems may
# retain a race, but they probably have other security problems
# anyway; this should be secure on well-behaved systems.  In any case,
# use of 'mktemp' is probably inappropriate here since it would fail in
# attempting to create different file names differing after the 14th
# character on file systems without long file names.
AC_DEFUN([AC_SYS_LONG_FILE_NAMES],
[AC_CACHE_CHECK(for long file names, ac_cv_sys_long_file_names,
[ac_cv_sys_long_file_names=yes
# Test for long file names in all the places we know might matter:
#      .		the current directory, where building will happen
#      $prefix/lib	where we will be installing things
#      $exec_prefix/lib	likewise
#      $TMPDIR		if set, where it might want to write temporary files
#      /tmp		where it might want to write temporary files
#      /var/tmp		likewise
#      /usr/tmp		likewise
for ac_dir in . "$TMPDIR" /tmp /var/tmp /usr/tmp "$prefix/lib" "$exec_prefix/lib"; do
  # Skip $TMPDIR if it is empty or bogus, and skip $exec_prefix/lib
  # in the usual case where exec_prefix is '${prefix}'.
  case $ac_dir in #(
    . | /* | ?:[[\\/]]*) ;; #(
    *) continue;;
  esac
  test -w "$ac_dir/." || continue # It is less confusing to not echo anything here.
  ac_xdir=$ac_dir/cf$$
  (umask 077 && mkdir "$ac_xdir" 2>/dev/null) || continue
  ac_tf1=$ac_xdir/conftest9012345
  ac_tf2=$ac_xdir/conftest9012346
  touch "$ac_tf1" 2>/dev/null && test -f "$ac_tf1" && test ! -f "$ac_tf2" ||
    ac_cv_sys_long_file_names=no
  rm -f -r "$ac_xdir" 2>/dev/null
  test $ac_cv_sys_long_file_names = no && break
done])
if test $ac_cv_sys_long_file_names = yes; then
  AC_DEFINE(HAVE_LONG_FILE_NAMES, 1,
	    [Define to 1 if you support file names longer than 14 characters.])
fi
])


# AC_SYS_RESTARTABLE_SYSCALLS
# ---------------------------
# If the system automatically restarts a system call that is
# interrupted by a signal, define 'HAVE_RESTARTABLE_SYSCALLS'.
AC_DEFUN([AC_SYS_RESTARTABLE_SYSCALLS],
[m4_warn([obsolete],
[$0: AC_SYS_RESTARTABLE_SYSCALLS is useful only when supporting very
old systems that lack 'sigaction' and 'SA_RESTART'.  Don't bother with
this macro unless you need to support very old systems like 4.2BSD and
SVR3.])dnl
AC_REQUIRE([AC_HEADER_SYS_WAIT])dnl
AC_CACHE_CHECK(for restartable system calls, ac_cv_sys_restartable_syscalls,
[AC_RUN_IFELSE([AC_LANG_SOURCE(
[/* Exit 0 (true) if wait returns something other than -1,
   i.e. the pid of the child, which means that wait was restarted
   after getting the signal.  */

AC_INCLUDES_DEFAULT
#include <signal.h>
#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

/* Some platforms explicitly require an extern "C" signal handler
   when using C++. */
#ifdef __cplusplus
extern "C"
#endif
void ucatch (int dummy) { }

int
main (void)
{
  int i = fork (), status;

  if (i == 0)
    {
      sleep (3);
      kill (getppid (), SIGINT);
      sleep (3);
      return 0;
    }

  signal (SIGINT, ucatch);

  status = wait (&i);
  if (status == -1)
    wait (&i);

  return status == -1;
}])],
	       [ac_cv_sys_restartable_syscalls=yes],
	       [ac_cv_sys_restartable_syscalls=no])])
if test $ac_cv_sys_restartable_syscalls = yes; then
  AC_DEFINE(HAVE_RESTARTABLE_SYSCALLS, 1,
	    [Define to 1 if system calls automatically restart after
	     interruption by a signal.])
fi
])# AC_SYS_RESTARTABLE_SYSCALLS


# AC_SYS_POSIX_TERMIOS
# --------------------
AC_DEFUN([AC_SYS_POSIX_TERMIOS],
[AC_CACHE_CHECK([POSIX termios], ac_cv_sys_posix_termios,
[AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
#include <unistd.h>
#include <termios.h>
]],
	     [/* SunOS 4.0.3 has termios.h but not the library calls.  */
   tcgetattr(0, 0);])],
	     ac_cv_sys_posix_termios=yes,
	     ac_cv_sys_posix_termios=no)])
])# AC_SYS_POSIX_TERMIOS




## ------------------------------------ ##
## Checks for not-quite-Unix variants.  ##
## ------------------------------------ ##


# AC_GNU_SOURCE
# -------------
AU_DEFUN([AC_GNU_SOURCE], [AC_USE_SYSTEM_EXTENSIONS])


# AC_CYGWIN
# ---------
# Check for Cygwin.  This is a way to set the right value for
# EXEEXT.
AU_DEFUN([AC_CYGWIN],
[AC_CANONICAL_HOST
case $host_os in
  *cygwin* | *msys* ) CYGWIN=yes;;
	 * ) CYGWIN=no;;
esac
], [$0 is obsolete: use AC_CANONICAL_HOST and check if $host_os
matches *cygwin* or *msys*])# AC_CYGWIN


# AC_EMXOS2
# ---------
# Check for EMX on OS/2.  This is another way to set the right value
# for EXEEXT.
AU_DEFUN([AC_EMXOS2],
[AC_CANONICAL_HOST
case $host_os in
  *emx* ) EMXOS2=yes;;
      * ) EMXOS2=no;;
esac
], [$0 is obsolete: use AC_CANONICAL_HOST and check if $host_os
matches *emx*])# AC_EMXOS2


# AC_MINGW32
# ----------
# Check for mingw32.  This is another way to set the right value for
# EXEEXT.
AU_DEFUN([AC_MINGW32],
[AC_CANONICAL_HOST
case $host_os in
  *mingw32* ) MINGW32=yes;;
	  * ) MINGW32=no;;
esac
], [$0 is obsolete: use AC_CANONICAL_HOST and check if $host_os
matches *mingw32*])# AC_MINGW32


# AC_USE_SYSTEM_EXTENSIONS
# ------------------------
# Enable extensions on systems that normally disable them,
# typically due to standards-conformance issues.
# We unconditionally define as many of the known feature-enabling
# as possible, reserving conditional behavior for macros that are
# known to cause problems on some platforms (such as __EXTENSIONS__).
AC_DEFUN_ONCE([AC_USE_SYSTEM_EXTENSIONS],
[AC_BEFORE([$0], [AC_PREPROC_IFELSE])dnl
AC_BEFORE([$0], [AC_COMPILE_IFELSE])dnl
AC_BEFORE([$0], [AC_LINK_IFELSE])dnl
AC_BEFORE([$0], [AC_RUN_IFELSE])dnl
AC_BEFORE([$0], [AC_CHECK_INCLUDES_DEFAULT])dnl
dnl #undef in AH_VERBATIM gets replaced with #define by AC_DEFINE.
dnl Use a different key than __EXTENSIONS__, as that name broke existing
dnl configure.ac when using autoheader 2.62.
dnl The macros below are in alphabetical order ignoring leading _ or __
dnl prefixes.
AH_VERBATIM([USE_SYSTEM_EXTENSIONS],
[/* Enable extensions on AIX, Interix, z/OS.  */
#ifndef _ALL_SOURCE
# undef _ALL_SOURCE
#endif
/* Enable general extensions on macOS.  */
#ifndef _DARWIN_C_SOURCE
# undef _DARWIN_C_SOURCE
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# undef __EXTENSIONS__
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# undef _GNU_SOURCE
#endif
/* Enable X/Open compliant socket functions that do not require linking
   with -lxnet on HP-UX 11.11.  */
#ifndef _HPUX_ALT_XOPEN_SOCKET_API
# undef _HPUX_ALT_XOPEN_SOCKET_API
#endif
/* Identify the host operating system as Minix.
   This macro does not affect the system headers' behavior.
   A future release of Autoconf may stop defining this macro.  */
#ifndef _MINIX
# undef _MINIX
#endif
/* Enable general extensions on NetBSD.
   Enable NetBSD compatibility extensions on Minix.  */
#ifndef _NETBSD_SOURCE
# undef _NETBSD_SOURCE
#endif
/* Enable OpenBSD compatibility extensions on NetBSD.
   Oddly enough, this does nothing on OpenBSD.  */
#ifndef _OPENBSD_SOURCE
# undef _OPENBSD_SOURCE
#endif
/* Define to 1 if needed for POSIX-compatible behavior.  */
#ifndef _POSIX_SOURCE
# undef _POSIX_SOURCE
#endif
/* Define to 2 if needed for POSIX-compatible behavior.  */
#ifndef _POSIX_1_SOURCE
# undef _POSIX_1_SOURCE
#endif
/* Enable POSIX-compatible threading on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# undef _POSIX_PTHREAD_SEMANTICS
#endif
/* Enable extensions specified by ISO/IEC TS 18661-5:2014.  */
#ifndef __STDC_WANT_IEC_60559_ATTRIBS_EXT__
# undef __STDC_WANT_IEC_60559_ATTRIBS_EXT__
#endif
/* Enable extensions specified by ISO/IEC TS 18661-1:2014.  */
#ifndef __STDC_WANT_IEC_60559_BFP_EXT__
# undef __STDC_WANT_IEC_60559_BFP_EXT__
#endif
/* Enable extensions specified by ISO/IEC TS 18661-2:2015.  */
#ifndef __STDC_WANT_IEC_60559_DFP_EXT__
# undef __STDC_WANT_IEC_60559_DFP_EXT__
#endif
/* Enable extensions specified by C23 Annex F.  */
#ifndef __STDC_WANT_IEC_60559_EXT__
# undef __STDC_WANT_IEC_60559_EXT__
#endif
/* Enable extensions specified by ISO/IEC TS 18661-4:2015.  */
#ifndef __STDC_WANT_IEC_60559_FUNCS_EXT__
# undef __STDC_WANT_IEC_60559_FUNCS_EXT__
#endif
/* Enable extensions specified by C23 Annex H and ISO/IEC TS 18661-3:2015.  */
#ifndef __STDC_WANT_IEC_60559_TYPES_EXT__
# undef __STDC_WANT_IEC_60559_TYPES_EXT__
#endif
/* Enable extensions specified by ISO/IEC TR 24731-2:2010.  */
#ifndef __STDC_WANT_LIB_EXT2__
# undef __STDC_WANT_LIB_EXT2__
#endif
/* Enable extensions specified by ISO/IEC 24747:2009.  */
#ifndef __STDC_WANT_MATH_SPEC_FUNCS__
# undef __STDC_WANT_MATH_SPEC_FUNCS__
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# undef _TANDEM_SOURCE
#endif
/* Enable X/Open extensions.  Define to 500 only if necessary
   to make mbstate_t available.  */
#ifndef _XOPEN_SOURCE
# undef _XOPEN_SOURCE
#endif
])dnl

  AC_REQUIRE([AC_CHECK_INCLUDES_DEFAULT])dnl
  _AC_CHECK_HEADER_ONCE([wchar.h])
  _AC_CHECK_HEADER_ONCE([minix/config.h])

dnl Defining __EXTENSIONS__ may break the system headers on some systems.
dnl (FIXME: Which ones?)
  AC_CACHE_CHECK([whether it is safe to define __EXTENSIONS__],
    [ac_cv_safe_to_define___extensions__],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM([[
#         define __EXTENSIONS__ 1
          ]AC_INCLUDES_DEFAULT])],
       [ac_cv_safe_to_define___extensions__=yes],
       [ac_cv_safe_to_define___extensions__=no])])

dnl HP-UX 11.11 defines mbstate_t only if _XOPEN_SOURCE is defined to
dnl 500, regardless of whether compiling with -Ae or -D_HPUX_SOURCE=1.
dnl But defining _XOPEN_SOURCE may turn *off* extensions on platforms
dnl not covered by turn-on-extensions macros (notably Dragonfly, Free,
dnl and OpenBSD, which don't have any equivalent of _NETBSD_SOURCE) so
dnl it should only be defined when necessary.
  AC_CACHE_CHECK([whether _XOPEN_SOURCE should be defined],
    [ac_cv_should_define__xopen_source],
    [ac_cv_should_define__xopen_source=no
    AS_IF([test $ac_cv_header_wchar_h = yes],
      [AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
          #include <wchar.h>
          mbstate_t x;]])],
        [],
        [AC_COMPILE_IFELSE(
          [AC_LANG_PROGRAM([[
            #define _XOPEN_SOURCE 500
            #include <wchar.h>
            mbstate_t x;]])],
          [ac_cv_should_define__xopen_source=yes])])])])

  AC_DEFINE([_ALL_SOURCE])
  AC_DEFINE([_DARWIN_C_SOURCE])
  AC_DEFINE([_GNU_SOURCE])
  AC_DEFINE([_HPUX_ALT_XOPEN_SOCKET_API])
  AC_DEFINE([_NETBSD_SOURCE])
  AC_DEFINE([_OPENBSD_SOURCE])
  AC_DEFINE([_POSIX_PTHREAD_SEMANTICS])
  AC_DEFINE([__STDC_WANT_IEC_60559_ATTRIBS_EXT__])
  AC_DEFINE([__STDC_WANT_IEC_60559_BFP_EXT__])
  AC_DEFINE([__STDC_WANT_IEC_60559_DFP_EXT__])
  AC_DEFINE([__STDC_WANT_IEC_60559_EXT__])
  AC_DEFINE([__STDC_WANT_IEC_60559_FUNCS_EXT__])
  AC_DEFINE([__STDC_WANT_IEC_60559_TYPES_EXT__])
  AC_DEFINE([__STDC_WANT_LIB_EXT2__])
  AC_DEFINE([__STDC_WANT_MATH_SPEC_FUNCS__])
  AC_DEFINE([_TANDEM_SOURCE])
  AS_IF([test $ac_cv_header_minix_config_h = yes],
    [MINIX=yes
    AC_DEFINE([_MINIX])
    AC_DEFINE([_POSIX_SOURCE])
    AC_DEFINE([_POSIX_1_SOURCE], [2])],
    [MINIX=])
  AS_IF([test $ac_cv_safe_to_define___extensions__ = yes],
    [AC_DEFINE([__EXTENSIONS__])])
  AS_IF([test $ac_cv_should_define__xopen_source = yes],
    [AC_DEFINE([_XOPEN_SOURCE], [500])])
])# AC_USE_SYSTEM_EXTENSIONS



## -------------------------- ##
## Checks for UNIX variants.  ##
## -------------------------- ##

# These macros are all obsolete, from the early days of Autoconf,
# before the invention of AC_CANONICAL_SYSTEM.  Autoupdate will
# replace each with inline code for a more modern feature check.

# AC_AIX
# ------
AU_DEFUN([AC_AIX], [AC_USE_SYSTEM_EXTENSIONS])


# AC_MINIX
# --------
AU_DEFUN([AC_MINIX], [AC_USE_SYSTEM_EXTENSIONS])


# AC_ISC_POSIX
# ------------
AU_DEFUN([AC_ISC_POSIX], [AC_SEARCH_LIBS([strerror], [cposix])])


# AC_XENIX_DIR
# ------------
AU_DEFUN([AC_XENIX_DIR],
[AC_HEADER_DIRENT
# Autoupdate added the next two lines to ensure that your configure
# script's behavior did not change.  They are safe to remove unless
# you have code that depends on the XENIX shell variable.
AC_CANONICAL_HOST
AS_CASE([$host_os], [xenix*], [XENIX=yes], [XENIX=no])
# End of code added by autoupdate
],
[Check for code depending on the XENIX shell variable.])


# AC_DYNIX_SEQ
# ------------
AU_DEFUN([AC_DYNIX_SEQ], [AC_FUNC_GETMNTENT])


# AC_IRIX_SUN
# -----------
AU_DEFUN([AC_IRIX_SUN],
[AC_FUNC_GETMNTENT
AC_CHECK_LIB([sun], [getpwnam])])


# AC_SCO_INTL
# -----------
AU_DEFUN([AC_SCO_INTL], [AC_FUNC_STRFTIME])
