# Check to make sure that the build environment is sane.    -*- Autoconf -*-

# Copyright (C) 1996-2024 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# _AM_SLEEP_FRACTIONAL_SECONDS
# ----------------------------
AC_DEFUN([_AM_SLEEP_FRACTIONAL_SECONDS], [dnl
AC_CACHE_CHECK([whether sleep supports fractional seconds],
               am_cv_sleep_fractional_seconds, [dnl
AS_IF([sleep 0.001 2>/dev/null], [am_cv_sleep_fractional_seconds=yes],
                                 [am_cv_sleep_fractional_seconds=no])
])])

# _AM_FILESYSTEM_TIMESTAMP_RESOLUTION
# -----------------------------------
# Determine the filesystem's resolution for file modification
# timestamps.  The coarsest we know of is FAT, with a resolution
# of only two seconds, even with the most recent "exFAT" extensions.
# The finest (e.g. ext4 with large inodes, XFS, ZFS) is one
# nanosecond, matching clock_gettime.  However, it is probably not
# possible to delay execution of a shell script for less than one
# millisecond, due to process creation overhead and scheduling
# granularity, so we don't check for anything finer than that. (See below.)
AC_DEFUN([_AM_FILESYSTEM_TIMESTAMP_RESOLUTION], [dnl
AC_REQUIRE([_AM_SLEEP_FRACTIONAL_SECONDS])
AC_CACHE_CHECK([filesystem timestamp resolution],
               am_cv_filesystem_timestamp_resolution, [dnl
# Default to the worst case.
am_cv_filesystem_timestamp_resolution=2

# Only try to go finer than 1 sec if sleep can do it.
# Don't try 1 sec, because if 0.01 sec and 0.1 sec don't work,
# - 1 sec is not much of a win compared to 2 sec, and
# - it takes 2 seconds to perform the test whether 1 sec works.
# 
# Instead, just use the default 2s on platforms that have 1s resolution,
# accept the extra 1s delay when using $sleep in the Automake tests, in
# exchange for not incurring the 2s delay for running the test for all
# packages.
#
am_try_resolutions=
if test "$am_cv_sleep_fractional_seconds" = yes; then
  # Even a millisecond often causes a bunch of false positives,
  # so just try a hundredth of a second. The time saved between .001 and
  # .01 is not terribly consequential.
  am_try_resolutions="0.01 0.1 $am_try_resolutions"
fi

# In order to catch current-generation FAT out, we must *modify* files
# that already exist; the *creation* timestamp is finer.  Use names
# that make ls -t sort them differently when they have equal
# timestamps than when they have distinct timestamps, keeping
# in mind that ls -t prints the *newest* file first.
rm -f conftest.ts?
: > conftest.ts1
: > conftest.ts2
: > conftest.ts3

# Make sure ls -t actually works.  Do 'set' in a subshell so we don't
# clobber the current shell's arguments. (Outer-level square brackets
# are removed by m4; they're present so that m4 does not expand
# <dollar><star>; be careful, easy to get confused.)
if (
     set X `[ls -t conftest.ts[12]]` &&
     {
       test "$[]*" != "X conftest.ts1 conftest.ts2" ||
       test "$[]*" != "X conftest.ts2 conftest.ts1";
     }
); then :; else
  # If neither matched, then we have a broken ls.  This can happen
  # if, for instance, CONFIG_SHELL is bash and it inherits a
  # broken ls alias from the environment.  This has actually
  # happened.  Such a system could not be considered "sane".
  _AS_ECHO_UNQUOTED(
    ["Bad output from ls -t: \"`[ls -t conftest.ts[12]]`\""],
    [AS_MESSAGE_LOG_FD])
  AC_MSG_FAILURE([ls -t produces unexpected output.
Make sure there is not a broken ls alias in your environment.])
fi

for am_try_res in $am_try_resolutions; do
  # Any one fine-grained sleep might happen to cross the boundary
  # between two values of a coarser actual resolution, but if we do
  # two fine-grained sleeps in a row, at least one of them will fall
  # entirely within a coarse interval.
  echo alpha > conftest.ts1
  sleep $am_try_res
  echo beta > conftest.ts2
  sleep $am_try_res
  echo gamma > conftest.ts3

  # We assume that 'ls -t' will make use of high-resolution
  # timestamps if the operating system supports them at all.
  if (set X `ls -t conftest.ts?` &&
      test "$[]2" = conftest.ts3 &&
      test "$[]3" = conftest.ts2 &&
      test "$[]4" = conftest.ts1); then
    #
    # Ok, ls -t worked. If we're at a resolution of 1 second, we're done,
    # because we don't need to test make.
    make_ok=true
    if test $am_try_res != 1; then
      # But if we've succeeded so far with a subsecond resolution, we
      # have one more thing to check: make. It can happen that
      # everything else supports the subsecond mtimes, but make doesn't;
      # notably on macOS, which ships make 3.81 from 2006 (the last one
      # released under GPLv2). https://bugs.gnu.org/68808
      # 
      # We test $MAKE if it is defined in the environment, else "make".
      # It might get overridden later, but our hope is that in practice
      # it does not matter: it is the system "make" which is (by far)
      # the most likely to be broken, whereas if the user overrides it,
      # probably they did so with a better, or at least not worse, make.
      # https://lists.gnu.org/archive/html/automake/2024-06/msg00051.html
      #
      # Create a Makefile (real tab character here):
      rm -f conftest.mk
      echo 'conftest.ts1: conftest.ts2' >conftest.mk
      echo '	touch conftest.ts2' >>conftest.mk
      #
      # Now, running
      #   touch conftest.ts1; touch conftest.ts2; make
      # should touch ts1 because ts2 is newer. This could happen by luck,
      # but most often, it will fail if make's support is insufficient. So
      # test for several consecutive successes.
      #
      # (We reuse conftest.ts[12] because we still want to modify existing
      # files, not create new ones, per above.)
      n=0
      make=${MAKE-make}
      until test $n -eq 3; do
        echo one > conftest.ts1
        sleep $am_try_res
        echo two > conftest.ts2 # ts2 should now be newer than ts1
        if $make -f conftest.mk | grep 'up to date' >/dev/null; then
          make_ok=false
          break # out of $n loop
        fi
        n=`expr $n + 1`
      done
    fi
    #
    if $make_ok; then
      # Everything we know to check worked out, so call this resolution good.
      am_cv_filesystem_timestamp_resolution=$am_try_res
      break # out of $am_try_res loop
    fi
    # Otherwise, we'll go on to check the next resolution.
  fi
done
rm -f conftest.ts?
# (end _am_filesystem_timestamp_resolution)
])])

# AM_SANITY_CHECK
# ---------------
AC_DEFUN([AM_SANITY_CHECK],
[AC_REQUIRE([_AM_FILESYSTEM_TIMESTAMP_RESOLUTION])
# This check should not be cached, as it may vary across builds of
# different projects.
AC_MSG_CHECKING([whether build environment is sane])
# Reject unsafe characters in $srcdir or the absolute working directory
# name.  Accept space and tab only in the latter.
am_lf='
'
case `pwd` in
  *[[\\\"\#\$\&\'\`$am_lf]]*)
    AC_MSG_ERROR([unsafe absolute working directory name]);;
esac
case $srcdir in
  *[[\\\"\#\$\&\'\`$am_lf\ \	]]*)
    AC_MSG_ERROR([unsafe srcdir value: '$srcdir']);;
esac

# Do 'set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
am_build_env_is_sane=no
am_has_slept=no
rm -f conftest.file
for am_try in 1 2; do
  echo "timestamp, slept: $am_has_slept" > conftest.file
  if (
    set X `ls -Lt "$srcdir/configure" conftest.file 2> /dev/null`
    if test "$[]*" = "X"; then
      # -L didn't work.
      set X `ls -t "$srcdir/configure" conftest.file`
    fi
    test "$[]2" = conftest.file
  ); then
    am_build_env_is_sane=yes
    break
  fi
  # Just in case.
  sleep "$am_cv_filesystem_timestamp_resolution"
  am_has_slept=yes
done

AC_MSG_RESULT([$am_build_env_is_sane])
if test "$am_build_env_is_sane" = no; then
  AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi

# If we didn't sleep, we still need to ensure time stamps of config.status and
# generated files are strictly newer.
am_sleep_pid=
AS_IF([test -e conftest.file || grep 'slept: no' conftest.file >/dev/null 2>&1],, [dnl
  ( sleep "$am_cv_filesystem_timestamp_resolution" ) &
  am_sleep_pid=$!
])
AC_CONFIG_COMMANDS_PRE(
  [AC_MSG_CHECKING([that generated files are newer than configure])
   if test -n "$am_sleep_pid"; then
     # Hide warnings about reused PIDs.
     wait $am_sleep_pid 2>/dev/null
   fi
   AC_MSG_RESULT([done])])
rm -f conftest.file
])
