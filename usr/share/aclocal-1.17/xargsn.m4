##                                                          -*- Autoconf -*-
# Copyright (C) 2022-2024 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# _AM_PROG_XARGS_N
# ----------------
# Check whether 'xargs -n' works.  It should work everywhere, so the fallback
# is not optimized at all as we never expect to use it.
AC_DEFUN([_AM_PROG_XARGS_N],
[AC_CACHE_CHECK([xargs -n works], am_cv_xargs_n_works, [dnl
AS_IF([test "`echo 1 2 3 | xargs -n2 echo`" = "1 2
3"], [am_cv_xargs_n_works=yes], [am_cv_xargs_n_works=no])])
AS_IF([test "$am_cv_xargs_n_works" = yes], [am__xargs_n='xargs -n'], [dnl
  am__xargs_n='am__xargs_n () { shift; sed "s/ /\\n/g" | while read am__xargs_n_arg; do "$@" "$am__xargs_n_arg"; done; }'
])dnl
AC_SUBST(am__xargs_n)
])
