##                                                          -*- Autoconf -*-
# Copyright (C) 2022-2024 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# _AM_PROG_RM_F
# ---------------
# Check whether 'rm -f' without any arguments works.
# https://bugs.gnu.org/10828
AC_DEFUN([_AM_PROG_RM_F],
[am__rm_f_notfound=
AS_IF([(rm -f && rm -fr && rm -rf) 2>/dev/null], [], [am__rm_f_notfound='""'])
AC_SUBST(am__rm_f_notfound)
])
