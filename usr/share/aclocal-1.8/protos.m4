## ------------------------------- ##                    -*- Autoconf -*-
## Check for function prototypes.  ##
## From Franc,ois Pinard           ##
## ------------------------------- ##

# Copyright (C) 1996, 1997, 1998, 2000, 2001, 2002, 2003
# Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 3

AC_DEFUN([AM_C_PROTOTYPES],
[AC_REQUIRE([AC_C_PROTOTYPES])
if test "$ac_cv_prog_cc_stdc" != no; then
  U= ANSI2KNR=
else
  U=_ ANSI2KNR=./ansi2knr
fi
# Ensure some checks needed by ansi2knr itself.
AC_REQUIRE([AC_HEADER_STDC])
AC_CHECK_HEADERS(string.h)
AC_SUBST(U)dnl
AC_SUBST(ANSI2KNR)dnl
])

AU_DEFUN([fp_C_PROTOTYPES], [AM_C_PROTOTYPES])
