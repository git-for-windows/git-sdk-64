/*
   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of interpreter of DINO.

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.

*/

#ifndef __D_CONFIG_H__
#define __D_CONFIG_H__

#ifdef HAVE_CONFIG_H
#include "d_config.h"
#else /* In this case we are oriented to ANSI C and dfcn.h */
#ifndef HAVE_FLOAT_H
#define HAVE_FLOAT_H
#endif
#ifndef HAVE_LIMITS_H
#define HAVE_LIMITS_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <stdio.h>

#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
#ifndef UCHAR_MAX
#define UCHAR_MAX 255
#endif
#ifndef UINT_MAX
#define UINT_MAX (INT_MAX * 2U + 1)
#endif
#ifndef INT_MAX
#define INT_MAX 2147483647
#endif  
#ifndef INT_MIN
#define INT_MIN (-INT_MAX-1)
#endif
#endif

#include <math.h>

#ifdef HAVE_FLOAT_H
#include <float.h>
#else
#define FLT_MAX  3.40282347e+38F         /* IEEE float */
#define DBL_MAX  1.7976931348623157e+308 /* IEEE double */
#define FLT_MANT_DIG  24
#define FLT_MAX_EXP  128
#define DBL_MANT_DIG  53
#define DBL_MAX_EXP 1024
#endif

#if INT_MAX == 2147483647
typedef int int_t;
typedef unsigned int unsigned_int_t;
#define MAX_INT   INT_MAX	/* int */
#else
#error there is no 32 bits int
#endif

#if DBL_MANT_DIG == 53 && DBL_MAX_EXP == 1024
typedef double floating_t;

#ifdef HUGE_VAL
#define FLOATING_HUGE_VAL HUGE_VAL
#endif

#define MAX_FLOAT DBL_MAX	/* double */
#elif FLT_MANT_DIG == 53 && FLT_MAX_EXP == 1024
typedef float floating_t;

#ifdef HUGE_VALF
#define FLOATING_HUGE_VAL HUGE_VALF
#endif

#define MAX_FLOAT FLT_MAX	/* float */
#else
#error there is no IEEE double
#endif

#ifdef WORDS_BIGENDIAN
static const char __nan__[8] = { 0x7f, 0xf8, 0, 0, 0, 0, 0, 0 };
#define IS_FLOATING_NAN(var)\
  ((((int_t *) &var) [0] & ((int_t *) __nan__) [0]) == ((int_t *) __nan__) [0])
#else
static const char __nan__[8] = { 0, 0, 0, 0, 0, 0, 0xf8, 0x7f };
#define IS_FLOATING_NAN(var)\
  ((((int_t *) &var) [1] & ((int_t *) __nan__) [1]) == ((int_t *) __nan__) [1])
#endif

#ifndef FLOATING_HUGE_VAL
#ifdef WORDS_BIGENDIAN
static const char __infinity[8] = { 0x7f, 0xf0, 0, 0, 0, 0, 0, 0 };
#else
static const char __infinity[8] = { 0, 0, 0, 0, 0, 0, 0xf0, 0x7f };
#endif
#define FLOATING_NAN (*(floating_t *) __infinity)
#endif

typedef unsigned char char_t;

#define MAX_CHAR  UCHAR_MAX /* unsigned char */

typedef char bool_t;

#endif /*#ifndef __D_CONFIG_H__ */
