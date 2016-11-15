/* FILE NAME:   IEEE.h

   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This is part of package IEEE; you can redistribute it and/or modify
   it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Include file of package for work with IEEE floating
                point numbers

   DESCRIPTION: This header file contains ANSI C prototype definitions
                of the package functions and definitions of macros and
                types of the package and C++ classes for work with IEEE
                floating point numbers.

   SPECIAL CONSIDERATION: Defining macro IEEE_QUAD switch on
       supporting IEEE quad numbers (128 bits).  This support results
       in more memory (about 100Kb).  C++: Defining macro
       `NO_TEMPLATE' (e.g. by option `-D' in C++ compiler command
       line) during the file compilation disables to use template
       functions for transformations to/from `sint' and `unsint'.

*/

#ifndef __IEEE_float__
#define __IEEE_float__

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#endif

/* The following macro value is size in bytes of an IEEE single precision
   floating point number.  The size of type `IEEE_float_t' must be equal
   the macro value. */

#define IEEE_FLOAT_SIZE 4

/* The following macro value is size in bytes of an IEEE double precision
   floating point number.  The size of type `IEEE_double_t' must be equal
   the macro value. */

#define IEEE_DOUBLE_SIZE 8

/* The following macro value is size in bytes of an IEEE quad precision
   floating point number.  The size of type `IEEE_quad_t' must be equal
   the macro value. */

#ifdef IEEE_QUAD
#define IEEE_QUAD_SIZE 16
#endif

/* The values of the following macros represents IEEE floating point
   exceptions.  The macros are used to form masks (with the aid of C
   operation `|') for functions `IEEE_set_sticky_status_bits'
   (`IEEE::set_sticky_status_bits') and `IEEE_set_trap_mask'
   (`IEEE::set_trap_mask') and to separate bits in mask returned by
   functions `IEEE_get_sticky_status_bits'
   (`IEEE::get_sticky_status_bits'), `IEEE_get_status_bits'
   (`IEEE::get_status_bits'), and `IEEE_get_trap_mask'
   (`IEEE::get_trap_mask'). */

/* Invalid operation.  This exception indicating that the source
   operand(s) is invalid for the operation to be performed.  See also
   commentaries for input exceptions of functions which simulate IEEE
   floating point operations. */

#define IEEE_INV  01

/* Reserved operand.  This exception indicating that an operation
   producing a numeric result has a reserved operand (NaN) as either a
   source operand or result.  See also commentaries for input
   exceptions of functions which simulate IEEE floating point
   operations. */

#define IEEE_RO   02

/* Overflow.  This exception indicating that the rounded result of an
   operation is too large to be expressed in the destination format.
   See also commentaries for output exceptions. */

#define IEEE_OFL  04

/* Underflow.  This exception indicating that the rounded result of an
   operation is too small to be expressed as a normalized number and
   the rounded result is imprecise or if underflow exception trap is
   disabled the rounded result is too small to be expressed as a
   normalized number.  See also commentaries for output exceptions. */

#define IEEE_UFL  010

/* Imprecise result.  This exception indicating that the rounded
   result of an operation is not equal to the infinitely-precise
   result or if overflow exception trap is disabled an overflow
   exception occurs.  See also commentaries for output exceptions. */

#define IEEE_IMP  020

/* Divide by zero.  This exception indicating that a finite non-zero
   floating point number is divided by zero.  See also commentaries
   for input exceptions of functions `IEEE_divide_single'
   (`IEEE_float:: operator/'), `IEEE_divide_double' (`IEEE_double::
   operator/') and IEEE_divide_quad' (`IEEE_quad:: operator/').. */

#define IEEE_DZ   040


/* The value of the following macros defines rounding control.
   Initial rounding mode is round to nearest. */

/* The following macro value represents round to nearest
   represenatable number, i.e. the result produced is the
   representable value nearest to the infinitely-precise result.
   There are special cases when infinitely precise result falls
   exactly halfway between two representable values.  In this cases
   the result will be whichever of those two representable values has
   a fractional part whos least significant bit is zero. */

#define IEEE_RN  0

/* The following macro value represents round toward minus infinity,
   i.e. the result produced is the representable value closest to but
   no greater than the infinitely precise result. */

#define IEEE_RM  1

/* The following macro value represents round toward plus infinity,
   i.e. the result produced is the representable value closest to but
   no less than the infinitely precise result. */

#define IEEE_RP  2

/* The following macro value represents round toward zero, i.e. the
   result produced is the representable value closest to but no
   greater in magnitude than the infinitely precise result. */

#define IEEE_RZ  3

/* The following macro values are maximal length of array for the
   corresponding string representation of IEEE numbers. */

#define MAX_SINGLE_2_STRING_LENGTH   35
#define MAX_DOUBLE_2_STRING_LENGTH   65
#define MAX_QUAD_2_STRING_LENGTH     126

#define MAX_SINGLE_4_STRING_LENGTH   23
#define MAX_DOUBLE_4_STRING_LENGTH   39
#define MAX_QUAD_4_STRING_LENGTH     70

#define MAX_SINGLE_8_STRING_LENGTH   19
#define MAX_DOUBLE_8_STRING_LENGTH   30
#define MAX_QUAD_8_STRING_LENGTH     51

#define MAX_SINGLE_10_STRING_LENGTH  18
#define MAX_DOUBLE_10_STRING_LENGTH  29
#define MAX_QUAD_10_STRING_LENGTH    47

#define MAX_SINGLE_16_STRING_LENGTH  17
#define MAX_DOUBLE_16_STRING_LENGTH  28
#define MAX_QUAD_16_STRING_LENGTH    42

#ifndef __cplusplus

/* The following type represents correspondingly IEEE single precision
   floating point numbers.  The numbers are represented by structure in order
   to correctly use C language operator `=' and argument passing. */

typedef struct {unsigned char float_bytes [IEEE_FLOAT_SIZE];} IEEE_float_t;

/* The following type represents correspondingly IEEE double precision
   floating point numbers.  The numbers are represented by structure
   in order to correctly use C language operator `=' and argument
   passing. */

typedef struct {unsigned char double_bytes [IEEE_DOUBLE_SIZE];} IEEE_double_t;

/* The following type represents correspondingly IEEE quad precision
   floating point numbers.  The numbers are represented by structure
   in order to correctly use C language operator `=' and argument
   passing. */

#ifdef IEEE_QUAD
typedef struct {unsigned char quad_bytes [IEEE_QUAD_SIZE];} IEEE_quad_t;
#endif

extern void IEEE_reset (void);

extern int IEEE_set_trap_mask (int mask);
extern int IEEE_get_trap_mask (void);

extern int IEEE_set_sticky_status_bits (int mask);
extern int IEEE_get_sticky_status_bits (void);

extern int IEEE_get_status_bits (void);

extern int IEEE_set_round (int round_mode);
extern int IEEE_get_round (void);

extern void default_floating_point_exception_trap (void);
extern void (*IEEE_set_floating_point_exception_trap
               (void (*function) (void))) (void);

extern IEEE_float_t IEEE_positive_zero (void);
extern IEEE_float_t IEEE_negative_zero (void);
extern IEEE_float_t IEEE_NaN (void);
extern IEEE_float_t IEEE_trapping_NaN (void);
extern IEEE_float_t IEEE_positive_infinity (void);
extern IEEE_float_t IEEE_negative_infinity (void);
extern IEEE_float_t IEEE_positive_maximum (void);
extern IEEE_float_t IEEE_negative_maximum (void);
extern IEEE_float_t IEEE_positive_minimum (void);
extern IEEE_float_t IEEE_negative_minimum (void);

extern IEEE_double_t IEEE_double_positive_zero (void);
extern IEEE_double_t IEEE_double_negative_zero (void);
extern IEEE_double_t IEEE_double_NaN (void);
extern IEEE_double_t IEEE_double_trapping_NaN (void);
extern IEEE_double_t IEEE_double_positive_infinity (void);
extern IEEE_double_t IEEE_double_negative_infinity (void);
extern IEEE_double_t IEEE_double_positive_maximum (void);
extern IEEE_double_t IEEE_double_negative_maximum (void);
extern IEEE_double_t IEEE_double_positive_minimum (void);
extern IEEE_double_t IEEE_double_negative_minimum (void);

#ifdef IEEE_QUAD
extern IEEE_quad_t IEEE_quad_positive_zero (void);
extern IEEE_quad_t IEEE_quad_negative_zero (void);
extern IEEE_quad_t IEEE_quad_NaN (void);
extern IEEE_quad_t IEEE_quad_trapping_NaN (void);
extern IEEE_quad_t IEEE_quad_positive_infinity (void);
extern IEEE_quad_t IEEE_quad_negative_infinity (void);
extern IEEE_quad_t IEEE_quad_positive_maximum (void);
extern IEEE_quad_t IEEE_quad_negative_maximum (void);
extern IEEE_quad_t IEEE_quad_positive_minimum (void);
extern IEEE_quad_t IEEE_quad_negative_minimum (void);
#endif

extern int IEEE_is_positive_zero (IEEE_float_t single_float);
extern int IEEE_is_negative_zero (IEEE_float_t single_float);
extern int IEEE_is_NaN (IEEE_float_t single_float);
extern int IEEE_is_trapping_NaN (IEEE_float_t single_float);
extern int IEEE_is_positive_infinity (IEEE_float_t single_float);
extern int IEEE_is_negative_infinity (IEEE_float_t single_float);

extern int IEEE_is_double_positive_zero (IEEE_double_t double_float);
extern int IEEE_is_double_negative_zero (IEEE_double_t double_float);
extern int IEEE_is_double_NaN (IEEE_double_t double_float);
extern int IEEE_is_double_trapping_NaN (IEEE_double_t double_float);
extern int IEEE_is_double_positive_infinity (IEEE_double_t double_float);
extern int IEEE_is_double_negative_infinity (IEEE_double_t double_float);

#ifdef IEEE_QUAD
extern int IEEE_is_quad_positive_zero (IEEE_quad_t quad_float);
extern int IEEE_is_quad_negative_zero (IEEE_quad_t quad_float);
extern int IEEE_is_quad_NaN (IEEE_quad_t quad_float);
extern int IEEE_is_quad_trapping_NaN (IEEE_quad_t quad_float);
extern int IEEE_is_quad_positive_infinity (IEEE_quad_t quad_float);
extern int IEEE_is_quad_negative_infinity (IEEE_quad_t quad_float);
#endif

extern int IEEE_is_normalized (IEEE_float_t single_float);
extern int IEEE_is_denormalized (IEEE_float_t single_float);

extern int IEEE_is_double_normalized (IEEE_double_t double_float);
extern int IEEE_is_double_denormalized (IEEE_double_t double_float);

#ifdef IEEE_QUAD
extern int IEEE_is_quad_normalized (IEEE_quad_t quad_float);
extern int IEEE_is_quad_denormalized (IEEE_quad_t quad_float);
#endif

extern IEEE_float_t IEEE_add_single (IEEE_float_t single1,
                                     IEEE_float_t single2);
extern IEEE_double_t IEEE_add_double (IEEE_double_t double1,
                                      IEEE_double_t double2);
#ifdef IEEE_QUAD
extern IEEE_quad_t IEEE_add_quad (IEEE_quad_t quad1, IEEE_quad_t quad2);
#endif

extern IEEE_float_t IEEE_subtract_single (IEEE_float_t single1,
                                          IEEE_float_t single2);

extern IEEE_double_t IEEE_subtract_double (IEEE_double_t double1,
                                           IEEE_double_t double2);
#ifdef IEEE_QUAD
extern IEEE_quad_t IEEE_subtract_quad (IEEE_quad_t quad1, IEEE_quad_t quad2);
#endif

extern IEEE_float_t IEEE_multiply_single (IEEE_float_t single1,
                                          IEEE_float_t single2);

extern IEEE_double_t IEEE_multiply_double (IEEE_double_t double1,
                                           IEEE_double_t double2);
#ifdef IEEE_QUAD
extern IEEE_quad_t IEEE_multiply_quad (IEEE_quad_t quad1, IEEE_quad_t quad2);
#endif

extern IEEE_float_t IEEE_divide_single (IEEE_float_t single1,
                                        IEEE_float_t single2);
extern IEEE_double_t IEEE_divide_double (IEEE_double_t double1,
                                         IEEE_double_t double2);
#ifdef IEEE_QUAD
extern IEEE_quad_t IEEE_divide_quad (IEEE_quad_t quad1, IEEE_quad_t quad2);
#endif

extern int IEEE_eq_single (IEEE_float_t single1, IEEE_float_t single2);
extern int IEEE_eq_double (IEEE_double_t double1, IEEE_double_t double2);
#ifdef IEEE_QUAD
extern int IEEE_eq_quad (IEEE_quad_t quad1, IEEE_quad_t quad2);
#endif

extern int IEEE_ne_single (IEEE_float_t single1, IEEE_float_t single2);
extern int IEEE_ne_double (IEEE_double_t double1, IEEE_double_t double2);
#ifdef IEEE_QUAD
extern int IEEE_ne_quad (IEEE_quad_t quad1, IEEE_quad_t quad2);
#endif

extern int IEEE_gt_single (IEEE_float_t single1, IEEE_float_t single2);
extern int IEEE_gt_double (IEEE_double_t double1, IEEE_double_t double2);
#ifdef IEEE_QUAD
extern int IEEE_gt_quad (IEEE_quad_t quad1, IEEE_quad_t quad2);
#endif

extern int IEEE_lt_single (IEEE_float_t single1, IEEE_float_t single2);
extern int IEEE_lt_double (IEEE_double_t double1, IEEE_double_t double2);
#ifdef IEEE_QUAD
extern int IEEE_lt_quad (IEEE_quad_t quad1, IEEE_quad_t quad2);
#endif

extern int IEEE_ge_single (IEEE_float_t single1, IEEE_float_t single2);
extern int IEEE_ge_double (IEEE_double_t double1, IEEE_double_t double2);
#ifdef IEEE_QUAD
extern int IEEE_ge_quad (IEEE_quad_t quad1, IEEE_quad_t quad2);
#endif

extern int IEEE_le_single (IEEE_float_t single1, IEEE_float_t single2);
extern int IEEE_le_double (IEEE_double_t double1, IEEE_double_t double2);
#ifdef IEEE_QUAD
extern int IEEE_le_quad (IEEE_quad_t quad1, IEEE_quad_t quad2);
#endif

extern IEEE_double_t IEEE_single_to_double (IEEE_float_t single_float);
#ifdef IEEE_QUAD
extern IEEE_quad_t IEEE_single_to_quad (IEEE_float_t single_float);
#endif

extern IEEE_float_t IEEE_double_to_single (IEEE_double_t double_float);

#ifdef IEEE_QUAD
extern IEEE_float_t IEEE_quad_to_single (IEEE_quad_t quad_float);
extern IEEE_quad_t IEEE_double_to_quad (IEEE_double_t double_float);
extern IEEE_double_t IEEE_quad_to_double (IEEE_quad_t quad_float);
#endif

extern IEEE_float_t IEEE_single_from_integer (int size, const void *integer);
extern IEEE_float_t IEEE_single_from_unsigned_integer
  (int size, const void *unsigned_integer);

extern IEEE_double_t IEEE_double_from_integer (int size, const void *integer);
extern IEEE_double_t IEEE_double_from_unsigned_integer
  (int size, const void *unsigned_integer);

#ifdef IEEE_QUAD
extern IEEE_quad_t IEEE_quad_from_integer (int size, const void *integer);
extern IEEE_quad_t IEEE_quad_from_unsigned_integer
  (int size, const void *unsigned_integer);
#endif

extern void IEEE_single_to_integer (int size, IEEE_float_t single_float,
                                    void *integer);
extern void IEEE_double_to_integer (int size, IEEE_double_t double_float,
                                    void *integer);
#ifdef IEEE_QUAD
extern void IEEE_quad_to_integer (int size, IEEE_quad_t quad_float,
				  void *integer);
#endif

extern void IEEE_single_to_unsigned_integer
  (int size, IEEE_float_t single_float, void *unsigned_integer);
extern void IEEE_double_to_unsigned_integer
  (int size, IEEE_double_t double_float, void *unsigned_integer);
#ifdef IEEE_QUAD
extern void IEEE_quad_to_unsigned_integer
  (int size, IEEE_quad_t quad_float, void *unsigned_integer);
#endif

extern char *IEEE_single_to_binary_string (IEEE_float_t single_float, int base,
					   char *result);
extern char *IEEE_double_to_binary_string (IEEE_double_t double_float,
					   int base, char *result);
#ifdef IEEE_QUAD
extern char *IEEE_quad_to_binary_string (IEEE_quad_t quad_float, int base,
					 char *result);
#endif

extern char *IEEE_single_to_string (IEEE_float_t single_float, char *result);
extern char *IEEE_double_to_string (IEEE_double_t double_float, char *result);
#ifdef IEEE_QUAD
extern char *IEEE_quad_to_string (IEEE_quad_t quad_float, char *result);
#endif

extern char *IEEE_single_from_binary_string (const char *operand, int base,
					     IEEE_float_t *result);
extern char *IEEE_double_from_binary_string (const char *operand, int base,
					     IEEE_double_t *result);
#ifdef IEEE_QUAD
extern char *IEEE_quad_from_binary_string (const char *operand, int base,
					   IEEE_quad_t *result);
#endif

extern char *IEEE_single_from_string (const char *operand,
                                      IEEE_float_t *result);
extern char *IEEE_double_from_string (const char *operand,
                                      IEEE_double_t *result);
#ifdef IEEE_QUAD
extern char *IEEE_quad_from_string (const char *operand, IEEE_quad_t *result);
#endif

#else /* #ifndef __cplusplus */


class IEEE
{
public:

  static int set_trap_mask (int mask);
  static int get_trap_mask (void);
             
  static int set_sticky_status_bits (int mask);
  static int get_sticky_status_bits (void);
  
  static int get_status_bits (void);
             
  static int set_round (int round_mode);
  static int get_round (void);
  
  static void default_floating_point_exception_trap (void);
  static void (*set_floating_point_exception_trap
               (void (*function) (void))) (void);

  static void reset (void);

};

class IEEE_double;

class IEEE_float
{  
public:

  unsigned char float_bytes [IEEE_FLOAT_SIZE];

  /* The following two functions allocate memory for the class
     instance. */

  inline void *operator new (size_t size)
    {
      return allocate::malloc (size);
    }

  inline void *operator new[] (size_t size)
    {
      return allocate::malloc (size);
    }

  /* The following two functions free memory for the class instance. */

  inline void operator delete (void *mem)
    {
      allocate:: free (mem);
    }

  inline void operator delete[] (void *mem)
    {
      allocate:: free (mem);
    }

  void positive_zero (void);
  void negative_zero (void);
  void NaN (void);
  void trapping_NaN (void);
  void positive_infinity (void);
  void negative_infinity (void);
  void positive_maximum (void);
  void negative_maximum (void);
  void positive_minimum (void);
  void negative_minimum (void);
  
  int is_positive_zero (void);
  int is_negative_zero (void);
  int is_NaN (void);
  int is_trapping_NaN (void);
  int is_positive_infinity (void);
  int is_negative_infinity (void);
  
  int is_normalized (void);
  int is_denormalized (void);
  
  class IEEE_float operator + (class IEEE_float &op);
  class IEEE_float operator - (class IEEE_float &op);
  class IEEE_float operator * (class IEEE_float &op);
  class IEEE_float operator / (class IEEE_float &op);
  
  int operator == (class IEEE_float &op);
  int operator != (class IEEE_float &op);
  int operator > (class IEEE_float &op);
  int operator < (class IEEE_float &op);
  int operator >= (class IEEE_float &op);
  int operator <= (class IEEE_float &op);
  
  class IEEE_double to_double (void);
#ifdef IEEE_QUAD
  class IEEE_quad to_quad (void);
#endif

  class IEEE_float &from_signed_integer (int size, const void *integer);
  class IEEE_float &from_unsigned_integer (int size,
                                           const void *unsigned_integer);

  void to_signed_integer (int size, void *integer);
  void to_unsigned_integer (int size, void *unsigned_integer);

  char *to_binary_string (int base, char *result);

  char *to_string (char *result);
  
  char *from_binary_string (const char *operand, int base);

  char *from_string (const char *operand);

  IEEE_float (void)
    {
      this->positive_zero ();
    }

  IEEE_float (float f)
    {
      char str [40];

      sprintf (str, "%.20e", f);
      this->from_string (str);
    }
};

typedef class IEEE_float IEEE_float_t;

#ifndef NO_TEMPLATE

template <int size> 
inline IEEE_float &IEEE_float_from_unsint
  (class IEEE_float &single, class unsint<size> &unsigned_integer)
{
  return single.from_unsigned_integer (size, unsigned_integer.container);
}

template <int size>
inline IEEE_float &IEEE_float_from_sint (class IEEE_float &single,
                                               class sint<size> &integer)
{
  return single.from_signed_integer (size, integer.container);
}

template <int size>
inline void IEEE_float_to_sint (class IEEE_float &single,
                                class sint<size> &integer)
{
  single.to_signed_integer (size, integer.container);
}

template <int size>
inline void IEEE_float_to_unsint (class IEEE_float &single,
                                  class unsint<size> &unsigned_integer)
{
  single.to_unsigned_integer (size, unsigned_integer.container);
}

#endif /* #ifndef NO_TEMPLATE */


class IEEE_double
{  
public:

  unsigned char double_bytes [IEEE_DOUBLE_SIZE];

  /* The following two functions allocate memory for the class
     instance. */

  inline void *operator new (size_t size)
    {
      return allocate::malloc (size);
    }

  inline void *operator new[] (size_t size)
    {
      return allocate::malloc (size);
    }

  /* The following two functions free memory for the class instance. */

  inline void operator delete (void *mem)
    {
      allocate:: free (mem);
    }

  inline void operator delete[] (void *mem)
    {
      allocate:: free (mem);
    }

  void positive_zero (void);
  void negative_zero (void);
  void NaN (void);
  void trapping_NaN (void);
  void positive_infinity (void);
  void negative_infinity (void);
  void positive_maximum (void);
  void negative_maximum (void);
  void positive_minimum (void);
  void negative_minimum (void);
  
  int is_positive_zero (void);
  int is_negative_zero (void);
  int is_NaN (void);
  int is_trapping_NaN (void);
  int is_positive_infinity (void);
  int is_negative_infinity (void);
  
  int is_normalized (void);
  int is_denormalized (void);
  
  class IEEE_double operator + (class IEEE_double &op);
  class IEEE_double operator - (class IEEE_double &op);
  class IEEE_double operator * (class IEEE_double &op);
  class IEEE_double operator / (class IEEE_double &op);
  
  int operator == (class IEEE_double &op);
  int operator != (class IEEE_double &op);
  int operator > (class IEEE_double &op);
  int operator < (class IEEE_double &op);
  int operator >= (class IEEE_double &op);
  int operator <= (class IEEE_double &op);
  
  class IEEE_float to_single (void);
#ifdef IEEE_QUAD
  class IEEE_quad to_quad (void);
#endif

  class IEEE_double &from_signed_integer (int size,
                                         const void *integer);
  class IEEE_double &from_unsigned_integer (int size,
                                           const void *unsigned_integer);

  void to_signed_integer (int size, void *integer);
  void to_unsigned_integer (int size, void *unsigned_integer);

  char *to_binary_string (int base, char *result);

  char *to_string (char *result);
  
  char *from_binary_string (const char *operand, int base);

  char *from_string (const char *operand);

  IEEE_double (void)
    {
      this->positive_zero ();
    }

  IEEE_double (double d)
    {
      char str [40];

      sprintf (str, "%.20e", d);
      this->from_string (str);
    }

};

typedef class IEEE_double IEEE_double_t;

#ifndef NO_TEMPLATE

template <int size> 
inline IEEE_double &IEEE_double_from_unsint
  (class IEEE_double &double_float, class unsint<size> &unsigned_integer)
{
  return double_float.from_unsigned_integer (size, unsigned_integer.container);
}

template <int size>
inline IEEE_double &IEEE_double_from_sint
  (class IEEE_double &double_float, class sint<size> &integer)
{
  return double_float.from_signed_integer (size, integer.container);
}

template <int size>
inline void IEEE_double_to_sint (class IEEE_double &double_float,
                                 class sint<size> &integer)
{
  double_float.to_signed_integer (size, integer.container);
}

template <int size>
inline void IEEE_double_to_unsint (class IEEE_double &double_float,
                                   class unsint<size> &unsigned_integer)
{
  double_float.to_unsigned_integer (size, unsigned_integer.container);
}

#endif /* #ifndef NO_TEMPLATE */


/* The following type represents correspondingly IEEE double precision
   floating point numbers.  The numbers are represented by structure
   in order to correctly use C language operator `=' and argument
   passing. */

typedef class IEEE_double IEEE_double_t;


#ifdef IEEE_QUAD
class IEEE_quad
{  
public:

  unsigned char quad_bytes [IEEE_QUAD_SIZE];

  /* The following two functions allocate memory for the class
     instance. */

  inline void *operator new (size_t size)
    {
      return allocate::malloc (size);
    }

  inline void *operator new[] (size_t size)
    {
      return allocate::malloc (size);
    }

  /* The following two functions free memory for the class instance. */

  inline void operator delete (void *mem)
    {
      allocate:: free (mem);
    }

  inline void operator delete[] (void *mem)
    {
      allocate:: free (mem);
    }

  void positive_zero (void);
  void negative_zero (void);
  void NaN (void);
  void trapping_NaN (void);
  void positive_infinity (void);
  void negative_infinity (void);
  void positive_maximum (void);
  void negative_maximum (void);
  void positive_minimum (void);
  void negative_minimum (void);
  
  int is_positive_zero (void);
  int is_negative_zero (void);
  int is_NaN (void);
  int is_trapping_NaN (void);
  int is_positive_infinity (void);
  int is_negative_infinity (void);
  
  int is_normalized (void);
  int is_denormalized (void);
  
  class IEEE_quad operator + (class IEEE_quad &op);
  class IEEE_quad operator - (class IEEE_quad &op);
  class IEEE_quad operator * (class IEEE_quad &op);
  class IEEE_quad operator / (class IEEE_quad &op);
  
  int operator == (class IEEE_quad &op);
  int operator != (class IEEE_quad &op);
  int operator > (class IEEE_quad &op);
  int operator < (class IEEE_quad &op);
  int operator >= (class IEEE_quad &op);
  int operator <= (class IEEE_quad &op);
  
  class IEEE_float to_single (void);
  class IEEE_double to_double (void);

  class IEEE_quad &from_signed_integer (int size,
                                         const void *integer);
  class IEEE_quad &from_unsigned_integer (int size,
                                           const void *unsigned_integer);

  void to_signed_integer (int size, void *integer);
  void to_unsigned_integer (int size, void *unsigned_integer);

  char *to_binary_string (int base, char *result);

  char *to_string (char *result);
  
  char *from_binary_string (const char *operand, int base);

  char *from_string (const char *operand);

  IEEE_quad (void)
    {
      this->positive_zero ();
    }

  IEEE_quad (double d)
    {
      char str [40];

      sprintf (str, "%.20e", d);
      this->from_string (str);
    }

};

typedef class IEEE_quad IEEE_quad_t;

#ifndef NO_TEMPLATE

template <int size> 
inline IEEE_quad &IEEE_quad_from_unsint
  (class IEEE_quad &quad_float, class unsint<size> &unsigned_integer)
{
  return quad_float.from_unsigned_integer (size, unsigned_integer.container);
}

template <int size>
inline IEEE_quad &IEEE_quad_from_sint
  (class IEEE_quad &quad_float, class sint<size> &integer)
{
  return quad_float.from_signed_integer (size, integer.container);
}

template <int size>
inline void IEEE_quad_to_sint (class IEEE_quad &quad_float,
                                 class sint<size> &integer)
{
  quad_float.to_signed_integer (size, integer.container);
}

template <int size>
inline void IEEE_quad_to_unsint (class IEEE_quad &quad_float,
                                   class unsint<size> &unsigned_integer)
{
  quad_float.to_unsigned_integer (size, unsigned_integer.container);
}

#endif /* #ifndef NO_TEMPLATE */


/* The following type represents correspondingly IEEE quad precision
   floating point numbers.  The numbers are represented by structure
   in order to correctly use C language operator `=' and argument
   passing. */

typedef class IEEE_quad IEEE_quad_t;
#endif /* #ifdef IEEE_QUAD */

#endif /* #ifndef __cplusplus */


#endif /* ifndef __IEEE_float__ */
