/* FILE NAME:   arithm.h

   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This is part of package of arbitrary precision integer arithmetic;
   you can redistribute it and/or modify it under the terms of the GNU
   Library General Public License as published by the Free Software
   Foundation; either version 2, or (at your option) any later
   version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Include file of package for arbitrary precision integer
                arithmetic

   DESCRIPTION: This header file contains ANSI C prototype definitions of
                the package functions and definitions of external
                variable of the package and C++ classes for arbitrary
                precision integer arithmetic.

   SPECIAL CONSIDERATION:
         C++: Defining macro `NO_TEMPLATE' (e.g. by option `-D' in C++
       compiler command line) during the file compilation disables to
       use template classes `sint' and `unsint'.

*/


#ifndef __ARITHMETIC__
#define __ARITHMETIC__

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#endif /* #ifdef HAVE_CONFIG_H */


#ifndef __cplusplus

extern int overflow_bit;
extern const unsigned char *zero_constant;

extern void default_arithmetic_overflow_reaction (void);

extern void
  (*set_unsigned_integer_overflow_reaction (void (*function) (void))) (void);
extern void (*set_integer_overflow_reaction (void (*function) (void))) (void);

extern void unsigned_integer_maximum (int size, void *result);
extern void integer_minimum (int size, void *result);
extern void integer_maximum (int size, void *result);

extern void add_unsigned_integer (int size, const void *op1, const void *op2,
                                  void *result);
extern void add_integer (int size, const void *op1, const void *op2,
                         void *result);
extern void subtract_unsigned_integer (int size, const void *op1,
                                       const void *op2, void *result);
extern void subtract_integer (int size, const void *op1, const void *op2,
                              void *result);
extern void multiply_unsigned_integer (int size, const void *op1,
                                       const void *op2, void *result);
extern void multiply_integer (int size, const void *op1, const void *op2,
                              void *result);
extern void divide_unsigned_integer (int size, const void *op1,
                                     const void *op2, void *result);
extern void divide_integer (int size, const void *op1, const void *op2,
                            void *result);
extern void unsigned_integer_remainder (int size, const void *op1,
                                        const void *op2, void *result);
extern void integer_remainder (int size, const void *op1, const void *op2,
                               void *result);

extern void unsigned_integer_shift_right (int size, const void *operand,
                                          int bits, void *result);
extern void integer_shift_right (int size, const void *operand,
                                 int bits, void *result);
extern void integer_shift_left (int size, const void *operand,
                                int bits, void *result);
extern void unsigned_integer_shift_left (int size, const void *operand,
                                         int bits, void *result);

extern void integer_or (int size, const void *op1,
                        const void *op2, void *result);
extern void unsigned_integer_or (int size, const void *op1,
                                 const void *op2, void *result);
extern void integer_and (int size, const void *op1,
                         const void *op2, void *result);
extern void unsigned_integer_and (int size, const void *op1,
                                  const void *op2, void *result);
extern void integer_not (int size, const void *operand, void *result);
extern void unsigned_integer_not (int size, const void *operand, void *result);

extern int eq_unsigned_integer (int size, const void *op1, const void *op2);
extern int eq_integer (int size, const void *op1, const void *op2);
extern int ne_unsigned_integer (int size, const void *op1, const void *op2);
extern int ne_integer (int size, const void *op1, const void *op2);
extern int gt_unsigned_integer (int size, const void *op1, const void *op2);
extern int gt_integer (int size, const void *op1, const void *op2);
extern int lt_unsigned_integer (int size, const void *op1, const void *op2);
extern int lt_integer (int size, const void *op1, const void *op2);
extern int ge_unsigned_integer (int size, const void *op1, const void *op2);
extern int ge_integer (int size, const void *op1, const void *op2);
extern int le_unsigned_integer (int size, const void *op1, const void *op2);
extern int le_integer (int size, const void *op1, const void *op2);

extern void change_unsigned_integer_size
  (int operand_size, const void *operand, int result_size, void *result);
extern void change_integer_size (int operand_size, const void *operand,
                                 int result_size, void *result);

extern char *unsigned_integer_to_based_string (int size, const void *operand,
					       int base, char *result);
extern char *unsigned_integer_to_string (int size, const void *operand,
                                         char *result);
extern char *integer_to_based_string (int size, const void *operand, int base,
				      char *result);
extern char *integer_to_string (int size, const void *operand, char *result);

extern char *unsigned_integer_from_based_string (int size, const char *operand,
						 int base, void *result);
extern char *unsigned_integer_from_string (int size, const char *operand,
                                           void *result);
extern char *integer_from_based_string (int size, const char *operand,
					int base, void *result);
extern char *integer_from_string (int size, const char *operand,
                                  void *result);


#else /* #ifndef __cplusplus */


#include <stdio.h>
#include "allocate.h"

class integer
{
public:  
  static int overflow_bit;
  static const unsigned char *zero_constant;
  static void default_arithmetic_overflow_reaction (void);
};


class signed_integer : public integer
{
  static void (*overflow_reaction) (void);

public:
  static void (*set_overflow_reaction (void (*function) (void))) (void);
  
  static void minimum (int size, void *result);
  static void maximum (int size, void *result);
  
  static void add (int size, const void *op1, const void *op2, void *result);
  static void subtract (int size, const void *op1, const void *op2,
                        void *result);
  static void multiply (int size, const void *op1, const void *op2,
                        void *result);
  static void divide (int size, const void *op1, const void *op2,
                      void *result);
  static void remainder (int size, const void *op1, const void *op2,
                         void *result);
  
  static void shift_right (int size, const void *operand,
                                   int bits, void *result);
  static void shift_left (int size, const void *operand,
                          int bits, void *result);
  
  static void _or_ (int size, const void *op1, const void *op2, void *result);
  static void _and_ (int size, const void *op1, const void *op2, void *result);
  static void _not_ (int size, const void *operand, void *result);
  
  static int eq (int size, const void *op1, const void *op2);
  static int ne (int size, const void *op1, const void *op2);
  static int gt (int size, const void *op1, const void *op2);
  static int lt (int size, const void *op1, const void *op2);
  static int ge (int size, const void *op1, const void *op2);
  static int le (int size, const void *op1, const void *op2);
  
  static void change_size (int operand_size, const void *operand,
                           int result_size, void *result);
  
  static char *to_based_string (int size, const void *operand, int base,
				char *result);
  static char *to_string (int size, const void *operand, char *result);
  
  static char *from_based_string (int size, const char *operand, int base,
				  void *result);
  static char *from_string (int size, const char *operand, void *result);
};


class unsigned_integer : public integer
{
  static void (*overflow_reaction) (void);

public:
  static void (*set_overflow_reaction (void (*function) (void))) (void);
  
  static void maximum (int size, void *result);
  
  static void add (int size, const void *op1, const void *op2, void *result);
  static void subtract (int size, const void *op1,
                        const void *op2, void *result);
  static void multiply (int size, const void *op1,
                        const void *op2, void *result);
  static void divide (int size, const void *op1,
                      const void *op2, void *result);
  static void remainder (int size, const void *op1,
                         const void *op2, void *result);
  
  static void shift_right (int size, const void *operand,
                           int bits, void *result);
  static void shift_left (int size, const void *operand,
                          int bits, void *result);
  
  static void _or_ (int size, const void *op1, const void *op2, void *result);
  static void _and_ (int size, const void *op1, const void *op2, void *result);
  static void _not_ (int size, const void *operand, void *result);
  
  static int eq (int size, const void *op1, const void *op2);
  static int ne (int size, const void *op1, const void *op2);
  static int gt (int size, const void *op1, const void *op2);
  static int lt (int size, const void *op1, const void *op2);
  static int ge (int size, const void *op1, const void *op2);
  static int le (int size, const void *op1, const void *op2);
  
  static void change_size
    (int operand_size, const void *operand, int result_size, void *result);
  
  static char *to_based_string (int size, const void *operand, int base,
				char *result);
  static char *to_string (int size, const void *operand, char *result);
  static char *from_based_string (int size, const char *operand, int base,
				  void *result);
  static char *from_string (int size, const char *operand, void *result);
};


#ifndef NO_TEMPLATE

/* Signed integers: */

template <int size>
class sint : public signed_integer
{
public:

  unsigned char container [size];

  /* The following two functions allocate memory for the class
     instance. */

  inline void *operator new (size_t s)
    {
      return allocate::malloc (s);
    }

  inline void *operator new[] (size_t s)
    {
      return allocate::malloc (s);
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

  static inline class sint<size> max (void)
    {
      class sint<size> result;

      maximum (size, result.container);
      return result;
    }

  static inline class sint<size> min (void)
    {
      class sint<size> result;

      minimum (size, result.container);
      return result;
    }

  inline class sint<size> operator + (const class sint<size> &op)
    {
      class sint<size> result;

      add (size, this->container, op.container, result.container);
      return result;
    }

  inline class sint<size> operator - (const class sint<size> &op)
    {
      class sint<size> result;

      subtract (size, this->container, op.container, result.container);
      return result;
    }

  inline class sint<size> operator * (const class sint<size> &op)
    {
      class sint<size> result;

      multiply (size, this->container, op.container, result.container);
      return result;
    }

  inline class sint<size> operator / (const class sint<size> &op)
    {
      class sint<size> result;

      divide (size, this->container, op.container, result.container);
      return result;
    }

  inline class sint<size> operator % (const class sint<size> &op)
    {
      class sint<size> result;

      remainder (size, this->container, op.container, result.container);
      return result;
    }

  inline class sint<size> operator >> (int bits)
    {
      class sint<size> result;

      shift_right (size, this->container, bits, result.container);
      return result;
    }

  inline class sint<size> operator << (int bits)
    {
      class sint<size> result;

      shift_left (size, this->container, bits, result.container);
      return result;
    }

  inline class sint<size> operator | (const class sint<size> &op)
    {
      class sint<size> result;

      _or_ (size, this->container, op.container, result.container);
      return result;
    }

  inline class sint<size> operator & (const class sint<size> &op)
    {
      class sint<size> result;

      _and_ (size, this->container, op.container, result.container);
      return result;
    }

  inline class sint<size> operator ~ (void)
    {
      class sint<size> result;

      _not_ (size, this->container, result.container);
      return result;
    }

  inline int operator == (const class sint<size> &op)
    {
      return eq (size, this->container, op.container);
    }                                               
                                                    
  inline int operator != (const class sint<size> &op)      
    {                                               
      return ne (size, this->container, op.container);
    }                                               
                                                    
  inline int operator > (const class sint<size> &op)  
    {                                               
      return gt (size, this->container, op.container);
    }                                               
                                                    
  inline int operator < (const class sint<size> &op)       
    {                                               
      return lt (size, this->container, op.container);
    }                                               
                                                    
  inline int operator <= (const class sint<size> &op)      
    {                                               
      return le (size, this->container, op.container);
    }                                               
                                                    
  inline int operator >= (const class sint<size> &op)      
    {                                               
      return ge (size, this->container, op.container);
    }

  inline char *to_based_str (int base, char *result)      
    {                                               
      return to_based_string (size, this->container, base, result);
    }

  inline char *to_str (char *result)      
    {                                               
      return to_string (size, this->container, result);
    }

  inline char *from_based_str (const char *operand, int base)
    {                                               
      return from_based_string (size, operand, base, this->container);
    }

  inline char *from_str (const char *operand)
    {                                               
      return from_string (size, operand, this->container);
    }

  inline sint (int value)
    {
      char representation [30];
      
      sprintf (representation, "%d", value);
      this->from_str (representation);
    }

  inline sint (void)
    {
      int current_byte_number;

      for (current_byte_number = 0;
           current_byte_number < size;
           current_byte_number++)
        this->container [current_byte_number] = 0;
    }

};

template <int result_size, int operand_size>
inline void new_size (class sint<operand_size> &operand,
                      class sint<result_size> &result)
{
  signed_integer::change_size (operand_size, operand.container,
                               result_size, result.container);
}


/* Unsigned integers: */

template <int size>
class unsint : public unsigned_integer
{
public:

  unsigned char container [size];

  /* The following two functions allocate memory for the class
     instance. */

  inline void *operator new (size_t s)
    {
      return allocate::malloc (s);
    }

  inline void *operator new[] (size_t s)
    {
      return allocate::malloc (s);
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

  static inline class unsint<size> max (void)
    {
      class unsint<size> result;

      maximum (size, result.container);
      return result;
    }

  inline class unsint<size> operator + (const class unsint<size> &op)
    {
      class unsint<size> result;

      add (size, this->container, op.container, result.container);
      return result;
    }

  inline class unsint<size> operator - (const class unsint<size> &op)
    {
      class unsint<size> result;

      subtract (size, this->container, op.container, result.container);
      return result;
    }

  inline class unsint<size> operator * (const class unsint<size> &op)
    {
      class unsint<size> result;

      multiply (size, this->container, op.container, result.container);
      return result;
    }

  inline class unsint<size> operator / (const class unsint<size> &op)
    {
      class unsint<size> result;

      divide (size, this->container, op.container, result.container);
      return result;
    }

  inline class unsint<size> operator % (const class unsint<size> &op)
    {
      class unsint<size> result;

      remainder (size, this->container, op.container, result.container);
      return result;
    }

  inline class unsint<size> operator >> (int bits)
    {
      class unsint<size> result;

      shift_right (size, this->container, bits, result.container);
      return result;
    }

  inline class unsint<size> operator << (int bits)
    {
      class unsint<size> result;

      shift_left (size, this->container, bits, result.container);
      return result;
    }

  inline class unsint<size> operator | (const class unsint<size> &op)
    {
      class unsint<size> result;

      _or_ (size, this->container, op.container, result.container);
      return result;
    }

  inline class unsint<size> operator & (const class unsint<size> &op)
    {
      class unsint<size> result;

      _and_ (size, this->container, op.container, result.container);
      return result;
    }

  inline class unsint<size> operator ~ (void)
    {
      class unsint<size> result;

      _not_ (size, this->container, result.container);
      return result;
    }

  inline int operator == (const class unsint<size> &op)
    {
      return eq (size, this->container, op.container);
    }                                               
                                                    
  inline int operator != (const class unsint<size> &op)      
    {                                               
      return ne (size, this->container, op.container);
    }                                               
                                                    
  inline int operator > (const class unsint<size> &op)  
    {                                               
      return gt (size, this->container, op.container);
    }                                               
                                                    
  inline int operator < (const class unsint<size> &op)       
    {                                               
      return lt (size, this->container, op.container);
    }                                               
                                                    
  inline int operator <= (const class unsint<size> &op)      
    {                                               
      return le (size, this->container, op.container);
    }                                               
                                                    
  inline int operator >= (const class unsint<size> &op)      
    {                                               
      return ge (size, this->container, op.container);
    }

  inline char *to_based_str (int base, char *result)      
    {                                               
      return to_based_string (size, this->container, base, result);
    }

  inline char *to_str (char *result)      
    {                                               
      return to_string (size, this->container, result);
    }

  inline char *from_based_str (const char *operand, int base)
    {                                               
      return from_based_string (size, operand, base, this->container);
    }

  inline char *from_str (const char *operand)
    {                                               
      return from_string (size, operand, this->container);
    }

  inline unsint (unsigned int value)
    {
      char representation [30];
      
      sprintf (representation, "%d", value);
      this->from_str (representation);
    }

  inline unsint (void)
    {
      int current_byte_number;

      for (current_byte_number = 0;
           current_byte_number < size;
           current_byte_number++)
        this->container [current_byte_number] = 0;
    }

};

template <int result_size, int operand_size>
inline void new_size (class unsint<operand_size> &operand,
                      class unsint<result_size> &result)
{
  unsigned_integer::change_size (operand_size, operand.container,
                                 result_size, result.container);
}

#endif /* ifndef NO_TEMPLATE */

#endif /* #ifndef __cplusplus */

#endif /* #ifndef __ARITHMETIC__ */
