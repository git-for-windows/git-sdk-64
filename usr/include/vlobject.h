/* FILE NAME:   vlobject.h

   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This is part of package for work with variable length objects; you
   can redistribute it and/or modify it under the terms of the GNU
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

   TITLE:       Include file of package for work with variable length
                objects (VLO)

   DESCRIPTION: This header file contains macros and C++ class for
       work with variable length objects (VLO).  Any number of bytes
       my be added to and removed from the end of VLO.  If it is
       needed the memory allocated for storing variable length object
       may be expanded possibly with changing the object place.  But
       between any additions of the bytes (tailoring) the object place
       is not changed.  To decrease number of changes of the object
       place the memory being allocated for the object is longer than
       the current object length.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C/C++
       compiler command line) before the package macros usage disables
       to fix some internal errors and errors of usage of the package.
         C: Because arguments of all macros which return a result
       (`VLO_LENGTH', `VLO_BEGIN', and `VLO_END') may be evaluated
       many times no side-effects should be in the arguments.  A file
       using the package can be compiled with option
       `-DVLO_DEFAULT_LENGTH=...'.  */


#ifndef __VLO__
#define __VLO__

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <stdlib.h>
#include <string.h>

#include "allocate.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif


/* Default initial size of memory is allocated for VLO when the object
   is created (with zero initial size).  This macro can be redefined
   in C compiler command line or with the aid of directive `#undef'
   before any using the package macros. */

#ifndef VLO_DEFAULT_LENGTH
#define VLO_DEFAULT_LENGTH 512
#endif


#ifndef __cplusplus


/* This type describes a descriptor of variable length object.  All
   work with variable length object is executed by following macros
   through the descriptors.  Structure (implementation) of this type
   is not needed for using variable length object.  But it should
   remember that work with the object through several descriptors is
   not safe. */

typedef struct
{
  /* Pointer to memory currently used for storing the VLO. */
  char *vlo_start;
  /* Pointer to first byte after the last VLO byte. */
  char *vlo_free;
  /* Pointer to first byte after the memory currently allocated for storing
     the VLO. */
  char *vlo_boundary;
} vlo_t;


/* This macro is used for creation of VLO with initial zero length.
   If initial length of memory needed for the VLO is equal to 0 the
   initial allocated memory length is equal to VLO_DEFAULT_LENGTH.
   VLO must be created before any using other macros of the package
   for work with given VLO.  The macro has not side effects. */

#define VLO_CREATE(vlo, initial_length)\
  do\
  {\
    vlo_t *_temp_vlo = &(vlo);\
    size_t temp_initial_length = (initial_length);\
    temp_initial_length = (temp_initial_length != 0 ? temp_initial_length\
                                                    : VLO_DEFAULT_LENGTH);\
    MALLOC (_temp_vlo->vlo_start, temp_initial_length);\
    _temp_vlo->vlo_boundary = _temp_vlo->vlo_start + temp_initial_length;\
    _temp_vlo->vlo_free = _temp_vlo->vlo_start;\
  }\
  while (0)


/* This macro is used for freeing memory allocated for VLO.  Any work
   (except for creation) with given VLO is not possible after
   evaluation of this macro.  The macro has not side effects. */

#ifndef NDEBUG
#define VLO_DELETE(vlo)\
  do\
  {\
    vlo_t *_temp_vlo = &(vlo);\
    assert (_temp_vlo->vlo_start != NULL);\
    FREE (_temp_vlo->vlo_start);\
    _temp_vlo->vlo_start = NULL;\
  }\
  while (0)
#else
#define VLO_DELETE(vlo) FREE ((vlo).vlo_start)
#endif /* #ifndef NDEBUG */

/* This macro makes that length of VLO will be equal to zero (but
   memory for VLO is not freed and not reallocated).  The macro has
   not side effects. */

#define VLO_NULLIFY(vlo)\
  do\
  {\
    vlo_t *_temp_vlo = &(vlo);\
    assert (_temp_vlo->vlo_start != NULL);\
    _temp_vlo->vlo_free = _temp_vlo->vlo_start;\
  }\
  while (0)


/* The following macro makes that length of memory allocated for VLO
   becames equal to VLO length.  The macro has not side effects. */

#define VLO_TAILOR(vlo) _VLO_tailor_function(&(vlo))


/* This macro returns current length of VLO.  The macro has side
   effects! */

#ifndef NDEBUG
#define VLO_LENGTH(vlo) ((vlo).vlo_start != NULL\
                         ? (vlo).vlo_free - (vlo).vlo_start\
                         : (abort (), 0))
#else
#define VLO_LENGTH(vlo) ((vlo).vlo_free - (vlo).vlo_start)
#endif /* #ifndef NDEBUG */


/* This macro returns pointer (of type `void *') to the first byte of
   the VLO.  The macro has side effects!  Remember also that the VLO
   may change own place after any addition. */

#ifndef NDEBUG
#define VLO_BEGIN(vlo) ((vlo).vlo_start != NULL\
                        ? (void *) (vlo).vlo_start\
                        : (abort (), (void *) 0))
#else
#define VLO_BEGIN(vlo) ((void *) (vlo).vlo_start)
#endif /* #ifndef NDEBUG */

/* This macro returns pointer (of type `void *') to the last byte of
   VLO.  The macro has side effects!  Remember also that the VLO may
   change own place after any addition. */

#ifndef NDEBUG
#define VLO_END(vlo) ((vlo).vlo_start != NULL\
                      ? (void *) ((vlo).vlo_free - 1)\
                      : (abort (), (void *) 0))
#else
#define VLO_END(vlo) ((void *) ((vlo).vlo_free - 1))
#endif /* #ifndef NDEBUG */

/* This macro returns pointer (of type `void *') to the next byte of
   the last byte of VLO.  The macro has side effects!  Remember also
   that the VLO may change own place after any addition. */

#ifndef NDEBUG
#define VLO_BOUND(vlo) ((vlo).vlo_start != NULL\
                        ? (void *) (vlo).vlo_free\
                        : (abort (), (void *) 0))
#else
#define VLO_BOUND(vlo) ((void *) (vlo).vlo_free)
#endif /* #ifndef NDEBUG */

/* This macro removes N bytes from the end of VLO.  VLO is nullified
   if its length is less than N.  The macro has not side effects. */

#define VLO_SHORTEN(vlo, n)\
  do\
  {\
    vlo_t *_temp_vlo = &(vlo);\
    size_t _temp_n = (n);\
    assert (_temp_vlo->vlo_start != NULL);\
    if ((size_t) VLO_LENGTH (*_temp_vlo) < _temp_n)\
      _temp_vlo->vlo_free = _temp_vlo->vlo_start;\
    else\
      _temp_vlo->vlo_free -= _temp_n;\
  }\
  while (0)


/* This macro increases length of VLO.  The values of bytes added to
   the end of VLO will be not defined.  The macro has not side
   effects. */

#define VLO_EXPAND(vlo, length)\
  do\
  {\
    vlo_t *_temp_vlo = &(vlo);\
    size_t _temp_length = (length);\
    assert (_temp_vlo->vlo_start != NULL);\
    if (_temp_vlo->vlo_free + _temp_length > _temp_vlo->vlo_boundary)\
      _VLO_expand_memory (_temp_vlo, _temp_length);\
    _temp_vlo->vlo_free += _temp_length;\
  }\
  while (0)


/* This macro adds a byte to the end of VLO.  The macro has not side
   effects. */

#define VLO_ADD_BYTE(vlo, b)\
  do\
  {\
    vlo_t *_temp_vlo = &(vlo);\
    assert (_temp_vlo->vlo_start != NULL);\
    if (_temp_vlo->vlo_free >= _temp_vlo->vlo_boundary)\
      _VLO_expand_memory (_temp_vlo, 1);\
    *_temp_vlo->vlo_free++ = (b);\
  }\
  while (0)


/* This macro adds memory bytes to the end of VLO.  The macro has not
   side effects. */

#define VLO_ADD_MEMORY(vlo, str, length)\
  do\
  {\
    vlo_t *_temp_vlo = &(vlo);\
    size_t _temp_length = (length);\
    assert (_temp_vlo->vlo_start != NULL);\
    if (_temp_vlo->vlo_free + _temp_length > _temp_vlo->vlo_boundary)\
      _VLO_expand_memory (_temp_vlo, _temp_length);\
    _VLO_memcpy (_temp_vlo->vlo_free, (str), _temp_length);\
    _temp_vlo->vlo_free += _temp_length;\
  }\
  while (0)


/* This macro adds C string (with end marker '\0') to the end of VLO.
   Before the addition the macro delete last character of the VLO.
   The last character is suggested to be C string end marker '\0'.
   The macro has not side effects. */

#define VLO_ADD_STRING(vlo, str) _VLO_add_string_function(&(vlo), (str))


/* The following functions are to be used only by the package macros.
   Remember that they are internal functions - all work with VLO is
   executed through the macros. */

extern void _VLO_tailor_function (vlo_t *vlo);
extern void _VLO_add_string_function (vlo_t *vlo, const char *str);
extern void _VLO_expand_memory (vlo_t *vlo, size_t additional_length);
extern void _VLO_memcpy (void *to, const void *from, size_t length);


#else /* #ifndef __cplusplus */


extern void _VLO_memcpy (void *to, const void *from, size_t length);

/* This type describes a descriptor of variable length object.  It
   should remember that work with the object through several
   descriptors is not safe. */

class vlo
{
  /* Pointer to memory currently used for storing the VLO. */
  char *vlo_start;
  /* Pointer to first byte after the last VLO byte. */
  char *vlo_free;
  /* Pointer to first byte after the memory currently allocated for storing
     the VLO. */
  char *vlo_boundary;
public:
  
  /* This function is used for creation of VLO with initial zero
     length.  If initial length of memory needed for the VLO is equal
     to 0 the initial allocated memory length is equal to
     VLO_DEFAULT_LENGTH. */
  
  inline vlo (size_t initial_length = VLO_DEFAULT_LENGTH)
    {
      initial_length = (initial_length != 0
                        ? initial_length : VLO_DEFAULT_LENGTH);
      vlo_start = (char *) allocate::malloc (initial_length);
      vlo_boundary = vlo_start + initial_length;
      vlo_free = vlo_start;
    }


  /* This function is used for freeing memory allocated for VLO.  Any
     work (except for creation) with given VLO is not possible after
     evaluation of this function. */

  inline ~vlo (void)
    {
#ifndef NDEBUG
      assert (vlo_start != NULL);
      allocate::free (vlo_start);
      vlo_start = NULL;
#else
      allocate::free (vlo_start);
#endif /* #ifndef NDEBUG */
    }

  /* The following two functions allocate memory for the descriptor. */

  inline void *operator new (size_t size)
    {
      return allocate::malloc (size);
    }

  inline void *operator new[] (size_t size)
    {
      return allocate::malloc (size);
    }

  /* The following two functions free memory for the descriptor. */

  inline void operator delete (void *mem)
    {
      allocate:: free (mem);
    }

  inline void operator delete[] (void *mem)
    {
      allocate:: free (mem);
    }

  /* This function makes that length of VLO will be equal to zero (but
     memory for VLO is not freed and not reallocated). */

  inline void nullify (void)
    {
      assert (vlo_start != NULL);
      vlo_free = vlo_start;
    }


  /* The following function makes that length of memory allocated for
     VLO becames equal to VLO length. */

  void tailor (void);


  /* This function returns current length of VLO. */

  inline size_t length (void)
    {
      assert (vlo_start != NULL);
      return vlo_free - vlo_start;
    }


  /* This function returns pointer (of type `void *') to the first byte
     of the VLO.  Remember also that the VLO may change own place
     after any addition. */

  inline void *begin (void)
    {
      assert (vlo_start != NULL);
      return (void *) vlo_start;
    }
  
  /* This function returns pointer (of type `void *') to the last byte
     of VLO.  Remember also that the VLO may change own place after
     any addition. */

  inline void *end (void)
    {
      assert (vlo_start != NULL);
      return (void *) (vlo_free - 1);
    }

  /* This function returns pointer (of type `void *') to the next byte
     of the last byte of VLO.  Remember also that the VLO may change
     own place after any addition. */

  inline void *bound (void)
    {
      assert (vlo_start != NULL);
      return (void *) vlo_free;
    }

  /* This function removes N bytes from the end of VLO.  VLO is nullified
     if its length is less than N. */

  inline void shorten (size_t n)
    {
      assert (vlo_start != NULL);
      if (length () < n)
        vlo_free = vlo_start;
      else
        vlo_free -= n;
    }


  /* This function increases length of VLO.  The values of bytes added
     to the end of VLO will be not defined. */

  void expand (size_t length)
    {
      assert (vlo_start != NULL);
      if (vlo_free + length > vlo_boundary)
        _VLO_expand_memory (length);
      vlo_free += length;
    }


  /* This function adds a byte to the end of VLO. */

  inline void add_byte (int b)
    {
      assert (vlo_start != NULL);
      if (vlo_free >= vlo_boundary)
        _VLO_expand_memory (1);
      *vlo_free++ = b;
    }


  /* This function adds memory bytes to the end of VLO. */

  inline void add_memory (const void *str, size_t length)
    {
      assert (vlo_start != NULL);
      if (vlo_free + length > vlo_boundary)
        _VLO_expand_memory (length);
      _VLO_memcpy (vlo_free, str, length);
      vlo_free += length;
    }


  /* This function adds C string (with end marker '\0') to the end of
     VLO.  Before the addition the function deletes last character of
     the VLO.  The last character is suggested to be C string end
     marker '\0'. */

  void add_string (const char *str);

private:

  /* The following functions is used only by the class functions. */

  void _VLO_expand_memory (size_t additional_length);
};

typedef vlo vlo_t;

#endif /* #ifndef __cplusplus */

#endif /* #ifndef __VLO__ */
