/* FILE NAME:   objstack.h

   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This is part of package for work with stack of objects; you can
   redistribute it and/or modify it under the terms of the GNU Library
   General Public License as published by the Free Software
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

   TITLE:       Include file of package for work with stacks of
                objects (OS)

   DESCRIPTION: The package `objstack' implements efficient work
       with stacks of objects (OS).  Work with the object on the stack
       top is analogous to one with a variable length object.  One
       motivation for the package is the problem of growing char
       strings in symbol tables.  Memory for OS is allocated by
       segments.  A segment may contain more one objects.  The most
       recently allocated segment contains object on the top of OS.
       If there is not sufficient free memory for the top object than
       new segment is created and the top object is transferred into
       the new segment, i.e. there is not any memory reallocation.
       Therefore the top object may change its address.  But other
       objects never change address.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C/C++
       compiler command line) before the package macros usage disables
       to fix some internal errors and errors of usage of the package.
         A file using the package can be compiled with option
       `-DOS_DEFAULT_SEGMENT_LENGTH=...'.
         C: Because arguments of all macros which return a result
       (`OS_TOP_LENGTH', `OS_TOP_BEGIN', and `OS_TOP_END') may be
       evaluated many times no side-effects should be in the
       arguments.

*/


#ifndef __OS__
#define __OS__

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <string.h>
#include <stdlib.h>
#include <stddef.h>

#include "allocate.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

/* This auxiliary structure is used to evaluation of maximum
   alignment for objects. */

struct _os_auxiliary_struct
{
  char _os_character;
  double _os_double;
};

/* This macro is auxiliary.  Its value is maximum alignment for objects. */

#ifdef VMS
/* It is necessarry for VMS C compiler. */
#define _OS_ALIGNMENT  4
#else
#define _OS_ALIGNMENT offsetof (struct _os_auxiliary_struct, _os_double)
#if 0
#define _OS_ALIGNMENT\
  ((char *) &((struct _os_auxiliary_struct *) 64)->_os_double - (char *) 64)
#else
#endif
#endif

/* This macro is auxiliary.  Its value is aligned address nearest to `a'. */

#define _OS_ALIGNED_ADDRESS(a)\
  ((void *) ((size_t) ((char *) (a) + (_OS_ALIGNMENT - 1))\
             & (~(size_t) (_OS_ALIGNMENT - 1))))

/* This macro value is default size of memory segments which will be
   allocated for OS when the stack is created (with zero initial
   segment size).  This is also minimal size of all segments.
   Original value of the macros is equal to 512.  This macro can be
   redefined in C compiler command line or with the aid of directive
   `#undef' before any using the package macros.  */

#ifndef OS_DEFAULT_SEGMENT_LENGTH
#define OS_DEFAULT_SEGMENT_LENGTH 512
#endif



#ifndef __cplusplus

/* This internal structure describes segment of an object stack. */

struct _os_segment
{
  struct _os_segment *os_previous_segment;
  char os_segment_contest [_OS_ALIGNMENT];
};

/* This type describes a descriptor of stack of objects.  All work
   with stack of objects is executed by following macros through the
   descriptors.  Structure (implementation) of this type is not needed
   for using stack of objects.  But it should remember that work with
   the stack through several descriptors is not safe. */

typedef struct
{
  /* The real length of the first memory segment. */
  size_t initial_segment_length;
  struct _os_segment *os_current_segment;
  /* Pointer to memory currently used for storing the top object. */
  char *os_top_object_start;
  /* Pointer to first byte after the last byte of the top object. */
  char *os_top_object_free;
  /* Pointer to first byte after the memory allocated for storing
     the OS segment and the top object. */
  char *os_boundary;
} os_t;


/* This macro creates OS which contains the single zero length object.
   If initial length of OS segment is equal to zero the allocated
   memory segments length is equal to `OS_DEFAULT_SEGMENT_LENGTH'.
   But in any case the segment length is always not less than maximum
   alignment.  OS must be created before any using other macros of the
   package for work with given OS.  The macro has not side effects. */

#define OS_CREATE(os, initial_segment_length)\
  _OS_create_function (&(os), initial_segment_length)

/* This macro is used for freeing memory allocated for OS.  Any work
   (except for creation) with given OS is not possible after
   evaluation of this macros.  The macro has not side effects. */

#define OS_DELETE(os) _OS_delete_function (& (os))

/* This macro is used for freeing memory allocated for OS except for
   the first segment.  The macro has not side effects. */

#define OS_EMPTY(os) _OS_empty_function (& (os))

/* This macro makes that length of variable length object on the top
   of OS will be equal to zero.  The macro has not side effects. */

#define OS_TOP_NULLIFY(os)\
  do\
  {\
    os_t *_temp_os = &(os);\
    assert (_temp_os->os_top_object_start != NULL);\
    _temp_os->os_top_object_free = _temp_os->os_top_object_start;\
  }\
  while (0)

/* The macro creates new variable length object with initial zero
   length on the top of OS .  The work (analogous to one with variable
   length object) with object which was on the top of OS are finished,
   i.e. the object will never more change address.  The macro has not
   side effects. */

#define OS_TOP_FINISH(os)\
  do\
  {\
    os_t *_temp_os = &(os);\
    assert (_temp_os->os_top_object_start != NULL);\
    _temp_os->os_top_object_start\
      = _OS_ALIGNED_ADDRESS (_temp_os->os_top_object_free);\
    _temp_os->os_top_object_free = _temp_os->os_top_object_start;\
  }\
  while (0)

/* This macro returns current length of variable length object on the
   top of OS.  The macro has side effects! */

#ifndef NDEBUG
#define OS_TOP_LENGTH(os)\
  ((os).os_top_object_start != NULL\
   ? (os).os_top_object_free - (os).os_top_object_start\
   : (abort (), 0))
#else
#define OS_TOP_LENGTH(os)  ((os).os_top_object_free - (os).os_top_object_start)
#endif

/* This macro returns pointer to the first byte of variable length
   object on the top of OS.  The macro has side effects!  Remember also
   that the top object may change own place after any addition. */

#ifndef NDEBUG
#define OS_TOP_BEGIN(os)\
  ((os).os_top_object_start != NULL ? (void *) (os).os_top_object_start\
                                    : (abort (), (void *) 0))
#else
#define OS_TOP_BEGIN(os) ((void *) (os).os_top_object_start)
#endif

/* This macro returns pointer (of type `void *') to the last byte of
   variable length object on the top OS.  The macro has side effects!
   Remember also that the top object may change own place after any
   addition. */

#ifndef NDEBUG
#define OS_TOP_END(os)\
  ((os).os_top_object_start != NULL ? (void *) ((os).os_top_object_free - 1)\
                                    : (abort (), (void *) 0))
#else
#define OS_TOP_END(os) ((void *) ((os).os_top_object_free - 1))
#endif

/* This macro returns pointer (of type `void *') to the next byte of
   the last byte of variable length object on the top OS.  The macro
   has side effects!  Remember also that the top object may change own
   place after any addition. */

#ifndef NDEBUG
#define OS_TOP_BOUND(os)\
  ((os).os_top_object_start != NULL ? (void *) (os).os_top_object_free\
                                    : (abort (), (void *) 0))
#else
#define OS_TOP_BOUND(os) ((void *) (os).os_top_object_free)
#endif

/* This macro removes N bytes from the end of variable length object
   on the top of OS.  The top variable length object is nullified if
   its length is less than N.  The macro has not side effects. */

#define OS_TOP_SHORTEN(os, n)\
  do\
  {\
    os_t *_temp_os = &(os);\
    size_t _temp_n = (n);\
    assert (_temp_os->os_top_object_start != NULL);\
    if ((size_t) OS_TOP_LENGTH (*_temp_os) < _temp_n)\
      _temp_os->os_top_object_free = _temp_os->os_top_object_start;\
    else\
      _temp_os->os_top_object_free -= _temp_n;\
  }\
  while (0)

/* This macro increases length of variable length object on the top of
   OS on given number of bytes.  The values of bytes added to the end
   of variable length object on the top of OS will be not defined.
   The macro has not side effects. */

#define OS_TOP_EXPAND(os, length)\
  do\
  {\
    os_t *_temp_os = &(os);\
    size_t _temp_length = (length);\
    assert (_temp_os->os_top_object_start != NULL);\
    if (_temp_os->os_top_object_free + _temp_length > _temp_os->os_boundary)\
      _OS_expand_memory (_temp_os, _temp_length);\
    _temp_os->os_top_object_free += _temp_length;\
  }\
  while (0)

/* This macro adds byte to the end of variable length object on the
   top of OS.  The macro has not side effects. */

#define OS_TOP_ADD_BYTE(os, b)\
  do\
  {\
    os_t *_temp_os = &(os);\
    assert (_temp_os->os_top_object_start != NULL);\
    if (_temp_os->os_top_object_free >= _temp_os->os_boundary)\
      _OS_expand_memory (_temp_os, 1);\
    *_temp_os->os_top_object_free++ = (b);\
  }\
  while (0)

/* This macro adds memory bytes to the end of variable length object
   on the top of OS.  The macro has not side effects. */

#define OS_TOP_ADD_MEMORY(os, str, length)\
  do\
  {\
    os_t *_temp_os = &(os);\
    size_t _temp_length = (length);\
    assert (_temp_os->os_top_object_start != NULL);\
    if (_temp_os->os_top_object_free + _temp_length > _temp_os->os_boundary)\
      _OS_expand_memory (_temp_os, _temp_length);\
    _OS_memcpy (_temp_os->os_top_object_free, (str), _temp_length);\
    _temp_os->os_top_object_free += _temp_length;\
  }\
  while (0)

/* This macro adds C string (with end marker '\0') to the end of
   variable length object on the top of OS.  Before the addition the
   macro delete last character of the object.  The last character is
   suggested to be C string end marker '\0'.  The macro has not side
   effects. */

#define OS_TOP_ADD_STRING(os, str) _OS_add_string_function(&(os), (str))

/* The following functions are to be used only by the package macros.
   Remember that they are internal functions - all work with OS is
   executed through the macros. */

extern void _OS_create_function (os_t *os, size_t initial_segment_length);
extern void _OS_delete_function (os_t *os);
extern void _OS_empty_function (os_t *os);
extern void _OS_add_string_function (os_t *os, const char *str);
extern void _OS_expand_memory (os_t *os, size_t additional_length);
extern void _OS_memcpy (void *to, const void *from, size_t length);


#else /* #ifndef __cplusplus */

extern void _OS_memcpy (void *to, const void *from, size_t length);

/* This class describes a descriptor of stack of objects.  All work
   with stack of objects is executed by member functions.  It should
   remember that work with the stack through several descriptors is
   not safe. */

class os
{
  /* The real length of the first memory segment. */
  size_t initial_segment_length;
  class _os_segment *os_current_segment;
  /* Pointer to memory currently used for storing the top object. */
  char *os_top_object_start;
  /* Pointer to the first byte after the last byte of the top
     object. */
  char *os_top_object_free;
  /* Pointer to first byte after the memory allocated for storing the
     OS segment and the top object. */
  char *os_boundary;

public:

  /* These constructors create OS which contains the single zero
     length object.  If initial length of OS segment is equal to zero
     or absent, the allocated memory segments length is equal to
     `OS_DEFAULT_SEGMENT_LENGTH'.  But in any case the segment length
     is always not less than maximum alignment. */

  os (size_t initial_segment_length = OS_DEFAULT_SEGMENT_LENGTH);
  
  /* This destructor is used for freeing memory allocated for OS. */

  ~os (void);

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

  /* This function is used for freeing memory allocated for OS except
     for the first segment. */

  void empty (void);

  /* This function makes that length of variable length object on the
     top of OS will be equal to zero. */

  inline void top_nullify (void)
    {
      assert (os_top_object_start != NULL);
      os_top_object_free = os_top_object_start;
    }

  /* The function creates new variable length object with initial zero
     length on the top of OS.  The work (analogous to one with
     variable length object) with object which was on the top of OS
     are finished, i.e. the object will never more change address. */

  inline void top_finish (void)
    {
      assert (os_top_object_start != NULL);
      os_top_object_start = (char *) _OS_ALIGNED_ADDRESS (os_top_object_free);
      os_top_object_free = os_top_object_start;
    }

  /* This function returns current length of variable length object on
     the top of OS. */

  inline size_t top_length (void)
    {
#ifndef NDEBUG
      return (os_top_object_start != NULL
              ? os_top_object_free - os_top_object_start
              : (abort (), 0));
#else
      return (os_top_object_free - os_top_object_start);
#endif
    }

  /* This function returns pointer to the first byte of variable
     length object on the top of OS.  Remember also that the top
     object may change own place after any addition. */

  inline void *top_begin (void)
    {
#ifndef NDEBUG
      return (os_top_object_start != NULL
              ? (void *) os_top_object_start
              : (abort (), (void *) 0));
#else
      return ((void *) os_top_object_start);
#endif
    }

  /* This function returns pointer (of type `void *') to the last byte
     of variable length object on the top OS.  Remember also that the
     top object may change own place after any addition. */

  inline void *top_end (void)
    {
#ifndef NDEBUG
      return (os_top_object_start != NULL
              ? (void *) (os_top_object_free - 1)
              : (abort (), (void *) 0));
#else
      return ((void *) (os_top_object_free - 1));
#endif
    }

  /* This function returns pointer (of type `void *') to the next byte
     of the last byte of variable length object on the top OS.
     Remember also that the top object may change own place after any
     addition. */

  inline void *top_bound (void)
    {
#ifndef NDEBUG
      return (os_top_object_start != NULL
              ? (void *) os_top_object_free
              : (abort (), (void *) 0));
#else
      return ((void *) os_top_object_free);
#endif
    }

  /* This function removes N bytes from the end of variable length
     object on the top of OS.  The top variable length object is
     nullified if its length is less than N. */

  inline void top_shorten (size_t n)
    {
      assert (os_top_object_start != NULL);
      if (top_length () < n)
        os_top_object_free = os_top_object_start;
      else
        os_top_object_free -= n;
    }

  /* This function increases length of variable length object on the
     top of OS on given number of bytes.  The values of bytes added to
     the end of variable length object on the top of OS will be not
     defined. */

  inline void top_expand (size_t length)
    {
      assert (os_top_object_start != NULL);
      if (os_top_object_free + length > os_boundary)
        _OS_expand_memory (length);
      os_top_object_free += length;
    }

  /* This function adds byte to the end of variable length object on
     the top of OS. */

  inline void top_add_byte (int b)
    {
      assert (os_top_object_start != NULL);
      if (os_top_object_free >= os_boundary)
        _OS_expand_memory (1);
      *os_top_object_free++ = b;
    }

  /* This function adds memory bytes to the end of variable length
     object on the top of OS. */

  inline void top_add_memory (const void *str, size_t length)
    {
      assert (os_top_object_start != NULL);
      if (os_top_object_free + length > os_boundary)
        _OS_expand_memory (length);
      _OS_memcpy (os_top_object_free, str, length);
      os_top_object_free += length;
    }

  /* This function adds C string (with end marker '\0') to the end of
     variable length object on the top of OS.  Before the addition the
     macro delete last character of the object.  The last character is
     suggested to be C string end marker '\0'. */

  void top_add_string (const char *str);

  /* The following function is used only by the package functions. */

  void _OS_expand_memory (size_t additional_length);
};

typedef os os_t;

/* This internal structure describes segment of an object stack. */

class _os_segment
{
  class _os_segment *os_previous_segment;
  char os_segment_contest [_OS_ALIGNMENT];
  friend os::os (size_t initial_segment_length);
  friend os::~os (void);
  friend void os::empty (void);
  friend void os::_OS_expand_memory (size_t additional_length);
};

#endif /* #ifndef __cplusplus */

#endif /* #ifndef __OS__ */
