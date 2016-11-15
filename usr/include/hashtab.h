/* FILE NAME:   hashtab.h

   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This is part of package for work with hash tables; you can
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

   TITLE:       Include file of package for work with variable length
                hash tables 

   DESCRIPTION: This header file contains type definitions and ANSI C
       prototype definitions of the package functions and definition of
       external variable of the package and C++ class `hash table'.

*/

#ifndef __HASH_TABLE__
#define __HASH_TABLE__

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#endif /* #ifdef HAVE_CONFIG_H */

#include <stdlib.h>

/* The hash table element is represented by the following type. */

typedef const void *hash_table_entry_t;


#ifndef __cplusplus


/* Hash tables are of the following type.  The structure
   (implementation) of this type is not needed for using the hash
   tables.  All work with hash table should be executed only through
   functions mentioned below. */

typedef struct
{
  /* Current size (in entries) of the hash table */
  size_t size;
  /* Current number of elements including also deleted elements */
  size_t number_of_elements;
  /* Current number of deleted elements in the table */
  size_t number_of_deleted_elements;
  /* The following member is used for debugging. Its value is number
     of all calls of `find_hash_table_entry' for the hash table. */
  int searches;
  /* The following member is used for debugging.  Its value is number
     of collisions fixed for time of work with the hash table. */
  int collisions;
  /* Pointer to function for evaluation of hash value (any unsigned value).
     This function has one parameter of type hash_table_entry_t. */
  unsigned (*hash_function) (hash_table_entry_t el_ptr);
  /* Pointer to function for test on equality of hash table elements (two
     parameter of type hash_table_entry_t. */
  int (*eq_function) (hash_table_entry_t el1_ptr,
                      hash_table_entry_t el2_ptr);
  /* Table itself */
  hash_table_entry_t *entries;
} *hash_table_t;


/* The following variable is used for debugging. Its value is number
   of all calls of `find_hash_table_entry' for all hash tables. */

extern int all_searches;

/* The following variable is used for debugging. Its value is number
   of collisions fixed for time of work with all hash tables. */

extern int all_collisions;

/* The prototypes of the package functions. */

extern hash_table_t create_hash_table
  (size_t size, unsigned (*hash_function) (hash_table_entry_t el_ptr),
   int (*eq_function) (hash_table_entry_t el1_ptr,
                       hash_table_entry_t el2_ptr));

extern void empty_hash_table (hash_table_t htab);

extern void delete_hash_table (hash_table_t htab);

extern hash_table_entry_t *find_hash_table_entry
  (hash_table_t htab, hash_table_entry_t element, int reserve);

extern void remove_element_from_hash_table_entry (hash_table_t htab,
                                                  hash_table_entry_t element);

extern size_t hash_table_size (hash_table_t htab);

extern size_t hash_table_elements_number (hash_table_t htab);

extern int hash_table_collisions (hash_table_t htab);

extern int all_hash_table_collisions (void);

#else /* #ifndef __cplusplus */



/* Hash tables are of the following class. */

class hash_table
{
  /* Current size (in entries) of the hash table */
  size_t _size;
  /* Current number of elements including also deleted elements */
  size_t number_of_elements;
  /* Current number of deleted elements in the table */
  size_t number_of_deleted_elements;
  /* The following member is used for debugging. Its value is number
     of all calls of `find_hash_table_entry' for the hash table. */
  int searches;
  /* The following member is used for debugging.  Its value is number
     of collisions fixed for time of work with the hash table. */
  int _collisions;
  /* Pointer to function for evaluation of hash value (any unsigned value).
     This function has one parameter of type hash_table_entry_t. */
  unsigned (*hash_function) (hash_table_entry_t el_ptr);
  /* Pointer to function for test on equality of hash table elements (two
     parameter of type hash_table_entry_t. */
  int (*eq_function) (hash_table_entry_t el1_ptr,
                      hash_table_entry_t el2_ptr);
  /* Table itself */
  hash_table_entry_t *entries;

  /* The following variable is used for debugging. Its value is number
     of all calls of `find_hash_table_entry' for all hash tables. */

  static int all_searches;

  /* The following variable is used for debugging. Its value is number
     of collisions fixed for time of work with all hash tables. */

  static int _all_collisions;

public:

  /* Constructor. */
  hash_table
    (size_t size, unsigned (*hash_function) (hash_table_entry_t el_ptr),
     int (*eq_function) (hash_table_entry_t el1_ptr,
                         hash_table_entry_t el2_ptr));
  /* Destructor. */
  ~hash_table (void);

  void *operator new (size_t);
  void *operator new[] (size_t);
  void operator delete (void *);
  void operator delete[] (void *);

  void empty (void);

  hash_table_entry_t *find_entry (hash_table_entry_t element, int reserve);
  
  void remove_element_from_entry (hash_table_entry_t element);

  /* The following function returns current size of given hash
     table. */

  inline size_t size (void)
    {
      return _size;
    }

  /* The following function returns current number of elements in
     given hash table. */

  inline size_t elements_number (void)
    {
      return number_of_elements - number_of_deleted_elements;
    }
  
  /* The following function returns number of percents of fixed
     collisions during all work with given hash table. */

  inline int collisions (void)
    {
      int searches;

      searches = this->searches;
      if (searches == 0)
        searches++;
      return _collisions * 100 / searches;
    }

  /* The following function returns number of percents of fixed
     collisions during all work with all hash tables. */
  
  static inline int all_collisions (void)
    {
      int searches;
      
      searches = all_searches;
      if (searches == 0)
        searches++;
      return _all_collisions * 100 / searches;
    }
private:

  void expand_hash_table (void);

};

typedef class hash_table *hash_table_t;


#endif /* #ifndef __cplusplus */

#endif /* #ifndef __HASH_TABLE__ */
