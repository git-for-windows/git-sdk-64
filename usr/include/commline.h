/* FILE NAME:   commline.h

   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This is part of package for work command line; you can redistribute
   it and/or modify it under the terms of the GNU Library General
   Public License as published by the Free Software Foundation; either
   version 2, or (at your option) any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Include file of package for work with command line

   DESCRIPTION: This header file contains ANSI C prototype definitions
                of the package functions and definitions of external
                variables of the package and C++ class for work with
                command line.

*/


#ifndef __COMMAND_LINE__
#define __COMMAND_LINE__

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#endif

#ifndef __cplusplus


extern int argument_count;
extern char **argument_vector;

extern int start_command_line_processing (int argc, char **argv,
                                          const char *description);
extern void output_command_line_description (void);
extern int next_operand (int flag_of_first);
extern int number_of_operands (void);
extern int next_option (int flag_of_first);
extern char *option_characteristics (int argument_number,
                                     int *option_has_argument);
extern int last_option_place (const char *option_name);
extern char *option_argument (const char *option_name);



#else /* #ifndef __cplusplus */


#include "objstack.h"
#include "vlobject.h"

class command_line
{
  /* Number of elements in previous vector.  I.e. this variable has
     analogous value as parameter `argc' of function `main'.*/

  int _argument_count;

  /* The following variable contains pointers to arguments in the
     command line.  The element with order number 0 points to the
     command name.  I.e. this variable has analogous value as
     parameter `argv' of function `main'. */

  char **_argument_vector;

  /* The following variable length oblect contains all option
     descriptions.  The elements are ordered by option name. */

  vlo_t *all_options;

  /* The following oblect stack contains all option names. */

  os_t *option_names;

  /* The following string is a reference to description of options.
     The description form is described in file header. */

  const char *description_of_options;

  /* The following variables are used by function `next_operand'. */

  /* Number of command line argument with which the next operand will
     be searched. */

  int next_operand_number;

  /* TRUE if all the rest arguments are operands (i.e. `--' is
     processed in command line). */

  int only_operands;

  /* The following variables are used by function next_option. */

  /* Number of command line argument with which the next option will
     be searched for. */

  int next_option_number;

  /* TRUE if all the rest arguments are operands (i.e. `--' is
     processed in command line. */

  int no_more_options;

  /* The following structure is used for storing information about
     options which may be present in command line. */

public:
  struct option_description
  {
    /* Option prefix in the option description on which option in the
       command line is recognized.  For example, `-D' is option name
       in option description `-DMACRO=DEFN'. */
    char *option_name;
    /* The rest in the option description.  For previous example, it
       is `MACRO=DEFN'. */
    char *option_suffix;
    /* TRUE if the option suffix starts with white spaces.  It means
       that the corresponding option has obligatory
       option-argument. */
    int separate_suffix_option;
  };

private:
  struct option_description *option_description (int argument_number);

public:
  
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

  inline int argument_count (void) {return _argument_count;}
  inline char **argument_vector (void) {return _argument_vector;}

  void output_description (void);
  int next_operand (int flag_of_first);
  int number_of_operands (void);
  int next_option (int flag_of_first);
  char *option_characteristics (int argument_number,
                                int *option_has_argument);
  int last_option_place (const char *option_name);
  char *option_argument (const char *option_name);

  command_line (int argc, char **argv, const char *description,
                int &correct_description_flag);
  ~command_line (void);
};


#endif /* #ifndef __cplusplus */

#endif /* #ifndef __COMMAND_LINE__ */
