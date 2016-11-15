/* This is interface file of general (working on any CFG) syntax
   parser with minimal error recovery and syntax directed translation.
   The algorithm is originated from Earley's algorithm.  The algorithm
   is sufficiently fast to be used in serious language processors. */

/*
   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This is part of Earley's parser implementation; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2, or (at your option) any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.

*/

/* Attention: It is distrubuted under GPL not LGPL. */

#ifndef __EARLEY__
#define __EARLEY__

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
#ifndef INT_MAX
#define INT_MAX 2147483647
#endif  
#endif
 
#include "allocate.h"

/* The following is a forward descration of grammar formed by function
   earley_read_grammar. */
struct grammar;

/* The following value is reserved to be designation of empty node for
   translation.  It should be positive number which is not intersected
   with symbol numbers. */
#define EARLEY_NIL_TRANSLATION_NUMBER       INT_MAX

/* The following values are earley parser error codes. */

#define EARLEY_NO_MEMORY                     1
#define EARLEY_UNDEFINED_OR_BAD_GRAMMAR      2
#define EARLEY_DESCRIPTION_SYNTAX_ERROR_CODE 3
#define EARLEY_FIXED_NAME_USAGE              4
#define EARLEY_REPEATED_TERM_DECL            5
#define EARLEY_NEGATIVE_TERM_CODE            6
#define EARLEY_REPEATED_TERM_CODE            7
#define EARLEY_NO_RULES                      8
#define EARLEY_TERM_IN_RULE_LHS              9
#define EARLEY_INCORRECT_TRANSLATION         10
#define EARLEY_NEGATIVE_COST                 11
#define EARLEY_INCORRECT_SYMBOL_NUMBER       12
#define EARLEY_REPEATED_SYMBOL_NUMBER        13
#define EARLEY_UNACCESSIBLE_NONTERM          14
#define EARLEY_NONTERM_DERIVATION            15
#define EARLEY_LOOP_NONTERM                  16
#define EARLEY_INVALID_TOKEN_CODE            17

/* The following describes the type of parse tree node. */
enum earley_tree_node_type
{
  EARLEY_NIL,
  EARLEY_ERROR,
  EARLEY_TERM,
  EARLEY_ANODE,
  EARLEY_ALT
};

/* The following node exists in one examplar. See comment to read_rule. */
struct earley_nil
{
  /* The following member is not used.  It is only to conform ANSI C
     standard. */
  int dummy;
};

/* The following node exists in one examplar.  It is used as
   transaltion of pseudo terminal `error'. */
struct earley_error
{
  /* The following member is not used.  It is only to conform ANSI C
     standard. */
  int dummy;
};

/* The following structure describes terminal node. */
struct earley_term
{
  /* The terminal code. */
  int code;
  /* The terminal attributes. */
  void *attr;
};

/* The following structure describes abstract node. */
struct earley_anode
{
  /* The abstract node name. */
  const char *name;
  /* The following value is cost of the node plus costs of all
     children if the cost flag is set up.  Otherwise, the value is cost
     of the abstract node itself. */
  int cost;
  /* References for nodes for which the abstract node refers.  The end
     marker of the array is value NULL. */
  struct earley_tree_node **children;
};

/* The following structure describes alternatives in the parse tree.
   Which are presents only for ambiguous grammar. */
struct earley_alt
{
  /* The node (all node types but alternative) for this
     alternative. */
  struct earley_tree_node *node;
  /* The next alternative. */
  struct earley_tree_node *next;
};

/* The folloiwing structure describes generalized node of the parse
   tree. */
struct earley_tree_node
{
  /* The type of node. */
  enum earley_tree_node_type type;
  /* The node itself. */
  union
  {
    struct earley_nil nil;
    struct earley_error error;
    struct earley_term term;
    struct earley_anode anode;
    struct earley_alt alt;
  } val;
};

#ifndef __cplusplus

/* The following function creates undefined grammar.  The function
   returns NULL if there is no memory.  This function should be called
   the first. */
extern struct grammar *earley_create_grammar (void);

/* The function returns the last occurred error code for given
   grammar. */
extern int earley_error_code (struct grammar *g);

/* The function returns message are always contains error message
   corresponding to the last occurred error code. */
extern const char *earley_error_message (struct grammar *g);

/* The following function reads terminals/rules into grammar G and
   checks it depending on STRICT_P.  It returns zero if it is all ok.
   Otherwise, the function returns error code occured (its code will
   be in earley_error_code and message in earley_error_message).

   READ_TERMINAL is function for reading terminals.  This function is
   called before function read_rule.  The function should return the
   name and the code of the next terminal.  If all terminals have been
   read the function returns NULL.  The return code should be
   nonnegative.

   READ_RULE is function called to read the next rule.  This function
   is called after function read_terminal.  The function should return
   the name of LHS rule and array of names of symbols in RHS of the
   rule (the array end marker should be NULL).  If all rules have been
   read the function returns NULL.  All symbol with name which was not
   provided the previous function are considered to be nonterminal.
   The function also returns translation given by abstract node name
   and its fields which will be translation of symbols (with indexes
   given in array *TRANSL) in the RHS of the rule.  All indexes in
   TRANSL should be different (so the translation of a symbol can not
   be represented twice).  The end marker of the array should be a
   negative value.  There is a reserved value of the translation
   symbol number denoting empty node.  It is value defined by macro
   EARLEY_NIL_TRANSLATION_NUMBER.  If *TRANSL is NULL or contains only
   the end marker, translations of the rule will be nil node.  If
   ABS_NODE is NULL, abstract node is not created.  In this case
   *TRANSL should be NULL or contain at most one element which means
   that the translation of the rule will be nil node or the
   translation of the symbol in RHS given by the single array element.
   The cost of the abstract node if given is passed through
   ANODE_COST. */
extern int
earley_read_grammar (struct grammar *g, int strict_p,
		     const char *(*read_terminal) (int *code),
		     const char *(*read_rule) (const char ***rhs,
					       const char **abs_node,
                                               int *anode_cost,
					       int **transl));

/* The following function is analogous to the previous one but it
   parses grammar desrciption. */
extern int
earley_parse_grammar (struct grammar *g, int strict_p,
		      const char *description);

/* The following functions set up different parameters which affect
   parser work.  The functions return the previous parameter value.

   o lookahead_level means usage of static (if 1) or dynamic (2)
     lookahead to decrease size of sets.  Static lookaheads gives the
     best results with the point of space and speed, dynamic ones does
     sligthly worse, and no usage of lookaheds does the worst.  The
     default value is 1.

   o debug_level says what debugging information to output (it works
     only if we compiled without defined macro NO_EARLEY_DEBUG_PRINT).
     The defualt value is 0.

   o one_parse_flag means building only one parse tree.  For
     unambiguous grammar the flag does not affect the result.  The
     default value is 1.

   o cost_flag means usage costs to build tree (trees if
     one_parse_flag is not set up) with minimal cost.  For unambiguous
     grammar the flag does not affect the result.  The default value
     is 0.

   o error_recovery_flag means making error recovery if syntax error
     occurred.  Otherwise, syntax error results in finishing parsing
     (although syntax_error is called once).  The default value is 1.

   o recovery_match means how much subsequent tokens should be
     successfuly shifted to finish error recovery.  The default value
     is 3. */
extern int earley_set_lookahead_level (struct grammar *grammar, int level);
extern int earley_set_debug_level (struct grammar *grammar, int level);
extern int earley_set_one_parse_flag (struct grammar *grammar, int flag);
extern int earley_set_cost_flag (struct grammar *grammar, int flag);
extern int earley_set_error_recovery_flag (struct grammar *grammar, int flag);
extern int earley_set_recovery_match (struct grammar *grammar, int n_toks);

/* The following function parses input according read grammar.  The
   function returns the error code (which will be also in
   earley_error_code).  If the code is zero, the function will also
   The *root will be NULL only if syntax error was occurred and error
   recovery was switched off).  The function sets up *AMBIGOUS_P if we
   found that the grammer is ambigous (it works even we asked only one
   parse tree without alternatives).

   The function READ_TOKEN provides input tokens.  It returns code the
   next input token and its attribute.  If the function returns
   negative value we've read all tokens.

   Function SYNTAX_ERROR prints error message about syntax error
   which occurred on token with number ERR_TOK_NUM and attribute
   ERR_TOK_ATTR.  The following four parameters describes made error
   recovery which ignored tokens starting with token given by 3rd and
   4th parameters.  The first token which was not ignored is described
   by the last parameters.  If the number of ignored tokens is zero,
   the all parameters describes the same token.  If the error recovery
   is not made (see comments for function
   `earley_set_error_recovery_flag'), the third and fifth parameters
   will be negative and forth and sixth parameters will be NULL.


   Function PARSE_ALLOC is used by earley parser to allocate memory
   for parse tree representation.  After calling earley_fin we free
   all memory allocated by earley parser.  At this point it is
   convenient to free all memory but parse tree.  Therefore we require
   the following function.  So the caller will be responsible to
   allocate and free memory for parse tree representation.  But the
   caller should not free the memory until earley_fin is called.  The
   function may be called even during reading the grammar not only
   during the parsing.  Function PARSE_FREE is used by the parser to
   free memory allocated by PARSE_ALLOC.  If it is NULL, the memory is
   not freed. */
extern int earley_parse (struct grammar *grammar,
			 int (*read_token) (void **attr),
			 void (*syntax_error) (int err_tok_num,
					       void *err_tok_attr,
					       int start_ignored_tok_num,
					       void *start_ignored_tok_attr,
					       int start_recovered_tok_num,
					       void *start_recovered_tok_attr),
			 void *(*parse_alloc) (int nmemb),
			 void (*parse_free) (void *mem),
			 struct earley_tree_node **root,
			 int *ambiguous_p);

/* The following function frees memory allocated for the grammar. */
extern void earley_free_grammar (struct grammar *grammar);

#else /* #ifndef __cplusplus */

class earley
{
  struct grammar *grammar;
  
public:

  /* The following constructor and destructor allocate and free memory
     for the grammar. */
  earley (void);
  ~earley (void);

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

  /* See comments for function earley_error_code. */
  int error_code (void);

  /* See comments for function earley_error_message. */
  const char *error_message (void);

  /* See comments for function earley_read_grammar. */
  int read_grammar (int strict_p,
			   const char *(*read_terminal) (int *code),
			   const char *(*read_rule) (const char ***rhs,
						     const char **abs_node,
						     int *anode_cost,
						     int **transl));

  /* See comments for function earley_parse_grammar. */
  int parse_grammar (int strict_p, const char *description);

  /* See comments for corresponding C functions. */
  int set_lookahead_level (int level);
  int set_debug_level (int level);
  int set_one_parse_flag (int flag);
  int set_cost_flag (int flag);
  int set_error_recovery_flag (int flag);
  int set_recovery_match (int n_toks);

  /* See comments for function earley_parse. */
  int parse (int (*read_token) (void **attr),
	     void (*syntax_error) (int err_tok_num,
				   void *err_tok_attr,
				   int start_ignored_tok_num,
				   void *start_ignored_tok_attr,
				   int start_recovered_tok_num,
				   void *start_recovered_tok_attr),
	     void *(*parse_alloc) (int nmemb),
	     void (*parse_free) (void *mem),
	     struct earley_tree_node **root,
	     int *ambiguous_p);
};

#endif /* #ifndef __cplusplus */

#endif /* #ifndef __EARLEY__ */
