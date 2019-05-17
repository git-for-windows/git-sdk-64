/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison interface for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     BEGIN_T = 258,
     END_T = 259,
     ENVIRONMENT_T = 260,
     STATE_T = 261,
     MULTILINE_T = 262,
     DELIM_T = 263,
     START_T = 264,
     ESCAPE_T = 265,
     NESTED_T = 266,
     EXIT_ALL = 267,
     EXIT_T = 268,
     VARDEF_T = 269,
     REDEF_T = 270,
     SUBST_T = 271,
     NONSENSITIVE_T = 272,
     WRONG_BACKREFERENCE = 273,
     LEVEL = 274,
     KEY = 275,
     STRINGDEF = 276,
     REGEXPNOPREPROC = 277,
     VARUSE = 278,
     BACKREFVAR = 279,
     WRONG_INCLUDE_FILE = 280,
     REGEXPDEF = 281
   };
#endif
/* Tokens.  */
#define BEGIN_T 258
#define END_T 259
#define ENVIRONMENT_T 260
#define STATE_T 261
#define MULTILINE_T 262
#define DELIM_T 263
#define START_T 264
#define ESCAPE_T 265
#define NESTED_T 266
#define EXIT_ALL 267
#define EXIT_T 268
#define VARDEF_T 269
#define REDEF_T 270
#define SUBST_T 271
#define NONSENSITIVE_T 272
#define WRONG_BACKREFERENCE 273
#define LEVEL 274
#define KEY 275
#define STRINGDEF 276
#define REGEXPNOPREPROC 277
#define VARUSE 278
#define BACKREFVAR 279
#define WRONG_INCLUDE_FILE 280
#define REGEXPDEF 281




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 2068 of yacc.c  */
#line 95 "../../../lib/srchilite/langdefparser.yy"

  int tok ; /* command */
  bool booloption ;
  const std::string * string ; /* string : id, ... */
  class srchilite::StringDef *stringdef;
  class StringDefs *stringdefs;
  class LangElem *langelem;
  class StateLangElem *statelangelem;
  class StateStartLangElem *statestartlangelem;
  class LangElems *langelems;
  class NamedSubExpsLangElem *namedsubexpslangelem;
  struct Key *key;
  struct ElementNamesList *keys;
  int flag ;
  unsigned int level;



/* Line 2068 of yacc.c  */
#line 121 "../../../lib/srchilite/langdefparser.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE langdef_lval;

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

extern YYLTYPE langdef_lloc;

