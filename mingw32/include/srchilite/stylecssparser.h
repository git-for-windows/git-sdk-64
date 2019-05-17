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
     BOLD = 258,
     ITALICS = 259,
     UNDERLINE = 260,
     FIXED = 261,
     NOTFIXED = 262,
     NOREF = 263,
     KEY = 264,
     COLOR = 265,
     BG_COLOR = 266,
     STRINGDEF = 267,
     BG_STRINGDEF = 268
   };
#endif
/* Tokens.  */
#define BOLD 258
#define ITALICS 259
#define UNDERLINE 260
#define FIXED 261
#define NOTFIXED 262
#define NOREF 263
#define KEY 264
#define COLOR 265
#define BG_COLOR 266
#define STRINGDEF 267
#define BG_STRINGDEF 268




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 2068 of yacc.c  */
#line 74 "../../../lib/srchilite/stylecssparser.yy"

  int tok ; /* command */
  const std::string * string ; /* string : id, ... */
  srchilite::StyleConstant flag ;
  srchilite::StyleConstants *styleconstants;
  srchilite::KeyList *keylist;



/* Line 2068 of yacc.c  */
#line 86 "../../../lib/srchilite/stylecssparser.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE stylecsssc_lval;


