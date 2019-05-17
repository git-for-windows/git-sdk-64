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
     DOC_TEMPLATE_T = 260,
     NODOC_TEMPLATE_T = 261,
     STYLE_TEMPLATE_T = 262,
     STYLE_SEPARATOR_T = 263,
     BOLD_T = 264,
     ITALICS_T = 265,
     UNDERLINE_T = 266,
     COLOR_T = 267,
     BG_COLOR_T = 268,
     FIXED_T = 269,
     NOTFIXED_T = 270,
     COLORMAP_T = 271,
     DEFAULT_T = 272,
     ONESTYLE_T = 273,
     TRANSLATIONS_T = 274,
     EXTENSION_T = 275,
     ANCHOR_T = 276,
     REFERENCE_T = 277,
     INLINE_REFERENCE_T = 278,
     POSTLINE_REFERENCE_T = 279,
     POSTDOC_REFERENCE_T = 280,
     KEY = 281,
     STRINGDEF = 282,
     REGEXDEF = 283,
     LINE_PREFIX_T = 284,
     LINENUM_T = 285,
     WRONG_INCLUDE_FILE = 286
   };
#endif
/* Tokens.  */
#define BEGIN_T 258
#define END_T 259
#define DOC_TEMPLATE_T 260
#define NODOC_TEMPLATE_T 261
#define STYLE_TEMPLATE_T 262
#define STYLE_SEPARATOR_T 263
#define BOLD_T 264
#define ITALICS_T 265
#define UNDERLINE_T 266
#define COLOR_T 267
#define BG_COLOR_T 268
#define FIXED_T 269
#define NOTFIXED_T 270
#define COLORMAP_T 271
#define DEFAULT_T 272
#define ONESTYLE_T 273
#define TRANSLATIONS_T 274
#define EXTENSION_T 275
#define ANCHOR_T 276
#define REFERENCE_T 277
#define INLINE_REFERENCE_T 278
#define POSTLINE_REFERENCE_T 279
#define POSTDOC_REFERENCE_T 280
#define KEY 281
#define STRINGDEF 282
#define REGEXDEF 283
#define LINE_PREFIX_T 284
#define LINENUM_T 285
#define WRONG_INCLUDE_FILE 286




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 2068 of yacc.c  */
#line 62 "../../../lib/srchilite/outlangdefparser.yy"

  int tok ; /* command */
  bool booloption ;
  const std::string * string ; /* string : id, ... */
  int flag ;



/* Line 2068 of yacc.c  */
#line 121 "../../../lib/srchilite/outlangdefparser.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE outlangdef_lval;


