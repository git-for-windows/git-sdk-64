
%{
  #include <stdio.h>  /* For printf, etc. */
  #include <math.h>   /* For pow, used in the grammar.  */
  #include "calc.h"   /* Contains definition of 'symrec'.  */
  int yylex (void);
  void yyerror (char const *);
%}


%define api.value.type union /* Generate YYSTYPE from these types:  */
%token <double>  NUM         /* Simple double precision number.  */
%token <symrec*> VAR FNCT    /* Symbol table pointer: variable and function.  */
%type  <double>  exp


%precedence '='
%left '-' '+'
%left '*' '/'
%precedence NEG /* negation--unary minus */
%right '^'      /* exponentiation */

/* Generate the parser description file.  */
%verbose
/* Enable run-time traces (yydebug).  */
%define parse.trace

/* Formatting semantic values.  */
%printer { fprintf (yyoutput, "%s", $$->name); } VAR;
%printer { fprintf (yyoutput, "%s()", $$->name); } FNCT;
%printer { fprintf (yyoutput, "%g", $$); } <double>;
%% /* The grammar follows.  */

input:
  %empty
| input line
;



line:
  '\n'
| exp '\n'   { printf ("%.10g\n", $1); }
| error '\n' { yyerrok;                }
;



exp:
  NUM                { $$ = $1;                         }
| VAR                { $$ = $1->value.var;              }
| VAR '=' exp        { $$ = $3; $1->value.var = $3;     }
| FNCT '(' exp ')'   { $$ = (*($1->value.fnctptr))($3); }
| exp '+' exp        { $$ = $1 + $3;                    }
| exp '-' exp        { $$ = $1 - $3;                    }
| exp '*' exp        { $$ = $1 * $3;                    }
| exp '/' exp        { $$ = $1 / $3;                    }
| '-' exp  %prec NEG { $$ = -$2;                        }
| exp '^' exp        { $$ = pow ($1, $3);               }
| '(' exp ')'        { $$ = $2;                         }
;

/* End of grammar.  */
%%

struct init
{
  char const *fname;
  double (*fnct) (double);
};



struct init const arith_fncts[] =
{
  { "atan", atan },
  { "cos",  cos  },
  { "exp",  exp  },
  { "ln",   log  },
  { "sin",  sin  },
  { "sqrt", sqrt },
  { 0, 0 },
};



/* The symbol table: a chain of 'struct symrec'.  */
symrec *sym_table;



/* Put arithmetic functions in table.  */
static
void
init_table (void)
{
  int i;
  for (i = 0; arith_fncts[i].fname != 0; i++)
    {
      symrec *ptr = putsym (arith_fncts[i].fname, FNCT);
      ptr->value.fnctptr = arith_fncts[i].fnct;
    }
}

#include <stdlib.h> /* malloc. */
#include <string.h> /* strlen. */


symrec *
putsym (char const *sym_name, int sym_type)
{
  symrec *ptr = (symrec *) malloc (sizeof (symrec));
  ptr->name = (char *) malloc (strlen (sym_name) + 1);
  strcpy (ptr->name,sym_name);
  ptr->type = sym_type;
  ptr->value.var = 0; /* Set value to 0 even if fctn.  */
  ptr->next = (struct symrec *)sym_table;
  sym_table = ptr;
  return ptr;
}



symrec *
getsym (char const *sym_name)
{
  symrec *ptr;
  for (ptr = sym_table; ptr != (symrec *) 0;
       ptr = (symrec *)ptr->next)
    if (strcmp (ptr->name, sym_name) == 0)
      return ptr;
  return 0;
}

#include <ctype.h>


int
yylex (void)
{
  int c;

  /* Ignore white space, get first nonwhite character.  */
  while ((c = getchar ()) == ' ' || c == '\t')
    continue;

  if (c == EOF)
    return 0;



  /* Char starts a number => parse the number.         */
  if (c == '.' || isdigit (c))
    {
      ungetc (c, stdin);
      scanf ("%lf", &yylval.NUM);
      return NUM;
    }


  /* Char starts an identifier => read the name.       */
  if (isalpha (c))
    {
      /* Initially make the buffer long enough
         for a 40-character symbol name.  */
      static size_t length = 40;
      static char *symbuf = 0;
      symrec *s;
      int i;

      if (!symbuf)
        symbuf = (char *) malloc (length + 1);

      i = 0;
      do

        {
          /* If buffer is full, make it bigger.        */
          if (i == length)
            {
              length *= 2;
              symbuf = (char *) realloc (symbuf, length + 1);
            }
          /* Add this character to the buffer.         */
          symbuf[i++] = c;
          /* Get another character.                    */
          c = getchar ();
        }


      while (isalnum (c));

      ungetc (c, stdin);
      symbuf[i] = '\0';



      s = getsym (symbuf);
      if (s == 0)
        s = putsym (symbuf, VAR);
      *((symrec**) &yylval) = s;
      return s->type;
    }

  /* Any other character is a token by itself.        */
  return c;
}


/* Called by yyparse on error.  */
void
yyerror (char const *s)
{
  fprintf (stderr, "%s\n", s);
}



int
main (int argc, char const* argv[])
{
  int i;
  /* Enable parse traces on option -p.  */
  for (i = 1; i < argc; ++i)
    if (!strcmp(argv[i], "-p"))
      yydebug = 1;
  init_table ();
  return yyparse ();
}

