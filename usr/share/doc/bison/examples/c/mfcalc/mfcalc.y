%{
  #include <stdio.h>  /* For printf, etc. */
  #include <math.h>   /* For pow, used in the grammar. */
  #include "calc.h"   /* Contains definition of 'symrec'. */
  int yylex (void);
  void yyerror (char const *);
%}

%define api.value.type union /* Generate YYSTYPE from these types: */
%token <double>  NUM     /* Double precision number. */
%token <symrec*> VAR FUN /* Symbol table pointer: variable/function. */
%nterm <double>  exp

%precedence '='
%left '-' '+'
%left '*' '/'
%precedence NEG /* negation--unary minus */
%right '^'      /* exponentiation */
/* Generate the parser description file. */
%verbose
/* Enable run-time traces (yydebug). */
%define parse.trace

/* Formatting semantic values. */
%printer { fprintf (yyo, "%s", $$->name); } VAR;
%printer { fprintf (yyo, "%s()", $$->name); } FUN;
%printer { fprintf (yyo, "%g", $$); } <double>;
%% /* The grammar follows. */
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
  NUM
| VAR                { $$ = $1->value.var;              }
| VAR '=' exp        { $$ = $3; $1->value.var = $3;     }
| FUN '(' exp ')'    { $$ = $1->value.fun ($3);         }
| exp '+' exp        { $$ = $1 + $3;                    }
| exp '-' exp        { $$ = $1 - $3;                    }
| exp '*' exp        { $$ = $1 * $3;                    }
| exp '/' exp        { $$ = $1 / $3;                    }
| '-' exp  %prec NEG { $$ = -$2;                        }
| exp '^' exp        { $$ = pow ($1, $3);               }
| '(' exp ')'        { $$ = $2;                         }
;
/* End of grammar. */
%%

struct init
{
  char const *name;
  func_t *fun;
};

struct init const arith_funs[] =
{
  { "atan", atan },
  { "cos",  cos  },
  { "exp",  exp  },
  { "ln",   log  },
  { "sin",  sin  },
  { "sqrt", sqrt },
  { 0, 0 },
};

/* The symbol table: a chain of 'struct symrec'. */
symrec *sym_table;

/* Put arithmetic functions in table. */
static void
init_table (void)
{
  for (int i = 0; arith_funs[i].name; i++)
    {
      symrec *ptr = putsym (arith_funs[i].name, FUN);
      ptr->value.fun = arith_funs[i].fun;
    }
}

/* The mfcalc code assumes that malloc and realloc
   always succeed, and that integer calculations
   never overflow.  Production-quality code should
   not make these assumptions.  */
#include <stdlib.h> /* malloc, realloc. */
#include <string.h> /* strlen. */

symrec *
putsym (char const *name, int sym_type)
{
  symrec *res = (symrec *) malloc (sizeof (symrec));
  res->name = strdup (name);
  res->type = sym_type;
  res->value.var = 0; /* Set value to 0 even if fun. */
  res->next = sym_table;
  sym_table = res;
  return res;
}

symrec *
getsym (char const *name)
{
  for (symrec *p = sym_table; p; p = p->next)
    if (strcmp (p->name, name) == 0)
      return p;
  return NULL;
}

#include <ctype.h>
#include <stddef.h>

int
yylex (void)
{
  int c = getchar ();

  /* Ignore white space, get first nonwhite character. */
  while (c == ' ' || c == '\t')
    c = getchar ();

  if (c == EOF)
    return 0;

  /* Char starts a number => parse the number. */
  if (c == '.' || isdigit (c))
    {
      ungetc (c, stdin);
      scanf ("%lf", &yylval.NUM);
      return NUM;
    }

  /* Char starts an identifier => read the name. */
  if (isalpha (c))
    {
      static ptrdiff_t bufsize = 0;
      static char *symbuf = 0;
      ptrdiff_t i = 0;
      do
        {
          /* If buffer is full, make it bigger. */
          if (bufsize <= i)
            {
              bufsize = 2 * bufsize + 40;
              symbuf = realloc (symbuf, bufsize);
            }
          /* Add this character to the buffer. */
          symbuf[i++] = c;
          /* Get another character. */
          c = getchar ();
        }
      while (isalnum (c));

      ungetc (c, stdin);
      symbuf[i] = '\0';

      symrec *s = getsym (symbuf);
      if (!s)
        s = putsym (symbuf, VAR);
      yylval.VAR = s; /* or yylval.FUN = s. */
      return s->type;
    }

  /* Any other character is a token by itself. */
  return c;
}

/* Called by yyparse on error. */
void yyerror (char const *s)
{
  fprintf (stderr, "%s\n", s);
}

int main (int argc, char const* argv[])
{
  /* Enable parse traces on option -p. */
  if (argc == 2 && strcmp(argv[1], "-p") == 0)
    yydebug = 1;
  init_table ();
  return yyparse ();
}
