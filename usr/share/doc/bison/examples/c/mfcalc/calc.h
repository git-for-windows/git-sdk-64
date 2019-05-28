/* Function type. */
typedef double (func_t) (double);

/* Data type for links in the chain of symbols. */
struct symrec
{
  char *name;  /* name of symbol */
  int type;    /* type of symbol: either VAR or FUN */
  union
  {
    double var;    /* value of a VAR */
    func_t *fun;   /* value of a FUN */
  } value;
  struct symrec *next;  /* link field */
};

typedef struct symrec symrec;

/* The symbol table: a chain of 'struct symrec'. */
extern symrec *sym_table;

symrec *putsym (char const *name, int sym_type);
symrec *getsym (char const *name);
