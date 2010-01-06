/* A Bison parser, made by GNU Bison 1.875c.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOKENS_STRING_ALL = 258,
     TOKENS_STRING_EVEN = 259,
     TOKENS_STRING_ODD = 260,
     TOKENS_STRING_NOT = 261,
     TOKENS_NUMBER = 262
   };
#endif
#define TOKENS_STRING_ALL 258
#define TOKENS_STRING_EVEN 259
#define TOKENS_STRING_ODD 260
#define TOKENS_STRING_NOT 261
#define TOKENS_NUMBER 262




/* Copy the first part of user declarations.  */
#line 1 "tokens.y"


/* Copyright 2007-2009 Cisco Systems, Inc.  All rights reserved. */

#include "plpa_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "plpa.h"
#include "plpa-taskset.h"


/*
 * Could this be done more efficiently?  Absolutely.
 *
 * But this is neat, elegant, and easy to understand / maintain.
 * Performance is not an issue here.
 */

/*
 * Typedefs
 */
typedef enum {
    ALL,
    PROCESSOR
} id_type_t;

/*
 * Global functions
 */
int token_parse(PLPA_NAME(cpu_set_t) *cpu_set);
void yyerror(char const *s);

/*
 * Local functions
 */
static void set_union(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *a, 
                      PLPA_NAME(cpu_set_t) *b);
static void set_copy(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *in);
static void cpu_set(PLPA_NAME(cpu_set_t) *out, int pos);
static void cpu_set_all(PLPA_NAME(cpu_set_t) *out, id_type_t type);
static void cpu_set_even(PLPA_NAME(cpu_set_t) *out, id_type_t type);
static void cpu_set_odd(PLPA_NAME(cpu_set_t) *out, id_type_t type);
static void cpu_set_range(PLPA_NAME(cpu_set_t) *out, int min, int max);
static void cpu_compliment(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *in);
static void sc_merge(PLPA_NAME(cpu_set_t) *out, 
                     PLPA_NAME(cpu_set_t) *cores, int cores_are_valid,
                     PLPA_NAME(cpu_set_t) *sockets, int sockets_are_valid);

/*
 * Local variables
 */
static int socket_list[PLPA_BITMASK_CPU_MAX];
static PLPA_NAME(cpu_set_t) *return_value;


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 59 "tokens.y"
typedef union YYSTYPE {
    PLPA_NAME(cpu_set_t) cpu_set;
    int number;
} YYSTYPE;
/* Line 191 of yacc.c.  */
#line 153 "tokens.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 165 "tokens.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

# ifndef YYFREE
#  define YYFREE free
# endif
# ifndef YYMALLOC
#  define YYMALLOC malloc
# endif

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   define YYSTACK_ALLOC alloca
#  endif
# else
#  if defined (alloca) || defined (_ALLOCA_H)
#   define YYSTACK_ALLOC alloca
#  else
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (defined (YYSTYPE_IS_TRIVIAL) && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined (__GNUC__) && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  20
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   44

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  13
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  10
/* YYNRULES -- Number of rules. */
#define YYNRULES  29
/* YYNRULES -- Number of states. */
#define YYNSTATES  47

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   262

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     9,     8,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    10,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    11,     2,    12,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned char yyprhs[] =
{
       0,     0,     3,     5,     7,     9,    11,    13,    17,    21,
      27,    29,    31,    33,    35,    39,    43,    47,    51,    55,
      57,    60,    65,    69,    71,    75,    79,    85,    87,    89
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      14,     0,    -1,    15,    -1,    18,    -1,    17,    -1,    16,
      -1,     7,    -1,     7,     8,     7,    -1,    15,     9,     7,
      -1,    15,     9,     7,     8,     7,    -1,     3,    -1,     4,
      -1,     5,    -1,    19,    -1,    18,     9,    19,    -1,    20,
      10,    20,    -1,    20,    10,    22,    -1,    22,    10,    20,
      -1,    22,    10,    22,    -1,     7,    -1,     6,     7,    -1,
       6,    11,    21,    12,    -1,    11,    21,    12,    -1,     7,
      -1,     7,     8,     7,    -1,    21,     9,     7,    -1,    21,
       9,     7,     8,     7,    -1,     3,    -1,     4,    -1,     5,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned char yyrline[] =
{
       0,    82,    82,    84,    87,    89,    92,    94,    96,    98,
     101,   103,   105,   108,   110,   113,   115,   117,   119,   122,
     124,   126,   128,   131,   133,   135,   137,   140,   142,   144
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TOKENS_STRING_ALL",
  "TOKENS_STRING_EVEN", "TOKENS_STRING_ODD", "TOKENS_STRING_NOT",
  "TOKENS_NUMBER", "'-'", "','", "'@'", "'{'", "'}'", "$accept", "start",
  "cpu_list", "cpu_numbers", "cpu_strings", "sc_list", "sc_expr",
  "sc_item", "sc_item_list", "sc_strings", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,    45,    44,
      64,   123,   125
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    13,    14,    14,    15,    15,    16,    16,    16,    16,
      17,    17,    17,    18,    18,    19,    19,    19,    19,    20,
      20,    20,    20,    21,    21,    21,    21,    22,    22,    22
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     1,     1,     1,     1,     1,     3,     3,     5,
       1,     1,     1,     1,     3,     3,     3,     3,     3,     1,
       2,     4,     3,     1,     3,     3,     5,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       0,    10,    11,    12,     0,     6,     0,     0,     2,     5,
       4,     3,    13,     0,     0,    20,     0,     0,    23,     0,
       1,     0,     0,     0,     0,     0,     7,     0,     0,    22,
       8,    27,    28,    29,    19,    14,    15,    16,    17,    18,
      21,    24,    25,     0,     0,     9,    26
};

/* YYDEFGOTO[NTERM-NUM]. */
static const yysigned_char yydefgoto[] =
{
      -1,     7,     8,     9,    10,    11,    12,    13,    19,    14
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -19
static const yysigned_char yypact[] =
{
      -3,    15,    17,    18,     7,    16,     0,    21,    20,   -19,
     -19,    22,   -19,    23,    24,   -19,     0,    25,    27,    10,
     -19,    29,     6,     6,     6,    11,   -19,    30,    31,   -19,
      32,   -19,   -19,   -19,   -19,   -19,   -19,   -19,   -19,   -19,
     -19,   -19,    33,    35,    36,   -19,   -19
};

/* YYPGOTO[NTERM-NUM].  */
static const yysigned_char yypgoto[] =
{
     -19,   -19,   -19,   -19,   -19,   -19,     8,   -18,    28,    -8
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -30
static const yysigned_char yytable[] =
{
       1,     2,     3,     4,     5,    36,    38,    18,     6,    31,
      32,    33,     4,    34,    15,    37,    39,     6,    16,    28,
      28,    20,    29,    40,    17,   -27,   -19,   -28,   -29,    21,
      35,    22,    26,    23,    24,    27,    30,    41,    42,     0,
      43,    44,    45,    46,    25
};

static const yysigned_char yycheck[] =
{
       3,     4,     5,     6,     7,    23,    24,     7,    11,     3,
       4,     5,     6,     7,     7,    23,    24,    11,    11,     9,
       9,     0,    12,    12,     8,    10,    10,    10,    10,     9,
      22,     9,     7,    10,    10,     8,     7,     7,     7,    -1,
       8,     8,     7,     7,    16
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,     3,     4,     5,     6,     7,    11,    14,    15,    16,
      17,    18,    19,    20,    22,     7,    11,     8,     7,    21,
       0,     9,     9,    10,    10,    21,     7,     8,     9,    12,
       7,     3,     4,     5,     7,    19,    20,    22,    20,    22,
      12,     7,     7,     8,     8,     7,     7
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)		\
   ((Current).first_line   = (Rhs)[1].first_line,	\
    (Current).first_column = (Rhs)[1].first_column,	\
    (Current).last_line    = (Rhs)[N].last_line,	\
    (Current).last_column  = (Rhs)[N].last_column)
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if defined (YYMAXDEPTH) && YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 83 "tokens.y"
    { set_copy(return_value, &yyvsp[0].cpu_set); }
    break;

  case 3:
#line 85 "tokens.y"
    { set_copy(return_value, &yyvsp[0].cpu_set); }
    break;

  case 4:
#line 88 "tokens.y"
    { set_copy(&yyval.cpu_set, &yyvsp[0].cpu_set); }
    break;

  case 5:
#line 90 "tokens.y"
    { set_copy(&yyval.cpu_set, &yyvsp[0].cpu_set); }
    break;

  case 6:
#line 93 "tokens.y"
    { cpu_set(&yyval.cpu_set, yyvsp[0].number); }
    break;

  case 7:
#line 95 "tokens.y"
    { cpu_set_range(&yyval.cpu_set, yyvsp[-2].number, yyvsp[0].number); }
    break;

  case 8:
#line 97 "tokens.y"
    { PLPA_NAME(cpu_set_t) temp; cpu_set(&temp, yyvsp[0].number); set_union(&yyval.cpu_set, &yyvsp[-2].cpu_set, &temp); }
    break;

  case 9:
#line 99 "tokens.y"
    { PLPA_NAME(cpu_set_t) temp; cpu_set_range(&temp, yyvsp[-2].number, yyvsp[0].number); set_union(&yyval.cpu_set, &yyvsp[-4].cpu_set, &temp); }
    break;

  case 10:
#line 102 "tokens.y"
    { cpu_set_all(&yyval.cpu_set, PROCESSOR); }
    break;

  case 11:
#line 104 "tokens.y"
    { cpu_set_even(&yyval.cpu_set, PROCESSOR); }
    break;

  case 12:
#line 106 "tokens.y"
    { cpu_set_odd(&yyval.cpu_set, PROCESSOR); }
    break;

  case 13:
#line 109 "tokens.y"
    { set_copy(&yyval.cpu_set, &yyvsp[0].cpu_set); }
    break;

  case 14:
#line 111 "tokens.y"
    { set_union(&yyval.cpu_set, &yyvsp[-2].cpu_set, &yyvsp[0].cpu_set); }
    break;

  case 15:
#line 114 "tokens.y"
    { sc_merge(&yyval.cpu_set, &yyvsp[-2].cpu_set, 1, &yyvsp[0].cpu_set, 1); }
    break;

  case 16:
#line 116 "tokens.y"
    { sc_merge(&yyval.cpu_set, &yyvsp[-2].cpu_set, 1, &yyvsp[0].cpu_set, 0); }
    break;

  case 17:
#line 118 "tokens.y"
    { sc_merge(&yyval.cpu_set, &yyvsp[-2].cpu_set, 0, &yyvsp[0].cpu_set, 1); }
    break;

  case 18:
#line 120 "tokens.y"
    { sc_merge(&yyval.cpu_set, &yyvsp[-2].cpu_set, 0, &yyvsp[0].cpu_set, 0); }
    break;

  case 19:
#line 123 "tokens.y"
    { cpu_set(&yyval.cpu_set, yyvsp[0].number); }
    break;

  case 20:
#line 125 "tokens.y"
    { PLPA_NAME(cpu_set_t) temp; cpu_set(&temp, yyvsp[0].number); cpu_compliment(&yyval.cpu_set, &temp); }
    break;

  case 21:
#line 127 "tokens.y"
    { cpu_compliment(&yyval.cpu_set, &yyvsp[-1].cpu_set); }
    break;

  case 22:
#line 129 "tokens.y"
    { set_copy(&yyval.cpu_set, &yyvsp[-1].cpu_set); }
    break;

  case 23:
#line 132 "tokens.y"
    { cpu_set(&yyval.cpu_set, yyvsp[0].number); }
    break;

  case 24:
#line 134 "tokens.y"
    { cpu_set_range(&yyval.cpu_set, yyvsp[-2].number, yyvsp[0].number); }
    break;

  case 25:
#line 136 "tokens.y"
    { PLPA_NAME(cpu_set_t) temp; cpu_set(&temp, yyvsp[0].number); set_union(&yyval.cpu_set, &yyvsp[-2].cpu_set, &temp); }
    break;

  case 26:
#line 138 "tokens.y"
    { PLPA_NAME(cpu_set_t) temp; cpu_set_range(&temp, yyvsp[-2].number, yyvsp[0].number); set_union(&yyval.cpu_set, &yyvsp[-4].cpu_set, &temp); }
    break;

  case 27:
#line 141 "tokens.y"
    { cpu_set_all(&yyval.cpu_set, ALL); }
    break;

  case 28:
#line 143 "tokens.y"
    { cpu_set_even(&yyval.cpu_set, ALL); }
    break;

  case 29:
#line 145 "tokens.y"
    { cpu_set_odd(&yyval.cpu_set, ALL); }
    break;


    }

/* Line 1000 of yacc.c.  */
#line 1215 "tokens.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  const char* yyprefix;
	  char *yymsg;
	  int yyx;

	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  int yyxbegin = yyn < 0 ? -yyn : 0;

	  /* Stay within bounds of both yycheck and yytname.  */
	  int yychecklim = YYLAST - yyn;
	  int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
	  int yycount = 0;

	  yyprefix = ", expecting ";
	  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      {
		yysize += yystrlen (yyprefix) + yystrlen (yytname [yyx]);
		yycount += 1;
		if (yycount == 5)
		  {
		    yysize = 0;
		    break;
		  }
	      }
	  yysize += (sizeof ("syntax error, unexpected ")
		     + yystrlen (yytname[yytype]));
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yyprefix = ", expecting ";
		  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			yyp = yystpcpy (yyp, yyprefix);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yyprefix = " or ";
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* If at end of input, pop the error token,
	     then the rest of the stack, then return failure.  */
	  if (yychar == YYEOF)
	     for (;;)
	       {
		 YYPOPSTACK;
		 if (yyssp == yyss)
		   YYABORT;
		 YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
		 yydestruct (yystos[*yyssp], yyvsp);
	       }
        }
      else
	{
	  YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
	  yydestruct (yytoken, &yylval);
	  yychar = YYEMPTY;

	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

#ifdef __GNUC__
  /* Pacify GCC when the user code never invokes YYERROR and the label
     yyerrorlab therefore never appears in user code.  */
  if (0)
     goto yyerrorlab;
#endif

  yyvsp -= yylen;
  yyssp -= yylen;
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      YYPOPSTACK;
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 147 "tokens.y"


int token_parse(PLPA_NAME(cpu_set_t) *cpu_set)
{
    int ret;

    PLPA_CPU_ZERO(cpu_set);
    return_value = cpu_set;
    ret = yyparse();
    if (0 != ret) {
        return ret;
    }
    return 0;
}

void yyerror (char const *s)
{
    fprintf(stderr, "ERROR: %s\n", s);
}

static void set_union(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *a, 
                      PLPA_NAME(cpu_set_t) *b)
{
    int i;
    PLPA_CPU_ZERO(out);
    for (i = 0; i < PLPA_BITMASK_CPU_MAX; ++i) {
        if (PLPA_CPU_ISSET(i, a) || PLPA_CPU_ISSET(i, b)) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void set_copy(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *in)
{
    int i;
    for (i = 0; i < PLPA_BITMASK_CPU_MAX; ++i) {
        if (PLPA_CPU_ISSET(i, in)) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void cpu_set(PLPA_NAME(cpu_set_t) *out, int pos)
{
    PLPA_CPU_ZERO(out);
    if (pos < PLPA_BITMASK_CPU_MAX) {
        PLPA_CPU_SET(pos, out);
    }
}

static void cpu_set_all(PLPA_NAME(cpu_set_t) *out, id_type_t type)
{
    int i, max_num, max_id, exists, online;
    PLPA_CPU_ZERO(out);

    /* Only set processor ID's that exist and are online */
    if (ALL == type) {
        max_id = PLPA_BITMASK_CPU_MAX;
    } else if (PROCESSOR == type) {
        PLPA_NAME(get_processor_data)(PLPA_NAME_CAPS(COUNT_ONLINE),
                                      &max_num, &max_id);
    }

    for (i = 0; i <= max_id; ++i) {
        if (0 == PLPA_NAME(get_processor_flags)(i, &exists, &online) &&
            exists && online) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void cpu_set_even(PLPA_NAME(cpu_set_t) *out, id_type_t type)
{
    int i, max_num, max_id, exists, online;
    PLPA_CPU_ZERO(out);

    /* Only set processor ID's that exist and are online */
    if (ALL == type) {
        max_id = PLPA_BITMASK_CPU_MAX;
    } else if (PROCESSOR == type) {
        PLPA_NAME(get_processor_data)(PLPA_NAME_CAPS(COUNT_ONLINE),
                                      &max_num, &max_id);
    }

    for (i = 0; i <= max_id; i += 2) {
        if (0 == PLPA_NAME(get_processor_flags)(i, &exists, &online) &&
            exists && online) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void cpu_set_odd(PLPA_NAME(cpu_set_t) *out, id_type_t type)
{
    int i, max_num, max_id, exists, online;
    PLPA_CPU_ZERO(out);

    /* Only set processor ID's that exist */
    if (ALL == type) {
        max_id = PLPA_BITMASK_CPU_MAX;
    } else if (PROCESSOR == type) {
        PLPA_NAME(get_processor_data)(PLPA_NAME_CAPS(COUNT_ONLINE),
                                      &max_num, &max_id);
    }

    for (i = 1; i <= max_id; i += 2) {
        if (0 == PLPA_NAME(get_processor_flags)(i, &exists, &online) &&
            exists && online) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void cpu_set_range(PLPA_NAME(cpu_set_t) *out, int min, int max)
{
    int i;
    PLPA_CPU_ZERO(out);
    for (i = min; i <= max && i < PLPA_BITMASK_CPU_MAX; ++i) {
        PLPA_CPU_SET(i, out);
    }
}

static void cpu_compliment(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *in)
{
    int i;
    PLPA_CPU_ZERO(out);
    for (i = 0; i < PLPA_BITMASK_CPU_MAX; ++i) {
        if (!PLPA_CPU_ISSET(i, in)) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void sc_merge(PLPA_NAME(cpu_set_t) *out, 
                     PLPA_NAME(cpu_set_t) *cores, int cores_are_valid,
                     PLPA_NAME(cpu_set_t) *sockets, int sockets_are_valid)
{
    int ret, i, core, socket, id, have_topo;
    int num_sockets, max_socket_id;
    int num_cores, max_core_id;

    /* This is just about the only function that's tricky.  Take a
       bitmask representing all the cores and a bitmask representing
       all the sockets and merge them into a single cpu_set_t
       representing real CPU id's using the plpa_map_to_processor_id()
       function.  But error out if this system doesn't support the
       topology information (because we won't be able to do the
       mapping). */

    PLPA_CPU_ZERO(out);
    if (0 != PLPA_NAME(have_topology_information)(&have_topo) ||
        0 == have_topo) {
        fprintf(stderr, "ERROR: This system does not support topology information\n");
        exit(1);
    }

    if (0 != PLPA_NAME(get_socket_info)(&num_sockets, &max_socket_id)) {
        fprintf(stderr, "ERROR: Unable to retrieve socket information\n");
        exit(1);
    }

    /* Even though I officially don't care about performance here,
       intentionally putting in a loop that is
       O(PLPA_BITMASK_CPU_MAX^2) gives me pause.  :-) So scan through
       the sockets array once and generate a list of the set bits in a
       much shorter array. */

    for (i = socket = 0; socket < PLPA_BITMASK_CPU_MAX; ++socket) {
        if (sockets_are_valid && socket > max_socket_id &&
            PLPA_CPU_ISSET(socket, sockets)) {
            fprintf(stderr, "ERROR: Invalid socket ID specified (%d; max socket ID is %d)\n",
                    socket, max_socket_id);
            exit(1);
        } else if (sockets_are_valid && 
                   ENOENT == PLPA_NAME(get_core_info)(socket, &num_cores,
                                                      &max_core_id) &&
                   PLPA_CPU_ISSET(socket, sockets)) {
            fprintf(stderr, "ERROR: Invalid socket ID specified (%d does not exist)\n",
                    socket);
            exit(1);
        } else if (PLPA_CPU_ISSET(socket, sockets)) {
            socket_list[i++] = socket;
        }
    }

    /* Bozo case: if there were no sockets set, we're done */

    if (0 == i) {
        return;
    }

    /* Otherwise, do the loop to create the mapping of sockets and
       cores, seting the final bitmask.  Yes, this is a double loop,
       but hopefully it's much smaller than
       PLPA_BITMASK_CPU_MAX^2. */

    for (core = 0; core < PLPA_BITMASK_CPU_MAX; ++core) {
        if (PLPA_CPU_ISSET(core, cores)) {
            for (socket = 0; socket < i; ++socket) {
                ret = PLPA_NAME(map_to_processor_id)(socket_list[socket],
                                                     core, &id);
                if (ENOENT == ret) {
                    if (cores_are_valid) {
                        fprintf(stderr, "ERROR: Invalid core@socket tuple (%d@%d does not exist)\n",
                                core, socket_list[socket]);
                        exit(1);
                    } else {
                        continue;
                    }
                } else if (0 != ret) {
                    fprintf(stderr, "ERROR: Failed to map %d@%d to processor ID\n",
                            core, socket);
                    exit(1);
                }
                PLPA_CPU_SET(id, out);
            }
        }
    }
}

