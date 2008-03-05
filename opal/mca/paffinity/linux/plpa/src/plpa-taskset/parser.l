%{ /* -*- C -*- */
/*
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "plpa.h"

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "plpa-taskset.h"
#include "tokens.h"

/*
 * local functions
 */
static int finish_parsing(void);

#define yyterminate() \
  return finish_parsing()

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED 0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE 0
#define YY_MAIN 0
#define YY_NO_UNPUT 1

%}

%option noyywrap

%%

[0-9]* { yylval.number = atoi(yytext); return TOKENS_NUMBER; }

"all" { return TOKENS_STRING_ALL; }

"even" { return TOKENS_STRING_EVEN; }

"odd" { return TOKENS_STRING_ODD; }

"not" { return TOKENS_STRING_NOT; }

. { return *yytext; }

%%


/*
 * This cleans up at the end of the parse (since, in this case, we
 * always parse the entire file) and prevents a memory leak.
 */
static int finish_parsing(void) 
{
    if (NULL != YY_CURRENT_BUFFER) {
        yy_delete_buffer(YY_CURRENT_BUFFER); 
#if defined(YY_CURRENT_BUFFER_LVALUE)
        YY_CURRENT_BUFFER_LVALUE = NULL;
#else
        YY_CURRENT_BUFFER = NULL;
#endif  /* YY_CURRENT_BUFFER_LVALUE */
    }
    return YY_NULL;
}

void parser_setup_string(char *str)
{
    yy_scan_string(str);
}
