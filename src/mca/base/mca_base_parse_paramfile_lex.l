%{ /* -*- C -*- */

#include <stdio.h>
#include <unistd.h>

#include "mca/base/mca_base_parse_paramfile_lex.h"

/*
 * local functions
 */
static int mca_base_yyerror(void);
static int mca_base_yywrap(void);

/*
 * global variables
 */
int mca_base_yynewlines = 1;
bool mca_base_parse_done = false;
char *mca_base_string = NULL;

%}

WHITE       [\f\t\v ]
CHAR        [A-Za-z0-9_\-\.]

%x VALUE

%%

{WHITE}*\n          { mca_base_yynewlines++; return MCA_BASE_PARSE_NEWLINE; }
#.*\n               { mca_base_yynewlines++; return MCA_BASE_PARSE_NEWLINE; }

{WHITE}*"="{WHITE}* { BEGIN(VALUE); return MCA_BASE_PARSE_EQUAL; }
{WHITE}+            ; /* whitespace */
{CHAR}+             { return MCA_BASE_PARSE_SINGLE_WORD; }

<VALUE>{WHITE}*\n   { BEGIN(INITIAL); }
<VALUE>[^\n]*[^\t \n]/[\t ]*\n      { return MCA_BASE_PARSE_VALUE; }

.	            { return MCA_BASE_PARSE_ERROR; }

%%


static int mca_base_yyerror(void)
{
    printf("%d: Invalid input (%s)\n", mca_base_yynewlines, mca_base_yytext);
    return MCA_BASE_PARSE_ERROR;
}


static int mca_base_yywrap(void)
{
    mca_base_parse_done = true;
    return 1;
}
