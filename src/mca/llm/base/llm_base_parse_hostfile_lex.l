%{ /* -*- C -*- */

#include <stdio.h>
#include "mca/llm/base/llm_base_parse_hostfile_lex.h"

/*
 * local functions
 */
static int yyerror(void);
int yywrap(void);

/*
 * global variables
 */
int yynewlines=1;
int mca_llm_base_parse_done = 0;
char *mca_llm_base_string = NULL;

%}

%%

\n                 { yynewlines++; return MCA_LLM_BASE_NEWLINE; }

\"[^\"]*\"        { mca_llm_base_string = yytext; return MCA_LLM_BASE_QUOTED_STRING; }

[\f\t\v ]          ;			/* whitespace */

"="                { return MCA_LLM_BASE_EQUAL; }

cpu                { mca_llm_base_string = yytext; return MCA_LLM_BASE_COUNT; }
count              { mca_llm_base_string = yytext; return MCA_LLM_BASE_COUNT; }

[A-Za-z0-9_\-\.]*  { mca_llm_base_string = yytext; return MCA_LLM_BASE_STRING; }

.	{ mca_llm_base_string = yytext; return MCA_LLM_BASE_ERROR; }

%%


static
int
yyerror()
{
    printf("%d: Invalid input (%s)\n", yynewlines, yytext);
    return MCA_LLM_BASE_ERROR; 
}


int
yywrap()
{
    mca_llm_base_parse_done = 1;
    return 1;
}
