%{ /* -*- C -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#include <stdio.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "mca/llm/base/llm_base_parse_hostfile_lex.h"

/*
 * local functions
 */
static int mca_llm_base_yyerror(void);
static int mca_llm_base_yywrap(void);

/*
 * global variables
 */
int mca_llm_base_yynewlines=1;
bool mca_llm_base_parse_done = false;
char *mca_llm_base_string = NULL;

%}

%%

\n                 { mca_llm_base_yynewlines++; return MCA_LLM_BASE_NEWLINE; }

\"[^\"]*\"         { mca_llm_base_string = yytext; return MCA_LLM_BASE_QUOTED_STRING; }

[\f\t\v ]          ;			/* whitespace */

"="                { return MCA_LLM_BASE_EQUAL; }

cpu                { mca_llm_base_string = yytext; return MCA_LLM_BASE_COUNT; }
count              { mca_llm_base_string = yytext; return MCA_LLM_BASE_COUNT; }

[A-Za-z0-9_\-\.]*  { mca_llm_base_string = yytext; return MCA_LLM_BASE_STRING; }

.	           { mca_llm_base_string = yytext; return MCA_LLM_BASE_ERROR; }

%%


static int mca_llm_base_yyerror(void)
{
    printf("%d: Invalid input (%s)\n", mca_llm_base_yynewlines, yytext);
    return MCA_LLM_BASE_ERROR; 
}


static int mca_llm_base_yywrap(void)
{
    mca_llm_base_parse_done = true;
    return 1;
}
