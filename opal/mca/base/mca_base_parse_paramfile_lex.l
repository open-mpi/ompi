%{ /* -*- C -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
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

#include "mca/base/mca_base_parse_paramfile_lex.h"

/*
 * local functions
 */
static int finish_parsing(void) ;
static int mca_base_yywrap(void);

/*
 * global variables
 */
int mca_base_yynewlines = 1;
bool mca_base_parse_done = false;
char *mca_base_string = NULL;

#define yyterminate() \
  return finish_parsing()

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

<VALUE>{WHITE}*\n   { BEGIN(INITIAL); return MCA_BASE_PARSE_NEWLINE; }
<VALUE>[^\n]*[^\t \n]/[\t ]*\n      { return MCA_BASE_PARSE_VALUE; }

.	            { return MCA_BASE_PARSE_ERROR; }

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


static int mca_base_yywrap(void)
{
    mca_base_parse_done = true;
    return 1;
}


/*
 * Ensure that we have a valid yybuffer to use.  Specifically, if this
 * scanner is invoked a second time, finish_parsing() (above) will
 * have been executed, and the current buffer will have been freed.
 * Flex doesn't recognize this fact because as far as it's concerned,
 * its internal state was already initialized, so it thinks it should
 * have a valid buffer.  Hence, here we ensure to give it a valid
 * buffer.
 */
int mca_base_param_init_buffer(FILE *file)
{
    YY_BUFFER_STATE buf = yy_create_buffer(file, YY_BUF_SIZE);
    yy_switch_to_buffer(buf);

    return 0;
}
