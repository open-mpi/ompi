%{ /* -*- C -*- */

#include "ompi_config.h"

#include <stdio.h>
#include <unistd.h>

#include "util/show_help_lex.h"

/*
 * public functions
 */
int ompi_show_help_finish_parsing(void);


/*
 * local functions
 */
static int ompi_show_help_yywrap(void);

/*
 * global variables
 */
int ompi_show_help_yynewlines = 1;
bool ompi_show_help_parse_done = false;
char *ompi_show_help_string = NULL;

#define yyterminate() \
  return ompi_show_help_finish_parsing()

%}

WHITE       [\f\t\v ]
CHAR        [A-Za-z0-9_\-\.]

%x CHOMP

%%

#.*\n               ; /* comment line */

^\[.+\]/.*\n { BEGIN(CHOMP); return OMPI_SHOW_HELP_PARSE_TOPIC; }

<CHOMP>.*\n { BEGIN(INITIAL); }

.*/\n { BEGIN(CHOMP); return OMPI_SHOW_HELP_PARSE_MESSAGE; }

%%


/*
 * This cleans up at the end of the parse (since, in this case, we
 * always parse the entire file) and prevents a memory leak.
 */
int ompi_show_help_finish_parsing(void) 
{
    if (NULL != YY_CURRENT_BUFFER) {
        yy_delete_buffer(YY_CURRENT_BUFFER); 
        YY_CURRENT_BUFFER_LVALUE = NULL;
    }
    return YY_NULL;
}

static int ompi_show_help_yywrap(void)
{
    ompi_show_help_parse_done = true;
    return 1;
}
