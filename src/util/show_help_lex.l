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
#ifdef __cplusplus
 extern "C" {
#endif
     static int ompi_show_help_yywrap(void);
#ifdef __cplusplus
 }
#endif

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
#if defined(YY_CURRENT_BUFFER_LVALUE)
        YY_CURRENT_BUFFER_LVALUE = NULL;
#else
        YY_CURRENT_BUFFER = NULL;
#endif  /* YY_CURRENT_BUFFER_LVALUE */
    }
    return YY_NULL;
}


static int ompi_show_help_yywrap(void)
{
    ompi_show_help_parse_done = true;
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
int ompi_show_help_init_buffer(FILE *file)
{
    YY_BUFFER_STATE buf = yy_create_buffer(file, YY_BUF_SIZE);
    yy_switch_to_buffer(buf);

    return 0;
}
