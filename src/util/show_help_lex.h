/*
 * $HEADER$
 */

#ifndef OMPI_SHOW_HELP_LEX_H
#define OMPI_SHOW_HELP_LEX_H

#include "ompi_config.h"

#ifdef malloc
#undef malloc
#endif
#ifdef realloc
#undef realloc
#endif
#ifdef free
#undef free
#endif

#include <stdio.h>

int ompi_show_help_yylex(void);
int ompi_show_help_init_buffer(FILE *file);

extern FILE *ompi_show_help_yyin;
extern bool ompi_show_help_parse_done;
extern char *ompi_show_help_yytext;
extern int ompi_show_help_yynewlines;

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED 0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE 0
#define YY_MAIN 0
#define YY_NO_UNPUT 1

enum {
    OMPI_SHOW_HELP_PARSE_DONE,
    OMPI_SHOW_HELP_PARSE_ERROR,

    OMPI_SHOW_HELP_PARSE_TOPIC,
    OMPI_SHOW_HELP_PARSE_MESSAGE,

    OMPI_SHOW_HELP_PARSE_MAX
};

#endif
