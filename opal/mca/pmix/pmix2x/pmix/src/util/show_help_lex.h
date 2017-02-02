/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_SHOW_HELP_LEX_H
#define PMIX_SHOW_HELP_LEX_H

#include <src/include/pmix_config.h>

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
BEGIN_C_DECLS
int pmix_show_help_yylex(void);
int pmix_show_help_init_buffer(FILE *file);
int pmix_show_help_yylex_destroy(void);

extern FILE *pmix_show_help_yyin;
extern bool pmix_show_help_parse_done;
extern char *pmix_show_help_yytext;
extern int pmix_show_help_yynewlines;

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED 0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE 0
#define YY_MAIN 0
#define YY_NO_UNPUT 1
#define YY_SKIP_YYWRAP 1

enum {
    PMIX_SHOW_HELP_PARSE_DONE,
    PMIX_SHOW_HELP_PARSE_ERROR,

    PMIX_SHOW_HELP_PARSE_TOPIC,
    PMIX_SHOW_HELP_PARSE_MESSAGE,

    PMIX_SHOW_HELP_PARSE_MAX
};
END_C_DECLS
#endif
