/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BASE_PARSE_PARAMFILE_LEX_H_
#define MCA_BASE_PARSE_PARAMFILE_LEX_H_

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

int mca_base_yylex(void);
int mca_base_param_init_buffer(FILE *file);

extern FILE *mca_base_yyin;
extern bool mca_base_parse_done;
extern char *mca_base_yytext;
extern int mca_base_yynewlines;

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED 0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE 0
#define YY_MAIN 0
#define YY_NO_UNPUT 1

enum {
    MCA_BASE_PARSE_DONE,
    MCA_BASE_PARSE_ERROR,

    MCA_BASE_PARSE_NEWLINE,
    MCA_BASE_PARSE_EQUAL,
    MCA_BASE_PARSE_SINGLE_WORD,
    MCA_BASE_PARSE_VALUE,

    MCA_BASE_PARSE_MAX
};

#endif
