/* -*- C -*-
 *
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

#ifndef MCA_LLM_BASE_PARSE_HOSTFILE_LEX_H_
#define MCA_LLM_BASE_PARSE_HOSTFILE_LEX_H_

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

extern int mca_llm_base_yylex(void);

extern FILE *mca_llm_base_yyin;
extern bool mca_llm_base_parse_done;
extern char *mca_llm_base_string;
extern int mca_llm_base_yynewlines;

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED 0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE 0
#define YY_MAIN 0
#define YY_NO_UNPUT 1

#define MCA_LLM_BASE_DONE           0
#define MCA_LLM_BASE_ERROR          1
#define MCA_LLM_BASE_QUOTED_STRING  2
#define MCA_LLM_BASE_EQUAL          3
#define MCA_LLM_BASE_STRING         4
#define MCA_LLM_BASE_COUNT          5
#define MCA_LLM_BASE_NEWLINE        6

#endif
