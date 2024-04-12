/* -*- C -*-
 *
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
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_UTIL_KEYVAL_LEX_H_
#define PMIX_UTIL_KEYVAL_LEX_H_

#include "src/include/pmix_config.h"

#ifdef malloc
#    undef malloc
#endif
#ifdef realloc
#    undef realloc
#endif
#ifdef free
#    undef free
#endif

#include <stdio.h>

#define PMIX_UTIL_KEYVAL_PARSE_DONE         0
#define PMIX_UTIL_KEYVAL_PARSE_ERROR        1
#define PMIX_UTIL_KEYVAL_PARSE_NEWLINE      2
#define PMIX_UTIL_KEYVAL_PARSE_EQUAL        3
#define PMIX_UTIL_KEYVAL_PARSE_SINGLE_WORD  4
#define PMIX_UTIL_KEYVAL_PARSE_VALUE        5
#define PMIX_UTIL_KEYVAL_PARSE_MCAVAR       6
#define PMIX_UTIL_KEYVAL_PARSE_ENVVAR       7
#define PMIX_UTIL_KEYVAL_PARSE_ENVEQL       8
#define PMIX_UTIL_KEYVAL_PARSE_MAX          9

int pmix_util_keyval_yylex(void);
int pmix_util_keyval_init_buffer(FILE *file);
int pmix_util_keyval_yylex_destroy(void);

extern FILE *pmix_util_keyval_yyin;
extern bool pmix_util_keyval_parse_done;
extern char *pmix_util_keyval_yytext;
extern int pmix_util_keyval_yynewlines;
extern int pmix_util_keyval_yylineno;

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED         0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE  0
#define YY_MAIN               0
#define YY_NO_UNPUT           1
#define YY_SKIP_YYWRAP        1

#endif
