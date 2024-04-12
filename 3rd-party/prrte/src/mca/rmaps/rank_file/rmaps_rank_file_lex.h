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
 * Copyright (c) 2008      Voltaire. All rights reserved
 *
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_MCA_RANK_FILE_RANKFILE_LEX_H_
#define PRTE_MCA_RANK_FILE_RANKFILE_LEX_H_
#include "prte_config.h"

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

BEGIN_C_DECLS

typedef union {
    int ival;
    char *sval;
} prte_rmaps_rank_file_value_t;

extern int prte_rmaps_rank_file_lex(void);
extern FILE *prte_rmaps_rank_file_in;
extern int prte_rmaps_rank_file_line;
extern bool prte_rmaps_rank_file_done;
extern prte_rmaps_rank_file_value_t prte_rmaps_rank_file_value;

int prte_rmaps_rank_file_wrap(void);

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED         0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE  0
#define YY_MAIN               0
#define YY_NO_UNPUT           1
#define YY_SKIP_YYWRAP        1

#define PRTE_RANKFILE_DONE          0
#define PRTE_RANKFILE_ERROR         1
#define PRTE_RANKFILE_QUOTED_STRING 2
#define PRTE_RANKFILE_EQUAL         3
#define PRTE_RANKFILE_INT           4
#define PRTE_RANKFILE_STRING        5
#define PRTE_RANKFILE_RANK          6
#define PRTE_RANKFILE_USERNAME      10
#define PRTE_RANKFILE_IPV4          11
#define PRTE_RANKFILE_HOSTNAME      12
#define PRTE_RANKFILE_NEWLINE       13
#define PRTE_RANKFILE_IPV6          14
#define PRTE_RANKFILE_SLOT          15
#define PRTE_RANKFILE_RELATIVE      16

END_C_DECLS

#endif
