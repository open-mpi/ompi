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
 * Copyright (c) 2016-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_UTIL_HOSTFILE_LEX_H_
#define PRTE_UTIL_HOSTFILE_LEX_H_

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

typedef union {
    int ival;
    char *sval;
} prte_hostfile_value_t;

extern int prte_util_hostfile_lex(void);
extern FILE *prte_util_hostfile_in;
extern int prte_util_hostfile_line;
extern bool prte_util_hostfile_done;
extern prte_hostfile_value_t prte_util_hostfile_value;
extern int prte_util_hostfile_lex_destroy(void);

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED         0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE  0
#define YY_MAIN               0
#define YY_NO_UNPUT           1
#define YY_SKIP_YYWRAP        1

#define PRTE_HOSTFILE_DONE              0
#define PRTE_HOSTFILE_ERROR             1
#define PRTE_HOSTFILE_QUOTED_STRING     2
#define PRTE_HOSTFILE_EQUAL             3
#define PRTE_HOSTFILE_INT               4
#define PRTE_HOSTFILE_STRING            5
#define PRTE_HOSTFILE_CPU               6
#define PRTE_HOSTFILE_COUNT             7
#define PRTE_HOSTFILE_SLOTS             8
#define PRTE_HOSTFILE_SLOTS_MAX         9
#define PRTE_HOSTFILE_USERNAME          10
#define PRTE_HOSTFILE_IPV4              11
#define PRTE_HOSTFILE_HOSTNAME          12
#define PRTE_HOSTFILE_NEWLINE           13
#define PRTE_HOSTFILE_IPV6              14
#define PRTE_HOSTFILE_SLOT              15
#define PRTE_HOSTFILE_RELATIVE          16
#define PRTE_HOSTFILE_BOARDS            17
#define PRTE_HOSTFILE_SOCKETS_PER_BOARD 18
#define PRTE_HOSTFILE_CORES_PER_SOCKET  19
/* ensure we can handle a rank_file input */
#define PRTE_HOSTFILE_RANK 20
#define PRTE_HOSTFILE_PORT 21

#endif
