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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_RDS_HOSTFILE_LEX_H_
#define ORTE_RDS_HOSTFILE_LEX_H_

#include "orte_config.h"

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

typedef union {
    int ival;
    char* sval;
} orte_rds_value_t;

extern int   orte_rds_hostfile_lex(void);
extern FILE *orte_rds_hostfile_in;
extern int   orte_rds_hostfile_line;
extern bool  orte_rds_hostfile_done;
extern orte_rds_value_t  orte_rds_hostfile_value;

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED 0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE 0
#define YY_MAIN 0
#define YY_NO_UNPUT 1
#define YY_SKIP_YYWRAP 1

#define ORTE_RDS_HOSTFILE_DONE           0
#define ORTE_RDS_HOSTFILE_ERROR          1
#define ORTE_RDS_HOSTFILE_QUOTED_STRING  2
#define ORTE_RDS_HOSTFILE_EQUAL          3
#define ORTE_RDS_HOSTFILE_INT            4
#define ORTE_RDS_HOSTFILE_STRING         5
#define ORTE_RDS_HOSTFILE_CPU            6
#define ORTE_RDS_HOSTFILE_COUNT          7
#define ORTE_RDS_HOSTFILE_SLOTS          8
#define ORTE_RDS_HOSTFILE_SLOTS_MAX      9
#define ORTE_RDS_HOSTFILE_USERNAME       10
#define ORTE_RDS_HOSTFILE_IPV4           11
#define ORTE_RDS_HOSTFILE_HOSTNAME       12
#define ORTE_RDS_HOSTFILE_NEWLINE        13

#endif
