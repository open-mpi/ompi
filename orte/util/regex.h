/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

/** @file:
 *
 */

#ifndef _ORTE_REGEX_H_
#define _ORTE_REGEX_H_

#include "orte_config.h"

#include "opal/class/opal_value_array.h"
#include "opal/class/opal_list.h"

#include "orte/mca/odls/odls_types.h"
#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

typedef struct {
    opal_list_item_t super;
    int start;
    int cnt;
} orte_regex_range_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_regex_range_t);

typedef struct {
    /* list object */
    opal_list_item_t super;
    char *prefix;
    char *suffix;
    int num_digits;
    opal_list_t ranges;
} orte_regex_node_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_regex_node_t);

/* NOTE: this is a destructive call for the nodes param - the
 * function will search and replace all commas with '\0'
 */
ORTE_DECLSPEC int orte_regex_create(char *nodes, char **regexp);

ORTE_DECLSPEC int orte_regex_extract_node_names(char *regexp, char ***names);

ORTE_DECLSPEC int orte_regex_extract_ppn(int num_nodes, char *regexp, int **ppn);

END_C_DECLS
#endif
