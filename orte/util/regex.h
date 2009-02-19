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

BEGIN_C_DECLS

ORTE_DECLSPEC int orte_regex_extract_node_names(char *regexp, char ***names);

ORTE_DECLSPEC int orte_regex_extract_ppn(int num_nodes, char *regexp, int **ppn);

END_C_DECLS
#endif
