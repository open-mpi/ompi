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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */

#include "orte_config.h"

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/mca/mca.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/base.h"
#include "orte/mca/ns/base/ns_private.h"

/*
 * "not available" functions
 */

int
orte_ns_base_dump_cells_not_available(void)
{
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_dump_jobs_not_available(void)
{
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_dump_tags_not_available(void)
{
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_dump_datatypes_not_available(void)
{
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

/****    DIAGNOSTIC FUNCTIONS    ****/
int orte_ns_base_print_dump(orte_buffer_t *buffer)
{
    char *line;
    orte_std_cntr_t n;

    n = 1;
    while (ORTE_SUCCESS == orte_dss.unpack(buffer, &line, &n, ORTE_STRING)) {
       opal_output(mca_ns_base_output, "%s", line);
       free(line);
       n=1;
    }

    return ORTE_SUCCESS;
}

