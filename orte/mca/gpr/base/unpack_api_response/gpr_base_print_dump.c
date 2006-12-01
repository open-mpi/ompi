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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"
#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "opal/util/output.h"

#include "orte/mca/gpr/base/base.h"

int orte_gpr_base_print_dump(orte_buffer_t *buffer)
{
    char *line;
    orte_std_cntr_t n;

    n = 1;
    while (ORTE_SUCCESS == orte_dss.unpack(buffer, &line, &n, ORTE_STRING)) {
       opal_output(orte_gpr_base_output, "%s", line);
       free(line);
       n=1;
    }

    return ORTE_SUCCESS;
}
