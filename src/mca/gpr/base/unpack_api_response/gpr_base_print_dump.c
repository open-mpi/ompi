/*
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
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "dps/dps.h"
#include "util/output.h"

#include "mca/gpr/base/base.h"

int orte_gpr_base_print_dump(orte_buffer_t *buffer, int output_id)
{
    char *line;
    size_t n;

    n = 1;
    while (ORTE_SUCCESS == orte_dps.unpack(buffer, &line, &n, ORTE_STRING)) {
	   ompi_output(output_id, "%s", line);
	   free(line);
       n=1;
    }

    return ORTE_SUCCESS;
}
