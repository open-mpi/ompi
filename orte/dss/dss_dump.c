/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "orte_config.h"
#include "orte/orte_types.h"

#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss.h"
#include "orte/dss/dss_internal.h"


int orte_dss_dump(int output_stream, void *src, orte_data_type_t type)
{
    char *sptr;
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss.print(&sptr, NULL, src, type))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    opal_output(output_stream, "%s", sptr);
    free(sptr);

    return ORTE_SUCCESS;
}


void orte_dss_dump_data_types(int output)
{
    orte_dss_type_info_t **ptr;
    orte_data_type_t j;
    orte_std_cntr_t i;

    opal_output(output, "DUMP OF REGISTERED DATA TYPES");

    ptr = (orte_dss_type_info_t**)(orte_dss_types->addr);
    for (i=0, j=0; j < orte_dss_num_reg_types &&
                   i < orte_dss_types->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            /* print out the info */
            opal_output(output, "\tIndex: %lu\tData type: %lu\tName: %s",
                        (unsigned long)j,
                        (unsigned long)ptr[i]->odti_type,
                        ptr[i]->odti_name);
        }
    }
}

