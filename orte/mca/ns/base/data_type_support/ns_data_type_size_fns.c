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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/ns_private.h"

/*
 * STANDARD SIZE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_ns_base_std_size(size_t *size, void *src, orte_data_type_t type)
{
    switch(type) {
        case ORTE_VPID:
            *size = sizeof(orte_vpid_t);
            break;

        case ORTE_JOBID:
            *size = sizeof(orte_jobid_t);
            break;

        case ORTE_CELLID:
            *size = sizeof(orte_cellid_t);
            break;

        case ORTE_NODEID:
            *size = sizeof(orte_nodeid_t);
            break;
            
        case ORTE_NAME:
            *size = sizeof(orte_process_name_t);
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
            return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    return ORTE_SUCCESS;
}
