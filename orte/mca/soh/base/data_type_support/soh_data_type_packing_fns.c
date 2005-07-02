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

#include "mca/errmgr/errmgr.h"
#include "dps/dps_internal.h"

#include "mca/soh/base/base.h"

/*
 * EXIT CODE
 */
int orte_soh_base_pack_exit_code(orte_buffer_t *buffer, void *src,
                                 size_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, src, num_vals, DPS_TYPE_INT))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * NODE STATE
 */
int orte_soh_base_pack_node_state(orte_buffer_t *buffer, void *src,
                                  size_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, src, num_vals, ORTE_INT8))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * PROC STATE
 */
int orte_soh_base_pack_proc_state(orte_buffer_t *buffer, void *src,
                                  size_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, src, num_vals, ORTE_INT8))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}
