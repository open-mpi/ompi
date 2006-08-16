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

#include "orte_config.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/smr/base/smr_private.h"

/*
 * EXIT CODE
 */
int orte_smr_base_pack_exit_code(orte_buffer_t *buffer, void *src,
                                 orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

/*
 * NODE STATE
 */
int orte_smr_base_pack_node_state(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_INT8))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

/*
 * PROC STATE
 */
int orte_smr_base_pack_proc_state(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_PROC_STATE_T))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

/*
 * JOB STATE
 */
int orte_smr_base_pack_job_state(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_JOB_STATE_T))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}
