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

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/smr/base/smr_private.h"

/*
 * COPY FOR NON-COMPLEX FUNCTIONS
 */
int orte_smr_base_copy_proc_state(orte_proc_state_t **dest, orte_proc_state_t *src, orte_data_type_t type)
{
    orte_proc_state_t *ps;

    ps = (orte_proc_state_t*)malloc(sizeof(orte_proc_state_t));
    if (NULL == ps) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    *ps = *src;
    *dest = ps;

    return ORTE_SUCCESS;
}

int orte_smr_base_copy_job_state(orte_job_state_t **dest, orte_job_state_t *src, orte_data_type_t type)
{
    orte_job_state_t *ps;

    ps = (orte_job_state_t*)malloc(sizeof(orte_job_state_t));
    if (NULL == ps) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    *ps = *src;
    *dest = ps;

    return ORTE_SUCCESS;
}

int orte_smr_base_copy_node_state(orte_node_state_t **dest, orte_node_state_t *src, orte_data_type_t type)
{
    orte_node_state_t *ps;

    ps = (orte_node_state_t*)malloc(sizeof(orte_node_state_t));
    if (NULL == ps) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    *ps = *src;
    *dest = ps;

    return ORTE_SUCCESS;
}

int orte_smr_base_copy_exit_code(orte_exit_code_t **dest, orte_exit_code_t *src, orte_data_type_t type)
{
    orte_exit_code_t *ps;

    ps = (orte_exit_code_t*)malloc(sizeof(orte_exit_code_t));
    if (NULL == ps) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    *ps = *src;
    *dest = ps;

    return ORTE_SUCCESS;
}
