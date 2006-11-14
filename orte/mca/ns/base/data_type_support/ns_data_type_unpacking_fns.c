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

#include "orte/mca/ns/base/ns_private.h"

/*
 * NAME
 */
int orte_ns_base_unpack_name(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i, num;
    orte_process_name_t* proc;
    orte_cellid_t *cellid;
    orte_jobid_t *jobid;
    orte_vpid_t *vpid;

    num = *num_vals;

    /* allocate space for all the cellids in a contiguous array */
    cellid = (orte_cellid_t*)malloc(num * sizeof(orte_cellid_t));
    if (NULL == cellid) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        *num_vals = 0;
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    /* now unpack them in one shot */
    if (ORTE_SUCCESS != (rc =
            orte_ns_base_unpack_cellid(buffer, cellid, num_vals, ORTE_CELLID))) {
        ORTE_ERROR_LOG(rc);
        *num_vals = 0;
        free(cellid);
        return rc;
    }

    /* allocate space for all the jobids in a contiguous array */
    jobid = (orte_jobid_t*)malloc(num * sizeof(orte_jobid_t));
    if (NULL == jobid) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        *num_vals = 0;
        free(cellid);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    /* now unpack them in one shot */
    if (ORTE_SUCCESS != (rc =
            orte_ns_base_unpack_jobid(buffer, jobid, num_vals, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        *num_vals = 0;
        free(jobid);
        free(cellid);
        return rc;
    }

    /* collect all the vpids in a contiguous array */
    vpid = (orte_vpid_t*)malloc(num * sizeof(orte_vpid_t));
    if (NULL == vpid) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        *num_vals = 0;
        free(jobid);
        free(cellid);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    /* now unpack them in one shot */
    if (ORTE_SUCCESS != (rc =
            orte_ns_base_unpack_vpid(buffer, vpid, num_vals, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        *num_vals = 0;
        free(vpid);
        free(jobid);
        free(cellid);
        return rc;
    }

    /* build the names from the cellid/jobid/vpid arrays */
    proc = (orte_process_name_t*)dest;
    for (i=0; i < num; i++) {
        proc->cellid = cellid[i];
        proc->jobid = jobid[i];
        proc->vpid = vpid[i];
        proc++;
    }

    /* cleanup */
    free(vpid);
    free(jobid);
    free(cellid);

    return ORTE_SUCCESS;
}

/*
 * CELLID
 */
int orte_ns_base_unpack_cellid(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_CELLID_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * NODEID
 */
int orte_ns_base_unpack_nodeid(orte_buffer_t *buffer, void *dest,
                               orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int ret;
    
    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_NODEID_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * JOBID
 */
int orte_ns_base_unpack_jobid(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_JOBID_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * VPID
 */
int orte_ns_base_unpack_vpid(orte_buffer_t *buffer, void *dest,
                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_VPID_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

