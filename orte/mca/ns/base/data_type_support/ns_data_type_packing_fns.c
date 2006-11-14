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

#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/ns/base/ns_private.h"

/*
 * NAME
 */
int orte_ns_base_pack_name(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i;
    orte_process_name_t* proc;
    orte_cellid_t *cellid;
    orte_jobid_t *jobid;
    orte_vpid_t *vpid;

    /* collect all the cellids in a contiguous array */
    cellid = (orte_cellid_t*)malloc(num_vals * sizeof(orte_cellid_t));
    if (NULL == cellid) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    proc = (orte_process_name_t*)src;
    for (i=0; i < num_vals; i++) {
        cellid[i] = proc->cellid;
        proc++;
    }
    /* now pack them in one shot */
    if (ORTE_SUCCESS != (rc =
            orte_ns_base_pack_cellid(buffer, cellid, num_vals, ORTE_CELLID))) {
        ORTE_ERROR_LOG(rc);
        free(cellid);
        return rc;
    }
    free(cellid);

    /* collect all the jobids in a contiguous array */
    jobid = (orte_jobid_t*)malloc(num_vals * sizeof(orte_jobid_t));
    if (NULL == jobid) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    proc = (orte_process_name_t*)src;
    for (i=0; i < num_vals; i++) {
        jobid[i] = proc->jobid;
        proc++;
    }
    /* now pack them in one shot */
    if (ORTE_SUCCESS != (rc =
            orte_ns_base_pack_jobid(buffer, jobid, num_vals, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        free(jobid);
        return rc;
    }
    free(jobid);

    /* collect all the vpids in a contiguous array */
    vpid = (orte_vpid_t*)malloc(num_vals * sizeof(orte_vpid_t));
    if (NULL == vpid) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    proc = (orte_process_name_t*)src;
    for (i=0; i < num_vals; i++) {
        vpid[i] = proc->vpid;
        proc++;
    }
    /* now pack them in one shot */
    if (ORTE_SUCCESS != (rc =
            orte_ns_base_pack_vpid(buffer, vpid, num_vals, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        free(vpid);
        return rc;
    }
    free(vpid);

    return ORTE_SUCCESS;
}

/*
 * CELLID
 */
int orte_ns_base_pack_cellid(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_CELLID_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * NODEID
 */
int orte_ns_base_pack_nodeid(orte_buffer_t *buffer, void *src,
                             orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int ret;
    
    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
                         ret = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_NODEID_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * JOBID
 */
int orte_ns_base_pack_jobid(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_JOBID_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * VPID
 */
int orte_ns_base_pack_vpid(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_VPID_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

