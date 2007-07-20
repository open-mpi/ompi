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

#include "orte/mca/ns/base/ns_private.h"

/*
 * VPID
 */
int orte_ns_base_copy_vpid(orte_vpid_t **dest, orte_vpid_t *src, orte_data_type_t type)
{
    orte_vpid_t *val;

    val = (orte_vpid_t*)malloc(sizeof(orte_vpid_t));
    if (NULL == val) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    *val = *src;
    *dest = val;

    return ORTE_SUCCESS;
}

/*
 * CELLID
 */
int orte_ns_base_copy_cellid(orte_cellid_t **dest, orte_cellid_t *src, orte_data_type_t type)
{
    orte_cellid_t *val;

    val = (orte_cellid_t*)malloc(sizeof(orte_cellid_t));
    if (NULL == val) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    *val = *src;
    *dest = val;

    return ORTE_SUCCESS;
}

/*
 * NODEID
 */
int orte_ns_base_copy_nodeid(orte_nodeid_t **dest, orte_nodeid_t *src, orte_data_type_t type)
{
    orte_nodeid_t *val;
    
    val = (orte_nodeid_t*)malloc(sizeof(orte_nodeid_t));
    if (NULL == val) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    *val = *src;
    *dest = val;
    
    return ORTE_SUCCESS;
}

/*
 * JOBID
 */
int orte_ns_base_copy_jobid(orte_jobid_t **dest, orte_jobid_t *src, orte_data_type_t type)
{
    orte_jobid_t *val;

    val = (orte_jobid_t*)malloc(sizeof(orte_jobid_t));
    if (NULL == val) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    *val = *src;
    *dest = val;

    return ORTE_SUCCESS;
}

/* COPY FUNCTIONS FOR COMPLEX TYPES */

/* PROCESS NAME */
int orte_ns_base_copy_name(orte_process_name_t **dest, orte_process_name_t *src, orte_data_type_t type)
{
    orte_process_name_t *val;

    val = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    if (NULL == val) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    val->cellid = src->cellid;
    val->jobid = src->jobid;
    val->vpid = src->vpid;

    *dest = val;
    return ORTE_SUCCESS;
}
