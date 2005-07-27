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
/** @file:
 */

#include "orte_config.h"

#include "include/orte_constants.h"
#include "util/proc_info.h"
#include "mca/base/mca_base_param.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ns/base/base.h"
#include "opal/util/output.h"


int orte_ns_base_get_peers(orte_process_name_t **procs, 
                           size_t *num_procs, size_t *self)
{
    size_t i;
    int rc;
    orte_cellid_t mycellid;
    orte_jobid_t myjobid;
    orte_vpid_t myvpid;
    
    *procs = (orte_process_name_t*)malloc(orte_process_info.num_procs *
                                            sizeof(orte_process_name_t));
    if (NULL == *procs) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_ns.get_cellid(&mycellid, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != orte_ns.get_jobid(&myjobid, orte_process_info.my_name)) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != orte_ns.get_vpid(&myvpid, orte_process_info.my_name)) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    for (i=0; i < orte_process_info.num_procs; i++) {
        (*procs)[i].cellid = mycellid;
        (*procs)[i].jobid = myjobid;
        (*procs)[i].vpid = orte_process_info.vpid_start + i;
    }

    *num_procs = orte_process_info.num_procs;
    *self = (size_t)(myvpid - orte_process_info.vpid_start);
    
    return ORTE_SUCCESS;
}
