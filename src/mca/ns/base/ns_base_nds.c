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
 */

#include "orte_config.h"

#include "include/orte_constants.h"
#include "mca/ns/base/ns_base_nds.h"
#include "util/proc_info.h"
#include "mca/base/mca_base_param.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ns/base/base.h"
#include "mca/ns/base/ns_base_nds.h"


static orte_ns_nds_t orte_ns_nds[] = {
{ "env", orte_ns_nds_env_get },
{ "pipe", orte_ns_nds_pipe_get },
{ NULL, NULL }
};


int orte_ns_base_set_my_name(void)
{
    int rc, id;
    char *mode;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    
    /* check to see if name has already been set - if so, leave it alone */
    if (NULL != orte_process_info.my_name) {
        return ORTE_SUCCESS;
    }
    
    /* first check if we are seed or singleton that couldn't
     * join an existing universe - if so, name is mandated */
    if (orte_process_info.seed || NULL == orte_process_info.ns_replica) {
        return orte_ns_base_create_process_name(
                                &(orte_process_info.my_name), 0, 0, 0);
    }
    
    /* okay, not seed/singleton attempt another approach */
    id = mca_base_param_register_string("ns", "nds", NULL, NULL, NULL);
    mca_base_param_lookup_string(id, &mode);

    if (NULL != mode) {  /* mode identified */ 
        orte_ns_nds_t* nds = orte_ns_nds;
        while(NULL != nds->mode) {
            if (0 == strcmp(mode, nds->mode)) {
                return nds->discover();
            }
            nds++;
        }
    }
     
    if (ORTE_SUCCESS != (rc = orte_ns.create_jobid(&jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.reserve_range(jobid, 1, &vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&(orte_process_info.my_name),
                                                0, jobid, vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    orte_process_info.num_procs = 1;
    orte_process_info.vpid_start = vpid;
    return ORTE_SUCCESS;
}

int orte_ns_base_get_peers(orte_process_name_t **procs, 
                           size_t *num_procs, size_t *self)
{
    int i, rc;
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

    *num_procs = (size_t)orte_process_info.num_procs;
    *self = (size_t)(myvpid - orte_process_info.vpid_start);
    
    return ORTE_SUCCESS;
}
