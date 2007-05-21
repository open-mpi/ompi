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
 *
 */

#include "orte_config.h"

#include "orte/orte_constants.h"

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"

#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/sds/env/sds_env.h"

orte_sds_base_module_t orte_sds_env_module = {
    orte_sds_base_basic_contact_universe,
    orte_sds_env_set_name,
    orte_sds_env_finalize,
};


int
orte_sds_env_set_name(void)
{
    int rc;
    int id;
    int vpid_start;
    int num_procs;
    int local_rank;
    int num_local_procs;
    char* name_string = NULL;
    char *local_daemon_uri = NULL;
    
    
    id = mca_base_param_register_string("ns", "nds", "name", NULL, NULL);
    mca_base_param_lookup_string(id, &name_string);

    if(name_string != NULL) {
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_process_name(
           &(orte_process_info.my_name),
           name_string))) {
           ORTE_ERROR_LOG(rc);
           free(name_string);
           return rc;
        }
        free(name_string);

    } else {

        orte_cellid_t cellid;
        orte_jobid_t jobid;
        orte_vpid_t vpid;
        char* cellid_string;
        char* jobid_string;
        char* vpid_string;
      
        id = mca_base_param_register_string("ns", "nds", "cellid", NULL, NULL);
        mca_base_param_lookup_string(id, &cellid_string);
        if (NULL == cellid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_cellid(&cellid, cellid_string))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
            
        id = mca_base_param_register_string("ns", "nds", "jobid", NULL, NULL);
        mca_base_param_lookup_string(id, &jobid_string);
        if (NULL == jobid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_jobid(&jobid, jobid_string))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }

        id = mca_base_param_register_string("ns", "nds", "vpid", NULL, NULL);
        mca_base_param_lookup_string(id, &vpid_string);
        if (NULL == vpid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_vpid(&vpid, vpid_string))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }

        if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(
           &(orte_process_info.my_name),
           cellid,
           jobid,
           vpid))) {
           ORTE_ERROR_LOG(rc);
           return rc;
        }
    }

    id = mca_base_param_register_int("ns", "nds", "vpid_start", NULL, -1);
    mca_base_param_lookup_int(id, &vpid_start);
    if (vpid_start < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    orte_process_info.vpid_start = (orte_vpid_t)vpid_start;

    id = mca_base_param_register_int("ns", "nds", "num_procs", NULL, -1);
    mca_base_param_lookup_int(id, &num_procs);
    if (num_procs < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    orte_process_info.num_procs = (orte_std_cntr_t)num_procs;

    /* it is okay for this param not to be found - for example, we don't bother
     * to set it for orteds - so just set it to an invalid value which indicates
     * it wasn't found if it isn't there
     */
    id = mca_base_param_register_int("ns", "nds", "local_rank", NULL, ORTE_VPID_INVALID);
    mca_base_param_lookup_int(id, &local_rank);
    orte_process_info.local_rank = (orte_vpid_t)local_rank;
    
    /* it is okay for this param not to be found - for example, we don't bother
     * to set it for orteds - so just set it to a value which indicates
     * it wasn't found if it isn't there
     */
    id = mca_base_param_register_int("ns", "nds", "num_local_procs", NULL, 0);
    mca_base_param_lookup_int(id, &num_local_procs);
    orte_process_info.num_local_procs = (orte_std_cntr_t)num_local_procs;
    
    id = mca_base_param_register_string("orte", "local_daemon", "uri", NULL, NULL);
    mca_base_param_lookup_string(id, &local_daemon_uri);
    if (NULL != local_daemon_uri) {
        /* if we are a daemon, then we won't have this param set, so allow
         * it not to be found
         */
        if (ORTE_SUCCESS != (rc = orte_sds_base_contact_orted(local_daemon_uri))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
    }
    
    return ORTE_SUCCESS;
}


int 
orte_sds_env_finalize(void)
{
    return ORTE_SUCCESS;
}
