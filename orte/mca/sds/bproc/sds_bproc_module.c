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
 *
 */

#include "orte_config.h"

#include "include/orte_constants.h"
#include "mca/base/mca_base_param.h"
#include "mca/sds/sds.h"
#include "mca/sds/base/base.h"
#include "mca/sds/bproc/sds_bproc.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "mca/errmgr/base/base.h"

orte_sds_base_module_t sds_bproc_module = {
    orte_sds_base_basic_contact_universe,
    orte_sds_bproc_set_name,
    orte_sds_bproc_finalize,
};


int
orte_sds_bproc_set_name(void)
{
    int rc;
    int id;
    int vpid_start;
    int global_vpid_start;
    int num_procs;
    char* name_string = NULL;
    id = mca_base_param_register_string("ns", "nds", "name", NULL, NULL);
    mca_base_param_lookup_string(id, &name_string);
    if(name_string != NULL) {
        if (ORTE_SUCCESS != (rc = orte_ns_base_convert_string_to_process_name(
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
        orte_vpid_t vpid_offset;
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

        vpid_string = getenv("BPROC_RANK");
        if (NULL == vpid_string) {
            opal_output(0, "orte_ns_nds_bproc_get: Error: Environment variable "
                           "BPROC_RANK not found.\n");
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        rc = orte_ns.convert_string_to_vpid(&vpid_offset, vpid_string);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
        id = mca_base_param_register_int("ns", "nds", "vpid_start", NULL, -1);
        mca_base_param_lookup_int(id, &vpid_start);
        if (vpid_start < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        rc = orte_ns.derive_vpid(&vpid, vpid_offset, vpid_start);
        if (ORTE_SUCCESS != rc) {
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

    id = mca_base_param_register_int("ns", "nds", "num_procs", NULL, -1);
    mca_base_param_lookup_int(id, &num_procs);
    if (num_procs < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    orte_process_info.num_procs = (size_t)num_procs;

    id = mca_base_param_register_int("ns", "nds", "global_vpid_start", NULL, -1);
    mca_base_param_lookup_int(id, &global_vpid_start);
    if (global_vpid_start < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    orte_process_info.vpid_start = (orte_vpid_t)global_vpid_start;

    return ORTE_SUCCESS;
}


int 
orte_sds_bproc_finalize(void)
{
    return ORTE_SUCCESS;
}
