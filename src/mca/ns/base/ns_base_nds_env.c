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
#include "util/proc_info.h"
#include "util/environ.h"
#include "mca/base/mca_base_param.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ns/base/base.h"
#include "mca/ns/base/ns_base_nds.h"


int orte_ns_nds_env_get(void)
{
    int rc;
    int id;
    int vpid_start;
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

    id = mca_base_param_register_int("ns", "nds", "num_procs", NULL, -1);
    mca_base_param_lookup_int(id, &num_procs);
    if (num_procs < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    orte_process_info.vpid_start = vpid_start;
    orte_process_info.num_procs = num_procs;
    return ORTE_SUCCESS;
}


int orte_ns_nds_env_put(const orte_process_name_t* name, 
                        orte_vpid_t vpid_start, size_t num_procs,
                        char ***env)
{
    char* param;
    char* cellid;
    char* jobid;
    char* vpid;
    char* value;
    int rc;

    if(ORTE_SUCCESS != (rc = orte_ns.get_cellid_string(&cellid, name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(ORTE_SUCCESS != (rc = orte_ns.get_jobid_string(&jobid, name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(ORTE_SUCCESS != (rc = orte_ns.get_vpid_string(&vpid, name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* set the mode to env */
    if(NULL == (param = mca_base_param_environ_variable("ns","nds",NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    ompi_setenv(param, "env", true, env);
    free(param);

    /* not a seed */
    if(NULL == (param = mca_base_param_environ_variable("seed",NULL,NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    ompi_unsetenv(param, env);
    free(param);

    /* setup the name */
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","cellid"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    ompi_setenv(param, cellid, true, env);
    free(param);
    free(cellid);

    if(NULL == (param = mca_base_param_environ_variable("ns","nds","jobid"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    ompi_setenv(param, jobid, true, env);
    free(param);
    free(jobid);

    if(NULL == (param = mca_base_param_environ_variable("ns","nds","vpid"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    ompi_setenv(param, vpid, true, env);
    free(param);
    free(vpid);

    asprintf(&value, "%d", vpid_start);
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","vpid_start"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    ompi_setenv(param, value, true, env);
    free(param);
    free(value);

    asprintf(&value, "%ld", num_procs);
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","num_procs"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    ompi_setenv(param, value, true, env);
    free(param);
    free(value);
    return ORTE_SUCCESS;
}


