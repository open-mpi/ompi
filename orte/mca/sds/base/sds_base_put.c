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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <errno.h>

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/orte_constants.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/base/base.h"

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
    opal_setenv(param, "env", true, env);
    free(param);

    /* not a seed */
    if(NULL == (param = mca_base_param_environ_variable("seed",NULL,NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_unsetenv(param, env);
    free(param);

    /* since we want to pass the name as separate components, make sure
     * that the "name" environmental variable is cleared!
     */
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","name"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_unsetenv(param, env);
    free(param);

    /* setup the name */
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","cellid"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, cellid, true, env);
    free(param);
    free(cellid);

    if(NULL == (param = mca_base_param_environ_variable("ns","nds","jobid"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, jobid, true, env);
    free(param);
    free(jobid);

    if(NULL == (param = mca_base_param_environ_variable("ns","nds","vpid"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, vpid, true, env);
    free(param);
    free(vpid);

    asprintf(&value, "%lu", (unsigned long) vpid_start);
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","vpid_start"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, value, true, env);
    free(param);
    free(value);

    asprintf(&value, "%lu", (unsigned long) num_procs);
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","num_procs"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, value, true, env);
    free(param);
    free(value);
    return ORTE_SUCCESS;
}

/**
 * sets up the environment so that a process launched with the bproc launcher can
 * figure out its name
 * @param cell the cell that the process belongs to.
 * @param job  the job the process belongs to
 * @param vpid_start the starting vpid for the current parallel launch
 * @param global_vpid_start the starting vpid for the job
 * @param num_procs the number of user processes in the job
 * @param env a pointer to the environment to setup
 * @retval ORTE_SUCCESS
 * @retval error
 */
int orte_ns_nds_bproc_put(orte_cellid_t cell, orte_jobid_t job, 
                          orte_vpid_t vpid_start, orte_vpid_t global_vpid_start,
                          int num_procs, char ***env) {
    char* param;
    char* value;
    int rc;

    /* set the mode to bproc */
    if(NULL == (param = mca_base_param_environ_variable("ns","nds",NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, "bproc", true, env);
    free(param);

    /* not a seed */
    if(NULL == (param = mca_base_param_environ_variable("seed",NULL,NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_unsetenv(param, env);
    free(param);

    /* since we want to pass the name as separate components, make sure
     * that the "name" environmental variable is cleared!
     */
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","name"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_unsetenv(param, env);
    free(param);

    /* setup the name */
    if(ORTE_SUCCESS != (rc = orte_ns.convert_cellid_to_string(&value, cell))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","cellid"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, value, true, env);
    free(param);
    free(value);

    if(ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&value, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","jobid"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, value, true, env);
    free(param);
    free(value);

    rc = orte_ns.convert_vpid_to_string(&value, vpid_start);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","vpid_start"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, value, true, env);
    free(param);
    free(value);

    rc = orte_ns.convert_vpid_to_string(&value, global_vpid_start);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","global_vpid_start"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, value, true, env);
    free(param);
    free(value);

    asprintf(&value, "%d", num_procs);
    if(NULL == (param = mca_base_param_environ_variable("ns","nds","num_procs")))
 {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, value, true, env);
    free(param);
    free(value);

    /* we have to set this environmental variable so bproc will give us our rank
     * after the launch */
    
    putenv("BPROC_RANK=XXXXXXX");
    opal_setenv("BPROC_RANK", "XXXXXXX", true, env);
    
    return ORTE_SUCCESS;
}


int orte_ns_nds_pipe_put(const orte_process_name_t* name, orte_vpid_t vpid_start, size_t num_procs, int fd)
{
    int rc;

    rc = write(fd,name,sizeof(orte_process_name_t));
    if(rc != sizeof(orte_process_name_t)) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_NOT_FOUND;
    }

    rc = write(fd,&vpid_start, sizeof(vpid_start));
    if(rc != sizeof(vpid_start)) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_NOT_FOUND;
    }

    rc = write(fd,&num_procs, sizeof(num_procs));
    if(rc != sizeof(num_procs)) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_NOT_FOUND;
    }

    return ORTE_SUCCESS;
}

