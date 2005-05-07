/* -*- C -*-
 *
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
 *
 * Convenience functions for accessing the General Purpose Registry
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include <string.h>

#include "include/orte_constants.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/errmgr/errmgr.h"

#include "mca/schema/base/base.h"


int orte_schema_base_get_proc_tokens(char ***proc_tokens, size_t* num_tokens, orte_process_name_t *proc)
{
    int rc;
    char** tokens;
    char* vpid_string;

    tokens = (char**)malloc(3 * sizeof(char*));
    if (NULL == tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.get_proc_name_string(&tokens[0], proc))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_string(&vpid_string, proc))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    asprintf(&tokens[1], "%s-%s", ORTE_VPID_KEY, vpid_string);
    free(vpid_string);
    tokens[2] = NULL;
    *proc_tokens = tokens;
    if(num_tokens != NULL)
        *num_tokens = 2;
    return ORTE_SUCCESS;
    
CLEANUP:
    if (NULL != tokens) {
        if (NULL != tokens[0])
            free(tokens[0]);
        if (NULL != tokens[1])
            free(tokens[1]);
        free(tokens);
    }
    return rc;
}

int orte_schema_base_get_node_tokens(char ***node_tokens, size_t* num_tokens, orte_cellid_t cellid, char *nodename)
{
    int rc;
    char** tokens;
    char* cellid_string;
    tokens = (char**)malloc(3 * sizeof(char*));
    if (NULL == tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.convert_cellid_to_string(&cellid_string, cellid))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    asprintf(&tokens[0], "%s-%s", ORTE_CELLID_KEY, cellid_string);
    free(cellid_string);
    tokens[1] = strdup(nodename);
    tokens[2] = NULL;
    *node_tokens = tokens;
    if(num_tokens != NULL)
        *num_tokens = 2;
    return ORTE_SUCCESS;
    
CLEANUP:
    if (NULL != tokens) {
        if (NULL != tokens[0])
            free(tokens[0]);
        if (NULL != tokens[1])
            free(tokens[1]);
        free(tokens);
    }
    return rc;
}

int orte_schema_base_get_cell_tokens(char ***tokens, size_t* num_tokens, orte_cellid_t cellid)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_schema_base_get_job_segment_name(char **name, orte_jobid_t jobid)
{
    char *jobidstring;
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobidstring, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (0 > asprintf(name, "%s-%s", ORTE_JOB_SEGMENT, jobidstring)) {
        free(jobidstring);
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    free(jobidstring);
    return ORTE_SUCCESS;
}


int orte_schema_base_extract_jobid_from_segment_name(orte_jobid_t *jobid, char *name)
{
    char *jobstring;
    orte_jobid_t job;
    int rc;
    
    jobstring = strrchr(name, '-');
    if (NULL == jobstring) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    jobstring++;
    if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_jobid(&job, jobstring))) {
        ORTE_ERROR_LOG(rc);
        ompi_output(0, "[%d,%d,%d] %s\n", ORTE_NAME_ARGS(orte_process_info.my_name), jobstring);
        return rc;
    }
    *jobid = job;
    return ORTE_SUCCESS;
}


/**
 * Set the process mapping in the registry.
 */

int orte_schema_base_store_my_info(void)
{
    int rc = ORTE_SUCCESS;
    orte_gpr_value_t value, *values;
    orte_gpr_keyval_t local_pid = { {OBJ_CLASS(ompi_object_t),0}, ORTE_PROC_LOCAL_PID_KEY, ORTE_PID };
    orte_gpr_keyval_t nodename = { {OBJ_CLASS(ompi_object_t),0}, ORTE_NODE_NAME_KEY, ORTE_STRING };
    orte_gpr_keyval_t* keyvals[2];
    size_t i;
    orte_jobid_t jobid;

    /* NOTE: cannot destruct the value object since the keyval's are statically
     * defined, so don't construct it either
     */
     
    keyvals[0] = &local_pid;
    keyvals[1] = &nodename;
    value.addr_mode = ORTE_GPR_OVERWRITE;
    
    if (ORTE_SUCCESS != (rc = orte_schema_base_get_proc_tokens(&value.tokens,
                                &value.num_tokens, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
                                
    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid(&jobid, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_schema_base_get_job_segment_name(&value.segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    value.keyvals = keyvals;
    value.cnt = 2;
    values = &value;
                                                                                                           
    local_pid.value.pid = orte_process_info.pid;
    nodename.value.strptr = strdup(orte_system_info.nodename);

    /* insert values into registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
    }

    /* cleanup memory */
    for (i=0; i < value.num_tokens; i++) {
        free(value.tokens[i]);
    }
    
    return rc;
}

