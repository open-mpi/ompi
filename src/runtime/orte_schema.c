/* -*- C -*-
 *
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

#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "include/orte_schema.h"

int orte_schema_get_proc_tokens(char ***tokens, int32_t* num_tokens, orte_process_name_t *proc);
int orte_schema_get_node_tokens(char ***tokens, int32_t* num_tokens, orte_cellid_t cellid, char *nodename);
int orte_schema_get_cell_tokens(char ***tokens, int32_t* num_tokens, orte_cellid_t cellid);
int orte_schema_get_job_segment_name(char **name, orte_jobid_t jobid);
int orte_schema_extract_jobid_from_segment_name(orte_jobid_t *jobid, char *name);

/*
 * globals
 */
orte_schema_t orte_schema = {
    orte_schema_get_proc_tokens,
    orte_schema_get_node_tokens,
    orte_schema_get_cell_tokens,
    orte_schema_get_job_segment_name,
    orte_schema_extract_jobid_from_segment_name
};

int orte_schema_open(void)
{
    /* just here to ensure library gets loaded for dynamic setup */
    return ORTE_SUCCESS;
}

int orte_schema_get_proc_tokens(char ***proc_tokens, int32_t* num_tokens, orte_process_name_t *proc)
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

int orte_schema_get_node_tokens(char ***node_tokens, int32_t* num_tokens, orte_cellid_t cellid, char *nodename)
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

int orte_schema_get_cell_tokens(char ***tokens, int32_t* num_tokens, orte_cellid_t cellid)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_schema_get_job_segment_name(char **name, orte_jobid_t jobid)
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


int orte_schema_extract_jobid_from_segment_name(orte_jobid_t *jobid, char *name)
{
    char *jobstring, *tmp;
    orte_jobid_t job;
    int rc;
    
    tmp = strrchr(name, '-');
    if (NULL == tmp) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    jobstring = strpbrk(tmp, "0123456789");
    if (NULL == jobstring) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_jobid(&job, jobstring))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    *jobid = job;
    return ORTE_SUCCESS;
}

