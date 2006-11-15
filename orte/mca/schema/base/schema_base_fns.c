/* -*- C -*-
 *
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

#include "orte/orte_constants.h"

#include "opal/util/output.h"

#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/schema/base/base.h"


int orte_schema_base_get_proc_tokens(char ***proc_tokens, orte_std_cntr_t* num_tokens, orte_process_name_t *proc)
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

int orte_schema_base_get_node_tokens(char ***node_tokens, orte_std_cntr_t* num_tokens, orte_cellid_t cellid, char *nodename)
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

int orte_schema_base_get_job_tokens(char ***job_tokens, orte_std_cntr_t* num_tokens, orte_jobid_t jobid)
{
    int rc;
    char** tokens;
    char* jobid_string;

    tokens = (char**)malloc(2 * sizeof(char*));
    if (NULL == tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    asprintf(&tokens[0], "%s-%s", ORTE_JOBID_KEY, jobid_string);
    free(jobid_string);
    tokens[1] = NULL;
    *job_tokens = tokens;
    if(num_tokens != NULL)
        *num_tokens = 1;
    return ORTE_SUCCESS;

CLEANUP:
    if (NULL != tokens) {
        if (NULL != tokens[0]) free(tokens[0]);
        free(tokens);
    }
    return rc;
}

int orte_schema_base_get_cell_tokens(char ***cell_tokens, orte_std_cntr_t* num_tokens, orte_cellid_t cellid)
{
    int rc;
    char **tokens;
    char *site, *resource, *cellstr;
    orte_std_cntr_t n, i;

    n = 1;

    if (ORTE_SUCCESS != (rc = orte_ns.get_cell_info(cellid, &site, &resource))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.convert_cellid_to_string(&cellstr, cellid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (NULL != site) n++;
    if (NULL != resource) n++;

    tokens = (char**)malloc(n * sizeof(char*));
    if (NULL == tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    tokens[0] = cellstr;
    i=1;
    if (NULL != site) {
        tokens[i] = site;
        i++;
    }
    if (NULL != resource) {
        tokens[i] = resource;
    }

    *num_tokens = n;
    *cell_tokens = tokens;

    return ORTE_SUCCESS;
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
        opal_output(0, "[%lu,%lu,%lu] %s\n", ORTE_NAME_ARGS(orte_process_info.my_name), jobstring);
        return rc;
    }
    *jobid = job;
    return ORTE_SUCCESS;
}


int orte_schema_base_get_std_trigger_name(char **name,
                    char *trigger,
                    orte_jobid_t jobid)
{
    char *jobidstring;
    int rc;

    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobidstring, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (0 > asprintf(name, "%s-%s", trigger, jobidstring)) {
        free(jobidstring);
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    free(jobidstring);
    return ORTE_SUCCESS;
}

bool orte_schema_base_check_std_trigger_name(char *name, char *trig)
{
    if (NULL == name || NULL == trig) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return false;
    }

    if (0 == strncmp(name, trig, strlen(trig))) return true;

    return false;
}


int orte_schema_base_extract_jobid_from_std_trigger_name(orte_jobid_t *jobid, char *trig)
{
    char *jobstring;
    orte_jobid_t job;
    int rc;

    jobstring = strrchr(trig, '-');
    if (NULL == jobstring) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    jobstring++;
    if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_jobid(&job, jobstring))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    *jobid = job;
    return ORTE_SUCCESS;
}


int orte_schema_base_get_std_subscription_name(char **name,
                    char *subscription,
                    orte_jobid_t jobid)
{
    char *jobidstring;
    int rc;

    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobidstring, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (0 > asprintf(name, "%s-%s", subscription, jobidstring)) {
        free(jobidstring);
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    free(jobidstring);
    return ORTE_SUCCESS;
}
