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
#include "orte/orte_constants.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>

#include "opal/util/trace.h"

#include "orte/mca/gpr/gpr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

/*
 *  Create the job segment and initialize the application context.
 */

int orte_rmgr_base_put_app_context(
    orte_jobid_t jobid,
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context)
{
    orte_gpr_value_t *value;
    orte_std_cntr_t i;
    orte_std_cntr_t job_slots;
    int rc;
    char *segment;

    OPAL_TRACE(2);

    rc = orte_rmgr_base_get_job_slots(jobid, &job_slots);
    if(ORTE_SUCCESS != rc) {
        return rc;
    }

    /* put context info on the job segment of the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_OVERWRITE,
                                                    segment, num_context, 1))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }

    free(segment); /* don't need this any more */

    value->tokens[0] = strdup(ORTE_JOB_GLOBALS);

    for(i=0; i<num_context; i++) {
        orte_app_context_t* app = app_context[i];
        app->idx = i;

        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[i]),
                                                         ORTE_JOB_APP_CONTEXT_KEY,
                                                         ORTE_APP_CONTEXT,
                                                         app))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        job_slots += app->num_procs;
    }

    rc = orte_gpr.put(1, &value);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    rc = orte_rmgr_base_set_job_slots(jobid, job_slots); /* JJH C/N napping breaks here */

cleanup:
    OBJ_RELEASE(value);
    return rc;
}


/*
 * Comparison function for sorting context by index.
 */

static int orte_rmgr_base_cmp_app_context(
    orte_app_context_t** app1,
    orte_app_context_t** app2)
{
    OPAL_TRACE(3);

    if ((*app1)->idx < (*app2)->idx) {
        return -1;
    } else if((*app1)->idx > (*app2)->idx) {
        return 1;
    }
    return 0;
}


/*
 *  Retreive the application context
 */

int orte_rmgr_base_get_app_context(
    orte_jobid_t jobid,
    orte_app_context_t*** app_context,
    orte_std_cntr_t* num_context)
{
    char *segment;
    char *tokens[2];
    char *keys[2];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, num_values = 0, index = 0;
    int rc;

    OPAL_TRACE(2);

    /* get the job segment name */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid)))
        return rc;

    tokens[0] = ORTE_JOB_GLOBALS;
    tokens[1] = NULL;

    keys[0] = ORTE_JOB_APP_CONTEXT_KEY;
    keys[1] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_TOKENS_OR,
        segment,
        tokens,
        keys,
        &num_values,
        &values
        );
    if (rc != ORTE_SUCCESS) {
        goto cleanup;
    }

    *num_context = 0;
    for(i=0; i<num_values; i++) {
        *num_context += values[i]->cnt;
    }
    if (0 == *num_context) {
        *app_context = NULL;
        return ORTE_SUCCESS;
    }

    *app_context = (orte_app_context_t**)malloc(*num_context * sizeof(orte_app_context_t*));
    for(i=0; i<num_values; i++) {
        orte_gpr_value_t* value = values[i];
        orte_gpr_keyval_t** keyvals = value->keyvals;
        orte_std_cntr_t k;
        for(k=0; k < value->cnt; k++) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&((*app_context)[index++]), keyvals[k]->value, ORTE_APP_CONTEXT))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            keyvals[k]->value->data = NULL;  /* protect the data storage from later release */
        }
    }
    qsort(*app_context, *num_context, sizeof(orte_app_context_t*),
        (int (*)(const void*,const void*))orte_rmgr_base_cmp_app_context);

cleanup:
    for(i=0; i<num_values; i++) {
        OBJ_RELEASE(values[i]);
    }
    if(NULL != values)
        free(values);
    free(segment);
    return rc;
}


/*
 * Query for the total number of process slots requested for the job.
 */

int orte_rmgr_base_get_job_slots(orte_jobid_t jobid, orte_std_cntr_t* proc_slots)
{
    char *segment;
    char *tokens[2];
    char *keys[2];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, num_values = 0, *ps;
    int rc;

    OPAL_TRACE(2);

    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid)))
        return rc;

    tokens[0] = ORTE_JOB_GLOBALS;
    tokens[1] = NULL;

    keys[0] = ORTE_JOB_SLOTS_KEY;
    keys[1] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        segment,
        tokens,
        keys,
        &num_values,
        &values
        );

    if(rc != ORTE_SUCCESS) {
        free(segment);
        return rc;
    }

    free(segment); /* don't need this any more */
    
    if(0 == num_values) {
        *proc_slots = 0;
        return ORTE_SUCCESS;
    }
    if(1 != num_values || values[0]->cnt != 1) {
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&ps, values[0]->keyvals[0]->value, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    *proc_slots = *ps;

    for(i=0; i<num_values; i++)
        OBJ_RELEASE(values[i]);
    if (NULL != values) free(values);
    
    return ORTE_SUCCESS;
}


/*
 * Set the total number of process slots requested for the job.
 */

int orte_rmgr_base_set_job_slots(orte_jobid_t jobid, orte_std_cntr_t proc_slots)
{
    orte_gpr_value_t *value;
    char *segment;
    int rc;

    OPAL_TRACE(2);

    /* put context info on the job segment of the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_OVERWRITE,
                                                    segment, 1, 1))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    free(segment);

    value->tokens[0] = strdup(ORTE_JOB_GLOBALS);

    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]),
                                                     ORTE_JOB_SLOTS_KEY,
                                                     ORTE_STD_CNTR,
                                                     &proc_slots))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
        
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
    }
    
cleanup:
    OBJ_RELEASE(value);
    return rc;
}
