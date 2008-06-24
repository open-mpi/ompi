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

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/dss/dss.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/schema/schema.h"

#include "orte/mca/pls/bproc/pls_bproc.h"

/**
 * Set the process pid in the job segment and indicate the state
 * as being launched.
 */

int orte_pls_bproc_set_proc_pid(const orte_process_name_t *name, pid_t pid, int nodenum)
{
    orte_gpr_value_t *values[1];
    char *segment;
    char *nodename;
    int rc;

    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, name->jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&values[0],
                                                    ORTE_GPR_OVERWRITE,
                                                    segment,
                                                    2, 0))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        free(segment);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    free(segment);

    if(ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&(values[0]->tokens), &(values[0]->num_tokens), (orte_process_name_t*)name))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[0]->keyvals[0]), ORTE_PROC_LOCAL_PID_KEY, ORTE_PID, &pid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        return rc;
    }

    asprintf(&nodename, "%ld", (long)nodenum);
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[0]->keyvals[1]), ORTE_NODE_NAME_KEY, ORTE_STRING, nodename))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        free(nodename);
        return rc;
    }
    free(nodename);
    
    rc = orte_gpr.put(1, values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        return rc;
    }

    OBJ_RELEASE(values[0]);

    /* set the process state to LAUNCHED */
    if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state((orte_process_name_t*)name, ORTE_PROC_STATE_LAUNCHED, 0))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

/**
 *  Retreive a specified process pid from the registry.
 */
int orte_pls_bproc_get_proc_pid(const orte_process_name_t* name, pid_t* pid)
{
    char *segment;
    char **tokens;
    orte_std_cntr_t num_tokens;
    char *keys[2];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, num_values = 0;
    pid_t *pptr;
    int rc;

    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, name->jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if(ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens, &num_tokens, (orte_process_name_t*)name))) {
        free(segment);
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    keys[0] = ORTE_PROC_LOCAL_PID_KEY;
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

    if(0 == num_values) {
        rc = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if(1 != num_values || values[0]->cnt != 1) {
        rc = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pptr, values[0]->keyvals[0]->value, ORTE_PID))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    *pid = *pptr;

cleanup:
    if(NULL != values) {
        for(i=0; i<num_values; i++) {
            if(NULL != values[i]) {
                OBJ_RELEASE(values[i]);
            }
        }
        if (NULL != values) free(values);
    }
    free(segment);

    return rc;
}

/**
 *  Retrieve all process pids for the specified job.
 */
int orte_pls_bproc_get_proc_pids(orte_jobid_t jobid, pid_t **pids, orte_std_cntr_t* num_pids, opal_list_t *attrs)
{
    char *segment;
    char *keys[2];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, num_values = 0;
    pid_t *pptr;
    int rc;

    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    keys[0] = ORTE_PROC_PID_KEY;
    keys[1] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        segment,
        NULL,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS) {
        free(segment);
        return rc;
    }

    if(0 == num_values) {
        *pids = NULL;
    } else {
        *pids = (pid_t*)malloc(sizeof(pid_t)*num_values);
        for(i=0; i<num_values; i++) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pptr, values[i]->keyvals[0]->value, ORTE_PID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            (*pids)[i] = *pptr;
        }
    }
    *num_pids = num_values;

cleanup:
    if(NULL != values) {
        for(i=0; i<num_values; i++) {
            if(NULL != values[i]) {
                OBJ_RELEASE(values[i]);
            }
        }
        if (NULL != values) free(values);
    }
    free(segment);
    return rc;
}


/**
 * Add a key-value to the node segment containing the process pid for
 * the daemons.
 */

int orte_pls_bproc_set_node_pid(orte_cellid_t cellid, char* node_name, orte_jobid_t jobid, pid_t pid)
{
    orte_gpr_value_t *values[1];
    char *jobid_string, *key;
    int rc;

    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&values[0],
                                                    ORTE_GPR_OVERWRITE,
                                                    ORTE_NODE_SEGMENT,
                                                    1, 0))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_schema.get_node_tokens(&(values[0]->tokens), &(values[0]->num_tokens), cellid, node_name))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        return rc;
    }

    asprintf(&key, "%s-%s", ORTE_PROC_PID_KEY, jobid_string);
    free(jobid_string);

    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[0]->keyvals[0]), key, ORTE_PID, &pid))) {
        ORTE_ERROR_LOG(rc);
        free(key);
        OBJ_RELEASE(values[0]);
        return rc;
    }
    free(key);

    rc = orte_gpr.put(1, values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

    OBJ_RELEASE(values[0]);
    return rc;
}


/**
 *  Retrieve all daemon pids for the specified job.
 */
int orte_pls_bproc_get_node_pids(orte_jobid_t jobid, pid_t **pids, orte_std_cntr_t* num_pids)
{
    char *keys[2];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, num_values = 0;
    int rc;
    char *jobid_string;
    pid_t *pptr;

    if(ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid)))
        goto cleanup;

    asprintf(&keys[0], "%s-%s", ORTE_PROC_PID_KEY, jobid_string);
    free(jobid_string);
    keys[1] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        ORTE_NODE_SEGMENT,
        NULL,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS) {
        free(keys[0]);
        return rc;
    }

    if(0 == num_values) {
        *pids = NULL;
    } else {
        *pids = (pid_t*)malloc(sizeof(pid_t)*num_values);
        for(i=0; i<num_values; i++) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pptr, values[i]->keyvals[0]->value, ORTE_PID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            *(pids[i]) = *pptr;
        }
    }
    *num_pids = num_values;

cleanup:
    if(NULL != values) {
        for(i=0; i<num_values; i++)
            OBJ_RELEASE(values[i]);
        if (NULL != values) free(values);
    }
    free(keys[0]);
    return rc;
}

/*
 * FUNCTIONS FOR DEALING WITH ABNORMAL TERMINATION OF BPROC
 * APPLICATION PROCESSES
 */
int orte_pls_bproc_comm_start(void)
{
    int rc;
    
    if (mca_pls_bproc_component.recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_BPROC_ABORT,
                                                      ORTE_RML_PERSISTENT,
                                                      orte_pls_bproc_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    mca_pls_bproc_component.recv_issued = true;
    
    return rc;
}


int orte_pls_bproc_comm_stop(void)
{
    int rc;
    
    if (!mca_pls_bproc_component.recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_BPROC_ABORT))) {
        ORTE_ERROR_LOG(rc);
    }
    mca_pls_bproc_component.recv_issued = false;
    
    return rc;
}


/*
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

void orte_pls_bproc_recv(int status, orte_process_name_t* sender,
                        orte_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    int rc;
        
    /* we don't care what was in the buffer - just set the state of the sender to ABORTED */
    if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(sender, ORTE_PROC_STATE_ABORTED, 0))) {
        ORTE_ERROR_LOG(rc);
    }
 }

