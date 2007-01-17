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
 */

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

static bool recv_issued=false;

int orte_rmgr_base_comm_start(void)
{
    int rc;

    if (recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_RMGR,
                                                      ORTE_RML_PERSISTENT,
                                                      orte_rmgr_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = true;
    
    return rc;
}

int orte_rmgr_base_comm_stop(void)
{
    int rc;
    
    if (!recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_RMGR))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = false;
    
    return rc;
}



/*
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

void orte_rmgr_base_recv(int status, orte_process_name_t* sender,
                        orte_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    orte_buffer_t answer;
    orte_rmgr_cmd_t command;
    orte_std_cntr_t i, count, num_context;
    orte_jobid_t job;
    orte_app_context_t **context;
    opal_list_item_t *item;
    opal_list_t attrs;
    int rc;

    OPAL_TRACE(2);
    
    /* get the command */
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &count, ORTE_RMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* setup to return an answer */
    OBJ_CONSTRUCT(&answer, orte_buffer_t);

    /* pack the command in the answer - this is done to allow the caller to check
     * that we are talking about the same command
     */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, &command, 1, ORTE_RMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    switch (command) {
        case ORTE_RMGR_SETUP_JOB_CMD:
            /* get the number of app_contexts */
            count = 1;
            if(ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &num_context, &count, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
                
            /* allocate space for them */
            if(NULL == (context = (orte_app_context_t**)malloc(sizeof(orte_app_context_t*)*num_context))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return;
            }
            
            /* and unpack them */
            count = num_context;
            if(ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, context, &count, ORTE_APP_CONTEXT))) {
                ORTE_ERROR_LOG(rc);
                free(context);
                return;
            }
                
            /* unpack the attributes */
            OBJ_CONSTRUCT(&attrs, opal_list_t);
            count = 1;
            if(ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &attrs, &count, ORTE_ATTR_LIST))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            /* process the request */
            if (ORTE_SUCCESS != (rc = orte_rmgr.setup_job(context, num_context, &job, &attrs))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP_SPAWN;
            }
            
            /* return the new jobid */
            if(ORTE_SUCCESS != (rc = orte_dss.pack(&answer, &job, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP_SPAWN;
            }
                
            goto CLEANUP_SPAWN;  /* clean up the attrs and contexts */
            break;

        case ORTE_RMGR_SPAWN_JOB_CMD:
            /* for proxy operations, we don't pack callback functions - the proxy
             * component takes care of registering the subscription itself.
             * Hence, the only things we receive here are the app_context objects
             */
            /* get the number of app_contexts */
            count = 1;
            if(ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &num_context, &count, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
                
            /* allocate space for them */
            if(NULL == (context = (orte_app_context_t**)malloc(sizeof(orte_app_context_t*)*num_context))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return;
            }
            
            /* and unpack them */
            count = num_context;
            if(ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, context, &count, ORTE_APP_CONTEXT))) {
                ORTE_ERROR_LOG(rc);
                free(context);
                return;
            }
            
            /* unpack the attributes */
            OBJ_CONSTRUCT(&attrs, opal_list_t);
            count = 1;
            if(ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &attrs, &count, ORTE_ATTR_LIST))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP_SPAWN;
            }
            
            /* process the request */
            /* init the job to be INVALID so we setup the job */
            job = ORTE_JOBID_INVALID;
            if (ORTE_SUCCESS != (rc = orte_rmgr.spawn_job(context, num_context, &job,
                                                          0, NULL, NULL, ORTE_PROC_STATE_NONE, &attrs))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP_SPAWN;
            }
            
            /* return the new jobid */
            if(ORTE_SUCCESS != (rc = orte_dss.pack(&answer, &job, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
            }

CLEANUP_SPAWN:
            for (i=0; i < num_context; i++) {
                OBJ_RELEASE(context[i]);
            }
            if (NULL != context) free(context);

            while (NULL != (item = opal_list_remove_first(&attrs))) {
                OBJ_RELEASE(item);
            }
            OBJ_DESTRUCT(&attrs);
            break;

        case ORTE_RMGR_SETUP_GATES_CMD:
            /* get the jobid */
            count = 1;
            if(ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            /* setup the stage gates */
            if (ORTE_SUCCESS != (rc = orte_rmgr_base_proc_stage_gate_init(job))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
            break;

                
        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }

SEND_ANSWER:
    if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
    }
    
    /* cleanup */
    OBJ_DESTRUCT(&answer);
}

