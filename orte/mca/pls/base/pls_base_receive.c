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
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/pls/pls_types.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/pls_private.h"

static bool recv_issued=false;

int orte_pls_base_comm_start(void)
{
    int rc;

    if (recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_PLS,
                                                      ORTE_RML_PERSISTENT,
                                                      orte_pls_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = true;
    
    return rc;
}


int orte_pls_base_comm_stop(void)
{
    int rc;
    
    if (!recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS))) {
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

void orte_pls_base_recv(int status, orte_process_name_t* sender,
                        orte_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    orte_buffer_t answer;
    orte_pls_cmd_flag_t command;
    orte_std_cntr_t count;
    orte_jobid_t job;
    orte_process_name_t *name;
    int32_t signal;
    opal_list_t attrs;
    opal_list_item_t *item;
    struct timeval timeout;
    int32_t secs, microsecs;
    int rc;

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &count, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    OBJ_CONSTRUCT(&answer, orte_buffer_t);

    if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, &command, 1, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
    }
    
    switch (command) {
        case ORTE_PLS_LAUNCH_JOB_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            if (ORTE_SUCCESS != (rc = orte_pls.launch_job(job))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
            
        case ORTE_PLS_TERMINATE_JOB_CMD:
            /* get the jobid to be terminated */
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
            
            /* get any attributes */
            OBJ_CONSTRUCT(&attrs, opal_list_t);
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &attrs, &count, ORTE_ATTR_LIST))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            /* get the timeout - packed as two separate int32's */
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &secs, &count, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &microsecs, &count, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
            timeout.tv_sec = secs;
            timeout.tv_usec = microsecs;
                
            /* issue the command */
            if (ORTE_SUCCESS != (rc = orte_pls.terminate_job(job, &timeout, &attrs))) {
                ORTE_ERROR_LOG(rc);
            }
            
            /* cleanup attribute list */
            while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
            OBJ_DESTRUCT(&attrs);
            break;
            
        case ORTE_PLS_TERMINATE_ORTEDS_CMD:
            /* get the jobid whose daemons are to be terminated */
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            /* get any attributes */
            OBJ_CONSTRUCT(&attrs, opal_list_t);
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &attrs, &count, ORTE_ATTR_LIST))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            /* get the timeout - packed as two separate int32's */
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &secs, &count, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &microsecs, &count, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
            timeout.tv_sec = secs;
            timeout.tv_usec = microsecs;
            
            /* issue the command */
            if (ORTE_SUCCESS != (rc = orte_pls.terminate_orteds(job, &timeout, &attrs))) {
                ORTE_ERROR_LOG(rc);
            }
                
            /* cleanup attribute list */
            while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
            OBJ_DESTRUCT(&attrs);
            break;
            
        case ORTE_PLS_SIGNAL_JOB_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &signal, &count, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            OBJ_CONSTRUCT(&attrs, opal_list_t);
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &attrs, &count, ORTE_ATTR_LIST))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            if (ORTE_SUCCESS != (rc = orte_pls.signal_job(job, signal, &attrs))) {
                ORTE_ERROR_LOG(rc);
            }

            while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
            OBJ_DESTRUCT(&attrs);
            break;
            
        case ORTE_PLS_TERMINATE_PROC_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &name, &count, ORTE_NAME))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            if (ORTE_SUCCESS != (rc = orte_pls.terminate_proc(name))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
            
        case ORTE_PLS_SIGNAL_PROC_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &name, &count, ORTE_NAME))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &signal, &count, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
                
            if (ORTE_SUCCESS != (rc = orte_pls.signal_proc(name, signal))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
            
        case ORTE_PLS_CANCEL_OPERATION_CMD:
            /* issue the command */
            if (ORTE_SUCCESS != (rc = orte_pls.cancel_operation())) {
                ORTE_ERROR_LOG(rc);
            }
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }
   
SEND_ANSWER:  /* send the answer */
    if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
    }
    
    /* cleanup */
    OBJ_DESTRUCT(&answer);
}

