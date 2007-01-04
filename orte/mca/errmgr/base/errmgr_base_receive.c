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
#include "orte/mca/rml/rml.h"

#include "orte/mca/errmgr/base/errmgr_private.h"

static bool recv_issued=false;

int orte_errmgr_base_comm_start(void)
{
    int rc;

    if (recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_ERRMGR,
                                                      ORTE_RML_PERSISTENT,
                                                      orte_errmgr_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = true;
    
    return rc;
}

int orte_errmgr_base_comm_stop(void)
{
    int rc;
    
    if (!recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ERRMGR))) {
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

void orte_errmgr_base_recv(int status, orte_process_name_t* sender,
                        orte_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    orte_buffer_t answer;
    orte_errmgr_cmd_flag_t command;
    orte_std_cntr_t count, nprocs;
    orte_process_name_t *procs;
    orte_jobid_t jobid;
    int rc;

    OPAL_TRACE(2);
    
    /* get the command */
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &count, ORTE_ERRMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* setup to return an answer */
    OBJ_CONSTRUCT(&answer, orte_buffer_t);

    /* pack the command in the answer - this is done to allow the caller to check
     * that we are talking about the same command
     */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, &command, 1, ORTE_ERRMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    switch (command) {
        case ORTE_ERRMGR_ABORT_PROCS_REQUEST_CMD:
            /* get the number of processes */
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &nprocs, &count, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
            /* get the required space */
            procs = (orte_process_name_t*)malloc(nprocs * sizeof(orte_process_name_t));
            if (NULL == procs) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                goto SEND_ANSWER;
            }
            
            /* unpack the array of process names */
            count = nprocs;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, procs, &count, ORTE_NAME))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
            /* if we didn't get the number we requested, then something is wrong */
            if (count != nprocs) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto SEND_ANSWER;
            }

            /* process the request */
            if (ORTE_SUCCESS != (rc = orte_errmgr.abort_procs_request(procs, nprocs))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
            break;
            
        case ORTE_ERRMGR_REGISTER_JOB_CMD:
            /* register the job to monitor for alerts */
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &jobid, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto SEND_ANSWER;
            }
            /* process the request */
            if (ORTE_SUCCESS != (rc = orte_errmgr.register_job(jobid))) {
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

