/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/util/opal_sos.h"
#include "opal/dss/dss.h"
#include "opal/threads/threads.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/routed/base/base.h"

static bool sync_recvd;

static void report_sync(int status, orte_process_name_t* sender,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tag, void *cbdata)
{
    /* just copy the payload to the sync_buf */
    opal_dss.copy_payload(orte_process_info.sync_buf, buffer);
    /* flag as complete */
    sync_recvd = true;
}

int orte_routed_base_register_sync(bool setup)
{
    opal_buffer_t buffer;
    int rc;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_SYNC_BY_PROC;
    char *rml_uri;
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s registering sync to daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(ORTE_PROC_MY_DAEMON)));
    
    /* we need to get the oob to establish
     * the connection - the oob will leave the connection "alive"
     * thereafter so we can communicate readily
     */
    
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);
    
    /* if we are setting up, tell the daemon to send back a nidmap */
    if (setup) {
        command = ORTE_DAEMON_SYNC_WANT_NIDMAP;
    }


    /* tell the daemon to sync */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }
    
    /* add our contact info to the buffer so the daemon can explicitly
     * store it
     */
    rml_uri = orte_rml.get_contact_info();
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &rml_uri, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        free(rml_uri);
        return rc;
    }
    if (NULL != rml_uri) free(rml_uri);
    
    /* send the sync command to our daemon */
    if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buffer, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }
    OBJ_DESTRUCT(&buffer);
    
    /* get the ack - need this to ensure that the sync communication
     * gets serviced by the event library on the orted prior to the
     * process exiting
     */
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s registering sync waiting for ack",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    sync_recvd = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SYNC,
                                 ORTE_RML_NON_PERSISTENT, report_sync, NULL);
    if (rc != ORTE_SUCCESS && OPAL_SOS_GET_ERROR_CODE(rc) != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(sync_recvd, 0, 1);
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s registering sync ack recvd",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return ORTE_SUCCESS;
}

int orte_routed_base_process_callback(orte_jobid_t job, opal_buffer_t *buffer)
{
    orte_proc_t *proc;
    orte_job_t *jdata;
    orte_std_cntr_t cnt;
    char *rml_uri;
    orte_vpid_t vpid;
    orte_epoch_t epoch;
    int rc;

    if (ORTE_JOB_FAMILY(job) == ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        /* came from singleton - don't process it */
        return ORTE_SUCCESS;
    }

    /* lookup the job object for this process */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* came from my job family - this is an error */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* unpack the data for each entry */
    cnt = 1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(buffer, &vpid, &cnt, ORTE_VPID))) {

        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &epoch, &cnt, ORTE_EPOCH))) {
            ORTE_ERROR_LOG(rc);
            continue;
        }        
        
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &rml_uri, &cnt, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            continue;
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed_binomial:callback got uri %s for job %s rank %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == rml_uri) ? "NULL" : rml_uri,
                             ORTE_JOBID_PRINT(job), ORTE_VPID_PRINT(vpid)));
        
        if (NULL == rml_uri) {
            /* should not happen */
            ORTE_ERROR_LOG(ORTE_ERR_FATAL);
            return ORTE_ERR_FATAL;
        }
        
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            continue;
        }
        
        /* update the record */
        proc->rml_uri = strdup(rml_uri);
        free(rml_uri);
        
        /* update the proc state */
        orte_errmgr.update_state(job, ORTE_JOB_STATE_UNDEF,
                                 &proc->name, ORTE_PROC_STATE_RUNNING, 0, 0);
        cnt = 1;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != OPAL_SOS_GET_ERROR_CODE(rc)) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }    

    return ORTE_SUCCESS;    
}
