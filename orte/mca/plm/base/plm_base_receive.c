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
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wakeup.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"

static bool recv_issued=false;

int orte_plm_base_comm_start(void)
{
    int rc;

    if (recv_issued) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive start comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_PLM,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_plm_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = true;
    
    return rc;
}


int orte_plm_base_comm_stop(void)
{
    int rc;
    
    if (!recv_issued) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive stop comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLM))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = false;
    
    return rc;
}


/* process incoming messages in order of receipt */
void orte_plm_base_receive_process_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    orte_plm_cmd_flag_t command;
    orte_std_cntr_t count;
    orte_jobid_t job;
    orte_job_t *jdata;
    opal_buffer_t answer;
    orte_vpid_t vpid;
    orte_proc_t **procs;
    orte_proc_state_t state;
    orte_exit_code_t exit_code;
    int rc, ret;
    
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &command, &count, ORTE_PLM_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    switch (command) {
        case ORTE_PLM_LAUNCH_JOB_CMD:
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:receive job launch command",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            /* setup a default response */
            OBJ_CONSTRUCT(&answer, opal_buffer_t);
            job = ORTE_JOBID_INVALID;
            
            /* get the job object */
            count = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &jdata, &count, ORTE_JOB))) {
                ORTE_ERROR_LOG(rc);
                goto ANSWER_LAUNCH;
            }
                
            /* launch it */
            if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdata))) {
                ORTE_ERROR_LOG(rc);
                goto ANSWER_LAUNCH;
            }
            job = jdata->jobid;
            
            /* if the child is an ORTE job, wait for the procs to report they are alive */
            if (!(jdata->controls & ORTE_JOB_CONTROL_NON_ORTE_JOB)) {
                ORTE_PROGRESSED_WAIT(false, jdata->num_reported, jdata->num_procs);
            }
            
        ANSWER_LAUNCH:
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:receive job %s launched",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job)));
            
            /* pack the jobid to be returned */
            if (ORTE_SUCCESS != (ret = opal_dss.pack(&answer, &job, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
            }
            
            /* send the response back to the sender */
            if (0 > (ret = orte_rml.send_buffer(&mev->sender, &answer, ORTE_RML_TAG_PLM_PROXY, 0))) {
                ORTE_ERROR_LOG(ret);
            }
            OBJ_DESTRUCT(&answer);
            break;
            
        case ORTE_PLM_UPDATE_PROC_STATE:
            count = 1;
            jdata = NULL;
            while (ORTE_SUCCESS == (rc = opal_dss.unpack(mev->buffer, &job, &count, ORTE_JOBID))) {
                
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:receive got update_proc_state for job %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOBID_PRINT(job)));
                
                /* lookup the job object */
                if (NULL == (jdata = orte_get_job_data_object(job))) {
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    return;
                }
                procs = (orte_proc_t**)jdata->procs->addr;
                count = 1;
                while (ORTE_SUCCESS == (rc = opal_dss.unpack(mev->buffer, &vpid, &count, ORTE_VPID))) {
                    if (ORTE_VPID_INVALID == vpid) {
                        /* flag indicates that this job is complete - move on */
                        break;
                    }
                    /* unpack the state */
                    count = 1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &state, &count, ORTE_PROC_STATE))) {
                        ORTE_ERROR_LOG(rc);
                        return;
                    }
                    /* unpack the exit code */
                    count = 1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &exit_code, &count, ORTE_EXIT_CODE))) {
                        ORTE_ERROR_LOG(rc);
                        return;
                    }
                    
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "%s plm:base:receive got update_proc_state for vpid %lu state %x exit_code %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         (unsigned long)vpid, (unsigned int)state, (int)exit_code));
                    
                    /* update the termination counter IFF the state is changing to something
                     * indicating terminated
                     */
                    if (ORTE_PROC_STATE_UNTERMINATED < state &&
                        ORTE_PROC_STATE_UNTERMINATED > procs[vpid]->state) {
                        ++jdata->num_terminated;
                    }
                    /* update the data */
                    procs[vpid]->state = state;
                    procs[vpid]->exit_code = exit_code;
                    
                    /* update orte's exit status if it is non-zero */
                    ORTE_UPDATE_EXIT_STATUS(exit_code);
                    
                }
                count = 1;
            }
            if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
                ORTE_ERROR_LOG(rc);
            } else {
                rc = ORTE_SUCCESS;
            }
            /* NOTE: jdata CAN BE NULL. This is caused by an orted
             * being ordered to kill all its procs, but there are no
             * procs left alive on that node. This can happen, for example,
             * when a proc aborts somewhere, but the procs on this node
             * have completed.
             * So check job has to know how to handle a NULL pointer
             */
            orte_plm_base_check_job_completed(jdata);
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
            return;
    }
    
    /* release the message */
    OBJ_RELEASE(mev);
    
    /* see if an error occurred - if so, wakeup so we can exit */
    if (ORTE_SUCCESS != rc) {
        orte_wakeup();
    }
}

/*
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

void orte_plm_base_recv(int status, orte_process_name_t* sender,
                        opal_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, orte_plm_base_receive_process_msg);

    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_PLM,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_plm_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}

