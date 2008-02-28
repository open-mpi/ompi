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

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wakeup.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"

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
                                                      ORTE_RML_PERSISTENT,
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


/*
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

void orte_plm_base_recv(int status, orte_process_name_t* sender,
                        opal_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
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

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &command, &count, ORTE_PLM_CMD))) {
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
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &jdata, &count, ORTE_JOB))) {
                ORTE_ERROR_LOG(rc);
                goto ANSWER_LAUNCH;
            }
            
            /* launch it */
            if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdata))) {
                ORTE_ERROR_LOG(rc);
                goto ANSWER_LAUNCH;
            }
            job = jdata->jobid;
            
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:receive job %s launched",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job)));
            
ANSWER_LAUNCH:
            /* pack the jobid to be returned */
            if (ORTE_SUCCESS != (ret = opal_dss.pack(&answer, &job, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
            }

            /* send the response back to the sender */
            if (0 > (ret = orte_rml.send_buffer(sender, &answer, ORTE_RML_TAG_PLM_PROXY, 0))) {
                ORTE_ERROR_LOG(ret);
            }
            OBJ_DESTRUCT(&answer);
            break;
            
        case ORTE_PLM_UPDATE_PROC_STATE:
            count = 1;
            jdata = NULL;
            while (ORTE_SUCCESS == (rc = opal_dss.unpack(buffer, &job, &count, ORTE_JOBID))) {

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
                while (ORTE_SUCCESS == (rc = opal_dss.unpack(buffer, &vpid, &count, ORTE_VPID))) {
                    if (ORTE_VPID_INVALID == vpid) {
                        /* flag indicates that this job is complete - move on */
                        break;
                    }
                    /* unpack the state */
                    count = 1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &state, &count, ORTE_PROC_STATE))) {
                        ORTE_ERROR_LOG(rc);
                        return;
                    }
                    /* unpack the exit code */
                    count = 1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &exit_code, &count, ORTE_EXIT_CODE))) {
                        ORTE_ERROR_LOG(rc);
                        return;
                    }
                    
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "%s plm:base:receive got update_proc_state for vpid %lu state %lu exit_code %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         (unsigned long)vpid, (unsigned long)state, (int)exit_code));
                    
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
                }
                count = 1;
            }
            if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
                ORTE_ERROR_LOG(rc);
            }
            /* NOTE: jdata CAN BE NULL. This is caused by an orted
             * being ordered to kill all its procs, but there are no
             * procs left alive on that node. This can happen, for example,
             * when a proc aborts somewhere, but the procs on this node
             * have completed.
             * So check job has to know how to handle a NULL pointer
             */
            orte_plm_base_check_job_completed(jdata);
            return;  /* we do not send a response for this operation */
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
            return;
    }
    
    
    /* see if an error occurred - if so, wakeup so we can exit */
    if (ORTE_SUCCESS != rc) {
        orte_wakeup(1);
    }
    
    return;
}

