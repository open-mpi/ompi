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
 *
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/event/event.h"
#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmgr/rmgr.h"

#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"

static orte_std_cntr_t orted_cmd_num_active;
static int completion_status;

static void orte_pls_base_orted_default_wakeup(int fd, short event, void *arg)
{
    /* protect for threads */
    OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
    
    /* cancel the receive - we didn't get everyone's response in time */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED_ACK);
    
    /* set the completion status to reflect timeout error */
    completion_status = ORTE_ERR_TIMEOUT;
    
    /* declare us "done" so we can exit cleanly */
    opal_condition_signal(&orte_pls_base.orted_cmd_cond);
    
    /* unlock us */
    OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);
}

static void orte_pls_base_orted_send_cb(int status,
                                        orte_process_name_t* peer,
                                        orte_buffer_t* req,
                                        orte_rml_tag_t tag,
                                        void* cbdata)
{
    /* nothing to do here - this just catches the callback when
    * the send is received on the far end
    */
    return;
}

static void orte_pls_base_cmd_ack(int status, orte_process_name_t* sender,
                                  orte_buffer_t* buffer, orte_rml_tag_t tag,
                                  void* cbdata)
{
    int ret;
    
    OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
    
    orted_cmd_num_active--;
    if (orted_cmd_num_active == 0) {
        opal_condition_signal(&orte_pls_base.orted_cmd_cond);
    } else {
        ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED_ACK,
                                      ORTE_RML_NON_PERSISTENT, orte_pls_base_cmd_ack, NULL);
        if (ret != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(ret);
            return;
        }
    }
    
    OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);
    return;
}

static int send_cmd(orte_buffer_t *cmd, struct timeval *timeout)
{
    opal_event_t* event = NULL;
    orte_vpid_t i, range;
    orte_process_name_t dummy;
    int rc;
    
    /* get the number of daemons out there */
    orte_ns.get_vpid_range(0, &range);
    
    /* send the commands as fast as we can */
    dummy.cellid = ORTE_PROC_MY_NAME->cellid;
    dummy.jobid = 0;
    for (i=0; i < range; i++) {
        
        if (ORTE_PROC_MY_NAME->vpid != i) { /* don't kill myself */
            dummy.vpid = i;
            if (0 > (rc = orte_rml.send_buffer_nb(&dummy, cmd, ORTE_RML_TAG_PLS_ORTED,
                                                  0, orte_pls_base_orted_send_cb, NULL))) {
                if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    return ORTE_ERR_COMM_FAILURE;
                }
            } else {
                /* be sure to protect the global variable */
                OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
                orted_cmd_num_active++;
                OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);
            }
        }
    }

    /* post the receive for the ack's */
    OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
    if (orted_cmd_num_active > 0) {
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED_ACK,
                                     ORTE_RML_NON_PERSISTENT, orte_pls_base_cmd_ack, NULL);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);
            return rc;
        }
    }
    OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);

    /* define the default completion status */
    completion_status = ORTE_SUCCESS;

    /* wait for all commands to have been received */
    OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
    if (orted_cmd_num_active > 0) {
        /* setup a delay to give the orteds time to complete their departure - wake us up if they
        * don't exit by the prescribed time
        */
        if (NULL != timeout &&  /* only do this if the user gave us a time to wait */
            NULL != (event = (opal_event_t*)malloc(sizeof(opal_event_t)))) {
            opal_evtimer_set(event, orte_pls_base_orted_default_wakeup, NULL);
            opal_evtimer_add(event, timeout);
        }
    /* now go to sleep until woken up */
    opal_condition_wait(&orte_pls_base.orted_cmd_cond, &orte_pls_base.orted_cmd_lock);
    }
    OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);

    /* log an error if one occurred */
    if (ORTE_SUCCESS != completion_status) {
        ORTE_ERROR_LOG(completion_status);
    }

    /* if started, kill the timer event so it doesn't hit us later */
    if (NULL != event) {
        opal_evtimer_del(event);
        free(event);
    }    

    /* we're done! */
    return completion_status;
}


static int get_jobids(orte_jobid_t **jobs, orte_std_cntr_t *num_jobs, bool *allocated,
                      orte_jobid_t job, opal_list_t *attrs)
{
    int rc;
    
    if (NULL != orte_rmgr.find_attribute(attrs, ORTE_NS_USE_JOB_FAMILY)) {
        /* need to include entire job family */
        if (ORTE_SUCCESS != (rc = orte_ns.get_job_family(jobs, num_jobs, job))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        *allocated = true;
    } else if (NULL != orte_rmgr.find_attribute(attrs, ORTE_NS_INCLUDE_DESCENDANTS)) {
        /* need to include all descendants in list */
        if (ORTE_SUCCESS != (rc = orte_ns.get_job_descendants(jobs, num_jobs, job))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        *allocated = true;
    } else if (NULL != orte_rmgr.find_attribute(attrs, ORTE_NS_INCLUDE_CHILDREN)) {
        /* just include the direct children of the job */
        if (ORTE_SUCCESS != (rc = orte_ns.get_job_children(jobs, num_jobs, job))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        *allocated = true;
    } else {
        /* just want daemons for this one job */
        *jobs = &job;
        *num_jobs = 1;
        *allocated = false;
    }
    
    return ORTE_SUCCESS;
}


int orte_pls_base_orted_cancel_operation(void)
{
    /* protect for threads */
    OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
    
    /* cancel any waiting receive - we don't want to hear it */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED_ACK);
    
    /* set the completion status to reflect cancellation -- no need to
        print anything */
    completion_status = ORTE_ERR_SILENT;
    
    /* declare us "done" so we can exit cleanly */
    opal_condition_signal(&orte_pls_base.orted_cmd_cond);
    
    /* unlock us */
    OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);
    
    return ORTE_SUCCESS;
}

int orte_pls_base_orted_exit(struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command;
    
    OPAL_TRACE(1);
    
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
    /* Default: tell the daemons to exit IF they are not part of a virtual
     * machine - i.e., they are not persistent
     */
    command = ORTE_DAEMON_EXIT_CMD;
    
    if (NULL != orte_rmgr.find_attribute(attrs, ORTE_DAEMON_HARD_KILL)) {
        /* order daemons to exit NO MATTER WHAT */
        command = ORTE_DAEMON_HALT_VM_CMD;
    }
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    
    /* send it! */
    if (ORTE_SUCCESS != (rc = send_cmd(&cmd, timeout))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&cmd);
    
    return rc;
}


int orte_pls_base_orted_kill_local_procs(orte_jobid_t job, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_KILL_LOCAL_PROCS;
    orte_jobid_t *jobs;
    orte_std_cntr_t num_jobs;
    bool allocated;
    
    OPAL_TRACE(1);
    
    /* get the array of jobs we need to kill */
    if (ORTE_SUCCESS != (rc = get_jobids(&jobs, &num_jobs, &allocated, job, attrs))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    
    /* pack the number of jobids */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &num_jobs, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    
    /* pack the array of jobids */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, jobs, num_jobs, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    if (allocated) free(jobs);  /* not needed any more */
    
    /* send it! */
    if (ORTE_SUCCESS != (rc = send_cmd(&cmd, timeout))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return rc;
}



int orte_pls_base_orted_signal_local_procs(orte_jobid_t job, int32_t signal, opal_list_t *attrs)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_SIGNAL_LOCAL_PROCS;
    orte_jobid_t *jobs;
    orte_std_cntr_t num_jobs;
    bool allocated;
    
    OPAL_TRACE(1);
    
    /* get the array of jobs we need to signal */
    if (ORTE_SUCCESS != (rc = get_jobids(&jobs, &num_jobs, &allocated, job, attrs))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    
    /* pack the number of jobids */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &num_jobs, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    
    /* pack the array of jobids */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, jobs, num_jobs, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        if (allocated) free(jobs);
        return rc;
    }
    if (allocated) free(jobs);  /* not needed any more */
    
    /* pack the signal */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &signal, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    
    /* send it! */
    if (ORTE_SUCCESS != (rc = send_cmd(&cmd, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return ORTE_SUCCESS;
}


int orte_pls_base_orted_add_local_procs(orte_gpr_notify_data_t *ndat)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_ADD_LOCAL_PROCS;
    
    OPAL_TRACE(1);
    
    /* pack the command */
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    
    /* pack the launch data for the daemons */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &ndat, 1, ORTE_GPR_NOTIFY_DATA))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    
    /* send it! */
    if (ORTE_SUCCESS != (rc = send_cmd(&cmd, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&cmd);
    
    return rc;
}

