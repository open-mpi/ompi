/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include "opal/util/argv.h"
#include "opal/runtime/opal_progress.h"
#include "opal/class/opal_pointer_array.h"

#include "opal/dss/dss.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/odls/odls.h"
#if OPAL_ENABLE_FT == 1
#include "orte/mca/snapc/snapc.h"
#endif
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_locks.h"
#include "orte/runtime/orte_wait.h"

#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"

#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"

static int orte_plm_base_report_launched(orte_jobid_t job);

int orte_plm_base_setup_job(orte_job_t *jdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:setup_job for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));

    /* insert the job object into the global pool */
    opal_pointer_array_add(orte_job_data, jdata);
    
    if (ORTE_SUCCESS != (rc = orte_ras.allocate(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }         

    if (ORTE_SUCCESS != (rc = orte_rmaps.map_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }         

#if 0
    /* RHC: Please leave this code here - it is needed for
     * rare debugging that doesn't merit a separate debug-flag,
     * but is a pain to have to replicate when needed
     */
    {
        opal_byte_object_t bo;
        int i;
        orte_nid_t **nodes;
        
        /* construct a nodemap */
        if (ORTE_SUCCESS != (rc = orte_util_encode_nodemap(&bo))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* construct the daemon map, if required - the decode function
         * knows what to do
         */
        if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(&bo, &orte_daemonmap))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* print-out the map */
        nodes = (orte_nid_t**)orte_daemonmap.addr;
        for (i=0; i < orte_daemonmap.size; i++) {
            if (NULL != nodes[i]) {
                fprintf(stderr, "NIDMAP: name %s daemon %s arch %0x\n",
                        nodes[i]->name, ORTE_VPID_PRINT(nodes[i]->daemon), nodes[i]->arch);
            }
        }
    }
#endif
    
    /* if we don't want to launch, now is the time to leave */
    if (orte_do_not_launch) {
        orte_finalize();
        exit(0);
    }
    
    /* quick sanity check - is the stdin target within range
     * of the job?
     */
    if (ORTE_VPID_WILDCARD != jdata->stdin_target &&
        ORTE_VPID_INVALID != jdata->stdin_target &&
        jdata->num_procs <= jdata->stdin_target) {
        /* this request cannot be met */
        orte_show_help("help-plm-base.txt", "stdin-target-out-of-range", true,
                       ORTE_VPID_PRINT(jdata->stdin_target),
                       ORTE_VPID_PRINT(jdata->num_procs));
        orte_finalize();
        exit(ORTE_ERROR_DEFAULT_EXIT_CODE);
    }
    
    /*** RHC: USER REQUEST TO TIE-OFF STDXXX TO /DEV/NULL
     *** WILL BE SENT IN LAUNCH MESSAGE AS PART OF CONTROLS FIELD.
     *** SO IF USER WANTS NO IO BEING SENT AROUND, THE ORTEDS
     *** WILL TIE IT OFF AND THE IOF WILL NEVER RECEIVE ANYTHING.
     *** THE IOF AUTOMATICALLY KNOWS TO OUTPUT ANY STDXXX
     *** DATA IT -DOES- RECEIVE TO THE APPROPRIATE FD, SO THERE
     *** IS NOTHING WE NEED DO HERE TO SETUP IOF
     ***/
    
#if OPAL_ENABLE_FT == 1
    /*
     * Notify the Global SnapC component regarding new job
     */
    if( ORTE_SUCCESS != (rc = orte_snapc.setup_job(jdata->jobid) ) ) {
        /* Silent Failure :/ JJH */
        ORTE_ERROR_LOG(rc);
    }
#endif
    
    return ORTE_SUCCESS;
}

int orte_plm_base_launch_apps(orte_jobid_t job)
{
    orte_job_t *jdata;
    orte_daemon_cmd_flag_t command;
    opal_buffer_t *buffer;
    int rc;
    orte_process_name_t name = {ORTE_JOBID_INVALID, 0};

    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch_apps for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));

    /* find the job's data record */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* bad jobid */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* setup the buffer */
    buffer = OBJ_NEW(opal_buffer_t);

    /* pack the add_local_procs command */
    command = ORTE_DAEMON_ADD_LOCAL_PROCS;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }

    /* get the local launcher's required data */
    if (ORTE_SUCCESS != (rc = orte_odls.get_add_procs_data(buffer, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* send the command to the daemons */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid,
                                                 buffer, ORTE_RML_TAG_DAEMON))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }
    OBJ_RELEASE(buffer);
    
    /* wait for all the daemons to report apps launched */
    if (ORTE_SUCCESS != (rc = orte_plm_base_report_launched(job))) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:launch failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job), ORTE_ERROR_NAME(rc)));
        return rc;
    }
    
    /* complete wiring up the iof */
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch wiring up iof",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* push stdin - the IOF will know what to do with the specified target */
    name.jobid = job;
    name.vpid = jdata->stdin_target;
    
    if (ORTE_SUCCESS != (rc = orte_iof.push(&name, ORTE_IOF_STDIN, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch completed for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));
    return rc;
}

/* daemons callback when they start - need to listen for them */
static int orted_num_callback;
static bool orted_failed_launch;
static orte_job_t *jdatorted;
static orte_proc_t **pdatorted;

void orte_plm_base_launch_failed(orte_jobid_t job, pid_t pid,
                                 int status, orte_job_state_t state)
{
    orte_job_t *jdata;
    char *pidstr;
    int sts;
    
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:base:launch_failed abort in progress, ignoring report",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch_failed for job %s, status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), status));

    /* no matter what, we must exit with a non-zero status */
    if (0 == status) {
        sts = ORTE_ERROR_DEFAULT_EXIT_CODE;
    } else {
        sts = status;
    }
    
    /* if this is the daemon job that failed, set the flag indicating
     * that a daemon failed so we use the proper
     * methods for attempting to shutdown the rest of the system
     */
    if (ORTE_PROC_MY_NAME->jobid == job) {
        /* set the flag indicating that a daemon failed so we use the proper
         * methods for attempting to shutdown the rest of the system
         */
        orte_abnormal_term_ordered = true;
        if (0 < pid) {
            asprintf(&pidstr, "%d", (int)pid);
        } else {
            /* if the pid is negative, then we couldn't get a real pid
             * to report here - so tell someone that
             */
            pidstr = strdup("unknown");
        }
        if (WIFSIGNALED(status)) { /* died on signal */
#ifdef WCOREDUMP
            if (WCOREDUMP(status)) {
                orte_show_help("help-plm-base.txt", "daemon-died-signal-core", true,
                               pidstr, WTERMSIG(status));
                sts = WTERMSIG(status);
            } else {
                orte_show_help("help-plm-base.txt", "daemon-died-signal", true,
                               pidstr, WTERMSIG(status));
                sts = WTERMSIG(status);
            }
#else
            orte_show_help("help-plm-base.txt", "daemon-died-signal", true,
                            pidstr, WTERMSIG(status));
            sts = WTERMSIG(status);
#endif /* WCOREDUMP */
        } else {
            orte_show_help("help-plm-base.txt", "daemon-died-no-signal", true,
                           pidstr, WEXITSTATUS(status));
            sts = WEXITSTATUS(status);
        }
        orted_failed_launch = true;
        free(pidstr);
   }
    
    
    /* Set the job state as indicated so orterun's exit status
       will be non-zero
     */
    /* find the job's data record */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* bad jobid */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        goto WAKEUP;
    }
    /* set the state */
    jdata->state = state;
    
WAKEUP:
    /* set orterun's exit code and wakeup so it can exit */
    ORTE_UPDATE_EXIT_STATUS(sts);
    orte_trigger_event(&orte_exit);
}

static void process_orted_launch_report(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    opal_buffer_t *buffer = mev->buffer;
    char *rml_uri;
    int rc, idx;
    int32_t arch;
    orte_node_t **nodes;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:orted_report_launch from daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&mev->sender)));
    
    /* update state */
    pdatorted[mev->sender.vpid]->state = ORTE_PROC_STATE_RUNNING;

    /* unpack its contact info */
    idx = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &rml_uri, &idx, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
        goto CLEANUP;
    }

    /* set the contact info into the hash table */
    if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
        ORTE_ERROR_LOG(rc);
        free(rml_uri);
        orted_failed_launch = true;
        goto CLEANUP;
    }
    /* lookup and record this daemon's contact info */
    pdatorted[mev->sender.vpid]->rml_uri = strdup(rml_uri);
    free(rml_uri);

    /* set the route to be direct */
    if (ORTE_SUCCESS != (rc = orte_routed.update_route(&mev->sender, &mev->sender))) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
        goto CLEANUP;
    }

    /* get the remote arch */
    idx = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &arch, &idx, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
        goto CLEANUP;
    }
    /* lookup the node */
    nodes = (orte_node_t**)orte_node_pool->addr;
    if (NULL == nodes[mev->sender.vpid]) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        orted_failed_launch = true;
        goto CLEANUP;
    }
    /* store the arch */
    nodes[mev->sender.vpid]->arch = arch;
    
    /* if a tree-launch is underway, send the cmd back */
    if (NULL != orte_tree_launch_cmd) {
        orte_rml.send_buffer(&mev->sender, orte_tree_launch_cmd, ORTE_RML_TAG_DAEMON, 0);
    }
    
CLEANUP:

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:orted_report_launch %s for daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orted_failed_launch ? "failed" : "completed",
                         ORTE_NAME_PRINT(&mev->sender)));

    if (orted_failed_launch) {
        orte_errmgr.incomplete_start(ORTE_PROC_MY_NAME->jobid, ORTE_ERROR_DEFAULT_EXIT_CODE);
    } else {
        orted_num_callback++;
    }

}

static void orted_report_launch(int status, orte_process_name_t* sender,
                                opal_buffer_t *buffer,
                                orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release when processed - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_orted_launch_report);
    
    /* reissue the recv */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK,
                                 ORTE_RML_NON_PERSISTENT, orted_report_launch, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
    }
}

    
int orte_plm_base_daemon_callback(orte_std_cntr_t num_daemons)
{
    int rc;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:daemon_callback",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    orted_num_callback = 0;
    orted_failed_launch = false;
    /* get the orted job data object */
    if (NULL == (jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    pdatorted = (orte_proc_t**)(jdatorted->procs->addr);

    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK,
                                 ORTE_RML_NON_PERSISTENT, orted_report_launch, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(orted_failed_launch, orted_num_callback, num_daemons);
    
    /* cancel the lingering recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:daemon_callback completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* all done launching - update the num_procs in my local structure if required
     * so that any subsequent communications are correctly routed
     */
    if (orte_process_info.num_procs != jdatorted->num_procs) {
        orte_process_info.num_procs = jdatorted->num_procs;
        /* update the routing tree */
        if (ORTE_SUCCESS != (rc = orte_routed.update_routing_tree())) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    /* if a tree-launch was underway, clear out the cmd */
    if (NULL != orte_tree_launch_cmd) {
        OBJ_RELEASE(orte_tree_launch_cmd);
    }
    
    return ORTE_SUCCESS;
}

/* the daemons actually report back that their procs have launched. Each
 * daemon will only send one message that contains the launch result
 * for their local procs.
 */
static bool app_launch_failed;
static orte_vpid_t num_daemons_reported=0;
static opal_event_t *dmn_report_ev=NULL;

/* catch timeout to allow cmds to progress */
static void timer_cb(int fd, short event, void *cbdata)
{
    /* free event */
    if (NULL != dmn_report_ev) {
        free(dmn_report_ev);
        dmn_report_ev = NULL;
    }
    /* declare time is up */
    app_launch_failed = true;
}

/* since the HNP also reports launch of procs, we need to separate out
 * the processing of the message vs its receipt so that the HNP
 * can call the processing part directly
 */
void orte_plm_base_app_report_launch(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    opal_buffer_t *buffer = mev->buffer;
    orte_std_cntr_t cnt;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    orte_proc_state_t state;
    orte_exit_code_t exit_code;
    pid_t pid;
    orte_job_t *jdata;
    orte_proc_t **procs;
    orte_process_name_t proc;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:app_report_launch from daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&mev->sender)));
    
    /* got a response - cancel the timer */
    if (NULL != dmn_report_ev) {
        opal_event_del(dmn_report_ev);
        free(dmn_report_ev);
        dmn_report_ev = NULL;
    }
    
    /* unpack the jobid being reported */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &jobid, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        app_launch_failed = true;
        orte_errmgr.incomplete_start(-1, -1); /* no way to know the jobid or exit code */
        return;
    }
    /* if the jobid is invalid, then we know that this is a failed
     * launch report from before we could even attempt to launch the
     * procs - most likely, while we were attempting to unpack the
     * launch cmd itself. In this case, just abort
     */
    if (ORTE_JOBID_INVALID == jobid) {
        jdata = NULL;
        app_launch_failed = true;
        goto CLEANUP;
    }
    
    num_daemons_reported++;

    /* get the job data object */
    if (NULL == (jdata = orte_get_job_data_object(jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        app_launch_failed = true;
        goto CLEANUP;
    }
    procs = (orte_proc_t**)(jdata->procs->addr);
    
    /* setup the process name */
    proc.jobid = jobid;
    
    /* the daemon will report the vpid, state, and pid of each
     * process it launches - we need the pid in particular so
     * that any debuggers can attach to the process
     */
    cnt = 1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(buffer, &vpid, &cnt, ORTE_VPID))) {
        if (ORTE_VPID_INVALID == vpid) {
            /* flag indicating we are done */
            break;
        }
        /* unpack the pid */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &pid, &cnt, OPAL_PID))) {
            ORTE_ERROR_LOG(rc);
            app_launch_failed = true;
            goto CLEANUP;
        }
        /* unpack the state */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &state, &cnt, ORTE_PROC_STATE))) {
            ORTE_ERROR_LOG(rc);
            app_launch_failed = true;
            goto CLEANUP;
        }
        /* unpack the exit code */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &exit_code, &cnt, ORTE_EXIT_CODE))) {
            ORTE_ERROR_LOG(rc);
            app_launch_failed = true;
            goto CLEANUP;
        }
        
        /* it is possible for a race condition to exist when the HNP does not have
         * local procs whereby the HNP will need to communicate to a remote
         * proc before it decodes the launch message itself and sets all the routes.
         * This has been seen in cases where no local procs are launched and
         * a debugger needs to attach to the job.
         * To support that situation, go ahead and update the route here
         */
        proc.vpid = vpid;
        /* if the sender is me, the route is direct to avoid infinite loops. We
         * know the jobid is the same since the sender was another daemon
         */
        if (mev->sender.vpid == ORTE_PROC_MY_NAME->vpid) {
            orte_routed.update_route(&proc, &proc);
        } else {
            orte_routed.update_route(&proc, &mev->sender);
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:app_report_launched for proc %s from daemon %s: pid %lu state %0x exit %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&(procs[vpid]->name)),
                             ORTE_NAME_PRINT(&mev->sender), (unsigned long)pid,
                             (int)state, (int)exit_code));
        
        /* lookup the proc and update values */
        procs[vpid]->pid = pid;
        procs[vpid]->state = state;
        procs[vpid]->exit_code = exit_code;
        if (ORTE_PROC_STATE_FAILED_TO_START == state) {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:app_report_launched daemon %s reports proc %s failed to start",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&mev->sender),
                                 ORTE_NAME_PRINT(&(procs[vpid]->name))));
            if (NULL == jdata->aborted_proc) {
                jdata->aborted_proc = procs[vpid];  /* only store this once */
                jdata->state = ORTE_JOB_STATE_FAILED_TO_START; /* update the job state */
            }
            /* ensure we have a non-zero exit code */
            if (0 == jdata->aborted_proc->exit_code) {
                jdata->aborted_proc->exit_code = ORTE_ERROR_DEFAULT_EXIT_CODE;
            }
            app_launch_failed = true;
            goto CLEANUP;
        }
        
        /* record that a proc reported */
        jdata->num_launched++;
    }
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    
    if (orte_report_launch_progress) {
        if (0 == num_daemons_reported % 100 || num_daemons_reported == orte_process_info.num_procs) {
            opal_output(orte_clean_output, "Reported: %d (out of %d) daemons -  %d (out of %d) procs",
                        (int)num_daemons_reported, (int)orte_process_info.num_procs,
                        (int)jdata->num_launched, (int)jdata->num_procs);
        }
    }

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:app_report_launch completed processing",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
CLEANUP:
    if (app_launch_failed) {
        if (NULL == jdata) {
            orte_errmgr.incomplete_start(ORTE_JOBID_INVALID, ORTE_ERROR_DEFAULT_EXIT_CODE);
        } else if (NULL == jdata->aborted_proc) {
            orte_errmgr.incomplete_start(jdata->jobid, ORTE_ERROR_DEFAULT_EXIT_CODE);
        } else {
            orte_errmgr.incomplete_start(jdata->jobid, jdata->aborted_proc->exit_code);
        }
    }

    /* restart the timer, if necessary */
    if (jdata->num_launched < jdata->num_procs && 0 < orte_startup_timeout) {
        ORTE_DETECT_TIMEOUT(&dmn_report_ev, orte_startup_timeout, 1000, 10000000, timer_cb);
    }
}


static void app_report_launch(int status, orte_process_name_t* sender,
                              opal_buffer_t *buffer,
                              orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release when processed - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, orte_plm_base_app_report_launch);

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:app_report_launch reissuing non-blocking recv",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* reissue the non-blocking receive */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_APP_LAUNCH_CALLBACK,
                                  ORTE_RML_NON_PERSISTENT, app_report_launch, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        app_launch_failed = true;
    }

}

static int orte_plm_base_report_launched(orte_jobid_t job)
{
    int rc;
    orte_job_t *jdata;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:report_launched for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));

    /* get the job data object */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* setup a timer - if we don't hear back from a daemon in the
     * defined time, then we know things have failed
     */
    if (0 < orte_startup_timeout) {
        ORTE_DETECT_TIMEOUT(&dmn_report_ev, orte_startup_timeout, 1000, 10000000, timer_cb);
    }

    /* we should get a callback from every daemon that is involved in
     * the launch. Fortunately, the mapper keeps track of this number
     * for us since num_nodes = num_participating_daemons
     */
    app_launch_failed = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_APP_LAUNCH_CALLBACK,
                                 ORTE_RML_NON_PERSISTENT, app_report_launch, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(app_launch_failed, jdata->num_launched, jdata->num_procs);

    /* cancel the lingering recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_APP_LAUNCH_CALLBACK))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:report_launched all apps reported",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* declare the job to be launched, but check to ensure
     * the procs haven't already reported in to avoid setting the
     * job back to an earlier state
     */
    if (jdata->state < ORTE_JOB_STATE_LAUNCHED) {
        jdata->state = ORTE_JOB_STATE_LAUNCHED;
    }
    
    return ORTE_SUCCESS;
}

int orte_plm_base_setup_orted_cmd(int *argc, char ***argv)
{
    int i, loc;
    char **tmpv;
    
    /* set default location */
    loc = -1;
    /* split the command apart in case it is multi-word */
    tmpv = opal_argv_split(orte_launch_agent, ' ');
    for (i = 0; NULL != tmpv && NULL != tmpv[i]; ++i) {
        if (0 == strcmp(tmpv[i], "orted")) {
            loc = i;
        }
        opal_argv_append(argc, argv, tmpv[i]);
    }
    opal_argv_free(tmpv);
    
    return loc;
}


int orte_plm_base_orted_append_basic_args(int *argc, char ***argv,
                                          char *ess,
                                          int *proc_vpid_index,
                                          bool heartbeat)
{
    char *param = NULL;
    int loc_id;
    char * amca_param_path = NULL;
    char * amca_param_prefix = NULL;
    char * tmp_force = NULL;
    int i, cnt;
    orte_job_t *jdata;
    char *rml_uri;
    unsigned long num_procs;
    
    /* check for debug flags */
    if (orte_debug_flag) {
        opal_argv_append(argc, argv, "--debug");
    }
    if (orte_debug_daemons_flag) {
        opal_argv_append(argc, argv, "--debug-daemons");
    }
    if (orte_debug_daemons_file_flag) {
        opal_argv_append(argc, argv, "--debug-daemons-file");
    }
    if (orted_spin_flag) {
        opal_argv_append(argc, argv, "--spin");
    }
    if ((int)ORTE_VPID_INVALID != orted_debug_failure) {
        opal_argv_append(argc, argv, "--debug-failure");
        asprintf(&param, "%d", orted_debug_failure);
        opal_argv_append(argc, argv, param);
        free(param);
    }
    if (0 < orted_debug_failure_delay) {
        opal_argv_append(argc, argv, "--debug-failure-delay");
        asprintf(&param, "%d", orted_debug_failure_delay);
        opal_argv_append(argc, argv, param);
        free(param);
    }
    if (heartbeat && 0 < orte_heartbeat_rate) {
        /* tell the daemon to do a heartbeat */
        opal_argv_append(argc, argv, "--heartbeat");
        asprintf(&param, "%d", orte_heartbeat_rate);
        opal_argv_append(argc, argv, param);
        free(param);
    }
    
    /* tell the orted what SDS component to use */
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "ess");
    opal_argv_append(argc, argv, ess);
    
    /* pass the daemon jobid */
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "orte_ess_jobid");
    orte_util_convert_jobid_to_string(&param, ORTE_PROC_MY_NAME->jobid);
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* setup to pass the vpid */
    if (NULL != proc_vpid_index) {
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "orte_ess_vpid");
        *proc_vpid_index = *argc;
        opal_argv_append(argc, argv, "<template>");
    }
    
    /* pass the total number of daemons that will be in the system */
    if (orte_process_info.hnp) {
        jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
        num_procs = jdata->num_procs;
    } else {
        num_procs = orte_process_info.num_procs;
    }
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "orte_ess_num_procs");
    asprintf(&param, "%lu", num_procs);
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* pass the uri of the hnp */
    if (orte_process_info.hnp) {
        rml_uri = orte_rml.get_contact_info();
    } else {
        rml_uri = orte_process_info.my_hnp_uri;
    }
    asprintf(&param, "\"%s\"", rml_uri);
    opal_argv_append(argc, argv, "--hnp-uri");
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* pass along any cmd line MCA params provided to mpirun,
     * being sure to "purge" any that would cause problems
     * on backend nodes
     */
    if (orte_process_info.hnp) {
        cnt = opal_argv_count(orted_cmd_line);    
        for (i=0; i < cnt; i+=3) {
            /* if the specified option is more than one word, we don't
             * have a generic way of passing it as some environments ignore
             * any quotes we add, while others don't - so we ignore any
             * such options. In most cases, this won't be a problem as
             * they typically only apply to things of interest to the HNP.
             * Individual environments can add these back into the cmd line
             * as they know if it can be supported
             */
            if (NULL != strchr(orted_cmd_line[i+2], ' ')) {
                continue;
            }
            /* The daemon will attempt to open the PLM on the remote
             * end. Only a few environments allow this, so the daemon
             * only opens the PLM -if- it is specifically told to do
             * so by giving it a specific PLM module. To ensure we avoid
             * confusion, do not include any directives here
             */
            if (0 == strcmp(orted_cmd_line[i+1], "plm")) {
                continue;
            }
            /* must be okay - pass it along */
            opal_argv_append(argc, argv, orted_cmd_line[i]);
            opal_argv_append(argc, argv, orted_cmd_line[i+1]);
            opal_argv_append(argc, argv, orted_cmd_line[i+2]);
        }
    }

    /* if output-filename was specified, pass that along */
    if (NULL != orte_output_filename) {
        opal_argv_append(argc, argv, "--output-filename");
        opal_argv_append(argc, argv, orte_output_filename);
    }
    
    /* if --xterm was specified, pass that along */
    if (NULL != orte_xterm) {
        opal_argv_append(argc, argv, "--xterm");
        opal_argv_append(argc, argv, orte_xterm);
    }
    
    /* 
     * Pass along the Aggregate MCA Parameter Sets
     */
    /* Add the 'prefix' param */
    loc_id = mca_base_param_find("mca", NULL, "base_param_file_prefix");
    mca_base_param_lookup_string(loc_id, &amca_param_prefix);
    if( NULL != amca_param_prefix ) {
        /* Could also use the short version '-am'
        * but being verbose has some value
        */
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "mca_base_param_file_prefix");
        opal_argv_append(argc, argv, amca_param_prefix);
    
        /* Add the 'path' param */
        loc_id = mca_base_param_find("mca", NULL, "base_param_file_path");
        mca_base_param_lookup_string(loc_id, &amca_param_path);
        if( NULL != amca_param_path ) {
            opal_argv_append(argc, argv, "-mca");
            opal_argv_append(argc, argv, "mca_base_param_file_path");
            opal_argv_append(argc, argv, amca_param_path);
        }
    
        /* Add the 'path' param */
        loc_id = mca_base_param_find("mca", NULL, "base_param_file_path_force");
        mca_base_param_lookup_string(loc_id, &tmp_force);
        if( NULL == tmp_force ) {
            /* Get the current working directory */
            tmp_force = (char *) malloc(sizeof(char) * OMPI_PATH_MAX);
            if( NULL == (tmp_force = getcwd(tmp_force, OMPI_PATH_MAX) )) {
                tmp_force = strdup("");
            }
        }
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "mca_base_param_file_path_force");
        opal_argv_append(argc, argv, tmp_force);
    
        free(tmp_force);
    
        if( NULL != amca_param_path ) {
            free(amca_param_path);
            amca_param_path = NULL;
        }

        if( NULL != amca_param_prefix ) {
            free(amca_param_prefix);
            amca_param_prefix = NULL;
        }
    }

    return ORTE_SUCCESS;
}

void orte_plm_base_check_job_completed(orte_job_t *jdata)
{
    orte_proc_t **procs;
    orte_vpid_t i;
    orte_std_cntr_t j;
    orte_job_t **jobs;
    
    /* if the incoming job data pointer is NULL, then all we can do
     * is check all jobs for complete
     */
    if (NULL == jdata) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed called with NULL pointer",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto CHECK_ALL_JOBS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:check_job_completed for job %s - num_terminated %lu  num_procs %lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid),
                         (unsigned long)jdata->num_terminated,
                         (unsigned long)jdata->num_procs));

    procs = (orte_proc_t**)jdata->procs->addr;
    
    /* if this job was ordered to abort, or if its state was already recorded
     * as abnormally terminated, then do not update its state
     */
    if (jdata->state < ORTE_JOB_STATE_TERMINATED) {
        for (i=0; i < jdata->num_procs; i++) {
            if (ORTE_PROC_STATE_FAILED_TO_START == procs[i]->state) {
                jdata->state = ORTE_JOB_STATE_FAILED_TO_START;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = procs[i];
                    jdata->abort = true;
                    ORTE_UPDATE_EXIT_STATUS(procs[i]->exit_code);
                }
                break;
            } else if (ORTE_PROC_STATE_ABORTED == procs[i]->state) {
                jdata->state = ORTE_JOB_STATE_ABORTED;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = procs[i];
                    jdata->abort = true;
                    ORTE_UPDATE_EXIT_STATUS(procs[i]->exit_code);
                }
                break;
            } else if (ORTE_PROC_STATE_ABORTED_BY_SIG == procs[i]->state) {
                jdata->state = ORTE_JOB_STATE_ABORTED_BY_SIG;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = procs[i];
                    jdata->abort = true;
                    ORTE_UPDATE_EXIT_STATUS(procs[i]->exit_code);
                }
                break;
            } else if (ORTE_PROC_STATE_TERM_WO_SYNC == procs[i]->state) {
                jdata->state = ORTE_JOB_STATE_ABORTED_WO_SYNC;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = procs[i];
                    jdata->abort = true;
                    ORTE_UPDATE_EXIT_STATUS(procs[i]->exit_code);
                    /* now treat a special case - if the proc exit'd without a required
                     * sync, it may have done so with a zero exit code. We want to ensure
                     * that the user realizes there was an error, so in this -one- case,
                     * we overwrite the process' exit code with a '1'
                     */
                    if (ORTE_PROC_STATE_TERM_WO_SYNC == procs[i]->state) {
                        ORTE_UPDATE_EXIT_STATUS(1);
                    }
                }
                break;
            }
            
        }
    }

    /* check the resulting job state and notify the appropriate places */
    
    if (ORTE_JOB_STATE_FAILED_TO_START == jdata->state) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed declared job %s failed to start by proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid),
                             (NULL == jdata->aborted_proc) ? "unknown" : ORTE_NAME_PRINT(&(jdata->aborted_proc->name))));
        /* report this to the errmgr - it will protect us from multiple calls */
        if (NULL == jdata->aborted_proc) {
            /* we don't know who caused us to abort */
            orte_errmgr.incomplete_start(jdata->jobid, ORTE_ERROR_DEFAULT_EXIT_CODE);
        } else {
            orte_errmgr.incomplete_start(jdata->jobid, jdata->aborted_proc->exit_code);
        }
        goto CHECK_ALL_JOBS;
    } else if (ORTE_JOB_STATE_ABORTED == jdata->state ||
               ORTE_JOB_STATE_ABORTED_BY_SIG == jdata->state ||
               ORTE_JOB_STATE_ABORTED_WO_SYNC == jdata->state) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed declared job %s aborted by proc %s with code %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid),
                             (NULL == jdata->aborted_proc) ? "unknown" : ORTE_NAME_PRINT(&(jdata->aborted_proc->name)),
                             (NULL == jdata->aborted_proc) ? ORTE_ERROR_DEFAULT_EXIT_CODE : jdata->aborted_proc->exit_code));
        /* report this to the errmgr */
        if (NULL == jdata->aborted_proc) {
            /* we don't know who caused us to abort */
            orte_errmgr.proc_aborted(ORTE_NAME_INVALID, ORTE_ERROR_DEFAULT_EXIT_CODE);
        } else {
            orte_errmgr.proc_aborted(&(jdata->aborted_proc->name), jdata->aborted_proc->exit_code);
        }
        goto CHECK_ALL_JOBS;
    } else if (jdata->num_terminated >= jdata->num_procs) {
        /* this job has terminated - now we need to check to see if ALL
         * the other jobs have also completed and wakeup if that is true
         */
        jdata->state = ORTE_JOB_STATE_TERMINATED;
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed declared job %s normally terminated - checking all jobs",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid)));

CHECK_ALL_JOBS:
        /* if the job that is being checked is the HNP, then we are
         * trying to terminate the orteds. In that situation, we
         * do -not- check all jobs - we simply notify the HNP
         * that the orteds are complete. Also check special case
         * if jdata is NULL - we want
         * to definitely declare the job done if the orteds
         * have completed, no matter what else may be happening.
         * This can happen if a ctrl-c hits in the "wrong" place
         * while launching
         */
        if (jdata == NULL || jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
            jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            if (jdata->num_terminated >= jdata->num_procs) {
                /* orteds are done! */
                jdata->state = ORTE_JOB_STATE_TERMINATED;
                orte_trigger_event(&orteds_exit);
                return;
            }
        }
        jobs = (orte_job_t**)orte_job_data->addr;
        for (j=0; j < orte_job_data->size; j++) {
            if (NULL == jobs[j]) {
                /* the jobs are left-justified in the array, so
                 * if we find a NULL, that means we are past all
                 * the jobs so we can just quit the loop
                 */
                break;
            }
            /* when checking for job termination, we must be sure to NOT check
             * our own job as it - rather obviously - has NOT terminated!
             */
            if (ORTE_PROC_MY_NAME->jobid != jobs[j]->jobid &&
                jobs[j]->num_terminated < jobs[j]->num_procs) {
                /* we have at least one job that is not done yet */
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:check_job_completed job %s is not terminated",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOBID_PRINT(jobs[j]->jobid)));
                return;
            }
        }
        /* if we get here, then all jobs are done, so wakeup */
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed all jobs terminated - waking up",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        orte_trigger_event(&orte_exit);
    }
    
}
