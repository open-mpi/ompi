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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
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

#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/runtime/opal_progress.h"

#include "opal/dss/dss.h"
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
#include "orte/runtime/orte_wakeup.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"

#include "orte/tools/orterun/totalview.h"

#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"

static int orte_plm_base_report_launched(orte_jobid_t job);

int orte_plm_base_setup_job(orte_job_t *jdata)
{
    int rc;
    orte_process_name_t name = {ORTE_JOBID_INVALID, 0};
    
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

    /*
     * setup I/O forwarding
     */
    name.jobid = jdata->jobid;
    if (ORTE_SUCCESS != (rc = orte_iof.iof_pull(&name, ORTE_NS_CMP_JOBID, ORTE_IOF_STDOUT, 1))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_iof.iof_pull(&name, ORTE_NS_CMP_JOBID, ORTE_IOF_STDERR, 2))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

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
    orte_daemon_cmd_flag_t command;
    opal_buffer_t *buffer;
    int rc;
    orte_process_name_t name = {ORTE_JOBID_INVALID, 0};

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch_apps for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));

    /* setup the buffer */
    buffer = OBJ_NEW(opal_buffer_t);

    /* pack the add_local_procs command */
    command = ORTE_DAEMON_ADD_LOCAL_PROCS;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }

    /* let the local launcher provide its required data */
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
    name.jobid = job;
    if (ORTE_SUCCESS != (rc = orte_iof.iof_push(&name, ORTE_NS_CMP_JOBID, ORTE_IOF_STDIN, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* init the debuggers */
#if 0
    orte_totalview_init_after_spawn(job);
#endif
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

void orte_plm_base_launch_failed(orte_jobid_t job, bool daemons_launching, pid_t pid,
                                 int status, orte_job_state_t state)
{
    orte_job_t *jdata;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch_failed for job %s during %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job),
                         (daemons_launching) ? "daemon launch" : "app launch"));

    if (daemons_launching) {
        if (WIFSIGNALED(status)) { /* died on signal */
#ifdef WCOREDUMP
            if (WCOREDUMP(status)) {
                opal_show_help("help-plm-base.txt", "daemon-died-signal-core", true,
                               pid, WTERMSIG(status));
            } else {
                opal_show_help("help-plm-base.txt", "daemon-died-signal", true,
                               pid, WTERMSIG(status));
            }
#else
            opal_show_help("help-plm-base.txt", "daemon-died-signal", true,
                           pid, WTERMSIG(status));
#endif /* WCOREDUMP */
        } else {
            opal_show_help("help-plm-base.txt", "daemon-died-no-signal", true,
                           pid, WEXITSTATUS(status));
        }
        orted_failed_launch = true;
        /* set the flag indicating that a daemon failed so we use the proper
         * methods for attempting to shutdown the rest of the system
         */
        orte_daemon_died = true;
        
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
    /* wakeup so orterun can exit */
    orte_wakeup(status);
}


static void orted_report_launch(int status, orte_process_name_t* sender,
                                opal_buffer_t *buffer,
                                orte_rml_tag_t tag, void *cbdata)
{
    char *rml_uri;
    int rc, idx;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:orted_report_launch from daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
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
        free(rml_uri);
        goto CLEANUP;
    }
    /* lookup and record this daemon's contact info */
    pdatorted[sender->vpid]->rml_uri = strdup(rml_uri);
    free(rml_uri);

    /* set the route to be direct */
    if (ORTE_SUCCESS != (rc = orte_routed.update_route(sender, sender))) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
        goto CLEANUP;
    }

    /* reissue the recv */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK,
                                 ORTE_RML_NON_PERSISTENT, orted_report_launch, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
    }

CLEANUP:

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:orted_report_launch %s for daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orted_failed_launch ? "failed" : "completed",
                         ORTE_NAME_PRINT(sender)));

    if (orted_failed_launch) {
        orte_errmgr.incomplete_start(ORTE_PROC_MY_NAME->jobid, jdatorted->aborted_proc->exit_code);
    } else {
        orted_num_callback++;
    }

}

int orte_plm_base_daemon_callback(orte_std_cntr_t num_daemons)
{
    int rc;
    opal_buffer_t *buf;
    orte_rml_cmd_flag_t command;

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
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:daemon_callback completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* all done launching - update the num_procs in my local structure */
    orte_process_info.num_procs = jdatorted->num_procs;

    /* update everyone's contact info so all daemons
     * can talk to each other
     */
    buf = OBJ_NEW(opal_buffer_t);
    /* pack the update-RML command */
    command = ORTE_RML_UPDATE_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_RML_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(ORTE_PROC_MY_NAME->jobid, buf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }
    /* send it */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid, buf, ORTE_RML_TAG_RML_INFO_UPDATE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }
    /* done with the buffer */
    OBJ_RELEASE(buf);

    return ORTE_SUCCESS;
}

/* the daemons actually report back that their procs have launched. Each
 * daemon will only send one message that contains the launch result
 * for their local procs.
 */
static bool app_launch_failed;

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
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:app_report_launch from daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&mev->sender)));
    
    /* unpack the jobid being reported */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &jobid, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        app_launch_failed = true;
        orte_errmgr.incomplete_start(-1, -1); /* no way to know the jobid or exit code */
        return;
    }
    /* get the job data object */
    if (NULL == (jdata = orte_get_job_data_object(jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        app_launch_failed = true;
        goto CLEANUP;
    }
    procs = (orte_proc_t**)(jdata->procs->addr);
    
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
            app_launch_failed = true;
            goto CLEANUP;
        }
        
        /* record that a proc reported */
        jdata->num_launched++;
    }
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:app_report_launch completed processing",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
CLEANUP:
    if (app_launch_failed) {
        orte_errmgr.incomplete_start(jdata->jobid, jdata->aborted_proc->exit_code);
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

int orte_plm_base_orted_append_basic_args(int *argc, char ***argv,
                                          char *ess,
                                          int *proc_vpid_index,
                                          int *node_name_index)
{
    char *param = NULL;
    int loc_id;
    char * amca_param_path = NULL;
    char * amca_param_prefix = NULL;
    char * tmp_force = NULL;
    int i, cnt;
    orte_job_t *jdata;
    char *rml_uri;
    
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
    jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "orte_ess_num_procs");
    asprintf(&param, "%lu", (unsigned long)(jdata->num_procs));
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* pass the uri of the hnp */
    rml_uri = orte_rml.get_contact_info();
    asprintf(&param, "\"%s\"", rml_uri);
    opal_argv_append(argc, argv, "--hnp-uri");
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* Node Name */
    if(NULL != node_name_index) {
        opal_argv_append(argc, argv, "--nodename");
        *node_name_index = *argc;
        opal_argv_append(argc, argv, "<template>");
    }
    
    /* pass along any cmd line MCA params provided to mpirun,
     * being sure to "purge" any that would cause problems
     * on backend nodes
     */
    cnt = opal_argv_count(orted_cmd_line);    
    for (i=0; i < cnt; i+=3) {
        /* if the specified option is more than one word, we don't
         * have a generic way of passing it as some environments ignore
         * any quotes we add, while others don't - so we ignore any
         * such options. In most cases, this won't be a problem as
         * they typically only apply to things of interest to the HNP
         */
        if (NULL != strchr(orted_cmd_line[i+2], ' ')) {
            continue;
        }
        /* must be okay - pass it along */
        opal_argv_append(argc, argv, orted_cmd_line[i]);
        opal_argv_append(argc, argv, orted_cmd_line[i+1]);
        opal_argv_append(argc, argv, orted_cmd_line[i+2]);
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
    }
    
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
                }
                break;
            } else if (ORTE_PROC_STATE_ABORTED == procs[i]->state) {
                jdata->state = ORTE_JOB_STATE_ABORTED;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = procs[i];
                    jdata->abort = true;
                }
                break;
            } else if (ORTE_PROC_STATE_ABORTED_BY_SIG == procs[i]->state) {
                jdata->state = ORTE_JOB_STATE_ABORTED_BY_SIG;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = procs[i];
                    jdata->abort = true;
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
                             ORTE_NAME_PRINT(&(jdata->aborted_proc->name))));
        /* if we are not already responding to an abort or an abnormal
         * termination, report this to the errmgr
         */
        if (!orte_abnormal_term_ordered && !orte_abort_in_progress) {
            orte_errmgr.incomplete_start(jdata->jobid, jdata->aborted_proc->exit_code);
        } else {
            /* otherwise, we need to check to see if the jobs are completed
             * so that we can continue to close down the job
             */
           goto CHECK_ALL_JOBS;
        }
    } else if (ORTE_JOB_STATE_ABORTED == jdata->state ||
               ORTE_JOB_STATE_ABORTED_BY_SIG == jdata->state) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed declared job %s aborted by proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid),
                             ORTE_NAME_PRINT(&(jdata->aborted_proc->name))));
        /* if we are not already responding to an abort or an abnormal
         * termination, report this to the errmgr
         */
        if (!orte_abnormal_term_ordered && !orte_abort_in_progress) {
            orte_errmgr.proc_aborted(&(jdata->aborted_proc->name), jdata->aborted_proc->exit_code);
        } else {
            /* otherwise, we need to check to see if the jobs are completed
             * so that we can continue to close down the job
             */
           goto CHECK_ALL_JOBS;
        }
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
         * that the orteds are complete
         *
         * NOTE: remember to protect against jdata being NULL!
         */
        if (NULL != jdata &&
            jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
            /* we have completed the orteds */
            int data=1;
            write(orteds_exit, &data, sizeof(int));
            opal_progress();
            return;
        }
        /* now check special case if jdata is NULL - we want
         * to definitely declare the job done if the orteds
         * have completed, no matter what else may be happening.
         * This can happen if a ctrl-c hits in the "wrong" place
         * while launching
         */
        if (jdata == NULL) {
            jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            if (jdata->num_terminated >= jdata->num_procs) {
                /* orteds are done! */
                int data=1;
                jdata->state = ORTE_JOB_STATE_TERMINATED;
                write(orteds_exit, &data, sizeof(int));
                opal_progress();
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
        orte_wakeup(0);
    }
    
}
