/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
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
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/util/argv.h"
#include "opal/util/opal_sos.h"
#include "opal/runtime/opal_progress.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/sysinfo/sysinfo.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/odls/odls.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_locks.h"
#include "orte/runtime/orte_quit.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/proc_info.h"
#include "orte/util/regex.h"

#include "orte/mca/odls/odls_types.h"

#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"

int orte_plm_base_setup_job(orte_job_t *jdata)
{
    orte_job_t *jdatorted;
    orte_app_context_t *app;
    int rc;
    int32_t ljob;
    int i;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:setup_job for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));

    /* if the job is not being restarted or hasn't already been given a jobid, prep it */
    if (ORTE_JOB_STATE_RESTART != jdata->state &&  ORTE_JOBID_INVALID == jdata->jobid) {
        /* get a jobid for it */
        if (ORTE_SUCCESS != (rc = orte_plm_base_create_jobid(jdata))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* store it on the global job data pool */
        ljob = ORTE_LOCAL_JOBID(jdata->jobid);
        opal_pointer_array_set_item(orte_job_data, ljob, jdata);
        
        /* set the job state */
        jdata->state = ORTE_JOB_STATE_INIT;

        /* if job recovery is not defined, set it to default */
        if (!jdata->recovery_defined) {
            /* set to system default */
            jdata->enable_recovery = orte_enable_recovery;
        }
        /* if app recovery is not defined, set apps to defaults */
        for (i=0; i < jdata->apps->size; i++) {
            if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
                continue;
            }
            if (!app->recovery_defined) {
                app->max_restarts = orte_max_restarts;
            }
        }
    }
    
    /* get the allocation */
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
        char *crud;
        orte_odls_job_t *jobdat;
        crud = orte_regex_encode_maps(jdata);
        opal_output(0, "maps regex: %s", (NULL == crud) ? "NULL" : crud);
        if (NULL == crud) {
            orte_never_launched = true;
            ORTE_UPDATE_EXIT_STATUS(0);
            orte_jobs_complete();
            return ORTE_ERROR;
        }
        orte_util_nidmap_init(NULL);
        orte_regex_decode_maps(crud, &jobdat);
        free(crud);
        /* print-out the map */
        orte_nidmap_dump();
        orte_jobmap_dump();
        /* printout the jobdat */
        opal_output(orte_clean_output, "****   DUMP OF JOBDAT %s (%d nodes %d procs)   ***",
                    ORTE_JOBID_PRINT(jobdat->jobid), (int)jobdat->num_nodes, (int)(jobdat->num_procs));
        opal_output(orte_clean_output, "\tNum slots: %d\tControl: %x\tStdin: %d",
                    (int)jobdat->total_slots_alloc, jobdat->controls, (int)jobdat->stdin_target);
        opal_output(orte_clean_output, "\tApp: %s", jobdat->apps[0]->app);
        opal_output(orte_clean_output, "\tCwd: %s", jobdat->apps[0]->cwd);
        crud = opal_argv_join(jobdat->apps[0]->argv, ',');
        opal_output(orte_clean_output, "\tArgv: %s", crud);
        free(crud);
        crud = opal_argv_join(jobdat->apps[0]->env, ',');
        opal_output(orte_clean_output, "\tEnv: %s", crud);
        free(crud);
        orte_never_launched = true;
        ORTE_UPDATE_EXIT_STATUS(0);
        orte_jobs_complete();
        return ORTE_ERROR;
    }

    {
        opal_byte_object_t bo;

        /* construct a nodemap */
        if (ORTE_SUCCESS != (rc = orte_util_encode_nodemap(&bo))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(&bo))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* print-out the map */
        orte_nidmap_dump();
    }
#endif
    
    /* if we don't want to launch, now is the time to leave */
    if (orte_do_not_launch) {
        orte_never_launched = true;
        ORTE_UPDATE_EXIT_STATUS(0);
        orte_jobs_complete();
        return ORTE_ERR_SILENT;
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
        orte_never_launched = true;
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        orte_jobs_complete();
        return ORTE_ERROR;
    }
    
    /* get the orted job data object */
    if (NULL == (jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (orte_process_info.num_procs != jdatorted->num_procs) {
        /* more daemons are being launched - update the routing tree to
         * ensure that the HNP knows how to route messages via
         * the daemon routing tree - this needs to be done
         * here to avoid potential race conditions where the HNP
         * hasn't unpacked its launch message prior to being
         * asked to communicate.
         */
        orte_process_info.num_procs = jdatorted->num_procs;

        if (orte_process_info.max_procs < orte_process_info.num_procs) {
            orte_process_info.max_procs = orte_process_info.num_procs;
        }

        if (ORTE_SUCCESS != (rc = orte_routed.update_routing_tree(ORTE_PROC_MY_NAME->jobid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /*** RHC: USER REQUEST TO TIE-OFF STDXXX TO /DEV/NULL
     *** WILL BE SENT IN LAUNCH MESSAGE AS PART OF CONTROLS FIELD.
     *** SO IF USER WANTS NO IO BEING SENT AROUND, THE ORTEDS
     *** WILL TIE IT OFF AND THE IOF WILL NEVER RECEIVE ANYTHING.
     *** THE IOF AUTOMATICALLY KNOWS TO OUTPUT ANY STDXXX
     *** DATA IT -DOES- RECEIVE TO THE APPROPRIATE FD, SO THERE
     *** IS NOTHING WE NEED DO HERE TO SETUP IOF
     ***/
    
#if OPAL_ENABLE_FT_CR == 1
    /*
     * Notify the Global SnapC component regarding new job (even if it was restarted)
     */
    if( ORTE_SUCCESS != (rc = orte_snapc.setup_job(jdata->jobid) ) ) {
        /* Silent Failure :/ JJH */
        ORTE_ERROR_LOG(rc);
    }
#endif
    
    return ORTE_SUCCESS;
}

static struct timeval app_launch_start, app_launch_stop;
static opal_event_t *dmn_report_ev=NULL;
bool app_launch_failed;

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

int orte_plm_base_launch_apps(orte_jobid_t job)
{
    orte_job_t *jdata;
    orte_daemon_cmd_flag_t command;
    opal_buffer_t *buffer;
    int rc;
    orte_process_name_t name = {ORTE_JOBID_INVALID, 0};

    /* if we are launching the daemon job, then we are
     * starting a virtual machine and there is no app
     * to launch. Just flag the launch as complete
     */
    if (ORTE_PROC_MY_NAME->jobid == job) {
        rc = ORTE_SUCCESS;
        goto WAKEUP;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch_apps for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));

    if (orte_timing) {
        gettimeofday(&app_launch_start, NULL);
    }
    
    if (ORTE_JOBID_INVALID == job) {
        /* we are only launching debugger daemons */
        jdata = orte_debugger_daemon;
    } else {
        if (NULL == (jdata = orte_get_job_data_object(job))) {
            /* bad jobid */
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            rc = ORTE_ERR_BAD_PARAM;
            goto WAKEUP;
        }
    }

    /* setup the buffer */
    buffer = OBJ_NEW(opal_buffer_t);

    /* pack the add_local_procs command */
    command = ORTE_DAEMON_ADD_LOCAL_PROCS;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        goto WAKEUP;
    }

    /* get the local launcher's required data */
    if (ORTE_SUCCESS != (rc = orte_odls.get_add_procs_data(buffer, job))) {
        ORTE_ERROR_LOG(rc);
        goto WAKEUP;
    }
    
    /* if we are timing, record the time we send this message */
    if (orte_timing) {
        gettimeofday(&jdata->launch_msg_sent, NULL);
    }
    
    /* send the command to the daemons */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid,
                                                 buffer, ORTE_RML_TAG_DAEMON))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        goto WAKEUP;
    }
    OBJ_RELEASE(buffer);
    
    /* setup a timer - if we don't launch within the
     * defined time, then we know things have failed
     */
    if (0 < orte_startup_timeout) {
        ORTE_DETECT_TIMEOUT(&dmn_report_ev, orte_startup_timeout, 1000, 10000000, timer_cb);
    }
    
    /* wait for all the daemons to report apps launched */
    app_launch_failed = false;
    ORTE_PROGRESSED_WAIT(app_launch_failed, jdata->num_launched, jdata->num_procs);
    
    if (ORTE_JOB_STATE_RUNNING != jdata->state) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:launch failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job), ORTE_ERROR_NAME(rc)));
        goto WAKEUP;
    }
    
    if (orte_timing) {
        int64_t maxsec, maxusec;
        char *tmpstr;
        gettimeofday(&app_launch_stop, NULL);
        /* subtract starting time to get time in microsecs for test */
        maxsec = app_launch_stop.tv_sec - app_launch_start.tv_sec;
        maxusec = app_launch_stop.tv_usec - app_launch_start.tv_usec;
        tmpstr = orte_pretty_print_timing(maxsec, maxusec);
        fprintf(orte_timing_output, "Time to launch apps: %s\n", tmpstr);
        free(tmpstr);
    }
    
    /* complete wiring up the iof */
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch wiring up iof",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* push stdin - the IOF will know what to do with the specified target */
    name.jobid = job;
    name.vpid = jdata->stdin_target;
    name.epoch = ORTE_EPOCH_INVALID;
    name.epoch = orte_ess.proc_get_epoch(&name);
    
    if (ORTE_SUCCESS != (rc = orte_iof.push(&name, ORTE_IOF_STDIN, 0))) {
        ORTE_ERROR_LOG(rc);
        goto WAKEUP;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch completed for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));
    
WAKEUP:
    /* wakeup anyone waiting for this */
    orte_plm_globals.spawn_complete = true;
    orte_plm_globals.spawn_status = rc;
    opal_condition_broadcast(&orte_plm_globals.spawn_cond);

    return rc;
}

/* daemons callback when they start - need to listen for them */
static int orted_num_callback;
static bool orted_failed_launch;
static orte_job_t *jdatorted;
static struct timeval daemonlaunchtime = {0,0}, daemonsetuptime = {0,0}, daemoncbtime = {0,0};

static void process_orted_launch_report(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    opal_buffer_t *buffer = mev->buffer;
    orte_process_name_t peer;
    char *rml_uri = NULL;
    int rc, idx;
    struct timeval recvtime;
    long secs, usecs;
    int64_t setupsec, setupusec;
    int64_t startsec, startusec;
    orte_proc_t *daemon=NULL;
    int32_t i, num_values;
    opal_sysinfo_value_t *info;
    orte_node_t *node;
    char *nodename;
    opal_list_item_t *item;
    
    /* see if we need to timestamp this receipt */
    if (orte_timing) {
        gettimeofday(&recvtime, NULL);
    }
    
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
        orted_failed_launch = true;
        goto CLEANUP;
    }

    rc = orte_rml_base_parse_uris(rml_uri, &peer, NULL );
    if( ORTE_SUCCESS != rc ) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
        goto CLEANUP;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:orted_report_launch from daemon %s via %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&peer),
                         ORTE_NAME_PRINT(&mev->sender)));
    
    /* update state and record for this daemon contact info */
    if (NULL == (daemon = (orte_proc_t*)opal_pointer_array_get_item(jdatorted->procs, peer.vpid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        orted_failed_launch = true;
        goto CLEANUP;
    }
    daemon->state = ORTE_PROC_STATE_RUNNING;
    daemon->rml_uri = rml_uri;

    /* if we are doing a timing test, unload the start and setup times of the daemon */
    if (orte_timing) {
        /* get the time stamp when the daemon first started */
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &startsec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &startusec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        /* save the latest daemon to start */
        if (startsec > daemonlaunchtime.tv_sec) {
            daemonlaunchtime.tv_sec = startsec;
            daemonlaunchtime.tv_usec = startusec;
        } else if (startsec == daemonlaunchtime.tv_sec &&
                   startusec > daemonlaunchtime.tv_usec) {
            daemonlaunchtime.tv_usec = startusec;
        }
        /* get the time required for the daemon to setup - locally computed by each daemon */
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &setupsec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &setupusec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        /* save the longest */
        if (setupsec > daemonsetuptime.tv_sec) {
            daemonsetuptime.tv_sec = setupsec;
            daemonsetuptime.tv_usec = setupusec;
        } else if (setupsec == daemonsetuptime.tv_sec &&
                   setupusec > daemonsetuptime.tv_usec) {
            daemonsetuptime.tv_usec = setupusec;
        }
        /* get the time stamp of when the daemon started to send this message to us */
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &setupsec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &setupusec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        /* check the time for the callback to complete and save the longest */
        ORTE_COMPUTE_TIME_DIFF(secs, usecs, setupsec, setupusec, recvtime.tv_sec, recvtime.tv_usec);
        if (secs > daemoncbtime.tv_sec) {
            daemoncbtime.tv_sec = secs;
            daemoncbtime.tv_usec = usecs;
        } else if (secs == daemoncbtime.tv_sec &&
                   usecs > daemoncbtime.tv_usec) {
            daemoncbtime.tv_usec = usecs;
        }
    }
    
    /* unpack the node name - we don't need it here, but it is included
     * in the message for other uses
     */
    idx = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &nodename, &idx, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
        goto CLEANUP;
    }
    
    /* store the local resources for that node */
    idx=1;
    node = daemon->node;
    if (OPAL_SUCCESS == opal_dss.unpack(buffer, &num_values, &idx, OPAL_INT32) &&
        0 < num_values) {
        /* clear the old list, if it exists */
        while (NULL != (item = opal_list_remove_first(&node->resources))) {
            OBJ_RELEASE(item);
        }
        for (i=0; i < num_values; i++) {
            info = OBJ_NEW(opal_sysinfo_value_t);
            idx=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &info->key, &idx, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                orted_failed_launch = true;
                goto CLEANUP;
            }
            idx=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &info->type, &idx, OPAL_DATA_TYPE_T))) {
                ORTE_ERROR_LOG(rc);
                orted_failed_launch = true;
                goto CLEANUP;
            }
            idx=1;
            if (OPAL_INT64 == info->type) {
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &(info->data.i64), &idx, OPAL_INT64))) {
                    ORTE_ERROR_LOG(rc);
                    orted_failed_launch = true;
                    goto CLEANUP;
                }
            } else if (OPAL_STRING == info->type) {
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &(info->data.str), &idx, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    orted_failed_launch = true;
                    goto CLEANUP;
                }
            }
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s adding resource %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 info->key));
            opal_list_append(&node->resources, &info->super);
        }
    }
    
    /* if a tree-launch is underway, send the cmd back */
    if (NULL != orte_tree_launch_cmd) {
        orte_rml.send_buffer(&peer, orte_tree_launch_cmd, ORTE_RML_TAG_DAEMON, 0);
    }
    
CLEANUP:

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:orted_report_launch %s for daemon %s (via %s) at contact %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orted_failed_launch ? "failed" : "completed",
                         ORTE_NAME_PRINT(&peer),
                         ORTE_NAME_PRINT(&mev->sender),
                         (NULL == daemon) ? "UNKNOWN" : daemon->rml_uri));

    /* release the message */
    OBJ_RELEASE(mev);

    if (orted_failed_launch) {
        if( NULL != rml_uri ) free(rml_uri);
        orte_errmgr.update_state(ORTE_PROC_MY_NAME->jobid, ORTE_JOB_STATE_FAILED_TO_START,
                                 NULL, ORTE_PROC_STATE_FAILED_TO_START,
                                 0, ORTE_ERROR_DEFAULT_EXIT_CODE);
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
    if (rc != ORTE_SUCCESS && OPAL_SOS_GET_ERROR_CODE(rc) != ORTE_ERR_NOT_IMPLEMENTED) {
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

    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK,
                                 ORTE_RML_NON_PERSISTENT, orted_report_launch, NULL);
    if (rc != ORTE_SUCCESS && OPAL_SOS_GET_ERROR_CODE(rc) != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(orted_failed_launch, orted_num_callback, num_daemons);
    
    /* cancel the lingering recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* if we are timing, output the results */
    if (orte_timing) {
        int64_t sec, usec;
        char *tmpstr;
        ORTE_COMPUTE_TIME_DIFF(sec, usec, orte_plm_globals.daemonlaunchstart.tv_sec,
                               orte_plm_globals.daemonlaunchstart.tv_usec,
                               daemonlaunchtime.tv_sec, daemonlaunchtime.tv_usec);
        tmpstr = orte_pretty_print_timing(sec, usec);
        fprintf(orte_timing_output, "Daemon launch was completed in %s\n", tmpstr);
        free(tmpstr);
        tmpstr = orte_pretty_print_timing(daemonsetuptime.tv_sec, daemonsetuptime.tv_usec);
        fprintf(orte_timing_output, "Daemon setup (from first exec statement to ready-for-commands) was completed in a maximum of %s\n", tmpstr);
        free(tmpstr);
        tmpstr = orte_pretty_print_timing(daemoncbtime.tv_sec, daemoncbtime.tv_usec);
        fprintf(orte_timing_output, "Daemon callback message to HNP took a maximum time of %s to reach the HNP\n", tmpstr);
        free(tmpstr);
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:daemon_callback completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if a tree-launch was underway, clear out the cmd */
    if (NULL != orte_tree_launch_cmd) {
        OBJ_RELEASE(orte_tree_launch_cmd);
    }
    
    return ORTE_SUCCESS;
}

int orte_plm_base_setup_orted_cmd(int *argc, char ***argv)
{
    int i, loc;
    char **tmpv;
    
    /* set default location to be 0, indicating that
     * only a single word is in the cmd
     */
    loc = 0;
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
                                          char *nodes)
{
    char *param = NULL;
    int loc_id;
    char * amca_param_path = NULL;
    char * amca_param_prefix = NULL;
    char * tmp_force = NULL;
    int i, cnt, rc;
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
    if (orte_report_bindings) {
        opal_argv_append(argc, argv, "--report-bindings");
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
    
    /* tell the orted what ESS component to use */
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "ess");
    opal_argv_append(argc, argv, ess);
    
    /* pass the daemon jobid */
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "orte_ess_jobid");
    if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&param, ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
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
    if (ORTE_PROC_IS_HNP) {
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
    if (ORTE_PROC_IS_HNP) {
        rml_uri = orte_rml.get_contact_info();
    } else {
        asprintf(&param, "\"%s\"", orte_rml.get_contact_info() );
        opal_argv_append(argc, argv, "--parent-uri");
        opal_argv_append(argc, argv, param);
        free(param);
    
        rml_uri = orte_process_info.my_hnp_uri;
    }
    asprintf(&param, "\"%s\"", rml_uri);
    opal_argv_append(argc, argv, "--hnp-uri");
    opal_argv_append(argc, argv, param);
    free(param);

    /* if given and we have static ports, pass the node list */
    if (orte_static_ports && NULL != nodes) {
        /* convert the nodes to a regex */
        if (ORTE_SUCCESS != (rc = orte_regex_create(nodes, &param))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        opal_argv_append(argc, argv, "--nodes");
        opal_argv_append(argc, argv, param);
        free(param);
    }
    
    /* pass along any cmd line MCA params provided to mpirun,
     * being sure to "purge" any that would cause problems
     * on backend nodes
     */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
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
            tmp_force = (char *) malloc(sizeof(char) * OPAL_PATH_MAX);
            if( NULL == (tmp_force = getcwd(tmp_force, OPAL_PATH_MAX) )) {
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

