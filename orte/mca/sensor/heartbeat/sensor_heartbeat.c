/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <stdio.h>

#include "opal_stdint.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/mca/pstat/pstat.h"
#include "opal/mca/event/event.h"

#include "orte/threads/threads.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_heartbeat.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(orte_jobid_t job);
static void stop(orte_jobid_t job);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_heartbeat_module = {
    init,
    finalize,
    start,
    stop
};

/* declare the local functions */
static void check_heartbeat(int fd, short event, void *arg);
static void send_heartbeat(int fd, short event, void *arg);
static void recv_rml_beats(int status, orte_process_name_t* sender,
                           opal_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata);
static void rml_callback_fn(int status,
                            struct orte_process_name_t* peer,
                            struct opal_buffer_t* buffer,
                            orte_rml_tag_t tag,
                            void* cbdata)
{
    OBJ_RELEASE(buffer);
}

/* local globals */
static opal_event_t *send_ev = NULL, *check_ev = NULL;
static struct timeval send_time, check_time;
static orte_job_t *daemons;
static orte_thread_ctl_t ctl;

static int init(void)
{
    int rc=ORTE_SUCCESS;
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s initializing heartbeat recvs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    OBJ_CONSTRUCT(&ctl, orte_thread_ctl_t);

    /* get the daemon job object */
    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        /* can't run */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    /* setup to receive heartbeats */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_SCHEDULER) {
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_HEARTBEAT,
                                                          ORTE_RML_PERSISTENT,
                                                          recv_rml_beats,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
        }
    }

    return rc;
}

static void finalize(void)
{
    if (NULL != send_ev) {
        opal_event_del(send_ev);
        free(send_ev);
        send_ev = NULL;
    }
    if (NULL != check_ev) {
        opal_event_del(check_ev);
        free(check_ev);
        check_ev = NULL;
    }
    
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_HEARTBEAT);

    OBJ_DESTRUCT(&ctl);
    return;
}

static void setup_time(char *input, struct timeval *time)
{
    char **val;

    /* set default */
    time->tv_sec = 0;
    time->tv_usec = 0;

    /* convert the rate to time */
    val = opal_argv_split(input, ':');
    if (NULL == val) {
        /* nothing to do */
        return;
    }
    if (NULL != val[0]) {
        time->tv_sec = strtol(val[0], NULL, 10);
    }
    if (NULL != val[1]) {
        time->tv_usec = strtol(val[1], NULL, 10);
    }
}


/*
 * Start sending and checking heartbeats
 */
static void start(orte_jobid_t jobid)
{

    /* only daemons send heartbeats */
    if (ORTE_PROC_IS_DAEMON) {
        /* convert the send rate */
        setup_time(mca_sensor_heartbeat_component.rate, &send_time);
        if (0 == send_time.tv_sec &&
            0 == send_time.tv_usec) {
            /* nothing to do */
            return;
        }
        /* setup the send */
        send_ev = (opal_event_t*)malloc(sizeof(opal_event_t));
        opal_event_evtimer_set(opal_event_base, send_ev, send_heartbeat, send_ev);
        opal_event_evtimer_add(send_ev, &send_time);

    } else if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_SCHEDULER) {
        /* convert the check rate */
        setup_time(mca_sensor_heartbeat_component.check, &check_time);
        if (0 == check_time.tv_sec &&
            0 == check_time.tv_usec) {
            /* no sense in running if we won't check */
            return;
        }

        /* setup the check */
        check_ev = (opal_event_t*)malloc(sizeof(opal_event_t));
        opal_event_evtimer_set(opal_event_base, check_ev, check_heartbeat, check_ev);
        opal_event_evtimer_add(check_ev, &check_time);
    }
}


static void stop(orte_jobid_t jobid)
{
    if (NULL != send_ev) {
        opal_event_del(send_ev);
        free(send_ev);
        send_ev = NULL;
    }
    if (NULL != check_ev) {
        opal_event_del(check_ev);
        free(check_ev);
        check_ev = NULL;
    }
    return;
}

static void send_heartbeat(int fd, short event, void *arg)
{
    opal_buffer_t *buf;
    opal_event_t *tmp = (opal_event_t*)arg;
    int rc;
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        goto reset;
    }

    /* if my HNP hasn't been defined yet, ignore - nobody listening yet */
    if (ORTE_JOBID_INVALID == ORTE_PROC_MY_HNP->jobid ||
        ORTE_VPID_INVALID == ORTE_PROC_MY_HNP->vpid) {
        goto reset;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s sending heartbeat",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* setup the buffer - nothing to pack as receipt alone is the "beat" */
    buf = OBJ_NEW(opal_buffer_t);
    
    /* send heartbeat */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf,
                                          ORTE_RML_TAG_HEARTBEAT, 0,
                                          rml_callback_fn, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto reset;
    }
    
 reset:
    /* reset the timer */
    opal_event_evtimer_add(tmp, &send_time);
}

/* this function automatically gets periodically called
 * by the event library so we can check on the state
 * of the various orteds
 */
static void check_heartbeat(int fd, short dummy, void *arg)
{
    int v;
    orte_proc_t *proc;
    opal_event_t *tmp = (opal_event_t*)arg;

    ORTE_ACQUIRE_THREAD(&ctl);

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s sensor:check_heartbeat",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        goto reset;
    }
    
    for (v=0; v < daemons->procs->size; v++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, v))) {
            continue;
        }
        /* ignore myself */
        if (proc->name.vpid == ORTE_PROC_MY_NAME->vpid) {
            continue;
        }
        if (ORTE_PROC_STATE_RUNNING != proc->state) {
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s sensor:heartbeat DAEMON %s IS NOT RUNNING",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            continue;
        }
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s CHECKING HEARTBEAT FOR %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name)));

        if (!proc->beat) {
            /* no heartbeat recvd in last window */
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s sensor:check_heartbeat FAILED for daemon %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            orte_errmgr.update_state(ORTE_PROC_MY_NAME->jobid, ORTE_JOB_STATE_HEARTBEAT_FAILED,
                                     &proc->name, ORTE_PROC_STATE_HEARTBEAT_FAILED,
                                     0, ORTE_ERR_HEARTBEAT_LOST);
        }
        /* reset for next period */
        proc->beat = false;
    }

 reset:
    ORTE_RELEASE_THREAD(&ctl);

    /* reset the timer */
    opal_event_evtimer_add(tmp, &check_time);
}

static void recv_rml_beats(int status, orte_process_name_t* sender,
                           opal_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata)
{
    orte_proc_t *proc;
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        return;
    }

    ORTE_ACQUIRE_THREAD(&ctl);

    /* get this daemon's object */
    if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, sender->vpid))) {
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s marked beat from %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(sender)));
        proc->beat = true;
    }

    ORTE_RELEASE_THREAD(&ctl);
}
