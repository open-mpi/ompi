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

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rmcast/rmcast.h"
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
#if ORTE_ENABLE_MULTICAST
static void recv_rmcast_beats(int status,
                              orte_rmcast_channel_t channel,
                              orte_rmcast_seq_t seq_num,
                              orte_rmcast_tag_t tag,
                              orte_process_name_t *sender,
                              opal_buffer_t *buf, void* cbdata);
static void rmcast_callback_fn(int status,
                               orte_rmcast_channel_t channel,
                               orte_rmcast_seq_t seq_num,
                               orte_rmcast_tag_t tag,
                               orte_process_name_t *sender,
                               opal_buffer_t *buf, void* cbdata);
#else
static void recv_rml_beats(int status, orte_process_name_t* sender,
                           opal_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata);
static void rml_callback_fn(int status,
                            struct orte_process_name_t* peer,
                            struct opal_buffer_t* buffer,
                            orte_rml_tag_t tag,
                            void* cbdata);
#endif

/* local globals */
static opal_event_t *send_ev = NULL, *check_ev = NULL;
static struct timeval send_time, check_time;
static orte_job_t *daemons;

#include MCA_timer_IMPLEMENTATION_HEADER
static inline double gettime(void) __opal_attribute_always_inline__;
static inline double gettime(void)
{
    double wtime;
#if OPAL_TIMER_USEC_NATIVE
    wtime = ((double) opal_timer_base_get_usec()) / 1000000.0;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
#endif
    return wtime;
}

static int init(void)
{
    int rc=ORTE_SUCCESS;
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s initializing heartbeat recvs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* get the daemon job object */
    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        /* can't run */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

#if ORTE_ENABLE_MULTICAST
    /* setup multicast recv for heartbeats */
    if (ORTE_SUCCESS != (rc = orte_rmcast.recv_buffer_nb(ORTE_RMCAST_SYS_CHANNEL,
                                                         ORTE_RMCAST_TAG_HEARTBEAT,
                                                         ORTE_RMCAST_PERSISTENT,
                                                         recv_rmcast_beats, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
#else
    /* setup RML recv for the HNP to receive heartbeats */
    if (ORTE_PROC_IS_HNP) {
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_HEARTBEAT,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          recv_rml_beats,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
        }
    }
#endif

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
    
#if ORTE_ENABLE_MULTICAST
    orte_rmcast.cancel_recv(ORTE_RMCAST_SYS_CHANNEL, ORTE_RMCAST_TAG_HEARTBEAT);
#else
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_HEARTBEAT);
#endif
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
    /* convert the send rate */
    setup_time(mca_sensor_heartbeat_component.rate, &send_time);
    if (0 == send_time.tv_sec &&
        0 == send_time.tv_usec) {
        /* nothing to do */
        return;
    }

    if (!ORTE_PROC_IS_DAEMON) {
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

    /* setup the send */
    send_ev = (opal_event_t*)malloc(sizeof(opal_event_t));
    opal_event_evtimer_set(opal_event_base, send_ev, send_heartbeat, send_ev);
    opal_event_evtimer_add(send_ev, &send_time);
    
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

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s sending heartbeat",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* setup the buffer - nothing to pack as receipt alone is the "beat" */
    buf = OBJ_NEW(opal_buffer_t);
    
#if ORTE_ENABLE_MULTICAST
    if (ORTE_SUCCESS != (rc = orte_rmcast.send_buffer_nb(ORTE_RMCAST_SYS_CHANNEL,
                                                         ORTE_RMCAST_TAG_HEARTBEAT, buf,
                                                         rmcast_callback_fn, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto reset;
    }
#else
    /* send heartbeat to HNP */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf,
                                          ORTE_RML_TAG_HEARTBEAT, 0,
                                          rml_callback_fn, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto reset;
    }
#endif
    
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
    time_t now;
    opal_event_t *tmp = (opal_event_t*)arg;
    orte_process_name_t name;
    double delta;

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s sensor:check_heartbeat",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        goto reset;
    }
    
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    
    /* compute a send time interval */
    delta = send_time.tv_sec + (double)send_time.tv_usec/1000000.0;

    /* get current time */
    now = gettime();
    
    /* cycle through the nidmap - make sure we check them all
     * in case multiple daemons are late so all of those that did
     * can be appropriately flagged
     */
    for (v=0; v < daemons->procs->size; v++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, v))) {
            continue;
        }
        /* ignore myself */
        if ((int)ORTE_PROC_MY_NAME->vpid == v) {
            continue;
        }
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s CHECKING HEARTBEAT FOR %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name)));

        if (0 == proc->beat) {
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s NO BEAT YET",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            /* haven't recvd a beat yet */
            continue;
        }

        /* compute number of heartbeats missed */
        proc->missed = (int)((double)(now - proc->beat) / delta);
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s MISSING %d BEATS",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), proc->missed));
        if (mca_sensor_heartbeat_component.missed < proc->missed) {
            /* heartbeat failed */
            name.vpid = v;
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s sensor:check_heartbeat FAILED for daemon %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&name)));
            orte_errmgr.update_state(ORTE_PROC_MY_NAME->jobid, ORTE_JOB_STATE_HEARTBEAT_FAILED,
                                     &name, ORTE_PROC_STATE_HEARTBEAT_FAILED,
                                     0, ORTE_ERR_HEARTBEAT_LOST);
            /* zero the last beat to indicate we are waiting to recv
             * the first beat from the restarted daemon
             */
            proc->beat = 0;
        }
    }

 reset:
    /* reset the timer */
    opal_event_evtimer_add(tmp, &check_time);
}

#if ORTE_ENABLE_MULTICAST
static void recv_rmcast_beats(int status,
                              orte_rmcast_channel_t channel,
                              orte_rmcast_seq_t seq_num,
                              orte_rmcast_tag_t tag,
                              orte_process_name_t *sender,
                              opal_buffer_t *buf, void* cbdata)
{
    orte_proc_t *proc;
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s recvd heartbeat from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* get this daemon's object */
    if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, sender->vpid))) {
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s updating beat time for %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(sender)));
        proc->beat = gettime();
    }
}

static void rmcast_callback_fn(int status,
                               orte_rmcast_channel_t channel,
                               orte_rmcast_seq_t seq_num,
                               orte_rmcast_tag_t tag,
                               orte_process_name_t *sender,
                               opal_buffer_t *buf, void* cbdata)
{
    OBJ_RELEASE(buf);
}

#else
static void recv_rml_beats(int status, orte_process_name_t* sender,
                           opal_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata)
{
    orte_proc_t *proc;
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_intialized) {
        goto reset;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s recvd heartbeat from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* get this daemon's object */
    if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, sender->vpid))) {
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s updating beat time for %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(sender)));
        proc->beat = gettime();
    }
    
 reset:
    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_HEARTBEAT,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      recv_rml_beats,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
}

static void rml_callback_fn(int status,
                            struct orte_process_name_t* peer,
                            struct opal_buffer_t* buffer,
                            orte_rml_tag_t tag,
                            void* cbdata)
{
    OBJ_RELEASE(buffer);
}
#endif
