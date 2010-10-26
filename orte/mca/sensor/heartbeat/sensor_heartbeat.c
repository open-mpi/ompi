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
                              orte_rmcast_tag_t tag,
                              orte_process_name_t *sender,
                              opal_buffer_t *buf, void* cbdata);
static void rmcast_callback_fn(int status,
                               orte_rmcast_channel_t channel,
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
static double timeout;

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
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s initializing heartbeat recvs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));


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
        opal_event.del(send_ev);
        OBJ_RELEASE(send_ev);
    }
    if (NULL != check_ev) {
        opal_event.del(check_ev);
        OBJ_RELEASE(check_ev);
    }
    
#if ORTE_ENABLE_MULTICAST
    orte_rmcast.cancel_recv(ORTE_RMCAST_SYS_CHANNEL, ORTE_RMCAST_TAG_HEARTBEAT);
#else
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_HEARTBEAT);
#endif
    return;
}

/*
 * Start sending and checking heartbeats
 */
static void start(orte_jobid_t jobid)
{
    uint64_t time;
    
    if (jobid != ORTE_PROC_MY_NAME->jobid && ORTE_JOBID_WILDCARD != jobid) {
        /* heartbeats are only for daemons and HNPs */
        return;
    }
    
    /* setup the send */
    time = mca_sensor_heartbeat_component.rate * 1000; /* convert to microsecs */
    send_ev = OBJ_NEW(opal_event_t);
    opal_event.evtimer_set(opal_event_base, send_ev, send_heartbeat, send_ev);
    send_time.tv_sec = time / 1000000;
    send_time.tv_usec = time % 1000000;
    opal_event.evtimer_add(send_ev, &send_time);
    
    /* define the timeout */
    timeout = 2.0 * (double)time;
    
    /* setup the check */
    time = mca_sensor_heartbeat_component.check * 1000; /* convert to microsecs */
    check_ev = OBJ_NEW(opal_event_t);
    opal_event.evtimer_set(opal_event_base, check_ev, check_heartbeat, check_ev);
    check_time.tv_sec = time / 1000000;
    check_time.tv_usec = time % 1000000;
    opal_event.evtimer_add(check_ev, &check_time);
}


static void stop(orte_jobid_t jobid)
{
    if (jobid != ORTE_PROC_MY_NAME->jobid && ORTE_JOBID_WILDCARD != jobid) {
        /* heartbeats are only for daemons and HNPs */
        return;
    }
    
    if (NULL != send_ev) {
        opal_event.del(send_ev);
        OBJ_RELEASE(send_ev);
    }
    if (NULL != check_ev) {
        opal_event.del(check_ev);
        OBJ_RELEASE(check_ev);
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
        return;
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
        return;
    }
#else
    /* send heartbeat to HNP */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf,
                                          ORTE_RML_TAG_HEARTBEAT, 0,
                                          rml_callback_fn, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return;
    }
#endif
    
    /* reset the timer */
    opal_event.evtimer_add(tmp, &send_time);
}

/* this function automatically gets periodically called
 * by the event library so we can check on the state
 * of the various orteds
 */
static void check_heartbeat(int fd, short dummy, void *arg)
{
    int v;
    orte_nid_t *nid;
    double now;
    opal_event_t *tmp = (opal_event_t*)arg;
    orte_process_name_t name;
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s sensor:check_heartbeat",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        return;
    }
    
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    
    /* get current time */
    now = gettime();
    
    /* cycle through the nidmap - make sure we check them all
     * in case multiple daemons are late so all of those that did
     * can be appropriately flagged
     */
    for (v=0; v < orte_nidmap.size; v++) {
        if (NULL == (nid = (orte_nid_t*)opal_pointer_array_get_item(&orte_nidmap, v))) {
            continue;
        }
        if (0 == nid->beat) {
            /* haven't recvd a beat yet */
            continue;
        }
        if ((now - nid->beat) > timeout) {
            nid->missed++;
            if (mca_sensor_heartbeat_component.missed < nid->missed) {
                /* heartbeat failed */
                name.vpid = v;
                orte_errmgr.update_state(ORTE_PROC_MY_NAME->jobid, ORTE_JOB_STATE_HEARTBEAT_FAILED,
                                         &name, ORTE_PROC_STATE_HEARTBEAT_FAILED,
                                         0, ORTE_ERR_HEARTBEAT_LOST);
            }
        }
    }

    /* reset the timer */
    opal_event.evtimer_add(tmp, &check_time);
}

#if ORTE_ENABLE_MULTICAST
static void recv_rmcast_beats(int status,
                              orte_rmcast_channel_t channel,
                              orte_rmcast_tag_t tag,
                              orte_process_name_t *sender,
                              opal_buffer_t *buf, void* cbdata)
{
    orte_nid_t *nid;
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s recvd heartbeat from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* get this daemon's nid - if it isn't here, just ignore
     * as this is caused by a race condition at startup
     */
    if (NULL != (nid = orte_util_lookup_nid(sender))) {
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s updating beat time for %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(sender)));
        nid->beat = gettime();
    }
}

static void rmcast_callback_fn(int status,
                               orte_rmcast_channel_t channel,
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
    orte_nid_t *nid;
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_intialized) {
        return;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s recvd heartbeat from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* get this daemon's nid - if it isn't here, just ignore
     * as this is caused by a race condition at startup
     */
    if (NULL != (nid = orte_util_lookup_nid(sender))) {
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s updating beat time for %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(sender)));
        nid->beat = gettime();
    } else {
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s no nidmap entry for %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(sender)));
    }
    
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
