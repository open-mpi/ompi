/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
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
#include "opal/mca/event/event.h"

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_heartbeat.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(orte_jobid_t job);
static void sample(void);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_heartbeat_module = {
    init,
    finalize,
    start,
    NULL,
    sample,
    NULL
};

/* declare the local functions */
static void check_heartbeat(int fd, short event, void *arg);
static void recv_beats(int status, orte_process_name_t* sender,
                       opal_buffer_t *buffer,
                       orte_rml_tag_t tag, void *cbdata);

/* local globals */
static orte_job_t *daemons=NULL;
static opal_event_t check_ev;
static bool check_active = false;
static struct timeval check_time;

static int init(void)
{
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                         "%s initializing heartbeat recvs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* setup to receive heartbeats */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_CM) {
        orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                ORTE_RML_TAG_HEARTBEAT,
                                ORTE_RML_PERSISTENT,
                                recv_beats, NULL);
    }

    if (ORTE_PROC_IS_HNP) {
        daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    }

    return ORTE_SUCCESS;
}

static void finalize(void)
{
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_HEARTBEAT);
    if (check_active) {
        opal_event_del(&check_ev);
        check_active = false;
    }
    return;
}

static void start(orte_jobid_t job)
{
    if (!check_active && NULL != daemons) {
        /* setup the check event */
        check_time.tv_sec = 3 * orte_sensor_base.rate.tv_sec;
        check_time.tv_usec = 0;
        opal_event_evtimer_set(orte_event_base, &check_ev, check_heartbeat, &check_ev);
        opal_event_evtimer_add(&check_ev, &check_time);
        check_active = true;
    }
}

static void sample(void)
{
    opal_buffer_t *buf;
    int rc;

    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        return;
    }

    /* if my HNP hasn't been defined yet, ignore - nobody listening yet */
    if (ORTE_JOBID_INVALID == ORTE_PROC_MY_HNP->jobid ||
        ORTE_VPID_INVALID == ORTE_PROC_MY_HNP->vpid) {
        opal_output_verbose(1, orte_sensor_base_framework.framework_output,
                            "%s sensor:heartbeat: HNP is not defined",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                         "%s sending heartbeat",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* if we want sampled data included, point to the bucket */
    buf = OBJ_NEW(opal_buffer_t);
    if (orte_sensor_base.log_samples) {
        opal_dss.copy_payload(buf, orte_sensor_base.samples);
        OBJ_RELEASE(orte_sensor_base.samples);
        /* start a new sample bucket */
        orte_sensor_base.samples = OBJ_NEW(opal_buffer_t);
    }

    /* send heartbeat */
    if (ORTE_SUCCESS != (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf,
                                                      ORTE_RML_TAG_HEARTBEAT,
                                                      orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
    }
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

    OPAL_OUTPUT_VERBOSE((3, orte_sensor_base_framework.framework_output,
                         "%s sensor:check_heartbeat",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        OPAL_OUTPUT_VERBOSE((3,  orte_sensor_base_framework.framework_output,
                             "%s IGNORING CHECK abnorm_term %s fin %s init %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             orte_abnormal_term_ordered ? "TRUE" : "FALSE",
                             orte_finalizing ? "TRUE" : "FALSE",
                             orte_initialized ? "TRUE" : "FALSE"));
        check_active = false;
        return;
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
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                                 "%s sensor:heartbeat DAEMON %s IS NOT RUNNING",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            continue;
        }

        if (0 == proc->beat) {
            /* no heartbeat recvd in last window */
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                                 "%s sensor:check_heartbeat FAILED for daemon %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            ORTE_ACTIVATE_PROC_STATE(&proc->name, ORTE_PROC_STATE_HEARTBEAT_FAILED);
        } else {
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                                 "%s HEARTBEAT DETECTED FOR %s: NUM BEATS %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name), proc->beat));
        }
        /* reset for next period */
        proc->beat = 0;
    }

    /* reset the timer */
    opal_event_evtimer_add(tmp, &check_time);
}

static void recv_beats(int status, orte_process_name_t* sender,
                       opal_buffer_t *buffer,
                       orte_rml_tag_t tag, void *cbdata)
{
    orte_proc_t *proc;
    int rc, n;
    char *component=NULL;
    opal_buffer_t *buf;

    opal_output_verbose(1, orte_sensor_base_framework.framework_output,
                        "%s received beat from %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(sender));

    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        return;
    }

    /* get this daemon's object */
    if (NULL != daemons) {
        if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, sender->vpid))) {
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                                 "%s marked beat from %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(sender)));
            proc->beat++;
            /* if this daemon has reappeared, reset things */
            if (ORTE_PROC_STATE_HEARTBEAT_FAILED == proc->state) {
                proc->state = ORTE_PROC_STATE_RUNNING;
            }
        }
    }

    /* unload any sampled data */
    n=1;
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(buffer, &buf, &n, OPAL_BUFFER))) {
        if (NULL != buf) {
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &component, &n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                break;
            }
            orte_sensor_base_log(component, buf);
            OBJ_RELEASE(buf);
            free(component);
            n=1;
        }
    }
    if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
    }
}
