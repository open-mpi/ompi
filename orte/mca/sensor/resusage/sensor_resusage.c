/*
 * Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved. 
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
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_ring_buffer.h"
#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "opal/mca/pstat/pstat.h"
#include "opal/mca/event/event.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_resusage.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(orte_jobid_t job);
static void stop(orte_jobid_t job);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_resusage_module = {
    init,
    finalize,
    start,
    stop
};

#define ORTE_RESUSAGE_LENGTH  16

/* define a tracking object */
typedef struct {
    opal_object_t super;
    orte_process_name_t name;
    pid_t pid;
    opal_ring_buffer_t stats;
} resusage_tracker_t;
static void constructor(resusage_tracker_t *ptr)
{
    ptr->pid = 0;
    OBJ_CONSTRUCT(&ptr->stats, opal_ring_buffer_t);
    opal_ring_buffer_init(&ptr->stats, ORTE_RESUSAGE_LENGTH);
}
static void destructor(resusage_tracker_t *ptr)
{
    resusage_tracker_t *res;

    while (NULL != (res = opal_ring_buffer_pop(&ptr->stats))) {
        OBJ_RELEASE(res);
    }
    OBJ_DESTRUCT(&ptr->stats);
}
OBJ_CLASS_INSTANCE(resusage_tracker_t,
                   opal_object_t,
                   constructor, destructor);

/* declare the local functions */
static void sample(int fd, short event, void *arg);

/* local globals */
static opal_event_t *sample_ev = NULL;
static opal_pointer_array_t procs;
static struct timeval sample_time;

static int init(void)
{
    OBJ_CONSTRUCT(&procs, opal_pointer_array_t);
    opal_pointer_array_init(&procs, 16, INT_MAX, 16);
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    int i;
    resusage_tracker_t *res;
    
    if (NULL != sample_ev) {
        opal_event_del(sample_ev);
        free(sample_ev);
    }
    for (i=0; i < procs.size; i++) {
        if (NULL != (res = (resusage_tracker_t*)opal_pointer_array_get_item(&procs, i))) {
            OBJ_RELEASE(res);
        }
    }
    OBJ_DESTRUCT(&procs);
    
    return;
}

/*
 * Start monitoring of local processes
 */
static void start(orte_jobid_t jobid)
{
    resusage_tracker_t *res, *rptr;
    orte_odls_child_t *child;
    opal_list_item_t *item;
    int i;

    /* is this to monitor my own job */
    if (jobid == ORTE_PROC_MY_NAME->jobid) {
        /* already on the tracker? */
        res = NULL;
        for (i=0; i < procs.size; i++) {
            if (NULL == (rptr = (resusage_tracker_t*)opal_pointer_array_get_item(&procs, i))) {
                continue;
            }
            if (rptr->name.jobid == jobid &&
                rptr->name.vpid == child->name->vpid) {
                /* got it! */
                res = rptr;
                break;
            }
        }
        if (NULL == res) {
            /* not on here yet, so add it */
            res = OBJ_NEW(resusage_tracker_t);
            res->name.jobid = jobid;
            res->name.vpid = ORTE_PROC_MY_NAME->vpid;
            res->pid = orte_process_info.pid;
            opal_pointer_array_add(&procs, res);
        }
        goto timer;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s sensor:resusage: starting monitoring for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jobid)));

    /* search for local children from this job */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_end(&orte_local_children)) {
        child = (orte_odls_child_t*)item;
        if (jobid == child->name->jobid || ORTE_JOBID_WILDCARD == jobid) {
            
            /* do we already have this proc in our tracker? */
            res = NULL;
            for (i=0; i < procs.size; i++) {
                if (NULL == (rptr = (resusage_tracker_t*)opal_pointer_array_get_item(&procs, i))) {
                    continue;
                }
                if (rptr->name.jobid == jobid &&
                    rptr->name.vpid == child->name->vpid) {
                    /* got it! */
                    res = rptr;
                    break;
                }
            }
            if (NULL == res) {
                /* don't have this one yet, so add it */
                OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                     "%s sensor:resusage: adding tracker for proc %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(child->name)));
                res = OBJ_NEW(resusage_tracker_t);
                res->name.jobid = jobid;
                res->name.vpid = child->name->vpid;
                res->pid = child->pid;
                opal_pointer_array_add(&procs, res);
            }
        }
    }
    
 timer:
    if (NULL == sample_ev) {
        /* startup a timer to wake us up periodically
         * for a data sample
         */
        sample_ev =  (opal_event_t *) malloc(sizeof(opal_event_t));
        opal_event_evtimer_set(opal_event_base, sample_ev, sample, sample_ev);
        sample_time.tv_sec = mca_sensor_resusage_component.sample_rate;
        sample_time.tv_usec = 0;
        opal_event_evtimer_add(sample_ev, &sample_time);
    }
    return;
}


static void stop(orte_jobid_t jobid)
{
    int i;
    resusage_tracker_t *res;

    for (i=0; i < procs.size; i++) {
        if (NULL == (res = (resusage_tracker_t*)opal_pointer_array_get_item(&procs, i))) {
            continue;
        }
        if (jobid == res->name.jobid || ORTE_JOBID_WILDCARD == jobid) {
            opal_pointer_array_set_item(&procs, i, NULL);
            OBJ_RELEASE(res);
        }
    }
    return;
}

static void sample(int fd, short event, void *arg)
{
    resusage_tracker_t *res;
    opal_pstats_t *stats, *st;
    int i, rc;
    
    /* if we are not sampling any more, then just return */
    if (NULL == sample_ev) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "sample:resusage sampling resource usage"));
    
    /* loop through our trackers */
    for (i=0; i < procs.size; i++) {
        if (NULL == (res = (resusage_tracker_t*)opal_pointer_array_get_item(&procs, i))) {
            continue;
        }
        /* get the stats for this process */
        stats = OBJ_NEW(opal_pstats_t);
        if (ORTE_SUCCESS != (rc = opal_pstat.query(res->pid, stats, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(stats);
            continue;
        }
        if (2 < opal_output_get_verbosity(orte_sensor_base.output)) {
            opal_output(0, "%s stats for proc %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&res->name));
            opal_dss.dump(0, stats, OPAL_PSTAT);
        }
        if (NULL != (st = (opal_pstats_t*)opal_ring_buffer_push(&res->stats, stats))) {
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s sensor:resusage: releasing prior sample",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            OBJ_RELEASE(st);
        }
    }

    /* restart the timer */
    opal_event_evtimer_add(sample_ev, &sample_time);
}
