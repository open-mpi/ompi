/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
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
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_memusage.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(orte_jobid_t job);
static void stop(orte_jobid_t job);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_memusage_module = {
    init,
    finalize,
    start,
    stop
};

/* define a tracking object */
typedef struct {
    opal_list_item_t super;
    orte_jobid_t jobid;
    unsigned long memory_limit;
} memusage_tracker_t;
static void constructor(memusage_tracker_t *ptr)
{
    ptr->memory_limit = 0;
}
OBJ_CLASS_INSTANCE(memusage_tracker_t,
                   opal_list_item_t,
                   constructor, NULL);

/* declare the local functions */
static void sample(int fd, short event, void *arg);

/* local globals */
static opal_event_t *sample_ev = NULL;
static opal_list_t jobs;
static struct timeval sample_time;

static int init(void)
{
    OBJ_CONSTRUCT(&jobs, opal_list_t);
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    opal_list_item_t *item;
    
    if (NULL != sample_ev) {
        opal_event.del(sample_ev);
        OBJ_RELEASE(sample_ev);
    }
    while (NULL != (item = opal_list_remove_first(&jobs))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&jobs);
    
    return;
}

/*
 * Start monitoring of local processes
 */
static void start(orte_jobid_t jobid)
{
    mca_base_component_t *c = &mca_sensor_memusage_component.super.base_version;
    memusage_tracker_t *job;
    orte_odls_job_t *jobdat;
    orte_app_context_t *app;
    opal_list_item_t *item;
    int rc, tmp;
    
    /* cannot monitor my own job */
    if (jobid == ORTE_PROC_MY_NAME->jobid && ORTE_JOBID_WILDCARD != jobid) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s starting memory monitoring for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jobid)));

    /* get the local jobdat for this job */
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_end(&orte_local_jobdata)) {
        jobdat = (orte_odls_job_t*)item;
        if (jobid == jobdat->jobid || ORTE_JOBID_WILDCARD == jobid) {
            /* must be at least one app_context, so use the first */
            if (NULL == (app = jobdat->apps[0])) {
                /* got a problem */
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                continue;
            }
            
            /* search the environ to get memory limit */
            tmp = 0;
            if (ORTE_SUCCESS != (rc = mca_base_param_find_int(c, "memory_limit", app->env, &tmp))) {
                /* was a default value given */
                if (0 < mca_sensor_memusage_component.memory_limit) {
                    tmp = mca_sensor_memusage_component.memory_limit;
                }
            }
            if (tmp <= 0) {
                /* we don't want to monitor this job */
                OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                     "%s memory monitoring for job %s is not requested",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOBID_PRINT(jobid)));
                continue;
            }
            
            job = OBJ_NEW(memusage_tracker_t);
            job->jobid = jobid;
            job->memory_limit = tmp;
            opal_list_append(&jobs, &job->super);
        }
    }
    
    if (NULL == sample_ev && !opal_list_is_empty(&jobs)) {
        /* startup a timer to wake us up periodically
         * for a data sample
         */
        sample_ev = OBJ_NEW(opal_event_t);
        opal_event.evtimer_set(sample_ev, sample, sample_ev);
        sample_time.tv_sec = mca_sensor_memusage_component.sample_rate;
        sample_time.tv_usec = 0;
        opal_event.evtimer_add(sample_ev, &sample_time);
    }
    return;
}


static void stop(orte_jobid_t jobid)
{
    opal_list_item_t *item;
    memusage_tracker_t *job;

    /* cannot monitor my own job */
    if (jobid == ORTE_PROC_MY_NAME->jobid && ORTE_JOBID_WILDCARD != jobid) {
        return;
    }
    
    for (item = opal_list_get_first(&jobs);
         item != opal_list_get_end(&jobs);
         item = opal_list_get_next(item)) {
        job = (memusage_tracker_t*)item;
        if (jobid == job->jobid || ORTE_JOBID_WILDCARD == jobid) {
            opal_list_remove_item(&jobs, item);
            OBJ_RELEASE(item);
        }
    }
    /* if no jobs remain, stop the sampling */
    if (opal_list_is_empty(&jobs) && NULL != sample_ev) {
        opal_event.del(sample_ev);
        OBJ_RELEASE(sample_ev);
    }
    return;
}

static void sample(int fd, short event, void *arg)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    opal_pstats_t stats;
    int rc;
    memusage_tracker_t *job;
    bool monitored;
    
    /* if we are not sampling any more, then just return */
    if (NULL == sample_ev) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "sample:memusage sampling resource usage"));
    
    /* loop through our local children */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;

        /* is this in a job we are monitoring */
        monitored = false;
        for (item = opal_list_get_first(&jobs);
             item != opal_list_get_end(&jobs);
             item = opal_list_get_next(item)) {
            job = (memusage_tracker_t*)item;
            if (child->name->jobid == job->jobid) {
                monitored = true;
                break;
            }
        }
        if (!monitored) {
            continue;
        }
        
        /* get the process resource utilization stats */
        OBJ_CONSTRUCT(&stats, opal_pstats_t);
        if (ORTE_SUCCESS != (rc = opal_pstat.query(child->pid, &stats))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&stats);
            continue;
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "sample:memusage got memory size of %lu Gbytes for proc %s",
                             (unsigned long)stats.vsize/1000000, ORTE_NAME_PRINT(child->name)));
        
        /* check the memory size for limit */
        if ((stats.vsize/1000000) > job->memory_limit) {
            /* memory limit exceeded */
           orte_show_help("help-orte-sensor-memusage.txt", "mem-limit-exceeded",
                           true, orte_process_info.nodename, ORTE_VPID_PRINT(child->name->vpid),
                           (unsigned long)stats.vsize/1000000, (unsigned long)job->memory_limit);
            orte_errmgr.update_state(child->name->jobid, ORTE_JOB_STATE_SENSOR_BOUND_EXCEEDED,
                                     child->name, ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED,
                                     0, ORTE_ERR_MEM_LIMIT_EXCEEDED);
        }
        OBJ_DESTRUCT(&stats);
    }
    
    /* restart the timer */
    opal_event.evtimer_add(sample_ev, &sample_time);
}
