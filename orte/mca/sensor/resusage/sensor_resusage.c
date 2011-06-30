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
#include "orte/mca/odls/base/odls_private.h"
#include "orte/runtime/orte_globals.h"
#include "orte/orted/orted.h"

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


/* declare the local functions */
static void sample(int fd, short event, void *arg);

/* local globals */
static opal_event_t *sample_ev = NULL;
static struct timeval sample_time;
static bool created = false;

static int init(void)
{
    orte_job_t *jdata;

    if (0 == mca_sensor_resusage_component.sample_rate) {
        /* not monitoring */
        return ORTE_ERROR;
    }

    /* see if my_proc and my_node are available on the global arrays */
    if (NULL == (jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        orte_sensor_base.my_proc = OBJ_NEW(orte_proc_t);
        orte_sensor_base.my_node = OBJ_NEW(orte_node_t);
        created = true;
    } else {
        if (NULL == (orte_sensor_base.my_proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, ORTE_PROC_MY_NAME->vpid))) {
            return ORTE_ERR_NOT_FOUND;
        }
        if (NULL == (orte_sensor_base.my_node = orte_sensor_base.my_proc->node)) {
            return ORTE_ERR_NOT_FOUND;
        }
        created = false;
    }

    return ORTE_SUCCESS;
}

static void finalize(void)
{
    if (NULL != sample_ev) {
        opal_event_del(sample_ev);
        free(sample_ev);
        sample_ev = NULL;
    }
    
    if (created) {
        OBJ_RELEASE(orte_sensor_base.my_proc);
        OBJ_RELEASE(orte_sensor_base.my_node);
    }

    return;
}

/*
 * Start monitoring of local processes
 */
static void start(orte_jobid_t jobid)
{
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
    if (NULL != sample_ev) {
        opal_event_del(sample_ev);
        free(sample_ev);
        sample_ev = NULL;
    }
    return;
}

static void sample(int fd, short event, void *arg)
{
    opal_pstats_t *stats, *st;
    opal_node_stats_t *nstats, *nst;
    int rc;
    opal_list_item_t *item;
    orte_odls_child_t *child, *hog=NULL;
    float in_use, max_mem;

    /* if we are not sampling any more, then just return */
    if (NULL == sample_ev) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "sample:resusage sampling resource usage"));
    
    /* update stats on ourself and the node */
    stats = OBJ_NEW(opal_pstats_t);
    nstats = OBJ_NEW(opal_node_stats_t);
    if (ORTE_SUCCESS != (rc = opal_pstat.query(orte_process_info.pid, stats, nstats))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(stats);
        OBJ_RELEASE(nstats);
        goto RESTART;
    }
    /* the stats framework can't know nodename or rank */
    strncpy(stats->node, orte_process_info.nodename, OPAL_PSTAT_MAX_STRING_LEN);
    stats->rank = ORTE_PROC_MY_NAME->vpid;
    /* store it */
    if (NULL != (st = (opal_pstats_t*)opal_ring_buffer_push(&orte_sensor_base.my_proc->stats, stats))) {
        OBJ_RELEASE(st);
    }
    if (NULL != (nst = (opal_node_stats_t*)opal_ring_buffer_push(&orte_sensor_base.my_node->stats, nstats))) {
        OBJ_RELEASE(nst);
    }

    /* loop through our children and update their stats */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        if (!child->alive) {
            continue;
        }
        if (0 == child->pid) {
            /* race condition */
            continue;
        }
        stats = OBJ_NEW(opal_pstats_t);
        if (ORTE_SUCCESS != (rc = opal_pstat.query(child->pid, stats, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(stats);
            continue;
        }
        /* the stats framework can't know nodename or rank */
        strncpy(stats->node, orte_process_info.nodename, OPAL_PSTAT_MAX_STRING_LEN);
        stats->rank = child->name->vpid;
        /* store it */
        if (NULL != (st = (opal_pstats_t*)opal_ring_buffer_push(&child->stats, stats))) {
            OBJ_RELEASE(st);
        }
    }

    /* are there any issues with node-level usage? */
    if (0.0 < mca_sensor_resusage_component.node_memory_limit) {
        OPAL_OUTPUT_VERBOSE((2, orte_sensor_base.output,
                             "%s CHECKING NODE MEM",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* compute the percentage of node memory in-use */
        if (NULL == (nst = (opal_node_stats_t*)opal_ring_buffer_poke(&orte_sensor_base.my_node->stats, -1))) {
            goto RELEASE;
        }
        in_use = 1.0 - (nst->free_mem / nst->total_mem);
        OPAL_OUTPUT_VERBOSE((2, orte_sensor_base.output,
                             "%s PERCENT USED: %f LIMIT: %f",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             in_use, mca_sensor_resusage_component.node_memory_limit));
        if (mca_sensor_resusage_component.node_memory_limit <= in_use) {
            /* loop through our children and find the biggest hog */
            hog = NULL;
            max_mem = 0.0;
            for (item = opal_list_get_first(&orte_local_children);
                 item != opal_list_get_end(&orte_local_children);
                 item = opal_list_get_next(item)) {
                child = (orte_odls_child_t*)item;
                if (!child->alive) {
                    continue;
                }
                if (0 == child->pid) {
                    /* race condition */
                    continue;
                }
                if (NULL == (st = (opal_pstats_t*)opal_ring_buffer_poke(&child->stats, -1))) {
                    continue;
                }
                OPAL_OUTPUT_VERBOSE((5, orte_sensor_base.output,
                                     "%s PROC %s AT VSIZE %f",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(child->name), st->vsize));
                if (max_mem < st->vsize) {
                    hog = child;
                    max_mem = st->vsize;
                }
            }
            if (NULL == hog) {
                /* if all children dead and we are still too big,
                 * then we must be the culprit - abort
                 */
                OPAL_OUTPUT_VERBOSE((2, orte_sensor_base.output,
                                     "%s NO CHILD: COMMITTING SUICIDE",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                opal_condition_signal(&orte_odls_globals.cond);
                OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
                orte_errmgr.abort(ORTE_ERR_MEM_LIMIT_EXCEEDED, NULL);
            } else {
                /* report the problem - this will normally kill the proc, so
                 * we have to release the ODLS thread first
                 */
                OPAL_OUTPUT_VERBOSE((2, orte_sensor_base.output,
                                     "%s REPORTING %s TO ERRMGR FOR EXCEEDING LIMITS",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(hog->name)));
                opal_condition_signal(&orte_odls_globals.cond);
                OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
                orte_errmgr.update_state(hog->name->jobid, ORTE_JOB_STATE_UNDEF,
                                         hog->name, ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED,
                                         hog->pid, ORTE_ERR_MEM_LIMIT_EXCEEDED);
            }
            goto RESTART;
        }
    }

    /* check proc limits */
    if (0.0 < mca_sensor_resusage_component.proc_memory_limit) {
        OPAL_OUTPUT_VERBOSE((2, orte_sensor_base.output,
                             "%s CHECKING PROC MEM",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* check my children first */
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;
            if (!child->alive) {
                continue;
            }
            if (0 == child->pid) {
                /* race condition */
                continue;
            }
            if (NULL == (st = (opal_pstats_t*)opal_ring_buffer_poke(&child->stats, -1))) {
                continue;
            }
            OPAL_OUTPUT_VERBOSE((5, orte_sensor_base.output,
                                 "%s PROC %s AT VSIZE %f",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name), st->vsize));
            if (mca_sensor_resusage_component.proc_memory_limit <= st->vsize) {
                /* report the problem - this will normally kill the proc, so
                 * we have to release the ODLS thread first
                 */
                opal_condition_signal(&orte_odls_globals.cond);
                OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
                orte_errmgr.update_state(child->name->jobid, ORTE_JOB_STATE_UNDEF,
                                         child->name, ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED,
                                         child->pid, ORTE_ERR_MEM_LIMIT_EXCEEDED);
                OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
             }
        }
    }

 RELEASE:
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);

 RESTART:
    /* restart the timer */
    if (NULL != sample_ev) {
        opal_event_evtimer_add(sample_ev, &sample_time);
    }
}
