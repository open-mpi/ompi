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
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/rmcast/rmcast.h"
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
static void read_stats(int fd, short event, void *arg);
static void check_heartbeat(int fd, short event, void *arg);
static void send_heartbeat(int fd, short event, void *arg);
static void recv_beats(int status,
                       orte_rmcast_channel_t channel,
                       orte_rmcast_seq_t seq_num,
                       orte_rmcast_tag_t tag,
                       orte_process_name_t *sender,
                       opal_buffer_t *buf, void* cbdata);
static void cbfunc(int status,
                   orte_rmcast_channel_t channel,
                   orte_rmcast_seq_t seq_num,
                   orte_rmcast_tag_t tag,
                   orte_process_name_t *sender,
                   opal_buffer_t *buf, void* cbdata)
{
    OBJ_RELEASE(buf);
}

/* local globals */
static opal_event_t *send_ev = NULL, *check_ev = NULL;
static struct timeval send_time, check_time;
static orte_job_t *daemons;
static orte_thread_ctl_t ctl;
static bool already_started;

static int init(void)
{
    int rc=ORTE_SUCCESS;
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s initializing heartbeat recvs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    OBJ_CONSTRUCT(&ctl, orte_thread_ctl_t);
    already_started = false;

    /* get the daemon job object */
    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        /* can't run */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    /* setup to receive heartbeats */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_SCHEDULER) {
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_buffer_nb(ORTE_RMCAST_HEARTBEAT_CHANNEL,
                                                             ORTE_RMCAST_TAG_HEARTBEAT,
                                                             ORTE_RMCAST_PERSISTENT,
                                                             recv_beats,
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
    
    orte_rmcast.cancel_recv(ORTE_RMCAST_HEARTBEAT_CHANNEL, ORTE_RMCAST_TAG_HEARTBEAT);

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

    /* if this isn't my jobid, then don't start or we can
     * confuse things
     */
    if (already_started || ORTE_PROC_MY_NAME->jobid != jobid) {
        return;
    }
    already_started = true;

    /* convert the send rate */
    setup_time(mca_sensor_heartbeat_component.rate, &send_time);
    /* convert the check rate */
    setup_time(mca_sensor_heartbeat_component.check, &check_time);

    /* only daemons send heartbeats */
    if (ORTE_PROC_IS_DAEMON) {
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
        if (0 == check_time.tv_sec &&
            0 == check_time.tv_usec) {
            /* no sense in running if we won't check */
            return;
        }

        /* setup the check */
        check_ev = (opal_event_t*)malloc(sizeof(opal_event_t));
        opal_event_evtimer_set(opal_event_base, check_ev, check_heartbeat, check_ev);
        opal_event_evtimer_add(check_ev, &check_time);

        /* if we want stats, then we'll setup our own timer
         * to catch stats on ourself - avoid this if
         * send timer wasn't defined for us as otherwise
         * we'll swamp the system with stat checks on ourself
         */
        if (mca_sensor_heartbeat_component.include_stats) {
            if (0 == send_time.tv_sec &&
                0 == send_time.tv_usec) {
                /* nothing to do */
                return;
            }
            send_ev = (opal_event_t*)malloc(sizeof(opal_event_t));
            opal_event_evtimer_set(opal_event_base, send_ev, read_stats, send_ev);
            opal_event_evtimer_add(send_ev, &send_time);
        }
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
    already_started = false;

    return;
}

static void copy_proc_stats(opal_pstats_t *dest, opal_pstats_t *src)
{
    long secs, usecs;
    float diff, usage;

    /* copy the individual fields */
    strncpy(dest->node, orte_process_info.nodename, OPAL_PSTAT_MAX_STRING_LEN);
    dest->rank = ORTE_PROC_MY_NAME->vpid;
    dest->pid = src->pid;
    memcpy(dest->cmd, src->cmd, sizeof(src->cmd));
    dest->state[0] = src->state[0];
    dest->priority = src->priority;
    dest->num_threads = src->num_threads;
    dest->vsize = src->vsize;
    dest->rss = src->rss;
    dest->peak_vsize = src->peak_vsize;
    dest->processor = src->processor;
    /* update the cpu utilization prior to copying the sample time */
    if (0 < dest->sample_time.tv_sec &&
        0 < dest->sample_time.tv_usec) {
        ORTE_COMPUTE_TIME_DIFF(secs, usecs, dest->sample_time.tv_sec, dest->sample_time.tv_usec,
                               src->sample_time.tv_sec, src->sample_time.tv_usec);
        diff = (float)secs + (float)usecs/1000000.0;
        ORTE_COMPUTE_TIME_DIFF(secs, usecs, dest->time.tv_sec, dest->time.tv_usec,
                               src->time.tv_sec, src->time.tv_usec);
        usage = (float)secs + (float)usecs/1000000.0;
        dest->percent_cpu = usage / diff;
    }
    dest->time.tv_sec = src->time.tv_sec;
    dest->time.tv_usec = src->time.tv_usec;
    dest->sample_time.tv_sec = src->sample_time.tv_sec;
    dest->sample_time.tv_usec = src->sample_time.tv_usec;
}

static void copy_node_stats(opal_node_stats_t *dest, opal_node_stats_t *src)
{
    dest->total_mem = src->total_mem;
    dest->free_mem = src->free_mem;
    dest->buffers = src->buffers;
    dest->cached = src->cached;
    dest->swap_cached = src->swap_cached;
    dest->swap_total = src->swap_total;
    dest->swap_free = src->swap_free;
    dest->mapped = src->mapped;
    dest->la = src->la;
    dest->la5 = src->la5;
    dest->la15 = src->la15;
    dest->sample_time.tv_sec = src->sample_time.tv_sec;
    dest->sample_time.tv_usec = src->sample_time.tv_usec;
}

static void read_stats(int fd, short event, void *arg)
{
    opal_event_t *tmp = (opal_event_t*)arg;
    int rc;
    opal_pstats_t stats;
    opal_node_stats_t nstats;
    orte_job_t *jdata;
    orte_proc_t *proc;

    ORTE_ACQUIRE_THREAD(&ctl);

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s sensor:heartbeat READING LOCAL STATS",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* get data on myself and the local node */
    OBJ_CONSTRUCT(&stats, opal_pstats_t);
    OBJ_CONSTRUCT(&nstats, opal_node_stats_t);
    if (ORTE_SUCCESS != (rc = opal_pstat.query(orte_process_info.pid, &stats, &nstats))) {
        ORTE_ERROR_LOG(rc);
        goto reset;
    }

    /* get my job object */
    if (NULL == (jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        goto reset;
    }
    /* find my proc object */
    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, ORTE_PROC_MY_NAME->vpid))) {
        goto reset;
    }
    /* copy the proc stats */
    copy_proc_stats(&proc->stats, &stats);
    /* copy the node stats */
    if (NULL != proc->node) {
        copy_node_stats(&proc->node->stats, &nstats);
    }

 reset:
    OBJ_DESTRUCT(&stats);
    OBJ_DESTRUCT(&nstats);
    ORTE_RELEASE_THREAD(&ctl);

    /* reset the timer */
    opal_event_evtimer_add(tmp, &send_time);
}

static void send_heartbeat(int fd, short event, void *arg)
{
    opal_buffer_t *buf;
    opal_event_t *tmp = (opal_event_t*)arg;
    int rc;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    opal_pstats_t stats, *st;
    opal_node_stats_t nstats, *nst;

    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        return;
    }

    ORTE_ACQUIRE_THREAD(&ctl);

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
    st = &stats;
    nst = &nstats;
    
    /* if we want process stats included, better get them */
    if (mca_sensor_heartbeat_component.include_stats) {
        /* include data on myself and on the node */
        OBJ_CONSTRUCT(&stats, opal_pstats_t);
        OBJ_CONSTRUCT(&nstats, opal_node_stats_t);
        if (ORTE_SUCCESS != (rc = opal_pstat.query(orte_process_info.pid, &stats, &nstats))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&stats);
            OBJ_DESTRUCT(&nstats);
            /* turn off the stats as it won't work */
            mca_sensor_heartbeat_component.include_stats = false;
            goto BEAT;
        }
        /* pack the node stats first */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &nst, 1, OPAL_NODE_STAT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&stats);
            OBJ_DESTRUCT(&nstats);
            goto BEAT;
        }
        OBJ_DESTRUCT(&nstats);
        /* the stats framework can't know nodename or rank, so fill them
         * in here and pack send my own data
         */
        strncpy(stats.node, orte_process_info.nodename, OPAL_PSTAT_MAX_STRING_LEN);
        stats.rank = ORTE_PROC_MY_NAME->vpid;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &st, 1, OPAL_PSTAT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&stats);
            goto BEAT;
        }
        OBJ_DESTRUCT(&stats);
        /* add data for my children */
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
            OBJ_CONSTRUCT(&stats, opal_pstats_t);
            if (ORTE_SUCCESS != (rc = opal_pstat.query(child->pid, &stats, NULL))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&stats);
                continue;
            }
            /* the stats framework can't know nodename or rank, so fill them
             * in here
             */
            strncpy(stats.node, orte_process_info.nodename, OPAL_PSTAT_MAX_STRING_LEN);
            stats.rank = child->name->vpid;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, child->name, 1, ORTE_NAME))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&stats);
                continue;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &st, 1, OPAL_PSTAT))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&stats);
                continue;
            }
            OBJ_DESTRUCT(&stats);
        }
        opal_condition_signal(&orte_odls_globals.cond);
        OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
    }

 BEAT:
    /* send heartbeat */
    if (0 > (rc = orte_rmcast.send_buffer_nb(ORTE_RMCAST_HEARTBEAT_CHANNEL,
                                             ORTE_RMCAST_TAG_HEARTBEAT, buf,
                                             cbfunc, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
    }
    
 reset:
    ORTE_RELEASE_THREAD(&ctl);

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

    OPAL_OUTPUT_VERBOSE((3, orte_sensor_base.output,
                         "%s sensor:check_heartbeat",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || orte_finalizing || !orte_initialized) {
        OPAL_OUTPUT_VERBOSE((3,  orte_sensor_base.output,
                             "%s IGNORING CHECK abnorm_term %s fin %s init %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             orte_abnormal_term_ordered ? "TRUE" : "FALSE",
                             orte_finalizing ? "TRUE" : "FALSE",
                             orte_initialized ? "TRUE" : "FALSE"));
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

        if (0 == proc->beat) {
            /* no heartbeat recvd in last window */
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s sensor:check_heartbeat FAILED for daemon %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            orte_errmgr.update_state(ORTE_PROC_MY_NAME->jobid, ORTE_JOB_STATE_HEARTBEAT_FAILED,
                                     &proc->name, ORTE_PROC_STATE_HEARTBEAT_FAILED,
                                     0, ORTE_ERR_HEARTBEAT_LOST);
        } else {
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s HEARTBEAT DETECTED FOR %s: NUM BEATS %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name), proc->beat));
        }
        /* reset for next period */
        proc->beat = 0;
    }

 reset:
    ORTE_RELEASE_THREAD(&ctl);

    /* reset the timer */
    opal_event_evtimer_add(tmp, &check_time);
}

static void recv_beats(int status,
                       orte_rmcast_channel_t channel,
                       orte_rmcast_seq_t seq_num,
                       orte_rmcast_tag_t tag,
                       orte_process_name_t *sender,
                       opal_buffer_t *buf, void* cbdata)
{
    orte_job_t *jdata;
    orte_proc_t *proc;
    opal_pstats_t *stats;
    opal_node_stats_t *nstats=NULL;
    orte_process_name_t name;
    int rc, n;

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
        proc->beat++;
        /* if this daemon has reappeared, reset things */
        if (ORTE_PROC_STATE_HEARTBEAT_FAILED == proc->state) {
            proc->state = ORTE_PROC_STATE_RUNNING;
        }
    }

    if (mca_sensor_heartbeat_component.include_stats) {
        /* unload the node stats */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &nstats, &n, OPAL_NODE_STAT))) {
            ORTE_ERROR_LOG(rc);
            /* turn off the stats */
            mca_sensor_heartbeat_component.include_stats = false;
            goto DEPART;
        }
        /* since we already have the daemon's proc object, store this data */
        if (NULL != proc->node) {
            copy_node_stats(&proc->node->stats, nstats);
        }
        OBJ_RELEASE(nstats);
        /* the first proc in the data will be the daemon, so get it now while
         * we still have the daemon's proc object
         */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &stats, &n, OPAL_PSTAT))) {
            ORTE_ERROR_LOG(rc);
            /* turn off the stats */
            mca_sensor_heartbeat_component.include_stats = false;
            goto DEPART;
        }
        copy_proc_stats(&proc->stats, stats);
        /* cleanup memory */
        OBJ_RELEASE(stats);
 
        /* now retrieve the data for each proc on that node */
        n=1;
        while (ORTE_SUCCESS == (rc = opal_dss.unpack(buf, &name, &n, ORTE_NAME))) {
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &stats, &n, OPAL_PSTAT))) {
                ORTE_ERROR_LOG(rc);
                break;
            }
            n=1;
            /* get the job object */
            if (NULL == (jdata = orte_get_job_data_object(name.jobid))) {
                OBJ_RELEASE(stats);
                continue;
            }
            /* find this proc */
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, name.vpid))) {
                OBJ_RELEASE(stats);
                continue;
            }
            copy_proc_stats(&proc->stats, stats);
            /* cleanup memory */
            OBJ_RELEASE(stats);
        }
        if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            ORTE_ERROR_LOG(rc);
        }
    }

 DEPART:
    ORTE_RELEASE_THREAD(&ctl);
}
