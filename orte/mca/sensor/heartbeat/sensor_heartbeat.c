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
static orte_job_t *daemons=NULL;
static orte_thread_ctl_t ctl;
static bool already_started=false;
static bool use_collected=false;

static int init(void)
{
    int rc=ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s initializing heartbeat recvs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    OBJ_CONSTRUCT(&ctl, orte_thread_ctl_t);
    already_started = false;

    /* check if resource usage is being sampled elsewhere */
    if (NULL != orte_sensor_base.my_proc) {
        use_collected = true;
        /* if I'm the HNP or scheduler, then I need the daemons job object */
        if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_SCHEDULER) {
            if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
                return ORTE_ERR_NOT_FOUND;
            }
        }
    } else {
        /* see if I have a job object */
        if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
            /* create those structs for this framework */
            orte_sensor_base.my_proc = OBJ_NEW(orte_proc_t);
            orte_sensor_base.my_node = OBJ_NEW(orte_node_t);
        } else {
            if (NULL == (orte_sensor_base.my_proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, ORTE_PROC_MY_NAME->vpid))) {
                return ORTE_ERR_NOT_FOUND;
            }
            if (NULL == (orte_sensor_base.my_node = orte_sensor_base.my_proc->node)) {
                return ORTE_ERR_NOT_FOUND;
            }
            /* protect the objects */
            OBJ_RETAIN(orte_sensor_base.my_proc);
            OBJ_RETAIN(orte_sensor_base.my_node);
        }
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

    OBJ_RELEASE(orte_sensor_base.my_proc);
    OBJ_RELEASE(orte_sensor_base.my_node);

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

static void read_stats(int fd, short event, void *arg)
{
    opal_event_t *tmp = (opal_event_t*)arg;
    int rc;
    opal_pstats_t *stats, *st;
    opal_node_stats_t *nstats, *ndstats;

    ORTE_ACQUIRE_THREAD(&ctl);

    if (use_collected) {
        /* nothing for us to do - already have the data */
        goto reset;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s sensor:heartbeat READING LOCAL STATS",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* get data on myself and the local node */
    stats = OBJ_NEW(opal_pstats_t);
    nstats = OBJ_NEW(opal_node_stats_t);
    if (ORTE_SUCCESS != (rc = opal_pstat.query(orte_process_info.pid, stats, nstats))) {
        ORTE_ERROR_LOG(rc);
        goto reset;
    }

    /* store the proc stats */
    if (NULL != (st = (opal_pstats_t*)opal_ring_buffer_push(&orte_sensor_base.my_proc->stats, stats))) {
        OBJ_RELEASE(st);
    }
    /* store the node stats */
    if (NULL != (ndstats = (opal_node_stats_t*)opal_ring_buffer_push(&orte_sensor_base.my_node->stats, nstats))) {
        OBJ_RELEASE(ndstats);
    }

 reset:
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
    opal_pstats_t *st;
    opal_node_stats_t *nst;

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
    
    /* if we want process stats included, better get them */
    if (mca_sensor_heartbeat_component.include_stats) {
        /* include data on myself and on the node */
        if (use_collected) {
            if (NULL == (st = (opal_pstats_t*)opal_ring_buffer_poke(&orte_sensor_base.my_proc->stats, -1))) {
                goto BEAT;
            }
            if (NULL == (nst = (opal_node_stats_t*)opal_ring_buffer_poke(&orte_sensor_base.my_node->stats, -1))) {
                goto BEAT;
            }
            /* protect the objects */
            OBJ_RETAIN(st);
            OBJ_RETAIN(nst);
        } else {
            st = OBJ_NEW(opal_pstats_t);
            nst = OBJ_NEW(opal_node_stats_t);
            if (ORTE_SUCCESS != (rc = opal_pstat.query(orte_process_info.pid, st, nst))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(st);
                OBJ_RELEASE(nst);
                /* turn off the stats as it won't work */
                mca_sensor_heartbeat_component.include_stats = false;
                goto BEAT;
            }
            /* the stats framework can't know nodename or rank, so fill them
             * in here and pack send my own data
             */
            strncpy(st->node, orte_process_info.nodename, OPAL_PSTAT_MAX_STRING_LEN);
            st->rank = ORTE_PROC_MY_NAME->vpid;
        }
        /* pack the node stats first */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &nst, 1, OPAL_NODE_STAT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(st);
            OBJ_RELEASE(nst);
            goto BEAT;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &st, 1, OPAL_PSTAT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(st);
            OBJ_RELEASE(nst);
            goto BEAT;
        }
        OBJ_RELEASE(st);
        OBJ_RELEASE(nst);
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
            if (use_collected) {
                if (NULL == (st = (opal_pstats_t*)opal_ring_buffer_poke(&child->stats, -1))) {
                    continue;
                }
                /* protect the object */
                OBJ_RETAIN(st);
            } else {
                st = OBJ_NEW(opal_pstats_t);
                if (ORTE_SUCCESS != (rc = opal_pstat.query(child->pid, st, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(st);
                    continue;
                }
                /* the stats framework can't know nodename or rank, so fill them
                 * in here
                 */
                strncpy(st->node, orte_process_info.nodename, OPAL_PSTAT_MAX_STRING_LEN);
                st->rank = child->name->vpid;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, child->name, 1, ORTE_NAME))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(st);
                continue;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &st, 1, OPAL_PSTAT))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(st);
                continue;
            }
            OBJ_RELEASE(st);
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
    opal_pstats_t *stats, *st;
    opal_node_stats_t *nstats=NULL, *ndstats;
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
        /* store the node stats */
        if (NULL != proc->node) {
            if (NULL != (ndstats = (opal_node_stats_t*)opal_ring_buffer_push(&proc->node->stats, nstats))) {
                OBJ_RELEASE(ndstats);
            }
        }
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
        /* store this data */
        if (NULL != (st = (opal_pstats_t*)opal_ring_buffer_push(&proc->stats, stats))) {
            OBJ_RELEASE(st);
        }
 
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
            /* store this data */
            if (NULL != (st = (opal_pstats_t*)opal_ring_buffer_push(&proc->stats, stats))) {
                OBJ_RELEASE(st);
            }
        }
        if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            ORTE_ERROR_LOG(rc);
        }
    }

 DEPART:
    ORTE_RELEASE_THREAD(&ctl);
}
