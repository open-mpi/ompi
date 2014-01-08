/*
 * Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved. 
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
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_ring_buffer.h"
#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "opal/mca/pstat/pstat.h"
#include "opal/mca/db/db.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_globals.h"
#include "orte/orted/orted.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_resusage.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void sample(void);
static void res_log(opal_buffer_t *sample);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_resusage_module = {
    init,
    finalize,
    NULL,
    NULL,
    sample,
    res_log
};

static bool log_enabled = true;

static int init(void)
{
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    return;
}

static void sample(void)
{
    opal_pstats_t *stats, *st;
    opal_node_stats_t *nstats, *nst;
    int rc, i;
    orte_proc_t *child, *hog=NULL;
    float in_use, max_mem;
    opal_buffer_t buf, *bptr;
    char *comp;

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                         "sample:resusage sampling resource usage"));
    
    /* setup a buffer for our stats */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* pack our name */
    comp = strdup("resusage");
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &comp, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return;
    }
    free(comp);

    /* update stats on ourself and the node */
    stats = OBJ_NEW(opal_pstats_t);
    nstats = OBJ_NEW(opal_node_stats_t);
    if (ORTE_SUCCESS != (rc = opal_pstat.query(orte_process_info.pid, stats, nstats))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(stats);
        OBJ_RELEASE(nstats);
        OBJ_DESTRUCT(&buf);
        return;
    }

    /* the stats framework can't know nodename or rank */
    strncpy(stats->node, orte_process_info.nodename, OPAL_PSTAT_MAX_STRING_LEN);
    stats->rank = ORTE_PROC_MY_NAME->vpid;
    /* locally save the stats */
    if (NULL != (st = (opal_pstats_t*)opal_ring_buffer_push(&orte_sensor_base.my_proc->stats, stats))) {
        OBJ_RELEASE(st);
    }
    if (NULL != (nst = (opal_node_stats_t*)opal_ring_buffer_push(&orte_sensor_base.my_node->stats, nstats))) {
        /* release the popped value */
        OBJ_RELEASE(nst);
    }

    /* pack them */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &orte_process_info.nodename, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &nstats, 1, OPAL_NODE_STAT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &stats, 1, OPAL_PSTAT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return;
    }

    /* loop through our children and update their stats */
    if (NULL != orte_local_children) {
        for (i=0; i < orte_local_children->size; i++) {
            if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                continue;
            }
            if (!child->alive) {
                continue;
            }
            if (0 == child->pid) {
                /* race condition */
                continue;
            }
            stats = OBJ_NEW(opal_pstats_t);
            if (ORTE_SUCCESS != opal_pstat.query(child->pid, stats, NULL)) {
                /* may hit a race condition where the process has
                 * terminated, so just ignore any error
                 */
                OBJ_RELEASE(stats);
                continue;
            }
            /* the stats framework can't know nodename or rank */
            strncpy(stats->node, orte_process_info.nodename, OPAL_PSTAT_MAX_STRING_LEN);
            stats->rank = child->name.vpid;
            /* store it */
            if (NULL != (st = (opal_pstats_t*)opal_ring_buffer_push(&child->stats, stats))) {
                OBJ_RELEASE(st);
            }
            /* pack them */
            if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &stats, 1, OPAL_PSTAT))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buf);
                return;
            }
        }
    }

    /* xfer any data for transmission */
    if (0 < buf.bytes_used) {
        bptr = &buf;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(orte_sensor_base.samples, &bptr, 1, OPAL_BUFFER))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return;
        }
    }
    OBJ_DESTRUCT(&buf);

    /* are there any issues with node-level usage? */
    nst = (opal_node_stats_t*)opal_ring_buffer_poke(&orte_sensor_base.my_node->stats, -1);
    if (NULL != nst && 0.0 < mca_sensor_resusage_component.node_memory_limit) {
        OPAL_OUTPUT_VERBOSE((2, orte_sensor_base_framework.framework_output,
                             "%s CHECKING NODE MEM",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* compute the percentage of node memory in-use */
        in_use = 1.0 - (nst->free_mem / nst->total_mem);
        OPAL_OUTPUT_VERBOSE((2, orte_sensor_base_framework.framework_output,
                             "%s PERCENT USED: %f LIMIT: %f",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             in_use, mca_sensor_resusage_component.node_memory_limit));
        if (mca_sensor_resusage_component.node_memory_limit <= in_use) {
            /* loop through our children and find the biggest hog */
            hog = NULL;
            max_mem = 0.0;
            for (i=0; i < orte_local_children->size; i++) {
                if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                    continue;
                }
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
                OPAL_OUTPUT_VERBOSE((5, orte_sensor_base_framework.framework_output,
                                     "%s PROC %s AT VSIZE %f",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name), st->vsize));
                if (max_mem < st->vsize) {
                    hog = child;
                    max_mem = st->vsize;
                }
            }
            if (NULL == hog) {
                /* if all children dead and we are still too big,
                 * then we must be the culprit - abort
                 */
                OPAL_OUTPUT_VERBOSE((2, orte_sensor_base_framework.framework_output,
                                     "%s NO CHILD: COMMITTING SUICIDE",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                orte_errmgr.abort(ORTE_ERR_MEM_LIMIT_EXCEEDED, NULL);
            } else {
                /* report the problem */
                OPAL_OUTPUT_VERBOSE((2, orte_sensor_base_framework.framework_output,
                                     "%s REPORTING %s TO ERRMGR FOR EXCEEDING LIMITS",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&hog->name)));
                ORTE_ACTIVATE_PROC_STATE(&hog->name, ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED);
            }
            /* since we have ordered someone to die, we've done enough for this
             * time around - don't check proc limits as well
             */
            return;
        }
    }

    /* check proc limits */
    if (0.0 < mca_sensor_resusage_component.proc_memory_limit) {
        OPAL_OUTPUT_VERBOSE((2, orte_sensor_base_framework.framework_output,
                             "%s CHECKING PROC MEM",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* check my children first */
        for (i=0; i < orte_local_children->size; i++) {
            if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                continue;
            }
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
            OPAL_OUTPUT_VERBOSE((5, orte_sensor_base_framework.framework_output,
                                 "%s PROC %s AT VSIZE %f",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&child->name), st->vsize));
            if (mca_sensor_resusage_component.proc_memory_limit <= st->vsize) {
                /* report the problem */
                ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED);
            }
        }
    }
}

static void res_log(opal_buffer_t *sample)
{
    opal_pstats_t *st=NULL;
    opal_node_stats_t *nst=NULL;
    int rc, n, i;
    opal_value_t kv[14];
    char *node;

    if (!log_enabled) {
        return;
    }

    /* unpack the node name */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &node, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the node stats */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &nst, &n, OPAL_NODE_STAT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    if (mca_sensor_resusage_component.log_node_stats) {
        /* convert this into an array of opal_value_t's - no clean way
         * to do this, so have to just manually map each field
         */
        for (i=0; i < 13; i++) {
            OBJ_CONSTRUCT(&kv[i], opal_value_t);
        }
        i=0;
        kv[i].key = strdup("ctime");
        kv[i].type = OPAL_TIMEVAL;
        kv[i].data.tv.tv_sec = nst->sample_time.tv_sec;
        kv[i++].data.tv.tv_usec = nst->sample_time.tv_usec;

        kv[i].key = "hostname";
        kv[i].type = OPAL_STRING;
        kv[i++].data.string = strdup(node);

        kv[i].key = strdup("total_mem");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->total_mem;

        kv[i].key = strdup("free_mem");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->free_mem;

        kv[i].key = strdup("buffers");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->buffers;

        kv[i].key = strdup("cached");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->cached;

        kv[i].key = strdup("swap_total");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->swap_total;

        kv[i].key = strdup("swap_free");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->swap_free;

        kv[i].key = strdup("mapped");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->mapped;

        kv[i].key = strdup("swap_cached");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->swap_cached;

        kv[i].key = strdup("la");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->la;

        kv[i].key = strdup("la5");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->la5;

        kv[i].key = strdup("la15");
        kv[i].type = OPAL_FLOAT;
        kv[i++].data.fval = nst->la15;

        /* store it */
        if (ORTE_SUCCESS != (rc = opal_db.add_log("nodestats", kv, 12))) {
            /* don't bark about it - just quietly disable the log */
            log_enabled = false;
        }
        for (i=0; i < 12; i++) {
            OBJ_DESTRUCT(&kv[i]);
        }
    }

    OBJ_RELEASE(nst);

    if (mca_sensor_resusage_component.log_process_stats) {
        /* unpack all process stats */
        n=1;
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(sample, &st, &n, OPAL_PSTAT))) {
            for (i=0; i < 14; i++) {
                OBJ_CONSTRUCT(&kv[i], opal_value_t);
            }
            kv[0].key = strdup("node");
            kv[0].type = OPAL_STRING;
            kv[0].data.string = strdup(st->node);
            kv[1].key = strdup("rank");
            kv[1].type = OPAL_INT32;
            kv[1].data.int32 = st->rank;
            kv[2].key = strdup("pid");
            kv[2].type = OPAL_PID;
            kv[2].data.pid = st->pid;
            kv[3].key = strdup("cmd");
            kv[3].type = OPAL_STRING;
            kv[3].data.string = strdup(st->cmd);
            kv[4].key = strdup("state");
            kv[4].type = OPAL_STRING;
            kv[4].data.string = (char*)malloc(3 * sizeof(char));
            kv[4].data.string[0] = st->state[0];
            kv[4].data.string[1] = st->state[1];
            kv[4].data.string[2] = '\0';
            kv[5].key = strdup("time");
            kv[5].type = OPAL_TIMEVAL;
            kv[5].data.tv.tv_sec = st->time.tv_sec;
            kv[5].data.tv.tv_usec = st->time.tv_usec;
            kv[6].key = strdup("percent_cpu");
            kv[6].type = OPAL_FLOAT;
            kv[6].data.fval = st->percent_cpu;
            kv[7].key = strdup("priority");
            kv[7].type = OPAL_INT32;
            kv[7].data.int32 = st->priority;
            kv[8].key = strdup("num_threads");
            kv[8].type = OPAL_INT16;
            kv[8].data.int16 = st->num_threads;
            kv[9].key = strdup("vsize");
            kv[9].type = OPAL_FLOAT;
            kv[9].data.fval = st->vsize;
            kv[10].key = strdup("rss");
            kv[10].type = OPAL_FLOAT;
            kv[10].data.fval = st->rss;
            kv[11].key = strdup("peak_vsize");
            kv[11].type = OPAL_FLOAT;
            kv[11].data.fval = st->peak_vsize;
            kv[12].key = strdup("processor");
            kv[12].type = OPAL_INT16;
            kv[12].data.int16 = st->processor;
            kv[13].key = strdup("sample_time");
            kv[13].type = OPAL_TIMEVAL;
            kv[13].data.tv.tv_sec = st->sample_time.tv_sec;
            kv[13].data.tv.tv_usec = st->sample_time.tv_usec;
            /* store it */
            if (ORTE_SUCCESS != (rc = opal_db.add_log("procstats", kv, 14))) {
                log_enabled = false;
            }
            for (i=0; i < 14; i++) {
                OBJ_DESTRUCT(&kv[i]);
            }
            OBJ_RELEASE(st);
            n=1;
        }
        if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            ORTE_ERROR_LOG(rc);
        }
    }
}
