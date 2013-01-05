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

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/mca/db/db.h"
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

#define ORTE_RESUSAGE_LENGTH  16
static int line_count = 0;
static bool log_enabled = true;
static FILE *nstat_fp, *pstat_fp;

static int init(void)
{
    if (NULL != mca_sensor_resusage_component.nstat_log) {
        if (0 == strcmp(mca_sensor_resusage_component.nstat_log, "-")) {
            nstat_fp = stdout;
        } else if (0 == strcmp(mca_sensor_resusage_component.nstat_log, "+")) {
            nstat_fp = stderr;
        } else {
            nstat_fp = fopen(mca_sensor_resusage_component.nstat_log, "w");
        }
    }

    if (NULL != mca_sensor_resusage_component.pstat_log) {
        if (0 == strcmp(mca_sensor_resusage_component.pstat_log, "-")) {
            pstat_fp = stdout;
        } else if (0 == strcmp(mca_sensor_resusage_component.pstat_log, "+")) {
            pstat_fp = stderr;
        } else {
            pstat_fp = fopen(mca_sensor_resusage_component.pstat_log, "w");
        }
    }

    return ORTE_SUCCESS;
}

static void finalize(void)
{
    if (NULL != mca_sensor_resusage_component.nstat_log &&
        0 != strcmp(mca_sensor_resusage_component.nstat_log, "-") &&
        0 != strcmp(mca_sensor_resusage_component.nstat_log, "+")) {
        fclose(nstat_fp);
    }

    if (NULL != mca_sensor_resusage_component.pstat_log &&
        0 != strcmp(mca_sensor_resusage_component.pstat_log, "-") &&
        0 != strcmp(mca_sensor_resusage_component.pstat_log, "+")) {
        fclose(pstat_fp);
    }
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

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
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
            if (ORTE_SUCCESS != (rc = opal_pstat.query(child->pid, stats, NULL))) {
                ORTE_ERROR_LOG(rc);
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

    /* xfer the data for transmission */
    bptr = &buf;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(orte_sensor_base.samples, &bptr, 1, OPAL_BUFFER))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return;
    }
    OBJ_DESTRUCT(&buf);

    /* are there any issues with node-level usage? */
    nst = (opal_node_stats_t*)opal_ring_buffer_poke(&orte_sensor_base.my_node->stats, -1);
    if (NULL != nst && 0.0 < mca_sensor_resusage_component.node_memory_limit) {
        OPAL_OUTPUT_VERBOSE((2, orte_sensor_base.output,
                             "%s CHECKING NODE MEM",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* compute the percentage of node memory in-use */
        in_use = 1.0 - (nst->free_mem / nst->total_mem);
        OPAL_OUTPUT_VERBOSE((2, orte_sensor_base.output,
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
                OPAL_OUTPUT_VERBOSE((5, orte_sensor_base.output,
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
                OPAL_OUTPUT_VERBOSE((2, orte_sensor_base.output,
                                     "%s NO CHILD: COMMITTING SUICIDE",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                orte_errmgr.abort(ORTE_ERR_MEM_LIMIT_EXCEEDED, NULL);
            } else {
                /* report the problem */
                OPAL_OUTPUT_VERBOSE((2, orte_sensor_base.output,
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
        OPAL_OUTPUT_VERBOSE((2, orte_sensor_base.output,
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
            OPAL_OUTPUT_VERBOSE((5, orte_sensor_base.output,
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

    if (NULL != mca_sensor_resusage_component.nstat_log) {
        if (0 == line_count) {
            /* print the column headers */
            fprintf(nstat_fp, "Node\tSampleTime\tTotMem\tLdAvg\tLdAvg5\tLdAvg15\tFreeMem\tBuffers\tCached\tSwapCached\tSwapTotal\tSwapFree\tMapped\n");
        }
        fprintf(nstat_fp, "%s\t%d.%06d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\n",
                node, (int)nst->sample_time.tv_sec, (int)nst->sample_time.tv_usec,
                nst->total_mem, nst->la, nst->la5, nst->la15, nst->free_mem, nst->buffers,
                nst->cached, nst->swap_cached, nst->swap_total, nst->swap_free, nst->mapped);
    }

    if (log_enabled) {
        /* convert this into an array of opal_value_t's - no clean way
         * to do this, so have to just manually map each field
         */
        for (i=0; i < 12; i++) {
            OBJ_CONSTRUCT(&kv[i], opal_value_t);
        }
        kv[0].key = strdup("la");
        kv[0].type = OPAL_FLOAT;
        kv[0].data.fval = nst->la;
        kv[1].key = strdup("la5");
        kv[1].type = OPAL_FLOAT;
        kv[1].data.fval = nst->la5;
        kv[2].key = strdup("la15");
        kv[2].type = OPAL_FLOAT;
        kv[2].data.fval = nst->la15;
        kv[3].key = strdup("total_mem");
        kv[3].type = OPAL_FLOAT;
        kv[3].data.fval = nst->total_mem;
        kv[4].key = strdup("free_mem");
        kv[4].type = OPAL_FLOAT;
        kv[4].data.fval = nst->free_mem;
        kv[5].key = strdup("buffers");
        kv[5].type = OPAL_FLOAT;
        kv[5].data.fval = nst->buffers;
        kv[6].key = strdup("cached");
        kv[6].type = OPAL_FLOAT;
        kv[6].data.fval = nst->cached;
        kv[7].key = strdup("swap_cached");
        kv[7].type = OPAL_FLOAT;
        kv[7].data.fval = nst->swap_cached;
        kv[8].key = strdup("swap_total");
        kv[8].type = OPAL_FLOAT;
        kv[8].data.fval = nst->swap_total;
        kv[9].key = strdup("swap_free");
        kv[9].type = OPAL_FLOAT;
        kv[9].data.fval = nst->swap_free;
        kv[10].key = strdup("mapped");
        kv[10].type = OPAL_FLOAT;
        kv[10].data.fval = nst->mapped;
        kv[11].key = strdup("sample_time");
        kv[11].type = OPAL_TIMEVAL;
        kv[11].data.tv.tv_sec = nst->sample_time.tv_sec;
        kv[11].data.tv.tv_usec = nst->sample_time.tv_usec;

        /* store it */
        if (ORTE_SUCCESS != (rc = orte_db.add_log("nodestats", kv, 12))) {
            /* don't bark about it - just quietly disable the log */
            log_enabled = false;
        }
        for (i=0; i < 12; i++) {
            OBJ_DESTRUCT(&kv[i]);
        }
    }

    OBJ_RELEASE(nst);

    /* unpack all process stats */
    n=1;
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(sample, &st, &n, OPAL_PSTAT))) {
        if (NULL != mca_sensor_resusage_component.pstat_log) {
            if (0 == line_count) {
                /* print the column headers */
                fprintf(pstat_fp, "Node\tSampleTime\tRank\tPid\tCmd\tState\tTime\tCpu\tPri\tNumThreads\tProcessor\tVSIZE\tRSS\tPeakVSIZE\n");
            }
            fprintf(pstat_fp, "%s\t%d.%06d\t%lu\t%s\t%c\t%d.%06d\t%f\t%d\t%d\t%d\t%f\t%f\t%f\n",
                    node, (int)st->sample_time.tv_sec, (int)st->sample_time.tv_usec,
                    (unsigned long)st->pid, st->cmd, st->state[0],
                    (int)st->time.tv_sec, (int)st->time.tv_usec, st->percent_cpu,
                    st->priority, (int)st->num_threads, (int)st->processor,
                    st->vsize, st->rss, st->peak_vsize);
        }
        if (log_enabled) {
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
            if (ORTE_SUCCESS != (rc = orte_db.add_log("procstats", kv, 14))) {
                log_enabled = false;
            }
            for (i=0; i < 14; i++) {
                OBJ_DESTRUCT(&kv[i]);
            }
        }
        OBJ_RELEASE(st);
        n=1;
    }
    if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
    }
    line_count++;
    if (30 == line_count) {
        line_count = 0;
    }
}
