/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 *
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"
#include "pmix_common.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <string.h>
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif

#include "src/include/pmix_globals.h"
#include "src/mca/pstat/base/base.h"
#include "src/mca/pstat/pstat.h"

#include "pstat_test.h"

static int init(void);
static int query(pid_t pid, pmix_proc_stats_t *stats, pmix_node_stats_t *nstats);
static int fini(void);

/*
 * Test pstat module
 */
const pmix_pstat_base_module_t pmix_pstat_test_module = {init, query, fini};

static int init(void)
{
    return PMIX_SUCCESS;
}

static int fini(void)
{
    return PMIX_SUCCESS;
}

static int query(pid_t pid, pmix_proc_stats_t *stats, pmix_node_stats_t *nstats)
{
    double dtime;

    if (NULL != stats) {
        /* record the time of this sample */
        gettimeofday(&stats->sample_time, NULL);
        /* check the nstats - don't do gettimeofday twice
         * as it is expensive
         */
        if (NULL != nstats) {
            nstats->sample_time.tv_sec = stats->sample_time.tv_sec;
            nstats->sample_time.tv_usec = stats->sample_time.tv_usec;
        }
    } else if (NULL != nstats) {
        /* record the time of this sample */
        gettimeofday(&nstats->sample_time, NULL);
    }

    if (NULL != stats) {
        stats->node = strdup(pmix_globals.hostname);

        stats->pid = pid;
        stats->cmd = strdup("UNKNOWN");
        stats->state = 'R';
        stats->priority = 2;
        stats->num_threads = 1;

        /* set the values to something identifiable for testing */
        stats->vsize = 1.75;
        stats->rss = 1.23;
        stats->peak_vsize = 7.89;

        /* convert to time in seconds */
        dtime = 12345.678;
        stats->time.tv_sec = (long) dtime;
        stats->time.tv_usec = (long) (1000000.0 * (dtime - stats->time.tv_sec));
        stats->priority = 2;
    }

    if (NULL != nstats) {
        /* set the memory values to something identifiable for testing */
        nstats->total_mem = 123.45;
        nstats->free_mem = 0.45;
        nstats->buffers = 1.33;
        nstats->cached = 0.56;
        nstats->swap_cached = 0.95;
        nstats->swap_total = 11.45;
        nstats->swap_free = 1.26;
        nstats->mapped = 12.98;
        /* set the load averages */
        nstats->la = 0.52;
        nstats->la5 = 1.03;
        nstats->la15 = 0.12;
    }

    return PMIX_SUCCESS;
}
