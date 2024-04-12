/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "pmix.h"

#include "src/include/pmix_globals.h"
#include "src/mca/preg/preg.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/hwloc/pmix_hwloc.h"

#include "src/mca/bfrops/base/base.h"

#define PMIX_CHECK_SIMPLE(r)        \
do {                                \
    if (r < 0) {                    \
        return PMIX_VALUE2_GREATER; \
    } else if (0 < r) {             \
        return PMIX_VALUE1_GREATER; \
    } else {                        \
        return PMIX_EQUAL;          \
    }                               \
} while(0)

static pmix_value_cmp_t cmp_string(char *s1, char *s2)
{
    int ret;

    if (NULL == s1 && NULL == s2) {
        return PMIX_EQUAL;
    } else if (NULL != s1 && NULL == s2) {
        return PMIX_VALUE1_GREATER;
    } else if (NULL == s1 && NULL != s2) {
        return PMIX_VALUE2_GREATER;
    } else {
        /* both are non-NULL */
        ret = strcmp(s1, s2);
        PMIX_CHECK_SIMPLE(ret);
    }
}

static pmix_value_cmp_t cmp_byte_object(pmix_byte_object_t *bo1,
                                        pmix_byte_object_t *bo2)
{
    int ret;

    if (bo1->size == bo2->size) {
        if (0 == bo1->size) {
            return PMIX_EQUAL;
        }
        ret = memcmp(bo1->bytes, bo2->bytes, bo1->size);
        PMIX_CHECK_SIMPLE(ret);
    } else if (bo1->size > bo2->size) {
        return PMIX_VALUE1_GREATER;
    } else {
        return PMIX_VALUE2_GREATER;
    }
}

static pmix_value_cmp_t cmp_proc_info(pmix_proc_info_t *pi1,
                                      pmix_proc_info_t *pi2)
{
    int ret;

    ret = memcmp(&pi1->proc, &pi2->proc, sizeof(pmix_proc_t));
    if (ret < 0) {
        return PMIX_VALUE2_GREATER;
    } else if (ret > 0) {
        return PMIX_VALUE1_GREATER;
    }
    /* proc fields are the same - check further */
    if (NULL == pi1->hostname && NULL != pi2->hostname) {
        return PMIX_VALUE2_GREATER;
    } else if (NULL != pi1->hostname && NULL == pi2->hostname) {
        return PMIX_VALUE1_GREATER;
    }
    ret = strcmp(pi1->hostname, pi2->hostname);
    if (ret < 0) {
        return PMIX_VALUE2_GREATER;
    } else if (0 < ret) {
        return PMIX_VALUE1_GREATER;
    }
    /* hostnames match */

    if (NULL == pi1->executable_name && NULL != pi2->executable_name) {
        return PMIX_VALUE2_GREATER;
    } else if (NULL != pi1->executable_name && NULL == pi2->executable_name) {
        return PMIX_VALUE1_GREATER;
    }
    ret = strcmp(pi1->executable_name, pi2->executable_name);
    if (ret < 0) {
        return PMIX_VALUE2_GREATER;
    } else if (0 < ret) {
        return PMIX_VALUE1_GREATER;
    }
    /* executables match */

    if (pi1->pid > pi2->pid) {
        return PMIX_VALUE1_GREATER;
    } else if (pi2->pid > pi1->pid) {
        return PMIX_VALUE2_GREATER;
    }
    /* pids match */

    if (pi1->exit_code > pi2->exit_code) {
        return PMIX_VALUE1_GREATER;
    } else if (pi2->exit_code > pi1->exit_code) {
        return PMIX_VALUE2_GREATER;
    }
    return PMIX_EQUAL;
}

static pmix_value_cmp_t cmp_envar(pmix_envar_t *e1,
                                  pmix_envar_t *e2)
{
    int ret;

    if (NULL != e1) {
        if (NULL == e2) {
            return PMIX_VALUE1_GREATER;
        }
        if (NULL == e1->envar && NULL == e2->envar) {
            goto checkvalue;
        } else if (NULL == e1->envar) {
            return PMIX_VALUE2_GREATER;
        } else if (NULL == e2->envar) {
            return PMIX_VALUE1_GREATER;
        }
        /* both must not be NULL */
        ret = strcmp(e1->envar, e2->envar);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else {
        /* if e1 is NULL and e2 is not, then e2 is greater */
        if (NULL != e2) {
            return PMIX_VALUE2_GREATER;
        } else {
            /* if both are NULL, then they are equal */
            return PMIX_EQUAL;
        }
    }

checkvalue:
    /* if both envar strings are NULL or are equal, then check value */
    if (NULL != e1->value) {
        if (NULL == e2->value) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(e1->value, e2->value);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != e2->value) {
        /* we know e1->value had to be NULL */
        return PMIX_VALUE2_GREATER;
    }

    /* finally, check separator */
    if (e1->separator < e2->separator) {
        return PMIX_VALUE2_GREATER;
    }
    if (e1->separator < e2->separator) {
        return PMIX_VALUE1_GREATER;
    }
    return PMIX_EQUAL;
}

static pmix_value_cmp_t cmp_coord(pmix_coord_t *c1,
                                  pmix_coord_t *c2)
{
    int ret;

    if (c1->view != c2->view) {
        return PMIX_VALUE_INCOMPATIBLE_OBJECTS;
    }
    if (0 == c1->dims && 0 != c2->dims) {
        return PMIX_VALUE2_GREATER;
    } else if (0 != c1->dims && 0 == c2->dims) {
        return PMIX_VALUE1_GREATER;
    }
    ret = memcmp(c1->coord, c2->coord, c1->dims * sizeof(uint32_t));
    PMIX_CHECK_SIMPLE(ret);
}

static pmix_value_cmp_t cmp_geometry(pmix_geometry_t *g1,
                                     pmix_geometry_t *g2)
{
    int ret;
    pmix_value_cmp_t rc;
    size_t n;

    if (g1->fabric != g2->fabric) {
        return PMIX_VALUE_INCOMPATIBLE_OBJECTS;
    }

    if (NULL != g1->uuid) {
        if (NULL == g2->uuid) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(g1->uuid, g2->uuid);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != g2->uuid) {
        /* we know g1->uuid had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* uuids match or are both NULL */

    if (NULL != g1->osname) {
        if (NULL == g2->osname) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(g1->osname, g2->osname);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != g2->osname) {
        /* we know g1->osname had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* osnames match or are both NULL */

    if (NULL == g1->coordinates && NULL == g2->coordinates) {
        return PMIX_EQUAL;
    } else if (NULL != g1->coordinates && NULL == g2->coordinates) {
        return PMIX_VALUE1_GREATER;
    } else if (NULL == g1->coordinates && NULL != g2->coordinates) {
        return PMIX_VALUE2_GREATER;
    }
    /* both are not NULL */
    if (g1->ncoords > g2->ncoords) {
        return PMIX_VALUE1_GREATER;
    } else if (g1->ncoords < g2->ncoords) {
        return PMIX_VALUE2_GREATER;
    }
    /* both have same number of coords */
    if (0 == g1->ncoords) {
        return PMIX_EQUAL;
    }
    for (n=0; n < g1->ncoords; n++) {
        rc = cmp_coord(&g1->coordinates[n], &g2->coordinates[n]);
        if (PMIX_EQUAL != rc) {
            return rc;
        }
    }
    return PMIX_EQUAL;
}

static pmix_value_cmp_t cmp_topo(pmix_topology_t *t1,
                                 pmix_topology_t *t2)
{
    int ret;
    char *p1, *p2;

    if (NULL == t1->source && NULL == t2->source) {
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    if (NULL != t1->source && NULL == t2->source) {
        return PMIX_VALUE_INCOMPATIBLE_OBJECTS;
    } else if (NULL == t1->source && NULL != t2->source) {
        return PMIX_VALUE_INCOMPATIBLE_OBJECTS;
    }
    /* both are not NULL */
    ret = strcmp(t1->source, t2->source);
    if (0 != ret) {
        return PMIX_VALUE_INCOMPATIBLE_OBJECTS;
    }
    /* sources match */

    if (NULL == t1->topology && NULL == t2->topology) {
        return PMIX_EQUAL;
    } else if (NULL != t1->topology && NULL == t2->topology) {
        return PMIX_VALUE1_GREATER;
    } else if (NULL == t1->topology && NULL != t2->topology) {
        return PMIX_VALUE2_GREATER;
    }
    /* both are not NULL */

    /* stringify the topologies */
    p1 = pmix_hwloc_print_topology(t1->topology);
    if (NULL == p1) {
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    p2 = pmix_hwloc_print_topology(t2->topology);
    if (NULL == p2) {
        free(p1);
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    ret = strcmp(p1, p2);
    free(p1);
    free(p2);
    PMIX_CHECK_SIMPLE(ret);
}

static pmix_value_cmp_t cmp_cpuset(pmix_cpuset_t *cs1,
                                   pmix_cpuset_t *cs2)
{
    int ret;
    char *p1, *p2;

    if (NULL == cs1->source && NULL == cs2->source) {
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    if (NULL != cs1->source && NULL == cs2->source) {
        return PMIX_VALUE_INCOMPATIBLE_OBJECTS;
    } else if (NULL == cs1->source && NULL != cs2->source) {
        return PMIX_VALUE_INCOMPATIBLE_OBJECTS;
    }
    /* both are not NULL */
    ret = strcmp(cs1->source, cs2->source);
    if (0 != ret) {
        return PMIX_VALUE_INCOMPATIBLE_OBJECTS;
    }
    /* sources match */

    /* stringify the cpusets */
    p1 = pmix_hwloc_print_cpuset(cs1->bitmap);
    if (NULL == p1) {
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    p2 = pmix_hwloc_print_cpuset(cs2->bitmap);
    if (NULL == p2) {
        free(p1);
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    ret = strcmp(p1, p2);
    free(p1);
    free(p2);
    PMIX_CHECK_SIMPLE(ret);
}

static pmix_value_cmp_t cmp_devdist(pmix_device_distance_t *dd1,
                                    pmix_device_distance_t *dd2)
{
    int ret;

    if (dd1->type != dd2->type) {
        return PMIX_VALUE_INCOMPATIBLE_OBJECTS;
    }

    if (NULL != dd1->uuid) {
        if (NULL == dd2->uuid) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(dd1->uuid, dd2->uuid);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != dd2->uuid) {
        /* we know g1->uuid had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* uuids match or are both NULL */

    if (NULL != dd1->osname) {
        if (NULL == dd2->osname) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(dd1->osname, dd2->osname);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != dd2->osname) {
        /* we know g1->osname had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* osnames match or are both NULL */

    if (dd1->mindist > dd2->mindist) {
        return PMIX_VALUE1_GREATER;
    } else if (dd1->mindist < dd2->mindist) {
        return PMIX_VALUE2_GREATER;
    }

    if (dd1->maxdist > dd2->maxdist) {
        return PMIX_VALUE1_GREATER;
    } else if (dd1->maxdist < dd2->maxdist) {
        return PMIX_VALUE2_GREATER;
    }
    return PMIX_EQUAL;
}

static pmix_value_cmp_t cmp_endpoint(pmix_endpoint_t *e1,
                                     pmix_endpoint_t *e2)
{
    pmix_value_cmp_t rc;
    int ret;

    if (NULL != e1->uuid) {
        if (NULL == e2->uuid) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(e1->uuid, e2->uuid);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != e2->uuid) {
        /* we know e1->uuid had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* uuids match or are both NULL */

    if (NULL != e1->osname) {
        if (NULL == e2->osname) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(e1->osname, e2->osname);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != e2->osname) {
        /* we know e1->osname had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* osnames match or are both NULL */

    rc = cmp_byte_object(&e1->endpt, &e2->endpt);
    return rc;
}

static pmix_value_cmp_t cmp_procstats(pmix_proc_stats_t *pcs1,
                                      pmix_proc_stats_t *pcs2)
{
    int ret;

    if (NULL == pcs1->node && NULL == pcs2->node) {
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    if (NULL != pcs1->node) {
        if (NULL == pcs2->node) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(pcs1->node, pcs2->node);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != pcs2->node) {
        /* we know pcs1->node had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL */

    ret = memcmp(&pcs1->proc, &pcs2->proc, sizeof(pmix_proc_t));
    if (ret < 0) {
        return PMIX_VALUE2_GREATER;
    } else if (0 < ret) {
        return PMIX_VALUE1_GREATER;
    }
    if (pcs1->pid > pcs2->pid) {
        return PMIX_VALUE1_GREATER;
    } else if (pcs1->pid < pcs2->pid) {
        return PMIX_VALUE2_GREATER;
    }

    if (NULL != pcs1->cmd) {
        if (NULL == pcs2->cmd) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(pcs1->cmd, pcs2->cmd);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != pcs2->cmd) {
        /* we know pcs1->cmd had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* cmds match or are both NULL */

    if (pcs1->state > pcs2->state) {
        return PMIX_VALUE1_GREATER;
    } else if (pcs1->state < pcs2->state) {
        return PMIX_VALUE2_GREATER;
    }

    ret = memcmp(&pcs1->time, &pcs2->time, sizeof(struct timeval));
    if (ret < 0) {
        return PMIX_VALUE2_GREATER;
    } else if (0 < ret) {
        return PMIX_VALUE1_GREATER;
    }

    if (pcs1->percent_cpu > pcs2->percent_cpu) {
        return PMIX_VALUE1_GREATER;
    } else if (pcs1->percent_cpu < pcs2->percent_cpu) {
        return PMIX_VALUE2_GREATER;
    }
    if (pcs1->priority > pcs2->priority) {
        return PMIX_VALUE1_GREATER;
    } else if (pcs1->priority < pcs2->priority) {
        return PMIX_VALUE2_GREATER;
    }
    if (pcs1->num_threads > pcs2->num_threads) {
        return PMIX_VALUE1_GREATER;
    } else if (pcs1->num_threads < pcs2->num_threads) {
        return PMIX_VALUE2_GREATER;
    }
    if (pcs1->pss > pcs2->pss) {
        return PMIX_VALUE1_GREATER;
    } else if (pcs1->pss < pcs2->pss) {
        return PMIX_VALUE2_GREATER;
    }
    if (pcs1->vsize > pcs2->vsize) {
        return PMIX_VALUE1_GREATER;
    } else if (pcs1->vsize < pcs2->vsize) {
        return PMIX_VALUE2_GREATER;
    }
    if (pcs1->rss > pcs2->rss) {
        return PMIX_VALUE1_GREATER;
    } else if (pcs1->rss < pcs2->rss) {
        return PMIX_VALUE2_GREATER;
    }
    if (pcs1->peak_vsize > pcs2->peak_vsize) {
        return PMIX_VALUE1_GREATER;
    } else if (pcs1->peak_vsize < pcs2->peak_vsize) {
        return PMIX_VALUE2_GREATER;
    }
    if (pcs1->processor > pcs2->processor) {
        return PMIX_VALUE1_GREATER;
    } else if (pcs1->processor < pcs2->processor) {
        return PMIX_VALUE2_GREATER;
    }

    ret = memcmp(&pcs1->sample_time, &pcs2->sample_time, sizeof(struct timeval));
    if (ret < 0) {
        return PMIX_VALUE2_GREATER;
    } else if (0 < ret) {
        return PMIX_VALUE1_GREATER;
    }
    return PMIX_EQUAL;
}

static pmix_value_cmp_t cmp_diskstats(pmix_disk_stats_t *ds1,
                                      pmix_disk_stats_t *ds2)
{
    int ret;

    if (NULL == ds1->disk && NULL == ds2->disk) {
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    if (NULL != ds1->disk) {
        if (NULL == ds2->disk) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(ds1->disk, ds2->disk);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != ds2->disk) {
        /* we know ds1->disk had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL */

    if (ds1->num_reads_completed > ds2->num_reads_completed) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->num_reads_completed < ds2->num_reads_completed) {
        return PMIX_VALUE2_GREATER;
    }
    if (ds1->num_reads_merged > ds2->num_reads_merged) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->num_reads_merged < ds2->num_reads_merged) {
        return PMIX_VALUE2_GREATER;
    }
    if (ds1->num_sectors_read > ds2->num_sectors_read) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->num_sectors_read < ds2->num_sectors_read) {
        return PMIX_VALUE2_GREATER;
    }
    if (ds1->milliseconds_reading > ds2->milliseconds_reading) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->milliseconds_reading < ds2->milliseconds_reading) {
        return PMIX_VALUE2_GREATER;
    }
    if (ds1->num_writes_completed > ds2->num_writes_completed) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->num_writes_completed < ds2->num_writes_completed) {
        return PMIX_VALUE2_GREATER;
    }
    if (ds1->num_writes_merged > ds2->num_writes_merged) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->num_writes_merged < ds2->num_writes_merged) {
        return PMIX_VALUE2_GREATER;
    }
    if (ds1->num_sectors_written > ds2->num_sectors_written) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->num_sectors_written < ds2->num_sectors_written) {
        return PMIX_VALUE2_GREATER;
    }
    if (ds1->milliseconds_writing > ds2->milliseconds_writing) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->milliseconds_writing < ds2->milliseconds_writing) {
        return PMIX_VALUE2_GREATER;
    }
    if (ds1->num_ios_in_progress > ds2->num_ios_in_progress) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->num_ios_in_progress < ds2->num_ios_in_progress) {
        return PMIX_VALUE2_GREATER;
    }
    if (ds1->milliseconds_io > ds2->milliseconds_io) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->milliseconds_io < ds2->milliseconds_io) {
        return PMIX_VALUE2_GREATER;
    }
    if (ds1->weighted_milliseconds_io > ds2->weighted_milliseconds_io) {
        return PMIX_VALUE1_GREATER;
    } else if (ds1->weighted_milliseconds_io < ds2->weighted_milliseconds_io) {
        return PMIX_VALUE2_GREATER;
    }

    return PMIX_EQUAL;
}

static pmix_value_cmp_t cmp_netstats(pmix_net_stats_t *ns1,
                                     pmix_net_stats_t *ns2)
{
    int ret;

    if (NULL == ns1->net_interface && NULL == ns2->net_interface) {
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    if (NULL != ns1->net_interface) {
        if (NULL == ns2->net_interface) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(ns1->net_interface, ns2->net_interface);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != ns2->net_interface) {
        /* we know ns1->disk had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL */

    if (ns1->num_bytes_recvd > ns2->num_bytes_recvd) {
        return PMIX_VALUE1_GREATER;
    } else if (ns1->num_bytes_recvd < ns2->num_bytes_recvd) {
        return PMIX_VALUE2_GREATER;
    }
    if (ns1->num_packets_recvd > ns2->num_packets_recvd) {
        return PMIX_VALUE1_GREATER;
    } else if (ns1->num_packets_recvd < ns2->num_packets_recvd) {
        return PMIX_VALUE2_GREATER;
    }
    if (ns1->num_recv_errs > ns2->num_recv_errs) {
        return PMIX_VALUE1_GREATER;
    } else if (ns1->num_recv_errs < ns2->num_recv_errs) {
        return PMIX_VALUE2_GREATER;
    }
    if (ns1->num_bytes_sent > ns2->num_bytes_sent) {
        return PMIX_VALUE1_GREATER;
    } else if (ns1->num_bytes_sent < ns2->num_bytes_sent) {
        return PMIX_VALUE2_GREATER;
    }
    if (ns1->num_packets_sent > ns2->num_packets_sent) {
        return PMIX_VALUE1_GREATER;
    } else if (ns1->num_packets_sent < ns2->num_packets_sent) {
        return PMIX_VALUE2_GREATER;
    }
    if (ns1->num_send_errs > ns2->num_send_errs) {
        return PMIX_VALUE1_GREATER;
    } else if (ns1->num_send_errs < ns2->num_send_errs) {
        return PMIX_VALUE2_GREATER;
    }

    return PMIX_EQUAL;
}

static pmix_value_cmp_t cmp_nodestats(pmix_node_stats_t *nds1,
                                      pmix_node_stats_t *nds2)
{
    int ret;
    pmix_value_cmp_t rc;
    size_t n;

    if (NULL == nds1->node && NULL == nds2->node) {
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    if (NULL != nds1->node) {
        if (NULL == nds2->node) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(nds1->node, nds2->node);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != nds2->node) {
        /* we know nds1->node had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL */

    if (nds1->la > nds2->la) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->la < nds2->la) {
        return PMIX_VALUE2_GREATER;
    }
    if (nds1->la5 > nds2->la5) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->la5 < nds2->la5) {
        return PMIX_VALUE2_GREATER;
    }
    if (nds1->la15 > nds2->la15) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->la15 < nds2->la15) {
        return PMIX_VALUE2_GREATER;
    }
    if (nds1->total_mem > nds2->total_mem) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->total_mem < nds2->total_mem) {
        return PMIX_VALUE2_GREATER;
    }
    if (nds1->free_mem > nds2->free_mem) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->free_mem < nds2->free_mem) {
        return PMIX_VALUE2_GREATER;
    }
    if (nds1->buffers > nds2->buffers) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->buffers < nds2->buffers) {
        return PMIX_VALUE2_GREATER;
    }
    if (nds1->cached > nds2->cached) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->cached < nds2->cached) {
        return PMIX_VALUE2_GREATER;
    }
    if (nds1->swap_cached > nds2->swap_cached) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->swap_cached < nds2->swap_cached) {
        return PMIX_VALUE2_GREATER;
    }
    if (nds1->swap_total > nds2->swap_total) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->swap_total < nds2->swap_total) {
        return PMIX_VALUE2_GREATER;
    }
    if (nds1->swap_free > nds2->swap_free) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->swap_free < nds2->swap_free) {
        return PMIX_VALUE2_GREATER;
    }
    if (nds1->mapped > nds2->mapped) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->mapped < nds2->mapped) {
        return PMIX_VALUE2_GREATER;
    }

    ret = memcmp(&nds1->sample_time, &nds2->sample_time, sizeof(struct timeval));
    if (ret < 0) {
        return PMIX_VALUE2_GREATER;
    } else if (0 < ret) {
        return PMIX_VALUE1_GREATER;
    }

    if (NULL != nds1->diskstats && NULL == nds2->diskstats) {
        return PMIX_VALUE1_GREATER;
    } else if (NULL == nds1->diskstats && NULL == nds2->diskstats) {
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL */
    if (nds1->ndiskstats > nds2->ndiskstats) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->ndiskstats < nds2->ndiskstats) {
        return PMIX_VALUE2_GREATER;
    }
    for (n=0; n < nds1->ndiskstats; n++) {
        rc = cmp_diskstats(&nds1->diskstats[n], &nds2->diskstats[n]);
        if (PMIX_EQUAL != rc) {
            return rc;
        }
    }
    if (NULL != nds1->netstats && NULL == nds2->netstats) {
        return PMIX_VALUE1_GREATER;
    } else if (NULL == nds1->netstats && NULL == nds2->netstats) {
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL */
    if (nds1->nnetstats > nds2->nnetstats) {
        return PMIX_VALUE1_GREATER;
    } else if (nds1->nnetstats < nds2->nnetstats) {
        return PMIX_VALUE2_GREATER;
    }
    for (n=0; n < nds1->nnetstats; n++) {
        rc = cmp_netstats(&nds1->netstats[n], &nds2->netstats[n]);
        if (PMIX_EQUAL != rc) {
            return rc;
        }
    }

    return PMIX_EQUAL;
}

static pmix_value_cmp_t cmp_regattr(pmix_regattr_t *r1,
                                    pmix_regattr_t *r2)
{
    int ret, c1, c2, n;

    if (NULL == r1->name && NULL == r2->name) {
        return PMIX_VALUE_COMPARISON_NOT_AVAIL;
    }
    if (NULL != r1->name) {
        if (NULL == r2->name) {
            return PMIX_VALUE1_GREATER;
        }
        ret = strcmp(r1->name, r2->name);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (0 < ret) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != r2->name) {
        /* we know r1->name had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL and equal */

    ret = strcmp(r1->string, r2->string);
    if (ret < 0) {
        return PMIX_VALUE2_GREATER;
    } else if (0 < ret) {
        return PMIX_VALUE1_GREATER;
    }

    if (r1->type > r2->type) {
        return PMIX_VALUE1_GREATER;
    } else if (r2->type > r1->type) {
        return PMIX_VALUE2_GREATER;
    }

    if (NULL == r1->description && NULL == r2->description) {
        return PMIX_EQUAL;
    } else if (NULL != r1->description && NULL == r2->description) {
        return PMIX_VALUE1_GREATER;
    } else if (NULL == r1->description && NULL != r2->description) {
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL */
    c1 = PMIx_Argv_count(r1->description);
    c2 = PMIx_Argv_count(r2->description);
    if (c1 > c2) {
        return PMIX_VALUE1_GREATER;
    } else if (c2 > c1) {
        return PMIX_VALUE2_GREATER;
    }
    /* same number of lines */
    for (n=0; n < c1; n++) {
        ret = strcmp(r1->description[n], r2->description[n]);
        if (ret < 0) {
            return PMIX_VALUE2_GREATER;
        } else if (ret > 0) {
            return PMIX_VALUE1_GREATER;
        }
    }
    return PMIX_EQUAL;
}

static pmix_value_cmp_t cmp_info(pmix_info_t *i1,
                                 pmix_info_t *i2)
{
    int ret;
    pmix_value_cmp_t rc;

    ret = strcmp(i1->key, i2->key);
    if (ret < 0) {
        return PMIX_VALUE2_GREATER;
    } else if (ret > 0) {
        return PMIX_VALUE1_GREATER;
    }
    rc = pmix_bfrops_base_value_cmp(&i1->value, &i2->value);
    return rc;
}

static pmix_value_cmp_t cmp_dbuf(pmix_data_buffer_t *db1,
                                 pmix_data_buffer_t *db2)
{
    int ret;

    if (NULL == db1->base_ptr && NULL == db2->base_ptr) {
        return PMIX_EQUAL;
    }
    if (NULL != db1->base_ptr) {
        if (NULL == db2->base_ptr) {
            return PMIX_VALUE1_GREATER;
        }
    } else if (NULL != db2->base_ptr) {
        /* we know db1->base_ptr had to be NULL */
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL */
    if (db1->bytes_used > db2->bytes_used) {
        return PMIX_VALUE1_GREATER;
    } else if (db2->bytes_used > db1->bytes_used) {
        return PMIX_VALUE2_GREATER;
    }
    /* same number of bytes used */
    ret = memcmp(db1->base_ptr, db2->base_ptr, db1->bytes_used);
    if (ret < 0) {
        return PMIX_VALUE2_GREATER;
    } else if (0 < ret) {
        return PMIX_VALUE1_GREATER;
    }
    return PMIX_EQUAL;
}

static pmix_value_cmp_t cmp_darray(pmix_data_array_t *d1,
                                   pmix_data_array_t *d2)
{
    size_t n;
    int ret;
    pmix_value_cmp_t rc;
    char *st1, *st2;
    pmix_byte_object_t *bo1, *bo2;
    pmix_proc_info_t *pi1, *pi2;
    pmix_coord_t *c1, *c2;
    pmix_geometry_t *g1, *g2;
    pmix_envar_t *e1, *e2;
    pmix_topology_t *t1, *t2;
    pmix_cpuset_t *cs1, *cs2;
    pmix_device_distance_t *dd1, *dd2;
    pmix_endpoint_t *end1, *end2;
    pmix_proc_stats_t *pcs1, *pcs2;
    pmix_disk_stats_t *ds1, *ds2;
    pmix_net_stats_t *ns1, *ns2;
    pmix_node_stats_t *nds1, *nds2;
    pmix_regattr_t *ra1, *ra2;
    pmix_info_t *i1, *i2;
    pmix_data_buffer_t *db1, *db2;

    if (NULL == d1 && NULL == d2) {
        return PMIX_EQUAL;
    } else if (NULL != d1 && NULL == d2) {
        return PMIX_VALUE1_GREATER;
    } else if (NULL == d1) {
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL */

    if (d1->type != d2->type) {
        return PMIX_VALUE_TYPE_DIFFERENT;
    }

    if (NULL == d1->array && NULL == d2->array) {
        return PMIX_EQUAL;
    } else if (NULL != d1->array && NULL == d2->array) {
        return PMIX_VALUE1_GREATER;
    } else if (NULL == d1->array) {
        return PMIX_VALUE2_GREATER;
    }
    /* both are non-NULL */

    if (d1->size > d2->size) {
        return PMIX_VALUE1_GREATER;
    } else if (d1->size < d2->size) {
        return PMIX_VALUE2_GREATER;
    }
    /* they are the same size */
    if (0 == d1->size) {
        return PMIX_EQUAL;
    }
    /* the size is greater than zero */

    switch (d1->type) {
        case PMIX_UNDEF:
            return PMIX_EQUAL;
            break;
        case PMIX_BOOL:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(bool));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_BYTE:
            ret = memcmp(d1->array, d2->array, d1->size);
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_STRING:
            st1 = (char*)d1->array;
            st2 = (char*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_string(&st1[n], &st2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_SIZE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(size_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_PID:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pid_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_INT:
        case PMIX_UINT:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(int));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_INT8:
        case PMIX_UINT8:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(int8_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_INT16:
        case PMIX_UINT16:
        case PMIX_STOR_ACCESS_TYPE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(int16_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_INT32:
        case PMIX_UINT32:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(int32_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_INT64:
        case PMIX_UINT64:
        case PMIX_STOR_MEDIUM:
        case PMIX_STOR_ACCESS:
        case PMIX_STOR_PERSIST:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(int64_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_FLOAT:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(float));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_DOUBLE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(double));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_TIMEVAL:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(struct timeval));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_TIME:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(time_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_STATUS:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_status_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_PROC_RANK:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_rank_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_PROC_NSPACE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_nspace_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_PROC:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_proc_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_INFO:
            i1 = (pmix_info_t*)d1->array;
            i2 = (pmix_info_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_info(&i1[n], &i2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_BYTE_OBJECT:
        case PMIX_COMPRESSED_STRING:
        case PMIX_COMPRESSED_BYTE_OBJECT:
        case PMIX_REGEX:
            bo1 = (pmix_byte_object_t*)d1->array;
            bo2 = (pmix_byte_object_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_byte_object(&bo1[n], &bo2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_PERSIST:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_persistence_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_SCOPE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_scope_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_DATA_RANGE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_data_range_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_PROC_STATE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_proc_state_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_PROC_INFO:
            pi1 = (pmix_proc_info_t*)d1->array;
            pi2 = (pmix_proc_info_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_proc_info(&pi1[n], &pi2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_DATA_ARRAY:
            return cmp_darray(d1->array, d2->array);
            break;
        case PMIX_POINTER:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(void*));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_ALLOC_DIRECTIVE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_alloc_directive_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_ENVAR:
            e1 = (pmix_envar_t*)d1->array;
            e2 = (pmix_envar_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_envar(&e1[n], &e2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_COORD:
            c1 = (pmix_coord_t*)d1->array;
            c2 = (pmix_coord_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_coord(&c1[n], &c2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_LINK_STATE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_link_state_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_JOB_STATE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_job_state_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_TOPO:
            t1 = (pmix_topology_t *)d1->array;
            t2 = (pmix_topology_t *)d1->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_topo(&t1[n], &t2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_PROC_CPUSET:
            cs1 = (pmix_cpuset_t *)d1->array;
            cs2 = (pmix_cpuset_t *)d1->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_cpuset(&cs1[n], &cs2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_LOCTYPE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_locality_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_GEOMETRY:
            g1 = (pmix_geometry_t*)d1->array;
            g2 = (pmix_geometry_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_geometry(&g1[n], &g2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_DEVTYPE:
            ret = memcmp(d1->array, d2->array, d1->size * sizeof(pmix_device_type_t));
            PMIX_CHECK_SIMPLE(ret);
            break;
        case PMIX_DEVICE_DIST:
            dd1 = (pmix_device_distance_t*)d1->array;
            dd2 = (pmix_device_distance_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_devdist(&dd1[n], &dd2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_ENDPOINT:
            end1 = (pmix_endpoint_t*)d1->array;
            end2 = (pmix_endpoint_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_endpoint(&end1[n], &end2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_DATA_BUFFER:
            db1 = (pmix_data_buffer_t*)d1->array;
            db2 = (pmix_data_buffer_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_dbuf(&db1[n], &db2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_PROC_STATS:
            pcs1 = (pmix_proc_stats_t*)d1->array;
            pcs2 = (pmix_proc_stats_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_procstats(&pcs1[n], &pcs2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_DISK_STATS:
            ds1 = (pmix_disk_stats_t*)d1->array;
            ds2 = (pmix_disk_stats_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_diskstats(&ds1[n], &ds2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_NET_STATS:
            ns1 = (pmix_net_stats_t*)d1->array;
            ns2 = (pmix_net_stats_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_netstats(&ns1[n], &ns2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;
        case PMIX_NODE_STATS:
            nds1 = (pmix_node_stats_t*)d1->array;
            nds2 = (pmix_node_stats_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_nodestats(&nds1[n], &nds2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;

        case PMIX_REGATTR:
            ra1 = (pmix_regattr_t*)d1->array;
            ra2 = (pmix_regattr_t*)d2->array;
            for (n=0; n < d1->size; n++) {
                rc = cmp_regattr(&ra1[n], &ra2[n]);
                if (PMIX_EQUAL != rc) {
                    return rc;
                }
            }
            return PMIX_EQUAL;
            break;

        default:
            pmix_output(0, "COMPARE-PMIX-VALUE: UNSUPPORTED TYPE %s (%d)",
                        PMIx_Data_type_string(d1->type), (int) d1->type);
    }
    return PMIX_VALUE_COMPARISON_NOT_AVAIL;

}

/* compare function for pmix_value_t */
pmix_value_cmp_t pmix_bfrops_base_value_cmp(pmix_value_t *p1,
                                            pmix_value_t *p2)
{
    pmix_value_cmp_t rc;
    int ret;

    if (p1->type != p2->type) {
        return PMIX_VALUE_TYPE_DIFFERENT;
    }

    switch (p1->type) {
    case PMIX_UNDEF:
        return PMIX_EQUAL;
        break;
    case PMIX_BOOL:
        ret = memcmp(&p1->data.flag, &p2->data.flag, sizeof(bool));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_BYTE:
        ret = memcmp(&p1->data.byte, &p2->data.byte, 1);
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_STRING:
        rc = cmp_string(p1->data.string, p2->data.string);
        return rc;
        break;
    case PMIX_SIZE:
        ret = memcmp(&p1->data.size, &p2->data.size, sizeof(size_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_PID:
        ret = memcmp(&p1->data.pid, &p2->data.pid, sizeof(pid_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_INT:
    case PMIX_UINT:
        ret = memcmp(&p1->data.integer, &p2->data.integer, sizeof(int));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_INT8:
    case PMIX_UINT8:
        ret = memcmp(&p1->data.int8, &p2->data.int8, sizeof(int8_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_INT16:
    case PMIX_UINT16:
    case PMIX_STOR_ACCESS_TYPE:
        ret = memcmp(&p1->data.int16, &p2->data.int16, sizeof(int16_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_INT32:
    case PMIX_UINT32:
        ret = memcmp(&p1->data.int32, &p2->data.int32, sizeof(int32_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_INT64:
    case PMIX_UINT64:
    case PMIX_STOR_MEDIUM:
    case PMIX_STOR_ACCESS:
    case PMIX_STOR_PERSIST:
        ret = memcmp(&p1->data.int64, &p2->data.int64, sizeof(int64_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_FLOAT:
        ret = memcmp(&p1->data.fval, &p2->data.fval, sizeof(float));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_DOUBLE:
        ret = memcmp(&p1->data.dval, &p2->data.dval, sizeof(double));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_TIMEVAL:
        ret = memcmp(&p1->data.tv, &p2->data.tv, sizeof(struct timeval));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_TIME:
        ret = memcmp(&p1->data.time, &p2->data.time, sizeof(time_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_STATUS:
        ret = memcmp(&p1->data.status, &p2->data.status, sizeof(pmix_status_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_PROC_RANK:
        ret = memcmp(&p1->data.rank, &p2->data.rank, sizeof(pmix_rank_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_PROC_NSPACE:
        ret = memcmp(p1->data.nspace, p2->data.nspace, sizeof(pmix_nspace_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_PROC:
        ret = memcmp(p1->data.proc, p2->data.proc, sizeof(pmix_proc_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_BYTE_OBJECT:
    case PMIX_COMPRESSED_STRING:
    case PMIX_COMPRESSED_BYTE_OBJECT:
    case PMIX_REGEX:
        rc = cmp_byte_object(&p1->data.bo, &p2->data.bo);
        return rc;
        break;
    case PMIX_PERSIST:
        ret = memcmp(&p1->data.persist, &p2->data.persist, sizeof(pmix_persistence_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_SCOPE:
        ret = memcmp(&p1->data.scope, &p2->data.scope, sizeof(pmix_scope_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_DATA_RANGE:
        ret = memcmp(&p1->data.range, &p2->data.range, sizeof(pmix_data_range_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_PROC_STATE:
        ret = memcmp(&p1->data.state, &p2->data.state, sizeof(pmix_proc_state_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_PROC_INFO:
        rc = cmp_proc_info(p1->data.pinfo, p2->data.pinfo);
        return rc;
        break;
    case PMIX_DATA_ARRAY:
        return cmp_darray(p1->data.darray, p2->data.darray);
        break;
    case PMIX_POINTER:
        ret = memcmp(p1->data.ptr, p2->data.ptr, sizeof(void*));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_ALLOC_DIRECTIVE:
        ret = memcmp(&p1->data.adir, &p2->data.adir, sizeof(pmix_alloc_directive_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_ENVAR:
        rc = cmp_envar(&p1->data.envar, &p2->data.envar);
        return rc;
        break;
    case PMIX_COORD:
        rc = cmp_coord(p1->data.coord, p2->data.coord);
        return rc;
        break;
    case PMIX_LINK_STATE:
        ret = memcmp(&p1->data.linkstate, &p2->data.linkstate, sizeof(pmix_link_state_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_JOB_STATE:
        ret = memcmp(&p1->data.jstate, &p2->data.jstate, sizeof(pmix_job_state_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_TOPO:
        rc = cmp_topo(p1->data.topo, p2->data.topo);
        return rc;
        break;
    case PMIX_PROC_CPUSET:
        rc = cmp_cpuset(p1->data.cpuset, p2->data.cpuset);
        return rc;
        break;
        case PMIX_LOCTYPE:
        ret = memcmp(&p1->data.locality, &p2->data.locality, sizeof(pmix_locality_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_GEOMETRY:
        rc = cmp_geometry(p1->data.geometry, p2->data.geometry);
        return rc;
        break;
        case PMIX_DEVTYPE:
        ret = memcmp(&p1->data.devtype, &p2->data.devtype, sizeof(pmix_device_type_t));
        PMIX_CHECK_SIMPLE(ret);
        break;
    case PMIX_DEVICE_DIST:
        rc = cmp_devdist(p1->data.devdist, p2->data.devdist);
        return rc;
        break;
    case PMIX_ENDPOINT:
        rc = cmp_endpoint(p1->data.endpoint, p2->data.endpoint);
        return rc;
        break;
    case PMIX_DATA_BUFFER:
        rc = cmp_dbuf(p1->data.dbuf, p2->data.dbuf);
        return rc;
        break;
    case PMIX_PROC_STATS:
        rc = cmp_procstats(p1->data.pstats, p2->data.pstats);
        return rc;
        break;
    case PMIX_DISK_STATS:
        rc = cmp_diskstats(p1->data.dkstats, p2->data.dkstats);
        return rc;
        break;
    case PMIX_NET_STATS:
        rc = cmp_netstats(p1->data.netstats, p2->data.netstats);
        return rc;
        break;
    case PMIX_NODE_STATS:
        rc = cmp_nodestats(p1->data.ndstats, p2->data.ndstats);
        return rc;
        break;

    /* account for types that don't use a specific field
     * in the union */
    case PMIX_REGATTR:
        rc = cmp_regattr(p1->data.ptr, p2->data.ptr);
        return rc;
        break;

    default:
        pmix_output(0, "COMPARE-PMIX-VALUE: UNSUPPORTED TYPE %s (%d)",
                    PMIx_Data_type_string(p1->type), (int) p1->type);
    }
    return PMIX_VALUE_COMPARISON_NOT_AVAIL;
}
