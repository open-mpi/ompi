/*
 * Copyright (c) 2011-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2016      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/show_help.h"

#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/hwloc/hwloc-internal.h"

/**
 * Global reflecting the BFA (set by MCA param).
 */
extern opal_hwloc_base_mbfa_t opal_hwloc_base_mbfa;

/**
 * Report a bind failure using the normal mechanisms if a component
 * fails to bind memory -- according to the value of the
 * hwloc_base_bind_failure_action MCA parameter.
 */
static int opal_hwloc_base_report_bind_failure(const char *file, int line, const char *msg, int rc)
{
    static int already_reported = 0;

    if (!already_reported && OPAL_HWLOC_BASE_MBFA_SILENT != opal_hwloc_base_mbfa) {
        char hostname[OPAL_MAXHOSTNAMELEN];
        gethostname(hostname, sizeof(hostname));

        opal_show_help(
            "help-opal-hwloc-base.txt", "mbind failure", true, hostname, getpid(), file, line, msg,
            (OPAL_HWLOC_BASE_MBFA_WARN == opal_hwloc_base_mbfa)
                ? "Warning -- your job will continue, but possibly with degraded performance"
                : "ERROR -- your job may abort or behave erraticly");
        already_reported = 1;
        return rc;
    }

    return OPAL_SUCCESS;
}

int opal_hwloc_base_memory_set(opal_hwloc_base_memory_segment_t *segments, size_t num_segments)
{
    int rc = OPAL_SUCCESS;
    char *msg = NULL;
    size_t i;
    hwloc_cpuset_t cpuset = NULL;

    /* bozo check */
    if (OPAL_SUCCESS != opal_hwloc_base_get_topology()) {
        msg = "hwloc_set_area_membind() failure - topology not available";
        return opal_hwloc_base_report_bind_failure(__FILE__, __LINE__, msg, rc);
    }

    /* This module won't be used unless the process is already
       processor-bound.  So find out where we're processor bound, and
       bind our memory there, too. */
    cpuset = hwloc_bitmap_alloc();
    if (NULL == cpuset) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        msg = "hwloc_bitmap_alloc() failure";
        goto out;
    }
    hwloc_get_cpubind(opal_hwloc_topology, cpuset, 0);
    for (i = 0; i < num_segments; ++i) {
        if (0
            != hwloc_set_area_membind(opal_hwloc_topology, segments[i].mbs_start_addr,
                                      segments[i].mbs_len, cpuset, HWLOC_MEMBIND_BIND,
                                      HWLOC_MEMBIND_STRICT)) {
            rc = OPAL_ERROR;
            msg = "hwloc_set_area_membind() failure";
            goto out;
        }
    }

out:
    if (NULL != cpuset) {
        hwloc_bitmap_free(cpuset);
    }
    if (OPAL_SUCCESS != rc) {
        return opal_hwloc_base_report_bind_failure(__FILE__, __LINE__, msg, rc);
    }
    return OPAL_SUCCESS;
}

int opal_hwloc_base_membind(opal_hwloc_base_memory_segment_t *segs, size_t count, int node_id)
{
    size_t i;
    int rc = OPAL_SUCCESS;
    char *msg = NULL;
    hwloc_cpuset_t cpuset = NULL;

    /* bozo check */
    if (OPAL_SUCCESS != opal_hwloc_base_get_topology()) {
        msg = "hwloc_set_area_membind() failure - topology not available";
        return opal_hwloc_base_report_bind_failure(__FILE__, __LINE__, msg, rc);
    }

    cpuset = hwloc_bitmap_alloc();
    if (NULL == cpuset) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        msg = "hwloc_bitmap_alloc() failure";
        goto out;
    }
    hwloc_bitmap_set(cpuset, node_id);
    for (i = 0; i < count; i++) {
        if (0
            != hwloc_set_area_membind(opal_hwloc_topology, segs[i].mbs_start_addr, segs[i].mbs_len,
                                      cpuset, HWLOC_MEMBIND_BIND, HWLOC_MEMBIND_STRICT)) {
            rc = OPAL_ERROR;
            msg = "hwloc_set_area_membind() failure";
            goto out;
        }
    }

out:
    if (NULL != cpuset) {
        hwloc_bitmap_free(cpuset);
    }
    if (OPAL_SUCCESS != rc) {
        return opal_hwloc_base_report_bind_failure(__FILE__, __LINE__, msg, rc);
    }
    return OPAL_SUCCESS;
}
