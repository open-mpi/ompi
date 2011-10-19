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
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "opal/mca/paffinity/paffinity.h"

#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"

void opal_hwloc_base_get_local_cpuset(void)
{
    hwloc_obj_t root;

    if (NULL != opal_hwloc_topology) {
        if (NULL == opal_hwloc_my_cpuset) {
            opal_hwloc_my_cpuset = hwloc_bitmap_alloc();
        }
        /* get the cpus we are bound to */
        hwloc_get_cpubind(opal_hwloc_topology, opal_hwloc_my_cpuset, HWLOC_CPUBIND_PROCESS);
        /* if the cpuset is empty, then we are not bound */
        if (hwloc_bitmap_iszero(opal_hwloc_my_cpuset)) {
            OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                                 "hwloc:base:get_local_cpuset MY LOCAL CPUSET WAS ZERO - NOT BOUND"));
            /* just insert the cpuset for the root object as we are unbound */
            root = hwloc_get_root_obj(opal_hwloc_topology);
            hwloc_bitmap_copy(opal_hwloc_my_cpuset, root->cpuset);
        }
    }
}

int opal_hwloc_base_report_bind_failure(const char *file,
                                        int line,
                                        const char *msg, int rc)
{
    static int already_reported = 0;

    if (!already_reported) {
        char hostname[64];
        gethostname(hostname, sizeof(hostname));

        opal_show_help("help-opal-hwloc-base.txt", "mbind failure", true,
                       hostname, getpid(), file, line, msg,
                       (OPAL_HWLOC_BASE_MBFA_WARN == opal_hwloc_base_mbfa) ?
                       "Warning -- your job will continue, but possibly with degraded performance" :
                       "ERROR -- your job may abort or behave erraticly");
        already_reported = 1;
        return rc;
    }

    return OPAL_SUCCESS;
}

static void recurse_locality(hwloc_obj_t obj,
                             opal_paffinity_locality_t *locality,
                             hwloc_cpuset_t peer1,
                             hwloc_cpuset_t peer2)
{
    unsigned i;
    hwloc_obj_t obj2;

#if OPAL_ENABLE_DEBUG
    {
        char debug[256], debug1[256], debug2[256];

        hwloc_bitmap_list_snprintf(debug, 256, obj->cpuset);
        hwloc_bitmap_list_snprintf(debug1, 256, peer1);
        hwloc_bitmap_list_snprintf(debug2, 256, peer2);
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                             "Type: %s Recurse: OBJ: %s PEER1: %s PEER2: %s",
                             hwloc_obj_type_string(obj->type), debug, debug1, debug2));
    }
#endif

    /* is peer1 on this object? */
    if (!hwloc_bitmap_intersects(obj->cpuset, peer1)) {
        /* no - move on */
            OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                                 "hwloc:base:recurs_locality Peer1 does not intersect"));
        goto MOVEON;
    }
    /* is peer2 on this object? */
    if (!hwloc_bitmap_intersects(obj->cpuset, peer2)) {
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                                 "hwloc:base:recurs_locality Peer2 does not intersect"));
        /* no - move on */
        goto MOVEON;
    }
    /* we share this object, so we must share locality
     * at this resource type - figure out what that is
     * and flag it
     */
    switch (obj->type) {
    case HWLOC_OBJ_PU:
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output, "hwloc:base:recurs_locality ON PU"));
        *locality |= OPAL_PROC_ON_HWTHREAD;
        break;
    case HWLOC_OBJ_CORE:
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output, "hwloc:base:recurs_locality ON CORE"));
        *locality |= OPAL_PROC_ON_CORE;
        break;
    case HWLOC_OBJ_CACHE:
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output, "hwloc:base:recurs_locality ON CACHE"));
        /* hwloc treats caches as a special case, so we have
         * to figure out which level of cache we have here
         */
        if (1 == obj->attr->cache.depth) {
            *locality |= OPAL_PROC_ON_L1CACHE;
        } else if (2 == obj->attr->cache.depth) {
            *locality |= OPAL_PROC_ON_L2CACHE;
        } else if (3 == obj->attr->cache.depth) {
            *locality |= OPAL_PROC_ON_L3CACHE;
        }
        break;
    case HWLOC_OBJ_SOCKET:
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output, "hwloc:base:recurs_locality ON SOCKET"));
        *locality |= OPAL_PROC_ON_SOCKET;
        break;
    case HWLOC_OBJ_NODE:
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output, "hwloc:base:recurs_locality ON NUMA"));
        *locality |= OPAL_PROC_ON_NUMA;
        break;
    default:
        /* ignore */
        break;
    }

 MOVEON:
    /* step to next object - it would be nice if we could
     * drop down the depth chart and avoid possible duplication
     * of checks. However, hwloc treats all caches as being at
     * a common depth, which makes this impossible. Fortunately,
     * the cpuset intersect test is lightweight, and we are not
     * used in performance-critical paths, so the penalty is small
     */
    for (i=0; i < obj->arity; i++) {
        obj2 = obj->children[i];
        /* process the child object */
        recurse_locality(obj2, locality, peer1, peer2);
    }
}

opal_paffinity_locality_t opal_hwloc_base_get_relative_locality(hwloc_topology_t topo,
                                                                hwloc_cpuset_t peer1,
                                                                hwloc_cpuset_t peer2)
{
    opal_paffinity_locality_t locality;
    hwloc_obj_t root;

    /* start with what we know - they share a node on a cluster
     * NOTE: we may alter that latter part as hwloc's ability to
     * sense multi-cu, multi-cluster systems grows
     */
    locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE | OPAL_PROC_ON_BOARD;

    /* start at the root object */
    root = hwloc_get_root_obj(topo);

    /* descend down the topology */
    recurse_locality(root, &locality, peer1, peer2);

    /* NOTE: hwloc isn't able to find cores on all platforms.  Example:
       PPC64 running RHEL 5.4 (linux kernel 2.6.18) only reports NUMA
       nodes and PU's.  Fine.  

       However, note that hwloc_get_obj_by_type() will return NULL in
       2 (effectively) different cases:

       - no objects of the requested type were found
       - the Nth object of the requested type was not found

       So see if we can find *any* cores by looking for the 0th core.
    */
    if (NULL == hwloc_get_obj_by_type(topo, HWLOC_OBJ_CORE, 0)) {
        /* nope - so if the two peer's share a HWTHREAD, also
         * declare them as sharing a core
         */
        if (OPAL_PROC_ON_LOCAL_HWTHREAD(locality)) {
            locality |= OPAL_PROC_ON_CORE;
        }
    }

    return locality;
}
