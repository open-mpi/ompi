/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2011-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2012-2017 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (C) 2018      Mellanox Technologies, Ltd.
 *                         All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2019-2021 IBM Corporation. All rights reserved.
 * Copyright (c) 2019-2020 Inria.  All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OPAL_HWLOC_WANT_SHMEM 1

#include "opal_config.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_ENDIAN_H
#    include <endian.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#if HAVE_FCNTL_H
#    include <fcntl.h>
#endif

#include "opal/constants.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/mca/threads/tsd.h"
#include "opal/runtime/opal.h"
#include "opal/util/argv.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/util/show_help.h"

#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/hwloc/hwloc-internal.h"

bool opal_hwloc_topo_in_shmem = false;

static void fill_cache_line_size(void)
{
    int i = 0, cache_level = 2;
    unsigned size;
    unsigned int cache_object = HWLOC_OBJ_L2CACHE;
    hwloc_obj_t obj;
    bool found = false;

    /* Look for the smallest L2 cache size */
    size = 4096;
    while (cache_level > 0 && !found) {
        i = 0;
        while (1) {
            obj = opal_hwloc_base_get_obj_by_type(opal_hwloc_topology, cache_object, cache_level, i,
                                                  OPAL_HWLOC_LOGICAL);
            if (NULL == obj) {
                --cache_level;
                cache_object = HWLOC_OBJ_L1CACHE;
                break;
            } else {
                if (NULL != obj->attr && obj->attr->cache.linesize > 0
                    && size > obj->attr->cache.linesize) {
                    size = obj->attr->cache.linesize;
                    found = true;
                }
            }
            ++i;
        }
    }

    /* If we found an L2 cache size in the hwloc data, save it in
       opal_cache_line_size.  Otherwise, we'll leave whatever default
       was set in opal_init.c */
    if (found) {
        opal_cache_line_size = (int) size;
    }
}

static int opal_hwloc_base_topology_set_flags(hwloc_topology_t topology, unsigned long flags, bool io)
{
    if (io) {
#if HWLOC_API_VERSION < 0x20000
        flags |= HWLOC_TOPOLOGY_FLAG_IO_DEVICES;
#else
        int ret = hwloc_topology_set_io_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_IMPORTANT);
        if (0 != ret) {
            return ret;
        }
#endif
    }
    return hwloc_topology_set_flags(topology, flags);
}

/* determine the node-level available cpuset based on
 * online vs allowed vs user-specified cpus.
 *
 * Only used in 'self discovery' case, when the topology
 * could not be retrieved from the RM.
 */
static int opal_hwloc_base_filter_cpus(hwloc_topology_t topo)
{
    hwloc_obj_t root;
    hwloc_cpuset_t avail = NULL;
    opal_hwloc_topo_data_t *sum;

    root = hwloc_get_root_obj(topo);

    if (NULL == root->userdata) {
        root->userdata = (void *) OBJ_NEW(opal_hwloc_topo_data_t);
    }
    sum = (opal_hwloc_topo_data_t *) root->userdata;

    /* should only ever enter here once, but check anyway */
    if (NULL != sum->available) {
        return OPAL_SUCCESS;
    }

/* get the root available cpuset */
#if HWLOC_API_VERSION < 0x20000
    if (NULL == root->online_cpuset || NULL == root->allowed_cpuset) {
        if (NULL == root->cpuset) {
            /* we have a really bad topology */
            return OPAL_ERR_NOT_SUPPORTED;
        }
        avail = hwloc_bitmap_dup(root->cpuset);
    } else {
        avail = hwloc_bitmap_alloc();
        hwloc_bitmap_and(avail, root->online_cpuset, root->allowed_cpuset);
    }
#else
    avail = hwloc_bitmap_dup(root->cpuset);
#endif
    /* cache this info */
    sum->available = avail;

    return OPAL_SUCCESS;
}

/**
 * Initializes opal_hwloc_my_cpuset (global variable in
 * opal/mca/hwloc/hwloc-internal.h) for this process.  opal_hwloc_my_cpuset
 * will be loaded with this process' binding, or, if the process is
 * not bound, use the hwloc root object's (available and online)
 * cpuset.
 */
static void opal_hwloc_base_set_local_cpuset(void)
{
    hwloc_obj_t root;

    if (NULL != opal_hwloc_topology) {
        if (NULL == opal_hwloc_my_cpuset) {
            opal_hwloc_my_cpuset = hwloc_bitmap_alloc();
        }

        /* get the cpus we are bound to */
        if (hwloc_get_cpubind(opal_hwloc_topology, opal_hwloc_my_cpuset, HWLOC_CPUBIND_PROCESS)
            < 0) {
            /* we are not bound - use the root's available cpuset */
            root = hwloc_get_root_obj(opal_hwloc_topology);
            hwloc_bitmap_copy(opal_hwloc_my_cpuset, root->cpuset);
        }
    }
}

int opal_hwloc_base_get_topology(void)
{
    int rc;
    opal_process_name_t wildcard_rank;
    char *val = NULL;
#if HWLOC_API_VERSION >= 0x20000
    int rc2, rc3, fd;
    uint64_t addr, *aptr, size, *sptr;
    char *shmemfile;
#endif

    opal_output_verbose(2, opal_hwloc_base_framework.framework_output, "hwloc:base:get_topology");

    /* see if we already have it */
    if (NULL != opal_hwloc_topology) {
        return OPAL_SUCCESS;
    }
    wildcard_rank.jobid = OPAL_PROC_MY_NAME.jobid;
    wildcard_rank.vpid = OPAL_VPID_WILDCARD;

#if HWLOC_API_VERSION >= 0x20000
    opal_output_verbose(2, opal_hwloc_base_framework.framework_output,
                        "hwloc:base: looking for topology in shared memory");

    /* first try to get the shmem link, if available */
    aptr = &addr;
    sptr = &size;
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_HWLOC_SHMEM_FILE, &wildcard_rank, (void **) &shmemfile,
                                   PMIX_STRING);
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc2, PMIX_HWLOC_SHMEM_ADDR, &wildcard_rank, (void **) &aptr,
                                   PMIX_SIZE);
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc3, PMIX_HWLOC_SHMEM_SIZE, &wildcard_rank, (void **) &sptr,
                                   PMIX_SIZE);
    if (OPAL_SUCCESS == rc && OPAL_SUCCESS == rc2 && OPAL_SUCCESS == rc3) {
        if (0 > (fd = open(shmemfile, O_RDONLY))) {
            free(shmemfile);
            OPAL_ERROR_LOG(OPAL_ERR_FILE_OPEN_FAILURE)
            return OPAL_ERR_FILE_OPEN_FAILURE;
        }
        free(shmemfile);
        if (0 != hwloc_shmem_topology_adopt(&opal_hwloc_topology, fd, 0, (void *) addr, size, 0)) {
            if (4 < opal_output_get_verbosity(opal_hwloc_base_framework.framework_output)) {
                FILE *file = fopen("/proc/self/maps", "r");
                if (file) {
                    char line[256];
                    opal_output(0, "Dumping /proc/self/maps");

                    while (fgets(line, sizeof(line), file) != NULL) {
                        char *end = strchr(line, '\n');
                        if (end) {
                            *end = '\0';
                        }
                        opal_output(0, "%s", line);
                    }
                    fclose(file);
                }
            }
            /* failed to adopt from shmem, fallback to other ways to get the topology */
        } else {
            opal_output_verbose(2, opal_hwloc_base_framework.framework_output,
                                "hwloc:base: topology in shared memory");
            opal_hwloc_topo_in_shmem = true;
            goto done;
        }
    }
#endif
    /* if that isn't available, then try to retrieve
     * the xml representation from the PMIx data store */
    opal_output_verbose(1, opal_hwloc_base_framework.framework_output,
                        "hwloc:base[%s:%d] getting topology XML string", __FILE__, __LINE__);
#if HWLOC_API_VERSION >= 0x20000
    OPAL_MODEX_RECV_VALUE_IMMEDIATE(rc, PMIX_HWLOC_XML_V2, &wildcard_rank, &val, PMIX_STRING);
#else
    OPAL_MODEX_RECV_VALUE_IMMEDIATE(rc, PMIX_HWLOC_XML_V1, &wildcard_rank, &val, PMIX_STRING);
#endif
    if (rc != OPAL_SUCCESS) {
        /* check the old topo key to keep compatibility with older RMs */
        OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_LOCAL_TOPO, &wildcard_rank, &val, PMIX_STRING);
    }

    if (OPAL_SUCCESS == rc && NULL != val) {
        opal_output_verbose(1, opal_hwloc_base_framework.framework_output,
                            "hwloc:base loading topology from XML");
        /* load the topology */
        if (0 != hwloc_topology_init(&opal_hwloc_topology)) {
            free(val);
            return OPAL_ERROR;
        }
        if (0 != hwloc_topology_set_xmlbuffer(opal_hwloc_topology, val, strlen(val) + 1)) {
            /* default to discovery */
            free(val);
            hwloc_topology_destroy(opal_hwloc_topology);
            goto discover;
        }
        /* since we are loading this from an external source, we have to
         * explicitly set a flag so hwloc sets things up correctly
         */
        if (0
            != opal_hwloc_base_topology_set_flags(opal_hwloc_topology,
                                                  HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM, true)) {
            /* default to discovery */
            hwloc_topology_destroy(opal_hwloc_topology);
            free(val);
            goto discover;
        }
        /* now load the topology */
        if (0 != hwloc_topology_load(opal_hwloc_topology)) {
            /* default to discovery */
            hwloc_topology_destroy(opal_hwloc_topology);
            free(val);
            goto discover;
        }
        free(val);
    } else {
    discover:
        opal_output_verbose(1, opal_hwloc_base_framework.framework_output,
                            "hwloc:base discovering topology");
        if (0 != hwloc_topology_init(&opal_hwloc_topology)
            || 0 != opal_hwloc_base_topology_set_flags(opal_hwloc_topology, 0, true)
            || 0 != hwloc_topology_load(opal_hwloc_topology)) {
            OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
            return OPAL_ERR_NOT_SUPPORTED;
        }
        /* filter the cpus thru any default cpu set */
        if (OPAL_SUCCESS != (rc = opal_hwloc_base_filter_cpus(opal_hwloc_topology))) {
            hwloc_topology_destroy(opal_hwloc_topology);
            return rc;
        }
    }

done:

    /* fill opal_cache_line_size global with the smallest L1 cache
       line size */
    fill_cache_line_size();

    /* Set or update our local cpuset - it could get used multiple
     * times, so it's more efficient to keep a global copy.
     */
    opal_hwloc_base_set_local_cpuset();

    return OPAL_SUCCESS;
}

/* hwloc treats cache objects as special
 * cases. Instead of having a unique type for each cache level,
 * there is a single cache object type, and the level is encoded
 * in an attribute union. So looking for cache objects involves
 * a multi-step test :-(
 */
static hwloc_obj_t df_search(hwloc_topology_t topo, hwloc_obj_t start, hwloc_obj_type_t target,
                             unsigned cache_level, unsigned int nobj,
                             opal_hwloc_resource_type_t rtype, unsigned int *num_objs)
{
    hwloc_obj_t obj;
    int search_depth;

    search_depth = hwloc_get_type_depth(topo, target);
    if (HWLOC_TYPE_DEPTH_MULTIPLE == search_depth) {
        /* either v1.x Cache, or Groups */
#if HWLOC_API_VERSION >= 0x20000
        return NULL;
#else
        if (cache_level != HWLOC_OBJ_CACHE)
            return NULL;
        search_depth = hwloc_get_cache_type_depth(topo, cache_level, (hwloc_obj_cache_type_t) -1);
#endif
    }
    if (HWLOC_TYPE_DEPTH_UNKNOWN == search_depth) {
        return NULL;
    }

    if (OPAL_HWLOC_LOGICAL == rtype) {
        if (num_objs) {
            *num_objs = hwloc_get_nbobjs_by_depth(topo, search_depth);
        }
        return hwloc_get_obj_by_depth(topo, search_depth, nobj);
    }
    if (OPAL_HWLOC_PHYSICAL == rtype) {
        /* the PHYSICAL object number is stored as the os_index. When
         * counting physical objects, we can't just count the number
         * that are in the hwloc tree as the only entries in the tree
         * are LOGICAL objects - i.e., any physical gaps won't show. So
         * we instead return the MAX os_index, as this is the best we
         * can do to tell you how many PHYSICAL objects are in the system.
         *
         * NOTE: if the last PHYSICAL object is not present (e.g., the last
         * socket on the node is empty), then the count we return will
         * be wrong!
         */
        hwloc_obj_t found = NULL;
        obj = NULL;
        if (num_objs) {
            *num_objs = 0;
        }
        while ((obj = hwloc_get_next_obj_by_depth(topo, search_depth, obj)) != NULL) {
            if (num_objs && obj->os_index > *num_objs) {
                *num_objs = obj->os_index;
            }
            if (obj->os_index == nobj) {
                found = obj;
            }
        }
        return found;
    }
    if (OPAL_HWLOC_AVAILABLE == rtype) {
        // The previous (3.x) code included a check for
        // available = opal_hwloc_base_get_available_cpus(topo, start)
        // and skipped objs that had hwloc_bitmap_iszero(available)
        hwloc_obj_t root;
        opal_hwloc_topo_data_t *rdata = NULL;
        root = hwloc_get_root_obj(topo);
        if (false == opal_hwloc_topo_in_shmem) {
            rdata = (opal_hwloc_topo_data_t *) root->userdata;
        }
        hwloc_cpuset_t constrained_cpuset;

        constrained_cpuset = hwloc_bitmap_alloc();
        if (rdata && rdata->available) {
            hwloc_bitmap_and(constrained_cpuset, start->cpuset, rdata->available);
        } else {
            hwloc_bitmap_copy(constrained_cpuset, start->cpuset);
        }

        unsigned idx = 0;
        if (num_objs) {
            *num_objs = hwloc_get_nbobjs_inside_cpuset_by_depth(topo, constrained_cpuset,
                                                                search_depth);
        }
        obj = NULL;
        while ((obj = hwloc_get_next_obj_inside_cpuset_by_depth(topo, constrained_cpuset,
                                                                search_depth, obj))
               != NULL) {
            if (idx == nobj) {
                hwloc_bitmap_free(constrained_cpuset);
                return obj;
            }
            idx++;
        }
        hwloc_bitmap_free(constrained_cpuset);
        return NULL;
    }
    return NULL;
}

unsigned int opal_hwloc_base_get_nbobjs_by_type(hwloc_topology_t topo, hwloc_obj_type_t target,
                                                unsigned cache_level,
                                                opal_hwloc_resource_type_t rtype)
{
    unsigned int num_objs;
    hwloc_obj_t obj;
    opal_hwloc_summary_t *sum;
    opal_hwloc_topo_data_t *data = NULL;
    int rc;

    /* bozo check */
    if (NULL == topo) {
        OPAL_OUTPUT_VERBOSE(
            (5, opal_hwloc_base_framework.framework_output, "hwloc:base:get_nbobjs NULL topology"));
        return 0;
    }

    /* if we want the number of LOGICAL objects, we can just
     * use the hwloc accessor to get it, unless it is a CACHE
     * as these are treated as special cases
     */
    if (OPAL_HWLOC_LOGICAL == rtype
#if HWLOC_API_VERSION < 0x20000
        && HWLOC_OBJ_CACHE != target
#endif
    ) {
        /* we should not get an error back, but just in case... */
        if (0 > (rc = hwloc_get_nbobjs_by_type(topo, target))) {
            opal_output(0, "UNKNOWN HWLOC ERROR");
            return 0;
        }
        return rc;
    }

    /* for everything else, we have to do some work */
    num_objs = 0;
    obj = hwloc_get_root_obj(topo);

    /* first see if the topology already has this summary */
    if (false == opal_hwloc_topo_in_shmem) {
        data = (opal_hwloc_topo_data_t *) obj->userdata;
    }
    if (NULL == data) {
        data = OBJ_NEW(opal_hwloc_topo_data_t);
        if (false == opal_hwloc_topo_in_shmem) {
            // Can't touch userdata if in read-only shmem!
            // We have to protect here for the case where obj->userdata
            // is in shmem and it is NULL.
            obj->userdata = (void *) data;
        }
    } else {
        OPAL_LIST_FOREACH (sum, &data->summaries, opal_hwloc_summary_t) {
            if (target == sum->type && cache_level == sum->cache_level && rtype == sum->rtype) {
                /* yep - return the value */
                OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_framework.framework_output,
                                     "hwloc:base:get_nbojbs pre-existing data %u of %s:%u",
                                     sum->num_objs, hwloc_obj_type_string(target), cache_level));
                return sum->num_objs;
            }
        }
    }

    /* don't already know it - go get it */
    df_search(topo, obj, target, cache_level, 0, rtype, &num_objs);

    /* cache the results for later */
    sum = OBJ_NEW(opal_hwloc_summary_t);
    sum->type = target;
    sum->cache_level = cache_level;
    sum->num_objs = num_objs;
    sum->rtype = rtype;
    opal_list_append(&data->summaries, &sum->super);

    OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_framework.framework_output,
                         "hwloc:base:get_nbojbs computed data %u of %s:%u", num_objs,
                         hwloc_obj_type_string(target), cache_level));

    return num_objs;
}

/* Return the Nth instance of the specified object
 * type from inside the topology
 */
hwloc_obj_t opal_hwloc_base_get_obj_by_type(hwloc_topology_t topo, hwloc_obj_type_t target,
                                            unsigned cache_level, unsigned int instance,
                                            opal_hwloc_resource_type_t rtype)
{
    hwloc_obj_t obj;

    /* bozo check */
    if (NULL == topo) {
        return NULL;
    }

    /* if we want the nth LOGICAL object, we can just
     * use the hwloc accessor to get it, unless it is a CACHE
     * as these are treated as special cases
     */
    if (OPAL_HWLOC_LOGICAL == rtype
#if HWLOC_API_VERSION < 0x20000
        && HWLOC_OBJ_CACHE != target
#endif
    ) {
        return hwloc_get_obj_by_type(topo, target, instance);
    }

    /* for everything else, we have to do some work */
    obj = hwloc_get_root_obj(topo);
    return df_search(topo, obj, target, cache_level, instance, rtype, NULL);
}

char *opal_hwloc_base_get_location(char *locality, hwloc_obj_type_t type, unsigned index)
{
    char **loc;
    char *srch, *ans = NULL;
    size_t n;

    if (NULL == locality) {
        return NULL;
    }
    switch (type) {
    case HWLOC_OBJ_NODE:
        srch = "NM";
        break;
    case HWLOC_OBJ_SOCKET:
        srch = "SK";
        break;
#if HWLOC_API_VERSION < 0x20000
    case HWLOC_OBJ_CACHE:
        if (3 == index) {
            srch = "L3";
        } else if (2 == index) {
            srch = "L2";
        } else {
            srch = "L1";
        }
        break;
#else
    case HWLOC_OBJ_L3CACHE:
        srch = "L3";
        break;
    case HWLOC_OBJ_L2CACHE:
        srch = "L2";
        break;
    case HWLOC_OBJ_L1CACHE:
        srch = "L1";
        break;
#endif
    case HWLOC_OBJ_CORE:
        srch = "CR";
        break;
    case HWLOC_OBJ_PU:
        srch = "HT";
        break;
    default:
        return NULL;
    }
    loc = opal_argv_split(locality, ':');
    for (n = 0; NULL != loc[n]; n++) {
        if (0 == strncmp(loc[n], srch, 2)) {
            ans = strdup(&loc[n][2]);
            break;
        }
    }
    opal_argv_free(loc);

    return ans;
}

opal_hwloc_locality_t opal_hwloc_compute_relative_locality(char *loc1, char *loc2)
{
    opal_hwloc_locality_t locality;
    char **set1, **set2;
    hwloc_bitmap_t bit1, bit2;
    size_t n1, n2;

    /* start with what we know - they share a node on a cluster
     * NOTE: we may alter that latter part as hwloc's ability to
     * sense multi-cu, multi-cluster systems grows
     */
    locality = OPAL_PROC_ON_NODE | OPAL_PROC_ON_HOST | OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER;

    /* if either location is NULL, then that isn't bound */
    if (NULL == loc1 || NULL == loc2) {
        return locality;
    }

    set1 = opal_argv_split(loc1, ':');
    set2 = opal_argv_split(loc2, ':');
    bit1 = hwloc_bitmap_alloc();
    bit2 = hwloc_bitmap_alloc();

    /* check each matching type */
    for (n1 = 0; NULL != set1[n1]; n1++) {
        /* convert the location into bitmap */
        hwloc_bitmap_list_sscanf(bit1, &set1[n1][2]);
        /* find the matching type in set2 */
        for (n2 = 0; NULL != set2[n2]; n2++) {
            if (0 == strncmp(set1[n1], set2[n2], 2)) {
                /* convert the location into bitmap */
                hwloc_bitmap_list_sscanf(bit2, &set2[n2][2]);
                /* see if they intersect */
                if (hwloc_bitmap_intersects(bit1, bit2)) {
                    /* set the corresponding locality bit */
                    if (0 == strncmp(set1[n1], "NM", 2)) {
                        locality |= OPAL_PROC_ON_NUMA;
                    } else if (0 == strncmp(set1[n1], "SK", 2)) {
                        locality |= OPAL_PROC_ON_SOCKET;
                    } else if (0 == strncmp(set1[n1], "L3", 2)) {
                        locality |= OPAL_PROC_ON_L3CACHE;
                    } else if (0 == strncmp(set1[n1], "L2", 2)) {
                        locality |= OPAL_PROC_ON_L2CACHE;
                    } else if (0 == strncmp(set1[n1], "L1", 2)) {
                        locality |= OPAL_PROC_ON_L1CACHE;
                    } else if (0 == strncmp(set1[n1], "CR", 2)) {
                        locality |= OPAL_PROC_ON_CORE;
                    } else if (0 == strncmp(set1[n1], "HT", 2)) {
                        locality |= OPAL_PROC_ON_HWTHREAD;
                    } else {
                        /* should never happen */
                        opal_output(0, "UNRECOGNIZED LOCALITY %s", set1[n1]);
                    }
                }
                break;
            }
        }
    }
    opal_argv_free(set1);
    opal_argv_free(set2);
    hwloc_bitmap_free(bit1);
    hwloc_bitmap_free(bit2);
    return locality;
}
