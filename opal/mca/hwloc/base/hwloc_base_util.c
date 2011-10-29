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
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "opal/mca/paffinity/paffinity.h"

#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"

/*
 * Provide the hwloc object that corresponds to the given
 * LOGICAL processor id.  Remember: "processor" here [usually] means "core" --
 * except that on some platforms, hwloc won't find any cores; it'll
 * only find PUs (!).  On such platforms, then do the same calculation
 * but with PUs instead of COREs.
 */
static hwloc_obj_t get_pu(hwloc_topology_t topo,  int lid)
{
    hwloc_obj_type_t obj_type = HWLOC_OBJ_CORE;
    hwloc_obj_t obj;

    /* hwloc isn't able to find cores on all platforms.  Example:
       PPC64 running RHEL 5.4 (linux kernel 2.6.18) only reports NUMA
       nodes and PU's.  Fine.  

       However, note that hwloc_get_obj_by_type() will return NULL in
       2 (effectively) different cases:

       - no objects of the requested type were found
       - the Nth object of the requested type was not found

       So first we have to see if we can find *any* cores by looking
       for the 0th core.  If we find it, then try to find the Nth
       core.  Otherwise, try to find the Nth PU. */
    if (NULL == hwloc_get_obj_by_type(topo, HWLOC_OBJ_CORE, 0)) {
        obj_type = HWLOC_OBJ_PU;
    }

    /* Now do the actual lookup. */
    obj = hwloc_get_obj_by_type(topo, obj_type, lid);
    if (NULL == obj) {
        opal_show_help("help-opal-hwloc-base.txt",
                       "logical-cpu-not-found", true,
                       opal_hwloc_base_cpu_set);
        return NULL;
    }

    /* Found the right core (or PU). Return the object */
    return obj;
}

/* determine the node-level available cpuset based on
 * online vs allowed vs user-specified cpus
 */
int opal_hwloc_base_filter_cpus(hwloc_topology_t topo)
{
    hwloc_obj_t root, pu;
    hwloc_cpuset_t avail, pucpus, res;
    opal_hwloc_topo_data_t *sum;
    char **ranges=NULL, **range=NULL;
    int idx, cpu, start, end;

    root = hwloc_get_root_obj(topo);

    if (NULL == root->userdata) {
        /* create the summary object */
        sum = OBJ_NEW(opal_hwloc_topo_data_t);
        root->userdata = (void*)sum;
    } else {
        sum = (opal_hwloc_topo_data_t*)root->userdata;
    }
    /* should only ever enter here once, but check anyway */
    if (NULL != sum->available) {
        return OPAL_SUCCESS;
    }

    /* process any specified default cpu set against this topology */
    if (NULL == opal_hwloc_base_cpu_set) {
        /* get the root available cpuset */
        avail = hwloc_bitmap_alloc();
        hwloc_bitmap_and(avail, root->online_cpuset, root->allowed_cpuset);
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                             "hwloc:base: no cpus specified - using root available cpuset"));
    } else {
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                             "hwloc:base: filtering cpuset"));
        /* find the specified logical cpus */
        ranges = opal_argv_split(opal_hwloc_base_cpu_set, ',');
        for (idx=0; idx < opal_argv_count(ranges); idx++) {
            range = opal_argv_split(ranges[idx], '-');
            switch (opal_argv_count(range)) {
            case 1:
                /* only one cpu given - get that object */
                cpu = strtoul(range[0], NULL, 10);
                if (NULL == (pu = get_pu(topo, cpu))) {
                    opal_argv_free(ranges);
                    opal_argv_free(range);
                    return OPAL_ERROR;
                }
                avail = opal_hwloc_base_get_available_cpus(topo, pu);
                break;
            case 2:
                /* range given */
                start = strtoul(range[0], NULL, 10);
                end = strtoul(range[1], NULL, 10);
                avail = hwloc_bitmap_alloc();
                hwloc_bitmap_zero(avail);
                res = hwloc_bitmap_alloc();
                for (cpu=start; cpu <= end; cpu++) {
                    if (NULL == (pu = get_pu(topo, cpu))) {
                        opal_argv_free(ranges);
                        opal_argv_free(range);
                        hwloc_bitmap_free(avail);
                        return OPAL_ERROR;
                    }
                    pucpus = opal_hwloc_base_get_available_cpus(topo, pu);
                    hwloc_bitmap_or(res, avail, pucpus);
                    hwloc_bitmap_copy(avail, res);
                }
                hwloc_bitmap_free(res);
                break;
            default:
                return OPAL_ERR_BAD_PARAM;
            }
            opal_argv_free(range);
        }
        if (NULL != ranges) {
            opal_argv_free(ranges);
        }
    }

    /* cache this info */
    sum->available = avail;

    return OPAL_SUCCESS;
}

int opal_hwloc_base_get_topology(void)
{
    int rc;

    OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                         "hwloc:base:get_topology"));

    if (0 != hwloc_topology_init(&opal_hwloc_topology) ||
        0 != hwloc_topology_load(opal_hwloc_topology)) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    /* filter the cpus thru any default cpu set */
    rc = opal_hwloc_base_filter_cpus(opal_hwloc_topology);
    return rc;
}

static void free_object(hwloc_obj_t obj)
{
    opal_hwloc_obj_data_t *data;
    unsigned k;

    /* free any data hanging on this object */
    if (NULL != obj->userdata) {
        data = (opal_hwloc_obj_data_t*)obj->userdata;
        OBJ_RELEASE(data);
    }

    /* loop thru our children */
    for (k=0; k < obj->arity; k++) {
        free_object(obj->children[k]);
    }
}

void opal_hwloc_base_free_topology(hwloc_topology_t topo)
{
    hwloc_obj_t obj;
    opal_hwloc_topo_data_t *rdata;
    unsigned k;

    obj = hwloc_get_root_obj(topo);
    /* release the root-level userdata */
    if (NULL != obj->userdata) {
        rdata = (opal_hwloc_topo_data_t*)obj->userdata;
        OBJ_RELEASE(rdata);
    }
    /* now recursively descend and release userdata
     * in the rest of the objects
     */
    for (k=0; k < obj->arity; k++) {
        free_object(obj->children[k]);
    }
    hwloc_topology_destroy(topo);
}

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

hwloc_cpuset_t opal_hwloc_base_get_available_cpus(hwloc_topology_t topo,
                                                  hwloc_obj_t obj)
{
    hwloc_obj_t root;
    hwloc_cpuset_t avail, specd=NULL;
    opal_hwloc_topo_data_t *rdata;
    opal_hwloc_obj_data_t *data;

    /* get the node-level information - it was created
     * when we got the topology, so it is always present
     * or that's an error
     */
    root = hwloc_get_root_obj(topo);
    rdata = (opal_hwloc_topo_data_t*)root->userdata;
    /* bozo check */
    if (NULL == rdata) {
        return NULL;
    }

    /* are we asking about the root object? */
    if (obj == root) {
        return rdata->available;
    }

    /* see if we already have this info */
    if (NULL == (data = (opal_hwloc_obj_data_t*)obj->userdata)) {
        /* nope - create the object */
        data = OBJ_NEW(opal_hwloc_obj_data_t);
        obj->userdata = (void*)data;
    }

    /* do we have the cpuset */
    if (NULL != data->available) {
        return data->available;
    }

    /* find the available processors on this object */
    avail = hwloc_bitmap_alloc();
    hwloc_bitmap_and(avail, obj->online_cpuset, obj->allowed_cpuset);

    /* filter this against the node-available processors */
    specd = hwloc_bitmap_alloc();
    hwloc_bitmap_and(specd, avail, rdata->available);

    /* cache the info */
    data->available = specd;

    /* cleanup */
    hwloc_bitmap_free(avail);
    return specd;
}

/* get the number of pu's under a given hwloc object */
unsigned int opal_hwloc_base_get_npus(hwloc_topology_t topo,
                                      hwloc_obj_t obj)
{
    opal_hwloc_obj_data_t *data;
    int i;
    unsigned int cnt;
    hwloc_cpuset_t cpuset;

    data = (opal_hwloc_obj_data_t*)obj->userdata;

    if (NULL == data || 0 == data->npus) {
        /* get the available cpuset for this object - this will
         * create and store the data
         */
        cpuset = opal_hwloc_base_get_available_cpus(topo, obj);
        /* count the number of bits that are set - there is
         * one bit for each available pu
         */
        for (i=hwloc_bitmap_first(cpuset), cnt=0;
             i < hwloc_bitmap_last(cpuset);
             i++) {
            if (hwloc_bitmap_isset(cpuset, i)) {
                cnt++;
            }
        }
        /* cache the info */
        data = (opal_hwloc_obj_data_t*)obj->userdata;
        data->npus = cnt;
    }

    return data->npus;
}

/* hwloc treats cache objects as special
 * cases. Instead of having a unique type for each cache level,
 * there is a single cache object type, and the level is encoded
 * in an attribute union. So looking for cache objects involves
 * a multi-step test :-(
 *
 * And, of course, we make things even worse because we don't
 * always care about what is physically or logicallly present,
 * but rather what is available to us. For example, we don't
 * want to map or bind to a cpu that is offline, or one that
 * we aren't allowed by use by the OS. So we have to also filter
 * the search to avoid those objects that don't have any cpus
 * we can use :-((
 */
static hwloc_obj_t df_search(hwloc_obj_t start,
                             hwloc_obj_type_t target,
                             unsigned cache_level,
                             unsigned int nobj,
                             opal_hwloc_resource_type_t rtype,
                             unsigned int *idx,
                             unsigned int *num_objs)
{
    unsigned k;
    hwloc_obj_t obj;
    hwloc_bitmap_t res;

    if (target == start->type) {
        if (HWLOC_OBJ_CACHE == start->type && cache_level != start->attr->cache.depth) {
            goto notfound;
        }
        if (OPAL_HWLOC_LOGICAL == rtype) {
            /* the hwloc tree is composed of LOGICAL objects, so the only
             * time we come here is when we are looking for logical caches
             */
            if (NULL != num_objs) {
                /* we are counting the number of caches at this level */
                *num_objs += 1;
            } else if (*idx == nobj) {
                /* found the specific instance of the cache level being sought */
                return start;
            }
            *idx += 1;
            return NULL;
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
            if (NULL != num_objs) {
                /* we are counting the number of these objects */
                if (*num_objs < (unsigned int)start->os_index) {
                    *num_objs = (unsigned int)start->os_index;
                }
            } else if (*idx == nobj) {
                /* found the specific instance of the cache level being sought */
                return start;
            }
            *idx += 1;
            return NULL;
        }
        if (OPAL_HWLOC_AVAILABLE == rtype) {
            /* if we want only the available objects, then check the
             * cpusets to see if we have something we can use here
             */
            res = hwloc_bitmap_alloc();
            hwloc_bitmap_and(res, start->online_cpuset, start->allowed_cpuset);
            if (hwloc_bitmap_iszero(res)) {
                hwloc_bitmap_free(res);
                goto notfound;
            }
            hwloc_bitmap_free(res);
            if (NULL != num_objs) {
                *num_objs += 1;
            } else if (*idx == nobj) {
                return start;
            }
            *idx += 1;
            return NULL;
        }
        /* if it wasn't one of the above, then we are lost */
        return NULL;
    }

 notfound:
    for (k=0; k < start->arity; k++) {
        obj = df_search(start->children[k], target, cache_level, nobj, rtype, idx, num_objs);
        if (NULL != obj) {
            return obj;
        }
    }
    
    return NULL;
}

unsigned int opal_hwloc_base_get_nbobjs_by_type(hwloc_topology_t topo,
                                                hwloc_obj_type_t target,
                                                unsigned cache_level,
                                                opal_hwloc_resource_type_t rtype)
{
    unsigned int num_objs, idx;
    hwloc_obj_t obj;
    opal_list_item_t *item;
    opal_hwloc_summary_t *sum;
    opal_hwloc_topo_data_t *data;
    int rc;

    /* bozo check */
    if (NULL == topo) {
        return 0;
    }

    /* if we want the number of LOGICAL objects, we can just
     * use the hwloc accessor to get it, unless it is a CACHE
     * as these are treated as special cases
     */
    if (OPAL_HWLOC_LOGICAL == rtype && HWLOC_OBJ_CACHE != target) {
        /* we should not get an error back, but just in case... */
        if (0 > (rc = hwloc_get_nbobjs_by_type(topo, target))) {
            opal_output(0, "UNKNOWN HWLOC ERROR");
            return 0;
        }
        return rc;
    }

    /* for everything else, we have to do some work */
    num_objs = 0;
    idx = 0;
    obj = hwloc_get_root_obj(topo);

    /* first see if the topology already has this summary */
    data = (opal_hwloc_topo_data_t*)obj->userdata;
    for (item = opal_list_get_first(&data->summaries);
         item != opal_list_get_end(&data->summaries);
         item = opal_list_get_next(item)) {
        sum = (opal_hwloc_summary_t*)item;
        if (target == sum->type &&
            cache_level == sum->cache_level &&
            rtype == sum->rtype) {
            /* yep - return the value */
            return sum->num_objs;
        }
    }

    /* don't already know it - go get it */
    df_search(obj, target, cache_level, 0, rtype, &idx, &num_objs);

    /* cache the results for later */
    sum = OBJ_NEW(opal_hwloc_summary_t);
    sum->type = target;
    sum->cache_level = cache_level;
    sum->num_objs = num_objs;
    sum->rtype = rtype;
    opal_list_append(&data->summaries, &sum->super);

    return num_objs;
}

/* as above, only return the Nth instance of the specified object
 * type from inside the topology
 */
hwloc_obj_t opal_hwloc_base_get_obj_by_type(hwloc_topology_t topo,
                                            hwloc_obj_type_t target,
                                            unsigned cache_level,
                                            unsigned int instance,
                                            opal_hwloc_resource_type_t rtype)
{
    unsigned int num_objs, idx;
    hwloc_obj_t obj;

    /* bozo check */
    if (NULL == topo) {
        return NULL;
    }

    /* if we want the nth LOGICAL object, we can just
     * use the hwloc accessor to get it, unless it is a CACHE
     * as these are treated as special cases
     */
    if (OPAL_HWLOC_LOGICAL == rtype && HWLOC_OBJ_CACHE != target) {
        return hwloc_get_obj_by_type(topo, target, instance);
    }

    /* for everything else, we have to do some work */
    num_objs = 0;
    idx = 0;
    obj = hwloc_get_root_obj(topo);
    return df_search(obj, target, cache_level, instance, rtype, &idx, NULL);
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
