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
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
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
#include "opal/threads/tsd.h"

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
    hwloc_cpuset_t avail = NULL, pucpus, res;
    opal_hwloc_topo_data_t *sum;
    char **ranges=NULL, **range=NULL;
    int idx, cpu, start, end;

    root = hwloc_get_root_obj(topo);

    if (NULL == root->userdata) {
        root->userdata = (void*)OBJ_NEW(opal_hwloc_topo_data_t);
    }
    sum = (opal_hwloc_topo_data_t*)root->userdata;

    /* should only ever enter here once, but check anyway */
    if (NULL != sum->available) {
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                             "hwloc:base:filter_cpus specified - already done"));
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
        0 != hwloc_topology_set_flags(opal_hwloc_topology, 
                                      (HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM |
                                       HWLOC_TOPOLOGY_FLAG_WHOLE_IO)) ||
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

    if (!already_reported &&
        OPAL_HWLOC_BASE_MBFA_SILENT != opal_hwloc_base_mbfa) {
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

    OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                         "hwloc:base: get available cpus"));

    /* get the node-level information */
    root = hwloc_get_root_obj(topo);
    rdata = (opal_hwloc_topo_data_t*)root->userdata;
    /* bozo check */
    if (NULL == rdata) {
        rdata = OBJ_NEW(opal_hwloc_topo_data_t);
        root->userdata = (void*)rdata;
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                             "hwloc:base:get_available_cpus first time - filtering cpus"));
    }
    /* ensure the topo-level cpuset was prepared */
    opal_hwloc_base_filter_cpus(topo);

    /* are we asking about the root object? */
    if (obj == root) {
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                             "hwloc:base:get_available_cpus root object"));
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
    if (NULL == rdata->available) {
        hwloc_bitmap_free(avail);
        return NULL;
    }
    specd = hwloc_bitmap_alloc();
    hwloc_bitmap_and(specd, avail, rdata->available);

    /* cache the info */
    data->available = specd;

    /* cleanup */
    hwloc_bitmap_free(avail);
    return specd;
}

static void df_search_cores(hwloc_obj_t obj, unsigned int *cnt)
{
    unsigned k;

    if (HWLOC_OBJ_CORE == obj->type) {
        *cnt += 1;
        return;
    }

    for (k=0; k < obj->arity; k++) {
        df_search_cores(obj->children[k], cnt);
    }
    return;
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
        if (!opal_hwloc_use_hwthreads_as_cpus) {
            /* if we are treating cores as cpus, then we really
             * want to know how many cores are in this object.
             * hwloc sets a bit for each "pu", so we can't just
             * count bits in this case as there may be more than
             * one hwthread/core. Instead, find the number of cores
             * in the system
             *
             * NOTE: remember, hwloc can't find "cores" in all
             * environments. So first check to see if it found
             * "core" at all.
             */
            if (NULL != hwloc_get_obj_by_type(topo, HWLOC_OBJ_CORE, 0)) {
                /* starting at the incoming obj, do a down-first search
                 * and count the number of cores under it
                 */
                cnt = 0;
                df_search_cores(obj, &cnt);
            }
        } else {

            /* if we are treating cores as cpus, or the system can't detect
             * "cores", then get the available cpuset for this object - this will
             * create and store the data
             */
            if (NULL == (cpuset = opal_hwloc_base_get_available_cpus(topo, obj))) {
                return 0;
            }
            /* count the number of bits that are set - there is
             * one bit for each available pu. We could just
             * subtract the first and last indices, but there
             * may be "holes" in the bitmap corresponding to
             * offline or unallowed cpus - so we have to
             * search for them
             */
            for (i=hwloc_bitmap_first(cpuset), cnt=0;
                 i <= hwloc_bitmap_last(cpuset);
                 i++) {
                if (hwloc_bitmap_isset(cpuset, i)) {
                    cnt++;
                }
            }
        }
        /* cache the info */
        if (NULL == data) {
            data = OBJ_NEW(opal_hwloc_obj_data_t);
            obj->userdata = (void*)data;
        }
        data->npus = cnt;
    }

    return data->npus;
}

unsigned int opal_hwloc_base_get_obj_idx(hwloc_topology_t topo,
                                         hwloc_obj_t obj,
                                         opal_hwloc_resource_type_t rtype)
{
    unsigned cache_level=0;
    opal_hwloc_obj_data_t *data;
    hwloc_obj_t ptr;
    unsigned int nobjs, i;

    OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                         "hwloc:base:get_idx"));

    /* see if we already have the info */
    data = (opal_hwloc_obj_data_t*)obj->userdata;

    if (NULL == data) {
        data = OBJ_NEW(opal_hwloc_obj_data_t);
        obj->userdata = (void*)data;
    }

    if (data->idx < UINT_MAX) {
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                             "hwloc:base:get_idx already have data: %u",
                             data->idx));
        return data->idx;
    }

    /* determine the number of objects of this type */
    if (HWLOC_OBJ_CACHE == obj->type) {
        cache_level = obj->attr->cache.depth;
    }
    nobjs = opal_hwloc_base_get_nbobjs_by_type(topo, obj->type, cache_level, rtype);

    OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                         "hwloc:base:get_idx found %u objects of type %s:%u",
                         nobjs, hwloc_obj_type_string(obj->type), cache_level));

    /* find this object */
    for (i=0; i < nobjs; i++) {
        ptr = opal_hwloc_base_get_obj_by_type(topo, obj->type, cache_level, i, rtype);
        if (ptr == obj) {
            data->idx = i;
            return i;
        }
    }
    /* if we get here, it wasn't found */
    opal_show_help("help-opal-hwloc-base.txt",
                   "obj-idx-failed", true,
                   hwloc_obj_type_string(obj->type), cache_level);
    return UINT_MAX;
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
    opal_hwloc_obj_data_t *data;

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
            /* check - do we already know the index of this object */
            data = (opal_hwloc_obj_data_t*)start->userdata;
            if (NULL == data) {
                data = OBJ_NEW(opal_hwloc_obj_data_t);
                start->userdata = (void*)data;
            }
            /* if we already know our location and it matches,
             * then we are good
             */
            if (UINT_MAX != data->idx && data->idx == nobj) {
                return start;
            }
            /* see if we already know our available cpuset */
            if (NULL == data->available) {
                /* if we want only the available objects, then check the
                 * cpusets to see if we have something we can use here
                 */
                res = hwloc_bitmap_alloc();
                hwloc_bitmap_and(res, start->online_cpuset, start->allowed_cpuset);
                if (hwloc_bitmap_iszero(res)) {
                    /* this object has no available cpus */
                    hwloc_bitmap_free(res);
                    goto notfound;
                }
                /* cache the info */
                data->available = res;
            }
            if (NULL != num_objs) {
                *num_objs += 1;
            } else if (*idx == nobj) {
                /* cache the location */
                data->idx = *idx;
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
        OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                             "hwloc:base:get_nbobjs NULL topology"));
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
    if (NULL == data) {
        data = OBJ_NEW(opal_hwloc_topo_data_t);
        obj->userdata = (void*)data;
    } else {
        for (item = opal_list_get_first(&data->summaries);
             item != opal_list_get_end(&data->summaries);
             item = opal_list_get_next(item)) {
            sum = (opal_hwloc_summary_t*)item;
            if (target == sum->type &&
                cache_level == sum->cache_level &&
                rtype == sum->rtype) {
                /* yep - return the value */
                OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                                     "hwloc:base:get_nbojbs pre-existing data %u of %s:%u",
                                     sum->num_objs, hwloc_obj_type_string(target), cache_level));
                return sum->num_objs;
            }
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

    OPAL_OUTPUT_VERBOSE((5, opal_hwloc_base_output,
                         "hwloc:base:get_nbojbs computed data %u of %s:%u",
                         num_objs, hwloc_obj_type_string(target), cache_level));

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

/* The current slot_list notation only goes to the core level - i.e., the location
 * is specified as socket:core. Thus, the code below assumes that all locations
 * are to be parsed under that notation.
 */

static int socket_to_cpu_set(char *cpus,
                             hwloc_topology_t topo,
                             hwloc_bitmap_t cpumask)
{
    char **range;
    int range_cnt;
    int lower_range, upper_range;
    int socket_id;
    hwloc_obj_t obj;
    hwloc_bitmap_t avail, res;

    if ('*' == cpus[0]) {
        /* requesting cpumask for ALL sockets */
        obj = hwloc_get_root_obj(topo);
        /* set to all available logical processors - essentially,
         * this specification equates to unbound
         */
        res = opal_hwloc_base_get_available_cpus(topo, obj);
        hwloc_bitmap_copy(cpumask, res);
        return OPAL_SUCCESS;
    }

    range = opal_argv_split(cpus,'-');
    range_cnt = opal_argv_count(range);
    switch (range_cnt) {
    case 1:  /* no range was present, so just one socket given */
        socket_id = atoi(range[0]);
        obj = opal_hwloc_base_get_obj_by_type(topo, HWLOC_OBJ_SOCKET, 0, socket_id, OPAL_HWLOC_LOGICAL);
        /* get the available logical cpus for this socket */
        res = opal_hwloc_base_get_available_cpus(topo, obj);
        hwloc_bitmap_copy(cpumask, res);
        break;
                
    case 2:  /* range of sockets was given */
        lower_range = atoi(range[0]);
        upper_range = atoi(range[1]);
        /* zero the bitmask */
        hwloc_bitmap_zero(cpumask);
        avail = hwloc_bitmap_alloc();
        /* cycle across the range of sockets */
        for (socket_id=lower_range; socket_id<=upper_range; socket_id++) {
            obj = opal_hwloc_base_get_obj_by_type(topo, HWLOC_OBJ_SOCKET, 0, socket_id, OPAL_HWLOC_LOGICAL);
            /* get the available logical cpus for this socket */
            res = opal_hwloc_base_get_available_cpus(topo, obj);
            /* set the corresponding bits in the bitmask */
            hwloc_bitmap_or(avail, cpumask, res);
            hwloc_bitmap_copy(cpumask, avail);
        }
        hwloc_bitmap_free(avail);
        break;
    default:
        opal_argv_free(range);
        return OPAL_ERROR;
    }
    opal_argv_free(range);

    return OPAL_SUCCESS;
}

static int socket_core_to_cpu_set(char *socket_core_list,
                                  hwloc_topology_t topo,
                                  hwloc_bitmap_t cpumask)
{
    int rc=OPAL_SUCCESS, i;
    char **socket_core, *corestr;
    char **range;
    int range_cnt;
    int lower_range, upper_range;
    int socket_id, core_id;
    hwloc_obj_t socket, core;
    hwloc_cpuset_t res, avail;
    unsigned int idx;
    hwloc_obj_type_t obj_type = HWLOC_OBJ_CORE;

    socket_core = opal_argv_split(socket_core_list, ':');
    socket_id = atoi(socket_core[0]);
    
    /* get the object for this socket id */
    if (NULL == (socket = opal_hwloc_base_get_obj_by_type(topo, HWLOC_OBJ_SOCKET, 0,
                                                          socket_id, OPAL_HWLOC_LOGICAL))) {
        opal_argv_free(socket_core);
        return OPAL_ERR_NOT_FOUND;
    }

    /* as described in comment near top of file, hwloc isn't able
     * to find cores on all platforms. Adjust the type here if
     * required
     */
    if (NULL == hwloc_get_obj_by_type(topo, HWLOC_OBJ_CORE, 0)) {
        obj_type = HWLOC_OBJ_PU;
    }

    for (i=0; NULL != socket_core[i]; i++) {
        if ('C' == socket_core[i][0] ||
            'c' == socket_core[i][0]) {
            corestr = &socket_core[i][1];
        } else {
            corestr = socket_core[i];
        }
        if ('*' == corestr[0]) {
            /* set to all available logical cpus on this socket */
            res = opal_hwloc_base_get_available_cpus(topo, socket);
            hwloc_bitmap_copy(cpumask, res);
            /* we are done - already assigned all cores! */
            rc = OPAL_SUCCESS;
            break;
        } else {
            range = opal_argv_split(corestr, '-');
            range_cnt = opal_argv_count(range);
            /* see if a range was set or not */
            switch (range_cnt) {
            case 1:  /* only one core specified */
                core_id = atoi(range[0]);
                /* get that object */
                idx = 0;
                if (NULL == (core = df_search(socket, obj_type, 0,
                                              core_id, OPAL_HWLOC_AVAILABLE,
                                              &idx, NULL))) {
                    return OPAL_ERR_NOT_FOUND;
                }
                /* get the cpus */
                res = opal_hwloc_base_get_available_cpus(topo, core);
                hwloc_bitmap_copy(cpumask, res);
                break;
                
            case 2:  /* range of core id's was given */
                lower_range = atoi(range[0]);
                upper_range = atoi(range[1]);
                hwloc_bitmap_zero(cpumask);
                avail = hwloc_bitmap_alloc();
                for (core_id=lower_range; core_id <= upper_range; core_id++) {
                    /* get that object */
                    idx = 0;
                    if (NULL == (core = df_search(socket, obj_type, 0,
                                                  core_id, OPAL_HWLOC_AVAILABLE,
                                                  &idx, NULL))) {
                        return OPAL_ERR_NOT_FOUND;
                    }
                    /* get the cpus */
                    res = opal_hwloc_base_get_available_cpus(topo, core);
                    /* add them into the result */
                    hwloc_bitmap_or(avail, cpumask, res);
                    hwloc_bitmap_copy(cpumask, avail);
                }
                hwloc_bitmap_free(avail);
                break;
                
            default:
                opal_argv_free(range);
                opal_argv_free(socket_core);
                return OPAL_ERROR;
            }
            opal_argv_free(range);
        }
    }
    opal_argv_free(socket_core);

    return rc;
}

int opal_hwloc_base_slot_list_parse(const char *slot_str,
                                    hwloc_topology_t topo,
                                    hwloc_cpuset_t cpumask)
{
    char **item;
    int rc, i;
    hwloc_obj_t pu;
    hwloc_cpuset_t pucpus;
    char **range;
    size_t range_cnt;
    int core_id, lower_range, upper_range;

    /* bozo checks */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    if (NULL == slot_str || 0 == strlen(slot_str)) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    opal_output_verbose(5, opal_hwloc_base_output,
                        "slot assignment: slot_list == %s",
                        slot_str);
    
    /* split at ';' */
    item = opal_argv_split (slot_str, ';');

    /* start with a clean mask */
    hwloc_bitmap_zero(cpumask);
    /* loop across the items and accumulate the mask */
    for (i=0; NULL != item[i]; i++) {
        /* if they specified "socket" by starting with an S/s,
         * or if they use socket:core notation, then parse the
         * socket/core info
         */
        if ('S' == item[i][0] ||
            's' == item[i][0] ||
            NULL != strchr(item[i], ':')) {
            /* specified a socket */
            if (NULL == strchr(item[i], ':')) {
                /* binding just to the socket level, though
                 * it could specify multiple sockets
                 */
                if (OPAL_SUCCESS != (rc = socket_to_cpu_set(&item[i][1],  /* skip the 'S' */
                                                            topo, cpumask))) {
                    opal_argv_free(item);
                    return rc;
                }
            } else {
                /* binding to a socket/whatever specification */
                if ('S' == item[i][0] ||
                    's' == item[i][0]) {
                    if (OPAL_SUCCESS != (rc = socket_core_to_cpu_set(&item[i][1],  /* skip the 'S' */
                                                                     topo, cpumask))) {
                        opal_argv_free(item);
                        return rc;
                    }
                } else {
                    if (OPAL_SUCCESS != (rc = socket_core_to_cpu_set(item[i],
                                                                     topo, cpumask))) {
                        opal_argv_free(item);
                        return rc;
                    }
                }
            }
        } else {
            /* just a core specification - see if one or a range was given */
            range = opal_argv_split(item[i], '-');
            range_cnt = opal_argv_count(range);
            hwloc_bitmap_zero(cpumask);
            /* see if a range was set or not */
            switch (range_cnt) {
            case 1:  /* only one core specified */
                core_id = atoi(range[0]);
                /* find the specified logical available cpu */
                if (NULL == (pu = get_pu(topo, core_id))) {
                    opal_argv_free(range);
                    opal_argv_free(item);
                    return OPAL_ERROR;
                }
                /* get the available cpus for that object */
                pucpus = opal_hwloc_base_get_available_cpus(topo, pu);
                 /* set that in the mask */
                hwloc_bitmap_copy(cpumask, pucpus);
                break;
                    
            case 2:  /* range of core id's was given */
                lower_range = atoi(range[0]);
                upper_range = atoi(range[1]);
                hwloc_bitmap_zero(cpumask);
                for (core_id=lower_range; core_id <= upper_range; core_id++) {
                    /* find the specified logical available cpu */
                    if (NULL == (pu = get_pu(topo, core_id))) {
                        opal_argv_free(range);
                        opal_argv_free(item);
                        return OPAL_ERROR;
                    }
                    /* get the available cpus for that object */
                    pucpus = opal_hwloc_base_get_available_cpus(topo, pu);
                    /* set that in the mask */
                    hwloc_bitmap_or(cpumask, cpumask, pucpus);
                }
                break;
                
            default:
                opal_argv_free(range);
                opal_argv_free(item);
                return OPAL_ERROR;
            }
        }
    }
    opal_argv_free(item);
    return OPAL_SUCCESS;
}

static opal_paffinity_locality_t get_locality(opal_hwloc_level_t level)
{
    opal_paffinity_locality_t lvl;

    switch(level) {
    case OPAL_HWLOC_NODE_LEVEL:
        lvl = OPAL_PROC_ON_NODE;
        break;
    case OPAL_HWLOC_NUMA_LEVEL:
        lvl = OPAL_PROC_ON_NUMA;
        break;
    case OPAL_HWLOC_SOCKET_LEVEL:
        lvl = OPAL_PROC_ON_SOCKET;
        break;
    case OPAL_HWLOC_L3CACHE_LEVEL:
        lvl = OPAL_PROC_ON_L3CACHE;
        break;
    case OPAL_HWLOC_L2CACHE_LEVEL:
        lvl = OPAL_PROC_ON_L2CACHE;
        break;
    case OPAL_HWLOC_L1CACHE_LEVEL:
        lvl = OPAL_PROC_ON_L1CACHE;
        break;
    case OPAL_HWLOC_CORE_LEVEL:
        lvl = OPAL_PROC_ON_CORE;
        break;
    case OPAL_HWLOC_HWTHREAD_LEVEL:
        lvl = OPAL_PROC_ON_HWTHREAD;
        break;
    }

    return lvl;
}

opal_paffinity_locality_t opal_hwloc_base_get_relative_locality(hwloc_topology_t topo,
                                                                opal_hwloc_level_t level1,
                                                                unsigned int peer1,
                                                                opal_hwloc_level_t level2,
                                                                unsigned int peer2)
{
    opal_paffinity_locality_t locality;
    hwloc_obj_t obj1, obj2;
    unsigned cache_level=0;
    opal_hwloc_level_t i, lvl;

    /* start with what we know - they share a node on a cluster
     * NOTE: we may alter that latter part as hwloc's ability to
     * sense multi-cu, multi-cluster systems grows
     */
    locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE | OPAL_PROC_ON_BOARD;

    /* TBD: handle procs bound at different levels - means they
     * are from different jobs
     */
    if (level1 != level2) {
        return locality;
    }

    lvl = level1;

    /* we know that the objects are bound to the same level, so
     * if the two objects are the index, then they share
     * all levels down to and including their own
     */
    if (peer1 == peer2) {
        for (i=lvl; 0 < i; i--) {
            opal_output_verbose(5, opal_hwloc_base_output,
                                "equal level - computing locality: %s",
                                opal_hwloc_base_print_level(i));
            locality |= get_locality(i);
        }
        goto checkpu;
    }

    /* get cache level if required */
    if (OPAL_HWLOC_L3CACHE_LEVEL == lvl) {
        cache_level = 3;
    } else if (OPAL_HWLOC_L2CACHE_LEVEL == lvl) {
        cache_level = 2;
    } else if (OPAL_HWLOC_L1CACHE_LEVEL == lvl) {
        cache_level = 1;
    }

    /* get the objects for these peers */
    opal_output_verbose(5, opal_hwloc_base_output,
                        "computing locality - getting object at level %s, index %u",
                        opal_hwloc_base_print_level(lvl), peer1);
    obj1 = opal_hwloc_base_get_obj_by_type(topo, opal_hwloc_levels[lvl],
                                           cache_level, peer1, OPAL_HWLOC_AVAILABLE);
    opal_output_verbose(5, opal_hwloc_base_output,
                        "computing locality - getting object at level %s, index %u",
                        opal_hwloc_base_print_level(lvl), peer2);
    obj2 = opal_hwloc_base_get_obj_by_type(topo, opal_hwloc_levels[lvl],
                                           cache_level, peer2, OPAL_HWLOC_AVAILABLE);

    /* climb the levels
     * NOTE: for now, we will just assume that the two objects
     * have a common topology above them - i.e., that each
     * object has the same levels above them. In cases where
     * nodes have heterogeneous sockets, this won't be true - but
     * leave that problem for another day
     */
    --lvl;
    while (OPAL_HWLOC_NODE_LEVEL < lvl &&
           NULL != obj1 && NULL != obj2 && obj1 != obj2) {
        opal_output_verbose(5, opal_hwloc_base_output,
                            "computing locality - shifting up from %s",
                            opal_hwloc_base_print_level(lvl));
        obj1 = obj1->parent;
        obj2 = obj2->parent;
        --lvl;
    }

    /* set the locality */
    for (i=lvl; 0 < i; i--) {
        opal_output_verbose(5, opal_hwloc_base_output,
                            "computing locality - filling level %s",
                            opal_hwloc_base_print_level(i));
        locality |= get_locality(i);
    }

 checkpu:
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

    opal_output_verbose(5, opal_hwloc_base_output,
                        "locality: %s",
                        opal_hwloc_base_print_locality(locality));

    return locality;
}

static hwloc_obj_t df_search_level(hwloc_obj_t start,
                                   hwloc_cpuset_t cpus,
                                   opal_hwloc_level_t *bind_level)
{
    unsigned k;
    hwloc_obj_t obj;
    hwloc_cpuset_t avail;

    /* get the available cpus */
    avail = opal_hwloc_base_get_available_cpus(opal_hwloc_topology, start);
    if (NULL != avail && 0 == hwloc_bitmap_compare(avail, cpus)) {
        /* convert the level */
        if (HWLOC_OBJ_MACHINE == start->type) {
            *bind_level = OPAL_HWLOC_NODE_LEVEL;
        } else if (HWLOC_OBJ_NODE == start->type) {
            *bind_level = OPAL_HWLOC_NUMA_LEVEL;
        } else if (HWLOC_OBJ_SOCKET == start->type) {
            *bind_level = OPAL_HWLOC_SOCKET_LEVEL;
        } else if (HWLOC_OBJ_CACHE == start->type) {
            if (3 == start->attr->cache.depth) {
                *bind_level = OPAL_HWLOC_L3CACHE_LEVEL;
            } else if (2 == start->attr->cache.depth) {
                *bind_level = OPAL_HWLOC_L2CACHE_LEVEL;
            } else {
                *bind_level = OPAL_HWLOC_L1CACHE_LEVEL;
            }
        } else if (HWLOC_OBJ_CORE == start->type) {
            *bind_level = OPAL_HWLOC_CORE_LEVEL;
        } else if (HWLOC_OBJ_PU == start->type) {
            *bind_level = OPAL_HWLOC_HWTHREAD_LEVEL;
        } else {
            /* We don't know what level it is, so just assign it to
               "node" */
            *bind_level = OPAL_HWLOC_NODE_LEVEL;
        }
        return start;
    }

    /* continue the search */
    for (k=0; k < start->arity; k++) {
        obj = df_search_level(start->children[k], cpus, bind_level);
        if (NULL != obj) {
            return obj;
        }
    }
    return NULL;
}

void opal_hwloc_base_get_level_and_index(hwloc_cpuset_t cpus,
                                         opal_hwloc_level_t *bind_level,
                                         unsigned int *bind_idx)
{
    hwloc_obj_t root, obj;

    /* if we don't have topology info, nothing we can do */
    if (NULL == opal_hwloc_topology) {
        *bind_level = OPAL_HWLOC_NODE_LEVEL;
        *bind_idx = 0;
        return;
    }

    /* start at the node level and do a down-first
     * search until we find an exact match for the cpus
     */
    *bind_level = OPAL_HWLOC_NODE_LEVEL;
    *bind_idx = 0;
    root = hwloc_get_root_obj(opal_hwloc_topology);
    obj = df_search_level(root, cpus, bind_level);

    if (NULL == obj) {
        /* no match found */
        return;
    }

    /* get the index */
    *bind_idx = opal_hwloc_base_get_obj_idx(opal_hwloc_topology, obj, OPAL_HWLOC_AVAILABLE);
}

#define OPAL_HWLOC_PRINT_MAX_SIZE   50
#define OPAL_HWLOC_PRINT_NUM_BUFS   16

static bool fns_init=false;
static opal_tsd_key_t print_tsd_key;
static char* opal_hwloc_print_null = "NULL";
typedef struct {
    char *buffers[OPAL_HWLOC_PRINT_NUM_BUFS];
    int cntr;
} opal_hwloc_print_buffers_t;

static void buffer_cleanup(void *value)
{
    int i;
    opal_hwloc_print_buffers_t *ptr;
    
    if (NULL != value) {
        ptr = (opal_hwloc_print_buffers_t*)value;
        for (i=0; i < OPAL_HWLOC_PRINT_NUM_BUFS; i++) {
            free(ptr->buffers[i]);
        }
    }
}

static opal_hwloc_print_buffers_t *get_print_buffer(void)
{
    opal_hwloc_print_buffers_t *ptr;
    int ret, i;
    
    if (!fns_init) {
        /* setup the print_args function */
        if (OPAL_SUCCESS != (ret = opal_tsd_key_create(&print_tsd_key, buffer_cleanup))) {
            return NULL;
        }
        fns_init = true;
    }
    
    ret = opal_tsd_getspecific(print_tsd_key, (void**)&ptr);
    if (OPAL_SUCCESS != ret) return NULL;
    
    if (NULL == ptr) {
        ptr = (opal_hwloc_print_buffers_t*)malloc(sizeof(opal_hwloc_print_buffers_t));
        for (i=0; i < OPAL_HWLOC_PRINT_NUM_BUFS; i++) {
            ptr->buffers[i] = (char *) malloc((OPAL_HWLOC_PRINT_MAX_SIZE+1) * sizeof(char));
        }
        ptr->cntr = 0;
        ret = opal_tsd_setspecific(print_tsd_key, (void*)ptr);
    }
    
    return (opal_hwloc_print_buffers_t*) ptr;
}

char* opal_hwloc_base_print_binding(opal_binding_policy_t binding)
{
    char *ret, *bind;
    opal_hwloc_print_buffers_t *ptr;

    switch(OPAL_GET_BINDING_POLICY(binding)) {
    case OPAL_BIND_TO_NONE:
        bind = "NONE";
        break;
    case OPAL_BIND_TO_BOARD:
        bind = "BOARD";
        break;
    case OPAL_BIND_TO_NUMA:
        bind = "NUMA";
        break;
    case OPAL_BIND_TO_SOCKET:
        bind = "SOCKET";
        break;
    case OPAL_BIND_TO_L3CACHE:
        bind = "L3CACHE";
        break;
    case OPAL_BIND_TO_L2CACHE:
        bind = "L2CACHE";
        break;
    case OPAL_BIND_TO_L1CACHE:
        bind = "L1CACHE";
        break;
    case OPAL_BIND_TO_CORE:
        bind = "CORE";
        break;
    case OPAL_BIND_TO_HWTHREAD:
        bind = "HWTHREAD";
        break;
    case OPAL_BIND_TO_CPUSET:
        bind = "CPUSET";
        break;
    default:
        bind = "UNKNOWN";
    }
    ptr = get_print_buffer();
    if (NULL == ptr) {
        return opal_hwloc_print_null;
    }
    /* cycle around the ring */
    if (OPAL_HWLOC_PRINT_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }
    if (!OPAL_BINDING_REQUIRED(binding) &&
        OPAL_BIND_OVERLOAD_ALLOWED(binding)) {
        snprintf(ptr->buffers[ptr->cntr], OPAL_HWLOC_PRINT_MAX_SIZE,
                 "%s:IF-SUPPORTED:OVERLOAD-ALLOWED", bind);
    } else if (OPAL_BIND_OVERLOAD_ALLOWED(binding)) {
        snprintf(ptr->buffers[ptr->cntr], OPAL_HWLOC_PRINT_MAX_SIZE,
                 "%s:OVERLOAD-ALLOWED", bind);
    } else if (!OPAL_BINDING_REQUIRED(binding)) {
        snprintf(ptr->buffers[ptr->cntr], OPAL_HWLOC_PRINT_MAX_SIZE,
                 "%s:IF-SUPPORTED", bind);
    } else {
        snprintf(ptr->buffers[ptr->cntr], OPAL_HWLOC_PRINT_MAX_SIZE, "%s", bind);
    }
    ret = ptr->buffers[ptr->cntr];
    ptr->cntr++;

    return ret;
}

char* opal_hwloc_base_print_level(opal_hwloc_level_t level)
{
    char *ret = "unknown";

    switch(level) {
    case OPAL_HWLOC_NODE_LEVEL:
        ret = "NODE";
        break;
    case OPAL_HWLOC_NUMA_LEVEL:
        ret = "NUMA";
        break;
    case OPAL_HWLOC_SOCKET_LEVEL:
        ret = "SOCKET";
        break;
    case OPAL_HWLOC_L3CACHE_LEVEL:
        ret = "L3CACHE";
        break;
    case OPAL_HWLOC_L2CACHE_LEVEL:
        ret = "L2CACHE";
        break;
    case OPAL_HWLOC_L1CACHE_LEVEL:
        ret = "L1CACHE";
        break;
    case OPAL_HWLOC_CORE_LEVEL:
        ret = "CORE";
        break;
    case OPAL_HWLOC_HWTHREAD_LEVEL:
        ret = "HWTHREAD";
        break;
    }
    return ret;
}

char* opal_hwloc_base_print_locality(opal_paffinity_locality_t locality)
{
    opal_hwloc_print_buffers_t *ptr;
    int idx;

    ptr = get_print_buffer();
    if (NULL == ptr) {
        return opal_hwloc_print_null;
    }
    /* cycle around the ring */
    if (OPAL_HWLOC_PRINT_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    idx = 0;

    if (OPAL_PROC_ON_LOCAL_CLUSTER(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'C';
        ptr->buffers[ptr->cntr][idx++] = 'L';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (OPAL_PROC_ON_LOCAL_CU(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'C';
        ptr->buffers[ptr->cntr][idx++] = 'U';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (OPAL_PROC_ON_LOCAL_NODE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'N';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (OPAL_PROC_ON_LOCAL_BOARD(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'B';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (OPAL_PROC_ON_LOCAL_NUMA(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'N';
        ptr->buffers[ptr->cntr][idx++] = 'u';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (OPAL_PROC_ON_LOCAL_SOCKET(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'S';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (OPAL_PROC_ON_LOCAL_L3CACHE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'L';
        ptr->buffers[ptr->cntr][idx++] = '3';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (OPAL_PROC_ON_LOCAL_L2CACHE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'L';
        ptr->buffers[ptr->cntr][idx++] = '2';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (OPAL_PROC_ON_LOCAL_L1CACHE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'L';
        ptr->buffers[ptr->cntr][idx++] = '1';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (OPAL_PROC_ON_LOCAL_CORE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'C';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (OPAL_PROC_ON_LOCAL_HWTHREAD(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'H';
        ptr->buffers[ptr->cntr][idx++] = 'w';
        ptr->buffers[ptr->cntr][idx++] = 't';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (0 < idx) {
        ptr->buffers[ptr->cntr][idx-1] = '\0';
    }
    return ptr->buffers[ptr->cntr];
}
