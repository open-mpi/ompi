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
 * Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

/* This component will only be compiled on Hwloc, where we are
   guaranteed to have <unistd.h> and friends */
#include <stdio.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "paffinity_hwloc.h"
#include "opal/mca/common/hwloc/hwloc/include/hwloc.h"

/*
 * Local functions
 */
static int module_init(void);
static int module_set(opal_paffinity_base_cpu_set_t cpumask);
static int module_get(opal_paffinity_base_cpu_set_t *cpumask);
static int module_map_to_processor_id(int socket, int core, int *processor_id);
static int module_map_to_socket_core(int processor_id, int *socket, int *core);
static int module_get_processor_info(int *num_processors);
static int module_get_socket_info(int *num_sockets);
static int module_get_core_info(int socket, int *num_cores);
static int module_get_physical_processor_id(int logical_processor_id,
                                            int *physical_processor_id);
static int module_get_physical_socket_id(int logical_socket_id,
                                         int *physical_socket_id);
static int module_get_physical_core_id(int physical_socket_id, 
                                       int logical_core_id,
                                       int *physical_core_id);

/*
 * Hwloc paffinity module
 */
static const opal_paffinity_base_module_1_1_0_t loc_module = {
    /* Initialization function */
    module_init,

    /* Module function pointers */
    module_set,
    module_get,
    module_map_to_processor_id,
    module_map_to_socket_core,
    module_get_processor_info,
    module_get_socket_info,
    module_get_core_info,
    module_get_physical_processor_id,
    module_get_physical_socket_id,
    module_get_physical_core_id,
    NULL
};

/*
 * Trivial DFS traversal recursion function
 */
static hwloc_obj_t dfs_find_os_index(hwloc_obj_t root, hwloc_obj_type_t type, 
                                     unsigned os_index)
{
    unsigned i;
    hwloc_obj_t ret;

    if (root->type == type && root->os_index == os_index) {
        return root;
    }
    for (i = 0; i < root->arity; ++i) {
        ret = dfs_find_os_index(root->children[i], type, os_index);
        if (NULL != ret) {
            return ret;
        }
    }

    return NULL;
}

/*
 * Trivial DFS traversal recursion function
 */
static hwloc_obj_t dfs_find_nth_item(hwloc_obj_t root, 
                                     hwloc_obj_type_t type, 
                                     unsigned *current,
                                     unsigned n)
{
    unsigned i;
    hwloc_obj_t ret;

    if (root->type == type) {
        if (*current == n) {
            return root;
        }
        ++(*current);
    }
    for (i = 0; i < root->arity; ++i) {
        ret = dfs_find_nth_item(root->children[i], type, current, n);
        if (NULL != ret) {
            return ret;
        }
    }

    return NULL;
}

/*
 * Trivial DFS traversal recursion function
 */
static int dfs_count_type(hwloc_obj_t root, hwloc_obj_type_t type)
{
    unsigned i;
    int count = 0;
    if (root->type == type) {
        ++count;
    }
    for (i = 0; i < root->arity; ++i) {
        count += dfs_count_type(root->children[i], type);
    }

    return count;
}


int opal_paffinity_hwloc_component_query(mca_base_module_t **module, 
                                         int *priority)
{
    int param;

    param = mca_base_param_find("paffinity", "hwloc", "priority");
    mca_base_param_lookup_int(param, priority);

    *module = (mca_base_module_t *)&loc_module;

    return OPAL_SUCCESS;
}


static int module_init(void)
{
    /* Nothing to do */

    return OPAL_SUCCESS;
}


static int module_set(opal_paffinity_base_cpu_set_t mask)
{
    int i, ret = OPAL_SUCCESS;
    hwloc_bitmap_t set;
    hwloc_topology_t *t = &mca_paffinity_hwloc_component.topology;

    set = hwloc_bitmap_alloc();
    hwloc_bitmap_zero(set);
    for (i = 0; ((unsigned int) i) < OPAL_PAFFINITY_BITMASK_T_NUM_BITS; ++i) {
        if (OPAL_PAFFINITY_CPU_ISSET(i, mask)) {
            hwloc_bitmap_set(set, i);
        }
    }

    if (0 != hwloc_set_cpubind(*t, set, 0)) {
        ret = OPAL_ERR_IN_ERRNO;
    }
    hwloc_bitmap_free(set);

    return ret;
}


static int module_get(opal_paffinity_base_cpu_set_t *mask)
{
    int i, ret = OPAL_SUCCESS;
    hwloc_bitmap_t set;
    hwloc_topology_t *t = &mca_paffinity_hwloc_component.topology;

    if (NULL == mask) {
        return OPAL_ERR_BAD_PARAM;
    }

    set = hwloc_bitmap_alloc();
    if (0 != hwloc_get_cpubind(*t, set, 0)) {
        ret = OPAL_ERR_IN_ERRNO;
    } else {
        OPAL_PAFFINITY_CPU_ZERO(*mask);
        for (i = 0; ((unsigned int) i) < 8 * sizeof(*mask); i++) {
            if (hwloc_bitmap_isset(set, i)) {
                OPAL_PAFFINITY_CPU_SET(i, *mask);
            }
        }
    }
    hwloc_bitmap_free(set);

    return ret;
}

/*
 * Returns mapping of PHYSICAL socket:core -> PHYSICAL processor id.
 *
 * Since paffinity currently does not understand hardware threads,
 * return the processor ID of the first hardware thread in the target
 * core.
 */
static int module_map_to_processor_id(int socket, int core, int *processor_id)
{
    hwloc_topology_t *t = &mca_paffinity_hwloc_component.topology;
    hwloc_obj_t obj;

    /* Traverse all sockets, looking for the right physical ID number.
       Once we find it, traverse all that socket's cores looking for
       the right physial ID number.  Once we find it, return the
       physical processor ID number. */
    for (obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_SOCKET, NULL);
         NULL != obj; 
         obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_SOCKET, obj)) {
        if (obj->os_index == (unsigned int) socket) {
            /* Ok, we found the right socket.  Browse its descendants
               looking for the core with the right os_index (don't
               assume all cores are at the same level). */

            obj = dfs_find_os_index(obj, HWLOC_OBJ_CORE, core);
            if (NULL != obj) {
                /* Ok, we found the right core.  Get the cpuset and
                   return the first PU (because hwloc understands
                   hardware threads, of which there might be multiple
                   on this core). */

                hwloc_bitmap_t good;
                good = hwloc_bitmap_alloc();
                if (NULL == good) {
                    return OPAL_ERR_OUT_OF_RESOURCE;
                }
                hwloc_bitmap_and(good, obj->online_cpuset,
                                 obj->allowed_cpuset);
                *processor_id = hwloc_bitmap_first(good);
                hwloc_bitmap_free(good);
                return OPAL_SUCCESS;
            }

            /* If we found the right socket but not the right core, we
               didn't find it. */
            return OPAL_ERR_NOT_FOUND;
        }
    }

    /* If we didn't even find the right socket, we didn't find it. */
    return OPAL_ERR_NOT_FOUND;
}

/*
 * Provides mapping of PHYSICAL processor id -> PHYSICAL socket:core.
 */
static int module_map_to_socket_core(int processor_id, int *socket, int *core)
{
    int ret = OPAL_ERR_NOT_FOUND;
    hwloc_obj_t obj;
    hwloc_topology_t *t = &mca_paffinity_hwloc_component.topology;
    hwloc_bitmap_t good;

    good = hwloc_bitmap_alloc();
    if (NULL == good) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Iterate through every core and find one that contains the
       processor_id.  Then find the corresponding socket. */
    for (obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_CORE, NULL);
         NULL != obj; 
         obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_CORE, obj)) {
        hwloc_bitmap_and(good, obj->online_cpuset, 
                         obj->allowed_cpuset);

        /* Does this core contain the processor_id in question? */
        if (hwloc_bitmap_isset(good, processor_id)) {
            *core = obj->os_index;

            /* Go upward from the core object until we find its parent
               socket. */
            while (HWLOC_OBJ_SOCKET != obj->type) {
                if (NULL == obj->parent) {
                    /* If we get to the root without finding a socket,
                       er..  Hmm.  Error! */
                    ret = OPAL_ERR_NOT_FOUND;
                    goto out;
                }
                obj = obj->parent;
            }
            *socket = obj->os_index;
            ret = OPAL_SUCCESS;
            goto out;
        }
    }

    /* If we didn't even find the right core, we didn't find it.  Fall
       through. */
    ret = OPAL_ERR_NOT_FOUND;

 out:
    hwloc_bitmap_free(good);
    return ret;
}

/*
 * Provides number of LOGICAL processors in a host.  Since paffinity
 * does not currently understand hardware threads, we interpret
 * "processors" to mean "cores".
 */
static int module_get_processor_info(int *num_processors)
{
    hwloc_topology_t *t = &mca_paffinity_hwloc_component.topology;

    /* Try the simple hwloc_get_nbobjs_by_type() first.  If we get -1,
       go aggregate ourselves (because it means that there are cores
       are multiple levels in the topology). */
    *num_processors = (int) hwloc_get_nbobjs_by_type(*t, HWLOC_OBJ_CORE);
    if (-1 == *num_processors) {
        hwloc_obj_t obj;

        *num_processors = 0;
        for (obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_CORE, NULL);
             NULL != obj; 
             obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_CORE, obj)) {
            if (HWLOC_OBJ_CORE == obj->type) {
                ++*num_processors;
            }
        }
    }

    return OPAL_SUCCESS;
}

/*
 * Provides the number of LOGICAL sockets in a host.
 */
static int module_get_socket_info(int *num_sockets)
{
    hwloc_topology_t *t = &mca_paffinity_hwloc_component.topology;

    /* Try the simple hwloc_get_nbobjs_by_type() first.  If we get -1,
       go aggregate ourselves (because it means that there are cores
       are multiple levels in the topology). */
    *num_sockets = (int) hwloc_get_nbobjs_by_type(*t, HWLOC_OBJ_SOCKET);
    if (-1 == *num_sockets) {
        hwloc_obj_t obj;

        *num_sockets = 0;
        for (obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_SOCKET, NULL);
             NULL != obj; 
             obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_SOCKET, obj)) {
            if (HWLOC_OBJ_CORE == obj->type) {
                ++*num_sockets;
            }
        }
    }

    return OPAL_SUCCESS;
}

/*
 * Provides the number of LOGICAL cores in a PHYSICAL socket. 
 */
static int module_get_core_info(int socket, int *num_cores)
{
    hwloc_obj_t obj;
    hwloc_topology_t *t = &mca_paffinity_hwloc_component.topology;

    /* Traverse all sockets, looking for the right physical ID
       number. */
    for (obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_SOCKET, NULL);
         NULL != obj; 
         obj = hwloc_get_next_obj_by_type(*t, HWLOC_OBJ_SOCKET, obj)) {
        if (obj->os_index == (unsigned int) socket) {
            /* Ok, we found the right socket.  Browse its descendants
               looking for all cores. */
            *num_cores = dfs_count_type(obj, HWLOC_OBJ_CORE);
            return OPAL_SUCCESS;
        }
    }

    /* If we didn't even find the right socket, we didn't find it. */
    return OPAL_ERR_NOT_FOUND;
}

/*
 * Provide the PHYSICAL processor id that corresponds to the given
 * LOGICAL processor id.  Remember: paffinity does not understand
 * hardware threads, so "processor" here [usually] means "core" --
 * except that on some platforms, hwloc won't find any cores; it'll
 * only find PUs (!).  On such platforms, then do the same calculation
 * but with PUs instead of COREs.
 */
static int module_get_physical_processor_id(int logical_processor_id,
                                            int *physical_processor_id)
{
    hwloc_obj_t obj;
    hwloc_bitmap_t good;
    hwloc_topology_t *t = &mca_paffinity_hwloc_component.topology;

    /* hwloc isn't able to find cores on all platforms.  Example:
       PPC64 running RHEL 5.4 (linux kernel 2.6.18) only reports NUMA
       nodes and PU's.  Fine.  So just use PUs in that situation. */
    obj = hwloc_get_obj_by_type(*t, HWLOC_OBJ_CORE, logical_processor_id);
    if (NULL == obj) {
        obj = hwloc_get_obj_by_type(*t, HWLOC_OBJ_PU, logical_processor_id);
        if (NULL == obj) {
            return OPAL_ERR_NOT_FOUND;
        }
    }

    /* Found the right core (or PU).  Now find the processor ID of the
       first PU available in that core. */
    good = hwloc_bitmap_alloc();
    if (NULL == good) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    hwloc_bitmap_and(good, obj->online_cpuset, 
                     obj->allowed_cpuset);
    *physical_processor_id = hwloc_bitmap_first(good);
    hwloc_bitmap_free(good);
    return OPAL_SUCCESS;
}

/*
 * Provide the PHYSICAL socket id that corresponds to the given
 * LOGICAL socket id
 */
static int module_get_physical_socket_id(int logical_socket_id,
                                         int *physical_socket_id)
{
    hwloc_obj_t obj;
    hwloc_topology_t *t = &mca_paffinity_hwloc_component.topology;

    obj = hwloc_get_obj_by_type(*t, HWLOC_OBJ_SOCKET, logical_socket_id);
    if (NULL == obj) {
        return OPAL_ERR_NOT_FOUND;
    }
    *physical_socket_id = obj->os_index;
    return OPAL_SUCCESS;
}

/*
 * Provide the PHYSICAL core id that corresponds to the given LOGICAL
 * core id on the given PHYSICAL socket id
 */
static int module_get_physical_core_id(int physical_socket_id, 
                                       int logical_core_id,
                                       int *physical_core_id)
{
    unsigned count = 0;
    hwloc_obj_t obj;
    hwloc_topology_t *t = &mca_paffinity_hwloc_component.topology;

    obj = hwloc_get_root_obj(*t);
    if (NULL == obj) {
        return OPAL_ERR_NOT_FOUND;
    }
    obj = dfs_find_os_index(obj, HWLOC_OBJ_SOCKET, physical_socket_id);
    if (NULL == obj) {
        return OPAL_ERR_NOT_FOUND;
    }

    /* Note that we can't look at hwloc's logical_index here -- hwloc
       counts logically across *all* cores.  We only want to find the
       Nth logical core under this particular socket. */
    obj = dfs_find_nth_item(obj, HWLOC_OBJ_CORE, &count, logical_core_id);
    if (NULL == obj) {
        return OPAL_ERR_NOT_FOUND;
    }
    *physical_core_id = obj->os_index;
    return OPAL_SUCCESS;
}

