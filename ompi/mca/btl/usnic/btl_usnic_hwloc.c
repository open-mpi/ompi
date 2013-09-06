/*
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * This file is only compiled (via AM_CONDITIONAL) if OPAL_HAVE_HWLOC
 * is set.
 */

#include "ompi_config.h"

#include <infiniband/verbs.h>

/* Define this before including hwloc.h so that we also get the hwloc
   verbs helper header file, too.  We have to do this level of
   indirection because the hwloc subsystem is a component -- we don't
   know its exact path.  We have to rely on the framework header files
   to find the right hwloc verbs helper file for us. */
#define OPAL_HWLOC_WANT_VERBS_HELPER 1
#include "opal/mca/hwloc/hwloc.h"

#include "ompi/constants.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/common/verbs/common_verbs.h"

#include "btl_usnic_hwloc.h"

/*
 * Local variables
 */
static hwloc_obj_t my_numa_node = NULL;
static int num_numa_nodes = 0;
static const struct hwloc_distances_s *matrix = NULL;

/*
 * Get the hwloc distance matrix (if we don't already have it).
 *
 * Note that the matrix data structure belongs to hwloc; we are not
 * responsibile for freeing it.
 */
static int get_distance_matrix(void)
{
    if (NULL == matrix) {
        matrix = hwloc_get_whole_distance_matrix_by_type(opal_hwloc_topology,
                                                         HWLOC_OBJ_NODE);
    }

    return (NULL == matrix) ? OMPI_ERROR : OMPI_SUCCESS;
}

/*
 * Find the NUMA node that covers a given cpuset
 */
static hwloc_obj_t find_numa_node(hwloc_bitmap_t cpuset)
{
    hwloc_obj_t obj;

    obj =
        hwloc_get_first_largest_obj_inside_cpuset(opal_hwloc_topology, cpuset);

    /* Go upwards until we hit the NUMA node or run out of parents */
    while (obj->type > HWLOC_OBJ_NODE &&
           NULL != obj->parent) {
        obj = obj->parent;
    }

    /* Make sure we ended up on the NUMA node */
    if (obj->type != HWLOC_OBJ_NODE) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic:filter_numa: could not find NUMA node where this process is bound; filtering by NUMA distance not possible");
        return NULL;
    }

    /* Finally, make sure that our cpuset doesn't span more than 1
       NUMA node */
    if (hwloc_get_nbobjs_inside_cpuset_by_type(opal_hwloc_topology,
                                               cpuset, HWLOC_OBJ_NODE) > 1) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic:filter_numa: this process is bound to more than 1 NUMA node; filtering by NUMA distance not possible");
        return NULL;
    }

    return obj;
}

/*
 * Find my NUMA node in the hwloc topology.  This is a Cisco
 * UCS-specific BTL, so I know that I'll always have a NUMA node
 * (i.e., not some unknown server type that may not have or report a
 * NUMA node).
 *
 * Note that the my_numa_node value we find is just a handle; we
 * aren't responsible for freeing it.
 */
static int find_my_numa_node(void)
{
    hwloc_obj_t obj;
    hwloc_bitmap_t cpuset;

    if (NULL != my_numa_node) {
        return OMPI_SUCCESS;
    }

    /* Get this process' binding */
    cpuset = hwloc_bitmap_alloc();
    if (NULL == cpuset) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    if (0 != hwloc_get_cpubind(opal_hwloc_topology, cpuset, 0)) {
        hwloc_bitmap_free(cpuset);
        return OMPI_ERR_NOT_AVAILABLE;
    }

    /* Get the largest object type in the cpuset */
    obj = find_numa_node(cpuset);
    hwloc_bitmap_free(cpuset);
    if (NULL == obj) {
        return OMPI_ERR_NOT_AVAILABLE;
    }

    /* Happiness */
    my_numa_node = obj;
    num_numa_nodes = hwloc_get_nbobjs_by_type(opal_hwloc_topology,
                                              HWLOC_OBJ_NODE);
    return OMPI_SUCCESS;

}

/*
 * Find a NUMA node covering the device associated with this module
 */
static hwloc_obj_t find_device_numa(ompi_btl_usnic_module_t *module)
{
    hwloc_obj_t obj;
    hwloc_bitmap_t cpuset;

    /* Bozo checks */
    assert(NULL != matrix);
    assert(NULL != my_numa_node);

    /* Find the NUMA node for the device */
    cpuset = hwloc_bitmap_alloc();
    if (NULL == cpuset) {
        return NULL;
    }
    if (0 != hwloc_ibv_get_device_cpuset(opal_hwloc_topology,
                                         module->device,
                                         cpuset)) {
        hwloc_bitmap_free(cpuset);
        return NULL;
    }

    obj = find_numa_node(cpuset);
    hwloc_bitmap_free(cpuset);
    return obj;
}

/*
 * Public entry point: find the hwloc NUMA distance from this process
 * to the usnic device in the specified module.
 */
int ompi_btl_usnic_hwloc_distance(ompi_btl_usnic_module_t *module)
{
    int ret;
    hwloc_obj_t dev_numa;

    /* Bozo check */
    assert(NULL != module);

    /* Is this process bound? */
    if (!proc_bound()) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic:filter_numa: not sorting devices by NUMA distance (process not bound)");
        return OMPI_SUCCESS;
    }

    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic:filter_numa: filtering devices by NUMA distance");

    /* Get the hwloc distance matrix for all NUMA nodes */
    if (OMPI_SUCCESS != (ret = get_distance_matrix())) {
        return ret;
    }

    /* Find my NUMA node */
    if (OMPI_SUCCESS != (ret = find_my_numa_node())) {
        return ret;
    }
    /* If my_numa_node is still NULL, that means we span more than 1
       NUMA node.  So... no sorting/pruning for you! */
    if (NULL == my_numa_node) {
        return OMPI_SUCCESS;
    }

    /* Find the NUMA node covering this module's device */
    dev_numa = find_device_numa(module);

    /* Lookup the distance between my NUMA node and the NUMA node of
       the device */
    if (NULL != dev_numa) {
        module->numa_distance = 
            matrix->latency[dev_numa->logical_index * num_numa_nodes +
                            my_numa_node->logical_index];

        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic:filter_numa: %s is distance %d from me",
                            ibv_get_device_name(module->device),
                            module->numa_distance);
    }

    return OMPI_SUCCESS;
}
