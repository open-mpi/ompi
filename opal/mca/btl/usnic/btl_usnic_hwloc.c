/*
 * Copyright (c) 2013-2016 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/hwloc/base/base.h"
#include "opal/constants.h"

#if BTL_IN_OPAL
#include "opal/mca/btl/base/base.h"
#else
#include "ompi/mca/btl/base/base.h"
#endif

#include "btl_usnic_hwloc.h"

/*
 * Local variables
 */
static int my_numa_node_index = -1;
static bool numa_node_index_found = false;
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
    hwloc_topology_t topo;

    if (NULL == matrix) {
        if (NULL == (topo = opal_hwloc_base_get_topology())) {
            return OPAL_ERROR;
        }
        matrix = hwloc_get_whole_distance_matrix_by_type(topo, HWLOC_OBJ_NODE);
    }

    return (NULL == matrix) ? OPAL_ERROR : OPAL_SUCCESS;
}

/*
 * Find the NUMA node that covers a given cpuset
 */
static hwloc_obj_t find_numa_node(hwloc_topology_t topo,
                                  hwloc_bitmap_t cpuset)
{
    hwloc_obj_t obj;

    obj =
        hwloc_get_first_largest_obj_inside_cpuset(topo, cpuset);

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
    if (hwloc_get_nbobjs_inside_cpuset_by_type(topo, cpuset, HWLOC_OBJ_NODE) > 1) {
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
static int find_my_numa_node_index(void)
{
    hwloc_obj_t obj;
    hwloc_bitmap_t cpuset;
    hwloc_topology_t topo;

    if (numa_node_index_found) {
        return my_numa_node_index;
    }

    if (NULL == (topo = opal_hwloc_base_get_topology())) {
        return OPAL_ERROR;
    }

    /* Get this process' binding */
    cpuset = hwloc_bitmap_alloc();
    if (NULL == cpuset) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    if (0 != hwloc_get_cpubind(topo, cpuset, 0)) {
        hwloc_bitmap_free(cpuset);
        opal_hwloc_base_free_topology(topo);
        return OPAL_ERR_NOT_AVAILABLE;
    }

    /* Get the largest object type in the cpuset */
    obj = find_numa_node(topo, cpuset);
    hwloc_bitmap_free(cpuset);
    if (NULL == obj) {
        opal_hwloc_base_free_topology(topo);
        return OPAL_ERR_NOT_AVAILABLE;
    }

    /* Happiness */
    my_numa_node_index = obj->logical_index;
    num_numa_nodes = hwloc_get_nbobjs_by_type(topo, HWLOC_OBJ_NODE);
    opal_hwloc_base_free_topology(topo);
    numa_node_index_found = true;
    return my_numa_node_index;

}

/*
 * Find a NUMA node covering the device associated with this module
 */
static hwloc_obj_t find_device_numa(hwloc_topology_t topo,
                                    opal_btl_usnic_module_t *module)
{
    struct fi_usnic_info *uip;
    hwloc_obj_t obj;

    /* Bozo checks */
    assert(NULL != matrix);

    uip = &module->usnic_info;

   /* Look for the IP device name in the hwloc topology (the usnic
       device is simply an alternate API to reach the same device, so
       if we find the IP device name, we've found the usNIC device) */
    obj = NULL;
    while (NULL != (obj = hwloc_get_next_osdev(topo, obj))) {
        assert(HWLOC_OBJ_OS_DEVICE == obj->type);
        if (0 == strcmp(obj->name, uip->ui.v1.ui_ifname)) {
            break;
        }
    }

    /* Did not find it */
    if (NULL == obj) {
        opal_hwloc_base_free_topology(topo);
        return NULL;
    }

    /* Search upwards to find the device's NUMA node */
    /* Go upwards until we hit the NUMA node or run out of parents */
    while (obj->type > HWLOC_OBJ_NODE &&
           NULL != obj->parent) {
        obj = obj->parent;
    }

    /* Make sure we ended up on the NUMA node */
    if (obj->type != HWLOC_OBJ_NODE) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic:filter_numa: could not find NUMA node for %s; filtering by NUMA distance not possible",
                            module->linux_device_name);
        opal_hwloc_base_free_topology(topo);
        return NULL;
    }
    opal_hwloc_base_free_topology(topo);

    return obj;
}

/*
 * Public entry point: find the hwloc NUMA distance from this process
 * to the usnic device in the specified module.
 */
int opal_btl_usnic_hwloc_distance(opal_btl_usnic_module_t *module)
{
    int ret;
    hwloc_obj_t dev_numa;
    hwloc_topology_t topo;

    /* Bozo check */
    assert(NULL != module);

    /* Is this process bound? */
    if (!proc_bound()) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic:filter_numa: not sorting devices by NUMA distance (process not bound)");
        return OPAL_SUCCESS;
    }

    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic:filter_numa: filtering devices by NUMA distance");

    /* Get the hwloc distance matrix for all NUMA nodes */
    if (OPAL_SUCCESS != (ret = get_distance_matrix())) {
        return ret;
    }

    /* Find my NUMA node */
    if (0 > (ret = find_my_numa_node_index())) {
        return ret;
    }

    /* Find the NUMA node covering this module's device */
    if (NULL == (topo = opal_hwloc_base_get_topology())) {
        return OPAL_ERROR;
    }

    dev_numa = find_device_numa(topo, module);

    /* Lookup the distance between my NUMA node and the NUMA node of
       the device */
    if (NULL != dev_numa) {
        module->numa_distance =
            matrix->latency[dev_numa->logical_index * num_numa_nodes + my_numa_node_index];

        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic:filter_numa: %s is distance %d from me",
                            module->linux_device_name,
                            module->numa_distance);
    }
    opal_hwloc_base_free_topology(topo);
    return OPAL_SUCCESS;
}
