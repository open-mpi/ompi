/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_COMMON_OFI_H
#define OPAL_MCA_COMMON_OFI_H
#include <rdma/fabric.h>
#include "opal/mca/hwloc/base/base.h"
#include "ompi/proc/proc.h"

/*
 * Initial size of device list when discovering devices for a provider
 * set to 64 to account for a system with 4 NIC that support 16 protocols each
 */
#define INITIAL_DEV_LIMIT 64

OPAL_DECLSPEC int mca_common_ofi_register_mca_variables(void);

/*
 * Takes a device BDF and returns it's NUMA node's logical index.
 * If no BDF is available, it returns OPAL_ERROR.
 */
static int
get_numanode_from_bus(hwloc_topology_t topology, unsigned int domain,
                       unsigned int bus, unsigned int dev, unsigned int func)
{
    hwloc_obj_t obj = NULL;

    if (NULL == topology)
        return OPAL_ERROR;

    /* get the pci device from bdf */
    obj = hwloc_get_pcidev_by_busid(topology, domain, bus, dev, func);
    if (NULL == obj)
        return OPAL_ERROR;

    /* get the package that contains this pci device */
    obj = hwloc_get_ancestor_obj_by_type(topology, HWLOC_OBJ_PACKAGE, obj);
    if (NULL == obj)
        return OPAL_ERROR;

    /* return the NUMA node's logical index under this package */
    return obj->memory_first_child->logical_index;
}

/*
 * Takes a hardware topology and finds the NUMA node that the current process is on.
 * If the process is not bound, returns OPAL_ERROR
 */
static int
get_process_numanode(hwloc_topology_t topology)
{
    int ret, logical_index = OPAL_ERROR;
    hwloc_bitmap_t cpuset;
    hwloc_obj_t obj = NULL;

    if (NULL == topology)
        return OPAL_ERROR;

    /* allocate memory for cpuset */
    cpuset = hwloc_bitmap_alloc();
    if (NULL == cpuset)
        return OPAL_ERROR;

    /* fill cpuset with the collection of cpu cores that the process runs on */
    ret = hwloc_get_cpubind(topology, cpuset, HWLOC_CPUBIND_PROCESS);
    if (0 > ret)
         goto error;

    /* get the package that contains the cpuset */
    obj = hwloc_get_first_largest_obj_inside_cpuset(topology, cpuset);
    if (NULL == obj)
        goto error;

    logical_index = obj->memory_first_child->logical_index;

error:
    hwloc_bitmap_free(cpuset);
    return logical_index;
}

/*
 * Takes an initially selected provider and a device exclude list
 * and checkes for devices on closer NUMA nodes, otherwise returns
 * the original provider.
 */
struct fi_info*
mca_common_ofi_select_device(struct fi_info *selected_provider)
{
    struct fi_info *device = selected_provider, *current_device = selected_provider;
    struct fi_info **device_table;
    struct fi_pci_attr pci;
    int numanode = OPAL_ERROR, proc_numanode = OPAL_ERROR;
    int ret;
    unsigned int num_device = 0, device_limit = INITIAL_DEV_LIMIT;
    bool node_found = false, same_node = false;

    ret = opal_hwloc_base_get_topology();
    if (0 > ret) {
        /* device selection can continue but there is no guarantee of locality */
        opal_output(0, "%s:%d:Failed to initialize topology\n", __FILE__, __LINE__);
    }

    /* allocate memory for device table */
    device_table = calloc(device_limit, sizeof(struct fi_info*));
    if (NULL == device_table) {
        opal_output(0, "%s:%d:Failed to allocate memory for device table\n", __FILE__, __LINE__);
        return NULL;
    }

    proc_numanode = get_process_numanode(opal_hwloc_topology);
    current_device = device;

    /* cycle through remaining fi_info objects, looking for like providers */
    while (NULL != current_device) {
        if (!strcmp(current_device->fabric_attr->prov_name, device->fabric_attr->prov_name)) {

            /* increases device table capacity if the table fills up */
            if (OPAL_UNLIKELY(num_device > device_limit - 1)) {
                device_limit = device_limit * 2;
                device_table = realloc(device_table, device_limit);
                if (NULL == device_table) {
                    opal_output(0, "%s:%d:Failed to reallocate memory for device table\n",
                                __FILE__, __LINE__);
                    goto err_free_table;
                }
            }

            if (NULL != current_device->nic) {
                pci = current_device->nic->bus_attr->attr.pci;
                numanode = get_numanode_from_bus(opal_hwloc_topology, pci.domain_id, pci.bus_id,
                                                 pci.device_id, pci.function_id);
            }

            /* If the numanode of the device and the proc match add it to the
             * list
             */
            same_node = (proc_numanode == numanode && numanode != OPAL_ERROR);

            if (same_node) {
                /* device is on the same NUMA node as the process */
                if (!node_found) {
                    /* No previous matches, reset previous entries */
                    num_device = 0;
                    node_found = true;
                }
                device_table[num_device] = current_device;
                num_device++;
            }

            if (!node_found) {
                /* device is not on the same NUMA node, but no previous match
                 * found, add it to the list
                 */
                device_table[num_device] = current_device;
                num_device++;
            }
        }
        current_device = current_device->next;
    }

    /* select device from rank % number of devices */
    if (num_device > 0) {
        device = device_table[opal_process_info.my_local_rank % num_device];
    }

err_free_table:
    free(device_table);
    return device;
}

#endif /* OPAL_MCA_COMMON_OFI_H */
