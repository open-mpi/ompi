/*
 * Copyright © 2010-2014 Inria.  All rights reserved.
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/** \file
 * \brief Macros to help interaction between hwloc and Myrinet Express.
 *
 * Applications that use both hwloc and Myrinet Express verbs may want to
 * include this file so as to get topology information for Myrinet hardware.
 *
 */

#ifndef HWLOC_MYRIEXPRESS_H
#define HWLOC_MYRIEXPRESS_H

#include <hwloc.h>
#include <hwloc/autogen/config.h>

#include <myriexpress.h>


#ifdef __cplusplus
extern "C" {
#endif


/** \defgroup hwlocality_myriexpress Interoperability with Myrinet Express
 *
 * This interface offers ways to retrieve topology information about
 * Myrinet Express hardware.
 *
 * @{
 */

/** \brief Get the CPU set of logical processors that are physically
 * close the MX board \p id.
 *
 * Return the CPU set describing the locality of the Myrinet Express
 * board whose index is \p id.
 *
 * Topology \p topology and device \p id must match the local machine.
 * I/O devices detection is not needed in the topology.
 *
 * The function only returns the locality of the device.
 * No additional information about the device is available.
 */
static __hwloc_inline int
hwloc_mx_board_get_device_cpuset(hwloc_topology_t topology,
				 unsigned id, hwloc_cpuset_t set)
{
  uint32_t in, out;

  if (!hwloc_topology_is_thissystem(topology)) {
    errno = EINVAL;
    return -1;
  }

  in = id;
  if (mx_get_info(NULL, MX_NUMA_NODE, &in, sizeof(in), &out, sizeof(out)) != MX_SUCCESS) {
    errno = EINVAL;
    return -1;
  }

  if (out != (uint32_t) -1) {
    hwloc_obj_t obj = NULL;
    while ((obj = hwloc_get_next_obj_by_type(topology, HWLOC_OBJ_NUMANODE, obj)) != NULL)
      if (obj->os_index == out) {
	hwloc_bitmap_copy(set, obj->cpuset);
	goto out;
      }
  }
  /* fallback to the full topology cpuset */
  hwloc_bitmap_copy(set, hwloc_topology_get_complete_cpuset(topology));

 out:
  return 0;
}

/** \brief Get the CPU set of logical processors that are physically
 * close the MX endpoint \p endpoint.
 *
 * Return the CPU set describing the locality of the Myrinet Express
 * board that runs the MX endpoint \p endpoint.
 *
 * Topology \p topology and device \p id must match the local machine.
 * I/O devices detection is not needed in the topology.
 *
 * The function only returns the locality of the endpoint.
 * No additional information about the endpoint or device is available.
 */
static __hwloc_inline int
hwloc_mx_endpoint_get_device_cpuset(hwloc_topology_t topology,
				    mx_endpoint_t endpoint, hwloc_cpuset_t set)
{
  uint64_t nid;
  uint32_t nindex, eid;
  mx_endpoint_addr_t eaddr;

  if (mx_get_endpoint_addr(endpoint, &eaddr) != MX_SUCCESS) {
    errno = EINVAL;
    return -1;
  }

  if (mx_decompose_endpoint_addr(eaddr, &nid, &eid) != MX_SUCCESS) {
    errno = EINVAL;
    return -1;
  }

  if (mx_nic_id_to_board_number(nid, &nindex) != MX_SUCCESS) {
    errno = EINVAL;
    return -1;
  }

  return hwloc_mx_board_get_device_cpuset(topology, nindex, set);
}

/** @} */


#ifdef __cplusplus
} /* extern "C" */
#endif


#endif /* HWLOC_MYRIEXPRESS_H */
