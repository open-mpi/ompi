/*
 * Copyright © 2010-2012 inria.  All rights reserved.
 * Copyright © 2010-2011 Université Bordeaux 1
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/** \file
 * \brief Macros to help interaction between hwloc and the CUDA Driver API.
 *
 * Applications that use both hwloc and the CUDA Driver API may want to
 * include this file so as to get topology information for CUDA devices.
 *
 */

#ifndef HWLOC_CUDA_H
#define HWLOC_CUDA_H

#include <hwloc.h>
#include <hwloc/autogen/config.h>
#include <hwloc/linux.h>
#include <hwloc/helper.h>

#include <cuda.h>


#ifdef __cplusplus
extern "C" {
#endif


/** \defgroup hwlocality_cuda CUDA Driver API Specific Functions
 * @{
 */

/** \brief Return the domain, bus and device IDs of device \p cudevice.
 */
static __hwloc_inline int
hwloc_cuda_get_device_pci_ids(hwloc_topology_t topology __hwloc_attribute_unused,
			       CUdevice cudevice, int *domain, int *bus, int *dev)
{
  CUresult cres;

#if CUDA_VERSION >= 4000
  cres = cuDeviceGetAttribute(domain, CU_DEVICE_ATTRIBUTE_PCI_DOMAIN_ID, cudevice);
  if (cres != CUDA_SUCCESS) {
    errno = ENOSYS;
    return -1;
  }
#else
  *domain = 0;
#endif
  cres = cuDeviceGetAttribute(bus, CU_DEVICE_ATTRIBUTE_PCI_BUS_ID, cudevice);
  if (cres != CUDA_SUCCESS) {
    errno = ENOSYS;
    return -1;
  }
  cres = cuDeviceGetAttribute(dev, CU_DEVICE_ATTRIBUTE_PCI_DEVICE_ID, cudevice);
  if (cres != CUDA_SUCCESS) {
    errno = ENOSYS;
    return -1;
  }

  return 0;
}

/** \brief Get the CPU set of logical processors that are physically
 * close to device \p cudevice.
 *
 * For the given CUDA Driver API device \p cudevice, read the corresponding
 * kernel-provided cpumap file and return the corresponding CPU set.
 * This function is currently only implemented in a meaningful way for
 * Linux; other systems will simply get a full cpuset.
 *
 * Topology \p topology must match the current machine.
 */
static __hwloc_inline int
hwloc_cuda_get_device_cpuset(hwloc_topology_t topology __hwloc_attribute_unused,
			     CUdevice cudevice, hwloc_cpuset_t set)
{
#ifdef HWLOC_LINUX_SYS
  /* If we're on Linux, use the sysfs mechanism to get the local cpus */
#define HWLOC_CUDA_DEVICE_SYSFS_PATH_MAX 128
  char path[HWLOC_CUDA_DEVICE_SYSFS_PATH_MAX];
  FILE *sysfile = NULL;
  int domainid, busid, deviceid;

  if (hwloc_cuda_get_device_pci_ids(topology, cudevice, &domainid, &busid, &deviceid))
    return -1;

  if (!hwloc_topology_is_thissystem(topology)) {
    errno = EINVAL;
    return -1;
  }

  sprintf(path, "/sys/bus/pci/devices/%04x:%02x:%02x.0/local_cpus", domainid, busid, deviceid);
  sysfile = fopen(path, "r");
  if (!sysfile)
    return -1;

  hwloc_linux_parse_cpumap_file(sysfile, set);
  if (hwloc_bitmap_iszero(set))
    hwloc_bitmap_copy(set, hwloc_topology_get_complete_cpuset(topology));

  fclose(sysfile);
#else
  /* Non-Linux systems simply get a full cpuset */
  hwloc_bitmap_copy(set, hwloc_topology_get_complete_cpuset(topology));
#endif
  return 0;
}

/** \brief Get the hwloc object for the PCI device corresponding
 * to device \p cudevice.
 *
 * For the given CUDA Runtime API device \p cudevice, return the hwloc PCI
 * object containing the device. Returns NULL if there is none.
 *
 * IO devices detection must be enabled in topology \p topology.
 */
static __hwloc_inline hwloc_obj_t
hwloc_cuda_get_device_pcidev(hwloc_topology_t topology, CUdevice cudevice)
{
  int domain, bus, dev;

  if (hwloc_cuda_get_device_pci_ids(topology, cudevice, &domain, &bus, &dev))
    return NULL;

  return hwloc_get_pcidev_by_busid(topology, domain, bus, dev, 0);
}

/** @} */


#ifdef __cplusplus
} /* extern "C" */
#endif


#endif /* HWLOC_CUDA_H */
