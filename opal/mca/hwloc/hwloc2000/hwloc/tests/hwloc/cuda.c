/*
 * Copyright © 2010-2018 Inria.  All rights reserved.
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <stdio.h>
#include <assert.h>
#include <cuda.h>
#include <hwloc.h>
#include <hwloc/cuda.h>

/* check the CUDA Driver API helpers */

int main(void)
{
  hwloc_topology_t topology;
  CUresult cres;
  CUdevice device;
  int count, i;
  int err;

  cres = cuInit(0);
  if (cres != CUDA_SUCCESS) {
    printf("cuInit failed %d\n", cres);
    return 0;
  }

  cres = cuDeviceGetCount(&count);
  if (cres != CUDA_SUCCESS) {
    printf("cuDeviceGetCount failed %d\n", cres);
    return 0;
  }
  printf("cuDeviceGetCount found %d devices\n", count);

  hwloc_topology_init(&topology);
  hwloc_topology_set_io_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_IMPORTANT);
  hwloc_topology_load(topology);

  for(i=0; i<count; i++) {
    hwloc_bitmap_t set;
    hwloc_obj_t osdev, osdev2, ancestor;
    const char *value;

    cres = cuDeviceGet(&device, i);
    if (cres != CUDA_SUCCESS) {
      printf("failed to get device %d\n", i);
      continue;
    }

    osdev = hwloc_cuda_get_device_osdev(topology, device);
    assert(osdev);
    osdev2 = hwloc_cuda_get_device_osdev_by_index(topology, i);
    assert(osdev == osdev2);

    ancestor = hwloc_get_non_io_ancestor_obj(topology, osdev);

    printf("found OSDev %s\n", osdev->name);
    err = strncmp(osdev->name, "cuda", 4);
    assert(!err);
    assert(atoi(osdev->name+4) == (int) i);

    value = hwloc_obj_get_info_by_name(osdev, "Backend");
    err = strcmp(value, "CUDA");
    assert(!err);

    assert(osdev->attr->osdev.type == HWLOC_OBJ_OSDEV_COPROC);

    value = osdev->subtype;
    assert(value);
    err = strcmp(value, "CUDA");
    assert(!err);

    value = hwloc_obj_get_info_by_name(osdev, "GPUModel");
    printf("found OSDev model %s\n", value);

    set = hwloc_bitmap_alloc();
    err = hwloc_cuda_get_device_cpuset(topology, device, set);
    if (err < 0) {
      printf("failed to get cpuset for device %d\n", i);
    } else {
      char *cpuset_string = NULL;
      hwloc_bitmap_asprintf(&cpuset_string, set);
      printf("got cpuset %s for device %d\n", cpuset_string, i);
      if (hwloc_bitmap_isequal(hwloc_topology_get_complete_cpuset(topology), hwloc_topology_get_topology_cpuset(topology)))
	/* only compare if the topology is complete, otherwise things can be significantly different */
	assert(hwloc_bitmap_isincluded(ancestor->cpuset, set));
      free(cpuset_string);
    }
    hwloc_bitmap_free(set);
  }

  hwloc_topology_destroy(topology);

  return 0;
}
