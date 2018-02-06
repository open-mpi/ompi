/*
 * Copyright © 2012-2018 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <stdio.h>
#include <assert.h>
#ifdef __APPLE__
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif
#include <hwloc.h>
#include <hwloc/opencl.h>

/* check the OpenCL helpers */

int main(void)
{
  hwloc_topology_t topology;
  cl_int clret;
  cl_platform_id *platform_ids;
  unsigned nrp, nrd, count, i, j;
  int err;

  hwloc_topology_init(&topology);
  hwloc_topology_set_type_filter(topology, HWLOC_OBJ_PCI_DEVICE, HWLOC_TYPE_FILTER_KEEP_IMPORTANT);
  hwloc_topology_set_type_filter(topology, HWLOC_OBJ_OS_DEVICE, HWLOC_TYPE_FILTER_KEEP_IMPORTANT);
  hwloc_topology_load(topology);

  clret = clGetPlatformIDs(0, NULL, &nrp);
  if (CL_SUCCESS != clret || !nrp)
    return 0;
  platform_ids = malloc(nrp * sizeof(*platform_ids));
  if (!platform_ids)
    return 0;
  clret = clGetPlatformIDs(nrp, platform_ids, &nrp);
  if (CL_SUCCESS != clret || !nrp)
    return 0;

  count = 0;
  for(i=0; i<nrp; i++) {
    cl_device_id *device_ids;

    clret = clGetDeviceIDs(platform_ids[i], CL_DEVICE_TYPE_ALL, 0, NULL, &nrd);
    if (CL_SUCCESS != clret || !nrd)
      continue;
    device_ids = malloc(nrd * sizeof(*device_ids));
    if (!device_ids)
      continue;
    clret = clGetDeviceIDs(platform_ids[i], CL_DEVICE_TYPE_ALL, nrd, device_ids, &nrd);
    if (CL_SUCCESS != clret || !nrd)
      continue;

    for(j=0; j<nrd; j++) {
      hwloc_bitmap_t set;
      hwloc_obj_t osdev, osdev2, ancestor;
      const char *value;
      unsigned p, d;

      osdev = hwloc_opencl_get_device_osdev_by_index(topology, i, j);
      /* we currently insert all OpenCL devices, except CPU devices */
      if (!osdev) {
	cl_device_type type;
	clGetDeviceInfo(device_ids[j], CL_DEVICE_TYPE, sizeof(type), &type, NULL);
	assert(type == CL_DEVICE_TYPE_CPU);
	continue;
      }

      /* try to get it from PCI locality (only works with AMD extensions) */
      osdev2 = hwloc_opencl_get_device_osdev(topology, device_ids[j]);
      if (osdev2) {
        assert(osdev == osdev2);
      }

      ancestor = hwloc_get_non_io_ancestor_obj(topology, osdev);

      set = hwloc_bitmap_alloc();
      err = hwloc_opencl_get_device_cpuset(topology, device_ids[j], set);
      if (err < 0) {
	printf("no cpuset for platform %u device %u\n", i, j);
      } else {
	char *cpuset_string = NULL;
	hwloc_bitmap_asprintf(&cpuset_string, set);
	printf("got cpuset %s for platform %u device %u\n", cpuset_string, i, j);
	free(cpuset_string);
	if (hwloc_bitmap_isequal(hwloc_topology_get_complete_cpuset(topology), hwloc_topology_get_topology_cpuset(topology)))
	  /* only compare if the topology is complete, otherwise things can be significantly different */
	  assert(hwloc_bitmap_isequal(set, ancestor->cpuset));
      }
      hwloc_bitmap_free(set);

      printf("found OSDev %s\n", osdev->name);
      err = sscanf(osdev->name, "opencl%ud%u", &p, &d);
      assert(err == 2);
      assert(p == i);
      assert(d == j);

      value = hwloc_obj_get_info_by_name(osdev, "Backend");
      err = strcmp(value, "OpenCL");
      assert(!err);

      assert(osdev->attr->osdev.type == HWLOC_OBJ_OSDEV_COPROC);

      value = osdev->subtype;
      assert(value);
      err = strcmp(value, "OpenCL");
      assert(!err);

      value = hwloc_obj_get_info_by_name(osdev, "GPUModel");
      printf("found OSDev model %s\n", value);

      count++;
    }
  }

  hwloc_topology_destroy(topology);

  return 0;
}
