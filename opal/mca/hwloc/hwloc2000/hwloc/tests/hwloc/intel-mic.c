/*
 * Copyright © 2013-2018 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <stdio.h>
#include <assert.h>
#include <hwloc.h>
#include <hwloc/intel-mic.h>

int main(void)
{
  hwloc_topology_t topology;
  int i;
  int err;

  hwloc_topology_init(&topology);
  hwloc_topology_set_type_filter(topology, HWLOC_OBJ_PCI_DEVICE, HWLOC_TYPE_FILTER_KEEP_IMPORTANT);
  hwloc_topology_set_type_filter(topology, HWLOC_OBJ_OS_DEVICE, HWLOC_TYPE_FILTER_KEEP_IMPORTANT);
  hwloc_topology_load(topology);

  for(i=0; ; i++) {
    hwloc_bitmap_t set;
    hwloc_obj_t osdev, ancestor;
    const char *value;

    osdev = hwloc_intel_mic_get_device_osdev_by_index(topology, i);
    if (!osdev)
      break;
    assert(osdev);

    ancestor = hwloc_get_non_io_ancestor_obj(topology, osdev);

    printf("found OSDev %s\n", osdev->name);
    err = strncmp(osdev->name, "mic", 3);
    assert(!err);
    assert(atoi(osdev->name+3) == (int) i);

    assert(osdev->attr->osdev.type == HWLOC_OBJ_OSDEV_COPROC);

    value = osdev->subtype;
    assert(value);
    err = strcmp(value, "MIC");
    assert(!err);

    value = hwloc_obj_get_info_by_name(osdev, "MICFamily");
    printf("found MICFamily %s\n", value);
    value = hwloc_obj_get_info_by_name(osdev, "MICSKU");
    printf("found MICSKU %s\n", value);
    value = hwloc_obj_get_info_by_name(osdev, "MICActiveCores");
    printf("found MICActiveCores %s\n", value);
    value = hwloc_obj_get_info_by_name(osdev, "MICMemorySize");
    printf("found MICMemorySize %s\n", value);

    set = hwloc_bitmap_alloc();
    err = hwloc_intel_mic_get_device_cpuset(topology, i, set);
    if (err < 0) {
      printf("failed to get cpuset for device %d\n", i);
    } else {
      char *cpuset_string = NULL;
      hwloc_bitmap_asprintf(&cpuset_string, set);
      printf("got cpuset %s for device %d\n", cpuset_string, i);
      if (hwloc_bitmap_isequal(hwloc_topology_get_complete_cpuset(topology), hwloc_topology_get_topology_cpuset(topology)))
	/* only compare if the topology is complete, otherwise things can be significantly different */
	assert(hwloc_bitmap_isequal(set, ancestor->cpuset));
      free(cpuset_string);
    }
    hwloc_bitmap_free(set);
  }

  hwloc_topology_destroy(topology);

  return 0;
}
