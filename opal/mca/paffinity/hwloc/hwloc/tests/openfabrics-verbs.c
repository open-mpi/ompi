/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <stdio.h>
#include <assert.h>
#include <infiniband/verbs.h>
#include <private/config.h>
#include <hwloc.h>
#include <hwloc/openfabrics-verbs.h>

/* check the ibverbs helpers */

int main(void)
{
  hwloc_topology_t topology;
  struct ibv_device **dev_list, *dev;
  int count, i;

  dev_list = ibv_get_device_list(&count);
  if (!dev_list) {
    fprintf(stderr, "ibv_get_device_list failed\n");
    return 0;
  }
  printf("ibv_get_device_list found %d devices\n", count);

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);

  for(i=0; i<count; i++) {
    hwloc_cpuset_t set;
    dev = dev_list[i];

    set = hwloc_cpuset_alloc();
    hwloc_ibv_get_device_cpuset(topology, dev, set);
    if (!set) {
      printf("failed to get cpuset for device %d (%s)\n",
	     i, ibv_get_device_name(dev));
    } else {
      char *cpuset_string = NULL;
      hwloc_cpuset_asprintf(&cpuset_string, set);
      printf("got cpuset %s for device %d (%s)\n",
	     cpuset_string, i, ibv_get_device_name(dev));
      free(cpuset_string);
      hwloc_cpuset_free(set);
    }
  }

  hwloc_topology_destroy(topology);

  ibv_free_device_list(dev_list);

  return 0;
}
