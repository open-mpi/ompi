/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

int main(void)
{
  hwloc_topology_t topology;
  hwloc_cpuset_t cpuset;
  hwloc_obj_t obj;

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);
  hwloc_topology_check(topology);
  cpuset = hwloc_cpuset_alloc();
  hwloc_cpuset_set(cpuset, 0);
  obj = hwloc_topology_insert_misc_object_by_cpuset(topology, cpuset, "test");
  hwloc_topology_insert_misc_object_by_parent(topology, obj, "test2");
  hwloc_topology_check(topology);
  hwloc_topology_destroy(topology);

  return 0;
}
