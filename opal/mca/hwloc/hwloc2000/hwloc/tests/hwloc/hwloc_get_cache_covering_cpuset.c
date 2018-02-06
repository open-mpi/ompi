/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/* check hwloc_get_cache_covering_cpuset() */

#define SYNTHETIC_TOPOLOGY_DESCRIPTION "numa:6 pack:5 l2:4 core:3 pu:2" /* 736bits wide topology */

int main(void)
{
  hwloc_topology_t topology;
  hwloc_obj_t obj, cache;
  hwloc_bitmap_t set;

  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, SYNTHETIC_TOPOLOGY_DESCRIPTION);
  hwloc_topology_load(topology);

  /* check the cache above a given cpu */
#define CPUINDEX 180
  set = hwloc_bitmap_alloc();
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, CPUINDEX);
  assert(obj);
  hwloc_bitmap_or(set, set, obj->cpuset);
  cache = hwloc_get_cache_covering_cpuset(topology, set);
  assert(cache);
  assert(hwloc_obj_type_is_dcache(cache->type));
  assert(cache->logical_index == CPUINDEX/2/3);
  assert(hwloc_obj_is_in_subtree(topology, obj, cache));
  hwloc_bitmap_free(set);

  /* check the cache above two nearby cpus */
#define CPUINDEX1 180
#define CPUINDEX2 183
  set = hwloc_bitmap_alloc();
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, CPUINDEX1);
  assert(obj);
  hwloc_bitmap_or(set, set, obj->cpuset);
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, CPUINDEX2);
  assert(obj);
  hwloc_bitmap_or(set, set, obj->cpuset);
  cache = hwloc_get_cache_covering_cpuset(topology, set);
  assert(cache);
  assert(hwloc_obj_type_is_dcache(cache->type));
  assert(cache->logical_index == CPUINDEX1/2/3);
  assert(cache->logical_index == CPUINDEX2/2/3);
  assert(hwloc_obj_is_in_subtree(topology, obj, cache));
  hwloc_bitmap_free(set);

  /* check no cache above two distant cpus */
#undef CPUINDEX1
#define CPUINDEX1 300
  set = hwloc_bitmap_alloc();
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, CPUINDEX1);
  assert(obj);
  hwloc_bitmap_or(set, set, obj->cpuset);
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, CPUINDEX2);
  assert(obj);
  hwloc_bitmap_or(set, set, obj->cpuset);
  cache = hwloc_get_cache_covering_cpuset(topology, set);
  assert(!cache);
  hwloc_bitmap_free(set);

  /* check no cache above higher level */
  set = hwloc_bitmap_alloc();
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PACKAGE, 0);
  assert(obj);
  hwloc_bitmap_or(set, set, obj->cpuset);
  cache = hwloc_get_cache_covering_cpuset(topology, set);
  assert(!cache);
  hwloc_bitmap_free(set);

  hwloc_topology_destroy(topology);

  return EXIT_SUCCESS;
}
