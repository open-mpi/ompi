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

/* check hwloc_get_shared_cache_covering_obj() */

#define SYNTHETIC_TOPOLOGY_DESCRIPTION_SHARED "6 5 4 3 2" /* 736bits wide topology */
#define SYNTHETIC_TOPOLOGY_DESCRIPTION_NONSHARED "6 5 4 1 2" /* 736bits wide topology */

int main(void)
{
  hwloc_topology_t topology;
  hwloc_obj_t obj, cache;

  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, SYNTHETIC_TOPOLOGY_DESCRIPTION_SHARED);
  hwloc_topology_load(topology);

  /* check the cache above a given cpu */
#define CPUINDEX 180
  obj = hwloc_get_obj_by_depth(topology, 5, CPUINDEX);
  assert(obj);
  cache = hwloc_get_shared_cache_covering_obj(topology, obj);
  assert(cache);
  assert(cache->type == HWLOC_OBJ_CACHE);
  assert(cache->logical_index == CPUINDEX/2/3);
  assert(hwloc_obj_is_in_subtree(topology, obj, cache));

  /* check no shared cache above the L2 cache */
  obj = hwloc_get_obj_by_depth(topology, 3, 0);
  assert(obj);
  cache = hwloc_get_shared_cache_covering_obj(topology, obj);
  assert(!cache);

  /* check no shared cache above the node */
  obj = hwloc_get_obj_by_depth(topology, 1, 0);
  assert(obj);
  cache = hwloc_get_shared_cache_covering_obj(topology, obj);
  assert(!cache);

  hwloc_topology_destroy(topology);


  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, SYNTHETIC_TOPOLOGY_DESCRIPTION_NONSHARED);
  hwloc_topology_load(topology);

  /* check the cache above a given cpu */
#define CPUINDEX 180
  obj = hwloc_get_obj_by_depth(topology, 5, CPUINDEX);
  assert(obj);
  cache = hwloc_get_shared_cache_covering_obj(topology, obj);
  assert(cache);
  assert(cache->type == HWLOC_OBJ_CACHE);
  assert(cache->logical_index == CPUINDEX/2/1);
  assert(hwloc_obj_is_in_subtree(topology, obj, cache));

  /* check no shared-cache above the core */
  obj = hwloc_get_obj_by_depth(topology, 4, CPUINDEX/2);
  assert(obj);
  cache = hwloc_get_shared_cache_covering_obj(topology, obj);
  assert(!cache);

  hwloc_topology_destroy(topology);

  return EXIT_SUCCESS;
}
