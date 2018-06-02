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

/* check hwloc_get_shared_cache_covering_obj() */

#define SYNTHETIC_TOPOLOGY_DESCRIPTION_SHARED "numa:1 group:6 pack:5 l2:4 core:3 pu:2" /* 736bits wide topology */
#define SYNTHETIC_TOPOLOGY_DESCRIPTION_NONSHARED "numa:1 group:6 pack:5 l2:4 core:1 pu:2" /* 736bits wide topology */

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
  assert(hwloc_obj_type_is_dcache(cache->type));
  assert(cache->logical_index == CPUINDEX/2/3);
  assert(hwloc_obj_is_in_subtree(topology, obj, cache));

  /* check no shared cache above the L2 cache */
  obj = hwloc_get_obj_by_depth(topology, 3, 0);
  assert(obj);
  cache = hwloc_get_shared_cache_covering_obj(topology, obj);
  assert(!cache);

  /* check no shared cache above the node */
  obj = hwloc_get_obj_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE, 0);
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
  assert(hwloc_obj_type_is_dcache(cache->type));
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
