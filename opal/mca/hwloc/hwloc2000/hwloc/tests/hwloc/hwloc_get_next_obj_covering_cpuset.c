/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009-2010 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/*
 * check hwloc_get_next_obj_covering_cpuset*()
 */

int
main (void)
{
  hwloc_topology_t topology;
  hwloc_bitmap_t set;
  hwloc_obj_t obj;
  int depth;
  int err;

  set = hwloc_bitmap_alloc();



  err = hwloc_topology_init (&topology);
  if (err)
    return EXIT_FAILURE;
  hwloc_topology_set_synthetic (topology, "pack:8 cores:2 1");
  err = hwloc_topology_load (topology);
  if (err)
    return EXIT_FAILURE;

  hwloc_bitmap_sscanf(set, "00008f18");

  obj = hwloc_get_next_obj_covering_cpuset_by_type(topology, set, HWLOC_OBJ_PACKAGE, NULL);
  assert(obj == hwloc_get_obj_by_depth(topology, 1, 1));
  obj = hwloc_get_next_obj_covering_cpuset_by_type(topology, set, HWLOC_OBJ_PACKAGE, obj);
  assert(obj == hwloc_get_obj_by_depth(topology, 1, 2));
  obj = hwloc_get_next_obj_covering_cpuset_by_type(topology, set, HWLOC_OBJ_PACKAGE, obj);
  assert(obj == hwloc_get_obj_by_depth(topology, 1, 4));
  obj = hwloc_get_next_obj_covering_cpuset_by_type(topology, set, HWLOC_OBJ_PACKAGE, obj);
  assert(obj == hwloc_get_obj_by_depth(topology, 1, 5));
  obj = hwloc_get_next_obj_covering_cpuset_by_type(topology, set, HWLOC_OBJ_PACKAGE, obj);
  assert(obj == hwloc_get_obj_by_depth(topology, 1, 7));
  obj = hwloc_get_next_obj_covering_cpuset_by_type(topology, set, HWLOC_OBJ_PACKAGE, obj);
  assert(!obj);

  hwloc_topology_destroy (topology);



  err = hwloc_topology_init (&topology);
  if (err)
    return EXIT_FAILURE;
  hwloc_topology_set_synthetic (topology, "nodes:2 pack:5 cores:3 4");
  err = hwloc_topology_load (topology);
  if (err)
    return EXIT_FAILURE;

  hwloc_bitmap_sscanf(set, "0ff08000");

  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_PACKAGE);
  assert(depth == 2);
  obj = hwloc_get_next_obj_covering_cpuset_by_depth(topology, set, depth, NULL);
  assert(obj == hwloc_get_obj_by_depth(topology, depth, 1));
  obj = hwloc_get_next_obj_covering_cpuset_by_depth(topology, set, depth, obj);
  assert(obj == hwloc_get_obj_by_depth(topology, depth, 2));
  obj = hwloc_get_next_obj_covering_cpuset_by_depth(topology, set, depth, obj);
  assert(!obj);

  hwloc_topology_destroy (topology);



  hwloc_bitmap_free(set);

  return EXIT_SUCCESS;
}
