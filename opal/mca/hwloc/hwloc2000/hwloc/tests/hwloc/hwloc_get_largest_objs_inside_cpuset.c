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

/* check hwloc_get_largest_objs_inside_cpuset()
 * and hwloc_get_first_largest_obj_inside_cpuset()
 */

#define SYNTHETIC_TOPOLOGY_DESCRIPTION "numa:6 pack:5 l2:4 core:3 pu:2" /* 736bits wide topology */

#define GIVEN_LARGESPLIT_CPUSET_STRING "8000,,,,,,,,,,,,,,,,,,,,,,1" /* first and last(735th) bit set */
#define GIVEN_TOOLARGE_CPUSET_STRING "10000,,,,,,,,,,,,,,,,,,,,,,0" /* 736th bit is too large for the 720-wide topology */
#define GIVEN_HARD_CPUSET_STRING "07ff,ffffffff,e0000000"

#define OBJ_MAX 16

int main(void)
{
  hwloc_topology_t topology;
  hwloc_obj_t objs[OBJ_MAX];
  hwloc_obj_t obj;
  hwloc_bitmap_t set;
  unsigned pus;
  int ret;

  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, SYNTHETIC_TOPOLOGY_DESCRIPTION);
  hwloc_topology_load(topology);

  pus = hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_PU);

  /* just get the system object */
  obj = hwloc_get_root_obj(topology);
  ret = hwloc_get_largest_objs_inside_cpuset(topology, obj->cpuset, objs, 1);
  assert(ret == 1);
  assert(objs[0] == obj);
  objs[0] = hwloc_get_first_largest_obj_inside_cpuset(topology, obj->cpuset);
  assert(objs[0] == obj);

  /* just get the very last object */
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, pus-1);
  ret = hwloc_get_largest_objs_inside_cpuset(topology, obj->cpuset, objs, 1);
  assert(ret == 1);
  assert(objs[0] == obj);

  /* try an empty one */
  set = hwloc_bitmap_alloc();
  ret = hwloc_get_largest_objs_inside_cpuset(topology, set, objs, 1);
  assert(ret == 0);
  objs[0] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  assert(objs[0] == NULL);
  hwloc_bitmap_free(set);

  /* try an impossible one */
  set = hwloc_bitmap_alloc();
  hwloc_bitmap_sscanf(set, GIVEN_TOOLARGE_CPUSET_STRING);
  ret = hwloc_get_largest_objs_inside_cpuset(topology, set, objs, 1);
  assert(ret == -1);
  objs[0] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  assert(objs[0] == NULL);
  hwloc_bitmap_free(set);

  /* try a harder one with 1 obj instead of 2 needed */
  set = hwloc_bitmap_alloc();
  hwloc_bitmap_sscanf(set, GIVEN_LARGESPLIT_CPUSET_STRING);
  ret = hwloc_get_largest_objs_inside_cpuset(topology, set, objs, 1);
  assert(ret == 1);
  assert(objs[0] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 0));
  objs[0] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  assert(objs[0] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 0));
  /* try a harder one with lots of objs instead of 2 needed */
  ret = hwloc_get_largest_objs_inside_cpuset(topology, set, objs, 2);
  assert(ret == 2);
  assert(objs[0] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 0));
  assert(objs[1] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, pus-1));
  objs[0] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  hwloc_bitmap_andnot(set, set, objs[0]->cpuset);
  objs[1] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  hwloc_bitmap_andnot(set, set, objs[1]->cpuset);
  objs[2] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  assert(objs[0] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 0));
  assert(objs[1] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, pus-1));
  assert(objs[2] == NULL);
  assert(hwloc_bitmap_iszero(set));
  hwloc_bitmap_free(set);

  /* try a very hard one */
  set = hwloc_bitmap_alloc();
  hwloc_bitmap_sscanf(set, GIVEN_HARD_CPUSET_STRING);
  ret = hwloc_get_largest_objs_inside_cpuset(topology, set, objs, OBJ_MAX);
  assert(objs[0] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 29));
  assert(objs[1] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_L2CACHE, 5));
  assert(objs[2] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_L2CACHE, 6));
  assert(objs[3] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_L2CACHE, 7));
  assert(objs[4] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_PACKAGE, 2));
  assert(objs[5] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_CORE, 36));
  assert(objs[6] == hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 74));
  hwloc_bitmap_free(set);

  hwloc_topology_destroy(topology);

  return EXIT_SUCCESS;
}
