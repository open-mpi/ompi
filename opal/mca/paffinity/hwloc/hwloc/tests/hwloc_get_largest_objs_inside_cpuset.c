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

/* check hwloc_get_largest_objs_inside_cpuset()
 * and hwloc_get_first_largest_obj_inside_cpuset()
 */

#define SYNTHETIC_TOPOLOGY_DESCRIPTION "6 5 4 3 2" /* 736bits wide topology */

#define GIVEN_LARGESPLIT_CPUSET_STRING "8000,,,,,,,,,,,,,,,,,,,,,,1" /* first and last(735th) bit set */
#define GIVEN_TOOLARGE_CPUSET_STRING "10000,,,,,,,,,,,,,,,,,,,,,,0" /* 736th bit is too large for the 720-wide topology */
#define GIVEN_HARD_CPUSET_STRING "07ff,ffffffff,e0000000"

#define OBJ_MAX 16

int main(void)
{
  hwloc_topology_t topology;
  unsigned depth;
  hwloc_obj_t objs[OBJ_MAX];
  hwloc_obj_t obj;
  hwloc_cpuset_t set;
  int ret;

  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, SYNTHETIC_TOPOLOGY_DESCRIPTION);
  hwloc_topology_load(topology);
  depth = hwloc_topology_get_depth(topology);

  /* just get the system object */
  obj = hwloc_get_root_obj(topology);
  ret = hwloc_get_largest_objs_inside_cpuset(topology, obj->cpuset, objs, 1);
  assert(ret == 1);
  assert(objs[0] == obj);
  objs[0] = hwloc_get_first_largest_obj_inside_cpuset(topology, obj->cpuset);
  assert(objs[0] == obj);

  /* just get the very last object */
  obj = hwloc_get_obj_by_depth(topology, depth-1, hwloc_get_nbobjs_by_depth(topology, depth-1)-1);
  ret = hwloc_get_largest_objs_inside_cpuset(topology, obj->cpuset, objs, 1);
  assert(ret == 1);
  assert(objs[0] == obj);

  /* try an empty one */
  set = hwloc_cpuset_alloc();
  ret = hwloc_get_largest_objs_inside_cpuset(topology, set, objs, 1);
  assert(ret == 0);
  objs[0] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  assert(objs[0] == NULL);
  hwloc_cpuset_free(set);

  /* try an impossible one */
  set = hwloc_cpuset_alloc();
  hwloc_cpuset_from_string(set, GIVEN_TOOLARGE_CPUSET_STRING);
  ret = hwloc_get_largest_objs_inside_cpuset(topology, set, objs, 1);
  assert(ret == -1);
  objs[0] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  assert(objs[0] == NULL);
  hwloc_cpuset_free(set);

  /* try a harder one with 1 obj instead of 2 needed */
  set = hwloc_cpuset_alloc();
  hwloc_cpuset_from_string(set, GIVEN_LARGESPLIT_CPUSET_STRING);
  ret = hwloc_get_largest_objs_inside_cpuset(topology, set, objs, 1);
  assert(ret == 1);
  assert(objs[0] == hwloc_get_obj_by_depth(topology, depth-1, 0));
  objs[0] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  assert(objs[0] == hwloc_get_obj_by_depth(topology, depth-1, 0));
  /* try a harder one with lots of objs instead of 2 needed */
  ret = hwloc_get_largest_objs_inside_cpuset(topology, set, objs, 2);
  assert(ret == 2);
  assert(objs[0] == hwloc_get_obj_by_depth(topology, depth-1, 0));
  assert(objs[1] == hwloc_get_obj_by_depth(topology, depth-1, hwloc_get_nbobjs_by_depth(topology, depth-1)-1));
  objs[0] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  hwloc_cpuset_andnot(set, set, objs[0]->cpuset);
  objs[1] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  hwloc_cpuset_andnot(set, set, objs[1]->cpuset);
  objs[2] = hwloc_get_first_largest_obj_inside_cpuset(topology, set);
  assert(objs[0] == hwloc_get_obj_by_depth(topology, depth-1, 0));
  assert(objs[1] == hwloc_get_obj_by_depth(topology, depth-1, hwloc_get_nbobjs_by_depth(topology, depth-1)-1));
  assert(objs[2] == NULL);
  assert(hwloc_cpuset_iszero(set));
  hwloc_cpuset_free(set);

  /* try a very hard one */
  set = hwloc_cpuset_alloc();
  hwloc_cpuset_from_string(set, GIVEN_HARD_CPUSET_STRING);
  ret = hwloc_get_largest_objs_inside_cpuset(topology, set, objs, OBJ_MAX);
  assert(objs[0] == hwloc_get_obj_by_depth(topology, 5, 29));
  assert(objs[1] == hwloc_get_obj_by_depth(topology, 3, 5));
  assert(objs[2] == hwloc_get_obj_by_depth(topology, 3, 6));
  assert(objs[3] == hwloc_get_obj_by_depth(topology, 3, 7));
  assert(objs[4] == hwloc_get_obj_by_depth(topology, 2, 2));
  assert(objs[5] == hwloc_get_obj_by_depth(topology, 4, 36));
  assert(objs[6] == hwloc_get_obj_by_depth(topology, 5, 74));
  hwloc_cpuset_free(set);

  hwloc_topology_destroy(topology);

  return EXIT_SUCCESS;
}
