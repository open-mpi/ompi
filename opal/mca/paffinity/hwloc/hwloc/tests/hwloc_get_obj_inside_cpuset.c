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

/*
 * check hwloc_get_obj_inside_cpuset*()
 */

int
main (void)
{
  hwloc_topology_t topology;
  hwloc_obj_t obj, root;
  int err;

  err = hwloc_topology_init (&topology);
  if (err)
    return EXIT_FAILURE;

  hwloc_topology_set_synthetic (topology, "nodes:2 sockets:3 caches:4 cores:5 6");

  err = hwloc_topology_load (topology);
  if (err)
    return EXIT_FAILURE;

  /* there is no second system object */
  root = hwloc_get_root_obj (topology);
  obj = hwloc_get_obj_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_SYSTEM, 1);
  assert(!obj);

  /* first system object is the top-level object of the topology */
  obj = hwloc_get_obj_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_MACHINE, 0);
  assert(obj == hwloc_get_root_obj(topology));

  /* first next-object object is the top-level object of the topology */
  obj = hwloc_get_next_obj_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_MACHINE, NULL);
  assert(obj == hwloc_get_root_obj(topology));
  /* there is no next object after the system object */
  obj = hwloc_get_next_obj_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_SYSTEM, obj);
  assert(!obj);

  /* check last PU */
  obj = hwloc_get_obj_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_PU, 2*3*4*5*6-1);
  assert(obj == hwloc_get_obj_by_depth(topology, 5, 2*3*4*5*6-1));
  /* there is no next PU after the last one */
  obj = hwloc_get_next_obj_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_PU, obj);
  assert(!obj);


  /* check there are 20 cores inside first socket */
  root = hwloc_get_obj_by_depth(topology, 2, 0);
  assert(hwloc_get_nbobjs_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_CORE) == 20);

  /* check there are 12 caches inside last node */
  root = hwloc_get_obj_by_depth(topology, 1, 1);
  assert(hwloc_get_nbobjs_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_CACHE) == 12);


  /* check first PU of second socket */
  root = hwloc_get_obj_by_depth(topology, 2, 1);
  obj = hwloc_get_obj_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_PU, 0);
  assert(obj == hwloc_get_obj_by_depth(topology, 5, 4*5*6));

  /* check third core of third socket */
  root = hwloc_get_obj_by_depth(topology, 2, 2);
  obj = hwloc_get_obj_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_CORE, 2);
  assert(obj == hwloc_get_obj_by_depth(topology, 4, 2*4*5+2));

  /* check first socket of second node */
  root = hwloc_get_obj_by_depth(topology, 1, 1);
  obj = hwloc_get_obj_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_SOCKET, 0);
  assert(obj == hwloc_get_obj_by_depth(topology, 2, 3));

  /* there is no node inside sockets */
  root = hwloc_get_obj_by_depth(topology, 2, 0);
  obj = hwloc_get_obj_inside_cpuset_by_type(topology, root->cpuset, HWLOC_OBJ_NODE, 0);
  assert(!obj);

  hwloc_topology_destroy (topology);

  return EXIT_SUCCESS;
}
