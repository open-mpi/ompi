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
 * check hwloc_get_obj_below_array_by_type()
 */

int
main (void)
{
  hwloc_topology_t topology;
  hwloc_obj_t obj;
  hwloc_obj_type_t typev[4];
  unsigned idxv[4];
  int err;

  err = hwloc_topology_init (&topology);
  if (err)
    return EXIT_FAILURE;

  hwloc_topology_set_synthetic (topology, "node:3 socket:3 core:3 proc:3");

  err = hwloc_topology_load (topology);
  if (err)
    return EXIT_FAILURE;


  /* find the first thread */
  typev[0] = HWLOC_OBJ_NODE;   idxv[0] = 0;
  typev[1] = HWLOC_OBJ_SOCKET; idxv[1] = 0;
  typev[2] = HWLOC_OBJ_CORE;   idxv[2] = 0;
  typev[3] = HWLOC_OBJ_PU;   idxv[3] = 0;
  obj = hwloc_get_obj_below_array_by_type(topology, 4, typev, idxv);
  assert(obj == hwloc_get_obj_by_depth(topology, 4, 0));

  /* find the last core */
  typev[0] = HWLOC_OBJ_NODE;   idxv[0] = 2;
  typev[1] = HWLOC_OBJ_SOCKET; idxv[1] = 2;
  typev[2] = HWLOC_OBJ_CORE;   idxv[2] = 2;
  obj = hwloc_get_obj_below_array_by_type(topology, 3, typev, idxv);
  assert(obj == hwloc_get_obj_by_depth(topology, 3, 26));

  /* misc tests */

  typev[0] = HWLOC_OBJ_SOCKET; idxv[0] = 2;
  obj = hwloc_get_obj_below_array_by_type(topology, 1, typev, idxv);
  assert(obj == hwloc_get_obj_by_depth(topology, 2, 2));

  typev[0] = HWLOC_OBJ_NODE;   idxv[0] = 2;
  typev[1] = HWLOC_OBJ_CORE;   idxv[1] = 2;
  obj = hwloc_get_obj_below_array_by_type(topology, 2, typev, idxv);
  assert(obj == hwloc_get_obj_by_depth(topology, 3, 20));
  /* check that hwloc_get_obj_below_by_type works as well */
  obj = hwloc_get_obj_below_by_type(topology, typev[0], idxv[0], typev[1], idxv[1]);
  assert(obj == hwloc_get_obj_by_depth(topology, 3, 20));

  typev[0] = HWLOC_OBJ_SOCKET; idxv[0] = 1;
  typev[1] = HWLOC_OBJ_PU;   idxv[1] = 1;
  obj = hwloc_get_obj_below_array_by_type(topology, 2, typev, idxv);
  assert(obj == hwloc_get_obj_by_depth(topology, 4, 10));
  /* check that hwloc_get_obj_below_by_type works as well */
  obj = hwloc_get_obj_below_by_type(topology, typev[0], idxv[0], typev[1], idxv[1]);
  assert(obj == hwloc_get_obj_by_depth(topology, 4, 10));


  hwloc_topology_destroy (topology);

  return EXIT_SUCCESS;
}
