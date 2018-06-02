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

  hwloc_topology_set_synthetic (topology, "numa:1 pack:3 l2:3 core:3 pu:3");

  err = hwloc_topology_load (topology);
  if (err)
    return EXIT_FAILURE;


  /* find the first thread */
  typev[0] = HWLOC_OBJ_PACKAGE;   idxv[0] = 0;
  typev[1] = HWLOC_OBJ_L2CACHE; idxv[1] = 0;
  typev[2] = HWLOC_OBJ_CORE;   idxv[2] = 0;
  typev[3] = HWLOC_OBJ_PU;   idxv[3] = 0;
  obj = hwloc_get_obj_below_array_by_type(topology, 4, typev, idxv);
  assert(obj == hwloc_get_obj_by_depth(topology, 4, 0));

  /* find the last core */
  typev[0] = HWLOC_OBJ_PACKAGE;   idxv[0] = 2;
  typev[1] = HWLOC_OBJ_L2CACHE; idxv[1] = 2;
  typev[2] = HWLOC_OBJ_CORE;   idxv[2] = 2;
  obj = hwloc_get_obj_below_array_by_type(topology, 3, typev, idxv);
  assert(obj == hwloc_get_obj_by_depth(topology, 3, 26));

  /* misc tests */

  typev[0] = HWLOC_OBJ_L2CACHE; idxv[0] = 2;
  obj = hwloc_get_obj_below_array_by_type(topology, 1, typev, idxv);
  assert(obj == hwloc_get_obj_by_depth(topology, 2, 2));

  typev[0] = HWLOC_OBJ_PACKAGE;   idxv[0] = 2;
  typev[1] = HWLOC_OBJ_CORE;   idxv[1] = 2;
  obj = hwloc_get_obj_below_array_by_type(topology, 2, typev, idxv);
  assert(obj == hwloc_get_obj_by_depth(topology, 3, 20));
  /* check that hwloc_get_obj_below_by_type works as well */
  obj = hwloc_get_obj_below_by_type(topology, typev[0], idxv[0], typev[1], idxv[1]);
  assert(obj == hwloc_get_obj_by_depth(topology, 3, 20));

  typev[0] = HWLOC_OBJ_L2CACHE; idxv[0] = 1;
  typev[1] = HWLOC_OBJ_PU;   idxv[1] = 1;
  obj = hwloc_get_obj_below_array_by_type(topology, 2, typev, idxv);
  assert(obj == hwloc_get_obj_by_depth(topology, 4, 10));
  /* check that hwloc_get_obj_below_by_type works as well */
  obj = hwloc_get_obj_below_by_type(topology, typev[0], idxv[0], typev[1], idxv[1]);
  assert(obj == hwloc_get_obj_by_depth(topology, 4, 10));


  hwloc_topology_destroy (topology);

  return EXIT_SUCCESS;
}
