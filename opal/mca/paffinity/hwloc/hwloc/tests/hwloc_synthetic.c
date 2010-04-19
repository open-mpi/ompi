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

/* check that object userdata is properly initialized */

int main(void)
{
  hwloc_topology_t topology;
  unsigned depth;
  unsigned i,j, width;

  /* check a synthetic topology */
  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "2 3 4 5 6");
  hwloc_topology_load(topology);

  /* internal checks */

  hwloc_topology_check(topology);

  /* local checks */
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 6);

  width = 1;
  for(i=0; i<6; i++) {
    /* check arities */
    assert(hwloc_get_nbobjs_by_depth(topology, i) == width);
    for(j=0; j<width; j++) {
      hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, i, j);
      assert(obj);
      assert(obj->arity == (i<5 ? i+2 : 0));
    }
    width *= i+2;
  }

  hwloc_topology_destroy(topology);

  return 0;
}
