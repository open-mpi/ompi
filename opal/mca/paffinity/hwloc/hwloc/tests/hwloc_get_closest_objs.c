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
 * check hwloc_get_closest_objs()
 *
 * - get the last object of the last level
 * - get all closest objects
 * - get the common ancestor of last level and its less close object.
 * - check that the ancestor is the system level
 */

int
main (void)
{
  hwloc_topology_t topology;
  unsigned depth;
  hwloc_obj_t last;
  hwloc_obj_t *closest;
  unsigned found;
  int err;
  unsigned numprocs;
  hwloc_obj_t ancestor;

  err = hwloc_topology_init (&topology);
  if (err)
    return EXIT_FAILURE;

  hwloc_topology_set_synthetic (topology, "2 3 4 5");

  err = hwloc_topology_load (topology);
  if (err)
    return EXIT_FAILURE;

  depth = hwloc_topology_get_depth(topology);

  /* get the last object of last level */
  numprocs =  hwloc_get_nbobjs_by_depth(topology, depth-1);
  last = hwloc_get_obj_by_depth(topology, depth-1, numprocs-1);

  /* allocate the array of closest objects */
  closest = malloc(numprocs * sizeof(*closest));
  assert(closest);

  /* get closest levels */
  found = hwloc_get_closest_objs (topology, last, closest, numprocs);
  printf("looked for %u closest entries, found %u\n", numprocs, found);
  assert(found == numprocs-1);

  /* check first found is closest */
  assert(closest[0] == hwloc_get_obj_by_depth(topology, depth-1, numprocs-5 /* arity is 5 on last level */));
  /* check some other expected positions */
  assert(closest[found-1] == hwloc_get_obj_by_depth(topology, depth-1, 1*3*4*5-1 /* last of first half */));
  assert(closest[found/2-1] == hwloc_get_obj_by_depth(topology, depth-1, 1*3*4*5+2*4*5-1 /* last of second third of second half */));
  assert(closest[found/2/3-1] == hwloc_get_obj_by_depth(topology, depth-1, 1*3*4*5+2*4*5+3*5-1 /* last of third quarter of third third of second half */));

  /* get ancestor of last and less close object */
  ancestor = hwloc_get_common_ancestor_obj(topology, last, closest[found-1]);
  assert(hwloc_obj_is_in_subtree(topology, last, ancestor));
  assert(hwloc_obj_is_in_subtree(topology, closest[found-1], ancestor));
  assert(ancestor == hwloc_get_root_obj(topology));
  printf("ancestor type %u depth %u number %u is system level\n",
	 ancestor->type, ancestor->depth, ancestor->logical_index);

  free(closest);
  hwloc_topology_destroy (topology);

  return EXIT_SUCCESS;
}
