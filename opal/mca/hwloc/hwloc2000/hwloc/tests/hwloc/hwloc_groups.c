/*
 * Copyright © 2011-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

int main(void)
{
  hwloc_topology_t topology;
  hwloc_obj_t obj;
  hwloc_obj_t objs[32];
  uint64_t values[32*32];
  int depth;
  hwloc_obj_type_t type;
  unsigned width;
  unsigned i, j;
  int err;

  /* intensive testing of two grouping cases (2+1 and 2+2+1) */

  /* group 3 numa nodes as 1 group of 2 and 1 on the side */
  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "node:3 pu:1");
  hwloc_topology_load(topology);
  /* 3 group at depth 1 */
  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_GROUP);
printf("depth %d\n", depth);
  assert(depth == 1);
  width = hwloc_get_nbobjs_by_depth(topology, 1);
  assert(width == 3);
  /* insert distances and groups */
  for(i=0; i<3; i++)
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, i);
  values[0] = 1; values[1] = 4; values[2] = 4;
  values[3] = 4; values[4] = 1; values[5] = 2;
  values[6] = 4; values[7] = 2; values[8] = 1;
  err = hwloc_distances_add(topology, 3, objs, values,
			    HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			    HWLOC_DISTANCES_ADD_FLAG_GROUP);
  assert(!err);
  /* 1 groups at depth 1 and 2 */
  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_GROUP);
  assert(depth == -2);
  type = hwloc_get_depth_type(topology, 1);
  assert(type == HWLOC_OBJ_GROUP);
  width = hwloc_get_nbobjs_by_depth(topology, 1);
  assert(width == 1);
  type = hwloc_get_depth_type(topology, 2);
  assert(type == HWLOC_OBJ_GROUP);
  width = hwloc_get_nbobjs_by_depth(topology, 2);
  assert(width == 3);
  /* 3 nodes */
  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
  assert(depth == HWLOC_TYPE_DEPTH_NUMANODE);
  width = hwloc_get_nbobjs_by_depth(topology, depth);
  assert(width == 3);
  /* find the root obj */
  obj = hwloc_get_root_obj(topology);
  assert(obj->arity == 2);
  /* check its children */
  /* first child is a group with PU+NUMA children */
  assert(obj->children[0]->type == HWLOC_OBJ_GROUP);
  assert(obj->children[0]->depth == 2);
  assert(obj->children[0]->arity == 1);
  assert(obj->children[0]->first_child->type == HWLOC_OBJ_PU);
  assert(obj->children[0]->memory_arity == 1);
  assert(obj->children[0]->memory_first_child->type == HWLOC_OBJ_NUMANODE);
  /* second child is a group with two group children */
  assert(obj->children[1]->type == HWLOC_OBJ_GROUP);
  assert(obj->children[1]->depth == 1);
  assert(obj->children[1]->arity == 2);
  assert(obj->children[1]->children[0]->type == HWLOC_OBJ_GROUP);
  assert(obj->children[1]->children[1]->type == HWLOC_OBJ_GROUP);
  assert(obj->children[1]->memory_arity == 0);
  hwloc_topology_destroy(topology);

  /* group 5 packages as 2 group of 2 and 1 on the side, all of them below a common node object */
  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "node:1 pack:5 pu:1");
  hwloc_topology_load(topology);
  for(i=0; i<5; i++)
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PACKAGE, i);
  values[ 0] = 1; values[ 1] = 2; values[ 2] = 4; values[ 3] = 4; values[ 4] = 4;
  values[ 5] = 2; values[ 6] = 1; values[ 7] = 4; values[ 8] = 4; values[ 9] = 4;
  values[10] = 4; values[11] = 4; values[12] = 1; values[13] = 4; values[14] = 4;
  values[15] = 4; values[16] = 4; values[17] = 4; values[18] = 1; values[19] = 2;
  values[20] = 4; values[21] = 4; values[22] = 4; values[23] = 2; values[24] = 1;
  err = hwloc_distances_add(topology, 5, objs, values,
			    HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			    HWLOC_DISTANCES_ADD_FLAG_GROUP);
  assert(!err);
  /* 1 node */
  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
  assert(depth == HWLOC_TYPE_DEPTH_NUMANODE);
  width = hwloc_get_nbobjs_by_depth(topology, depth);
  assert(width == 1);
  /* 2 groups at depth 1 */
  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_GROUP);
  assert(depth == 1);
  width = hwloc_get_nbobjs_by_depth(topology, depth);
  assert(width == 2);
  /* 5 packages at depth 2 */
  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_PACKAGE);
  assert(depth == 2);
  width = hwloc_get_nbobjs_by_depth(topology, depth);
  assert(width == 5);
  /* check root */
  obj = hwloc_get_root_obj(topology);
  assert(obj->arity == 3);
  assert(obj->memory_arity == 1);
  /* check root children */
  assert(obj->children[0]->type == HWLOC_OBJ_GROUP);
  assert(obj->children[0]->depth == 1);
  assert(obj->children[0]->arity == 2);
  assert(obj->children[1]->type == HWLOC_OBJ_PACKAGE);
  assert(obj->children[1]->depth == 2);
  assert(obj->children[1]->arity == 1);
  assert(obj->children[2]->type == HWLOC_OBJ_GROUP);
  assert(obj->children[2]->depth == 1);
  assert(obj->children[2]->arity == 2);
  obj = obj->memory_first_child;
  assert(obj->type == HWLOC_OBJ_NUMANODE);
  assert(obj->arity == 0);
  assert(obj->memory_arity == 0);
  hwloc_topology_destroy(topology);

/* testing of adding/replacing/removing distance matrices
   and grouping with/without accuracy
 */

  /* grouping matrix 4*2*2 */
  for(i=0; i<16; i++) {
    for(j=0; j<16; j++)
      if (i==j)
        values[i+16*j] = values[j+16*i] = 30;
      else if (i/2==j/2)
        values[i+16*j] = values[j+16*i] = 50;
      else if (i/4==j/4)
        values[i+16*j] = values[j+16*i] = 70;
      else
        values[i+16*j] = values[j+16*i] = 90;
  }

  /* default 2*8*1 */
  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "node:2 core:8 pu:1");
  hwloc_topology_load(topology);
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 4);
  width = hwloc_get_nbobjs_by_depth(topology, 0);
  assert(width == 1);
  width = hwloc_get_nbobjs_by_depth(topology, 1);
  assert(width == 2);
  width = hwloc_get_nbobjs_by_depth(topology, 2);
  assert(width == 16);
  width = hwloc_get_nbobjs_by_depth(topology, 3);
  assert(width == 16);
  width = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE);
  assert(width == 2);
  /* group 8cores as 2*2*2 */
  for(i=0; i<16; i++)
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_CORE, i);
  assert(!hwloc_distances_add(topology, 16, objs, values,
			      HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			      HWLOC_DISTANCES_ADD_FLAG_GROUP));
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 6);
  width = hwloc_get_nbobjs_by_depth(topology, 0);
  assert(width == 1);
  width = hwloc_get_nbobjs_by_depth(topology, 1);
  assert(width == 2);
  width = hwloc_get_nbobjs_by_depth(topology, 2);
  assert(width == 4);
  width = hwloc_get_nbobjs_by_depth(topology, 3);
  assert(width == 8);
  width = hwloc_get_nbobjs_by_depth(topology, 4);
  assert(width == 16);
  width = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE);
  assert(width == 2);
  hwloc_topology_destroy(topology);

  /* play with accuracy */
  values[0] = 29; /* diagonal, instead of 3 (0.0333% error) */
  values[1] = 51; values[16] = 52; /* smallest group, instead of 5 (0.02% error) */
  putenv("HWLOC_GROUPING_ACCURACY=0.1"); /* ok */
  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "node:2 core:8 pu:1");
  hwloc_topology_load(topology);
  for(i=0; i<16; i++)
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_CORE, i);
  assert(!hwloc_distances_add(topology, 16, objs, values,
			      HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			      HWLOC_DISTANCES_ADD_FLAG_GROUP|HWLOC_DISTANCES_ADD_FLAG_GROUP_INACCURATE));
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 6);
  hwloc_topology_destroy(topology);

  putenv("HWLOC_GROUPING_ACCURACY=try"); /* ok */
  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "node:2 core:8 pu:1");
  hwloc_topology_load(topology);
  for(i=0; i<16; i++)
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_CORE, i);
  assert(!hwloc_distances_add(topology, 16, objs, values,
			      HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			      HWLOC_DISTANCES_ADD_FLAG_GROUP|HWLOC_DISTANCES_ADD_FLAG_GROUP_INACCURATE));
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 6);
  hwloc_topology_destroy(topology);

  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "node:2 core:8 pu:1");
  hwloc_topology_load(topology);
  for(i=0; i<16; i++)
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_CORE, i);
  assert(!hwloc_distances_add(topology, 16, objs, values,
			      HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			      HWLOC_DISTANCES_ADD_FLAG_GROUP /* no inaccurate flag, cannot group */));
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 4);
  hwloc_topology_destroy(topology);

  putenv("HWLOC_GROUPING_ACCURACY=0.01"); /* too small, cannot group */
  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "node:2 core:8 pu:1");
  hwloc_topology_load(topology);
  for(i=0; i<16; i++)
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_CORE, i);
  assert(!hwloc_distances_add(topology, 16, objs, values,
			      HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			      HWLOC_DISTANCES_ADD_FLAG_GROUP|HWLOC_DISTANCES_ADD_FLAG_GROUP_INACCURATE));
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 4);
  hwloc_topology_destroy(topology);

  putenv("HWLOC_GROUPING_ACCURACY=0"); /* full accuracy, cannot group */
  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "node:2 core:8 pu:1");
  hwloc_topology_load(topology);
  for(i=0; i<16; i++)
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_CORE, i);
  assert(!hwloc_distances_add(topology, 16, objs, values,
			      HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			      HWLOC_DISTANCES_ADD_FLAG_GROUP|HWLOC_DISTANCES_ADD_FLAG_GROUP_INACCURATE));
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 4);
  hwloc_topology_destroy(topology);

  /* default 2*4*4 */
  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, "node:2 core:4 pu:4");
  hwloc_topology_load(topology);
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 4);
  width = hwloc_get_nbobjs_by_depth(topology, 0);
  assert(width == 1);
  width = hwloc_get_nbobjs_by_depth(topology, 1);
  assert(width == 2);
  width = hwloc_get_nbobjs_by_depth(topology, 2);
  assert(width == 8);
  width = hwloc_get_nbobjs_by_depth(topology, 3);
  assert(width == 32);
  width = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE);
  assert(width == 2);
  /* apply useless core distances */
  for(i=0; i<8; i++) {
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_CORE, i);
    for(j=0; j<8; j++)
      if (i==j)
        values[i+8*j] = values[j+8*i] = 1;
      else if (i/4==j/4)
        values[i+8*j] = values[j+8*i] = 4;
      else
        values[i+8*j] = values[j+8*i] = 8;
  }
  assert(!hwloc_distances_add(topology, 8, objs, values,
			      HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			      HWLOC_DISTANCES_ADD_FLAG_GROUP));
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 4);
  /* group 4cores as 2*2 */
  for(i=0; i<8; i++) {
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_CORE, i);
    for(j=0; j<8; j++)
      if (i==j)
        values[i+8*j] = values[j+8*i] = 1;
      else if (i/2==j/2)
        values[i+8*j] = values[j+8*i] = 4;
      else
        values[i+8*j] = values[j+8*i] = 8;
  }
  assert(!hwloc_distances_add(topology, 8, objs, values,
			      HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			      HWLOC_DISTANCES_ADD_FLAG_GROUP));
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 5);
  width = hwloc_get_nbobjs_by_depth(topology, 0);
  assert(width == 1);
  width = hwloc_get_nbobjs_by_depth(topology, 1);
  assert(width == 2);
  width = hwloc_get_nbobjs_by_depth(topology, 2);
  assert(width == 4);
  width = hwloc_get_nbobjs_by_depth(topology, 3);
  assert(width == 8);
  width = hwloc_get_nbobjs_by_depth(topology, 4);
  assert(width == 32);
  width = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE);
  assert(width == 2);
  /* group 4PUs as 2*2 */
  for(i=0; i<32; i++) {
    objs[i] = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, i);
    for(j=0; j<32; j++)
      if (i==j)
        values[i+32*j] = values[j+32*i] = 1;
      else if (i/2==j/2)
        values[i+32*j] = values[j+32*i] = 4;
      else
        values[i+32*j] = values[j+32*i] = 8;
  }
  assert(!hwloc_distances_add(topology, 32, objs, values,
			      HWLOC_DISTANCES_KIND_MEANS_LATENCY|HWLOC_DISTANCES_KIND_FROM_USER,
			      HWLOC_DISTANCES_ADD_FLAG_GROUP));
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 6);
  width = hwloc_get_nbobjs_by_depth(topology, 0);
  assert(width == 1);
  width = hwloc_get_nbobjs_by_depth(topology, 1);
  assert(width == 2);
  width = hwloc_get_nbobjs_by_depth(topology, 2);
  assert(width == 4);
  width = hwloc_get_nbobjs_by_depth(topology, 3);
  assert(width == 8);
  width = hwloc_get_nbobjs_by_depth(topology, 4);
  assert(width == 16);
  width = hwloc_get_nbobjs_by_depth(topology, 5);
  assert(width == 32);
  width = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE);
  assert(width == 2);
  hwloc_topology_destroy(topology);

  return 0;
}
