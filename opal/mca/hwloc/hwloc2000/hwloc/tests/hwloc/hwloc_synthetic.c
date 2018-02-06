/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2018 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/* check that object userdata is properly initialized */

static void check_level(hwloc_topology_t topology, unsigned depth, unsigned width, unsigned arity)
{
  unsigned j;
  assert(hwloc_get_nbobjs_by_depth(topology, depth) == width);
  for(j=0; j<width; j++) {
    hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, depth, j);
    assert(obj);
    assert(obj->arity == arity);
  }
}

int main(void)
{
  hwloc_topology_t topology;
  unsigned depth;
  char buffer[1024];
  int err;

  /* check a synthetic topology */
  hwloc_topology_init(&topology);
  err = hwloc_topology_set_synthetic(topology, "pack:2 numa:3 l2:4 core:5 pu:6");
  assert(!err);
  hwloc_topology_load(topology);

  assert(hwloc_get_memory_parents_depth(topology) == 2);

  /* internal checks */

  hwloc_topology_check(topology);

  /* local checks */
  depth = hwloc_topology_get_depth(topology);
  assert(depth == 6);

  check_level(topology, 0, 1, 2);
  check_level(topology, 1, 2, 3);
  check_level(topology, 2, 6, 4);
  check_level(topology, 3, 24, 5);
  check_level(topology, 4, 120, 6);
  check_level(topology, 5, 720, 0);
  check_level(topology, HWLOC_TYPE_DEPTH_NUMANODE, 6, 0);

  err = hwloc_topology_export_synthetic(topology, buffer, sizeof(buffer), 0);
  assert(err == 83);
  err = strcmp("Package:2 Group:3 [NUMANode(memory=1073741824)] L2Cache:4(size=4194304) Core:5 PU:6", buffer);
  assert(!err);

  assert(hwloc_get_memory_parents_depth(topology) == 2);

  err = hwloc_topology_export_synthetic(topology, buffer, sizeof(buffer), HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_EXTENDED_TYPES|HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_ATTRS|HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_V1);
  assert(err == 47);
  err = strcmp("Socket:2 Group:3 NUMANode:1 Cache:4 Core:5 PU:6", buffer);
  assert(!err);

  hwloc_topology_destroy(topology);



  hwloc_topology_init(&topology);
  err = hwloc_topology_set_type_filter(topology, HWLOC_OBJ_L1ICACHE, HWLOC_TYPE_FILTER_KEEP_ALL);
  err = hwloc_topology_set_synthetic(topology, "pack:2(indexes=3,5) numa:2(memory=256GB indexes=pack) l3u:1(size=20mb) l2:2 l1i:1(size=16kB) l1dcache:2 core:1 pu:2(indexes=l2)");
  assert(!err);
  hwloc_topology_load(topology);

  assert(hwloc_get_memory_parents_depth(topology) == 2);

  err = hwloc_topology_export_synthetic(topology, buffer, sizeof(buffer), 0);
  assert(err == 181);
  err = strcmp("Package:2 L3Cache:2(size=20971520) [NUMANode(memory=274877906944 indexes=2*2:1*2)] L2Cache:2(size=4194304) L1iCache:1(size=16384) L1dCache:2(size=32768) Core:1 PU:2(indexes=4*8:1*4)", buffer);
  assert(!err);

  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PACKAGE, 1)->os_index == 5);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_NUMANODE, 1)->os_index == 2);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 12)->os_index == 3);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 13)->os_index == 11);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 14)->os_index == 19);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 15)->os_index == 27);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 16)->os_index == 4);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 17)->os_index == 12);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 18)->os_index == 20);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 19)->os_index == 28);

  hwloc_topology_destroy(topology);




  hwloc_topology_init(&topology);
  err = hwloc_topology_set_synthetic(topology, "pack:2 core:2 pu:2(indexes=0,4,2,6,1,5,3,7)");
  assert(!err);
  hwloc_topology_load(topology);

  assert(hwloc_get_memory_parents_depth(topology) == 0);

  err = hwloc_topology_export_synthetic(topology, buffer, sizeof(buffer), 0);
  assert(err == 72);
  err = strcmp("[NUMANode(memory=1073741824)] Package:2 Core:2 PU:2(indexes=4*2:2*2:1*2)", buffer);
  assert(!err);

  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 0)->os_index == 0);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 1)->os_index == 4);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 2)->os_index == 2);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 3)->os_index == 6);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 4)->os_index == 1);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 5)->os_index == 5);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 6)->os_index == 3);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 7)->os_index == 7);

  hwloc_topology_destroy(topology);




  hwloc_topology_init(&topology);
  err = hwloc_topology_set_synthetic(topology, "pack:2 numa:2 core:1 pu:2(indexes=0,4,2,6,1,3,5,7)");
  assert(!err);
  hwloc_topology_load(topology);

  assert(hwloc_get_memory_parents_depth(topology) == 2);

  err = hwloc_topology_export_synthetic(topology, buffer, sizeof(buffer), 0);
  assert(err == 76);
  err = strcmp("Package:2 Core:2 [NUMANode(memory=1073741824)] PU:2(indexes=0,4,2,6,1,3,5,7)", buffer);
  assert(!err);

  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 0)->os_index == 0);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 1)->os_index == 4);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 2)->os_index == 2);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 3)->os_index == 6);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 4)->os_index == 1);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 5)->os_index == 3);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 6)->os_index == 5);
  assert(hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 7)->os_index == 7);

  hwloc_topology_destroy(topology);




  hwloc_topology_init(&topology);
  err = hwloc_topology_set_synthetic(topology, "pack:2 [numa(memory=1GB)] [numa(memory=1MB)] core:2 [numa(indexes=8,7,5,6,4,3,1,2)] pu:4");
  assert(!err);
  hwloc_topology_load(topology);

  assert(hwloc_get_memory_parents_depth(topology) == HWLOC_TYPE_DEPTH_MULTIPLE);

  err = hwloc_topology_export_synthetic(topology, buffer, sizeof(buffer), 0);
  assert(err == 114);
  err = strcmp("Package:2 [NUMANode(memory=1073741824)] [NUMANode(memory=1048576)] Core:2 [NUMANode(indexes=8,7,5,6,4,3,1,2)] PU:4", buffer);
  assert(!err);

  err = hwloc_topology_export_synthetic(topology, buffer, sizeof(buffer), HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_V1);
  assert(err == -1);
  assert(errno == EINVAL);

  err = hwloc_topology_export_synthetic(topology, buffer, sizeof(buffer), HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_IGNORE_MEMORY);
  assert(err == 21);
  err = strcmp("Package:2 Core:2 PU:4", buffer);
  assert(!err);

  hwloc_topology_destroy(topology);

  return 0;
}
