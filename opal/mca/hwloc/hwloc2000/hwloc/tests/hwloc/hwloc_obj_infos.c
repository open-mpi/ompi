/*
 * Copyright © 2011 inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <assert.h>

/* check obj infos */

#define NAME1 "foobar"
#define VALUE1 "myvalue"
#define NAME2 "foobaz"
#define VALUE2 "myothervalue"

int main(void)
{
  hwloc_topology_t topology;
  hwloc_obj_t obj;

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);

  obj = hwloc_get_root_obj(topology);

  if (hwloc_obj_get_info_by_name(obj, NAME1)
      || hwloc_obj_get_info_by_name(obj, NAME2))
    return 0;

  hwloc_obj_add_info(obj, NAME1, VALUE1);
  assert(!hwloc_obj_get_info_by_name(obj, NAME2));
  hwloc_obj_add_info(obj, NAME2, VALUE2);

  if (strcmp(hwloc_obj_get_info_by_name(obj, NAME1), VALUE1))
    assert(0);
  if (strcmp(hwloc_obj_get_info_by_name(obj, NAME2), VALUE2))
    assert(0);

  hwloc_topology_destroy(topology);

  return 0;
}
