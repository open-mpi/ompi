/*
 * Copyright © 2016-2018 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <stdio.h>
#include <errno.h>

#include <hwloc.h>
#include <private/misc.h> /* for for_each_*child() */

static void check(hwloc_topology_t topology, hwloc_obj_t obj, int verbose)
{
  hwloc_obj_t child;
  char buffer[64];
  hwloc_obj_type_t type;
  union hwloc_obj_attr_u attr;
  int depth;
  int err;

  err = hwloc_obj_type_snprintf(buffer, sizeof(buffer), obj, verbose);
  assert(err > 0);
  err = hwloc_type_sscanf(buffer, &type, &attr, sizeof(attr));
  assert(!err);
  assert(obj->type == type);
  if (hwloc_obj_type_is_cache(type)) {
    assert(attr.cache.type == obj->attr->cache.type);
    assert(attr.cache.depth == obj->attr->cache.depth);
  } else if (type == HWLOC_OBJ_GROUP) {
    assert(attr.group.depth == obj->attr->group.depth);
  } else if (type == HWLOC_OBJ_BRIDGE) {
    assert(attr.bridge.upstream_type == obj->attr->bridge.upstream_type);
    assert(attr.bridge.downstream_type == obj->attr->bridge.downstream_type);
  } else if (type == HWLOC_OBJ_OS_DEVICE) {
    assert(attr.osdev.type == obj->attr->osdev.type);
  }

  err = hwloc_type_sscanf_as_depth(buffer, NULL, topology, &depth);
  assert(!err);
  assert(depth == (int) obj->depth);

  for_each_child(child, obj)
    check(topology, child, verbose);
  for_each_memory_child(child, obj)
    check(topology, child, verbose);
  for_each_io_child(child, obj)
    check(topology, child, verbose);
  for_each_misc_child(child, obj)
    check(topology, child, verbose);
}

/* check whether type_sscanf() understand what type_snprintf() wrote */
int main(void)
{
  int err;
  hwloc_topology_t topology;

  err = hwloc_topology_init(&topology);
  assert(!err);
  hwloc_topology_set_all_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_ALL);
  err = hwloc_topology_load(topology);
  assert(!err);

  check(topology, hwloc_get_root_obj(topology), 0);
  check(topology, hwloc_get_root_obj(topology), 1);

  hwloc_topology_destroy(topology);
}
