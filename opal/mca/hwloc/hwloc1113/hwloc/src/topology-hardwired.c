/*
 * Copyright © 2015-2016 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>

#include <hwloc.h>
#include <private/private.h>

int hwloc_look_hardwired_fujitsu_k(struct hwloc_topology *topology)
{
  /* If a broken core gets disabled, its bit disappears and other core bits are NOT shifted towards 0.
   * Node is not given to user job, not need to handle that case properly.
   */
  unsigned i;
  hwloc_obj_t obj;
  hwloc_bitmap_t set;

  for(i=0; i<8; i++) {
    set = hwloc_bitmap_alloc();
    hwloc_bitmap_set(set, i);

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
    obj->cpuset = hwloc_bitmap_dup(set);
    obj->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;
    obj->attr->cache.depth = 1;
    obj->attr->cache.size = 32*1024;
    obj->attr->cache.linesize = 128;
    obj->attr->cache.associativity = 2;
    hwloc_insert_object_by_cpuset(topology, obj);

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
    obj->cpuset = hwloc_bitmap_dup(set);
    obj->attr->cache.type = HWLOC_OBJ_CACHE_DATA;
    obj->attr->cache.depth = 1;
    obj->attr->cache.size = 32*1024;
    obj->attr->cache.linesize = 128;
    obj->attr->cache.associativity = 2;
    hwloc_insert_object_by_cpuset(topology, obj);

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_CORE, i);
    obj->cpuset = set;
    hwloc_insert_object_by_cpuset(topology, obj);
  }

  set = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(set, 0, 7);

  obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
  obj->cpuset = hwloc_bitmap_dup(set);
  obj->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
  obj->attr->cache.depth = 2;
  obj->attr->cache.size = 6*1024*1024;
  obj->attr->cache.linesize = 128;
  obj->attr->cache.associativity = 12;
  hwloc_insert_object_by_cpuset(topology, obj);

  obj = hwloc_alloc_setup_object(HWLOC_OBJ_PACKAGE, 0);
  obj->cpuset = set;
  hwloc_obj_add_info(obj, "CPUVendor", "Fujitsu");
  hwloc_obj_add_info(obj, "CPUModel", "SPARC64 VIIIfx");
  hwloc_insert_object_by_cpuset(topology, obj);

  hwloc_setup_pu_level(topology, 8);

  return 0;
}

int hwloc_look_hardwired_fujitsu_fx10(struct hwloc_topology *topology)
{
  /* If a broken core gets disabled, its bit disappears and other core bits are NOT shifted towards 0.
   * Node is not given to user job, not need to handle that case properly.
   */
  unsigned i;
  hwloc_obj_t obj;
  hwloc_bitmap_t set;

  for(i=0; i<16; i++) {
    set = hwloc_bitmap_alloc();
    hwloc_bitmap_set(set, i);

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
    obj->cpuset = hwloc_bitmap_dup(set);
    obj->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;
    obj->attr->cache.depth = 1;
    obj->attr->cache.size = 32*1024;
    obj->attr->cache.linesize = 128;
    obj->attr->cache.associativity = 2;
    hwloc_insert_object_by_cpuset(topology, obj);

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
    obj->cpuset = hwloc_bitmap_dup(set);
    obj->attr->cache.type = HWLOC_OBJ_CACHE_DATA;
    obj->attr->cache.depth = 1;
    obj->attr->cache.size = 32*1024;
    obj->attr->cache.linesize = 128;
    obj->attr->cache.associativity = 2;
    hwloc_insert_object_by_cpuset(topology, obj);

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_CORE, i);
    obj->cpuset = set;
    hwloc_insert_object_by_cpuset(topology, obj);
  }

  set = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(set, 0, 15);

  obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
  obj->cpuset = hwloc_bitmap_dup(set);
  obj->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
  obj->attr->cache.depth = 2;
  obj->attr->cache.size = 12*1024*1024;
  obj->attr->cache.linesize = 128;
  obj->attr->cache.associativity = 24;
  hwloc_insert_object_by_cpuset(topology, obj);

  obj = hwloc_alloc_setup_object(HWLOC_OBJ_PACKAGE, 0);
  obj->cpuset = set;
  hwloc_obj_add_info(obj, "CPUVendor", "Fujitsu");
  hwloc_obj_add_info(obj, "CPUModel", "SPARC64 IXfx");
  hwloc_insert_object_by_cpuset(topology, obj);

  hwloc_setup_pu_level(topology, 16);

  return 0;
}

int hwloc_look_hardwired_fujitsu_fx100(struct hwloc_topology *topology)
{
  /* If a broken core gets disabled, its bit disappears and other core bits are NOT shifted towards 0.
   * Node is not given to user job, not need to handle that case properly.
   */
  unsigned i;
  hwloc_obj_t obj;
  hwloc_bitmap_t set;

  for(i=0; i<34; i++) {
    set = hwloc_bitmap_alloc();
    hwloc_bitmap_set(set, i);

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
    obj->cpuset = hwloc_bitmap_dup(set);
    obj->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;
    obj->attr->cache.depth = 1;
    obj->attr->cache.size = 64*1024;
    obj->attr->cache.linesize = 256;
    obj->attr->cache.associativity = 4;
    hwloc_insert_object_by_cpuset(topology, obj);

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
    obj->cpuset = hwloc_bitmap_dup(set);
    obj->attr->cache.type = HWLOC_OBJ_CACHE_DATA;
    obj->attr->cache.depth = 1;
    obj->attr->cache.size = 64*1024;
    obj->attr->cache.linesize = 256;
    obj->attr->cache.associativity = 4;
    hwloc_insert_object_by_cpuset(topology, obj);

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_CORE, i);
    obj->cpuset = set;
    hwloc_insert_object_by_cpuset(topology, obj);
  }

  obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
  obj->cpuset = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(obj->cpuset, 0, 15);
  hwloc_bitmap_set(obj->cpuset, 32);
  obj->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
  obj->attr->cache.depth = 2;
  obj->attr->cache.size = 12*1024*1024;
  obj->attr->cache.linesize = 256;
  obj->attr->cache.associativity = 24;
  hwloc_insert_object_by_cpuset(topology, obj);

  obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
  obj->cpuset = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(obj->cpuset, 16, 31);
  hwloc_bitmap_set(obj->cpuset, 33);
  obj->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
  obj->attr->cache.depth = 2;
  obj->attr->cache.size = 12*1024*1024;
  obj->attr->cache.linesize = 256;
  obj->attr->cache.associativity = 24;
  hwloc_insert_object_by_cpuset(topology, obj);

  obj = hwloc_alloc_setup_object(HWLOC_OBJ_PACKAGE, 0);
  obj->cpuset = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(obj->cpuset, 0, 33);
  hwloc_obj_add_info(obj, "CPUVendor", "Fujitsu");
  hwloc_obj_add_info(obj, "CPUModel", "SPARC64 XIfx");
  hwloc_insert_object_by_cpuset(topology, obj);

  hwloc_setup_pu_level(topology, 34);

  return 0;
}
