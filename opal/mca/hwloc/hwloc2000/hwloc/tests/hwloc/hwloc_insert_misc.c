/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009-2010 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

int main(void)
{
  hwloc_topology_t topology, topology2, reload;
  hwloc_obj_t obj;
  hwloc_bitmap_t set;
  char *buf1, *buf2;
  int buflen1, buflen2, err;
  hwloc_topology_diff_t diff;

  /* build and annotate a topology */
  err = hwloc_topology_init(&topology);
  assert(!err);
  err = hwloc_topology_set_synthetic(topology, "NUMA:2 pack:2 core:2 pu:2");
  assert(!err);
  err = hwloc_topology_set_type_filter(topology, HWLOC_OBJ_MISC, HWLOC_TYPE_FILTER_KEEP_ALL);
  assert(!err);
  err = hwloc_topology_load(topology);
  assert(!err);
  hwloc_topology_check(topology);
  /* Misc below root */
  obj = hwloc_get_root_obj(topology);
  obj = hwloc_topology_insert_misc_object(topology, obj, "below root");
  assert(obj);
  /* Misc below previous Misc */
  obj = hwloc_topology_insert_misc_object(topology, obj, "below Misc below root");
  assert(obj);
  /* Misc below last NUMA node */
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_NUMANODE, 1);
  assert(obj);
  obj = hwloc_topology_insert_misc_object(topology, obj, "below last NUMA");
  assert(obj);
  /* Misc below last Package */
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PACKAGE, 3);
  assert(obj);
  obj = hwloc_topology_insert_misc_object(topology, obj, "below last Package");
  assert(obj);
  /* Misc below last Core */
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_CORE, 7);
  assert(obj);
  obj = hwloc_topology_insert_misc_object(topology, obj, "below last Core");
  assert(obj);
  /* Misc below first PU */
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PU, 0);
  assert(obj);
  obj = hwloc_topology_insert_misc_object(topology, obj, "below first PU");
  assert(obj);
  hwloc_topology_check(topology);
  /* restrict it to only 3 Packages node without dropping Misc objects */
  set = hwloc_bitmap_dup(hwloc_topology_get_topology_cpuset(topology));
  obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_PACKAGE, 3);
  assert(obj);
  hwloc_bitmap_andnot(set, set, obj->cpuset);
  err = hwloc_topology_restrict(topology, set, HWLOC_RESTRICT_FLAG_ADAPT_MISC);
  assert(!err);
  hwloc_topology_check(topology);

  /* check that export/reimport/export gives same export buffer */
  err = hwloc_topology_export_xmlbuffer(topology, &buf1, &buflen1, 0);
  assert(!err);
  err = hwloc_topology_init(&reload);
  assert(!err);
  err = hwloc_topology_set_xmlbuffer(reload, buf1, buflen1);
  assert(!err);
  err = hwloc_topology_set_type_filter(reload, HWLOC_OBJ_MISC, HWLOC_TYPE_FILTER_KEEP_ALL);
  assert(!err);
  err = hwloc_topology_load(reload);
  assert(!err);
  hwloc_topology_check(reload);
  err = hwloc_topology_export_xmlbuffer(reload, &buf2, &buflen2, 0);
  assert(!err);
  assert(buflen1 == buflen2);
  err = strcmp(buf1, buf2);
  assert(!err);
  hwloc_free_xmlbuffer(reload, buf2);
  hwloc_topology_destroy(reload);

  /* build another restricted topology manually without Packages */
  err = hwloc_topology_init(&topology2);
  assert(!err);
  err = hwloc_topology_set_synthetic(topology2, "NUMA:2 pack:2 core:2 pu:2"); /* must keep the same topology string to avoid SyntheticDescription info difference */
  assert(!err);
  err = hwloc_topology_set_type_filter(topology2, HWLOC_OBJ_PACKAGE, HWLOC_TYPE_FILTER_KEEP_NONE);
  assert(!err);
  err = hwloc_topology_set_type_filter(topology2, HWLOC_OBJ_MISC, HWLOC_TYPE_FILTER_KEEP_ALL);
  assert(!err);
  err = hwloc_topology_load(topology2);
  assert(!err);
  hwloc_topology_check(topology2);
  err = hwloc_topology_restrict(topology2, set, HWLOC_RESTRICT_FLAG_ADAPT_MISC);
  assert(!err);

  /* reimport without Packages and Misc, and check they are equal */
  err = hwloc_topology_init(&reload);
  assert(!err);
  err = hwloc_topology_set_xmlbuffer(reload, buf1, buflen1);
  assert(!err);
  err = hwloc_topology_set_type_filter(reload, HWLOC_OBJ_PACKAGE, HWLOC_TYPE_FILTER_KEEP_NONE);
  assert(!err);
  err = hwloc_topology_load(reload);
  assert(!err);
  err = hwloc_topology_diff_build(reload, topology2, 0, &diff);
  assert(!err);
  assert(!diff);
  hwloc_topology_destroy(reload);

  /* re-add some Misc now */
  /* Misc below root */
  obj = hwloc_get_root_obj(topology2);
  obj = hwloc_topology_insert_misc_object(topology2, obj, "below root");
  assert(obj);
  /* Misc below previous Misc */
  obj = hwloc_topology_insert_misc_object(topology2, obj, "below Misc below root");
  assert(obj);
  /* Misc below last NUMA node */
  obj = hwloc_get_obj_by_type(topology2, HWLOC_OBJ_NUMANODE, 1);
  assert(obj);
  hwloc_topology_insert_misc_object(topology2, obj, "below last NUMA");
  /* Misc below parent Group of this NUMA node (where Package and Core Misc will end up) */
  assert(obj->parent->type == HWLOC_OBJ_GROUP);
  hwloc_topology_insert_misc_object(topology2, obj->parent, "below last Package");
  hwloc_topology_insert_misc_object(topology2, obj->parent, "below last Core");
  assert(obj);
  /* Misc below first PU */
  obj = hwloc_get_obj_by_type(topology2, HWLOC_OBJ_PU, 0);
  assert(obj);
  obj = hwloc_topology_insert_misc_object(topology2, obj, "below first PU");
  assert(obj);

  /* reimport without Packages and check they are equal*/
  err = hwloc_topology_init(&reload);
  assert(!err);
  err = hwloc_topology_set_xmlbuffer(reload, buf1, buflen1);
  assert(!err);
  err = hwloc_topology_set_type_filter(reload, HWLOC_OBJ_PACKAGE, HWLOC_TYPE_FILTER_KEEP_NONE);
  assert(!err);
  err = hwloc_topology_set_type_filter(reload, HWLOC_OBJ_MISC, HWLOC_TYPE_FILTER_KEEP_ALL);
  assert(!err);
  err = hwloc_topology_load(reload);
  assert(!err);
  err = hwloc_topology_diff_build(reload, topology2, 0, &diff);
  assert(!err);
  assert(!diff);
  hwloc_topology_destroy(reload);

  hwloc_free_xmlbuffer(topology, buf1);
  hwloc_topology_destroy(topology);
  hwloc_topology_destroy(topology2);
  hwloc_bitmap_free(set);

  return 0;
}
