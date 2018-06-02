/*
 * Copyright © 2016-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 *
 * This program modifies PU and NUMA node os_index for debugging.
 * Applications are not supposed to do that!
 * And it may cause warnings since objects may become out-of-order
 * with respect to their cpusets.
 */

#include <hwloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#if HWLOC_API_VERSION >= 0x20000
#define HWLOC2
#endif

static void usage(FILE *output, const char *callname)
{
  fprintf(output, "%s <input.xml> <output.xml> <type> <old index> <new index>\n", callname);
  fprintf(output, " Replaces object type <type> os_index <old index> with <new index>\n");
  fprintf(output, " <type> should be PU or NUMANode\n");
}

static void switch_set_index(hwloc_bitmap_t set, unsigned old_index, unsigned new_index)
{
  if (hwloc_bitmap_isset(set, old_index)) {
    hwloc_bitmap_clr(set, old_index);
    hwloc_bitmap_set(set, new_index);
  }
}

static void switch_pu_index(hwloc_obj_t obj, unsigned old_index, unsigned new_index)
{
  hwloc_obj_t child;

  if (obj->type == HWLOC_OBJ_PU) {
    assert(obj->os_index == old_index);
    obj->os_index = new_index;
  }

  switch_set_index(obj->cpuset, old_index, new_index);
#ifndef HWLOC2
  switch_set_index(obj->online_cpuset, old_index, new_index);
#endif
  switch_set_index(obj->complete_cpuset, old_index, new_index);

  for(child = obj->first_child; child; child = child->next_sibling)
    if (child->complete_cpuset && hwloc_bitmap_isset(child->complete_cpuset, old_index))
      switch_pu_index(child, old_index, new_index);
  for(child = obj->memory_first_child; child; child = child->next_sibling)
    if (child->complete_cpuset && hwloc_bitmap_isset(child->complete_cpuset, old_index))
      switch_pu_index(child, old_index, new_index);
}

static void switch_numa_index(hwloc_obj_t obj, unsigned old_index, unsigned new_index)
{
  hwloc_obj_t child;

  if (obj->type == HWLOC_OBJ_NUMANODE) {
    assert(obj->os_index == old_index);
    obj->os_index = new_index;
  }

  switch_set_index(obj->nodeset, old_index, new_index);
  switch_set_index(obj->complete_nodeset, old_index, new_index);

  for(child = obj->first_child; child; child = child->next_sibling)
    if (child->complete_nodeset && hwloc_bitmap_isset(child->complete_nodeset, old_index))
      switch_numa_index(child, old_index, new_index);
  for(child = obj->memory_first_child; child; child = child->next_sibling)
    if (child->complete_nodeset && hwloc_bitmap_isset(child->complete_nodeset, old_index))
      switch_numa_index(child, old_index, new_index);
}

int main(int argc, char *argv[])
{
  hwloc_obj_type_t type;
  unsigned old_index, new_index;
  const char *callname = argv[0];
  hwloc_topology_t topology;
  int err;

  if (argc < 6) {
    usage(stderr, callname);
    exit(EXIT_FAILURE);
  }

#ifdef HWLOC2
  err = hwloc_type_sscanf(argv[3], &type, NULL, 0);
#else
  err = hwloc_obj_type_sscanf(argv[3], &type, NULL, NULL, 0);
#endif
  if (err < 0) {
    fprintf(stderr, "Failed to recognize type `%s'\n", argv[3]);
    usage(stderr, callname);
    exit(EXIT_FAILURE);
  }
  if (type != HWLOC_OBJ_PU && type != HWLOC_OBJ_NUMANODE) {
    fprintf(stderr, "Invalid type `%s', should be PU or NUMA node\n", argv[3]);
    usage(stderr, callname);
    exit(EXIT_FAILURE);
  }

  old_index = atoi(argv[4]);
  new_index = atoi(argv[5]);
  if (old_index == new_index) {
    fprintf(stderr, "Nothing to do\n");
    exit(EXIT_SUCCESS);
  }

  err = hwloc_topology_init(&topology);
  if (err < 0) {
    fprintf(stderr, "hwloc_topology_init() failed (%s)\n", strerror(errno));
    usage(stderr, callname);
    exit(EXIT_FAILURE);
  }

  err = hwloc_topology_set_xml(topology, argv[1]);
  if (err < 0) {
    fprintf(stderr, "hwloc_topology_set_xml() on file `%s' failed (%s)\n", argv[1], strerror(errno));
    usage(stderr, callname);
    exit(EXIT_FAILURE);
  }

#ifdef HWLOC2
  err = hwloc_topology_set_flags(topology,
				 HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM);
  err = hwloc_topology_set_all_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_ALL);
#else
  err = hwloc_topology_set_flags(topology,
				 HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM
				 | HWLOC_TOPOLOGY_FLAG_WHOLE_IO
				 | HWLOC_TOPOLOGY_FLAG_ICACHES);
#endif

  err = hwloc_topology_load(topology);
  if (err < 0) {
    fprintf(stderr, "hwloc_topology_load() failed (%s)\n", strerror(errno));
    usage(stderr, callname);
    exit(EXIT_FAILURE);
  }

  if (HWLOC_OBJ_PU == type) {
    hwloc_const_bitmap_t cpset = hwloc_topology_get_complete_cpuset(topology);
    if (!hwloc_bitmap_isset(cpset, old_index)) {
      fprintf(stderr, "Old PU os_index %u doesn't exist\n", old_index);
      usage(stderr, callname);
      exit(EXIT_FAILURE);
    }
    if (hwloc_bitmap_isset(cpset, new_index)) {
      fprintf(stderr, "New PU os_index %u already exists\n", new_index);
      usage(stderr, callname);
      exit(EXIT_FAILURE);
    }

    switch_set_index((hwloc_bitmap_t)hwloc_topology_get_allowed_cpuset(topology), old_index, new_index);
    switch_pu_index(hwloc_get_root_obj(topology), old_index, new_index);

  } else if (HWLOC_OBJ_NUMANODE == type) {
    hwloc_const_bitmap_t cnset = hwloc_topology_get_complete_nodeset(topology);
    if (!cnset || hwloc_bitmap_isfull(cnset)) {
      fprintf(stderr, "Topology doesn't have NUMA nodes\n");
      usage(stderr, callname);
      exit(EXIT_FAILURE);
    }
    if (!hwloc_bitmap_isset(cnset, old_index)) {
      fprintf(stderr, "Old NUMA node os_index %u doesn't exist\n", old_index);
      usage(stderr, callname);
      exit(EXIT_FAILURE);
    }
    if (hwloc_bitmap_isset(cnset, new_index)) {
      fprintf(stderr, "New NUMA node os_index %u already exists\n", new_index);
      usage(stderr, callname);
      exit(EXIT_FAILURE);
    }

    switch_set_index((hwloc_bitmap_t)hwloc_topology_get_allowed_nodeset(topology), old_index, new_index);
    switch_numa_index(hwloc_get_root_obj(topology), old_index, new_index);
  }

  err = hwloc_topology_export_xml(topology, argv[2], 0);
  if (err < 0) {
    fprintf(stderr, "hwloc_topology_export_xml() on file `%s' failed (%s)\n", argv[2], strerror(errno));
    usage(stderr, callname);
    exit(EXIT_FAILURE);
  }

  hwloc_topology_destroy(topology);

  printf("Beware that hwloc may warn about out-of-order objects when reloading %s\n", argv[2]);
  return 0;
}
