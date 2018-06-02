/* This example program looks for caches shared between this process
 * and another one based on their current binding.
 *
 * Copyright © 2014-2015 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
  pid_t hispid;
  hwloc_topology_t topology;
  hwloc_bitmap_t set, hisset;
  hwloc_obj_t obj;
  int err;

  /* find the pid of the other process, otherwise use my own pid */
  if (argc >= 2) {
    hispid = atoi(argv[1]);
  } else {
    hispid = getpid();
  }

  /* create a topology with instruction caches enables */
  err = hwloc_topology_init(&topology);
  if (err < 0) {
    fprintf(stderr, "failed to initialize the topology\n");
    return EXIT_FAILURE;
  }
  hwloc_topology_set_icache_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_ALL);
  err = hwloc_topology_load(topology);
  if (err < 0) {
    fprintf(stderr, "failed to load the topology\n");
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }

  /* find where I am running */
  set = hwloc_bitmap_alloc();
  if (!set) {
    fprintf(stderr, "failed to allocate my bitmap\n");
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }
  err = hwloc_get_cpubind(topology, set, 0);
  if (err < 0) {
    fprintf(stderr, "failed to get my binding\n");
    hwloc_bitmap_free(set);
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }

  /* find where the other process is running */
  hisset = hwloc_bitmap_alloc();
  if (!hisset) {
    fprintf(stderr, "failed to allocate his bitmap\n");
    hwloc_bitmap_free(set);
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }
  /* FIXME: on windows, hispid should be replaced with OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, hispid); */
  err = hwloc_get_proc_cpubind(topology, hispid, hisset, 0);
  if (err < 0) {
    fprintf(stderr, "failed to get his binding\n");
    hwloc_bitmap_free(hisset);
    hwloc_bitmap_free(set);
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }

  /* merge both process binding into mine */
  hwloc_bitmap_or(set, set, hisset);

  /* find the smallest object covering this set */
  obj = hwloc_get_obj_covering_cpuset(topology, set);

  /* display parents of type cache */
  while (obj) {
    if (hwloc_obj_type_is_cache(obj->type)) {
      char type[64];
      char attr[64];
      hwloc_obj_type_snprintf(type, sizeof(type), obj, 0);
      hwloc_obj_attr_snprintf(attr, sizeof(attr), obj, ", ", 0);
      printf("Found object %s with attributes %s\n", type, attr);
    }
    /* next parent up in the tree */
    obj = obj->parent;
  }

  hwloc_bitmap_free(hisset);
  hwloc_bitmap_free(set);
  hwloc_topology_destroy(topology);
  return EXIT_SUCCESS;
}
