/* This example program plays with:
 * - retrieving and changing CPU binding of current process and thread
 * - retrieving the location where the current thread executes
 * - combining/modifying cpusets using the bitmap API
 *
 * Copyright © 2014-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int main(void)
{
  hwloc_topology_t topology;
  hwloc_bitmap_t set, set2;
  hwloc_const_bitmap_t cset_available, cset_all;
  hwloc_obj_t obj;
  char *buffer;
  char type[64];
  unsigned i;
  int err;

  /* create a topology */
  err = hwloc_topology_init(&topology);
  if (err < 0) {
    fprintf(stderr, "failed to initialize the topology\n");
    return EXIT_FAILURE;
  }
  err = hwloc_topology_load(topology);
  if (err < 0) {
    fprintf(stderr, "failed to load the topology\n");
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }

  /* retrieve the entire set of available PUs */
  cset_available = hwloc_topology_get_topology_cpuset(topology);

  /* retrieve the CPU binding of the current entire process */
  set = hwloc_bitmap_alloc();
  if (!set) {
    fprintf(stderr, "failed to allocate a bitmap\n");
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }
  err = hwloc_get_cpubind(topology, set, HWLOC_CPUBIND_PROCESS);
  if (err < 0) {
    fprintf(stderr, "failed to get cpu binding\n");
    hwloc_bitmap_free(set);
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }

  /* display the processing units that cannot be used by this process */
  if (hwloc_bitmap_isequal(set, cset_available)) {
    printf("this process can use all available processing units in the system\n");
  } else {
    /* compute the set where we currently cannot run.
     * we can't modify cset_available because it's a system read-only one,
     * so we do   set = available &~ set
     */
    hwloc_bitmap_andnot(set, cset_available, set);
    hwloc_bitmap_asprintf(&buffer, set);
    printf("process cannot use %d process units (%s) among %d in the system\n",
	   hwloc_bitmap_weight(set), buffer, hwloc_bitmap_weight(cset_available));
    free(buffer);
    /* restore set where it was before the &~ operation above */
    hwloc_bitmap_andnot(set, cset_available, set);
  }
  /* print the smallest object covering the current process binding */
  obj = hwloc_get_obj_covering_cpuset(topology, set);
  hwloc_obj_type_snprintf(type, sizeof(type), obj, 0);
  printf("process is bound within object %s logical index %u\n", type, obj->logical_index);

  /* retrieve the single PU where the current thread actually runs within this process binding */
  set2 = hwloc_bitmap_alloc();
  if (!set2) {
    fprintf(stderr, "failed to allocate a bitmap\n");
    hwloc_bitmap_free(set);
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }
  err = hwloc_get_last_cpu_location(topology, set2, HWLOC_CPUBIND_THREAD);
  if (err < 0) {
    fprintf(stderr, "failed to get last cpu location\n");
    hwloc_bitmap_free(set);
    hwloc_bitmap_free(set2);
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }
  /* sanity checks that are not actually needed but help the reader */
  /* this thread runs within the process binding */
  assert(hwloc_bitmap_isincluded(set2, set));
  /* this thread runs on a single PU at a time */
  assert(hwloc_bitmap_weight(set2) == 1);

  /* print the logical number of the PU where that thread runs */
  /* extract the PU OS index from the bitmap */
  i = hwloc_bitmap_first(set2);
  obj = hwloc_get_pu_obj_by_os_index(topology, i);
  printf("thread is now running on PU logical index %u (OS/physical index %u)\n",
	 obj->logical_index, i);

  /* migrate this single thread to where other PUs within the current binding */
  hwloc_bitmap_andnot(set2, set, set2);
  err = hwloc_set_cpubind(topology, set2, HWLOC_CPUBIND_THREAD);
  if (err < 0) {
    fprintf(stderr, "failed to set thread binding\n");
    hwloc_bitmap_free(set);
    hwloc_bitmap_free(set2);
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }
  /* reprint the PU where that thread runs */
  err = hwloc_get_last_cpu_location(topology, set2, HWLOC_CPUBIND_THREAD);
  if (err < 0) {
    fprintf(stderr, "failed to get last cpu location\n");
    hwloc_bitmap_free(set);
    hwloc_bitmap_free(set2);
    hwloc_topology_destroy(topology);
    return EXIT_FAILURE;
  }
  /* print the logical number of the PU where that thread runs */
  /* extract the PU OS index from the bitmap */
  i = hwloc_bitmap_first(set2);
  obj = hwloc_get_pu_obj_by_os_index(topology, i);
  printf("thread is running on PU logical index %u (OS/physical index %u)\n",
	 obj->logical_index, i);

  hwloc_bitmap_free(set);
  hwloc_bitmap_free(set2);

  /* retrieve the entire set of all PUs */
  cset_all = hwloc_topology_get_complete_cpuset(topology);
  if (hwloc_bitmap_isequal(cset_all, cset_available)) {
    printf("all hardware PUs are available\n");
  } else {
    printf("only %d hardware PUs are available in the machine among %d\n",
	   hwloc_bitmap_weight(cset_available), hwloc_bitmap_weight(cset_all));
  }

  hwloc_topology_destroy(topology);
  return EXIT_SUCCESS;
}
