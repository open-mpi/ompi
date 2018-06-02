/*
 * Copyright © 2016-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>
#include <stdio.h>
#include <assert.h>

#define LEN 1048576

int main(void)
{
  const struct hwloc_topology_support *support;
  char *buffer;
  hwloc_topology_t topology;
  hwloc_bitmap_t set = hwloc_bitmap_alloc();
  hwloc_bitmap_t total = hwloc_bitmap_alloc();
  hwloc_obj_t node;
  char *s;
  int err;

  err = hwloc_topology_init(&topology);
  assert(!err);
  err = hwloc_topology_load(topology);
  assert(!err);

  support = hwloc_topology_get_support(topology);
  if (!support->membind->get_area_memlocation)
    goto out;

  buffer = hwloc_alloc(topology, LEN);
  assert(buffer);
  printf("buffer %p length %d\n", buffer, LEN);

  err = hwloc_get_area_memlocation(topology, buffer, LEN, set, HWLOC_MEMBIND_BYNODESET);
  if (err < 0 && errno == ENOSYS) {
    fprintf(stderr, "hwloc_get_area_memlocation() failed with ENOSYS, aborting\n");
    goto out_with_buffer;
  }
  assert(!err);
  hwloc_bitmap_asprintf(&s, set);
  printf("address %p length %d allocated in nodeset %s\n", buffer, LEN, s);
  free(s);
  hwloc_bitmap_copy(total, set);

  node = NULL;
 next1:
  node = hwloc_get_next_obj_by_type(topology, HWLOC_OBJ_NUMANODE, node);
  if (!node)
    goto out_with_buffer;
  if (!node->attr->numanode.local_memory)
    goto next1;
  printf("binding to 1st node and touching 1st quarter\n");
  err = hwloc_set_area_membind(topology, buffer, LEN, node->nodeset, HWLOC_MEMBIND_BIND, HWLOC_MEMBIND_BYNODESET);
  if (err < 0 && errno == ENOSYS) {
    fprintf(stderr, "hwloc_set_area_membind() failed with ENOSYS, aborting\n");
    goto out_with_buffer;
  }
  assert(!err);

  memset(buffer, 0, LEN/4);
  err = hwloc_get_area_memlocation(topology, buffer, 1, set, HWLOC_MEMBIND_BYNODESET);
  assert(!err);
  hwloc_bitmap_asprintf(&s, set);
  printf("address %p length %d allocated in nodeset %s\n", buffer, LEN/4, s);
  free(s);
  hwloc_bitmap_or(total, total, set);

 next2:
  node = hwloc_get_next_obj_by_type(topology, HWLOC_OBJ_NUMANODE, node);
  if (!node)
    goto out_with_nomorenodes;
  if (!node->attr->numanode.local_memory)
    goto next2;
  printf("binding to 2nd node and touching 2nd quarter\n");
  err = hwloc_set_area_membind(topology, buffer, LEN, node->nodeset, HWLOC_MEMBIND_BIND, HWLOC_MEMBIND_BYNODESET);
  assert(!err);

  memset(buffer+LEN/4, 0, LEN/4);
  err = hwloc_get_area_memlocation(topology, buffer+LEN/4, LEN/4, set, HWLOC_MEMBIND_BYNODESET);
  assert(!err);
  hwloc_bitmap_asprintf(&s, set);
  printf("address %p length %d allocated in nodeset %s\n", buffer+LEN/4, LEN/4, s);
  free(s);
  hwloc_bitmap_or(total, total, set);

 next3:
  node = hwloc_get_next_obj_by_type(topology, HWLOC_OBJ_NUMANODE, node);
  if (!node)
    goto out_with_nomorenodes;
  if (!node->attr->numanode.local_memory)
    goto next3;
  printf("binding to 3rd node and touching 3rd quarter\n");
  err = hwloc_set_area_membind(topology, buffer, LEN, node->nodeset, HWLOC_MEMBIND_BIND, HWLOC_MEMBIND_BYNODESET);
  assert(!err);

  memset(buffer+LEN/2, 0, LEN/4);
  err = hwloc_get_area_memlocation(topology, buffer+LEN/2, LEN/4, set, HWLOC_MEMBIND_BYNODESET);
  assert(!err);
  hwloc_bitmap_asprintf(&s, set);
  printf("address %p length %d allocated in nodeset %s\n", buffer+LEN/2, LEN/4, s);
  free(s);
  hwloc_bitmap_or(total, total, set);

 next4:
  node = hwloc_get_next_obj_by_type(topology, HWLOC_OBJ_NUMANODE, node);
  if (!node)
    goto out_with_nomorenodes;
  if (!node->attr->numanode.local_memory)
    goto next4;
  printf("binding to 4th node and touching 4th quarter\n");
  err = hwloc_set_area_membind(topology, buffer, LEN, node->nodeset, HWLOC_MEMBIND_BIND, HWLOC_MEMBIND_BYNODESET);
  assert(!err);

  memset(buffer+3*LEN/4, 0, LEN/4);
  err = hwloc_get_area_memlocation(topology, buffer+3*LEN/4, LEN/4, set, HWLOC_MEMBIND_BYNODESET);
  assert(!err);
  hwloc_bitmap_asprintf(&s, set);
  printf("address %p length %d allocated in nodeset %s\n", buffer+3*LEN/4, LEN/4, s);
  free(s);
  hwloc_bitmap_or(total, total, set);

 out_with_nomorenodes:
  err = hwloc_get_area_memlocation(topology, buffer, LEN, set, HWLOC_MEMBIND_BYNODESET);
  assert(!err);
  hwloc_bitmap_asprintf(&s, set);
  printf("address %p length %d located on %s\n", buffer, LEN, s);
  free(s);
  assert(hwloc_bitmap_isincluded(total, set));

 out_with_buffer:
  hwloc_free(topology, buffer, LEN);

 out:
  hwloc_topology_destroy(topology);
  hwloc_bitmap_free(set);
  hwloc_bitmap_free(total);
  return 0;
}
