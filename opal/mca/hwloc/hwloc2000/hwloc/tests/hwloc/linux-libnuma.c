/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009-2010 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>
#include <assert.h>
#define NUMA_VERSION1_COMPATIBILITY
#include <hwloc/linux-libnuma.h>

/* check the linux libnuma helpers */

int main(void)
{
  hwloc_topology_t topology;
  hwloc_bitmap_t set, set2, nocpunomemnodeset, nocpubutmemnodeset, nomembutcpunodeset, nomembutcpucpuset;
  hwloc_obj_t node;
  struct bitmask *bitmask, *bitmask2;
  unsigned long mask;
  unsigned long maxnode;
  int i;

  if (numa_available() < 0)
    /* libnuma has inconsistent behavior when the kernel isn't NUMA-aware.
     * don't try to check everything precisely.
     */
    exit(77);

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);

  /* convert full stuff between cpuset and libnuma */
  set = hwloc_bitmap_alloc();
  nocpunomemnodeset = hwloc_bitmap_alloc();
  nocpubutmemnodeset = hwloc_bitmap_alloc();
  nomembutcpunodeset = hwloc_bitmap_alloc();
  nomembutcpucpuset = hwloc_bitmap_alloc();
  /* gather all nodes if any */
  node = NULL;
  while ((node = hwloc_get_next_obj_by_type(topology, HWLOC_OBJ_NUMANODE, node)) != NULL) {
    hwloc_bitmap_or(set, set, node->cpuset);
    if (hwloc_bitmap_iszero(node->cpuset)) {
      if (node->attr->numanode.local_memory)
        hwloc_bitmap_set(nocpubutmemnodeset, node->os_index);
      else
	hwloc_bitmap_set(nocpunomemnodeset, node->os_index);
    } else if (!node->attr->numanode.local_memory) {
      hwloc_bitmap_set(nomembutcpunodeset, node->os_index);
      hwloc_bitmap_or(nomembutcpucpuset, nomembutcpucpuset, node->cpuset);
    }
  }

  set2 = hwloc_bitmap_alloc();
  hwloc_cpuset_from_linux_libnuma_bitmask(topology, set2, numa_all_nodes_ptr);
  /* numa_all_nodes_ptr doesn't contain NODES with CPU but no memory */
  hwloc_bitmap_or(set2, set2, nomembutcpucpuset);
  assert(hwloc_bitmap_isequal(set, set2));
  hwloc_bitmap_free(set2);

  bitmask = hwloc_cpuset_to_linux_libnuma_bitmask(topology, set);
  /* numa_all_nodes_ptr contains NODES with no CPU but with memory */
  hwloc_bitmap_foreach_begin(i, nocpubutmemnodeset) { numa_bitmask_setbit(bitmask, i); } hwloc_bitmap_foreach_end();
  assert(numa_bitmask_equal(bitmask, numa_all_nodes_ptr));
  numa_bitmask_free(bitmask);

  hwloc_bitmap_free(set);

  /* convert full stuff between nodeset and libnuma */
  set = hwloc_bitmap_dup(hwloc_get_root_obj(topology)->complete_nodeset);

  set2 = hwloc_bitmap_alloc();
  hwloc_nodeset_from_linux_libnuma_bitmask(topology, set2, numa_all_nodes_ptr);
  /* numa_all_nodes_ptr doesn't contain NODES with no CPU and no memory */
  hwloc_bitmap_foreach_begin(i, nocpunomemnodeset) { hwloc_bitmap_set(set2, i); } hwloc_bitmap_foreach_end();
  /* numa_all_nodes_ptr doesn't contain NODES with CPU but no memory */
  hwloc_bitmap_or(set2, set2, nomembutcpunodeset);
  assert(hwloc_bitmap_isequal(set, set2));
  hwloc_bitmap_free(set2);

  bitmask = hwloc_nodeset_to_linux_libnuma_bitmask(topology, set);
  assert(numa_bitmask_equal(bitmask, numa_all_nodes_ptr));
  numa_bitmask_free(bitmask);

  hwloc_bitmap_free(set);

  /* convert empty stuff between cpuset and libnuma */
  bitmask = numa_bitmask_alloc(1);
  set = hwloc_bitmap_alloc();
  hwloc_cpuset_from_linux_libnuma_bitmask(topology, set, bitmask);
  numa_bitmask_free(bitmask);
  assert(hwloc_bitmap_iszero(set));
  hwloc_bitmap_free(set);

  mask=0;
  set = hwloc_bitmap_alloc();
  hwloc_cpuset_from_linux_libnuma_ulongs(topology, set, &mask, sizeof(mask)*8);
  assert(hwloc_bitmap_iszero(set));
  hwloc_bitmap_free(set);

  set = hwloc_bitmap_alloc();
  bitmask = hwloc_cpuset_to_linux_libnuma_bitmask(topology, set);
  bitmask2 = numa_bitmask_alloc(1);
  assert(numa_bitmask_equal(bitmask, bitmask2));
  numa_bitmask_free(bitmask);
  numa_bitmask_free(bitmask2);
  hwloc_bitmap_free(set);

  set = hwloc_bitmap_alloc();
  maxnode = sizeof(mask)*8;
  hwloc_cpuset_to_linux_libnuma_ulongs(topology, set, &mask, &maxnode);
  assert(!mask);
  assert(!maxnode);
  hwloc_bitmap_free(set);

  /* convert empty stuff between nodeset and libnuma */
  bitmask = numa_bitmask_alloc(1);
  set = hwloc_bitmap_alloc();
  hwloc_nodeset_from_linux_libnuma_bitmask(topology, set, bitmask);
  numa_bitmask_free(bitmask);
  assert(hwloc_bitmap_iszero(set));
  hwloc_bitmap_free(set);

  mask=0;
  set = hwloc_bitmap_alloc();
  hwloc_nodeset_from_linux_libnuma_ulongs(topology, set, &mask, sizeof(mask)*8);
  assert(hwloc_bitmap_iszero(set));
  hwloc_bitmap_free(set);

  set = hwloc_bitmap_alloc();
  bitmask = hwloc_nodeset_to_linux_libnuma_bitmask(topology, set);
  bitmask2 = numa_bitmask_alloc(1);
  assert(numa_bitmask_equal(bitmask, bitmask2));
  numa_bitmask_free(bitmask);
  numa_bitmask_free(bitmask2);
  hwloc_bitmap_free(set);

  set = hwloc_bitmap_alloc();
  maxnode = sizeof(mask)*8;
  hwloc_nodeset_to_linux_libnuma_ulongs(topology, set, &mask, &maxnode);
  assert(!mask);
  assert(!maxnode);
  hwloc_bitmap_free(set);

  /* convert first node (with CPU and memory) between cpuset/nodeset and libnuma */
  node = hwloc_get_next_obj_by_type(topology, HWLOC_OBJ_NUMANODE, NULL);
  while (node && (!node->attr->numanode.local_memory || hwloc_bitmap_iszero(node->cpuset)))
    /* skip nodes with no cpus or no memory to avoid strange libnuma behaviors */
    node = hwloc_get_next_obj_by_type(topology, HWLOC_OBJ_NUMANODE, node);
  if (node) {
    /* convert first node between cpuset and libnuma */
    bitmask = hwloc_cpuset_to_linux_libnuma_bitmask(topology, node->cpuset);
    assert(numa_bitmask_isbitset(bitmask, node->os_index));
    numa_bitmask_clearbit(bitmask, node->os_index);
    bitmask2 = numa_bitmask_alloc(node->os_index + 1);
    assert(numa_bitmask_equal(bitmask, bitmask2));
    numa_bitmask_free(bitmask);
    numa_bitmask_free(bitmask2);

    maxnode = sizeof(mask)*8;
    hwloc_cpuset_to_linux_libnuma_ulongs(topology, node->cpuset, &mask, &maxnode);
    if (node->os_index >= sizeof(mask)*8) {
      assert(!maxnode);
      assert(!mask);
    } else {
      assert(maxnode == node->os_index + 1);
      assert(mask == (1UL << node->os_index));
    }

    set = hwloc_bitmap_alloc();
    bitmask = numa_bitmask_alloc(node->os_index + 1);
    numa_bitmask_setbit(bitmask, node->os_index);
    hwloc_cpuset_from_linux_libnuma_bitmask(topology, set, bitmask);
    numa_bitmask_free(bitmask);
    assert(hwloc_bitmap_isequal(set, node->cpuset));
    hwloc_bitmap_free(set);

    set = hwloc_bitmap_alloc();
    if (node->os_index >= sizeof(mask)*8) {
      mask = 0;
    } else {
      mask = 1UL << node->os_index;
    }
    hwloc_cpuset_from_linux_libnuma_ulongs(topology, set, &mask, node->os_index + 1);
    assert(hwloc_bitmap_isequal(set, node->cpuset));
    hwloc_bitmap_free(set);

    /* convert first node between nodeset and libnuma */
    bitmask = hwloc_nodeset_to_linux_libnuma_bitmask(topology, node->nodeset);
    assert(numa_bitmask_isbitset(bitmask, node->os_index));
    numa_bitmask_clearbit(bitmask, node->os_index);
    bitmask2 = numa_bitmask_alloc(node->os_index + 1);
    assert(numa_bitmask_equal(bitmask, bitmask2));
    numa_bitmask_free(bitmask);
    numa_bitmask_free(bitmask2);

    maxnode = sizeof(mask)*8;
    hwloc_nodeset_to_linux_libnuma_ulongs(topology, node->nodeset, &mask, &maxnode);
    if (node->os_index >= sizeof(mask)*8) {
      assert(!maxnode);
      assert(!mask);
    } else {
      assert(maxnode == node->os_index + 1);
      assert(mask == (1UL << node->os_index));
    }

    set = hwloc_bitmap_alloc();
    bitmask = numa_bitmask_alloc(node->os_index + 1);
    numa_bitmask_setbit(bitmask, node->os_index);
    hwloc_nodeset_from_linux_libnuma_bitmask(topology, set, bitmask);
    numa_bitmask_free(bitmask);
    assert(hwloc_bitmap_isequal(set, node->nodeset));
    hwloc_bitmap_free(set);

    set = hwloc_bitmap_alloc();
    if (node->os_index >= sizeof(mask)*8) {
      mask = 0;
    } else {
      mask = 1UL << node->os_index;
    }
    hwloc_nodeset_from_linux_libnuma_ulongs(topology, set, &mask, node->os_index + 1);
    assert(hwloc_bitmap_isequal(set, node->nodeset));
    hwloc_bitmap_free(set);
  }

  hwloc_bitmap_free(nomembutcpucpuset);
  hwloc_bitmap_free(nomembutcpunodeset);
  hwloc_bitmap_free(nocpubutmemnodeset);
  hwloc_bitmap_free(nocpunomemnodeset);

  hwloc_topology_destroy(topology);
  return 0;
}
