/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2014 Inria.  All rights reserved.
 * Copyright © 2009-2010, 2012 Université Bordeaux
 * See COPYING in top-level directory.
 */

/** \file
 * \brief Macros to help interaction between hwloc and Linux libnuma.
 *
 * Applications that use both Linux libnuma and hwloc may want to
 * include this file so as to ease conversion between their respective types.
*/

#ifndef HWLOC_LINUX_LIBNUMA_H
#define HWLOC_LINUX_LIBNUMA_H

#include <hwloc.h>
#include <numa.h>


#ifdef __cplusplus
extern "C" {
#endif


/** \defgroup hwlocality_linux_libnuma_ulongs Interoperability with Linux libnuma unsigned long masks
 *
 * This interface helps converting between Linux libnuma unsigned long masks
 * and hwloc cpusets and nodesets.
 *
 * It also offers a consistent behavior on non-NUMA machines
 * or non-NUMA-aware kernels by assuming that the machines have a single
 * NUMA node.
 *
 * \note Topology \p topology must match the current machine.
 *
 * \note The behavior of libnuma is undefined if the kernel is not NUMA-aware.
 * (when CONFIG_NUMA is not set in the kernel configuration).
 * This helper and libnuma may thus not be strictly compatible in this case,
 * which may be detected by checking whether numa_available() returns -1.
 *
 * @{
 */


/** \brief Convert hwloc CPU set \p cpuset into the array of unsigned long \p mask
 *
 * \p mask is the array of unsigned long that will be filled.
 * \p maxnode contains the maximal node number that may be stored in \p mask.
 * \p maxnode will be set to the maximal node number that was found, plus one.
 *
 * This function may be used before calling set_mempolicy, mbind, migrate_pages
 * or any other function that takes an array of unsigned long and a maximal
 * node number as input parameter.
 */
static __hwloc_inline int
hwloc_cpuset_to_linux_libnuma_ulongs(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset,
				    unsigned long *mask, unsigned long *maxnode)
{
  int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
  unsigned long outmaxnode = -1;

  /* round-up to the next ulong and clear all bytes */
  *maxnode = (*maxnode + 8*sizeof(*mask) - 1) & ~(8*sizeof(*mask) - 1);
  memset(mask, 0, *maxnode/8);

  if (depth != HWLOC_TYPE_DEPTH_UNKNOWN) {
    hwloc_obj_t node = NULL;
    while ((node = hwloc_get_next_obj_covering_cpuset_by_depth(topology, cpuset, depth, node)) != NULL) {
      if (node->os_index >= *maxnode)
	continue;
      mask[node->os_index/sizeof(*mask)/8] |= 1UL << (node->os_index % (sizeof(*mask)*8));
      if (outmaxnode == (unsigned long) -1 || outmaxnode < node->os_index)
	outmaxnode = node->os_index;
    }

  } else {
    /* if no numa, libnuma assumes we have a single node */
    if (!hwloc_bitmap_iszero(cpuset)) {
      mask[0] = 1;
      outmaxnode = 0;
    }
  }

  *maxnode = outmaxnode+1;
  return 0;
}

/** \brief Convert hwloc NUMA node set \p nodeset into the array of unsigned long \p mask
 *
 * \p mask is the array of unsigned long that will be filled.
 * \p maxnode contains the maximal node number that may be stored in \p mask.
 * \p maxnode will be set to the maximal node number that was found, plus one.
 *
 * This function may be used before calling set_mempolicy, mbind, migrate_pages
 * or any other function that takes an array of unsigned long and a maximal
 * node number as input parameter.
 */
static __hwloc_inline int
hwloc_nodeset_to_linux_libnuma_ulongs(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset,
				      unsigned long *mask, unsigned long *maxnode)
{
  int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
  unsigned long outmaxnode = -1;

  /* round-up to the next ulong and clear all bytes */
  *maxnode = (*maxnode + 8*sizeof(*mask) - 1) & ~(8*sizeof(*mask) - 1);
  memset(mask, 0, *maxnode/8);

  if (depth != HWLOC_TYPE_DEPTH_UNKNOWN) {
    hwloc_obj_t node = NULL;
    while ((node = hwloc_get_next_obj_by_depth(topology, depth, node)) != NULL) {
      if (node->os_index >= *maxnode)
	continue;
      if (!hwloc_bitmap_isset(nodeset, node->os_index))
	continue;
      mask[node->os_index/sizeof(*mask)/8] |= 1UL << (node->os_index % (sizeof(*mask)*8));
      if (outmaxnode == (unsigned long) -1 || outmaxnode < node->os_index)
	outmaxnode = node->os_index;
    }

  } else {
    /* if no numa, libnuma assumes we have a single node */
    if (!hwloc_bitmap_iszero(nodeset)) {
      mask[0] = 1;
      outmaxnode = 0;
    }
  }

  *maxnode = outmaxnode+1;
  return 0;
}

/** \brief Convert the array of unsigned long \p mask into hwloc CPU set
 *
 * \p mask is a array of unsigned long that will be read.
 * \p maxnode contains the maximal node number that may be read in \p mask.
 *
 * This function may be used after calling get_mempolicy or any other function
 * that takes an array of unsigned long as output parameter (and possibly
 * a maximal node number as input parameter).
 */
static __hwloc_inline int
hwloc_cpuset_from_linux_libnuma_ulongs(hwloc_topology_t topology, hwloc_cpuset_t cpuset,
				      const unsigned long *mask, unsigned long maxnode)
{
  int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);

  if (depth != HWLOC_TYPE_DEPTH_UNKNOWN) {
    hwloc_obj_t node = NULL;
    hwloc_bitmap_zero(cpuset);
    while ((node = hwloc_get_next_obj_by_depth(topology, depth, node)) != NULL)
      if (node->os_index < maxnode
	  && (mask[node->os_index/sizeof(*mask)/8] & (1UL << (node->os_index % (sizeof(*mask)*8)))))
	hwloc_bitmap_or(cpuset, cpuset, node->cpuset);
  } else {
    /* if no numa, libnuma assumes we have a single node */
    if (mask[0] & 1)
      hwloc_bitmap_copy(cpuset, hwloc_topology_get_complete_cpuset(topology));
    else
      hwloc_bitmap_zero(cpuset);
  }

  return 0;
}

/** \brief Convert the array of unsigned long \p mask into hwloc NUMA node set
 *
 * \p mask is a array of unsigned long that will be read.
 * \p maxnode contains the maximal node number that may be read in \p mask.
 *
 * This function may be used after calling get_mempolicy or any other function
 * that takes an array of unsigned long as output parameter (and possibly
 * a maximal node number as input parameter).
 */
static __hwloc_inline int
hwloc_nodeset_from_linux_libnuma_ulongs(hwloc_topology_t topology, hwloc_nodeset_t nodeset,
					const unsigned long *mask, unsigned long maxnode)
{
  int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);

  if (depth != HWLOC_TYPE_DEPTH_UNKNOWN) {
    hwloc_obj_t node = NULL;
    hwloc_bitmap_zero(nodeset);
    while ((node = hwloc_get_next_obj_by_depth(topology, depth, node)) != NULL)
      if (node->os_index < maxnode
	  && (mask[node->os_index/sizeof(*mask)/8] & (1UL << (node->os_index % (sizeof(*mask)*8)))))
	hwloc_bitmap_set(nodeset, node->os_index);
  } else {
    /* if no numa, libnuma assumes we have a single node */
    if (mask[0] & 1)
      hwloc_bitmap_fill(nodeset);
    else
      hwloc_bitmap_zero(nodeset);
  }

  return 0;
}

/** @} */



/** \defgroup hwlocality_linux_libnuma_bitmask Interoperability with Linux libnuma bitmask
 *
 * This interface helps converting between Linux libnuma bitmasks
 * and hwloc cpusets and nodesets.
 *
 * It also offers a consistent behavior on non-NUMA machines
 * or non-NUMA-aware kernels by assuming that the machines have a single
 * NUMA node.
 *
 * \note Topology \p topology must match the current machine.
 *
 * \note The behavior of libnuma is undefined if the kernel is not NUMA-aware.
 * (when CONFIG_NUMA is not set in the kernel configuration).
 * This helper and libnuma may thus not be strictly compatible in this case,
 * which may be detected by checking whether numa_available() returns -1.
 *
 * @{
 */


/** \brief Convert hwloc CPU set \p cpuset into the returned libnuma bitmask
 *
 * The returned bitmask should later be freed with numa_bitmask_free.
 *
 * This function may be used before calling many numa_ functions
 * that use a struct bitmask as an input parameter.
 *
 * \return newly allocated struct bitmask.
 */
static __hwloc_inline struct bitmask *
hwloc_cpuset_to_linux_libnuma_bitmask(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset) __hwloc_attribute_malloc;
static __hwloc_inline struct bitmask *
hwloc_cpuset_to_linux_libnuma_bitmask(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset)
{
  int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
  struct bitmask *bitmask = numa_allocate_cpumask();
  if (!bitmask)
    return NULL;

  if (depth != HWLOC_TYPE_DEPTH_UNKNOWN) {
    hwloc_obj_t node = NULL;
    while ((node = hwloc_get_next_obj_covering_cpuset_by_depth(topology, cpuset, depth, node)) != NULL)
      if (node->memory.local_memory)
	numa_bitmask_setbit(bitmask, node->os_index);
  } else {
    /* if no numa, libnuma assumes we have a single node */
    if (!hwloc_bitmap_iszero(cpuset))
      numa_bitmask_setbit(bitmask, 0);
  }

  return bitmask;
}

/** \brief Convert hwloc NUMA node set \p nodeset into the returned libnuma bitmask
 *
 * The returned bitmask should later be freed with numa_bitmask_free.
 *
 * This function may be used before calling many numa_ functions
 * that use a struct bitmask as an input parameter.
 *
 * \return newly allocated struct bitmask.
 */
static __hwloc_inline struct bitmask *
hwloc_nodeset_to_linux_libnuma_bitmask(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset) __hwloc_attribute_malloc;
static __hwloc_inline struct bitmask *
hwloc_nodeset_to_linux_libnuma_bitmask(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset)
{
  int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
  struct bitmask *bitmask = numa_allocate_cpumask();
  if (!bitmask)
    return NULL;

  if (depth != HWLOC_TYPE_DEPTH_UNKNOWN) {
    hwloc_obj_t node = NULL;
    while ((node = hwloc_get_next_obj_by_depth(topology, depth, node)) != NULL)
      if (hwloc_bitmap_isset(nodeset, node->os_index) && node->memory.local_memory)
	numa_bitmask_setbit(bitmask, node->os_index);
  } else {
    /* if no numa, libnuma assumes we have a single node */
    if (!hwloc_bitmap_iszero(nodeset))
      numa_bitmask_setbit(bitmask, 0);
  }

  return bitmask;
}

/** \brief Convert libnuma bitmask \p bitmask into hwloc CPU set \p cpuset
 *
 * This function may be used after calling many numa_ functions
 * that use a struct bitmask as an output parameter.
 */
static __hwloc_inline int
hwloc_cpuset_from_linux_libnuma_bitmask(hwloc_topology_t topology, hwloc_cpuset_t cpuset,
					const struct bitmask *bitmask)
{
  int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);

  if (depth != HWLOC_TYPE_DEPTH_UNKNOWN) {
    hwloc_obj_t node = NULL;
    hwloc_bitmap_zero(cpuset);
    while ((node = hwloc_get_next_obj_by_depth(topology, depth, node)) != NULL)
      if (numa_bitmask_isbitset(bitmask, node->os_index))
	hwloc_bitmap_or(cpuset, cpuset, node->cpuset);
  } else {
    /* if no numa, libnuma assumes we have a single node */
    if (numa_bitmask_isbitset(bitmask, 0))
      hwloc_bitmap_copy(cpuset, hwloc_topology_get_complete_cpuset(topology));
    else
      hwloc_bitmap_zero(cpuset);
  }

  return 0;
}

/** \brief Convert libnuma bitmask \p bitmask into hwloc NUMA node set \p nodeset
 *
 * This function may be used after calling many numa_ functions
 * that use a struct bitmask as an output parameter.
 */
static __hwloc_inline int
hwloc_nodeset_from_linux_libnuma_bitmask(hwloc_topology_t topology, hwloc_nodeset_t nodeset,
					 const struct bitmask *bitmask)
{
  int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);

  if (depth != HWLOC_TYPE_DEPTH_UNKNOWN) {
    hwloc_obj_t node = NULL;
    hwloc_bitmap_zero(nodeset);
    while ((node = hwloc_get_next_obj_by_depth(topology, depth, node)) != NULL)
      if (numa_bitmask_isbitset(bitmask, node->os_index))
	hwloc_bitmap_set(nodeset, node->os_index);
  } else {
    /* if no numa, libnuma assumes we have a single node */
    if (numa_bitmask_isbitset(bitmask, 0))
      hwloc_bitmap_fill(nodeset);
    else
      hwloc_bitmap_zero(nodeset);
  }

  return 0;
}

/** @} */


#ifdef __cplusplus
} /* extern "C" */
#endif


#endif /* HWLOC_LINUX_NUMA_H */
