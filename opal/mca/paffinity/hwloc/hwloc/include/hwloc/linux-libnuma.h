/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
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


/** \defgroup hwlocality_linux_libnuma_ulongs Helpers for manipulating Linux libnuma unsigned long masks
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
static __inline int
hwloc_cpuset_to_linux_libnuma_ulongs(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset,
				    unsigned long *mask, unsigned long *maxnode)
{
  unsigned long outmaxnode = -1;
  hwloc_obj_t node = NULL;
  unsigned nbnodes = hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_NODE);
  unsigned i;

  for(i=0; i<*maxnode/HWLOC_BITS_PER_LONG; i++)
    mask[i] = 0;

  if (nbnodes) {
    while ((node = hwloc_get_next_obj_covering_cpuset_by_type(topology, cpuset, HWLOC_OBJ_NODE, node)) != NULL) {
      if (node->os_index >= *maxnode)
	break;
      mask[node->os_index/HWLOC_BITS_PER_LONG] |= 1 << (node->os_index % HWLOC_BITS_PER_LONG);
      outmaxnode = node->os_index;
    }

  } else {
    /* if no numa, libnuma assumes we have a single node */
    if (!hwloc_cpuset_iszero(cpuset)) {
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
static __inline int
hwloc_cpuset_from_linux_libnuma_ulongs(hwloc_topology_t topology, hwloc_cpuset_t cpuset,
				      const unsigned long *mask, unsigned long maxnode)
{
  hwloc_obj_t node;
  int depth;
  unsigned i;

  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NODE);
  /* cannot be HWLOC_TYPE_DEPTH_MULTIPLE */

  if (depth == HWLOC_TYPE_DEPTH_UNKNOWN) {
    /* if no numa, libnuma assumes we have a single node */
    if (mask[0] & 1)
      hwloc_cpuset_copy(cpuset, hwloc_topology_get_complete_cpuset(topology));
    else
      hwloc_cpuset_zero(cpuset);

  } else {
    hwloc_cpuset_zero(cpuset);
    for(i=0; i<maxnode; i++)
      if (mask[i/HWLOC_BITS_PER_LONG] & (1 << (i% HWLOC_BITS_PER_LONG))) {
	node = hwloc_get_obj_by_depth(topology, depth, i);
	if (node)
	  hwloc_cpuset_or(cpuset, cpuset, node->cpuset);
      }
  }

  return 0;
}

/** @} */



/** \defgroup hwlocality_linux_libnuma_bitmask Helpers for manipulating Linux libnuma bitmask
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
static __inline struct bitmask * __hwloc_attribute_malloc
hwloc_cpuset_to_linux_libnuma_bitmask(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset)
{
  struct bitmask *bitmask;
  hwloc_obj_t node = NULL;
  unsigned nbnodes = hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_NODE);

  if (nbnodes) {
    bitmask = numa_bitmask_alloc(nbnodes);
    if (!bitmask)
      return NULL;
    while ((node = hwloc_get_next_obj_covering_cpuset_by_type(topology, cpuset, HWLOC_OBJ_NODE, node)) != NULL)
      numa_bitmask_setbit(bitmask, node->os_index);

  } else {
    /* if no numa, libnuma assumes we have a single node */
    bitmask = numa_bitmask_alloc(1);
    if (!bitmask)
      return NULL;
    if (!hwloc_cpuset_iszero(cpuset))
      numa_bitmask_setbit(bitmask, 0);
  }

  return bitmask;
}

/** \brief Convert libnuma bitmask \p bitmask into hwloc CPU set \p cpuset
 *
 * This function may be used after calling many numa_ functions
 * that use a struct bitmask as an output parameter.
 */
static __inline int
hwloc_cpuset_from_linux_libnuma_bitmask(hwloc_topology_t topology, hwloc_cpuset_t cpuset,
				       const struct bitmask *bitmask)
{
  hwloc_obj_t node;
  int depth;
  int i;

  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NODE);
  /* cannot be HWLOC_TYPE_DEPTH_MULTIPLE */

  if (depth == HWLOC_TYPE_DEPTH_UNKNOWN) {
    /* if no numa, libnuma assumes we have a single node */
    if (numa_bitmask_isbitset(bitmask, 0))
      hwloc_cpuset_copy(cpuset, hwloc_topology_get_complete_cpuset(topology));
    else
      hwloc_cpuset_zero(cpuset);

  } else {
    hwloc_cpuset_zero(cpuset);
    for(i=0; i<NUMA_NUM_NODES; i++)
      if (numa_bitmask_isbitset(bitmask, i)) {
	node = hwloc_get_obj_by_depth(topology, depth, i);
	if (node)
	  hwloc_cpuset_or(cpuset, cpuset, node->cpuset);
      }
  }

  return 0;
}

/** @} */



#ifdef NUMA_VERSION1_COMPATIBILITY
/** \defgroup hwlocality_linux_libnuma_nodemask Helpers for manipulating Linux libnuma nodemask_t
 * @{
 */


/** \brief Convert hwloc CPU set \p cpuset into libnuma nodemask \p nodemask
 *
 * This function may be used before calling some old libnuma functions
 * that use a nodemask_t as an input parameter.
 */
static __inline int
hwloc_cpuset_to_linux_libnuma_nodemask(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset,
				      nodemask_t *nodemask)
{
  hwloc_obj_t node = NULL;
  unsigned nbnodes = hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_NODE);

  nodemask_zero(nodemask);
  if (nbnodes) {
    while ((node = hwloc_get_next_obj_covering_cpuset_by_type(topology, cpuset, HWLOC_OBJ_NODE, node)) != NULL)
      nodemask_set(nodemask, node->os_index);

  } else {
    /* if no numa, libnuma assumes we have a single node */
    if (!hwloc_cpuset_iszero(cpuset))
      nodemask_set(nodemask, 0);
  }

  return 0;
}

/** \brief Convert libnuma nodemask \p nodemask into hwloc CPU set \p cpuset
 *
 * This function may be used before calling some old libnuma functions
 * that use a nodemask_t as an output parameter.
 */
static __inline int
hwloc_cpuset_from_linux_libnuma_nodemask(hwloc_topology_t topology, hwloc_cpuset_t cpuset,
					const nodemask_t *nodemask)
{
  hwloc_obj_t node;
  int depth;
  int i;

  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NODE);
  /* cannot be HWLOC_TYPE_DEPTH_MULTIPLE */

  if (depth == HWLOC_TYPE_DEPTH_UNKNOWN) {
    /* if no numa, libnuma assumes we have a single node */
    if (nodemask_isset(nodemask, 0))
      hwloc_cpuset_copy(cpuset, hwloc_topology_get_complete_cpuset(topology));
    else
      hwloc_cpuset_zero(cpuset);

  } else {
    hwloc_cpuset_zero(cpuset);
    for(i=0; i<NUMA_NUM_NODES; i++)
      if (nodemask_isset(nodemask, i)) {
	node = hwloc_get_obj_by_depth(topology, depth, i);
	if (node)
	  hwloc_cpuset_or(cpuset, cpuset, node->cpuset);
      }
  }

  return 0;
}

/** @} */
#endif /* NUMA_VERSION1_COMPATIBILITY */


#endif /* HWLOC_LINUX_NUMA_H */
