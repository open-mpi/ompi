/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/** \file
 * \brief High-level hwloc traversal helpers.
 */

#ifndef HWLOC_HELPER_H
#define HWLOC_HELPER_H

#ifndef HWLOC_H
#error Please include the main hwloc.h instead
#endif

#include <stdlib.h>
#include <errno.h>


/** \defgroup hwlocality_helper_types Object Type Helpers
 * @{
 */

/** \brief Returns the depth of objects of type \p type or below
 *
 * If no object of this type is present on the underlying architecture, the
 * function returns the depth of the first "present" object typically found
 * inside \p type.
 */
static __inline int __hwloc_attribute_pure
hwloc_get_type_or_below_depth (hwloc_topology_t topology, hwloc_obj_type_t type)
{
  int depth = hwloc_get_type_depth(topology, type);

  if (depth != HWLOC_TYPE_DEPTH_UNKNOWN)
    return depth;

  /* find the highest existing level with type order >= */
  for(depth = hwloc_get_type_depth(topology, HWLOC_OBJ_PU); ; depth--)
    if (hwloc_compare_types(hwloc_get_depth_type(topology, depth), type) < 0)
      return depth+1;

  /* Shouldn't ever happen, as there is always a SYSTEM level with lower order and known depth.  */
  abort();
}

/** \brief Returns the depth of objects of type \p type or above
 *
 * If no object of this type is present on the underlying architecture, the
 * function returns the depth of the first "present" object typically
 * containing \p type.
 */
static __inline int __hwloc_attribute_pure
hwloc_get_type_or_above_depth (hwloc_topology_t topology, hwloc_obj_type_t type)
{
  int depth = hwloc_get_type_depth(topology, type);

  if (depth != HWLOC_TYPE_DEPTH_UNKNOWN)
    return depth;

  /* find the lowest existing level with type order <= */
  for(depth = 0; ; depth++)
    if (hwloc_compare_types(hwloc_get_depth_type(topology, depth), type) > 0)
      return depth-1;

  /* Shouldn't ever happen, as there is always a PU level with higher order and known depth.  */
  abort();
}

/** @} */



/** \defgroup hwlocality_helper_traversal_basic Basic Traversal Helpers
 * @{
 */

/** \brief Returns the top-object of the topology-tree.
 *
 * Its type is typically ::HWLOC_OBJ_MACHINE but it could be different
 * for complex topologies.  This function replaces the old deprecated
 * hwloc_get_system_obj().
 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_root_obj (hwloc_topology_t topology)
{
  return hwloc_get_obj_by_depth (topology, 0, 0);
}

static __inline hwloc_obj_t __hwloc_attribute_deprecated
hwloc_get_system_obj (hwloc_topology_t topology) { return hwloc_get_root_obj (topology); }

/** \brief Returns the ancestor object of \p obj at depth \p depth. */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_ancestor_obj_by_depth (hwloc_topology_t topology __hwloc_attribute_unused, unsigned depth, hwloc_obj_t obj)
{
  hwloc_obj_t ancestor = obj;
  if (obj->depth < depth)
    return NULL;
  while (ancestor && ancestor->depth > depth)
    ancestor = ancestor->parent;
  return ancestor;
}

/** \brief Returns the ancestor object of \p obj with type \p type. */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_ancestor_obj_by_type (hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_type_t type, hwloc_obj_t obj)
{
  hwloc_obj_t ancestor = obj->parent;
  while (ancestor && ancestor->type != type)
    ancestor = ancestor->parent;
  return ancestor;
}

/** \brief Returns the next object at depth \p depth.
 *
 * If \p prev is \c NULL, return the first object at depth \p depth.
 */
static __inline hwloc_obj_t
hwloc_get_next_obj_by_depth (hwloc_topology_t topology, unsigned depth, hwloc_obj_t prev)
{
  if (!prev)
    return hwloc_get_obj_by_depth (topology, depth, 0);
  if (prev->depth != depth)
    return NULL;
  return prev->next_cousin;
}

/** \brief Returns the next object of type \p type.
 *
 * If \p prev is \c NULL, return the first object at type \p type.  If
 * there are multiple or no depth for given type, return \c NULL and
 * let the caller fallback to hwloc_get_next_obj_by_depth().
 */
static __inline hwloc_obj_t
hwloc_get_next_obj_by_type (hwloc_topology_t topology, hwloc_obj_type_t type,
		   hwloc_obj_t prev)
{
  int depth = hwloc_get_type_depth(topology, type);
  if (depth == HWLOC_TYPE_DEPTH_UNKNOWN || depth == HWLOC_TYPE_DEPTH_MULTIPLE)
    return NULL;
  return hwloc_get_next_obj_by_depth (topology, depth, prev);
}

/** \brief Returns the object of type ::HWLOC_OBJ_PU with \p os_index.
 *
 * \note The \p os_index field of object should most of the times only be
 * used for pretty-printing purpose. Type ::HWLOC_OBJ_PU is the only case
 * where \p os_index could actually be useful, when manually binding to
 * processors.
 * However, using CPU sets to hide this complexity should often be preferred.
 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_pu_obj_by_os_index(hwloc_topology_t topology, unsigned os_index)
{
  hwloc_obj_t obj = NULL;
  while ((obj = hwloc_get_next_obj_by_type(topology, HWLOC_OBJ_PU, obj)) != NULL)
    if (obj->os_index == os_index)
      return obj;
  return NULL;
}

/** \brief Return the next child.
 *
 * If \p prev is \c NULL, return the first child.
 */
static __inline hwloc_obj_t
hwloc_get_next_child (hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t parent, hwloc_obj_t prev)
{
  if (!prev)
    return parent->first_child;
  if (prev->parent != parent)
    return NULL;
  return prev->next_sibling;
}

/** \brief Returns the common parent object to objects lvl1 and lvl2 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_common_ancestor_obj (hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj1, hwloc_obj_t obj2)
{
  while (obj1->depth > obj2->depth)
    obj1 = obj1->parent;
  while (obj2->depth > obj1->depth)
    obj2 = obj2->parent;
  while (obj1 != obj2) {
    obj1 = obj1->parent;
    obj2 = obj2->parent;
  }
  return obj1;
}

/** \brief Returns true if _obj_ is inside the subtree beginning
    with \p subtree_root. */
static __inline int __hwloc_attribute_pure
hwloc_obj_is_in_subtree (hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj, hwloc_obj_t subtree_root)
{
  return hwloc_cpuset_isincluded(obj->cpuset, subtree_root->cpuset);
}

/** @} */



/** \defgroup hwlocality_helper_find_inside Finding Objects Inside a CPU set
 * @{
 */

/** \brief Get the first largest object included in the given cpuset \p set.
 *
 * \return the first object that is included in \p set and whose parent is not.
 *
 * This is convenient for iterating over all largest objects within a CPU set
 * by doing a loop getting the first largest object and clearing its CPU set
 * from the remaining CPU set.
 */
static __inline hwloc_obj_t
hwloc_get_first_largest_obj_inside_cpuset(hwloc_topology_t topology, hwloc_const_cpuset_t set)
{
  hwloc_obj_t obj = hwloc_get_root_obj(topology);
  if (!hwloc_cpuset_intersects(obj->cpuset, set))
    return NULL;
  while (!hwloc_cpuset_isincluded(obj->cpuset, set)) {
    /* while the object intersects without being included, look at its children */
    hwloc_obj_t child = NULL;
    while ((child = hwloc_get_next_child(topology, obj, child)) != NULL) {
      if (hwloc_cpuset_intersects(child->cpuset, set))
	break;
    }
    if (!child)
      /* no child intersects, return their father */
      return obj;
    /* found one intersecting child, look at its children */
    obj = child;
  }
  /* obj is included, return it */
  return obj;
}

/** \brief Get the set of largest objects covering exactly a given cpuset \p set
 *
 * \return the number of objects returned in \p objs.
 */
HWLOC_DECLSPEC int hwloc_get_largest_objs_inside_cpuset (hwloc_topology_t topology, hwloc_const_cpuset_t set,
						 hwloc_obj_t * __hwloc_restrict objs, int max);

/** \brief Return the next object at depth \p depth included in CPU set \p set.
 *
 * If \p prev is \c NULL, return the first object at depth \p depth
 * included in \p set.  The next invokation should pass the previous
 * return value in \p prev so as to obtain the next object in \p set.
 */
static __inline hwloc_obj_t
hwloc_get_next_obj_inside_cpuset_by_depth (hwloc_topology_t topology, hwloc_const_cpuset_t set,
					   unsigned depth, hwloc_obj_t prev)
{
  hwloc_obj_t next = hwloc_get_next_obj_by_depth(topology, depth, prev);
  while (next && !hwloc_cpuset_isincluded(next->cpuset, set))
    next = next->next_cousin;
  return next;
}

/** \brief Return the next object of type \p type included in CPU set \p set.
 *
 * If there are multiple or no depth for given type, return \c NULL
 * and let the caller fallback to
 * hwloc_get_next_obj_inside_cpuset_by_depth().
 */
static __inline hwloc_obj_t
hwloc_get_next_obj_inside_cpuset_by_type (hwloc_topology_t topology, hwloc_const_cpuset_t set,
					  hwloc_obj_type_t type, hwloc_obj_t prev)
{
  int depth = hwloc_get_type_depth(topology, type);
  if (depth == HWLOC_TYPE_DEPTH_UNKNOWN || depth == HWLOC_TYPE_DEPTH_MULTIPLE)
    return NULL;
  return hwloc_get_next_obj_inside_cpuset_by_depth(topology, set, depth, prev);
}

/** \brief Return the \p index -th object at depth \p depth included in CPU set \p set.
 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_obj_inside_cpuset_by_depth (hwloc_topology_t topology, hwloc_const_cpuset_t set,
				      unsigned depth, unsigned idx)
{
  unsigned count = 0;
  hwloc_obj_t obj = hwloc_get_obj_by_depth (topology, depth, 0);
  while (obj) {
    if (hwloc_cpuset_isincluded(obj->cpuset, set)) {
      if (count == idx)
	return obj;
      count++;
    }
    obj = obj->next_cousin;
  }
  return NULL;
}

/** \brief Return the \p idx -th object of type \p type included in CPU set \p set.
 *
 * If there are multiple or no depth for given type, return \c NULL
 * and let the caller fallback to
 * hwloc_get_obj_inside_cpuset_by_depth().
 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_obj_inside_cpuset_by_type (hwloc_topology_t topology, hwloc_const_cpuset_t set,
				     hwloc_obj_type_t type, unsigned idx)
{
  int depth = hwloc_get_type_depth(topology, type);
  if (depth == HWLOC_TYPE_DEPTH_UNKNOWN || depth == HWLOC_TYPE_DEPTH_MULTIPLE)
    return NULL;
  return hwloc_get_obj_inside_cpuset_by_depth(topology, set, depth, idx);
}

/** \brief Return the number of objects at depth \p depth included in CPU set \p set. */
static __inline unsigned __hwloc_attribute_pure
hwloc_get_nbobjs_inside_cpuset_by_depth (hwloc_topology_t topology, hwloc_const_cpuset_t set,
					 unsigned depth)
{
  hwloc_obj_t obj = hwloc_get_obj_by_depth (topology, depth, 0);
  int count = 0;
  while (obj) {
    if (hwloc_cpuset_isincluded(obj->cpuset, set))
      count++;
    obj = obj->next_cousin;
  }
  return count;
}

/** \brief Return the number of objects of type \p type included in CPU set \p set.
 *
 * If no object for that type exists inside CPU set \p set, 0 is
 * returned.  If there are several levels with objects of that type
 * inside CPU set \p set, -1 is returned.
 */
static __inline int __hwloc_attribute_pure
hwloc_get_nbobjs_inside_cpuset_by_type (hwloc_topology_t topology, hwloc_const_cpuset_t set,
					hwloc_obj_type_t type)
{
  int depth = hwloc_get_type_depth(topology, type);
  if (depth == HWLOC_TYPE_DEPTH_UNKNOWN)
    return 0;
  if (depth == HWLOC_TYPE_DEPTH_MULTIPLE)
    return -1; /* FIXME: agregate nbobjs from different levels? */
  return hwloc_get_nbobjs_inside_cpuset_by_depth(topology, set, depth);
}

/** @} */



/** \defgroup hwlocality_helper_find_covering Finding a single Object covering at least CPU set
 * @{
 */

/** \brief Get the child covering at least CPU set \p set.
 *
 * \return \c NULL if no child matches or if \p set is empty.
 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_child_covering_cpuset (hwloc_topology_t topology __hwloc_attribute_unused, hwloc_const_cpuset_t set,
				hwloc_obj_t parent)
{
  hwloc_obj_t child;

  if (hwloc_cpuset_iszero(set))
    return NULL;

  child = parent->first_child;
  while (child) {
    if (hwloc_cpuset_isincluded(set, child->cpuset))
      return child;
    child = child->next_sibling;
  }
  return NULL;
}

/** \brief Get the lowest object covering at least CPU set \p set
 *
 * \return \c NULL if no object matches or if \p set is empty.
 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_obj_covering_cpuset (hwloc_topology_t topology, hwloc_const_cpuset_t set)
{
  struct hwloc_obj *current = hwloc_get_root_obj(topology);

  if (hwloc_cpuset_iszero(set))
    return NULL;

  if (!hwloc_cpuset_isincluded(set, current->cpuset))
    return NULL;

  while (1) {
    hwloc_obj_t child = hwloc_get_child_covering_cpuset(topology, set, current);
    if (!child)
      return current;
    current = child;
  }
}


/** @} */



/** \defgroup hwlocality_helper_find_coverings Finding a set of similar Objects covering at least a CPU set
 * @{
 */

/** \brief Iterate through same-depth objects covering at least CPU set \p set
 *
 * If object \p prev is \c NULL, return the first object at depth \p
 * depth covering at least part of CPU set \p set.  The next
 * invokation should pass the previous return value in \p prev so as
 * to obtain the next object covering at least another part of \p set.
 */
static __inline hwloc_obj_t
hwloc_get_next_obj_covering_cpuset_by_depth(hwloc_topology_t topology, hwloc_const_cpuset_t set,
					    unsigned depth, hwloc_obj_t prev)
{
  hwloc_obj_t next = hwloc_get_next_obj_by_depth(topology, depth, prev);
  while (next && !hwloc_cpuset_intersects(set, next->cpuset))
    next = next->next_cousin;
  return next;
}

/** \brief Iterate through same-type objects covering at least CPU set \p set
 *
 * If object \p prev is \c NULL, return the first object of type \p
 * type covering at least part of CPU set \p set.  The next invokation
 * should pass the previous return value in \p prev so as to obtain
 * the next object of type \p type covering at least another part of
 * \p set.
 *
 * If there are no or multiple depths for type \p type, \c NULL is returned.
 * The caller may fallback to hwloc_get_next_obj_covering_cpuset_by_depth()
 * for each depth.
 */
static __inline hwloc_obj_t
hwloc_get_next_obj_covering_cpuset_by_type(hwloc_topology_t topology, hwloc_const_cpuset_t set,
					   hwloc_obj_type_t type, hwloc_obj_t prev)
{
  int depth = hwloc_get_type_depth(topology, type);
  if (depth == HWLOC_TYPE_DEPTH_UNKNOWN || depth == HWLOC_TYPE_DEPTH_MULTIPLE)
    return NULL;
  return hwloc_get_next_obj_covering_cpuset_by_depth(topology, set, depth, prev);
}

/** @} */



/** \defgroup hwlocality_helper_find_cache Cache-specific Finding Helpers
 * @{
 */

/** \brief Get the first cache covering a cpuset \p set
 *
 * \return \c NULL if no cache matches
 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_cache_covering_cpuset (hwloc_topology_t topology, hwloc_const_cpuset_t set)
{
  hwloc_obj_t current = hwloc_get_obj_covering_cpuset(topology, set);
  while (current) {
    if (current->type == HWLOC_OBJ_CACHE)
      return current;
    current = current->parent;
  }
  return NULL;
}

/** \brief Get the first cache shared between an object and somebody else
 *
 * \return \c NULL if no cache matches
 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_shared_cache_covering_obj (hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj)
{
  hwloc_obj_t current = obj->parent;
  while (current) {
    if (!hwloc_cpuset_isequal(current->cpuset, obj->cpuset)
        && current->type == HWLOC_OBJ_CACHE)
      return current;
    current = current->parent;
  }
  return NULL;
}

/** @} */



/** \defgroup hwlocality_helper_traversal Advanced Traversal Helpers
 * @{
 */

/** \brief Do a depth-first traversal of the topology to find and sort
 *
 *  all objects that are at the same depth than \p src.
 *  Report in \p objs up to \p max physically closest ones to \p src.
 *
 *  \return the number of objects returned in \p objs.
 */
/* TODO: rather provide an iterator? Provide a way to know how much should be allocated? By returning the total number of objects instead? */
HWLOC_DECLSPEC unsigned hwloc_get_closest_objs (hwloc_topology_t topology, hwloc_obj_t src, hwloc_obj_t * __hwloc_restrict objs, unsigned max);

/** \brief Find an object below another object, both specified by types and indexes.
 *
 * Start from the top system object and find object of type \p type1
 * and index \p idx1.  Then look below this object and find another
 * object of type \p type2 and index \p idx2.  Indexes are specified
 * within the parent, not withing the entire system.
 *
 * For instance, if type1 is SOCKET, idx1 is 2, type2 is CORE and idx2
 * is 3, return the fourth core object below the third socket.
 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_obj_below_by_type (hwloc_topology_t topology,
			     hwloc_obj_type_t type1, unsigned idx1,
			     hwloc_obj_type_t type2, unsigned idx2)
{
  hwloc_obj_t obj;

  obj = hwloc_get_obj_by_type (topology, type1, idx1);
  if (!obj)
    return NULL;

  return hwloc_get_obj_inside_cpuset_by_type(topology, obj->cpuset, type2, idx2);
}

/** \brief Find an object below a chain of objects specified by types and indexes.
 *
 * This is a generalized version of hwloc_get_obj_below_by_type().
 *
 * Arrays \p typev and \p idxv must contain \p nr types and indexes.
 *
 * Start from the top system object and walk the arrays \p typev and \p idxv.
 * For each type and index couple in the arrays, look under the previously found
 * object to find the index-th object of the given type.
 * Indexes are specified within the parent, not withing the entire system.
 *
 * For instance, if nr is 3, typev contains NODE, SOCKET and CORE,
 * and idxv contains 0, 1 and 2, return the third core object below
 * the second socket below the first NUMA node.
 */
static __inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_obj_below_array_by_type (hwloc_topology_t topology, int nr, hwloc_obj_type_t *typev, unsigned *idxv)
{
  hwloc_obj_t obj = hwloc_get_root_obj(topology);
  int i;

  for(i=0; i<nr; i++) {
    obj = hwloc_get_obj_inside_cpuset_by_type(topology, obj->cpuset, typev[i], idxv[i]);
    if (!obj)
      return NULL;
  }

  return obj;
}

/** @} */



/** \defgroup hwlocality_helper_binding Binding Helpers
 * @{
 */

/** \brief Distribute \p n items over the topology under \p root
 *
 * Array \p cpuset will be filled with \p n cpusets distributed linearly over
 * the topology under \p root .
 *
 * This is typically useful when an application wants to distribute \p n
 * threads over a machine, giving each of them as much private cache as
 * possible and keeping them locally in number order.
 *
 * The caller may typically want to also call hwloc_cpuset_singlify()
 * before binding a thread so that it does not move at all.
 */
static __inline void
hwloc_distribute(hwloc_topology_t topology, hwloc_obj_t root, hwloc_cpuset_t *cpuset, unsigned n)
{
  unsigned i;
  unsigned u;
  unsigned chunk_size, complete_chunks;
  hwloc_cpuset_t *cpusetp;

  if (!root->arity || n == 1) {
    /* Got to the bottom, we can't split any more, put everything there.  */
    for (i=0; i<n; i++)
      cpuset[i] = hwloc_cpuset_dup(root->cpuset);
    return;
  }

  /* Divide n in root->arity chunks.  */
  chunk_size = (n + root->arity - 1) / root->arity;
  complete_chunks = n % root->arity;
  if (!complete_chunks)
    complete_chunks = root->arity;

  /* Allocate complete chunks first.  */
  for (cpusetp = cpuset, i = 0;
       i < complete_chunks;
       i ++, cpusetp += chunk_size)
    hwloc_distribute(topology, root->children[i], cpusetp, chunk_size);

  /* Now allocate not-so-complete chunks.  */
  for (u = i;
       u < root->arity;
       u++, cpusetp += chunk_size-1)
    hwloc_distribute(topology, root->children[u], cpusetp, chunk_size-1);
}

/** @} */

/** \defgroup hwlocality_helper_cpuset Cpuset Helpers
 * @{
 */
/* \brief Get complete CPU set
 *
 * \return the complete CPU set of logical processors of the system. If the
 * topology is the result of a combination of several systems, NULL is
 * returned.
 *
 * \note The returned cpuset is not newly allocated and should thus not be
 * changed or freed; hwloc_cpuset_dup must be used to obtain a local copy.
 */
static __inline hwloc_const_cpuset_t __hwloc_attribute_pure
hwloc_topology_get_complete_cpuset(hwloc_topology_t topology)
{
  return hwloc_get_root_obj(topology)->complete_cpuset;
}

/* \brief Get topology CPU set
 *
 * \return the CPU set of logical processors of the system for which hwloc
 * provides topology information. This is equivalent to the cpuset of the
 * system object. If the topology is the result of a combination of several
 * systems, NULL is returned.
 *
 * \note The returned cpuset is not newly allocated and should thus not be
 * changed or freed; hwloc_cpuset_dup must be used to obtain a local copy.
 */
static __inline hwloc_const_cpuset_t __hwloc_attribute_pure
hwloc_topology_get_topology_cpuset(hwloc_topology_t topology)
{
  return hwloc_get_root_obj(topology)->cpuset;
}

/** \brief Get online CPU set
 *
 * \return the CPU set of online logical processors of the system. If the
 * topology is the result of a combination of several systems, NULL is
 * returned.
 *
 * \note The returned cpuset is not newly allocated and should thus not be
 * changed or freed; hwloc_cpuset_dup must be used to obtain a local copy.
 */
static __inline hwloc_const_cpuset_t __hwloc_attribute_pure
hwloc_topology_get_online_cpuset(hwloc_topology_t topology)
{
  return hwloc_get_root_obj(topology)->online_cpuset;
}

/** \brief Get allowed CPU set
 *
 * \return the CPU set of allowed logical processors of the system. If the
 * topology is the result of a combination of several systems, NULL is
 * returned.
 *
 * \note The returned cpuset is not newly allocated and should thus not be
 * changed or freed, hwloc_cpuset_dup must be used to obtain a local copy.
 */
static __inline hwloc_const_cpuset_t __hwloc_attribute_pure
hwloc_topology_get_allowed_cpuset(hwloc_topology_t topology)
{
  return hwloc_get_root_obj(topology)->allowed_cpuset;
}


/** @} */

#endif /* HWLOC_HELPER_H */
