/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2014 Inria.  All rights reserved.
 * Copyright © 2009-2012 Université Bordeaux
 * Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/**
 * This file contains the inline code of functions declared in hwloc.h
 */

#ifndef HWLOC_DEPRECATED_H
#define HWLOC_DEPRECATED_H

#ifndef HWLOC_H
#error Please include the main hwloc.h instead
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* backward compat with v1.10 before Socket->Package renaming */
#define HWLOC_OBJ_SOCKET HWLOC_OBJ_PACKAGE
/* backward compat with v1.10 before Node->NUMANode clarification */
#define HWLOC_OBJ_NODE HWLOC_OBJ_NUMANODE

/** \brief Return an object type from the string
 *
 * \return -1 if unrecognized.
 */
HWLOC_DECLSPEC hwloc_obj_type_t hwloc_obj_type_of_string (const char * string) __hwloc_attribute_pure __hwloc_attribute_deprecated;

/** \brief Stringify a given topology object into a human-readable form.
 *
 * \note This function is deprecated in favor of hwloc_obj_type_snprintf()
 * and hwloc_obj_attr_snprintf() since it is not very flexible and
 * only prints physical/OS indexes.
 *
 * Fill string \p string up to \p size characters with the description
 * of topology object \p obj in topology \p topology.
 *
 * If \p verbose is set, a longer description is used. Otherwise a
 * short description is used.
 *
 * \p indexprefix is used to prefix the \p os_index attribute number of
 * the object in the description. If \c NULL, the \c # character is used.
 *
 * If \p size is 0, \p string may safely be \c NULL.
 *
 * \return the number of character that were actually written if not truncating,
 * or that would have been written (not including the ending \\0).
 */
HWLOC_DECLSPEC int hwloc_obj_snprintf(char * __hwloc_restrict string, size_t size,
				      hwloc_topology_t topology, hwloc_obj_t obj,
				      const char * __hwloc_restrict indexprefix, int verbose) __hwloc_attribute_deprecated;

/** \brief Distribute \p n items over the topology under \p root
 *
 * Array \p cpuset will be filled with \p n cpusets recursively distributed
 * linearly over the topology under \p root, down to depth \p until (which can
 * be INT_MAX to distribute down to the finest level).
 *
 * This is typically useful when an application wants to distribute \p n
 * threads over a machine, giving each of them as much private cache as
 * possible and keeping them locally in number order.
 *
 * The caller may typically want to also call hwloc_bitmap_singlify()
 * before binding a thread so that it does not move at all.
 *
 * \note This function requires the \p root object to have a CPU set.
 */
static __hwloc_inline void
hwloc_distribute(hwloc_topology_t topology, hwloc_obj_t root, hwloc_cpuset_t *set, unsigned n, unsigned until) __hwloc_attribute_deprecated;
static __hwloc_inline void
hwloc_distribute(hwloc_topology_t topology, hwloc_obj_t root, hwloc_cpuset_t *set, unsigned n, unsigned until)
{
  hwloc_distrib(topology, &root, 1, set, n, until, 0);
}

/** \brief Distribute \p n items over the topology under \p roots
 *
 * This is the same as hwloc_distribute(), but takes an array of roots instead of
 * just one root.
 *
 * \note This function requires the \p roots objects to have a CPU set.
 */
static __hwloc_inline void
hwloc_distributev(hwloc_topology_t topology, hwloc_obj_t *roots, unsigned n_roots, hwloc_cpuset_t *set, unsigned n, unsigned until) __hwloc_attribute_deprecated;
static __hwloc_inline void
hwloc_distributev(hwloc_topology_t topology, hwloc_obj_t *roots, unsigned n_roots, hwloc_cpuset_t *set, unsigned n, unsigned until)
{
  hwloc_distrib(topology, roots, n_roots, set, n, until, 0);
}

#ifdef __cplusplus
} /* extern "C" */
#endif


#endif /* HWLOC_INLINES_H */
