/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009, 2017 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#define _GNU_SOURCE
#include <sched.h>
#include <assert.h>
#include <private/autogen/config.h> /* for HWLOC_HAVE_OLD_SCHED_SETAFFINITY */
#include <hwloc.h>
#include <hwloc/glibc-sched.h>

/* check the linux libnuma helpers */

int main(void)
{
  hwloc_topology_t topology;
#ifdef HWLOC_HAVE_CPU_SET
  int depth;
  hwloc_bitmap_t hwlocset;
  cpu_set_t schedset;
  hwloc_obj_t obj;
  int err;
#endif /* HWLOC_HAVE_CPU_SET */

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);

#ifdef HWLOC_HAVE_CPU_SET

  depth = hwloc_topology_get_depth(topology);

  hwlocset = hwloc_bitmap_dup(hwloc_topology_get_complete_cpuset(topology));
  hwloc_cpuset_to_glibc_sched_affinity(topology, hwlocset, &schedset, sizeof(schedset));
#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_setaffinity(0, &schedset);
#else
  err = sched_setaffinity(0, sizeof(schedset), &schedset);
#endif
  assert(!err);
  hwloc_bitmap_free(hwlocset);

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_getaffinity(0, &schedset);
#else
  err = sched_getaffinity(0, sizeof(schedset), &schedset);
#endif
  assert(!err);
  hwlocset = hwloc_bitmap_alloc();
  hwloc_cpuset_from_glibc_sched_affinity(topology, hwlocset, &schedset, sizeof(schedset));
  assert(hwloc_bitmap_isincluded(hwlocset, hwloc_topology_get_complete_cpuset(topology)));
  hwloc_bitmap_andnot(hwlocset, hwlocset, hwloc_topology_get_allowed_cpuset(topology));
  assert(hwloc_bitmap_iszero(hwlocset));
  hwloc_bitmap_free(hwlocset);

  obj = hwloc_get_obj_by_depth(topology, depth-1, hwloc_get_nbobjs_by_depth(topology, depth-1) - 1);
  assert(obj);
  assert(obj->type == HWLOC_OBJ_PU);

  hwlocset = hwloc_bitmap_dup(obj->cpuset);
  hwloc_cpuset_to_glibc_sched_affinity(topology, hwlocset, &schedset, sizeof(schedset));
#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_setaffinity(0, &schedset);
#else
  err = sched_setaffinity(0, sizeof(schedset), &schedset);
#endif
  assert(!err);
  hwloc_bitmap_free(hwlocset);

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_getaffinity(0, &schedset);
#else
  err = sched_getaffinity(0, sizeof(schedset), &schedset);
#endif
  assert(!err);
  hwlocset = hwloc_bitmap_alloc();
  hwloc_cpuset_from_glibc_sched_affinity(topology, hwlocset, &schedset, sizeof(schedset));
  assert(hwloc_bitmap_isequal(hwlocset, obj->cpuset));
  hwloc_bitmap_free(hwlocset);

#endif /* HWLOC_HAVE_CPU_SET */

  hwloc_topology_destroy(topology);
  return 0;
}
