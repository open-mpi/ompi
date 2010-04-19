/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#define _GNU_SOURCE
#include <sched.h>
#include <assert.h>
#include <private/config.h>
#include <hwloc.h>
#include <hwloc/glibc-sched.h>

/* check the linux libnuma helpers */

int main(void)
{
  hwloc_topology_t topology;
  unsigned depth;
  hwloc_cpuset_t hwlocset;
  cpu_set_t schedset;
  hwloc_obj_t obj;
  int err;

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);
  depth = hwloc_topology_get_depth(topology);

  hwlocset = hwloc_cpuset_dup(hwloc_topology_get_complete_cpuset(topology));
  hwloc_cpuset_to_glibc_sched_affinity(topology, hwlocset, &schedset, sizeof(schedset));
#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_setaffinity(0, sizeof(schedset));
#else
  err = sched_setaffinity(0, sizeof(schedset), &schedset);
#endif
  assert(!err);
  hwloc_cpuset_free(hwlocset);

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_getaffinity(0, sizeof(schedset));
#else
  err = sched_getaffinity(0, sizeof(schedset), &schedset);
#endif
  assert(!err);
  hwlocset = hwloc_cpuset_alloc();
  hwloc_cpuset_from_glibc_sched_affinity(topology, hwlocset, &schedset, sizeof(schedset));
  assert(hwloc_cpuset_isincluded(hwlocset, hwloc_topology_get_complete_cpuset(topology)));
  hwloc_cpuset_andnot(hwlocset, hwlocset, hwloc_topology_get_online_cpuset(topology));
  hwloc_cpuset_andnot(hwlocset, hwlocset, hwloc_topology_get_allowed_cpuset(topology));
  assert(hwloc_cpuset_iszero(hwlocset));
  hwloc_cpuset_free(hwlocset);

  obj = hwloc_get_obj_by_depth(topology, depth-1, hwloc_get_nbobjs_by_depth(topology, depth-1) - 1);
  assert(obj);
  assert(obj->type == HWLOC_OBJ_PU);

  hwlocset = hwloc_cpuset_dup(obj->cpuset);
  hwloc_cpuset_to_glibc_sched_affinity(topology, hwlocset, &schedset, sizeof(schedset));
#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_setaffinity(0, sizeof(schedset));
#else
  err = sched_setaffinity(0, sizeof(schedset), &schedset);
#endif
  assert(!err);
  hwloc_cpuset_free(hwlocset);

#ifdef HWLOC_HAVE_OLD_SCHED_SETAFFINITY
  err = sched_getaffinity(0, sizeof(schedset));
#else
  err = sched_getaffinity(0, sizeof(schedset), &schedset);
#endif
  assert(!err);
  hwlocset = hwloc_cpuset_alloc();
  hwloc_cpuset_from_glibc_sched_affinity(topology, hwlocset, &schedset, sizeof(schedset));
  assert(hwloc_cpuset_isequal(hwlocset, obj->cpuset));
  hwloc_cpuset_free(hwlocset);

  hwloc_topology_destroy(topology);
  return 0;
}
