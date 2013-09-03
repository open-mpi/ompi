/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2013 Inria.  All rights reserved.
 * Copyright © 2009-2010, 2012 Université Bordeaux 1
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>

#include <sys/types.h>
#include <stdlib.h>
#include <inttypes.h>
#include <sys/param.h>
#include <pthread.h>
#ifdef HAVE_PTHREAD_NP_H
#include <pthread_np.h>
#endif
#ifdef HAVE_SYS_CPUSET_H
#include <sys/cpuset.h>
#endif
#ifdef HAVE_SYS_SYSCTL_H
#include <sys/sysctl.h>
#endif

#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>

#if defined(HAVE_SYS_CPUSET_H) && defined(HAVE_CPUSET_SETAFFINITY)
static void
hwloc_freebsd_bsd2hwloc(hwloc_bitmap_t hwloc_cpuset, const cpuset_t *cset)
{
  unsigned cpu;
  hwloc_bitmap_zero(hwloc_cpuset);
  for (cpu = 0; cpu < CPU_SETSIZE; cpu++)
    if (CPU_ISSET(cpu, cset))
      hwloc_bitmap_set(hwloc_cpuset, cpu);
}

static void
hwloc_freebsd_hwloc2bsd(hwloc_const_bitmap_t hwloc_cpuset, cpuset_t *cset)
{
  unsigned cpu;
  CPU_ZERO(cset);
  for (cpu = 0; cpu < CPU_SETSIZE; cpu++)
    if (hwloc_bitmap_isset(hwloc_cpuset, cpu))
      CPU_SET(cpu, cset);
}

static int
hwloc_freebsd_set_sth_affinity(hwloc_topology_t topology __hwloc_attribute_unused, cpulevel_t level, cpuwhich_t which, id_t id, hwloc_const_bitmap_t hwloc_cpuset, int flags __hwloc_attribute_unused)
{
  cpuset_t cset;

  hwloc_freebsd_hwloc2bsd(hwloc_cpuset, &cset);

  if (cpuset_setaffinity(level, which, id, sizeof(cset), &cset))
    return -1;

  return 0;
}

static int
hwloc_freebsd_get_sth_affinity(hwloc_topology_t topology __hwloc_attribute_unused, cpulevel_t level, cpuwhich_t which, id_t id, hwloc_bitmap_t hwloc_cpuset, int flags __hwloc_attribute_unused)
{
  cpuset_t cset;

  if (cpuset_getaffinity(level, which, id, sizeof(cset), &cset))
    return -1;

  hwloc_freebsd_bsd2hwloc(hwloc_cpuset, &cset);
  return 0;
}

static int
hwloc_freebsd_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_freebsd_set_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_PID, -1, hwloc_cpuset, flags);
}

static int
hwloc_freebsd_get_thisproc_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_freebsd_get_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_PID, -1, hwloc_cpuset, flags);
}

static int
hwloc_freebsd_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_freebsd_set_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_TID, -1, hwloc_cpuset, flags);
}

static int
hwloc_freebsd_get_thisthread_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_freebsd_get_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_TID, -1, hwloc_cpuset, flags);
}

static int
hwloc_freebsd_set_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_freebsd_set_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_PID, pid, hwloc_cpuset, flags);
}

static int
hwloc_freebsd_get_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_freebsd_get_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_PID, pid, hwloc_cpuset, flags);
}

#ifdef hwloc_thread_t

#if HAVE_DECL_PTHREAD_SETAFFINITY_NP
#pragma weak pthread_setaffinity_np
static int
hwloc_freebsd_set_thread_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_thread_t tid, hwloc_const_bitmap_t hwloc_cpuset, int flags __hwloc_attribute_unused)
{
  int err;
  cpuset_t cset;

  if (!pthread_setaffinity_np) {
    errno = ENOSYS;
    return -1;
  }

  hwloc_freebsd_hwloc2bsd(hwloc_cpuset, &cset);

  err = pthread_setaffinity_np(tid, sizeof(cset), &cset);

  if (err) {
    errno = err;
    return -1;
  }

  return 0;
}
#endif

#if HAVE_DECL_PTHREAD_GETAFFINITY_NP
#pragma weak pthread_getaffinity_np
static int
hwloc_freebsd_get_thread_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_thread_t tid, hwloc_bitmap_t hwloc_cpuset, int flags __hwloc_attribute_unused)
{
  int err;
  cpuset_t cset;

  if (!pthread_getaffinity_np) {
    errno = ENOSYS;
    return -1;
  }

  err = pthread_getaffinity_np(tid, sizeof(cset), &cset);

  if (err) {
    errno = err;
    return -1;
  }

  hwloc_freebsd_bsd2hwloc(hwloc_cpuset, &cset);
  return 0;
}
#endif
#endif
#endif

#if (defined HAVE_SYSCTL) && (defined HAVE_SYS_SYSCTL_H)
static void
hwloc_freebsd_node_meminfo_info(struct hwloc_topology *topology)
{
       int mib[2] = { CTL_HW, HW_PHYSMEM };
       size_t len = sizeof(topology->levels[0][0]->memory.local_memory);
       sysctl(mib, 2, &topology->levels[0][0]->memory.local_memory, &len, NULL, 0);
}
#endif

static int
hwloc_look_freebsd(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  unsigned nbprocs = hwloc_fallback_nbprocessors(topology);

  if (!topology->levels[0][0]->cpuset) {
    /* Nobody (even the x86 backend) created objects yet, setup basic objects */
    hwloc_alloc_obj_cpusets(topology->levels[0][0]);
    hwloc_setup_pu_level(topology, nbprocs);
  }

  /* Add FreeBSD specific information */
#if (defined HAVE_SYSCTL) && (defined HAVE_SYS_SYSCTL_H)
  hwloc_freebsd_node_meminfo_info(topology);
#endif
  hwloc_obj_add_info(topology->levels[0][0], "Backend", "FreeBSD");
  if (topology->is_thissystem)
    hwloc_add_uname_info(topology);
  return 1;
}

void
hwloc_set_freebsd_hooks(struct hwloc_binding_hooks *hooks __hwloc_attribute_unused,
			struct hwloc_topology_support *support __hwloc_attribute_unused)
{
#if defined(HAVE_SYS_CPUSET_H) && defined(HAVE_CPUSET_SETAFFINITY)
  hooks->set_thisproc_cpubind = hwloc_freebsd_set_thisproc_cpubind;
  hooks->get_thisproc_cpubind = hwloc_freebsd_get_thisproc_cpubind;
  hooks->set_thisthread_cpubind = hwloc_freebsd_set_thisthread_cpubind;
  hooks->get_thisthread_cpubind = hwloc_freebsd_get_thisthread_cpubind;
  hooks->set_proc_cpubind = hwloc_freebsd_set_proc_cpubind;
  hooks->get_proc_cpubind = hwloc_freebsd_get_proc_cpubind;
#ifdef hwloc_thread_t
#if HAVE_DECL_PTHREAD_SETAFFINITY_NP
  hooks->set_thread_cpubind = hwloc_freebsd_set_thread_cpubind;
#endif
#if HAVE_DECL_PTHREAD_GETAFFINITY_NP
  hooks->get_thread_cpubind = hwloc_freebsd_get_thread_cpubind;
#endif
#endif
#endif
  /* TODO: get_last_cpu_location: find out ki_lastcpu */
}

static struct hwloc_backend *
hwloc_freebsd_component_instantiate(struct hwloc_disc_component *component,
				    const void *_data1 __hwloc_attribute_unused,
				    const void *_data2 __hwloc_attribute_unused,
				    const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_look_freebsd;
  return backend;
}

static struct hwloc_disc_component hwloc_freebsd_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_CPU,
  "freebsd",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_freebsd_component_instantiate,
  50,
  NULL
};

const struct hwloc_component hwloc_freebsd_component = {
  HWLOC_COMPONENT_ABI,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_freebsd_disc_component
};
