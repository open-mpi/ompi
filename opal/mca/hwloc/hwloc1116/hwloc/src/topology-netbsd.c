/*
 * Copyright © 2012 Aleksej Saushev, The NetBSD Foundation
 * Copyright © 2009-2014 Inria.  All rights reserved.
 * Copyright © 2009-2010 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#define _NETBSD_SOURCE /* request "_np" functions */

#include <private/autogen/config.h>

#include <sys/types.h>
#include <stdlib.h>
#include <inttypes.h>
#include <sys/param.h>
#include <pthread.h>
#include <sched.h>
#ifdef HAVE_SYS_SYSCTL_H
#include <sys/sysctl.h>
#endif

#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>

static void
hwloc_netbsd_bsd2hwloc(hwloc_bitmap_t hwloc_cpuset, const cpuset_t *cpuset)
{
  unsigned cpu, cpulimit;
  int found = 0;
  hwloc_bitmap_zero(hwloc_cpuset);
  cpulimit = cpuset_size(cpuset) * CHAR_BIT;
  for (cpu = 0; cpu < cpulimit; cpu++)
    if (cpuset_isset(cpu, cpuset)) {
      hwloc_bitmap_set(hwloc_cpuset, cpu);
      found++;
    }
  /* when never bound, it returns an empty set, fill it instead */
  if (!found)
    hwloc_bitmap_fill(hwloc_cpuset);
}

static void
hwloc_netbsd_hwloc2bsd(hwloc_const_bitmap_t hwloc_cpuset, cpuset_t *cpuset)
{
  unsigned cpu, cpulimit;
  cpuset_zero(cpuset);
  cpulimit = cpuset_size(cpuset) * CHAR_BIT;
  for (cpu = 0; cpu < cpulimit; cpu++)
    if (hwloc_bitmap_isset(hwloc_cpuset, cpu))
      cpuset_set(cpu, cpuset);
}

static int
hwloc_netbsd_set_proc_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_pid_t pid, hwloc_const_bitmap_t hwloc_cpuset, int flags __hwloc_attribute_unused)
{
  int status;
  cpuset_t *cpuset = cpuset_create();
  hwloc_netbsd_hwloc2bsd(hwloc_cpuset, cpuset);
  status = sched_setaffinity_np(pid, cpuset_size(cpuset), cpuset);
  cpuset_destroy(cpuset);
  return status;
}

static int
hwloc_netbsd_get_proc_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_pid_t pid, hwloc_bitmap_t hwloc_cpuset, int flags __hwloc_attribute_unused)
{
  int status;
  cpuset_t *cpuset = cpuset_create();
  status = sched_getaffinity_np(pid, cpuset_size(cpuset), cpuset);
  hwloc_netbsd_bsd2hwloc(hwloc_cpuset, cpuset);
  cpuset_destroy(cpuset);
  return status;
}


static int
hwloc_netbsd_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_netbsd_set_proc_cpubind(topology, 0, hwloc_cpuset, flags);
}

static int
hwloc_netbsd_get_thisproc_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_netbsd_get_proc_cpubind(topology, 0, hwloc_cpuset, flags);
}


static int
hwloc_netbsd_set_thread_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_thread_t tid, hwloc_const_bitmap_t hwloc_cpuset, int flags __hwloc_attribute_unused)
{
  int status;
  cpuset_t *cpuset = cpuset_create();
  hwloc_netbsd_hwloc2bsd(hwloc_cpuset, cpuset);
  status = pthread_setaffinity_np(tid, cpuset_size(cpuset), cpuset);
  cpuset_destroy(cpuset);

  if (status) {
    errno = status;
    return -1;
  }
  return 0;
}

static int
hwloc_netbsd_get_thread_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_thread_t tid, hwloc_bitmap_t hwloc_cpuset, int flags __hwloc_attribute_unused)
{
  int status;
  cpuset_t *cpuset = cpuset_create();
  status = pthread_getaffinity_np(tid, cpuset_size(cpuset), cpuset);
  hwloc_netbsd_bsd2hwloc(hwloc_cpuset, cpuset);
  cpuset_destroy(cpuset);

  if (status) {
    errno = status;
    return -1;
  }
  return 0;
}


static int
hwloc_netbsd_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_netbsd_set_thread_cpubind(topology, pthread_self(), hwloc_cpuset, flags);
}

static int
hwloc_netbsd_get_thisthread_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_netbsd_get_thread_cpubind(topology, pthread_self(), hwloc_cpuset, flags);
}

#if (defined HAVE_SYSCTL) && (defined HAVE_SYS_SYSCTL_H)
static void
hwloc_netbsd_node_meminfo_info(struct hwloc_topology *topology)
{
  int mib[2] = { CTL_HW, HW_PHYSMEM64 };
  unsigned long physmem;
  size_t len = sizeof(physmem);
  sysctl(mib, 2, &physmem, &len, NULL, 0);
  topology->levels[0][0]->memory.local_memory = physmem;
}
#endif

static int
hwloc_look_netbsd(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  unsigned nbprocs = hwloc_fallback_nbprocessors(topology);

  if (!topology->levels[0][0]->cpuset) {
    /* Nobody (even the x86 backend) created objects yet, setup basic objects */
    hwloc_alloc_obj_cpusets(topology->levels[0][0]);
    hwloc_setup_pu_level(topology, nbprocs);
  }

  /* Add NetBSD specific information */
#if (defined HAVE_SYSCTL) && (defined HAVE_SYS_SYSCTL_H)
  hwloc_netbsd_node_meminfo_info(topology);
#endif
  hwloc_obj_add_info(topology->levels[0][0], "Backend", "NetBSD");
  if (topology->is_thissystem)
    hwloc_add_uname_info(topology, NULL);
  return 1;
}

void
hwloc_set_netbsd_hooks(struct hwloc_binding_hooks *hooks __hwloc_attribute_unused,
                        struct hwloc_topology_support *support __hwloc_attribute_unused)
{
  hooks->set_proc_cpubind = hwloc_netbsd_set_proc_cpubind;
  hooks->get_proc_cpubind = hwloc_netbsd_get_proc_cpubind;
  hooks->set_thisproc_cpubind = hwloc_netbsd_set_thisproc_cpubind;
  hooks->get_thisproc_cpubind = hwloc_netbsd_get_thisproc_cpubind;

  hooks->set_thread_cpubind = hwloc_netbsd_set_thread_cpubind;
  hooks->get_thread_cpubind = hwloc_netbsd_get_thread_cpubind;
  hooks->set_thisthread_cpubind = hwloc_netbsd_set_thisthread_cpubind;
  hooks->get_thisthread_cpubind = hwloc_netbsd_get_thisthread_cpubind;
}

static struct hwloc_backend *
hwloc_netbsd_component_instantiate(struct hwloc_disc_component *component,
				   const void *_data1 __hwloc_attribute_unused,
				   const void *_data2 __hwloc_attribute_unused,
				   const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_look_netbsd;
  return backend;
}

static struct hwloc_disc_component hwloc_netbsd_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_CPU,
  "netbsd",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_netbsd_component_instantiate,
  50,
  NULL
};

const struct hwloc_component hwloc_netbsd_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_netbsd_disc_component
};
