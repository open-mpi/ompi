/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#include <private/config.h>

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

#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>

#ifdef HAVE_SYS_CPUSET_H
static void
hwloc_freebsd_bsd2hwloc(hwloc_cpuset_t hwloc_cpuset, const cpuset_t *cpuset)
{
  unsigned cpu;
  hwloc_cpuset_zero(hwloc_cpuset);
  for (cpu = 0; cpu < HWLOC_NBMAXCPUS && cpu < CPU_SETSIZE; cpu++)
    if (CPU_ISSET(cpu, cpuset))
      hwloc_cpuset_set(hwloc_cpuset, cpu);
}

static void
hwloc_freebsd_hwloc2bsd(hwloc_const_cpuset_t hwloc_cpuset, cpuset_t *cpuset)
{
  unsigned cpu;
  CPU_ZERO(cpuset);
  for (cpu = 0; cpu < HWLOC_NBMAXCPUS && cpu < CPU_SETSIZE; cpu++)
    if (hwloc_cpuset_isset(hwloc_cpuset, cpu))
      CPU_SET(cpu, cpuset);
}

static int
hwloc_freebsd_set_sth_affinity(hwloc_topology_t topology __hwloc_attribute_unused, cpulevel_t level, cpuwhich_t which, id_t id, hwloc_const_cpuset_t hwloc_cpuset, int policy __hwloc_attribute_unused)
{
  cpuset_t cpuset;

  hwloc_freebsd_hwloc2bsd(hwloc_cpuset, &cpuset);

  if (cpuset_setaffinity(level, which, id, sizeof(cpuset), &cpuset))
    return -1;

  return 0;
}

static int
hwloc_freebsd_get_sth_affinity(hwloc_topology_t topology __hwloc_attribute_unused, cpulevel_t level, cpuwhich_t which, id_t id, hwloc_cpuset_t hwloc_cpuset, int policy __hwloc_attribute_unused)
{
  cpuset_t cpuset;

  if (cpuset_getaffinity(level, which, id, sizeof(cpuset), &cpuset))
    return -1;

  hwloc_freebsd_bsd2hwloc(hwloc_cpuset, &cpuset);
  return 0;
}

static int
hwloc_freebsd_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_cpuset, int policy)
{
  return hwloc_freebsd_set_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_PID, -1, hwloc_cpuset, policy);
}

static int
hwloc_freebsd_get_thisproc_cpubind(hwloc_topology_t topology, hwloc_cpuset_t hwloc_cpuset, int policy)
{
  return hwloc_freebsd_get_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_PID, -1, hwloc_cpuset, policy);
}

static int
hwloc_freebsd_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_cpuset, int policy)
{
  return hwloc_freebsd_set_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_TID, -1, hwloc_cpuset, policy);
}

static int
hwloc_freebsd_get_thisthread_cpubind(hwloc_topology_t topology, hwloc_cpuset_t hwloc_cpuset, int policy)
{
  return hwloc_freebsd_get_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_TID, -1, hwloc_cpuset, policy);
}

static int
hwloc_freebsd_set_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t hwloc_cpuset, int policy)
{
  return hwloc_freebsd_set_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_PID, pid, hwloc_cpuset, policy);
}

static int
hwloc_freebsd_get_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t hwloc_cpuset, int policy)
{
  return hwloc_freebsd_get_sth_affinity(topology, CPU_LEVEL_WHICH, CPU_WHICH_PID, pid, hwloc_cpuset, policy);
}

#ifdef hwloc_thread_t

#if HAVE_DECL_PTHREAD_SETAFFINITY_NP
#pragma weak pthread_setaffinity_np
static int
hwloc_freebsd_set_thread_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_thread_t tid, hwloc_const_cpuset_t hwloc_cpuset, int policy __hwloc_attribute_unused)
{
  int err;
  cpuset_t cpuset;

  if (!pthread_setaffinity_np) {
    errno = ENOSYS;
    return -1;
  }

  hwloc_freebsd_hwloc2bsd(hwloc_cpuset, &cpuset);

  err = pthread_setaffinity_np(tid, sizeof(cpuset), &cpuset);

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
hwloc_freebsd_get_thread_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_thread_t tid, hwloc_cpuset_t hwloc_cpuset, int policy __hwloc_attribute_unused)
{
  int err;
  cpuset_t cpuset;

  if (!pthread_getaffinity_np) {
    errno = ENOSYS;
    return -1;
  }

  err = pthread_getaffinity_np(tid, sizeof(cpuset), &cpuset);

  if (err) {
    errno = err;
    return -1;
  }

  hwloc_freebsd_bsd2hwloc(hwloc_cpuset, &cpuset);
  return 0;
}
#endif
#endif
#endif

void
hwloc_look_freebsd(struct hwloc_topology *topology)
{
  unsigned nbprocs = hwloc_fallback_nbprocessors(topology);

#ifdef HAVE__SC_LARGE_PAGESIZE
  topology->levels[0][0]->attr->machine.huge_page_size_kB = sysconf(_SC_LARGE_PAGESIZE);
#endif

  hwloc_set_freebsd_hooks(topology);
  hwloc_look_x86(topology, nbprocs);

  hwloc_setup_pu_level(topology, nbprocs);
}

void
hwloc_set_freebsd_hooks(struct hwloc_topology *topology)
{
#ifdef HAVE_SYS_CPUSET_H
  topology->set_thisproc_cpubind = hwloc_freebsd_set_thisproc_cpubind;
  topology->get_thisproc_cpubind = hwloc_freebsd_get_thisproc_cpubind;
  topology->set_thisthread_cpubind = hwloc_freebsd_set_thisthread_cpubind;
  topology->get_thisthread_cpubind = hwloc_freebsd_get_thisthread_cpubind;
  topology->set_proc_cpubind = hwloc_freebsd_set_proc_cpubind;
  topology->get_proc_cpubind = hwloc_freebsd_get_proc_cpubind;
#ifdef hwloc_thread_t
#if HAVE_DECL_PTHREAD_SETAFFINITY_NP
  topology->set_thread_cpubind = hwloc_freebsd_set_thread_cpubind;
#endif
#if HAVE_DECL_PTHREAD_GETAFFINITY_NP
  topology->get_thread_cpubind = hwloc_freebsd_get_thread_cpubind;
#endif
#endif
#endif
}
