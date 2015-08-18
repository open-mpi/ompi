/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2013 Inria.  All rights reserved.
 * Copyright © 2009-2011 Université Bordeaux
 * See COPYING in top-level directory.
 */

/** \file
 * \brief Macros to help interaction between hwloc and Linux.
 *
 * Applications that use hwloc on Linux may want to include this file
 * if using some low-level Linux features.
 */

#ifndef HWLOC_LINUX_H
#define HWLOC_LINUX_H

#include <hwloc.h>
#include <stdio.h>


#ifdef __cplusplus
extern "C" {
#endif


/** \defgroup hwlocality_linux Linux-specific helpers
 *
 * This includes helpers for manipulating Linux kernel cpumap files, and hwloc
 * equivalents of the Linux sched_setaffinity and sched_getaffinity system calls.
 *
 * @{
 */

/** \brief Convert a linux kernel cpumap file \p file into hwloc CPU set.
 *
 * Might be used when reading CPU set from sysfs attributes such as topology
 * and caches for processors, or local_cpus for devices.
 */
HWLOC_DECLSPEC int hwloc_linux_parse_cpumap_file(FILE *file, hwloc_cpuset_t set);

/** \brief Bind a thread \p tid on cpus given in cpuset \p set
 *
 * The behavior is exactly the same as the Linux sched_setaffinity system call,
 * but uses a hwloc cpuset.
 *
 * \note This is equivalent to calling hwloc_set_proc_cpubind() with
 * HWLOC_CPUBIND_THREAD as flags.
 */
HWLOC_DECLSPEC int hwloc_linux_set_tid_cpubind(hwloc_topology_t topology, pid_t tid, hwloc_const_cpuset_t set);

/** \brief Get the current binding of thread \p tid
 *
 * The behavior is exactly the same as the Linux sched_getaffinity system call,
 * but uses a hwloc cpuset.
 *
 * \note This is equivalent to calling hwloc_get_proc_cpubind() with
 * HWLOC_CPUBIND_THREAD as flags.
 */
HWLOC_DECLSPEC int hwloc_linux_get_tid_cpubind(hwloc_topology_t topology, pid_t tid, hwloc_cpuset_t set);

/** \brief Get the last physical CPU where thread \p tid ran.
 *
 * \note This is equivalent to calling hwloc_get_proc_last_cpu_location() with
 * HWLOC_CPUBIND_THREAD as flags.
 */
HWLOC_DECLSPEC int hwloc_linux_get_tid_last_cpu_location(hwloc_topology_t topology, pid_t tid, hwloc_bitmap_t set);

/** @} */


#ifdef __cplusplus
} /* extern "C" */
#endif


#endif /* HWLOC_GLIBC_SCHED_H */
