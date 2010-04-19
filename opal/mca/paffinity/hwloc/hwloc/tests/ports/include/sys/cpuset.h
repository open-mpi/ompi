/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_SYS_CPUSET_H
#define HWLOC_PORT_SYS_CPUSET_H

#include <limits.h>

typedef long cpuset_t;
typedef int cpulevel_t;
typedef int cpuwhich_t;

#define CPU_LEVEL_WHICH 3
#define CPU_WHICH_TID 1
#define CPU_WHICH_PID 2

#undef CPU_SETSIZE
#define CPU_SETSIZE (sizeof(cpuset_t) * CHAR_BIT)
#undef CPU_ZERO
#define CPU_ZERO(cpuset) (*(cpuset) = 0)
#undef CPU_SET
#define CPU_SET(cpu, cpuset) (*(cpuset) |= (1<<(cpu)))
#undef CPU_ISSET
#define CPU_ISSET(cpu, cpuset) (*(cpuset) & (1<<(cpu)))

int cpuset_getaffinity(cpulevel_t level, cpuwhich_t which, id_t id, size_t size, cpuset_t *cpuset);
int cpuset_setaffinity(cpulevel_t level, cpuwhich_t which, id_t id, size_t size, const cpuset_t *cpuset);

#endif /* HWLOC_PORT_SYS_CPUSET_H */
