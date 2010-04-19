/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_CPUSET_H
#define HWLOC_PORT_CPUSET_H

typedef int cpu_cursor_t;
#define SET_CURSOR_INIT -1
typedef int cpuid_t;
#define CPU_NONE -1
typedef int cpuset_t;

int cpusetcreate(cpuset_t *set);
int cpuemptyset(cpuset_t set);
int cpuxorset(cpuset_t set1, cpuset_t set2, cpuset_t res);
int cpucountset(cpuset_t set);
int cpuaddset(cpuset_t set, cpuid_t cpuid );
cpuid_t cpu_foreach(cpuset_t cpuset, int flags, cpu_cursor_t *cursor);
int cpusetdestroy(cpuset_t *set);

#endif /* HWLOC_PORT_CPUSET_H */
