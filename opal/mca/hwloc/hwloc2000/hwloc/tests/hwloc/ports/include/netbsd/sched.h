/*
 * Copyright © 2012 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_NETBSD_SCHED_H
#define HWLOC_PORT_NETBSD_SCHED_H

typedef struct _cpuset cpuset_t;
typedef unsigned long cpuid_t;

cpuset_t *cpuset_create(void);
void    cpuset_destroy(cpuset_t *);
void    cpuset_zero(cpuset_t *);
int     cpuset_set(cpuid_t, cpuset_t *);
int     cpuset_isset(cpuid_t, const cpuset_t *);
size_t  cpuset_size(const cpuset_t *);

int     sched_getaffinity_np(pid_t, size_t, cpuset_t *);
int     sched_setaffinity_np(pid_t, size_t, cpuset_t *);

#endif /* HWLOC_PORT_NETBSD_SCHED_H */
