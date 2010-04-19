/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_NUMA_H
#define HWLOC_PORT_NUMA_H

#include "sys/types.h"
#include "radset.h"
#include "cpuset.h"

int rad_get_num(void);
int rad_get_cpus(radid_t rad, cpuset_t cpuset);
ssize_t rad_get_physmem(radid_t rad);

#endif /* HWLOC_PORT_NUMA_H */
