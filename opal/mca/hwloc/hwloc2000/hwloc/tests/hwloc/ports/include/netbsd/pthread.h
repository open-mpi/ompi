/*
 * Copyright © 2012 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_NETBSD_PTHREAD_H
#define HWLOC_PORT_NETBSD_PTHREAD_H

#include <sched.h>

pthread_t       pthread_self(void);

int     pthread_getaffinity_np(pthread_t, size_t, cpuset_t *);
int     pthread_setaffinity_np(pthread_t, size_t, cpuset_t *);

#endif /* HWLOC_PORT_NETBSD_PTHREAD_H */
