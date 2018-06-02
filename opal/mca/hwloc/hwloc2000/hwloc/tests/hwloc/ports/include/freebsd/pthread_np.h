/*
 * Copyright © 2009 Université Bordeaux
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_FREEBSD_PTHREAD_NP_H
#define HWLOC_PORT_FREEBSD_PTHREAD_NP_H

#include <sys/cpuset.h>

int pthread_getaffinity_np(pthread_t, size_t, cpuset_t *);
int pthread_setaffinity_np(pthread_t, size_t, const cpuset_t *);

#endif /* HWLOC_PORT_FREEBSD_PTHREAD_NP_H */
