/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_PTHREAD_NP_H
#define HWLOC_PORT_PTHREAD_NP_H

#if 0
/* Conflict with Linux' */
int pthread_getaffinity_np(pthread_t, size_t, cpuset_t *);
int pthread_setaffinity_np(pthread_t, size_t, const cpuset_t *);
#endif

#endif /* HWLOC_PORT_PTHREAD_NP_H */
