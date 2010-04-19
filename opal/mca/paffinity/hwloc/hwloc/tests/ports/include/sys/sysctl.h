/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_SYS_SYSCTL_H
#define HWLOC_PORT_SYS_SYSCTL_H

extern int sysctl(int *name, int name_len, void *oldp, size_t *oldlenp, void *newp, size_t newlen);
extern int sysctlbyname(const char *name, void *oldp, size_t *oldlenp, void *newp, size_t newlen);

#endif /* HWLOC_PORT_SYS_SYSCTL_H */
