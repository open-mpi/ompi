/*
 * Copyright © 2009-2016 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_DARWIN_SYS_SYSCTL_H
#define HWLOC_PORT_DARWIN_SYS_SYSCTL_H

extern int sysctl(int *name, int name_len, void *oldp, size_t *oldlenp, void *newp, size_t newlen);
extern int sysctlbyname(const char *name, void *oldp, size_t *oldlenp, void *newp, size_t newlen);

#define CTL_HW 6
#define HW_PHYSMEM 5

/* hide Linux' host disabling _SC_LARGE_PAGESIZE */
#undef HAVE_DECL__SC_LARGE_PAGESIZE
#define HAVE_DECL__SC_LARGE_PAGESIZE 1
#undef _SC_LARGE_PAGESIZE
#define _SC_LARGE_PAGESIZE 33

#endif /* HWLOC_PORT_DARWIN_SYS_SYSCTL_H */
