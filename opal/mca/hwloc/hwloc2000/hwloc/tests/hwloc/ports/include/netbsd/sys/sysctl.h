/*
 * Copyright © 2014 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_NETBSD_SYS_SYSCTL_H
#define HWLOC_PORT_NETBSD_SYS_SYSCTL_H

extern int sysctl(const int *, unsigned int, void *, size_t *, const void *, size_t);

#define CTL_HW 6
#define HW_PHYSMEM64 13

#endif /* HWLOC_PORT_NETBSD_SYS_SYSCTL_H */
