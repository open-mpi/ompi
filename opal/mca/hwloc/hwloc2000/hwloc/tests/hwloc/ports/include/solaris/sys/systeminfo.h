/*
 * Copyright © 2011-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_SOLARIS_SYS_SYSTEMINFO_H
#define HWLOC_PORT_SOLARIS_SYS_SYSTEMINFO_H

#define SI_ARCHITECTURE 6
#define SI_HW_PROVIDER 8
#define SI_PLATFORM 513

extern int sysinfo(int, char *, long);

#endif /* HWLOC_PORT_SOLARIS_SYS_SYSTEMINFO_H */
