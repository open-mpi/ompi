/*
 * Copyright © 2012 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_AIX_PROCINFO_H
#define HWLOC_PORT_AIX_PROCINFO_H

#include <sys/thread.h>
#include <sys/processor.h>

struct thrdsinfo {
	unsigned long ti_tid;
	unsigned long ti_cpuid;
};

struct thrdentry64 {
	tid_t ti_tid;
	cpu_t ti_cpuid;
};

extern int getthrds(pid_t, void *, int,  tid_t *, int);
extern int getthrds64(pid_t, void *, int, tid64_t *, int);

#endif /* HWLOC_PORT_AIX_PROCINFO_H */
