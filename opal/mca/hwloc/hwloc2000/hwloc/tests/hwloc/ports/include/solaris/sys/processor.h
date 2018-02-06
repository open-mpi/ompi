/*
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_SOLARIS_SYS_PROCESSOR_H
#define HWLOC_PORT_SOLARIS_SYS_PROCESSOR_H

#include <sys/procset.h>
typedef int processorid_t;
#define PBIND_NONE -1
#define PBIND_QUERY -2

extern int processor_bind(idtype_t idtype, id_t id, processorid_t processorid, processorid_t *obind);
extern processorid_t getcpuid(void);

/* hide Linux' host disabling _SC_LARGE_PAGESIZE */
#undef HAVE_DECL__SC_LARGE_PAGESIZE
#define HAVE_DECL__SC_LARGE_PAGESIZE 1
#undef _SC_LARGE_PAGESIZE
#define _SC_LARGE_PAGESIZE 33

#endif /* HWLOC_PORT_SOLARIS_SYS_PROCESSOR_H */
