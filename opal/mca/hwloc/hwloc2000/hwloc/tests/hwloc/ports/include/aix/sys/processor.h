/*
 * Copyright © 2009 inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_AIX_SYS_PROCESSOR_H
#define HWLOC_PORT_AIX_SYS_PROCESSOR_H

typedef short cpu_t;
#define BINDPROCESS 1
#define BINDTHREAD 2
#define PROCESSOR_CLASS_ANY ((cpu_t)(-1))
extern int bindprocessor(int What, int Who, cpu_t Where);

extern cpu_t mycpu(void);

#endif /* HWLOC_PORT_AIX_SYS_PROCESSOR_H */
