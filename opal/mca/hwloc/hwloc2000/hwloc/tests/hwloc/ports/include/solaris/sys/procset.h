/*
 * Copyright © 2009-2014 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_SOLARIS_SYS_PROCSET_H
#define HWLOC_PORT_SOLARIS_SYS_PROCSET_H

#ifndef __ENUM_IDTYPE_T
/* Linux doesn't have idtype_t unless __ENUM_IDTYPE_T is set */
typedef int idtype_t;
#ifndef P_PID
/* If there's no idtype_t, P_PID could exist as a #define */
#define P_PID 1
#endif
#endif

/* Linux never has P_LWPID and P_MYID */
#define P_LWPID 2
#define P_MYID 3

#endif /* HWLOC_PORT_SOLARIS_SYS_PROCSET_H */
