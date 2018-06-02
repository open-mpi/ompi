/*
 * Copyright © 2009 inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_AIX_SYS_THREAD_H
#define HWLOC_PORT_AIX_SYS_THREAD_H

typedef long tid_t;
typedef uint64_t tid64_t;

tid_t thread_self(void);
struct __pthrdsinfo {
  tid_t __pi_tid;
};
#define PTHRDSINFO_QUERY_TID 0x10
int pthread_getthrds_np (pthread_t * thread, int mode, struct __pthrdsinfo * buf, int bufsize, void * regbuf, int * regbufsize);

#endif /* HWLOC_PORT_AIX_SYS_THREAD_H */
