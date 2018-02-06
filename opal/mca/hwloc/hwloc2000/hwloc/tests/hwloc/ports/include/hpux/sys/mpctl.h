/*
 * Copyright © 2009-2015 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_HPUX_SYS_MPCTL_H
#define HWLOC_PORT_HPUX_SYS_MPCTL_H

typedef int spu_t, ldom_t, pthread_spu_t, pthread_ldom_t;

typedef enum mpc_request {
  MPC_GETNUMSPUS,
  MPC_GETFIRSTSPU,
  MPC_GETNEXTSPU,

  MPC_GETNUMSPUS_SYS,
  MPC_GETFIRSTSPU_SYS,
  MPC_GETNEXTSPU_SYS,

  MPC_GETCURRENTSTSPU,
  MPC_SETPROCESS,
  MPC_SETPROCESS_FORCE,
  MPC_SETLWP,
  MPC_SETLWP_FORCE,
  MPC_SETLWP_UP,

  MPC_SETLDOM,
  MPC_SETLWPLDOM,

  MPC_GETNUMLDOMS,
  MPC_GETFIRSTLDOM,
  MPC_GETNEXTLDOM,
  MPC_GETNUMLDOMS_SYS,
  MPC_GETFIRSTLDOM_SYS,
  MPC_GETNEXTLDOM_SYS,

  MPC_SPUTOLDOM,
  MPC_LDOMSPUS,
  MPC_LDOMSPUS_SYS,

  MPC_GETPROCESS_BINDVALUE,
  MPC_GETLWP_BINDVALUE
} mpc_request_t;

#define MPC_SPUFLOAT 0
#define MPC_LDOMFLOAT 1
#define MPC_SELFPID 2
#define MPC_SELFLWPPID 3

extern int mpctl(mpc_request_t, spu_t, ...);

#define _SC_CCNUMA_SUPPORT 0
#define _SC_PSET_SUPPORT 1

int pthread_processor_bind_np(int request, pthread_spu_t *answer, pthread_spu_t spu, pthread_t tid);

int pthread_ldom_bind_np(pthread_ldom_t *answer, pthread_ldom_t ldom, pthread_t tid);

#define PTHREAD_SELFTID_NP 0
#define PTHREAD_LDOMFLOAT_NP 0
#define PTHREAD_SPUFLOAT_NP 0

#define PTHREAD_BIND_ADVISORY_NP 0
#define PTHREAD_BIND_FORCED_NP 1

#define MAP_MEM_LOCAL 0
#define MAP_MEM_INTERLEAVED 1
#define MAP_MEM_FIRST_TOUCH 2

#endif /* HWLOC_PORT_HPUX_SYS_MPCTL_H */
