/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_SYS_RSET_H
#define HWLOC_PORT_SYS_RSET_H

#include <stdint.h>

#include "thread.h"

typedef void *rsethandle_t;

#define RS_EMPTY 3
#define RS_ALL 2
#define RS_PARTITION 3

#define RS_TESTRESOURCE 11

typedef int rsinfo_t;
#define R_NUMPROCS 0
#define R_MAXSDL 3
#define R_SMPSDL 4
#define R_MCMSDL 5
#define R_MAXPROCS 6
#define R_L2CSDL 11
#define R_CORESDL 12

#define R_PROCS 6

#define R_NADA 0
#define R_PROCESS 1
#define R_RSET 2
#define R_SUBRANGE 3
#define R_SHM 4
#define R_FILDES 5
#define R_THREAD 6

rsethandle_t rs_alloc (unsigned int flags);
int rs_numrads(rsethandle_t rset, unsigned int sdl, unsigned int flags);
int rs_getrad (rsethandle_t rset, rsethandle_t rad, unsigned int sdl, unsigned int index, unsigned int flags);
int rs_getinfo(rsethandle_t rseth, rsinfo_t info_type, unsigned int flags);
int rs_op(unsigned int command, rsethandle_t rseth1, rsethandle_t rseth2, unsigned int flags, unsigned int id);
void rs_free(rsethandle_t rseth);

typedef int rstype_t;
typedef struct subrange subrange_t;

typedef union {
  pid_t at_pid;
  tid_t at_tid;
  int at_shmid;
  int at_fd;
  rsethandle_t at_rset;
  subrange_t *at_subrange;
} rsid_t;

struct subrange {
  uint64_t su_offset;
  uint64_t su_length;
  rstype_t su_rstype;
  unsigned int su_policy;
  rsid_t su_rsid;
  uint64_t su_rsoffset;
  uint64_t su_rslength;
};

int ra_attachrset (rstype_t rstype, rsid_t rsid, rsethandle_t rset, unsigned int flags);
int ra_detachrset (rstype_t rstype, rsid_t rsid, unsigned int flags);
int ra_getrset (rstype_t rstype, rsid_t rsid, unsigned int flags, rsethandle_t rset);

#endif /* HWLOC_PORT_SYS_RSET_H */
