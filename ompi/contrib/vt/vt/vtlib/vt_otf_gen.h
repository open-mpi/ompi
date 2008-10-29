/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_GEN_OTF_H
#define _VT_GEN_OTF_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include <stdlib.h>
#include <stdio.h>

#include "vt_defs.h"

#define VT__USER                  0
#define VT__SYNC                  1
#define VT__FLUSH                 2
#define VT__STAT                  3
#define VT__PREG                  4
#define VT__REGID_NUM             5

extern int vt_trc_regid[VT__REGID_NUM];

/*
 *-----------------------------------------------------------------------------
 * Typedefs (to make data types opaque)
 *-----------------------------------------------------------------------------
 */

/* Trace file generated at run time */
typedef struct VTGen_struct VTGen;

/*
 *-----------------------------------------------------------------------------
 * VTGen
 *-----------------------------------------------------------------------------
 */

EXTERN VTGen* VTGen_open              ( const char* namestub, uint32_t tid,
					size_t buffer_size, uint8_t mode );

EXTERN void VTGen_flush               ( VTGen* gen, uint8_t markFlush,
					uint64_t flushBTime, uint64_t* flushETime );

EXTERN void VTGen_close               ( VTGen* gen );

EXTERN void VTGen_delete              ( VTGen* gen );

EXTERN void VTGen_init_trc_id         ( VTGen* gen, uint32_t trcid );

EXTERN char* VTGen_get_name           ( VTGen* gen );

EXTERN char* VTGen_get_defname        ( VTGen* gen );

EXTERN char* VTGen_get_eventname      ( VTGen* gen );

EXTERN char* VTGen_get_statname       ( VTGen* gen );

      
/* -- Writing trace records -- */

/* - Definition records - */

EXTERN void VTGen_write_DEFINITION_COMMENT ( VTGen* gen,
					      const char* comment );

EXTERN void VTGen_write_DEF_SCL_FILE  ( VTGen* gen,
					 uint32_t fid,
					 const char* fname );

EXTERN void VTGen_write_DEF_SCL       ( VTGen* gen,
					 uint32_t sid,
					 uint32_t fid,
					 uint32_t ln );

EXTERN void VTGen_write_DEF_FILE_GROUP ( VTGen* gen,
					    uint32_t gid,
					    const char* gname );

EXTERN void VTGen_write_DEF_FILE      ( VTGen* gen,
					 uint32_t fid,
					 const char* fname,
					 uint32_t gid );

EXTERN void VTGen_write_DEF_FUNCTION_GROUP ( VTGen* gen,
					      uint32_t rdid,
					      const char* rdesc );

EXTERN void VTGen_write_DEF_FUNCTION  ( VTGen* gen,
					 uint32_t rid,
					 const char* rname,
					 uint32_t rdid,
					 uint32_t sid );

EXTERN void VTGen_write_DEF_COLLECTIVE_OPERATION ( VTGen* gen,
						    uint32_t cid,
						    const char* cname,
						    uint32_t ctype );

EXTERN void VTGen_write_DEF_COUNTER_GROUP ( VTGen* gen,
					     uint32_t gid,
					     const char* gname );

EXTERN void VTGen_write_DEF_COUNTER   ( VTGen* gen,
					 uint32_t cid,
					 const char* cname,
					 uint32_t cprop,
					 uint32_t gid,
					 const char* cunit );

EXTERN void VTGen_write_DEF_PROCESS_GROUP ( VTGen* gen,
					     uint32_t cid,
					     const char* grpn,
					     uint32_t grpc,
					     uint32_t grpv[] ); 

/* - Event records - */

/* -- Region -- */

EXTERN void VTGen_write_ENTER(VTGen* gen, uint64_t* time, uint32_t rid,
       uint32_t sid, uint8_t metc, uint64_t metv[]);

EXTERN void VTGen_write_LEAVE(VTGen* gen, uint64_t* time, uint32_t rid,
       uint32_t sid, uint8_t metc, uint64_t metv[]);

/* -- File I/O -- */

EXTERN void VTGen_write_FILE_OPERATION(VTGen* gen, uint64_t* time,
       uint64_t* etime, uint32_t fid, uint64_t hid,
       uint32_t op, uint64_t bytes, uint32_t sid);

/* -- Counter -- */

EXTERN void VTGen_write_COUNTER(VTGen* gen, uint64_t* time, uint32_t cid,
       uint64_t cval);

/* -- Comment -- */

EXTERN void VTGen_write_COMMENT(VTGen* gen, uint64_t* time,
       const char* comment);

/* -- MPI-1 -- */

EXTERN void VTGen_write_SEND_MSG(VTGen* gen, uint64_t* time, uint32_t dpid,
       uint32_t cid, uint32_t tag, uint32_t sent, uint32_t sid);

EXTERN void VTGen_write_RECV_MSG(VTGen* gen, uint64_t* time, uint32_t spid,
       uint32_t cid, uint32_t tag, uint32_t recvd, uint32_t sid);

EXTERN void VTGen_write_COLLECTIVE_OPERATION(VTGen* gen, uint64_t* time,
       uint64_t* etime, uint32_t rid, uint32_t cid, uint32_t rpid,
       uint32_t sent, uint32_t recvd, uint32_t sid);

/* -- OpenMP -- */

EXTERN void VTGen_write_OMP_FORK(VTGen* gen, uint64_t* time);

EXTERN void VTGen_write_OMP_JOIN(VTGen* gen, uint64_t* time);

/* -- Summary -- */

EXTERN void VTGen_write_FUNCTION_SUMMARY(VTGen* gen, uint64_t* time,
       uint32_t rid, uint64_t cnt, uint64_t excl, uint64_t incl);

EXTERN void VTGen_write_MESSAGE_SUMMARY(VTGen* gen, uint64_t* time,
       uint32_t peer, uint32_t cid, uint32_t tag,
       uint64_t scnt, uint64_t rcnt, uint64_t sent, uint64_t recvd);

EXTERN void VTGen_write_FILE_OPERATION_SUMMARY(VTGen* gen, uint64_t* time,
       uint32_t fid, uint64_t nopen, uint64_t nclose, uint64_t nread,
       uint64_t nwrite, uint64_t nseek, uint64_t read, uint64_t wrote);

/* -- VampirTrace Internal -- */

EXTERN void VTGen_write_ENTER_STAT(VTGen* gen, uint64_t* time,
       uint8_t metc, uint64_t metv[]);

EXTERN void VTGen_write_EXIT_STAT(VTGen* gen, uint64_t* time,
       uint8_t metc, uint64_t metv[]);

EXTERN void VTGen_write_ENTER_FLUSH(VTGen* gen, uint64_t* time,
       uint8_t metc, uint64_t metv[]);

EXTERN void VTGen_write_EXIT_FLUSH(VTGen* gen, uint64_t* time,
       uint8_t metc, uint64_t metv[]);

#endif
