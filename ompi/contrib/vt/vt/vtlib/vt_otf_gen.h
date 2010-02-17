/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
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

EXTERN VTGen* VTGen_open(const char* tnameprefix, const char* tnamesuffix,
                         const char* tnameextern, uint32_t ptid, uint32_t tid,
                         size_t buffer_size);

EXTERN void VTGen_flush(VTGen* gen, uint8_t lastFlush,
			uint64_t flushBTime, uint64_t* flushETime);

EXTERN void VTGen_close(VTGen* gen);

EXTERN void VTGen_delete(VTGen* gen);

EXTERN void VTGen_destroy(VTGen* gen);

EXTERN uint8_t VTGen_get_buflevel(VTGen* gen);


/* -- Writing trace records -- */


/* - Definition records - */

EXTERN void VTGen_write_DEFINITION_COMMENT(VTGen* gen, const char* comment);

EXTERN void VTGen_write_DEF_SCL_FILE(VTGen* gen, uint32_t fid,
				     const char* fname);

EXTERN void VTGen_write_DEF_SCL(VTGen* gen, uint32_t sid, uint32_t fid,
				uint32_t ln );

EXTERN void VTGen_write_DEF_FILE_GROUP(VTGen* gen, uint32_t gid,
				       const char* gname);

EXTERN void VTGen_write_DEF_FILE(VTGen* gen, uint32_t fid, const char* fname,
				 uint32_t gid);

EXTERN void VTGen_write_DEF_FUNCTION_GROUP(VTGen* gen, uint32_t rdid,
					   const char* rdesc);

EXTERN void VTGen_write_DEF_FUNCTION(VTGen* gen, uint32_t rid,
				     const char* rname, uint32_t rdid,
				     uint32_t sid );

EXTERN void VTGen_write_DEF_COLLECTIVE_OPERATION(VTGen* gen, uint32_t cid,
						 const char* cname,
						 uint32_t ctype );

EXTERN void VTGen_write_DEF_COUNTER_GROUP(VTGen* gen, uint32_t gid,
					  const char* gname);

EXTERN void VTGen_write_DEF_COUNTER(VTGen* gen, uint32_t cid,
				    const char* cname, uint32_t cprop,
				    uint32_t gid, const char* cunit);

EXTERN void VTGen_write_DEF_PROCESS_GROUP(VTGen* gen, uint32_t cid,
					  const char* grpn, uint32_t grpc,
					  uint32_t grpv[]);

/* -- Marker -- */

EXTERN void VTGen_write_DEF_MARKER(VTGen* gen, uint32_t mid, const char* mname,
				   uint32_t mtype);


/* - Event records - */


/* -- Region -- */

EXTERN void VTGen_write_ENTER(VTGen* gen, uint64_t* time, uint32_t rid,
			      uint32_t sid, uint8_t metc, uint64_t metv[]);

EXTERN void VTGen_write_LEAVE(VTGen* gen, uint64_t* time, uint32_t rid,
			      uint32_t sid, uint8_t metc, uint64_t metv[]);

/* -- File I/O -- */

EXTERN void VTGen_write_FILE_OPERATION(VTGen* gen, uint64_t* time,
				       uint64_t* etime, uint32_t fid,
				       uint64_t hid, uint32_t op,
				       uint64_t bytes, uint32_t sid);

EXTERN void VTGen_write_BEGIN_FILE_OPERATION(VTGen* gen, uint64_t* time,
					     uint64_t hid, uint32_t sid);

EXTERN void VTGen_write_END_FILE_OPERATION(VTGen* gen, uint64_t* time,
					   uint32_t fid, uint64_t hid,
					   uint32_t op, uint64_t bytes,
					   uint32_t sid);

/* -- Counter -- */

EXTERN void VTGen_write_COUNTER(VTGen* gen, uint64_t* time, uint32_t cid,
				uint64_t cval);

/* -- Comment -- */

EXTERN void VTGen_write_COMMENT(VTGen* gen, uint64_t* time,
				const char* comment);

/* -- Marker -- */

EXTERN void VTGen_write_MARKER(VTGen* gen, uint64_t* time, uint32_t mid,
			       const char* mtext);

/* -- MPI-1 -- */

EXTERN void VTGen_write_SEND_MSG(VTGen* gen, uint64_t* time, uint32_t dpid,
				 uint32_t cid, uint32_t tag, uint32_t sent,
				 uint32_t sid);

EXTERN void VTGen_write_RECV_MSG(VTGen* gen, uint64_t* time, uint32_t spid,
				 uint32_t cid, uint32_t tag, uint32_t recvd,
				 uint32_t sid);

EXTERN void VTGen_write_COLLECTIVE_OPERATION(VTGen* gen, uint64_t* time,
					     uint64_t* etime, uint32_t rid,
					     uint32_t cid, uint32_t rpid,
					     uint32_t sent, uint32_t recvd,
					     uint32_t sid);

/* -- MPI2 - 1sided --*/

EXTERN void VTGen_write_RMA_PUT( VTGen* gen, uint64_t* time, uint32_t opid, 
        uint32_t tpid, uint32_t cid, uint32_t tag, uint32_t len, uint32_t sid);

EXTERN void VTGen_write_RMA_PUTRE( VTGen* gen, uint64_t* time, uint32_t opid, 
        uint32_t tpid, uint32_t cid, uint32_t tag, uint64_t len, uint32_t sid);

EXTERN void VTGen_write_RMA_GET( VTGen* gen, uint64_t* time, uint32_t opid, 
        uint32_t tpid, uint32_t cid, uint32_t tag, uint64_t len, uint32_t sid);

EXTERN void VTGen_write_RMA_END( VTGen* gen, uint64_t* time, uint32_t rpid,
        uint32_t cid, uint32_t tag, uint32_t sid);

/* -- VampirTrace Internal -- */

EXTERN void VTGen_write_ENTER_STAT(VTGen* gen, uint64_t* time, uint8_t metc,
				   uint64_t metv[]);

EXTERN void VTGen_write_EXIT_STAT(VTGen* gen, uint64_t* time, uint8_t metc,
				  uint64_t metv[]);

EXTERN void VTGen_write_ENTER_FLUSH(VTGen* gen, uint64_t* time, uint8_t metc,
				    uint64_t metv[]);

EXTERN void VTGen_write_EXIT_FLUSH(VTGen* gen, uint64_t* time, uint8_t metc,
				   uint64_t metv[]);


/* - Summary records - */


EXTERN void VTGen_write_FUNCTION_SUMMARY(VTGen* gen, uint64_t* time,
					 uint32_t rid, uint64_t cnt,
					 uint64_t excl, uint64_t incl);

EXTERN void VTGen_write_MESSAGE_SUMMARY(VTGen* gen, uint64_t* time,
					uint32_t peer, uint32_t cid,
					uint32_t tag, uint64_t scnt,
					uint64_t rcnt, uint64_t sent,
					uint64_t recvd);

EXTERN void VTGen_write_COLLECTIVE_OPERATION_SUMMARY(VTGen* gen,
						     uint64_t* time,
						     uint32_t cid,
						     uint32_t rid,
						     uint64_t scnt,
						     uint64_t rcnt,
						     uint64_t sent,
						     uint64_t recvd);

EXTERN void VTGen_write_FILE_OPERATION_SUMMARY(VTGen* gen, uint64_t* time,
					       uint32_t fid, uint64_t nopen,
					       uint64_t nclose, uint64_t nread,
					       uint64_t nwrite, uint64_t nseek,
					       uint64_t read, uint64_t wrote);

#endif /* _VT_GEN_OTF_H */
