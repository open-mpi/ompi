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

#ifndef _VT_UNIFY_STATS_HDLR_H_
#define _VT_UNIFY_STATS_HDLR_H_

#include "vt_inttypes.h"

#include "otf.h"

int Handle_FunctionSummary(
   OTF_WStream* wstream, 
   uint64_t time, uint32_t function, uint32_t process, uint64_t invocations, 
   uint64_t exclTime, uint64_t inclTime );

int Handle_MessageSummary(
   OTF_WStream* wstream,
   uint64_t time, uint32_t process, uint32_t peer, uint32_t comm,
   uint32_t type, uint64_t sentNumber, uint64_t receivedNumber,
   uint64_t sentBytes, uint64_t receivedBytes );

int Handle_CollopSummary(
   OTF_WStream* wstream, 
   uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
   uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes,
   uint64_t receivedBytes );

int Handle_FileOperationSummary(
   OTF_WStream* wstream,
   uint64_t time, uint32_t fileid, uint32_t process, uint64_t nopen,
   uint64_t nclose, uint64_t nread, uint64_t nwrite, uint64_t nseek,
   uint64_t bytesread, uint64_t byteswrite );

#endif // _VT_UNIFY_STATS_HDLR_H_
