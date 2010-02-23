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

#ifndef _VT_UNIFY_EVENTS_HDLR_H_
#define _VT_UNIFY_EVENTS_HDLR_H_

#include "vt_inttypes.h"

#include "otf.h"

int Handle_Enter(
   OTF_WStream* wstream,
   uint64_t time, uint32_t statetoken, uint32_t cpuid,
   uint32_t scltoken );

int Handle_Leave(
   OTF_WStream* wstream,
   uint64_t time, uint32_t statetoken, uint32_t cpuid,
   uint32_t scltoken );

int Handle_FileOperation(
   OTF_WStream* wstream,
   uint64_t time, uint32_t filetoken, uint32_t cpuid,
   uint64_t handleid, uint32_t operation, uint64_t bytes,
   uint64_t duration, uint32_t scltoken );

int Handle_BeginFileOperation(
   OTF_WStream* wstream,
   uint64_t time, uint32_t cpuid, uint64_t handleid,
   uint32_t scltoken );

int Handle_EndFileOperation(
   OTF_WStream* wstream,
   uint64_t time, uint32_t cpuid, uint32_t filetoken,
   uint64_t handleid, uint32_t operation, uint64_t bytes,
   uint32_t scltoken );

int Handle_SendMsg(
   OTF_WStream* wstream,
   uint64_t time, uint32_t sender, uint32_t receiver,
   uint32_t communicator, uint32_t msgtag, 
   uint32_t msglength, uint32_t scltoken );

int Handle_RecvMsg(
   OTF_WStream* wstream,
   uint64_t time, uint32_t receiver, uint32_t sender,
   uint32_t communicator, uint32_t msgtag,
   uint32_t msglength, uint32_t scltoken );

int Handle_CollectiveOperation(
   OTF_WStream* wstream,
   uint64_t time, uint32_t process,
   uint32_t functionToken, uint32_t communicator,
   uint32_t rootprocess, uint32_t sent,
   uint32_t received, uint64_t duration,
   uint32_t scltoken );

int Handle_RMAPut(
   OTF_WStream* wstream,
   uint64_t time, uint32_t process, uint32_t origin,
   uint32_t dest, uint32_t communicator, uint32_t tag,
   uint64_t bytes, uint32_t scltoken );

int Handle_RMAPutRemoteEnd(
   OTF_WStream* wstream,
   uint64_t time, uint32_t process, uint32_t origin,
   uint32_t dest, uint32_t communicator, uint32_t tag,
   uint64_t bytes, uint32_t scltoken );

int Handle_RMAGet(
   OTF_WStream* wstream,
   uint64_t time, uint32_t process, uint32_t origin,
   uint32_t dest, uint32_t communicator, uint32_t tag,
   uint64_t bytes, uint32_t scltoken );

int Handle_RMAEnd(
   OTF_WStream* wstream,
   uint64_t time, uint32_t process, uint32_t remote,
   uint32_t communicator, uint32_t tag, uint32_t scltoken );


int Handle_Counter(
   OTF_WStream* wstream,
   uint64_t time, uint32_t process, uint32_t counter_token,
   uint64_t value );

int Handle_EventComment(
   OTF_WStream* wstream,
   uint64_t time, uint32_t process, const char* comment );

#endif // _VT_UNIFY_EVENTS_HDLR_H_
