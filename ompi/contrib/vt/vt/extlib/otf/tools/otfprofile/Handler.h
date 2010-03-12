/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#ifndef HANDLER_H
#define HANDLER_H

#include "OTF_inttypes.h"

#include "Summary.h"

#define RETURN_HANDLER_OK 0
#define RETURN_HANDLER_ABORT 1

using namespace std;

/* SummaryHandler */

int handleFunctionSummary (void *firsthandlerarg, uint64_t time, uint32_t function, uint32_t process, uint64_t invocations, 			   uint64_t exclTime, uint64_t inclTime);

int handleMessageSummary (void *firsthandlerarg, uint64_t time, uint32_t process, uint32_t peer, uint32_t comm, uint32_t type, 			  uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes, uint64_t receivedBytes);

int handleCollopSummary (void *firsthandlerarg, uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
			uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes, uint64_t receivedBytes);

/*********************/

int handleDefCreator(void *firsthandlerarg, uint32_t stream, const char *creator);

int handleDefVersion(void *firsthandlerarg, uint32_t stream, uint8_t major, uint8_t minor, uint8_t sub, const char *string);

int handleDefTimerResolution(void* firsthandlerarg, uint32_t streamid, uint64_t ticks_per_sec);

int handleDefFunction(void* firsthandlerarg, uint32_t streamid,
	uint32_t func, const char* name, uint32_t group, uint32_t scltoken);

int handleDefFunctionGroup(void* firsthandlerarg, uint32_t streamid,
	uint32_t funcg, const char* name);
	
int handleDefProcess(void* firsthandlerarg, uint32_t streamid,
	uint32_t proc, const char* name, uint32_t parent);

int handleDefProcessGroup(void *firsthandlerarg, uint32_t stream,
	 uint32_t procGroup, const char *name, uint32_t numberOfProcs,
	 const uint32_t *procs);
	
int handleDefCollectiveOperation(void* firsthandlerarg, uint32_t streamid, 
	uint32_t collop, const char* name, uint32_t type);
	
int handleDefCounter(void* firsthandlerarg, uint32_t streamid,
	uint32_t counter, const char* name, uint32_t properties, 
   uint32_t countergroup, const char* unit);

int handleEnter(void* firsthandlerarg, uint64_t time, uint32_t func,
	uint32_t proc, uint32_t scltoken);

int handleLeave(void* firsthandlerarg, uint64_t time, uint32_t func,
	uint32_t proc, uint32_t scltoken);

int handleCounter(void* firsthandlerarg, uint64_t time, uint32_t proc,
	uint32_t counter, uint64_t value);
	
int handleRecvMsg(void* firsthandlerarg, uint64_t time, uint32_t receiver,
	uint32_t sender, uint32_t communicator, uint32_t msgtype, uint32_t msglength,
	uint32_t scltoken);

int handleSendMsg(void* firsthandlerarg, uint64_t time, uint32_t sender,
	uint32_t receiver, uint32_t communicator, uint32_t msgtype, uint32_t msglength,
	uint32_t scltoken);
	
int handleCollectiveOperation(void* firsthandlerarg, uint64_t time, 
	uint32_t proc, uint32_t collop, uint32_t procgroup, 
	uint32_t rootprocess, uint32_t sent, uint32_t received, 
	uint64_t duration, uint32_t scltoken);
	
int set_time_sum_container(global_data* gd_ptr);
int mergeProgTime(global_data* gd, global_data* data);

#endif /* HANDLER_H */
