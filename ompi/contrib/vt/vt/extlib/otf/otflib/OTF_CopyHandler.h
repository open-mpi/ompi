/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_CopyHandler.h
 * 
 *  @brief Provides handlers for copying a trace.
 *
 *  \ingroup internal
 */

#ifndef OTF_COPYOTF_CopyHandler_R_H
#define OTF_COPYOTF_CopyHandler_R_H


#include "OTF_inttypes.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* *** Definition OTF_CopyHandler_r *** ************************************* */

int OTF_CopyHandler_DefinitionComment( void* userData, uint32_t stream,
	const char* comment );

int OTF_CopyHandler_DefTimerResolution( void* userData, uint32_t stream,
	uint64_t ticksPerSecond );

int OTF_CopyHandler_DefProcess( void* userData, uint32_t stream, uint32_t process,
	const char* name, uint32_t parent );

int OTF_CopyHandler_DefProcessGroup( void* userData, uint32_t stream,
	uint32_t procGroup, const char* name, uint32_t numberOfProcs,
	const uint32_t* procs );

int OTF_CopyHandler_DefFunction( void* userData, uint32_t stream, uint32_t func,
	const char* name, uint32_t funcGroup, uint32_t source );

int OTF_CopyHandler_DefFunctionGroup( void* userData, uint32_t stream,
	uint32_t funcGroup, const char* name );

int OTF_CopyHandler_DefCollectiveOperation( void* userData, uint32_t stream,
	uint32_t collOp, const char* name, uint32_t type );

int OTF_CopyHandler_DefCounter( void* userData, uint32_t stream, uint32_t counter,
	const char* name, uint32_t properties, uint32_t counterGroup,
	const char* unit );

int OTF_CopyHandler_DefCounterGroup( void* userData, uint32_t stream,
	uint32_t counterGroup, const char* name );

int OTF_CopyHandler_DefScl( void* userData, uint32_t stream, uint32_t source,
	uint32_t sourceFile, uint32_t line );

int OTF_CopyHandler_DefSclFile( void* userData, uint32_t stream,
	uint32_t sourceFile, const char* name );

int OTF_CopyHandler_DefVersion( void* userData, uint32_t stream,
	uint8_t major, uint8_t minor, uint8_t sub, const char* string );

int OTF_CopyHandler_DefCreator( void* userData, uint32_t stream,
	const char* creator );

int OTF_CopyHandler_DefFile( void* userData, uint32_t stream, uint32_t token,
	const char* name, uint32_t group );
	
int OTF_CopyHandler_DefFileGroup( void* userData, uint32_t stream,
	uint32_t token, const char* name );
	
	
int OTF_CopyHandler_Enter( void* userData, uint64_t time, uint32_t function, 
	uint32_t process, uint32_t source );

int OTF_CopyHandler_Leave( void* userData, uint64_t time, uint32_t function, 
	uint32_t process, uint32_t source );

int OTF_CopyHandler_SendMsg( void* userData, uint64_t time, uint32_t sender, 
	uint32_t receiver, uint32_t group, uint32_t type, uint32_t length,
	uint32_t source );

int OTF_CopyHandler_RecvMsg( void* userData, uint64_t time, uint32_t recvProc,
	uint32_t sendProc, uint32_t group, uint32_t type, uint32_t length, 
	uint32_t source );

int OTF_CopyHandler_Counter( void* userData, uint64_t time, uint32_t process, 
	uint32_t counter, uint64_t value );

int OTF_CopyHandler_CollectiveOperation( void* userData, uint64_t time,
	uint32_t process, uint32_t collective, uint32_t procGroup,
	uint32_t rootProc, uint32_t sent, uint32_t received, uint64_t duration, 
	uint32_t source );

int OTF_CopyHandler_EventComment( void* userData, uint64_t time, uint32_t process, 
	const char* comment );

int OTF_CopyHandler_BeginProcess( void* userData, uint64_t time, uint32_t process );

int OTF_CopyHandler_EndProcess( void* userData, uint64_t time, uint32_t process );

int OTF_CopyHandler_SnapshotComment( void* userData, uint64_t time,
	uint32_t process, const char* comment );

int OTF_CopyHandler_FileOperation( void* userData, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source );
	

int OTF_CopyHandler_EnterSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source );

int OTF_CopyHandler_SendSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t source );

int OTF_CopyHandler_OpenFileSnapshot( void* userData, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source );


int OTF_CopyHandler_SummaryComment( void * userData, uint64_t time,
	uint32_t process, const char* comment );

int OTF_CopyHandler_FunctionSummary( void* userData, uint64_t time,
	uint32_t function, uint32_t process, uint64_t invocations,
	uint64_t exclTime, uint64_t inclTime );

int OTF_CopyHandler_FunctionGroupSummary( void* userData, uint64_t time,
	uint32_t funcGroup, uint32_t process, uint64_t invocations,
	uint64_t exclTime, uint64_t inclTime );

int OTF_CopyHandler_MessageSummary( void* userData, uint64_t time, uint32_t process,
	uint32_t peer, uint32_t comm, uint32_t type, uint64_t sentNumber,
	uint64_t receivedNumber, uint64_t sentBytes, uint64_t receivedBytes );

int OTF_CopyHandler_FileOperationSummary( void* userData, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );

int OTF_CopyHandler_FileGroupOperationSummary( void* userData, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_COPYOTF_CopyHandler_R_H */

