/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef OTFTOVTF3_HANDLER_H
#define OTFTOVTF3_HANDLER_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_inttypes.h"

#include "otf.h"

typedef struct {


	uint64_t num;

	uint64_t minNum;
	uint64_t maxNum;

	bool records[OTF_NRECORDS]; /* enabled record types */

	FILE* outfile;

} Control;

/* *** Definition handler *** ************************************* */


int handleDefinitionComment( void* userData, uint32_t stream,
	const char* comment );

int handleDefTimerResolution( void* userData, uint32_t stream,
	uint64_t ticksPerSecond );

int handleDefProcess( void* userData, uint32_t stream, uint32_t process,
	const char* name, uint32_t parent );

int handleDefProcessGroup( void* userData, uint32_t stream,
	uint32_t procGroup, const char* name, uint32_t numberOfProcs,
	const uint32_t* procs );

int handleDefFunction( void* userData, uint32_t stream, uint32_t func,
	const char* name, uint32_t funcGroup, uint32_t source );

int handleDefFunctionGroup( void* userData, uint32_t stream,
	uint32_t funcGroup, const char* name );

int handleDefCollectiveOperation( void* userData, uint32_t stream,
	uint32_t collOp, const char* name, uint32_t type );

int handleDefCounter( void* userData, uint32_t stream, uint32_t counter,
	const char* name, uint32_t properties, uint32_t counterGroup,
	const char* unit );

int handleDefCounterGroup( void* userData, uint32_t stream,
	uint32_t counterGroup, const char* name );

int handleDefScl( void* userData, uint32_t stream, uint32_t source,
	uint32_t sourceFile, uint32_t line );

int handleDefSclFile( void* userData, uint32_t stream,
	uint32_t sourceFile, const char* name );

int handleDefCreator( void* userData, uint32_t stream,
	const char* creator );

int handleDefVersion( void* userData, uint32_t stream, uint8_t major,
	uint8_t minor, uint8_t sub, const char* string );


int handleEnter( void* userData, uint64_t time, uint32_t function,
	uint32_t process, uint32_t source );

int handleLeave( void* userData, uint64_t time, uint32_t function,
	uint32_t process, uint32_t source );

int handleSendMsg( void* userData, uint64_t time, uint32_t sender,
	uint32_t receiver, uint32_t group, uint32_t type, uint32_t length,
	uint32_t source );

int handleRecvMsg( void* userData, uint64_t time, uint32_t recvProc,
	uint32_t sendProc, uint32_t group, uint32_t type, uint32_t length, 
	uint32_t source );

int handleCounter( void* userData, uint64_t time, uint32_t process,
	uint32_t counter, uint64_t value );

int handleCollectiveOperation( void* userData, uint64_t time,
	uint32_t process, uint32_t collective, uint32_t procGroup,
	uint32_t rootProc, uint32_t sent, uint32_t received, uint64_t duration, 
	uint32_t source );

int handleBeginCollectiveOperation( void* userData, uint64_t time,
		uint32_t process, uint32_t collOp, uint64_t matchingId,
		uint32_t procGroup, uint32_t rootprocess, uint32_t sent,
		uint32_t received, uint32_t scltoken );

int handleEndCollectiveOperation( void* userData, uint64_t time,
		uint32_t process, uint64_t matchingId );

int handleEventComment( void* userData, uint64_t time, uint32_t process,
	const char* comment );

int handleBeginProcess( void* userData, uint64_t time, uint32_t process );

int handleEndProcess( void* userData, uint64_t time, uint32_t process );


int handleSnapshotComment( void* userData, uint64_t time,
	uint32_t process, const char* comment );

int handleEnterSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source );

int handleSendSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t source );

int handleOpenFileSnapshot( void* userData, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source );


int handleSummaryComment( void * userData, uint64_t time,
	uint32_t process, const char* comment );

int handleFunctionSummary( void* userData, uint64_t time,
	uint32_t function, uint32_t process, uint64_t invocations,
	uint64_t exclTime, uint64_t inclTime );

int handleFunctionGroupSummary( void* userData, uint64_t time,
	uint32_t funcGroup, uint32_t process, uint64_t invocations,
	uint64_t exclTime, uint64_t inclTime );

int handleMessageSummary( void* userData, uint64_t time, uint32_t process,
	uint32_t peer, uint32_t comm, uint32_t type, uint64_t sentNumber,
	uint64_t receivedNumber, uint64_t sentBytes, uint64_t receivedBytes );

int handleCollopSummary( void* userData, uint64_t time, uint32_t process,
	uint32_t comm, uint32_t collective, uint64_t sentNumber, uint64_t receivedNumber,
	uint64_t sentBytes, uint64_t receivedBytes);

int handleDefFile( void* userData, uint32_t stream,
                   uint32_t token, const char *name,
                   uint32_t group );

int handleDefFileGroup( void* userData, uint32_t stream,
                        uint32_t token, const char *name );

int handleFileOperation( void* userData, uint64_t time,
                         uint32_t fileid, uint32_t process,
                         uint64_t handleid, uint32_t operation,
                         uint64_t bytes, uint64_t duration,
                         uint32_t source );

int handleBeginFileOperation( void* userData, uint64_t time, uint32_t process,
		uint64_t handleid, uint32_t scltoken );

int handleEndFileOperation( void* userData, uint64_t time, uint32_t process,
		uint32_t fileid, uint64_t handleid, uint32_t operation,
		uint64_t bytes, uint32_t scltoken );

int handleRMAPut( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken );

int handleRMAPutRemoteEnd( void* userData, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes,
        uint32_t scltoken );

int handleRMAGet( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken);

int handleRMAEnd( void* userData, uint64_t time, uint32_t process,
        uint32_t remote, uint32_t communicator, uint32_t tag, uint32_t scltoken );

int handleUnknown( void* fcb, uint64_t time, uint32_t process,
	const char* record );


int handleDefMarker( void *userData, uint32_t stream, uint32_t token, const char* name, uint32_t type );

int handleMarker( void *userData, uint64_t time, uint32_t process,
	uint32_t token, const char* text );


#endif /* OTFTOVTF3_HANDLER_H */
