/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef HANDLER_H
#define HANDLER_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_inttypes.h"
#include "OTF_Writer.h"


typedef struct {

	uint32_t deftoken;
	char *name;
	uint32_t group;
	uint32_t scltoken;
}functionT;

typedef struct {

	uint32_t deftoken;
	char* name;
}functiongroupT;

typedef struct {

	uint32_t streamid;

	uint64_t ticksPerSecond;
	
	functionT *functions;
	int nfunctions; /* number of functions in the functioninfo-array */
	int sfunctions; /* size of the functioninfo-array */
	functiongroupT *functiongroups;
	int nfunctiongroups; /* number of groups in the functiongroup-array */
	int sfunctiongroups; /* size of the functiongroups-array */
	
}streaminfoT;

typedef struct hashtabS
{
	streaminfoT entry;
	
	int entryvecsize;
	streaminfoT *p_entryvec;
}hashtabT;

typedef struct {

	OTF_Writer *writer;
	hashtabT *hash;
	int nstreaminfos; /* number of streams in the  streaminfos-array */

	int error;
	
}fcbT;

/* *** Definition handler *** ************************************* */

int handleDefinitionComment( void* firsthandlerarg, uint32_t streamid,
	const char* comment );
	
int handleDefTimerResolution( void* firsthandlerarg,
	uint32_t streamid, uint64_t ticksPerSecond );

int handleDefProcess( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t parent );

int handleDefProcessGroup( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t n, uint32_t* array );

int handleDefFunction( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t group, uint32_t scltoken );

int handleDefFunctionGroup( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name );

int handleDefCollectiveOperation( void* firsthandlerarg, uint32_t streamid,
	uint32_t collOp, const char* name, uint32_t type );

int handleDefCounter( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t properties, 
	uint32_t countergroup, const char* unit );

int handleDefCounterGroup( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name );

int handleDefScl( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, uint32_t sclfile, uint32_t sclline );

int handleDefSclFile( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* filename );

int handleDefCreator( void* firsthandlerarg, uint32_t streamid,
	const char* creator );

int handleDefFile( void* firsthandlerarg, uint32_t stream, uint32_t token,
	const char* name, uint32_t group );
	
int handleDefFileGroup( void* firsthandlerarg, uint32_t stream,
	uint32_t token, const char* name );

/* *** Event handler *** ****************************************** */

int handleEventComment( void* firsthandlerarg, uint64_t time,
	uint32_t process, const char* comment );

int handleCounter( void* firsthandlerarg, uint64_t time,
	uint32_t process, uint32_t counter_token, uint64_t value );

int handleEnter( void* firsthandlerarg, uint64_t time,
	uint32_t statetoken, uint32_t cpuid, uint32_t scltoken );

int handleCollectiveOperation( void* firsthandlerarg, uint64_t time,
    uint32_t process, uint32_t functionToken, uint32_t communicator, 
    uint32_t rootprocess, uint32_t sent, uint32_t received, 
    uint64_t duration, uint32_t scltoken );

int handleBeginCollectiveOperation( void* fcb, uint64_t time, uint32_t process,
		uint32_t collOp, uint64_t matchingId, uint32_t procGroup,
		uint32_t rootprocess, uint64_t sent, uint64_t received,
		uint32_t scltoken );

int handleEndCollectiveOperation( void* fcb, uint64_t time, uint32_t process,
		uint64_t matchingId );

int handleRecvMsg( void* firsthandlerarg, uint64_t time,
	uint32_t receiver, uint32_t sender, uint32_t communicator, 
	uint32_t msgtype, uint32_t msglength,
	uint32_t scltoken );

int handleSendMsg( void* firsthandlerarg, uint64_t time,
	uint32_t sender, uint32_t receiver, uint32_t communicator, 
	uint32_t msgtype, uint32_t msglength, uint32_t scltoken );

int handleLeave( void* firsthandlerarg, uint64_t time,
	uint32_t statetoken, uint32_t cpuid, uint32_t scltoken );

int handleBeginProcess( void* firsthandlerarg, uint64_t time,
	uint32_t process );

int handleEndProcess( void* firsthandlerarg, uint64_t time,
	uint32_t process );

int handleFileOperation( void* firsthandlerarg, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source );

int handleBeginFileOperation( void* fcb, uint64_t time, uint32_t process,
		uint64_t handleid, uint32_t scltoken );

int handleEndFileOperation( void* fcb, uint64_t time, uint32_t process,
		uint32_t fileid, uint64_t handleid, uint32_t operation,
		uint64_t bytes, uint32_t scltoken );

int handleRMAPut( void* firsthandlerarg, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken );

int handleRMAPutRemoteEnd( void* firsthandlerarg, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes,
        uint32_t scltoken );

int handleRMAGet( void* firsthandlerarg, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken);

int handleRMAEnd( void* firsthandlerarg, uint64_t time, uint32_t process,
        uint32_t remote, uint32_t communicator, uint32_t tag, uint32_t scltoken );


/* *** Handlers for OTF snapshot records ****************************** */


int handleSnapshotComment( void *firsthandlerarg, uint64_t time,
	uint32_t process, const char* comment );
	
int handleEnterSnapshot( void *firsthandlerarg, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source );

int handleSendSnapshot( void *firsthandlerarg, uint64_t time,
	uint64_t originaltime, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t source );
	
int handleOpenFileSnapshot( void* firsthandlerarg, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process,
	uint64_t handleid, uint32_t source );

	
/* *** Summary handler *** ****************************************** */

int handleSummaryComment( void* firsthandlerarg, uint64_t time,
	uint32_t process, const char* comment );
	
int handleFunctionSummary( void* firsthandlerarg,
	uint64_t time, uint32_t function, uint32_t process, 
	uint64_t count, uint64_t excltime, uint64_t incltime );

int handleFunctionGroupSummary( void* firsthandlerarg,
	uint64_t time, uint32_t functiongroup, uint32_t process, 
	uint64_t count, uint64_t excltime, uint64_t incltime );

int handleMessageSummary( void* firsthandlerarg,
	uint64_t time, uint32_t process, uint32_t peer, 
	uint32_t comm, uint32_t tag, uint64_t number_sent, uint64_t number_recvd,
	uint64_t bytes_sent, uint64_t bytes_recved );

int handleCollopSummary( void* firsthandlerarg,
	uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
	uint64_t number_sent, uint64_t number_recvd, uint64_t bytes_sent,
	uint64_t bytes_recved );


int handleFileOperationSummary( void* firsthandlerarg, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t nopen, uint64_t nclose, uint64_t nread,
	uint64_t nwrite, uint64_t nseek, uint64_t bytesread, uint64_t byteswrite );

int handleFileGroupOperationSummary( void* firsthandlerarg, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );


/* *** Marker handler *** ******************************************* */


int handleDefMarker( void *userData, uint32_t stream, 
	uint32_t token, const char* name, uint32_t type );

int handleMarker( void *userData, uint64_t time,
	uint32_t process, uint32_t token, const char* text );
    

/* *** Misc handlers *** ******************************************** */

int handleUnknown( void* fcb, uint64_t time, uint32_t process, const char* record );


#endif /* OTF_handleH */
