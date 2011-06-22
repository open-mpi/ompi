/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Johannes Spazier
*/

#ifndef HANDLER_H
#define HANDLER_H

#ifdef HAVE_CONFIG_H
#	include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>

#if defined(HAVE_SYS_SELECT_H) && HAVE_SYS_SELECT_H
#	include <sys/select.h>
#else /* HAVE_SYS_SELECT_H */
#	include <sys/time.h>
#	include <sys/types.h>
#	include <unistd.h>
#endif /* HAVE_SYS_SELECT_H */

#include <mpi.h>

#include "otf.h"

/* data structures */

typedef struct {
  int id;
  int num_cpus;
  int *cpus;
} OutStream;

typedef struct {
  int num_ostreams;
  OutStream *ostreams;
} RankData;

typedef struct {
  double progress;
  uint8_t is_alive;
} ProgressBuf;

typedef struct {
  MPI_Request request;
  ProgressBuf value;
  ProgressBuf buf;
  int num_cpus;
  double percent;
} ProgressInfo;

typedef struct {
  int my_rank;
  int num_ranks;
  int ranks_alive;
  double tmp_progress;
  MPI_Datatype buftype;
} GlobalData;

/* function declarations */

double update_progress( ProgressInfo* info, GlobalData* data, int cur_ostream, int num_ostreams);

int finish_everything( char *infile, char* outfile, ProgressInfo* info,
	RankData* rank_data, int ret );

void setDefinitionHandlerArray( OTF_HandlerArray* handlers, OTF_WStream* wstream);

void setEventHandlerArray( OTF_HandlerArray* handlers, OTF_WStream* wstream);


/* handlers */

/* definitions */

int handleDefinitionComment (void *userData, uint32_t stream, const char *comment,
	OTF_KeyValueList *list);

int handleDefTimerResolution (void *userData, uint32_t stream, uint64_t ticksPerSecond,
	OTF_KeyValueList *list);

int handleDefProcess (void *userData, uint32_t stream, uint32_t process, const char *name,
	uint32_t parent, OTF_KeyValueList *list);

int handleDefProcessGroup (void *userData, uint32_t stream, uint32_t procGroup, const char *name,
	uint32_t numberOfProcs, const uint32_t *procs, OTF_KeyValueList *list);

int handleDefAttributeList (void *userData, uint32_t stream, uint32_t attr_token, uint32_t num,
	OTF_ATTR_TYPE *array, OTF_KeyValueList *list);

int handleDefProcessOrGroupAttributes(void *userData, uint32_t stream, uint32_t proc_token,
	uint32_t attr_token, OTF_KeyValueList *list);

int handleDefFunction (void *userData, uint32_t stream, uint32_t func, const char *name,
	uint32_t funcGroup, uint32_t source, OTF_KeyValueList *list);

int handleDefFunctionGroup (void *userData, uint32_t stream, uint32_t funcGroup,
	const char *name, OTF_KeyValueList *list);

int handleDefCollectiveOperation (void *userData, uint32_t stream, uint32_t collOp,
	const char *name, uint32_t type, OTF_KeyValueList *list);

int handleDefCounter (void *userData, uint32_t stream, uint32_t counter, const char *name,
	uint32_t properties, uint32_t counterGroup, const char *unit, OTF_KeyValueList *list);

int handleDefCounterGroup (void *userData, uint32_t stream, uint32_t counterGroup, const char *name,
	OTF_KeyValueList *list);

int handleDefScl (void *userData, uint32_t stream, uint32_t source, uint32_t sourceFile,
	uint32_t line, OTF_KeyValueList *list);

int handleDefSclFile (void *userData, uint32_t stream, uint32_t sourceFile, const char *name,
	OTF_KeyValueList *list);

int handleDefCreator (void *userData, uint32_t stream, const char *creator, OTF_KeyValueList *list);

int handleDefVersion (void *userData, uint32_t stream, uint8_t major, uint8_t minor,
	uint8_t sub, const char *string);

int handleDefFile (void *userData, uint32_t stream, uint32_t token, const char *name,
	uint32_t group, OTF_KeyValueList *list);

int handleDefFileGroup (void *userData, uint32_t stream, uint32_t token, const char *name,
	OTF_KeyValueList *list);

int handleDefKeyValue (void *userData, uint32_t stream, uint32_t token, OTF_Type type,
	const char *name, const char *desc, OTF_KeyValueList *list);

int handleDefTimeRange( void*             userData,
                        uint32_t          stream,
                        uint64_t          counter_token,
                        uint64_t          number_of_members,
                        OTF_KeyValueList* kvlist );

int handleDefCounterAssignments( void*             userData,
                                 uint32_t          stream,
                                 uint32_t          counter_token,
                                 uint32_t          number_of_members,
                                 const uint32_t*   procs_or_groups,
                                 OTF_KeyValueList* kvlist );

/* events */

int handleNoOp (void *userData, uint64_t time, uint32_t process, OTF_KeyValueList *list);

int handleEnter (void *userData, uint64_t time, uint32_t function, uint32_t process,
	uint32_t source, OTF_KeyValueList *list);
	
int handleLeave( void *userData, uint64_t time,	uint32_t function, uint32_t process,
	uint32_t source, OTF_KeyValueList *list );
	
int handleSendMsg( void *userData, uint64_t time, uint32_t sender, uint32_t receiver,
	uint32_t group, uint32_t type, uint32_t length, uint32_t source, OTF_KeyValueList *list);

int handleRecvMsg( void *userData, uint64_t time, uint32_t recvProc, uint32_t sendProc,
	uint32_t group, uint32_t type, uint32_t length, uint32_t source, OTF_KeyValueList *list);

int handleCounter( void *userData, uint64_t time, uint32_t process, uint32_t counter,
	uint64_t value, OTF_KeyValueList *list);

int handleCollectiveOperation( void *userData, uint64_t time, uint32_t process, uint32_t collective,
	uint32_t procGroup, uint32_t rootProc, uint32_t sent, uint32_t received, uint64_t duration,
	uint32_t source, OTF_KeyValueList *list);

int handleBeginCollectiveOperation( void *userData, uint64_t time, uint32_t process, uint32_t collOp,
	uint64_t matchingId, uint32_t procGroup, uint32_t rootProc, uint64_t sent, uint64_t received,
	uint32_t scltoken, OTF_KeyValueList *list);

int handleEndCollectiveOperation( void *userData, uint64_t time, uint32_t process,
	uint64_t matchingId, OTF_KeyValueList *list);

int handleEventComment( void *userData, uint64_t time, uint32_t process, const char *comment,
	OTF_KeyValueList *list);

int handleBeginProcess( void *userData, uint64_t time, uint32_t process, OTF_KeyValueList *list);

int handleEndProcess( void *userData, uint64_t time, uint32_t process, OTF_KeyValueList *list);

int handleFileOperation( void *userData, uint64_t time, uint32_t fileid, uint32_t process,
	uint64_t handleid, uint32_t operation, 	uint64_t bytes, uint64_t duration, uint32_t source,
	OTF_KeyValueList *list);

int handleBeginFileOperation( void *userData, uint64_t time, uint32_t process,
	uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList *list);

int handleEndFileOperation( void *userData, uint64_t time, uint32_t process,
	uint32_t fileid, uint64_t matchingId, uint64_t handleId, uint32_t operation, uint64_t bytes,
	uint32_t scltoken, OTF_KeyValueList *list);

int handleRMAPut( void *userData, uint64_t time, uint32_t process, uint32_t origin,
	uint32_t target, uint32_t communicator, uint32_t tag, uint64_t bytes,
	uint32_t source, OTF_KeyValueList *list);

int handleRMAPutRemoteEnd( void *userData, uint64_t time, uint32_t process, uint32_t origin,
	uint32_t target, uint32_t communicator, uint32_t tag, uint64_t bytes, uint32_t source,
	OTF_KeyValueList *list);

int handleRMAGet( void *userData, uint64_t time, uint32_t process, uint32_t origin,
	uint32_t target, uint32_t communicator, uint32_t tag, uint64_t bytes,
	uint32_t source, OTF_KeyValueList *list);

int handleRMAEnd( void *userData, uint64_t time, uint32_t process, uint32_t remote,
	uint32_t communicator, uint32_t tag, uint32_t source, OTF_KeyValueList *list);

/* snapshots */

int handleSnapshotComment (void *userData, uint64_t time, uint32_t process, const char *comment,
	OTF_KeyValueList *list);

int handleEnterSnapshot (void *userData, uint64_t time, uint64_t originaltime, uint32_t function,
	uint32_t process, uint32_t source, OTF_KeyValueList *list);

int handleSendSnapshot (void *userData, uint64_t time, uint64_t originaltime,
	uint32_t sender, uint32_t receiver, uint32_t procGroup, uint32_t tag,
	uint32_t length, uint32_t source, OTF_KeyValueList *list);

int handleOpenFileSnapshot (void *userData, uint64_t time, uint64_t originaltime, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t source, OTF_KeyValueList *list);

int handleBeginCollopSnapshot(void *userData, uint64_t time, uint64_t originaltime, uint32_t process,
	uint32_t collOp, uint64_t matchingId, uint32_t procGroup, uint32_t rootProc, uint64_t sent,
	uint64_t received, uint32_t scltoken, OTF_KeyValueList *list);

int handleBeginFileOpSnapshot(void *userData, uint64_t time, uint64_t originaltime, uint32_t process,
	uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList *list);

/* statistics */

int handleSummaryComment (void *userData, uint64_t time, uint32_t process,
	const char *comment, OTF_KeyValueList *list);

int handleFunctionSummary (void *userData, uint64_t time, uint32_t function,
	uint32_t process, uint64_t invocations, uint64_t exclTime, uint64_t inclTime,
	OTF_KeyValueList *list);

int handleFunctionGroupSummary (void *userData, uint64_t time, uint32_t funcGroup,
	uint32_t process, uint64_t invocations, uint64_t exclTime, uint64_t inclTime,
	OTF_KeyValueList *list);

int handleMessageSummary (void *userData, uint64_t time, uint32_t process, uint32_t peer,
	uint32_t comm, uint32_t type, uint64_t sentNumber, uint64_t receivedNumber,
	uint64_t sentBytes, uint64_t receivedBytes, OTF_KeyValueList *list);

int handleCollopSummary (void *userData, uint64_t time, uint32_t process, uint32_t comm,
	uint32_t collective, uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes,
	uint64_t receivedBytes, OTF_KeyValueList *list);

int handleFileOperationSummary (void *userData, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t nopen, uint64_t nclose, uint64_t nread, uint64_t nwrite,
	uint64_t nseek, uint64_t bytesread, uint64_t byteswrite, OTF_KeyValueList *list);

int handleFileGroupOperationSummary (void *userData, uint64_t time, uint32_t groupid,
	uint32_t process, uint64_t nopen, uint64_t nclose, uint64_t nread, uint64_t nwrite,
	uint64_t nseek, uint64_t bytesread, uint64_t byteswrite, OTF_KeyValueList *list);

/* marker */

int handleDefMarker( void *userData, uint32_t stream, uint32_t token, const char *name,
	uint32_t type, OTF_KeyValueList *list);
	
int handleMarker( void *userData, uint64_t time, uint32_t process, uint32_t token,
	const char *text, OTF_KeyValueList *list);

/* unknown */

int handleUnknownRecord (void *userData, uint64_t time, uint32_t process, const char *record);

#endif /* HANDLER_H */
