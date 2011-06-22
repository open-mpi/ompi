/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Johannes Spazier
*/

#include "handler.h"

/* definitions */
int handleDefinitionComment (void *userData, uint32_t stream, const char *comment,
	OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefinitionCommentKV( wstream, comment,
			list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefTimerResolution (void *userData, uint32_t stream, uint64_t ticksPerSecond,
	OTF_KeyValueList *list) {
  
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefTimerResolutionKV( wstream, ticksPerSecond,
			list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefProcess (void *userData, uint32_t stream, uint32_t process, const char *name,
	uint32_t parent, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefProcessKV( wstream, process, name,
			parent, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefProcessGroup (void *userData, uint32_t stream, uint32_t procGroup, const char *name,
	uint32_t numberOfProcs, const uint32_t *procs, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefProcessGroupKV( wstream, procGroup, name,
			numberOfProcs, procs, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefAttributeList (void *userData, uint32_t stream, uint32_t attr_token, uint32_t num,
	OTF_ATTR_TYPE *array, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefAttributeListKV( wstream, attr_token,
			num, array, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefProcessOrGroupAttributes(void *userData, uint32_t stream, uint32_t proc_token,
	uint32_t attr_token, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefProcessOrGroupAttributesKV( wstream, proc_token,
			attr_token, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefFunction (void *userData, uint32_t stream, uint32_t func, const char *name,
	uint32_t funcGroup, uint32_t source, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefFunctionKV( wstream, func, name, funcGroup,
			source, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefFunctionGroup (void *userData, uint32_t stream, uint32_t funcGroup,
	const char *name, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefFunctionGroupKV( wstream, funcGroup,
			name, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefCollectiveOperation (void *userData, uint32_t stream, uint32_t collOp,
	const char *name, uint32_t type, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefCollectiveOperationKV( wstream, collOp,
			name, type, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefCounter (void *userData, uint32_t stream, uint32_t counter, const char *name,
	uint32_t properties, uint32_t counterGroup, const char *unit, OTF_KeyValueList *list) {
    
  
    OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 	  
	  	return ( 0 == OTF_WStream_writeDefCounterKV( wstream, counter, name,
			properties, counterGroup, unit, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefCounterGroup (void *userData, uint32_t stream, uint32_t counterGroup, const char *name,
	OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefCounterGroupKV( wstream, counterGroup,
			name, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefScl (void *userData, uint32_t stream, uint32_t source, uint32_t sourceFile,
	uint32_t line, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefSclKV( wstream, source, sourceFile,
			line, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefSclFile (void *userData, uint32_t stream, uint32_t sourceFile, const char *name,
	OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefSclFileKV( wstream, sourceFile,
			name, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefCreator (void *userData, uint32_t stream, const char *creator, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefCreatorKV( wstream, creator,
			list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefVersion (void *userData, uint32_t stream, uint8_t major, uint8_t minor,
	uint8_t sub, const char *string) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeOtfVersion( wstream ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefFile (void *userData, uint32_t stream, uint32_t token, const char *name,
	uint32_t group, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefFileKV( wstream, token, name,
			group, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefFileGroup (void *userData, uint32_t stream, uint32_t token, const char *name,
	OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefFileGroupKV( wstream, token, name,
			list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefKeyValue (void *userData, uint32_t stream, uint32_t token, OTF_Type type,
	const char *name, const char *desc, OTF_KeyValueList *list) {
    
  
      	OTF_WStream* wstream = (OTF_WStream*) userData;
  
  	if( 0 != stream ) {

		fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

		return OTF_RETURN_ABORT;

	} else {
	 
	  	return ( 0 == OTF_WStream_writeDefKeyValueKV( wstream, token, type,
			name, desc, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}

int handleDefTimeRange( void*             userData,
                        uint32_t          stream,
                        uint64_t          minTime,
                        uint64_t          maxTime,
                        OTF_KeyValueList* kvlist ) {


    OTF_WStream* wstream = (OTF_WStream*) userData;

    if ( 0 != stream ) {

        fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefTimeRange(
            wstream,
            minTime,
            maxTime,
            kvlist ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
    }
}

int handleDefCounterAssignments( void*             userData,
                                 uint32_t          stream,
                                 uint32_t          counter_token,
                                 uint32_t          number_of_members,
                                 const uint32_t*   procs_or_groups,
                                 OTF_KeyValueList* kvlist ) {


    OTF_WStream* wstream = (OTF_WStream*) userData;

    if ( 0 != stream ) {

        fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefCounterAssignments(
            wstream,
            counter_token,
            number_of_members,
            procs_or_groups,
            kvlist ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
    }
}

/* events */
int handleNoOp (void *userData, uint64_t time, uint32_t process, OTF_KeyValueList *list) {
  
  
   	OTF_WStream* wstream = (OTF_WStream*) userData;
  	
	return ( 0 == OTF_WStream_writeNoOpKV( wstream, time, process,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	
}

int handleEnter( void *userData, uint64_t time, uint32_t function, uint32_t process,
	uint32_t source, OTF_KeyValueList *list) {
  
  
  	OTF_WStream* wstream = (OTF_WStream*) userData;
	
	return ( 0 == OTF_WStream_writeEnterKV( wstream, time, function,
		process, source, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	
}

int handleLeave( void *userData, uint64_t time,	uint32_t function, uint32_t process,
	uint32_t source, OTF_KeyValueList *list ) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
	return ( 0 == OTF_WStream_writeLeaveKV( wstream, time, function,
		process, source, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleSendMsg( void *userData, uint64_t time, uint32_t sender, uint32_t receiver,
	uint32_t group, uint32_t type, uint32_t length, uint32_t source, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeSendMsgKV( wstream, time, sender, receiver, group,
		type, length, source, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleRecvMsg( void *userData, uint64_t time, uint32_t recvProc, uint32_t sendProc,
	uint32_t group, uint32_t type, uint32_t length, uint32_t source, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeRecvMsgKV( wstream, time, recvProc, sendProc,
		group, type, length, source, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleCounter( void *userData, uint64_t time, uint32_t process, uint32_t counter,
	uint64_t value, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeCounterKV( wstream, time, process, counter,
		value, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleCollectiveOperation( void *userData, uint64_t time, uint32_t process, uint32_t collective,
	uint32_t procGroup, uint32_t rootProc, uint32_t sent, uint32_t received, uint64_t duration,
	uint32_t source, OTF_KeyValueList *list) {
  
  
   	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeCollectiveOperationKV( wstream, time, process,
		collective, procGroup, rootProc, sent, received, duration, source,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleBeginCollectiveOperation( void *userData, uint64_t time, uint32_t process, uint32_t collOp,
	uint64_t matchingId, uint32_t procGroup, uint32_t rootProc, uint64_t sent, uint64_t received,
	uint32_t scltoken, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeBeginCollectiveOperationKV( wstream, time, process,
		collOp,	matchingId, procGroup, rootProc, sent, received, scltoken,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleEndCollectiveOperation( void *userData, uint64_t time, uint32_t process,
	uint64_t matchingId, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeEndCollectiveOperationKV( wstream, time, process,
		matchingId, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleEventComment( void *userData, uint64_t time, uint32_t process, const char *comment,
	OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeEventCommentKV( wstream, time, process, comment,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleBeginProcess( void *userData, uint64_t time, uint32_t process, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeBeginProcessKV( wstream, time, process,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleEndProcess( void *userData, uint64_t time, uint32_t process, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeEndProcessKV( wstream, time, process,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFileOperation( void *userData, uint64_t time, uint32_t fileid, uint32_t process,
	uint64_t handleid, uint32_t operation, 	uint64_t bytes, uint64_t duration, uint32_t source,
	OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeFileOperationKV( wstream, time, fileid,
		process, handleid, operation, bytes, duration, source,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleBeginFileOperation( void *userData, uint64_t time, uint32_t process,
	uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeBeginFileOperationKV( wstream, time, process,
		matchingId, scltoken, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleEndFileOperation( void *userData, uint64_t time, uint32_t process,
	uint32_t fileid, uint64_t matchingId, uint64_t handleId, uint32_t operation,
    uint64_t bytes,	uint32_t scltoken, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeEndFileOperationKV( wstream, time, process,
		fileid, matchingId, handleId, operation, bytes, scltoken,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleRMAPut( void *userData, uint64_t time, uint32_t process, uint32_t origin,
	uint32_t target, uint32_t communicator, uint32_t tag, uint64_t bytes,
	uint32_t source, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeRMAPutKV( wstream, time, process, origin,
		target, communicator, tag, bytes, source,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleRMAPutRemoteEnd( void *userData, uint64_t time, uint32_t process, uint32_t origin,
	uint32_t target, uint32_t communicator, uint32_t tag, uint64_t bytes, uint32_t source,
	OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeRMAPutRemoteEndKV( wstream, time, process, origin,
		target, communicator, tag, bytes, source,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleRMAGet( void *userData, uint64_t time, uint32_t process, uint32_t origin,
	uint32_t target, uint32_t communicator, uint32_t tag, uint64_t bytes,
	uint32_t source, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeRMAGetKV( wstream, time, process, origin,
		target, communicator, tag, bytes, source,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleRMAEnd( void *userData, uint64_t time, uint32_t process, uint32_t remote,
	uint32_t communicator, uint32_t tag, uint32_t source, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
  
	return ( 0 == OTF_WStream_writeRMAEndKV( wstream, time, process, remote,
		communicator, tag, source, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

/* snapshots */
int handleSnapshotComment(void *userData, uint64_t time, uint32_t process, const char *comment,
	OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeSnapshotCommentKV( wstream, time, process, 
		comment, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleEnterSnapshot(void *userData, uint64_t time, uint64_t originaltime, uint32_t function,
	uint32_t process, uint32_t source, OTF_KeyValueList *list) {
    
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeEnterSnapshotKV( wstream, time, originaltime, function,
		process, source, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
  
}

int handleSendSnapshot(void *userData, uint64_t time, uint64_t originaltime,
    uint32_t sender, uint32_t receiver, uint32_t procGroup, uint32_t tag,
    uint32_t length, uint32_t source, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeSendSnapshotKV( wstream, time, originaltime, sender,
		receiver, procGroup, tag, length, source,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleOpenFileSnapshot(void *userData, uint64_t time, uint64_t originaltime, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t source, OTF_KeyValueList *list) {
    
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeOpenFileSnapshotKV( wstream, time, originaltime, fileid,
		process, handleid, source, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
  
}

int handleBeginCollopSnapshot(void *userData, uint64_t time, uint64_t originaltime, uint32_t process,
    uint32_t collOp, uint64_t matchingId, uint32_t procGroup, uint32_t rootProc, uint64_t sent,
    uint64_t received, uint32_t scltoken, OTF_KeyValueList *list) {
  
 
    OTF_WStream* wstream = (OTF_WStream*) userData;
    
    return ( 0 == OTF_WStream_writeBeginCollopSnapshotKV( wstream, time, originaltime, process,
        collOp, matchingId, procGroup, rootProc, sent, received, scltoken, list) )
        ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleBeginFileOpSnapshot(void *userData, uint64_t time, uint64_t originaltime, uint32_t process,
    uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList *list) {
  
  
    OTF_WStream* wstream = (OTF_WStream*) userData;
    
    return ( 0 == OTF_WStream_writeBeginFileOpSnapshotKV( wstream, time, originaltime, process,
        matchingId, scltoken, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

/* statistics */
int handleSummaryComment(void *userData, uint64_t time, uint32_t process,
	const char *comment, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeSummaryCommentKV( wstream, time, process, comment,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFunctionSummary(void *userData, uint64_t time, uint32_t function,
	uint32_t process, uint64_t invocations, uint64_t exclTime, uint64_t inclTime,
	OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeFunctionSummaryKV( wstream, time, function, process,
		invocations, exclTime, inclTime,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFunctionGroupSummary(void *userData, uint64_t time, uint32_t funcGroup,
	uint32_t process, uint64_t invocations, uint64_t exclTime, uint64_t inclTime,
	OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeFunctionGroupSummaryKV( wstream, time, funcGroup,
		process, invocations, exclTime, inclTime, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleMessageSummary(void *userData, uint64_t time, uint32_t process, uint32_t peer,
	uint32_t comm, uint32_t type, uint64_t sentNumber, uint64_t receivedNumber,
	uint64_t sentBytes, uint64_t receivedBytes, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeMessageSummaryKV( wstream, time, process,
		peer, comm,	type, sentNumber, receivedNumber, sentBytes, receivedBytes,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleCollopSummary(void *userData, uint64_t time, uint32_t process, uint32_t comm,
	uint32_t collective, uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes,
	uint64_t receivedBytes, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeCollopSummaryKV( wstream, time, process,
		comm, collective, sentNumber, receivedNumber, sentBytes,
		receivedBytes, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFileOperationSummary(void *userData, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t nopen, uint64_t nclose, uint64_t nread, uint64_t nwrite,
	uint64_t nseek, uint64_t bytesread, uint64_t byteswrite, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeFileOperationSummaryKV( wstream, time,
		fileid, process, nopen, nclose, nread, nwrite, nseek, bytesread,
		byteswrite, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFileGroupOperationSummary(void *userData, uint64_t time, uint32_t groupid,
	uint32_t process, uint64_t nopen, uint64_t nclose, uint64_t nread, uint64_t nwrite,
	uint64_t nseek, uint64_t bytesread, uint64_t byteswrite, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
  	return ( 0 == OTF_WStream_writeFileGroupOperationSummaryKV( wstream, time,
		groupid, process, nopen, nclose, nread, nwrite, nseek, bytesread,
		byteswrite, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


/* marker */
int handleDefMarker( void *userData, uint32_t stream, uint32_t token, const char *name,
	uint32_t type, OTF_KeyValueList *list) {
  
  
  	OTF_WStream* wstream = (OTF_WStream*) userData;
	
	return ( 0 == OTF_WStream_writeDefMarkerKV( wstream, token, name, type,
		list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}
	
int handleMarker( void *userData, uint64_t time, uint32_t process, uint32_t token,
	const char *text, OTF_KeyValueList *list) {
  
  
    	OTF_WStream* wstream = (OTF_WStream*) userData;
	
	return ( 0 == OTF_WStream_writeMarkerKV( wstream, time, process, token,
		text, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}
	
	
/* unknown */
int handleUnknownRecord (void *userData, uint64_t time, uint32_t process, const char *record) {
  
  
	fprintf( stderr, "Error: unknown record >%s< at process 0x%x\n", record, process );

  	return OTF_RETURN_ABORT;
}

	
void setDefinitionHandlerArray( OTF_HandlerArray* handlers, OTF_WStream* wstream) {
  
  	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefinitionComment,
		OTF_DEFINITIONCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFINITIONCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefTimerResolution,
		OTF_DEFTIMERRESOLUTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFTIMERRESOLUTION_RECORD);

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefProcess,
		OTF_DEFPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefProcessGroup,
		OTF_DEFPROCESSGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFPROCESSGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefAttributeList,
		OTF_DEFATTRLIST_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFATTRLIST_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefProcessOrGroupAttributes,
		OTF_DEFPROCESSORGROUPATTR_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFPROCESSORGROUPATTR_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefFunction,
		OTF_DEFFUNCTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFFUNCTION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefFunctionGroup,
		OTF_DEFFUNCTIONGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFFUNCTIONGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefCollectiveOperation,
		OTF_DEFCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefCounter,
		OTF_DEFCOUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFCOUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefCounterGroup,
		OTF_DEFCOUNTERGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFCOUNTERGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefScl,
		OTF_DEFSCL_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFSCL_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefSclFile,
		OTF_DEFSCLFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFSCLFILE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefVersion,
		OTF_DEFVERSION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFVERSION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefCreator,
		OTF_DEFCREATOR_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFCREATOR_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefFile,
		OTF_DEFFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFFILE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefFileGroup,
		OTF_DEFFILEGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFFILEGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefKeyValue,
		OTF_DEFKEYVALUE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_DEFKEYVALUE_RECORD );
		
	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefMarker,
		OTF_DEFMARKER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_DEFMARKER_RECORD );
		
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefTimeRange,
        OTF_DEFTIMERANGE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream,
        OTF_DEFTIMERANGE_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefCounterAssignments,
        OTF_DEFCOUNTERASSIGNMENTS_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream,
        OTF_DEFCOUNTERASSIGNMENTS_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleUnknownRecord,
		OTF_UNKNOWN_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_UNKNOWN_RECORD );
}

void setEventHandlerArray( OTF_HandlerArray* handlers, OTF_WStream* wstream) {
   
  	/* events */
  	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEventComment,
		OTF_EVENTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_EVENTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleCounter,
		OTF_COUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_COUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleNoOp,
		OTF_NOOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_NOOP_RECORD );
		
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEnter,
		OTF_ENTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_ENTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleCollectiveOperation,
		OTF_COLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_COLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleBeginCollectiveOperation,
		OTF_BEGINCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_BEGINCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEndCollectiveOperation,
		OTF_ENDCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_ENDCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleRecvMsg,
		OTF_RECEIVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_RECEIVE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleSendMsg,
		OTF_SEND_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_SEND_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleLeave,
		OTF_LEAVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_LEAVE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleBeginProcess,
		OTF_BEGINPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_BEGINPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEndProcess,
		OTF_ENDPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_ENDPROCESS_RECORD );
			
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFileOperation,
		OTF_FILEOPERATION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_FILEOPERATION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleBeginFileOperation,
		OTF_BEGINFILEOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_BEGINFILEOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEndFileOperation,
		OTF_ENDFILEOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_ENDFILEOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
			(OTF_FunctionPointer*) handleRMAPut,
			OTF_RMAPUT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_RMAPUT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
			(OTF_FunctionPointer*) handleRMAPutRemoteEnd,
			OTF_RMAPUTRE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_RMAPUTRE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
			(OTF_FunctionPointer*) handleRMAGet,
			OTF_RMAGET_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_RMAGET_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
			(OTF_FunctionPointer*) handleRMAEnd,
			OTF_RMAEND_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_RMAEND_RECORD );
		
	/* snapshots */
	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleSnapshotComment,
		OTF_SNAPSHOTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_SNAPSHOTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEnterSnapshot,
		OTF_ENTERSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_ENTERSNAPSHOT_RECORD );
	
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleSendSnapshot,
		OTF_SENDSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_SENDSNAPSHOT_RECORD );	

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleOpenFileSnapshot,
		OTF_OPENFILESNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_OPENFILESNAPSHOT_RECORD );
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleBeginCollopSnapshot,
        OTF_BEGINCOLLOPSNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_BEGINCOLLOPSNAPSHOT_RECORD );
            
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleBeginFileOpSnapshot,
        OTF_BEGINFILEOPSNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_BEGINFILEOPSNAPSHOT_RECORD ); 
		
	/* statistics */
	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleSummaryComment,
		OTF_SUMMARYCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_SUMMARYCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFunctionSummary,
		OTF_FUNCTIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_FUNCTIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFunctionGroupSummary,
		OTF_FUNCTIONGROUPSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_FUNCTIONGROUPSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleMessageSummary,
		OTF_MESSAGESUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_MESSAGESUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleCollopSummary,
		OTF_COLLOPSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_COLLOPSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFileOperationSummary,
		OTF_FILEOPERATIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_FILEOPERATIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFileGroupOperationSummary,
		OTF_FILEGROUPOPERATIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
		(void*) wstream, OTF_FILEGROUPOPERATIONSUMMARY_RECORD );
		
	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleMarker,
		OTF_MARKER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
		(void*) wstream, OTF_MARKER_RECORD );
		
		
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleUnknownRecord,
		OTF_UNKNOWN_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        (void*) wstream, OTF_UNKNOWN_RECORD );
	
}
