/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include <assert.h>

#include "OTF_Platform.h"
#include "OTF_Writer.h"

#include "handler.h"
#include "hash.h"

/* *** Definition handler *** ************************************* */

int handleDefinitionComment( void* fcbx, uint32_t streamid,
	const char* comment, OTF_KeyValueList *list ) {
	

	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );

		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefinitionCommentKV( ((fcbT*) fcbx)->writer, streamid,
			comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}
	
int handleDefTimerResolution( void* fcbx,
	uint32_t streamid, uint64_t ticksPerSecond, OTF_KeyValueList *list ) {
	
	
	fcbT *fcb;
	streaminfoT* si;
	

	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {


		fcb = ((fcbT*) fcbx);
		si = hash_search( fcb->hash, streamid );

		si->ticksPerSecond = ticksPerSecond;
	
		return ( 0 == OTF_Writer_writeDefTimerResolutionKV( fcb->writer, streamid,
			ticksPerSecond, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}


int handleDefProcess( void* fcbx, uint32_t streamid, uint32_t deftoken,
	const char* name, uint32_t parent, OTF_KeyValueList *list ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefProcessKV( ((fcbT*) fcbx)->writer, streamid,
			deftoken, name, parent, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefProcessGroup( void* fcbx, uint32_t streamid, uint32_t deftoken,
	const char* name, uint32_t n, uint32_t* array, OTF_KeyValueList *list ) {



	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefProcessGroupKV( ((fcbT*) fcbx)->writer, streamid, deftoken,
			name, n, array, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}

int handleDefAttributeList( void* fcbx, uint32_t stream,
	uint32_t attr_token, uint32_t num, OTF_ATTR_TYPE* array, OTF_KeyValueList* list ) {


	if( 0 != stream ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefAttributeListKV( ((fcbT*) fcbx)->writer, stream, attr_token,
			num, array, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}

int handleDefProcessOrGroupAttributes( void* fcbx, uint32_t stream,
	uint32_t proc_token, uint32_t attr_token, OTF_KeyValueList* list ) {


	if( 0 != stream ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefProcessOrGroupAttributesKV( ((fcbT*) fcbx)->writer, stream, proc_token,
			attr_token, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefFunction(  void* fcbx, uint32_t streamid,
		uint32_t deftoken, const char* name, 
		uint32_t group, uint32_t scltoken, OTF_KeyValueList *list ) {


	fcbT *fcb;
	streaminfoT* si;
	
	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		fcb = ((fcbT*) fcbx);
		si = hash_search( fcb->hash, streamid );
		

		/* allocate new memory if necessary */
		if ( si->nfunctions >= si->sfunctions )
		{
			si->sfunctions = ( si->sfunctions > 0 ) ? ( 2* si->sfunctions ) : 20;
			si->functions = (functionT*) realloc( si->functions,
				si->sfunctions * sizeof(functionT) );
				
			assert( NULL != si->functions );
		}
	
		/* insert all data about the function */
		si->functions[si->nfunctions].deftoken = deftoken;
		si->functions[si->nfunctions].name = strdup( name );
		si->functions[si->nfunctions].group = group;
		si->functions[si->nfunctions].scltoken = scltoken;
	
		++(si->nfunctions);
			
		return ( 0 == OTF_Writer_writeDefFunctionKV( fcb->writer, streamid, deftoken,
			name, group, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefFunctionGroup( void* fcbx, uint32_t streamid,
		uint32_t deftoken, const char* name, OTF_KeyValueList *list ) {


	fcbT *fcb;
	streaminfoT* si;
	
	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		fcb = ((fcbT*) fcbx);
		si = hash_search( fcb->hash, streamid );
		
		/* allocate new memory if necessary */
		if ( si->nfunctiongroups >= si->sfunctiongroups )
		{
			si->sfunctiongroups = ( si->sfunctiongroups > 0 )
				? ( 2* si->sfunctiongroups ) : 10;
			si->functiongroups = (functiongroupT*) realloc( si->functiongroups,
				si->sfunctiongroups * sizeof(functiongroupT) );
				
			assert( NULL != si->functiongroups );
		}
	
		/* insert all data about the function */
		si->functiongroups[si->nfunctiongroups].deftoken = deftoken;
		si->functiongroups[si->nfunctiongroups].name = strdup( name );
	
		++(si->nfunctiongroups);
		
		return ( 0 == OTF_Writer_writeDefFunctionGroupKV( fcb->writer, streamid,
			deftoken, name, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefCollectiveOperation( void* fcbx, uint32_t streamid,
	uint32_t collOp, const char* name, uint32_t type, OTF_KeyValueList *list ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefCollectiveOperationKV( ((fcbT*) fcbx)->writer, streamid,
			collOp, name, type, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}


int handleDefCounter( void* fcbx, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t properties, 
		uint32_t countergroup, const char* unit, OTF_KeyValueList *list ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefCounterKV( ((fcbT*) fcbx)->writer, streamid,
			deftoken, name, properties, countergroup, unit, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefCounterGroup( void* fcbx, uint32_t streamid,
		uint32_t deftoken, const char* name, OTF_KeyValueList *list ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefCounterGroupKV( ((fcbT*) fcbx)->writer, streamid, 
			deftoken, name, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefScl(  void* fcbx, uint32_t streamid,
		uint32_t deftoken, uint32_t sclfile, uint32_t sclline, OTF_KeyValueList *list ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefSclKV( ((fcbT*) fcbx)->writer, streamid, deftoken,
			sclfile, sclline, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefSclFile(  void* fcbx, uint32_t streamid,
		uint32_t deftoken, const char* filename, OTF_KeyValueList *list ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefSclFileKV( ((fcbT*) fcbx)->writer, streamid, 
			deftoken, filename, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefCreator( void* fcbx, uint32_t streamid,
		const char* creator, OTF_KeyValueList *list ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefCreatorKV( ((fcbT*) fcbx)->writer, streamid,
			creator, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}


int handleDefFile( void* fcbx, uint32_t streamid, uint32_t token,
	const char* name, uint32_t group, OTF_KeyValueList *list ) {

	
	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefFileKV( ((fcbT*) fcbx)->writer,
		streamid, token, name, group, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}
	
	
int handleDefFileGroup( void* fcbx, uint32_t streamid,
	uint32_t token, const char* name, OTF_KeyValueList *list ) {

	
	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefFileGroupKV( ((fcbT*) fcbx)->writer,
		streamid, token, name, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}

int handleDefKeyValue( void *fcbx, uint32_t streamid, uint32_t token,
	OTF_Type type, const char *name, const char *desc, OTF_KeyValueList *list) {

	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefKeyValueKV( ((fcbT*) fcbx)->writer,
		streamid, token, type, name, desc, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}

int handleDefTimeRange( void*             fcbx,
                        uint32_t          streamid,
                        uint64_t          minTime,
                        uint64_t          maxTime,
                        OTF_KeyValueList* list ) {


    if ( 0 != streamid ) {

        fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_Writer_writeDefTimeRange(
            ((fcbT*) fcbx)->writer,
            streamid,
            minTime,
            maxTime,
            list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
    }
}

int handleDefCounterAssignments( void*             fcbx,
                                 uint32_t          streamid,
                                 uint32_t          counter_token,
                                 uint32_t          number_of_members,
                                 const uint32_t*   procs_or_groups,
                                 OTF_KeyValueList* list ) {


    if ( 0 != streamid ) {

        fprintf( stderr, "Error: cannot merge traces with local definitions. Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_Writer_writeDefCounterAssignments(
            ((fcbT*) fcbx)->writer,
            streamid,
            counter_token,
            number_of_members,
            procs_or_groups,
            list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
    }
}

/* *** Event handler *** ****************************************** */


int handleNoOp( void* fcb, uint64_t time,
	uint32_t process, OTF_KeyValueList *list ) {
 
  	return ( 0 == OTF_Writer_writeNoOpKV( ((fcbT*) fcb)->writer, time, process,
		list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleEventComment( void* fcb, uint64_t time, uint32_t process,
		const char* comment, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeEventCommentKV( ((fcbT*) fcb)->writer, time, process,
		comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleCounter( void* fcb, uint64_t time, uint32_t process,
	uint32_t counter_token, uint64_t value, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeCounterKV( ((fcbT*) fcb)->writer, time, 
		process, counter_token, value, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleEnter( void* fcb, uint64_t time, uint32_t statetoken,
	uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeEnterKV( ((fcbT*) fcb)->writer, time, 
		statetoken,	cpuid, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleCollectiveOperation( void* fcb, uint64_t time,
    	uint32_t process, uint32_t functionToken, uint32_t communicator, 
    	uint32_t rootprocess, uint32_t sent, uint32_t received, 
    	uint64_t duration, uint32_t scltoken, OTF_KeyValueList *list ) {


    	return ( 0 == OTF_Writer_writeCollectiveOperationKV( ((fcbT*) fcb)->writer, time, 
    	process, functionToken, communicator, rootprocess, 
		sent, received, duration, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleBeginCollectiveOperation( void* fcb, uint64_t time, uint32_t process,
		uint32_t collOp, uint64_t matchingId, uint32_t procGroup,
		uint32_t rootprocess, uint64_t sent, uint64_t received,
		uint32_t scltoken, OTF_KeyValueList *list )
{
	
	return (0 == OTF_Writer_writeBeginCollectiveOperationKV(
			((fcbT*) fcb)->writer, time, process, collOp,
			matchingId, procGroup, rootprocess, sent, received,
			scltoken, list )) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleEndCollectiveOperation( void* fcb, uint64_t time, uint32_t process,
		uint64_t matchingId, OTF_KeyValueList *list )
{
	return (0 == OTF_Writer_writeEndCollectiveOperationKV(
			((fcbT*) fcb)->writer, time, process, matchingId, list)) ?
			OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleRecvMsg( void* fcb, uint64_t time,
		uint32_t receiver, uint32_t sender, uint32_t communicator, 
		uint32_t msgtype, uint32_t msglength, uint32_t scltoken, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeRecvMsgKV( ((fcbT*) fcb)->writer, time, receiver,
		sender, communicator, msgtype, msglength, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleSendMsg( void* fcb, uint64_t time,
		uint32_t sender, uint32_t receiver, uint32_t communicator, 
		uint32_t msgtype, uint32_t msglength, uint32_t scltoken, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeSendMsgKV( ((fcbT*) fcb)->writer, time, sender,
		receiver, communicator, msgtype, msglength, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleLeave( void* fcb, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeLeaveKV( ((fcbT*) fcb)->writer, time, statetoken,
		cpuid, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleBeginProcess( void* fcb, uint64_t time,
		uint32_t cpuid, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeBeginProcessKV( ((fcbT*) fcb)->writer, time, cpuid, list ) );
}
int handleEndProcess( void* fcb, uint64_t time,
		uint32_t cpuid, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeEndProcessKV( ((fcbT*) fcb)->writer, time, cpuid, list ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleFileOperation( void* fcb, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeFileOperationKV( ((fcbT*) fcb)->writer, time, fileid,
		process, handleid, operation, bytes, duration, source, list ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleBeginFileOperation( void* fcb, uint64_t time,	uint32_t process,
	uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList *list )
{
	return (0 == OTF_Writer_writeBeginFileOperationKV( ((fcbT*) fcb)->writer,
			time, process, matchingId, scltoken, list )) ? OTF_RETURN_ABORT
			: OTF_RETURN_OK;
}


int handleEndFileOperation( void* fcb, uint64_t time,
		uint32_t process, uint32_t fileid, uint64_t matchingId, uint64_t handleId,
		uint32_t operation, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList *list )
{
	return (0 == OTF_Writer_writeEndFileOperationKV( ((fcbT*) fcb)->writer,
			time, process, fileid, matchingId, handleId, operation, bytes,
			scltoken, list )) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleRMAPut( void* fcb, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList *list ) {


        return ( 0 == OTF_Writer_writeRMAPutKV( ((fcbT*) fcb)->writer, time,
                process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleRMAPutRemoteEnd( void* fcb, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes,
        uint32_t scltoken, OTF_KeyValueList *list ) {


        return ( 0 == OTF_Writer_writeRMAPutRemoteEndKV( ((fcbT*) fcb)->writer,
                time, process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleRMAGet( void* fcb, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList *list) {


        return ( 0 == OTF_Writer_writeRMAGetKV( ((fcbT*) fcb)->writer, time,
                process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleRMAEnd( void* fcb, uint64_t time, uint32_t process, uint32_t remote, 
	uint32_t communicator, uint32_t tag, uint32_t scltoken, OTF_KeyValueList *list ) {


        return ( 0 == OTF_Writer_writeRMAEndKV( ((fcbT*) fcb)->writer, time,
                process, remote, communicator, tag, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


/* *** snapshot handlers ********************************************** */


int handleSnapshotComment( void *fcb, uint64_t time,
	uint32_t process, const char* comment, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeSnapshotCommentKV( ((fcbT*) fcb)->writer, time,
		process, comment, list ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleEnterSnapshot( void *fcb, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeEnterSnapshotKV( ((fcbT*) fcb)->writer,
		time, originaltime, function, process, source, list ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleSendSnapshot( void *fcb, uint64_t time, uint64_t originaltime,
    uint32_t sender, uint32_t receiver,	uint32_t procGroup, uint32_t tag,
    uint32_t length, uint32_t source, OTF_KeyValueList *list ) {
	

	return ( 0 == OTF_Writer_writeSendSnapshotKV( ((fcbT*) fcb)->writer,
		time, originaltime, sender, receiver, procGroup, tag, length, source, list ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleOpenFileSnapshot( void* fcb, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process,
	uint64_t handleid, uint32_t source, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeOpenFileSnapshotKV( ((fcbT*) fcb)->writer, time,
		originaltime, fileid, process, handleid, source, list ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleBeginCollopSnapshot( void *fcb, uint64_t time,
    uint64_t originaltime, uint32_t process, uint32_t collOp,
    uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
    uint64_t sent, uint64_t received, uint32_t scltoken, OTF_KeyValueList *list) {
  
    
    return ( 0 == OTF_Writer_writeBeginCollopSnapshotKV( ((fcbT*) fcb)->writer, time,
        originaltime, process, collOp, matchingId, procGroup, rootProc, sent, received,
        scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
  
}

int handleBeginFileOpSnapshot( void *fcb, uint64_t time,
    uint64_t originaltime, uint32_t process, uint64_t matchingId,
    uint32_t scltoken, OTF_KeyValueList *list) {
  
  
    return ( 0 == OTF_Writer_writeBeginFileOpSnapshotKV( ((fcbT*) fcb)->writer, time,
        originaltime, process, matchingId, scltoken, list ) )
        ? OTF_RETURN_ABORT : OTF_RETURN_OK;
  
}


/* *** Summary handlers *** ****************************************** */


int handleSummaryComment( void* fcb, uint64_t time,
	uint32_t process, const char* comment, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeSummaryCommentKV( ((fcbT*) fcb)->writer, time,
		process, comment, list ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;;
}


int handleFunctionSummary( void* fcb,
		uint64_t time, uint32_t function, uint32_t process, 
		uint64_t count, uint64_t excltime, uint64_t incltime, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeFunctionSummaryKV( ((fcbT*) fcb)->writer, 
		time, function, process, count, excltime, incltime, list ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleFunctionGroupSummary( void* fcb,
		uint64_t time,  uint32_t functiongroup,  uint32_t process,  
		uint64_t count,  uint64_t excltime,  uint64_t incltime, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeFunctionGroupSummaryKV( ((fcbT*) fcb)->writer, 
		time, functiongroup, process, count, excltime, incltime, list ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleMessageSummary( void* fcb,
		uint64_t time, uint32_t process, uint32_t peer, 
		uint32_t comm, uint32_t tag, uint64_t number_sent, 
		uint64_t number_recvd, uint64_t bytes_sent, uint64_t bytes_recved,
		OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeMessageSummaryKV( ((fcbT*) fcb)->writer, 
		time, process, peer, comm, tag, number_sent, number_recvd, bytes_sent,
		bytes_recved, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleCollopSummary( void* fcb,
		uint64_t time, uint32_t process, uint32_t comm,
		uint32_t collective, uint64_t number_sent, uint64_t number_recvd,
		uint64_t bytes_sent, uint64_t bytes_recved, OTF_KeyValueList *list ) {

	return ( 0 == OTF_Writer_writeCollopSummaryKV( ((fcbT*) fcb)->writer,
		time, process, comm, collective, number_sent, number_recvd, bytes_sent,
		bytes_recved, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleFileOperationSummary( void* fcb, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t nopen, uint64_t nclose, uint64_t nread,
	uint64_t nwrite, uint64_t nseek, uint64_t bytesread, uint64_t byteswrite,
	OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeFileOperationSummaryKV( ((fcbT*) fcb)->writer,
		time, fileid, process, nopen, nclose, nread, nwrite, nseek,
		bytesread, byteswrite, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFileGroupOperationSummary( void* fcb, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList *list ) {

	
	return ( 0 == OTF_Writer_writeFileGroupOperationSummaryKV( ((fcbT*) fcb)->writer,
		time, groupid, process, nopen, nclose, nread, nwrite, nseek,
		bytesread, byteswrite, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleDefMarker( void *fcb, uint32_t stream, uint32_t token,
	const char* name, uint32_t type, OTF_KeyValueList *list ) {

	/* even if marker definitions could be read from many streams, they are 
	written to stream 0 forcedly, because this is where all markers belong. */
	stream= 0;

	return ( 0 == OTF_Writer_writeDefMarkerKV( ((fcbT*) fcb)->writer,
		stream, token, name, type, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;;
}


int handleMarker( void *fcb, uint64_t time, uint32_t process,
	uint32_t token, const char* text, OTF_KeyValueList *list ) {


	return ( 0 == OTF_Writer_writeMarkerKV( ((fcbT*) fcb)->writer,
		time, process, token, text, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleUnknown( void* fcb, uint64_t time, uint32_t process, const char* record ) {

#ifdef OTF_VERBOSE

	if ( (uint64_t) -1 != time ) {

		printf( "  unknown record >%s< at process 0x%x time 0x%llu\n",
			record, process, (unsigned long long) time );

	} else {

		printf( "  unknown record >%s< at stream 0x%x\n", 
			record, process );
	}

#endif /* OTF_VERBOSE */

	return OTF_RETURN_ABORT;
}

