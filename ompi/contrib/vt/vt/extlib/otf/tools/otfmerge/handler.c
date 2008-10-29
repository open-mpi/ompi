/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include <assert.h>

#include "OTF_Platform.h"
#include "OTF_Writer.h"

#include "handler.h"
#include "hash.h"

/* *** Definition handler *** ************************************* */

int handleDefinitionComment( void* fcbx, uint32_t streamid,
	const char* comment ) {
	

	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );

		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefinitionComment( ((fcbT*) fcbx)->writer, streamid,
			comment ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}
	
int handleDefTimerResolution( void* fcbx,
	uint32_t streamid, uint64_t ticksPerSecond ) {
	
	
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
	
		return ( 0 == OTF_Writer_writeDefTimerResolution( fcb->writer, streamid,
			ticksPerSecond ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}


int handleDefProcess( void* fcbx, uint32_t streamid
		, uint32_t deftoken, const char* name, uint32_t parent ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefProcess( ((fcbT*) fcbx)->writer, streamid,
			deftoken, name, parent ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefProcessGroup( void* fcbx, uint32_t streamid
		, uint32_t deftoken, const char* name, uint32_t n, uint32_t* array ) {



	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefProcessGroup( ((fcbT*) fcbx)->writer, streamid, deftoken,
			name, n, array ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefFunction(  void* fcbx, uint32_t streamid,
		uint32_t deftoken, const char* name, 
		uint32_t group, uint32_t scltoken ) {


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
	
		
		return ( 0 == OTF_Writer_writeDefFunction( fcb->writer, streamid, deftoken,
			name, group, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefFunctionGroup( void* fcbx, uint32_t streamid,
		uint32_t deftoken, const char* name ) {


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
		
		return ( 0 == OTF_Writer_writeDefFunctionGroup( fcb->writer, streamid,
			deftoken, name ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefCollectiveOperation( void* fcbx, uint32_t streamid,
	uint32_t collOp, const char* name, uint32_t type ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefCollectiveOperation( ((fcbT*) fcbx)->writer, streamid,
			collOp, name, type ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
	}
}


int handleDefCounter( void* fcbx, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t properties, 
		uint32_t countergroup, const char* unit ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefCounter( ((fcbT*) fcbx)->writer, streamid,
			deftoken, name, properties, countergroup, unit ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefCounterGroup( void* fcbx, uint32_t streamid,
		uint32_t deftoken, const char* name ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefCounterGroup( ((fcbT*) fcbx)->writer, streamid, 
			deftoken, name ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefScl(  void* fcbx, uint32_t streamid,
		uint32_t deftoken, uint32_t sclfile, uint32_t sclline ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefScl( ((fcbT*) fcbx)->writer, streamid, deftoken,
			sclfile, sclline ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefSclFile(  void* fcbx, uint32_t streamid,
		uint32_t deftoken, const char* filename ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefSclFile( ((fcbT*) fcbx)->writer, streamid, 
			deftoken, filename ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	}
}


int handleDefCreator( void* fcbx, uint32_t streamid,
		const char* creator ) {


	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefCreator( ((fcbT*) fcbx)->writer, streamid,
			creator ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}


int handleDefFile( void* fcbx, uint32_t streamid, uint32_t token,
	const char* name, uint32_t group ) {

	
	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefFile( ((fcbT*) fcbx)->writer,
		streamid, token, name, group ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}
	
	
int handleDefFileGroup( void* fcbx, uint32_t streamid,
	uint32_t token, const char* name ) {

	
	if( 0 != streamid ) {

		fprintf( stderr, "ERROR: cannot merge traces with local definitions. aborting\n" );

		assert( 0 );
		
		return OTF_RETURN_ABORT;

	} else {

		return ( 0 == OTF_Writer_writeDefFileGroup( ((fcbT*) fcbx)->writer,
		streamid, token, name ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
			
	}
}

/* *** Event handler *** ****************************************** */


int handleEventComment( void* fcb, uint64_t time, uint32_t process,
		const char* comment ) {


	return ( 0 == OTF_Writer_writeEventComment( ((fcbT*) fcb)->writer, time, process,
		comment ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleCounter( void* fcb, uint64_t time,
		uint32_t process, uint32_t counter_token, uint64_t value ) {


	return ( 0 == OTF_Writer_writeCounter( ((fcbT*) fcb)->writer, time, 
		process, counter_token, value ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleEnter( void* fcb, uint64_t time,
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {


	return ( 0 == OTF_Writer_writeEnter( ((fcbT*) fcb)->writer, time, 
		statetoken,	cpuid, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleCollectiveOperation( void* fcb, uint64_t time,
    	uint32_t process, uint32_t functionToken, uint32_t communicator, 
    	uint32_t rootprocess, uint32_t sent, uint32_t received, 
    	uint64_t duration, uint32_t scltoken ) {


    return ( 0 == OTF_Writer_writeCollectiveOperation( ((fcbT*) fcb)->writer, time, 
    	process, functionToken, communicator, rootprocess, 
		sent, received, duration, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleRecvMsg( void* fcb, uint64_t time,
		uint32_t receiver, uint32_t sender, uint32_t communicator, 
		uint32_t msgtype, uint32_t msglength, uint32_t scltoken ) {


	return ( 0 == OTF_Writer_writeRecvMsg( ((fcbT*) fcb)->writer, time, receiver,
		sender, communicator, msgtype, msglength, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleSendMsg( void* fcb, uint64_t time,
		uint32_t sender, uint32_t receiver, uint32_t communicator, 
		uint32_t msgtype, uint32_t msglength, uint32_t scltoken ) {


	return ( 0 == OTF_Writer_writeSendMsg( ((fcbT*) fcb)->writer, time, sender,
		receiver, communicator, msgtype, msglength, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleLeave( void* fcb, uint64_t time,
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {


	return ( 0 == OTF_Writer_writeLeave( ((fcbT*) fcb)->writer, time, statetoken,
		cpuid, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleBeginProcess( void* fcb, uint64_t time,
		uint32_t cpuid ) {


	return ( 0 == OTF_Writer_writeBeginProcess( ((fcbT*) fcb)->writer, time, cpuid ) );
}
int handleEndProcess( void* fcb, uint64_t time,
		uint32_t cpuid ) {


	return ( 0 == OTF_Writer_writeEndProcess( ((fcbT*) fcb)->writer, time, cpuid ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleFileOperation( void* fcb, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source ) {


	return ( 0 == OTF_Writer_writeFileOperation( ((fcbT*) fcb)->writer, time, fileid,
		process, handleid, operation, bytes, duration, source ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


/* *** snapshot handlers ********************************************** */


int handleSnapshotComment( void *fcb, uint64_t time,
	uint32_t process, const char* comment ) {


	return ( 0 == OTF_Writer_writeSnapshotComment( ((fcbT*) fcb)->writer, time,
		process, comment ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleEnterSnapshot( void *fcb, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source ) {


	return ( 0 == OTF_Writer_writeEnterSnapshot( ((fcbT*) fcb)->writer,
		time, originaltime, function, process, source ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleSendSnapshot( void *fcb, uint64_t time,
	uint64_t originaltime, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t source ) {
	

	return ( 0 == OTF_Writer_writeSendSnapshot( ((fcbT*) fcb)->writer,
		time, originaltime, sender, receiver, procGroup, tag, source ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleOpenFileSnapshot( void* fcb, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process,
	uint64_t handleid, uint32_t source ) {


	return ( 0 == OTF_Writer_writeOpenFileSnapshot( ((fcbT*) fcb)->writer, time,
		originaltime, fileid, process, handleid, source ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


/* *** Summary handlers *** ****************************************** */


int handleSummaryComment( void* fcb, uint64_t time,
	uint32_t process, const char* comment ) {


	return ( 0 == OTF_Writer_writeSummaryComment( ((fcbT*) fcb)->writer, time,
		process, comment ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;;
}


int handleFunctionSummary( void* fcb,
		uint64_t time, uint32_t function, uint32_t process, 
		uint64_t count, uint64_t excltime, uint64_t incltime ) {


	return ( 0 == OTF_Writer_writeFunctionSummary( ((fcbT*) fcb)->writer, 
		time, function, process, count, excltime, incltime ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleFunctionGroupSummary( void* fcb,
		uint64_t time,  uint32_t functiongroup,  uint32_t process,  
		uint64_t count,  uint64_t excltime,  uint64_t incltime ) {


	return ( 0 == OTF_Writer_writeFunctionGroupSummary( ((fcbT*) fcb)->writer, 
		time, functiongroup, process, count, excltime, incltime ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleMessageSummary( void* fcb,
		uint64_t time, uint32_t process, uint32_t peer, 
		uint32_t comm, uint32_t tag, uint64_t number_sent, 
		uint64_t number_recvd, uint64_t bytes_sent, uint64_t bytes_recved ) {


	return ( 0 == OTF_Writer_writeMessageSummary( ((fcbT*) fcb)->writer, 
		time, process, peer, comm, tag, number_sent, number_recvd, bytes_sent,
		bytes_recved ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleFileOperationSummary( void* fcb, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t nopen, uint64_t nclose, uint64_t nread,
	uint64_t nwrite, uint64_t nseek, uint64_t bytesread, uint64_t byteswrite ) {


	return ( 0 == OTF_Writer_writeFileOperationSummary( ((fcbT*) fcb)->writer,
		time, fileid, process, nopen, nclose, nread, nwrite, nseek,
		bytesread, byteswrite ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFileGroupOperationSummary( void* fcb, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite ) {

	
	return ( 0 == OTF_Writer_writeFileOperationSummary( ((fcbT*) fcb)->writer,
		time, groupid, process, nopen, nclose, nread, nwrite, nseek,
		bytesread, byteswrite ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
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

