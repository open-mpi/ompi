/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "config.h"


#include "OTF_Platform.h"
#include "OTF_CopyHandler.h"
#include "OTF_Writer.h"


/* *** Definition handlers *** ************************************* */

int OTF_CopyHandler_DefinitionComment( void* userData, uint32_t stream,
		const char* comment ) {


	return ( 0 == OTF_Writer_writeDefinitionComment( (OTF_Writer*)userData, stream,
		comment ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefTimerResolution( void* userData,
		uint32_t stream, uint64_t ticksPerSecond ) {


	return ( 0 == OTF_Writer_writeDefTimerResolution( (OTF_Writer*)userData, stream,
		ticksPerSecond ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefProcess( void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, uint32_t parent ) {


	return ( 0 == OTF_Writer_writeDefProcess( (OTF_Writer*)userData, stream,
		deftoken, name, parent ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefProcessGroup( void* userData, uint32_t stream, 
		uint32_t deftoken, const char* name, uint32_t n, const uint32_t* procs ) {


	return ( 0 == OTF_Writer_writeDefProcessGroup( (OTF_Writer*)userData, stream, deftoken,
		name, n, procs ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefFunction(  void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, 
		uint32_t group, uint32_t scltoken ) {


	return ( 0 == OTF_Writer_writeDefFunction( (OTF_Writer*)userData, stream, deftoken,
		name, group, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefFunctionGroup( void* userData, uint32_t stream,
		uint32_t deftoken, const char* name ) {


	return ( 0 == OTF_Writer_writeDefFunctionGroup( (OTF_Writer*)userData, stream,
		deftoken, name ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefCollectiveOperation( void* userData, uint32_t stream,
	uint32_t collOp, const char* name, uint32_t type ) {
	
	
	return ( 0 == OTF_Writer_writeDefCollectiveOperation( (OTF_Writer*)userData, stream,
		collOp, name, type ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefCounter( void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, uint32_t properties, 
		uint32_t countergroup, const char* unit ) {


	return ( 0 == OTF_Writer_writeDefCounter( (OTF_Writer*)userData, stream,
		deftoken, name, properties, countergroup, unit ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefCounterGroup( void* userData, uint32_t stream,
		uint32_t deftoken, const char* name ) {


	return ( 0 == OTF_Writer_writeDefCounterGroup( (OTF_Writer*)userData, stream,
			deftoken, name ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefScl(  void* userData, uint32_t stream, 
		uint32_t deftoken, uint32_t sclfile, uint32_t sclline ) {


	return ( 0 == OTF_Writer_writeDefScl( (OTF_Writer*)userData, stream, deftoken,
		sclfile, sclline ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefSclFile(  void* userData, uint32_t stream,
		uint32_t deftoken, const char* filename ) {


	return ( 0 == OTF_Writer_writeDefSclFile( (OTF_Writer*)userData, stream,
		deftoken, filename ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefVersion( void* userData, uint32_t stream, 
		uint8_t major, uint8_t minor, uint8_t sub, const char* string ) {


	/* version is writen implicitly */

	return OTF_RETURN_OK; /* success */
}


int OTF_CopyHandler_DefCreator( void* userData, uint32_t stream,
		const char* creator ) {


	return ( 0 == OTF_Writer_writeDefCreator( (OTF_Writer*)userData, stream, creator )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefFile( void* userData, uint32_t stream, uint32_t token,
	const char* name, uint32_t group ) {


	return ( 0 == OTF_Writer_writeDefFile( (OTF_Writer*)userData, stream, token,
		name, group ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefFileGroup( void* userData, uint32_t stream,
	uint32_t token, const char* name ) {


	return ( 0 == OTF_Writer_writeDefFileGroup( (OTF_Writer*)userData, stream, token,
		name ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


/* *** Event handlers *** ****************************************** */


int OTF_CopyHandler_EventComment( void* userData, uint64_t time, uint32_t process,
		const char* comment ) {


	return ( 0 == OTF_Writer_writeEventComment( (OTF_Writer*)userData, time,
		process, comment ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_Counter( void* userData, uint64_t time, 
		uint32_t process, uint32_t counter_token, uint64_t value ) {


	return ( 0 == OTF_Writer_writeCounter( (OTF_Writer*)userData, time,
		process, counter_token, value ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_Enter( void* userData, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {


	return ( 0 == OTF_Writer_writeEnter( (OTF_Writer*)userData, time,
		statetoken,	cpuid, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_CollectiveOperation( void* userData, uint64_t time, 
    	uint32_t process, uint32_t functionToken, uint32_t communicator, 
    	uint32_t rootprocess, uint32_t sent, uint32_t received, 
    	uint64_t duration, uint32_t scltoken ) {


    return ( 0 == OTF_Writer_writeCollectiveOperation( (OTF_Writer*)userData, time,
    	process, functionToken, communicator, rootprocess, 
		sent, received, duration, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_RecvMsg( void* userData, uint64_t time, 
		uint32_t receiver, uint32_t sender, uint32_t communicator, 
		uint32_t msgtype, uint32_t msglength, uint32_t scltoken ) {


	return ( 0 == OTF_Writer_writeRecvMsg( (OTF_Writer*)userData, time, receiver,
		sender, communicator, msgtype, msglength, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_SendMsg( void* userData, uint64_t time, 
		uint32_t sender, uint32_t receiver, uint32_t communicator, 
		uint32_t msgtype, uint32_t msglength, uint32_t scltoken ) {


	return ( 0 == OTF_Writer_writeSendMsg( (OTF_Writer*)userData, time, sender,
		receiver, communicator, msgtype, msglength, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_Leave( void* userData, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {


	return ( 0 == OTF_Writer_writeLeave( (OTF_Writer*)userData, time, statetoken,
		cpuid, scltoken ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_BeginProcess( void* userData, uint64_t time,
		uint32_t process ) {


	return ( 0 == OTF_Writer_writeBeginProcess( (OTF_Writer*)userData, time,
		process ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_EndProcess( void* userData, uint64_t time,
		uint32_t process ) {


	return ( 0 == OTF_Writer_writeEndProcess( (OTF_Writer*)userData, time,
		process ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_FileOperation( void* userData, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source ) {


	return ( 0 == OTF_Writer_writeFileOperation( (OTF_Writer*)userData, time, fileid,
		process, handleid, operation, bytes, duration, source )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


/* *** snapshot handlers ********************************************** */


int OTF_CopyHandler_SnapshotComment( void* userData, uint64_t time,
	uint32_t process, const char* comment ) {


	return ( 0 == OTF_Writer_writeSnapshotComment( (OTF_Writer*)userData,
		time, process, comment ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_EnterSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source ) {


	return ( 0 == OTF_Writer_writeEnterSnapshot( (OTF_Writer*)userData,
		time, originaltime, function, process, source )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_SendSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t source ) {
	

	return ( 0 == OTF_Writer_writeSendSnapshot( (OTF_Writer*)userData,
		time, originaltime, sender, receiver, procGroup, tag, source )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_OpenFileSnapshot( void* userData, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source ) {


	return ( 0 == OTF_Writer_writeOpenFileSnapshot( (OTF_Writer*)userData, time,
		originaltime, fileid, process, handleid, source )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}
/* *** summary handlers ********************************************** */
int OTF_CopyHandler_SummaryComment( void * userData, uint64_t time,
	uint32_t process, const char* comment ) {


	return ( 0 == OTF_Writer_writeSummaryComment( (OTF_Writer*)userData,
		time, process, comment ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_FunctionSummary( void* userData, 
		uint64_t time, uint32_t function, uint32_t process, 
		uint64_t count, uint64_t excltime, uint64_t incltime ) {


	return ( 0 == OTF_Writer_writeFunctionSummary( (OTF_Writer*)userData,
		time, function, process, count, excltime, incltime )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_FunctionGroupSummary( void* userData, 
		uint64_t time,  uint32_t functiongroup,  uint32_t process,  
		uint64_t count,  uint64_t excltime,  uint64_t incltime ) {


	return ( 0 == OTF_Writer_writeFunctionGroupSummary( (OTF_Writer*)userData,
		time, functiongroup, process, count, excltime, incltime )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_MessageSummary( void* userData, uint64_t time,
	uint32_t process, uint32_t peer, uint32_t comm, uint32_t type,
	uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes,
	uint64_t receivedBytes ) {


	return ( 0 == OTF_Writer_writeMessageSummary((OTF_Writer*) userData,
		time, process, peer, comm, type, sentNumber, receivedNumber, sentBytes,
		receivedBytes ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_FileOperationSummary( void* userData, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite ) {


	return ( 0 == OTF_Writer_writeFileOperationSummary( (OTF_Writer*) userData,
		time, fileid, process, nopen, nclose, nread, nwrite, nseek,
		bytesread, byteswrite ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_FileGroupOperationSummary( void* userData, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite ) {


	return ( 0 == OTF_Writer_writeFileOperationSummary( (OTF_Writer*) userData,
		time, groupid, process, nopen, nclose, nread, nwrite, nseek,
		bytesread, byteswrite ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}
