/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/


#include <cassert>

#include "Handler.h"
#include "Control.h"


/* *** Definition handler *** ************************************* */


int handleDeftimerresolution( void* firsthandlerarg, uint32_t streamid,
		uint64_t ticksPerSecond ) {


	/* fprintf( stderr, "  %s\n", __PRETTY_FUNCTION__ ); */
	
	return OTF_RETURN_OK;
}


int handleDefprocess( void* firsthandlerarg, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t parent ) {
	
	
	/* fprintf( stderr, "  %s\n", __PRETTY_FUNCTION__ ); */

	Control* control= (Control*) firsthandlerarg;

	control->state->defProcess( deftoken );

	return OTF_RETURN_OK;
}


int handleDefFunction( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t group, uint32_t scltoken ) {
	

	/* fprintf( stderr, "  %s\n", __PRETTY_FUNCTION__ ); */
	
	Control* control= (Control*) firsthandlerarg;
	
	control->state->defFunction( deftoken, group );

	return OTF_RETURN_OK;
}


int handleDefFile( void* firsthandlerarg, uint32_t stream, uint32_t token,
	const char* name, uint32_t group ) {


	/* fprintf( stderr, "  %s\n", __PRETTY_FUNCTION__ ); */
	
	Control* control= (Control*) firsthandlerarg;
	
	control->state->defFile( token, group );

	return OTF_RETURN_OK;
}


/* *** Event handler *** ****************************************** */


int handleCounter( void* firsthandlerarg, uint64_t time, uint32_t process,
		uint32_t token, uint64_t value ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */

	return OTF_RETURN_OK;
}


int handleEnter( void* firsthandlerarg, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */

	Control* control= (Control*) firsthandlerarg;
	
	while ( time > control->checkTime( time ) )
		;

	control->state->enterFunction( time, cpuid, statetoken );

	return OTF_RETURN_OK;
}


int handleLeave( void* firsthandlerarg, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */
	Control* control= (Control*) firsthandlerarg;
	
	while ( time > control->checkTime( time ) )
		;

	control->state->leaveFunction( time, cpuid, statetoken );

	return OTF_RETURN_OK;
}


int handleRecvmsg( void* firsthandlerarg, uint64_t time, uint32_t receiver,
		uint32_t sender, uint32_t procGroup, uint32_t msgtag, 
		uint32_t msglength, uint32_t scltoken ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */
	
	Control* control= (Control*) firsthandlerarg;
	
	while ( time > control->checkTime( time ) )
		;

	control->state->recvMessage( sender, receiver, procGroup, msgtag,
		msglength );
	
	return OTF_RETURN_OK;
}


int handleSendmsg( void* firsthandlerarg, uint64_t time, uint32_t sender,
		uint32_t receiver, uint32_t procGroup, uint32_t msgtag, 
		uint32_t msglength, uint32_t scltoken ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */
	
	Control* control= (Control*) firsthandlerarg;
	
	while ( time > control->checkTime( time ) )
		;

	control->state->sendMessage( time, sender, receiver, procGroup, msgtag,
		msglength, scltoken );
	
	return OTF_RETURN_OK;
}


int handleBeginProcess( void* firsthandlerarg, uint64_t time, uint32_t process ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */

	Control* control= (Control*) firsthandlerarg;
	
	while ( time > control->checkTime( time ) )
		;

	return OTF_RETURN_OK;
}


int handleEndProcess( void* firsthandlerarg, uint64_t time, uint32_t process ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */

	Control* control= (Control*) firsthandlerarg;
	
	while ( time > control->checkTime( time ) )
		;

	return OTF_RETURN_OK;
}


int handleEventComment( void* firsthandlerarg, uint64_t time, 
	const char* comment ) {
	
	
	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */

	Control* control= (Control*) firsthandlerarg;
	
	while ( time > control->checkTime( time ) )
		;

	return OTF_RETURN_OK;
}


int handleCollectiveOperation( void* firsthandlerarg, uint64_t time, 
    uint32_t process, uint32_t collective, uint32_t communicator, 
    uint32_t rootprocess, uint32_t sent, uint32_t received, 
    uint64_t duration, uint32_t scltoken ) {
	
	
	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */

	Control* control= (Control*) firsthandlerarg;
	
	while ( time > control->checkTime( time ) )
		;

	return OTF_RETURN_OK;
}


int handleFileOperation( void* firsthandlerarg, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */
	
	Control* control= (Control*) firsthandlerarg;

	while ( time > control->checkTime( time ) )
		;

	return control->state->fileOperation( time, fileid, process, handleid, operation,
		bytes, duration, source );
}
