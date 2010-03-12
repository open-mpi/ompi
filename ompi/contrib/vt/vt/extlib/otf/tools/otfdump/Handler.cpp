/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include <cassert>

#include "Handler.h"

int handleDefinitionComment( void* userData, uint32_t stream,
	const char* comment ) {


	Control* c= (Control*) userData;

	if( c->records[OTF_DEFINITIONCOMMENT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefComment: stream %u, comment \"%s\"\n",
				(long long unsigned) c->num, stream, comment );
		}
	}

	return OTF_RETURN_OK;
}


int handleDefTimerResolution( void* userData, uint32_t stream,
	uint64_t ticksPerSecond ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFTIMERRESOLUTION_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefTimerResolution: stream %u, TicksPerSecond %llu\n",
				(long long unsigned) c->num, stream,
				(long long unsigned) ticksPerSecond );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefProcess( void* userData, uint32_t stream, uint32_t process,
	const char* name, uint32_t parent ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFPROCESS_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefProcess: stream %u, process %u, name \"%s\", parent %u\n",
				(long long unsigned) c->num, stream, process, name, parent );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefProcessGroup( void* userData, uint32_t stream,
	uint32_t group, const char* name, uint32_t numberOfProcs,
	const uint32_t* procs ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFPROCESSGROUP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefProcessGroup: stream %u, group %u, name \"%s\", procs ",
				(long long unsigned) c->num, stream, group, name );

			for( uint32_t i= 0; i < numberOfProcs; ++i )
				fprintf( c->outfile, "%u, ", procs[i] );

			fprintf( c->outfile, "\n" );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefFunction( void* userData, uint32_t stream, uint32_t func,
	const char* name, uint32_t funcGroup, uint32_t source ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFFUNCTION_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefFunction: stream %u, function %u, name \"%s\", group %u, source %u\n",
				(long long unsigned) c->num, stream, func, name, funcGroup, source );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefFunctionGroup( void* userData, uint32_t stream,
	uint32_t funcGroup, const char* name ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFFUNCTIONGROUP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefFunctionGroup: stream %u, group %u, name \"%s\"\n",
				(long long unsigned) c->num, stream, funcGroup, name );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefCollectiveOperation( void* userData, uint32_t stream,
	uint32_t collOp, const char* name, uint32_t type ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFCOLLOP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefCollective: stream %u, collective %u, name \"%s\", type %u\n",
				(long long unsigned) c->num, stream,  collOp, name, type );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefCounter( void* userData, uint32_t stream, uint32_t counter,
	const char* name, uint32_t properties, uint32_t counterGroup,
	const char* unit ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFCOUNTER_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefCounter: stream %u, counter %u, name \"%s\", properties %u, group %u, unit \"%s\"\n",
				(long long unsigned) c->num, stream, counter, name, properties, counterGroup, unit );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefCounterGroup( void* userData, uint32_t stream,
	uint32_t counterGroup, const char* name ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFCOUNTERGROUP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefCounterGroup: stream %u, group %u, name \"%s\"\n",
				(long long unsigned) c->num, stream, counterGroup, name );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefScl( void* userData, uint32_t stream, uint32_t source,
	uint32_t sourceFile, uint32_t line ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFSCL_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefScl: stream %u, source %u, file %u, line %u\n",
				(long long unsigned) c->num, stream, source, sourceFile, line );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefSclFile( void* userData, uint32_t stream,
	uint32_t sourceFile, const char* name ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFSCLFILE_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefSclFile: stream %u, file %u, name \"%s\"\n",
				(long long unsigned) c->num, stream, sourceFile, name );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefCreator( void* userData, uint32_t stream,
	const char* creator ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFCREATOR_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefCreator: stream %u, creator \"%s\"\n",
				(long long unsigned) c->num, stream, creator );
		}
	}


	return OTF_RETURN_OK;
}

	
int handleDefVersion( void* userData, uint32_t stream, uint8_t major,
	uint8_t minor, uint8_t sub, const char* string ) {
	Control* c= (Control*) userData;


	if( c->records[OTF_DEFVERSION_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \tDefVersion: stream %u, version: %u.%u.%u \"%s\"\n",
				(long long unsigned) c->num, stream, major, minor, sub, string );
		}
	}


	return OTF_RETURN_OK;
}


int handleEnter( void* userData, uint64_t time, uint32_t function,
	uint32_t process, uint32_t source ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_ENTER_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu Enter: function %u, process %u, source %u\n",
				(long long unsigned) c->num, (long long unsigned) time,
				function, process, source );
		}
	}


	return OTF_RETURN_OK;
}


int handleLeave( void* userData, uint64_t time, uint32_t function,
	uint32_t process, uint32_t source ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_LEAVE_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu Leave: function %u, process %u, source %u\n",
				(long long unsigned) c->num, (long long unsigned) time,
				 function, process, source);
		}
	}


	return OTF_RETURN_OK;
}


int handleSendMsg( void* userData, uint64_t time, uint32_t sender,
	uint32_t receiver, uint32_t group, uint32_t type, uint32_t length,
	uint32_t source ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_SEND_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu SendMessage: sender %u, receiver %u, group %u, type %u, length %u, source %u\n",
				(long long unsigned) c->num, (long long unsigned) time,
				sender, receiver, group, type, length, source );
		}
	}


	return OTF_RETURN_OK;
}


int handleRecvMsg( void* userData, uint64_t time, uint32_t recvProc,
	uint32_t sendProc, uint32_t group, uint32_t type, uint32_t length, 
	uint32_t source ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_RECEIVE_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu ReceiveMessage: receiver %u, sender %u, group %u, type %u, length %u, source %u\n",
				(long long unsigned) c->num, (long long unsigned) time,
				recvProc, sendProc, group, type, length, source );
		}
	}


	return OTF_RETURN_OK;
}


int handleCounter( void* userData, uint64_t time, uint32_t process,
	uint32_t counter, uint64_t value ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_COUNTER_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu Counter: process %u, counter %u, value %llu\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, counter, (long long unsigned) value );
		}
	}


	return OTF_RETURN_OK;
}


int handleCollectiveOperation( void* userData, uint64_t time,
	uint32_t process, uint32_t collective, uint32_t procGroup,
	uint32_t rootProc, uint32_t sent, uint32_t received, uint64_t duration, 
	uint32_t source ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_COLLOP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu Collective: process %u, collective %u, group %u, root %u, sent %u, received %u, duration %llu, source %u\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, collective, procGroup, rootProc, sent, received,
				(long long unsigned) duration, source );
		}
	}


	return OTF_RETURN_OK;
}


int handleBeginCollectiveOperation( void* userData, uint64_t time,
		uint32_t process, uint32_t collOp, uint64_t matchingId,
		uint32_t procGroup, uint32_t rootProc, uint32_t sent,
		uint32_t received, uint32_t scltoken )
{
	Control* c = (Control*) userData;

	if( c->records[OTF_BEGINCOLLOP_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu BeginCollective: "
					"process %u, collective %u, "
					"group %u, matchinId %llu, "
					"root %u, sent %u, "
					"received %u, source %u\n",
					(long long unsigned) c->num,
					(long long unsigned) time, process,
					collOp, procGroup,
					(long long unsigned) matchingId,
					rootProc, sent, received, scltoken );
		}
	}

	return OTF_RETURN_OK;
}


int handleEndCollectiveOperation( void* userData, uint64_t time,
		uint32_t process, uint64_t matchingId )
{
	Control* c = (Control*) userData;

	if( c->records[OTF_ENDCOLLOP_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu EndCollective: "
					"process %u, matchingId %llu\n",
					(long long unsigned) c->num,
					(long long unsigned) time, process,
					(long long unsigned) matchingId );
		}
	}

	return OTF_RETURN_OK;
}


int handleEventComment( void* userData, uint64_t time, uint32_t process,
	const char* comment ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_EVENTCOMMENT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu EventComment: process %u, comment \"%s\"\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, comment );
		}
	}


	return OTF_RETURN_OK;
}


int handleBeginProcess( void* userData, uint64_t time, uint32_t process ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_BEGINPROCESS_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu BeginProcess: process %u\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process );
		}
	}


	return OTF_RETURN_OK;
}


int handleEndProcess( void* userData, uint64_t time, uint32_t process ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_ENDPROCESS_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu EndProcess: process %u\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process );
		}
	}


	return OTF_RETURN_OK;
}


int handleSnapshotComment( void* userData, uint64_t time,
	uint32_t process, const char* comment ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_SNAPSHOTCOMMENT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu SnapComment: process %u, comment \"%s\"\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, comment );
		}
	}


	return OTF_RETURN_OK;
}


int handleEnterSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_ENTERSNAPSHOT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu SnapEnter: otime %llu, process %u, function %u, source %u\n",
				(long long unsigned) c->num, (long long unsigned) time,
				(long long unsigned) originaltime, process, function, source );
		}
	}


	return OTF_RETURN_OK;
}


int handleSendSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t source ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_SENDSNAPSHOT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu SnapSend: otime %llu, sender %u, receiver %u, group %u, tag %u, source %u\n",
				(long long unsigned) c->num, (long long unsigned) time,
				(long long unsigned) originaltime, sender, receiver, procGroup,
				tag, source );
		}
	}


	return OTF_RETURN_OK;
}


int handleSummaryComment( void * userData, uint64_t time,
	uint32_t process, const char* comment ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_SUMMARYCOMMENT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu StatComment: process %u, comment \"%s\"\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, comment );
		}
	}


	return OTF_RETURN_OK;
}


int handleFunctionSummary( void* userData, uint64_t time,
	uint32_t function, uint32_t process, uint64_t invocations,
	uint64_t exclTime, uint64_t inclTime ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_FUNCTIONSUMMARY_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu StatFunction: process %u, function %u, invocations %llu, excltime %llu, incltime %llu\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, function, (long long unsigned) invocations,
				(long long unsigned) exclTime, (long long unsigned) inclTime );
		}
	}


	return OTF_RETURN_OK;
}


int handleFunctionGroupSummary( void* userData, uint64_t time,
	uint32_t funcGroup, uint32_t process, uint64_t invocations,
	uint64_t exclTime, uint64_t inclTime ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_FUNCTIONGROUPSUMMARY_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu StatFunctionGroup: process %u, group %u, invocations %llu, excltime %llu, incltime %llu\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, funcGroup, (long long unsigned) invocations,
				(long long unsigned) exclTime, (long long unsigned) inclTime );
		}
	}


	return OTF_RETURN_OK;
}


int handleMessageSummary( void* userData, uint64_t time, uint32_t process,
	uint32_t peer, uint32_t comm, uint32_t type, uint64_t sentNumber,
	uint64_t receivedNumber, uint64_t sentBytes, uint64_t receivedBytes ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_MESSAGESUMMARY_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu StatMessage: process %u, "
				"peer %u, group %u, type %u, numsent %llu, numreceived %llu, "
				"bytessent %llu, bytesreceived %llu\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, peer, comm, type, (long long unsigned) sentNumber,
				(long long unsigned) receivedNumber, (long long unsigned) sentBytes,
				(long long unsigned) receivedBytes );
		}
	}


	return OTF_RETURN_OK;
}


int handleCollopSummary( void* userData, uint64_t time, uint32_t process,
	uint32_t comm, uint32_t collective, uint64_t sentNumber, uint64_t receivedNumber,
	uint64_t sentBytes, uint64_t receivedBytes) {


	Control* c= (Control*) userData;


	if( c->records[OTF_COLLOPSUMMARY_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu StatCollOp: process %u, group %u, "
				"col %u, numsent %llu, numreceived %llu, "
				"bytessent %llu, bytesreceived %llu\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, comm, collective, (long long unsigned) sentNumber,
				(long long unsigned) receivedNumber, (long long unsigned) sentBytes,
				(long long unsigned) receivedBytes );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefFile( void* userData, uint32_t stream,
           uint32_t token, const char *name,
           uint32_t group )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_DEFFILE_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum ) {

                        fprintf( c->outfile, "(#%llu) \tDefFile: %s, stream %llu, "
                                             "token %llu, group %llu\n",
                                (long long unsigned) c->num, name, (long long unsigned) stream,
                                (long long unsigned) token, (long long unsigned) group );
                }
        }
        return OTF_RETURN_OK;
}


int handleDefFileGroup( void* userData, uint32_t stream,
                uint32_t token, const char *name )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_DEFFILEGROUP_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum ) {

                        fprintf( c->outfile, "(#%llu) \tDefFileGroup: %s, token %llu, "
                                             "stream %llu\n",
                                (long long unsigned) c->num, name,
                                (long long unsigned) token, (long long unsigned) stream);
                }
        }
        return OTF_RETURN_OK;
}


int handleFileOperation( void* userData, uint64_t time,
                 uint32_t fileid, uint32_t process,
                 uint64_t handleid, uint32_t operation,
                 uint64_t bytes, uint64_t duration,
                 uint32_t source )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_FILEOPERATION_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum ) {
        
                        fprintf( c->outfile, "(#%llu) \t%llu FileOperation: file ID %llu, "
                                             "process %llu, handle ID %llu, operation %llu, "
                                             "bytes %llu, duration %llu, source %llu\n",
                                (long long unsigned) c->num, (long long unsigned) time,
                                (long long unsigned) fileid, (long long unsigned) process,
                                (long long unsigned) handleid, (long long unsigned) operation,
                                (long long unsigned) bytes, (long long unsigned) duration,
                                (long long unsigned) source);
                }
        }
        return OTF_RETURN_OK;
}


int handleBeginFileOperation( void* userData, uint64_t time,
		uint32_t process, uint64_t handleid, uint32_t scltoken )
{
        Control* c = (Control*) userData;

	if( c->records[OTF_BEGINFILEOP_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu BeginFileOperation: "
					"process %llu, handle ID %llu, "
					"source %llu\n",
					(long long unsigned) c->num,
					(long long unsigned) time,
					(long long unsigned) process,
					(long long unsigned) handleid,
					(long long unsigned) scltoken );
		}
	}
	return OTF_RETURN_OK;
}


int handleEndFileOperation( void* userData, uint64_t time,
		uint32_t process, uint32_t fileid, uint64_t handleid,
		uint32_t operation, uint64_t bytes, uint32_t scltoken )
{
	Control* c = (Control*) userData;

	if( c->records[OTF_ENDFILEOP_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu EndFileOperation: "
					"process %llu, file ID %llu, "
					"handle ID %llu, operation %llu, "
					"bytes %llu, source %llu\n",
					(long long unsigned) c->num,
					(long long unsigned) time,
					(long long unsigned) process,
					(long long unsigned) fileid,
					(long long unsigned) handleid,
					(long long unsigned) operation,
					(long long unsigned) bytes,
					(long long unsigned) scltoken );
		}
	}
	return OTF_RETURN_OK;
}


int handleRMAPut( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t source )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_RMAPUT_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum ) {
  
                    if( origin != 0 ) {
                        /* transfer initiated from 3rd-party process */
                        fprintf( c->outfile, "(#%llu) \t%llu RMAPut: initiator %u, "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu\n",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, origin, target,
                                 communicator, tag, (long long unsigned) bytes,
                                 (long long unsigned) source);
                    }
                    else {
                        fprintf( c->outfile, "(#%llu) \t%llu RMAPut: "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu\n",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, target,
                                 communicator, tag, (long long unsigned) bytes,
                                 (long long unsigned) source);
                    }
                }
        }
        return OTF_RETURN_OK;
}


int handleRMAPutRemoteEnd( void* userData, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes, uint32_t source )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_RMAPUTRE_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum ) {

                    if( origin != 0 ) {
                        /* transfer initiated from 3rd-party process */
                        fprintf( c->outfile, "(#%llu) \t%llu RMAPutRemoteEnd: initiator %u, "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu\n",
                             (long long unsigned) c->num, (long long unsigned) time,
                             process, origin, target,
                             communicator, tag, (long long unsigned) bytes,
                             (long long unsigned) source);
                    }
                    else {
                        fprintf( c->outfile, "(#%llu) \t%llu RMAPutRemoteEnd: "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu\n",
                             (long long unsigned) c->num, (long long unsigned) time,
                             process, target,
                             communicator, tag, (long long unsigned) bytes,
                             (long long unsigned) source);
                    }
                }
        }
        return OTF_RETURN_OK;
}


int handleRMAGet( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t source)
{
        Control* c= (Control*) userData;

        if( c->records[OTF_RMAGET_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum ) {
  
                    if( origin != 0 ) {
                        /* transfer initiated from 3rd-party process */
                        fprintf( c->outfile, "(#%llu) \t%llu RMAGet: initiator %u, "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu\n",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, origin, target,
                                 communicator, tag, (long long unsigned) bytes,
                                 (long long unsigned) source);
                    }
                    else {
                        fprintf( c->outfile, "(#%llu) \t%llu RMAGet: "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu\n",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, target,
                                 communicator, tag, (long long unsigned) bytes,
                                 (long long unsigned) source);
                    }
                }
        }
        return OTF_RETURN_OK;
}


int handleRMAEnd( void* userData, uint64_t time, uint32_t process,
        uint32_t remote, uint32_t communicator, uint32_t tag, uint32_t source )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_RMAEND_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum ) {
  
                    if( remote != 0 ) {
                        /* transfer initiated from 3rd-party process */
                        fprintf( c->outfile, "(#%llu) \t%llu RMAEnd: initiator %u, "
                                             "remote %u, communicator %u, "
                                             "tag %u, source %llu\n",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, remote,
                                 communicator, tag,
                                 (long long unsigned) source);
                    }
                    else {
                        fprintf( c->outfile, "(#%llu) \t%llu RMAEnd: "
                                             "process %u, communicator %u, "
                                             "tag %u, source %llu\n",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, communicator, tag,
                                 (long long unsigned) source);
                    }
                }
        }
        return OTF_RETURN_OK;
}


int handleUnknown( void* userData, uint64_t time, uint32_t process,
	const char* record ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_UNKNOWN_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu Unknown: process %u, record \"%s\"\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, record );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefMarker( void *userData, uint32_t stream, uint32_t token, const char* name, uint32_t type ) {

	Control* c= (Control*) userData;

	if( c->records[OTF_DEFMARKER_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) DefMarker: ID %u, name \"%s\", type %u\n",
				(long long unsigned) c->num, token, name, type );
		}
	}

	return OTF_RETURN_OK;
}


int handleMarker( void *userData, uint64_t time, uint32_t process, 
		uint32_t token, const char* text ) {

	Control* c= (Control*) userData;

	if( c->records[OTF_MARKER_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum ) {

			fprintf( c->outfile, "(#%llu) \t%llu Marker: ID %u, process %u, text \"%s\"\n",
				(long long unsigned) c->num, (long long unsigned) time, token, process, text );
		}
	}

	return OTF_RETURN_OK;

}

