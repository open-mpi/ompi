/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "config.h"


#include "OTF_Platform.h"
#include "OTF_RStream.h"
#include "OTF_Platform.h"
#include "OTF_Reader.h"
#include "OTF_Parse.h"


/** constructor - internal use only */
int OTF_RStream_init( OTF_RStream* rstream );

/** destructor - internal use only */
int OTF_RStream_finish( OTF_RStream* rstream );


/* ************************************************************************** */


int OTF_RStream_init( OTF_RStream* rstream ) {


	rstream->namestub= NULL;
	rstream->id= (uint32_t) -1;

	rstream->defBuffer= NULL;
	rstream->eventBuffer= NULL;
	rstream->snapsBuffer= NULL;
	rstream->statsBuffer= NULL;

	rstream->buffersizes= 1024*1024;
	
#ifdef HAVE_ZLIB
	rstream->zbuffersizes= 1024*10;
#endif /* HAVE_ZLIB */

	rstream->manager= NULL;

	rstream->recordLimit= OTF_READ_MAXRECORDS;

	return 1;
}


int OTF_RStream_finish( OTF_RStream* rstream ) {


	int ret=1;
	int tmpret;
	
	free( rstream->namestub );
	rstream->namestub= NULL;

	rstream->id= (uint32_t) -1;

	if ( NULL != rstream->defBuffer ) {

		tmpret= OTF_RBuffer_close( rstream->defBuffer );
		ret &= tmpret;
		if( 0 == tmpret ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"cannot close defbuffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */		
		}
		rstream->defBuffer= NULL;
	}

	if ( NULL != rstream->eventBuffer ) {

		tmpret= OTF_RBuffer_close( rstream->eventBuffer );
		ret &= tmpret;
		if( 0 == tmpret ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"cannot close event buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */		
		}
		rstream->eventBuffer= NULL;
	}

	if ( NULL != rstream->snapsBuffer ) {

		tmpret= OTF_RBuffer_close( rstream->snapsBuffer );
		ret &= tmpret;
		if( 0 == tmpret ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"cannot close snapshots buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */		
		}
		rstream->snapsBuffer= NULL;
	}

	if ( NULL != rstream->statsBuffer ) {

		tmpret= OTF_RBuffer_close( rstream->statsBuffer );
		ret &= tmpret;
		if( 0 == tmpret ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"cannot close statistics buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */		
		}
		rstream->statsBuffer= NULL;
	}

	return ret;
}


OTF_RStream* OTF_RStream_open( const char* namestub, uint32_t id, OTF_FileManager* manager ) {


	OTF_RStream* ret= (OTF_RStream*) malloc( sizeof(OTF_RStream) );
	if( NULL == ret ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif /* OTF_VERBOSE */

		return NULL;
	}

	OTF_RStream_init( ret );

	ret->namestub= strdup( namestub );
	ret->id= id;

	if( NULL == manager ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif /* OTF_VERBOSE */

		free( ret );
		ret= NULL;

		return NULL;
	}
	ret->manager= manager;

	/* leave buffers alone, they are allocated on demand */

	return ret;
}


int OTF_RStream_close( OTF_RStream* rstream ) {


	int ret= 1;
	
	if( NULL == rstream ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"rstream has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif /* OTF_VERBOSE */

		return 0;
	}

	ret &= OTF_RStream_finish( rstream );

	free( rstream );

	return ret;
}


OTF_RBuffer* OTF_RStream_getDefBuffer( OTF_RStream* rstream ) {


	char* filename;

	
	if ( NULL == rstream->defBuffer ) {

		filename= OTF_getFilename( rstream->namestub,
			rstream->id, OTF_FILETYPE_DEF, 0, NULL );
		if( NULL == filename ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */

			return NULL;
		}

		rstream->defBuffer= OTF_RBuffer_open( filename, rstream->manager );
		free( filename );

		if ( NULL == rstream->defBuffer ) {

			return NULL;
		}

		OTF_RBuffer_setSize( rstream->defBuffer, rstream->buffersizes );
#ifdef HAVE_ZLIB
		OTF_RBuffer_setZBufferSize( rstream->defBuffer, rstream->zbuffersizes );
#endif /* HAVE_ZLIB */
	}

	return rstream->defBuffer;
}


int OTF_RStream_closeDefBuffer( OTF_RStream* rstream ) {


	int ret= 1;
	
	if ( NULL != rstream->defBuffer ) {

		ret&= OTF_RBuffer_close( rstream->defBuffer );
		if( 0 == ret ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"closing defbuffer failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */

		}
		
		rstream->defBuffer= NULL;
	}
	
	return ret;
}


OTF_RBuffer* OTF_RStream_getEventBuffer( OTF_RStream* rstream ) {


	char* filename;


	if ( NULL == rstream->eventBuffer ) {

		filename = OTF_getFilename( rstream->namestub,
			rstream->id, OTF_FILETYPE_EVENT, 0, NULL );
		if( NULL == filename ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */

			return NULL;
		}

		rstream->eventBuffer= OTF_RBuffer_open( filename, rstream->manager );
		free( filename );

		if ( NULL == rstream->eventBuffer ) {

			return NULL;
		}

		OTF_RBuffer_setSize( rstream->eventBuffer, rstream->buffersizes );
#ifdef HAVE_ZLIB
		OTF_RBuffer_setZBufferSize( rstream->eventBuffer, rstream->zbuffersizes );
#endif /* HAVE_ZLIB */
	}

	return rstream->eventBuffer;
}


int OTF_RStream_closeEventBuffer( OTF_RStream* rstream ) {


	int ret= 1;
	
	if ( NULL != rstream->eventBuffer ) {

		ret&= OTF_RBuffer_close( rstream->eventBuffer );
		if( 0 == ret ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"closing event buffer failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */
		}
		rstream->eventBuffer= NULL;
	}
	
	return ret;
}


OTF_RBuffer* OTF_RStream_getSnapsBuffer( OTF_RStream* rstream ) {


	char* filename;


	if ( NULL == rstream->snapsBuffer ) {

		filename= OTF_getFilename( rstream->namestub,
			rstream->id, OTF_FILETYPE_SNAPS, 0, NULL );
		if( NULL == filename ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */

			return NULL;
		}

		rstream->snapsBuffer= OTF_RBuffer_open( filename, rstream->manager );
		free( filename );

		if ( NULL == rstream->snapsBuffer ) {

			return NULL;
		}

		OTF_RBuffer_setSize( rstream->snapsBuffer, rstream->buffersizes );
#ifdef HAVE_ZLIB
		OTF_RBuffer_setZBufferSize( rstream->snapsBuffer, rstream->zbuffersizes );
#endif /* HAVE_ZLIB */
	}

	return rstream->snapsBuffer;
}


int OTF_RStream_closeSnapsBuffer( OTF_RStream* rstream ) {


	int ret= 1;
	
	if ( NULL != rstream->snapsBuffer ) {

		ret&= OTF_RBuffer_close( rstream->snapsBuffer );
		if( 0 == ret ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"closing snapshots buffer failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */
		}
		rstream->snapsBuffer= NULL;
	}
	
	return ret;
}


OTF_RBuffer* OTF_RStream_getStatsBuffer( OTF_RStream* rstream ) {


	char* filename;


	if ( NULL == rstream->statsBuffer ) {

		filename= OTF_getFilename( rstream->namestub,
			rstream->id, OTF_FILETYPE_STATS, 0, NULL );
		if( NULL == filename ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */

			return NULL;
		}


		rstream->statsBuffer= OTF_RBuffer_open( filename, rstream->manager );
		free( filename );

		if ( NULL == rstream->statsBuffer ) {

#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_RBuffer_open() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */

			return NULL;
		}

		OTF_RBuffer_setSize( rstream->statsBuffer, rstream->buffersizes );
#ifdef HAVE_ZLIB
		OTF_RBuffer_setZBufferSize( rstream->statsBuffer, rstream->zbuffersizes );
#endif /* HAVE_ZLIB */
	}

	return rstream->statsBuffer;
}


int OTF_RStream_closeStatsBuffer( OTF_RStream* rstream ) {


	int ret= 1;
	
	if ( NULL != rstream->statsBuffer ) {

		ret&= OTF_RBuffer_close( rstream->statsBuffer );
		if( 0 == ret ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"closing statistics buffer failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */
		}
		rstream->statsBuffer= NULL;
	}
	
	return ret;
}


void OTF_RStream_setBufferSizes( OTF_RStream* rstream, uint32_t size ) {


	if ( 50 > size ) {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"intended buffer size %u is too small, rejected.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
#		endif /* OTF_VERBOSE */
		
		return;

	} else if ( 500 > size ) {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"buffer size %u is very small, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
#		endif /* OTF_VERBOSE */

	} else if ( 10 * 1024 *1024 < size ) {

#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"buffer size %u is rather big, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
#		endif /* OTF_VERBOSE */
	}

	rstream->buffersizes= size;
}


uint32_t OTF_RStream_getBufferSizes(OTF_RStream* rstream) {


	return rstream->buffersizes;
}


void OTF_RStream_setZBufferSizes( OTF_RStream* rstream, uint32_t size ) {


#ifdef HAVE_ZLIB
	
	if ( 32 > size ) {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"intended zbuffer size %u is too small, rejected.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
#		endif /* OTF_VERBOSE */
		
		return;

	} else if ( 512 > size ) {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"zbuffer size %u is very small, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
#		endif /* OTF_VERBOSE */

	} else if ( 10 * 1024 *1024 < size ) {

#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"zbuffer size %u is rather big, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
#		endif /* OTF_VERBOSE */
	}

	rstream->zbuffersizes= size;

#endif /* HAVE_ZLIB */
}


uint32_t OTF_RStream_getZBufferSizes(OTF_RStream* rstream) {


#ifdef HAVE_ZLIB
	return rstream->zbuffersizes;
#else /* HAVE_ZLIB */
	return 0;
#endif /* HAVE_ZLIB */
}


void OTF_RStream_setRecordLimit( OTF_RStream* rstream, uint64_t limit ) {


	if( limit == OTF_READ_ERROR ) {

#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"limit cannot be set to %llu. Reset to OTF_READ_MAXRECORDS.\n",
				__FUNCTION__, __FILE__, __LINE__,
				(long long unsigned) limit );
#		endif /* OTF_VERBOSE */
	
		limit= OTF_READ_MAXRECORDS;
	}

	rstream->recordLimit= limit;
}


uint64_t OTF_RStream_getRecordLimit( OTF_RStream* rstream ) {


	return rstream->recordLimit;
}



uint64_t OTF_RStream_readDefinitions( OTF_RStream* rstream, OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;

	int ret;


	/* initialized? */
	if ( NULL == rstream->defBuffer ) {


		/* init */

		rstream->defBuffer= OTF_RStream_getDefBuffer( rstream );

		if ( NULL == rstream->defBuffer ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"the stream has no def buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */
			
			/* there is no def buffer available for this stream */
			return OTF_READ_ERROR;
		}
	}


	while ( NULL != OTF_RBuffer_getRecord( rstream->defBuffer ) ) {

		if ( recordcount >= rstream->recordLimit ) {

			/* record count limit reached, return */
			return recordcount;
		}

		ret= OTF_Reader_parseDefRecord( rstream->defBuffer, handlers, rstream->id );
		if ( 0 == ret ) {

			/* maybe later an errorhandler gives the record to the user */
			return OTF_READ_ERROR;
		}

		recordcount++;
	}

	return recordcount;
}


uint64_t OTF_RStream_readEvents( OTF_RStream* rstream, OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;

	int ret;

#	ifdef OTF_DEBUG
		uint64_t currenttime= 0;
		uint64_t oldtime= 0;
#	endif


	/* initialized? */
	if ( NULL == rstream->eventBuffer ) {


		/* init */

		rstream->eventBuffer= OTF_RStream_getEventBuffer( rstream );
		if( NULL == rstream->eventBuffer ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"the stream has no event buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */
			
			return OTF_READ_ERROR;
		}


		/* jump to start time - update buffer-firstTime, lastTime, curTime */
		OTF_RBuffer_searchTime( rstream->eventBuffer, 0 );
	}


	while ( NULL != OTF_RBuffer_getRecord( rstream->eventBuffer ) ) {

		if ( recordcount >= rstream->recordLimit ) {

			/* record count limit reached, return */
			return recordcount;
		}

		/* debug check */
#		ifdef OTF_DEBUG
			oldtime= currenttime;
		/* inlined OTF_RBuffer_getCurrentTime() */
			currenttime= rstream->eventBuffer->time;
			
			if ( oldtime > currenttime ) {
		
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"Time does decrease!!! %llu < %llu.\n",
					__FUNCTION__, __FILE__, __LINE__,
					(unsigned long long) currenttime,
					(unsigned long long) oldtime );
				
				return  OTF_READ_ERROR;
			}
#		endif

		ret= OTF_Reader_parseEventRecord( rstream->eventBuffer, handlers );
		if ( 0 == ret ) {

			/* maybe later an errorhandler gives the record to the user */
			return OTF_READ_ERROR;
		}

		recordcount++;
	}

	return recordcount;
}


uint64_t OTF_RStream_readSnapshots( OTF_RStream* rstream, OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;

	int ret;

#	ifdef OTF_DEBUG
		uint64_t currenttime= 0;
		uint64_t oldtime= 0;
#	endif


	/* initialized? */
	if ( NULL == rstream->snapsBuffer ) {


		/* init */

		rstream->snapsBuffer= OTF_RStream_getSnapsBuffer( rstream );
		if( NULL == rstream->snapsBuffer ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"the stream has no snapshots buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */
			
			return OTF_READ_ERROR;
		}

		
		/* jump to start time - update buffer-firstTime, lastTime, curTime */
		OTF_RBuffer_searchTime( rstream->snapsBuffer, 0 );
	}


	while ( NULL != OTF_RBuffer_getRecord( rstream->snapsBuffer ) ) {

		if ( recordcount >= rstream->recordLimit ) {

			/* record count limit reached, return */
			return recordcount;
		}

		/* debug check */
#		ifdef OTF_DEBUG
			oldtime= currenttime;
			/* inlined OTF_RBuffer_getCurrentTime() */
			currenttime= rstream->snapsBuffer->time;
			
			if ( oldtime > currenttime ) {
		
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"Time does decrease!!! %llu < %llu.\n",
					__FUNCTION__, __FILE__, __LINE__,
					(unsigned long long) currenttime,
					(unsigned long long) oldtime );
				
				return OTF_READ_ERROR;
			}
#		endif


		ret= OTF_Reader_parseSnapshotsRecord( rstream->snapsBuffer, handlers );
		if ( 0 == ret ) {

			/* maybe later an errorhandler gives the record to the user */
			return OTF_READ_ERROR;
		}

		recordcount++;
	}


	return recordcount;
}


uint64_t OTF_RStream_readStatistics( OTF_RStream* rstream, OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;

	int ret;

#	ifdef OTF_DEBUG
		uint64_t currenttime= 0;
		uint64_t oldtime= 0;
#	endif


	/* initialized? */
	if ( NULL == rstream->statsBuffer ) {


		/* init */

		rstream->statsBuffer= OTF_RStream_getStatsBuffer( rstream );
		if( NULL == rstream->statsBuffer ) {
			
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"the stream has no statistics buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif /* OTF_VERBOSE */
			
			return OTF_READ_ERROR;
		}

		
		/* jump to start time - update buffer-firstTime, lastTime, curTime */
		OTF_RBuffer_searchTime( rstream->statsBuffer, 0 );
	}


	while ( NULL != OTF_RBuffer_getRecord( rstream->statsBuffer ) ) {

		if ( recordcount >= rstream->recordLimit ) {

			/* record count limit reached, return */
			return recordcount;
		}

		/* debug check */
#		ifdef OTF_DEBUG
			oldtime= currenttime;
			/* inlined OTF_RBuffer_getCurrentTime() */
			currenttime= rstream->statsBuffer->time;

			if ( oldtime > currenttime ) {
		
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"Time does decrease!!! %llu < %llu.\n",
					__FUNCTION__, __FILE__, __LINE__,
					(unsigned long long) currenttime,
					(unsigned long long) oldtime );
				
				return OTF_READ_ERROR;
			}
#		endif


		ret= OTF_Reader_parseStatisticsRecord( rstream->statsBuffer, handlers );
		if ( 0 == ret ) {


			/* maybe later an errorhandler gives the record to the user */
			return OTF_READ_ERROR;
		}

		recordcount++;
	}


	return recordcount;
}


uint8_t OTF_RStream_eventProgress( OTF_RStream* rstream, uint64_t* minimum,
	uint64_t* current, uint64_t* maximum ) {
	
	
	return OTF_RStream_eventTimeProgress( rstream, minimum, current, maximum );
}

	
uint8_t OTF_RStream_snapshotProgress( OTF_RStream* rstream,
		uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {
		
		
	return OTF_RStream_snapshotTimeProgress( rstream, minimum, current, maximum );
}

		
uint8_t OTF_RStream_statisticProgress( OTF_RStream* rstream,
	uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {
	
	
	return OTF_RStream_statisticTimeProgress( rstream, minimum, current, maximum );
}


uint8_t OTF_RStream_eventTimeProgress( OTF_RStream* rstream,
		uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {


	if ( NULL == rstream->eventBuffer ||
		rstream->eventBuffer->time < rstream->eventBuffer->firstTime ||
		rstream->eventBuffer->time > rstream->eventBuffer->lastTime ) {
	
		return 0;
	}

	*minimum= rstream->eventBuffer->firstTime;
	*current= rstream->eventBuffer->time;
	*maximum= rstream->eventBuffer->lastTime;

	return 1;
}


uint8_t OTF_RStream_snapshotTimeProgress( OTF_RStream* rstream,
		uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {


	if ( NULL == rstream->snapsBuffer ||
		rstream->snapsBuffer->time < rstream->snapsBuffer->firstTime ||
		rstream->snapsBuffer->time > rstream->snapsBuffer->lastTime ) {
	
		return 0;
	}

	*minimum= rstream->snapsBuffer->firstTime;
	*current= rstream->snapsBuffer->time;
	*maximum= rstream->snapsBuffer->lastTime;

	return 1;
}


uint8_t OTF_RStream_statisticTimeProgress( OTF_RStream* rstream,
		uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {


	if ( NULL == rstream->statsBuffer ||
		rstream->statsBuffer->time < rstream->statsBuffer->firstTime ||
		rstream->statsBuffer->time > rstream->statsBuffer->lastTime ) {
	
		return 0;
	}

	*minimum= rstream->statsBuffer->firstTime;
	*current= rstream->statsBuffer->time;
	*maximum= rstream->statsBuffer->lastTime;

	return 1;
}


uint8_t OTF_RStream_eventBytesProgress( OTF_RStream* rstream, uint64_t* minimum,
	uint64_t* current, uint64_t* maximum ) {
	
	
	*minimum= 0;
	*maximum= OTF_RBuffer_getFileSize( rstream->eventBuffer );
	*current= OTF_RBuffer_getFilePos( rstream->eventBuffer );
		
	if( *current > *maximum ) {
		*current= *maximum;
	}
	
	return 1;
}


uint8_t OTF_RStream_snapshotBytesProgress( OTF_RStream* rstream,
		uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {
		
		
	*minimum= 0;
	*maximum= OTF_RBuffer_getFileSize( rstream->snapsBuffer );
	*current= OTF_RBuffer_getFilePos( rstream->snapsBuffer );
		
	if( *current > *maximum ) {
		*current= *maximum;
	}
	
	return 1;
}


uint8_t OTF_RStream_statisticBytesProgress( OTF_RStream* rstream,
	uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {
	
	
	*minimum= 0;
	*maximum= OTF_RBuffer_getFileSize( rstream->statsBuffer );
	*current= OTF_RBuffer_getFilePos( rstream->statsBuffer );
		
	if( *current > *maximum ) {
		*current= *maximum;
	}
	
	return 1;
}
