/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "config.h"


#include "OTF_Platform.h"
#include "OTF_Definitions.h"

#include <string.h>
#include <stdio.h>

#include "OTF_WStream.h"
#include "OTF_Platform.h"
#include "OTF_Filenames.h"

#include "OTF_Keywords.h"


/** constructor - internal use only */
int OTF_WStream_init( OTF_WStream* wstream );

/** destructor - internal use only */
int OTF_WStream_finish( OTF_WStream* wstream );

/** Write a DEFVERSION record to stream 'wstream'. */
int OTF_WStream_writeDefVersion( OTF_WStream* wstream, uint8_t major,
	uint8_t minor, uint8_t sub, const char* string );

/* ************************************************************************* */


int OTF_WStream_init( OTF_WStream* wstream ) {


	wstream->namestub= NULL;
	wstream->id= (uint32_t) -1;

	wstream->format= OTF_WSTREAM_FORMAT_SHORT;

	wstream->defBuffer= NULL;
	wstream->eventBuffer= NULL;
	wstream->snapsBuffer= NULL;
	wstream->statsBuffer= NULL;

#ifdef HAVE_ZLIB
	wstream->compression= 0;
	wstream->zbuffersizes= 1024*10;
#endif /* HAVE_ZLIB */

	wstream->buffersizes= 1024*1024;
	
	return 1;
}


int OTF_WStream_finish( OTF_WStream* wstream ) {


	int ret= 1;
#	ifdef OTF_DEBUG
		int tmpret;
#	endif
	
	free( wstream->namestub );
	wstream->namestub= NULL;

	wstream->id= (uint32_t) -1;


	if ( NULL != wstream->defBuffer ) {

#		ifndef OTF_DEBUG
			ret&= OTF_WBuffer_close( wstream->defBuffer );
#		else
			tmpret= OTF_WBuffer_close( wstream->defBuffer );
			if( 0 == tmpret ) {
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_close() failed for the def buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
			ret&= tmpret;
#		endif

		wstream->defBuffer= NULL;
	}

	if ( NULL != wstream->eventBuffer ) {

#		ifndef OTF_DEBUG
			ret&= OTF_WBuffer_close( wstream->eventBuffer );
#		else
			tmpret= OTF_WBuffer_close( wstream->eventBuffer );
			if( 0 == tmpret ) {
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_close() failed for the event buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
			ret&= tmpret;
#		endif

		wstream->eventBuffer= NULL;
	}

	if ( NULL != wstream->snapsBuffer ) {

#		ifndef OTF_DEBUG
			ret&= OTF_WBuffer_close( wstream->snapsBuffer );
#		else
			tmpret= OTF_WBuffer_close( wstream->snapsBuffer );
			if( 0 == tmpret ) {
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_close() failed for the snapshots buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
			ret&= tmpret;
#		endif
		
		wstream->snapsBuffer= NULL;
	}

	if ( NULL != wstream->statsBuffer ) {

#		ifndef OTF_DEBUG
			ret&= OTF_WBuffer_close( wstream->statsBuffer );
#		else
			tmpret= OTF_WBuffer_close( wstream->statsBuffer );
			if( 0 == tmpret ) {
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_close() failed for the statistics buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
			ret&= tmpret;
#		endif
		
		wstream->statsBuffer= NULL;
	}

	return ret;
}


OTF_WStream* OTF_WStream_open( const char* namestub, uint32_t id,
		OTF_FileManager* manager ) {


	OTF_WStream* ret= NULL;

	if( NULL == manager ) {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif

		return NULL;
	}

	ret= (OTF_WStream*) malloc( sizeof(OTF_WStream) );
	if( NULL == ret ) {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif

		return NULL;
	}

	OTF_WStream_init( ret );

	ret->namestub= strdup( namestub );
	ret->id= id;
	ret->manager= manager;

	/* leave buffers allone, they are allocated on demand */

	return ret;
}


int OTF_WStream_close( OTF_WStream* wstream ) {


	int ret= 0;
	
	if ( NULL != wstream ) {

		ret= OTF_WStream_finish( wstream );
#		ifdef OTF_VERBOSE
			if( 0 == ret ) {
			
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WStream_finish() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
#		endif

		free( wstream );
	}

	return ret;
}


int OTF_WStream_flush( OTF_WStream* wstream ) {


	int retval= 1;

	if ( NULL != wstream->defBuffer ) {

		retval= retval & OTF_WBuffer_flush( wstream->defBuffer );
	}

	if ( NULL != wstream->eventBuffer ) {

		retval= retval & OTF_WBuffer_flush( wstream->eventBuffer );
	}

	if ( NULL != wstream->snapsBuffer ) {

		retval= retval & OTF_WBuffer_flush( wstream->snapsBuffer );
	}

	if ( NULL != wstream->statsBuffer ) {

		retval= retval & OTF_WBuffer_flush( wstream->statsBuffer );
	}

	return retval;
}


OTF_WBuffer* OTF_WStream_getDefBuffer( OTF_WStream* wstream ) {


	/* have filename allocated an freed in order to avoid problems with 
	pre-allocated buffer being too short */


	char* filename;


	if ( NULL == wstream->defBuffer ) {


		filename= OTF_getFilename( wstream->namestub,
			wstream->id, OTF_FILETYPE_DEF, 0, NULL );
		if( NULL == filename ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}

#ifdef HAVE_ZLIB
		if ( wstream->compression > 0 ) {
		
			filename = strcat( filename, ".z" );
		}
#endif /* HAVE_ZLIB */

		if( NULL == filename ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}

		wstream->defBuffer= OTF_WBuffer_open_zlevel( filename, wstream->manager,
			wstream->compression );

		if( NULL == wstream->defBuffer ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_open() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}

#ifdef HAVE_ZLIB
		OTF_WBuffer_setZBufferSize( wstream->defBuffer, wstream->zbuffersizes );
#endif /* HAVE_ZLIB */
		
		free( filename );

#		ifndef OTF_DEBUG
			OTF_WBuffer_setSize( wstream->defBuffer, wstream->buffersizes );
#		else
			if( 0 == OTF_WBuffer_setSize( wstream->defBuffer,
				wstream->buffersizes ) ) {
			
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_setSize() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
#		endif
	}

	return wstream->defBuffer;
}


OTF_WBuffer* OTF_WStream_getEventBuffer( OTF_WStream* wstream ) {


	/* have filename allocated an freed in order to avoid problems with 
	pre-allocated buffer being too short */


	char* filename;


	if ( NULL == wstream->eventBuffer ) {


		filename= OTF_getFilename( wstream->namestub,
			wstream->id, OTF_FILETYPE_EVENT, 0, NULL );
		if( NULL == filename ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}

#ifdef HAVE_ZLIB
		if ( wstream->compression > 0 ) {
		
			filename = strcat( filename, ".z" );
		}
#endif /* HAVE_ZLIB */

		if( NULL == filename ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}
		
		wstream->eventBuffer= OTF_WBuffer_open_zlevel( filename, wstream->manager,
			wstream->compression );

		if( NULL == wstream->eventBuffer ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_open() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}

#ifdef HAVE_ZLIB
		OTF_WBuffer_setZBufferSize( wstream->eventBuffer, wstream->zbuffersizes );
#endif /* HAVE_ZLIB */
		
		free( filename );

#		ifndef OTF_DEBUG
			OTF_WBuffer_setSize( wstream->eventBuffer, wstream->buffersizes );
#		else
			if( 0 == OTF_WBuffer_setSize( wstream->eventBuffer,
				wstream->buffersizes ) ) {
			
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_setSize() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
#		endif
	}

	return wstream->eventBuffer;
}


OTF_WBuffer* OTF_WStream_getSnapshotBuffer( OTF_WStream* wstream ) {


	/* have filename allocated an freed in order to avoid problems with 
	pre-allocated buffer being too short */


	char* filename;


	if ( NULL == wstream->snapsBuffer ) {


		filename= OTF_getFilename( wstream->namestub,
			wstream->id, OTF_FILETYPE_SNAPS, 0, NULL );
		if( NULL == filename ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}

#ifdef HAVE_ZLIB
		if ( wstream->compression > 0) {
		
			filename = strcat( filename, ".z" );
		}
#endif /* HAVE_ZLIB */

		if( NULL == filename ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}
		
		wstream->snapsBuffer= OTF_WBuffer_open_zlevel( filename, wstream->manager,
			wstream->compression );
		if( NULL == wstream->snapsBuffer ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_open() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}

#ifdef HAVE_ZLIB
		OTF_WBuffer_setZBufferSize( wstream->snapsBuffer, wstream->zbuffersizes );
#endif /* HAVE_ZLIB */
		
		free( filename );

#		ifndef OTF_DEBUG
			OTF_WBuffer_setSize( wstream->snapsBuffer, wstream->buffersizes );
#		else
			if( 0 == OTF_WBuffer_setSize( wstream->snapsBuffer,
				wstream->buffersizes ) ) {
			
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_setSize() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
#		endif
	}

	return wstream->snapsBuffer;
}


OTF_WBuffer* OTF_WStream_getStatsBuffer( OTF_WStream* wstream ) {


	/* have filename allocated an freed in order to avoid problems with 
	pre-allocated buffer being too short */


	char* filename;


	if ( NULL == wstream->statsBuffer ) {


		filename= OTF_getFilename( wstream->namestub,
			wstream->id, OTF_FILETYPE_STATS, 0, NULL );
		if( NULL == filename ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}

#ifdef HAVE_ZLIB
		if ( wstream->compression > 0) {
		
			filename = strcat( filename, ".z" );
		}
#endif /* HAVE_ZLIB */

		if( NULL == filename ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}
		
		wstream->statsBuffer= OTF_WBuffer_open_zlevel( filename, wstream->manager,
			wstream->compression );
		if( NULL == wstream->statsBuffer ) {
		
#			ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_open() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
#			endif

			return NULL;
		}

		free( filename );

#		ifndef OTF_DEBUG
			OTF_WBuffer_setSize( wstream->statsBuffer, wstream->buffersizes );
#		else
			if( 0 == OTF_WBuffer_setSize( wstream->statsBuffer,
				wstream->buffersizes ) ) {
			
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_setSize() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
#		endif
	}

	return wstream->statsBuffer;
}


int OTF_WStream_setCompression( OTF_WStream* wstream, OTF_FileCompression
	compression ) {


#ifdef HAVE_ZLIB	
	if ( compression <= 9 ) {
	
		wstream->compression = compression;
		
		return 1;

	} else {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"compression is no expected value (%u). ignored.\n",
				__FUNCTION__, __FILE__, __LINE__, compression );
#		endif

		return 0;
	}
	
#else /* HAVE_ZLIB */

	if( 0 == compression ) {
	
		return 1;
		
	} else {
	
		return 0;
	}

#endif /* HAVE_ZLIB */
}

	
OTF_FileCompression OTF_WStream_getCompression( OTF_WStream* wstream ) {

#ifdef HAVE_ZLIB
	return wstream->compression;
#else
	return 0;
#endif /* HAVE_ZLIB */
}


void OTF_WStream_setBufferSizes( OTF_WStream* wstream, uint32_t size ) {


	if ( 50 > size ) {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"intended buffer size %u is too small, rejected.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
#		endif
		return;

	} else if ( 500 > size ) {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"buffer size %u is very small, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
#		endif

	} else if ( 10 * 1024 *1024 < size ) {

#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"buffer size %u is rather big, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
#		endif
	}

	wstream->buffersizes= size;
}


uint32_t OTF_WStream_getBufferSizes(OTF_WStream* wstream) {


	return wstream->buffersizes;
}


void OTF_WStream_setZBufferSizes( OTF_WStream* wstream, uint32_t size ) {


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

	wstream->zbuffersizes= size;

#endif /* HAVE_ZLIB */
}


uint32_t OTF_WStream_getZBufferSizes(OTF_WStream* wstream) {


#ifdef HAVE_ZLIB
	return wstream->zbuffersizes;
#else /* HAVE_ZLIB */
	return 0;
#endif /* HAVE_ZLIB */
}


void OTF_WStream_setFormat( OTF_WStream* wstream, uint32_t format ) {


#	ifdef OTF_VERBOSE
		if ( format > 1 ) {
	
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"unknown ouput format chosen.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	
	wstream->format= format;
}


uint32_t OTF_WStream_getFormat( OTF_WStream* wstream ) {


	return wstream->format;
}


/* *** definition record write handlers *** ******************************** */


int OTF_WStream_writeDefinitionComment( OTF_WStream* wstream,
		const char* comment ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFINITIONCOMMENT );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFINITIONCOMMENT " " );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefTimerResolution( OTF_WStream* wstream, 
		uint64_t ticksPerSecond ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFTIMERRESOLUTION );

		OTF_WBuffer_writeUint64( buffer, ticksPerSecond );
		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFTIMERRESOLUTION " " );

		OTF_WBuffer_writeUint64( buffer, ticksPerSecond );

		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefProcess( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t parent ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	
#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
			

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFPROCESS );

		OTF_WBuffer_writeUint32( buffer, deftoken );

		if ( NULL != name ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
			OTF_WBuffer_writeString( buffer, name );
		}

		if ( 0 != parent ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_PARENT );
			OTF_WBuffer_writeUint32( buffer, parent );
		}

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {


		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFPROCESS " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );

		if ( NULL != name ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
			OTF_WBuffer_writeString( buffer, name );
		}

		if ( 0 != parent ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_PARENT " " );
			OTF_WBuffer_writeUint32( buffer, parent );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefProcessGroup( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t n, const uint32_t* array ) {


	unsigned int i;
	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	
#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFPROCESSGROUP );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_MEMBERS );

		for ( i = 0; i < n; ++i ) {

			OTF_WBuffer_writeUint32( buffer, array[i] );
			OTF_WBuffer_writeChar( buffer, ',' );
		}

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFPROCESSGROUP " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_MEMBERS " " );

		for ( i = 0; i < n; ++i ) {

			OTF_WBuffer_writeUint32( buffer, array[i] );
			OTF_WBuffer_writeChar( buffer, ',' );
		}

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );

		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefFunction( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t group, uint32_t scltoken ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
			
	
	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFFUNCTION );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP );
		OTF_WBuffer_writeUint32( buffer, group );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {


		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFFUNCTION " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_GROUP " " );
		OTF_WBuffer_writeUint32( buffer, group );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefFunctionGroup( OTF_WStream* wstream, 
		uint32_t deftoken, const char* name ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
			
	
	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFFUNCTIONGROUP );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFFUNCTIONGROUP " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefCollectiveOperation( OTF_WStream* wstream, 
		uint32_t collOp, const char* name, uint32_t type ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


#	ifdef OTF_DEBUG
		if( 0 == collOp ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFCOLLOP );

		OTF_WBuffer_writeUint32( buffer, collOp );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TYPE );
		OTF_WBuffer_writeUint32( buffer, type );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFCOLLOP " " );

		OTF_WBuffer_writeUint32( buffer, collOp );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TYPE " " );
		OTF_WBuffer_writeUint32( buffer, type );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefCounter( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t properties, uint32_t countergroup, 
		const char* unit ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFCOUNTER );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP );
		OTF_WBuffer_writeUint32( buffer, countergroup );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROPERTIES );
		OTF_WBuffer_writeUint32( buffer, properties );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_UNIT );
		OTF_WBuffer_writeString( buffer, unit );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFCOUNTER " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_GROUP " " );
		OTF_WBuffer_writeUint32( buffer, countergroup );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_PROPERTIES " " );
		OTF_WBuffer_writeUint32( buffer, properties );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_UNIT " " );
		OTF_WBuffer_writeString( buffer, unit );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefCounterGroup( OTF_WStream* wstream, 
		uint32_t deftoken, const char* name ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFCOUNTERGROUP );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFCOUNTERGROUP " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefScl( OTF_WStream* wstream, uint32_t deftoken, 
		uint32_t sclfile, uint32_t sclline ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFSCL );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_FILE );
		OTF_WBuffer_writeUint32( buffer, sclfile );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_LINE );
		OTF_WBuffer_writeUint32( buffer, sclline );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFSCL " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_FILE " " );
		OTF_WBuffer_writeUint32( buffer, sclfile );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_LINE " " );
		OTF_WBuffer_writeUint32( buffer, sclline );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefSclFile( OTF_WStream* wstream, uint32_t deftoken, 
		const char* filename ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFSCLFILE );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, filename );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFSCLFILE " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, filename );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefVersion( OTF_WStream* wstream, uint8_t major,
		uint8_t minor, uint8_t sub, const char* string ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFVERSION );

		OTF_WBuffer_writeUint32( buffer, major );
		OTF_WBuffer_writeChar( buffer, '.' );
		OTF_WBuffer_writeUint32( buffer, minor );
		OTF_WBuffer_writeChar( buffer, '.' );
		OTF_WBuffer_writeUint32( buffer, sub );
		OTF_WBuffer_writeString( buffer, string );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFVERSION " " );

		OTF_WBuffer_writeUint32( buffer, major );
		OTF_WBuffer_writeChar( buffer, '.' );
		OTF_WBuffer_writeUint32( buffer, minor );
		OTF_WBuffer_writeChar( buffer, '.' );
		OTF_WBuffer_writeUint32( buffer, sub );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeOtfVersion( OTF_WStream* wstream ) {


	return OTF_WStream_writeDefVersion( wstream, OTF_VERSION_MAYOR,
		OTF_VERSION_MINOR, OTF_VERSION_SUB, OTF_VERSION_STRING );
}


int OTF_WStream_writeDefCreator( OTF_WStream* wstream, const char* creator ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFCREATOR );

		OTF_WBuffer_writeString( buffer, creator );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFCREATOR " " );

		OTF_WBuffer_writeString( buffer, creator );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeDefFile( OTF_WStream* wstream, uint32_t token,
	const char* name, uint32_t group ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


#	ifdef OTF_DEBUG
		if( 0 == token ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
			
	
	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFFILE );

		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP );
		OTF_WBuffer_writeUint32( buffer, group );

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {


		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFFILE " " );

		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_GROUP " " );
		OTF_WBuffer_writeUint32( buffer, group );


		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}

int OTF_WStream_writeDefFileGroup( OTF_WStream* wstream, uint32_t token,
	const char* name ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );


#	ifdef OTF_DEBUG
		if( 0 == token ) {
		
			fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
			
	
	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFFILEGROUP );

		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {


		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFFILEGROUP " " );

		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );


		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}
/* *** event record write handlers *** ************************************* */


int OTF_WStream_writeEnter( OTF_WStream* wstream, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );


	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, cpuid ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_ENTER );

		OTF_WBuffer_writeUint32( buffer, statetoken );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_ENTER " " );

		OTF_WBuffer_writeUint32( buffer, statetoken );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeRecvMsg( OTF_WStream* wstream, uint64_t time, 
		uint32_t receiver, uint32_t sender, uint32_t communicator, 
		uint32_t msgtag, uint32_t msglength, uint32_t scltoken ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );


	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, receiver ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_RECEIVE );

		OTF_WBuffer_writeUint32( buffer, sender );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH );
		OTF_WBuffer_writeUint32( buffer, msglength );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, msgtag );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, communicator );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_RECEIVE " " );

		OTF_WBuffer_writeUint32( buffer, sender );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_LENGTH " " );
		OTF_WBuffer_writeUint32( buffer, msglength );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, msgtag );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, communicator );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeSendMsg( OTF_WStream* wstream, uint64_t time, uint32_t sender,
		uint32_t receiver, uint32_t communicator, uint32_t msgtag, 
		uint32_t msglength, uint32_t scltoken ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );


	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, sender ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SEND );

		OTF_WBuffer_writeUint32( buffer, receiver );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH );
		OTF_WBuffer_writeUint32( buffer, msglength );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, msgtag );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, communicator );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SEND " " );

		OTF_WBuffer_writeUint32( buffer, receiver );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_LENGTH " " );
		OTF_WBuffer_writeUint32( buffer, msglength );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, msgtag );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, communicator );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeLeave( OTF_WStream* wstream, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, cpuid ) ) return 0;


	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_LEAVE );

		if( 0 != statetoken || 0 != scltoken ) {
		
			OTF_WBuffer_writeUint32( buffer, statetoken );
		}

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_LEAVE " " );

		if( 0 != statetoken || 0 != scltoken ) {
		
			OTF_WBuffer_writeUint32( buffer, statetoken );
		}
		
		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeCounter( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, uint32_t counter_token, uint64_t value ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );


	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_COUNTER );

		OTF_WBuffer_writeUint32( buffer, counter_token );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_VALUE );
		OTF_WBuffer_writeUint64( buffer, value );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_COUNTER " " );

		OTF_WBuffer_writeUint32( buffer, counter_token );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_VALUE " " );
		OTF_WBuffer_writeUint64( buffer, value );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeCollectiveOperation( OTF_WStream* wstream, uint64_t time, 
        uint32_t process, uint32_t functionToken, uint32_t communicator, 
	uint32_t rootprocess, uint32_t sent, uint32_t received, 
	uint64_t duration, uint32_t scltoken ) {


    OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );


    if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

    if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_COLLECTIVEOPERATION );

    	OTF_WBuffer_writeUint32( buffer, functionToken );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
    	OTF_WBuffer_writeUint32( buffer, communicator );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_ROOT );
    	OTF_WBuffer_writeUint32( buffer, rootprocess );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT );
    	OTF_WBuffer_writeUint32( buffer, sent );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD );
    	OTF_WBuffer_writeUint32( buffer, received );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_DURATION );
    	OTF_WBuffer_writeUint64( buffer, duration );

    	if ( 0 != scltoken ) {

    	    OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
    	    OTF_WBuffer_writeUint32( buffer, scltoken );
    	}

    	OTF_WBuffer_writeNewline( buffer );

    } else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_COLLECTIVEOPERATION " " );

    	OTF_WBuffer_writeUint32( buffer, functionToken );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
    	OTF_WBuffer_writeUint32( buffer, communicator );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_ROOT " " );
    	OTF_WBuffer_writeUint32( buffer, rootprocess );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SENT " " );
    	OTF_WBuffer_writeUint32( buffer, sent );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_RECVD " " );
    	OTF_WBuffer_writeUint32( buffer, received );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_DURATION " " );
    	OTF_WBuffer_writeUint64( buffer, duration );

    	if ( 0 != scltoken ) {

    	    OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
    	    OTF_WBuffer_writeUint32( buffer, scltoken );
    	}

    	OTF_WBuffer_writeNewline( buffer );
    }

    	return 1;
}


int OTF_WStream_writeEventComment( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, const char* comment ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );


	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_EVENTCOMMENT );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_EVENTCOMMENT " " );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeBeginProcess( OTF_WStream* wstream, uint64_t time,
    uint32_t process ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );


	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_BEGINPROCESS );
			
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_BEGINPROCESS " " );
			
		OTF_WBuffer_writeNewline( buffer );

	}

	return 1;
}


int OTF_WStream_writeEndProcess( OTF_WStream* wstream, uint64_t time,
    uint32_t process ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );


	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_ENDPROCESS );
			
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_ENDPROCESS " " );
			
		OTF_WBuffer_writeNewline( buffer );

	}

	return 1;
}


int OTF_WStream_writeFileOperation( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t handleid, uint32_t operation,
	uint64_t bytes, uint64_t duration, uint32_t source ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );
	

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_FILEOPERATION );

		
    	OTF_WBuffer_writeUint32( buffer, fileid );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID );
    	OTF_WBuffer_writeUint64( buffer, handleid );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_OPERATION );
    	OTF_WBuffer_writeUint32( buffer, operation );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTES );
    	OTF_WBuffer_writeUint64( buffer, bytes );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_DURATION );
    	OTF_WBuffer_writeUint64( buffer, duration );

    	if( 0 != source ) {
    		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
    		OTF_WBuffer_writeUint32( buffer, source );
    	}


		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_FILEOPERATION " " );


    	OTF_WBuffer_writeUint32( buffer, fileid );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_HANDLEID " " );
    	OTF_WBuffer_writeUint64( buffer, handleid );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_OPERATION " " );
    	OTF_WBuffer_writeUint32( buffer, operation );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_BYTES " " );
    	OTF_WBuffer_writeUint64( buffer, bytes );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_DURATION " " );
    	OTF_WBuffer_writeUint64( buffer, duration );

    	if( 0 != source ) {
    		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
    		OTF_WBuffer_writeUint32( buffer, source );
    	}
		
			
		OTF_WBuffer_writeNewline( buffer );

	}

	return 1;
}


/* *** public snapshot record write handlers *** */


int OTF_WStream_writeSnapshotComment( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, const char* comment ) {


	OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );


	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_S_SNAPSHOT_COMMENT );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SNAPSHOT_PREFIX OTF_KEYWORD_L_SNAPSHOT_COMMENT " " );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeEnterSnapshot( OTF_WStream* wstream, uint64_t time,
	    uint64_t originaltime, uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {


	OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, cpuid ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_S_SNAPSHOT_ENTER );

		OTF_WBuffer_writeUint32( buffer, statetoken );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_OTIME );
		OTF_WBuffer_writeUint64( buffer, originaltime );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SNAPSHOT_PREFIX OTF_KEYWORD_L_SNAPSHOT_ENTER " " );

		OTF_WBuffer_writeUint32( buffer, statetoken );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_OTIME " " );
		OTF_WBuffer_writeUint64( buffer, originaltime );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}



int OTF_WStream_writeSendSnapshot( OTF_WStream* wstream, uint64_t time,
		uint64_t originaltime, uint32_t sender, uint32_t receiver,
		uint32_t procGroup, uint32_t tag, uint32_t source ) {
	
	
	OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, sender ) ) return 0;
	
	
	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_S_SNAPSHOT_SEND );

		OTF_WBuffer_writeUint32( buffer, receiver );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_OTIME );
		OTF_WBuffer_writeUint64( buffer, originaltime );
		
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP );
		OTF_WBuffer_writeUint32( buffer, procGroup );
		
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, tag );
		
		if ( 0 != source ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, source );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_L_SNAPSHOT_SEND " " );

		OTF_WBuffer_writeUint32( buffer, receiver );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_OTIME " " );
		OTF_WBuffer_writeUint64( buffer, originaltime );
		
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_S_LOCAL_GROUP " " );
		OTF_WBuffer_writeUint32( buffer, procGroup );
		
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, tag );
		
		if ( 0 != source ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, source );
		}

		OTF_WBuffer_writeNewline( buffer );
	}
	
	return 1;
}


int OTF_WStream_writeOpenFileSnapshot( OTF_WStream* wstream,uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source ) {


	OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_S_SNAPSHOT_OPENFILE );

		OTF_WBuffer_writeUint32( buffer, fileid );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_OTIME );
		OTF_WBuffer_writeUint64( buffer, originaltime );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID );
		OTF_WBuffer_writeUint64( buffer, handleid );

		if ( 0 != source ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, source );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SNAPSHOT_PREFIX OTF_KEYWORD_L_SNAPSHOT_OPENFILE " " );

		OTF_WBuffer_writeUint32( buffer, fileid );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_OTIME " " );
		OTF_WBuffer_writeUint64( buffer, originaltime );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_HANDLEID " " );
		OTF_WBuffer_writeUint64( buffer, handleid );

		if ( 0 != source ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, source );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;

}


/* *** public summary record write handlers *** */


int OTF_WStream_writeSummaryComment( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, const char* comment ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );


	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SUM_PREFIX OTF_KEYWORD_S_SUMCOMMENT );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SUM_PREFIX OTF_KEYWORD_L_SUMCOMMENT " " );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );
	}

	return 1;
}


int OTF_WStream_writeFunctionSummary( OTF_WStream* wstream, 
		uint64_t time, uint32_t function, uint32_t process, 
		uint64_t count, uint64_t excltime, uint64_t incltime ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );
	
	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;


	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_SUMFUNCTION );

		OTF_WBuffer_writeUint32( buffer, function );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COUNT );
		OTF_WBuffer_writeUint64( buffer, count );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_EXCLTIME );
		OTF_WBuffer_writeUint64( buffer, excltime );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_INCLTIME );
		OTF_WBuffer_writeUint64( buffer, incltime );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_SUMFUNCTION " " );

		OTF_WBuffer_writeUint32( buffer, function );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COUNT " " );
		OTF_WBuffer_writeUint64( buffer, count );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_EXCLTIME " " );
		OTF_WBuffer_writeUint64( buffer, excltime );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_INCLTIME " " );
		OTF_WBuffer_writeUint64( buffer, incltime );
	}

	OTF_WBuffer_writeNewline( buffer );
	
	return 1;
}


int OTF_WStream_writeFunctionGroupSummary( OTF_WStream* wstream, 
		uint64_t time,  uint32_t functiongroup,  uint32_t process,  
		uint64_t count,  uint64_t excltime,  uint64_t incltime ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );
	
	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;


	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_SUMFUNCTIONGROUP );
	
		OTF_WBuffer_writeUint32( buffer, functiongroup );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COUNT );
		OTF_WBuffer_writeUint64( buffer, count );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_EXCLTIME );
		OTF_WBuffer_writeUint64( buffer, excltime );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_INCLTIME );
		OTF_WBuffer_writeUint64( buffer, incltime );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_SUMFUNCTIONGROUP " " );

		OTF_WBuffer_writeUint32( buffer, functiongroup );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COUNT " " );
		OTF_WBuffer_writeUint64( buffer, count );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_EXCLTIME " " );
		OTF_WBuffer_writeUint64( buffer, excltime );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_INCLTIME " " );
		OTF_WBuffer_writeUint64( buffer, incltime );
	}

	OTF_WBuffer_writeNewline( buffer );
	
	return 1;
}


int OTF_WStream_writeMessageSummary( OTF_WStream* wstream, 
		uint64_t time, uint32_t process, uint32_t peer, 
		uint32_t comm, uint32_t tag, uint64_t number_sent,
		uint64_t number_recvd, uint64_t bytes_sent, 
		uint64_t bytes_recved ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );
	
	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;


	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_SUMMESSAGE );

		OTF_WBuffer_writeUint32( buffer, peer );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, comm );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, tag );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSENT );
		OTF_WBuffer_writeUint64( buffer, number_sent );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERRECVD );
		OTF_WBuffer_writeUint64( buffer, number_recvd );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT );
		OTF_WBuffer_writeUint64( buffer, bytes_sent );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD );
		OTF_WBuffer_writeUint64( buffer, bytes_recved );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_SUMMESSAGE " " );

		OTF_WBuffer_writeUint32( buffer, peer );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, comm );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, tag );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERSENT " " );
		OTF_WBuffer_writeUint64( buffer, number_sent );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERRECVD " " );
		OTF_WBuffer_writeUint64( buffer, number_recvd );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SENT " " );
		OTF_WBuffer_writeUint64( buffer, bytes_sent );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_RECVD " " );
		OTF_WBuffer_writeUint64( buffer, bytes_recved );
	}

	OTF_WBuffer_writeNewline( buffer );
	
	return 1;
}


int OTF_WStream_writeFileOperationSummary( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );
	
	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;


	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_SUMFILEOPERATION );

		OTF_WBuffer_writeUint32( buffer, fileid );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBEROPEN );
		OTF_WBuffer_writeUint64( buffer, nopen );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERCLOSE );
		OTF_WBuffer_writeUint64( buffer, nclose );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERREAD );
		OTF_WBuffer_writeUint64( buffer, nread );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERWRITE );
		OTF_WBuffer_writeUint64( buffer, nwrite );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSEEK );
		OTF_WBuffer_writeUint64( buffer, nseek );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESREAD );
		OTF_WBuffer_writeUint64( buffer, bytesread );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESWRITE );
		OTF_WBuffer_writeUint64( buffer, byteswrite );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_SUMFILEOPERATION " " );

		OTF_WBuffer_writeUint32( buffer, fileid );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBEROPEN " " );
		OTF_WBuffer_writeUint64( buffer, nopen );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERCLOSE " " );
		OTF_WBuffer_writeUint64( buffer, nclose );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERREAD " " );
		OTF_WBuffer_writeUint64( buffer, nread );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERWRITE " " );
		OTF_WBuffer_writeUint64( buffer, nwrite );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERSEEK " " );
		OTF_WBuffer_writeUint64( buffer, nseek );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_BYTESREAD " " );
		OTF_WBuffer_writeUint64( buffer, bytesread );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_BYTESWRITE " " );
		OTF_WBuffer_writeUint64( buffer, byteswrite );
	}

	OTF_WBuffer_writeNewline( buffer );
	
	return 1;
	
}


int OTF_WStream_writeFileGroupOperationSummary( OTF_WStream* wstream, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );
	
	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;


	if ( OTF_WSTREAM_FORMAT_SHORT == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_SUMFILEGROUPOPERATION );

		OTF_WBuffer_writeUint32( buffer, groupid );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBEROPEN );
		OTF_WBuffer_writeUint64( buffer, nopen );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERCLOSE );
		OTF_WBuffer_writeUint64( buffer, nclose );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERREAD );
		OTF_WBuffer_writeUint64( buffer, nread );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERWRITE );
		OTF_WBuffer_writeUint64( buffer, nwrite );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSEEK );
		OTF_WBuffer_writeUint64( buffer, nseek );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESREAD );
		OTF_WBuffer_writeUint64( buffer, bytesread );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESWRITE );
		OTF_WBuffer_writeUint64( buffer, byteswrite );

	} else if ( OTF_WSTREAM_FORMAT_LONG == wstream->format ) {

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_SUMFILEGROUPOPERATION " " );

		OTF_WBuffer_writeUint32( buffer, groupid );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBEROPEN " " );
		OTF_WBuffer_writeUint64( buffer, nopen );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERCLOSE " " );
		OTF_WBuffer_writeUint64( buffer, nclose );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERREAD " " );
		OTF_WBuffer_writeUint64( buffer, nread );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERWRITE " " );
		OTF_WBuffer_writeUint64( buffer, nwrite );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERSEEK " " );
		OTF_WBuffer_writeUint64( buffer, nseek );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_BYTESREAD " " );
		OTF_WBuffer_writeUint64( buffer, bytesread );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_BYTESWRITE " " );
		OTF_WBuffer_writeUint64( buffer, byteswrite );
	}

	OTF_WBuffer_writeNewline( buffer );
	
	return 1;
	
}
