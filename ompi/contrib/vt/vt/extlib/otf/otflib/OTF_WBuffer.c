/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "config.h"


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "OTF_Platform.h"
#include "OTF_WBuffer.h"


/** constructor - internal use only */
int OTF_WBuffer_init( OTF_WBuffer* wbuffer );

/** destructor - internal use only */
int OTF_WBuffer_finish( OTF_WBuffer* wbuffer );

/* ************************************************************************** */


int OTF_WBuffer_init( OTF_WBuffer* wbuffer ) {


	wbuffer->file= NULL;

	/* buffer is allocated on demand */
	wbuffer->buffer = NULL;
	wbuffer->size = 0;
	wbuffer->pos = 0;

	wbuffer->process = (uint32_t) -1;
	wbuffer->time = 0;

#ifdef HAVE_ZLIB
	wbuffer->zbuffersize= 1024*10;
#endif /* HAVE_ZLIB */
	
	return 1;
}


int OTF_WBuffer_finish( OTF_WBuffer* wbuffer ) {


	/* buffer shall be empty now */
	if( 0 != wbuffer->pos ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"buffer is not empty (but is supposed to).\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif

		return 0;
	}

	free( wbuffer->buffer );
	wbuffer->buffer= NULL;

	return 1;
}


OTF_WBuffer* OTF_WBuffer_open( const char* filename, OTF_FileManager* manager ) {


	/* choose standard compression */
	return OTF_WBuffer_open_zlevel( filename, manager, OTF_FILECOMPRESSION_COMPRESSED );
}


int OTF_WBuffer_close( OTF_WBuffer* wbuffer ) {


#	ifndef OTF_DEBUG
		int ret= OTF_WBuffer_flush( wbuffer );
		
		ret&= OTF_File_close( wbuffer->file );
		
		ret&= OTF_WBuffer_finish( wbuffer );
	
#	else
		int ret= 1;
		int tmpret= OTF_WBuffer_flush( wbuffer );
		if( 0 == tmpret ) {
			
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_WBuffer_flush() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
		ret&= tmpret;
		
		tmpret= OTF_File_close( wbuffer->file );
		if( 0 == tmpret ) {
			
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_close() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
		ret&= tmpret;
		
		tmpret= OTF_WBuffer_finish( wbuffer );
		if( 0 == tmpret ) {
			
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_finish() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
		}
		ret&= tmpret;

#	endif

	free( wbuffer );
	
	return ret;
}


int OTF_WBuffer_setSize( OTF_WBuffer* wbuffer, size_t size ) {


	if ( size < wbuffer->size ) {

#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"cannot shrink buffer from %u to %u.\n",
				__FUNCTION__, __FILE__, __LINE__, (uint32_t) wbuffer->size,
				(uint32_t) size );
#		endif

		return 0;
	}

	wbuffer->buffer= (char*) realloc( wbuffer->buffer, 
		size * sizeof(char) );
	if( NULL == wbuffer->buffer ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif

		return 0;
	}
	
	wbuffer->size= (uint32_t) size;

	return 1;
}


void OTF_WBuffer_setZBufferSize( OTF_WBuffer* wbuffer, uint32_t size ) {


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

	wbuffer->zbuffersize= size;
	
	if( NULL != wbuffer->file ) {
		OTF_File_setZBufferSize( wbuffer->file, wbuffer->zbuffersize );
	}
#endif /* HAVE_ZLIB */

}


int OTF_WBuffer_flush( OTF_WBuffer* wbuffer ) {


	int retval= 1;
	size_t ret;


	ret= OTF_File_write( wbuffer->file, wbuffer->buffer, wbuffer->pos );
	if( ret != wbuffer->pos ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_write() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif
		retval= 0;
	}
	
	wbuffer->pos= 0;

	return retval;
}


int OTF_WBuffer_guarantee( OTF_WBuffer* wbuffer, size_t space ) {


	if ( wbuffer->pos + space <= wbuffer->size ) {

		return 1;
	}

	/* not enough space available */

	if ( space > wbuffer->size ) {

#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"requested %u bytes > buffer size %u.\n",
				__FUNCTION__, __FILE__, __LINE__, 
				(uint32_t) space, wbuffer->size );
#		endif
		
		return 0;
	}

	if( 0 == OTF_WBuffer_flush( wbuffer ) ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_WBuffer_flush() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif
		
		return 0;
	}

	return 1;
}


int OTF_WBuffer_setTimeAndProcess( OTF_WBuffer* wbuffer, 
		uint64_t t, uint32_t p ) {


	/* write timestamp 0 always in order to generate a valid time stamp 
	at the very beginning of every stream */
	if ( ( t == wbuffer->time ) && ( 0 < t ) ) {

		if ( p == wbuffer->process ) {

			return 1;
		}

		OTF_WBuffer_writeChar( wbuffer, '*' );
		OTF_WBuffer_writeUint32( wbuffer, p );
		OTF_WBuffer_writeNewline( wbuffer );

		wbuffer->process = p;

		return 1;
	}


	/* time must be strictly monotonous increasing or 0 (see above), 
	the '==' case is already catched above and must not happen here */
	if ( ( t > wbuffer->time ) || ( 0 == t ) ) {

		OTF_WBuffer_writeUint64( wbuffer, t );
		OTF_WBuffer_writeNewline( wbuffer );

		OTF_WBuffer_writeChar( wbuffer, '*' );
		OTF_WBuffer_writeUint32( wbuffer, p );
		OTF_WBuffer_writeNewline( wbuffer );

		wbuffer->time = t;
		wbuffer->process = p;

		return 1;

	} else {

		/* print error message only once per buffer.
		there should be _no_ way to avoid this error message! */
		if ( ( (uint64_t) -1 ) != wbuffer->time ) {

			fprintf( stderr, "OTF ERROR in function %s, file: %s, line: %i:\n "
				"time not increasing. (t= %llu, p= %u)\n",
				__FUNCTION__, __FILE__, __LINE__, 
				(unsigned long long int) t, (unsigned int) p );

			/* write comment to the end of the stream */
			OTF_WBuffer_writeKeyword( wbuffer, "#" );
			OTF_WBuffer_writeString( wbuffer, "error due to unsorted time stamp, aborted" );
			OTF_WBuffer_writeNewline( wbuffer );

			/* disable the buffer for future use, don't repeat the error message*/
			wbuffer->time = (uint64_t) -1;
		}

		return 0;
	}
}


uint32_t OTF_WBuffer_writeKeyword( OTF_WBuffer* wbuffer, 
		const char* keyword ) {


	uint32_t i;
	uint32_t l= (uint32_t) strlen( keyword );

	int ret= OTF_WBuffer_guarantee( wbuffer, l );
	if( 0 == ret ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_WBuffer_guarantee() failed.\n",
				__FUNCTION__, __FILE__, __LINE__  );
#		endif

		return 0;
	}

	i= 0;
	while ( i < l ) {

		wbuffer->buffer[ wbuffer->pos +i ]= keyword[i];
		i++;
	}

	wbuffer->pos += l;

	return l;
}


uint32_t OTF_WBuffer_writeString( OTF_WBuffer* wbuffer, const char* string ) {


	uint32_t i;
	uint32_t l= (uint32_t) (( NULL != string ) ? strlen( string ) : 0);
	
	if( 0 == OTF_WBuffer_guarantee( wbuffer, l+2 ) ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_WBuffer_guarantee() failed.\n",
				__FUNCTION__, __FILE__, __LINE__  );
#		endif

		return 0;
	}


	wbuffer->buffer[ wbuffer->pos ] = '"';
	++wbuffer->pos;

	i = 0;
	while ( i < l ) {

		if ( ( '\n' != string[i] ) && ( '"' != string[i] ) ) {
	
			wbuffer->buffer[ wbuffer->pos +i ]= string[i];

		} else {

			wbuffer->buffer[ wbuffer->pos +i ]= ' ';
		}

		i++;
	}

	wbuffer->pos += l;

	wbuffer->buffer[ wbuffer->pos ] = '"';
	++wbuffer->pos;

	return l;
}


uint32_t OTF_WBuffer_writeChar( OTF_WBuffer* wbuffer, const char character ) {


	if( 0 == OTF_WBuffer_guarantee( wbuffer, 1 ) ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_WBuffer_guarantee() failed.\n",
				__FUNCTION__, __FILE__, __LINE__  );
#		endif

		return 0;
	}


	wbuffer->buffer[ wbuffer->pos ]= character;

	wbuffer->pos++;

	return 1;
}


uint32_t OTF_WBuffer_writeUint32( OTF_WBuffer* wbuffer, uint32_t value ) {


	static char dig[16] = {	'0', '1', '2', '3', '4', '5', '6', '7',
		'8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };

	uint32_t l= 0;
	int s= 28;	
	uint32_t v= 0;
	char* p = NULL;

	/* at max 8 digits will be written */
	if( 0 == OTF_WBuffer_guarantee( wbuffer, 8 ) ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_WBuffer_guarantee() failed.\n",
				__FUNCTION__, __FILE__, __LINE__  );
#		endif

		return 0;
	}


	p= wbuffer->buffer + wbuffer->pos;

	/* skip leading zeros */
	while ( ( 0 == v ) && ( s >= 0 ) ) {
	
		v= ( value >> s ) & 0xf ;
		s -= 4;
	}

	p[l++]= dig[v];

	while ( s >= 0 ) {
	
		v= ( value >> s ) & 0xf ;
		s -= 4;

		p[l++]= dig[v];
	}

	wbuffer->pos += l;

	return l;
}


uint32_t OTF_WBuffer_writeUint64( OTF_WBuffer* wbuffer, uint64_t value ) {


	static char dig[16] = {	'0', '1', '2', '3', '4', '5', '6', '7',
		'8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };

	uint32_t l= 0;
	int s= 60;	
	uint32_t v= 0;
	char* p = NULL;

	/* at max 16 digits will be written */
	if( 0 == OTF_WBuffer_guarantee( wbuffer, 16 ) ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_WBuffer_guarantee() failed.\n",
				__FUNCTION__, __FILE__, __LINE__  );
#		endif

		return 0;
	}


	p= wbuffer->buffer + wbuffer->pos;

	/* skip leading zeros */
	while ( ( 0 == v ) && ( s >= 0 ) ) {

		v= (uint32_t) (( value >> s ) & 0xf) ;
		s -= 4;
	}

	p[l++]= dig[v];
	
	while ( s >= 0 ) {
	
		v= (uint32_t) (( value >> s ) & 0xf) ;
		s -= 4;

		p[l++]= dig[v];
	}

	wbuffer->pos += l;

	return l;
}


uint32_t OTF_WBuffer_writeNewline( OTF_WBuffer* wbuffer ) {


	if( 0 == OTF_WBuffer_guarantee( wbuffer, 1 ) ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_WBuffer_guarantee() failed.\n",
				__FUNCTION__, __FILE__, __LINE__  );
#		endif

		return 0;
	}


	wbuffer->buffer[ wbuffer->pos ] = '\n';
	++wbuffer->pos;

	return 1;
}


OTF_WBuffer* OTF_WBuffer_open_zlevel( const char* filename,
	OTF_FileManager* manager, OTF_FileCompression compression ) {



	OTF_WBuffer* ret= (OTF_WBuffer*) malloc( sizeof(OTF_WBuffer) );
	if( NULL == ret ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif

		return NULL;
	}

	OTF_WBuffer_init( ret );

	if( NULL == manager ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif

		free( ret );
		ret= NULL;

		return NULL;
	}
	ret->file= OTF_File_open_zlevel( filename, manager, OTF_FILEMODE_WRITE, compression );
	if( NULL == ret->file ) {
		
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_open() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif

		free( ret );
		ret= NULL;

		return NULL;
	}

#ifdef HAVE_ZLIB
	OTF_File_setZBufferSize( ret->file, ret->zbuffersize );
#endif /* HAVE_ZLIB */

	return ret;

}
