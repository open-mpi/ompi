/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/* macros to enable 64 bit file access. make sure all std headers are 
included AFTER this macro definitions */

/* config.h handles this now: #define _LARGEFILE_SOURCE */

#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE 
#define _LARGE_FILES


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_Platform.h"
#include "OTF_inttypes.h"


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
	#include <unistd.h>
#endif

#ifdef HAVE_IO_H
	#include <io.h>
#endif


/* vs does not know F_OK*/
#ifndef F_OK
	#define F_OK 00
#endif

#ifdef HAVE_ZLIB

#include <zlib.h>


#endif /* HAVE_ZLIB */

#include "OTF_File.h"
#include "OTF_Platform.h"
#include "OTF_Definitions.h"
#include "OTF_Errno.h"


struct struct_OTF_File {

	/** own copy of filename */
	char* filename;

	/** actual file handle, it is NULL if file is currently closed, 
	!= NULL otherwise */
	FILE* file;

#ifdef HAVE_ZLIB

	/** zlib object */
	z_stream* z;

	/** zlib entry buffer ... what a nice wordplay */
	unsigned char* zbuffer;
	
	unsigned char* ybuffer;

	uint32_t zbuffersize;

#endif /* HAVE_ZLIB */

	/** keep file pos when the real file is closed, 
	undefined while file is open, == 0 before opened for the first time */
	uint64_t pos;
	
	OTF_FileMode mode;

	OTF_FileManager* manager;
};


void OTF_File_init( OTF_File* file ) {


	file->filename= NULL;
	file->file= NULL;
#ifdef HAVE_ZLIB
	file->z= NULL;
	file->zbuffer= NULL;
	file->ybuffer= NULL;
	file->zbuffersize= 1024*10;
#endif /* HAVE_ZLIB */
	file->pos= 0;
	file->mode= OTF_FILEMODE_NOTHING;
	file->manager= NULL;
}


void OTF_File_finalize( OTF_File* file ) {


	file->filename= NULL;
	file->file= NULL;
#ifdef HAVE_ZLIB
	file->z= NULL;
	file->zbuffer= NULL;
	file->ybuffer= NULL;
	file->zbuffersize= 0;
#endif /* HAVE_ZLIB */
	file->pos= 0;
	file->mode= OTF_FILEMODE_NOTHING;
	file->manager= NULL;
}


OTF_File* OTF_File_open( const char* filename, 
	OTF_FileManager* manager, OTF_FileMode mode ) {


	return OTF_File_open_zlevel( filename, manager, mode, OTF_FILECOMPRESSION_COMPRESSED );
}


size_t OTF_File_write( OTF_File* file, const void* ptr, size_t size ) {


	size_t byteswritten;

#ifdef HAVE_ZLIB
	int len = 0;
	int rest = (int) size;
	int status;
#endif/* HAVE_ZLIB */
	
	if( OTF_FILEMODE_WRITE != file->mode ) {
		
		OTF_fprintf ( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"current file->mode is not OTF_FILEMODE_WRITE. writing forbidden.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}


	/*
	OTF_fprintf( stderr, "OTF_File_write: %u / %u file handles\n", 
		OTF_FileManager_getCount( file->manager ), 
		OTF_FileManager_getNumber( file->manager ) );
	*/
	
	if( 0 == OTF_File_revive( file, OTF_FILEMODE_WRITE ) ) {
		
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_revive() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

#ifdef HAVE_ZLIB

	if ( NULL != file->z ) {
	
		/* step 1 */
		/* is any data in the y-buffer */
		if ( 0 < file->z->avail_in ) {
			
			/* len of the piece to fill the y buffer (to 10Kbyte) */
			len = file->zbuffersize - file->z->avail_in;
			
			/* is enough data in the "*ptr" to fill the ybuffer fully */
			if ( len <= rest ) {
			
				memcpy( file->ybuffer + file->z->avail_in, ptr, len );
				file->z->avail_in = file->zbuffersize;
				file->z->next_in = file->ybuffer;
				file->z->avail_out = file->zbuffersize;
				file->z->next_out = file->zbuffer;
				
				status = deflate( file->z, Z_FULL_FLUSH );
				if ( status != Z_OK ) {
			
					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
							"error in compressing, status %u.\n",
							__FUNCTION__, __FILE__, __LINE__, status );
					
					return 0;
				}
				
				byteswritten= fwrite( file->zbuffer, 1, file->zbuffersize - file->z->avail_out, file->file );
				if( byteswritten < (file->zbuffersize - file->z->avail_out) ) {

					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
							"less bytes written than expected %u < %u.\n",
							__FUNCTION__, __FILE__, __LINE__, (uint32_t) byteswritten,
							(uint32_t) (file->zbuffersize - file->z->avail_out) );

				}

				/* test if avail_in really ran empty */
				if ( 0 < file->z->avail_in ) {

					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
							"error in compressing.\n",
							__FUNCTION__, __FILE__, __LINE__ );

					return 0;
				}

				rest -= len;

			} else {

				/* no, it is not */
			
				/* only copy the new data into the ybuffer */
				memcpy( file->ybuffer + file->z->avail_in, ptr, rest );
				file->z->avail_in += rest;
				rest = 0;
			}
		}
		
		
		/* step 2 */
		/* if theres more than 10k in the "*ptr" */
		while( (uint32_t) rest >= file->zbuffersize ) {
		
			file->z->avail_in = file->zbuffersize;
			file->z->next_in = ( ( ( unsigned char* ) ptr ) + len );
			file->z->avail_out = file->zbuffersize;
			file->z->next_out = file->zbuffer;
			
			rest -= file->zbuffersize;
			len += file->zbuffersize;
			
			status = deflate( file->z, Z_FULL_FLUSH );
			if ( status != Z_OK ) {
		
				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"error in compressing, status %u.\n",
						__FUNCTION__, __FILE__, __LINE__, status );

				return 0;
			}
			
			byteswritten= fwrite( file->zbuffer, 1, file->zbuffersize - file->z->avail_out,
				file->file );
			if( byteswritten < (file->zbuffersize - file->z->avail_out) ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"less bytes written than expected %u < %u.\n",
						__FUNCTION__, __FILE__, __LINE__, (uint32_t) byteswritten,
						(uint32_t) (file->zbuffersize - file->z->avail_out) );

			}

			/* test if avail_in really ran empty */
			if ( 0 < file->z->avail_in ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"error in compressing.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				return 0;
			}
		}
		
		
		/* step 3 */
		/* is there less than 10k data left ... throw it into the ybuffer */
		if ( rest > 0 ) {
		
			memcpy( file->ybuffer, ( ( unsigned char* ) ptr ) + len, rest );
			file->z->avail_in = rest;
		}
		
		return size;
		
	} else {

#endif /* HAVE_ZLIB */

		/*
		OTF_fprintf( stderr, "OTF_File_write(): buffer %p, size %u file %p\n", ptr, 
			(uint32_t) size, file->file );
		*/
		
		byteswritten= fwrite( ptr, 1, size, file->file );
		if( byteswritten < size ) {

			OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"less bytes written than expected %u < %u.\n",
					__FUNCTION__, __FILE__, __LINE__, (uint32_t) byteswritten,
					(uint32_t) size );

		}

		return byteswritten;

#ifdef HAVE_ZLIB
	}
#endif /* HAVE_ZLIB */

}


size_t OTF_File_read( OTF_File* file, void* ptr, size_t size ) {


#ifdef HAVE_ZLIB
	/* size_t read; */
	int status;
#endif /* HAVE_ZLIB */


	if( OTF_FILEMODE_WRITE == file->mode ) {
		
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"current file->mode is OTF_FILEMODE_WRITE. reading forbidden.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}
	
	if( 0 == OTF_File_revive( file, OTF_FILEMODE_READ ) ) {
		
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_revive() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

#ifdef HAVE_ZLIB

	if ( NULL != file->z ) {

		file->z->next_out= ptr;
		file->z->avail_out= (uInt) size;

		while ( 0 < file->z->avail_out ) {

			if ( 0 == file->z->avail_in ) {

		
				file->z->avail_in= (uInt) fread( file->zbuffer, 1, file->zbuffersize, file->file );
				file->z->next_in= file->zbuffer;
			}

			if ( 0 == file->z->avail_in ) {

				break;
			}

			status = inflate( file->z, Z_SYNC_FLUSH );
			if ( status != Z_OK ) {
		
				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"error in uncompressing, status %u.\n",
						__FUNCTION__, __FILE__, __LINE__, status );
				
				return 0;
			}
		}

		return size - file->z->avail_out;

	} else {

		return fread( ptr, 1, size, file->file );
	}

#else /* HAVE_ZLIB */

	return fread( ptr, 1, size, file->file );

#endif /* HAVE_ZLIB */
}


int OTF_File_seek( OTF_File* file, uint64_t pos ) {


	int ret;

#ifdef HAVE_ZLIB
	int sync;
	uint64_t read;
#endif /* HAVE_ZLIB */

	if( OTF_FILEMODE_WRITE == file->mode ) {
		
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"current file->mode is OTF_FILEMODE_WRITE. seeking forbidden.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return -1;
	}
		
	
	if( 0 == OTF_File_revive( file, OTF_FILEMODE_SEEK ) ) {

		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_revive() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return -1;
	}
	

	ret= fseeko( file->file, pos, SEEK_SET );
	
#ifdef HAVE_ZLIB

	if ( NULL != file->z && 0 == ret ) {

		do {

			/*
			OTF_fprintf( stderr, "OTF_File_seek() with zlib: jump to %llu\n", 
					(unsigned long long) pos );
			*/

			read= fread( file->zbuffer, 1, file->zbuffersize, file->file );

			/*
			OTF_fprintf( stderr, "OTF_File_seek() with zlib: read %llu bytes\n", 
					(unsigned long long) read );
			*/

			file->z->next_in= file->zbuffer;
			file->z->avail_in= (uInt) read;
			file->z->total_in= 0;

			/* re-initialize z object */
			inflateEnd( file->z );
			inflateInit( file->z );

			/* do not sync at very beginning of compressed stream because it 
			would skip the first block */
			sync= Z_OK;
			if ( 0 != pos ) {

				sync= inflateSync( file->z );
			}

			if ( Z_OK == sync ) {

				return ret;
			}

			if ( Z_BUF_ERROR == sync ) {
			
				continue;
			}
			
			if ( Z_DATA_ERROR == sync ) {
			
				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"Z_DATA_ERROR.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return -1;
			}

			if ( Z_STREAM_ERROR == sync ) {
			
				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"Z_STREAM_ERROR.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return -1;
			}

		} while ( 1 );
	}

#endif /* HAVE_ZLIB */

	return ret;
}


uint64_t OTF_File_tell( OTF_File* file ) {


	if ( NULL != file->file ) {

		file->pos= ftello( file->file );
	}

	return file->pos;
}


uint64_t OTF_File_size( OTF_File* file ) {


	struct stat st;

	if ( stat( file->filename, &st ) == -1 ) {

		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"stat() failed: %s\n",
						__FUNCTION__, __FILE__, __LINE__,
						strerror(errno) );

		return 0;
	} else {

		return st.st_size;

	}
}


int OTF_File_close( OTF_File* file ) {


#ifdef HAVE_ZLIB
	size_t byteswritten;
	int status;
#endif /* HAVE_ZLIB */

	
	if ( NULL == file ) {
			
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"file has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
				
		return 0;
	}


#ifdef HAVE_ZLIB

	if ( NULL != file->z ) {

		if ( OTF_FILEMODE_WRITE != file->mode ) {

			inflateEnd( file->z );

		} else {
		
			if ( file->z->avail_in > 0 ) {
			
				file->z->next_in = file->ybuffer;
				file->z->next_out = file->zbuffer;
				file->z->avail_out = file->zbuffersize;
				
				status = deflate( file->z, Z_FULL_FLUSH );
				if ( status != Z_OK ) {
			
					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
							"error in uncompressing, status %u.\n",
							__FUNCTION__, __FILE__, __LINE__, status );
					
					return 0;
				}
				
				if( 0 == OTF_File_revive( file, OTF_FILEMODE_WRITE ) ) {

					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
							"OTF_File_revive() failed.\n",
							__FUNCTION__, __FILE__, __LINE__ );
					
					return 0;
				}
					
				byteswritten= fwrite( file->zbuffer, 1, file->zbuffersize -
					file->z->avail_out, file->file );
				if( byteswritten < (file->zbuffersize - file->z->avail_out) ) {

					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"less bytes written than expected %u < %u.\n",
						__FUNCTION__, __FILE__, __LINE__, (uint32_t) byteswritten,
						(uint32_t) (file->zbuffersize - file->z->avail_out) );

				}
			}
			
			deflateEnd( file->z );
		}

		free( file->z );
		
		free( file->ybuffer );

		free( file->zbuffer );
	}
	
#endif /* HAVE_ZLIB */

	if ( NULL != file->file ) {

		OTF_FileManager_suspendFile( file->manager, file );
	}

	free( file->filename );
	
	OTF_File_finalize( file );

	free( file );
	file = NULL;
	
	return 1;
}


OTF_FileStatus OTF_File_status( OTF_File* file ) {


	if ( NULL == file->file ) {

		if ( 0 == file->pos ) {

			return OTF_FILESTATUS_CLOSED;

		} else {

			return OTF_FILESTATUS_SUSPENDED;
		}
	}

	return OTF_FILESTATUS_ACTIVE;
}


void OTF_File_suspend( OTF_File* file ) {


	/* get status and close OS file */

	file->pos= ftello( file->file );
	fclose( file->file );
	file->file= NULL;
}


int OTF_File_revive( OTF_File* file, OTF_FileMode mode  ) {


	switch ( mode ) {

	case OTF_FILEMODE_READ :
	
		/* *** read *** */

		if ( NULL == file->file ) {

			/* file currently closed */

			/* 
			OTF_fprintf( stderr, "OTF_File_revive() READ: ask FileManager for free handle\n" );
			*/
			if ( 0 == OTF_FileManager_guaranteeFile( file->manager ) ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_guaranteeFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}

			if ( 0 != file->pos ) {

				/* re-open */

				file->file= fopen( file->filename, "rb" );
				if( NULL == file->file ) {
				
					/* show this error every time */
					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for reading. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );

					return 0;
				}

				fseeko( file->file, file->pos, SEEK_SET );

			} else {

				/* open first time */

				file->file= fopen( file->filename, "rb" );
				if( NULL == file->file ) {

					/* show this error every time */
					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for reading. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );

					return 0;
				}
			}

			
			if ( 0 == OTF_FileManager_registerFile( file->manager, file ) ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_registerFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}

		} else {

			/* file already opened */
			/*
			OTF_fprintf( stderr, "OTF_File_revive() READ: update FileManagers LRU list\n" );
			*/
			if ( 0 ==  OTF_FileManager_touchFile( file->manager, file ) ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_touchFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}
		}

		return 1;

	case OTF_FILEMODE_WRITE :

		/* *** write *** */

		if ( NULL == file->file ) {

			/* file currently closed */

			/*
			OTF_fprintf( stderr, "OTF_File_revive() WRITE: ask FileManager for free handle\n" );
			*/
			if ( 0 == OTF_FileManager_guaranteeFile( file->manager ) ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_guaranteeFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}

			if ( 0 != file->pos ) {

				/* re-open */

				file->file= fopen( file->filename, "ab" );
				if( NULL == file->file ) {
					
					/* show this error every time */
					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for writing. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );
				
					return 0;
				}

			} else {

				/* open first time */

				file->file= fopen( file->filename, "wb" );
				if( NULL == file->file ) {
					
					/* show this error every time */
					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for writing. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );
				
					return 0;
				}
			}

			/*
			OTF_fprintf( stderr, "OTF_File_revive() WRITE: register opened file with FileManager\n" );
			*/
			if ( 0 == OTF_FileManager_registerFile( file->manager, file ) ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_registerFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}

		} else {

			/* file already opened */
			/*
			OTF_fprintf( stderr, "OTF_File_revive() WRITE: update FileManagers LRU list\n" );
			*/
			if ( 0 ==  OTF_FileManager_touchFile( file->manager, file ) ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_touchFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}
		}

		return 1;

	case OTF_FILEMODE_SEEK :
	
		/* *** seek *** */

		if ( NULL == file->file ) {

			/* file currently closed */

			/*
			OTF_fprintf( stderr, "OTF_File_revive() READ: ask FileManager for free handle\n" );
			*/
			if ( 0 == OTF_FileManager_guaranteeFile( file->manager ) ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_guaranteeFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				return 0;
			}

			if ( 0 != file->pos ) {

				/* re-open */

				file->file= fopen( file->filename, "rb" );
				if( NULL == file->file ) {
					
					/* show this error every time */
					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for reading. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );
				
					return 0;
				}

				/* dont need to seek to the saved position because there 
				will be another seek anyway*/
				/*
				fseeko( file->file, file->pos, SEEK_SET );
				*/

			} else {

				/* open first time */

				file->file= fopen( file->filename, "rb" );
				if( NULL == file->file ) {
					
					/* show this error every time */
					OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for reading. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );
				
					return 0;
				}
			}

			/*
			OTF_fprintf( stderr, "OTF_File_revive() SEEK: register opened file with FileManager\n" );
			*/
			if ( 0 == OTF_FileManager_registerFile( file->manager, file ) ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_registerFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}

		} else {

			/* file already opened */
			/*
			OTF_fprintf( stderr, "OTF_File_revive() READ: update FileManagers LRU list\n" );
			*/
			if ( 0 ==  OTF_FileManager_touchFile( file->manager, file ) ) {

				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_touchFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}
		}

		return 1;


	default:

		/* *** unknown mode *** */

		return 0;
	}
}


void OTF_File_setZBufferSize( OTF_File* file, uint32_t size ) {


#ifdef HAVE_ZLIB
	
	if( NULL != file->z ) {
	
		if ( 32 > size ) {
		
			OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"intended zbuffer size %u is too small, rejected.\n",
					__FUNCTION__, __FILE__, __LINE__, size );
			
			return;
	
		} else if ( 512 > size ) {
		
			OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"zbuffer size %u is very small, accepted though.\n",
					__FUNCTION__, __FILE__, __LINE__, size );
	
		} else if ( 10 * 1024 *1024 < size ) {
	
			OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"zbuffer size %u is rather big, accepted though.\n",
					__FUNCTION__, __FILE__, __LINE__, size );

		}
	
		file->zbuffersize= size;
	
		if( NULL != file->zbuffer ) {
			free( file->zbuffer );
		}
		file->zbuffer= malloc( size );
		assert( file->zbuffer );
		
		if( NULL != file->ybuffer ) {
			free( file->ybuffer );
		}
		file->ybuffer= malloc( size );
		assert( file->ybuffer );
	
	
	}

#endif /* HAVE_ZLIB */
}


OTF_File* OTF_File_open_zlevel( const char* filename, OTF_FileManager* manager,
	OTF_FileMode mode, OTF_FileCompression zlevel ) {


	uint32_t len;


	/* OTF_fprintf( stderr, "OTF_File_open_zlevel() zlevel: %u, filename: \"%s\"\n", zlevel, filename ); */

	OTF_File* ret= (OTF_File*) malloc( sizeof(OTF_File) );
	if( NULL == ret ) {

		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	OTF_File_init( ret );

	if( NULL == filename ) {

		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"no filename has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		free( ret );
		ret= NULL;

		return NULL;
	}
	
	len= (uint32_t) strlen( filename );
	ret->filename= malloc( len +3 );
	if( NULL == ret->filename ) {

		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		free( ret );
		ret= NULL;
		
		return NULL;
	}
	
	strncpy( ret->filename, filename, len +1 );
	
	ret->mode = mode;

	if ( OTF_FILEMODE_READ == mode || OTF_FILEMODE_SEEK == mode ) {

#ifdef HAVE_ZLIB

		if ( 0 != access( ret->filename, F_OK ) ) {

			/* file not found, try '.z' suffix */

			strncpy( ret->filename +len, ".z", 3 );

			/*
			OTF_fprintf( stderr, "try '%s'\n", ret->filename );
			*/

			if ( 0 != access( ret->filename, F_OK ) ) {

				/* file still not found, give up */
				free( ret->filename );
				ret->filename= NULL;
				free( ret );
				ret= NULL;

				return ret;
			}

			ret->z= malloc( sizeof(z_stream) );
			if( NULL == ret->z ) {
		
				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				free( ret->filename );
				ret->filename= NULL;
				free( ret );
				ret= NULL;
				
				return NULL;
			}

			ret->z->next_in= NULL;
			ret->z->avail_in= 0;
			ret->z->zalloc= NULL;
			ret->z->zfree= NULL;
			ret->z->opaque= NULL;

			inflateInit( ret->z );

			ret->zbuffer= malloc( ret->zbuffersize );
			ret->ybuffer= malloc( ret->zbuffersize );
			if( NULL == ret->zbuffer || NULL == ret->ybuffer) {
		
				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				free( ret->z );
				ret->z= NULL;
				free( ret->filename );
				ret->filename= NULL;
				free( ret );
				ret= NULL;

				return NULL;
			}
		}

#else /* HAVE_ZLIB */

		if ( 0 != access( ret->filename, F_OK ) ) {

			/* file still not found, give up */
			free( ret->filename );
			ret->filename= NULL;
			free( ret );
			ret= NULL;

			return ret;
		}

#endif /* HAVE_ZLIB */

	} else {

		/* filemode write */

#ifdef HAVE_ZLIB

		/* is a .z appended to the file name */
		if ( len > 2 && 0 == strcmp( ret->filename + len - 2, ".z" ) ) {
		
			ret->z= malloc( sizeof(z_stream) );
			if( NULL == ret->z ) {
		
				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				free( ret->filename );
				ret->filename= NULL;
				free( ret );
				ret= NULL;
				
				return NULL;
			}

			ret->z->next_in= NULL;
			ret->z->avail_in= 0;
			ret->z->zalloc= NULL;
			ret->z->zfree= NULL;
			ret->z->opaque= NULL;

			deflateInit( ret->z, zlevel );

			ret->zbuffer= malloc( ret->zbuffersize );
			ret->ybuffer= malloc( ret->zbuffersize );
			if( NULL == ret->zbuffer || NULL == ret->ybuffer ) {
		
				OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				free( ret->z );
				ret->z= NULL;
				free( ret->filename );
				ret->filename= NULL;
				free( ret );
				ret= NULL;
				
				return NULL;
			}
		}
#endif /* HAVE_ZLIB */

	}

	if( NULL == manager ) {
		
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		
#		ifdef HAVE_ZLIB
			free( ret->zbuffer );
			ret->zbuffer= NULL;
			free( ret->ybuffer );
			ret->ybuffer= NULL;
			free( ret->z );
			ret->z= NULL;
#		endif /* HAVE_ZLIB */
		free( ret->filename );
		ret->filename= NULL;
		free( ret );
		ret= NULL;
		
		return NULL;
	}
	ret->manager= manager;

	return ret;
}
