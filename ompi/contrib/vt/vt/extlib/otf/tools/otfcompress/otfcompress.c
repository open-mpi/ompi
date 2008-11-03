/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "config.h"


#define _LARGEFILE64_SOURCE 
#define _LARGEFILE64_SOURCE 
#define _LARGE_FILES
#define _FILE_OFFSET_BITS 64

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#ifdef HAVE_SYS_TIME_H
	#include <sys/time.h>
#else
	#include "sys_time.h"
#endif


#ifdef HAVE_ZLIB
	#include <zlib.h>
#endif /* HAVE_ZLIB */

#ifdef HAVE_UNISTD_H
	#include <unistd.h>
#endif

#include "OTF_inttypes.h"
#include "OTF_Platform.h"
#include "OTF_Definitions.h"

#define SHOW_HELPTEXT { \
	int l = 0; while( Helptext[l] ) { printf( "%s", Helptext[l++] ); } }

static const char* Helptext[] = {
"                                                                      \n",
" otf(de)compress - compression program for single OTF files.          \n",
"                                                                      \n",
"    Usage: otf(de)compress [OPTIONS] <FILES>                          \n",
"                                                                      \n",
"      -h, --help     show this help message                           \n",
"      -V     show OTF version                                         \n",
"      -c     compress (default action when called as 'otfcompress')   \n",
"      -d     decompress (default action if called as 'otfdecompress') \n",
"      -h     show this help message                                   \n",
"      -k     keep original file (compressed resp. uncompressed)       \n",
"      -[0-9] use given compression level (default 4)                  \n",
"                0 - plain                                             \n",
"                1 - minimum compression, fastest                      \n",
"                9 - maximum compression, slowest                      \n",
"                                                                      \n", NULL };

#ifdef HAVE_ZLIB


int compressFile( const char* filename, const char* outfilename, 
	uint32_t level, uint32_t blocksize );

int decompressFile( const char* filename, const char* outfilename, 
	uint32_t blocksize );


#define ZLEVEL	4
#define BUFFERSIZE (10*1024)

#define MODE_DEFAULT 0
#define MODE_COMPRESS 1
#define MODE_DECOMPRESS 2


int main ( int argc, const char** argv ) {


	int ret;
	int i;

	uint32_t mode= MODE_DEFAULT;
	const char* command= NULL;
	const char* p;

	uint32_t len;
	const char* infilename;
	char* outfilename= NULL;

	int keep= 0;
	int zlevel= ZLEVEL;


	if ( 1 >= argc ) {

			SHOW_HELPTEXT;
			return 0;
	}
	

	/* collect parameters */
	for ( i= 1; i < argc; i++ ) {

		if ( 0 == strcmp( "-h", argv[i] ) || 0 == strcmp( "--help", argv[i] ) ) {

			SHOW_HELPTEXT;
			exit( 0 );

		} else 

		if ( 0 == strcmp( "-k", argv[i] ) ) {

			keep= 1;

		} else 

		if ( 0 == strcmp( "-c", argv[i] ) ) {

			if ( MODE_DEFAULT == mode ) {

				mode= MODE_COMPRESS;

			} else {

				fprintf( stderr, "mode already set, cannot re-set, ignoring '-c'\n" );
			}

		} else 

		if ( 0 == strcmp( "-d", argv[i] ) ) {

			if ( MODE_DEFAULT == mode ) {

				mode= MODE_DECOMPRESS;

			} else {

				fprintf( stderr, "mode already set, cannot re-set, ignoring '-d'\n" );
			}

		} else 

		if ( ( '-' == argv[i][0] ) && ( '0' <= argv[i][1] ) && ( '9' >= argv[i][1] ) )  {

			zlevel= (int) ( argv[i][1] - '0' );

		} else 
		
		if ( 0 == strcmp( "-V", argv[i] ) ) {
		
			printf( "%u.%u.%u \"%s\"\n", OTF_VERSION_MAYOR, OTF_VERSION_MINOR,
				OTF_VERSION_SUB, OTF_VERSION_STRING);
			exit( 0 );
		}
	}

	/* look at argv[0], mode defaults to compress/decomress according to command name */
	if ( MODE_DEFAULT == mode ) {
	
		p= strrchr( argv[0], '/' );
		command = NULL != p ? p+1 : argv[0];

		if ( 0 == strcmp( "otfcompress", command ) ) {

			mode= MODE_COMPRESS;
		}

		if ( 0 == strcmp( "otfdecompress", command ) ) {

			mode= MODE_DECOMPRESS;
		}

		if ( 0 == strcmp( "otfuncompress", command ) ) {

			mode= MODE_DECOMPRESS;
		}
	}


	/* files to (de)compress */
	for ( i= 1; i < argc; i++ ) {

		if ( '-' == argv[i][0] ) {

			/* switches already handled */

		} else {
		
			/* assume argument is a file name */

			switch ( mode ) {

			case MODE_DECOMPRESS:

				/* decompress file */
				infilename= argv[i];
				len= (uint32_t) strlen( infilename );
				outfilename= realloc( outfilename, len +1 );
				assert( NULL != outfilename );

				/* built outfilename */
				strncpy( outfilename, infilename, len +1 );

				/* find and remove ".z" at the end */
				if ( ( 2 >= len ) || ( 0 != strcmp( ".z", outfilename +len -2 ) ) ) {
				
					fprintf( stderr, "ERROR: no trailing '.z' in filename '%s', rejecting\n", 
						infilename );

					continue;
				}

				outfilename[len -2]= '\0';

				fprintf( stdout, "decompress \"%s\" -> \"%s\"\n", 
					infilename, outfilename );

				ret= decompressFile( infilename, outfilename, BUFFERSIZE );

				if ( 0 == ret ) {

					if ( keep ) {

						/* keep compressed file */

					} else {

						/* remove compressed file */
						ret= unlink( infilename );

						if ( 0 != ret ) {

							fprintf( stderr, "error removing '%s'\n", infilename );
						}
					}

				} else {

					fprintf( stderr, "decompress error for file '%s'\n", infilename );
				} 

				break;

			case MODE_COMPRESS:
			case MODE_DEFAULT:
			default:

				/* compress file */
				infilename= argv[i];
				len= (uint32_t) strlen( infilename );

				/* check for ".z" at the end and refuse compression if found */
				if ( ( 2 < len ) && ( 0 == strcmp( ".z", infilename +len -2 ) ) ) {
				
					fprintf( stderr, "ERROR: found '.z' suffix in filename, "
						"file '%s' seems already compressed, skip\n", 
						infilename );

					continue;
				}

				outfilename= realloc( outfilename, len +3 );
				assert( NULL != outfilename );

				/* built outfilename */
				strncpy( outfilename, infilename, len +1 );
				strncpy( outfilename +len, ".z", 3 );
				
				fprintf( stdout, "compress \"%s\" -> \"%s\"\n", 
					infilename, outfilename );

				ret= compressFile( infilename, outfilename, zlevel, BUFFERSIZE );

				if ( 0 == ret ) {

					if ( keep ) {

						/* rename original file */

						outfilename= realloc( outfilename, len +3 );
						assert( NULL != outfilename );

						/* built outfilename */
						strncpy( outfilename, infilename, len +1 );
						strncpy( outfilename +len, ".original", 10 );

						ret= rename( infilename, outfilename );
						
						if ( 0 != ret ) {

							fprintf( stderr, "error renaming '%s' to '%s'\n", 
								infilename, outfilename );
						}
						
					} else {

						/* remove uncompressed */

						ret= unlink( infilename );

						if ( 0 != ret ) {

							fprintf( stderr, "error removing '%s'\n", infilename );
						}
					}

				} else {

					fprintf( stderr, "compress error for file '%s'\n", infilename );
				}

				break;
			}
		}
	}

	free( outfilename );
	outfilename= NULL;
	infilename= NULL;

	return 0;
}



int compressFile( const char* filename, const char* outfilename, 
		uint32_t level, uint32_t blocksize ) {


	z_stream z;
	int status;

	unsigned char* inbuf;
	unsigned char* outbuf;
	FILE *fin;
	FILE *fout;


	uint64_t totalin= 0;
	uint64_t totalout= 0;
	uint64_t read;
	uint64_t write;

	struct timeval start;
	struct timeval stop;
	double time;


	inbuf= (unsigned char*) malloc( blocksize * sizeof(unsigned char) );
	assert( NULL != inbuf );
	outbuf= (unsigned char*) malloc( blocksize * sizeof(unsigned char) );
	assert( NULL != outbuf );

	fin= fopen( filename, "rb" );
	if ( NULL == fin ) {
	
		fprintf( stderr, "cannot open file '%s' for reading, skip\n", filename );
		free( inbuf );
		free( outbuf );
		return 1;
	}
	fout= fopen( outfilename, "wb" );
	if ( NULL == fout ) {
	
		fprintf( stderr, "cannot open file '%s' for writing, skip\n", outfilename );
		fclose( fin );
		free( inbuf );
		free( outbuf );
		return 1;
	}

	z.zalloc= NULL;
	z.zfree= NULL;
	z.opaque= NULL;

	deflateInit( &z, level );

	z.avail_in= 0;
	z.next_out= outbuf;
	z.avail_out= blocksize;

	gettimeofday( &start, NULL );

	while ( 1 ) {

		/* is no more data in the buffer? */
		if ( 0 == z.avail_in ) {

			/* get new data */
			z.next_in= inbuf;
			read= fread( inbuf, sizeof(char), blocksize, fin );

			if ( 0 == read ) {

				break;
			}

			totalin += read;
			z.avail_in= (uInt) read;
		}

		/* deflate the buffer */
		status= deflate( &z, Z_FULL_FLUSH );
		write= blocksize - z.avail_out;

		if ( Z_OK != status ) {
		
			fprintf( stderr, "    status= %u\n", status );
		}

		write= blocksize - z.avail_out;
		totalout += write;

		/* write to output if there is any data */
		if ( write > 0 ) {

			fwrite( outbuf, sizeof(char), (size_t) write, fout );
		}
/*
		gettimeofday( &stop, NULL );
		time= ((double) ( stop.tv_sec - start.tv_sec ) ) + 
			0.000001 * ((double) ( stop.tv_usec - start.tv_usec ) );
		fprintf( stderr, " %llu / %llu %6.2fs\n", 
			totalout, totalin, time );
*/
		z.next_out= outbuf;
		z.avail_out= blocksize;
	}

	gettimeofday( &stop, NULL );
	time= ((double) ( stop.tv_sec - start.tv_sec ) ) + 
			0.000001 * ((double) ( stop.tv_usec - start.tv_usec ) );

	fprintf( stdout, "\tlevel= %u, buf= %ub : %llub / %llub = %6.2f%%, time= %5.3fs\n", 
		level, blocksize, 
		(unsigned long long) totalout, 
		(unsigned long long) totalin, 
		100.0 * ((double) totalout) / ((double) totalin), 
		time );

	deflateEnd( &z );
	fclose( fin );
	fclose( fout );

	free( inbuf );
	free( outbuf );

	return 0;
}


int decompressFile( const char* infilename, const char* outfilename, uint32_t blocksize ) {


	z_stream z;
	int status;

	unsigned char* inbuf;
	unsigned char* outbuf;
	FILE *fin;
	FILE *fout;

	uint64_t totalin= 0;
	uint64_t totalout= 0;
	uint64_t read;
	uint64_t write;

	struct timeval start;
	struct timeval stop;
	double time;


	inbuf= (unsigned char*) malloc( blocksize * sizeof(unsigned char) );
	assert( NULL != inbuf );
	outbuf= (unsigned char*) malloc( blocksize * sizeof(unsigned char) );
	assert( NULL != outbuf );

	fin= fopen( infilename, "rb" );
	if ( NULL == fin ) {
	
		fprintf( stderr, "cannot open file '%s' for reading, skip\n", infilename );
		free( inbuf );
		free( outbuf );
		return 1;
	}
	fout= fopen( outfilename, "wb" );
	if ( NULL == fout ) {
	
		fprintf( stderr, "cannot open file '%s' for writing, skip\n", outfilename );
		fclose( fin );
		free( inbuf );
		free( outbuf );
		return 1;
	}

	z.zalloc = NULL;
	z.zfree = NULL;
	z.opaque = NULL;
	
	inflateInit( &z );
	
	/* initial setup */
	z.avail_in = 0;
	z.next_out = outbuf;
	z.avail_out = blocksize;
	
	gettimeofday( &start, NULL );

	while(1) {

		/* is no more data in the buffer? */
		if ( z.avail_in == 0 ) {

			/* get new data */
			z.next_in= inbuf;
			read= fread( inbuf, sizeof( char ), blocksize, fin );

			/* is the file empty? */
			if ( 0 == read ) {

				break;
			}

			totalin += read;
			z.avail_in= (uInt) read;
		}


		/* uncompress */
		status = inflate( &z, Z_SYNC_FLUSH );
		if ( Z_OK != status ) {
		
			fprintf( stderr, "error in uncompressing\n" );
			fclose( fin );
			fclose( fout );
			return 0;
		}
		
		/* get size of the out buffer */
		write= blocksize - z.avail_out;
		totalout += write;

		/* write to output if there is any data */
		if ( 0 < write ) {

			fwrite( outbuf, sizeof( char ), (size_t) write, fout );
		}

		z.next_out = outbuf;
		z.avail_out = blocksize;
	}

	gettimeofday( &stop, NULL );
	time= ((double) ( stop.tv_sec - start.tv_sec ) ) + 
			0.000001 * ((double) ( stop.tv_usec - start.tv_usec ) );

	fprintf( stdout, "\tbuf= %ub : %llub / %llub = %6.2f%%, time= %5.3fs\n", 
		blocksize, 
		(unsigned long long) totalin, 
		(unsigned long long) totalout, 
		100.0 * ((double) totalin) / ((double) totalout), 
		time );

	/* finalize everything */

	inflateEnd( &z );
	fclose( fin );
	fclose( fout );
	
	free( inbuf );
	free( outbuf );

	return 0;
}


#else /* HAVE_ZLIB */


int main ( int argc, const char** argv ) {


	fprintf( stderr, "'zlib' not available, otfcompress de-activated\n" );


	return 1;
}


#endif /* HAVE_ZLIB */


