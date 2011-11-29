/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstring>
#include <cerrno>

#include <set>
#include <iostream>
#include <cassert>
using namespace std;

#ifdef HAVE_UNISTD_H
	#include <unistd.h>
#endif

#include "OTF_Platform.h"

#include "Handler.h"
#include "Control.h"

#define SHOW_HELPTEXT { \
	int l = 0; while( Helptext[l] ) { printf( "%s", Helptext[l++] ); } }

static const char* Helptext[] = {
"                                                                          \n",
" otfaux - append snapshots and statistics to existing otf traces          \n",
"            at given 'break' time stamps.                                 \n",
"                                                                          \n",
"                                                                          \n",
" otfaux  [Options] <file name>                                            \n",
"                                                                          \n",
" Options:                                                                 \n",
"     -h, --help     show this help message                                \n",
"     -V             show OTF version                                      \n",
"     -b <size>      buffer size for read and write operations             \n",
"     -n <n>         number of breaks (distributed regularly)              \n",
"                    if -p and -t are not set, the default for -n is 200   \n",
"                    breaks                                                \n",
"     -p <p>         create break every 'p' ticks                          \n",
"                    (if both, -n and -p are specified the one producing   \n",
"                    more breaks wins)                                     \n",
"     -t <t>         define (additional) break at given time stamp         \n",
"     -F             force overwrite old snapshots and statistics          \n",
"     -R             delete existing snapshots and statistics only         \n",
"     -f <n>         max number of filehandles output                      \n",
"     --funcgroups   create functiongroup summaries instead of             \n",
"                    function summaries                                    \n",
"     --filegroups   create file group summaries instead of file           \n",
"                    summaries                                             \n",
"     -v             verbose mode, print break time stamps                 \n",
"     -a             show advancing progress during operation              \n",
"                                                                          \n",
"     --snapshots    write ONLY snapshots but NO statistics                \n",
"     --statistics   write ONLY statistics but NO snapshots                \n",
"                                                                          \n",
"     -s a[,b]*      regard given streams only when computing statistics.  \n",
"                    expects a single token or comma separated list.       \n",
"                    this implies the '--statistics' option!               \n",
"     -l             list existing stream tokens                           \n",
"                                                                          \n", NULL };

#define DEFAULT_SUMMARYNUMBER 200


void checkExistingFile( const char* tmpfilename, bool forceoverwrite, bool deleteonly );


int main ( int argc, const char** argv ) {

	char* filename = NULL;
	int buffersize= 1024;

	/** minimum number of snapshots distributed regularly over the trace's 
	time interval, i.e. not at the very beginning or very end */
	uint64_t summary_number= 0;

	/** distance of successive snapshots in ticks */
	uint64_t summary_distance= (uint64_t) -1;

	OTF_FileManager* manager;
	OTF_Reader* reader= NULL;
	OTF_Writer* writer= NULL;
	OTF_MasterControl* mc= NULL;

	OTF_HandlerArray* handlers;
	
	char *namestub;
	bool forceoverwrite= false;
	bool deleteonly= false;

	OTF_FileCompression compression= OTF_FILECOMPRESSION_UNCOMPRESSED;
	unsigned int maxfilehandles = 100;

	bool verbose= false;
	bool usefunctiongroups= false;
	bool usefilegroups= false;
	bool showprogress= false;
	bool listonly= false;

    bool doSnapshots= true;
    bool doStatistics= true;

	uint64_t read;

	/* has something been set? 1= n, 2= p, 4= t */
	int npt= 0;
	
	/** set of selected streams, all streams if set is empty! */
	std::set<uint32_t> streams;

	/** list of explicit time stamps for snapshots */
	std::set<uint64_t> timestamps;


	/* argument handling */

	if ( 1 >= argc ) {

		SHOW_HELPTEXT;
		exit(0);
	}

	for ( int i = 1; i < argc; i++ ) {

		if ( ( 0 == strcmp( "-i", argv[i] ) ) && ( i+1 < argc ) ) {
		
			filename= strdup( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-b", argv[i] ) ) && ( i+1 < argc ) ) {
		
			buffersize = atoi( argv[i+1] );
			++i;

		} else if ( 0 == strcmp( "--help", argv[i] ) ||	0 == strcmp( "-h", argv[i] ) ) {
				
			SHOW_HELPTEXT;
			exit(0);

		} else if ( 0 == strcmp( "-V", argv[i] ) ) {
		
			printf( "%u.%u.%u \"%s\"\n", OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
				OTF_VERSION_SUB, OTF_VERSION_STRING);
			exit( 0 );

		} else if ( ( 0 == strcmp( "-n", argv[i] ) ) && ( i+1 < argc ) ) {
		
			summary_number= atoi( argv[i+1] );
			npt= 1;
			++i;

		} else if ( ( 0 == strcmp( "-p", argv[i] ) ) && ( i+1 < argc ) ) {
		
			summary_distance= atoi( argv[i+1] );
			npt= 2;
			++i;

		} else if ( ( 0 == strcmp( "-t", argv[i] ) ) && ( i+1 < argc ) ) {
		
			uint64_t time = atoi( argv[i+1] );
			timestamps.insert( time );
			npt= 4;
			++i;

		} else if ( 0 == strcmp( "-F", argv[i] ) ) {
		
			forceoverwrite= true;

		} else if ( 0 == strcmp( "-R", argv[i] ) ) {
		
			deleteonly= true;

		} else if( 0 == strcmp( "-z", argv[i] ) ) {
		
			compression= OTF_FILECOMPRESSION_COMPRESSED;
			
		} else if ( ( 0 == strcmp( "-f", argv[i] ) ) && ( i+1 < argc ) ) {
		
			maxfilehandles= atoi( argv[i+1] );
			++i;

		} else if( 0 == strcmp( "-a", argv[i] ) ) {
		
			showprogress= true;

		} else if( 0 == strcmp( "-v", argv[i] ) ) {
		
			verbose= true;
			
		}else if( 0 == strcmp( "--funcgroups", argv[i] ) ) {
		
			usefunctiongroups= true;
			
		} else if( 0 == strcmp( "--filegroups", argv[i] ) ) {
		
			usefilegroups= true;
			
		} else if( 0 == strcmp( "-l", argv[i] ) ) {
		
			listonly= true;

		} else if ( ( 0 == strcmp( "-s", argv[i] ) ) && ( i+1 < argc ) ) {

			/* operation for selected streams IMPLIES '--statistics' */
			doSnapshots= false;


			const char* p= argv[i+1];
			++i;

			while ( '\0' != *p ) {

				uint32_t token= strtol( p, (char**) NULL, 16 );

				streams.insert( token );
				
				/* search comma or '\n' */
				while ( ( '\0' != *p ) && ( ',' != *p ) ) {

					++p;
				}

				/* skip comma */
				if ( ',' == *p ) {
				
					++p;
				}
			}

		} else if( 0 == strcmp( "--snapshots", argv[i] ) ) {
		
		    doStatistics= false;

		} else if( 0 == strcmp( "--statistics", argv[i] ) ) {
		
			doSnapshots= false;

		} else {

			if ( '-' != argv[i][0] ) {
			
				filename= strdup( argv[i] );

			} else {

				fprintf( stderr, "ERROR: Unknown argument.\n" );
				exit(1);
			}
		}
	}


	if ( NULL == filename ) {
	
		printf( " no input file specified, abort\n" );
		exit(1);
	}
	
	if ( ! ( doStatistics || doSnapshots ) ) {

		printf( "ERROR: contradicting options '--snapshots' and '--statistics', abort\n" );
		exit(1);
	}


	if ( doSnapshots && !streams.empty() ) {

		printf( "ERROR: contradicting options '--snapshots' and '-s', abort\n" );
		exit(1);
	}

	/* n has been set, not p, not t */
	if ( 1 == npt && 2 > summary_number ) {
	
		printf( " you must at least have 2 breaks\n" );
		exit(1);
	}
	
	if ( 1 > maxfilehandles ) {
	
		printf( " there must be at least 1 available filehandle\n" );
		exit(1);
	}
	
	if ( 0 == npt ) {
	
		summary_number= 200;
	}
	
	/* open filemanager */
	manager= OTF_FileManager_open( maxfilehandles );
	assert( NULL != manager );

	/* Open OTF Reader */
	reader= OTF_Reader_open( filename, manager );
	if ( NULL == reader ) {

		fprintf( stderr, "%s ERROR: could not open '%s'\n", "otfaux", filename );
		return 1;
	}

	OTF_Reader_setBufferSizes( reader, buffersize );
	mc= OTF_Reader_getMasterControl( reader );


	if ( listonly ) {

		printf( "stream ID : process IDs ... \n" );

		uint32_t index= 0;
		const OTF_MapEntry* entry;
		while ( NULL != ( entry= OTF_MasterControl_getEntryByIndex( mc, index ) ) ) {

			printf( "%x :  ", entry->argument );
			for ( uint32_t i= 0; i < entry->n; ++i ) {

				printf( "%x ", entry->values[i] );
			}
			printf( "\n" );

			++index;
		}

		/* exit otfaux */
		return 0;
	}

	/** if 'streams' is empty, i.e. all existing streams are to be used, 
	fill all existing stream tokens into 'streams'. */
	if ( streams.empty() ) {
	
	
		uint32_t index= 0;
		const OTF_MapEntry* entry;
		while ( NULL != ( entry= OTF_MasterControl_getEntryByIndex( mc, index ) ) ) {

			streams.insert( entry->argument );
			++index;
		}
	}

	/* check if there are already existing statistics and snapshots */
	/* get streamcount and namestub */
	namestub= OTF_stripFilename( filename );

	
	std::set<uint32_t>::const_iterator jt= streams.begin();
	std::set<uint32_t>::const_iterator jtend= streams.end();

	for ( ; jt != jtend; ++jt ) {	

		char* tmpfilename;
		OTF_FileType type;

		type= OTF_FILETYPE_SNAPS;
		tmpfilename= OTF_getFilename( namestub, *jt, type, 0, NULL );
		checkExistingFile( tmpfilename, forceoverwrite, deleteonly );
		free( tmpfilename );

		type= OTF_FILETYPE_STATS;
		tmpfilename= OTF_getFilename( namestub, *jt, type, 0, NULL );
		checkExistingFile( tmpfilename, forceoverwrite, deleteonly );
		free( tmpfilename );

		type= OTF_FILETYPE_SNAPS | OTF_FILECOMPRESSION_COMPRESSED;
		tmpfilename= OTF_getFilename( namestub, *jt, type, 0, NULL );
		checkExistingFile( tmpfilename, forceoverwrite, deleteonly );
		free( tmpfilename );

		type= OTF_FILETYPE_STATS | OTF_FILECOMPRESSION_COMPRESSED;
		tmpfilename= OTF_getFilename( namestub, *jt, type, 0, NULL );
		checkExistingFile( tmpfilename, forceoverwrite, deleteonly );
		free( tmpfilename );
	}

	/* deleting files is done by now, exit */
	if ( deleteonly ) {

		return 0;
	}

	/* Open OTF Writer */
	writer= OTF_Writer_open( filename, 0, manager );
	assert( NULL != writer );
	OTF_Writer_setBufferSizes( writer, buffersize );
	OTF_Writer_setCompression( writer, compression );
	OTF_Writer_setMasterControl( writer, mc );

	free( filename );


	Control* control= new Control( writer, verbose, usefunctiongroups,
		usefilegroups, doSnapshots, doStatistics );


	handlers= OTF_HandlerArray_open();

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefFunction,
		OTF_DEFFUNCTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFFUNCTION_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDeftimerresolution,
		OTF_DEFTIMERRESOLUTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFTIMERRESOLUTION_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefprocess,
		OTF_DEFPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefFile,
		OTF_DEFFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFFILE_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefCollectiveOperation,
		OTF_DEFCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFCOLLOP_RECORD );

	
	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleCounter,
		OTF_COUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_COUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleEnter,
		OTF_ENTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_ENTER_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleRecvmsg,
		OTF_RECEIVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_RECEIVE_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleSendmsg,
		OTF_SEND_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_SEND_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleLeave,
		OTF_LEAVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_LEAVE_RECORD );
	
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleBeginProcess,
		OTF_BEGINPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
		OTF_BEGINPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleEndProcess,
		OTF_ENDPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_ENDPROCESS_RECORD );

    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleCollectiveOperation,
        OTF_COLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_COLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEventComment,
		OTF_EVENTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
		OTF_EVENTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFileOperation,
		OTF_FILEOPERATION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
		OTF_FILEOPERATION_RECORD );
        
        
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleBeginCollectiveOperation,
        OTF_BEGINCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_BEGINCOLLOP_RECORD );
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleEndCollectiveOperation,
        OTF_ENDCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_ENDCOLLOP_RECORD );
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleBeginFileOperation,
        OTF_BEGINFILEOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_BEGINFILEOP_RECORD );
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleEndFileOperation,
        OTF_ENDFILEOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_ENDFILEOP_RECORD );
	
	
	/* cout << "read " << read << " defs" << endl; */
	read = OTF_Reader_readDefinitions( reader, handlers );
	if( read == OTF_READ_ERROR ) {
		fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort.\n");
		return 1;
	}

	/** compute where to put snapshots */

	uint64_t tmin= 0;
	uint64_t tcur= 0;
	uint64_t tmax= (uint64_t) -1;

	/* init read operation but do not start to read records yet. this ensures the
	time interval of the trace is extracted */
	OTF_Reader_setRecordLimit( reader, 0 );
	read = OTF_Reader_readEvents( reader, handlers );
	if( read == OTF_READ_ERROR ) {
		fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort.\n");
		return 1;
	}

	/* cout << "read " << read << " events" << endl; */
	OTF_Reader_setRecordLimit( reader, OTF_READ_MAXRECORDS );

	OTF_Reader_eventTimeProgress( reader, &tmin, &tcur, &tmax );

    /*
     * Increment in order to place final statistics after the very last
     * event.
     */
    tmax += 1;

	/*
	cout << "total time " << 
		(unsigned long long) tmin << " - " << 
		(unsigned long long) tmax << endl;
	*/

	double d= ((double) ( tmax - tmin ) ) / ((double) summary_number );
	d= ( d <= (double) summary_distance ) ? d : (double) summary_distance;
	d= ( 1.0 < d ) ? d : 1.0;

    /*
     * generated sample points, but don't include t_min,
     * it is defined as 0-point
     *
     * Tmax was incremented before to be after the very last event.
     */
    for ( double t= (double) tmin + d; t < tmax; t += d ) {
        control->addTime( (uint64_t) t );
    }


	/* append user defined time stamps */

	set<uint64_t>::const_iterator it= timestamps.begin();
	set<uint64_t>::const_iterator itend= timestamps.end();
	for ( ; it != itend; ++it ) {

		control->addTime( *it );
	}

    /*
     * Place very last statistics _after_ the last record but not right
     * before it. Needs to be triggered explictily in the end.
     *
     * Tmax was incremented before to be the timestamp after the very
     * last event timestamp.
     */
    if ( control->getLastTime() < tmax ) {

        control->addTime( tmax );
    }


	/* restrict streams resp. processes to be read */

	/* first disable all processes */
	OTF_Reader_setProcessStatusAll( reader, 0 );

	/* then enable all processes of all selected streams */
	jt= streams.begin();
	jtend= streams.end();
	for ( ; jt != jtend; ++jt ) {

		OTF_MapEntry* entry= OTF_MasterControl_getEntry( mc, *jt );

		if ( NULL != entry ) {

			for ( uint32_t i= 0; i < entry->n; ++i ) {

				OTF_Reader_setProcessStatus( reader, entry->values[i], 1 );
			}

		} else {
		
			printf( "WARNING: stream '%x' undefined\n", *jt );
		}
	}

	if ( true == control->timestamps.empty() ) {
	
		fprintf( stderr,  " no statistics or snapshots will be created.\n" );
		exit(0);
	}

	if ( showprogress ) {

		/* show progress report */

		uint64_t min;
		uint64_t cur;
		uint64_t max;

		OTF_Reader_setRecordLimit( reader, 100000 );

		while ( 0 < ( read = OTF_Reader_readEvents( reader, handlers ) ) ) {

			if( read == OTF_READ_ERROR ) {
				fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort. Abort.\n");
				return 1;
			}

			OTF_Reader_eventProgress( reader, &min, &cur, &max );
			fprintf( stdout, "    progress %4.1f %%\r%10s", 
				100.0 * ( (double) ( cur - min ) ) / ( (double) ( max - min ) ), "" );
		}

		fprintf( stdout, "%40s\n", "" );

		OTF_Reader_setRecordLimit( reader, OTF_READ_MAXRECORDS );

	} else {

		read = OTF_Reader_readEvents( reader, handlers );
		if( read == OTF_READ_ERROR ) {
			fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort.\n");
			return 1;
		}
		/* cout << "read " << read << " events" << endl; */
	}

    /*
     * Explicitly trigger writing for the timestamp of the very last
     * event such that this event is included in the final statistics.
     *
     * Tmax was incremented before to be the timestamp after the very
     * last event timestamp.
     */
    control->checkTime( tmax );
    
    delete control;

	OTF_Reader_close( reader );


	/* DO NOT close the writer in order to prevent the original MasterControl file
	to be overwritten. Instead close only all the streams of that writer. */
	OTF_Writer_closeAllStreams( writer );

	OTF_HandlerArray_close( handlers );
	OTF_FileManager_close( manager );

	return (0);
}


void checkExistingFile( const char* tmpfilename, bool forceoverwrite, bool deleteonly ) {


	FILE* tmpfile= fopen( tmpfilename, "rb" );
	if ( NULL != tmpfile ) {
	
		fclose( tmpfile );
			
		if ( forceoverwrite || deleteonly ) {

				unlink( tmpfilename );

		} else {
		
			printf( "ERROR: will not overwrite existing file '%s', abort\n", tmpfilename );
			exit( 1 );
		}
	}
}

