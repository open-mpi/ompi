/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "otf.h"

#include "handler.h"
#include "hash.h"


#define OTFMERGE_STRING "otfmerge"

#define FINISH_EVERYTHING	hash_delete( fcb.hash); fcb.hash= NULL; \
							OTF_Reader_close( reader ); \
							OTF_Writer_close( fcb.writer ); \
							OTF_FileManager_close( manager ); \
							OTF_HandlerArray_close( handlers ); \

#define SHOW_HELPTEXT { \
	int l = 0; while( Helptext[l] ) { printf( "%s", Helptext[l++] ); } }

static const char* Helptext[] = {
"                                                                  \n",
" otfmerge - converter program of OTF library.                     \n",
"                                                                  \n",
" otfmerge [Options] <input file name>                             \n",
"                                                                  \n",
"   options:                                                       \n",
"      -h, --help    show this help message                        \n",
"      -V            show OTF version                              \n",
"      -n <n>        set number of streams for output              \n",
"                    set this to 0 for using one stream per process\n",
"                    standard is 1                                 \n",
"      -f <n>        set max number of filehandles available       \n",
"      -o <name>     namestub of the output file (default 'out')   \n",
"      -rb <size>    set buffersize of the reader                  \n",
"      -wb <size>    set buffersize of the writer                  \n",
"      -stats        cover statistics too                          \n",
"      -snaps        cover snaphots too                            \n",
"      -z <zlevel>   write compressed output                       \n",
"                    zlevel reaches from 0 to 9 where 0 is no      \n",
"                    compression and 9 is the highest level        \n",
"      -l            write long OTF format                         \n",
"      -p            show progress                                 \n",
"                                                                  \n",
"                                                                  \n", NULL };

void initProgressDisplay( void );
void finishProgressDisplay( void );
void updateProgressDisplay( uint32_t i, uint64_t max, uint64_t cur );

int main ( int argc, char** argv ) {


	OTF_Reader* reader = NULL;
	OTF_HandlerArray* handlers;
	OTF_FileManager* manager;
	OTF_MasterControl* mc;
	OTF_MapEntry* mapentry;
	
	int read_stats= 0;
	int read_snaps= 0;
	int nstreams = 1;
	int nfiles = 200;
	int longformat = 0;
	int showprogress= 0;
	int i;

	int readerbuffersize = 1024 * 1024;
	int writerbuffersize = 1024 * 1024;
	OTF_FileCompression compression= 0;
	
	fcbT fcb;
	
	char* infile= NULL;
	char* outfile= NULL;

	uint64_t minbytes;
	uint64_t maxbytes;
	uint64_t curbytes;
	uint64_t minbytestmp;
	uint64_t maxbytestmp;
	uint64_t curbytestmp;
	uint64_t recordsperupdate;
	uint64_t totalbytes;
	uint32_t progress_counter= 0;

	uint64_t retde;
	uint64_t retma;
	uint64_t retev;
	uint64_t retst;
	uint64_t retsn;

	fcb.error= 0;

	if ( 1 >= argc ) {

			SHOW_HELPTEXT;
			return 0;
	}

	for ( i = 1; i < argc; i++ ) {


		if ( ( 0 == strcmp( "-i", argv[i] ) ) && ( i+1 < argc ) ) {

			infile= argv[i+1];
			++i;

		} else if ( ( 0 == strcmp( "-n", argv[i] ) ) && ( i+1 < argc ) ) {

			nstreams = atoi( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-rb", argv[i] ) ) && ( i+1 < argc ) ) {

			readerbuffersize = atoi( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-wb", argv[i] ) ) && ( i+1 < argc ) ) {

			writerbuffersize = atoi( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-f", argv[i] ) ) && ( i+1 < argc ) ) {

			nfiles = atoi( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-o", argv[i] ) ) && ( i+1 < argc ) ) {

			outfile= argv[i+1];
			++i;

		} else if ( 0 == strcmp( "-stats", argv[i] ) ) {

			read_stats= 1;

		} else if ( 0 == strcmp( "-snaps", argv[i] ) ) {

			read_snaps= 1;

		} else if ( ( 0 == strcmp( "-z", argv[i] ) ) && ( i+1 < argc ) ) {

			compression= atoi( argv[i+1] );

			++i;

		} else if ( 0 == strcmp( "-l", argv[i] ) ) {

			longformat = 1;

		} else if ( 0 == strcmp( "-p", argv[i] ) ) {

			showprogress= 1;

		} else if ( 0 == strcmp( "--help", argv[i] ) || 
				0 == strcmp( "-h", argv[i] ) ) {

			SHOW_HELPTEXT;
			return 0;

		} else if ( 0 == strcmp( "-V", argv[i] ) ) {
		
			printf( "%u.%u.%u \"%s\"\n", OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
				OTF_VERSION_SUB, OTF_VERSION_STRING);
			exit( 0 );

		} else {

			if ( '-' != argv[i][0] ) {

				infile= argv[i];

			} else{

				fprintf( stderr, "ERROR: Unknown option: '%s'\n", argv[i] );
				exit(1);
			}
		}
	}
	
	if ( nfiles < 1 ) {
	
		fprintf( stderr, "ERROR: less than 1 filehandle is not permitted\n" );
		exit(1);
	}
	if ( nstreams < 0 ) {
	
		fprintf( stderr, "ERROR: less than 0 stream is not permitted\n" );
		exit(1);
	}
	if ( NULL == infile ) {
	
		fprintf( stderr, "ERROR: no input file specified\n" );
		exit(1);
	}
	if ( NULL == outfile ) {

		/*
		fprintf( stderr, "ERROR: no output file has been specified\n" );
		exit(1);
		*/

		outfile= "out.otf";
	}

	handlers = OTF_HandlerArray_open();

	manager= OTF_FileManager_open( nfiles );
	if( NULL == manager) {
		fprintf( stderr, "Error: Unable to initialize File Manager. aborting\n" );
		exit(1);
	}
	
	reader = OTF_Reader_open( infile, manager );

	if ( NULL == reader ) {

		fprintf( stderr, "Error: Unable to open '%s'. aborting\n", infile );
		OTF_FileManager_close( manager );
		OTF_HandlerArray_close( handlers );
		exit(1);
	}

	OTF_Reader_setBufferSizes( reader, readerbuffersize );
	
	fcb.writer = OTF_Writer_open( outfile, nstreams, manager );
	OTF_Writer_setBufferSizes( fcb.writer, writerbuffersize );
	OTF_Writer_setCompression( fcb.writer, compression );
	if( longformat )
		OTF_Writer_setFormat( fcb.writer, OTF_WSTREAM_FORMAT_LONG );
	else
                OTF_Writer_setFormat( fcb.writer, OTF_WSTREAM_FORMAT_SHORT );

	mc = OTF_Reader_getMasterControl( reader );

	/* set your own handler functions */

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefinitionComment,
		OTF_DEFINITIONCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFINITIONCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefTimerResolution,
		OTF_DEFTIMERRESOLUTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFTIMERRESOLUTION_RECORD);

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefProcess,
		OTF_DEFPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefProcessGroup,
		OTF_DEFPROCESSGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFPROCESSGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefFunction,
		OTF_DEFFUNCTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFFUNCTION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefFunctionGroup,
		OTF_DEFFUNCTIONGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFFUNCTIONGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefCollectiveOperation,
		OTF_DEFCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefCounter,
		OTF_DEFCOUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFCOUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefCounterGroup,
		OTF_DEFCOUNTERGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFCOUNTERGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefScl,
		OTF_DEFSCL_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFSCL_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefSclFile,
		OTF_DEFSCLFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFSCLFILE_RECORD );

/*	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefversion,
		OTF_DEFVERSION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFVERSION_RECORD );
*/
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefCreator,
		OTF_DEFCREATOR_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFCREATOR_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefFile,
		OTF_DEFFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFFILE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefFileGroup,
		OTF_DEFFILEGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_DEFFILEGROUP_RECORD );



	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEventComment,
		OTF_EVENTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_EVENTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleCounter,
		OTF_COUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_COUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEnter,
		OTF_ENTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_ENTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleCollectiveOperation,
		OTF_COLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_COLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleBeginCollectiveOperation,
		OTF_BEGINCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_BEGINCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEndCollectiveOperation,
		OTF_ENDCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_ENDCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleRecvMsg,
		OTF_RECEIVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_RECEIVE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleSendMsg,
		OTF_SEND_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_SEND_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleLeave,
		OTF_LEAVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_LEAVE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleBeginProcess,
		OTF_BEGINPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb, 
		OTF_BEGINPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEndProcess,
		OTF_ENDPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb, 
		OTF_ENDPROCESS_RECORD );
			
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFileOperation,
		OTF_FILEOPERATION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb, 
		OTF_FILEOPERATION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleBeginFileOperation,
		OTF_BEGINFILEOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb, 
		OTF_BEGINFILEOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEndFileOperation,
		OTF_ENDFILEOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb, 
		OTF_ENDFILEOP_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) handleRMAPut,
                OTF_RMAPUT_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb,
                OTF_RMAPUT_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) handleRMAPutRemoteEnd,
                OTF_RMAPUTRE_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb,
                OTF_RMAPUTRE_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) handleRMAGet,
                OTF_RMAGET_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb,
                OTF_RMAGET_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) handleRMAEnd,
                OTF_RMAEND_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb,
                OTF_RMAEND_RECORD );


	/* snapshot records */

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleSnapshotComment,
		OTF_SNAPSHOTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb,
		OTF_SNAPSHOTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEnterSnapshot,
		OTF_ENTERSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb, 
		OTF_ENTERSNAPSHOT_RECORD );
	
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleSendSnapshot,
		OTF_SENDSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb, 
		OTF_SENDSNAPSHOT_RECORD );	

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleOpenFileSnapshot,
		OTF_OPENFILESNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb, 
		OTF_OPENFILESNAPSHOT_RECORD );


	/* summary records */

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleSummaryComment,
		OTF_SUMMARYCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
	&fcb, OTF_SUMMARYCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFunctionSummary,
		OTF_FUNCTIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_FUNCTIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFunctionGroupSummary,
		OTF_FUNCTIONGROUPSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_FUNCTIONGROUPSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleMessageSummary,
		OTF_MESSAGESUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_MESSAGESUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleCollopSummary,
		OTF_COLLOPSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_COLLOPSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFileOperationSummary,
		OTF_FILEOPERATIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_FILEOPERATIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFileGroupOperationSummary,
		OTF_FILEGROUPOPERATIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
		&fcb, OTF_FILEGROUPOPERATIONSUMMARY_RECORD );

	/* marker record types */

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefMarker, OTF_DEFMARKER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb, OTF_DEFMARKER_RECORD );
	
	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleMarker, OTF_MARKER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fcb, OTF_MARKER_RECORD );	

	/* misc records */
	
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleUnknown,
		OTF_UNKNOWN_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fcb, OTF_UNKNOWN_RECORD );

	/* ask the mastercontrol for the number of streams and create the
	 * streaminfos array
	 */
	fcb.nstreaminfos = 0;
	
	while ( 0 !=  OTF_MasterControl_getEntryByIndex( mc, fcb.nstreaminfos )) {
	
		fcb.nstreaminfos++;
	}
	
	fcb.hash = hash_new ();
	
	/* global stream yourself, because he isnt in the mapping */
	hash_add( fcb.hash, 0 );
	
	/* add all streams to the hash */
	for( i = 0; i < fcb.nstreaminfos; i++ ) {
	
		mapentry = OTF_MasterControl_getEntryByIndex( mc, i );
		
		hash_add( fcb.hash, mapentry->argument );
	}
	
	/* read definitions */
	retde= OTF_Reader_readDefinitions( reader, handlers );
	if( OTF_READ_ERROR == retde || 1 == fcb.error ) {
		fprintf( stderr, "Error while reading definitions. aborting\n" );
		FINISH_EVERYTHING;
		exit(1);
	}

	/* read markers */
	retma= OTF_Reader_readMarkers( reader, handlers );
	if( OTF_READ_ERROR == retma || 1 == fcb.error ) {
		fprintf( stderr, "Error while reading marker records. aborting\n" );
		FINISH_EVERYTHING;
		exit(1);
	}

	
	if ( 0 == showprogress ) {

		/* do not show the progress */
		
		/* read events */
		retev= OTF_Reader_readEvents( reader, handlers );
		if( OTF_READ_ERROR == retev ) {
			fprintf( stderr, "Error while reading events. aborting\n" );
			FINISH_EVERYTHING;
			exit(1);
		}
		
		/* read stats */
		if ( 1 == read_stats ) {
		
			retst= OTF_Reader_readStatistics( reader, handlers );
			if( OTF_READ_ERROR == retst ) {
				fprintf( stderr, "Error while reading statistics. aborting\n" );
				FINISH_EVERYTHING;
				exit(1);
			}

		}
		
		/* read snaps */
		if ( 1 == read_snaps ) {
		
			retsn= OTF_Reader_readSnapshots( reader, handlers );
			if( OTF_READ_ERROR == retsn ) {
				fprintf( stderr, "Error while reading snapshots. aborting\n" );
				FINISH_EVERYTHING;
				exit(1);
			}
			
		}
		
	} else {

		/* show progress */

		initProgressDisplay();

		/* calculate how many records will be read */
		minbytes= 0;
		curbytes= 0;
		maxbytes= 0;
		
		OTF_Reader_setRecordLimit( reader, 0 );
		
		retev= OTF_Reader_readEvents( reader, handlers );
		if( OTF_READ_ERROR == retev ) {
			fprintf( stderr, "Error while reading events. aborting\n" );
			FINISH_EVERYTHING;
			exit(1);
		}
		
		if ( 1 == read_stats ) {
			retst= OTF_Reader_readStatistics( reader, handlers );
			if( OTF_READ_ERROR == retst ) {
				fprintf( stderr, "Error while reading statistics. aborting\n" );
				FINISH_EVERYTHING;
				exit(1);
			}
		}
		if ( 1 == read_snaps ) {
			retsn= OTF_Reader_readSnapshots( reader, handlers );
			if( OTF_READ_ERROR == retsn ) {
				fprintf( stderr, "Error while reading snapshots. aborting\n" );
				FINISH_EVERYTHING;
				exit(1);
			}
		}

		OTF_Reader_eventBytesProgress( reader, &minbytestmp, &curbytestmp, &maxbytestmp );

		minbytes+= minbytestmp;
		maxbytes+= maxbytestmp;
		
		if ( 1 == read_stats ) {
		
			OTF_Reader_statisticBytesProgress( reader, &minbytestmp, &curbytestmp, &maxbytestmp );
			minbytes+= minbytestmp;
			maxbytes+= maxbytestmp;
		}
		
		/* read snaps */
		if ( 1 == read_snaps ) {
		
			OTF_Reader_snapshotBytesProgress( reader, &minbytestmp, &curbytestmp, &maxbytestmp );
			minbytes+= minbytestmp;
			maxbytes+= maxbytestmp;
		}

		curbytes= 0;
		totalbytes= maxbytes - minbytes;


		/* fixed number of records per update in order to provide
		frequent update */
		recordsperupdate= 100000;
		OTF_Reader_setRecordLimit( reader, recordsperupdate );
		
		while ( 0 != ( retev= OTF_Reader_readEvents( reader, handlers ) ) ) {

			if( OTF_READ_ERROR == retev ) {
				fprintf( stderr, "Error while reading events. aborting\n" );
				FINISH_EVERYTHING;
				exit(1);
			}
			
			OTF_Reader_eventBytesProgress( reader, 
				&minbytestmp, &curbytestmp, &maxbytestmp );

			curbytes += curbytestmp - minbytestmp - curbytes;

			updateProgressDisplay( progress_counter++, totalbytes, curbytes );
		}

		/* read stats */
		while ( ( 1 == read_stats ) && 
				( 0 != ( retst= OTF_Reader_readStatistics( reader, handlers ) ) ) ) {
					
			if( OTF_READ_ERROR == retst ) {
				fprintf( stderr, "Error while reading statistics. aborting\n" );
				FINISH_EVERYTHING;
				exit(1);
			}
			
			OTF_Reader_statisticBytesProgress( reader, 
				&minbytestmp, &curbytestmp, &maxbytestmp );
	
			curbytes += curbytestmp - minbytestmp - curbytes;

			updateProgressDisplay( progress_counter++, totalbytes, curbytes );
		}
		
		/* read snaps */
		while ( ( 1 == read_snaps ) && 
				( 0 != ( retsn= OTF_Reader_readSnapshots( reader, handlers ) ) ) ) {
					
			if( OTF_READ_ERROR == retsn ) {
				fprintf( stderr, "Error while reading snapshots. aborting\n" );
				FINISH_EVERYTHING;
				exit(1);
			}
			
			OTF_Reader_snapshotBytesProgress( reader, 
					&minbytestmp, &curbytestmp, &maxbytestmp );
	
			curbytes += curbytestmp - minbytestmp - curbytes;

			updateProgressDisplay( progress_counter++, totalbytes, curbytes );
		}

		finishProgressDisplay();
	}

	FINISH_EVERYTHING;
	
	return 0;
}


void initProgressDisplay() {


	printf( " %7.2f %%\r", 0.0 );
	fflush( stdout );
}


void finishProgressDisplay() {


	printf( " %7.2f %%  done\n", 100.0 );
	fflush( stdout );
}


void updateProgressDisplay( uint32_t i, uint64_t max, uint64_t cur ) {


/*	static char animation[]= {"-", "\\", "|", "/" }; */
	static char* animation[]= { "", "." };


/*	printf( "%llu / %llu \n", cur, max ); */

	printf( " %7.2f %%  %s \r", 
		( ((double) cur) * 100.0 / ((double) max) ),
		animation[ i % ( sizeof(animation) / sizeof(animation[0]) ) ] );
	fflush( stdout );
}

