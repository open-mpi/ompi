/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

using namespace std;

#include <cassert>
#include <iostream>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "otf.h"
#include "OTF_Platform.h"

#include "collect_data.h"
#include "otfprofile-mpi.h"
#include "summarize_data.h"
#include "reduce_data.h"
#include "create_latex.h"


/* define the following macro to synchronize the error indicator with all
   worker ranks

   This enforces that all ranks will be terminated by calling MPI_Abort if
   anyone fails. This is necessary to work around a bug that appears at least
   with Open MPI where calling MPI_Abort on one task doesn't terminate all
   other ranks. */
#define SYNC_ERROR

/* define the following macro to print result data to stdout */
/*#define SHOW_RESULTS*/


/* parse command line options
return 0 if succeeded, 1 if help text or version showed, -1 if failed */
static int parse_command_line( int argc, char** argv, AllData& alldata );

/* assign trace processes to analysis processes explicitly in order to allow
sophisticated grouping of MPI ranks/processes/threads/GPU threads/etc.
in the future, return true if succeeded  */
static bool assign_procs_to_ranks( AllData& alldata );

#ifdef SHOW_RESULTS
/* show results on stdout */
static void show_results( const AllData& alldata );
#endif /* SHOW_RESULTS */

/* show helptext */
static void show_helptext( void );


int main( int argc, char** argv ) {

    int ret= 0;

    /* start MPI */

    int my_rank;
    int num_ranks;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank );
    MPI_Comm_size(MPI_COMM_WORLD, &num_ranks );

    AllData alldata( my_rank, num_ranks );

    do {

        /* step 0: parse command line options */
        if ( 0 != ( ret= parse_command_line( argc, argv, alldata ) ) ) {

            if ( 1 == ret ) {

                ret= 0;

            } else { /* -1 == ret */

                ret= 1;

            }

            break;

        }

        VerbosePrint( alldata, 1, true, "initializing\n" );

        MPI_Barrier( MPI_COMM_WORLD );

        /* step 1: assign trace processes to analysis processes */
        if ( !assign_procs_to_ranks( alldata ) ) {

            ret= 1;
            break;

        }

        MPI_Barrier( MPI_COMM_WORLD );

        if ( 1 <= alldata.params.verbose_level && 0 == my_rank ) {

            alldata.measureBlockMap[ "analyze data" ].start();

        }

        /* step 2: collect data by reading input trace file */
        if ( !CollectData( alldata ) ) {

            ret= 1;
            break;

        }

        MPI_Barrier( MPI_COMM_WORLD );

        /* step 3: summarize data; every analysis rank summarizes it's local
        data independently */
        if ( !SummarizeData( alldata ) ) {

            ret= 1;
            break;

        }

        MPI_Barrier( MPI_COMM_WORLD );

        /* step 4: reduce data to master */
        if ( !ReduceData( alldata ) ) {

           ret= 1;
           break;

        }

        MPI_Barrier( MPI_COMM_WORLD );

        if ( 1 <= alldata.params.verbose_level && 0 == my_rank ) {

            alldata.measureBlockMap[ "analyze data" ].stop();

        }

        /* step 5: produce outputs */

        /* step 5.1: write CSV data */

        /* the master coordinates the length of each workers CSV text output,
        then every worker writes the own portion of the result CSV file
        (or couple of files) */

        /* do later */

        /* MPI_Barrier( MPI_COMM_WORLD ); */

        /* the master generates the result data from the global data
        reduced above */

        if ( 0 == my_rank ) {

#ifdef SHOW_RESULTS
            /* step 5.2: show result data on stdout */
            show_results( alldata );
#endif /* SHOW_RESULTS */

            alldata.measureBlockMap[ "produce output" ].start();

            /* step 5.3: generate PGF output */
            if ( !CreateTex( alldata ) ) {

                ret= 1;
                break;

            }

            alldata.measureBlockMap[ "produce output" ].stop();

        }

    } while( false );

    /* either finalize or abort on error */

    if ( 0 == ret ) {

        /* show runtime measurement results */
        if ( 1 <= alldata.params.verbose_level && 0 == my_rank ) {

            cout << "runtime measurement results:" << endl;
            for ( map < string, MeasureBlock >::const_iterator it=
                  alldata.measureBlockMap.begin();
                  it != alldata.measureBlockMap.end(); it++ ) {

                cout << " " << it->first << ": " << it->second.duration()
                     << "s" << endl;
            }

        }

        MPI_Finalize();

        VerbosePrint( alldata, 1, true, "done\n" );

    } else {

        MPI_Abort( MPI_COMM_WORLD, ret );

    }

    return ret;
}


static int parse_command_line( int argc, char** argv, AllData& alldata ) {

    int ret= 0;

    Params& params= alldata.params;

    /* parse command line options */

    enum { ERR_OK, ERR_OPT_UNKNOWN, ERR_ARG_MISSING, ERR_ARG_INVALID };
    int parse_error= ERR_OK;

    int i;

    for ( i = 1; i < argc; i++ ) {

        /* -h, --help */
        if ( 0 == strcmp( "-h", argv[i] ) ||
             0 == strcmp( "--help", argv[i] ) ) {

            if ( 0 == alldata.myRank ) {

                show_helptext();

            }

            ret= 1;
            break;

        /* -V */
        } else if ( 0 == strcmp( "-V", argv[i] ) ) {

            if ( 0 == alldata.myRank ) {

                printf( "%u.%u.%u \"%s\"\n",
                        OTF_VERSION_MAJOR, OTF_VERSION_MINOR, OTF_VERSION_SUB,
                        OTF_VERSION_STRING );

            }

            ret= 1;
            break;

        /* -v */
        } else if ( 0 == strcmp( "-v", argv[i] ) ) {

            params.verbose_level++;

        /* -p */
        } else if ( 0 == strcmp( "-p", argv[i] ) ) {

            params.progress= true;

        /* -f */
        } else if ( 0 == strcmp( "-f", argv[i] ) ) {

            if ( i == argc - 1 ) {

                parse_error= ERR_ARG_MISSING;
                break;

            }

            int tmp= atoi( argv[i+1] );
            if ( 0 >= tmp ) {

                parse_error= ERR_ARG_INVALID;
                break;
            }

            params.max_file_handles= tmp;
            i++;

        /* -b */
        } else if ( 0 == strcmp( "-b", argv[i] ) ) {

            if ( i == argc - 1 ) {

                parse_error= ERR_ARG_MISSING;
                break;

            }

            int tmp= atoi( argv[i+1] );
            if ( 0 >= tmp ) {

                parse_error= ERR_ARG_INVALID;
                break;
            }

            params.buffer_size= tmp;
            i++;

        /* -o */
        } else if ( 0 == strcmp( "-o", argv[i] ) ) {

            if ( i == argc - 1 ) {

                parse_error= ERR_ARG_MISSING;
                break;

            }

            params.output_file_prefix= argv[++i];

        /* --stat */
        } else if ( 0 == strcmp( "--stat", argv[i] ) ) {

           params.read_from_stats= true;

#if defined(PDFTEX) && defined(HAVE_PGFPLOTS_1_4) && HAVE_PGFPLOTS_1_4

        /* --nopdf */
        } else if ( 0 == strcmp( "--nopdf", argv[i] ) ) {

           params.create_pdf= false;

#endif /* PDFTEX && HAVE_PGFPLOTS_1_4 */

        /* input file or unknown option */
        } else {

            if ( 0 == params.input_file_prefix.length() ) {

                char* tmp= OTF_stripFilename( argv[i] );
                if ( tmp ) {

                    params.input_file_prefix= tmp;
                    free( tmp );

                }

            } else {

                parse_error= ERR_OPT_UNKNOWN;
                break;

            }

        }

    }

    /* show specific message on error */
    if ( ERR_OK != parse_error ) {

        if ( 0 == alldata.myRank ) {

            switch( parse_error ) {

                case ERR_OPT_UNKNOWN:

                    cerr << "ERROR: Unknown option '" << argv[i] << "'."
                         << endl;
                    break;

                case ERR_ARG_MISSING:

                    cerr << "ERROR: Expected argument for option '" << argv[i]
                         << "'." << endl;
                    break;

                case ERR_ARG_INVALID:

                    cerr << "ERROR: Invalid argument for option '" << argv[i]
                         << "'." << endl;
                    break;

                default:

                    break;

            }

        }

        ret= -1;

    /* show help text if no input trace file is given */
    } else if ( 0 == params.input_file_prefix.length() ) {

        if ( 0 == alldata.myRank ) {

            show_helptext();

        }

        ret= 1;

    }

    return ret;
}


static bool assign_procs_to_ranks( AllData& alldata ) {

    bool error= false;

    OTF_FileManager* manager= NULL;
    OTF_MasterControl* master= NULL;

    if ( 0 == alldata.myRank ) {

        /* rank 0 reads OTF master control of input trace file */

        manager= OTF_FileManager_open( 1 );
        assert( manager );

        master= OTF_MasterControl_new( manager );
        assert( master );

        int master_read_ret=
            OTF_MasterControl_read( master,
                alldata.params.input_file_prefix.c_str() );

        /* that's the first access to the input trace file; show tidy error
        message if failed */
        if ( 0 == master_read_ret ) {

            cerr << "ERROR: Unable to open file '"
                 << alldata.params.input_file_prefix << ".otf' for reading."
                 << endl;
            error= true;
        }
    }

    /* broadcast error indicator to workers because Open MPI had all
    ranks except rank 0 waiting endlessly in the MPI_Recv, when the '.otf' file
    was absent. */
    if ( SyncError( alldata, error, 0 ) ) {

        return false;

    }

    if ( 0 == alldata.myRank ) {

        do {

            /* fill the global array of processes */

            alldata.myProcessesNum= OTF_MasterControl_getrCount( master );
            alldata.myProcessesList=
                (uint32_t*)malloc( alldata.myProcessesNum * sizeof(uint32_t) );
            assert( alldata.myProcessesList );

            uint32_t i= 0;
            uint32_t j= 0;

            while( true ) {

                OTF_MapEntry* entry =
                    OTF_MasterControl_getEntryByIndex( master, i );

                if( NULL == entry) break;

                for ( uint32_t k= 0; k< entry->n; k++ ) {

                    alldata.myProcessesList[j]= entry->values[k];
                    j++;
                }

                i++;
            }
            assert( alldata.myProcessesNum == j );

            /* DEBUG */
            /*cerr << "processes in trace: ";
            for ( uint32_t k= 0; k < alldata.myProcessesNum; k++ ) {

                cerr << alldata.myProcessesList[k] << " ";
            }
            cerr << endl;*/


            /* now we may re-arrange the process list for a better layout
            - note that this layout is optimal to re-use OTF streams
            if there are multiple processes per stream
            - one may read the OTF definitions to know how to re-arrange */

            /* get number of ranks per worker, send to workers */

            /* remaining ranks and remaining workers */
            uint32_t r_ranks= alldata.myProcessesNum;
            uint32_t r_workers= alldata.numRanks;

            uint32_t pos= 0;
            bool warn_for_empty= true;
            for ( int w= 0; w < (int)alldata.numRanks; w++ ) {

                uint32_t n= ( ( r_ranks / r_workers ) * r_workers < r_ranks) ?
                    ( r_ranks / r_workers +1 ) : ( r_ranks / r_workers );

                if ( ( 0 == n ) && warn_for_empty ) {

                    cerr << "Warning: more analysis ranks than trace processes, "
                         << "ranks " << w << " to " << alldata.numRanks -1
                         << " are unemployed" << endl;

                    warn_for_empty= false;
                }

                if ( 0 == w ) {

                    /* for master itself simply truncate processesList,
                    don't send and receive */
                    alldata.myProcessesNum= n;

                } else {

                    MPI_Send( &n, 1, MPI_INT, w, 2, MPI_COMM_WORLD );

                    MPI_Send( alldata.myProcessesList + pos, n, MPI_INT,
                              w, 3, MPI_COMM_WORLD );

                }

                pos += n;
                r_ranks -= n;
                r_workers -= 1;
            }

        } while( false );

        /* close OTF master control and file manager */
        OTF_MasterControl_close( master );
        OTF_FileManager_close( manager );

    } else { /* 0 != my_rank */

        /* workers receive number and sub-list of their ranks to process */

        alldata.myProcessesNum= 0;

        MPI_Status status;

        MPI_Recv( &alldata.myProcessesNum, 1, MPI_INT, 0, 2, MPI_COMM_WORLD,
                  &status );

        alldata.myProcessesList= (uint32_t*)malloc(
            alldata.myProcessesNum * sizeof(uint32_t) );
        assert( alldata.myProcessesList );

        MPI_Recv( alldata.myProcessesList, alldata.myProcessesNum, MPI_INT, 0,
                  3, MPI_COMM_WORLD, &status );

    }

    /* DEBUG */
    /*cerr << " worker " << my_rank << " handles: ";
    for ( uint32_t v= 0; v < alldata.myProcessesNum; v++ ) {

        cerr << alldata.myProcessesList[v] << " ";
    }
    cerr << endl;*/

    return !error;
}


#ifdef SHOW_RESULTS

static void show_results( const AllData& alldata ) {

#   define PRINT_MIN_MAX_AVG(v,u) (v.cnt) << " x avg " << ((double)(v.sum))/(v.cnt) << "(" << (v.min) << "-" << (v.max) << ") " << u 

    cout << endl << " global data per function: " << endl;
    {
        map< uint64_t, FunctionData >::const_iterator it= alldata.functionMapGlobal.begin();
        map< uint64_t, FunctionData >::const_iterator itend= alldata.functionMapGlobal.end();
        while ( itend != it ) {

            cout << "     global function " << it->first << " -> " ;
            if ( it->second.count.cnt ) {
                cout << "\t"<<
                    " cnt: " << PRINT_MIN_MAX_AVG(it->second.count,"[#]") << 
                    " exc: " << PRINT_MIN_MAX_AVG(it->second.excl_time,"[s]") << 
                    " inc: " << PRINT_MIN_MAX_AVG(it->second.incl_time,"[s]") << endl;
            }

            it++;
        }
    }

    cout << endl << " global counter data per function: " << endl;
    {
        map< Pair, FunctionData, ltPair >::const_iterator it= alldata.counterMapGlobal.begin();
        map< Pair, FunctionData, ltPair >::const_iterator itend= alldata.counterMapGlobal.end();
        while ( itend != it ) {

            cout << "     global counter " << it->first.a << " per function " << it->first.b << " -> " << endl;
            if ( it->second.count.cnt ) {
                cout << "\t"<<
                    " cnt: " << PRINT_MIN_MAX_AVG(it->second.count,"[#]");
                cout << " exc: ";
                if ( it->second.excl_time.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.excl_time,"[#]");
                } else {
                    cout << "0 [#]";
                }
                cout << " inc: ";
                if ( it->second.incl_time.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.incl_time,"[#]");
                } else {
                    cout << "0 [#]";
                }
                cout << endl;
            }

            it++;
        }
    }

    cout << endl << " global message data per cluster pair: " << endl;
    {
        map< Pair, MessageData >::const_iterator it=    alldata.messageMapPerClusterPair.begin();
        map< Pair, MessageData >::const_iterator itend= alldata.messageMapPerClusterPair.end();
        while ( itend != it ) {

            if ( it->second.count_send.cnt ) {
                cout << "\tsent " << it->first.a << " --> " << it->first.b <<
                    " cnt: " << PRINT_MIN_MAX_AVG(it->second.count_send,"[#]");
                cout << " byt: ";
                if ( it->second.bytes_send.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.bytes_send,"[b]");
                } else {
                    cout << "0 [b]";
                }
                cout << " dur: ";
                if ( it->second.duration_send.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.duration_send,"[s]");
                } else {
                    cout << "0 [s]";
                }
                cout << endl;
            }
            if ( it->second.count_recv.cnt ) {
                cout << "\trecv " << it->first.a << " <-- " << it->first.b <<
                    " cnt: " << PRINT_MIN_MAX_AVG(it->second.count_recv,"[#]");
                cout << " byt: ";
                if ( it->second.bytes_recv.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.bytes_recv,"[b]");
                } else {
                    cout << "0 [b]";
                }
                cout << " dur: ";
                if ( it->second.duration_recv.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.duration_recv,"[s]");
                } else {
                    cout << "0 [s]";
                }
                cout << endl;
            }

            it++;
        }
    }

    cout << endl << " global message data per cluster: " << endl;
    {
        map< uint64_t, MessageData >::const_iterator it=    alldata.messageMapPerCluster.begin();
        map< uint64_t, MessageData >::const_iterator itend= alldata.messageMapPerCluster.end();
        while ( itend != it ) {

            cout << "     msg of cluster " << it->first << " -> " << endl;
            if ( it->second.count_send.cnt ) {
                cout << "\tsent" << 
                " cnt: " << PRINT_MIN_MAX_AVG(it->second.count_send,"[#]");
                cout << " byt: ";
                if ( it->second.bytes_send.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.bytes_send,"[b]");
                } else {
                    cout << "0 [b]";
                }
                cout << " dur: ";
                if ( it->second.duration_send.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.duration_send,"[s]");
                } else {
                    cout << "0 [s]";
                }
                cout << endl;
            }
            if ( it->second.count_recv.cnt ) {
                cout << "\trecv" << 
                    " cnt: " << PRINT_MIN_MAX_AVG(it->second.count_recv,"[#]");
                cout << " byt: ";
                if ( it->second.bytes_recv.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.bytes_recv,"[b]");
                } else {
                    cout << "0 [b]";
                }
                cout << " dur: ";
                if ( it->second.duration_recv.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.duration_recv,"[s]");
                } else {
                    cout << "0 [s]";
                }
                cout << endl;
            }

            it++;
        }
    }

    cout << endl << " global message speed per length: " << endl;
    {
        map< Pair, MessageSpeedData, ltPair >::const_iterator it= alldata.messageSpeedMapPerLength.begin();
        map< Pair, MessageSpeedData, ltPair >::const_iterator itend= alldata.messageSpeedMapPerLength.end();
        while ( itend != it ) {

            cout << "     msg of speed-bin " << it->first.a << " length-bin " << it->first.b << " -> ";
            if ( it->second.count.cnt ) {
                cout << "\t" << 
                    " cnt: " << PRINT_MIN_MAX_AVG(it->second.count,"[#]") << endl;
            }

            it++;
        }
    }

    cout << endl << " global collective data per cluster: " << endl;
    {
        map< Pair, CollectiveData, ltPair >::const_iterator it=    alldata.collectiveMapPerCluster.begin();
        map< Pair, CollectiveData, ltPair >::const_iterator itend= alldata.collectiveMapPerCluster.end();
        while ( itend != it ) {

            cout << "     collop of class " << it->first.a << " cluster " << it->first.b << " -> " << endl;
            if ( it->second.count_send.cnt ) {
                cout << "\tsent" << 
                " cnt: " << PRINT_MIN_MAX_AVG(it->second.count_send,"[#]");
                cout << " byt: ";
                if ( it->second.bytes_send.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.bytes_send,"[b]");
                } else {
                    cout << "0 [b]";
                }
                cout << " dur: ";
                if ( it->second.duration_send.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.duration_send,"[s]");
                } else {
                    cout << "0 [s]";
                }
                cout << endl;
            }
            if ( it->second.count_recv.cnt ) {
                cout << "\trecv" << 
                    " cnt: " << PRINT_MIN_MAX_AVG(it->second.count_recv,"[#]");
                cout << " byt: ";
                if ( it->second.bytes_recv.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.bytes_recv,"[b]");
                } else {
                    cout << "0 [b]";
                }
                cout << " dur: ";
                if ( it->second.duration_recv.cnt ) {
                    cout << PRINT_MIN_MAX_AVG(it->second.duration_recv,"[s]");
                } else {
                    cout << "0 [s]";
                }
                cout << endl;
            }

            it++;
        }
    }
}

#endif /* SHOW_RESULTS */


static void show_helptext() {

    cout << endl
         << " otfprofile-mpi - generate a profile of a trace in LaTeX format." << endl
         << endl
         << " Syntax: otfprofile-mpi [options] <input file name>" << endl
         << endl
         << "   options:" << endl
         << "      -h, --help           show this help message" << endl
         << "      -V                   show OTF version" << endl
         << "      -v                   increase output verbosity" << endl
         << "                           (can be used more than once)" << endl
         << "      -p                   show progress" << endl
         << "      -f <n>               max. number of filehandles available per rank" << endl
         << "                           (default: " << Params::DEFAULT_MAX_FILE_HANDLES << ")" << endl
         << "      -b <size>            set buffersize of the reader" << endl
         << "                           (default: " << Params::DEFAULT_BUFFER_SIZE << ")" << endl
         << "      -o <prefix>          specify the prefix of output file(s)" << endl
         << "                           (default: " << Params::DEFAULT_OUTPUT_FILE_PREFIX() << ")" << endl
         << "      --stat               read only summarized information, no events" << endl
#if defined(PDFTEX) && defined(HAVE_PGFPLOTS_1_4) && HAVE_PGFPLOTS_1_4
         << "      --nopdf              do not produce PDF output" << endl
#else /* PDFTEX && HAVE_PGFPLOTS_1_4 */
         << endl
         << " PDF creation requires the PGFPLOTS package version >1.4" << endl
         << " http://sourceforge.net/projects/pgfplots/ " << endl
#endif /* PDFTEX && HAVE_PGFPLOTS_1_4 */
         << endl;
}


void VerbosePrint( AllData& alldata, uint8_t level, bool root_only,
         const char* fmt, ... ) {

    if ( alldata.params.verbose_level >= level ) {

        va_list ap;

        va_start( ap, fmt );

        /* either only rank 0 print the message */
        if ( root_only ) {

            if ( 0 == alldata.myRank ) {

                vprintf( fmt, ap );
            }

        /* or all ranks print the message */
        } else {

            char msg[1024];

            /* prepend current rank to message */
            snprintf( msg, sizeof( msg ) -1, "[%u] ", alldata.myRank );
            vsnprintf( msg + strlen( msg ), sizeof( msg ) -1, fmt, ap );

            /* print message */
            printf( "%s ", msg );

        }

        va_end( ap );

    }
}


bool SyncError( AllData& alldata, bool& error, uint32_t root ) {

#ifdef SYNC_ERROR

    if ( 1 < alldata.numRanks ) {

        int buf= ( error ) ? 1 : 0;

        /* either broadcast the error indicator from one rank (root)
        or reduce them from all */

        if ( root != (uint32_t)-1 ) {

            MPI_Bcast( &buf, 1, MPI_INT, (int)root, MPI_COMM_WORLD );

            error= ( 1 == buf );

        } else {

            int recv_buf;

            MPI_Allreduce( &buf, &recv_buf, 1, MPI_INT, MPI_MAX,
                           MPI_COMM_WORLD );

            error= ( 1 == recv_buf );

        }

    }

#endif /* SYNC_ERROR */

    return error;
}


uint64_t Logi( uint64_t x, uint64_t b ) {

    assert( b > 1 );

    uint64_t c= 1;
    uint64_t i= 0;

    while( c <= x ) {

        c*= b;
        i++;
    }

    return i;
}
