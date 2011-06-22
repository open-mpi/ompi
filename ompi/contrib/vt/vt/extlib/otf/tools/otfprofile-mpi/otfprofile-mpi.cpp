/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

using namespace std;

#include <cassert>
#include <iostream>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "mpi.h"

#include "otf.h"

#include "datastructs.h"
#include "collect_data.h"
#include "summarize_data.h"
#include "reduce_data.h"
#include "create_latex.h"


#define FPRINTF_ROOT if(my_rank == 0) fprintf


/* define this macro to print result data to stdout */
/*#define SHOW_RESULTS*/

/* define this macro to have runtime measurement of certain profile scopes */
/*#define RUNTIME_MEASUREMENT*/


#ifdef RUNTIME_MEASUREMENT

    struct MeasureBlock {

#       define GETTIME() MPI_Wtime()

        double start_time;
        double stop_time;

        MeasureBlock() : start_time(-1.0), stop_time(-1.0) {}

        void start() {
            start_time= GETTIME();
        }
        void stop() {
            assert( -1.0 != start_time );
            stop_time= GETTIME();
        }
        double duration() const {
            assert( -1.0 != start_time && -1.0 != stop_time );
            return stop_time - start_time;
        }
    };

    /* store per-measure block runtimes */
    map < string, MeasureBlock > MeasureBlocksMap;

#endif /* RUNTIME_MEASUREMENT */


/* parse command line options
return 0 if succeeded, 1 if help text or version showed, 2 if failed */
static int parse_command_line( uint32_t my_rank, int argc, char** argv,
               AllData& alldata );

/* assign trace processes to analysis processes explicitly in order to allow
sophisticated grouping of MPI ranks/processes/threads/GPU threads/etc.
in the future, return true if succeeded  */
static bool assign_procs_to_ranks( uint32_t my_rank, uint32_t num_ranks,
                AllData& alldata );

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

    do {

        AllData alldata;

        /* step 0: parse command line options */
        if ( 0 !=
             ( ret= parse_command_line( my_rank, argc, argv, alldata ) ) ) {

            break;

        }

        MPI_Barrier( MPI_COMM_WORLD );

        /* step 1: assign trace processes to analysis processes */
        if ( !assign_procs_to_ranks( my_rank, num_ranks, alldata ) ) {

            ret= 1;
            break;

        }

        MPI_Barrier( MPI_COMM_WORLD );

#ifdef RUNTIME_MEASUREMENT
        if ( 0 == my_rank ) {

            MeasureBlocksMap[ "analyze data" ].start();

        }
#endif /* RUNTIME_MEASUREMENT */

        /* step 2: collect data by reading input trace file */
        if ( !collectData( my_rank, num_ranks, alldata ) ) {

            ret= 1;
            break;

        }

        MPI_Barrier( MPI_COMM_WORLD );

        /* step 3: summarize data; every analysis rank summarizes it's local
        data independently */
        if ( !summarizeData( my_rank, num_ranks, alldata ) ) {

            ret= 1;
            break;

        }

        MPI_Barrier( MPI_COMM_WORLD );

        /* step 4: reduce data to master */
        if ( !reduceData( my_rank, num_ranks, alldata ) ) {

           ret= 1;
           break;

        }

        MPI_Barrier( MPI_COMM_WORLD );

#ifdef RUNTIME_MEASUREMENT
        if ( 0 == my_rank ) {

            MeasureBlocksMap[ "analyze data" ].stop();

        }
#endif /* RUNTIME_MEASUREMENT */

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

#ifdef RUNTIME_MEASUREMENT
            MeasureBlocksMap[ "write tex" ].start();
#endif /* RUNTIME_MEASUREMENT */

            /* step 5.3: generate PGF output */
            if ( !createTex( alldata ) ) {

                ret= 1;
                break;

            }

#ifdef RUNTIME_MEASUREMENT
            MeasureBlocksMap[ "write tex" ].stop();
#endif /* RUNTIME_MEASUREMENT */

        }

    } while( false );

#ifdef RUNTIME_MEASUREMENT

    /* show runtime measurement results */

    if ( 0 == my_rank && 0 == ret ) {

        cout << endl << "runtime measurement results:" << endl;
        for ( map < string, MeasureBlock >::const_iterator it=
              MeasureBlocksMap.begin(); it != MeasureBlocksMap.end(); it++ ) {

            cout << "   " << it->first << ": " << it->second.duration()
                 << "s" << endl;
        }

    }

#endif /* RUNTIME_MEASUREMENT */

    /* either finalize or abort on error */

    if ( 0 == ret || 1 == ret ) {

        MPI_Finalize();

    } else {

        MPI_Abort( MPI_COMM_WORLD, ret );

    }

    return ret;
}


static int parse_command_line( uint32_t my_rank, int argc, char** argv,
               AllData& alldata ) {

    int ret= 0;

    Params& params= alldata.params;

    /* show help text if no options are given */
    if ( 1 == argc ) {

        if ( 0 == my_rank ) {

            show_helptext();

        }

        return 1;

    }

    /* read environment variables */

    char* env;

    env= getenv( "OTF_PROFILE_LATEX" );
    if ( env && 0 < strlen( env ) )
        params.latex_command= env;
    env= getenv( "OTF_PROFILE_DVIPDF" );
    if ( env && 0 < strlen( env ) )
        params.dvipdf_command= env;

    /* parse command line options */

    enum { ERR_OK, ERR_OPT_UNKNOWN, ERR_ARG_MISSING, ERR_ARG_INVALID };
    int parse_error= ERR_OK;

    int i;

    for ( i = 1; i < argc; i++ ) {

        /* -h, --help */
        if ( 0 == strcmp( "-h", argv[i] ) ||
             0 == strcmp( "--help", argv[i] ) ) {

            if ( 0 == my_rank ) {

                show_helptext();

            }

            ret= 1;
            break;

        /* -V */
        } else if ( 0 == strcmp( "-V", argv[i] ) ) {

            FPRINTF_ROOT( stdout, "%u.%u.%u \"%s\"\n",
                          OTF_VERSION_MAJOR, OTF_VERSION_MINOR, OTF_VERSION_SUB,
                          OTF_VERSION_STRING );

            ret= 1;
            break;

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

        /* --nopdf */
        } else if ( 0 == strcmp( "--nopdf", argv[i] ) ) {

           params.create_pdf= false;

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

        switch( parse_error ) {

            case ERR_OPT_UNKNOWN:

                FPRINTF_ROOT( stderr, "ERROR: Unknown option '%s'.\n", argv[i] );
                break;

            case ERR_ARG_MISSING:

                FPRINTF_ROOT( stderr, "ERROR: Expected argument for option '%s'.\n",
                              argv[i] );
                break;

            case ERR_ARG_INVALID:

                FPRINTF_ROOT( stderr, "ERROR: Invalid argument for option '%s'.\n",
                              argv[i] );
                break;

            default:

                break;

        }

        ret= 2;
    }

    return ret;
}


static bool assign_procs_to_ranks( uint32_t my_rank, uint32_t num_ranks,
                AllData& alldata ) {

    bool ret= true;

    if ( 0 == my_rank ) {

        /* rank 0 reads OTF master control of input trace file */

        OTF_FileManager* manager= OTF_FileManager_open( 1 );
        assert( manager );

        OTF_MasterControl* master= OTF_MasterControl_new( manager );
        assert( master );

        do {

            int master_read_ret=
                OTF_MasterControl_read( master,
                    alldata.params.input_file_prefix.c_str() );

            /* that's the first access to the input trace file; show tidy error
            message if failed */
            if ( 0 == master_read_ret ) {

                cerr << "ERROR: Unable to open file '"
                     << alldata.params.input_file_prefix << ".otf' for reading."
                     << endl;
                ret= false;
                break;

            }

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
            uint32_t r_workers= num_ranks;

            uint32_t pos= 0;
            bool warn_for_empty= true;
            for ( int w= 0; w < (int)num_ranks; w++ ) {

                uint32_t n= ( ( r_ranks / r_workers ) * r_workers < r_ranks) ?
                    ( r_ranks / r_workers +1 ) : ( r_ranks / r_workers );

                if ( ( 0 == n ) && warn_for_empty ) {

                    cerr << "Warning: more analysis ranks than trace processes, " <<
                            "ranks " << w << " to " << num_ranks -1 << " are unemployed" << endl;

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

    return ret;
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
         << "      -f <n>               max. number of filehandles available per rank" << endl
         << "                           (default: " << Params::DEFAULT_MAX_FILE_HANDLES << ")" << endl
         << "      -b <size>            set buffersize of the reader" << endl
         << "                           (default: " << Params::DEFAULT_BUFFER_SIZE << ")" << endl
         << "      -o <prefix>          specify the prefix of output file(s)" << endl
         << "                           (default: " << Params::DEFAULT_OUTPUT_FILE_PREFIX() << ")" << endl
         << "      --stat               read only summarized information, no events" << endl
         << "      --nopdf              do not produce PDF output" << endl
         << endl
         << "   environment variables:" << endl
         << "      OTF_PROFILE_LATEX    LaTeX command" << endl
         << "                           (default: " << Params::DEFAULT_LATEX_COMMAND() << ")" << endl
         << "      OTF_PROFILE_DVIPDF   DVI to PDF converter command" << endl
         << "                           (default: " << Params::DEFAULT_DVIPDF_COMMAND() << ")" << endl
         << endl
         << " PDF creation requires the PGFPLOTS package version >1.4" << endl
         << " http://sourceforge.net/projects/pgfplots/ " << endl
         << endl;
}
