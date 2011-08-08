/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_unify.h"
#include "vt_unify_hooks_tdb.h"

#include "otf.h"

#include <iostream>
#include <fstream>

#include <sstream>

#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>

#ifdef _SX
    #include <sys/socket.h> // needed on NEC SX platforms for gethostname()
#endif /* _SX */

#ifndef HOST_NAME_MAX
    #define HOST_NAME_MAX 256
#endif /* HOST_NAME_MAX */

#if defined(HAVE_OMP) && HAVE_OMP
    #define GET_THREAD_ID( var ) int var = omp_get_thread_num();
#else /* HAVE_OMP */
    #define GET_THREAD_ID( var ) int var = 0;
#endif /* HAVE_OMP */

#define GET_PARAM(type, var, i) type var = (type) args[i];

//////////////////// class HooksTdbC ////////////////////

// public methods
//

HooksTdbC::HooksTdbC() : HooksBaseC(),
   MinStartTimeEpoch( (uint64_t)-1 ), MaxStopTimeEpoch( 0 ) {}

HooksTdbC::~HooksTdbC() {}

bool HooksTdbC::isEnabled() {

    static int enabled = -1;

    if( enabled == -1 ) {

        enabled = 0;
        char* createtdb_env = getenv( "VT_UNIFY_CREATE_TDB" );
        if( createtdb_env ) {

            std::string tmp( createtdb_env );
            for( uint32_t i = 0; i < tmp.length(); i++ )
                tmp[i] = tolower( tmp[i] );

            if( tmp.compare( "yes" ) == 0 ||
                tmp.compare( "true" ) == 0 ||
                tmp.compare( "1" ) == 0 ) {

                enabled = 1;

            }

        }

    }

    return enabled == 1;

}

// private methods
//

// vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

// initialization/finalization hooks
//

void HooksTdbC::initHook() {

    /* nothing to do at this point */

}

void HooksTdbC::finalizeHook( const bool & error ) {

    if( error ) {

        std::cout << " ERROR IN HooksTdbC::finalizeHook ABORT." << std::endl;
        return;

    }

    if( !RootData.create_output ) {

        /* do not output anything */
        /* --> this normally happens if
           vtunify prints the helptext */

        return;

    }

    std::map<uint32_t, CollOpC>::iterator collop_it;

    /* collect data from other threads in master thread #0 */
    for( uint32_t i = 1; i < ThreadVector.size(); i++ ) {

           ThreadVector[0] += ThreadVector[i];

    }

#ifdef VT_MPI

#define NUM_SEND_PARTS 3

    uint64_t *recv_buffer = NULL;

    VT_MPI_INT *recv_count = NULL;
    uint64_t sum_recv_count = 0;
    uint64_t sum_sent_count = 0;
    VT_MPI_INT *displ = NULL;

    uint64_t *send_buffer = new uint64_t[NUM_SEND_PARTS];

    /* this is the number of properties in the ThreadData class */
    int num_attributes = 13;

    int collop_map_size = ThreadVector[0].collop.size() * 4;
    int io_map_size = ThreadVector[0].io.size() * 8;

    send_buffer[0] = (uint64_t) num_attributes;
    send_buffer[1] = (uint64_t) collop_map_size;
    send_buffer[2] = (uint64_t) io_map_size;

    for( int i = 0; i < NUM_SEND_PARTS; i++ ) {

        sum_sent_count += send_buffer[i];

    }

    if( MyRank != 0 ) {

        /* first gatherv */
        CALL_MPI( MPI_Gather( send_buffer, NUM_SEND_PARTS, MPI_LONG_LONG_INT,
                              recv_buffer, 0, MPI_LONG_LONG_INT, 0,
                              MPI_COMM_WORLD ) );

    } else {

        recv_count = new VT_MPI_INT[ NumRanks ];
        displ = new VT_MPI_INT[ NumRanks ];

        recv_buffer = new uint64_t[ NumRanks * NUM_SEND_PARTS ];

        CALL_MPI( MPI_Gather( send_buffer, NUM_SEND_PARTS, MPI_LONG_LONG_INT,
                              recv_buffer, NUM_SEND_PARTS, MPI_LONG_LONG_INT,
                              0,  MPI_COMM_WORLD ) );

        displ[0] = 0;
        sum_recv_count = 0;

        for( VT_MPI_INT j = 0; j < NumRanks; j++ ) {

            recv_count[j] = 0;

            for( int i = 0; i < NUM_SEND_PARTS; i++ ) {

                recv_count[j] += recv_buffer[j * NUM_SEND_PARTS + i];

            }

            displ[j] = sum_recv_count;
            sum_recv_count += recv_count[j];

        }

    }

    /* reuse send_buffer */
    delete [] send_buffer;

    send_buffer = new uint64_t[ sum_sent_count ];

    ThreadVector[0].toBuffer( send_buffer );

    if( MyRank != 0 ) {

        CALL_MPI( MPI_Gatherv( send_buffer, sum_sent_count, MPI_LONG_LONG_INT,
                               recv_buffer, recv_count, displ, MPI_LONG_LONG_INT,
                               0, MPI_COMM_WORLD ) );

    } else {

        uint64_t *recv_data_buffer = new uint64_t[ sum_recv_count ];
        uint64_t *p = recv_data_buffer;

        CALL_MPI( MPI_Gatherv( send_buffer, sum_sent_count, MPI_LONG_LONG_INT,
                               recv_data_buffer, recv_count, displ,
                               MPI_LONG_LONG_INT, 0, MPI_COMM_WORLD ) );

        ThreadDataC tmp_vector;

        p += recv_count[0];

        for( VT_MPI_INT j = 1; j < NumRanks; j++ ) {

            tmp_vector.fromBuffer( p, &(recv_buffer[ j * NUM_SEND_PARTS ]) );

            ThreadVector[0] += tmp_vector;

            p += recv_count[j];

        }

        uint32_t collop_type;

        for( collop_it = ThreadVector[0].collop.begin(); collop_it != ThreadVector[0].collop.end(); ++collop_it ) {

            collop_type = RootData.collop_def[ collop_it->first ];

            RootData.collop[collop_type].num += collop_it->second.num;
            RootData.collop[collop_type].bytes_sent += collop_it->second.bytes_sent;
            RootData.collop[collop_type].bytes_recv += collop_it->second.bytes_recv;

        }

    }


    /* OUTPUT */


    if( MyRank == 0 ) {

#endif /*VT_MPI*/

        RootData.setFilepath( Params.out_file_prefix );
        RootData.setHostname();
        RootData.setOtfVersion();

        /* assume that vtunify creates one stream per process */
        RootData.num_processes = RootData.num_streams = UnifyCtls.size();
        RootData.is_compressed = Params.docompress;
        RootData.starttime = RootData.runtime = 0;
        if( MinStartTimeEpoch != (uint64_t)-1 && MaxStopTimeEpoch != 0 )
        {
           RootData.starttime = MinStartTimeEpoch / 1000000;
           RootData.runtime =  ( MaxStopTimeEpoch - MinStartTimeEpoch );
        }

        RootData.calcFilesize();

        std::string tdb_file = RootData.filename + ".tdb";
        std::ofstream outfile( tdb_file.c_str() );

        outfile << "filename;" << RootData.filename << std::endl;
        outfile << "filepath;" << RootData.filepath << std::endl;
        outfile << "hostname;" << RootData.hostname << std::endl;
        outfile << "otf_version;" << RootData.otf_version << std::endl;
        outfile << "creator;" << RootData.creator << std::endl;
        outfile << "num_processes;" << RootData.num_processes << std::endl;
        outfile << "num_active_processes;" << RootData.num_active_processes << std::endl;
        outfile << "num_streams;" << RootData.num_streams << std::endl;
        outfile << "num_threads;" << RootData.num_threads << std::endl;
        outfile << "starttime;" << RootData.starttime << std::endl;
        outfile << "runtime;" << RootData.runtime << std::endl;
        outfile << "filesize;" << RootData.filesize << std::endl;
        outfile << "compression;" << RootData.is_compressed << std::endl;

        outfile << "num_events; " << ThreadVector[0].num_events << std::endl;
        outfile << "num_enter; " << ThreadVector[0].num_enter << std::endl;
        outfile << "num_leave; " << ThreadVector[0].num_leave << std::endl;
        outfile << "num_sent; " << ThreadVector[0].num_sent << std::endl;
        outfile << "num_recv; " << ThreadVector[0].num_recv << std::endl;
        outfile << "bytes_sent; " << ThreadVector[0].bytes_sent << std::endl;
        outfile << "bytes_recv; " << ThreadVector[0].bytes_recv << std::endl;
        outfile << "num_rma; " << ThreadVector[0].num_rma << std::endl;
        outfile << "bytes_rma; " << ThreadVector[0].bytes_rma << std::endl;

        outfile << "num_marker; " << ThreadVector[0].num_marker << std::endl;
        outfile << "num_stats; " << ThreadVector[0].num_stats << std::endl;
        outfile << "num_snaps; " << ThreadVector[0].num_snaps << std::endl;
        outfile << "num_vt_flushes; " << ThreadVector[0].num_vt_flushes << std::endl;


        std::map<uint32_t, std::string>::const_iterator fgroup_it;

        for( fgroup_it = RootData.fgroup.begin(); fgroup_it != RootData.fgroup.end(); ++fgroup_it ) {

            outfile << "function_group;" << fgroup_it->second << std::endl;

        }


        for( uint32_t i = 0; i < RootData.counter.size(); i++ ) {

            outfile << "counter;" << RootData.counter[i] << std::endl;

        }


        std::map<std::string, std::string>::const_iterator vt_cit;

        for( vt_cit = RootData.vt_env.begin(); vt_cit != RootData.vt_env.end(); ++vt_cit ) {

            outfile << "vt_env_vars;" << vt_cit->first << ";" << vt_cit->second << std::endl;

        }

        std::map<uint32_t,std::string> col_id_to_name;

        col_id_to_name[ OTF_COLLECTIVE_TYPE_UNKNOWN ] = "Unknown";
        col_id_to_name[ OTF_COLLECTIVE_TYPE_BARRIER ] = "Barrier";
        col_id_to_name[ OTF_COLLECTIVE_TYPE_ONE2ALL ] = "One2All";
        col_id_to_name[ OTF_COLLECTIVE_TYPE_ALL2ONE ] = "All2One";
        col_id_to_name[ OTF_COLLECTIVE_TYPE_ALL2ALL ] = "All2All";

        for( collop_it = RootData.collop.begin(); collop_it != RootData.collop.end(); ++collop_it ) {

            outfile << "collops;" << col_id_to_name[ collop_it->first ] << ";" << collop_it->second.num
            << ";" << collop_it->second.bytes_sent << ";" << collop_it->second.bytes_recv << std::endl;

        }

        std::map<uint32_t,std::string> ioflag_to_name;

        ioflag_to_name[ 0 ] = "all";
        ioflag_to_name[ 1 ] = "normal";
        ioflag_to_name[ OTF_IOFLAG_IOFAILED ] = "failed";
        ioflag_to_name[ OTF_IOFLAG_ASYNC ] = "async";
        ioflag_to_name[ OTF_IOFLAG_COLL ] = "coll";
        ioflag_to_name[ OTF_IOFLAG_DIRECT ] = "direct";
        ioflag_to_name[ OTF_IOFLAG_SYNC ] = "sync";
        ioflag_to_name[ OTF_IOFLAG_ISREADLOCK ] = "readlock";

        std::map<uint32_t, IoC>::iterator io_it;

        for( io_it = ThreadVector[0].io.begin(); io_it != ThreadVector[0].io.end(); ++io_it ) {

            outfile << "io;"
                    << ioflag_to_name[ io_it->first ] << ";"
                    << io_it->second.num_read << ";"
                    << io_it->second.bytes_read << ";"
                    << io_it->second.num_written << ";"
                    << io_it->second.bytes_written << ";"
                    << io_it->second.num_open << ";"
                    << io_it->second.num_close << ";"
                    << io_it->second.num_seek << std::endl;

        }

        outfile.close();

#ifdef VT_MPI
    }
#endif /* VT_MPI */

}

// phase hooks
//

void HooksTdbC::phaseHook_GetUnifyControls_post() {

    /* resize vector of thread data in respect of max. thread number */

#if defined(HAVE_OMP) && HAVE_OMP
    ThreadVector.resize( omp_get_max_threads() );
#else /* HAVE_OMP */
    ThreadVector.resize( 1 );
#endif /* HAVE_OMP */

}

void HooksTdbC::phaseHook_UnifyDefinitions_post() {

    /* thread-safe: this and all definition records are always called by thread 0 on rank 0 */

    std::map<uint32_t, uint32_t>::const_iterator flush_it;

    for( flush_it = RootData.flush_fgroup_ids.begin();
         flush_it != RootData.flush_fgroup_ids.end(); ++flush_it ) {

        if( RootData.fgroup[ flush_it->second ] == "VT_API" ) {

            /* this is necessary because of a bug in VT
               --> a user-defined function named "flush" is
               assigned to group "VT_API" too */
            if( RootData.vt_flush_id == 0 )
                RootData.vt_flush_id = flush_it->first;

        }

    }

}

void HooksTdbC::phaseHook_UnifyEvents_pre() {

    /* if this is not called, an error occurred or
       vtunify just printed the helptext */

    GET_THREAD_ID( thread_id );

    if( thread_id == 0 ) {

        RootData.create_output = true;

#ifdef VT_MPI

        CALL_MPI( MPI_Bcast( &(RootData.vt_flush_id), 1, MPI_UNSIGNED, 0,
                             MPI_COMM_WORLD ) );

#endif /* VT_MPI */

    }

}

// record hooks
//

void HooksTdbC::writeRecHook_DefComment( HooksC::VaArgsT & args ) {

    /* thread-safe */

    /* get hook arguments */
    GET_PARAM( std::string*, comment, 1 );
    GET_PARAM( bool*, do_write, 2 );

    if( *do_write ) {

        std::string vt_comment = *comment;
        std::string vt_env_name;
        std::string vt_env_value;
        std::string::size_type start_pos;
        std::string::size_type pos;

        start_pos = vt_comment.find_first_not_of( ' ', 0 );

        if( start_pos != std::string::npos ) {

            vt_comment.erase( 0, start_pos );

            if( vt_comment.length() > 3 && vt_comment.substr( 0, 3 ) == "VT_" ) {

                pos = vt_comment.find( ": ", 0 );

                if( pos != std::string::npos ) {

                    vt_env_name = vt_comment.substr( 0, pos );
                    vt_env_value = vt_comment.substr( pos + 2, vt_comment.length() );

                    RootData.vt_env[vt_env_name] = vt_env_value;

                }

            }

        }

    }

}

void HooksTdbC::writeRecHook_DefCreator( HooksC::VaArgsT & args ) {

    /* thread-safe */

    /* get hook arguments */
    GET_PARAM( std::string*, creator, 1 );
    GET_PARAM( bool*, do_write, 2 );

    if( *do_write ) {

        RootData.creator = *creator;

    }

}

void HooksTdbC::writeRecHook_DefProcess( HooksC::VaArgsT & args ) {

    /* thread-safe */

    /* get hook arguments */
    GET_PARAM( uint32_t*, parent, 3 );
    GET_PARAM( bool*, do_write, 4 );

    if( *do_write ) {

        if( *parent > 0 ) {

            RootData.num_threads++;

        } else {

            RootData.num_active_processes++;

        }

    }

}

void HooksTdbC::writeRecHook_DefFunctionGroup( HooksC::VaArgsT & args ) {

    /* thread-safe */

    /* get hook arguments */
    GET_PARAM( uint32_t*, fgroup, 1 );
    GET_PARAM( std::string*, fgroup_name, 2 );
    GET_PARAM( bool*, do_write, 3 );

    if( *do_write ) {

        RootData.fgroup[ *fgroup ] = *fgroup_name;

    }

}

void HooksTdbC::writeRecHook_DefFunction( HooksC::VaArgsT & args ) {

    /* thread-safe */

    /* get hook arguments */
    GET_PARAM( uint32_t*, function, 1 );
    GET_PARAM( std::string*, name, 2 );
    GET_PARAM( uint32_t*, fgroup, 3 );
    GET_PARAM( bool*, do_write, 5 );

    if( *do_write && *name == "flush" ) {

        RootData.flush_fgroup_ids[ *function ] = *fgroup;

    }

}

void HooksTdbC::writeRecHook_DefCollOp( HooksC::VaArgsT & args ) {

    /* thread-safe */

    /* get hook arguments */
    GET_PARAM( uint32_t*, collop, 1 );
    GET_PARAM( uint32_t*, type, 3 );
    GET_PARAM( bool*, do_write, 4 );

    if( *do_write ) {

        RootData.collop_def[*collop] = *type;

    }

}

void HooksTdbC::writeRecHook_DefCounter( HooksC::VaArgsT & args ) {

    /* thread-safe */

    /* get hook arguments */
    GET_PARAM( std::string*, counter_name, 2 );
    GET_PARAM( bool*, do_write, 6 );

    if( *do_write ) {

        RootData.counter.push_back( *counter_name );

    }

}

void HooksTdbC::writeRecHook_FunctionSummary( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 7 );

    if( *do_write ) {

        ThreadVector[thread_id].num_stats++;

    }

}

void HooksTdbC::writeRecHook_MessageSummary( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 9 );

    if( *do_write ) {

        ThreadVector[thread_id].num_stats++;

    }

}

void HooksTdbC::writeRecHook_CollOpSummary( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 9 );

    if( *do_write ) {

        ThreadVector[thread_id].num_stats++;

    }

}

void HooksTdbC::writeRecHook_FileOpSummary( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 11 );

    if( *do_write ) {

        ThreadVector[thread_id].num_stats++;

    }

}

void HooksTdbC::writeRecHook_MarkerSpot( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 5 );

    if( *do_write ) {

        ThreadVector[thread_id].num_marker++;

    }

}

void HooksTdbC::writeRecHook_Enter( HooksC::VaArgsT & args )
{

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( uint32_t*, function, 2 );
    GET_PARAM( bool*, do_write, 6 );

    if( *do_write ) {

        if( *function == RootData.vt_flush_id ) {

            ThreadVector[thread_id].num_vt_flushes++;

        }

        ThreadVector[thread_id].num_enter++;

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_Leave( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 6 );

    if( *do_write ) {

        ThreadVector[thread_id].num_leave++;

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_BeginFileOp( HooksC::VaArgsT & args )
{

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 6 );

    if( *do_write ) {

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_EndFileOp( HooksC::VaArgsT & args ) {

    /* get hook arguments */
    GET_PARAM( uint32_t*, operation, 6 );
    GET_PARAM( uint64_t*, bytes, 7 );
    GET_PARAM( bool*, do_write, 11 );

    if( *do_write ) {

        handleFileOperation( operation, bytes );

    }

}

void HooksTdbC::writeRecHook_SendMsg( HooksC::VaArgsT & args )
{

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( uint32_t*, msglength, 6 );
    GET_PARAM( bool*, do_write, 9 );

    if( *do_write ) {

        ThreadVector[thread_id].num_sent++;

        ThreadVector[thread_id].bytes_sent += *msglength;

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_RecvMsg( HooksC::VaArgsT & args )
{

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( uint32_t*, msglength, 6 );
    GET_PARAM( bool*, do_write, 9 );

    if( *do_write ) {

        ThreadVector[thread_id].num_recv++;

        ThreadVector[thread_id].bytes_recv += *msglength;

        ThreadVector[thread_id].num_events++;

    }
}

void HooksTdbC::writeRecHook_BeginCollOp( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( uint32_t*, collop, 3 );
    GET_PARAM( uint64_t*, sent, 7 );
    GET_PARAM( uint64_t*, recv, 8 );
    GET_PARAM( bool*, do_write, 11 );

    if( *do_write ) {

        ThreadVector[thread_id].collop[*collop].num++;

        ThreadVector[thread_id].collop[*collop].bytes_sent += *sent;

        ThreadVector[thread_id].collop[*collop].bytes_recv += *recv;

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_EndCollOp( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 5 );

    if( *do_write ) {

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_RMAPut( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( uint64_t*, bytes, 7 );
    GET_PARAM( bool*, do_write, 10 );

    if( *do_write ) {

        ThreadVector[thread_id].num_rma++;

        ThreadVector[thread_id].bytes_rma += *bytes;

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( uint64_t*, bytes, 7 );
    GET_PARAM( bool*, do_write, 10 );

    if( *do_write ) {

        ThreadVector[thread_id].num_rma++;

        ThreadVector[thread_id].bytes_rma += *bytes;

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_RMAGet( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( uint64_t*, bytes, 7 );
    GET_PARAM( bool*, do_write, 10 );

    if( *do_write ) {

        ThreadVector[thread_id].num_rma++;

        ThreadVector[thread_id].bytes_rma += *bytes;

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_RMAEnd( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 8 );

    if( *do_write ) {

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_Counter( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 6 );

    if( *do_write ) {

        ThreadVector[thread_id].num_events++;

    }

}

void HooksTdbC::writeRecHook_EventComment( HooksC::VaArgsT & args ) {

    GET_THREAD_ID( thread_id );

    /* get hook arguments */
    GET_PARAM( bool*, do_write, 5 );

    if( *do_write ) {

        ThreadVector[thread_id].num_events++;

    }

}

// generic hook
void HooksTdbC::genericHook( const uint32_t & id, HooksC::VaArgsT & args ) {

    if( id == VT_UNIFY_HOOKS_TDB_GENID__STARTSTOPTIME_EPOCH ) {

        MinStartTimeEpoch = *((uint64_t*)args[0]);
        MaxStopTimeEpoch = *((uint64_t*)args[1]);

    }

}

// ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

void HooksTdbC::handleFileOperation( uint32_t *operation, uint64_t *bytes) {

    GET_THREAD_ID( thread_id );

    uint32_t fileop;
    uint32_t ioflags;

#define NFLAGS 6

    uint32_t avail_flags[NFLAGS] = { OTF_IOFLAG_IOFAILED, OTF_IOFLAG_ASYNC, OTF_IOFLAG_COLL,
                                     OTF_IOFLAG_DIRECT, OTF_IOFLAG_SYNC, OTF_IOFLAG_ISREADLOCK };

    std::vector<uint32_t> set_flags;

    /* OTF_FILEOP_{OPEN,CLOSE,READ,WRITE,SEEK...} */
    fileop = *operation & OTF_FILEOP_BITS;

    /* OTF_IOFLAGS_{ASYNC,DIRECT,SYNC,COLL...} */
    ioflags = *operation & OTF_IOFLAGS_BITS;

    for( uint32_t i = 0; i < NFLAGS; i++ ) {

        if( avail_flags[i] & ioflags ) {

            set_flags.push_back( avail_flags[i] );

        }

    }

    /* check if it has no special ioflag */
    if( set_flags.size() == 0 ) {

        /* save io without ioflag in 1 */
        set_flags.push_back(1);

    }

    /* sum all io_activity in 0 */
    set_flags.push_back(0);


    for( uint32_t i = 0; i < set_flags.size(); i++ ) {


        switch( fileop ) {

          case OTF_FILEOP_OPEN:

                ThreadVector[thread_id].io[ set_flags[i] ].num_open++;

                break;

          case OTF_FILEOP_CLOSE:

                ThreadVector[thread_id].io[ set_flags[i] ].num_close++;

                break;

          case OTF_FILEOP_READ:

                ThreadVector[thread_id].io[ set_flags[i] ].num_read++;
                ThreadVector[thread_id].io[ set_flags[i] ].bytes_read += *bytes;

                break;

          case OTF_FILEOP_WRITE:

                ThreadVector[thread_id].io[ set_flags[i] ].num_written++;
                ThreadVector[thread_id].io[ set_flags[i] ].bytes_written += *bytes;

                break;

          case OTF_FILEOP_SEEK:

                ThreadVector[thread_id].io[ set_flags[i] ].num_seek++;

                break;

        }

    }

    ThreadVector[thread_id].num_events++;

}

//////////////////// class HooksTdbC::CollOpC ////////////////////

// public methods
//

HooksTdbC::CollOpC::CollOpC()
    : num(0), bytes_sent(0), bytes_recv(0) {}

HooksTdbC::CollOpC::CollOpC( uint64_t _num, uint64_t _bytes_sent, uint64_t _bytes_recv )
    : num(_num), bytes_sent(_bytes_sent), bytes_recv(_bytes_recv) {}

HooksTdbC::CollOpC HooksTdbC::CollOpC::operator+=( const HooksTdbC::CollOpC & cs ) {

    num += cs.num;
    bytes_sent += cs.bytes_sent;
    bytes_recv += cs.bytes_recv;

    return *this;
}

//////////////////// class HooksTdbC::IoC ////////////////////

// public methods
//

HooksTdbC::IoC::IoC()
    : num_read(0), bytes_read(0), num_written(0), bytes_written(0), num_open(0),
      num_close(0), num_seek(0) {}

HooksTdbC::IoC HooksTdbC::IoC::operator+=( const HooksTdbC::IoC & is) {

    num_read += is.num_read;
    bytes_read += is.bytes_read;
    num_written += is.num_written;
    bytes_written += is.bytes_written;
    num_open += is.num_open;
    num_close += is.num_close;
    num_seek += is.num_seek;

    return *this;

}

//////////////////// class HooksTdbC::MasterDataC ////////////////////

// public methods
//

HooksTdbC::MasterDataC::MasterDataC() {

    starttime = 0;
    runtime = 0;
    filesize = 0;
    num_streams = 0;

    num_processes = 0;
    num_active_processes = 0;
    num_threads = 0;

    is_compressed = false;

    create_output = false;

    vt_flush_id = 0;

}

HooksTdbC::MasterDataC::~MasterDataC() {}

uint64_t HooksTdbC::MasterDataC::calcFilesize() {

    struct stat buf;
    std::string fname;
    std::string suffix[5] = {".events", ".def", ".stats", ".snaps", ".marker"};

    filesize = 0;

    fname = filename + ".otf";

    if( access( fname.c_str(), F_OK ) == 0 ) {

        stat( fname.c_str(), &buf );
        filesize += buf.st_size;

    }

    for( uint8_t i = 0; i < 5; i++ ) {

        fname = filename + ".";
        fname += intToHex( 0 ) + suffix[i];

        if( access( fname.c_str(), F_OK ) == 0 ) {

            stat( fname.c_str(), &buf );
            filesize += buf.st_size;

        }

    }

    for( uint32_t i = 0; i < UnifyCtls.size(); i++ ) {

        for( uint8_t j = 0; j < 5; j++ ) {

            fname = filename + ".";

            fname += intToHex( UnifyCtls[i]->streamid ) + suffix[j];

            if( access( fname.c_str(), F_OK ) == 0 ) {

                stat( fname.c_str(), &buf );
                filesize += buf.st_size;

            }

        }

    }

    return filesize;

}

std::string HooksTdbC::MasterDataC::intToHex( int i ) {

    std::ostringstream oss;
    oss << std::hex << i;

    return oss.str();

}

std::string HooksTdbC::MasterDataC::safeCwd() {

    int max_path = 4096;
    char *buf = new char[max_path];

    while( 1 ) {

      if( ! getcwd( buf, max_path ) ) {

        if( errno == ERANGE ) {

            /* buf is too small */

            /* resize buf and try again */
            max_path += 1024;

            delete [] buf;
            buf = new char[max_path];

        } else {

            /* an error occurred */

            delete [] buf;
            return NULL;

        }

      } else {

          /* all right */

          delete [] buf;
          return std::string( buf );

      }

    }

}

int HooksTdbC::MasterDataC::setFilepath( std::string file_prefix ) {

    std::string current_dir;
    std::string target_dir = ".";
    std::string absolute_dir;
    std::string fname = file_prefix;

    size_t last_slash = 0;

    last_slash = file_prefix.find_last_of( '/' );

    if( last_slash != std::string::npos ) {

        target_dir = file_prefix.substr( 0, last_slash );
        fname = file_prefix.substr( last_slash + 1 );

    }

    current_dir = safeCwd();

    if( chdir( target_dir.c_str() ) )

        return 1;

    absolute_dir = safeCwd();

    if( chdir( current_dir.c_str() ) )
        return 1;


    filepath = absolute_dir;
    filename = fname;


    return 0;

}

int HooksTdbC::MasterDataC::setHostname() {

    char name[HOST_NAME_MAX];

    if( gethostname( name, HOST_NAME_MAX ) ) {

        return 1;

    }

    hostname = std::string( name );

    return 0;

}

void HooksTdbC::MasterDataC::setOtfVersion() {

    otf_version = OTF_VERSION_MAJOR + 48;
    otf_version += ".";
    otf_version += OTF_VERSION_MINOR + 48;
    otf_version += ".";
    otf_version += OTF_VERSION_SUB + 48;
    otf_version += " \"";
    otf_version += OTF_VERSION_STRING;
    otf_version += "\"";

}

//////////////////// class HooksTdbC::ThreadDataC ////////////////////

// public methods
//

HooksTdbC::ThreadDataC::ThreadDataC()
    : num_events(0), num_enter(0), num_leave(0), num_sent(0),
      num_recv(0), bytes_sent(0), bytes_recv(0), num_rma(0),
      bytes_rma(0), num_marker(0), num_stats(0), num_snaps(0),
      num_vt_flushes(0) {}

HooksTdbC::ThreadDataC HooksTdbC::ThreadDataC::operator+=( const HooksTdbC::ThreadDataC & td ) {

    std::map<uint32_t, HooksTdbC::CollOpC>::const_iterator collop_it;
    std::map<uint32_t, HooksTdbC::IoC>::const_iterator io_it;

    num_events += td.num_events;
    num_enter += td.num_enter;
    num_leave += td.num_leave;
    num_sent += td.num_sent;
    num_recv += td.num_recv;
    bytes_sent += td.bytes_sent;
    bytes_recv += td.bytes_recv;
    num_rma += td.num_rma;
    bytes_rma += bytes_rma;
    num_marker += td.num_marker;
    num_stats += td.num_stats;
    num_snaps += td.num_snaps;
    num_vt_flushes += td.num_vt_flushes;

    for( collop_it = td.collop.begin(); collop_it != td.collop.end(); ++collop_it ) {

        collop[collop_it->first] += collop_it->second;

    }

    for( io_it = td.io.begin(); io_it != td.io.end(); ++io_it ) {

        io[io_it->first] += io_it->second;

    }

    return *this;

}

bool HooksTdbC::ThreadDataC::toBuffer( uint64_t *buf ) {

    int offset = 0;

    std::map<uint32_t, HooksTdbC::CollOpC>::const_iterator collop_it;
    std::map<uint32_t, HooksTdbC::IoC>::const_iterator io_it;

    buf[offset++] = num_enter;
    buf[offset++] = num_leave;
    buf[offset++] = num_sent;
    buf[offset++] = num_recv;
    buf[offset++] = bytes_sent;
    buf[offset++] = bytes_recv;
    buf[offset++] = num_events;
    buf[offset++] = num_rma;
    buf[offset++] = bytes_rma;
    buf[offset++] = num_marker;
    buf[offset++] = num_stats;
    buf[offset++] = num_snaps;
    buf[offset++] = num_vt_flushes;

    for( collop_it = collop.begin(); collop_it != collop.end(); ++collop_it ) {

        buf[ offset++ ] = (uint64_t) collop_it->first;
        buf[ offset++ ] = collop_it->second.num;
        buf[ offset++ ] = collop_it->second.bytes_sent;
        buf[ offset++ ] = collop_it->second.bytes_recv;

    }

    for( io_it = io.begin(); io_it != io.end(); ++io_it ) {

        buf[ offset++ ] = (uint64_t) io_it->first;
        buf[ offset++ ] = io_it->second.num_read;
        buf[ offset++ ] = io_it->second.bytes_read;
        buf[ offset++ ] = io_it->second.num_written;
        buf[ offset++ ] = io_it->second.bytes_written;
        buf[ offset++ ] = io_it->second.num_open;
        buf[ offset++ ] = io_it->second.num_close;
        buf[ offset++ ] = io_it->second.num_seek;

    }

    return true;

}

HooksTdbC::ThreadDataC HooksTdbC::ThreadDataC::fromBuffer( const uint64_t *buf, const uint64_t *num_bytes ) {

    uint64_t offset = 0;
    int pos;

    uint64_t max_bytes = num_bytes[0];

    num_enter = buf[ offset++ ];
    num_leave = buf[ offset++ ];
    num_sent = buf[ offset++ ];
    num_recv = buf[ offset++ ];
    bytes_sent = buf[ offset++ ];
    bytes_recv = buf[ offset++ ];
    num_events = buf[ offset++ ];
    num_rma = buf[ offset++ ];
    bytes_rma = buf[ offset++ ];
    num_marker = buf[ offset++ ];
    num_stats = buf[ offset++ ];
    num_snaps = buf[ offset++ ];
    num_vt_flushes = buf[ offset++ ];

    max_bytes += num_bytes[1];

    while( offset < max_bytes ) {

        pos = offset++;

        collop[ buf[ pos ] ].num = buf[ offset++ ];
        collop[ buf[ pos ] ].bytes_sent = buf[ offset++ ];
        collop[ buf[ pos ] ].bytes_recv = buf[ offset++ ];

    }

    max_bytes += num_bytes[2];

    while( offset < max_bytes ) {

        pos = offset++;

        io[ buf[ pos ] ].num_read = buf[ offset++ ];
        io[ buf[ pos ] ].bytes_read = buf[ offset++ ];
        io[ buf[ pos ] ].num_written = buf[ offset++ ];
        io[ buf[ pos ] ].bytes_written = buf[ offset++ ];
        io[ buf[ pos ] ].num_open = buf[ offset++ ];
        io[ buf[ pos ] ].num_close = buf[ offset++ ];
        io[ buf[ pos ] ].num_seek = buf[ offset++ ];

    }

    return *this;

}
