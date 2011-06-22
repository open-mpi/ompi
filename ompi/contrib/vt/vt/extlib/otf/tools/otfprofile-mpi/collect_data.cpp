/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

using namespace std;

#include <cassert>
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "otf.h"
#include "otfaux.h"

#include "mpi.h"

#include "collect_data.h"


/* logarithm to base b for unsigned 64-bit integer x */
static uint64_t logi( uint64_t x, uint64_t b= 2 ) {

    assert( b > 1 );

    uint64_t c= 1;
    uint64_t i= 0;

    while( c <= x ) {

        c*= b;
        i++;
    }

    return i;
}


/* definition record handler functions */

static int handle_def_creator( void* fha, uint32_t stream, const char* creator,
               OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->creator= creator;

    return OTF_RETURN_OK;
}


static int handle_def_version( void* fha, uint32_t stream,
               uint8_t major, uint8_t minor, uint8_t sub, const char* suffix,
               OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    ostringstream version;
    version << (int)major << "." << (int)minor;
    if ( sub > 0 ) {

        version << "." << (int)sub;

    }
    version << suffix;

    alldata->version= version.str();

    return OTF_RETURN_OK;
}


static int handle_def_comment( void* fha, uint32_t stream, const char* comment,
               OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    if ( 0 < alldata->comments.length() ) {

        alldata->comments+= "\n";

    }
    alldata->comments+= comment;

    return OTF_RETURN_OK;
}


static int handle_def_timerres( void* fha, uint32_t stream,
               uint64_t ticksPerSecond, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->timerResolution= ticksPerSecond;

    return OTF_RETURN_OK;
}


static int handle_def_process( void* fha, uint32_t stream, uint32_t process,
               const char* name, uint32_t parent, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->allProcesses.insert( Process( process, parent ) );
    alldata->processIdNameMap[process]= name;

    return OTF_RETURN_OK;
}


static int handle_def_function( void* fha, uint32_t stream, uint32_t function,
               const char* name, uint32_t funcGroup, uint32_t source,
               OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->functionIdNameMap[function]= name;

    return OTF_RETURN_OK;
}


static int handle_def_collop( void* fha, uint32_t stream, uint32_t collOp,
               const char* name, uint32_t type, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->collectiveOperationsToClasses[collOp]= type;

    return OTF_RETURN_OK;
}


static int handle_def_counter( void* fha, uint32_t stream, uint32_t counter,
               const char* name, uint32_t properties, uint32_t counterGroup,
               const char* unit, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    if ( OTF_COUNTER_TYPE_ACC == ( properties & OTF_COUNTER_TYPE_BITS ) ) {

        alldata->countersOfInterest.insert( counter );

        alldata->counterIdNameMap[counter]= name;

    }

    return OTF_RETURN_OK;
}


static int handle_def_keyvalue( void* fha, uint32_t stream, uint32_t key,
               OTF_Type type, const char* name, const char* description,
               OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    if ( 0 == strcmp( name, OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_NAME ) ) {

        alldata->recvTimeKey= key;

    }

    return OTF_RETURN_OK;
}


/* event record handler functions */

static int handle_enter( void* fha, uint64_t time, uint32_t function,
               uint32_t process, uint32_t source, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    list<StackType>& stack= alldata->stackPerProcess[ process ];
    stack.push_back( StackType( function, time ) );

    return OTF_RETURN_OK;
}


static int handle_leave( void* fha, uint64_t time, uint32_t function,
               uint32_t process, uint32_t source, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    list<StackType>& stack= alldata->stackPerProcess[ process ];
    assert( !stack.empty() );

    StackType& top= stack.back();
    list<StackType>::reverse_iterator parent_it= ++stack.rbegin();

    uint64_t func= top.fid;
    uint64_t incl= time - top.timestamp;
    uint64_t excl= incl - top.childDuration;

    if ( parent_it != stack.rend() ) {

        parent_it->childDuration += incl;

    }


    for ( map< uint64_t, StackType::CounterData >::const_iterator it=
          top.counterIdDataMap.begin( );
          it != top.counterIdDataMap.end( ); it++ ) {

        const uint64_t& counter= it->first;
        const uint64_t& firstvalue= it->second.firstValue;
        const uint64_t& lastvalue= it->second.lastValue;
        const uint64_t& lasttime= it->second.lastTime;

        if ( lasttime == time && firstvalue != (uint64_t)-1 &&
             lastvalue != (uint64_t)-1 ) {

            uint64_t counter_incl= lastvalue - firstvalue;
            uint64_t counter_excl= counter_incl - it->second.childDelta;

            alldata->counterMapPerFunctionRank[ Triple( counter, func, process ) ]
                .add( 1, counter_excl, counter_incl );

            if ( parent_it != stack.rend() ) {

                parent_it->counterIdDataMap[ counter ].childDelta+= counter_incl;

            }

        }

    }


    stack.pop_back();

    /*
    cerr << " func " << func << " @ process " << process << ": " << 
        "excl " << excl << " ticks, incl " << incl << " ticks" << endl;
    */
    alldata->functionMapPerRank[ Pair( func, process ) ].add( 1, excl, incl );

    return OTF_RETURN_OK;
}


static int handle_counter( void* fha, uint64_t time, uint32_t process,
               uint32_t counter, uint64_t value, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    list<StackType>& stack= alldata->stackPerProcess[ process ];

    if ( stack.empty( ) ) {

        return OTF_RETURN_OK;

    }

    if ( alldata->countersOfInterest.find( counter ) ==
         alldata->countersOfInterest.end( ) ) {

        return OTF_RETURN_OK;

    }

    StackType& top= stack.back( );

    if ( time == top.timestamp ) {

        top.counterIdDataMap[ counter ].firstValue= value;

    } else {

        map< uint64_t, StackType::CounterData >::iterator it=
            top.counterIdDataMap.find( counter );

        if ( it != top.counterIdDataMap.end() ) {

            StackType::CounterData& top_counter= it->second;
            top_counter.lastValue= value;
            top_counter.lastTime= time;

        }
    }

    return OTF_RETURN_OK;
}


static int handle_send( void* fha, uint64_t time, uint32_t sender,
               uint32_t receiver, uint32_t group, uint32_t type,
               uint32_t length, uint32_t source, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    double duration= 0.0;

    /* get matching receive time from key-values, if available */

    if ( 0 != alldata->recvTimeKey ) {

        uint64_t recv_time;
        if ( OTF_KeyValueList_getUint64( kvlist, alldata->recvTimeKey,
                 &recv_time ) == 0 ) {

            duration= (double) ( recv_time - time );

        }
    }

    alldata->messageMapPerRankPair[ Pair(sender, receiver) ]
        .add_send( 1, length, duration );

    /* get message speed */

    if ( length > 0 && duration > 0.0 ) {

        uint64_t speed_bin=
            logi( (uint64_t)(
                  ( (double)length * (double)alldata->timerResolution ) /
                  duration ), MessageSpeedData::BIN_LOG_BASE );

        uint64_t length_bin= logi( length, MessageSpeedData::BIN_LOG_BASE );

        alldata->messageSpeedMapPerLength[ Pair( speed_bin, length_bin ) ]
            .add( 1 );

    }

    return OTF_RETURN_OK;
}


static int handle_recv( void* fha, uint64_t time, uint32_t receiver,
               uint32_t sender, uint32_t group, uint32_t type, uint32_t length,
               uint32_t source, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    /* duration will never be available at receive event */
    double duration= 0.0;

    alldata->messageMapPerRankPair[ Pair(receiver, sender) ]
        .add_recv( 1, length, duration );

    return OTF_RETURN_OK;
}

static int handle_begin_collop( void* fha, uint64_t time, uint32_t process,
               uint32_t collOp, uint64_t matchingId, uint32_t procGroup,
               uint32_t rootProc, uint64_t sent, uint64_t received,
               uint32_t scltoken, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->pendingCollectives[ Pair( matchingId, process ) ]=
        PendingCollective( collOp, sent, received, time );

    return OTF_RETURN_OK;
}

static int handle_end_collop( void* fha, uint64_t time, uint32_t process,
               uint64_t matchingId, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    /* get corresponding pending collective operation */

    map< Pair, PendingCollective, ltPair >::iterator pending_it=
        alldata->pendingCollectives.find( Pair( matchingId, process ) );
    assert( pending_it != alldata->pendingCollectives.end() );

    const PendingCollective& pending= pending_it->second;

    /* get class of collective operation */

    map< uint64_t, uint64_t >::const_iterator op_class_it=
        alldata->collectiveOperationsToClasses.find( pending.collop );
    assert( op_class_it != alldata->collectiveOperationsToClasses.end() );

    const uint64_t& op_class= op_class_it->second;

    /* calculate duration */
    double duration= (double) ( time - pending.begin_time );

    /* add collective operation to statistics */

    if ( OTF_COLLECTIVE_TYPE_BARRIER == op_class ) {

        alldata->collectiveMapPerRank[ Pair( op_class, process ) ]
            .add_send( 1, 0, duration );
        alldata->collectiveMapPerRank[ Pair( op_class, process ) ]
            .add_recv( 1, 0, duration );

    } else {

        if ( 0 < pending.bytes_send ) {

            alldata->collectiveMapPerRank[ Pair( op_class, process ) ]
                .add_send( 1, pending.bytes_send, duration );

        }
        if ( 0 < pending.bytes_recv ) {

            alldata->collectiveMapPerRank[ Pair( op_class, process ) ]
                .add_recv( 1, pending.bytes_recv, duration );

        }

    }

    /* erase processed pending collective operation from map */
    alldata->pendingCollectives.erase( pending_it );

    return OTF_RETURN_OK;
}


static int handle_function_summary( void* fha, uint64_t time, uint32_t func,
               uint32_t process, uint64_t count, uint64_t exclTime,
               uint64_t inclTime, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    /* add/overwrite function statistics */

    FunctionData tmp;
    tmp.add( count, exclTime, inclTime );
    alldata->functionMapPerRank[ Pair( func, process ) ]= tmp;

    return OTF_RETURN_OK;
}


static int handle_message_summary( void* fha, uint64_t time, uint32_t process,
               uint32_t peer, uint32_t comm, uint32_t type, uint64_t sentNumber,
               uint64_t receivedNumber, uint64_t sentBytes,
               uint64_t receivedBytes, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    /* do handle this record only if there is a peer and no
    communicator and tag (default behavior of VampirTrace) */
    if ( 0 != peer && 0 == comm && 0 == type ) {

        /* add/overwrite message statistics */

        MessageData tmp;

        if ( 0 < sentNumber ) {

            tmp.count_send.cnt= tmp.count_send.sum= sentNumber;
            tmp.count_send.min= tmp.count_send.max= 0;

            tmp.bytes_send.cnt= sentNumber;
            tmp.bytes_send.sum= sentBytes;
            tmp.bytes_send.min= tmp.bytes_send.max= 0;

        }
        if ( 0 < receivedNumber ) {

            tmp.count_recv.cnt= tmp.count_recv.sum= receivedNumber;
            tmp.count_recv.min= tmp.count_recv.max= 0;

            tmp.bytes_recv.cnt= receivedNumber;
            tmp.bytes_recv.sum= receivedBytes;
            tmp.bytes_recv.min= tmp.bytes_recv.max= 0;

        }

        alldata->messageMapPerRankPair[ Pair(process, peer) ]= tmp;

    }

    return OTF_RETURN_OK;
}


static int handle_collop_summary( void* fha, uint64_t time, uint32_t process,
               uint32_t comm, uint32_t collOp, uint64_t sentNumber,
               uint64_t receivedNumber, uint64_t sentBytes,
               uint64_t receivedBytes, OTF_KeyValueList* kvlist ) {


    AllData* alldata= (AllData*) fha;


    /* do handle this record only if there is a coll.-op and no communicator
    (default behavior of VampirTrace) */
    if ( 0 != collOp && 0 == comm ) {

        /* get class of collective operation */

        map< uint64_t, uint64_t >::const_iterator op_class_it=
            alldata->collectiveOperationsToClasses.find( collOp );
        assert( op_class_it != alldata->collectiveOperationsToClasses.end() );

        const uint64_t& op_class= op_class_it->second;

        /* add/overwrite collective operation statistics */

        CollectiveData tmp;

        if ( 0 < sentNumber ) {

            tmp.count_send.cnt= tmp.count_send.sum= sentNumber;
            tmp.count_send.min= tmp.count_send.max= 0;

            tmp.bytes_send.cnt= sentNumber;
            tmp.bytes_send.sum= sentBytes;
            tmp.bytes_send.min= tmp.bytes_send.max= 0;

        }
        if ( 0 < receivedNumber ) {

            tmp.count_recv.cnt= tmp.count_recv.sum= receivedNumber;
            tmp.count_recv.min= tmp.count_recv.max= 0;

            tmp.bytes_recv.cnt= receivedNumber;
            tmp.bytes_recv.sum= receivedBytes;
            tmp.bytes_recv.min= tmp.bytes_recv.max= 0;

        }

        alldata->collectiveMapPerRank[ Pair( op_class, process ) ]= tmp;

    }

    return OTF_RETURN_OK;
}


static void read_definitions( OTF_Reader* reader, AllData& alldata ) {

    /* open OTF handler array */
    OTF_HandlerArray* handlers= OTF_HandlerArray_open( );
    assert( handlers );

    /* set record handler functions */

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_creator,
        OTF_DEFCREATOR_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_version,
        OTF_DEFVERSION_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_comment,
        OTF_DEFINITIONCOMMENT_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_timerres,
        OTF_DEFTIMERRESOLUTION_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_process,
        OTF_DEFPROCESS_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_function,
        OTF_DEFFUNCTION_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_collop,
        OTF_DEFCOLLOP_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_counter,
        OTF_DEFCOUNTER_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_keyvalue,
        OTF_DEFKEYVALUE_RECORD );

    /* set record handler's first arguments */

    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFCREATOR_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFVERSION_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFINITIONCOMMENT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFTIMERRESOLUTION_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFPROCESS_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFFUNCTION_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFCOUNTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFKEYVALUE_RECORD );

    /* read definitions */
    uint64_t defs_read_ret= OTF_Reader_readDefinitions( reader, handlers );
    assert( OTF_READ_ERROR != defs_read_ret );

    /* close OTF handler array */
    OTF_HandlerArray_close( handlers );
}


static void share_definitions( uint32_t my_rank, uint32_t num_ranks,
                               AllData& alldata ) {

    MPI_Barrier( MPI_COMM_WORLD );

    char* buffer;
    int buffer_size= 0;
    int buffer_pos= 0;

    /* get size needed to send definitions to workers */

    if ( my_rank == 0 ) {

        MPI_Pack_size( 1 + alldata.collectiveOperationsToClasses.size() * 2 +
                       1 + alldata.countersOfInterest.size() +
                       1 /* timerResolution */ +
                       1 /* recvTimeKey */,
                       MPI_LONG_LONG_INT, MPI_COMM_WORLD, &buffer_size );

    }

    /* broadcast buffer size */
    MPI_Bcast( &buffer_size, 1, MPI_INT, 0, MPI_COMM_WORLD );

    /* allocate buffer */
    buffer= new char[ buffer_size ];
    assert( buffer );

    /* pack definitions to buffer */

    if ( my_rank == 0 ) {

        /* collectiveOperationsToClasses.size() */
        uint64_t collop_classes_map_size=
            alldata.collectiveOperationsToClasses.size();
        MPI_Pack( &collop_classes_map_size, 1, MPI_LONG_LONG_INT, buffer,
                  buffer_size, &buffer_pos, MPI_COMM_WORLD );

        /* collectiveOperationsToClasses */
        for ( map< uint64_t, uint64_t >::const_iterator it =
              alldata.collectiveOperationsToClasses.begin();
              it != alldata.collectiveOperationsToClasses.end(); it++ ) {

            /* collectiveOperationsToClasses.first */
            uint64_t first= it->first;
            MPI_Pack( &first, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );
            /* collectiveOperationsToClasses.second */
            uint64_t second= it->second;
            MPI_Pack( &second, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

        }

        /* countersOfInterest.size() */
        uint64_t counters_size= alldata.countersOfInterest.size();
        MPI_Pack( &counters_size, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                  &buffer_pos, MPI_COMM_WORLD );

        /* countersOfInterest */
        for ( set< uint64_t >::const_iterator it=
              alldata.countersOfInterest.begin();
              it != alldata.countersOfInterest.end(); it++ ) {

            uint64_t counter= *it;
            MPI_Pack( &counter, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

        }

        /* recvTimeKey */
        MPI_Pack( &(alldata.recvTimeKey), 1, MPI_LONG_LONG_INT, buffer,
                  buffer_size, &buffer_pos, MPI_COMM_WORLD );

        /* timerResolution */
        MPI_Pack( &(alldata.timerResolution), 1, MPI_LONG_LONG_INT, buffer,
                  buffer_size, &buffer_pos, MPI_COMM_WORLD );

    }

    /* broadcast definitions buffer */
    MPI_Bcast( buffer, buffer_size, MPI_PACKED, 0, MPI_COMM_WORLD );

    /* unpack definitions from buffer */

    if ( my_rank != 0 ) {

        /* collectiveOperationsToClasses.size() */
        uint64_t collop_classes_map_size;
        MPI_Unpack( buffer, buffer_size, &buffer_pos,
                    &collop_classes_map_size, 1, MPI_LONG_LONG_INT,
                    MPI_COMM_WORLD );

        /* collectiveOperationsToClasses */
        for ( uint64_t i= 0; i < collop_classes_map_size; i++ ) {

            /* collectiveOperationsToClasses.first */
            uint64_t first;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &first, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            /* collectiveOperationsToClasses.second */
            uint64_t second;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &second, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.collectiveOperationsToClasses[ first ]= second;

        }

        /* countersOfInterest.size() */
        uint64_t counters_size;
        MPI_Unpack( buffer, buffer_size, &buffer_pos,
                    &counters_size, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

        /* countersOfInterest */
        for ( uint64_t i= 0; i < counters_size; i++ ) {

            uint64_t counter;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &counter, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.countersOfInterest.insert( counter );

        }

        /* recvTimeKey */
        MPI_Unpack( buffer, buffer_size, &buffer_pos, &(alldata.recvTimeKey),
                    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

        /* timerResolution */
        MPI_Unpack( buffer, buffer_size, &buffer_pos,
                    &(alldata.timerResolution), 1, MPI_LONG_LONG_INT,
                    MPI_COMM_WORLD );

    }

    delete[] buffer;
}


static void read_events( OTF_Reader* reader, AllData& alldata ) {

    /* open OTF handler array */
    OTF_HandlerArray* handlers= OTF_HandlerArray_open( );
    assert( handlers );

    /* set record handler functions */

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_enter,
        OTF_ENTER_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_leave,
        OTF_LEAVE_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_counter,
        OTF_COUNTER_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_send,
        OTF_SEND_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_recv,
        OTF_RECEIVE_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_begin_collop,
        OTF_BEGINCOLLOP_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_end_collop,
        OTF_ENDCOLLOP_RECORD );

    /* set record handler's first arguments */

    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_ENTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_LEAVE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_COUNTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_SEND_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_RECEIVE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_BEGINCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_ENDCOLLOP_RECORD );

    /* select processes to read */
    OTF_Reader_setProcessStatusAll( reader, 0 );
    for ( uint32_t i= 0; i < alldata.myProcessesNum; i++ ) {

        OTF_Reader_enableProcess( reader, alldata.myProcessesList[ i ] );
    }

    /* read events */
    uint64_t events_read_ret= OTF_Reader_readEvents( reader, handlers );
    assert( OTF_READ_ERROR != events_read_ret );

    /* close OTF handler array */
    OTF_HandlerArray_close( handlers );
}


static void read_statistics( OTF_Reader* reader, AllData& alldata ) {

    /* open OTF handler array */
    OTF_HandlerArray* handlers= OTF_HandlerArray_open( );
    assert( handlers );

    /* set record handler functions */

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_function_summary,
        OTF_FUNCTIONSUMMARY_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_message_summary,
        OTF_MESSAGESUMMARY_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_collop_summary,
        OTF_COLLOPSUMMARY_RECORD );

    /* set record handler's first arguments */

    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_FUNCTIONSUMMARY_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_MESSAGESUMMARY_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_COLLOPSUMMARY_RECORD );

    /* select processes to read */
    OTF_Reader_setProcessStatusAll( reader, 0 );
    for ( uint32_t i= 0; i < alldata.myProcessesNum; i++ ) {

        OTF_Reader_enableProcess( reader, alldata.myProcessesList[ i ] );
    }

    /* read events */
    uint64_t stats_read_ret= OTF_Reader_readStatistics( reader, handlers );
    assert( OTF_READ_ERROR != stats_read_ret );

    /* close OTF handler array */
    OTF_HandlerArray_close( handlers );
}


bool collectData( uint32_t my_rank, uint32_t num_ranks, AllData& alldata ) {

    bool ret= true;

    /* open OTF file manager and reader */

    OTF_FileManager* manager=
        OTF_FileManager_open( alldata.params.max_file_handles );
    assert( manager );

    OTF_Reader* reader=
        OTF_Reader_open( alldata.params.input_file_prefix.c_str(), manager );
    assert( reader );

    if ( my_rank == 0 ) {

        /* read definitions */
        read_definitions( reader, alldata );

    }

    /* share definitions needed for reading events to workers */

    if ( num_ranks > 1 ) {

        share_definitions( my_rank, num_ranks, alldata );

    }

    /* either read data from events or statistics */

    if ( alldata.params.read_from_stats ) {

        read_statistics( reader, alldata );

    } else {

        read_events( reader, alldata );

    }

    /* close OTF file manager and reader */

    OTF_Reader_close( reader );
    OTF_FileManager_close( manager );

    return ret;
}
