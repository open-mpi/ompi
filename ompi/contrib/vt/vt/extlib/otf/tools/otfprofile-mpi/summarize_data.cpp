/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

using namespace std;

#include <cassert>
#include <iostream>

#include "summarize_data.h"


static void get_clustering( AllData& alldata ) {

    uint32_t r_processes= alldata.allProcesses.size();
    uint32_t r_clusters= Clustering::MAX_CLUSTERS;

    set< Process, ltProcess >::iterator pos= alldata.allProcesses.begin();

    for ( uint32_t c= 0;
          c < Clustering::MAX_CLUSTERS && 0 < r_processes; c++ ) {

        uint32_t n=
            ( ( r_processes / r_clusters ) * r_clusters < r_processes ) ?
            ( r_processes / r_clusters + 1 ) : ( r_processes / r_clusters );

        for ( uint32_t i= 0; i < n; i++ ) {

            bool inserted= alldata.clustering.insert( c+1, pos->process );
            assert( inserted );

            pos++;
            r_processes--;

        }

        r_clusters--;

    }
}


static void share_clustering( AllData& alldata ) {

    MPI_Barrier( MPI_COMM_WORLD );

    char* buffer;
    int buffer_size= 0;
    int buffer_pos= 0;

    if ( 0 == alldata.myRank ) {

        /* get size needed to send clustering information to workers */

        int size;

        /* alldata.clustering.clustersToProcesses.size() + firsts */
        MPI_Pack_size( 1 + alldata.clustering.clustersToProcesses.size(),
                       MPI_LONG_LONG_INT, MPI_COMM_WORLD, &size );
        buffer_size+= size;

        /* alldata.clustering.clustersToProcesses.second.size() + second */
        for ( map< uint64_t, set<uint64_t> >::const_iterator it=
              alldata.clustering.clustersToProcesses.begin();
              it != alldata.clustering.clustersToProcesses.end(); it++ ) {

            MPI_Pack_size( 1 + it->second.size(), MPI_LONG_LONG_INT,
                           MPI_COMM_WORLD, &size );
            buffer_size+= size;

        }

    }

    /* broadcast buffer size */
    MPI_Bcast( &buffer_size, 1, MPI_INT, 0, MPI_COMM_WORLD );

    /* allocate buffer */
    buffer= new char[ buffer_size ];
    assert( buffer );

    /* pack clustering information to buffer */

    if ( 0 == alldata.myRank ) {

        /* alldata.clustering.clustersToProcesses.size() */
        uint64_t clust_proc_map_size=
            alldata.clustering.clustersToProcesses.size();
        MPI_Pack( &clust_proc_map_size, 1, MPI_LONG_LONG_INT, buffer,
                  buffer_size, &buffer_pos, MPI_COMM_WORLD );

        /* alldata.clustering.clustersToProcesses */
        for ( map< uint64_t, set<uint64_t> >::const_iterator it=
              alldata.clustering.clustersToProcesses.begin();
              it != alldata.clustering.clustersToProcesses.end(); it++ ) {

            /* alldata.clustering.clustersToProcesses.first */
            uint64_t cluster= it->first;
            MPI_Pack( &cluster, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

            /* alldata.clustering.clustersToProcesses.second.size() */
            uint64_t processes_size= it->second.size();
            MPI_Pack( &processes_size, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

            /* alldata.clustering.clustersToProcesses.second */
            for ( set<uint64_t>::const_iterator it2= it->second.begin();
                  it2 != it->second.end(); it2++ ) {

                uint64_t process= *it2;
                MPI_Pack( &process, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                          &buffer_pos, MPI_COMM_WORLD );

            }

        }

    }

    /* broadcast definitions buffer */
    MPI_Bcast( buffer, buffer_size, MPI_PACKED, 0, MPI_COMM_WORLD );

    /* unpack clustering information from buffer */

    if ( 0 != alldata.myRank ) {

        /* alldata.clustering.clustersToProcesses.size() */
        uint64_t clust_proc_map_size;
        MPI_Unpack( buffer, buffer_size, &buffer_pos, &clust_proc_map_size, 1,
                    MPI_LONG_LONG_INT, MPI_COMM_WORLD );

        /* alldata.clustering.clustersToProcesses */
        for ( uint64_t i= 0; i < clust_proc_map_size; i++ ) {

            /* alldata.clustering.clustersToProcesses.first */
            uint64_t cluster;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &cluster, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            /* alldata.clustering.clustersToProcesses.second.size() */
            uint64_t processes_size;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &processes_size, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            /* alldata.clustering.clustersToProcesses.second */
            for ( uint64_t j= 0; j < processes_size; j++ ) {

                uint64_t process;
                MPI_Unpack( buffer, buffer_size, &buffer_pos, &process, 1,
                            MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                bool inserted= alldata.clustering.insert( cluster, process );
                assert( inserted );

            }

        }

    }

    delete[] buffer;
}


bool SummarizeData( AllData& alldata ) {

    bool error= false;

    /* rank 0 gets clustering information */

    if ( 0 == alldata.myRank ) {

        get_clustering( alldata );

    }

    /* share clustering information to workers */

    if ( 1 < alldata.numRanks ) {

        share_clustering( alldata );

    }

    /* macro to set min, max to sum before summarizing */
#   define MINMAX2SUM(v) \
    if( 0 != (v).cnt ) { \
        (v).cnt = 1; \
        (v).min= (v).max= (v).sum; \
    } else { \
        (v).cnt = 0; \
        /* (v).min= OTF_UINT64_MAX; (v).max= 0; \
           ^^^ this is set already by the constructor and never touched \
           if (v).cnt == 0. Therefore, it is ignored when computing min/max \
           further on. */ \
    }

    /* summarize map ( func x rank ) to map ( func ) */
    {
        map< Pair, FunctionData, ltPair >::iterator it= alldata.functionMapPerRank.begin();
        map< Pair, FunctionData, ltPair >::iterator itend= alldata.functionMapPerRank.end();
        while ( itend != it ) {

            alldata.functionMapGlobal[ it->first.a ].add( it->second );
            it++;
        }
        alldata.functionMapPerRank.clear();
    }

    /* summarize map ( counter x func x rank ) to map ( counter x func ) */
    {
        map< Triple, CounterData, ltTriple >::iterator it= alldata.counterMapPerFunctionRank.begin();
        map< Triple, CounterData, ltTriple >::iterator itend= alldata.counterMapPerFunctionRank.end();
        while ( itend != it ) {

            alldata.counterMapGlobal[ Pair( it->first.a, it->first.b ) ].add( it->second );
            it++;
        }
        alldata.counterMapPerFunctionRank.clear();
    }

    /* will be generated from messageMapPerRankPair, is only used to generate
    messageMapPerCluster */
    map< uint64_t, MessageData > message_map_per_rank;

    /* summarize map ( rank x rank ) to map ( cluster x cluster ) */
    {
        map< Pair, MessageData, ltPair >::iterator it= alldata.messageMapPerRankPair.begin();
        map< Pair, MessageData, ltPair >::iterator itend= alldata.messageMapPerRankPair.end();
        while ( itend != it ) {

            uint64_t cluster_a= it->first.a;
            uint64_t cluster_b= it->first.b;

            message_map_per_rank[ cluster_a ].add( it->second );

            if ( alldata.clustering.enabled ) {

                cluster_a= alldata.clustering.process2cluster( it->first.a );
                assert( 0 != cluster_a );
                cluster_b= alldata.clustering.process2cluster( it->first.b );
                assert( 0 != cluster_b );

            }

            MINMAX2SUM( it->second.count_send );
            MINMAX2SUM( it->second.count_recv );
            MINMAX2SUM( it->second.bytes_send );
            MINMAX2SUM( it->second.bytes_recv );
            MINMAX2SUM( it->second.duration_send );
            MINMAX2SUM( it->second.duration_recv );

            alldata.messageMapPerClusterPair[ Pair( cluster_a, cluster_b ) ].add( it->second );
            it++;
        }
        alldata.messageMapPerRankPair.clear();
    }

    /* summarize map ( rank ) to map ( cluster ) */
    {
        map< uint64_t, MessageData >::iterator it= message_map_per_rank.begin();
        map< uint64_t, MessageData >::iterator itend= message_map_per_rank.end();
        while ( itend != it ) {

            uint64_t cluster= it->first;

            if ( alldata.clustering.enabled ) {

                cluster= alldata.clustering.process2cluster( it->first );
                assert( 0 != cluster );

            }

            MINMAX2SUM( it->second.count_send );
            MINMAX2SUM( it->second.count_recv );
            MINMAX2SUM( it->second.bytes_send );
            MINMAX2SUM( it->second.bytes_recv );
            MINMAX2SUM( it->second.duration_send );
            MINMAX2SUM( it->second.duration_recv );

            alldata.messageMapPerCluster[ cluster ].add( it->second );
            it++;
        }
        message_map_per_rank.clear();
    }

    /* summarize map ( class x rank ) to map ( class x cluster ) */
    {
        map< Pair, CollectiveData, ltPair >::iterator it= alldata.collectiveMapPerRank.begin();
        map< Pair, CollectiveData, ltPair >::iterator itend= alldata.collectiveMapPerRank.end();
        while ( itend != it ) {

            const uint64_t& op_class= it->first.a;
            uint64_t cluster= it->first.b;

            if ( alldata.clustering.enabled ) {

                cluster= alldata.clustering.process2cluster( it->first.b );
                assert( 0 != cluster );

            }

            MINMAX2SUM( it->second.count_send );
            MINMAX2SUM( it->second.count_recv );
            MINMAX2SUM( it->second.bytes_send );
            MINMAX2SUM( it->second.bytes_recv );
            MINMAX2SUM( it->second.duration_send );
            MINMAX2SUM( it->second.duration_recv );

            alldata.collectiveMapPerCluster[ Pair( op_class, cluster ) ].add( it->second );
            it++;
        }
        alldata.collectiveMapPerRank.clear();
    }

    return !error;
}
