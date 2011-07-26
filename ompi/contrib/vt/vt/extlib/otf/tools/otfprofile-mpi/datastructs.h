/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#ifndef DATASTRUCTS_H
#define DATASTRUCTS_H


using namespace std;

#include <stdlib.h>
#include <cassert>

#include <string>
#include <map>
#include <list>
#include <set>

#include "mpi.h"

#include "OTF_inttypes.h"


/* *** program parameters *** */

struct Params {

    static const uint32_t DEFAULT_MAX_FILE_HANDLES= 50;
    static const uint32_t DEFAULT_BUFFER_SIZE= 1024 * 1024;
    static const uint8_t  DEFAULT_VERBOSE_LEVEL= 0;
    static const string   DEFAULT_OUTPUT_FILE_PREFIX() { return "result"; }

    uint32_t max_file_handles;
    uint32_t buffer_size;
    uint8_t  verbose_level;
    bool     progress;
    bool     read_from_stats;

    bool     create_pdf;
    string   input_file_prefix;
    string   output_file_prefix;

    Params()
        : max_file_handles(DEFAULT_MAX_FILE_HANDLES),
          buffer_size(DEFAULT_BUFFER_SIZE),
          verbose_level(DEFAULT_VERBOSE_LEVEL), progress(false),
          read_from_stats(false), create_pdf(true),
          output_file_prefix(DEFAULT_OUTPUT_FILE_PREFIX()) {}
};


/* *** progress information *** */

struct Progress {

    /* maximum number of records to read between progress updates */
    static const uint64_t EVENTS_RECORD_LIMIT= 1000000;
    static const uint64_t STATS_RECORD_LIMIT= 100;

    /* message tag to use for communication */
    static const int      MSG_TAG= 500;

    uint64_t     cur_bytes;      /* current bytes read */
    uint64_t     max_bytes;      /* max. bytes readable */

    MPI_Request  send_request;   /* sender request handle */

    uint64_t*    recv_buffers;   /* receive buffers */
    MPI_Request* recv_requests;  /* persistent receive request handles */
    MPI_Status*  recv_statuses;  /* receive statuses */
    int*         recv_indices;   /* indices of completed recv. operations */

    uint64_t*    rank_cur_bytes; /* current bytes read per rank (except rank 0) */
    uint32_t     ranks_left;     /* root keeps track of ranks left to query */
};


/* *** runtime measurement *** */

struct MeasureBlock {

   /* routine to get a global timestamp */
#  define GETTIME() MPI_Wtime()

   double start_time; /* start timestamp of measurement block */
   double stop_time;  /* stop timestamp of measurement block */

   MeasureBlock() : start_time(-1.0), stop_time(-1.0) {}

   /* start runtime measurment */
   void start() {

       start_time= GETTIME();
   }

   /* stop runtime measurment */
   void stop() {

       assert( -1.0 != start_time );
       stop_time= GETTIME();
   }

   /* get result of runtime measurement */
   double duration() const {

      assert( -1.0 != start_time && -1.0 != stop_time );
      return stop_time - start_time;
   }
};


/* *** pair of values as map key *** */

struct Pair {

    uint64_t a;
    uint64_t b;

    Pair() : a(0), b(0) {}
    Pair( uint64_t aa, uint64_t bb ) : a(aa), b(bb) {}
    ~Pair() {}
};


struct ltPair {

    bool operator()( const Pair& p1, const Pair& p2 ) const {

        /* a is the major number for comparison, this gives a better 
        order when reducing the entries over the first argument */

        if ( p1.a == p2.a ) {

            return p1.b < p2.b;

        } else {

            return p1.a < p2.a;
        }
    }
};


/* *** triplett of values as map key *** */

struct Triple {

    uint64_t a;
    uint64_t b;
    uint64_t c;

    Triple() : a(0), b(0), c(0) {}
    Triple( uint64_t aa, uint64_t bb, uint64_t cc ) : a(aa), b(bb), c(cc) {}
    ~Triple() {}
};


struct ltTriple {

    bool operator()( const Triple& p1, const Triple& p2 ) const {

        /* a is the major number for comparison, this gives a better 
        order when reducing the entries over the first argument */

        if ( p1.a == p2.a ) {

            if ( p1.b == p2.b ) {

                return p1.c < p2.c;

            } else {

                return p1.b < p2.b;
            }

        } else {

            return p1.a < p2.a;
        }
    }
};


struct Process {

    uint64_t process;
    uint64_t parent;

    Process() : process(0), parent(0) {}
    Process( uint64_t _process, uint64_t _parent )
        : process(_process), parent(_parent) {}
    ~Process() {}
};


struct ltProcess {

    bool operator()( const Process& p1, const Process& p2 ) const {

        /* involve parent for sorting? */
        return p1.process < p2.process;
    }
};


/* class that collects the minimum, the maximum, and the sum for some values. 
the minimum will ignore the value '0' though because it should only record 
actual occurences */
template <class type>
class min_max_avg {

public:

    type min;
    type max;
    type sum;
    uint64_t cnt;

    min_max_avg( type a= (type) OTF_UINT64_MAX, type b= (type) 0, type s= (type) 0, uint64_t c= 0 ) : 
            min( a ), max( b ), sum( s ), cnt( c ) {}
    ~min_max_avg() {}

    /* append a single value */
    void append( const type& value ) {

        if ( ((type) 0) != value ) {

            min= ( value < min ) ? value : min;
            max= ( value > max ) ? value : max;
            sum += value;
            cnt += 1;
        }
    }

    /* add another min_max_avg object as if all their values were appended to on object */
    void add( const min_max_avg<type>& other ) {

        min= ( other.min < min ) ? other.min : min;
        max= ( other.max > max ) ? other.max : max;
        sum += other.sum;
        cnt += other.cnt;
    }
};


/* manage clustering of processes (or ranks/threads/whatever) -- clustering 
reduces the potentially unlimited numbers of processes to a fixed number of 
clusters (or bins, groups, buckets, ... ). Instead of a per-process basis
global statisitics are collected per cluster. The processes can be spread 
over the clusters either consecutively, or round-robin, or randomly, or in
any special scheme, e.g. separate clusters for GPU theads and host processes.
Therefore, the Clustering structure manages explicit mappings from cluster IDs to
process IDs. Every process belongs to one cluster exclusively. */

struct Clustering {

    static const uint32_t MAX_CLUSTERS= 16;

    map< uint64_t, uint64_t > processesToClusters;
    map< uint64_t, set<uint64_t> > clustersToProcesses;

    bool enabled;


    Clustering() : enabled( false ) {}
    ~Clustering() {}

    /* insert process into a cluster, return true if succeeded */
    bool insert( uint64_t cluster, uint64_t process ) {

        /* insert the new entry if and only if there was no process with this ID before, 
        because every process can only be in one cluster */

        pair< map< uint64_t, uint64_t >::const_iterator, bool> ret= 
            processesToClusters.insert( pair< uint64_t, uint64_t >( process, cluster ) );

        if ( ret.second ) {

            clustersToProcesses[ cluster ].insert( process );

            /* set indicator that clustering is enabled, if there are more than
            one process within a cluster */
            if ( !enabled && 1 < clustersToProcesses[ cluster ].size() ) {

                enabled= true;
            }

            return true;
        }

        return false;
    }

    /* return the cluster ID for the given process ID, return 0 if not found */
    uint64_t process2cluster( uint64_t process ) const {

        map< uint64_t, uint64_t >::const_iterator it= processesToClusters.find( process );

        return ( processesToClusters.end() != it ) ? it->second : ( uint64_t) 0 ;
    }

    /* return a const pointer to the set or NULL if there is no such cluster,
    this is better than the [] operator which would create an empty set if 
    a search goes negative */
    const set<uint64_t>* cluster2processes( uint64_t cluster ) const {

        map< uint64_t, set<uint64_t> >::const_iterator it= clustersToProcesses.find( cluster );

        return ( clustersToProcesses.end() != it ) ? ( & it->second ) : NULL ;
    }

    /* return number of clusters */
    uint32_t numClusters( ) const {

        return clustersToProcesses.size();
    }
};


struct StackType {

    /* the function ID */
    uint64_t fid;

    /* the enter timestamp when this was pushed to the top-of-stack */
    uint64_t timestamp;

    /* the duration consumed by child calls, it is the sum of all child call's
    inclusive durations, with this one can compute the exclusive durations of
    the currrent call based on the inclusive time which comes from the end
    timestamps minus the start timestamp. */
    uint64_t childDuration;

    struct CounterData {

        /* the first counter value relevant to the enter timestamp */
        uint64_t firstValue;

        /* the last counter value */
        uint64_t lastValue;

        /* the timestamp on which the last counter value occurred */
        uint64_t lastTime;

        /* similar to childDuration but for the counter values */
        uint64_t childDelta;

        CounterData()
            : firstValue( (uint64_t)-1 ), lastValue( (uint64_t)-1 ),
              lastTime( (uint64_t)-1 ), childDelta( 0 ) {}
    };

    /* the counter data on this stack level */
    map< uint64_t, CounterData > counterIdDataMap;

    StackType( uint64_t f, uint64_t t )
        : fid( f ), timestamp( t ), childDuration( 0 ) {}
    ~StackType() {}
};


struct FunctionData {

    min_max_avg<uint64_t> count;
    min_max_avg<double> excl_time;
    min_max_avg<double> incl_time;

    FunctionData( ) {}
    ~FunctionData( ) {}

    void add( uint64_t n= 0, double ex= 0.0, double in= 0.0 ) {

        count.append( n );
        excl_time.append( ex );
        incl_time.append( in );
    }

    void add( const FunctionData& other ) {

        count.add( other.count );
        excl_time.add( other.excl_time );
        incl_time.add( other.incl_time );
    }
};


/* counter data are similar to function data */
typedef FunctionData CounterData;


struct MessageData {

    min_max_avg<uint64_t> count_send;
    min_max_avg<uint64_t> count_recv;
    min_max_avg<uint64_t> bytes_send;
    min_max_avg<uint64_t> bytes_recv;
    min_max_avg<double>   duration_send;
    min_max_avg<double>   duration_recv;


    MessageData( ) {}
    ~MessageData( ) {}

    void add_send( uint64_t n= 0, uint64_t b= 0, double d= 0.0 ) {

        count_send.append( n );
        bytes_send.append( b );
        duration_send.append( d );
    }

    void add_recv( uint64_t n= 0, uint64_t b= 0, double d= 0.0 ) {

        count_recv.append( n );
        bytes_recv.append( b );
        duration_recv.append( d );
    }

    void add( const MessageData& other ) {

        count_send.add( other.count_send );
        count_recv.add( other.count_recv );
        bytes_send.add( other.bytes_send );
        bytes_recv.add( other.bytes_recv );
        duration_send.add( other.duration_send );
        duration_recv.add( other.duration_recv );
    }
};


struct MessageSpeedData {

    static const uint32_t BIN_LOG_BASE= 2;

    min_max_avg<uint64_t> count;


    MessageSpeedData( ) {}
    ~MessageSpeedData( ) {}

    void add( uint64_t n= 0 ) {

        count.append( n );
    }

    void add( const MessageSpeedData& other ) {

        count.add( other.count );
    }
};


/* collective data are similar to message data */
typedef MessageData CollectiveData;


struct PendingCollective {

    uint32_t collop;
    uint64_t bytes_send;
    uint64_t bytes_recv;
    uint64_t begin_time;

    PendingCollective()
        : collop(0), bytes_send(0), bytes_recv(0), begin_time(0) {}
    PendingCollective( uint32_t _collop, uint64_t _bytes_send,
        uint64_t _bytes_recv, uint64_t _begin_time )
        : collop(_collop), bytes_send(_bytes_send), bytes_recv(_bytes_recv),
          begin_time(_begin_time) {}
    ~PendingCollective() {}
};


/* *** management and statistics data structures, needed on all ranks *** */

struct AllData {

    const uint32_t myRank;
    const uint32_t numRanks;

    /* number and list of processes to be handled by every worker */
    uint32_t  myProcessesNum;
    uint32_t* myProcessesList;

    /* one instance of send/receive buffer to be re-used all the time */
    uint32_t packbuffersize;
    char* packbuffer;

    /* program parameters */
    Params params;

    /* progress information */
    Progress progress;

    /* store per-measure block runtimes */
    map< string, MeasureBlock > measureBlockMap;

    /* clustering information for ranks */
    Clustering clustering;




    /* trace context information; only significant on rank 0 */

    /* trace creator */
    string creator;

    /* OTF version */
    string version;

    /* definition comments */
    string comments;

    /* all defined process IDs with its parents */
    set< Process, ltProcess > allProcesses;

    /* map function IDs to their corresponding names */
    map< uint64_t, string > functionIdNameMap;

    /* map counter IDs to their corresponding names */
    map< uint64_t, string > counterIdNameMap;

    /* map process IDs to their corresponding names */
    map< uint64_t, string > processIdNameMap;




    /* data collection helper datastructures, they hold management and temp
    information while reading the OTF streams */

    /* this is the function stack per trace process which is tracked while 
    reading the trace processes of the current worker */
    map< uint64_t, list<StackType> > stackPerProcess;

    /* temporary store per-rank collective begin operations
    Pair is <matching-id,rank> */
    map< Pair, PendingCollective, ltPair > pendingCollectives;

    /* map all defined collective operations to the four
    classes { barrier, one-to-all, all-to-one, all-to-all }
    use the OTF constants for the four classes */
    map< uint64_t, uint64_t > collectiveOperationsToClasses;

    /* OTF counter IDs to consider in statistics, ignore all other counters */
    set< uint64_t > countersOfInterest;

    /* timer resolution (ticks per second) */
    uint64_t timerResolution;

    /* key for OTF key-value-pairs with message matching information */
    uint64_t recvTimeKey; 




    /* data collection containers:
    the following maps are for collecting individual data per trace rank,
    they will be summarized to the next set of maps */

    /* store per-function statistics over the ranks, Pair is <funcId,rank> */
    map< Pair, FunctionData, ltPair > functionMapPerRank;

    /* store per-counter statistics over the functions and ranks, 
    Triple is <counterId,funcId,rank> */
    map< Triple, CounterData, ltTriple > counterMapPerFunctionRank;

    /* store send-recv statistics for P2P messages per communication pairs, 
    ATTENTION: Pair is <rank,peer> and not <sender,receiver> ! Recently changed ! */
    map< Pair, MessageData, ltPair > messageMapPerRankPair;

    /* store per-collop.-class statistics over the ranks, 
    Pair is <collective-class,rank> */
    map< Pair, CollectiveData, ltPair > collectiveMapPerRank;




    /* data summarization and reduction containers:
    the following maps are filled when summarizing the previous set of maps,
    they will be considered in the later reduce operation over the analysis ranks. */

    /* compact function statistics summed over all ranks */
    map< uint64_t, FunctionData > functionMapGlobal;

    /* store per-counter statistics over the functions and ranks, 
    Pair is <counterId,funcId> */
    map< Pair, CounterData, ltPair > counterMapGlobal;

    /* compact send-recv statistics for P2P messages per communicating clusters,
    clusters are groups of neigbor ranks, 
    ATTENTION: Pair is <rank,peer> and not <sender,receiver> ! Recently changed ! */
    map< Pair, MessageData, ltPair > messageMapPerClusterPair;

    /* compact send-receive statistics per cluster without differenciating the
    communication partners; cluster is a group of ranks */
    map< uint64_t, MessageData > messageMapPerCluster;

    /* store per-speed-bin statistics over the length-bins of P2P messages, 
    Pair is <speed-bin,length-bin> where bin is log2(<speed|length>) */
    map< Pair, MessageSpeedData, ltPair > messageSpeedMapPerLength;

    /* compact collective operation statistics per cluster;
    cluster is a group of ranks, Pair is <collective-class,cluster> */
    map< Pair, CollectiveData, ltPair > collectiveMapPerCluster;




    AllData( uint32_t my_rank, uint32_t num_ranks ) :
        myRank(my_rank), numRanks(num_ranks),
        myProcessesNum(0), myProcessesList(NULL),
        packbuffersize(0), packbuffer(NULL), timerResolution(0),
        recvTimeKey(0) {}

    ~AllData() {

        myProcessesNum= 0;
        free( myProcessesList );
        myProcessesList= NULL;

        packbuffersize= 0;
        free( packbuffer );
        packbuffer= NULL;
    };

    char* guaranteePackBuffer( uint32_t size ) {

        if ( packbuffersize < size ) {

            packbuffersize= size;
            packbuffer= (char*) realloc( packbuffer, packbuffersize * sizeof(char) );
            assert( NULL != packbuffer );
        }

        return packbuffer;
    }


    char* freePackBuffer( ) {

        free( packbuffer );
        packbuffer= NULL;
        packbuffersize= 0;

        return NULL;
    }


    char* getPackBuffer( ) {

        return packbuffer;
    }
};


#endif /* DATASTRUCTS_H */
