#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "mpi.h"
#include "ompi/peruse/peruse.h"

#define ARRAY_SIZE (((128*1024 - 20)*5 + 65496) / sizeof(int))
#define SHORT_OUTPUT 0

typedef struct peruse_timed_events {
    peruse_event_h event_h;
    MPI_Aint unique_id;
    double occur_at;
    void* param;
    peruse_comm_spec_t spec;
} peruse_timed_events_t;

char * operation_string[] = {
    "PERUSE_SEND",
    "PERUSE_RECV",
    "PERUSE_PUT",
    "PERUSE_GET",
    "PERUSE_ACC",
    "PERUSE_IO_READ",
    "PERUSE_IO_WRITE"
};

/*
 * Global variables
 */
int comm_rank, comm_size;
MPI_Comm comm_result;
peruse_timed_events_t* array_of_comm_spec = NULL;
int max_array_of_comm_spec_index = 0;
int current_array_of_comm_spec_index = 0;
double initial_wtime = 0;

/* Static functions */
static int collective_dump_events_trace( void );
static int callback_func (peruse_event_h event_h, MPI_Aint unique_id, peruse_comm_spec_t* spec, void* param);

/*
 * Global Callback functions
 */
static int print_generated_events( int rank, peruse_timed_events_t* spec_array, int number )
{
    peruse_timed_events_t* spec = spec_array;
    int i;

    for( i = 0; i < number; i++ ) {
#if SHORT_OUTPUT
        printf( "(Rank:%d op %s) %37s unique_id:%p at %g param:%p\n",
                rank, operation_string[spec->spec.operation], 
                (char*)spec->param, spec->unique_id,
                spec->occur_at - initial_wtime, spec->param );
#else
        printf ("(Rank:%d) Called %s event_h:%p unique_id:%p at %g spec:%p param:%p "
                "comm:%p buf:%p, count:%d datatype:%p peer:%d tag:%d operation:%s\n",
                rank, (char*)spec->param, spec->event_h, (void*)spec->unique_id, spec->occur_at - initial_wtime,
                (void*)spec, spec->param, (void*)spec->spec.comm, spec->spec.buf, spec->spec.count,
                (void*)spec->spec.datatype, spec->spec.peer, spec->spec.tag,
                operation_string[spec->spec.operation]);
#endif  /* SHORT_OUTPUT */
        spec++;
    }
    fflush(stdout);
    return 0;
}

static int collective_start_experiment( void )
{
    if( 0 == comm_rank ) {
        static int experiment=1;

        printf( "\n\nStarting Experiment %d:\n\n\n", experiment++ );
    }
    MPI_Barrier (comm_result);
    return 0;
}

static int collective_dump_events_trace( void )
{
    if( 0 == comm_rank ) {
        int i;

        print_generated_events( 0, array_of_comm_spec, current_array_of_comm_spec_index );
        printf( "\n\n\n\n\n" );
        for( i = 1; i < comm_size; i++ ) {
            MPI_Status status;

            MPI_Probe( i, 1111, comm_result, &status );
            MPI_Recv( array_of_comm_spec, status._count, MPI_BYTE, i, 1111, comm_result, &status );
            current_array_of_comm_spec_index = status._count / sizeof(peruse_timed_events_t);
            print_generated_events( i, array_of_comm_spec, current_array_of_comm_spec_index );
        }
        printf( "\n\n\n\n\n" );
    } else {
        MPI_Send( array_of_comm_spec, current_array_of_comm_spec_index * sizeof(peruse_timed_events_t),
                  MPI_BYTE, 0, 1111, comm_result );
    }
    MPI_Barrier( comm_result );
    current_array_of_comm_spec_index = 0;
    return 0;
}

static int callback_func( peruse_event_h event_h, MPI_Aint unique_id,
                          peruse_comm_spec_t* spec, void* param )
{
    peruse_timed_events_t* current = array_of_comm_spec + current_array_of_comm_spec_index;

    current->spec      = *spec;
    current->occur_at  = MPI_Wtime();
    current->param     = param;
    current->event_h   = event_h;
    current->unique_id = unique_id;
    current_array_of_comm_spec_index++;
    if( current_array_of_comm_spec_index == max_array_of_comm_spec_index )
        current_array_of_comm_spec_index = 0;  /* overwrite */
    return MPI_SUCCESS;
}

struct event_t{
    char ev_name[64];
    int ev_num;
    peruse_event_h ev_handle;
};
typedef struct event_t event_t;

event_t events[] = {
    {"PERUSE_COMM_REQ_ACTIVATE",             0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_REQ_MATCH_UNEX",           0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_REQ_INSERT_IN_POSTED_Q",   0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_REQ_REMOVE_FROM_POSTED_Q", 0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_REQ_XFER_BEGIN",           0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_REQ_XFER_CONTINUE",        0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_REQ_XFER_END",             0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_REQ_COMPLETE",             0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_REQ_NOTIFY",               0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_MSG_ARRIVED",              0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_MSG_INSERT_IN_UNEX_Q",     0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_MSG_REMOVE_FROM_UNEX_Q",   0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_MSG_MATCH_POSTED_REQ",     0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_SEARCH_POSTED_Q_BEGIN",    0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_SEARCH_POSTED_Q_END",      0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_SEARCH_UNEX_Q_BEGIN",      0, PERUSE_EVENT_HANDLE_NULL},
    {"PERUSE_COMM_SEARCH_UNEX_Q_END",        0, PERUSE_EVENT_HANDLE_NULL}
};

int main (int argc, char * argv[])
{
    int ret, array[ARRAY_SIZE];
    unsigned int i;
    MPI_Status status;
    MPI_Request request;
  
    int peruse_num_supported;
    char ** peruse_event_names;
    int * peruse_events;

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (MPI_COMM_WORLD, &comm_rank);
    MPI_Comm_size (MPI_COMM_WORLD, &comm_size);

    if( comm_size < 2 ) {
        /* We're looking for send & receive messages we need at least 2 nodes. */
        printf( "Please use at least 2 processes\n" );
        MPI_Finalize();
        return -1;
    }
    /*sleep(20);*/
    MPI_Comm_dup (MPI_COMM_WORLD, &comm_result);
    printf ("(Rank:%d) MPI_COMM_WORLD:%p comm_result:%p\n",
            comm_rank, (void*)MPI_COMM_WORLD, (void*)comm_result);

    PERUSE_Init ();
    PERUSE_Query_supported_events (&peruse_num_supported,
                                   &peruse_event_names,
                                   &peruse_events);

    for( i = 0; i < (sizeof(events) / sizeof(events[i])); i++ ) {
        ret = PERUSE_Query_event (events[i].ev_name, &events[i].ev_num);
        if (ret != PERUSE_SUCCESS) {
            fprintf( stderr, "(%s:%d): %d PERUSE_Query_event: Unexpected error for event %s\n",
                     __FILE__, __LINE__, i, events[i].ev_name );
            continue;
        }
        
        ret = PERUSE_Event_comm_register (events[i].ev_num, MPI_COMM_WORLD, &callback_func, (void*)&events[i].ev_name, &events[i].ev_handle);
        if (ret != PERUSE_SUCCESS) {
            fprintf( stderr, "(%s:%d): %d PERUSE_Event_comm_register: Unexpected error for event %s\n",
                     __FILE__, __LINE__, i, events[i].ev_name );
            continue;
        }
        
        ret = PERUSE_Event_activate (events[i].ev_handle);
        if (ret != PERUSE_SUCCESS) {
            fprintf( stderr, "(%s:%d): %d PERUSE_Event_activate: Unexpected error for event %s\n",
                     __FILE__, __LINE__, i, events[i].ev_name );
            continue;
        }
    }
    
    /* Initialize the array of events */
    max_array_of_comm_spec_index = 1000;
    array_of_comm_spec = (peruse_timed_events_t*)malloc( sizeof(peruse_timed_events_t) *
                                                         max_array_of_comm_spec_index );
    initial_wtime = MPI_Wtime();
    collective_start_experiment();
    /*
     * Experiment number 1
     */
    if (comm_rank == 0) {
        /*
         * Delay the send, so that the MPI_RECV is posted for sure in order
         * to trigger the PERUSE_COMM_MSG_MATCH_POSTED_REQ-event on rank 1
         * The unique_id is taken from the internal request.
         */
        sleep (1);
        MPI_Send (array, ARRAY_SIZE, MPI_INT, 1, 4711, MPI_COMM_WORLD);
    } else if (comm_rank == 1) {
        MPI_Recv (array, ARRAY_SIZE, MPI_INT, 0, 4711, MPI_COMM_WORLD, &status);
    }

    collective_dump_events_trace();

    /*
     * Experiment number 2
     */
    collective_start_experiment();

    if (comm_rank == 0) {
        /*
         * Delay the send, so that the MPI_RECV is posted for sure in order
         * to trigger the PERUSE_COMM_MSG_MATCH_POSTED_REQ-event on rank 1
         * The unique_id is the request specified here.
         */
        sleep (1);
        MPI_Isend (array, ARRAY_SIZE, MPI_INT, 1, 4711, MPI_COMM_WORLD, &request);
    } else if (comm_rank == 1) {
        MPI_Irecv (array, ARRAY_SIZE, MPI_INT, 0, 4711, MPI_COMM_WORLD, &request);
    }
    MPI_Wait( &request, &status );

    collective_dump_events_trace();

    /*
     * Experiment number 3
     */
    collective_start_experiment();

    if (comm_rank == 0) {
        /*
         * Delay the probei/recv, so that the MPI_SEND is posted for sure in order
         * to trigger the PERUSE_COMM_REQ_INSERT_IN_POSTED_Q-event
         */
        MPI_Send (array, ARRAY_SIZE, MPI_INT, 1, 4711, MPI_COMM_WORLD);
    } else if (comm_rank == 1) {
        sleep (1);
        MPI_Probe( 0, 4711, MPI_COMM_WORLD, &status );
        MPI_Recv (array, ARRAY_SIZE, MPI_INT, 0, 4711, MPI_COMM_WORLD, &status);
    }

    collective_dump_events_trace();

    /* Deactivate event handles and free them */
    for(i = 0; i < (sizeof(events) / sizeof(events[i])); i++) {
        if( PERUSE_EVENT_HANDLE_NULL == events[i].ev_handle ) continue;
        PERUSE_Event_deactivate( events[i].ev_handle );
        PERUSE_Event_release( &(events[i].ev_handle) );
    }
    free( array_of_comm_spec );
    MPI_Finalize ();
    return 0;
}

