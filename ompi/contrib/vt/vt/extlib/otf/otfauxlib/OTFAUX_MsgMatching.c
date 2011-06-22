#include <config.h>

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#include <otf.h>

#include <jenkins_hash.h>

#include "otfaux.h"

typedef struct OTFAUX_MsgMatching_Recv {
    /** next in queue */
    struct OTFAUX_MsgMatching_Recv* next;

    uint64_t time;
    uint32_t size, scl;
} OTFAUX_MsgMatching_Recv;

typedef struct OTFAUX_MsgMatching_Queue {
    /** for hash chaining */
    struct OTFAUX_MsgMatching_Queue* next;

    uint64_t sender, receiver;
    uint32_t tag, comm;

    /** cached hash value of this queue */
    uint32_t hash;

    /** queue of receives */
    OTFAUX_MsgMatching_Recv* head;
    OTFAUX_MsgMatching_Recv** tail;
} OTFAUX_MsgMatching_Queue;

#define QUEUE_HASH_SHIFT 10
#define QUEUE_HASH_SIZE (1 << QUEUE_HASH_SHIFT)
#define QUEUE_HASH_MASK (QUEUE_HASH_SIZE - 1)

struct OTFAUX_MsgMatching_Context {
    /** The messages queues, identified by the quadtrupel
        (sender, receiver, tag, comm) */
    OTFAUX_MsgMatching_Queue* queues[ QUEUE_HASH_SIZE ];

    /** unused OTFAUX_MsgMatching_Recv objects */
    OTFAUX_MsgMatching_Recv* free_list;
};

OTFAUX_MsgMatching_Context*
OTFAUX_MsgMatching_create( void )
{
    OTFAUX_MsgMatching_Context* new_context = calloc( 1, sizeof( *new_context ) );

    /* nothing to initialize */

    return new_context;
}

static void
free_recv_list( OTFAUX_MsgMatching_Recv** recv_list )
{
    while ( *recv_list )
    {
        OTFAUX_MsgMatching_Recv* next = ( *recv_list )->next;
        free( *recv_list );
        *recv_list = next;
    }
}

void
OTFAUX_MsgMatching_destroy( OTFAUX_MsgMatching_Context* mm_context )
{
    int i;
    for ( i = 0; i < QUEUE_HASH_SIZE; i++ )
    {
        while ( mm_context->queues[ i ] )
        {
            OTFAUX_MsgMatching_Queue* next = mm_context->queues[ i ]->next;
            free_recv_list( &mm_context->queues[ i ]->head );
            free( mm_context->queues[ i ] );
            mm_context->queues[ i ] = next;
        }
    }

    free_recv_list( &mm_context->free_list );

    free( mm_context );
}

/* hashing of queues */
static uint32_t
hash_queue( uint64_t sender,
            uint64_t receiver,
            uint32_t tag,
            uint32_t comm )
{
    uint32_t queue_hash = 0;

    queue_hash += hash( &sender,   sizeof( sender ),   queue_hash );
    queue_hash += hash( &receiver, sizeof( receiver ), queue_hash );
    queue_hash += hash( &tag,      sizeof( tag ),      queue_hash );
    queue_hash += hash( &comm,     sizeof( comm ),     queue_hash );

    return queue_hash;
}

static OTFAUX_MsgMatching_Queue*
create_queue( uint32_t queue_hash,
              uint64_t sender,
              uint64_t receiver,
              uint32_t tag,
              uint32_t comm )
{
    OTFAUX_MsgMatching_Queue* new_queue = calloc( 1, sizeof( *new_queue ) );

    if ( new_queue )
    {
        /* store queue atttributes */
        new_queue->sender   = sender;
        new_queue->receiver = receiver;
        new_queue->tag      = tag;
        new_queue->comm     = comm;
        
        /* cache hash value for this queue */
        new_queue->hash = queue_hash;

        /* initialize the recv queue */
        new_queue->tail = &new_queue->head;
    }

    return new_queue;
}

static OTFAUX_MsgMatching_Queue*
get_queue( OTFAUX_MsgMatching_Context* mm_context,
           uint64_t sender,
           uint64_t receiver,
           uint32_t tag,
           uint32_t comm,
           int create )
{
    uint32_t queue_hash = hash_queue( sender, receiver, tag, comm );
    OTFAUX_MsgMatching_Queue** queue_bucket = &mm_context->queues[ queue_hash & QUEUE_HASH_MASK ];
    OTFAUX_MsgMatching_Queue* queue = *queue_bucket;

    /* search in hash chain */
    while ( queue )
    {
        if ( queue->hash == queue_hash
             && queue->sender == sender
             && queue->receiver == receiver
             && queue->tag == tag
             && queue->comm == comm )
        {
            /* found */
            return queue;
        }

        queue = queue->next;
    }

    if ( create )
    {
        queue = create_queue( queue_hash, sender, receiver, tag, comm );
        if ( !queue )
        {
            return NULL;
        }

        /* chain into hash table */
        queue->next = *queue_bucket;
        *queue_bucket = queue;
    }

    return queue;
}

void
OTFAUX_MsgMatching_enqueueRecv( OTFAUX_MsgMatching_Context* mm_context,
                                uint64_t sender,
                                uint64_t receiver,
                                uint32_t tag,
                                uint32_t comm,
                                uint64_t time,
                                uint32_t size,
                                uint32_t scl )
{
    OTFAUX_MsgMatching_Queue* queue;
    OTFAUX_MsgMatching_Recv* new_recv;

    /* create this queue if its not present */
    queue = get_queue( mm_context, sender, receiver, tag, comm, 1 );

    /* create new recv entry, use free_list if possible */
    if ( mm_context->free_list )
    {
        new_recv = mm_context->free_list;
        mm_context->free_list = new_recv->next;
        new_recv->next = NULL;
    } else {
        new_recv = calloc( 1, sizeof( *new_recv ) );
        if ( !new_recv )
        {
            return;
        }
    }
    new_recv->time = time;
    new_recv->size = size;
    new_recv->scl  = scl;

    /* enqueue recv into queue */
    *queue->tail = new_recv;
    queue->tail = &new_recv->next;
}

int
OTFAUX_MsgMatching_matchSend( OTFAUX_MsgMatching_Context* mm_context,
                              uint64_t sender,
                              uint64_t receiver,
                              uint32_t tag,
                              uint32_t comm,
                              uint64_t* ptime,
                              uint32_t* psize,
                              uint32_t* pscl )
{
    OTFAUX_MsgMatching_Queue* queue;
    OTFAUX_MsgMatching_Recv* recv;

    /* don't create this queue if its not present */
    queue = get_queue( mm_context, sender, receiver, tag, comm, 0 );

    /* no queue -> no recv || no recv in queue */
    if ( !queue || !queue->head )
    {
        return 0;
    }

    /* unqeue recv */
    recv = queue->head;
    queue->head = recv->next;
    if ( !queue->head )
    {
        queue->tail = &queue->head;
    }

    if ( ptime )
    {
        *ptime = recv->time;
    }
    if ( psize )
    {
        *psize = recv->size;
    }
    if ( pscl )
    {
        *pscl = recv->scl;
    }

    /* put the now unused recv object into free list */
    recv->next = mm_context->free_list;
    mm_context->free_list = recv;

    return 1;
}

/** release empty queues and recv in free list */
void
OTFAUX_MsgMatching_releaseMemory( OTFAUX_MsgMatching_Context* mm_context )
{
    int i;
    for ( i = 0; i < QUEUE_HASH_SIZE; i++ )
    {
        OTFAUX_MsgMatching_Queue* full_queue = NULL;
        OTFAUX_MsgMatching_Queue* queue = mm_context->queues[ i ];
        while ( queue )
        {
            OTFAUX_MsgMatching_Queue* next = queue->next;
            /* release this queue if no recv are enqueued */
            if ( queue->head )
            {
                queue->next = full_queue;
                full_queue = queue;
            }
            else
            {
                free( queue );
            }

            queue = next;
        }
        mm_context->queues[ i ] = full_queue;
    }

    free_recv_list( &mm_context->free_list );
}
