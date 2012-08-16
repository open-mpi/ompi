/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_ML_PAYLOAD_BUFFERS_H
#define MCA_ML_PAYLOAD_BUFFERS_H

#include "ompi/include/ompi/constants.h"
#include "opal/threads/mutex.h"

struct buffer_t {
    /* payload */
    void *payload;

    /* next payload buffer - need this because of wrap around, and
     * because we want to allocate several buffers at once, but only
     * manipulate one entry
     */
    struct buffer_t *next_buffer;
};
typedef struct buffer_t buffer_t;

struct ml_buffers_t {
    /* fifo size */
    int fifo_size;

    /* write index - next to allocate */
    int head_index;
    opal_mutex_t head_lock;

    /* read index - next to free */
    int tail_index;

    /* number available - used to detect full queue */
    int n_segments_available;

    /* mask - assumes that fifo link is a power of 2 */
    int mask;

    /* fifo */
    buffer_t *fifo;
};

typedef struct ml_buffers_t ml_buffers_t;

/* Initialization function */

static inline int ml_fifo_init(
        int fifo_size, void *memory_chunk, size_t size_of_memory_chunk,
        size_t segment_alignment,
        size_t segment_size, ml_buffers_t *buffer_fifo)
{
    /* local variable */
    ptrdiff_t allocation_base, memory_chunk_ptr;
    size_t memory_to_allocate, allocated_fifo_size,
           allocated_segment_size, seg;

    /* make sure fifo size is power of 2, and round up if not - want
     * efficient addressing */
    if( 0 >= fifo_size ) {
        return OMPI_ERROR;
    }
    allocated_fifo_size=1;
    while ( allocated_fifo_size < (size_t)fifo_size ) {
        allocated_fifo_size*=2;
    }

    /* set buffer size to match its alignment - round size up */
    allocated_segment_size=segment_size;
    if( 0 >= segment_alignment ) {
        /* multiples of alignmnet */
        allocated_segment_size=( (allocated_segment_size-1)/segment_alignment)+1;
        allocated_segment_size=allocated_segment_size*segment_alignment;
    }

    /* adjust base pointer to segment alignment */
    memory_chunk_ptr = (ptrdiff_t )memory_chunk;
    allocation_base=( ( memory_chunk_ptr-1)/segment_alignment)+1;
    allocation_base=allocated_segment_size*segment_alignment;

    /* check for input consistency */
    memory_to_allocate=size_of_memory_chunk-(allocation_base-memory_chunk_ptr);
    if( (allocated_segment_size * allocated_fifo_size) < memory_to_allocate ) {
        return OMPI_ERROR;
    }

    /* allocate the fifo array */
    buffer_fifo->fifo=(buffer_t *)malloc(sizeof(buffer_t)*allocated_fifo_size);
    if( NULL == buffer_fifo->fifo) {
        return OMPI_ERROR;
    }

    /* Initialize structure */
    for( seg=0 ; seg < allocated_fifo_size ; seg++ ) {
        buffer_fifo->fifo[seg].payload=
            (void *)(allocation_base+seg*allocated_segment_size);
    }
    for( seg=0 ; seg < allocated_fifo_size-1 ; seg++ ) {
        buffer_fifo->fifo[seg].next_buffer=
            &(buffer_fifo->fifo[seg+1]);
    }
    buffer_fifo->fifo[allocated_fifo_size-1].next_buffer=
        &(buffer_fifo->fifo[0]);

    buffer_fifo->head_index=0;
    buffer_fifo->tail_index=0;
    buffer_fifo->n_segments_available=allocated_fifo_size;
    buffer_fifo->fifo_size=allocated_fifo_size;
    buffer_fifo->mask=buffer_fifo->fifo_size-1;
    OBJ_CONSTRUCT(&(buffer_fifo->head_lock), opal_mutex_t);

    /* return */
    return OMPI_SUCCESS;
}

/*
 * Allocate several buffers.  Either all requested buffers are allocated,
 *  or none are allocated.
 */
static inline buffer_t *ml_fifo_alloc_n_buffers(int n_to_allocate,
 ml_buffers_t *buffer_fifo)
{
  /* local variables */
  buffer_t *ret=NULL;

  /* RLG - probably want to try a few times before giving up */
  if(!OPAL_THREAD_TRYLOCK(&(buffer_fifo->head_lock))) {
      if( buffer_fifo->n_segments_available >= n_to_allocate ) {
          ret=&(buffer_fifo->fifo[buffer_fifo->head_index]);
          buffer_fifo->head_index=(buffer_fifo->head_index+n_to_allocate);
          /* wrap around */
          buffer_fifo->head_index&=buffer_fifo->mask;

          buffer_fifo->n_segments_available -= n_to_allocate;
      }
      OPAL_THREAD_UNLOCK(&(buffer_fifo->head_lock));
  }  /* end of allocatoin */

  return ret;
}

/* return buffers */
static inline void ml_fifo_return_n_buffers(int n_to_return,
  ml_buffers_t *buffer_fifo)
{

  OPAL_THREAD_LOCK(&(buffer_fifo->head_lock));

  /* move tail pointer - RLG:  Do we really need the tail pointer ? */
  buffer_fifo->tail_index=(buffer_fifo->tail_index+n_to_return);
  /* wrap around */
  buffer_fifo->tail_index&=buffer_fifo->mask;

  /* adjust number of available buffers */
  buffer_fifo->n_segments_available += n_to_return;

  OPAL_THREAD_UNLOCK(&(buffer_fifo->head_lock));

}

#endif

