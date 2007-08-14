/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _OMPI_CIRCULAR_BUFFER_FIFO
#define _OMPI_CIRCULAR_BUFFER_FIFO

#include "ompi/constants.h"
#include "opal/sys/cache.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/mpool/mpool.h"
#include "opal/util/pow2.h"


/** @file
 *
 *  This defines a set of functions to create, and manipulate a FIFO
 *  set up in a circular buffer.  FIFO elements are assumed to be
 *  pointers.  Pointers are written to the head, and read from the
 *  tail.  For thread safety, a spin lock is provided in the
 *  ompi_cb_fifo_ctl_t structure, but it's use must be managed by
 *  the calling routines - this is not by these set of routines.
 *  Queues are addressed relative to an offset from the base of
 *  a memory pool, in this way, different processes with different
 *  base addresses can access these queue at the same time.
 */

/* error code */
#define OMPI_CB_ERROR             -1
#define OMPI_CB_FREE      (void *)-2
#define OMPI_CB_RESERVED  (void *)-3
#define OMPI_CB_NULL      (void *)-4

/*
 * Structure used to keep track of the fifo status
 */
struct ompi_cb_fifo_ctl_t {
    /* spin-lock for access control */
    opal_atomic_lock_t lock;

    /* current queue index */
    volatile int fifo_index;

    /* number of entries that have been used, but not invalidated.  used
     * for lazy resource reclamation */
    int num_to_clear;
};
typedef struct ompi_cb_fifo_ctl_t ompi_cb_fifo_ctl_t;

/* data structure used to describe the fifo */
struct ompi_cb_fifo_t {
    /* head of queue - where next entry will be written (sender address)*/
    ompi_cb_fifo_ctl_t *head;

    /* tail of queue - next element to read (receiver address) */
    ompi_cb_fifo_ctl_t *tail;

    /* head of queue - where next entry will be written (receiver address) */
    ompi_cb_fifo_ctl_t *recv_head;

    /* circular buffer array (sender address) */
    volatile void **queue;

    /* circular buffer array (receiver address) */
    volatile void **recv_queue;

    /* frequency of lazy free */
    int lazy_free_frequency;

    /* mask - to handle wrap around */
    unsigned int mask;
};

typedef struct ompi_cb_fifo_t ompi_cb_fifo_t;

/**
 * Return the fifo size
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @returncode fifo size
 *
 */
static inline int ompi_cb_fifo_size(ompi_cb_fifo_t *fifo) {

    return fifo->mask + 1;
}

/**
 * Initialize a fifo 
 *
 * @param size_of_fifo Length of fifo array (IN)
 *
 * @param fifo_memory_locality_index Locality index to apply to
 *                                   the fifo array.  Not currently
 *                                   in use (IN)
 *
 * @param tail_memory_locality_index Locality index to apply to the
 *                                   head control structure.  Not
 *                                   currently in use (IN)
 *
 * @param tail_memory_locality_index Locality index to apply to the
 *                                   tail control structure.  Not
 *                                   currently in use (IN)
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @param memory_allocator Pointer to the memory allocator to use
 *                         to allocate memory for this fifo. (IN)
 *
 * @returncode Error code
 *
 */
static inline int ompi_cb_fifo_init(int size_of_fifo,
        int lazy_free_freq, int fifo_memory_locality_index, 
        int head_memory_locality_index, int tail_memory_locality_index, 
        ompi_cb_fifo_t *fifo, ptrdiff_t offset,
        mca_mpool_base_module_t *memory_allocator)
{
    int i, size;
    char *buf;

    /* verify that size is power of 2, and greater that 0 - if not,
     * round up */
    if(size_of_fifo <= 0) {
        return OMPI_ERROR;
    }

    /* set fifo size */
    size = opal_round_up_to_nearest_pow2(size_of_fifo);

    /* set lazy free frequence */
    if((lazy_free_freq <= 0) || (lazy_free_freq > size)) {
        return OMPI_ERROR;
    }

    fifo->lazy_free_frequency = lazy_free_freq;

    /* this will be used to mask off the higher order bits,
     * and use the & operator for the wrap-around */
    fifo->mask = (size - 1);

    /* allocate fifo array */
    buf = memory_allocator->mpool_alloc(memory_allocator,
            sizeof(void *) * size + 2*CACHE_LINE_SIZE, CACHE_LINE_SIZE, 0,
            NULL);
    if (NULL == buf) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    fifo->queue = (volatile void**)(buf + 2*CACHE_LINE_SIZE);
    /* buffer address in a receiver address space */
    fifo->recv_queue = (volatile void**)((char*)fifo->queue - offset);
    /* initialize the queue entries */
    for (i = 0; i < size; i++) {
        fifo->queue[i] = OMPI_CB_FREE;
    }

    fifo->head = (ompi_cb_fifo_ctl_t*)buf;
    /* head address in a receiver address space */
    fifo->recv_head = (ompi_cb_fifo_ctl_t*)((char*)fifo->head - offset);
    fifo->tail = (ompi_cb_fifo_ctl_t*)(buf + CACHE_LINE_SIZE);

    /* initialize the head structure */
    opal_atomic_unlock(&(fifo->head->lock));
    fifo->head->fifo_index=0;
    fifo->head->num_to_clear=0;

    /* initialize the head structure */
    opal_atomic_unlock(&(fifo->tail->lock));
    fifo->tail->fifo_index=0;
    fifo->tail->num_to_clear=0;

    /* recalculate tail address in a receiver address space */
    fifo->tail = (ompi_cb_fifo_ctl_t*)((char*)fifo->tail - offset);

    /* return */
    return OMPI_SUCCESS;
}

/**
 * function to cleanup the fifo
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @param memory_allocator Pointer to the memory allocator to use
 *                         to allocate memory for this fifo. (IN)
 *
 */
static inline int ompi_cb_fifo_free(ompi_cb_fifo_t *fifo,
        mca_mpool_base_module_t *memory_allocator)
{
    char *ptr;

    /* make sure null fifo is not passed in */
    if(NULL == fifo) {
        return OMPI_ERROR;
    }

    /* free fifo array */
    if(OMPI_CB_NULL != fifo->head){
        ptr=(char *)(fifo->head);
        memory_allocator->mpool_free(memory_allocator, ptr, NULL);
        fifo->queue = (volatile void**)OMPI_CB_NULL;
    }

    return OMPI_SUCCESS;
}

/**
 * Write pointer to the specified slot
 *
 * @param slot Slot index (IN)
 *
 * @param data Pointer value to write in specified slot (IN)
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @returncode Slot index to which data is written
 *
 */
static inline int ompi_cb_fifo_write_to_slot(int slot, void* data,
        ompi_cb_fifo_t *fifo)
{
    volatile void **ptr;
    int wrote_to_slot = OMPI_CB_ERROR;
    /* make sure that this slot is already reserved */
    ptr=fifo->queue;
    if (ptr[slot] == OMPI_CB_RESERVED ) {
        opal_atomic_rmb();
        ptr[slot] = data;
        opal_atomic_wmb();
        return slot;
    } else {
        return wrote_to_slot;
    }
}

/**
 * Try to write pointer to the head of the queue
 *
 * @param data Pointer value to write in specified slot (IN)
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @returncode Slot index to which data is written
 *
 */
static inline int ompi_cb_fifo_write_to_head(void *data, ompi_cb_fifo_t *fifo)
{
    volatile void **ptr;
    ompi_cb_fifo_ctl_t *h_ptr;
    int index;

    h_ptr=fifo->head;
    ptr=fifo->queue;
    index = h_ptr->fifo_index;

    /* make sure the head is pointing at a free element */
    if (ptr[index] == OMPI_CB_FREE) {
        opal_atomic_rmb();
        ptr[index] = data;
        opal_atomic_wmb(); 
        h_ptr->fifo_index = (index + 1) & fifo->mask;
        return index;
    }

    /* return */
    return OMPI_CB_ERROR;
}

/**
 * Reserve slot in the fifo array
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @returncode Slot index to which data is written
 *
 * @returncode OMPI_CB_ERROR failed to allocate index
 *
 */
static inline int ompi_cb_fifo_get_slot(ompi_cb_fifo_t *fifo)
{
    return ompi_cb_fifo_write_to_head(OMPI_CB_RESERVED, fifo);
}

/**
 * Try to read pointer from the tail of the queue
 *
 * @param data Pointer to where data was be written (OUT)
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @param flush_entries_read force the lazy free to happen (IN)
 *
 * @param queue_empty checks to see if the fifo is empty, but only if
 * flush_entries_read is set (OUT)
 *
 * @returncode Slot index to which data is written
 *
 */
static inline void *ompi_cb_fifo_read_from_tail(
        ompi_cb_fifo_t *fifo,
        bool flush_entries_read, bool *queue_empty)
{
    int index, i;
    volatile void **q_ptr;
    ompi_cb_fifo_ctl_t *t_ptr;
    void *read_from_tail;

    *queue_empty=false;

    t_ptr=fifo->tail;
    q_ptr=fifo->recv_queue;
    index = t_ptr->fifo_index;
    read_from_tail = (void *)q_ptr[index];
    opal_atomic_rmb();

    /* check to see that the data is valid */
    if ((read_from_tail == OMPI_CB_FREE) ||
            (read_from_tail == OMPI_CB_RESERVED)) {
         return (void*)OMPI_CB_FREE;
    }

    /* increment counter for later lazy free */
    t_ptr->num_to_clear++;

    t_ptr->fifo_index = (index + 1) & fifo->mask;

    /* check to see if time to do a lazy free of queue slots */
    if ( (t_ptr->num_to_clear == fifo->lazy_free_frequency) ||
            flush_entries_read ) {
        ompi_cb_fifo_ctl_t *h_ptr = fifo->recv_head;
        index = (index - t_ptr->num_to_clear + 1) & fifo->mask;

        for (i = 0; i < t_ptr->num_to_clear; i++) {
            q_ptr[index] = OMPI_CB_FREE;
            index = (index + 1) & fifo->mask;
        }
        opal_atomic_wmb();
        t_ptr->num_to_clear = 0;

        /* check to see if queue is empty */
        if( flush_entries_read &&
                (t_ptr->fifo_index == h_ptr->fifo_index) ) {
            *queue_empty=true;
        }
    }

    return read_from_tail;
}

#endif				/* !_OMPI_CIRCULAR_BUFFER_FIFO */
