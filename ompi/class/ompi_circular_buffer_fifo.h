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
    volatile int num_to_clear;

};
typedef struct ompi_cb_fifo_ctl_t ompi_cb_fifo_ctl_t;

/* data structure used to describe the fifo */
struct ompi_cb_fifo_t {

    /* size of fifo */
    int size;

    /* frequency of lazy free */
    int lazy_free_frequency;

    /* fifo memory locality index */
    int fifo_memory_locality_index;

    /* head memory locality index */
    int head_memory_locality_index;

    /* tail memory locality index */
    int tail_memory_locality_index;

    /* head of queue - where next entry will be written */
    ompi_cb_fifo_ctl_t *head;

    /* tail of queue - next element to read */
    ompi_cb_fifo_ctl_t *tail;

    /* mask - to handle wrap around */
    unsigned int mask;

    /* circular buffer array */
    volatile void **queue;

};

typedef struct ompi_cb_fifo_t ompi_cb_fifo_t;

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
static inline void *ompi_cb_fifo_read_from_tail(ompi_cb_fifo_t *fifo,
        bool flush_entries_read, bool *queue_empty, ssize_t offset) 
{
    int index = 0,clearIndex, i;
    void **q_ptr;
    ompi_cb_fifo_ctl_t *h_ptr, *t_ptr;
    void *read_from_tail = (void *)OMPI_CB_ERROR;

    *queue_empty=false;

    h_ptr=(ompi_cb_fifo_ctl_t *)( (char *)(fifo->head) + offset);
    t_ptr=(ompi_cb_fifo_ctl_t *)( (char *)(fifo->tail) + offset);
    q_ptr=(void **)( (char *)(fifo->queue) + offset);

    /* check to see that the data is valid */
    if ((q_ptr[t_ptr->fifo_index] == OMPI_CB_FREE) ||
            (q_ptr[t_ptr->fifo_index] == OMPI_CB_RESERVED)) 
    {
        return (void *)OMPI_CB_FREE;
    }

    /* set return data */
    index = t_ptr->fifo_index;
    read_from_tail = (void *)q_ptr[index];
    opal_atomic_rmb();
    t_ptr->num_to_clear++;

    /* increment counter for later lazy free */
    (t_ptr->fifo_index)++;
    (t_ptr->fifo_index) &= fifo->mask;

    /* check to see if time to do a lazy free of queue slots */
    if ( (t_ptr->num_to_clear == fifo->lazy_free_frequency) ||
            flush_entries_read ) {
        clearIndex = index - t_ptr->num_to_clear + 1;
        clearIndex &= fifo->mask;
        
        for (i = 0; i < t_ptr->num_to_clear; i++) {
            q_ptr[clearIndex] = OMPI_CB_FREE;
            clearIndex++;
            clearIndex &= fifo->mask;
        }
        t_ptr->num_to_clear = 0;

        /* check to see if queue is empty */
        if( flush_entries_read && 
                (t_ptr->fifo_index == h_ptr->fifo_index) ) {
            *queue_empty=true;
        }
    }

    /* return */
    return read_from_tail;
}

/**
 * Return the fifo size
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @returncode fifo size
 *
 */
static inline int ompi_cb_fifo_size(ompi_cb_fifo_t *fifo) {

    return fifo->size;
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
static inline int ompi_cb_fifo_init_same_base_addr(int size_of_fifo, 
        int lazy_free_freq, int fifo_memory_locality_index, 
        int head_memory_locality_index, int tail_memory_locality_index, 
        ompi_cb_fifo_t *fifo, mca_mpool_base_module_t *memory_allocator) 
{

    int errorCode = OMPI_SUCCESS,i;
    size_t len_to_allocate;

    /* verify that size is power of 2, and greatter that 0 - if not, 
     * round up */
    if ( 0 >= size_of_fifo) {
        return OMPI_ERROR;
    }

    /* set fifo size */
    fifo->size = opal_round_up_to_nearest_pow2(size_of_fifo);

    /* set lazy free frequence */
    if( ( 0 >= lazy_free_freq ) || 
            ( lazy_free_freq > fifo->size) ) {
        return OMPI_ERROR;
    }
    fifo->lazy_free_frequency=lazy_free_freq;
    
    /* this will be used to mask off the higher order bits,
     * and use the & operator for the wrap-around */
    fifo->mask = (fifo->size - 1);

    /* allocate fifo array */
    len_to_allocate = sizeof(void *) * fifo->size;
    fifo->queue = (volatile void**)memory_allocator->mpool_alloc(memory_allocator, len_to_allocate,CACHE_LINE_SIZE, 0, NULL);
    if ( NULL == fifo->queue) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize the queue entries */
    for (i = 0; i < fifo->size; i++) {
        fifo->queue[i] = OMPI_CB_FREE;
    }

    /* allocate head control structure */
    len_to_allocate = sizeof(ompi_cb_fifo_ctl_t);
    fifo->head = (ompi_cb_fifo_ctl_t*)memory_allocator->mpool_alloc(memory_allocator, len_to_allocate,CACHE_LINE_SIZE, 0, NULL);
    if ( NULL == fifo->head) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize the head structure */
    opal_atomic_unlock(&(fifo->head->lock));
    fifo->head->fifo_index=0;
    fifo->head->num_to_clear=0;

    /* allocate tail control structure */
    len_to_allocate = sizeof(ompi_cb_fifo_ctl_t);
    fifo->tail = (ompi_cb_fifo_ctl_t*)memory_allocator->mpool_alloc(memory_allocator, len_to_allocate,CACHE_LINE_SIZE, 0, NULL);
    if ( NULL == fifo->tail) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize the head structure */
    opal_atomic_unlock(&(fifo->tail->lock));
    fifo->tail->fifo_index=0;
    fifo->tail->num_to_clear=0;

    /* set memory locality indecies */
    fifo->fifo_memory_locality_index=fifo_memory_locality_index;
    fifo->head_memory_locality_index=head_memory_locality_index;
    fifo->tail_memory_locality_index=tail_memory_locality_index;

    /* return */
    return errorCode;
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
static inline int ompi_cb_fifo_free_same_base_addr( ompi_cb_fifo_t *fifo, 
        mca_mpool_base_module_t *memory_allocator) 
{

    int errorCode = OMPI_SUCCESS;
    char *ptr;

    /* make sure null fifo is not passed in */
    if ( NULL == fifo) {
        return OMPI_ERROR;
    }

    /* free fifo array */
    if( OMPI_CB_NULL != fifo->head ){
        ptr=(char *)(fifo->queue);
        memory_allocator->mpool_free(memory_allocator, ptr, NULL);
        fifo->queue = (volatile void**)OMPI_CB_NULL;
    }

    /* free head control structure */
    if( OMPI_CB_NULL != fifo->head) {
        ptr=(char *)(fifo->head);
        memory_allocator->mpool_free(memory_allocator, ptr, NULL);
        fifo->head = (ompi_cb_fifo_ctl_t*)OMPI_CB_NULL;
    }

    /* free tail control structure */
    if( OMPI_CB_NULL != fifo->tail) {
        ptr=(char *)(fifo->tail);
        memory_allocator->mpool_free(memory_allocator, ptr, NULL);
        fifo->tail = (ompi_cb_fifo_ctl_t*)OMPI_CB_NULL;
    }

    /* return */
    return errorCode;
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
static inline int ompi_cb_fifo_write_to_slot_same_base_addr(int slot, void* data,
        ompi_cb_fifo_t *fifo)
{
    volatile void **ptr;
    int wrote_to_slot = OMPI_CB_ERROR;
    /* make sure that this slot is already reserved */
    ptr=fifo->queue;
    if (ptr[slot] == OMPI_CB_RESERVED ) {
        opal_atomic_wmb();
        ptr[slot] = data;
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
static inline int ompi_cb_fifo_write_to_head_same_base_addr(void *data, ompi_cb_fifo_t *fifo) 
{
    volatile void **ptr;
    ompi_cb_fifo_ctl_t *h_ptr;
    int slot = OMPI_CB_ERROR, index;

    h_ptr=fifo->head;
    index = h_ptr->fifo_index;
    /* make sure the head is pointing at a free element */
    ptr=fifo->queue;
    if (ptr[index] == OMPI_CB_FREE) {
        slot = index;
        opal_atomic_wmb(); 
        ptr[slot] = data;
        (h_ptr->fifo_index)++;
        /* wrap around */
        (h_ptr->fifo_index) &= fifo->mask;
    }

    /* return */
    return slot;
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
static inline int ompi_cb_fifo_get_slot_same_base_addr(ompi_cb_fifo_t *fifo)
{
    volatile void **ptr;
    ompi_cb_fifo_ctl_t *h_ptr;
    int return_value = OMPI_CB_ERROR,index;

    h_ptr=fifo->head;
    ptr=fifo->queue;
    index = h_ptr->fifo_index;
    /* try and reserve slot */
    if ( OMPI_CB_FREE == ptr[index] ) {
        ptr[index] = OMPI_CB_RESERVED;
        return_value = index;
        (h_ptr->fifo_index)++;
        (h_ptr->fifo_index) &= fifo->mask;
    }

    /* return */
    return return_value;
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
static inline void *ompi_cb_fifo_read_from_tail_same_base_addr(
        ompi_cb_fifo_t *fifo,
        bool flush_entries_read, bool *queue_empty)
{
    int index = 0,clearIndex, i;
    volatile void **q_ptr;
    ompi_cb_fifo_ctl_t *h_ptr, *t_ptr;
    void *read_from_tail = (void *)OMPI_CB_ERROR;

    *queue_empty=false;

    h_ptr=fifo->head;
    t_ptr=fifo->tail;
    q_ptr=fifo->queue;

    /* check to see that the data is valid */
    if ((q_ptr[t_ptr->fifo_index] == OMPI_CB_FREE) ||
            (q_ptr[t_ptr->fifo_index] == OMPI_CB_RESERVED)) {
         read_from_tail=(void *)OMPI_CB_FREE;
         goto CLEANUP;
    }

    /* set return data */
    index = t_ptr->fifo_index;
    read_from_tail = (void *)q_ptr[index];
    opal_atomic_rmb();
    t_ptr->num_to_clear++;

    /* increment counter for later lazy free */
    (t_ptr->fifo_index)++;
    (t_ptr->fifo_index) &= fifo->mask;

    /* check to see if time to do a lazy free of queue slots */
    if ( (t_ptr->num_to_clear == fifo->lazy_free_frequency) ||
            flush_entries_read ) {
        clearIndex = index - t_ptr->num_to_clear + 1;
        clearIndex &= fifo->mask;

        for (i = 0; i < t_ptr->num_to_clear; i++) {
            q_ptr[clearIndex] = OMPI_CB_FREE;
            clearIndex++;
            clearIndex &= fifo->mask;
        }
        t_ptr->num_to_clear = 0;

        /* check to see if queue is empty */
        if( flush_entries_read &&
                (t_ptr->fifo_index == h_ptr->fifo_index) ) {
            *queue_empty=true;
        }
    }

CLEANUP:
    return read_from_tail;
}

#endif				/* !_OMPI_CIRCULAR_BUFFER_FIFO */
