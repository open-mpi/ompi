/*
 * $HEADER$
 */

#ifndef _OMPI_CIRCULAR_BUFFER_FIFO
#define _OMPI_CIRCULAR_BUFFER_FIFO

#include "include/constants.h"
#include "include/sys/cache.h"
#include "include/sys/atomic.h"
#include "mca/mpool/mpool.h"
#include "util/pow2.h"


/** @file
 *
 *  This defines a set of functions to create, and manipulate a FIFO
 *  set up in a circular buffer.  FIFO elements are assumed to be
 *  pointers.  Pointers are written to the head, and read from the
 *  tail.  For thread safety, a spin lock is provided in the
 *  ompi_cb_fifo_ctl_t structure, but it's use must be managed by
 *  the calling routines - this is not by these set of routines.
 */

/* error code */
#define OMPI_CB_ERROR             -1
#define OMPI_CB_FREE      (void *)-2
#define OMPI_CB_RESERVED  (void *)-3

/*
 * Structure used to keep track of the fifo status
 */
struct ompi_cb_fifo_ctl_t {
    /* spin-lock for access control */
    ompi_lock_t lock;

    /* current queue index */
    volatile unsigned int fifo_index;

    /* number of entries that have been used, but not invalidated.  used
     * for lazy resource reclamation */
    volatile unsigned int num_to_clear;

};
typedef struct ompi_cb_fifo_ctl_t ompi_cb_fifo_ctl_t;

/* default settings */

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
static inline int ompi_cb_fifo_init(int size_of_fifo, int lazy_free_freq,
        int fifo_memory_locality_index, int head_memory_locality_index, 
        int tail_memory_locality_index, ompi_cb_fifo_t *fifo, 
        mca_mpool_base_module_t *memory_allocator) 
{

    int errorCode = OMPI_SUCCESS,i;
    size_t len_to_allocate;

    /* verify that size is power of 2, and greatter that 0 - if not, 
     * round up */
    if ( 0 >= size_of_fifo) {
        return OMPI_ERROR;
    }

    /* set fifo size */
    fifo->size = ompi_round_up_to_nearest_pow2(size_of_fifo);

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
    fifo->queue=memory_allocator->mpool_alloc(len_to_allocate,CACHE_LINE_SIZE);
    if ( NULL == fifo->queue) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize the queue entries */
    for (i = 0; i < fifo->size; i++) {
        fifo->queue[i] = OMPI_CB_FREE;
    }

    /* allocate head control structure */
    len_to_allocate = sizeof(ompi_cb_fifo_ctl_t);
    fifo->head=memory_allocator->mpool_alloc(len_to_allocate,CACHE_LINE_SIZE);
    if ( NULL == fifo->head) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize the head structure */
    ompi_atomic_unlock(&(fifo->head->lock));
    fifo->head->fifo_index=0;
    fifo->head->num_to_clear=0;

    /* allocate tail control structure */
    len_to_allocate = sizeof(ompi_cb_fifo_ctl_t);
    fifo->tail=memory_allocator->mpool_alloc(len_to_allocate,CACHE_LINE_SIZE);
    if ( NULL == fifo->tail) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize the head structure */
    ompi_atomic_unlock(&(fifo->tail->lock));
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
static inline int ompi_cb_fifo_free( ompi_cb_fifo_t *fifo, 
        mca_mpool_base_module_t *memory_allocator) 
{

    int errorCode = OMPI_SUCCESS;

    /* make sure null fifo is not passed in */
    if ( NULL == fifo) {
        return OMPI_ERROR;
    }

    /* free fifo array */
    if( NULL != fifo->queue){
        memory_allocator->mpool_free(fifo->queue);
        fifo->queue=NULL;
    }

    /* free head control structure */
    if( NULL != fifo->head) {
        memory_allocator->mpool_free(fifo->head);
        fifo->head=NULL;

    }

    /* free tail control structure */
    if( NULL != fifo->tail) {
        memory_allocator->mpool_free(fifo->tail);
        fifo->tail=NULL;
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
static inline int ompi_cb_fifo_write_to_slot(int slot, void* data, ompi_cb_fifo_t *fifo) {
    int wrote_to_slot = OMPI_CB_ERROR;
    /* make sure that this slot is already reserved */
    if (fifo->queue[slot] == OMPI_CB_RESERVED ) {
        fifo->queue[slot] = data;
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
static inline int ompi_cb_fifo_write_to_head(void *data, ompi_cb_fifo_t *fifo) {
    int slot = OMPI_CB_ERROR, index;
    index = fifo->head->fifo_index;
    /* make sure the head is pointing at a free element - avoid wrap
     * around */
    if (fifo->queue[index] == OMPI_CB_FREE) {
        slot = index;
        fifo->queue[slot] = data;
        (fifo->head->fifo_index)++;
        (fifo->head->fifo_index) &= fifo->mask;
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
static inline int ompi_cb_fifo_get_slot(ompi_cb_fifo_t *fifo) {
    int return_value = OMPI_CB_ERROR,index;

    index = fifo->head->fifo_index;
    /* try and reserve slot */
    if (fifo->queue[index] == OMPI_CB_FREE) {
        fifo->queue[index] = OMPI_CB_RESERVED;
        return_value = index;
        (fifo->head->fifo_index)++;
        (fifo->head->fifo_index) &= fifo->mask;
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
static inline void *ompi_cb_fifo_read_from_tail(ompi_cb_fifo_t *fifo,
        bool flush_entries_read, bool *queue_empty) 
{
    int index = 0,clearIndex, i;
    void *read_from_tail = (void *)OMPI_CB_ERROR;
    *queue_empty=false;

    /* check to see that the data is valid */
    if ((fifo->queue[fifo->tail->fifo_index] == OMPI_CB_FREE) ||
            (fifo->queue[fifo->tail->fifo_index] == OMPI_CB_RESERVED)) 
    {
        return (void *)OMPI_CB_ERROR;
    }

    /* set return data */
    index = fifo->tail->fifo_index;
    read_from_tail = (void *)fifo->queue[index];
    fifo->tail->num_to_clear++;

    /* check to see if time to do a lazy free of queue slots */
    if ( (fifo->tail->num_to_clear == fifo->lazy_free_frequency) ||
            flush_entries_read ) {
        clearIndex = index - fifo->tail->num_to_clear + 1;
        clearIndex &= fifo->mask;
        
        for (i = 0; i < fifo->tail->num_to_clear; i++) {
            fifo->queue[clearIndex] = OMPI_CB_FREE;
            clearIndex++;
            clearIndex &= fifo->mask;
        }
        fifo->tail->num_to_clear = 0;

        /* check to see if queue is empty */
        if( flush_entries_read && 
                (fifo->tail->fifo_index == fifo->head->fifo_index) ) {
            *queue_empty=true;
        }
    }
    
    /* increment counter for later lazy free */
    (fifo->tail->fifo_index)++;
    (fifo->tail->fifo_index) &= fifo->mask;

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

#endif				/* !_OMPI_CIRCULAR_BUFFER_FIFO */
