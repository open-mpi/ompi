/*
 * $HEADER$
 */

#ifndef _OMPI_CIRCULAR_BUFFER_FIFO
#define _OMPI_CIRCULAR_BUFFER_FIFO

#include "include/constants.h"
#include "include/sys/cache.h"
#include "os/atomic.h"
#include "mca/mpool/mpool.h"
#include "util/pow2.h"


/** @file
 *
 *  This defines a set of functions to create, and manipulate a FIFO
 *  set up in a circular buffer.  FIFO elements are assumed to be
 *  pointers.  Pointers are written to the head, and read from the
 *  tail.
 */

/* error code */
#define OMPI_CB_ERROR             -1
#define OMPI_CB_FREE      (void *) 0
#define OMPI_CB_RESERVED  (void *) 1

/*
 * Structure used to keep track of the fifo status
 */
struct ompi_cb_fifo_ctl_t {
    /* spin-lock for access control */
    ompi_lock_data_t lock;

    /* current queue index */
    volatile unsigned int fifo_index;

    /* number of entries that have been used, but not invalidated.  used
     * for lazy resource reclamation */
    volatile unsigned int num_to_clear;

};
typedef struct ompi_cb_fifo_ctl_t ompi_cb_fifo_ctl_t;

/* default settings */
enum {
    ompi_default_fifo_size = 512, ompi_lazy_free_frequency = 500
};

/* data structure used to describe the fifo */
struct ompi_cb_fifo_t {

    /* size of fifo */
    int size;

    /* frequency of lazy free */
    int lazy_free_frequency;

    /* head of queue - where next entry will be written */
    ompi_cb_fifo_ctl_t *head;

    /* tail of queue - next element to read */
    ompi_cb_fifo_ctl_t *tail;

    /* mask - to handle wrap around */
    unsigned int mask;

    /* circular buffer array */
    void **queue;

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
    spinunlock(&(fifo->head->lock));
    fifo->head->fifo_index=0;
    fifo->head->num_to_clear=0;

    /* allocate tail control structure */
    len_to_allocate = sizeof(ompi_cb_fifo_ctl_t);
    fifo->tail=memory_allocator->mpool_alloc(len_to_allocate,CACHE_LINE_SIZE);
    if ( NULL == fifo->tail) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize the head structure */
    spinunlock(&(fifo->tail->lock));
    fifo->tail->fifo_index=0;
    fifo->tail->num_to_clear=0;

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
    }

    /* free head control structure */
    if( NULL != fifo->head) {
            memory_allocator->mpool_free(fifo->head);
    }

    /* free tail control structure */
    if( NULL != fifo->tail) {
            memory_allocator->mpool_free(fifo->tail);
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
    spinlock(&(fifo->head->lock));
    index = fifo->head->fifo_index;
    /* make sure the head is pointing at a free element - avoid wrap
     * around */
    if (fifo->queue[index] == OMPI_CB_FREE) {
        slot = index;
        fifo->queue[slot] = data;
        (fifo->head->fifo_index)++;
        (fifo->head->fifo_index) &= fifo->mask;
    }
    spinunlock(&(fifo->head->lock));

    /* return */
    return slot;
}

/**
 * Try and write pointer ot the head of the queue without locking 
 * the head structure
 *
 * Try to write pointer to the head of the queue
 *
 * @param data Pointer value to write in specified slot (IN)
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @returncode Slot index to which data is written
 *
 */
static inline int ompi_cb_fifo_write_to_head_no_lock(void *data, ompi_cb_fifo_t *fifo) 
{
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

    spinlock(&(fifo->head->lock));
    index = fifo->head->fifo_index;
    /* try and reserve slot */
    if (fifo->queue[index] == OMPI_CB_FREE) {
        fifo->queue[index] = OMPI_CB_RESERVED;
        return_value = index;
        (fifo->head->fifo_index)++;
        (fifo->head->fifo_index) &= fifo->mask;
    }
    spinunlock(&(fifo->head->lock));

    /* return */
    return return_value;
}

/**
 * Reserve slot in the fifo array - no locking for thread safety done
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @returncode Slot index to which data is written
 *
 * @returncode OMPI_CB_ERROR failed to allocate index
 *
 */
static inline int ompi_cb_fifo_get_slot_no_lock(ompi_cb_fifo_t *fifo) 
{
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
 * @param data Pointer to where data was be written (out)
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @returncode Slot index to which data is written
 *
 */
static inline int ompi_cb_fifo_read_from_tail(void** data, ompi_cb_fifo_t *fifo) 
{
    int read_from_tail = OMPI_CB_ERROR,index = 0,clearIndex, i;

    /* check to see that the data is valid */
    if ((fifo->queue[fifo->tail->fifo_index] == OMPI_CB_FREE) ||
            (fifo->queue[fifo->tail->fifo_index] == OMPI_CB_RESERVED)) 
    {
        return OMPI_CB_ERROR;
    }

    /* lock tail - for thread safety */
    spinlock(&(fifo->tail->lock));

    /* make sure that there is still data to read  - other thread
     * may have gotten here first */
    if ( (fifo->queue[fifo->tail->fifo_index] == OMPI_CB_FREE ) ||
            (fifo->queue[fifo->tail->fifo_index] == OMPI_CB_RESERVED ) ) {
        spinunlock(&(fifo->tail->lock));
        return OMPI_CB_ERROR;
    }

    /* set return data */
    index = fifo->tail->fifo_index;
    *data = fifo->queue[index];
    fifo->tail->num_to_clear++;

    /* check to see if time to do a lazy free of queue slots */
    if (fifo->tail->num_to_clear == ompi_lazy_free_frequency) {
        clearIndex = index - ompi_lazy_free_frequency + 1;
        clearIndex &= fifo->mask;
        
        for (i = 0; i < ompi_lazy_free_frequency; i++) {
            fifo->queue[clearIndex] = OMPI_CB_FREE;
            clearIndex++;
            clearIndex &= fifo->mask;
        }
            fifo->tail->num_to_clear = 0;
    }
    
    /* increment counter for later lazy free */
    read_from_tail = fifo->tail->fifo_index;
    (fifo->tail->fifo_index)++;
    (fifo->tail->fifo_index) &= fifo->mask;

    /* unlock tail */
    spinunlock(&(fifo->tail->lock));

    /* return */
    return read_from_tail;
}


/**
 * Try to read pointer from the tail of the queue - no locking for
 * thread safety.
 *
 * @param data Pointer to where data was be written (out)
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @returncode Slot index to which data is written
 *
 */
static inline int ompi_cb_fifo_read_from_tail_no_lock(void** data, ompi_cb_fifo_t *fifo) 
{
    int read_from_tail = OMPI_CB_ERROR,index = 0,clearIndex, i;

    /* check to see that the data is valid */
    if ((fifo->queue[fifo->tail->fifo_index] == OMPI_CB_FREE) ||
            (fifo->queue[fifo->tail->fifo_index] == OMPI_CB_RESERVED)) 
    {
        return OMPI_CB_ERROR;
    }

    /* lock tail - for thread safety */

    /* make sure that there is still data to read  - other thread
     * may have gotten here first */
    if ((fifo->queue[fifo->tail->fifo_index] == OMPI_CB_FREE ) ||
            (fifo->queue[fifo->tail->fifo_index] == OMPI_CB_RESERVED )) {
        return OMPI_CB_ERROR;
    }

    /* set return data */
    index = fifo->tail->fifo_index;
    *data = fifo->queue[index];
    fifo->tail->num_to_clear++;

    /* check to see if time to do a lazy free of queue slots */
    if (fifo->tail->num_to_clear == ompi_lazy_free_frequency) {
        clearIndex = index - ompi_lazy_free_frequency + 1;
        clearIndex &= fifo->mask;
        
        for (i = 0; i < ompi_lazy_free_frequency; i++) {
            fifo->queue[clearIndex] = OMPI_CB_FREE;
            clearIndex++;
            clearIndex &= fifo->mask;
        }
            fifo->tail->num_to_clear = 0;
    }
    
    /* increment counter for later lazy free */
    read_from_tail = fifo->tail->fifo_index;
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
