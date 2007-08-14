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

#ifndef _OMPI_FIFO
#define _OMPI_FIFO

#include "ompi/constants.h"
#include "opal/sys/cache.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/class/ompi_circular_buffer_fifo.h"


/** @file
 *
 * This defines a set of functions to create, and manipulate a FIFO
 * implemented as a link list of circular buffer FIFO's.  FIFO
 * elements are assumed to be pointers.  Pointers are written to the
 * head, and read from the tail.  For thread safety, a spin lock is
 * provided in the !!!!!ompi_cb_fifo_ctl_t!!!! structure, but it's use
 * must be managed by the calling routines - this is not by these set
 * of routines.  When a write to a circular buffer queue will overflow
 * that queue, the next circular buffer queue if the link list is
 * used, if it is empty, or a new one is inserted into the list.
 *
 * This set of routines is currently exclusively used by the sm btl,
 * and has been tailored to meet its needs (i.e., it is probably not
 * suitable as a general purpose fifo). 
 *
 * Before describing any further, a note about mmap() is in order.
 * mmap() is used to create/attach shared memory segments to a
 * process.  It is used by OMPI to manage shared memory.
 * Specifically, each process ends up calling mmap() to create or
 * attach shared memory; the end result is that multiple processes
 * have the same shared memory segment attached to their process.
 * This shared memory is therefore used here in the fifo code.  
 *
 * However, it is important to note that when attaching the same
 * shared memory segment to multiple processes, mmap() does *not* need
 * to return the same virtual address to the beginning of the shared
 * memory segment to each process.  That is, the virtual address
 * returned in each process will point to the same shared memory
 * segment as all others, but its virtual address value may be
 * different.  Specifically, process A may get the value X back from
 * mmap(), while process B, who attached the same shared memory
 * segment as process A, may get back the value Y from mmap().
 * Process C may attach the same shared memory segment and get back
 * value X from mmap().  This is perfectly legal mmap() behavior.
 *
 * As such, our code -- including this fifo code -- needs to be able
 * to handle the cases where the base address is the same and the
 * cases where it is different.
 *
 * There are four main interface functions:
 *
 * ompi_fifo_init_same_base_addr(): create a fifo for the case where
 * the creating process shares a common shared memory segment base
 * address.
 *
 * ompi_fifo_write_to_head_same_base_addr(): write a value to the head
 * of the fifo for the case where the shared memory segment virtual
 * address is the same as the process who created the fifo.
 *
 * ompi_fifo_read_from_tail_same_base_addr(): read a value from the
 * tail of the fifo for the case where the shared memory segment
 * virtual address is the same as the process who created the fifo.
 *
 * ompi_fifo_read_from_tail(): read a value from the tail of the fifo
 * for the case where the shared memory segment virtual address is
 * *not* the same as the process who created the fifo.
 *
 * The data structures used in these fifos are carefully structured to
 * be lockless, even when used in shared memory.  However, this is
 * predicated upon there being only exactly *ONE* concurrent writer
 * and *ONE* concurrent reader (in terms of the sm btl, two fifos are
 * established between each process pair; one for data flowing A->B
 * and one for data flowing B->A).  Hence, the writer always looks at
 * the "head" and the reader always looks at the "tail."
 *
 * The general scheme of the fifo is that this class is an upper-level
 * manager for the ompi_circular_buffer_fifo_t class.  When an
 * ompi_fifo_t instance is created, it creates an
 * ompi_circular_buffer_fifo_t.  Items can then be put into the fifo
 * until the circular buffer fills up (i.e., items have not been
 * removed from the circular buffer, so it gets full).  The
 * ompi_fifo_t class will manage this case and create another
 * circular_buffer and start putting items in there.  This can
 * continue indefinitely; the ompi_fifo_t class will create a linked
 * list of circular buffers in order to create storage for any items
 * that need to be put in the fifo.  
 *
 * The tail will then read from these circular buffers in order,
 * draining them as it goes.
 *
 * The linked list of circular buffers is created in a circle, so if
 * you have N circular buffers, the fill pattern will essentially go
 * in a circle (assuming that the reader is dutifully reading/draining
 * behind the writer).  Yes, this means that we have a ring of
 * circular buffers.  A single circular buffer is treated as a
 * standalone entitle, a reader/writer pair can utilize it
 * indefinitely; they will never move on to the next circular buffer
 * unless the writer gets so far ahead of the reader that the current
 * circular buffer fills up and the writer moves on to the next
 * circular buffer.  In this case, the reader will eventually drain
 * the current circular buffer and then move on to the next circular
 * buffer (and assumedly eventually catch up to the writer).
 *
 * The natural question of "why bother doing this instead of just
 * having an array of pointers that you realloc?" arises.  The intent
 * with this class is to have a lockless structure -- using realloc,
 * by definition, means that you would have to lock every single
 * access to the array to ensure that it doesn't get realloc'ed from
 * underneath you.  This is definitely something we want to avoid for
 * performance reasons.
 *
 * Hence, once you get your head wrapped around this scheme, it
 * actually does make sense (and give good performance).
 *
 ********************************* NOTE *******************************
 * 
 * Although the scheme is designed to be lockless, there is currently
 * one lock used in this scheme.  There is a nasty race condition
 * between multiple processes that if the writer fills up a circular
 * buffer before anything this read, it can make the decision to
 * create a new circular buffer (because that one is full).  However,
 * if, at the same time, the reader takes over -- after the decision
 * has been made to make a new circular buffer, and after some [but
 * not all] of the data fields are updated to reflect this -- the
 * reader can drain the entire current circular buffer, obviating the
 * need to make a new circular buffer (because there's now space
 * available in the current one).  The reader will then update some
 * data fields in the fifo.
 *
 * This can lead to a fifo management consistency error -- the reader
 * thinks it is advancing to the next circular bufer but it really
 * ends up back on the same circular buffer (because the writer had
 * not updated the "next cb" field yet).  The reader is then stuck in
 * a cb where nothing will arrive until the writer loops all the way
 * around (i.e., through all other existing circular buffers) and
 * starts writing to the circular buffer where the reader is waiting.
 * This effectively means that the reader will miss a lot of messages.
 *
 * So we had to add a lock to protect this -- when the writer decides
 * to make a new circular buffer and when the reader decides to move
 * to the new circular buffer.  It is a rather coarse-grained lock; it
 * convers a relatively large chunk of code in the writing_to_head
 * function, but, interestingly enough, this seems to create *better*
 * performance for sending large messages via shared memory (i.e.,
 * netpipe graphs with and without this lock show that using the lock
 * gives better overall bandwidth for large messages).  We do lose a
 * bit of overall bandwidth for mid-range message sizes, though.
 *
 * We feel that this lock can probably be eventually removed from the
 * implementation; we recognized this race condition and ran out of
 * time to fix is properly (i.e., in a lockless way).  As such, we
 * employed a lock to serialize the access and protect it that way.
 * This issue should be revisited someday to remove the lock.
 *
 * See the notes in the writer function for more details on the lock.
 */

/*
 * Structure by the the  ompi_fifo routines to keep track of some
 * extra queue information not needed by the ompi_cb_fifo routines.
 */
struct ompi_cb_fifo_wrapper_t {
    /* pointer to ompi_cb_fifo_ctl_t structure in use */
    ompi_cb_fifo_t cb_fifo;

    /* pointer to next ompi_cb_fifo_ctl_t structure.  This is always
       stored as an absolute address. */
    struct ompi_cb_fifo_wrapper_t *next_fifo_wrapper;

    /* flag indicating if cb_fifo has over flown - need this to force
     * release of entries already read */
    volatile bool cb_overflow;
};

typedef struct ompi_cb_fifo_wrapper_t ompi_cb_fifo_wrapper_t;

/* data structure used to describe the fifo */
struct ompi_fifo_t {
    /* locks for multi-process synchronization */
    opal_atomic_lock_t fifo_lock;

    /* locks for thread synchronization */
    opal_atomic_lock_t *head_lock;

    /* locks for thread synchronization */
    opal_atomic_lock_t *tail_lock;

    /* size of fifo */
    int size;

    /* fifo memory locality index */
    int fifo_memory_locality_index;

    /* head memory locality index */
    int head_memory_locality_index;

    /* tail memory locality index */
    int tail_memory_locality_index;

    /* offset between sender and receiver shared mapping */
    ptrdiff_t offset;

    /* pointer to head (write) ompi_cb_fifo_t structure.  This is
       always stored as an sender size address. */
    ompi_cb_fifo_wrapper_t *head;

    /* pointer to tail (read) ompi_cb_fifo_t structure.  This is
       always stored as an receiver size address. */
    ompi_cb_fifo_wrapper_t *tail;
};

typedef struct ompi_fifo_t ompi_fifo_t;

/**
 * Initialize a fifo 
 *
 * @param size_of_cb_fifo Length of fifo array (IN)
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
static inline int ompi_fifo_init(int size_of_cb_fifo,
        int lazy_free_freq, int fifo_memory_locality_index, 
        int head_memory_locality_index, int tail_memory_locality_index, 
        ompi_fifo_t *fifo, ptrdiff_t offset,
        mca_mpool_base_module_t *memory_allocator)
{
    int error_code;

    fifo->offset = offset;
    fifo->size = size_of_cb_fifo;
    fifo->fifo_memory_locality_index = fifo_memory_locality_index;
    fifo->head_memory_locality_index = head_memory_locality_index;
    fifo->tail_memory_locality_index = tail_memory_locality_index;

    /* allocate head ompi_cb_fifo_t structure and place for head and tail locks
     * on different cache lines */
    fifo->head = (ompi_cb_fifo_wrapper_t*)memory_allocator->mpool_alloc(
            memory_allocator, sizeof(ompi_cb_fifo_wrapper_t), CACHE_LINE_SIZE,
            0, NULL);
    if(NULL == fifo->head) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize the circular buffer fifo head structure */
    error_code=ompi_cb_fifo_init(size_of_cb_fifo,
            lazy_free_freq, fifo_memory_locality_index, 
            head_memory_locality_index, tail_memory_locality_index, 
            &(fifo->head->cb_fifo), offset, memory_allocator);
    if ( OMPI_SUCCESS != error_code ) {
        return error_code;
    }

    /* finish head initialization */
    opal_atomic_init(&(fifo->fifo_lock), OPAL_ATOMIC_UNLOCKED);
    fifo->head->next_fifo_wrapper = fifo->head;
    fifo->head->cb_overflow=false;  /* no attempt to overflow the queue */

    /* set the tail */
    fifo->tail = (ompi_cb_fifo_wrapper_t*)((char*)fifo->head - offset);

    /* return */
    return error_code;
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
static inline int ompi_fifo_write_to_head(void *data,
        ompi_fifo_t *fifo, mca_mpool_base_module_t *fifo_allocator)
{
    int error_code=OMPI_SUCCESS;
    ompi_cb_fifo_wrapper_t *next_ff;

    /* attempt to write data to head ompi_fifo_cb_fifo_t */
    error_code = ompi_cb_fifo_write_to_head(data, &fifo->head->cb_fifo);

    /* If the queue is full, create a new circular buffer and put the
       data in it. */
    if(OMPI_CB_ERROR == error_code) {
        /* NOTE: This is the lock described in the top-level comment
           in this file.  There are corresponding uses of this lock in
           both of the read routines.  We need to protect this whole
           section -- setting cb_overflow to true through setting the
           next_fifo_wrapper to the next circular buffer.  It is
           likely possible to do this in a finer grain; indeed, it is
           likely that we can get rid of this lock altogther, but it
           will take some refactoring to make the data updates
           safe.  */
        opal_atomic_lock(&fifo->fifo_lock);

        /* mark queue as overflown */
        fifo->head->cb_overflow = true;

        /* We retry to write to the old head before creating new one just in
         * case consumer read all entries after first attempt failed, but
         * before we set cb_overflow to true */
        error_code=ompi_cb_fifo_write_to_head(data, &fifo->head->cb_fifo);

        if(error_code != OMPI_CB_ERROR) {
            fifo->head->cb_overflow = false;
            opal_atomic_unlock(&(fifo->fifo_lock));
            return OMPI_SUCCESS;
        }

        /* see if next queue is available - while the next queue
         * has not been emptied, it will be marked as overflowen*/
        next_ff = fifo->head->next_fifo_wrapper;

        /* if next queue not available, allocate new queue */
        if (next_ff->cb_overflow) {
            /* allocate head ompi_cb_fifo_t structure */
            next_ff = (ompi_cb_fifo_wrapper_t*)fifo_allocator->mpool_alloc(
                    fifo_allocator, sizeof(ompi_cb_fifo_wrapper_t),
                    CACHE_LINE_SIZE, 0, NULL);
            if (NULL == next_ff) {
                opal_atomic_unlock(&fifo->fifo_lock);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            /* initialize the circular buffer fifo head structure */
            error_code = ompi_cb_fifo_init(fifo->size,
                    fifo->head->cb_fifo.lazy_free_frequency,
                    fifo->fifo_memory_locality_index,
                    fifo->head_memory_locality_index,
                    fifo->tail_memory_locality_index,
                    &(next_ff->cb_fifo), fifo->offset, fifo_allocator);
            if (OMPI_SUCCESS != error_code) {
                opal_atomic_unlock(&fifo->fifo_lock);
                return error_code;
            }

            /* finish new element initialization */
            /* only one element in the link list */
            next_ff->next_fifo_wrapper = fifo->head->next_fifo_wrapper;
            next_ff->cb_overflow = false; /* no attempt to overflow the queue */
            fifo->head->next_fifo_wrapper = next_ff;
        }

        /* reset head pointer */
        fifo->head = next_ff;
        opal_atomic_unlock(&fifo->fifo_lock);

        /* write data to new head structure */
        error_code=ompi_cb_fifo_write_to_head(data, &fifo->head->cb_fifo);
        if( OMPI_CB_ERROR == error_code ) {
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS; 
}


/**
 * Try to read pointer from the tail of the queue
 *
 * @param fifo Pointer to data structure defining this fifo (IN)
 *
 * @returncode Pointer - OMPI_CB_FREE indicates no data to read
 *
 */
static inline
void *ompi_fifo_read_from_tail(ompi_fifo_t *fifo)
{
    /* local parameters */
    void *return_value;
    bool queue_empty;

    /* get next element */
    return_value = ompi_cb_fifo_read_from_tail(&fifo->tail->cb_fifo,
            fifo->tail->cb_overflow, &queue_empty);

    /* check to see if need to move on to next cb_fifo in the link list */
    if(queue_empty) {
        /* queue_emptied - move on to next element in fifo */
        /* See the big comment at the top of this file about this
           lock. */
        opal_atomic_lock(&(fifo->fifo_lock));
        if(fifo->tail->cb_overflow == true) {
            fifo->tail->cb_overflow = false;
            fifo->tail = (ompi_cb_fifo_wrapper_t*)
                ((char*)fifo->tail->next_fifo_wrapper - fifo->offset);
        }
        opal_atomic_unlock(&(fifo->fifo_lock));
    }

    return return_value;
}

#endif				/* !_OMPI_FIFO */
