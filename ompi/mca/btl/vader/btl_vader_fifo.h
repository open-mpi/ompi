/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.  
 *                         All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_VADER_FIFO_H
#define MCA_BTL_VADER_FIFO_H

#include "btl_vader.h"
#include "btl_vader_endpoint.h"
#include "btl_vader_frag.h"

#define VADER_FIFO_FREE  ((intptr_t)-2)

/*
 * Shared Memory FIFOs
 *
 * The FIFO is implemented as a linked list of frag headers. The fifo has multiple
 * producers and a single consumer (in the single thread case) so the tail needs
 * to be modified by an atomic or protected by a atomic lock.
 *
 * Since the frags live in shared memory that is mapped differently into
 * each address space, the head and tail pointers are relative (each process must
 * add its own offset).
 *
 * We introduce some padding at the end of the structure but it is probably unnecessary.
 */

#if defined(OPAL_HAVE_ATOMIC_SWAP_64)

/* lock free fifo */
struct vader_fifo_t {
    volatile intptr_t fifo_head;
    volatile intptr_t fifo_tail;
    char pad[VADER_CACHE_LINE_PAD - 2 * sizeof (intptr_t)];
};
typedef struct vader_fifo_t vader_fifo_t;

static inline int vader_fifo_init (vader_fifo_t *fifo)
{
    fifo->fifo_head = fifo->fifo_tail = VADER_FIFO_FREE;

    return OMPI_SUCCESS;
}

static inline void vader_fifo_write (mca_btl_vader_hdr_t *hdr, int rank)
{
    vader_fifo_t *fifo = mca_btl_vader_component.fifo[rank];
    intptr_t prev, value = VIRTUAL2RELATIVE(hdr);

    hdr->next = VADER_FIFO_FREE;

    prev = opal_atomic_swap_ptr (&fifo->fifo_tail, value);

    opal_atomic_rmb ();

    if (OPAL_LIKELY(VADER_FIFO_FREE != prev)) {
        hdr = (mca_btl_vader_hdr_t *) RELATIVE2VIRTUAL(prev);
        hdr->next = value;
    } else {
        fifo->fifo_head = value;
    }

    opal_atomic_wmb ();
}

static inline mca_btl_vader_hdr_t *vader_fifo_read (vader_fifo_t *fifo)
{
    mca_btl_vader_hdr_t *hdr;
    intptr_t value;

    opal_atomic_rmb ();

    value = opal_atomic_swap_ptr (&fifo->fifo_head, VADER_FIFO_FREE);
    if (VADER_FIFO_FREE == value) {
        /* fifo is empty or we lost the race with another thread */
        return NULL;
    }

    hdr = (mca_btl_vader_hdr_t *) RELATIVE2VIRTUAL(value);

    if (OPAL_UNLIKELY(VADER_FIFO_FREE == hdr->next)) {
        if (!opal_atomic_cmpset_ptr (&fifo->fifo_tail, (void *)value,
                                     (void *)VADER_FIFO_FREE)) {
            while (VADER_FIFO_FREE == hdr->next) {
                opal_atomic_rmb ();
            }

            fifo->fifo_head = hdr->next;
        }
    } else {
        fifo->fifo_head = hdr->next;
    }

    opal_atomic_wmb ();

    return hdr; 
}

#else /* defined(OPAL_HAVE_ATOMIC_SWAP_64) */

struct vader_fifo_t {
    volatile void *fifo_head;
    opal_atomic_lock_t head_lock;
    volatile void *fifo_tail;
    opal_atomic_lock_t tail_lock;
};
typedef struct vader_fifo_t vader_fifo_t;

static inline int vader_fifo_init (vader_fifo_t *fifo)
{
    fifo->fifo_head = fifo->fifo_tail = VADER_FIFO_FREE;
    opal_atomic_init (&(fifo->head_lock), OPAL_ATOMIC_UNLOCKED);
    opal_atomic_init (&(fifo->tail_lock), OPAL_ATOMIC_UNLOCKED);
    opal_atomic_unlock(&(fifo->head_lock));  /* should be unnecessary */
    opal_atomic_unlock(&(fifo->tail_lock));  /* should be unnecessary */

    return OMPI_SUCCESS;
}

static inline int vader_fifo_write (void *value, vader_fifo_t *fifo)
{
    mca_btl_vader_hdr_t *hdr;
    void *prev;
    int rc;

    hdr = (mca_btl_vader_hdr_t *) RELATIVE2VIRTUAL(value);
    hdr->next = VADER_FIFO_FREE;

    opal_atomic_rmb();

    opal_atomic_lock(&(fifo->tail_lock));
    prev = (void *) fifo->fifo_tail;
    fifo->fifo_tail = value;

    if (OPAL_LIKELY(VADER_FIFO_FREE != prev)) {
        hdr = (mca_btl_vader_hdr_t *) RELATIVE2VIRTUAL(prev);
        hdr->next = value;
    } else {
         fifo->fifo_head = value;
    }

    opal_atomic_unlock(&(fifo->tail_lock));

    return OMPI_SUCCESS;
}


static inline void *vader_fifo_read (vader_fifo_t *fifo)
{
    mca_btl_vader_hdr_t *hdr;
    void *value;
    int rc;

    /* aquire thread lock */
    if (opal_using_threads ()) {
        opal_atomic_lock (&fifo->head_lock);
    }

    opal_atomic_rmb();
    if (VADER_FIFO_FREE == fifo->fifo_head) {
        return VADER_FIFO_FREE;
    }

    value = (void *) fifo->fifo_head;
    hdr = (mca_btl_vader_hdr_t *) RELATIVE2VIRTUAL(value);
    fifo->fifo_head = hdr->next;

    if (OPAL_UNLIKELY(VADER_FIFO_FREE == fifo->fifo_head)) {
        opal_atomic_rmb();
        opal_atomic_lock(&(fifo->tail_lock));

        if (OPAL_LIKELY(fifo->fifo_tail == value)) {
            fifo->fifo_tail = VADER_FIFO_FREE;
        } else {
            fifo->fifo_head = hdr->next;
        }
        opal_atomic_unlock(&(fifo->tail_lock));
    }

    /* release thread lock */
    if (opal_using_threads ()) {
        opal_atomic_unlock (&fifo->head_lock);
    }
    
    return value;
}
#endif /* else !defined(OPAL_HAVE_ATOMIC_SWAP_64) */

#endif /* MCA_BTL_VADER_FIFO_H */
